import re
import json
import asyncio
from typing import List, Tuple, Optional, Dict, Any

from sqlalchemy.orm import Session
from sqlalchemy.sql import func
from pydantic import BaseModel, Field

from app.models.input_source_model import InputSource
from app.models.procedure_model import Procedure
from app.models.m204_file_model import M204File
from app.models.m204_variable_model import M204Variable
from app.models.image_statement_model import ImageStatement
from app.models.procedure_call_model import ProcedureCall

from app.schemas.m204_analysis_schema import (
    M204ProcedureCreateSchema, M204ProcedureResponseSchema,
    M204FileCreateSchema, M204FileResponseSchema,
    M204VariableCreateSchema, M204VariableResponseSchema,
    M204ProcedureCallCreateSchema, M204ProcedureCallResponseSchema,
    M204AnalysisResultDataSchema,
    ImageStatementCreateSchema, ImageStatementResponseSchema
)
# Importing M204FieldVsamSuggestion from parmlib_analysis_service to avoid redefinition
from app.services.parmlib_analysis_service import M204FieldVsamSuggestion
from app.services.rag_service import RagService # For type hinting
from app.utils.logger import log
from app.config.llm_config import llm_config

# --- Pydantic Models for LLM Structured Output ---

class M204ConceptIdentificationOutput(BaseModel):
    """Structured output for identifying key M204 concepts in a procedure."""
    procedure_name: str = Field(description="The original M204 procedure name.")
    identified_concepts: List[str] = Field(description="A list of key M204 commands, keywords, or concepts (e.g., FIND, FOR EACH VALUE, %variables, IMAGE, SCREEN) found or inferred from the procedure. These concepts will be used to retrieve relevant documentation.")
    brief_reasoning: str = Field(description="A very brief explanation of why these concepts are relevant to understanding the procedure's core functionality.")

class ProcedureAnalysisOutput(BaseModel):
    """Structured output for procedure summarization and COBOL name suggestion."""
    procedure_name: str = Field(description="The original M204 procedure name.")
    cobol_name_suggestion: str = Field(description="A suitable COBOL program name for this procedure, considering potential naming conflicts and COBOL naming conventions (e.g., 8 characters, alphanumeric).")
    procedure_summary: str = Field(description="A detailed summary of the M204 procedure, covering its purpose, key logic, inputs, outputs, and interactions (e.g., file I/O, database calls), informed by the provided RAG context.")

class M204VariableToCobolOutput(BaseModel):
    """Structured output for M204 variable to COBOL name suggestion."""
    m204_variable_name: str = Field(description="The original M204 variable name.")
    suggested_cobol_variable_name: str = Field(description="A suitable COBOL variable name, following COBOL naming conventions (e.g., max 30 chars, alphanumeric, hyphens, avoid M204-specific symbols).")
    reasoning: Optional[str] = Field(description="Brief reasoning for the suggestion.", default=None)

class M204FileVsamAnalysisOutput(BaseModel):
    """Structured output for VSAM analysis of an M204 file."""
    m204_file_name: str = Field(description="The original M204 file name being analyzed.")
    suggested_vsam_type: str = Field(description="Suggested VSAM organization (e.g., KSDS, ESDS, RRDS, LDS).")
    field_specific_suggestions: List[M204FieldVsamSuggestion] = Field(description="List of VSAM-specific suggestions for relevant M204 fields in this file.")
    overall_reasoning: Optional[str] = Field(description="Overall reasoning for the VSAM structure suggestion for the file.", default=None)

class TestCase(BaseModel):
    """Defines the structure for a single unit test case."""
    test_case_id: str = Field(description="A unique identifier for the test case (e.g., TC_001, TC_VALID_INPUT).")
    description: str = Field(description="A brief description of what this test case covers.")
    preconditions: Optional[List[str]] = Field(description="Any preconditions or setup required (e.g., specific data in files, %variables set).", default_factory=list)
    inputs: Dict[str, Any] = Field(description="Key-value pairs of input parameters or %variables and their test values.")
    expected_outputs: Dict[str, Any] = Field(description="Key-value pairs of expected output %variables, screen elements, or file states and their values.")
    expected_behavior_description: str = Field(description="A textual description of the expected behavior, side effects, or outcome (e.g., 'Record X is written to FILEA', 'Error message Y is displayed').")

class ProcedureTestCaseGenerationOutput(BaseModel):
    """Structured output for LLM-generated unit test cases for a procedure."""
    procedure_name: str = Field(description="The original M204 procedure name.")
    test_cases: List[TestCase] = Field(description="A list of suggested unit test cases for the procedure.")
    generation_reasoning: Optional[str] = Field(description="Brief reasoning behind the types of test cases generated or any challenges.", default=None)


# --- Helper Functions ---
def _parse_m204_parameters(params_str: Optional[str]) -> Optional[Dict[str, Any]]:
    if not params_str:
        return None
    
    parsed_params: Dict[str, Any] = {"parameters": []}
    param_pattern = re.compile(
        r"([%A-Z0-9_#@$-]+)\s*(?:IS\s+([A-Z\s]+?)(?:\s+LEN\s+(\d+))?)?", 
        re.IGNORECASE
    )
    
    individual_params = params_str.split(',')
    for p_str in individual_params:
        p_str = p_str.strip()
        if not p_str:
            continue
        
        match = param_pattern.match(p_str)
        if match:
            name = match.group(1)
            param_type = match.group(2).strip().upper() if match.group(2) else "UNTYPED"
            length = int(match.group(3)) if match.group(3) else None
            
            param_detail: Dict[str, Any] = {"name": name, "type": param_type}
            if length is not None:
                param_detail["length"] = length
            parsed_params["parameters"].append(param_detail)
        else:
            # Handle simple names without type/len as UNTYPED
            if re.fullmatch(r"[%A-Z0-9_#@$-]+", p_str, re.IGNORECASE):
                 parsed_params["parameters"].append({"name": p_str, "type": "UNTYPED"})
            else:
                parsed_params["parameters"].append({"name": p_str, "type": "UNKNOWN_FORMAT"})
                log.debug(f"Could not fully parse parameter: {p_str} in '{params_str}' as a standard M204 parameter. Stored as UNKNOWN_FORMAT.")
            
    return parsed_params if parsed_params["parameters"] else None


async def _analyze_single_procedure_llm(
    proc_name: str,
    params_str: Optional[str],
    current_procedure_content: str,
    m204_param_names: List[str], 
    input_source_id_for_debug: int,
    original_filename_for_debug: str,
    rag_service: Optional[RagService] 
) -> Dict[str, Any]:
    """
    Performs LLM analysis (concept ID, RAG, summarization, test case generation) for a single M204 procedure.
    """
    generated_summary: Optional[str] = f"AI analysis could not be completed for {proc_name}."
    suggested_cobol_name: Optional[str] = proc_name 
    suggested_test_cases_json: Optional[List[Dict[str, Any]]] = None
    rag_context_for_summary: str = "No RAG context was available or generated for summarization."
    json_text_output_concept: Optional[str] = "N/A"
    json_text_output_summary: Optional[str] = "N/A"
    json_text_output_test_cases: Optional[str] = "N/A"

    try:
        log.info(f"M204_LLM_TASK_START: Analyzing procedure: {proc_name} from file: {original_filename_for_debug} (InputSourceID: {input_source_id_for_debug})")

        max_proc_content_len_for_prompt = 3500 
        truncated_proc_content = current_procedure_content
        if len(current_procedure_content) > max_proc_content_len_for_prompt:
            truncated_proc_content = current_procedure_content[:max_proc_content_len_for_prompt] + "\n... [CONTENT TRUNCATED] ..."
            log.warning(f"M204_LLM_TASK: Procedure {proc_name} content truncated for LLM prompt due to length > {max_proc_content_len_for_prompt}.")

        if not llm_config._llm:
            log.warning(f"M204_LLM_TASK: LLM not available. Skipping full LLM analysis for procedure {proc_name}.")
            return {
                "proc_name": proc_name, 
                "generated_summary": "LLM not available for analysis.",
                "suggested_cobol_name": proc_name, # Fallback
                "suggested_test_cases_json": None
            }

        # STEP 1: Identify M204 Concepts
        concept_identification_prompt_fstr = f"""
You are an M204 expert. Analyze the following M204 procedure content and identify the key M204 commands, keywords, programming constructs, or concepts that are most important for understanding its core functionality.
Focus on elements like file I/O (FIND, GET, STORE), looping (FOR EACH VALUE), screen interactions (IMAGE, SCREEN, FORM), variable usage (%, $), control flow (IF, ELSE, GO TO), and any specific M204 features used.

M204 Procedure Name: {proc_name}
M204 Procedure Parameters: {params_str or "None"}

M204 Procedure Content:
```m204
{truncated_proc_content}
```

Respond with a JSON object containing the following keys:
- "procedure_name": string (The original M204 procedure name: "{proc_name}")
- "identified_concepts": list of strings (Key M204 commands, keywords, or concepts found or inferred. Include parameter names if they are significant for understanding.)
- "brief_reasoning": string (A very brief explanation of why these concepts are relevant)
"""
        log.info(f"M204_LLM_TASK: Step 1: Identifying concepts for {proc_name}")
        concept_output_model: Optional[M204ConceptIdentificationOutput] = None
        try:
            concept_identifier_llm = llm_config._llm.as_structured_llm(M204ConceptIdentificationOutput)
            completion_response_concept = await concept_identifier_llm.acomplete(prompt=concept_identification_prompt_fstr)
            json_text_output_concept = completion_response_concept.text
            loaded_concept_data = json.loads(json_text_output_concept)
            concept_output_model = M204ConceptIdentificationOutput(**loaded_concept_data)
        except Exception as e_concept_llm:
            log.error(f"M204_LLM_TASK: Error during concept identification LLM call for {proc_name}: {e_concept_llm}. Raw output: '{json_text_output_concept}'", exc_info=True)
            # Continue without RAG if concept ID fails

        identified_concepts_for_rag: List[str] = []
        if concept_output_model and concept_output_model.identified_concepts:
            identified_concepts_for_rag = concept_output_model.identified_concepts
            log.info(f"M204_LLM_TASK: Identified concepts for {proc_name}: {identified_concepts_for_rag}. Reason: {concept_output_model.brief_reasoning}")
        
        concepts_and_params_for_rag = list(set(identified_concepts_for_rag + m204_param_names))


        # STEP 2: RAG Query
        if rag_service and concepts_and_params_for_rag:
            log.info(f"M204_LLM_TASK: Step 2: Performing RAG query for {proc_name} with concepts: {concepts_and_params_for_rag}")
            try:
                rag_context_for_summary = await rag_service.query_documentation_for_concepts(concepts_and_params_for_rag)
                log.info(f"M204_LLM_TASK: RAG context retrieved for {proc_name} (length: {len(rag_context_for_summary)}).")
            except Exception as e_rag:
                log.error(f"M204_LLM_TASK: Error during RAG query for {proc_name}: {e_rag}", exc_info=True)
                rag_context_for_summary = "Error retrieving RAG context. Proceeding with summary based on code only."
        elif not rag_service:
            log.warning(f"M204_LLM_TASK: RAG service not available for procedure {proc_name}. Skipping RAG query.")
            rag_context_for_summary = "RAG service not available. Summary will be based on code only."
        elif rag_service and not concepts_and_params_for_rag:            
            log.info(f"M204_LLM_TASK: No concepts identified or parameters present for RAG query for procedure {proc_name}. Skipping RAG query.")
            rag_context_for_summary = "No specific M204 concepts or parameters identified for RAG query. Summary will be based on code only."
        else: # Should not happen if rag_service is None or concepts_and_params_for_rag is empty
            log.warning(f"M204_LLM_TASK: RAG query skipped for {proc_name} due to unexpected condition.")


        # STEP 3: Summarize procedure
        summarization_prompt_fstr = f"""
You are an expert mainframe M204 and COBOL developer. Your task is to analyze the given M204 procedure.
Based on the procedure's code and the relevant documentation context, provide:
1.  A detailed summary of the M204 procedure: Explain its purpose, main logic, key operations (e.g., database interactions like FIND, GET, STORE; screen displays using FORMS, IMAGE; control flow like IF, ELSE, GO TO; looping constructs), inputs (parameters, %variables), and outputs (screen, files, %variables).
2.  A COBOL program name suggestion: Propose a suitable name if this M204 procedure were to be converted to a COBOL program. The name should be valid for COBOL (typically 8 characters, alphanumeric, starting with a letter, using hyphens not underscores) and distinct. If the original M204 name is suitable and valid, you can suggest it after transforming it to be COBOL-compliant (e.g., M204_PROC_NAME to M204-PCN).

M204 Procedure Name: {proc_name}
M204 Procedure Parameters: {params_str or "None"}

Relevant Documentation Context (based on concepts identified in the procedure):
{rag_context_for_summary}

M204 Procedure Content:
```m204
{truncated_proc_content}
```

Respond with a JSON object containing the following keys:
- "procedure_name": string (The original M204 procedure name: "{proc_name}")
- "cobol_name_suggestion": string (A suitable COBOL program name)
- "procedure_summary": string (A detailed summary of the M204 procedure)
"""
        summary_output_model: Optional[ProcedureAnalysisOutput] = None
        log.info(f"M204_LLM_TASK: Step 3: Generating summary and COBOL name for {proc_name} with RAG context.")
        try:
            summarizer_llm = llm_config._llm.as_structured_llm(ProcedureAnalysisOutput)
            completion_response_summary = await summarizer_llm.acomplete(prompt=summarization_prompt_fstr)
            json_text_output_summary = completion_response_summary.text
            loaded_summary_data = json.loads(json_text_output_summary)
            summary_output_model = ProcedureAnalysisOutput(**loaded_summary_data)
        except Exception as e_summary_llm:
            log.error(f"M204_LLM_TASK: Error during summarization LLM call for {proc_name}: {e_summary_llm}. Raw output: '{json_text_output_summary}'", exc_info=True)

        if summary_output_model:
            generated_summary = summary_output_model.procedure_summary
            suggested_cobol_name = summary_output_model.cobol_name_suggestion
            log.info(f"M204_LLM_TASK: Summary and COBOL name generated for {proc_name}: Summary len={len(generated_summary)}, COBOL Name='{suggested_cobol_name}'")
        
        # STEP 4: Generate Unit Test Cases
        log.info(f"M204_LLM_TASK: Step 4: Generating test cases for {proc_name}")
        summary_for_test_gen = generated_summary if generated_summary and "AI analysis could not be completed" not in generated_summary else "Summary was not available or failed to generate."
        
        test_case_generation_prompt_fstr = f"""
You are an expert M204 software tester and developer.
Based on the M204 procedure's code, its summary, and its parameters, generate a list of representative unit test cases.
For each test case, provide:
- "test_case_id": A unique identifier for the test case (e.g., TC_LOGIN_SUCCESS, TC_VALIDATE_INPUT_ERROR).
- "description": A brief description of the test scenario.
- "preconditions": A list of strings describing any necessary preconditions (e.g., specific data in a file, certain %variables set).
- "inputs": A dictionary of key-value pairs for input parameters and key %variables with their test values.
- "expected_outputs": A dictionary of key-value pairs for expected output %variables, screen elements, or file states and their values.
- "expected_behavior_description": A textual description of the expected behavior or outcome.

Focus on:
- Happy path scenarios.
- Edge cases (e.g., empty inputs, boundary values for numeric inputs if types are known/inferred).
- Error handling (e.g., invalid parameters, file I/O errors if inferable from commands like FIND with ON ERROR).
- Different logical branches if apparent from the code (e.g., IF/ELSE conditions) or summary.
- Consider the M204 parameters: {params_str or "None"}. If parameters are defined, include test cases that vary these parameters.

M204 Procedure Name: {proc_name}
M204 Procedure Parameters: {params_str or "None"}

Procedure Summary:
{summary_for_test_gen}

M204 Procedure Content:
```m204
{truncated_proc_content}
```

Respond with a JSON object structured according to the ProcedureTestCaseGenerationOutput model.
The main keys should be "procedure_name" (must be "{proc_name}") and "test_cases".
Each item in the "test_cases" list must follow the TestCase model structure:
  "test_case_id": string
  "description": string
  "preconditions": list of strings or null
  "inputs": object (dictionary)
  "expected_outputs": object (dictionary)
  "expected_behavior_description": string
If no specific test cases can be generated (e.g., procedure is too simple or abstract), return an empty list for "test_cases".
"""
        test_case_output_model: Optional[ProcedureTestCaseGenerationOutput] = None
        try:
            test_case_generator_llm = llm_config._llm.as_structured_llm(ProcedureTestCaseGenerationOutput)
            completion_response_test_cases = await test_case_generator_llm.acomplete(prompt=test_case_generation_prompt_fstr)
            json_text_output_test_cases = completion_response_test_cases.text
            loaded_test_case_data = json.loads(json_text_output_test_cases)
            test_case_output_model = ProcedureTestCaseGenerationOutput(**loaded_test_case_data)
            if test_case_output_model and test_case_output_model.test_cases:
                suggested_test_cases_json = [tc.model_dump() for tc in test_case_output_model.test_cases] # Store as list of dicts
                log.info(f"M204_LLM_TASK: Generated {len(suggested_test_cases_json)} test cases for {proc_name}.")
            else:
                log.info(f"M204_LLM_TASK: No test cases generated by LLM for {proc_name} or model parsing failed.")
                suggested_test_cases_json = [] # Ensure it's an empty list if no cases
        except Exception as e_test_case_llm:
            log.error(f"M204_LLM_TASK: Error during test case generation LLM call for {proc_name}: {e_test_case_llm}. Raw output: '{json_text_output_test_cases}'", exc_info=True)
            suggested_test_cases_json = None # Indicate failure

    except Exception as e_llm_main:
        log.error(f"M204_LLM_TASK: Main error during multi-step LLM analysis for procedure {proc_name}: {e_llm_main}", exc_info=True)
        # Fallback values are already set

    log.info(f"M204_LLM_TASK_END: Finished analyzing procedure: {proc_name} from file: {original_filename_for_debug}")
    return {
        "proc_name": proc_name, 
        "generated_summary": generated_summary,
        "suggested_cobol_name": suggested_cobol_name,
        "suggested_test_cases_json": suggested_test_cases_json
    }

async def _extract_and_store_m204_procedures(
    db: Session, input_source: InputSource, file_content: str, rag_service: Optional[RagService]
) -> List[Procedure]:
    log.info(f"M204_SERVICE: Extracting M204 procedures for file ID: {input_source.input_source_id} ({input_source.original_filename})")
    procedures_processed_for_db = [] # Stores Procedure ORM objects
    
    proc_pattern = re.compile(
        r"^\s*(?:(PUBLIC|PRIVATE)\s+)?(SUBROUTINE|PROCEDURE)\s+([A-Z0-9_#@$-]{1,32})(?:\s*\(([^)]*)\))?",
        re.IGNORECASE | re.MULTILINE
    )
    lines = file_content.splitlines()
    
    temp_procs_info = []
    for i, line_content_for_match in enumerate(lines):
        match = proc_pattern.match(line_content_for_match)
        if match:
            start_line_num = i + 1
            temp_procs_info.append({
                "match_obj": match,
                "start_line": start_line_num,
                "original_index": len(temp_procs_info)
            })

    llm_processing_tasks = []
    procedures_undergoing_llm_data = []
    procedures_not_undergoing_llm_data = []

    for proc_data_from_parse in temp_procs_info:
        match = proc_data_from_parse["match_obj"]
        start_line_num = proc_data_from_parse["start_line"]
        original_idx = proc_data_from_parse["original_index"]

        proc_visibility_keyword = match.group(1)
        proc_type_keyword = match.group(2).upper()
        proc_name = match.group(3)
        params_str = match.group(4) if match.group(4) else None

        actual_proc_type = proc_type_keyword
        if proc_visibility_keyword:
            actual_proc_type = f"{proc_visibility_keyword.upper()} {proc_type_keyword}"

        end_line_num: Optional[int] = None
        current_procedure_content: Optional[str] = None

        if original_idx + 1 < len(temp_procs_info):
            end_line_num = temp_procs_info[original_idx+1]["start_line"] - 1
        else:
            end_line_num = len(lines) 
        
        if start_line_num <= end_line_num:
            current_procedure_content = "\n".join(lines[start_line_num-1:end_line_num])
        else: 
            current_procedure_content = "" 
            log.warning(f"M204_SERVICE: Procedure {proc_name} has invalid line numbers: start={start_line_num}, end={end_line_num}")

        parsed_params_json = _parse_m204_parameters(params_str)
        m204_param_names_for_llm = [p['name'] for p in parsed_params_json['parameters']] if parsed_params_json and 'parameters' in parsed_params_json else []

        base_proc_db_data = {
            "proc_name": proc_name,
            "actual_proc_type": actual_proc_type,
            "params_str": params_str,
            "parsed_params_json": parsed_params_json,
            "start_line_num": start_line_num,
            "end_line_num": end_line_num,
            "current_procedure_content": current_procedure_content,
        }

        if current_procedure_content and llm_config._llm:
            llm_processing_tasks.append(
                _analyze_single_procedure_llm(
                    proc_name=proc_name,
                    params_str=params_str,
                    current_procedure_content=current_procedure_content,
                    m204_param_names=m204_param_names_for_llm,
                    input_source_id_for_debug=input_source.input_source_id,
                    original_filename_for_debug=input_source.original_filename,
                    rag_service=rag_service
                )
            )
            procedures_undergoing_llm_data.append(base_proc_db_data)
        else:
            base_proc_db_data["generated_summary"] = None
            base_proc_db_data["suggested_cobol_name"] = proc_name 
            base_proc_db_data["suggested_test_cases_json"] = None
            if not current_procedure_content:
                 log.warning(f"M204_SERVICE: Procedure {proc_name} has no content. Skipping LLM analysis.")
            elif not llm_config._llm:
                 log.info(f"M204_SERVICE: LLM not configured. Skipping LLM analysis for procedure {proc_name}.")
            procedures_not_undergoing_llm_data.append(base_proc_db_data)

    llm_results_list = []
    if llm_processing_tasks:
        log.info(f"M204_SERVICE: Executing {len(llm_processing_tasks)} LLM analysis tasks in parallel for file {input_source.original_filename}.")
        llm_results_list = await asyncio.gather(*llm_processing_tasks, return_exceptions=True)
        log.info(f"M204_SERVICE: Finished {len(llm_processing_tasks)} LLM analysis tasks for file {input_source.original_filename}.")

    all_proc_data_for_db_ops = []
    for i, llm_result_or_exc in enumerate(llm_results_list):
        proc_static_data = procedures_undergoing_llm_data[i]
        proc_name = proc_static_data["proc_name"]
        generated_summary, suggested_cobol_name, suggested_test_cases = None, proc_name, None

        if isinstance(llm_result_or_exc, Exception):
            log.error(f"M204_SERVICE: LLM analysis task for procedure {proc_name} failed: {llm_result_or_exc}", exc_info=llm_result_or_exc)
            generated_summary = f"Error during AI analysis for {proc_name}. Check logs."
        elif isinstance(llm_result_or_exc, dict) and llm_result_or_exc.get("proc_name") == proc_name:
            generated_summary = llm_result_or_exc.get("generated_summary")
            suggested_cobol_name = llm_result_or_exc.get("suggested_cobol_name", proc_name)
            suggested_test_cases = llm_result_or_exc.get("suggested_test_cases_json")
        else: 
            log.error(f"M204_SERVICE: Unexpected result or mismatched proc_name for {proc_name} from LLM task: {llm_result_or_exc}")
            generated_summary = f"Unexpected error or result format from AI analysis for {proc_name}."
        
        all_proc_data_for_db_ops.append({
            **proc_static_data, 
            "generated_summary": generated_summary, 
            "suggested_cobol_name": suggested_cobol_name,
            "suggested_test_cases_json": suggested_test_cases
        })
    
    all_proc_data_for_db_ops.extend(procedures_not_undergoing_llm_data)

    for proc_data in all_proc_data_for_db_ops:
        existing_proc = db.query(Procedure).filter_by(
            project_id=input_source.project_id,
            m204_proc_name=proc_data["proc_name"],
            input_source_id=input_source.input_source_id # Ensure it's for this specific file
        ).first()

        proc_schema_data = M204ProcedureCreateSchema(
            project_id=input_source.project_id,
            input_source_id=input_source.input_source_id,
            m204_proc_name=proc_data["proc_name"],
            m204_proc_type=proc_data["actual_proc_type"],
            m204_parameters_string=proc_data["params_str"],
            parsed_parameters_json=proc_data["parsed_params_json"],
            start_line_in_source=proc_data["start_line_num"],
            end_line_in_source=proc_data["end_line_num"],
            procedure_content=proc_data["current_procedure_content"],
            summary=proc_data["generated_summary"],
            target_cobol_program_name=proc_data["suggested_cobol_name"],
            suggested_test_cases_json=proc_data.get("suggested_test_cases_json")
        )

        if existing_proc:
            update_needed = False
            for key, value in proc_schema_data.model_dump(exclude_none=True).items():
                if getattr(existing_proc, key) != value:
                    setattr(existing_proc, key, value)
                    update_needed = True
            if update_needed:
                db.add(existing_proc)
                log.info(f"M204_SERVICE: Updating existing procedure '{proc_data['proc_name']}' (ID: {existing_proc.proc_id}) in file {input_source.original_filename}.")
            procedures_processed_for_db.append(existing_proc)
        else:
            # Check for duplicates by name in other files within the same project
            other_file_proc = db.query(Procedure).filter(
                Procedure.project_id == input_source.project_id,
                Procedure.m204_proc_name == proc_data["proc_name"],
                Procedure.input_source_id != input_source.input_source_id
            ).first()
            if other_file_proc:
                 log.warning(f"M204_SERVICE: Procedure '{proc_data['proc_name']}' in file {input_source.original_filename} (ID: {input_source.input_source_id}) has the same name as a procedure in another file (ID: {other_file_proc.input_source_id}, ProcID: {other_file_proc.proc_id}). This instance will be saved.")

            db_proc = Procedure(**proc_schema_data.model_dump(exclude_none=True))
            db.add(db_proc)
            procedures_processed_for_db.append(db_proc)
            log.info(f"M204_SERVICE: Created new procedure '{proc_data['proc_name']}' from file {input_source.original_filename}.")
    
    log.info(f"M204_SERVICE: Finished extracting and queueing for DB {len(procedures_processed_for_db)} M204 procedures for file ID: {input_source.input_source_id}.")
    return procedures_processed_for_db



async def _extract_and_store_m204_datasets(
    db: Session, input_source: InputSource, file_content: str
) -> List[M204File]:
    log.info(f"M204_SERVICE: Extracting M204 DEFINE DATASET statements for file ID: {input_source.input_source_id} ({input_source.original_filename}) - (Aligned with old service logic)")
    defined_m204_files = []
    # Regex to capture logical name and the rest of the parameters string from the first line
    define_dataset_initial_pattern = re.compile(
        r"^\s*DEFINE\s+DATASET\s+([A-Z0-9_#@$-.]+)\s*(.*)",
        re.IGNORECASE
    )
    # Regex to capture logical name and parameters from a potentially combined multi-line statement
    define_dataset_full_pattern = re.compile(
        r"^\s*DEFINE\s+DATASET\s+([A-Z0-9_#@$-.]+)\s*(.*)",
        re.IGNORECASE | re.DOTALL # DOTALL might be needed if newlines are in the combined string
    )
    lines = file_content.splitlines()
    
    # Regex to find DDNAME=value, handling quotes or no quotes
    ddname_value_capture_pattern = r"'([A-Z0-9_#@$-]{1,8})'|\"([A-Z0-9_#@$-]{1,8})\"|([A-Z0-9_#@$-]{1,8})"
    ddname_search_regex = rf"DDNAME\s*=\s*(?:{ddname_value_capture_pattern})"

    i = 0
    while i < len(lines):
        current_line_original = lines[i]
        current_line_stripped = current_line_original.strip()
        initial_match = define_dataset_initial_pattern.match(current_line_stripped)

        if initial_match:
            m204_logical_name_from_first_line = initial_match.group(1) 
            # params_on_first_line = initial_match.group(2) # Not directly used like this in old logic for final attributes
            start_line_num = i + 1 # 1-based

            statement_lines_for_db_attributes = [current_line_original] # Store original lines for m204_attributes
            stripped_statement_parts_for_param_parsing = [current_line_stripped] # Store stripped lines for DDNAME parsing

            current_physical_line_idx = i # Tracks the end of the statement
            # Continuation logic from old service (hyphen based, skip comments)
            while lines[current_physical_line_idx].strip().endswith('-') and \
                  current_physical_line_idx + 1 < len(lines):
                current_physical_line_idx += 1
                next_line_original = lines[current_physical_line_idx]
                next_line_stripped = next_line_original.strip()

                if next_line_stripped.startswith('*'): # Skip comment lines
                    current_physical_line_idx -= 1 # Adjust index back
                    break 
                statement_lines_for_db_attributes.append(next_line_original)
                stripped_statement_parts_for_param_parsing.append(next_line_stripped)
            
            end_line_num = current_physical_line_idx + 1
            
            # This is what old service stored as m204_attributes
            final_m204_attributes_for_db = "\n".join(statement_lines_for_db_attributes)
            # This is what old service used for parsing DDNAME and logical name from the full statement
            full_statement_for_param_parsing = " ".join(stripped_statement_parts_for_param_parsing) 
            
            # Re-parse the combined statement to get the definitive logical name and params string (as per old service)
            final_parse_match = define_dataset_full_pattern.match(full_statement_for_param_parsing)
            
            m204_authoritative_logical_name: str
            params_str_for_ddname_search: str

            if final_parse_match:
                m204_authoritative_logical_name = final_parse_match.group(1).strip().upper()
                params_str_for_ddname_search = final_parse_match.group(2) or ""
            else:
                # Fallback if re-parsing fails (should ideally not happen if initial match was good)
                log.warning(f"M204_SERVICE: Could not re-parse combined multi-line DEFINE DATASET statement starting at line {start_line_num} in {input_source.original_filename}. Using first line data. Statement: '{full_statement_for_param_parsing[:200]}'")
                m204_authoritative_logical_name = m204_logical_name_from_first_line.strip().upper()
                params_str_for_ddname_search = initial_match.group(2) or ""
            
            # Extract DDNAME (m204_file_name) from the params_str_for_ddname_search
            extracted_ddname_value = None
            ddname_match = re.search(ddname_search_regex, params_str_for_ddname_search, re.IGNORECASE)
            if ddname_match:
                for group_val in ddname_match.groups():
                    if group_val:
                        extracted_ddname_value = group_val.strip().upper()
                        break
            
            file_key_for_m204_file_name: str
            if extracted_ddname_value:
                file_key_for_m204_file_name = extracted_ddname_value
            else:
                file_key_for_m204_file_name = m204_authoritative_logical_name # Fallback to logical name for m204_file_name
                log.warning(f"M204_SERVICE: DEFINE DATASET for '{m204_authoritative_logical_name}' at line {start_line_num} in {input_source.original_filename} does not specify a DDNAME. Using M204 logical name '{m204_authoritative_logical_name}' as m204_file_name.")

            log.info(f"M204_SERVICE: Processing DEFINE DATASET: Logical Name='{m204_authoritative_logical_name}', Extracted DDNAME='{extracted_ddname_value}', Effective m204_file_name='{file_key_for_m204_file_name}', Full Attributes='{final_m204_attributes_for_db[:100].replace(chr(10), ' ')}...' at lines {start_line_num}-{end_line_num}")

            existing_m204_file = db.query(M204File).filter(
                M204File.project_id == input_source.project_id,
                func.upper(M204File.m204_file_name) == file_key_for_m204_file_name 
            ).first()

            file_data_dict = {
                "project_id": input_source.project_id,
                "defined_in_input_source_id": input_source.input_source_id,
                "m204_file_name": file_key_for_m204_file_name,
                "m204_logical_dataset_name": m204_authoritative_logical_name,
                "m204_attributes": final_m204_attributes_for_db, # Store full original statement
                "definition_line_number_start": start_line_num,
                "definition_line_number_end": end_line_num,
                "is_db_file": None # Initialize as None, like old service
            }
            
            # Convert to schema for validation and consistent data structure
            # This assumes M204FileCreateSchema can handle all these fields.
            try:
                file_schema_for_update_check = M204FileCreateSchema(**file_data_dict)
            except Exception as e_schema:
                log.error(f"M204_SERVICE: Error creating schema for M204File '{file_key_for_m204_file_name}': {e_schema}", exc_info=True)
                i = end_line_num # Advance i past the processed lines
                continue


            if existing_m204_file:
                update_needed = False
                # Compare specific fields as old service might have, or use generic comparison
                # For closer old service behavior, compare specific fields it cared about:
                if existing_m204_file.defined_in_input_source_id != file_schema_for_update_check.defined_in_input_source_id:
                    existing_m204_file.defined_in_input_source_id = file_schema_for_update_check.defined_in_input_source_id
                    update_needed = True
                if existing_m204_file.definition_line_number_start != file_schema_for_update_check.definition_line_number_start: # old: defined_at_line
                    existing_m204_file.definition_line_number_start = file_schema_for_update_check.definition_line_number_start
                    update_needed = True
                if existing_m204_file.definition_line_number_end != file_schema_for_update_check.definition_line_number_end:
                    existing_m204_file.definition_line_number_end = file_schema_for_update_check.definition_line_number_end
                    update_needed = True
                if existing_m204_file.m204_attributes != file_schema_for_update_check.m204_attributes:
                    existing_m204_file.m204_attributes = file_schema_for_update_check.m204_attributes
                    update_needed = True
                if existing_m204_file.m204_logical_dataset_name != file_schema_for_update_check.m204_logical_dataset_name:
                    existing_m204_file.m204_logical_dataset_name = file_schema_for_update_check.m204_logical_dataset_name
                    update_needed = True
                # is_db_file is not typically set by DEFINE DATASET in old logic, but if it was set by JCL/PARMLIB, don't revert to None unless explicitly intended.
                # The new schema sets it to None here, so if existing_m204_file.is_db_file was True/False, it would be an update.
                # To be safe and like old, only update if the new value (None) is different and old wasn't None.
                # However, the schema default is None, so this comparison is fine.
                if existing_m204_file.is_db_file != file_schema_for_update_check.is_db_file: # is_db_file is None in schema
                    existing_m204_file.is_db_file = file_schema_for_update_check.is_db_file
                    update_needed = True


                if update_needed:
                    db.add(existing_m204_file)
                    log.info(f"M204_SERVICE: Updating existing M204File '{file_key_for_m204_file_name}' (ID: {existing_m204_file.m204_file_id}) based on DEFINE DATASET in {input_source.original_filename}.")
                defined_m204_files.append(existing_m204_file)
            else:
                try:
                    # Use the schema that was already created and validated
                    db_m204_file = M204File(**file_schema_for_update_check.model_dump(exclude_none=True))
                    db.add(db_m204_file)
                    defined_m204_files.append(db_m204_file)
                    log.info(f"M204_SERVICE: Created new M204File '{file_key_for_m204_file_name}' (Logical: '{m204_authoritative_logical_name}') from DEFINE DATASET in {input_source.original_filename}.")
                except Exception as e_create:
                    log.error(f"M204_SERVICE: Error creating M204File '{file_key_for_m204_file_name}': {e_create}", exc_info=True)

            i = end_line_num # Advance i past the processed lines (end_line_num is 1-based index of last line)
            continue # Ensure we skip the i += 1 at the end of the loop for this iteration
        
        i += 1 
    return defined_m204_files

async def _extract_and_store_m204_variables(
    db: Session, input_source: InputSource, file_content: str, procedures_in_file: List[Procedure], rag_service: Optional[RagService]
) -> List[M204Variable]:
    log.info(f"M204_SERVICE: Extracting M204 variables for file ID: {input_source.input_source_id}")
    variables_created = []
    lines = file_content.splitlines()
    
    percent_var_pattern = re.compile(r"(%[A-Z0-9_#@$-]+)", re.IGNORECASE)
    dollar_var_pattern = re.compile(r"(\$[A-Z0-9_#@$-]+)", re.IGNORECASE)
    define_var_pattern = re.compile(
        r"^\s*DEFINE\s+"
        r"(?:(PUBLIC|PRIVATE)\s+)?\s*"
        r"(%[A-Z0-9_#@$-]+)\s*" # Variable name must start with %
        r"(?:\(([^)]*)\))?"      # Optional attributes in parentheses
        r"(?:\s+(PUBLIC|PRIVATE))?", # Optional PUBLIC/PRIVATE keyword at the end
        re.IGNORECASE | re.MULTILINE
    )
    
    found_var_details: Dict[str, Dict[str, Any]] = {} 
    
    proc_line_map = {proc.start_line_in_source: proc.proc_id for proc in procedures_in_file if proc.start_line_in_source and proc.proc_id is not None}
    proc_id_to_name_map = {p.proc_id: p.m204_proc_name for p in procedures_in_file if p.proc_id}
    current_proc_id_for_line: Optional[int] = None
    sorted_proc_starts = sorted(proc_line_map.keys())

    for i, line_content in enumerate(lines):
        line_num = i + 1
        
        temp_proc_id_for_line: Optional[int] = None
        for start_line in sorted_proc_starts:
            if line_num >= start_line:
                temp_proc_id_for_line = proc_line_map[start_line]
            else:
                break 
        current_proc_id_for_line = temp_proc_id_for_line

        for match in define_var_pattern.finditer(line_content):
            visibility_keyword1 = match.group(1)
            var_name = match.group(2)
            attributes_str = match.group(3)
            visibility_keyword2 = match.group(4)
            
            scope = "LOCAL" 
            if visibility_keyword1:
                scope = visibility_keyword1.upper()
            elif visibility_keyword2:
                scope = visibility_keyword2.upper()

            parsed_attrs = _parse_m204_parameters(attributes_str) 
            
            proc_context_key = f"_proc_{current_proc_id_for_line}" if current_proc_id_for_line else "_file_"
            var_key = f"{var_name}_{scope}{proc_context_key}_{line_num}" 
            
            found_var_details[var_key] = {
                "name": var_name, "type": "PERCENT", "scope": scope, 
                "proc_id": current_proc_id_for_line, "line": line_num, 
                "attributes_text": attributes_str, "attributes_json": parsed_attrs,
                "is_defined": True
            }
        
        for match in percent_var_pattern.finditer(line_content):
            var_name = match.group(1)
            scope = "LOCAL" 
            if current_proc_id_for_line is None:
                scope = "GLOBAL" 
            
            proc_context_key = f"_proc_{current_proc_id_for_line}" if current_proc_id_for_line else "_file_"
            var_key = f"{var_name}_{scope}{proc_context_key}" 
            
            if var_key not in found_var_details or (found_var_details[var_key]["line"] > line_num and not found_var_details[var_key]["is_defined"]):
                found_var_details[var_key] = {
                    "name": var_name, "type": "PERCENT", "scope": scope, 
                    "proc_id": current_proc_id_for_line, "line": line_num, 
                    "attributes_text": None, "attributes_json": None,
                    "is_defined": False
                }
        
        for match in dollar_var_pattern.finditer(line_content):
            var_name = match.group(1)
            scope = "SYSTEM_OR_USER_DEFINED_DOLLAR" 
            proc_context_key = f"_proc_{current_proc_id_for_line}" if current_proc_id_for_line else "_file_"
            var_key = f"{var_name}_{scope}{proc_context_key}"

            if var_key not in found_var_details or (found_var_details[var_key]["line"] > line_num and not found_var_details[var_key]["is_defined"]):
                found_var_details[var_key] = {
                    "name": var_name, "type": "DOLLAR", "scope": scope, 
                    "proc_id": current_proc_id_for_line, "line": line_num, 
                    "attributes_text": None, "attributes_json": None,
                    "is_defined": False 
                }
                
    for details in found_var_details.values():
        suggested_cobol_name_for_var: Optional[str] = None
        
        if llm_config._llm and details["type"] == "PERCENT": 
            variable_name_for_llm = details["name"]
            variable_type_for_llm = details["type"]
            attributes_for_llm = details["attributes_text"]
            proc_name_context_for_llm = proc_id_to_name_map.get(details["proc_id"]) if details["proc_id"] else "File Level"

            var_prompt_fstr = f"""
You are an M204 to COBOL migration expert. Suggest a COBOL-compliant variable name for the given M204 variable.
M204 Variable Name: {variable_name_for_llm}
M204 Variable Type: {variable_type_for_llm}
M204 Attributes (if any): {attributes_for_llm or "None"}
Context (Procedure or File Level): {proc_name_context_for_llm}

Respond with a JSON object containing "m204_variable_name", "suggested_cobol_variable_name", and "reasoning".
The COBOL name should be max 30 chars, alphanumeric, use hyphens, and avoid M204-specific symbols like '%'.
"""
            try:
                var_namer_llm = llm_config._llm.as_structured_llm(M204VariableToCobolOutput)
                completion_response_var = await var_namer_llm.acomplete(prompt=var_prompt_fstr)
                loaded_var_data = json.loads(completion_response_var.text)
                var_output_model = M204VariableToCobolOutput(**loaded_var_data)
                if var_output_model.m204_variable_name == variable_name_for_llm:
                    suggested_cobol_name_for_var = var_output_model.suggested_cobol_variable_name
            except Exception as e_var_llm:
                log.error(f"M204_SERVICE: Error during LLM COBOL name suggestion for variable {variable_name_for_llm}: {e_var_llm}", exc_info=True)
        
        existing_var_query = db.query(M204Variable).filter_by(
            project_id=input_source.project_id, 
            input_source_id=input_source.input_source_id,
            variable_name=details["name"], 
            scope=details["scope"],
            definition_line_number=details["line"] 
        )
        if details["proc_id"] is not None:
            existing_var_query = existing_var_query.filter(M204Variable.procedure_id == details["proc_id"])
        else:
            existing_var_query = existing_var_query.filter(M204Variable.procedure_id.is_(None))
        
        existing_var = existing_var_query.first()

        # This dictionary contains all fields that M204VariableCreateSchema would expect,
        # matching the old service's logic and ORM model structure.
        var_data_for_schema_and_orm = {
            "project_id": input_source.project_id,
            "input_source_id": input_source.input_source_id,
            "procedure_id": details["proc_id"],
            "variable_name": details["name"],
            "variable_type": details["type"],
            "scope": details["scope"],
            "definition_line_number": details["line"], 
            "attributes_text": details["attributes_text"],
            "attributes_json": details["attributes_json"],
            "is_explicitly_defined": details["is_defined"],
            "suggested_cobol_name": suggested_cobol_name_for_var
        }

        if existing_var:
            updated = False
            for key, value in var_data_for_schema_and_orm.items():
                if getattr(existing_var, key) != value:
                    setattr(existing_var, key, value)
                    updated = True
            if updated:
                db.add(existing_var)
                log.debug(f"M204_SERVICE: Updating existing M204Variable '{details['name']}' (ID: {existing_var.m204_variable_id}).")
            variables_created.append(existing_var)
        else:
            try:
                # Use M204VariableCreateSchema as in the old service
                var_create_data = M204VariableCreateSchema(**var_data_for_schema_and_orm)
                db_variable = M204Variable(**var_create_data.model_dump(exclude_none=True))
                db.add(db_variable)
                variables_created.append(db_variable)
                log.debug(f"M204_SERVICE: Created new M204Variable '{details['name']}' using schema.")
            except Exception as e_create:
                log.error(f"M204_SERVICE: Error creating M204Variable '{details['name']}': {e_create}", exc_info=True)
            
    return variables_created


async def _extract_and_store_m204_procedure_calls(
    db: Session, input_source: InputSource, file_content: str, defined_procedures_in_file: List[Procedure]
) -> List[ProcedureCall]:
    log.info(f"M204_SERVICE: Extracting M204 procedure calls for file ID: {input_source.input_source_id} using old service logic.")
    procedure_calls_created = []
    lines = file_content.splitlines()
    # Regex from old service: does not capture arguments string
    call_pattern = re.compile(r"^\s*CALL\s+([A-Z0-9_#@$-]+)(?:\s*\(.*?\))?", re.IGNORECASE | re.MULTILINE)
    
    proc_line_map = {proc.start_line_in_source: proc.proc_id for proc in defined_procedures_in_file if proc.start_line_in_source and proc.proc_id is not None}
    current_calling_proc_id_for_line: Optional[int] = None
    sorted_proc_starts = sorted(proc_line_map.keys())

    for i, line_content in enumerate(lines): # Iterate with original line content for regex
        line_num = i + 1
        
        # Determine current calling procedure ID based on line number
        temp_calling_proc_id: Optional[int] = None
        for start_line in sorted_proc_starts:
            # Find the procedure object corresponding to this start_line
            # This logic is slightly adapted from the old service to ensure proc_obj is correctly fetched for boundary checks
            proc_obj_for_scope = next((p for p in defined_procedures_in_file if p.start_line_in_source == start_line and p.proc_id == proc_line_map.get(start_line)), None)

            if proc_obj_for_scope and proc_obj_for_scope.end_line_in_source and \
               proc_obj_for_scope.start_line_in_source <= line_num <= proc_obj_for_scope.end_line_in_source:
                temp_calling_proc_id = proc_line_map[start_line]
                break 
            elif proc_obj_for_scope and not proc_obj_for_scope.end_line_in_source and \
                 proc_obj_for_scope.start_line_in_source <= line_num: # If no end line, assume it continues
                temp_calling_proc_id = proc_line_map[start_line]
                # Don't break, might be a nested proc definition without clear end, though less common for calls
            elif line_num < start_line: # Optimization: if current line is before next proc start
                break
        current_calling_proc_id_for_line = temp_calling_proc_id

        match = call_pattern.match(line_content) # Match on the original line_content as in old service
        if match:
            called_proc_name = match.group(1)
            
            # Check for existing call using 'line_number'
            existing_call = db.query(ProcedureCall).filter_by(
                project_id=input_source.project_id,
                calling_input_source_id=input_source.input_source_id,
                calling_procedure_id=current_calling_proc_id_for_line,
                called_procedure_name=called_proc_name,
                line_number=line_num  # Field name is line_number
            ).first()

            if existing_call:
                # If it exists, check if argument fields need clearing if model/schema changed
                # Assuming the model will be updated to not have these fields, or they'll be nullable
                update_existing = False
                if hasattr(existing_call, 'call_arguments_string') and existing_call.call_arguments_string is not None:
                    existing_call.call_arguments_string = None
                    update_existing = True
                if hasattr(existing_call, 'parsed_arguments_json') and existing_call.parsed_arguments_json is not None:
                    existing_call.parsed_arguments_json = None
                    update_existing = True
                if hasattr(existing_call, 'is_external') and existing_call.is_external is True: # Old schema default was None
                    existing_call.is_external = None # Will be set by _resolve_procedure_calls
                    update_existing = True

                if update_existing:
                    db.add(existing_call)
                    log.debug(f"M204_SERVICE: Clearing argument/external fields for existing call to '{called_proc_name}' at line {line_num} to align with old logic.")
                procedure_calls_created.append(existing_call)
                continue

            try:
                # Create schema instance as per old service (line_number, is_external=None, no arguments)
                # This assumes M204ProcedureCallCreateSchema is updated to match this
                call_data = M204ProcedureCallCreateSchema(
                    project_id=input_source.project_id,
                    calling_input_source_id=input_source.input_source_id,
                    calling_procedure_id=current_calling_proc_id_for_line,
                    called_procedure_name=called_proc_name,
                    line_number=line_num, # Field name is line_number
                    is_external=None      # As per old service's schema instantiation
                    # No call_arguments_string or parsed_arguments_json passed
                )
                # Instantiate model from schema dump
                # Ensure your ProcedureCall model matches the fields in the (modified) schema
                db_call = ProcedureCall(**call_data.model_dump(exclude_none=True)) 
                db.add(db_call)
                procedure_calls_created.append(db_call)
                log.debug(f"M204_SERVICE: Created new procedure call to '{called_proc_name}' at line {line_num} using old logic.")
            except Exception as e:
                log.error(f"M204_SERVICE: Error creating DB entry for procedure call to '{called_proc_name}' at line {line_num}: {e}", exc_info=True)
    
    log.info(f"M204_SERVICE: Finished extracting {len(procedure_calls_created)} procedure calls for file ID {input_source.input_source_id} using old logic.")
    return procedure_calls_created


async def _resolve_procedure_calls(db: Session, project_id: int, calls_in_file: List[ProcedureCall], procs_in_file: List[Procedure]):
    log.info(f"M204_SERVICE: Resolving internal/external status for {len(calls_in_file)} procedure calls in project {project_id}.")
    # Map of procedure names defined within the current file being processed
    proc_names_in_current_file = {p.m204_proc_name: p.proc_id for p in procs_in_file if p.proc_id is not None}
    
    for call in calls_in_file:
        original_is_external = call.is_external
        original_resolved_id = call.resolved_procedure_id
        update_call_needed = False

        # Check if called procedure is in the current file
        if call.called_procedure_name in proc_names_in_current_file:
            call.is_external = False
            call.resolved_procedure_id = proc_names_in_current_file[call.called_procedure_name]
            log.debug(f"M204_SERVICE: Call to '{call.called_procedure_name}' resolved internally to ProcID {call.resolved_procedure_id} within the same file.")
        else:
            # Check if called procedure exists anywhere else in the project
            external_proc = db.query(Procedure.proc_id).filter(
                Procedure.project_id == project_id,
                Procedure.m204_proc_name == call.called_procedure_name,
                Procedure.input_source_id != call.calling_input_source_id # Ensure it's not the current file again
            ).first()
            
            if external_proc:
                call.is_external = True # It's external to this file, but exists in project
                call.resolved_procedure_id = external_proc.proc_id
                log.debug(f"M204_SERVICE: Call to '{call.called_procedure_name}' resolved externally to ProcID {call.resolved_procedure_id} in another file within project.")
            else:
                call.is_external = True # Truly external or unresolved within project
                call.resolved_procedure_id = None
                log.debug(f"M204_SERVICE: Call to '{call.called_procedure_name}' is external and not found in other project files. Marked as unresolved.")
        
        if call.is_external != original_is_external or call.resolved_procedure_id != original_resolved_id:
            update_call_needed = True
        
        if update_call_needed:
            db.add(call)


async def _extract_and_store_image_statements(
    db: Session, input_source: InputSource, file_content: str
) -> List[ImageStatement]:
    log.info(f"M204_SERVICE: Extracting IMAGE statements for file ID: {input_source.input_source_id} ({input_source.original_filename}) using field names aligned with old service logic.")
    image_statements_created = []
    image_start_pattern = re.compile(r"^\s*IMAGE(?:\s+([A-Z0-9_#@$-.]+))?.*", re.IGNORECASE)
    end_image_pattern = re.compile(r"^\s*END\s+IMAGE\s*$", re.IGNORECASE)

    lines = file_content.splitlines()
    current_line_idx = 0
    while current_line_idx < len(lines):
        line_content_on_start_line = lines[current_line_idx]
        # 'line_number_val' corresponds to 'line_number' in the old model/schema
        line_number_val = current_line_idx + 1 

        match = image_start_pattern.match(line_content_on_start_line.strip())
        if match:
            # 'referenced_m204_logical_name_val' corresponds to 'referenced_m204_logical_name'
            referenced_m204_logical_name_val = match.group(1).strip() if match.group(1) else None
            
            image_content_lines = [line_content_on_start_line] 
            
            # Determine the end of the IMAGE block
            image_block_end_idx = current_line_idx
            for j in range(current_line_idx + 1, len(lines)):
                image_content_lines.append(lines[j])
                image_block_end_idx = j
                if end_image_pattern.match(lines[j].strip()):
                    break 
            # else: # No END IMAGE found
                # log.warning(f"M204_SERVICE: IMAGE statement starting at line {line_number_val} in {input_source.original_filename} does not have a corresponding END IMAGE. Processing up to current EOF of block.")

            # 'image_content_val' corresponds to 'image_content'
            image_content_val = "\n".join(image_content_lines)

            # Query ImageStatement using 'line_number' as the key, like in the old service
            existing_image = db.query(ImageStatement).filter_by(
                project_id=input_source.project_id,
                input_source_id=input_source.input_source_id,
                line_number=line_number_val  # Assuming ImageStatement model has 'line_number'
            ).first()
            
            if existing_image:
                changed = False
                # Update using model field names expected by the old service logic
                if existing_image.referenced_m204_logical_name != referenced_m204_logical_name_val:
                    existing_image.referenced_m204_logical_name = referenced_m204_logical_name_val
                    changed = True
                
                if existing_image.image_content != image_content_val:
                    existing_image.image_content = image_content_val
                    changed = True
                
                # Note: The old service logic for ImageStatement did not involve 'end_line_number' directly on the model.
                # If the current model has an 'end_line_number' field, it's not being updated here to strictly follow old logic.

                if changed:
                    db.add(existing_image)
                    log.info(f"M204_SERVICE: Updated IMAGE statement (ID: {existing_image.image_statement_id}) at line {line_number_val} in file {input_source.original_filename}.")
                image_statements_created.append(existing_image)
            else:
                try:
                    # Create using ImageStatementCreateSchema.
                    # This assumes the schema is defined with fields:
                    # line_number, image_content, referenced_m204_logical_name
                    image_data_for_create = ImageStatementCreateSchema(
                        project_id=input_source.project_id,
                        input_source_id=input_source.input_source_id,
                        line_number=line_number_val,
                        image_content=image_content_val,
                        referenced_m204_logical_name=referenced_m204_logical_name_val
                        # No end_line_number here, to match old service's direct interaction pattern
                    )
                    # Create model instance from schema data
                    db_image = ImageStatement(**image_data_for_create.model_dump(exclude_none=True if referenced_m204_logical_name_val is None else False))
                    db.add(db_image)
                    image_statements_created.append(db_image)
                    log.info(f"M204_SERVICE: Created new IMAGE statement at line {line_number_val} in file {input_source.original_filename}.")
                except Exception as e_create: 
                    log.error(f"M204_SERVICE: Error creating ImageStatement DB entry at line {line_number_val} for {input_source.original_filename}: {e_create}", exc_info=True)
            
            current_line_idx = image_block_end_idx + 1 # Move main loop past this IMAGE block
        else:
            current_line_idx += 1 # Not an IMAGE start, move to next line
            
    return image_statements_created

async def process_m204_analysis(
    db: Session, 
    input_source: InputSource, 
    file_content: str, 
    rag_service: Optional[RagService]
) -> Tuple[M204AnalysisResultDataSchema, List[M204File]]:
    """
    Main function to process M204 source file content (procedures, datasets, vars, calls, images).
    Returns the analysis results and a list of M204File objects identified as DB files.
    """
    log.info(f"M204_SERVICE: Starting M204 specific processing for file: {input_source.original_filename} (ID: {input_source.input_source_id})")
    
    # These functions add objects to the session (db.add)
    extracted_procedures = await _extract_and_store_m204_procedures(db, input_source, file_content, rag_service)
    extracted_m204_datasets = await _extract_and_store_m204_datasets(db, input_source, file_content) # These are M204File objects
    extracted_variables = await _extract_and_store_m204_variables(db, input_source, file_content, extracted_procedures, rag_service)
    extracted_procedure_calls = await _extract_and_store_m204_procedure_calls(db, input_source, file_content, extracted_procedures)
    extracted_image_statements = await _extract_and_store_image_statements(db, input_source, file_content)
    
    # Resolve calls after all procedures in the file are known
    await _resolve_procedure_calls(db, input_source.project_id, extracted_procedure_calls, extracted_procedures)

    # The main orchestrator (analysis_service.perform_source_file_analysis) will handle the commit.
    # Refresh objects to ensure response models get the latest data from the session.
    
    refreshed_items = []
    all_extracted_orm_objects = extracted_procedures + extracted_m204_datasets + \
                                extracted_variables + extracted_procedure_calls + extracted_image_statements
    for item in all_extracted_orm_objects:
        try:
            if item in db.new or item in db.dirty:
                db.flush() # Assign IDs if new, or ensure changes are in session
            db.refresh(item)
            refreshed_items.append(item)
        except Exception as e_refresh:
            log.warning(f"M204_SERVICE: Could not refresh item {type(item).__name__} (ID: {getattr(item, 'id', 'N/A')}) during M204 processing: {e_refresh}. Using potentially uncommitted data for response.")
            refreshed_items.append(item) # Add it anyway

    # Re-populate lists from refreshed items
    refreshed_procedures = [p for p in refreshed_items if isinstance(p, Procedure)]
    refreshed_m204_datasets = [f for f in refreshed_items if isinstance(f, M204File)] # These are M204File objects
    refreshed_variables = [v for v in refreshed_items if isinstance(v, M204Variable)]
    refreshed_procedure_calls = [pc for pc in refreshed_items if isinstance(pc, ProcedureCall)]
    refreshed_image_statements = [img for img in refreshed_items if isinstance(img, ImageStatement)]

    procedure_responses = [M204ProcedureResponseSchema.model_validate(p) for p in refreshed_procedures]
    m204_dataset_responses = [M204FileResponseSchema.model_validate(df) for df in refreshed_m204_datasets]
    variable_responses = [M204VariableResponseSchema.model_validate(v) for v in refreshed_variables]
    procedure_call_responses = [M204ProcedureCallResponseSchema.model_validate(pc) for pc in refreshed_procedure_calls]
    image_statement_responses = [ImageStatementResponseSchema.model_validate(img) for img in refreshed_image_statements]
    
    # Identify M204Files that are DB files from this analysis (e.g., from DEFINE DATASET)
    # These are candidates for VSAM enhancement by the orchestrator.
    m204_db_files_identified = [mf for mf in refreshed_m204_datasets if mf.is_db_file]

    analysis_result_data = M204AnalysisResultDataSchema(
        procedures_found=procedure_responses,
        defined_files_found=m204_dataset_responses, 
        defined_fields_found=[], # Fields are typically from PARMLIB, not M204 source directly
        variables_found=variable_responses,
        procedure_calls_found=procedure_call_responses,
        image_statements_found=image_statement_responses
    )
    log.info(f"M204_SERVICE: Completed M204 analysis for file: {input_source.original_filename}. Identified {len(m204_db_files_identified)} DB files.")
    return analysis_result_data, m204_db_files_identified


async def enhance_m204_db_file_with_vsam_suggestions(db: Session, m204_file: M204File):
    """
    Public function to trigger LLM-based VSAM enhancement for a single M204File.
    This is intended to be called by the main analysis orchestrator.
    Updates the M204File and its M204Fields in the session.
    """
    if not llm_config._llm:
        log.warning(f"M204_VSAM_ENHANCE: LLM not available. Skipping VSAM enhancement for M204 file: {m204_file.m204_file_name}")
        return
    
    if not m204_file.is_db_file:
        log.debug(f"M204_VSAM_ENHANCE: M204 file {m204_file.m204_file_name} (ID: {m204_file.m204_file_id}) is not a DB file. Skipping VSAM enhancement.")
        return

    log.info(f"M204_VSAM_ENHANCE: Attempting LLM-based VSAM enhancement for M204 DB file: {m204_file.m204_file_name} (ID: {m204_file.m204_file_id})")
    
    # Ensure fields are loaded for the context
    # If m204_file.fields is accessed and the session is not flushed, it might not pick up newly created fields
    # from PARMLIB analysis if that happened in the same transaction without intermediate flush/commit.
    # The orchestrator should manage transaction boundaries. Assuming fields are available or loaded.
    db.refresh(m204_file) # Refresh to get latest state, including potentially loaded relationships
    if m204_file.fields: # Access fields after refresh
        log.debug(f"M204_VSAM_ENHANCE: File {m204_file.m204_file_name} has {len(m204_file.fields)} fields for VSAM context.")
        for fld in m204_file.fields: # Ensure fields themselves are refreshed if they were modified
            db.refresh(fld)


    file_attributes = m204_file.m204_attributes or "Not defined via DEFINE DATASET."
    
    fields_context_list = []
    if m204_file.fields:
        for field in m204_file.fields:
            fields_context_list.append({
                "name": field.field_name,
                "attributes_text": field.attributes_text or "No attributes text.",
                "attributes_json": field.attributes_json or {},
                "current_is_key_component": field.is_primary_key_component,
                "current_key_order": field.target_vsam_key_order,
                "current_vsam_data_type": field.target_vsam_data_type,
                "current_vsam_length": field.target_vsam_length
            })
    
    fields_context_str = json.dumps(fields_context_list, indent=2) if fields_context_list else "No fields defined or found for this file (e.g., via PARMLIB)."
    
    prompt_fstr = f"""
You are an expert Mainframe M204 to COBOL/VSAM migration specialist.
Analyze the following M204 file information, which has been identified as an M204 database file.
Your goal is to suggest its VSAM organization and refine key structure and field attributes for VSAM.

M204 File Name (DDNAME): {m204_file.m204_file_name}
M204 Logical Dataset Name (if from DEFINE DATASET): {m204_file.m204_logical_dataset_name or "N/A"}
M204 File 'DEFINE DATASET' Attributes (if available):
{file_attributes}

Defined Fields (from PARMLIB, if available), including any current VSAM-related suggestions:
```json
{fields_context_str}
```

Based on this information, considering both the original M204 field attributes and any 'current_is_key_component', 'current_key_order', 'current_vsam_data_type', 'current_vsam_length' suggestions from the input JSON:

1.  Suggest the most appropriate VSAM file organization (KSDS, ESDS, RRDS, or LDS) for the M204 file.
2.  For each field listed in the input `Defined Fields` JSON, provide refined suggestions for its role in a VSAM structure:
    *   `m204_field_name`: string (Must match one of the input field names from the "name" key in the `Defined Fields` JSON)
    *   `is_key_component`: boolean (true if this field should be part of the primary key for KSDS/RRDS)
    *   `key_order`: integer (1-based order if it's a key component. Null if not a key component.)
    *   `vsam_data_type`: string (suggest/confirm a COBOL PICTURE clause like "PIC X(10)", "PIC 9(7) COMP-3". Null if not applicable.)
    *   `vsam_length`: integer (suggest/confirm length in bytes for VSAM. Null if not applicable.)
    *   `reasoning`: string (briefly why you made these suggestions for this field, especially if refining previous suggestions)

Respond with a JSON object structured according to the M204FileVsamAnalysisOutput model.
Ensure `m204_field_name` in `field_specific_suggestions` matches one of the input field names.
If no fields are provided or applicable for key structure (e.g., for ESDS), `field_specific_suggestions` can be an empty list or only include fields that might still have relevant VSAM data type/length suggestions even if not keys.
For KSDS, identify the primary key field(s). For ESDS/LDS, key-related attributes are not applicable. For RRDS, the key is a relative record number but fields might still be described.
Your suggestions for fields should be the final determination, considering all available information.
"""
    json_text_output: Optional[str] = None
    try:
        vsam_analyzer_llm = llm_config._llm.as_structured_llm(M204FileVsamAnalysisOutput)
        completion_response = await vsam_analyzer_llm.acomplete(prompt=prompt_fstr)
        json_text_output = completion_response.text
        
        loaded_vsam_data = json.loads(json_text_output)
        vsam_output = M204FileVsamAnalysisOutput(**loaded_vsam_data)

        if vsam_output.m204_file_name != m204_file.m204_file_name:
            log.warning(f"M204_VSAM_ENHANCE: LLM returned VSAM data for '{vsam_output.m204_file_name}' but expected '{m204_file.m204_file_name}'. Skipping update for this file.")
            return

        # Update M204File
        m204_file.target_vsam_type = vsam_output.suggested_vsam_type
        log.info(f"M204_VSAM_ENHANCE: LLM suggested VSAM type for {m204_file.m204_file_name}: {m204_file.target_vsam_type}. Reasoning: {vsam_output.overall_reasoning or 'N/A'}")

        primary_key_components = []
        existing_fields_map = {f.field_name: f for f in m204_file.fields}

        for field_suggestion in vsam_output.field_specific_suggestions:
            db_field = existing_fields_map.get(field_suggestion.m204_field_name)
            if db_field:
                db_field.is_primary_key_component = field_suggestion.is_key_component
                db_field.target_vsam_key_order = field_suggestion.key_order if field_suggestion.is_key_component else None
                db_field.target_vsam_data_type = field_suggestion.vsam_data_type
                db_field.target_vsam_length = field_suggestion.vsam_length
                # db_field.llm_vsam_suggestion_reasoning = field_suggestion.reasoning # If you add this field to model
                db.add(db_field) # Add to session for update
                log.info(f"M204_VSAM_ENHANCE: LLM updated VSAM attributes for field '{db_field.field_name}' in {m204_file.m204_file_name}: key_comp={db_field.is_primary_key_component}, order={db_field.target_vsam_key_order}, type={db_field.target_vsam_data_type}, len={db_field.target_vsam_length}. Reason: {field_suggestion.reasoning or 'N/A'}")
                if db_field.is_primary_key_component and db_field.target_vsam_key_order is not None:
                    primary_key_components.append({"name": db_field.field_name, "order": db_field.target_vsam_key_order})
            else:
                log.warning(f"M204_VSAM_ENHANCE: LLM provided VSAM suggestion for field '{field_suggestion.m204_field_name}' which was not found in the current fields of M204File '{m204_file.m204_file_name}'.")
        
        if primary_key_components:
            primary_key_components.sort(key=lambda x: x["order"])
            m204_file.primary_key_field_name = ", ".join([comp["name"] for comp in primary_key_components])
            log.info(f"M204_VSAM_ENHANCE: LLM derived/updated primary key for {m204_file.m204_file_name}: {m204_file.primary_key_field_name}")
        else:
            if m204_file.primary_key_field_name is not None: # If it was previously set, now clear it
                 log.info(f"M204_VSAM_ENHANCE: No primary key components identified by LLM for {m204_file.m204_file_name}. Clearing existing PK '{m204_file.primary_key_field_name}'.")            
            m204_file.primary_key_field_name = None

        db.add(m204_file) # Add to session for update

    except json.JSONDecodeError as e_json:
        log.error(f"M204_VSAM_ENHANCE: LLM VSAM enhancement for {m204_file.m204_file_name}: JSON parsing error. Raw output: '{json_text_output if json_text_output else 'N/A'}'. Error: {e_json}", exc_info=True)
    except Exception as e_llm:
        log.error(f"M204_VSAM_ENHANCE: LLM VSAM enhancement for {m204_file.m204_file_name}: Error during LLM call or processing. Error: {e_llm}", exc_info=True)
        if json_text_output: # Log raw output if available during other exceptions
            log.error(f"M204_VSAM_ENHANCE: LLM raw output during error for {m204_file.m204_file_name}: {json_text_output}")
