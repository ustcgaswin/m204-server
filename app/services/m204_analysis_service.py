import re
import json
import asyncio
from typing import List, Tuple, Optional, Dict, Any, Match

from sqlalchemy.orm import Session
from sqlalchemy.sql import func
from pydantic import BaseModel, Field

from app.models.input_source_model import InputSource
from app.models.procedure_model import Procedure
from app.models.m204_file_model import M204File
from app.models.m204_variable_model import M204Variable
from app.models.procedure_call_model import ProcedureCall
from app.models.m204_image_definition_model import M204ImageDefinition

from app.models.m204_open_statement import M204OpenStatement


from app.schemas.m204_analysis_schema import (
    M204ProcedureCreateSchema, M204ProcedureResponseSchema,
    M204FileCreateSchema, M204FileResponseSchema,
    M204VariableCreateSchema, M204VariableResponseSchema,
    M204ProcedureCallCreateSchema, M204ProcedureCallResponseSchema,
    M204AnalysisResultDataSchema,
    M204OpenStatementResponseSchema,
)
# Importing M204FieldVsamSuggestion from parmlib_analysis_service to avoid redefinition
from app.services.parmlib_analysis_service import M204FieldVsamSuggestion
from app.services.rag_service import RagService # For type hinting
from app.utils.logger import log
from app.config.llm_config import llm_config
from llama_index.core.node_parser import SentenceSplitter


# --- Pydantic Models for LLM Structured Output ---

class M204ConceptIdentificationOutput(BaseModel):
    """Structured output for identifying key M204 concepts in a procedure."""
    procedure_name: str = Field(description="The original M204 procedure name.")
    identified_concepts: List[str] = Field(description="A list of key M204 commands, keywords, or concepts (e.g., FIND, FOR EACH VALUE, %variables, IMAGE, SCREEN) found or inferred from the procedure. These concepts will be used to retrieve relevant documentation.")
    brief_reasoning: str = Field(description="A very brief explanation of why these concepts are relevant to understanding the procedure's core functionality.")

class ProcedureAnalysisOutput(BaseModel):
    """Structured output for procedure summarization and COBOL name suggestion."""
    procedure_name: str = Field(description="The original M204 procedure name.")
    cobol_function_name_suggestion: str = Field(description="A suitable COBOL function or entry point name for this procedure, considering potential naming conflicts and COBOL naming conventions (e.g., 30 characters, alphanumeric, hyphens).") # MODIFIED HERE
    procedure_summary: str = Field(description="A detailed summary of the M204 procedure, covering its purpose, key logic, inputs, outputs, and interactions (e.g., file I/O, database calls), informed by the provided RAG context.")

class M204VariableToCobolOutput(BaseModel):
    """Structured output for M204 variable to COBOL name suggestion."""
    m204_variable_name: str = Field(description="The original M204 variable name.")
    suggested_cobol_variable_name: str = Field(description="A suitable COBOL variable name, following COBOL naming conventions (e.g., max 30 chars, alphanumeric, hyphens, avoid M204-specific symbols).")
    suggested_cobol_variable_type: Optional[str] = Field(description="A suitable COBOL variable type (e.g., PIC X(10), PIC 9(5)V99 COMP-3).", default=None)
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
    preconditions: Optional[List[str]] = Field(default=None, description="Any preconditions or setup required (e.g., specific data in files, %variables set).")
    inputs: Dict[str, Any] = Field(description="Key-value pairs of input parameters or %variables and their test values.")
    expected_outputs: Dict[str, Any] = Field(description="Key-value pairs of expected output %variables, screen elements, or file states and their values.")
    expected_behavior_description: str = Field(description="A textual description of the expected behavior, side effects, or outcome (e.g., 'Record X is written to FILEA', 'Error message Y is displayed').")

class ProcedureTestCaseGenerationOutput(BaseModel):
    """Structured output for LLM-generated unit test cases for a procedure."""
    procedure_name: str = Field(description="The original M204 procedure name.")
    test_cases: List[TestCase] = Field(description="A list of suggested unit test cases for the procedure.")
    generation_reasoning: Optional[str] = Field(description="Brief reasoning behind the types of test cases generated or any challenges.", default=None)


class ImageFieldToCobolOutput(BaseModel):
    """Structured output for M204 IMAGE field to COBOL name suggestion."""
    m204_image_name: str = Field(description="The M204 IMAGE statement name.")
    m204_field_name: str = Field(description="The original M204 field name within the IMAGE.")
    suggested_cobol_field_name: str = Field(description="A suitable COBOL field name, following COBOL naming conventions (e.g., max 30 chars, alphanumeric, hyphens, avoid M204-specific symbols).")
    reasoning: Optional[str] = Field(description="Brief reasoning for the suggestion.", default=None)


class M204IterativeDescriptionOutput(BaseModel):
    """Structured output for iteratively generating a detailed description of an M204 source file."""
    updated_description: str = Field(description="The refined and extended description of the M204 source file based on the current chunk and previous summary.")
    reasoning_for_update: Optional[str] = Field(description="Brief reasoning for how the current chunk updated the description.", default=None)
    key_elements_in_chunk: List[str] = Field(default_factory=list, description="List of key M204 statements or elements identified in the current chunk (e.g., PROCEDURE, SUBROUTINE, IMAGE, DEFINE DATASET, FIND, FOR EACH VALUE, %variables).")

LLM_API_CALL_BATCH_SIZE = 20


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



def strip_markdown_code_block(text: str) -> str:
    """
    Remove triple backtick code blocks (e.g., ```json ... ```) from LLM output.
    """
    text = text.strip()
    match = re.match(r"^```[a-zA-Z]*\n([\s\S]*?)\n```$", text)
    if match:
        return match.group(1).strip()
    match = re.match(r"^```[a-zA-Z]*\s*([\s\S]*?)\s*```$", text)
    if match:
        return match.group(1).strip()
    return text

async def _extract_and_store_m204_open_statements(
    db: Session,
    input_source: InputSource,
    file_content: str
) -> list:
    """
    Extracts OPEN {FILENAME} statements from M204 source and stores them in the DB.
    """
    open_statements = []
    open_pattern = re.compile(r"^\s*OPEN\s+([A-Z0-9_#@$-]+)", re.IGNORECASE | re.MULTILINE)
    lines = file_content.splitlines()

    for i, line in enumerate(lines):
        match = open_pattern.match(line)
        if match:
            m204_file_name = match.group(1).strip().upper()
            line_number = i + 1

            # Check for existing
            existing = db.query(M204OpenStatement).filter_by(
                project_id=input_source.project_id,
                input_source_id=input_source.input_source_id,
                m204_file_name=m204_file_name,
                line_number=line_number
            ).first()
            if existing:
                open_statements.append(existing)
                continue

            open_stmt = M204OpenStatement(
                project_id=input_source.project_id,
                input_source_id=input_source.input_source_id,
                m204_file_name=m204_file_name,
                line_number=line_number
            )
            db.add(open_stmt)
            open_statements.append(open_stmt)
    return open_statements



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
    suggested_cobol_function_name: Optional[str] = proc_name # MODIFIED VARIABLE NAME
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
                "suggested_cobol_function_name": proc_name, # MODIFIED KEY and Fallback
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
            loaded_concept_data = json.loads(strip_markdown_code_block(json_text_output_concept))
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
                # Convert list of concepts to a single query string
                rag_query_string = " ".join(concepts_and_params_for_rag)
                rag_context_for_summary = await rag_service.aquery(rag_query_string)
                log.info(f"M204_LLM_TASK: RAG context retrieved for {proc_name} (length: {len(rag_context_for_summary if rag_context_for_summary else '')}).")
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
2.  A COBOL function/entry point name suggestion: Propose a suitable name if this M204 procedure were to be converted to call a specific COBOL function or entry point. The name should be valid for COBOL (typically up to 30 characters, alphanumeric, starting with a letter, using hyphens not underscores) and descriptive of the targeted COBOL logic. If the original M204 name is suitable and valid after transformation (e.g., M204_PROC_NAME to M204-PCN-FUNC), you can suggest that.

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
- "cobol_function_name_suggestion": string (A suitable COBOL function or entry point name)
- "procedure_summary": string (A detailed summary of the M204 procedure)
"""
        summary_output_model: Optional[ProcedureAnalysisOutput] = None
        log.info(f"M204_LLM_TASK: Step 3: Generating summary and COBOL function name for {proc_name} with RAG context.") # MODIFIED Log
        try:
            summarizer_llm = llm_config._llm.as_structured_llm(ProcedureAnalysisOutput)
            completion_response_summary = await summarizer_llm.acomplete(prompt=summarization_prompt_fstr)
            json_text_output_summary = completion_response_summary.text
            loaded_summary_data = json.loads(strip_markdown_code_block(json_text_output_summary))
            summary_output_model = ProcedureAnalysisOutput(**loaded_summary_data)
        except Exception as e_summary_llm:
            log.error(f"M204_LLM_TASK: Error during summarization LLM call for {proc_name}: {e_summary_llm}. Raw output: '{json_text_output_summary}'", exc_info=True)

        if summary_output_model:
            generated_summary = summary_output_model.procedure_summary
            suggested_cobol_function_name = summary_output_model.cobol_function_name_suggestion # MODIFIED VARIABLE NAME
            log.info(f"M204_LLM_TASK: Summary and COBOL function name generated for {proc_name}: Summary len={len(generated_summary)}, COBOL Function Name='{suggested_cobol_function_name}'") # MODIFIED Log
        
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
            loaded_test_case_data = json.loads(strip_markdown_code_block(json_text_output_test_cases))
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
        "suggested_cobol_function_name": suggested_cobol_function_name, # MODIFIED KEY
        "suggested_test_cases_json": suggested_test_cases_json
    }


async def _task_generate_and_store_m204_description(
    db: Session,
    input_source_id: int,
    file_content: str,
    original_filename: str
):
    """
    Synchronous task to generate M204 description and store it in InputSource.
    Uses the database session provided by the caller.
    """
    try:
        log.info(f"M204_DESC_TASK: Starting description generation for {original_filename} (ID: {input_source_id})")
        
        input_s = db.query(InputSource).filter(InputSource.input_source_id == input_source_id).first()
        if not input_s:
            log.error(f"M204_DESC_TASK: InputSource with ID {input_source_id} not found in DB. Cannot store description.")
            return

        description = await _generate_m204_description_iteratively_with_llm(file_content, original_filename)
        
        if description:
            # IMPORTANT: Assumes InputSource model has a field 'm204_detailed_description'
            if hasattr(input_s, 'm204_detailed_description'):
                input_s.m204_detailed_description = description
                db.add(input_s)
                # The calling function will be responsible for the commit.
                log.info(f"M204_DESC_TASK: Description generated and queued for storage for {original_filename} (ID: {input_source_id}). Length: {len(description)}")
            else:
                log.error(f"M204_DESC_TASK: InputSource model does not have 'm204_detailed_description' field for {original_filename} (ID: {input_source_id}). Description not saved.")
        else:
            log.warning(f"M204_DESC_TASK: Description generation returned empty for {original_filename} (ID: {input_source_id}).")
            
    except Exception as e:
        log.error(f"M204_DESC_TASK: Error in M204 description generation/storage for {original_filename} (ID: {input_source_id}): {e}", exc_info=True)
        # Re-raise the exception to allow the orchestrator to handle it
        raise


async def _generate_m204_description_iteratively_with_llm(file_content: str, original_filename: str) -> str:
    """
    Uses an LLM to iteratively generate a detailed description of the M204 source file's content,
    focusing on functional requirements, rules, and logic.
    Splits the M204 code into chunks and processes them sequentially.
    """
    if not llm_config._llm:
        log.warning("M204_SERVICE_LLM_DESC: LLM is not configured. Cannot generate M204 detailed description.")
        return "LLM not configured. M204 description could not be generated."

    try:
        # Adjust chunk_size for M204 code. M204 lines can be dense.
        text_splitter = SentenceSplitter(chunk_size=300, chunk_overlap=30)
        m204_chunks = text_splitter.split_text(file_content)
    except Exception as e_splitter:
        log.error(f"M204_SERVICE_LLM_DESC: Error splitting M204 content for {original_filename}: {e_splitter}", exc_info=True)
        return f"Error splitting M204 content: {str(e_splitter)}"

    if not m204_chunks:
        log.warning(f"M204_SERVICE_LLM_DESC: No M204 chunks generated for {original_filename}. Content might be too short or empty.")
        return "M204 content was empty or too short to process for description."

    accumulated_description = f"Initial analysis of M204 source file: {original_filename}."
    llm_structured_caller = llm_config._llm.as_structured_llm(M204IterativeDescriptionOutput)

    log.info(f"M204_SERVICE_LLM_DESC: Starting iterative description generation for {original_filename} with {len(m204_chunks)} chunks.")

    for i, chunk in enumerate(m204_chunks):
        log.info(f"M204_SERVICE_LLM_DESC: Processing chunk {i+1}/{len(m204_chunks)} for {original_filename}.")
        prompt = f"""
You are a meticulous product analyst and M204 expert. Your task is to iteratively build a comprehensive document detailing the functional requirements, rules, and logic from an M204 source file: '{original_filename}'.
You will receive the M204 content in chunks. For each chunk, you will also receive the description (functional requirements, rules, logic) accumulated so far from previous chunks.

Your goal is to:
1. Analyze the 'Current M204 Chunk'.
2. Extract *all* functional requirements, rules, and logic from this chunk. Pay close attention to even small details, conditions, constraints, and specific behaviors implemented in the code.
3. Describe the key functionalities, user benefits (if inferable), system interactions, data flows, and any implemented business rules comprehensively.
4. Update and extend the 'Previous Description' by intelligently merging, refining, and expanding it with the new findings from the current chunk. The 'updated_description' should be a clear, cohesive narrative of these requirements, rules, and logic, presented in paragraphs. Do not omit any details, no matter how minor they seem.
5. Identify key M204 statements or elements in the current chunk that led to these findings (e.g., PROCEDURE, SUBROUTINE, IMAGE, DEFINE DATASET, FIND, FOR EACH VALUE, %variables, IF/ELSE conditions).

Previous Description (Functional Requirements, Rules, Logic so far):
--- PREVIOUS DESCRIPTION START ---
{accumulated_description}
--- PREVIOUS DESCRIPTION END ---

Current M204 Chunk to Analyze (Part {i+1} of {len(m204_chunks)} of M204 source file '{original_filename}'):
--- CURRENT M204 CHUNK START ---
{chunk}
--- CURRENT M204 CHUNK END ---

Respond with a JSON object structured according to the M204IterativeDescriptionOutput model:
- "updated_description": string (The comprehensive, updated text of functional requirements, rules, and logic, presented as clear, cohesive paragraphs.)
- "reasoning_for_update": string (Briefly explain how the current chunk contributed to or modified the understanding of the requirements, rules, or logic.)
- "key_elements_in_chunk": list of strings (List of key M204 statements or elements identified in the current chunk that informed the updated description.)

The JSON output must conform to this Pydantic model:
class M204IterativeDescriptionOutput(BaseModel):
    updated_description: str
    reasoning_for_update: Optional[str]
    key_elements_in_chunk: List[str]
"""
        completion_response = None
        try:
            completion_response = await llm_structured_caller.acomplete(prompt)
            if completion_response and completion_response.text:
                loaded_data = json.loads(strip_markdown_code_block(completion_response.text))
                response_model = M204IterativeDescriptionOutput(**loaded_data)
                accumulated_description = response_model.updated_description
                log.debug(f"M204_SERVICE_LLM_DESC: Chunk {i+1} processed for {original_filename}. Reasoning: {response_model.reasoning_for_update}. Keys: {response_model.key_elements_in_chunk}")
            else:
                log.error(f"M204_SERVICE_LLM_DESC: LLM returned empty or no text for chunk {i+1} of {original_filename}.")
                accumulated_description += f"\n\n[LLM Error processing chunk {i+1} for M204 description: LLM returned no content for this section.]"

        except json.JSONDecodeError as e_json_iter:
            raw_output_text = completion_response.text if completion_response and hasattr(completion_response, 'text') else "Raw output not available or completion_response is None"
            log.error(f"M204_SERVICE_LLM_DESC: JSON parsing error for M204 description chunk {i+1} of {original_filename}: {e_json_iter}. Raw output: '{raw_output_text}'", exc_info=True)
            accumulated_description += f"\n\n[LLM Error processing chunk {i+1} for M204 description: Could not parse LLM output. Error: {str(e_json_iter)}]"
        except Exception as e_llm_iter:
            log.error(f"M204_SERVICE_LLM_DESC: Error during LLM call or processing for M204 description chunk {i+1} of {original_filename}: {e_llm_iter}", exc_info=True)
            accumulated_description += f"\n\n[LLM Error processing chunk {i+1} for M204 description: Could not fully integrate this section. Error: {str(e_llm_iter)}]"

    log.info(f"M204_SERVICE_LLM_DESC: Finished iterative M204 description generation for {original_filename}.")
    return accumulated_description


async def _extract_and_store_m204_procedures(
    db: Session, input_source: InputSource, file_content: str, rag_service: Optional[RagService]
) -> List[Procedure]:
    log.info(f"M204_SERVICE: Extracting M204 procedures for file ID: {input_source.input_source_id} ({input_source.original_filename})")
    procedures_processed_for_db = [] # Stores Procedure ORM objects
    
    # Pattern for "KEYWORD NAME" format, e.g., "SUBROUTINE MYPROC" or "PUBLIC PROCEDURE MYPROC (PARAMS)"
    proc_pattern_keyword_first = re.compile(
        r"^\s*(?:(PUBLIC|PRIVATE)\s+)?(SUBROUTINE|PROCEDURE)\s+([A-Z0-9_#@$-]{1,32})(?:\s*\(([^)]*)\))?",
        re.IGNORECASE | re.MULTILINE
    )
    # Pattern for "NAME: KEYWORD" format, e.g., "MYPROC: SUBROUTINE" or "MYPROC: PROCEDURE (PARAMS)"
    proc_pattern_name_first = re.compile(
        r"^\s*([A-Z0-9_#@$-]{1,32})\s*:\s*(PUBLIC\s+|PRIVATE\s+)?(SUBROUTINE|PROCEDURE)(?:\s*\(([^)]*)\))?",
        re.IGNORECASE | re.MULTILINE
    )

    lines = file_content.splitlines()
    
    temp_procs_info = [] # Stores dictionaries with normalized procedure info
    for i, line_content_for_match in enumerate(lines):
        proc_name: Optional[str] = None
        proc_type_keyword: Optional[str] = None
        params_str: Optional[str] = None
        proc_visibility_keyword: Optional[str] = None # e.g., PUBLIC, PRIVATE
        
        match_keyword_first = proc_pattern_keyword_first.match(line_content_for_match)
        match_name_first = None

        if match_keyword_first:
            proc_visibility_keyword = match_keyword_first.group(1)
            proc_type_keyword = match_keyword_first.group(2)
            proc_name = match_keyword_first.group(3)
            params_str = match_keyword_first.group(4)
        else:
            match_name_first = proc_pattern_name_first.match(line_content_for_match)
            if match_name_first:
                proc_name = match_name_first.group(1)
                # Visibility keyword might be between colon and SUBROUTINE/PROCEDURE
                visibility_in_name_first = match_name_first.group(2)
                if visibility_in_name_first:
                    proc_visibility_keyword = visibility_in_name_first.strip().upper()
                proc_type_keyword = match_name_first.group(3)
                params_str = match_name_first.group(4)

        if proc_name and proc_type_keyword:
            start_line_num = i + 1
            temp_procs_info.append({
                "proc_name": proc_name,
                "proc_type_keyword": proc_type_keyword.upper(),
                "params_str": params_str.strip() if params_str else None,
                "proc_visibility_keyword": proc_visibility_keyword.upper() if proc_visibility_keyword else None,
                "start_line": start_line_num,
                "original_index": len(temp_procs_info) # Used for sorting and determining end_line
            })

    # Sort by start line to correctly determine procedure content boundaries
    temp_procs_info.sort(key=lambda p: p["start_line"])
    # Update original_index after sorting if it's critical for anything other than end_line logic based on next item
    for idx, item in enumerate(temp_procs_info):
        item["original_index"] = idx


    llm_processing_tasks = []
    procedures_undergoing_llm_data = []
    procedures_not_undergoing_llm_data = []

    for proc_data_from_parse in temp_procs_info:
        # These are now directly from the parsed info
        proc_name = proc_data_from_parse["proc_name"]
        proc_type_keyword = proc_data_from_parse["proc_type_keyword"] # Already upper
        params_str = proc_data_from_parse["params_str"]
        proc_visibility_keyword = proc_data_from_parse["proc_visibility_keyword"] # Already upper or None
        start_line_num = proc_data_from_parse["start_line"]
        original_idx = proc_data_from_parse["original_index"]

        actual_proc_type = proc_type_keyword 
        if proc_visibility_keyword:
            actual_proc_type = f"{proc_visibility_keyword} {proc_type_keyword}"


        end_line_num: Optional[int] = None
        current_procedure_content: Optional[str] = None

        # Determine end_line_num based on the start of the next procedure or end of file
        if original_idx + 1 < len(temp_procs_info):
            end_line_num = temp_procs_info[original_idx+1]["start_line"] - 1
        else:
            end_line_num = len(lines) 
        
        if start_line_num <= end_line_num:
            # Content includes the declaration line itself up to the line before the next proc or EOF
            # This will include "END SUBROUTINE" if it's within this block.
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
            base_proc_db_data["suggested_cobol_function_name"] = proc_name 
            base_proc_db_data["suggested_test_cases_json"] = None
            if not current_procedure_content:
                 log.warning(f"M204_SERVICE: Procedure {proc_name} has no content. Skipping LLM analysis.")
            elif not llm_config._llm:
                 log.info(f"M204_SERVICE: LLM not configured. Skipping LLM analysis for procedure {proc_name}.")
            procedures_not_undergoing_llm_data.append(base_proc_db_data)

    llm_results_list = []
    if llm_processing_tasks:
        log.info(f"M204_SERVICE: Executing {len(llm_processing_tasks)} LLM analysis tasks in batches for file {input_source.original_filename}.")
        for i in range(0, len(llm_processing_tasks), LLM_API_CALL_BATCH_SIZE):
            batch_tasks = llm_processing_tasks[i:i + LLM_API_CALL_BATCH_SIZE]
            log.info(f"M204_SERVICE: Processing batch {i // LLM_API_CALL_BATCH_SIZE + 1} with {len(batch_tasks)} tasks for file {input_source.original_filename}.")
            batch_results = await asyncio.gather(*batch_tasks, return_exceptions=True)
            llm_results_list.extend(batch_results)
            log.info(f"M204_SERVICE: Finished batch {i // LLM_API_CALL_BATCH_SIZE + 1} for file {input_source.original_filename}.")
        log.info(f"M204_SERVICE: Finished all {len(llm_processing_tasks)} LLM analysis tasks in batches for file {input_source.original_filename}.")

    all_proc_data_for_db_ops = []
    for i, llm_result_or_exc in enumerate(llm_results_list):
        proc_static_data = procedures_undergoing_llm_data[i]
        proc_name = proc_static_data["proc_name"]
        generated_summary, suggested_cobol_function_name_val, suggested_test_cases = None, proc_name, None

        if isinstance(llm_result_or_exc, Exception):
            log.error(f"M204_SERVICE: LLM analysis task for procedure {proc_name} failed: {llm_result_or_exc}", exc_info=llm_result_or_exc)
            generated_summary = f"Error during AI analysis for {proc_name}. Check logs."
        elif isinstance(llm_result_or_exc, dict) and llm_result_or_exc.get("proc_name") == proc_name:
            generated_summary = llm_result_or_exc.get("generated_summary")
            suggested_cobol_function_name_val = llm_result_or_exc.get("suggested_cobol_function_name", proc_name)
            suggested_test_cases = llm_result_or_exc.get("suggested_test_cases_json")
        else: 
            log.error(f"M204_SERVICE: Unexpected result or mismatched proc_name for {proc_name} from LLM task: {llm_result_or_exc}")
            generated_summary = f"Unexpected error or result format from AI analysis for {proc_name}."
        
        all_proc_data_for_db_ops.append({
            **proc_static_data, 
            "generated_summary": generated_summary, 
            "suggested_cobol_function_name": suggested_cobol_function_name_val,
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
            target_cobol_function_name=proc_data["suggested_cobol_function_name"],
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




async def extract_and_store_main_loop(db: Session, input_source_id: int, file_content: str):
    """
    Extracts the main processing loop from the file content and stores it in the InputSource record.
    This is intended to be called by the orchestrator after structural analysis.
    Only the content between the first line that is 'B' or 'BEGIN' and the last line that is 'END' is processed.
    """
    try:
        log.info(f"M204_MAIN_LOOP_TASK: Starting main loop extraction for InputSource ID: {input_source_id}")
        input_s = db.query(InputSource).filter(InputSource.input_source_id == input_source_id).first()
        if not input_s:
            log.error(f"M204_MAIN_LOOP_TASK: InputSource with ID {input_source_id} not found. Cannot store main loop content.")
            return

        # Find the first line that is 'B' or 'BEGIN' and the last line that is 'END'
        lines = file_content.splitlines()
        start_idx = None
        end_idx = None

        for idx, line in enumerate(lines):
            if re.match(r'^\s*(B|BEGIN)\s*$', line, re.IGNORECASE):
                start_idx = idx
                break

        for idx in range(len(lines) - 1, -1, -1):
            if re.match(r'^\s*END\s*$', lines[idx], re.IGNORECASE):
                end_idx = idx
                break

        if start_idx is not None and end_idx is not None and start_idx < end_idx:
            main_loop_section = "\n".join(lines[start_idx:end_idx + 1])
        else:
            main_loop_section = None

        main_loop_content = _extract_main_loop_content(main_loop_section) if main_loop_section else None

        if main_loop_content:
            if hasattr(input_s, 'main_processing_loop_content'):
                input_s.main_processing_loop_content = main_loop_content
                db.add(input_s)
                log.info(f"M204_MAIN_LOOP_TASK: Main loop content extracted and queued for storage for InputSource ID: {input_source_id}. Length: {len(main_loop_content)}")
            else:
                log.error("M204_MAIN_LOOP_TASK: InputSource model does not have 'main_processing_loop_content' field. Main loop content not saved.")
        else:
            log.info(f"M204_MAIN_LOOP_TASK: No main processing loop (B/BEGIN...END) found for InputSource ID: {input_source_id}.")

    except Exception as e:
        log.error(f"M204_MAIN_LOOP_TASK: Error in main loop extraction/storage for InputSource ID {input_source_id}: {e}", exc_info=True)
        # Re-raise to allow the orchestrator to handle it
        raise




def _extract_main_loop_content(file_content: str) -> Optional[str]:
    """
    Extracts the main processing logic from an M204 procedure file.

    This function defines the main logic block as the content between the
    first line containing 'B' or 'BEGIN' and the last line containing 'END'.

    Within this block, it filters out and skips over the entirety of:
    - IMAGE ... END IMAGE blocks
    - SUBROUTINE ... END SUBROUTINE blocks
    - PROCEDURE ... END PROCEDURE blocks
    - Single-line variable declarations (%VAR IS ...)
    - Root-level comments (lines starting with *)

    All other content is preserved.

    Args:
        file_content: The string content of the M204 procedure file.

    Returns:
        A string containing the extracted and filtered main logic, or None if
        a valid BEGIN...END block is not found.
    """
    if not file_content:
        return None

    # --- Regular Expression Definitions ---
    begin_pattern = re.compile(r"^\s*(B|BEGIN)\s*$", re.IGNORECASE | re.MULTILINE)
    end_pattern = re.compile(r"^\s*END\s*$", re.IGNORECASE | re.MULTILINE)
    comment_pattern = re.compile(r"^\s*\*.*", re.MULTILINE)

    m204_var_declaration_pattern = re.compile(
        r"^\s*(?:(PUBLIC|PRIVATE)\s+)?(%[A-Z0-9_#@$-]+)\s+IS\s+(.*)",
        re.IGNORECASE | re.MULTILINE,
    )
    proc_pattern_keyword_first = re.compile(
        r"^\s*(?:(PUBLIC|PRIVATE)\s+)?(SUBROUTINE|PROCEDURE)\s+([A-Z0-9_#@$-]{1,32})",
        re.IGNORECASE | re.MULTILINE,
    )
    proc_pattern_name_first = re.compile(
        r"^\s*([A-Z0-9_#@$-]{1,32})\s*:\s*(PUBLIC\s+|PRIVATE\s+)?(SUBROUTINE|PROCEDURE)",
        re.IGNORECASE | re.MULTILINE,
    )
    end_proc_pattern = re.compile(
        r"^\s*END\s+(SUBROUTINE|PROCEDURE)\b", re.IGNORECASE | re.MULTILINE
    )
    image_start_pattern = re.compile(
        r"^\s*IMAGE\s+([A-Z0-9_.#@$-]+)", re.IGNORECASE | re.MULTILINE
    )
    image_end_pattern = re.compile(
        r"^\s*END\s+IMAGE\b", re.IGNORECASE | re.MULTILINE
    )

    # --- Phase 1: Find the overall boundaries ---
    first_begin_match = begin_pattern.search(file_content)
    if not first_begin_match:
        return None

    end_matches = list(end_pattern.finditer(file_content))
    if not end_matches:
        return None
    last_end_match = end_matches[-1]

    start_boundary = first_begin_match.start()
    end_boundary = last_end_match.end()

    if start_boundary >= last_end_match.start():
        return None  # Invalid if last END is before first BEGIN

    # --- Phase 2: Parse content within boundaries, skipping blocks ---
    main_content_parts: List[str] = []
    search_pos = start_boundary

    while search_pos < end_boundary:
        content_slice = file_content[search_pos:end_boundary]
        found_matches: List[Tuple[Match, str]] = []

        # Find the next block to potentially skip
        if m := image_start_pattern.search(content_slice):
            found_matches.append((m, "image"))
        if m := proc_pattern_keyword_first.search(content_slice):
            found_matches.append((m, "proc"))
        if m := proc_pattern_name_first.search(content_slice):
            found_matches.append((m, "proc"))
        if m := m204_var_declaration_pattern.search(content_slice):
            found_matches.append((m, "var"))
        if m := comment_pattern.search(content_slice):
            found_matches.append((m, "comment"))

        if not found_matches:
            main_content_parts.append(content_slice)
            break

        first_match, match_type = min(found_matches, key=lambda m: m[0].start())
        match_start_in_file = search_pos + first_match.start()
        match_end_in_file = search_pos + first_match.end()

        main_content_parts.append(file_content[search_pos:match_start_in_file])

        # --- Decision Logic to skip the block ---
        if match_type == "image":
            end_image_match = image_end_pattern.search(file_content, pos=match_end_in_file)
            search_pos = end_image_match.end() if end_image_match else end_boundary
        elif match_type == "proc":
            end_proc_match = end_proc_pattern.search(file_content, pos=match_end_in_file)
            search_pos = end_proc_match.end() if end_proc_match else end_boundary
        elif match_type in ("var", "comment"):
            # Variable declarations and comments are single-line, so just skip the match
            search_pos = match_end_in_file

    result = "".join(main_content_parts).strip()
    return result if result else None



async def _extract_and_store_m204_datasets(
    db: Session, input_source: InputSource, file_content: str
) -> List[M204File]:
    log.info(f"M204_SERVICE: Extracting M204 DEFINE DATASET statements for file ID: {input_source.input_source_id} ({input_source.original_filename})")
    defined_m204_files = []
    define_dataset_initial_pattern = re.compile(
        r"^\s*DEFINE\s+DATASET\s+([A-Z0-9_#@$-.]+)\s*(.*)",
        re.IGNORECASE
    )
    define_dataset_full_pattern = re.compile(
        r"^\s*DEFINE\s+DATASET\s+([A-Z0-9_#@$-.]+)\s*(.*)",
        re.IGNORECASE | re.DOTALL 
    )
    lines = file_content.splitlines()
    
    ddname_value_capture_pattern = r"'([A-Z0-9_#@$-]{1,8})'|\"([A-Z0-9_#@$-]{1,8})\"|([A-Z0-9_#@$-]{1,8})"
    ddname_search_regex = rf"DDNAME\s*=\s*(?:{ddname_value_capture_pattern})"

    i = 0
    while i < len(lines):
        current_line_original = lines[i]
        current_line_stripped = current_line_original.strip()
        initial_match = define_dataset_initial_pattern.match(current_line_stripped)

        if initial_match:
            m204_logical_name_from_first_line = initial_match.group(1) 
            start_line_num = i + 1 

            statement_lines_for_db_attributes = [current_line_original] 
            stripped_statement_parts_for_param_parsing = [current_line_stripped] 

            current_physical_line_idx = i 
            while lines[current_physical_line_idx].strip().endswith('-') and \
                  current_physical_line_idx + 1 < len(lines):
                current_physical_line_idx += 1
                next_line_original = lines[current_physical_line_idx]
                next_line_stripped = next_line_original.strip()

                if next_line_stripped.startswith('*'): 
                    current_physical_line_idx -= 1 
                    break 
                statement_lines_for_db_attributes.append(next_line_original)
                stripped_statement_parts_for_param_parsing.append(next_line_stripped)
            
            end_line_num = current_physical_line_idx + 1
            
            final_m204_attributes_for_db = "\n".join(statement_lines_for_db_attributes)
            full_statement_for_param_parsing = " ".join(stripped_statement_parts_for_param_parsing) 
            
            final_parse_match = define_dataset_full_pattern.match(full_statement_for_param_parsing)
            
            m204_authoritative_logical_name: str
            params_str_for_ddname_search: str

            if final_parse_match:
                m204_authoritative_logical_name = final_parse_match.group(1).strip().upper()
                params_str_for_ddname_search = final_parse_match.group(2) or ""
            else:
                log.warning(f"M204_SERVICE: Could not re-parse combined multi-line DEFINE DATASET statement starting at line {start_line_num} in {input_source.original_filename}. Using first line data. Statement: '{full_statement_for_param_parsing[:200]}'")
                m204_authoritative_logical_name = m204_logical_name_from_first_line.strip().upper()
                params_str_for_ddname_search = initial_match.group(2) or ""
            
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
                file_key_for_m204_file_name = m204_authoritative_logical_name 
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
                "m204_attributes": final_m204_attributes_for_db, 
                "definition_line_number_start": start_line_num,
                "definition_line_number_end": end_line_num,
                "is_db_file": None, 
                "file_definition_json": None
            }
            
            try:
                file_schema_for_create_or_update = M204FileCreateSchema(**file_data_dict)
            except Exception as e_schema:
                log.error(f"M204_SERVICE: Error creating schema for M204File '{file_key_for_m204_file_name}' from DEFINE DATASET: {e_schema}", exc_info=True)
                i = end_line_num 
                continue

            if existing_m204_file:
                update_needed = False
                if existing_m204_file.defined_in_input_source_id != file_schema_for_create_or_update.defined_in_input_source_id:
                    existing_m204_file.defined_in_input_source_id = file_schema_for_create_or_update.defined_in_input_source_id
                    update_needed = True
                if existing_m204_file.definition_line_number_start != file_schema_for_create_or_update.definition_line_number_start:
                    existing_m204_file.definition_line_number_start = file_schema_for_create_or_update.definition_line_number_start
                    update_needed = True
                if existing_m204_file.definition_line_number_end != file_schema_for_create_or_update.definition_line_number_end:
                    existing_m204_file.definition_line_number_end = file_schema_for_create_or_update.definition_line_number_end
                    update_needed = True
                if existing_m204_file.m204_attributes != file_schema_for_create_or_update.m204_attributes:
                    existing_m204_file.m204_attributes = file_schema_for_create_or_update.m204_attributes
                    update_needed = True
                if existing_m204_file.m204_logical_dataset_name != file_schema_for_create_or_update.m204_logical_dataset_name:
                    existing_m204_file.m204_logical_dataset_name = file_schema_for_create_or_update.m204_logical_dataset_name
                    update_needed = True
                
                if existing_m204_file.is_db_file is None and file_schema_for_create_or_update.is_db_file is not None : 
                     existing_m204_file.is_db_file = file_schema_for_create_or_update.is_db_file 
                     update_needed = True
                
                if update_needed:
                    db.add(existing_m204_file)
                    log.info(f"M204_SERVICE: Updating existing M204File '{file_key_for_m204_file_name}' (ID: {existing_m204_file.m204_file_id}) based on DEFINE DATASET in {input_source.original_filename}.")
                defined_m204_files.append(existing_m204_file)
            else:
                try:
                    db_m204_file = M204File(**file_schema_for_create_or_update.model_dump(exclude_none=True))
                    db.add(db_m204_file)
                    defined_m204_files.append(db_m204_file)
                    log.info(f"M204_SERVICE: Created new M204File '{file_key_for_m204_file_name}' (Logical: '{m204_authoritative_logical_name}') from DEFINE DATASET in {input_source.original_filename}.")
                except Exception as e_create:
                    log.error(f"M204_SERVICE: Error creating M204File '{file_key_for_m204_file_name}' from DEFINE DATASET: {e_create}", exc_info=True)

            i = end_line_num 
            continue 
        
        i += 1 
    return defined_m204_files

def _parse_m204_variable_declaration_attributes(attr_declaration_string: Optional[str]) -> Dict[str, Any]:
    """
    Parses the attribute string from M204 variable declarations like '%VAR IS TYPE LEN X ...'.
    Example input: "STRING LEN 9", "FIXED", "STRING LEN 40 ARRAY(20) NO FS"
    Returns a dictionary of parsed attributes.
    """
    if not attr_declaration_string:
        return {}

    parsed_attrs: Dict[str, Any] = {}
    remaining_str = attr_declaration_string.strip()

    # Extract main type (first word, typically)
    # This pattern tries to capture common M204 types. It might need refinement for complex/multi-word types.
    type_match = re.match(r"([A-Z0-9_]+(?:\s+[A-Z0-9_]+)*?)(\s+LEN|\s+ARRAY|\s+OCCURS|\s+KEYED|\s+ORDERED|\s+UNIQUE|\s+VISIBLE|\s+INVISIBLE|\s+NOMINAL|\s+CODED|\s+TRANSLATE|\s+NO\s+FS|$)", remaining_str, re.IGNORECASE)
    if type_match:
        declared_type = type_match.group(1).strip().upper()
        parsed_attrs["declared_m204_type"] = declared_type
        # Update remaining_str to be after the matched type
        # This is a bit tricky if the type itself has spaces.
        # For simplicity, we'll assume the type is captured and we adjust remaining_str from its end.
        # A more robust parser might tokenize the string.
        # This finds the start of the next known keyword or end of string to isolate the type.
        
        # Re-evaluate remaining_str based on where the type declaration ends
        # Find the end of the type part before known keywords or end of string
        type_end_idx = len(declared_type)
        remaining_str = remaining_str[type_end_idx:].strip()

    else:
        # If no clear type found, store the whole string or handle as error
        log.warning(f"Could not clearly parse declared_m204_type from: '{attr_declaration_string}'")
        parsed_attrs["raw_attributes_unparsed"] = remaining_str
        # Attempt to parse other known attributes even if type is unclear
        # return parsed_attrs # Optionally return early if type is mandatory for further parsing

    # Extract LEN
    len_match = re.search(r"(?:^|\s)LEN\s+(\d+)", remaining_str, re.IGNORECASE)
    if len_match:
        parsed_attrs["length"] = int(len_match.group(1))
        # Remove LEN part for further parsing (simplistic removal)
        remaining_str = re.sub(r"(?:^|\s)LEN\s+\d+", "", remaining_str, count=1, flags=re.IGNORECASE).strip()


    # Extract ARRAY or OCCURS (synonyms in some M204 contexts)
    array_occurs_match = re.search(r"(?:^|\s)(?:ARRAY|OCCURS)\s*\(([^)]+)\)", remaining_str, re.IGNORECASE)
    if array_occurs_match:
        dimensions_str = array_occurs_match.group(1)
        try:
            parsed_attrs["array_dimensions"] = [int(d.strip()) for d in dimensions_str.split(',')]
        except ValueError:
            log.warning(f"Could not parse array/occurs dimensions: {dimensions_str} in '{attr_declaration_string}'")
            parsed_attrs["array_dimensions_raw"] = dimensions_str
        remaining_str = re.sub(r"(?:^|\s)(?:ARRAY|OCCURS)\s*\([^)]+\)", "", remaining_str, count=1, flags=re.IGNORECASE).strip()


    # Store any remaining keywords/attributes as a list
    if remaining_str:
        other_keywords = [kw.strip().upper() for kw in remaining_str.split() if kw.strip()]
        if other_keywords:
            # Check for known boolean-like attributes
            known_boolean_attrs = ["KEYED", "ORDERED", "UNIQUE", "VISIBLE", "INVISIBLE", "NOMINAL", "CODED", "TRANSLATE"]
            # Handle "NO FS" as a special case or other "NO <keyword>"
            
            final_other_keywords = []
            skip_next = False
            for i, kw in enumerate(other_keywords):
                if skip_next:
                    skip_next = False
                    continue
                if kw == "NO" and i + 1 < len(other_keywords):
                    final_other_keywords.append(f"NO {other_keywords[i+1]}")
                    skip_next = True
                elif kw in known_boolean_attrs:
                     parsed_attrs[kw.lower()] = True # Store known attributes directly
                else:
                    final_other_keywords.append(kw) # Add to general list if not specifically handled

            if final_other_keywords:
                 parsed_attrs["other_m204_keywords"] = final_other_keywords
            
    return parsed_attrs




async def _extract_and_store_m204_variables(
    db: Session, input_source: InputSource, file_content: str, procedures_in_file: List[Procedure], rag_service: Optional[RagService]
) -> List[M204Variable]:
    log.info(f"M204_SERVICE: Extracting explicitly defined M204 variables for file ID: {input_source.input_source_id}")
    variables_processed_for_db = []
    lines = file_content.splitlines()
    
    m204_var_declaration_pattern = re.compile(
        r"^\s*(?:(PUBLIC|PRIVATE)\s+)?(%[A-Z0-9_#@$-]+)\s+IS\s+(.*)", 
        re.IGNORECASE
    )
    
    proc_line_map = {proc.start_line_in_source: proc.proc_id for proc in procedures_in_file if proc.start_line_in_source and proc.proc_id is not None}
    proc_id_to_name_map = {p.proc_id: p.m204_proc_name for p in procedures_in_file if p.proc_id}
    sorted_proc_starts = sorted(proc_line_map.keys())

    parsed_variables_data = [] # Stores data for all parsed variables before LLM and DB ops

    for i, line_content in enumerate(lines):
        line_num = i + 1
        
        current_proc_id_for_line: Optional[int] = None
        temp_proc_id_for_line: Optional[int] = None
        for start_line in sorted_proc_starts:
            proc_obj = next((p for p in procedures_in_file if p.start_line_in_source == start_line), None)
            if proc_obj:
                if proc_obj.end_line_in_source and start_line <= line_num <= proc_obj.end_line_in_source:
                    temp_proc_id_for_line = proc_line_map.get(start_line)
                    break 
                elif not proc_obj.end_line_in_source and start_line <= line_num:
                    temp_proc_id_for_line = proc_line_map.get(start_line)
                elif line_num < start_line:
                    break 
        current_proc_id_for_line = temp_proc_id_for_line

        declaration_match = m204_var_declaration_pattern.match(line_content)
        if declaration_match:
            visibility_keyword = declaration_match.group(1)
            var_name = declaration_match.group(2)
            attributes_declaration_str = declaration_match.group(3).strip()
            
            scope = "LOCAL"
            if visibility_keyword:
                scope = visibility_keyword.upper()
            elif current_proc_id_for_line is None:
                scope = "GLOBAL"

            parsed_declaration_attributes = _parse_m204_variable_declaration_attributes(attributes_declaration_str)
            proc_name_context_for_llm = proc_id_to_name_map.get(current_proc_id_for_line) if current_proc_id_for_line else "File Level"
            # Get the declared M204 type from parsed attributes
            declared_m204_type = parsed_declaration_attributes.get("declared_m204_type", "UNKNOWN")


            parsed_variables_data.append({
                "var_name": var_name,
                "attributes_declaration_str": attributes_declaration_str,
                "scope": scope,
                "line_num": line_num,
                "current_proc_id_for_line": current_proc_id_for_line,
                "proc_name_context_for_llm": proc_name_context_for_llm,
                "parsed_declaration_attributes": parsed_declaration_attributes,
                "declared_m204_type": declared_m204_type, # Store the parsed M204 type
                "suggested_cobol_name": None, # To be filled by LLM
                "suggested_cobol_variable_type": None, # To be filled by LLM
                "original_index": len(parsed_variables_data) # To map LLM results
            })

    # Parallel LLM calls for COBOL name suggestions
    if llm_config._llm and parsed_variables_data:
        llm_tasks = []
        var_namer_llm = llm_config._llm.as_structured_llm(M204VariableToCobolOutput)
        log.info(f"M204_SERVICE_VAR_LLM: Preparing {len(parsed_variables_data)} LLM tasks for variable name and type suggestions in file {input_source.original_filename}.")

        for var_data in parsed_variables_data:
            variable_name_for_llm = var_data["var_name"]
            attributes_for_llm = var_data["attributes_declaration_str"]
            proc_name_context_for_llm = var_data["proc_name_context_for_llm"]
            declared_m204_type_for_llm = var_data["declared_m204_type"] # Use the stored parsed type
            
            var_prompt_fstr = f"""
You are an M204 to COBOL migration expert. Suggest a COBOL-compliant variable name and type for the given M204 variable.
M204 Variable Name: {variable_name_for_llm}
M204 Variable Prefix Type: PERCENT 
M204 Declared Type (from 'IS' clause): {declared_m204_type_for_llm}
M204 Full Declaration Attributes (from 'IS' clause): {attributes_for_llm or "None"}
Context (Procedure or File Level): {proc_name_context_for_llm}

Respond with a JSON object containing "m204_variable_name", "suggested_cobol_variable_name", "suggested_cobol_variable_type", and "reasoning".
The COBOL name should be max 30 chars, alphanumeric, use hyphens, and avoid M204-specific symbols like '%'.
The COBOL variable type should be a COBOL PICTURE clause (e.g., PIC X(10), PIC 9(5)V99 COMP-3).
Consider the M204 declared type and attributes for meaningful COBOL suggestions.
"""
            llm_tasks.append(var_namer_llm.acomplete(prompt=var_prompt_fstr))
        
        llm_raw_results_list = []
        if llm_tasks:
            log.info(f"M204_SERVICE_VAR_LLM: Starting {len(llm_tasks)} LLM calls in batches for variable names and types in file {input_source.original_filename}.")
            for i in range(0, len(llm_tasks), LLM_API_CALL_BATCH_SIZE):
                batch_tasks = llm_tasks[i:i + LLM_API_CALL_BATCH_SIZE]
                log.info(f"M204_SERVICE_VAR_LLM: Processing batch {i // LLM_API_CALL_BATCH_SIZE + 1} with {len(batch_tasks)} tasks for variable names and types in {input_source.original_filename}.")
                batch_results = await asyncio.gather(*batch_tasks, return_exceptions=True)
                llm_raw_results_list.extend(batch_results)
                log.info(f"M204_SERVICE_VAR_LLM: Finished batch {i // LLM_API_CALL_BATCH_SIZE + 1} for variable names and types in {input_source.original_filename}.")
            log.info(f"M204_SERVICE_VAR_LLM: Finished all {len(llm_tasks)} LLM calls for variable names and types in {input_source.original_filename}.")

            for idx, result_or_exc in enumerate(llm_raw_results_list):
                target_var_data = parsed_variables_data[idx] # Assumes order is preserved
                variable_name_for_llm = target_var_data["var_name"]
                suggested_name = None
                suggested_type = None

                if isinstance(result_or_exc, Exception):
                    log.error(f"M204_SERVICE_VAR_LLM: Error suggesting COBOL name/type for variable {variable_name_for_llm}: {result_or_exc}", exc_info=True)
                elif result_or_exc:
                    try:
                        loaded_var_data = json.loads(strip_markdown_code_block(result_or_exc.text))
                        var_output_model = M204VariableToCobolOutput(**loaded_var_data)
                        if var_output_model.m204_variable_name == variable_name_for_llm:
                            suggested_name = var_output_model.suggested_cobol_variable_name
                            suggested_type = var_output_model.suggested_cobol_variable_type
                        else:
                            log.warning(f"M204_SERVICE_VAR_LLM: Mismatched variable name from LLM for {variable_name_for_llm}. LLM response: {result_or_exc.text}")
                    except Exception as e_parse_llm:
                        log.error(f"M204_SERVICE_VAR_LLM: Error parsing LLM response for variable {variable_name_for_llm}: {e_parse_llm}. Raw: '{result_or_exc.text}'", exc_info=True)
                target_var_data["suggested_cobol_name"] = suggested_name
                target_var_data["suggested_cobol_variable_type"] = suggested_type

    # Database operations
    for var_data in parsed_variables_data:
        var_name = var_data["var_name"]
        scope = var_data["scope"]
        current_proc_id_for_line = var_data["current_proc_id_for_line"]
        line_num = var_data["line_num"]
        
        existing_var_query = db.query(M204Variable).filter_by(
            project_id=input_source.project_id,
            input_source_id=input_source.input_source_id,
            variable_name=var_name,
            scope=scope
        )
        if current_proc_id_for_line is not None:
            existing_var_query = existing_var_query.filter(M204Variable.procedure_id == current_proc_id_for_line)
        else:
            existing_var_query = existing_var_query.filter(M204Variable.procedure_id.is_(None))
        
        existing_var = existing_var_query.first()

        var_schema_data_dict = {
            "project_id": input_source.project_id,
            "input_source_id": input_source.input_source_id,
            "procedure_id": current_proc_id_for_line,
            "variable_name": var_name,
            "variable_type": var_data["declared_m204_type"], # Use the parsed M204 type
            "scope": scope,
            "attributes": var_data["parsed_declaration_attributes"],
            "definition_line_number": line_num,
            "cobol_mapped_variable_name": var_data["suggested_cobol_name"],
            "cobol_variable_type": var_data["suggested_cobol_variable_type"],
        }

        try:
            var_create_schema = M204VariableCreateSchema(**var_schema_data_dict)
        except Exception as e_schema:
            log.error(f"M204_SERVICE: Schema validation error for M204Variable '{var_name}': {e_schema}", exc_info=True)
            continue

        if existing_var:
            updated = False
            # Ensure all fields from the schema are checked and updated
            update_data = var_create_schema.model_dump(exclude_none=True)
            for key, value in update_data.items():
                if getattr(existing_var, key) != value:
                    setattr(existing_var, key, value)
                    updated = True
            
            # Explicitly check if cobol_variable_type needs to be set to None if not in update_data but exists in DB
            if 'cobol_variable_type' not in update_data and existing_var.cobol_variable_type is not None:
                existing_var.cobol_variable_type = None
                updated = True
            elif 'cobol_variable_type' in update_data and existing_var.cobol_variable_type != update_data['cobol_variable_type']:
                existing_var.cobol_variable_type = update_data['cobol_variable_type']
                updated = True


            if updated:
                db.add(existing_var)
                log.info(f"M204_SERVICE: Updating existing M204Variable definition '{var_name}' (ID: {existing_var.variable_id}) from line {line_num}.")
            variables_processed_for_db.append(existing_var)
        else:
            try:
                db_variable = M204Variable(**var_create_schema.model_dump(exclude_none=True))
                db.add(db_variable)
                variables_processed_for_db.append(db_variable)
                log.info(f"M204_SERVICE: Created new M204Variable definition '{var_name}' from line {line_num}.")
            except Exception as e_create:
                log.error(f"M204_SERVICE: Error creating M204Variable definition '{var_name}': {e_create}", exc_info=True)
            
    log.info(f"M204_SERVICE: Finished extracting and processing {len(variables_processed_for_db)} explicitly defined M204 variables for file ID: {input_source.input_source_id}.")
    return variables_processed_for_db



async def _extract_and_store_m204_procedure_calls(
    db: Session, input_source: InputSource, file_content: str, defined_procedures_in_file: List[Procedure]
) -> List[ProcedureCall]:
    log.info(f"M204_SERVICE: Extracting M204 procedure calls for file ID: {input_source.input_source_id}.")
    procedure_calls_created = []
    lines = file_content.splitlines()
    call_pattern = re.compile(r"^\s*CALL\s+([A-Z0-9_#@$-]+)(?:\s*\(.*?\))?", re.IGNORECASE | re.MULTILINE)

    for i, line_content in enumerate(lines):
        line_num = i + 1
        match = call_pattern.match(line_content)
        if not match:
            continue

        current_calling_proc_id_for_line: Optional[int] = None
        # Find the procedure that contains the current line number by checking all defined procedures
        for proc in defined_procedures_in_file:
            if proc.start_line_in_source and proc.end_line_in_source:
                if proc.start_line_in_source <= line_num <= proc.end_line_in_source:
                    current_calling_proc_id_for_line = proc.proc_id
                    break  # Found the containing procedure, no need to check others

        called_proc_name = match.group(1)

        existing_call = db.query(ProcedureCall).filter_by(
            project_id=input_source.project_id,
            calling_input_source_id=input_source.input_source_id,
            calling_procedure_id=current_calling_proc_id_for_line,
            called_procedure_name=called_proc_name,
            line_number=line_num
        ).first()

        if existing_call:
            # If you need to update existing calls, the logic would go here.
            # For now, we just append it to the list of calls processed in this run.
            procedure_calls_created.append(existing_call)
            continue

        try:
            call_data = M204ProcedureCallCreateSchema(
                project_id=input_source.project_id,
                calling_input_source_id=input_source.input_source_id,
                calling_procedure_id=current_calling_proc_id_for_line,
                called_procedure_name=called_proc_name,
                line_number=line_num,
                is_external=None # Will be resolved later
            )
            db_call = ProcedureCall(**call_data.model_dump(exclude_none=True))
            db.add(db_call)
            procedure_calls_created.append(db_call)
            log.debug(f"M204_SERVICE: Created new procedure call to '{called_proc_name}' at line {line_num}.")
        except Exception as e:
            log.error(f"M204_SERVICE: Error creating DB entry for procedure call to '{called_proc_name}' at line {line_num}: {e}", exc_info=True)

    log.info(f"M204_SERVICE: Finished extracting {len(procedure_calls_created)} procedure calls for file ID {input_source.input_source_id}.")
    return procedure_calls_created

async def _resolve_procedure_calls(db: Session, project_id: int, calls_in_file: List[ProcedureCall], procs_in_file: List[Procedure]):
    log.info(f"M204_SERVICE: Resolving internal/external status for {len(calls_in_file)} procedure calls in project {project_id}.")
    proc_names_in_current_file = {p.m204_proc_name: p.proc_id for p in procs_in_file if p.proc_id is not None}
    
    for call in calls_in_file:
        original_is_external = call.is_external
        original_resolved_id = call.resolved_procedure_id
        update_call_needed = False

        if call.called_procedure_name in proc_names_in_current_file:
            call.is_external = False
            call.resolved_procedure_id = proc_names_in_current_file[call.called_procedure_name]
            log.debug(f"M204_SERVICE: Call to '{call.called_procedure_name}' resolved internally to ProcID {call.resolved_procedure_id} within the same file.")
        else:
            external_proc = db.query(Procedure.proc_id).filter(
                Procedure.project_id == project_id,
                Procedure.m204_proc_name == call.called_procedure_name,
                Procedure.input_source_id != call.calling_input_source_id 
            ).first()
            
            if external_proc:
                call.is_external = True 
                call.resolved_procedure_id = external_proc.proc_id
                log.debug(f"M204_SERVICE: Call to '{call.called_procedure_name}' resolved externally to ProcID {call.resolved_procedure_id} in another file within project.")
            else:
                call.is_external = True 
                call.resolved_procedure_id = None
                log.debug(f"M204_SERVICE: Call to '{call.called_procedure_name}' is external and not found in other project files. Marked as unresolved.")
        
        if call.is_external != original_is_external or call.resolved_procedure_id != original_resolved_id:
            update_call_needed = True
        
        if update_call_needed:
            db.add(call)

# --- IMAGE Statement Extraction ---


async def _parse_image_definition(
    image_name_context: str,
    image_content: str
) -> Dict[str, Any]:
    """
    Parse IMAGE definition content to extract field information for COBOL FDs,
    including LLM-suggested COBOL field names (LLM calls made in parallel,
    bounded by a semaphore).
    """
    import re
    import asyncio
    import json

    # Semaphore to bound concurrent LLM calls per image
    llm_semaphore = asyncio.Semaphore(10)

    # 1) Extract raw field lines
    parsed_field_details = []
    lines = image_content.strip().split('\n')
    for line in lines:
        line = line.strip()
        if not line or line.upper().startswith('END IMAGE'):
            continue

        field_match = re.match(
            r"([A-Z0-9_.#@$-]+)\s+IS\s+([A-Z]+)"
            r"(?:\s+LEN\s+(\d+))?"
            r"(?:\s+DIGITS\s+(\d+))?"
            r"(?:\s+DP\s+(\d+))?",
            line,
            re.IGNORECASE
        )
        if not field_match:
            continue

        parsed_field_details.append({
            "m204_field_name_original": field_match.group(1),
            "m204_type":               field_match.group(2).upper(),
            "length":                  int(field_match.group(3))
                                        if field_match.group(3) else None,
            "digits":                  int(field_match.group(4))
                                        if field_match.group(4) else None,
            "decimal_places":          int(field_match.group(5))
                                        if field_match.group(5) else None,
            "original_index":          len(parsed_field_details)
        })

    # 2) Fan-out LLM calls in parallel, bounded by semaphore
    llm_results_map: Dict[int, Any] = {}
    if llm_config._llm and parsed_field_details:
        field_namer_llm = (
            llm_config._llm
            .as_structured_llm(ImageFieldToCobolOutput)
        )

        async def call_llm(field_data):
            prompt = f"""
You are an M204→COBOL migration expert. Suggest a COBOL-compliant
field name for this M204 field.
M204 IMAGE: {image_name_context}
M204 Field: {field_data['m204_field_name_original']}
Type: {field_data['m204_type']}
Length: {field_data['length'] or 'N/A'}
Digits: {field_data['digits'] or 'N/A'}
DP: {field_data['decimal_places'] or 'N/A'}

Respond with JSON per ImageFieldToCobolOutput.
"""
            async with llm_semaphore:
                return await field_namer_llm.acomplete(prompt=prompt)

        tasks = [call_llm(fd) for fd in parsed_field_details]
        raw_results = await asyncio.gather(*tasks, return_exceptions=True)

        for idx, result in enumerate(raw_results):
            fd = parsed_field_details[idx]
            orig = fd["m204_field_name_original"]
            fallback = orig.upper().replace('.', '-')
            suggested = fallback
            reasoning = None

            if isinstance(result, Exception):
                log.error(
                    f"LLM error for {image_name_context}.{orig}",
                    exc_info=True
                )
            else:
                try:
                    data = json.loads(strip_markdown_code_block(result.text))
                    out = ImageFieldToCobolOutput(**data)
                    if (out.m204_image_name == image_name_context
                            and out.m204_field_name == orig):
                        suggested = out.suggested_cobol_field_name
                        reasoning = out.reasoning
                    else:
                        log.warning(
                            f"LLM returned mismatched names for "
                            f"{image_name_context}.{orig}; using fallback"
                        )
                except Exception:
                    log.error(
                        f"Error parsing LLM JSON for {image_name_context}.{orig}",
                        exc_info=True
                    )

            llm_results_map[fd["original_index"]] = {
                "suggested": suggested,
                "reasoning": reasoning
            }

    # 3) Build final fields output
    final_fields_output = []
    for fd in parsed_field_details:
        idx = fd["original_index"]
        m204_type = fd["m204_type"]
        length = fd["length"]
        digits = fd["digits"]
        decp = fd["decimal_places"]

        # Data type mapping
        type_map = {
            'STRING': {'cobol_type': 'CHARACTER',      'pic': 'PIC X'},
            'PACKED': {'cobol_type': 'PACKED_DECIMAL', 'pic': 'PIC 9 COMP-3'},
            'BINARY': {'cobol_type': 'BINARY',         'pic': 'PIC 9 COMP'},
            'FLOAT':  {'cobol_type': 'FLOATING_POINT', 'pic': 'COMP-1'},
            'DOUBLE': {'cobol_type': 'FLOATING_POINT', 'pic': 'COMP-2'},
        }.get(m204_type, {'cobol_type': m204_type, 'pic': 'PIC X'})

        # PIC and byte-length logic
        suggested_pic = None
        byte_len = length
        if m204_type == 'STRING' and length:
            suggested_pic = f"PIC X({length})"
        elif m204_type == 'PACKED' and digits:
            if decp:
                suggested_pic = (
                    f"PIC S9({digits-decp})V9({decp}) COMP-3"
                )
            else:
                suggested_pic = f"PIC S9({digits}) COMP-3"
            byte_len = (digits // 2) + 1
        elif m204_type == 'BINARY' and length:
            if length == 2:
                suggested_pic = "PIC S9(4) COMP"
            elif length == 4:
                suggested_pic = "PIC S9(9) COMP"
            elif length == 8:
                suggested_pic = "PIC S9(18) COMP"
            else:
                suggested_pic = f"PIC S9({length*2-1}) COMP"
        elif m204_type == 'FLOAT':
            suggested_pic = "COMP-1"
            byte_len = 4
        elif m204_type == 'DOUBLE':
            suggested_pic = "COMP-2"
            byte_len = 8

        llm_sugg = llm_results_map.get(idx, {})
        final_fields_output.append({
            "field_name": fd["m204_field_name_original"],
            "suggested_cobol_field_name": llm_sugg.get("suggested"),
            "data_type": type_map['cobol_type'],
            "m204_type": m204_type,
            "length": length,
            "digits": digits,
            "decimal_places": decp,
            "position": idx + 1,
            "cobol_layout_suggestions": {
                "cobol_picture_clause": suggested_pic,
                "field_byte_length": byte_len,
                "reasoning": (
                    llm_sugg.get("reasoning")
                    or f"Based on M204 type {m204_type}"
                )
            }
        })

    return {"fields": final_fields_output,
            "total_fields": len(final_fields_output)}



# async def _extract_and_store_m204_image_statements(
#     db: Session, input_source: InputSource, file_content: str
# ) -> List[M204ImageDefinition]:
#     """
#     REFACTORED: Extracts IMAGE statements from M204 source and stores them as
#     independent M204ImageDefinition records. It no longer tries to link them
#     to M204File records directly.
#     """
#     log.info(f"M204_SERVICE: Extracting IMAGE statements as definitions for file ID: {input_source.input_source_id} ({input_source.original_filename})")
#     m204_images_created = []
#     lines = file_content.splitlines()
#     image_start_pattern = re.compile(r"^\s*IMAGE\s+([A-Z0-9_.#@$-]+)", re.IGNORECASE)
    
#     i = 0
#     while i < len(lines):
#         line_content_for_match = lines[i] 
#         current_line_num_for_start = i + 1
        
#         image_match = image_start_pattern.match(line_content_for_match.strip())
#         if image_match:
#             image_name_from_statement = image_match.group(1)
#             log.debug(f"M204_SERVICE_IMG: Found IMAGE statement start: '{image_name_from_statement}' at line {current_line_num_for_start} in file '{input_source.original_filename}'.")
            
#             temp_image_content_lines = []
#             found_end_image = False
#             image_content_end_line_idx = i
            
#             for j in range(i + 1, len(lines)):
#                 if lines[j].strip().upper() == 'END IMAGE':
#                     image_content_end_line_idx = j
#                     found_end_image = True
#                     break
#                 temp_image_content_lines.append(lines[j])
            
#             if not found_end_image:
#                 log.warning(f"M204_SERVICE_IMG: IMAGE '{image_name_from_statement}' at line {current_line_num_for_start} did not have a corresponding 'END IMAGE'. Skipping.")
#                 i += 1
#                 continue

#             image_block_content_for_parse = "\n".join(temp_image_content_lines)
#             parsed_image_fields_data = await _parse_image_definition(image_name_from_statement, image_block_content_for_parse)
            
#             if parsed_image_fields_data and parsed_image_fields_data.get("fields"):
#                 existing_image_def = db.query(M204ImageDefinition).filter(
#                     M204ImageDefinition.project_id == input_source.project_id,
#                     M204ImageDefinition.image_name == image_name_from_statement,
#                     M204ImageDefinition.input_source_id == input_source.input_source_id
#                 ).first()

#                 if not existing_image_def:
#                     new_image_def = M204ImageDefinition(
#                         project_id=input_source.project_id,
#                         input_source_id=input_source.input_source_id,
#                         image_name=image_name_from_statement,
#                         start_line_number=current_line_num_for_start,
#                         end_line_number=image_content_end_line_idx + 1,
#                         fields_json=parsed_image_fields_data
#                     )
#                     db.add(new_image_def)
#                     m204_images_created.append(new_image_def)
#                     log.info(f"M204_SERVICE_IMG: Created new M204ImageDefinition for '{image_name_from_statement}'.")
#                 else:
#                     existing_image_def.fields_json = parsed_image_fields_data
#                     db.add(existing_image_def)
#                     m204_images_created.append(existing_image_def)
#                     log.info(f"M204_SERVICE_IMG: Updated existing M204ImageDefinition for '{image_name_from_statement}'.")
            
#             i = image_content_end_line_idx + 1
#             continue
        
#         i += 1
#     log.info(f"M204_SERVICE: Finished extracting {len(m204_images_created)} IMAGE definitions for file ID: {input_source.input_source_id}.")
#     return m204_images_created


async def _extract_and_store_m204_image_statements(
    db: Session,
    input_source: InputSource,
    file_content: str
) -> List[M204ImageDefinition]:
    """
    Extract all IMAGE…END IMAGE blocks, parse them in parallel,
    and upsert M204ImageDefinition records.
    """
    log.info(
        f"M204_SERVICE: Bulk extracting IMAGE blocks for file "
        f"ID={input_source.input_source_id}"
    )
    lines = file_content.splitlines()
    start_re = re.compile(r"^\s*IMAGE\s+([A-Z0-9_.#@$-]+)", re.IGNORECASE)

    # 1) Collect IMAGE blocks
    blocks: List[Tuple[str,int,int,str]] = []
    i = 0
    while i < len(lines):
        m = start_re.match(lines[i].strip())
        if not m:
            i += 1
            continue

        name = m.group(1)
        start_ln = i + 1
        body: List[str] = []
        end_ln = None

        for j in range(i + 1, len(lines)):
            if lines[j].strip().upper() == "END IMAGE":
                end_ln = j + 1
                break
            body.append(lines[j])

        if end_ln is None:
            log.warning(
                f"M204_SERVICE_IMG: IMAGE '{name}' @ line "
                f"{start_ln} has no END IMAGE, skipping."
            )
            i += 1
            continue

        blocks.append((name, start_ln, end_ln, "\n".join(body)))
        i = end_ln

    log.info(f"M204_SERVICE: Found {len(blocks)} IMAGE blocks.")

    # 2) Parse each block concurrently
    parse_tasks = [
        _parse_image_definition(name, text)
        for name, _, _, text in blocks
    ]
    parsed_list = await asyncio.gather(*parse_tasks)

    # 3) Upsert results
    results: List[M204ImageDefinition] = []
    for (name, s_ln, e_ln, _), parsed in zip(blocks, parsed_list):
        if not parsed or not parsed.get("fields"):
            continue

        existing = (
            db.query(M204ImageDefinition)
              .filter_by(
                  project_id      = input_source.project_id,
                  input_source_id = input_source.input_source_id,
                  image_name      = name
              )
              .first()
        )
        if existing:
            existing.fields_json      = parsed
            existing.start_line_number= s_ln
            existing.end_line_number  = e_ln
            db.add(existing)
            results.append(existing)
            log.info(f"Updated IMAGE '{name}'")
        else:
            new_def = M204ImageDefinition(
                project_id       = input_source.project_id,
                input_source_id  = input_source.input_source_id,
                image_name       = name,
                start_line_number= s_ln,
                end_line_number  = e_ln,
                fields_json      = parsed
            )
            db.add(new_def)
            results.append(new_def)
            log.info(f"Created IMAGE '{name}'")

    log.info(
        f"M204_SERVICE: Upserted {len(results)} IMAGE definitions for "
        f"file ID={input_source.input_source_id}"
    )
    return results

async def _link_images_to_files(db: Session, input_source: InputSource, file_content: str) -> List[M204File]:
    """
    NEW: Scans source code for commands that link an IMAGE to a FILE (e.g., READ IMAGE...FROM)
    and copies the image layout into the file's file_definition_json.
    """
    log.info(f"M204_LINKER: Starting IMAGE to FILE linkage analysis for {input_source.original_filename}")
    updated_files = []
    link_pattern = re.compile(
        r"^\s*(?:READ|WRITE)\s+IMAGE\s+([A-Z0-9_#@$-]+)\s+(?:FROM|TO)\s+([A-Z0-9_#@$-.]+)", # Allow '.' in file name
        re.IGNORECASE | re.MULTILINE
    )
    # Import flag_modified and other necessary modules
    from sqlalchemy.orm.attributes import flag_modified
    from sqlalchemy import or_

    for match in link_pattern.finditer(file_content):
        image_name = match.group(1)
        file_name_from_usage = match.group(2) # This will be 'MORTGAGE.INPUT'
        log.info(f"M204_LINKER: Found potential link: IMAGE '{image_name}' with FILE '{file_name_from_usage}'")

        image_def = db.query(M204ImageDefinition).filter(
            M204ImageDefinition.project_id == input_source.project_id,
            func.upper(M204ImageDefinition.image_name) == image_name.upper()
        ).first()

        # CORRECTED QUERY: Check against both logical name and file name (DDNAME)
        m204_file = db.query(M204File).filter(
            M204File.project_id == input_source.project_id,
            or_(
                func.upper(M204File.m204_logical_dataset_name) == file_name_from_usage.upper(),
                func.upper(M204File.m204_file_name) == file_name_from_usage.upper()
            )
        ).first()

        if image_def and m204_file:
            log.info(f"M204_LINKER: Matched IMAGE '{image_name}' to M204File '{m204_file.m204_file_name}' (Logical: {m204_file.m204_logical_dataset_name}). Updating JSON.")
            
            image_definition_for_json = {
                "image_name": image_def.image_name,
                "source_type": "IMAGE_STATEMENT",
                "fields": image_def.fields_json.get("fields", []) if image_def.fields_json else []
            }

            updated_json = m204_file.file_definition_json or {}
            if "image_definitions" not in updated_json:
                updated_json["image_definitions"] = []
            
            # Check for duplicates before appending
            existing_images = [img.get("image_name") for img in updated_json["image_definitions"]]
            if image_definition_for_json["image_name"] not in existing_images:
                updated_json["image_definitions"].append(image_definition_for_json)
                m204_file.file_definition_json = updated_json
                # Mark the JSON field as modified
                flag_modified(m204_file, "file_definition_json")
                db.add(m204_file)
                if m204_file not in updated_files:
                    updated_files.append(m204_file)
                log.info(f"M204_LINKER: Appended layout from IMAGE '{image_name}' to M204File '{m204_file.m204_file_name}'.")
            else:
                log.info(f"M204_LINKER: Layout from IMAGE '{image_name}' already exists in M204File '{m204_file.m204_file_name}'. Skipping append.")
        else:
            if not image_def:
                log.warning(f"M204_LINKER: Could not find a parsed M204ImageDefinition for IMAGE '{image_name}'.")
            if not m204_file:
                log.warning(f"M204_LINKER: Could not find a defined M204File for FILE '{file_name_from_usage}'.")

    return updated_files
    
# --- Main Processing Function ---


async def process_m204_analysis(
    db: Session, 
    input_source: InputSource, 
    file_content: str, 
    rag_service: Optional[RagService]
) -> Tuple[M204AnalysisResultDataSchema, List[M204File]]:
    """
    Main function to process M204 source file content for structural analysis.
    The function will only return after all analysis is complete.
    Returns the analysis results and a list of M204File objects identified as DB files.
    Main loop and description generation are handled separately by the orchestrator.
    """
    log.info(f"M204_SERVICE: Starting M204 structural processing for file: {input_source.original_filename} (ID: {input_source.input_source_id})")

    # --- Extract OPEN statements ---
    extracted_open_statements = await _extract_and_store_m204_open_statements(db, input_source, file_content)

    extracted_procedures = await _extract_and_store_m204_procedures(db, input_source, file_content, rag_service)
    log.debug(f"M204_SERVICE: Flushing session after procedure extraction for {input_source.original_filename} to ensure IDs are available.")
    db.flush()
    log.debug(f"M204_SERVICE: Session flushed. {len(extracted_procedures)} procedures processed initially.")

    extracted_dataset_files = await _extract_and_store_m204_datasets(db, input_source, file_content) 

    # --- Logging M204File table contents before IMAGE statement processing ---
    try:
        log.info(f"M204_SERVICE_PRE_IMG_LOG: Querying M204File table for project_id {input_source.project_id} before IMAGE statement processing for file {input_source.original_filename} (ID: {input_source.input_source_id}).")
        db.flush() 
        m204_files_in_db_before_image = db.query(M204File).filter(M204File.project_id == input_source.project_id).all()
        if m204_files_in_db_before_image:
            log.info(f"M204_SERVICE_PRE_IMG_LOG: Found {len(m204_files_in_db_before_image)} M204File records in project {input_source.project_id} (after potential flush):")
            for idx, f_db_log in enumerate(m204_files_in_db_before_image):
                log.info(
                    f"M204_SERVICE_PRE_IMG_LOG: Record {idx + 1}: "
                    f"ID={f_db_log.m204_file_id}, "
                    f"FileName='{f_db_log.m204_file_name}', "
                    f"LogicalName='{f_db_log.m204_logical_dataset_name}', "
                    f"DefinedInSourceID={f_db_log.defined_in_input_source_id}, "
                    f"Attributes='{f_db_log.m204_attributes[:100].replace(chr(10), ' ') if f_db_log.m204_attributes else 'N/A'}...', "
                    f"IsDBFile={f_db_log.is_db_file}, "
                    f"FileDefJSON_keys='{list(f_db_log.file_definition_json.keys()) if f_db_log.file_definition_json else None}'"
                )
        else:
            log.info(f"M204_SERVICE_PRE_IMG_LOG: No M204File records found in project {input_source.project_id} (after potential flush) before IMAGE statement processing.")
        
        if extracted_dataset_files:
            log.info(f"M204_SERVICE_PRE_IMG_LOG: {len(extracted_dataset_files)} M204File objects in 'extracted_dataset_files' list (from current run):")
            for idx, f_mem in enumerate(extracted_dataset_files):
                 log.info(
                    f"M204_SERVICE_PRE_IMG_LOG: In-memory object {idx + 1}: "
                    f"ID={f_mem.m204_file_id if hasattr(f_mem, 'm204_file_id') and f_mem.m204_file_id is not None else 'N/A (pre-flush or no ID yet)'}, "
                    f"FileName='{f_mem.m204_file_name}', "
                    f"LogicalName='{f_mem.m204_logical_dataset_name}', "
                    f"DefinedInSourceID={f_mem.defined_in_input_source_id}"
                )
        else:
            log.info("M204_SERVICE_PRE_IMG_LOG: 'extracted_dataset_files' list is empty (from current run).")

    except Exception as e_log_table:
        log.error(f"M204_SERVICE_PRE_IMG_LOG: Error logging M204File table contents: {e_log_table}", exc_info=True)
    # --- End of logging M204File table contents ---

    # REFACTORED: This now saves to the new M204ImageDefinition table and doesn't return files.
    await _extract_and_store_m204_image_statements(db, input_source, file_content)
    
    # FLUSH a second time to ensure M204ImageDefinition records are available for the linker.
    log.debug(f"M204_SERVICE: Flushing session after IMAGE statement extraction for {input_source.original_filename} to ensure definitions are queryable.")
    db.flush()

    log.info(f"M204_SERVICE: Starting parallel extraction of variables and procedure calls for {input_source.original_filename}")
    variable_task = _extract_and_store_m204_variables(db, input_source, file_content, extracted_procedures, rag_service)
    procedure_call_task = _extract_and_store_m204_procedure_calls(db, input_source, file_content, extracted_procedures)
    
    results = await asyncio.gather(variable_task, procedure_call_task, return_exceptions=True)
    log.info(f"M204_SERVICE: Finished parallel extraction of variables and procedure calls for {input_source.original_filename}")

    extracted_variables: List[M204Variable] = []
    extracted_procedure_calls: List[ProcedureCall] = []

    if isinstance(results[0], Exception):
        log.error(f"M204_SERVICE: Error during parallel variable extraction for file {input_source.original_filename}: {results[0]}", exc_info=results[0])
    elif results[0] is not None:
        extracted_variables = results[0]

    if isinstance(results[1], Exception):
        log.error(f"M204_SERVICE: Error during parallel procedure call extraction for file {input_source.original_filename}: {results[1]}", exc_info=results[1])
    elif results[1] is not None:
        extracted_procedure_calls = results[1]
    
    await _resolve_procedure_calls(db, input_source.project_id, extracted_procedure_calls, extracted_procedures)

    # NEW STEP: After all elements are parsed, link IMAGEs to FILEs based on usage.
    log.info(f"M204_SERVICE: Starting IMAGE to FILE linkage analysis for {input_source.original_filename}")
    updated_m204_files_from_linking = await _link_images_to_files(db, input_source, file_content)
    log.info(f"M204_SERVICE: Finished IMAGE to FILE linkage analysis. {len(updated_m204_files_from_linking)} files were updated with IMAGE layouts.")

    # --- Description Generation is now handled by the orchestrator ---

    all_m204_files_map: Dict[Any, M204File] = {} 
    for f_list in [extracted_dataset_files, updated_m204_files_from_linking]:
        for f_obj in f_list:
            obj_key_for_map = f_obj.m204_file_id if hasattr(f_obj, 'm204_file_id') and f_obj.m204_file_id is not None else id(f_obj)
            if obj_key_for_map not in all_m204_files_map:
                all_m204_files_map[obj_key_for_map] = f_obj
    
    all_extracted_orm_objects = list(extracted_procedures) + \
                                list(all_m204_files_map.values()) + \
                                list(extracted_variables) + \
                                list(extracted_procedure_calls)
    
    refreshed_items = []
    for item in all_extracted_orm_objects:
        try:
            item_id_attr = None
            if hasattr(item, 'proc_id'): 
                item_id_attr = 'proc_id'
            elif hasattr(item, 'm204_file_id'): 
                item_id_attr = 'm204_file_id'
            elif hasattr(item, 'variable_id'): 
                item_id_attr = 'variable_id'
            elif hasattr(item, 'call_id'): 
                item_id_attr = 'call_id'
            elif hasattr(item, 'image_definition_id'):
                item_id_attr = 'image_definition_id'

            if item not in db and not db.object_session(item): 
                db.add(item) 

            if db.is_modified(item) or item in db.new:
                 log.debug(f"M204_SERVICE: Flushing item {type(item).__name__} (obj_id: {id(item)}) before refresh.")
                 db.flush([item]) 
            
            current_item_id = getattr(item, item_id_attr) if item_id_attr else None

            if item in db and current_item_id is not None:
                log.debug(f"M204_SERVICE: Refreshing item {type(item).__name__} with ID {current_item_id}.")
                db.refresh(item)
                refreshed_items.append(item)
            elif item in db: 
                log.debug(f"M204_SERVICE: Item {type(item).__name__} (obj_id: {id(item)}) is in session but might not be refreshable (e.g., no PK {current_item_id}). Appending as is.")
                refreshed_items.append(item) 
            else: 
                log.warning(f"M204_SERVICE: Item {type(item).__name__} (obj_id: {id(item)}) was not in session after potential flush. Using potentially uncommitted data for response.")
                refreshed_items.append(item) 

        except Exception as e_refresh:
            log.warning(f"M204_SERVICE: Could not refresh item {type(item).__name__} (obj_id: {id(item)}) during M204 processing: {e_refresh}. Using potentially uncommitted data for response.", exc_info=True)
            refreshed_items.append(item) 

    refreshed_procedures = [p for p in refreshed_items if isinstance(p, Procedure)]
    refreshed_m204_files = [f for f in refreshed_items if isinstance(f, M204File)]
    refreshed_variables = [v for v in refreshed_items if isinstance(v, M204Variable)]
    refreshed_procedure_calls = [pc for pc in refreshed_items if isinstance(pc, ProcedureCall)]

    procedure_responses = [M204ProcedureResponseSchema.model_validate(p) for p in refreshed_procedures]
    m204_file_responses = [M204FileResponseSchema.model_validate(df) for df in refreshed_m204_files]
    variable_responses = [M204VariableResponseSchema.model_validate(v) for v in refreshed_variables]
    procedure_call_responses = [M204ProcedureCallResponseSchema.model_validate(pc) for pc in refreshed_procedure_calls]
    open_statement_responses = [M204OpenStatementResponseSchema.model_validate(o) for o in extracted_open_statements]
    
    m204_db_files_identified = [mf for mf in refreshed_m204_files if mf.is_db_file is True]

    analysis_result_data = M204AnalysisResultDataSchema(
        procedures_found=procedure_responses,
        defined_files_found=m204_file_responses, 
        defined_fields_found=[], 
        variables_found=variable_responses,
        procedure_calls_found=procedure_call_responses,
        open_statements_found=open_statement_responses,
    )

    log.info(f"M204_SERVICE: Completed M204 analysis for file: {input_source.original_filename}. Identified {len(m204_db_files_identified)} DB files for potential VSAM enhancement.")
    return analysis_result_data, m204_db_files_identified

async def enhance_m204_db_file_with_vsam_suggestions(db: Session, m204_file: M204File):
    """
    Updates an M204File (only if is_db_file is True) and its JSON field definitions with VSAM suggestions.
    """
    if not llm_config._llm:
        log.warning(f"M204_VSAM_ENHANCE: LLM not available. Skipping VSAM enhancement for M204 file: {m204_file.m204_file_name}")
        return
    
    # This function should ONLY operate on files confirmed to be M204 Database Files
    if not m204_file.is_db_file:
        log.debug(f"M204_VSAM_ENHANCE: M204 file {m204_file.m204_file_name} (ID: {m204_file.m204_file_id}) is not a DB file (is_db_file is False or None). Skipping VSAM enhancement.")
        return

    log.info(f"M204_VSAM_ENHANCE: Attempting LLM-based VSAM enhancement for M204 DB file: {m204_file.m204_file_name} (ID: {m204_file.m204_file_id})")
    
    db.refresh(m204_file) # Get latest state
    
    file_attributes_from_define_dataset = m204_file.m204_attributes or "Not defined via DEFINE DATASET."
    
    fields_context_list = []
    if m204_file.file_definition_json:
        # Check for image definitions first, as they are more likely for non-parmlib files
        if "image_definitions" in m204_file.file_definition_json:
            for image_def in m204_file.file_definition_json.get("image_definitions", []):
                for field in image_def.get("fields", []):
                    fields_context_list.append({
                        "name": field.get("field_name"),
                        "attributes_text": f"Type: {field.get('m204_type')}, Length: {field.get('length')}",
                        "current_cobol_picture_clause": field.get("cobol_layout_suggestions", {}).get("cobol_picture_clause"),
                        "current_vsam_length": field.get("cobol_layout_suggestions", {}).get("field_byte_length")
                    })
        # Then check for parmlib structure
        elif m204_file.file_definition_json.get('file_type') == "db_file" and m204_file.file_definition_json.get('source') == "parmlib":
            fields = m204_file.file_definition_json.get('fields', {})
            for field_name, field_data in fields.items():
                vsam_suggestions = field_data.get('vsam_suggestions', {}) 
                fields_context_list.append({
                    "name": field_name,
                    "attributes_text": ', '.join(field_data.get('attributes', [])),
                    "current_is_key_component": vsam_suggestions.get('is_key_component', False),
                    "current_key_order": vsam_suggestions.get('key_order'),
                    "current_cobol_picture_clause": vsam_suggestions.get('cobol_picture_clause'),
                    "current_vsam_length": vsam_suggestions.get('vsam_length')
                })
        else:
            log.warning(f"M204_VSAM_ENHANCE: M204File '{m204_file.m204_file_name}' is_db_file=True, but its file_definition_json does not match known structures (parmlib or image). JSON: {m204_file.file_definition_json}. Skipping VSAM field analysis.")
    
    fields_context_str = json.dumps(fields_context_list, indent=2) if fields_context_list else "No PARMLIB or IMAGE fields defined or found for this file."
    
    prompt_fstr = f"""
You are an expert Mainframe M204 to COBOL/VSAM migration specialist.
Analyze the following M204 database file information.
Your goal is to suggest its VSAM organization and refine key structure and field attributes for VSAM.

M204 File Name (DDNAME): {m204_file.m204_file_name}
M204 Logical Dataset Name (if from DEFINE DATASET): {m204_file.m204_logical_dataset_name or "N/A"}
M204 File 'DEFINE DATASET' Attributes (if available):
{file_attributes_from_define_dataset}

Defined Fields (from PARMLIB or IMAGE definitions), including any current VSAM-related suggestions:
```json
{fields_context_str}
```

Based on this information, considering both the original M204 field attributes and any 'current_is_key_component', 'current_key_order', 'current_cobol_picture_clause', 'current_vsam_length' suggestions from the input JSON:

1.  Suggest the most appropriate VSAM file organization (KSDS, ESDS, RRDS, or LDS) for this M204 database file.
2.  For each field listed in the input `Defined Fields` JSON, provide refined suggestions for its role in a VSAM structure:
    *   `m204_field_name`: string (Must match one of the input field names from the "name" key in the `Defined Fields` JSON)
    *   `is_key_component`: boolean (true if this field should be part of the primary key for KSDS/RRDS)
    *   `key_order`: integer (1-based order if it's a key component. Null if not a key component.)
    *   `cobol_picture_clause`: string (suggest/confirm a COBOL PICTURE clause like "PIC X(10)", "PIC 9(7) COMP-3". Null if not applicable.)
    *   `vsam_length`: integer (suggest/confirm length in bytes for VSAM. Null if not applicable.)
    *   `reasoning`: string (briefly why you made these suggestions for this field, especially if refining previous suggestions)

Respond with a JSON object structured according to the M204FileVsamAnalysisOutput model.
Ensure `m204_field_name` in `field_specific_suggestions` matches one of the input field names.
If no fields are provided or applicable for key structure (e.g., for ESDS), `field_specific_suggestions` can be an empty list.
For KSDS, identify the primary key field(s).
Your suggestions for fields should be the final determination, considering all available information.
"""
    json_text_output: Optional[str] = None
    try:
        vsam_analyzer_llm = llm_config._llm.as_structured_llm(M204FileVsamAnalysisOutput)
        completion_response = await vsam_analyzer_llm.acomplete(prompt=prompt_fstr)
        json_text_output = completion_response.text
        
        loaded_vsam_data = json.loads(strip_markdown_code_block(json_text_output))
        vsam_output = M204FileVsamAnalysisOutput(**loaded_vsam_data)

        if vsam_output.m204_file_name != m204_file.m204_file_name:
            log.warning(f"M204_VSAM_ENHANCE: LLM returned VSAM data for '{vsam_output.m204_file_name}' but expected '{m204_file.m204_file_name}'. Skipping update for this file.")
            return

        m204_file.target_vsam_type = vsam_output.suggested_vsam_type
        log.info(f"M204_VSAM_ENHANCE: LLM suggested VSAM type for {m204_file.m204_file_name}: {m204_file.target_vsam_type}. Reasoning: {vsam_output.overall_reasoning or 'N/A'}")

        if m204_file.file_definition_json and vsam_output.field_specific_suggestions:
            file_definition = m204_file.file_definition_json
            primary_key_components = []
            
            # Handle PARMLIB structure
            if file_definition.get('source') == 'parmlib':
                parmlib_fields = file_definition.get('fields', {})
                for field_suggestion in vsam_output.field_specific_suggestions:
                    field_name_from_llm = field_suggestion.m204_field_name
                    if field_name_from_llm in parmlib_fields:
                        if 'vsam_suggestions' not in parmlib_fields[field_name_from_llm]:
                            parmlib_fields[field_name_from_llm]['vsam_suggestions'] = {}
                        
                        parmlib_fields[field_name_from_llm]['vsam_suggestions'].update(field_suggestion.model_dump())
                        log.info(f"M204_VSAM_ENHANCE: Updated VSAM suggestions for PARMLIB field '{field_name_from_llm}' in {m204_file.m204_file_name}.")
                        
                        if field_suggestion.is_key_component and field_suggestion.key_order is not None:
                            primary_key_components.append({"name": field_name_from_llm, "order": field_suggestion.key_order})
            
            # Handle IMAGE structure
            elif 'image_definitions' in file_definition:
                for image_def in file_definition.get("image_definitions", []):
                    for field in image_def.get("fields", []):
                        for field_suggestion in vsam_output.field_specific_suggestions:
                            if field.get("field_name") == field_suggestion.m204_field_name:
                                if 'vsam_suggestions' not in field:
                                    field['vsam_suggestions'] = {}
                                field['vsam_suggestions'].update(field_suggestion.model_dump())
                                log.info(f"M204_VSAM_ENHANCE: Updated VSAM suggestions for IMAGE field '{field_suggestion.m204_field_name}' in {m204_file.m204_file_name}.")
                                
                                if field_suggestion.is_key_component and field_suggestion.key_order is not None:
                                    primary_key_components.append({"name": field_suggestion.m204_field_name, "order": field_suggestion.key_order})

            if primary_key_components:
                primary_key_components.sort(key=lambda x: x["order"])
                m204_file.primary_key_field_name = ", ".join([comp["name"] for comp in primary_key_components])
                log.info(f"M204_VSAM_ENHANCE: Updated primary key for {m204_file.m204_file_name}: {m204_file.primary_key_field_name}")
            else:
                if m204_file.primary_key_field_name is not None:
                    log.info(f"M204_VSAM_ENHANCE: No primary key components identified. Clearing existing PK '{m204_file.primary_key_field_name}'.")
                m204_file.primary_key_field_name = None

            # Notify SQLAlchemy that the JSON has been modified in-place
            from sqlalchemy.orm.attributes import flag_modified
            flag_modified(m204_file, "file_definition_json")

        db.add(m204_file) # Add to session for commit

    except json.JSONDecodeError as e_json:
        log.error(f"M204_VSAM_ENHANCE: LLM VSAM enhancement for {m204_file.m204_file_name}: JSON parsing error. Raw output: '{json_text_output if json_text_output else 'N/A'}'. Error: {e_json}", exc_info=True)
    except Exception as e_llm:
        log.error(f"M204_VSAM_ENHANCE: LLM VSAM enhancement for {m204_file.m204_file_name}: Error during LLM call or processing. Error: {e_llm}", exc_info=True)
        if json_text_output:
            log.error(f"M204_VSAM_ENHANCE: LLM raw output during error for {m204_file.m204_file_name}: {json_text_output}")