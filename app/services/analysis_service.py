from sqlalchemy.orm import Session
from typing import List, Tuple, Optional, Dict, Any
import re
import os
import json

from app.models.input_source_model import InputSource
from app.models.procedure_model import Procedure
from app.models.m204_file_model import M204File # Updated import
from app.models.m204_file_field_model import M204Field
from app.models.m204_variable_model import M204Variable
from app.models.image_statement_model import ImageStatement
from app.models.procedure_call_model import ProcedureCall
from app.models.dd_statement_model import DDStatement
from sqlalchemy import inspect as sqlalchemy_inspect

from app.schemas.m204_analysis_schema import (
    M204ProcedureCreateSchema, M204ProcedureResponseSchema,
    M204FileCreateSchema, M204FileResponseSchema, # Updated import
    M204VariableCreateSchema, M204VariableResponseSchema,
    M204ProcedureCallCreateSchema, M204ProcedureCallResponseSchema,
    M204AnalysisResultDataSchema,
    M204DefineFieldCreateSchema, M204DefineFieldResponseSchema,
    ImageStatementCreateSchema, ImageStatementResponseSchema
)
from app.schemas.generic_analysis_schema import (
    DDStatementCreateSchema, DDStatementResponseSchema,
    GenericAnalysisResultDataSchema
)
from app.schemas.analysis_schema import UnifiedAnalysisReportSchema

from app.utils.logger import log
from fastapi import HTTPException, status
from sqlalchemy.sql import func
from app.config.llm_config import llm_config
from pydantic import BaseModel,Field
from llama_index.core.prompts import RichPromptTemplate
from app.services.rag_service import RagService
import asyncio

_shared_rag_service: Optional[RagService] = None

def set_global_rag_service(instance: RagService):
    """Sets the global RAG service instance for this module."""
    global _shared_rag_service
    _shared_rag_service = instance
    log.info(f"Global RAG service instance has been set in analysis_service: {type(instance)}")


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

class M204FieldVsamSuggestion(BaseModel):
    """Structured output for VSAM suggestions for a single M204 field."""
    m204_field_name: str = Field(description="The original M204 field name.")
    is_key_component: bool = Field(description="Is this field likely part of the VSAM primary key?")
    key_order: Optional[int] = Field(description="If part of the key, its order (1-based). Null if not a key component.", default=None)
    vsam_data_type: Optional[str] = Field(description="Suggested COBOL PICTURE clause or VSAM data type (e.g., 'PIC X(20)', 'PIC 9(5) COMP-3'). Null if not applicable.", default=None)
    vsam_length: Optional[int] = Field(description="Suggested length in bytes for VSAM. Null if not applicable.", default=None)
    reasoning: Optional[str] = Field(description="Brief reasoning for these suggestions for the field.", default=None)

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



async def _enhance_single_m204_field_with_vsam_suggestions(db: Session, field: M204Field, m204_file: M204File):
    """
    Uses LLM to suggest VSAM attributes for a single M204Field.
    Updates the M204Field in the session.
    """
    if not llm_config._llm:
        log.warning(f"LLM not available. Skipping VSAM enhancement for M204 field: {field.field_name} in file {m204_file.m204_file_name}")
        return

    log.info(f"Attempting LLM-based VSAM enhancement for M204 field: {field.field_name} in file: {m204_file.m204_file_name}")

    field_attributes_text = field.attributes_text or "Not defined."
    field_attributes_json_str = json.dumps(field.attributes_json, indent=2) if field.attributes_json else "{}"

    prompt_fstr = f"""
You are an expert Mainframe M204 to COBOL/VSAM migration specialist.
Analyze the following M204 field, which is part of the M204 file '{m204_file.m204_file_name}'.
Your goal is to suggest its potential VSAM attributes if this M204 file were migrated to VSAM.

M204 File Context: {m204_file.m204_file_name}
M204 Field Name: {field.field_name}
M204 Field PARMLIB Attributes (text): {field_attributes_text}
M204 Field PARMLIB Attributes (parsed JSON):
```json
{field_attributes_json_str}
```

Based on this information, particularly the field attributes (look for KEY, ORDERED, UNIQUE, INDEXED keywords or typical key field names like 'CUSTOMER-ID', 'ORDER-NUMBER', etc.):

Provide suggestions for its role in a VSAM structure:
*   `m204_field_name`: string (Must be "{field.field_name}")
*   `is_key_component`: boolean (true if this field is likely part of the primary key for KSDS/RRDS)
*   `key_order`: integer (1-based order if it's a key component, e.g., 1 for the first part of a composite key. Null if not a key component.)
*   `vsam_data_type`: string (suggest a COBOL PICTURE clause like "PIC X(10)", "PIC 9(7) COMP-3". Null if not applicable.)
*   `vsam_length`: integer (suggested length in bytes for VSAM. Null if not applicable.)
*   `reasoning`: string (briefly why you made these suggestions for this field)

Respond with a JSON object structured according to the M204FieldVsamSuggestion model.
Ensure `m204_field_name` in your response matches the input field name "{field.field_name}".
"""

    json_text_output: Optional[str] = None
    try:
        field_vsam_suggester_llm = llm_config._llm.as_structured_llm(M204FieldVsamSuggestion)
        completion_response = await field_vsam_suggester_llm.acomplete(prompt=prompt_fstr)
        json_text_output = completion_response.text
        
        loaded_suggestion_data = json.loads(json_text_output)
        field_suggestion = M204FieldVsamSuggestion(**loaded_suggestion_data)

        if field_suggestion.m204_field_name != field.field_name:
            log.warning(f"LLM returned VSAM data for field '{field_suggestion.m204_field_name}' but expected '{field.field_name}' in file '{m204_file.m204_file_name}'. Skipping update for this field.")
            return

        # Update M204Field
        field.is_primary_key_component = field_suggestion.is_key_component
        field.target_vsam_key_order = field_suggestion.key_order if field_suggestion.is_key_component else None
        field.target_vsam_data_type = field_suggestion.vsam_data_type
        field.target_vsam_length = field_suggestion.vsam_length
        db.add(field)
        log.info(f"LLM suggested VSAM attributes for field '{field.field_name}' in {m204_file.m204_file_name}: key_comp={field.is_primary_key_component}, order={field.target_vsam_key_order}, type={field.target_vsam_data_type}, len={field.target_vsam_length}. Reason: {field_suggestion.reasoning or 'N/A'}")

    except json.JSONDecodeError as e_json:
        log.error(f"LLM VSAM enhancement for field {field.field_name} in {m204_file.m204_file_name}: JSON parsing error. Raw output: '{json_text_output if json_text_output else 'N/A'}'. Error: {e_json}", exc_info=True)
    except Exception as e_llm:
        log.error(f"LLM VSAM enhancement for field {field.field_name} in {m204_file.m204_file_name}: Error during LLM call or processing. Error: {e_llm}", exc_info=True)
        if json_text_output:
            log.error(f"LLM raw output during error for field {field.field_name}: {json_text_output}")


async def _enhance_single_m204_db_file_with_vsam_suggestions(db: Session, m204_file: M204File):
    """
    Uses LLM to suggest VSAM attributes for a given M204File marked as a DB file.
    Updates the M204File and its M204Fields in the session.
    """
    log.debug(f"The input file to enhance is: {m204_file.m204_file_name} (ID: {m204_file.m204_file_id})")
    if not llm_config._llm:
        log.warning(f"LLM not available. Skipping VSAM enhancement for M204 file: {m204_file.m204_file_name}")
        return
    
    if not m204_file.is_db_file:
        log.debug(f"M204 file {m204_file.m204_file_name} is not a DB file. Skipping VSAM enhancement.")
        return

    log.info(f"Attempting LLM-based VSAM enhancement for M204 DB file: {m204_file.m204_file_name} (ID: {m204_file.m204_file_id})")
    log.debug(f"M204File state before VSAM enhancement: ID={m204_file.m204_file_id}, Name='{m204_file.m204_file_name}', LogicalName='{m204_file.m204_logical_dataset_name}', Attributes='{m204_file.m204_attributes}', IsDBFile={m204_file.is_db_file}, TargetVSAMType='{m204_file.target_vsam_type}', PK='{m204_file.primary_key_field_name}'")

    # Gather context
    file_attributes = m204_file.m204_attributes or "Not defined via DEFINE DATASET."
    
    fields_context_list = []
    if m204_file.fields: # Ensure fields are loaded
        log.debug(f"Fields for M204File '{m204_file.m204_file_name}' (ID: {m204_file.m204_file_id}) before file-level VSAM LLM call:")
        for field_idx, field in enumerate(m204_file.fields):
            log.debug(f"  Field[{field_idx}]: ID={field.m204_field_id}, Name='{field.field_name}', AttrsText='{field.attributes_text}', AttrsJSON={field.attributes_json}, PKComp={field.is_primary_key_component}, KeyOrder={field.target_vsam_key_order}, VSAMType='{field.target_vsam_data_type}', VSAMLen={field.target_vsam_length}")
            fields_context_list.append({
                "name": field.field_name,
                "attributes_text": field.attributes_text or "No attributes text.",
                "attributes_json": field.attributes_json or {},
                # Include previously determined VSAM suggestions for the field
                "current_is_key_component": field.is_primary_key_component,
                "current_key_order": field.target_vsam_key_order,
                "current_vsam_data_type": field.target_vsam_data_type,
                "current_vsam_length": field.target_vsam_length
            })
    
    fields_context_str = json.dumps(fields_context_list, indent=2) if fields_context_list else "No fields defined or found for this file in PARMLIB."
    log.debug(f"Fields context being sent to LLM for file '{m204_file.m204_file_name}':\n{fields_context_str}")

    prompt_fstr = f"""
You are an expert Mainframe M204 to COBOL/VSAM migration specialist.
Analyze the following M204 file information, which has been identified as an M204 database file.
Your goal is to suggest its VSAM organization and refine key structure and field attributes for VSAM.

M204 File Name: {m204_file.m204_file_name}
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
        log.debug(f"LLM raw JSON output for VSAM file analysis of '{m204_file.m204_file_name}':\n{json_text_output}")
        
        loaded_vsam_data = json.loads(json_text_output)
        vsam_output = M204FileVsamAnalysisOutput(**loaded_vsam_data)
        log.debug(f"Parsed VSAM analysis output model for '{m204_file.m204_file_name}': {vsam_output.model_dump_json(indent=2)}")


        if vsam_output.m204_file_name != m204_file.m204_file_name:
            log.warning(f"LLM returned VSAM data for '{vsam_output.m204_file_name}' but expected '{m204_file.m204_file_name}'. Skipping update for this file.")
            return

        # Update M204File
        m204_file.target_vsam_type = vsam_output.suggested_vsam_type
        log.info(f"LLM suggested VSAM type for {m204_file.m204_file_name}: {m204_file.target_vsam_type}. Reasoning: {vsam_output.overall_reasoning or 'N/A'}")

        primary_key_components = []
        # Create a map of existing fields for quick lookup
        existing_fields_map = {f.field_name: f for f in m204_file.fields}

        for field_suggestion in vsam_output.field_specific_suggestions:
            db_field = existing_fields_map.get(field_suggestion.m204_field_name)
            if db_field:
                log.debug(f"  Before field update for '{db_field.field_name}': PKComp={db_field.is_primary_key_component}, KeyOrder={db_field.target_vsam_key_order}, VSAMType='{db_field.target_vsam_data_type}', VSAMLen={db_field.target_vsam_length}")
                db_field.is_primary_key_component = field_suggestion.is_key_component
                db_field.target_vsam_key_order = field_suggestion.key_order if field_suggestion.is_key_component else None
                db_field.target_vsam_data_type = field_suggestion.vsam_data_type
                db_field.target_vsam_length = field_suggestion.vsam_length
                db.add(db_field)
                log.info(f"Updated M204Field '{db_field.field_name}' (ID: {db_field.m204_field_id}) for VSAM based on file-level analysis: key_comp={db_field.is_primary_key_component}, order={db_field.target_vsam_key_order}, type={db_field.target_vsam_data_type}, len={db_field.target_vsam_length}. Reason: {field_suggestion.reasoning or 'N/A'}")
                log.debug(f"  After field update for '{db_field.field_name}': PKComp={db_field.is_primary_key_component}, KeyOrder={db_field.target_vsam_key_order}, VSAMType='{db_field.target_vsam_data_type}', VSAMLen={db_field.target_vsam_length}")
                if db_field.is_primary_key_component:
                    primary_key_components.append({"name": db_field.field_name, "order": db_field.target_vsam_key_order or 0})
            else:
                log.warning(f"LLM suggested VSAM attributes for field '{field_suggestion.m204_field_name}' which was not found in M204File '{m204_file.m204_file_name}' during file-level update.")
        
        # Sort primary key components by order and store names
        if primary_key_components:
            primary_key_components.sort(key=lambda x: x["order"])
            m204_file.primary_key_field_name = ", ".join([comp["name"] for comp in primary_key_components])
            log.info(f"LLM derived/updated primary key for {m204_file.m204_file_name}: {m204_file.primary_key_field_name}")
        else:
            # If no key components were identified by this file-level analysis, clear any previous primary key.
            if m204_file.primary_key_field_name is not None: # Only log if it's changing
                 log.info(f"Clearing primary key for {m204_file.m204_file_name} as no key components identified in file-level VSAM analysis.")
            m204_file.primary_key_field_name = None

        db.add(m204_file)
        log.debug(f"M204File state after VSAM enhancement for '{m204_file.m204_file_name}': ID={m204_file.m204_file_id}, TargetVSAMType='{m204_file.target_vsam_type}', PK='{m204_file.primary_key_field_name}'")


    except json.JSONDecodeError as e_json:
        log.error(f"LLM VSAM enhancement for {m204_file.m204_file_name}: JSON parsing error. Raw output: '{json_text_output if json_text_output else 'N/A'}'. Error: {e_json}", exc_info=True)
    except Exception as e_llm:
        log.error(f"LLM VSAM enhancement for {m204_file.m204_file_name}: Error during LLM call or processing. Error: {e_llm}", exc_info=True)
        if json_text_output:
            log.error(f"LLM raw output during error for {m204_file.m204_file_name}: {json_text_output}")

# --- Helper Functions ---
async def _get_input_source_for_analysis(db: Session, input_source_id: int) -> InputSource:
    input_source = db.query(InputSource).filter(InputSource.input_source_id == input_source_id).first()
    if not input_source:
        log.warning(f"Input source file with ID {input_source_id} not found for analysis.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Input source file not found.")
    
    if not input_source.file_path_or_identifier or not os.path.exists(input_source.file_path_or_identifier):
        log.error(f"File path missing or file does not exist for InputSource ID {input_source_id} at '{input_source.file_path_or_identifier}'.")
        raise HTTPException(status_code=status.HTTP_409_CONFLICT, detail="File path missing or file does not exist on server.")
    return input_source

async def _read_file_content(file_path: str) -> str:
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            return f.read()
    except Exception as e:
        log.error(f"Error reading file at path {file_path}: {e}")
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Could not read file content: {e}")

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
            param_type = match.group(2).strip() if match.group(2) else "UNTYPED"
            length = int(match.group(3)) if match.group(3) else None
            
            param_detail: Dict[str, Any] = {"name": name, "type": param_type}
            if length is not None:
                param_detail["length"] = length
            parsed_params["parameters"].append(param_detail)
        else:
            parsed_params["parameters"].append({"name": p_str, "type": "UNKNOWN"})
            log.debug(f"Could not fully parse parameter: {p_str} in '{params_str}'")
            
    return parsed_params if parsed_params["parameters"] else None


async def _extract_and_store_m204_procedures(
    db: Session, input_source: InputSource, file_content: str
) -> List[Procedure]:
    log.info(f"Extracting M204 procedures for file ID: {input_source.input_source_id} ({input_source.original_filename})")
    procedures_created = []
    
    proc_pattern = re.compile(
        r"^\s*(?:(PUBLIC|PRIVATE)\s+)?(SUBROUTINE|PROCEDURE)\s+([A-Z0-9_#@$-]{1,32})(?:\s*\(([^)]*)\))?",
        re.IGNORECASE | re.MULTILINE
    )
    lines = file_content.splitlines()
    
    # 1. Initial parse to identify all procedure blocks and their basic info
    temp_procs_info = []
    for i, line_content_for_match in enumerate(lines):
        match = proc_pattern.match(line_content_for_match)
        if match:
            start_line_num = i + 1
            temp_procs_info.append({
                "match_obj": match,
                "start_line": start_line_num,
                "original_index": len(temp_procs_info) # Index in this list
            })

    llm_processing_tasks = []
    # Stores dicts of procedure data needed for DB ops, for procs undergoing LLM
    procedures_undergoing_llm_data = []
    # Stores dicts of procedure data for procs not undergoing LLM
    procedures_not_undergoing_llm_data = []

    # 2. Prepare data and create LLM tasks
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
        
        if start_line_num <= end_line_num: # Ensure valid range
            current_procedure_content = "\n".join(lines[start_line_num-1:end_line_num])
        else: 
            current_procedure_content = "" 
            log.warning(f"Procedure {proc_name} has invalid line numbers: start={start_line_num}, end={end_line_num}")


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
                    original_filename_for_debug=input_source.original_filename
                )
            )
            procedures_undergoing_llm_data.append(base_proc_db_data)
        else:
            base_proc_db_data["generated_summary"] = None
            base_proc_db_data["suggested_cobol_name"] = proc_name 
            base_proc_db_data["suggested_test_cases_json"] = None # Ensure this is set for no-LLM cases
            if not current_procedure_content:
                 base_proc_db_data["generated_summary"] = "Procedure content is empty or could not be extracted."
            elif not llm_config._llm:
                 base_proc_db_data["generated_summary"] = "LLM is not configured. No AI analysis performed."
            procedures_not_undergoing_llm_data.append(base_proc_db_data)

    # 3. Execute LLM tasks in parallel
    llm_results_list = []
    if llm_processing_tasks:
        log.info(f"Executing {len(llm_processing_tasks)} LLM analysis tasks in parallel for file {input_source.original_filename}.")
        llm_results_list = await asyncio.gather(*llm_processing_tasks, return_exceptions=True)
        log.info(f"Finished {len(llm_processing_tasks)} LLM analysis tasks for file {input_source.original_filename}.")


    # 4. Process results and perform database operations sequentially
    for i, llm_result_or_exc in enumerate(llm_results_list):
        proc_static_data = procedures_undergoing_llm_data[i]
        proc_name = proc_static_data["proc_name"]

        generated_summary: Optional[str]
        suggested_cobol_name: Optional[str]
        suggested_test_cases: Optional[List[Dict[str, Any]]] # Renamed for clarity

        if isinstance(llm_result_or_exc, Exception):
            log.error(f"LLM analysis task for procedure {proc_name} failed: {llm_result_or_exc}", exc_info=llm_result_or_exc)
            generated_summary = f"Error during AI analysis for {proc_name}. Check logs."
            suggested_cobol_name = proc_name 
            suggested_test_cases = None
        elif isinstance(llm_result_or_exc, dict) and llm_result_or_exc.get("proc_name") == proc_name:
            generated_summary = llm_result_or_exc.get("generated_summary")
            suggested_cobol_name = llm_result_or_exc.get("suggested_cobol_name")
            suggested_test_cases = llm_result_or_exc.get("suggested_test_cases_json") # Get test cases
        else: 
            log.error(f"Unexpected result or mismatched proc_name for {proc_name} from LLM task: {llm_result_or_exc}")
            generated_summary = f"Unexpected error or result format from AI analysis for {proc_name}."
            suggested_cobol_name = proc_name
            suggested_test_cases = None

        db_data_for_procedure = {
            **proc_static_data, 
            "generated_summary": generated_summary, 
            "suggested_cobol_name": suggested_cobol_name,
            "suggested_test_cases_json": suggested_test_cases # Add to data for DB
        }
        
        existing_proc = db.query(Procedure).filter_by(
            project_id=input_source.project_id,
            m204_proc_name=db_data_for_procedure["proc_name"],
            input_source_id=input_source.input_source_id
        ).first()

        if existing_proc:
            update_needed = False
            if existing_proc.start_line_in_source != db_data_for_procedure["start_line_num"]:
                existing_proc.start_line_in_source = db_data_for_procedure["start_line_num"]; update_needed = True
            if existing_proc.end_line_in_source != db_data_for_procedure["end_line_num"]:
                existing_proc.end_line_in_source = db_data_for_procedure["end_line_num"]; update_needed = True
            if existing_proc.m204_proc_type != db_data_for_procedure["actual_proc_type"]:
                existing_proc.m204_proc_type = db_data_for_procedure["actual_proc_type"]; update_needed = True
            if existing_proc.m204_parameters_string != db_data_for_procedure["params_str"]:
                existing_proc.m204_parameters_string = db_data_for_procedure["params_str"]
                existing_proc.parsed_parameters_json = db_data_for_procedure["parsed_params_json"]; update_needed = True
            elif existing_proc.parsed_parameters_json != db_data_for_procedure["parsed_params_json"]:
                 existing_proc.parsed_parameters_json = db_data_for_procedure["parsed_params_json"]; update_needed = True
            if hasattr(existing_proc, 'procedure_content') and existing_proc.procedure_content != db_data_for_procedure["current_procedure_content"]:
                existing_proc.procedure_content = db_data_for_procedure["current_procedure_content"]; update_needed = True
            if db_data_for_procedure["generated_summary"] is not None and existing_proc.summary != db_data_for_procedure["generated_summary"]:
                existing_proc.summary = db_data_for_procedure["generated_summary"]; update_needed = True
            if db_data_for_procedure["suggested_cobol_name"] is not None and existing_proc.target_cobol_program_name != db_data_for_procedure["suggested_cobol_name"]:
                existing_proc.target_cobol_program_name = db_data_for_procedure["suggested_cobol_name"]; update_needed = True
            
            # Update suggested_test_cases_json
            if db_data_for_procedure.get("suggested_test_cases_json") is not None and \
               existing_proc.suggested_test_cases_json != db_data_for_procedure["suggested_test_cases_json"]:
                existing_proc.suggested_test_cases_json = db_data_for_procedure["suggested_test_cases_json"]
                update_needed = True
            elif db_data_for_procedure.get("suggested_test_cases_json") is None and existing_proc.suggested_test_cases_json is not None:
                existing_proc.suggested_test_cases_json = None # Clear it if LLM returned None
                update_needed = True

            if update_needed:
                db.add(existing_proc)
                log.info(f"Updating existing procedure {db_data_for_procedure['proc_name']} (ID: {existing_proc.proc_id}) with new analysis.")
            procedures_created.append(existing_proc)
        else: 
            other_file_proc = db.query(Procedure).filter(
                Procedure.project_id == input_source.project_id,
                Procedure.m204_proc_name == db_data_for_procedure["proc_name"],
                Procedure.input_source_id != input_source.input_source_id
            ).first()
            if other_file_proc:
                 log.warning(f"Procedure '{db_data_for_procedure['proc_name']}' (in file {input_source.input_source_id}) also defined in input source ID {other_file_proc.input_source_id}.")

            try:
                proc_schema_data = M204ProcedureCreateSchema(
                    project_id=input_source.project_id,
                    input_source_id=input_source.input_source_id,
                    m204_proc_name=db_data_for_procedure["proc_name"],
                    m204_proc_type=db_data_for_procedure["actual_proc_type"],
                    m204_parameters_string=db_data_for_procedure["params_str"],
                    parsed_parameters_json=db_data_for_procedure["parsed_params_json"],
                    start_line_in_source=db_data_for_procedure["start_line_num"],
                    end_line_in_source=db_data_for_procedure["end_line_num"],
                    procedure_content=db_data_for_procedure["current_procedure_content"],
                    summary=db_data_for_procedure["generated_summary"],
                    target_cobol_program_name=db_data_for_procedure["suggested_cobol_name"],
                    suggested_test_cases_json=db_data_for_procedure.get("suggested_test_cases_json") # Add here
                )
                db_procedure = Procedure(**proc_schema_data.model_dump(exclude_none=True))
                db.add(db_procedure)
                procedures_created.append(db_procedure)
            except Exception as e:
                log.error(f"Error creating DB entry for M204 procedure {db_data_for_procedure['proc_name']}: {e}", exc_info=True)

    # Process procedures that did NOT undergo LLM analysis
    for proc_data_no_llm in procedures_not_undergoing_llm_data:
        existing_proc = db.query(Procedure).filter_by(
            project_id=input_source.project_id,
            m204_proc_name=proc_data_no_llm["proc_name"],
            input_source_id=input_source.input_source_id
        ).first()

        if existing_proc:
            update_needed = False
            if existing_proc.start_line_in_source != proc_data_no_llm["start_line_num"]:
                existing_proc.start_line_in_source = proc_data_no_llm["start_line_num"]; update_needed = True
            if existing_proc.end_line_in_source != proc_data_no_llm["end_line_num"]:
                existing_proc.end_line_in_source = proc_data_no_llm["end_line_num"]; update_needed = True
            if existing_proc.m204_proc_type != proc_data_no_llm["actual_proc_type"]:
                existing_proc.m204_proc_type = proc_data_no_llm["actual_proc_type"]; update_needed = True
            if existing_proc.m204_parameters_string != proc_data_no_llm["params_str"]:
                existing_proc.m204_parameters_string = proc_data_no_llm["params_str"]
                existing_proc.parsed_parameters_json = proc_data_no_llm["parsed_params_json"]; update_needed = True
            elif existing_proc.parsed_parameters_json != proc_data_no_llm["parsed_params_json"]:
                 existing_proc.parsed_parameters_json = proc_data_no_llm["parsed_params_json"]; update_needed = True
            if hasattr(existing_proc, 'procedure_content') and existing_proc.procedure_content != proc_data_no_llm["current_procedure_content"]:
                existing_proc.procedure_content = proc_data_no_llm["current_procedure_content"]; update_needed = True
            
            if proc_data_no_llm["generated_summary"] is not None and existing_proc.summary != proc_data_no_llm["generated_summary"]:
                existing_proc.summary = proc_data_no_llm["generated_summary"]; update_needed = True
            elif proc_data_no_llm["generated_summary"] is None and existing_proc.summary is not None: 
                existing_proc.summary = None; update_needed = True

            if proc_data_no_llm["suggested_cobol_name"] is not None and existing_proc.target_cobol_program_name != proc_data_no_llm["suggested_cobol_name"]:
                existing_proc.target_cobol_program_name = proc_data_no_llm["suggested_cobol_name"]; update_needed = True
            
            # For no-LLM cases, suggested_test_cases_json is None. Clear if it was previously set.
            if proc_data_no_llm.get("suggested_test_cases_json") is None and existing_proc.suggested_test_cases_json is not None:
                existing_proc.suggested_test_cases_json = None
                update_needed = True
            
            if update_needed:
                db.add(existing_proc)
                log.info(f"Updating existing procedure {proc_data_no_llm['proc_name']} (ID: {existing_proc.proc_id}) (no new LLM analysis).")
            procedures_created.append(existing_proc)
        else: 
            other_file_proc = db.query(Procedure).filter(
                Procedure.project_id == input_source.project_id,
                Procedure.m204_proc_name == proc_data_no_llm["proc_name"],
                Procedure.input_source_id != input_source.input_source_id
            ).first()
            if other_file_proc:
                 log.warning(f"Procedure '{proc_data_no_llm['proc_name']}' (in file {input_source.input_source_id}) also defined in input source ID {other_file_proc.input_source_id}.")
            try:
                proc_schema_data = M204ProcedureCreateSchema(
                    project_id=input_source.project_id,
                    input_source_id=input_source.input_source_id,
                    m204_proc_name=proc_data_no_llm["proc_name"],
                    m204_proc_type=proc_data_no_llm["actual_proc_type"],
                    m204_parameters_string=proc_data_no_llm["params_str"],
                    parsed_parameters_json=proc_data_no_llm["parsed_params_json"],
                    start_line_in_source=proc_data_no_llm["start_line_num"],
                    end_line_in_source=proc_data_no_llm["end_line_num"],
                    procedure_content=proc_data_no_llm["current_procedure_content"],
                    summary=proc_data_no_llm["generated_summary"], 
                    target_cobol_program_name=proc_data_no_llm["suggested_cobol_name"],
                    suggested_test_cases_json=proc_data_no_llm.get("suggested_test_cases_json") # Add here (will be None)
                )
                db_procedure = Procedure(**proc_schema_data.model_dump(exclude_none=True))
                db.add(db_procedure)
                procedures_created.append(db_procedure)
            except Exception as e:
                log.error(f"Error creating DB entry for M204 procedure {proc_data_no_llm['proc_name']} (no LLM): {e}", exc_info=True)

    # 5. Finalize DB operations for all procedures
    try:
        if procedures_created: 
            db.flush() 
            for proc_obj in procedures_created:
                if proc_obj and (proc_obj in db.dirty or proc_obj in db.new or sqlalchemy_inspect(proc_obj).persistent):
                    try:
                        db.refresh(proc_obj)
                    except Exception as e_refresh_ind:
                        log.warning(f"Could not refresh individual procedure {getattr(proc_obj, 'm204_proc_name', 'N/A')} (ID: {getattr(proc_obj, 'proc_id', 'N/A')}): {e_refresh_ind}")
    except Exception as e_flush_refresh:
        log.error(f"Error flushing/refreshing session after processing procedures for file ID {input_source.input_source_id}: {e_flush_refresh}", exc_info=True)
    
    log.info(f"Finished extracting and storing {len(procedures_created)} M204 procedures for file ID: {input_source.input_source_id}.")
    return procedures_created

async def _extract_and_store_m204_datasets(
    db: Session, input_source: InputSource, file_content: str
) -> List[M204File]:
    log.info(f"Extracting M204 DEFINE DATASET statements for file ID: {input_source.input_source_id} ({input_source.original_filename})")
    defined_m204_files = []
    define_dataset_pattern = re.compile(
        r"^\s*DEFINE\s+DATASET\s+([A-Z0-9_#@$-.]+)\s*(.*)",  # Allows '.' in logical name
        re.IGNORECASE
    )
    lines = file_content.splitlines()
    
    ddname_value_capture_pattern = r"'([A-Z0-9_#@$-]{1,8})'|\"([A-Z0-9_#@$-]{1,8})\"|([A-Z0-9_#@$-]{1,8})"
    ddname_search_regex = rf"DDNAME\s*=\s*(?:{ddname_value_capture_pattern})"

    i = 0
    while i < len(lines):
        current_line_stripped = lines[i].strip()
        initial_match = define_dataset_pattern.match(current_line_stripped)

        if initial_match:
            m204_logical_name_from_first_line = initial_match.group(1) 
            params_on_first_line = initial_match.group(2)
            start_line_num = i + 1

            statement_lines_for_db = [lines[i]] 
            stripped_statement_parts_for_regex = [current_line_stripped]

            current_physical_line_idx = i
            while lines[current_physical_line_idx].strip().endswith('-') and \
                  current_physical_line_idx + 1 < len(lines):
                current_physical_line_idx += 1
                next_line_original = lines[current_physical_line_idx]
                next_line_stripped = next_line_original.strip()

                if next_line_stripped.startswith('*'): 
                    current_physical_line_idx -= 1 
                    break
                statement_lines_for_db.append(next_line_original)
                stripped_statement_parts_for_regex.append(next_line_stripped)
            
            final_m204_attributes_for_db = "\n".join(statement_lines_for_db)
            full_statement_for_regex = " ".join(stripped_statement_parts_for_regex)
            
            final_parse_match = define_dataset_pattern.match(full_statement_for_regex)

            if final_parse_match:
                m204_logical_name = final_parse_match.group(1).strip().upper() # Normalize to uppercase
                params_str = final_parse_match.group(2)
            else:
                log.warning(f"Could not re-parse combined multi-line DEFINE DATASET statement starting at line {start_line_num} in {input_source.original_filename}. Using first line data only. Statement: '{full_statement_for_regex[:200]}'")
                m204_logical_name = m204_logical_name_from_first_line.strip().upper() # Normalize to uppercase
                params_str = params_on_first_line
                final_m204_attributes_for_db = lines[i].strip()

            ddname_match = re.search(ddname_search_regex, params_str, re.IGNORECASE)
            extracted_ddname_value = None
            if ddname_match:
                # Iterate through groups to find the non-None one and strip it
                for group_val in ddname_match.groups(): # ddname_match.groups() will be (val_sq, val_dq, val_uq)
                    if group_val:
                        extracted_ddname_value = group_val.strip().upper() # Normalize to uppercase
                        break
            
            if extracted_ddname_value:
                file_key_name = extracted_ddname_value 
            else:
                file_key_name = m204_logical_name # Already uppercased and stripped
                log.warning(
                    f"DEFINE DATASET for '{m204_logical_name}' in {input_source.original_filename} at line {start_line_num} "
                    f"does not have a clearly parsable DDNAME parameter or the DDNAME format is unexpected. "
                    f"Using M204 logical name '{m204_logical_name}' as m204_file_name. "
                    f"Full parameters string (first 100 chars): '{params_str[:100]}'"
                )
            
            log.debug(f"Processing DEFINE DATASET: Logical Name='{m204_logical_name}', Extracted DDNAME='{extracted_ddname_value}', Effective File Key Name (for m204_file_name)='{file_key_name}' from file '{input_source.original_filename}' line {start_line_num}")

            # Use case-insensitive comparison for lookup when checking for existing M204File
            existing_m204_file = db.query(M204File).filter(
                M204File.project_id == input_source.project_id, 
                func.upper(M204File.m204_file_name) == file_key_name # file_key_name is already uppercased
            ).first()

            if existing_m204_file:
                update_needed = False
                if existing_m204_file.defined_in_input_source_id != input_source.input_source_id or \
                   existing_m204_file.defined_at_line != start_line_num:
                    log.debug(f"Updating M204File '{file_key_name}' (ID: {existing_m204_file.m204_file_id}) source definition from {existing_m204_file.defined_in_input_source_id}/L{existing_m204_file.defined_at_line} to {input_source.input_source_id}/L{start_line_num}")
                    existing_m204_file.defined_in_input_source_id = input_source.input_source_id
                    existing_m204_file.defined_at_line = start_line_num
                    update_needed = True
                
                if existing_m204_file.m204_attributes != final_m204_attributes_for_db:
                    log.debug(f"Updating M204File '{file_key_name}' (ID: {existing_m204_file.m204_file_id}) attributes.")
                    existing_m204_file.m204_attributes = final_m204_attributes_for_db
                    update_needed = True
                
                if existing_m204_file.m204_logical_dataset_name != m204_logical_name: # m204_logical_name is already uppercased
                    log.debug(f"Updating M204File '{file_key_name}' (ID: {existing_m204_file.m204_file_id}) logical name from '{existing_m204_file.m204_logical_dataset_name}' to '{m204_logical_name}'.")
                    existing_m204_file.m204_logical_dataset_name = m204_logical_name 
                    update_needed = True

                if update_needed:
                    db.add(existing_m204_file)
                    log.info(f"Updated existing M204File '{file_key_name}' (ID: {existing_m204_file.m204_file_id}, logical name: '{m204_logical_name}') based on DEFINE DATASET in {input_source.original_filename} at line {start_line_num}.")
                else:
                    log.debug(f"No updates needed for existing M204File '{file_key_name}' (ID: {existing_m204_file.m204_file_id}) from {input_source.original_filename} at line {start_line_num}.")
                defined_m204_files.append(existing_m204_file)
            
            else: # Create new M204File
                try:
                    file_data = M204FileCreateSchema(
                        project_id=input_source.project_id, 
                        defined_in_input_source_id=input_source.input_source_id,
                        m204_file_name=file_key_name,  # Stored uppercased
                        m204_logical_dataset_name=m204_logical_name, # Stored uppercased
                        m204_attributes=final_m204_attributes_for_db,
                        defined_at_line=start_line_num,
                        is_db_file=None 
                    )
                    db_m204_file = M204File(**file_data.model_dump())
                    db.add(db_m204_file)
                    defined_m204_files.append(db_m204_file)
                    log.info(f"Created new M204File '{file_key_name}' (logical name: '{m204_logical_name}') from DEFINE DATASET in {input_source.original_filename} at line {start_line_num}.")
                except Exception as e:
                    log.error(f"Error creating DB entry for M204File (from DEFINE DATASET: {file_key_name}, logical name: {m204_logical_name}, line: {start_line_num}): {e}", exc_info=True)
            
            i = current_physical_line_idx + 1 
            continue 
        
        i += 1 
    return defined_m204_files

async def _extract_and_store_m204_variables(
    db: Session, input_source: InputSource, file_content: str, procedures_in_file: List[Procedure]
) -> List[M204Variable]:
    log.info(f"Extracting M204 variables for file ID: {input_source.input_source_id}")
    variables_created = []
    lines = file_content.splitlines()
    
    percent_var_pattern = re.compile(r"(%[A-Z0-9_#@$-]+)", re.IGNORECASE)
    dollar_var_pattern = re.compile(r"(\$[A-Z0-9_#@$-]+)", re.IGNORECASE)
    define_var_pattern = re.compile(
        r"^\s*DEFINE\s+"
        r"(?:(PUBLIC|PRIVATE)\s+)?\s*"
        r"(%[A-Z0-9_#@$-]+)\s*"
        r"(?:\(([^)]*)\))?"
        r"(?:\s+(PUBLIC|PRIVATE))?",
        re.IGNORECASE | re.MULTILINE
    )
    
    found_var_details = {} 
    proc_line_map = {proc.start_line_in_source: proc.proc_id for proc in procedures_in_file if proc.start_line_in_source and proc.proc_id is not None}
    proc_id_to_name_map = {p.proc_id: p.m204_proc_name for p in procedures_in_file if p.proc_id}
    current_proc_id_for_line = None
    sorted_proc_starts = sorted(proc_line_map.keys())

    for i, line_content in enumerate(lines):
        line_num = i + 1
        temp_proc_id_for_line = None
        for start_line in sorted_proc_starts:
            proc_obj = next((p for p in procedures_in_file if p.start_line_in_source == start_line), None)
            if proc_obj and proc_obj.end_line_in_source and start_line <= line_num <= proc_obj.end_line_in_source:
                temp_proc_id_for_line = proc_line_map[start_line]
                break 
            elif line_num < start_line : 
                break
        current_proc_id_for_line = temp_proc_id_for_line

        for match in define_var_pattern.finditer(line_content):
            scope_keyword_before = match.group(1)
            var_name = match.group(2)
            attributes_str = match.group(3)
            scope_keyword_after = match.group(4)
            scope = "LOCAL"
            if scope_keyword_before: scope = scope_keyword_before.upper()
            elif scope_keyword_after: scope = scope_keyword_after.upper()
            
            effective_proc_id = current_proc_id_for_line if scope not in ["PUBLIC", "PRIVATE"] else None
            var_key = f"{var_name}_{scope}_{effective_proc_id or 'global_file'}"
            if var_key not in found_var_details:
                found_var_details[var_key] = {
                    "name": var_name, "type": "%scalar_defined", "scope": scope, 
                    "line": line_num, "proc_id": effective_proc_id,
                    "attrs": {"definition_attributes": attributes_str} if attributes_str else {}
                }
        
        for match in percent_var_pattern.finditer(line_content):
            var_name = match.group(1)
            # Check if this variable was already defined with DEFINE. If so, its details are already captured.
            # This check needs to be robust enough to consider scope.
            is_already_defined = False
            for key, val_details in found_var_details.items():
                if val_details["name"] == var_name and val_details["type"] == "%scalar_defined":
                    # If defined globally (PUBLIC/PRIVATE) or locally within the same procedure context
                    if val_details["scope"] in ["PUBLIC", "PRIVATE"]:
                        is_already_defined = True
                        break
                    if val_details["scope"] == "LOCAL" and val_details["proc_id"] == current_proc_id_for_line:
                        is_already_defined = True
                        break
            if is_already_defined:
                continue

            var_key_local = f"{var_name}_LOCAL_{current_proc_id_for_line or 'global_file'}"
            # We assume %variables not explicitly defined are LOCAL to their procedure or global to the file if outside any proc.
            if var_key_local not in found_var_details:
                 found_var_details[var_key_local] = {
                    "name": var_name, "type": "%scalar_used", "scope": "LOCAL", 
                    "line": line_num, "proc_id": current_proc_id_for_line, "attrs": {}
                }
        
        for match in dollar_var_pattern.finditer(line_content):
            var_name = match.group(1)
            var_key = f"{var_name}_GLOBAL_None" # $variables are typically global
            if var_key not in found_var_details:
                 found_var_details[var_key] = {
                    "name": var_name, "type": "$list_used", "scope": "GLOBAL", 
                    "line": line_num, "proc_id": None, "attrs": {}
                }
                
    for details in found_var_details.values():
        suggested_cobol_name_for_var: Optional[str] = None
        
        if llm_config._llm:
            try:
                procedure_name_context = "Global or undefined procedure context"
                if details["proc_id"]:
                    procedure_name_context = proc_id_to_name_map.get(details["proc_id"], f"Procedure ID {details['proc_id']}")

                variable_attributes_for_prompt = json.dumps(details["attrs"]) if details["attrs"] else "None"

                variable_mapping_prompt_fstr = f"""
You are an expert M204 to COBOL migration assistant.
Given the M204 variable details below, suggest a COBOL-compliant variable name.
The COBOL name should be descriptive, follow standard COBOL naming conventions (e.g., up to 30 alphanumeric characters, hyphens allowed, typically starting with a prefix related to its usage or data type if applicable, avoid M204-specific symbols like '%' or '$').
Ensure the name is valid for use in COBOL programs (e.g. starts with a letter, contains only letters, numbers, and hyphens).

M204 Variable Name: {details["name"]}
M204 Variable Type: {details["type"]}
M204 Variable Scope: {details["scope"]}
Procedure Context (if applicable): {procedure_name_context}
Additional Attributes: {variable_attributes_for_prompt}

Respond with a JSON object containing the following keys:
- "m204_variable_name": string (The original M204 variable name: "{details["name"]}")
- "suggested_cobol_variable_name": string (The suggested COBOL variable name)
- "reasoning": string (Optional: A brief explanation for your suggestion)
"""
                log.debug(f"Attempting to get COBOL name for M204 variable: {details['name']}")
                variable_mapper_llm = llm_config._llm.as_structured_llm(M204VariableToCobolOutput)
                completion_response = await variable_mapper_llm.acomplete(prompt=variable_mapping_prompt_fstr)
                json_text_output = completion_response.text
                loaded_map_data = json.loads(json_text_output)
                mapping_output_model = M204VariableToCobolOutput(**loaded_map_data)

                if mapping_output_model and mapping_output_model.suggested_cobol_variable_name:
                    suggested_cobol_name_for_var = mapping_output_model.suggested_cobol_variable_name
                    log.info(f"Suggested COBOL name for M204 var '{details['name']}' is '{suggested_cobol_name_for_var}'. Reasoning: {mapping_output_model.reasoning or 'N/A'}")
                else:
                    log.warning(f"LLM did not provide a COBOL name suggestion for M204 variable: {details['name']}")
            except Exception as e_llm_var_map:
                log.error(f"LLM error or JSON parsing error during COBOL name suggestion for M204 variable '{details['name']}': {e_llm_var_map}", exc_info=True)
                log.error(f"LLM raw output for variable mapping of '{details['name']}': {json_text_output if 'json_text_output' in locals() else 'N/A'}")

        existing_var_query = db.query(M204Variable).filter_by(
            project_id=input_source.project_id, 
            input_source_id=input_source.input_source_id,
            variable_name=details["name"], 
            scope=details["scope"],
            definition_line_number=details["line"] # Using definition line for uniqueness within a file/scope
        )
        if details["proc_id"] is not None:
            existing_var_query = existing_var_query.filter_by(procedure_id=details["proc_id"])
        else:
            existing_var_query = existing_var_query.filter(M204Variable.procedure_id.is_(None))
        
        existing_var = existing_var_query.first()

        if existing_var:
            update_existing_var = False
            if suggested_cobol_name_for_var and existing_var.cobol_mapped_variable_name != suggested_cobol_name_for_var:
                existing_var.cobol_mapped_variable_name = suggested_cobol_name_for_var
                update_existing_var = True
            # Potentially update other fields if they can change, e.g., attributes or type if re-parsed
            if existing_var.variable_type != details["type"]:
                existing_var.variable_type = details["type"]
                update_existing_var = True
            if existing_var.attributes != details["attrs"]:
                existing_var.attributes = details["attrs"]
                update_existing_var = True

            if update_existing_var:
                db.add(existing_var)
                log.info(f"Updating existing M204 variable '{existing_var.variable_name}' (ID: {existing_var.variable_id}) with new COBOL name or details.")
            variables_created.append(existing_var)
            continue
        
        try:
            var_data = M204VariableCreateSchema(
                project_id=input_source.project_id, input_source_id=input_source.input_source_id,
                procedure_id=details["proc_id"], variable_name=details["name"],
                variable_type=details["type"], scope=details["scope"],
                attributes=details["attrs"], definition_line_number=details["line"],
                cobol_mapped_variable_name=suggested_cobol_name_for_var
            )
            db_variable = M204Variable(**var_data.model_dump(exclude_none=True))
            db.add(db_variable)
            variables_created.append(db_variable)
        except Exception as e:
            log.error(f"Error creating DB entry for M204 variable {details['name']}: {e}", exc_info=True)
    return variables_created

async def _analyze_single_procedure_llm(
    proc_name: str,
    params_str: Optional[str],
    current_procedure_content: str,
    m204_param_names: List[str], # Derived from parsed_params_json
    input_source_id_for_debug: int,
    original_filename_for_debug: str
) -> Dict[str, Any]:
    """
    Performs LLM analysis (concept ID, RAG, summarization, test case generation) for a single M204 procedure.
    This function is intended to be run concurrently for multiple procedures.
    """
    generated_summary: Optional[str] = f"AI analysis could not be completed for {proc_name}."
    suggested_cobol_name: Optional[str] = proc_name # Default fallback
    suggested_test_cases_json: Optional[List[Dict[str, Any]]] = None # New field for test cases
    rag_context_for_summary: str = "No RAG context was available or generated for summarization."
    json_text_output_concept: Optional[str] = "N/A"
    json_text_output_summary: Optional[str] = "N/A"
    json_text_output_test_cases: Optional[str] = "N/A" # For debugging test case LLM output

    try:
        log.info(f"LLM_TASK_START: Analyzing procedure: {proc_name} from file: {original_filename_for_debug}")

        max_proc_content_len_for_prompt = 3500
        truncated_proc_content = current_procedure_content
        if len(current_procedure_content) > max_proc_content_len_for_prompt:
            truncated_proc_content = current_procedure_content[:max_proc_content_len_for_prompt] + "\n... [content truncated for brevity]"
            log.warning(f"LLM_TASK: Procedure {proc_name} content truncated for LLM prompts.")

        # STEP 1: Identify M204 Concepts
        # ... (existing concept identification code remains the same) ...
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
        log.info(f"LLM_TASK: Step 1: Identifying concepts for {proc_name}")
        concept_output_model: Optional[M204ConceptIdentificationOutput] = None
        try:
            concept_identification_llm = llm_config._llm.as_structured_llm(M204ConceptIdentificationOutput)
            completion_response_concept = await concept_identification_llm.acomplete(prompt=concept_identification_prompt_fstr)
            json_text_output_concept = completion_response_concept.text
            loaded_concept_data = json.loads(json_text_output_concept)
            concept_output_model = M204ConceptIdentificationOutput(**loaded_concept_data)
            log.info(f"LLM_TASK: Identified concepts for {proc_name}: {concept_output_model.identified_concepts if concept_output_model else 'None'}")
        except Exception as e_concept_llm:
            log.error(f"LLM_TASK: LLM error or JSON parsing error during concept identification for {proc_name}: {e_concept_llm}", exc_info=True)
            log.error(f"LLM_TASK: LLM raw output for concept identification of {proc_name}: {json_text_output_concept}")

        identified_concepts_for_rag: List[str] = []
        if concept_output_model and concept_output_model.identified_concepts:
            identified_concepts_for_rag = concept_output_model.identified_concepts

        concepts_and_params_for_rag = list(set(identified_concepts_for_rag + m204_param_names))


        # STEP 2: RAG Query
        # ... (existing RAG query code remains the same) ...
        if _shared_rag_service and concepts_and_params_for_rag:
            rag_query_text = (
                f"Provide detailed explanations, usage patterns, and considerations for the following M204 concepts and parameters "
                f"relevant to understanding the procedure '{proc_name}': {', '.join(concepts_and_params_for_rag)}. "
                f"Focus on how these concepts interact and their typical roles in M204 procedures."
            )
            log.info(f"LLM_TASK: Step 2: Querying RAG for concepts: {', '.join(concepts_and_params_for_rag)} for procedure {proc_name}")
            rag_context_for_summary = await _shared_rag_service.aquery(rag_query_text, similarity_top_k=3)
            if rag_context_for_summary is None or "Error performing async RAG query" in rag_context_for_summary or "RAG Index is not available" in rag_context_for_summary:
                log.warning(f"LLM_TASK: Could not retrieve valid context from RAG for concepts of {proc_name}. Proceeding with generic or no RAG context.")
                rag_context_for_summary = f"RAG context retrieval failed for identified concepts: {', '.join(concepts_and_params_for_rag)}. Analyze based on code alone."
            else:
                log.info(f"LLM_TASK: Successfully retrieved RAG context for {proc_name} based on identified concepts.")
        elif not _shared_rag_service:
            log.warning(f"LLM_TASK: RAG service not available. Skipping RAG query for {proc_name}.")
            rag_context_for_summary = "RAG service was not available. Analysis based on code alone."
        elif _shared_rag_service: # This case implies concepts_and_params_for_rag is empty
            log.warning(f"LLM_TASK: No specific M204 concepts identified by LLM for {proc_name}. Using generic RAG query or proceeding without specific RAG context.")
            generic_rag_query = f"Provide general context on M204 procedure structure, common commands, and control flow, relevant for understanding a procedure named '{proc_name}'."
            rag_context_for_summary = await _shared_rag_service.aquery(generic_rag_query, similarity_top_k=1)
            if rag_context_for_summary is None or "Error performing async RAG query" in rag_context_for_summary or "RAG Index is not available" in rag_context_for_summary:
                 rag_context_for_summary = "No specific M204 concepts were identified, and generic RAG context retrieval also failed. Analyze based on code alone."
        else: # This case implies concepts_and_params_for_rag is empty AND RAG service not available
            log.warning(f"LLM_TASK: No specific M204 concepts identified and RAG service not available for {proc_name}. Proceeding without RAG context.")
            rag_context_for_summary = "No specific M204 concepts were identified, and RAG service was not available. Analyze based on code alone."


        # STEP 3: Summarize procedure
        # ... (existing summarization code remains largely the same, but ensure `generated_summary` is captured for the next step) ...
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
        summary_output_model: Optional[ProcedureAnalysisOutput] = None # Initialize here
        try:
            prompt_debug_dir = "debug_prompts"
            os.makedirs(prompt_debug_dir, exist_ok=True)
            prompt_file_path = os.path.join(prompt_debug_dir, f"prompt_summary_{input_source_id_for_debug}_{proc_name}.txt")
            await asyncio.to_thread(
                lambda: open(prompt_file_path, "w", encoding="utf-8").write(summarization_prompt_fstr)
            )
            log.info(f"LLM_TASK: Summarization prompt for {proc_name} saved to {prompt_file_path}")
        except Exception as e_file:
            log.error(f"LLM_TASK: Error saving summarization prompt to file for {proc_name}: {e_file}")

        log.info(f"LLM_TASK: Step 3: Generating summary and COBOL name for {proc_name} with RAG context.")
        try:
            summarization_llm = llm_config._llm.as_structured_llm(ProcedureAnalysisOutput)
            completion_response_summary = await summarization_llm.acomplete(prompt=summarization_prompt_fstr)
            json_text_output_summary = completion_response_summary.text
            loaded_summary_data = json.loads(json_text_output_summary)
            summary_output_model = ProcedureAnalysisOutput(**loaded_summary_data)
        except Exception as e_summary_llm:
            log.error(f"LLM_TASK: LLM error or JSON parsing error during summary generation for {proc_name}: {e_summary_llm}", exc_info=True)
            log.error(f"LLM_TASK: LLM raw output for summary generation of {proc_name}: {json_text_output_summary}")

        if summary_output_model:
            if summary_output_model.procedure_name == proc_name:
                generated_summary = summary_output_model.procedure_summary # Capture for test case generation
                suggested_cobol_name = summary_output_model.cobol_name_suggestion
                log.info(f"LLM_TASK: Generated summary for procedure {proc_name}: {' '.join(generated_summary.split()[:20])}...")
                log.info(f"LLM_TASK: Suggested COBOL name for {proc_name}: {suggested_cobol_name}")
            else:
                log.warning(f"LLM_TASK: Mismatched procedure name in summary output for {proc_name}. LLM returned for {summary_output_model.procedure_name}. Using fallback summary.")
                generated_summary = f"LLM analysis for {proc_name} returned data for a different procedure ({summary_output_model.procedure_name})."
        
        # STEP 4: Generate Unit Test Cases
        log.info(f"LLM_TASK: Step 4: Generating test cases for {proc_name}")
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
            test_case_llm = llm_config._llm.as_structured_llm(ProcedureTestCaseGenerationOutput)
            completion_response_test_cases = await test_case_llm.acomplete(prompt=test_case_generation_prompt_fstr)
            json_text_output_test_cases = completion_response_test_cases.text
            loaded_test_case_data = json.loads(json_text_output_test_cases)
            test_case_output_model = ProcedureTestCaseGenerationOutput(**loaded_test_case_data)

            if test_case_output_model and test_case_output_model.procedure_name == proc_name:
                suggested_test_cases_json = [tc.model_dump() for tc in test_case_output_model.test_cases]
                log.info(f"LLM_TASK: Generated {len(suggested_test_cases_json)} test cases for procedure {proc_name}.")
            elif test_case_output_model:
                log.warning(f"LLM_TASK: Mismatched procedure name in test case output for {proc_name}. LLM returned for {test_case_output_model.procedure_name}. No test cases will be stored.")
                suggested_test_cases_json = None 
            else:
                log.info(f"LLM_TASK: No test cases were generated or model parsing failed for procedure {proc_name}.")
                suggested_test_cases_json = None

        except Exception as e_test_case_llm:
            log.error(f"LLM_TASK: LLM error or JSON parsing error during test case generation for {proc_name}: {e_test_case_llm}", exc_info=True)
            log.error(f"LLM_TASK: LLM raw output for test case generation of {proc_name}: {json_text_output_test_cases}")
            suggested_test_cases_json = None


    except Exception as e_llm_main:
        log.error(f"LLM_TASK: Main error during multi-step LLM analysis for procedure {proc_name}: {e_llm_main}", exc_info=True)
        # Fallback values are already set

    log.info(f"LLM_TASK_END: Finished analyzing procedure: {proc_name} from file: {original_filename_for_debug}")
    return {
        "proc_name": proc_name, 
        "generated_summary": generated_summary,
        "suggested_cobol_name": suggested_cobol_name,
        "suggested_test_cases_json": suggested_test_cases_json # Add to return
    }

async def _extract_and_store_m204_procedure_calls(
    db: Session, input_source: InputSource, file_content: str, defined_procedures_in_file: List[Procedure]
) -> List[ProcedureCall]:
    log.info(f"Extracting M204 procedure calls for file ID: {input_source.input_source_id}")
    procedure_calls_created = []
    lines = file_content.splitlines()
    call_pattern = re.compile(r"^\s*CALL\s+([A-Z0-9_#@$-]+)(?:\s*\(.*?\))?", re.IGNORECASE | re.MULTILINE)
    
    proc_line_map = {proc.start_line_in_source: proc.proc_id for proc in defined_procedures_in_file if proc.start_line_in_source and proc.proc_id is not None}
    current_calling_proc_id_for_line = None
    sorted_proc_starts = sorted(proc_line_map.keys())

    for i, line_content in enumerate(lines):
        line_num = i + 1
        temp_calling_proc_id = None
        for start_line in sorted_proc_starts:
            proc_obj = next((p for p in defined_procedures_in_file if p.start_line_in_source == start_line), None)
            if proc_obj and proc_obj.end_line_in_source and start_line <= line_num <= proc_obj.end_line_in_source:
                temp_calling_proc_id = proc_line_map[start_line]
                break
            elif line_num < start_line:
                break
        current_calling_proc_id_for_line = temp_calling_proc_id

        match = call_pattern.match(line_content)
        if match:
            called_proc_name = match.group(1)
            existing_call = db.query(ProcedureCall).filter_by(
                project_id=input_source.project_id,
                calling_input_source_id=input_source.input_source_id,
                calling_procedure_id=current_calling_proc_id_for_line,
                called_procedure_name=called_proc_name,
                line_number=line_num
            ).first()

            if existing_call:
                procedure_calls_created.append(existing_call)
                continue

            try:
                call_data = M204ProcedureCallCreateSchema(
                    project_id=input_source.project_id,
                    calling_input_source_id=input_source.input_source_id,
                    calling_procedure_id=current_calling_proc_id_for_line,
                    called_procedure_name=called_proc_name,
                    line_number=line_num,
                    is_external=None
                )
                db_call = ProcedureCall(**call_data.model_dump())
                db.add(db_call)
                procedure_calls_created.append(db_call)
            except Exception as e:
                log.error(f"Error creating DB entry for procedure call to '{called_proc_name}' at line {line_num}: {e}")
    return procedure_calls_created

async def _resolve_procedure_calls(db: Session, project_id: int, calls_in_file: List[ProcedureCall], procs_in_file: List[Procedure]):
    log.info(f"Resolving internal/external status for {len(calls_in_file)} procedure calls in file ID {calls_in_file[0].calling_input_source_id if calls_in_file else 'N/A'}.")
    proc_names_in_file = {p.m204_proc_name: p.proc_id for p in procs_in_file if p.proc_id is not None}
    
    for call in calls_in_file:
        original_is_external = call.is_external
        original_resolved_id = call.resolved_procedure_id

        if call.called_procedure_name in proc_names_in_file:
            call.is_external = False
            call.resolved_procedure_id = proc_names_in_file[call.called_procedure_name]
        else:
            resolved_proc_globally = db.query(Procedure).filter(
                Procedure.project_id == project_id,
                Procedure.m204_proc_name == call.called_procedure_name
            ).first()
            if resolved_proc_globally and resolved_proc_globally.proc_id is not None:
                call.is_external = False 
                call.resolved_procedure_id = resolved_proc_globally.proc_id
            else:
                call.is_external = True 
                call.resolved_procedure_id = None
        
        if call.is_external != original_is_external or call.resolved_procedure_id != original_resolved_id:
            db.add(call)


async def _extract_and_store_image_statements(
    db: Session, input_source: InputSource, file_content: str
) -> List[ImageStatement]:
    log.info(f"Extracting IMAGE statements for file ID: {input_source.input_source_id} ({input_source.original_filename})")
    image_statements_created = []
    # IMAGE statement can reference the M204 logical dataset name or be standalone (e.g., for screens).
    # Group 1 captures the optional M204 logical dataset name.
    image_start_pattern = re.compile(r"^\s*IMAGE(?:\s+([A-Z0-9_#@$-.]+))?.*", re.IGNORECASE)
    end_image_pattern = re.compile(r"^\s*END\s+IMAGE\s*$", re.IGNORECASE)

    lines = file_content.splitlines()
    current_line_idx = 0
    while current_line_idx < len(lines):
        line_content_on_start_line = lines[current_line_idx]
        start_line_num = current_line_idx + 1 # 1-based line number for the IMAGE statement

        match = image_start_pattern.match(line_content_on_start_line)
        if match:
            referenced_logical_name = match.group(1).strip() if match.group(1) else None
            
            image_block_lines = []
            image_block_end_idx = current_line_idx # Tracks the last line index of the current IMAGE block

            # Consume lines from the start of the IMAGE statement until END IMAGE or EOF
            for block_line_idx in range(current_line_idx, len(lines)):
                current_block_line_content = lines[block_line_idx]
                image_block_lines.append(current_block_line_content) # Store the original line
                image_block_end_idx = block_line_idx
                
                # Check if the current line (stripped) matches the END IMAGE pattern
                if end_image_pattern.match(current_block_line_content.strip()):
                    break
            
            full_image_content = "\n".join(image_block_lines)

            # Check for an existing ImageStatement based on its starting line in the source file
            existing_image = db.query(ImageStatement).filter_by(
                project_id=input_source.project_id,
                input_source_id=input_source.input_source_id,
                line_number=start_line_num,
            ).first()

            if existing_image:
                changed = False
                # Update referenced_m204_logical_name if it has changed
                if (referenced_logical_name is not None and existing_image.referenced_m204_logical_name != referenced_logical_name) or \
                   (referenced_logical_name is None and existing_image.referenced_m204_logical_name is not None):
                    existing_image.referenced_m204_logical_name = referenced_logical_name
                    changed = True
                
                # Update image_content if it has changed
                if existing_image.image_content != full_image_content:
                    existing_image.image_content = full_image_content
                    changed = True
                
                if changed: 
                    db.add(existing_image)
                    log.info(f"Updated IMAGE statement at line {start_line_num} in file ID {input_source.input_source_id} (InputSource: {input_source.original_filename}).")
                image_statements_created.append(existing_image)
            else:
                try:
                    image_data = ImageStatementCreateSchema(
                        project_id=input_source.project_id,
                        input_source_id=input_source.input_source_id,
                        line_number=start_line_num,
                        image_content=full_image_content,
                        referenced_m204_logical_name=referenced_logical_name 
                    )
                    # Ensure model_dump excludes None if the schema field is Optional and not provided
                    db_image_statement = ImageStatement(**image_data.model_dump(exclude_none=True if referenced_logical_name is None else False)) 
                    db.add(db_image_statement)
                    image_statements_created.append(db_image_statement)
                    log.info(f"Created new IMAGE statement at line {start_line_num} in file ID {input_source.input_source_id} (InputSource: {input_source.original_filename}).")
                except Exception as e:
                    log.error(f"Error creating DB entry for IMAGE statement at line {start_line_num} in file ID {input_source.input_source_id}: {e}", exc_info=True)
            
            # Advance the main loop index past the processed IMAGE block
            current_line_idx = image_block_end_idx + 1
        else:
            # Not an IMAGE statement start, move to the next line
            current_line_idx += 1
            
    return image_statements_created


async def _process_m204_analysis(db: Session, input_source: InputSource, file_content: str) -> M204AnalysisResultDataSchema:
    log.info(f"Starting M204 specific processing for file: {input_source.original_filename}")
    
    extracted_procedures = await _extract_and_store_m204_procedures(db, input_source, file_content)
    # Call the renamed function and use a more generic variable name
    extracted_m204_defined_entities = await _extract_and_store_m204_datasets(db, input_source, file_content)
    extracted_variables = await _extract_and_store_m204_variables(db, input_source, file_content, extracted_procedures)
    extracted_procedure_calls = await _extract_and_store_m204_procedure_calls(db, input_source, file_content, extracted_procedures)
    extracted_image_statements = await _extract_and_store_image_statements(db, input_source, file_content)
    
    await _resolve_procedure_calls(db, input_source.project_id, extracted_procedure_calls, extracted_procedures)

    try:
        db.commit()
        # Update variable name in this list comprehension
        all_extracted_items = extracted_procedures + extracted_m204_defined_entities + \
                              extracted_variables + extracted_procedure_calls + extracted_image_statements
        for item in all_extracted_items:
            if item in db.dirty or item in db.new:
                try: 
                    db.flush() 
                    db.refresh(item)
                except Exception as e_refresh: log.warning(f"Could not refresh item {item} after commit: {e_refresh}")
    except Exception as e_commit:
        db.rollback()
        log.error(f"Failed to commit M204 analysis results for file ID {input_source.input_source_id}: {e_commit}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Database commit error during M204 analysis: {e_commit}")

    procedure_responses = [M204ProcedureResponseSchema.model_validate(p) for p in extracted_procedures]
    # Populate M204FileResponseSchema from the extracted entities
    m204_entity_responses = [M204FileResponseSchema.model_validate(df) for df in extracted_m204_defined_entities]
    variable_responses = [M204VariableResponseSchema.model_validate(v) for v in extracted_variables]
    procedure_call_responses = [M204ProcedureCallResponseSchema.model_validate(pc) for pc in extracted_procedure_calls]
    image_statement_responses = [ImageStatementResponseSchema.model_validate(img) for img in extracted_image_statements]
    
    return M204AnalysisResultDataSchema(
        procedures_found=procedure_responses,
        defined_files_found=m204_entity_responses, # Use the new variable for response
        defined_fields_found=[], 
        variables_found=variable_responses,
        procedure_calls_found=procedure_call_responses,
        image_statements_found=image_statement_responses
    )


async def _extract_and_store_dd_statements(
    db: Session, input_source: InputSource, file_content: str
) -> Tuple[List[DDStatement], List[M204File]]: 
    log.info(f"Extracting DD statements from JCL file ID: {input_source.input_source_id} ({input_source.original_filename}) for project ID: {input_source.project_id}")
    dd_statements_created = []
    m204_files_updated_by_jcl: List[M204File] = [] 

    # --- Debugging: List all M204Files for this project ---
    log.debug("Calling extract and store dd statements")
    try:
        all_project_m204_files = db.query(M204File.m204_file_id, M204File.m204_file_name, M204File.m204_logical_dataset_name, M204File.defined_in_input_source_id).filter(M204File.project_id == input_source.project_id).all()
        if not all_project_m204_files:
            log.debug(f"DEBUG_JCL_MATCH: No M204File entries found in DB for project_id {input_source.project_id} before processing JCL DDs from {input_source.original_filename}.")
        else:
            log.debug(f"DEBUG_JCL_MATCH: M204File entries in DB for project_id {input_source.project_id} (before JCL DD processing from {input_source.original_filename}):")
            for mf_id, mf_name, mf_logical_name, mf_def_src_id in all_project_m204_files:
                log.debug(f"  - ID: {mf_id}, m204_file_name: '{mf_name}', logical_name: '{mf_logical_name}', defined_in_src_id: {mf_def_src_id}")
    except Exception as e_debug_query:
        log.error(f"DEBUG_JCL_MATCH: Error querying M204Files for debugging: {e_debug_query}")
    # --- End Debugging ---

    lines = file_content.splitlines()
    job_pattern = re.compile(r"^\/\/\s*([A-Z0-9#@$-]{1,8})\s+JOB\s+", re.IGNORECASE)
    exec_pattern = re.compile(r"^\/\/\s*([A-Z0-9#@$-]{1,8})?\s+EXEC\s+(?:PGM=([A-Z0-9#@$-]{1,8})|PROC=([A-Z0-9#@$-]{1,8})|\S+)", re.IGNORECASE)
    dd_pattern = re.compile(r"^\/\/\s*([A-Z0-9#@$-.]{1,8})\s+DD\s+(.*)", re.IGNORECASE)
    current_job_name, current_step_name = None, None
    
    for i, line_content_full in enumerate(lines):
        line_num, line_stripped = i + 1, line_content_full.strip()
        if job_match := job_pattern.match(line_stripped):
            current_job_name, current_step_name = job_match.group(1), None
            continue
        if exec_match := exec_pattern.match(line_stripped):
            current_step_name = exec_match.group(1) or (f"PGM_{exec_match.group(2)}" if exec_match.group(2) else (f"PROC_{exec_match.group(3)}" if exec_match.group(3) else "ANON_STEP"))
            continue
        if dd_match := dd_pattern.match(line_stripped):
            dd_name_raw, dd_params_str = dd_match.group(1), dd_match.group(2)
            dd_name = dd_name_raw.strip() # Strip the DD name extracted from JCL

            dsn_val, disp_val, params_dict = None, None, {"raw_parameters_string": dd_params_str}
            if dsn_search := re.search(r"(?:DSN|DATASET)\s*=\s*([^,\s(]+(?:\([^)]+\))?)(?:,|\s|$)", dd_params_str, re.IGNORECASE): dsn_val = dsn_search.group(1)
            if disp_search := re.search(r"DISP\s*=\s*(?:\(([^)]+)\)|([A-Z0-9#@$-]+(?:,[A-Z0-9#@$-]+)*))", dd_params_str, re.IGNORECASE): disp_val = disp_search.group(1) or disp_search.group(2)
            
            is_explicit_m204_db_dsn = False
            if dsn_val:
                if re.search(r"\.DBASEF", dsn_val, re.IGNORECASE) or \
                   re.search(r"\.[A-Z0-9#@$-]*204", dsn_val, re.IGNORECASE): 
                    is_explicit_m204_db_dsn = True
            
            log.debug(f"JCL DD Processing: Attempting to match DD Name='{dd_name}' (raw='{dd_name_raw}') from JCL '{input_source.original_filename}' line {line_num} within project_id {input_source.project_id}")
            
            existing_m204_file = db.query(M204File).filter(
                M204File.project_id == input_source.project_id,
                func.upper(M204File.m204_file_name) == func.upper(dd_name) 
            ).first()

            if existing_m204_file:
                log.info(f"Found matching M204File for JCL DD '{dd_name}': ID={existing_m204_file.m204_file_id}, Stored m204_file_name='{existing_m204_file.m204_file_name}', Stored logical_name='{existing_m204_file.m204_logical_dataset_name}'")
                update_m204_file = False
                if is_explicit_m204_db_dsn and existing_m204_file.is_db_file is not True:
                    existing_m204_file.is_db_file = True
                    update_m204_file = True
                elif not is_explicit_m204_db_dsn and existing_m204_file.is_db_file is None: 
                    existing_m204_file.is_db_file = False 
                    update_m204_file = True

                if dsn_val and existing_m204_file.target_vsam_dataset_name != dsn_val:
                    existing_m204_file.target_vsam_dataset_name = dsn_val # Corrected variable name
                    update_m204_file = True
                
                if update_m204_file:
                    db.add(existing_m204_file)
                    if existing_m204_file not in m204_files_updated_by_jcl:
                        m204_files_updated_by_jcl.append(existing_m204_file)
                    log.info(f"Updated M204File '{existing_m204_file.m204_file_name}' (ID: {existing_m204_file.m204_file_id}) based on JCL DD statement '{dd_name}' in {input_source.original_filename}.")
            else:
                log.warning(f"JCL DD statement for '{dd_name}' (from raw '{dd_name_raw}') in {input_source.original_filename} (project_id: {input_source.project_id}, line: {line_num}) does not match any M204File. It will not be linked to an M204File entry.")

            existing_dd = db.query(DDStatement).filter_by(
                project_id=input_source.project_id, input_source_id=input_source.input_source_id,
                job_name=current_job_name, step_name=current_step_name,
                dd_name=dd_name, line_number_start=line_num 
            ).first()
            if existing_dd:
                if existing_dd.parameters_json.get("raw_parameters_string") != dd_params_str or \
                   existing_dd.dsn != dsn_val or existing_dd.disposition != disp_val:
                    existing_dd.dsn = dsn_val
                    existing_dd.disposition = disp_val
                    existing_dd.parameters_json = params_dict
                    existing_dd.raw_statement_text = line_stripped
                    db.add(existing_dd)
                dd_statements_created.append(existing_dd)
                continue
            try:
                dd_data = DDStatementCreateSchema(
                    project_id=input_source.project_id, input_source_id=input_source.input_source_id,
                    job_name=current_job_name, step_name=current_step_name,
                    dd_name=dd_name, dsn=dsn_val, disposition=disp_val,
                    line_number_start=line_num, line_number_end=line_num,
                    raw_statement_text=line_stripped, parameters_json=params_dict
                )
                db_dd_statement = DDStatement(**dd_data.model_dump())
                db.add(db_dd_statement)
                dd_statements_created.append(db_dd_statement)
            except Exception as e: 
                log.error(f"Error creating DB entry for DD statement '{dd_name}' at line {line_num}: {e}", exc_info=True)
    
    return dd_statements_created, m204_files_updated_by_jcl


async def _process_jcl_analysis(
    db: Session, input_source: InputSource, file_content: str
) -> Tuple[GenericAnalysisResultDataSchema, List[M204File]]:
    log.info(f"Starting JCL analysis for file: {input_source.original_filename} (ID: {input_source.input_source_id})")
    extracted_dd_statements, m204_files_from_jcl = await _extract_and_store_dd_statements(db, input_source, file_content)
    
    try:
        db.commit()
        
        for dd in extracted_dd_statements: 
            if dd in db.dirty or dd in db.new: 
                try: 
                    db.flush()
                    db.refresh(dd)
                except Exception as e_refresh_dd: log.warning(f"Could not refresh DD statement {dd.dd_statement_id} after commit: {e_refresh_dd}")
        
        for m204file in m204_files_from_jcl:
            if m204file in db.dirty or m204file in db.new:
                try:
                    db.flush()
                    db.refresh(m204file)
                except Exception as e_refresh_m204: log.warning(f"Could not refresh M204File {m204file.m204_file_id} (from JCL) after commit: {e_refresh_m204}")

    except Exception as e_commit:
        db.rollback()
        log.error(f"Failed to commit JCL analysis results for file ID {input_source.input_source_id}: {e_commit}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Database commit error during JCL analysis: {e_commit}")

    dd_responses = [DDStatementResponseSchema.model_validate(dd) for dd in extracted_dd_statements]
    summary_msg = f"JCL analysis processed. Found {len(dd_responses)} DD statements. Identified/updated {len(m204_files_from_jcl)} M204File entries from JCL."
    
    schema_result = GenericAnalysisResultDataSchema(dd_statements_found=dd_responses, summary=summary_msg)
    return schema_result, m204_files_from_jcl


async def _extract_m204_fields_from_parmlib(
    db: Session, input_source: InputSource, file_content: str
) -> List[M204Field]:
    log.info(f"Extracting M204 fields from PARMLIB file ID: {input_source.input_source_id} ({input_source.original_filename}) Project ID: {input_source.project_id}")
    extracted_fields: List[M204Field] = []
    lines = file_content.splitlines()

    field_def_pattern = re.compile(
        r"^\s*(?:DEFINE\s+FIELD\s+)?([A-Z0-9_#@$-]+)\s*(?:\((.*?)\)|(.*?))$",
        re.IGNORECASE
    )
    attr_pair_pattern = re.compile(
        r'''([A-Z0-9_#@$-]+)\s*=\s*((?:'[^']*')|(?:"[^"]*")|[^,]+)'''
    )
    current_file_context_name: Optional[str] = None
    file_context_pattern = re.compile(r"^\s*(?:FILE|M204FILE)\s*=?\s*([A-Z0-9_#@$-]+)", re.IGNORECASE)

    for i, line_content in enumerate(lines):
        line_num = i + 1
        line_stripped = line_content.strip()

        if not line_stripped or line_stripped.startswith(('*', '#', ';')):
            log.debug(f"PARMLIB_PARSE: Skipping comment or empty line {line_num} in {input_source.original_filename}")
            continue

        file_context_match = file_context_pattern.match(line_stripped)
        if file_context_match:
            current_file_context_name = file_context_match.group(1).upper() # Normalize to uppercase
            log.info(f"PARMLIB_PARSE: Context switched to M204File name (e.g., DDNAME): '{current_file_context_name}' at line {line_num} in {input_source.original_filename}")
            continue

        match = field_def_pattern.match(line_stripped)
        if match:
            field_name = match.group(1)
            attributes_str_raw = match.group(2) or match.group(3) or ""
            log.debug(f"PARMLIB_PARSE: Matched field definition for '{field_name}' with raw attributes '{attributes_str_raw}' at line {line_num} in {input_source.original_filename}. Current file context: '{current_file_context_name}'")

            parsed_attrs: Dict[str, Any] = {}
            if attributes_str_raw:
                for attr_match in attr_pair_pattern.finditer(attributes_str_raw):
                    key = attr_match.group(1).upper()
                    raw_value = attr_match.group(2).strip()

                    processed_value: Any
                    if (raw_value.startswith("'") and raw_value.endswith("'")) or \
                       (raw_value.startswith('"') and raw_value.endswith('"')):
                        processed_value = raw_value[1:-1]
                    else:
                        processed_value = raw_value

                    if isinstance(processed_value, str):
                        if processed_value.isdigit():
                            final_value = int(processed_value)
                        elif processed_value.lower() in ['true', 'yes']:
                            final_value = True
                        elif processed_value.lower() in ['false', 'no']:
                            final_value = False
                        else:
                            final_value = processed_value
                    else:
                        final_value = processed_value
                    parsed_attrs[key] = final_value
                    log.debug(f"PARMLIB_PARSE: Parsed attribute for '{field_name}': {key} = {final_value} (type: {type(final_value)})")

                potential_keywords = [kw.strip().upper() for kw in attributes_str_raw.split(',') if '=' not in kw and kw.strip()]
                for kw in potential_keywords:
                    if kw and kw not in parsed_attrs:
                        if kw in ["KEY", "OPTIONAL", "INDEXED", "ORDERED", "UNIQUE", "ENCRYPTED", "INVISIBLE", "REQUIRED", "VISIBLE", "STRING", "NUMERIC", "UPDATE IN PLACE"]:
                             parsed_attrs[kw] = True
                             log.debug(f"PARMLIB_PARSE: Parsed standalone keyword attribute for '{field_name}': {kw} = True")

            m204_file_id_for_field: Optional[int] = None
            db_m204_file_obj: Optional[M204File] = None

            if current_file_context_name:
                try:
                    all_project_m204_files_in_parmlib_context = db.query(
                        M204File.m204_file_id, 
                        M204File.m204_file_name, 
                        M204File.m204_logical_dataset_name, 
                        M204File.defined_in_input_source_id,
                        M204File.project_id
                    ).filter(M204File.project_id == input_source.project_id).all()
                    if not all_project_m204_files_in_parmlib_context:
                        log.debug(f"PARMLIB_DEBUG: No M204File entries found in DB for project_id {input_source.project_id} when searching for file name '{current_file_context_name}' in {input_source.original_filename}.")
                    else:
                        log.debug(f"PARMLIB_DEBUG: M204File entries in DB for project_id {input_source.project_id} (before looking up file name '{current_file_context_name}' for {input_source.original_filename}):")
                        for mf_id, mf_name, mf_logical, mf_def_src_id, mf_proj_id in all_project_m204_files_in_parmlib_context:
                            log.debug(f"  - ProjID: {mf_proj_id}, M204FileID: {mf_id}, FileName: '{mf_name}', LogicalName: '{mf_logical}', DefinedInSrcID: {mf_def_src_id}")
                except Exception as e_debug_query_parmlib:
                    log.error(f"PARMLIB_DEBUG: Error querying M204Files for debugging: {e_debug_query_parmlib}")

                log.debug(f"PARMLIB_PARSE: Looking up M204File with m204_file_name '{current_file_context_name}' for project ID {input_source.project_id}")
                db_m204_file_obj = db.query(M204File).filter(
                    M204File.project_id == input_source.project_id,
                    func.upper(M204File.m204_file_name) == current_file_context_name # current_file_context_name is already uppercased
                ).first()

                if db_m204_file_obj:
                    m204_file_id_for_field = db_m204_file_obj.m204_file_id
                    log.info(f"PARMLIB_PARSE: Found existing M204File by m204_file_name '{current_file_context_name}' (ID: {m204_file_id_for_field}, Logical Name: '{db_m204_file_obj.m204_logical_dataset_name}') for field '{field_name}'.")
                    if db_m204_file_obj.is_db_file is not True:
                        db_m204_file_obj.is_db_file = True # Mark as DB file if fields are being defined for it
                        db.add(db_m204_file_obj)
                        log.info(f"PARMLIB_PARSE: Marked M204File '{current_file_context_name}' (ID: {m204_file_id_for_field}) as DB file due to PARMLIB field definitions.")
                else:
                    log.warning(f"PARMLIB_PARSE: M204File with m204_file_name '{current_file_context_name}' was NOT FOUND for project ID {input_source.project_id}. Field '{field_name}' at line {line_num} in {input_source.original_filename} will not be associated with an M204File.")
                    continue
            else:
                log.warning(f"PARMLIB_PARSE: Field '{field_name}' defined in PARMLIB '{input_source.original_filename}' at line {line_num} is outside a 'FILE' context. It will not be associated with any M204File.")
                continue

            log.debug(f"PARMLIB_PARSE: Checking for existing M204Field '{field_name}' for M204File ID {m204_file_id_for_field} (Project ID: {input_source.project_id})")
            existing_field = db.query(M204Field).filter_by(
                project_id=input_source.project_id,
                field_name=field_name,
                m204_file_id=m204_file_id_for_field
            ).first()

            field_object_for_enhancement: Optional[M204Field] = None

            if existing_field:
                log.debug(f"PARMLIB_PARSE: Found existing M204Field '{field_name}' (ID: {existing_field.m204_field_id}). Checking for updates.")
                update_needed = False
                if existing_field.attributes_text != attributes_str_raw:
                    log.debug(f"PARMLIB_PARSE: Updating attributes_text for field '{field_name}'. Old: '{existing_field.attributes_text}', New: '{attributes_str_raw}'")
                    existing_field.attributes_text = attributes_str_raw
                    update_needed = True
                if existing_field.attributes_json != parsed_attrs:
                    log.debug(f"PARMLIB_PARSE: Updating attributes_json for field '{field_name}'. Old: {existing_field.attributes_json}, New: {parsed_attrs}")
                    existing_field.attributes_json = parsed_attrs
                    update_needed = True
                if existing_field.definition_line_number != line_num:
                    log.debug(f"PARMLIB_PARSE: Updating definition_line_number for field '{field_name}'. Old: {existing_field.definition_line_number}, New: {line_num}")
                    existing_field.definition_line_number = line_num
                    update_needed = True
                if existing_field.defined_in_input_source_id != input_source.input_source_id:
                    log.debug(f"PARMLIB_PARSE: Updating defined_in_input_source_id for field '{field_name}'. Old: {existing_field.defined_in_input_source_id}, New: {input_source.input_source_id}")
                    existing_field.defined_in_input_source_id = input_source.input_source_id
                    update_needed = True

                if update_needed:
                    db.add(existing_field)
                    log.info(f"PARMLIB_PARSE: Updating existing M204Field '{field_name}' (ID: {existing_field.m204_field_id}) for M204File ID {m204_file_id_for_field}.")
                else:
                    log.info(f"PARMLIB_PARSE: No updates needed for existing M204Field '{field_name}' (ID: {existing_field.m204_field_id}).")
                extracted_fields.append(existing_field)
                field_object_for_enhancement = existing_field
                # continue # Removed continue to allow LLM enhancement below
            else: # Create new M204Field
                try:
                    log.debug(f"PARMLIB_PARSE: Creating new M204Field '{field_name}' for M204File ID {m204_file_id_for_field}, Project ID {input_source.project_id}, defined in InputSource ID {input_source.input_source_id} at line {line_num}.")
                    field_data = M204DefineFieldCreateSchema(
                        project_id=input_source.project_id,
                        defined_in_input_source_id=input_source.input_source_id,
                        m204_file_id=m204_file_id_for_field,
                        field_name=field_name,
                        attributes_text=attributes_str_raw if attributes_str_raw else None,
                        attributes_json=parsed_attrs if parsed_attrs else None,
                        definition_line_number=line_num
                    )
                    db_field = M204Field(**field_data.model_dump(exclude_none=True))
                    db.add(db_field)
                    extracted_fields.append(db_field)
                    field_object_for_enhancement = db_field
                    log.info(f"PARMLIB_PARSE: Created new M204Field '{field_name}' for M204File ID {m204_file_id_for_field} from PARMLIB {input_source.original_filename}.")
                except Exception as e:
                    log.error(f"PARMLIB_PARSE: Error creating DB entry for M204 field '{field_name}' at line {line_num} in {input_source.original_filename} for M204File ID {m204_file_id_for_field}: {e}", exc_info=True)
                    continue # Skip LLM enhancement if creation failed
            
            # Perform LLM-based VSAM suggestion for the field if applicable
            if field_object_for_enhancement and db_m204_file_obj and db_m204_file_obj.is_db_file:
                if llm_config._llm:
                    await _enhance_single_m204_field_with_vsam_suggestions(db, field_object_for_enhancement, db_m204_file_obj)
                else:
                    log.debug(f"LLM not configured. Skipping VSAM enhancement for field {field_object_for_enhancement.field_name}")
            elif field_object_for_enhancement and db_m204_file_obj and not db_m204_file_obj.is_db_file:
                 log.debug(f"Skipping VSAM enhancement for field {field_object_for_enhancement.field_name} as parent file {db_m204_file_obj.m204_file_name} is not marked as a DB file.")


        else: # No match for field_def_pattern
            log.debug(f"PARMLIB_PARSE: Line {line_num} in {input_source.original_filename} ('{line_stripped[:60]}...') did not match field definition or file context pattern.")

    log.info(f"PARMLIB_PARSE: Finished extracting {len(extracted_fields)} M204 fields from PARMLIB file ID: {input_source.input_source_id} ({input_source.original_filename}).")
    return extracted_fields


async def _process_parmlib_analysis(db: Session, input_source: InputSource, file_content: str) -> M204AnalysisResultDataSchema:
    log.info(f"Starting PARMLIB analysis for file: {input_source.original_filename} (ID: {input_source.input_source_id})")
    
    # M204File objects might be created/updated within _extract_m204_fields_from_parmlib
    defined_fields_extracted = await _extract_m204_fields_from_parmlib(db, input_source, file_content)
    
    # Collect M204Files that were potentially modified or created
    # This is a bit indirect; ideally, _extract_m204_fields_from_parmlib would return them.
    # For now, we rely on them being in db.dirty or db.new after the call.
    m204_files_from_parmlib = [item for item in db if isinstance(item, M204File) and (item in db.dirty or item in db.new)]


    try:
        db.commit()
        for field in defined_fields_extracted: 
            if field in db.dirty or field in db.new: 
                try: 
                    db.flush()
                    db.refresh(field)
                except Exception as e_refresh: log.warning(f"Could not refresh M204Field {field.m204_field_id} after commit: {e_refresh}")
        
        for m204file in m204_files_from_parmlib: # Refresh M204Files from PARMLIB context
             if m204file in db.dirty or m204file in db.new:
                try:
                    db.flush()
                    db.refresh(m204file)
                except Exception as e_refresh_m204_parm: log.warning(f"Could not refresh M204File {m204file.m204_file_id} (from PARMLIB) after commit: {e_refresh_m204_parm}")


    except Exception as e_commit:
        db.rollback()
        log.error(f"Failed to commit PARMLIB analysis results for file ID {input_source.input_source_id}: {e_commit}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Database commit error during PARMLIB analysis: {e_commit}")

    field_responses = [M204DefineFieldResponseSchema.model_validate(f) for f in defined_fields_extracted]
    
    # Query M204Files that might have been created/updated by this PARMLIB analysis to include in response
    # This is to ensure the response reflects all changes made by this specific PARMLIB processing.
    # We'll only include those linked to *this* input_source_id as their definer or context.
    updated_m204_files_for_response = db.query(M204File).filter(
        M204File.project_id == input_source.project_id,
        M204File.m204_file_id.in_([f.m204_file_id for f in m204_files_from_parmlib if f.m204_file_id is not None])
    ).all()
    m204_file_responses_from_parmlib = [M204FileResponseSchema.model_validate(mf) for mf in updated_m204_files_for_response]


    return M204AnalysisResultDataSchema(
        procedures_found=[],
        defined_files_found=m204_file_responses_from_parmlib, 
        defined_fields_found=field_responses,
        variables_found=[],
        procedure_calls_found=[],
        image_statements_found=[]
    )

# --- Main Analysis Dispatcher ---


async def perform_source_file_analysis(db: Session, input_source_id: int) -> UnifiedAnalysisReportSchema:
    input_source = await _get_input_source_for_analysis(db, input_source_id)
    file_content = await _read_file_content(input_source.file_path_or_identifier)
    
    file_type = input_source.source_type.lower() if input_source.source_type else "unknown"
    analysis_details: Optional[Any] = None 
    analysis_status_val = "failed" 
    message = ""
    errors: List[str] = []
    m204_files_potentially_updated_for_vsam_enhancement: List[M204File] = []

    input_source.analysis_status = "analysis_in_progress" 
    input_source.error_message = None 
    input_source.last_analyzed_timestamp = func.now()
    try:
        db.commit()
        db.refresh(input_source)
    except Exception as e_commit_progress:
        db.rollback()
        log.error(f"Failed to update analysis_status to in_progress for {input_source_id}: {e_commit_progress}")
        errors.append(f"Initial status update failed: {str(e_commit_progress)}")


    try:
        if file_type in ["m204", "src", "s", "user", "proc"]:
            log.info(f"Dispatching to M204 analysis for file ID: {input_source_id}")
            analysis_details = await _process_m204_analysis(db, input_source, file_content)
            message = (f"M204 analysis completed. Procedures: {len(analysis_details.procedures_found)}, "
                       f"Defined M204 Datasets: {len(analysis_details.defined_files_found)}, "
                       f"Variables: {len(analysis_details.variables_found)}, "
                       f"Procedure Calls: {len(analysis_details.procedure_calls_found)}, "
                       f"Image Statements: {len(analysis_details.image_statements_found)}.")
            analysis_status_val = "analysis_completed"
            # Collect M204Files from M204AnalysisResultDataSchema for potential VSAM enhancement
            if analysis_details and analysis_details.defined_files_found:
                m204_file_ids = [mf.m204_file_id for mf in analysis_details.defined_files_found if mf.m204_file_id is not None]
                if m204_file_ids:
                    m204_files_potentially_updated_for_vsam_enhancement.extend(
                        db.query(M204File).filter(M204File.m204_file_id.in_(m204_file_ids)).all()
                    )

        elif file_type == "jcl":
            log.info(f"Dispatching to JCL analysis for file ID: {input_source_id}")
            jcl_analysis_result, m204_files_from_jcl = await _process_jcl_analysis(db, input_source, file_content)
            analysis_details = jcl_analysis_result
            message = jcl_analysis_result.summary or "JCL analysis processed."
            analysis_status_val = "analysis_completed"
            if m204_files_from_jcl:
                # Avoid duplicates if already added (e.g. if M204 analysis ran on another file in same transaction, though unlikely here)
                existing_ids = {mf.m204_file_id for mf in m204_files_potentially_updated_for_vsam_enhancement}
                for m204_file in m204_files_from_jcl:
                    if m204_file.m204_file_id not in existing_ids:
                        m204_files_potentially_updated_for_vsam_enhancement.append(m204_file)
                        existing_ids.add(m204_file.m204_file_id)


        elif file_type == "parmlib": 
            log.info(f"Dispatching to PARMLIB analysis for file ID: {input_source_id}")
            parmlib_details = await _process_parmlib_analysis(db, input_source, file_content)
            analysis_details = parmlib_details 
            message = (f"PARMLIB analysis processed. Found {len(parmlib_details.defined_fields_found)} field definitions. "
                       f"Identified/updated {len(parmlib_details.defined_files_found)} M204File entries from PARMLIB context.")
            analysis_status_val = "analysis_completed"
            if parmlib_details and parmlib_details.defined_files_found:
                m204_file_ids = [mf.m204_file_id for mf in parmlib_details.defined_files_found if mf.m204_file_id is not None]
                if m204_file_ids:
                    # Avoid duplicates if already added from M204 analysis
                    existing_ids = {mf.m204_file_id for mf in m204_files_potentially_updated_for_vsam_enhancement}
                    new_files_to_add = db.query(M204File).filter(
                        M204File.m204_file_id.in_(m204_file_ids),
                        ~M204File.m204_file_id.in_(existing_ids)
                    ).all()
                    m204_files_potentially_updated_for_vsam_enhancement.extend(new_files_to_add)
        else: 
            log.warning(f"Unsupported file type '{file_type}' for analysis of input_source_id: {input_source_id}.")
            unsupported_msg = f"File type '{file_type}' is not supported for detailed analysis."
            message = unsupported_msg
            analysis_status_val = "unsupported_type"
            errors.append(unsupported_msg)
            analysis_details = None 
        
        # --- VSAM Enhancement Step ---
        if m204_files_potentially_updated_for_vsam_enhancement and llm_config._llm:
            log.info(f"Starting VSAM enhancement for {len(m204_files_potentially_updated_for_vsam_enhancement)} M204 DB files related to input source {input_source_id}.")
            # Dedup list of M204File objects just in case, based on ID
            unique_m204_files_for_vsam = {mf.m204_file_id: mf for mf in m204_files_potentially_updated_for_vsam_enhancement}.values()
            log.debug(f"Unique m204 files or vsam: {unique_m204_files_for_vsam}")
            for m204_file_obj in unique_m204_files_for_vsam:
                if m204_file_obj.is_db_file: # Double check it's marked as a DB file
                    # Refresh the object and its fields relationship before passing to LLM
                    log.debug("It is a db file")
                    try:
                        db.refresh(m204_file_obj) 
                        # Eagerly load fields if not already loaded by selectin
                        if 'fields' not in sqlalchemy_inspect(m204_file_obj).unloaded:
                             _ = m204_file_obj.fields # Access to load
                    except Exception as e_refresh_vsam:
                        log.error(f"Could not refresh M204File {m204_file_obj.m204_file_id} before VSAM enhancement: {e_refresh_vsam}")
                        continue # Skip this file if refresh fails

                    await _enhance_single_m204_db_file_with_vsam_suggestions(db, m204_file_obj)
            try:
                db.commit() # Commit changes from VSAM enhancement
                log.info(f"Committed VSAM enhancement changes for input source {input_source_id}.")
            except Exception as e_commit_vsam:
                db.rollback()
                log.error(f"Failed to commit VSAM enhancement changes for input source {input_source_id}: {e_commit_vsam}", exc_info=True)
                errors.append(f"VSAM enhancement commit failed: {str(e_commit_vsam)}")
        elif not llm_config._llm:
             log.info("LLM not configured. Skipping VSAM enhancement step.")


        input_source.analysis_status = analysis_status_val
        if analysis_status_val in ["analysis_completed", "unsupported_type"] and not errors:
             input_source.error_message = None
        elif errors:
            input_source.error_message = "; ".join(list(set(errors)))[:1023] 
    
    except HTTPException as he: 
        log.error(f"HTTPException during analysis of {input_source_id} ({file_type}): {he.detail}", exc_info=True)
        http_error_msg = f"Analysis failed: {he.detail}"
        message = http_error_msg
        errors.append(http_error_msg)
        input_source.analysis_status = "analysis_failed"
        input_source.error_message = "; ".join(list(set(errors)))[:1023]
    except Exception as e:
        log.error(f"Unexpected error during analysis of {input_source_id} ({file_type}): {e}", exc_info=True)
        unexpected_error_msg = "An unexpected server error occurred during analysis."
        message = unexpected_error_msg
        errors.append(unexpected_error_msg)
        errors.append(str(e)) 
        input_source.analysis_status = "analysis_failed"
        input_source.error_message = "; ".join(list(set(errors)))[:1023]
    
    if input_source.analysis_status == "analysis_failed" and not input_source.error_message and errors:
        input_source.error_message = "; ".join(list(set(errors)))[:1023]
    elif input_source.analysis_status == "analysis_failed" and not input_source.error_message:
        input_source.error_message = "Analysis failed with an unspecified error."


    try:
        db.commit() # Commit final input_source status
    except Exception as e_commit_final:
        db.rollback()
        log.error(f"Failed to commit final analysis_status for {input_source_id}: {e_commit_final}", exc_info=True)
        final_commit_error_msg = f"Failed to save final analysis state: {str(e_commit_final)}"
        errors.append(final_commit_error_msg)
        input_source.analysis_status = "analysis_failed" 
        
        current_err_msg = input_source.error_message or ""
        additional_err = f"CommitFail: {str(e_commit_final)[:200]}"
        combined_errors = list(set(current_err_msg.split("; ") + [additional_err]))
        input_source.error_message = "; ".join(filter(None, combined_errors))[:1023]


    # Refresh analysis_details if M204Files were updated by VSAM step
    # This is to ensure the returned report reflects the latest VSAM data.
    if analysis_status_val == "analysis_completed" and isinstance(analysis_details, M204AnalysisResultDataSchema):
        updated_defined_files_found = []
        # Use the potentially larger list that includes files from JCL/PARMLIB for refreshing the response
        all_potentially_updated_files_map = {mf.m204_file_id: mf for mf in m204_files_potentially_updated_for_vsam_enhancement}

        for file_resp_schema in analysis_details.defined_files_found:
            matching_db_file = all_potentially_updated_files_map.get(file_resp_schema.m204_file_id)
            if matching_db_file:
                try:
                    # Ensure the DB session has the latest state of this object before validation
                    # If it was committed in the VSAM step, it should be up-to-date.
                    # A refresh here ensures we get any changes if the object was modified and committed.
                    db.refresh(matching_db_file) 
                    updated_defined_files_found.append(M204FileResponseSchema.model_validate(matching_db_file))
                except Exception as e_reval:
                    log.warning(f"Could not re-validate M204FileResponseSchema for {file_resp_schema.m204_file_id} after VSAM enhancement: {e_reval}. Using potentially stale data for response.")
                    updated_defined_files_found.append(file_resp_schema) # Keep old one if refresh/validation fails
            else:
                updated_defined_files_found.append(file_resp_schema)
        analysis_details.defined_files_found = updated_defined_files_found
    elif analysis_status_val == "analysis_completed" and isinstance(analysis_details, GenericAnalysisResultDataSchema):
        # JCL analysis doesn't directly return M204FileResponseSchema in its 'details'.
        # The summary message is the primary output. VSAM enhancement happens on DB.
        # No direct update to 'analysis_details.defined_files_found' is needed here as it's not part of GenericAnalysisResultDataSchema.
        pass


    return UnifiedAnalysisReportSchema(
        input_source_id=input_source.input_source_id,
        original_filename=input_source.original_filename,
        file_type_processed=file_type,
        analysis_status=input_source.analysis_status, 
        message=message,
        details=analysis_details,
        errors=list(set(errors)) 
    )
