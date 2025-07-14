import re
import json
import asyncio # Added for asyncio.gather
from typing import List, Optional, Dict, Any, Tuple # Added Tuple

from sqlalchemy.orm import Session
from sqlalchemy.sql import func
from pydantic import BaseModel, Field

from app.models.input_source_model import InputSource
from app.models.m204_file_model import M204File
from app.schemas.m204_analysis_schema import (
    M204AnalysisResultDataSchema,
    M204FileCreateSchema,
    M204FileResponseSchema
)
from app.utils.logger import log
from app.config.llm_config import llm_config

LLM_API_CALL_BATCH_SIZE = 20


class M204FieldVsamSuggestion(BaseModel):
    """Structured output for VSAM suggestions for a single M204 field."""
    m204_field_name: str = Field(description="The original M204 field name.")
    is_key_component: bool = Field(description="Is this field likely part of the VSAM primary key?")
    key_order: Optional[int] = Field(description="If part of the key, its order (1-based). Null if not a key component.", default=None)
    suggested_cobol_field_name: Optional[str] = Field(description="Suggested COBOL field name, following COBOL naming conventions (e.g., max 30 chars, alphanumeric, hyphens).", default=None)
    cobol_picture_clause: Optional[str] = Field(description="Suggested COBOL PICTURE clause (e.g., 'PIC X(20)', 'PIC 9(5) COMP-3'). Null if not applicable.", default=None)
    vsam_length: Optional[int] = Field(description="Suggested length in bytes for VSAM. Null if not applicable.", default=None)
    reasoning: Optional[str] = Field(description="Brief reasoning for these suggestions for the field.", default=None)


async def _get_llm_vsam_suggestions_for_field(field_name: str, attributes: List[str], file_name: str) -> Dict[str, Any]:
    """Get LLM suggestions for a single field and return as dict for JSON storage."""
    if not llm_config._llm:
        # The calling function _populate_vsam_suggestions_for_all_fields_parallel handles the global LLM check.
        # This function assumes LLM is available if called.
        return {} 

    log.info(f"PARMLIB_LLM_FIELD: Requesting VSAM suggestions for field: {field_name} in file: {file_name}")

    attributes_text = ", ".join(attributes) if attributes else "Not defined"

    prompt_fstr = f"""
You are an expert Mainframe M204 to COBOL/VSAM migration specialist.
Analyze the following M204 field from the PARMLIB definition.

M204 File: {file_name}
Field Name: {field_name}
Field Attributes: {attributes_text}

Based on the field attributes (look for KEY, ORDERED, UNIQUE, INDEXED keywords or typical key field names like 'CUSTOMER-ID', 'ORDER-NUMBER', etc.):

Provide suggestions for its role in a VSAM structure:
*   `m204_field_name`: string (Must be "{field_name}")
*   `is_key_component`: boolean (true if this field is likely part of the primary key for KSDS/RRDS)
*   `key_order`: integer (1-based order if it's a key component, e.g., 1 for the first part of a composite key. Null if not a key component.)
*   `suggested_cobol_field_name`: string (A suitable COBOL field name for this M204 field. Follow COBOL naming conventions: max 30 chars, alphanumeric, use hyphens, avoid M204-specific symbols. Null if not applicable.)
*   `cobol_picture_clause`: string (suggest a COBOL PICTURE clause like "PIC X(10)", "PIC 9(7) COMP-3". Null if not applicable.)
*   `vsam_length`: integer (suggested length in bytes for VSAM. Null if not applicable.)
*   `reasoning`: string (briefly why you made these suggestions for this field)

Respond with a JSON object structured according to the M204FieldVsamSuggestion model.
"""
    json_text_output = "" # Initialize to handle potential errors before assignment
    try:
        field_vsam_suggester_llm = llm_config._llm.as_structured_llm(M204FieldVsamSuggestion)
        completion_response = await field_vsam_suggester_llm.acomplete(prompt=prompt_fstr)
        json_text_output = completion_response.text

        log.info(f"PARMLIB_LLM_FIELD: Raw LLM JSON response for field '{field_name}': {json_text_output}")

        
        loaded_suggestion_data = json.loads(json_text_output)
        field_suggestion = M204FieldVsamSuggestion(**loaded_suggestion_data)

        if field_suggestion.m204_field_name != field_name:
            log.warning(f"PARMLIB_LLM_FIELD: LLM returned VSAM data for field '{field_suggestion.m204_field_name}' but expected '{field_name}'. Using anyway.")

        # Return the model dump which includes all fields, even if None by default
        return field_suggestion.model_dump(exclude_none=False) # Ensure all fields are present for consistency

    except json.JSONDecodeError as e_json:
        log.error(f"PARMLIB_LLM_FIELD: JSON parsing error for field {field_name} in file {file_name}: {e_json}. Raw output: '{json_text_output}'")
        return {"error": f"JSON parsing error: {e_json}", "raw_output": json_text_output, "m204_field_name": field_name}
    except Exception as e_llm:
        log.error(f"PARMLIB_LLM_FIELD: LLM error for field {field_name} in file {file_name}: {e_llm}. Raw output: '{json_text_output if json_text_output else 'N/A'}'")
        return {"error": f"LLM error: {e_llm}", "raw_output": json_text_output if json_text_output else 'N/A', "m204_field_name": field_name}

async def _populate_vsam_suggestions_for_all_fields_parallel(
    all_parsed_file_contexts: List[Dict[str, Any]]
):
    """
    Populates VSAM suggestions for all fields across multiple file contexts using parallel LLM calls with batching.
    Modifies the `all_parsed_file_contexts` list in place.
    """
    if not llm_config._llm:
        log.info("PARMLIB_LLM_BATCH: LLM not configured. Skipping batch VSAM suggestions.")
        # Ensure vsam_suggestions key exists for all fields if LLM is off, and m204_field_name is present
        for file_data in all_parsed_file_contexts:
            for field_name, field_info_dict in file_data.get("fields", {}).items():
                if "vsam_suggestions" not in field_info_dict:
                    field_info_dict["vsam_suggestions"] = {"m204_field_name": field_name} # Add field name for consistency
        return

    # Stores tuples of (task_coroutine, field_info_dict_ref, original_field_name_str)
    # This helps in associating results back to the correct field_info_dict after batch processing.
    llm_tasks_with_references: List[Tuple[Any, Dict[str, Any], str]] = [] 

    for file_data in all_parsed_file_contexts:
        current_file_name = file_data.get("file_name", "UNKNOWN_FILE")
        for field_name, field_info_dict in file_data.get("fields", {}).items():
            attributes = field_info_dict.get("attributes", [])
            # Create the coroutine for the LLM call
            task_coro = _get_llm_vsam_suggestions_for_field(field_name, attributes, current_file_name)
            # Store the coroutine along with the dictionary to update and the original field name
            llm_tasks_with_references.append((task_coro, field_info_dict, field_name))

    if not llm_tasks_with_references:
        log.info("PARMLIB_LLM_BATCH: No fields found requiring LLM VSAM suggestions.")
        return

    log.info(f"PARMLIB_LLM_BATCH: Preparing {len(llm_tasks_with_references)} LLM calls for VSAM field suggestions to be processed in batches of {LLM_API_CALL_BATCH_SIZE}.")
    
    all_llm_results_ordered = [] # To store results in the original order of tasks

    for i in range(0, len(llm_tasks_with_references), LLM_API_CALL_BATCH_SIZE):
        # Get the current batch of tasks with their references
        current_batch_with_refs = llm_tasks_with_references[i:i + LLM_API_CALL_BATCH_SIZE]
        # Extract just the coroutines for asyncio.gather
        batch_coroutines = [item[0] for item in current_batch_with_refs]
        
        batch_number = (i // LLM_API_CALL_BATCH_SIZE) + 1
        log.info(f"PARMLIB_LLM_BATCH: Processing batch {batch_number} with {len(batch_coroutines)} tasks.")
        
        try:
            batch_results = await asyncio.gather(*batch_coroutines, return_exceptions=True)
            all_llm_results_ordered.extend(batch_results)
            log.info(f"PARMLIB_LLM_BATCH: Finished batch {batch_number}.")
        except Exception as e_batch_gather:
            # This catch is a safeguard, as return_exceptions=True should prevent gather from raising directly.
            # Individual exceptions will be in batch_results.
            log.error(f"PARMLIB_LLM_BATCH: Unexpected error during asyncio.gather for batch {batch_number}: {e_batch_gather}", exc_info=True)
            # Fill results for this batch with error objects to maintain order and length
            error_result = {"error": f"Batch gather error: {e_batch_gather}", "m204_field_name": "BATCH_ERROR"}
            all_llm_results_ordered.extend([error_result] * len(batch_coroutines))


    log.info(f"PARMLIB_LLM_BATCH: Finished all {len(llm_tasks_with_references)} LLM calls across all batches.")

    # Now, iterate through the ordered results and update the original field_info_dicts
    for idx, result_or_exc in enumerate(all_llm_results_ordered):
        # Get the original field_info_dict and field_name using the index from llm_tasks_with_references
        _task_coro, field_info_to_update, original_field_name = llm_tasks_with_references[idx] 
        
        if isinstance(result_or_exc, Exception):
            log.error(f"PARMLIB_LLM_BATCH: Error processing field '{original_field_name}': {result_or_exc}", exc_info=result_or_exc)
            field_info_to_update["vsam_suggestions"] = {"error": str(result_or_exc), "m204_field_name": original_field_name}
        elif isinstance(result_or_exc, dict):
            # Ensure m204_field_name is present in the result, even if it's an error dict from _get_llm_vsam_suggestions_for_field
            if "m204_field_name" not in result_or_exc:
                # This case might happen if the error occurred before m204_field_name was added,
                # or if the result is from the batch_gather safeguard.
                result_or_exc["m204_field_name"] = original_field_name
            field_info_to_update["vsam_suggestions"] = result_or_exc 
            log.debug(f"PARMLIB_LLM_BATCH: Successfully updated VSAM suggestions for field '{original_field_name}'.")
        else:
            log.error(f"PARMLIB_LLM_BATCH: Unexpected result type for field '{original_field_name}': {type(result_or_exc)}. Result: {result_or_exc}")
            field_info_to_update["vsam_suggestions"] = {"error": "Unexpected result type from LLM task.", "m204_field_name": original_field_name}



async def _extract_m204_files_from_parmlib(
    db: Session, input_source: InputSource, file_content: str
) -> List[M204File]:
    """
    Extracts PARMLIB data for a single M204 file, using the user-provided association.
    The entire PARMLIB file content is treated as the definition for the single
    M204 DB file name stored in `input_source.m204_db_file_name_association`.
    """
    log.info(
        f"PARMLIB_PARSE: Starting extraction from PARMLIB file ID "
        f"{input_source.input_source_id} ({input_source.original_filename})"
    )

    # --- New logic: Rely on user-provided association ---
    m204_db_file_name = input_source.m204_db_file_name_association
    if not m204_db_file_name:
        log.error(
            f"PARMLIB_PARSE: Aborting analysis for InputSource ID "
            f"{input_source.input_source_id}. No M204 DB file name association was "
            f"provided for this PARMLIB file."
        )
        # This is a critical failure for the new workflow.
        # We could raise an exception or return an empty list with a clear log message.
        return []

    log.info(
        f"PARMLIB_PARSE: Associating this PARMLIB content with the user-specified "
        f"M204 DB file: '{m204_db_file_name}'"
    )

    # Initialize a single file context for the entire file.
    file_data: Dict[str, Any] = {
        "file_type": "db_file",
        "source": "parmlib",
        "file_name": m204_db_file_name.upper(),
        "definition_start_line": 1, # The definition is the whole file
        "root_level_commands": [],
        "fields": {}
    }

    lines = file_content.splitlines()
    field_def_pattern = re.compile(
        r"^\s*DEFINE\s+FIELD\s+([A-Z0-9_#@$-]+)",
        re.IGNORECASE
    )
    root_cmd_pattern = re.compile(
        r"^\s*(RESET|SET|PARAMETER|FILE|M204FILE)", # Also capture FILE/M204FILE as a root command now
        re.IGNORECASE
    )

    i = 0
    while i < len(lines):
        line_content_original = lines[i]
        start_line = i + 1
        log.debug(f"Line {start_line}: raw content: {line_content_original!r}")

        # Handle line continuations ending with '-'
        logical_idx = i
        line_for_parsing = line_content_original
        while (line_for_parsing.rstrip().endswith("-")
               and logical_idx + 1 < len(lines)):
            next_part = lines[logical_idx + 1].strip()
            line_for_parsing = (
                line_for_parsing.rstrip()[:-1] + " " + next_part
            )
            logical_idx += 1

        line_stripped = line_for_parsing.strip()
        log.debug(f"Line {start_line}: logical content: {line_for_parsing!r}")

        # Skip blank or comment lines
        if not line_stripped or line_stripped[0] in ("*", "#", ";"):
            log.debug(f"Line {start_line}: skipped blank/comment")
            i = logical_idx + 1
            continue

        # Detect root-level commands (now includes FILE/M204FILE)
        m_root = root_cmd_pattern.match(line_stripped)
        if m_root:
            file_data["root_level_commands"].append({
                "command_text": line_stripped,
                "line_number": start_line
            })
            log.debug(
                f"PARMLIB_PARSE: Added root-level command: "
                f"'{line_stripped}' for file "
                f"{file_data['file_name']} at line {start_line}"
            )
            i = logical_idx + 1
            continue

        # Detect field definitions
        m_field = field_def_pattern.match(line_stripped)
        if m_field:
            field_name = m_field.group(1)
            log.debug(
                f"Line {start_line}: matched DEFINE FIELD, parsing: "
                f"{line_for_parsing!r}"
            )

            attributes_list: List[str] = []
            m_paren = re.search(r"\((.*?)\)", line_for_parsing)
            if m_paren:
                raw_attrs = m_paren.group(1)
                cleaned = re.sub(r"\s+", " ", raw_attrs).strip()
                for attr in cleaned.split(","):
                    a = attr.strip()
                    if a:
                        attributes_list.append(a)
                log.debug(
                    f"Line {start_line}: extracted attributes "
                    f"{attributes_list}"
                )
            else:
                log.debug(
                    f"Line {start_line}: no attributes found for field "
                    f"'{field_name}'"
                )

            file_data["fields"][field_name] = {
                "attributes": attributes_list,
                "line_number": start_line,
                "vsam_suggestions": {"m204_field_name": field_name}
            }
            log.debug(
                "PARMLIB_PARSE: Staged field "
                f"'{field_name}' for file "
                f"{file_data['file_name']} from line "
                f"{start_line}"
            )
            i = logical_idx + 1
            continue

        log.debug(
            f"Line {start_line}: did not match any known pattern, skipping"
        )
        i = logical_idx + 1

    # The single file context is now fully populated.
    all_parsed_file_contexts = [file_data]
    log.info(
        "PARMLIB_PARSE: Initial parsing complete. "
        f"Processed content for M204 DB file '{m204_db_file_name}'."
    )

    # Enrich field suggestions via parallel LLM calls
    await _populate_vsam_suggestions_for_all_fields_parallel(
        all_parsed_file_contexts
    )
    log.info(
        "PARMLIB_PARSE: Parallel LLM processing for field "
        "suggestions complete."
    )

    # Persist the single parsed context
    processed_db_files: List[M204File] = []
    saved = await _save_parmlib_file(db, input_source, file_data)
    if saved:
        processed_db_files.append(saved)

    log.info(
        "PARMLIB_PARSE: Finished processing and saving M204 DB file "
        f"'{m204_db_file_name}' from PARMLIB for "
        f"input source {input_source.input_source_id}."
    )
    return processed_db_files


async def _save_parmlib_file(db: Session, input_source: InputSource, file_data: Dict[str, Any]) -> Optional[M204File]:
    """Save or update M204File with JSON data for DB files"""
    file_name = file_data["file_name"]
    
    # Find existing file
    m204_file = db.query(M204File).filter(
        M204File.project_id == input_source.project_id,
        func.upper(M204File.m204_file_name) == file_name # Match on m204_file_name for PARMLIB
    ).first()
    
    if not m204_file:
        # Create new DB file
        try:
            # PARMLIB file_name is the m204_file_name. m204_logical_dataset_name can be the same.
            file_create_data = M204FileCreateSchema(
                project_id=input_source.project_id,
                m204_file_name=file_name,
                m204_logical_dataset_name=file_name, # For PARMLIB, logical name often matches file name
                defined_in_input_source_id=input_source.input_source_id,
                definition_line_number_start=file_data.get("definition_start_line"),
                is_db_file=True, # PARMLIB definitions are for DB files
                file_definition_json=file_data # Store the parsed PARMLIB structure
            )
            m204_file = M204File(**file_create_data.model_dump(exclude_none=True))
            db.add(m204_file)
            # db.flush() # Not strictly necessary here, commit will handle it.
            log.info(f"PARMLIB_SAVE: Created new M204 DB file '{file_name}' with {len(file_data.get('fields', {}))} fields from PARMLIB.")
        except Exception as e:
            log.error(f"PARMLIB_SAVE: Error creating M204File for PARMLIB file '{file_name}': {e}", exc_info=True)
            return None
    else:
        # Update existing file
        update_needed = False

        if m204_file.defined_in_input_source_id != input_source.input_source_id:
            m204_file.defined_in_input_source_id = input_source.input_source_id
            update_needed = True
        
        new_start_line = file_data.get("definition_start_line")
        if new_start_line is not None and m204_file.definition_line_number_start != new_start_line:
            m204_file.definition_line_number_start = new_start_line
            update_needed = True
            
        if not m204_file.is_db_file:
            m204_file.is_db_file = True # Mark as DB file if PARMLIB defines it
            update_needed = True

        # PARMLIB definitions are authoritative for DB structure.
        # If an IMAGE statement was processed first for the same logical name,
        # the PARMLIB data should ideally augment or take precedence for DB aspects.
        existing_definition = m204_file.file_definition_json or {}
        
        # Simple overwrite for PARMLIB data if it's different.
        # More complex merging could be done if IMAGE and PARMLIB provide conflicting field lists,
        # but for now, assume PARMLIB is the source of truth for DB fields.
        if existing_definition != file_data:
            m204_file.file_definition_json = file_data
            update_needed = True
        
        if update_needed:
            db.add(m204_file) # Add to session for update
            log.info(f"PARMLIB_SAVE: M204File '{file_name}' (ID: {m204_file.m204_file_id}) marked for update with PARMLIB data.")
        else:
            log.info(f"PARMLIB_SAVE: No update deemed necessary for M204File '{file_name}' (ID: {m204_file.m204_file_id}) based on new PARMLIB data.")
            
    return m204_file


async def process_parmlib_analysis(db: Session, input_source: InputSource, file_content: str) -> Tuple[M204AnalysisResultDataSchema, List[int]]:
    """
    Main function to process PARMLIB file content with simplified JSON storage.
    """
    log.info(f"PARMLIB_PROCESS: Starting PARMLIB analysis for file: {input_source.original_filename} (ID: {input_source.input_source_id})")
    
    processed_files = await _extract_m204_files_from_parmlib(db, input_source, file_content)
    
    refreshed_files: List[M204File] = []
    if processed_files:
        try:
            # Flush all pending changes from _save_parmlib_file to ensure IDs are set
            # and objects are in a consistent state before refresh.
            log.debug(f"PARMLIB_PROCESS: Flushing session before refreshing {len(processed_files)} files.")
            db.flush() 
        except Exception as e_flush_all:
            log.error(f"PARMLIB_PROCESS: Error during session flush before refresh: {e_flush_all}", exc_info=True)
            # Proceed with current state of objects if flush fails

        for m204_file_obj in processed_files:
            try:
                # Only refresh if it's persistent (has an identity) and is still in the session
                if m204_file_obj in db and m204_file_obj.m204_file_id is not None:
                    db.refresh(m204_file_obj)
                    refreshed_files.append(m204_file_obj)
                elif m204_file_obj.m204_file_id is not None: # Has ID but maybe not in this session's identity map anymore
                    log.warning(f"PARMLIB_PROCESS: M204File {m204_file_obj.m204_file_id} has ID but not in current session's tracked objects prior to refresh. Appending as is.")
                    refreshed_files.append(m204_file_obj)
                else: # No ID, likely means it wasn't persisted correctly or flush failed
                    log.warning(f"PARMLIB_PROCESS: M204File for '{m204_file_obj.m204_file_name}' has no ID. Appending as is (potentially uncommitted).")
                    refreshed_files.append(m204_file_obj)
            except Exception as e_refresh:
                log.warning(f"PARMLIB_PROCESS: Could not refresh M204File {getattr(m204_file_obj, 'm204_file_id', 'N/A')}: {e_refresh}. Using current session state for response.")
                refreshed_files.append(m204_file_obj) 
    else:
        log.info("PARMLIB_PROCESS: No files were processed from PARMLIB content.")


    file_responses = [M204FileResponseSchema.model_validate(f) for f in refreshed_files]
    m204_db_file_ids_identified = [f.m204_file_id for f in refreshed_files if f.is_db_file and f.m204_file_id is not None]

    analysis_result = M204AnalysisResultDataSchema(
        procedures_found=[],
        defined_files_found=file_responses,
        defined_fields_found=[], 
        variables_found=[],
        procedure_calls_found=[]
    )
    log.info(f"PARMLIB_PROCESS: Completed PARMLIB analysis for file: {input_source.original_filename}. Found {len(file_responses)} defined files.")
    return analysis_result, m204_db_file_ids_identified