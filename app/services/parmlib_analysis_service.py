import re
import json
from typing import List, Optional, Dict, Any

from sqlalchemy.orm import Session
from sqlalchemy.sql import func
from pydantic import BaseModel, Field

from app.models.input_source_model import InputSource
from app.models.m204_file_model import M204File
from app.models.m204_file_field_model import M204Field
from app.schemas.m204_analysis_schema import (
    M204AnalysisResultDataSchema,
    M204DefineFieldCreateSchema, M204DefineFieldResponseSchema,
    M204FileResponseSchema
)
from app.utils.logger import log
from app.config.llm_config import llm_config
# Note: _shared_rag_service is not directly used by these PARMLIB functions.
# If future PARMLIB LLM tasks require RAG, it might need to be passed.

class M204FieldVsamSuggestion(BaseModel):
    """Structured output for VSAM suggestions for a single M204 field."""
    m204_field_name: str = Field(description="The original M204 field name.")
    is_key_component: bool = Field(description="Is this field likely part of the VSAM primary key?")
    key_order: Optional[int] = Field(description="If part of the key, its order (1-based). Null if not a key component.", default=None)
    vsam_data_type: Optional[str] = Field(description="Suggested COBOL PICTURE clause or VSAM data type (e.g., 'PIC X(20)', 'PIC 9(5) COMP-3'). Null if not applicable.", default=None)
    vsam_length: Optional[int] = Field(description="Suggested length in bytes for VSAM. Null if not applicable.", default=None)
    reasoning: Optional[str] = Field(description="Brief reasoning for these suggestions for the field.", default=None)


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
                # Debug logging for M204File lookup context
                # try:
                #     all_project_m204_files_in_parmlib_context = db.query(
                #         M204File.m204_file_id, 
                #         M204File.m204_file_name, 
                #         M204File.m204_logical_dataset_name, 
                #         M204File.defined_in_input_source_id,
                #         M204File.project_id
                #     ).filter(M204File.project_id == input_source.project_id).all()
                #     if not all_project_m204_files_in_parmlib_context:
                #         log.debug(f"PARMLIB_DEBUG: No M204File entries found in DB for project_id {input_source.project_id} when searching for file name '{current_file_context_name}' in {input_source.original_filename}.")
                #     else:
                #         log.debug(f"PARMLIB_DEBUG: M204File entries in DB for project_id {input_source.project_id} (before looking up file name '{current_file_context_name}' for {input_source.original_filename}):")
                #         for mf_id, mf_name, mf_logical, mf_def_src_id, mf_proj_id in all_project_m204_files_in_parmlib_context:
                #             log.debug(f"  - ProjID: {mf_proj_id}, M204FileID: {mf_id}, FileName: '{mf_name}', LogicalName: '{mf_logical}', DefinedInSrcID: {mf_def_src_id}")
                # except Exception as e_debug_query_parmlib:
                #     log.error(f"PARMLIB_DEBUG: Error querying M204Files for debugging: {e_debug_query_parmlib}")

                log.debug(f"PARMLIB_PARSE: Looking up M204File with m204_file_name '{current_file_context_name}' for project ID {input_source.project_id}")
                db_m204_file_obj = db.query(M204File).filter(
                    M204File.project_id == input_source.project_id,
                    func.upper(M204File.m204_file_name) == current_file_context_name # current_file_context_name is already uppercased
                ).first()

                if db_m204_file_obj:
                    m204_file_id_for_field = db_m204_file_obj.m204_file_id
                    log.info(f"PARMLIB_PARSE: Found existing M204File by m204_file_name '{current_file_context_name}' (ID: {m204_file_id_for_field}, Logical Name: '{db_m204_file_obj.m204_logical_dataset_name}') for field '{field_name}'.")
                    if db_m204_file_obj.is_db_file is not True: # Mark as DB file if fields are being defined for it
                        db_m204_file_obj.is_db_file = True 
                        db.add(db_m204_file_obj)
                        log.info(f"PARMLIB_PARSE: Marked M204File '{current_file_context_name}' (ID: {m204_file_id_for_field}) as DB file due to PARMLIB field definitions.")
                else:
                    log.warning(f"PARMLIB_PARSE: M204File with m204_file_name '{current_file_context_name}' was NOT FOUND for project ID {input_source.project_id}. Field '{field_name}' at line {line_num} in {input_source.original_filename} will not be associated with an M204File.")
                    continue # Skip this field if its parent M204File context is not found
            else:
                log.warning(f"PARMLIB_PARSE: Field '{field_name}' defined in PARMLIB '{input_source.original_filename}' at line {line_num} is outside a 'FILE' context. It will not be associated with any M204File.")
                continue # Skip this field if it's not within a FILE context

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
                    log.debug(f"PARMLIB_PARSE: No updates needed for existing M204Field '{field_name}' (ID: {existing_field.m204_field_id}).")
                extracted_fields.append(existing_field)
                field_object_for_enhancement = existing_field
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


async def process_parmlib_analysis(db: Session, input_source: InputSource, file_content: str) -> M204AnalysisResultDataSchema:
    """
    Main function to process PARMLIB file content.
    This function will be called by the main analysis_service orchestrator.
    """
    log.info(f"Starting PARMLIB analysis for file: {input_source.original_filename} (ID: {input_source.input_source_id}) in service: parmlib_analysis_service")
    
    defined_fields_extracted = await _extract_m204_fields_from_parmlib(db, input_source, file_content)
    
    # Collect M204Files that were potentially modified (e.g., is_db_file flag set) or created during field extraction.
    # These files are already in the session (db.dirty or db.new).
    # We need to ensure they are committed and refreshed to get their latest state for the response.
    
    # The main orchestrator (analysis_service.perform_source_file_analysis) will handle the commit.
    # This function should return the extracted data.
    
    # Refresh objects to ensure response models get the latest data, especially if LLM updated them.
    # This might be redundant if the orchestrator commits and refreshes globally,
    # but good for ensuring this module returns fresh data.
    refreshed_fields = []
    for field in defined_fields_extracted:
        try:
            if field in db.new or field in db.dirty: # Ensure flush before refresh if pending
                db.flush()
            db.refresh(field)
            refreshed_fields.append(field)
        except Exception as e_refresh_field:
            log.warning(f"Could not refresh M204Field {getattr(field, 'm204_field_id', 'N/A')} during PARMLIB processing: {e_refresh_field}. Using potentially uncommitted data for response.")
            refreshed_fields.append(field) # Add it anyway

    field_responses = [M204DefineFieldResponseSchema.model_validate(f) for f in refreshed_fields]
    
    # Identify M204Files associated with the extracted fields to include in the response.
    # These files might have been marked as is_db_file=True.
    m204_file_ids_from_fields = list(set(f.m204_file_id for f in refreshed_fields if f.m204_file_id is not None))
    
    updated_m204_files_for_response: List[M204File] = []
    if m204_file_ids_from_fields:
        updated_m204_files_for_response = db.query(M204File).filter(
            M204File.m204_file_id.in_(m204_file_ids_from_fields)
        ).all()
        # Refresh these M204File objects as well
        refreshed_m204_files = []
        for mfile in updated_m204_files_for_response:
            try:
                if mfile in db.new or mfile in db.dirty:
                    db.flush()
                db.refresh(mfile)
                refreshed_m204_files.append(mfile)
            except Exception as e_refresh_mfile:
                log.warning(f"Could not refresh M204File {getattr(mfile, 'm204_file_id', 'N/A')} during PARMLIB processing: {e_refresh_mfile}. Using potentially uncommitted data for response.")
                refreshed_m204_files.append(mfile)
        updated_m204_files_for_response = refreshed_m204_files


    m204_file_responses_from_parmlib = [M204FileResponseSchema.model_validate(mf) for mf in updated_m204_files_for_response]

    return M204AnalysisResultDataSchema(
        procedures_found=[],
        defined_files_found=m204_file_responses_from_parmlib, 
        defined_fields_found=field_responses,
        variables_found=[],
        procedure_calls_found=[],
        image_statements_found=[]
    )