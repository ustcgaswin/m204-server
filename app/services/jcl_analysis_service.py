import re
from typing import List, Tuple, Optional, Dict, Any

from sqlalchemy.orm import Session
from sqlalchemy.sql import func

from app.models.input_source_model import InputSource
from app.models.m204_file_model import M204File
from app.models.dd_statement_model import DDStatement

from app.schemas.generic_analysis_schema import (
    DDStatementCreateSchema, DDStatementResponseSchema,
    GenericAnalysisResultDataSchema
)
from app.utils.logger import log

async def _extract_and_store_dd_statements_from_jcl(
    db: Session, input_source: InputSource, file_content: str
) -> Tuple[List[DDStatement], List[M204File]]:
    """
    Extracts DD statements from JCL content, stores them, and identifies/updates
    associated M204File records.
    Returns a list of created/updated DDStatement objects and a list of
    M204File objects that were affected by this JCL.
    """
    log.info(f"JCL_SERVICE: Extracting DD statements from JCL file ID: {input_source.input_source_id} ({input_source.original_filename}) for project ID: {input_source.project_id}")
    dd_statements_processed: List[DDStatement] = []
    m204_files_affected_by_jcl: List[M204File] = []

    lines = file_content.splitlines()
    job_pattern = re.compile(r"^\/\/\s*([A-Z0-9#@$-]{1,8})\s+JOB\s+", re.IGNORECASE)
    exec_pattern = re.compile(r"^\/\/\s*([A-Z0-9#@$-]{1,8})?\s+EXEC\s+(?:PGM=([A-Z0-9#@$-]{1,8})|PROC=([A-Z0-9#@$-]{1,8})|\S+)", re.IGNORECASE)
    dd_pattern = re.compile(r"^\/\/\s*([A-Z0-9#@$-.]{1,8})\s+DD\s+(.*)", re.IGNORECASE)
    current_job_name: Optional[str] = None
    current_step_name: Optional[str] = None
    
    for i, line_content_full in enumerate(lines):
        line_num = i + 1
        line_stripped = line_content_full.strip()

        if not line_stripped or line_stripped.startswith("*"): # Skip empty or full-line comment
            if "//*" not in line_stripped: # JCL comments specifically start with //*
                 continue

        if job_match := job_pattern.match(line_stripped):
            current_job_name = job_match.group(1)
            current_step_name = None # Reset step name on new JOB
            log.debug(f"JCL_SERVICE_PARSE: JOB detected: {current_job_name} at line {line_num}")
            continue
        if exec_match := exec_pattern.match(line_stripped):
            current_step_name = exec_match.group(1) or \
                                (f"PGM_{exec_match.group(2)}" if exec_match.group(2) else \
                                (f"PROC_{exec_match.group(3)}" if exec_match.group(3) else "ANON_STEP"))
            log.debug(f"JCL_SERVICE_PARSE: EXEC detected: {current_step_name} in JOB {current_job_name} at line {line_num}")
            continue
        
        dd_match = dd_pattern.match(line_stripped)
        if dd_match:
            dd_name_raw = dd_match.group(1)
            dd_params_str = dd_match.group(2)
            dd_name = dd_name_raw.strip().upper() # Normalize DD name

            log.debug(f"JCL_SERVICE_PARSE: DD statement found: Name='{dd_name}' (Raw='{dd_name_raw}'), Params='{dd_params_str[:100]}...' at line {line_num} in Job='{current_job_name}', Step='{current_step_name}'")

            dsn_val: Optional[str] = None
            disp_val: Optional[str] = None
            params_dict: Dict[str, Any] = {"raw_parameters_string": dd_params_str}

            # Enhanced DSN parsing to handle various formats including PDS members
            dsn_search = re.search(r"(?:DSN|DATASET)\s*=\s*((?:[^,\s(]+(?:\([^)]+\))?)|(?:&[^,\s]+))", dd_params_str, re.IGNORECASE)
            if dsn_search:
                dsn_val = dsn_search.group(1)
                log.debug(f"JCL_SERVICE_PARSE: Extracted DSN: '{dsn_val}' for DD '{dd_name}'")

            disp_search = re.search(r"DISP\s*=\s*(?:\(([^)]+)\)|([A-Z0-9#@$-]+(?:,[A-Z0-9#@$-]+)*))", dd_params_str, re.IGNORECASE)
            if disp_search:
                disp_val = disp_search.group(1) or disp_search.group(2)
                log.debug(f"JCL_SERVICE_PARSE: Extracted DISP: '{disp_val}' for DD '{dd_name}'")
            
            is_explicit_m204_db_dsn = False
            if dsn_val:
                # Check for common M204 database file naming patterns
                if re.search(r"\.DBASEF", dsn_val, re.IGNORECASE) or \
                   re.search(r"\.[A-Z0-9#@$-]*204", dsn_val, re.IGNORECASE) or \
                   re.search(r"M204\.DB", dsn_val, re.IGNORECASE): # Added another common pattern
                    is_explicit_m204_db_dsn = True
                    log.debug(f"JCL_SERVICE_PARSE: DSN '{dsn_val}' for DD '{dd_name}' identified as explicit M204 DB DSN.")
            
            # Attempt to find a matching M204File by DDNAME (m204_file_name)
            # M204File.m204_file_name is expected to be the DDNAME.
            log.debug(f"JCL_SERVICE_MATCH: Looking up M204File with m204_file_name (DDNAME) '{dd_name}' for project ID {input_source.project_id}")
            existing_m204_file = db.query(M204File).filter(
                M204File.project_id == input_source.project_id,
                func.upper(M204File.m204_file_name) == dd_name # dd_name is already uppercased
            ).first()

            if existing_m204_file:
                log.info(f"JCL_SERVICE_MATCH: Found matching M204File for JCL DD '{dd_name}': ID={existing_m204_file.m204_file_id}, Stored m204_file_name='{existing_m204_file.m204_file_name}'")
                update_m204_file = False
                if is_explicit_m204_db_dsn and existing_m204_file.is_db_file is not True:
                    existing_m204_file.is_db_file = True
                    update_m204_file = True
                    log.debug(f"JCL_SERVICE_UPDATE: Marking M204File '{dd_name}' as DB file due to explicit DSN.")
                elif not is_explicit_m204_db_dsn and existing_m204_file.is_db_file is None: 
                    # If not explicitly M204 and not yet set, assume not a DB file from JCL context alone
                    existing_m204_file.is_db_file = False 
                    update_m204_file = True
                    log.debug(f"JCL_SERVICE_UPDATE: Marking M204File '{dd_name}' as NOT a DB file (is_db_file=None previously).")

                if dsn_val and existing_m204_file.target_vsam_dataset_name != dsn_val:
                    existing_m204_file.target_vsam_dataset_name = dsn_val
                    update_m204_file = True
                    log.debug(f"JCL_SERVICE_UPDATE: Updating target_vsam_dataset_name for M204File '{dd_name}' to '{dsn_val}'.")
                
                if update_m204_file:
                    db.add(existing_m204_file)
                    if existing_m204_file not in m204_files_affected_by_jcl:
                        m204_files_affected_by_jcl.append(existing_m204_file)
                    log.info(f"JCL_SERVICE_UPDATE: Updated M204File '{existing_m204_file.m204_file_name}' (ID: {existing_m204_file.m204_file_id}) based on JCL DD statement '{dd_name}'.")
            else:
                log.warning(f"JCL_SERVICE_MATCH: JCL DD statement for '{dd_name}' in {input_source.original_filename} (project_id: {input_source.project_id}, line: {line_num}) does not match any existing M204File by m204_file_name (DDNAME).")

            # Process DDStatement record
            existing_dd = db.query(DDStatement).filter_by(
                project_id=input_source.project_id, 
                input_source_id=input_source.input_source_id,
                job_name=current_job_name, 
                step_name=current_step_name,
                dd_name=dd_name, 
                line_number_start=line_num 
            ).first()

            if existing_dd:
                update_dd_needed = False
                if existing_dd.parameters_json.get("raw_parameters_string") != dd_params_str:
                    existing_dd.parameters_json = params_dict
                    update_dd_needed = True
                if existing_dd.dsn != dsn_val:
                    existing_dd.dsn = dsn_val
                    update_dd_needed = True
                if existing_dd.disposition != disp_val:
                    existing_dd.disposition = disp_val
                    update_dd_needed = True
                if existing_dd.raw_statement_text != line_stripped: # Store the single, stripped line
                    existing_dd.raw_statement_text = line_stripped
                    update_dd_needed = True
                
                if update_dd_needed:
                    db.add(existing_dd)
                    log.info(f"JCL_SERVICE_DD: Updating existing DDStatement '{dd_name}' (ID: {existing_dd.dd_statement_id}).")
                dd_statements_processed.append(existing_dd)
            else: 
                try:
                    dd_data = DDStatementCreateSchema(
                        project_id=input_source.project_id, 
                        input_source_id=input_source.input_source_id,
                        job_name=current_job_name, 
                        step_name=current_step_name,
                        dd_name=dd_name, 
                        dsn=dsn_val, 
                        disposition=disp_val,
                        line_number_start=line_num, 
                        line_number_end=line_num, # Assuming single line for now
                        raw_statement_text=line_stripped, # Store the single, stripped line
                        parameters_json=params_dict
                    )
                    db_dd_statement = DDStatement(**dd_data.model_dump(exclude_none=True))
                    db.add(db_dd_statement)
                    dd_statements_processed.append(db_dd_statement)
                    log.info(f"JCL_SERVICE_DD: Created new DDStatement '{dd_name}'.")
                except Exception as e: 
                    log.error(f"JCL_SERVICE_DD: Error creating DB entry for DD statement '{dd_name}' at line {line_num}: {e}", exc_info=True)
    
    log.info(f"JCL_SERVICE: Finished extracting {len(dd_statements_processed)} DD statements and identified {len(m204_files_affected_by_jcl)} affected M204 files from JCL file ID: {input_source.input_source_id}.")
    return dd_statements_processed, m204_files_affected_by_jcl


async def process_jcl_analysis(
    db: Session, input_source: InputSource, file_content: str
) -> Tuple[GenericAnalysisResultDataSchema, List[M204File]]:
    """
    Main function to process JCL file content.
    This function will be called by the main analysis_service orchestrator.
    """
    log.info(f"JCL_SERVICE: Starting JCL analysis for file: {input_source.original_filename} (ID: {input_source.input_source_id})")
    
    extracted_dd_statements, m204_files_from_jcl = await _extract_and_store_dd_statements_from_jcl(db, input_source, file_content)
    
    # The main orchestrator (analysis_service.perform_source_file_analysis) will handle the commit.
    # This function should return the extracted data.
    
    # Refresh objects to ensure response models get the latest data.
    refreshed_dds = []
    for dd in extracted_dd_statements:
        try:
            if dd in db.new or dd in db.dirty: # Ensure flush before refresh if pending
                db.flush() # Flush to assign IDs if new, or persist changes if dirty
            db.refresh(dd)
            refreshed_dds.append(dd)
        except Exception as e_refresh_dd:
            log.warning(f"JCL_SERVICE: Could not refresh DDStatement {getattr(dd, 'dd_statement_id', 'N/A')} during JCL processing: {e_refresh_dd}. Using potentially uncommitted data for response.")
            refreshed_dds.append(dd) # Add it anyway

    refreshed_m204_files = []
    for mfile in m204_files_from_jcl:
        try:
            if mfile in db.new or mfile in db.dirty:
                db.flush()
            db.refresh(mfile)
            refreshed_m204_files.append(mfile)
        except Exception as e_refresh_mfile:
            log.warning(f"JCL_SERVICE: Could not refresh M204File {getattr(mfile, 'm204_file_id', 'N/A')} (from JCL) during JCL processing: {e_refresh_mfile}. Using potentially uncommitted data for response.")
            refreshed_m204_files.append(mfile)


    dd_responses = [DDStatementResponseSchema.model_validate(dd) for dd in refreshed_dds]
    summary_msg = (f"JCL analysis processed. Found {len(dd_responses)} DD statements. "
                   f"Identified/updated {len(refreshed_m204_files)} M204File entries from this JCL.")
    
    schema_result = GenericAnalysisResultDataSchema(
        dd_statements_found=dd_responses, 
        summary=summary_msg
    )
    
    log.info(f"JCL_SERVICE: Completed JCL analysis for file: {input_source.original_filename}. Summary: {summary_msg}")
    return schema_result, refreshed_m204_files