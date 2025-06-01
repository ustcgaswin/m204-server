import re
from typing import List, Tuple, Optional, Dict, Any

from sqlalchemy.orm import Session
from sqlalchemy.sql import func
from pydantic import BaseModel, Field # Added for LLM structured output

from app.models.input_source_model import InputSource
from app.models.m204_file_model import M204File
from app.models.dd_statement_model import DDStatement

from app.schemas.generic_analysis_schema import (
    DDStatementCreateSchema, DDStatementResponseSchema,
    GenericAnalysisResultDataSchema
)
from app.utils.logger import log
from app.config.llm_config import llm_config # Import LLM configuration
import json

# LlamaIndex text splitter
from llama_index.core.node_parser import SentenceSplitter

# --- Pydantic Model for LLM Structured Output (Iterative JCL Description) ---
class JCLIterativeDescriptionOutput(BaseModel):
    updated_description: str = Field(description="The refined and extended description of the JCL based on the current chunk and previous summary.")
    reasoning_for_update: Optional[str] = Field(description="Brief reasoning for how the current chunk updated the description.", default=None)
    key_elements_in_chunk: List[str] = Field(default_factory=list, description="List of key JCL statements or elements identified in the current chunk (e.g., JOB, EXEC PGM=, DD DSN=).")


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

        if not line_stripped or line_stripped.startswith("*"): 
            if "//*" not in line_stripped: 
                 continue

        if job_match := job_pattern.match(line_stripped):
            current_job_name = job_match.group(1)
            current_step_name = None 
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
            dd_name = dd_name_raw.strip().upper()

            log.debug(f"JCL_SERVICE_PARSE: DD statement found: Name='{dd_name}' (Raw='{dd_name_raw}'), Params='{dd_params_str[:100]}...' at line {line_num} in Job='{current_job_name}', Step='{current_step_name}'")

            dsn_val: Optional[str] = None
            disp_val: Optional[str] = None
            params_dict: Dict[str, Any] = {"raw_parameters_string": dd_params_str}

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
                if re.search(r"\.DBASEF", dsn_val, re.IGNORECASE) or \
                   re.search(r"\.[A-Z0-9#@$-]*204", dsn_val, re.IGNORECASE) or \
                   re.search(r"M204\.DB", dsn_val, re.IGNORECASE): 
                    is_explicit_m204_db_dsn = True
                    log.debug(f"JCL_SERVICE_PARSE: DSN '{dsn_val}' for DD '{dd_name}' identified as explicit M204 DB DSN.")
            
            log.debug(f"JCL_SERVICE_MATCH: Looking up M204File with m204_file_name (DDNAME) '{dd_name}' for project ID {input_source.project_id}")
            existing_m204_file = db.query(M204File).filter(
                M204File.project_id == input_source.project_id,
                func.upper(M204File.m204_file_name) == dd_name 
            ).first()

            if existing_m204_file:
                log.info(f"JCL_SERVICE_MATCH: Found matching M204File for JCL DD '{dd_name}': ID={existing_m204_file.m204_file_id}, Stored m204_file_name='{existing_m204_file.m204_file_name}'")
                update_m204_file = False
                if is_explicit_m204_db_dsn and existing_m204_file.is_db_file is not True:
                    existing_m204_file.is_db_file = True
                    update_m204_file = True
                    log.debug(f"JCL_SERVICE_UPDATE: Marking M204File '{dd_name}' as DB file due to explicit DSN.")
                elif not is_explicit_m204_db_dsn and existing_m204_file.is_db_file is None: 
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
                if existing_dd.raw_statement_text != line_stripped: 
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
                        line_number_end=line_num, 
                        raw_statement_text=line_stripped, 
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

async def _generate_jcl_description_iteratively_with_llm(file_content: str, original_filename: str) -> str:
    """
    Uses an LLM to iteratively generate a detailed description of the JCL file's content.
    Splits the JCL into chunks and processes them sequentially.
    """
    if not llm_config._llm:
        log.warning("JCL_SERVICE_LLM: LLM is not configured. Cannot generate JCL detailed description.")
        return "LLM not configured. JCL description could not be generated."

    try:
        # Using SentenceSplitter; chunk_size is in tokens. JCL lines are short, so 200 tokens might be many lines.
        # For JCL, a line-based splitter might be more intuitive, but adhering to "LlamaIndex splitters".
        # Adjust chunk_size and chunk_overlap as needed.
        # A chunk_size of 200 tokens for JCL might be too small, leading to many calls.
        # Consider a larger chunk_size like 500-1000 for JCL if context window allows.
        # For this example, sticking to the requested "200".
        text_splitter = SentenceSplitter(chunk_size=200, chunk_overlap=20) # 200 tokens, 20 tokens overlap
        jcl_chunks = text_splitter.split_text(file_content)
    except Exception as e_splitter:
        log.error(f"JCL_SERVICE_LLM: Error splitting JCL content for {original_filename}: {e_splitter}", exc_info=True)
        return f"Error splitting JCL content: {str(e_splitter)}"

    if not jcl_chunks:
        log.warning(f"JCL_SERVICE_LLM: No JCL chunks generated for {original_filename}. Content might be too short or empty.")
        return "JCL content was empty or too short to process for description."

    accumulated_description = f"Initial analysis of JCL file: {original_filename}."
    # Assuming JCLIterativeDescriptionOutput is defined in the same file or imported correctly
    llm_structured_caller = llm_config._llm.as_structured_llm(JCLIterativeDescriptionOutput)

    log.info(f"JCL_SERVICE_LLM: Starting iterative description generation for {original_filename} with {len(jcl_chunks)} chunks.")

    for i, chunk in enumerate(jcl_chunks):
        log.info(f"JCL_SERVICE_LLM: Processing chunk {i+1}/{len(jcl_chunks)} for {original_filename}.")
        prompt = f"""
You are an expert JCL analyst. You are tasked with iteratively building a comprehensive description of a JCL file: '{original_filename}'.
You will receive the JCL content in chunks. For each chunk, you will also receive the description accumulated so far from previous chunks.
Your goal is to analyze the current JCL chunk and update/extend the accumulated description to incorporate the new information from this chunk.
Focus on the overall purpose, job definitions (JOB statements), step executions (EXEC statements, PGM=, PROC=), dataset definitions (DD statements, DSN=, DISP=), data flow, utility usage (e.g., IEBGENER, IDCAMS, SORT), and any control logic (e.g., COND=).

Previous Description (summary so far):
--- PREVIOUS DESCRIPTION START ---
{accumulated_description}
--- PREVIOUS DESCRIPTION END ---

Current JCL Chunk to Analyze (Chunk {i+1} of {len(jcl_chunks)}):
--- CURRENT JCL CHUNK START ---
{chunk}
--- CURRENT JCL CHUNK END ---

Based on the 'Previous Description' and the 'Current JCL Chunk', provide an 'updated_description'.
The 'updated_description' should be a complete, coherent narrative that integrates the new findings from the current chunk with the previous summary.
Do not just append; intelligently merge, refine, and expand the description. If the current chunk introduces a new JOB or a distinct phase, structure the description accordingly.
Also, provide a brief 'reasoning_for_update' explaining what new insights or details the current chunk added or how it modified the understanding.
Finally, list any 'key_elements_in_chunk' such as specific JOB, EXEC, or DD statements that were significant in this chunk.

Respond with a JSON object structured according to the JCLIterativeDescriptionOutput model.
The JSON output must conform to this Pydantic model:
class JCLIterativeDescriptionOutput(BaseModel):
    updated_description: str
    reasoning_for_update: Optional[str]
    key_elements_in_chunk: List[str]
"""
        completion_response = None # Initialize for broader scope in except block
        try:
            # Corrected handling of LLM structured output
            completion_response = await llm_structured_caller.acomplete(prompt)
            if completion_response and completion_response.text:
                loaded_data = json.loads(completion_response.text)
                response_model = JCLIterativeDescriptionOutput(**loaded_data)
                accumulated_description = response_model.updated_description
                log.debug(f"JCL_SERVICE_LLM: Chunk {i+1} processed. Reasoning: {response_model.reasoning_for_update}. Keys: {response_model.key_elements_in_chunk}")
            else:
                log.error(f"JCL_SERVICE_LLM: LLM returned empty or no text for chunk {i+1} of {original_filename}.")
                accumulated_description += f"\n\n[LLM Error processing chunk {i+1}: LLM returned no content for this section.]"

        except json.JSONDecodeError as e_json_iter:
            raw_output_text = completion_response.text if completion_response and hasattr(completion_response, 'text') else "Raw output not available or completion_response is None"
            log.error(f"JCL_SERVICE_LLM: JSON parsing error for chunk {i+1} of {original_filename}: {e_json_iter}. Raw output: '{raw_output_text}'", exc_info=True)
            accumulated_description += f"\n\n[LLM Error processing chunk {i+1}: Could not parse LLM output. Error: {str(e_json_iter)}]"
        except Exception as e_llm_iter:
            # This will catch other errors from the LLM call or Pydantic model validation if json.loads was successful
            log.error(f"JCL_SERVICE_LLM: Error during LLM call or processing for chunk {i+1} of {original_filename}: {e_llm_iter}", exc_info=True)
            accumulated_description += f"\n\n[LLM Error processing chunk {i+1}: Could not fully integrate this section. Error: {str(e_llm_iter)}]"
            # Optionally, break or decide to continue with potentially degraded quality
            # For now, we continue and append an error note.

    log.info(f"JCL_SERVICE_LLM: Finished iterative description generation for {original_filename}.")
    return accumulated_description

async def process_jcl_analysis(
    db: Session, input_source: InputSource, file_content: str
) -> Tuple[GenericAnalysisResultDataSchema, List[M204File]]:
    """
    Main function to process JCL file content.
    This function will be called by the main analysis_service orchestrator.
    """
    log.info(f"JCL_SERVICE: Starting JCL analysis for file: {input_source.original_filename} (ID: {input_source.input_source_id})")
    
    extracted_dd_statements, m204_files_from_jcl = await _extract_and_store_dd_statements_from_jcl(db, input_source, file_content)
    
    # Generate and store detailed JCL description using iterative LLM process
    jcl_description_final = "Description generation not attempted or file not identified as JCL."
    if input_source.source_type and "jcl" in input_source.source_type.lower():
        log.info(f"JCL_SERVICE: Initiating detailed JCL description generation for {input_source.original_filename}")
        jcl_description_final = await _generate_jcl_description_iteratively_with_llm(
            file_content, 
            input_source.original_filename or f"InputSourceID_{input_source.input_source_id}"
        )
        try:
            input_source.jcl_detailed_description = jcl_description_final
            db.add(input_source) 
            log.info(f"JCL_SERVICE: Updated InputSource ID {input_source.input_source_id} with detailed JCL description (length: {len(jcl_description_final)}).")
        except Exception as e_save_desc:
            log.error(f"JCL_SERVICE: Failed to assign JCL description to InputSource ID {input_source.input_source_id} in session: {e_save_desc}", exc_info=True)
            # If saving fails, the generated description is lost for this run unless logged/handled otherwise.
            # The input_source object in the session might still hold it if the error is during flush/commit by caller.

    # The main orchestrator (analysis_service.perform_source_file_analysis) will handle the commit.
    
    refreshed_dds = []
    for dd in extracted_dd_statements:
        try:
            if dd in db.new or dd in db.dirty: 
                db.flush() 
            db.refresh(dd)
            refreshed_dds.append(dd)
        except Exception as e_refresh_dd:
            log.warning(f"JCL_SERVICE: Could not refresh DDStatement {getattr(dd, 'dd_statement_id', 'N/A')} during JCL processing: {e_refresh_dd}. Using potentially uncommitted data for response.")
            refreshed_dds.append(dd) 

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
    
    description_status_msg = "Not applicable for this file type."
    if input_source.source_type and "jcl" in input_source.source_type.lower():
        if "LLM not configured" in jcl_description_final:
            description_status_msg = "LLM not configured."
        elif "Error generating JCL description" in jcl_description_final or "Error splitting JCL content" in jcl_description_final:
            description_status_msg = "Failed (LLM/Splitter Error)."
        elif "empty or too short to process" in jcl_description_final:
            description_status_msg = "Content too short."
        elif jcl_description_final and len(jcl_description_final) > 50: # Arbitrary length to indicate some success
            description_status_msg = "Generated."
        else:
            description_status_msg = "Attempted, but result seems minimal."


    summary_msg = (f"JCL analysis processed. Found {len(dd_responses)} DD statements. "
                   f"Identified/updated {len(refreshed_m204_files)} M204File entries from this JCL. "
                   f"Detailed JCL description status: {description_status_msg}")
    
    schema_result = GenericAnalysisResultDataSchema(
        dd_statements_found=dd_responses, 
        summary=summary_msg
    )
    
    log.info(f"JCL_SERVICE: Completed JCL analysis for file: {input_source.original_filename}. Summary: {summary_msg}")
    return schema_result, refreshed_m204_files
