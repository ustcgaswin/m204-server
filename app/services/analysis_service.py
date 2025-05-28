import os
import asyncio
from sqlalchemy.orm import Session
from typing import List, Optional, Union

from app.models.input_source_model import InputSource
from app.models.m204_file_model import M204File
from sqlalchemy.sql import func
from fastapi import HTTPException, status

from app.schemas.m204_analysis_schema import M204AnalysisResultDataSchema, M204FileResponseSchema
from app.schemas.generic_analysis_schema import GenericAnalysisResultDataSchema
from app.schemas.analysis_schema import UnifiedAnalysisReportSchema

from app.utils.logger import log
from app.config.llm_config import llm_config # For checking if LLM is configured
from app.services.rag_service import RagService

# Import new service modules
from app.services import parmlib_analysis_service
from app.services import jcl_analysis_service
from app.services import m204_analysis_service


_shared_rag_service: Optional[RagService] = None

def set_global_rag_service(instance: RagService):
    """Sets the global RAG service instance for this module and passes it to sub-services if needed."""
    global _shared_rag_service
    _shared_rag_service = instance
    log.info(f"ORCHESTRATOR: Global RAG service instance has been set in analysis_service: {type(instance)}")
    # If m204_analysis_service also needs a global RAG instance, you could set it here too,
    # but it's cleaner to pass it as an argument to its processing functions.

async def _get_input_source_for_analysis(db: Session, input_source_id: int) -> InputSource:
    input_source = db.query(InputSource).filter(InputSource.input_source_id == input_source_id).first()
    if not input_source:
        log.warning(f"ORCHESTRATOR: Input source file with ID {input_source_id} not found for analysis.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Input source file not found.")
    
    if not input_source.file_path_or_identifier or not os.path.exists(input_source.file_path_or_identifier):
        log.error(f"ORCHESTRATOR: File path missing or file does not exist for InputSource ID {input_source_id} at '{input_source.file_path_or_identifier}'.")
        raise HTTPException(status_code=status.HTTP_409_CONFLICT, detail="File path missing or file does not exist on server.")
    return input_source

async def _read_file_content(file_path: str) -> str:
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
        return content
    except Exception as e:
        log.error(f"ORCHESTRATOR: Error reading file at path {file_path}: {e}")
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Could not read file content: {e}")


async def perform_source_file_analysis(db: Session, input_source_id: int) -> UnifiedAnalysisReportSchema:
    """
    Orchestrates the analysis of a source file by dispatching to specific service modules
    based on file type, and then performs common post-processing like VSAM enhancement.
    """
    input_source = await _get_input_source_for_analysis(db, input_source_id)
    file_content = await _read_file_content(input_source.file_path_or_identifier)
    
    file_type = input_source.source_type.lower() if input_source.source_type else "unknown"
    analysis_details: Optional[Union[M204AnalysisResultDataSchema, GenericAnalysisResultDataSchema]] = None
    analysis_status_val = "failed" 
    message = f"Analysis initiated for {input_source.original_filename} (Type: {file_type})."
    errors: List[str] = []
    
    # List to collect M204File objects that are identified as DB files and need VSAM enhancement
    m204_db_files_for_vsam_enhancement: List[M204File] = []

    input_source.analysis_status = "analysis_in_progress"
    input_source.error_message = None
    input_source.last_analyzed_timestamp = func.now()
    try:
        db.add(input_source)
        db.commit() # Commit "in_progress" status
        db.refresh(input_source)
    except Exception as e_commit_progress:
        db.rollback()
        log.error(f"ORCHESTRATOR: Failed to update InputSource status to 'analysis_in_progress' for ID {input_source_id}: {e_commit_progress}", exc_info=True)
        # Allow to proceed with analysis if this fails, but log it.

    try:
        log.info(f"ORCHESTRATOR: Starting analysis for InputSource ID: {input_source_id}, File: {input_source.original_filename}, Type: {file_type}")

        if file_type == "parmlib":
            analysis_details = await parmlib_analysis_service.process_parmlib_analysis(db, input_source, file_content)
            message = f"PARMLIB analysis completed for {input_source.original_filename}."
            # Collect M204Files that were marked as DB files (is_db_file=True)
            # These are present in analysis_details.defined_files_found
            if analysis_details and isinstance(analysis_details, M204AnalysisResultDataSchema):
                for m204_file_resp_schema in analysis_details.defined_files_found:
                    # We need the ORM object, not just the schema, for VSAM enhancement
                    m204_file_obj = db.query(M204File).filter(M204File.m204_file_id == m204_file_resp_schema.m204_file_id).first()
                    if m204_file_obj and m204_file_obj.is_db_file:
                        if m204_file_obj not in m204_db_files_for_vsam_enhancement:
                             m204_db_files_for_vsam_enhancement.append(m204_file_obj)
            log.info(f"ORCHESTRATOR: PARMLIB analysis for {input_source.original_filename} found {len(analysis_details.defined_fields_found if analysis_details else [])} fields. {len(m204_db_files_for_vsam_enhancement)} DB files identified for VSAM enhancement.")

        elif file_type == "jcl":
            jcl_analysis_result, m204_files_from_jcl = await jcl_analysis_service.process_jcl_analysis(db, input_source, file_content)
            analysis_details = jcl_analysis_result
            message = f"JCL analysis completed for {input_source.original_filename}."
            # m204_files_from_jcl contains M204File ORM objects that might have been updated (e.g., is_db_file, target_vsam_dataset_name)
            for m204_file_obj in m204_files_from_jcl:
                if m204_file_obj.is_db_file: # Check if it was marked as a DB file
                    if m204_file_obj not in m204_db_files_for_vsam_enhancement:
                        m204_db_files_for_vsam_enhancement.append(m204_file_obj)
            log.info(f"ORCHESTRATOR: JCL analysis for {input_source.original_filename} found {len(analysis_details.dd_statements_found if analysis_details else [])} DD statements. {len(m204_db_files_for_vsam_enhancement)} DB files identified/updated for VSAM enhancement.")

        elif file_type in ["m204", "online", "batch", "include", "screen", "map", "subroutine", "userlanguage"]: # M204 source code
            m204_analysis_result, m204_db_files_from_m204_service = await m204_analysis_service.process_m204_analysis(db, input_source, file_content, _shared_rag_service)
            analysis_details = m204_analysis_result
            message = f"M204 source code analysis completed for {input_source.original_filename}."
            # m204_db_files_from_m204_service contains M204File ORM objects identified as DB files (e.g., from DEFINE DATASET)
            for m204_file_obj in m204_db_files_from_m204_service:
                if m204_file_obj.is_db_file: # Should already be true if returned by this service
                     if m204_file_obj not in m204_db_files_for_vsam_enhancement:
                        m204_db_files_for_vsam_enhancement.append(m204_file_obj)
            log.info(f"ORCHESTRATOR: M204 source analysis for {input_source.original_filename} completed. {len(m204_db_files_for_vsam_enhancement)} DB files identified for VSAM enhancement.")
        
        else:
            message = f"File type '{file_type}' for {input_source.original_filename} is not specifically handled by detailed analysis. Basic processing completed."
            log.warning(f"ORCHESTRATOR: Unhandled file type '{file_type}' for InputSource ID: {input_source_id}, File: {input_source.original_filename}")
            analysis_status_val = "analysis_completed_with_warnings" # Or "completed_no_details"
            errors.append(f"File type '{file_type}' not supported for detailed analysis.")

        if not errors and analysis_details is not None: # If primary analysis was successful
            analysis_status_val = "analysis_completed"
        elif not errors and analysis_details is None and file_type not in ["parmlib", "jcl", "m204", "online", "batch", "include", "screen", "map", "subroutine", "userlanguage"]:
            analysis_status_val = "analysis_completed_no_details" # For unhandled types that don't error
            message = f"Analysis for {input_source.original_filename} (Type: {file_type}) completed. No specific details extracted for this type."


        # Perform VSAM enhancement for all identified DB files
        if m204_db_files_for_vsam_enhancement and llm_config._llm:
            log.info(f"ORCHESTRATOR: Starting VSAM enhancement for {len(m204_db_files_for_vsam_enhancement)} M204 DB files related to {input_source.original_filename}.")
            vsam_enhancement_tasks = []
            for m204_db_file in m204_db_files_for_vsam_enhancement:
                # Ensure the file object is fresh from the session before passing
                db.refresh(m204_db_file)
                if m204_db_file.fields: # Pre-load/refresh fields if the relationship is lazy
                    for f in m204_db_file.fields:
                        db.refresh(f)
                vsam_enhancement_tasks.append(
                    m204_analysis_service.enhance_m204_db_file_with_vsam_suggestions(db, m204_db_file)
                )
            
            await asyncio.gather(*vsam_enhancement_tasks, return_exceptions=True) # Exceptions are logged within the tasks
            log.info(f"ORCHESTRATOR: Completed VSAM enhancement tasks for M204 DB files related to {input_source.original_filename}.")
            # The analysis_details might now be stale if M204Files or M204Fields were updated.
            # We will re-fetch/re-validate for the response if necessary.

        elif m204_db_files_for_vsam_enhancement and not llm_config._llm:
            log.warning(f"ORCHESTRATOR: LLM not configured. Skipping VSAM enhancement for {len(m204_db_files_for_vsam_enhancement)} M204 DB files.")


        # Commit all changes made by the specific analysis service and VSAM enhancement
        db.commit()
        log.info(f"ORCHESTRATOR: Successfully committed all analysis changes for InputSource ID: {input_source_id}, File: {input_source.original_filename}")
        analysis_status_val = "analysis_completed" if not errors else "analysis_completed_with_warnings"


    except HTTPException as he:
        db.rollback()
        log.error(f"ORCHESTRATOR: HTTPException during analysis of {input_source.original_filename} (ID: {input_source_id}): {he.detail}", exc_info=True)
        errors.append(f"Analysis error: {he.detail}")
        message = f"Analysis failed for {input_source.original_filename}: {he.detail}"
        analysis_status_val = "analysis_failed"
    except Exception as e:
        db.rollback()
        log.error(f"ORCHESTRATOR: Unexpected error during analysis of {input_source.original_filename} (ID: {input_source_id}): {e}", exc_info=True)
        errors.append(f"Unexpected analysis error: {str(e)}")
        message = f"Unexpected error during analysis of {input_source.original_filename}."
        analysis_status_val = "analysis_failed"
    
    # Final update to InputSource status
    input_source.analysis_status = analysis_status_val
    if errors:
        input_source.error_message = "; ".join(list(set(errors))) # Store unique errors
    else:
        input_source.error_message = None # Clear previous errors if successful
    
    try:
        db.add(input_source)
        db.commit()
        db.refresh(input_source)
    except Exception as e_commit_final:
        db.rollback() # Rollback status update if it fails
        log.error(f"ORCHESTRATOR: Failed to commit final InputSource status for ID {input_source_id}: {e_commit_final}", exc_info=True)
        # The analysis itself might have been committed earlier, but status update failed.

    # Refresh analysis_details if M204Files were updated by VSAM step, to ensure response has latest data.
    # This involves re-validating the schema objects from potentially updated ORM objects.
    if analysis_status_val.startswith("analysis_completed") and analysis_details:
        if isinstance(analysis_details, M204AnalysisResultDataSchema):
            # Re-fetch and re-validate M204FileResponseSchema for defined_files_found
            refreshed_defined_files = []
            for m204_file_resp in analysis_details.defined_files_found:
                m204_file_orm = db.query(M204File).get(m204_file_resp.m204_file_id)
                if m204_file_orm:
                    db.refresh(m204_file_orm) # Refresh the ORM object
                    if m204_file_orm.fields: # Refresh fields too
                        for f_orm in m204_file_orm.fields:
                            db.refresh(f_orm)
                    refreshed_defined_files.append(M204FileResponseSchema.model_validate(m204_file_orm))
            analysis_details.defined_files_found = refreshed_defined_files
            # Similar refresh logic could be applied to defined_fields_found if they are directly modified by VSAM step
            # and part of M204AnalysisResultDataSchema from PARMLIB.
            # For now, parmlib_analysis_service's _enhance_single_m204_field_with_vsam_suggestions updates fields directly.
            # The file-level VSAM enhancement in m204_analysis_service also updates fields.
            # The refresh within those services before returning should be sufficient, but an orchestrator-level refresh ensures consistency.

        # No specific M204File list in GenericAnalysisResultDataSchema to refresh directly here for JCL.
        # JCL service returns affected M204Files separately, which are used for VSAM enhancement.
        # The DDStatementResponseSchema in its details should be fine.
    
    return UnifiedAnalysisReportSchema(
        input_source_id=input_source.input_source_id,
        original_filename=input_source.original_filename,
        file_type_processed=file_type,
        analysis_status=input_source.analysis_status,
        message=message,
        details=analysis_details,
        errors=list(set(errors)) 
    )
