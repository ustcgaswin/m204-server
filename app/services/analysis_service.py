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
from app.services.rag_service import  get_global_rag_service # MODIFIED: Import get_global_rag_service

# Import new service modules
from app.services import parmlib_analysis_service
from app.services import jcl_analysis_service
from app.services import m204_analysis_service

# REMOVED/COMMENTED: _shared_rag_service and local set_global_rag_service
# _shared_rag_service: Optional[RagService] = None

# def set_global_rag_service(instance: RagService):
#     """Sets the global RAG service instance for this module and passes it to sub-services if needed."""
#     global _shared_rag_service
#     _shared_rag_service = instance
#     log.info(f"ORCHESTRATOR: Global RAG service instance has been set in analysis_service: {type(instance)}")

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

    # Get the RAG service instance from the rag_service module
    current_rag_service_instance = get_global_rag_service()
    if current_rag_service_instance:
        log.info(f"ORCHESTRATOR: Retrieved RAG service instance: {type(current_rag_service_instance)}")
    else:
        log.warning("ORCHESTRATOR: RAG service instance is not available globally.")


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
            
            if analysis_details and isinstance(analysis_details, M204AnalysisResultDataSchema):
                for m204_file_resp_schema in analysis_details.defined_files_found:
                    m204_file_obj = db.query(M204File).filter(M204File.m204_file_id == m204_file_resp_schema.m204_file_id).first()
                    if m204_file_obj and m204_file_obj.is_db_file:
                        if m204_file_obj not in m204_db_files_for_vsam_enhancement:
                             m204_db_files_for_vsam_enhancement.append(m204_file_obj)
            
            total_parmlib_fields_found = 0
            num_parmlib_files_processed = 0
            if analysis_details and analysis_details.defined_files_found:
                num_parmlib_files_processed = len(analysis_details.defined_files_found)
                for file_schema in analysis_details.defined_files_found:
                    if file_schema.file_definition_json:
                        json_data = file_schema.file_definition_json
                        if isinstance(json_data, dict):
                            fields_to_count = None
                            if json_data.get("file_type") == "db_file":
                                fields_to_count = json_data.get("fields")
                            elif json_data.get("file_type") == "mixed":
                                db_def_part = json_data.get("db_file_definition")
                                if isinstance(db_def_part, dict):
                                    fields_to_count = db_def_part.get("fields")
                            
                            if isinstance(fields_to_count, dict):
                                total_parmlib_fields_found += len(fields_to_count)
            
            log.info(f"ORCHESTRATOR: PARMLIB analysis for {input_source.original_filename} processed {num_parmlib_files_processed} file definitions, finding {total_parmlib_fields_found} fields. {len(m204_db_files_for_vsam_enhancement)} DB files identified for VSAM enhancement.")

        elif file_type == "jcl":
            jcl_analysis_result, m204_files_from_jcl = await jcl_analysis_service.process_jcl_analysis(db, input_source, file_content)
            analysis_details = jcl_analysis_result
            message = f"JCL analysis completed for {input_source.original_filename}."
            for m204_file_obj in m204_files_from_jcl:
                if m204_file_obj.is_db_file: 
                    if m204_file_obj not in m204_db_files_for_vsam_enhancement:
                        m204_db_files_for_vsam_enhancement.append(m204_file_obj)
            log.info(f"ORCHESTRATOR: JCL analysis for {input_source.original_filename} found {len(analysis_details.dd_statements_found if analysis_details and hasattr(analysis_details, 'dd_statements_found') else [])} DD statements. {len(m204_db_files_for_vsam_enhancement)} DB files identified/updated for VSAM enhancement.")

        elif file_type in ["m204", "online", "batch", "include", "screen", "map", "subroutine", "userlanguage"]: 
            m204_analysis_result, m204_db_files_from_m204_service = await m204_analysis_service.process_m204_analysis(db, input_source, file_content, current_rag_service_instance) # MODIFIED: Pass the fetched RAG instance
            analysis_details = m204_analysis_result
            message = f"M204 source code analysis completed for {input_source.original_filename}."
            for m204_file_obj in m204_db_files_from_m204_service:
                if m204_file_obj.is_db_file: 
                     if m204_file_obj not in m204_db_files_for_vsam_enhancement:
                        m204_db_files_for_vsam_enhancement.append(m204_file_obj)
            log.info(f"ORCHESTRATOR: M204 source analysis for {input_source.original_filename} completed. {len(m204_db_files_for_vsam_enhancement)} DB files identified for VSAM enhancement.")
        
        else:
            message = f"File type '{file_type}' for {input_source.original_filename} is not specifically handled by detailed analysis. Basic processing completed."
            log.warning(f"ORCHESTRATOR: Unhandled file type '{file_type}' for InputSource ID: {input_source_id}, File: {input_source.original_filename}")
            analysis_status_val = "analysis_completed_with_warnings" 
            errors.append(f"File type '{file_type}' not supported for detailed analysis.")

        if not errors and analysis_details is not None: 
            analysis_status_val = "analysis_completed"
        elif not errors and analysis_details is None and file_type not in ["parmlib", "jcl", "m204", "online", "batch", "include", "screen", "map", "subroutine", "userlanguage"]:
            analysis_status_val = "analysis_completed_no_details" 
            message = f"Analysis for {input_source.original_filename} (Type: {file_type}) completed. No specific details extracted for this type."


        if m204_db_files_for_vsam_enhancement and llm_config._llm:
            log.info(f"ORCHESTRATOR: Starting VSAM enhancement for {len(m204_db_files_for_vsam_enhancement)} M204 DB files related to {input_source.original_filename}.")
            vsam_enhancement_tasks = []
            for m204_db_file in m204_db_files_for_vsam_enhancement:
                db.refresh(m204_db_file) 
                vsam_enhancement_tasks.append(
                    m204_analysis_service.enhance_m204_db_file_with_vsam_suggestions(db, m204_db_file)
                )
            
            await asyncio.gather(*vsam_enhancement_tasks, return_exceptions=True) 
            log.info(f"ORCHESTRATOR: Completed VSAM enhancement tasks for M204 DB files related to {input_source.original_filename}.")

        elif m204_db_files_for_vsam_enhancement and not llm_config._llm:
            log.warning(f"ORCHESTRATOR: LLM not configured. Skipping VSAM enhancement for {len(m204_db_files_for_vsam_enhancement)} M204 DB files.")


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
    
    input_source.analysis_status = analysis_status_val
    if errors:
        input_source.error_message = "; ".join(list(set(errors))) 
    else:
        input_source.error_message = None 
    
    try:
        db.add(input_source)
        db.commit()
        db.refresh(input_source)
    except Exception as e_commit_final:
        db.rollback() 
        log.error(f"ORCHESTRATOR: Failed to commit final InputSource status for ID {input_source_id}: {e_commit_final}", exc_info=True)

    if analysis_status_val.startswith("analysis_completed") and analysis_details:
        if isinstance(analysis_details, M204AnalysisResultDataSchema):
            refreshed_defined_files = []
            for m204_file_resp in analysis_details.defined_files_found:
                m204_file_orm = db.query(M204File).get(m204_file_resp.m204_file_id)
                if m204_file_orm:
                    db.refresh(m204_file_orm) 
                    refreshed_defined_files.append(M204FileResponseSchema.model_validate(m204_file_orm))
            analysis_details.defined_files_found = refreshed_defined_files
    
    return UnifiedAnalysisReportSchema(
        input_source_id=input_source.input_source_id,
        original_filename=input_source.original_filename,
        file_type_processed=file_type,
        analysis_status=input_source.analysis_status,
        message=message,
        details=analysis_details,
        errors=list(set(errors)) 
    )
