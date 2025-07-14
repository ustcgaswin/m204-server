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
from app.config.llm_config import llm_config 
from app.services.rag_service import  get_global_rag_service

# Import new service modules
from app.services import parmlib_analysis_service
from app.services import jcl_analysis_service
from app.services import m204_analysis_service

from app.config.db_config import SessionLocal

def get_new_db():
    return SessionLocal()



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
    
    m204_db_file_ids_for_vsam_enhancement: List[int] = []

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
        db.commit()
        db.refresh(input_source)
    except Exception as e_commit_progress:
        db.rollback()
        log.error(f"ORCHESTRATOR: Failed to update InputSource status to 'analysis_in_progress' for ID {input_source_id}: {e_commit_progress}", exc_info=True)

    try:
        log.info(f"ORCHESTRATOR: Starting analysis for InputSource ID: {input_source_id}, File: {input_source.original_filename}, Type: {file_type}")

        if file_type == "parmlib":
            # PARMLIB analysis is simpler and doesn't use concurrent sub-tasks, so passing the object is safe.
            analysis_details, m204_file_ids_from_parmlib = await parmlib_analysis_service.process_parmlib_analysis(db, input_source, file_content)
            message = f"PARMLIB analysis completed for {input_source.original_filename}."
            
            for m204_file_id in m204_file_ids_from_parmlib:
                if m204_file_id not in m204_db_file_ids_for_vsam_enhancement:
                     m204_db_file_ids_for_vsam_enhancement.append(m204_file_id)
            
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
            
            log.info(f"ORCHESTRATOR: PARMLIB analysis for {input_source.original_filename} processed {num_parmlib_files_processed} file definitions, finding {total_parmlib_fields_found} fields. {len(m204_db_file_ids_for_vsam_enhancement)} DB files identified for VSAM enhancement.")

        elif file_type == "jcl":
            db_analysis = get_new_db()
            db_desc = get_new_db() if llm_config._llm else None

            task_defs = []

            log.info(f"ORCHESTRATOR: Scheduling JCL analysis for {input_source.original_filename}.")
            task_defs.append((
                "analysis",
                db_analysis,
                jcl_analysis_service.process_jcl_analysis(
                    db_analysis,
                    input_source.input_source_id,
                    file_content,
                    current_rag_service_instance
                )
            ))

            if llm_config._llm:
                log.info(f"ORCHESTRATOR: Scheduling JCL description generation for {input_source.original_filename}.")
                task_defs.append((
                    "description",
                    db_desc,
                    jcl_analysis_service.generate_and_store_jcl_description(
                        db_desc,
                        input_source.input_source_id,
                        file_content
                    )
                ))
            else:
                log.warning(f"ORCHESTRATOR: LLM not configured; skipping JCL description for {input_source.original_filename}.")

            tasks = [asyncio.create_task(coro) for (_, _, coro) in task_defs]
            results = await asyncio.gather(*tasks, return_exceptions=True)

            m204_file_ids_from_jcl = []
            for (name, session, _), result in zip(task_defs, results):
                try:
                    if isinstance(result, Exception):
                        log.error(
                            f"ORCHESTRATOR: JCL {name} task failed for "
                            f"{input_source.original_filename}: {result}",
                            exc_info=True
                        )
                        errors.append(f"Failed JCL {name}")
                        session.rollback()
                    else:
                        log.info(
                            f"ORCHESTRATOR: JCL {name} task succeeded for "
                            f"{input_source.original_filename}."
                        )
                        session.commit()
                        if name == "analysis":
                            analysis_details, m204_file_ids_from_jcl = result
                finally:
                    if session:
                        session.close()

            message = f"JCL analysis completed for {input_source.original_filename}."
            for m204_file_id in m204_file_ids_from_jcl:
                if m204_file_id not in m204_db_file_ids_for_vsam_enhancement:
                    m204_db_file_ids_for_vsam_enhancement.append(m204_file_id)
            log.info(f"ORCHESTRATOR: JCL analysis for {input_source.original_filename} found {len(analysis_details.dd_statements_found if analysis_details and hasattr(analysis_details, 'dd_statements_found') else [])} DD statements. {len(m204_db_file_ids_for_vsam_enhancement)} DB files identified/updated for VSAM enhancement.")

        elif file_type in ["m204", "online", "batch", "include", "screen", "map", "subroutine", "userlanguage", "src", "s", "user", "proc"]:
            db_analysis = get_new_db()
            db_desc     = get_new_db() if llm_config._llm else None
            db_loop     = get_new_db()

            task_defs = []

            log.info(f"ORCHESTRATOR: Scheduling structural analysis for {input_source.original_filename}.")
            task_defs.append((
                "analysis",
                db_analysis,
                m204_analysis_service.process_m204_analysis(
                    db_analysis,
                    input_source.input_source_id,
                    file_content,
                    current_rag_service_instance
                )
            ))

            if llm_config._llm:
                log.info(f"ORCHESTRATOR: Scheduling detailed description for {input_source.original_filename}.")
                task_defs.append((
                    "description",
                    db_desc,
                    m204_analysis_service._task_generate_and_store_m204_description(
                        db=db_desc,
                        input_source_id=input_source.input_source_id,
                        file_content=file_content,
                        original_filename=input_source.original_filename
                    )
                ))
            else:
                log.warning(f"ORCHESTRATOR: LLM not configured; skipping description for {input_source.original_filename}.")

            log.info(f"ORCHESTRATOR: Scheduling main-loop extraction for {input_source.original_filename}.")
            task_defs.append((
                "main_loop",
                db_loop,
                m204_analysis_service.extract_and_store_main_loop(
                    db=db_loop,
                    input_source_id=input_source.input_source_id,
                    file_content=file_content
                )
            ))

            tasks = [asyncio.create_task(coro) for (_, _, coro) in task_defs]
            results = await asyncio.gather(*tasks, return_exceptions=True)

            m204_file_ids_from_m204_service = []
            for (name, session, _), result in zip(task_defs, results):
                try:
                    if isinstance(result, Exception):
                        log.error(
                            f"ORCHESTRATOR: {name} task failed for "
                            f"{input_source.original_filename}: {result}",
                            exc_info=True
                        )
                        errors.append(f"Failed M204 {name}")
                        session.rollback()
                    else:
                        log.info(
                            f"ORCHESTRATOR: {name} task succeeded for "
                            f"{input_source.original_filename}."
                        )
                        session.commit()
                        if name == "analysis":
                            analysis_details, m204_file_ids_from_m204_service = result
                finally:
                    if session:
                        session.close()

            message = (
                f"M204 source code analysis completed for "
                f"{input_source.original_filename}."
            )
            for m204_file_id in m204_file_ids_from_m204_service:
                if m204_file_id not in m204_db_file_ids_for_vsam_enhancement:
                    m204_db_file_ids_for_vsam_enhancement.append(m204_file_id)

            log.info(
                f"ORCHESTRATOR: M204 source analysis for "
                f"{input_source.original_filename} completed; "
                f"{len(m204_db_file_ids_for_vsam_enhancement)} DB files identified for VSAM enhancement."
            )

        else:
            message = f"File type '{file_type}' for {input_source.original_filename} is not specifically handled by detailed analysis. Basic processing completed."
            log.warning(f"ORCHESTRATOR: Unhandled file type '{file_type}' for InputSource ID: {input_source_id}, File: {input_source.original_filename}")
            analysis_details = GenericAnalysisResultDataSchema(summary=message)
            errors.append(f"File type '{file_type}' not supported for detailed analysis.")

        if not errors and analysis_details is not None: 
            analysis_status_val = "completed"
        elif errors:
            analysis_status_val = "completed_with_warnings"
        else:
            analysis_status_val = "completed"
            message = f"Analysis for {input_source.original_filename} (Type: {file_type}) completed. No specific details extracted for this type."

        if m204_db_file_ids_for_vsam_enhancement and llm_config._llm:
            log.info(f"ORCHESTRATOR: Starting VSAM enhancement for {len(m204_db_file_ids_for_vsam_enhancement)} M204 DB files related to {input_source.original_filename}.")
            
            async def enhance_and_commit(m204_file_id: int):
                db_vsam = get_new_db()
                try:
                    m204_db_file = db_vsam.query(M204File).get(m204_file_id)
                    if m204_db_file and m204_db_file.is_db_file:
                        await m204_analysis_service.enhance_m204_db_file_with_vsam_suggestions(db_vsam, m204_db_file)
                        db_vsam.commit()
                        log.info(f"ORCHESTRATOR_VSAM: Successfully enhanced and committed file ID {m204_file_id}.")
                    else:
                        log.warning(f"ORCHESTRATOR_VSAM: Could not find M204File with ID {m204_file_id} for enhancement or it is not a DB file.")
                except Exception as e_vsam:
                    log.error(f"ORCHESTRATOR_VSAM: VSAM enhancement task failed for file ID {m204_file_id}: {e_vsam}", exc_info=True)
                    errors.append(f"Failed VSAM enhancement for file ID {m204_file_id}")
                    db_vsam.rollback()
                finally:
                    db_vsam.close()

            vsam_enhancement_tasks = [enhance_and_commit(file_id) for file_id in m204_db_file_ids_for_vsam_enhancement]
            await asyncio.gather(*vsam_enhancement_tasks)
            log.info(f"ORCHESTRATOR: Completed VSAM enhancement tasks for M204 DB files related to {input_source.original_filename}.")

        elif m204_db_file_ids_for_vsam_enhancement and not llm_config._llm:
            log.warning(f"ORCHESTRATOR: LLM not configured. Skipping VSAM enhancement for {len(m204_db_file_ids_for_vsam_enhancement)} M204 DB files.")

        # This commit is for the main session, which might not have changes if all work was done in sub-tasks.
        # It's generally safe but might be redundant. The final status commit is more critical.
        db.commit()
        log.info(f"ORCHESTRATOR: Main session commit after analysis tasks for InputSource ID: {input_source_id}")
        analysis_status_val = "completed" if not errors else "completed_with_warnings"

    except HTTPException as he:
        db.rollback()
        log.error(f"ORCHESTRATOR: HTTPException during analysis of {input_source.original_filename} (ID: {input_source_id}): {he.detail}", exc_info=True)
        errors.append(f"Analysis error: {he.detail}")
        message = f"Analysis failed for {input_source.original_filename}: {he.detail}"
        analysis_status_val = "failed"
    except Exception as e:
        db.rollback()
        log.error(f"ORCHESTRATOR: Unexpected error during analysis of {input_source.original_filename} (ID: {input_source_id}): {e}", exc_info=True)
        errors.append(f"Unexpected analysis error: {str(e)}")
        message = f"Unexpected error during analysis of {input_source.original_filename}."
        analysis_status_val = "failed"
    
    # Final status update in the main session
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

    # Refresh M204File objects in the main session to get the latest data after VSAM enhancement
    if analysis_status_val.startswith("completed") and analysis_details:
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
        file_type_processed=input_source.source_type or "unknown",
        analysis_status=input_source.analysis_status,
        message=message,
        errors=list(set(errors)),
        details=analysis_details
    )