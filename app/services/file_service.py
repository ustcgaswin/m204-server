from sqlalchemy.orm import Session
from fastapi import UploadFile, HTTPException, status
from typing import List, Optional 

from app.models.input_source_model import InputSource
from app.schemas.input_source_schema import InputSourceResponseSchema
from app.utils import file_utils # Ensure this points to the more complete file_utils
from app.utils.file_utils import FileSaveError, FileRemoveError, FileOperationError
from app.utils.logger import log
from app.services import project_service
import os 

async def save_uploaded_file_and_create_db_entry(
    db: Session,
    project_id: int,
    file: UploadFile,
    user_defined_path_segment_for_file: str # Changed parameter name for clarity
) -> InputSource:
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.error(f"Project with ID {project_id} not found when attempting to save file '{file.filename}'.")
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Project with ID {project_id} not found. Cannot upload file."
        )
    project_name_for_folder = db_project.project_name

    actual_saved_path = None
    try:
        # Pass the individual path segment to the utility function
        actual_saved_path = await file_utils.save_upload_file(file, project_name_for_folder, user_defined_path_segment_for_file)
        log.info(f"File '{file.filename}' for project_id {project_id} (folder: '{project_name_for_folder}') saved to '{actual_saved_path}' (derived from user segment: '{user_defined_path_segment_for_file}').")
    except FileSaveError as e:
        log.error(f"Service: FileSaveError for '{file.filename}' (project {project_id}, folder '{project_name_for_folder}', user segment '{user_defined_path_segment_for_file}'): {e.args[0]} - Original: {e.original_exception}")
        raise HTTPException(
            status_code=status.HTTP_409_CONFLICT if "already exists" in e.args[0] else status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=e.args[0]
        )
    except FileOperationError as e:
        log.error(f"Service: FileOperationError for '{file.filename}' (project {project_id}, folder '{project_name_for_folder}', user segment '{user_defined_path_segment_for_file}'): {e.args[0]}")
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"A server configuration error occurred: {e.args[0]}"
        )
    except Exception as e:
        log.error(f"Service: Unexpected error during file save for '{file.filename}' (project {project_id}, folder '{project_name_for_folder}', user segment '{user_defined_path_segment_for_file}'): {e}", exc_info=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="An unexpected server error occurred while attempting to save the file."
        )

    original_file_name = file.filename
    file_extension = None
    if original_file_name:
        _, ext = os.path.splitext(original_file_name)
        file_extension = ext.lstrip('.').lower() if ext else "unknown" 

    db_input_source = InputSource(
        project_id=project_id,
        original_filename=original_file_name,
        source_type=file_extension, 
        file_path_or_identifier=actual_saved_path,
        analysis_status="uploaded" 
    )

    try:
        db.add(db_input_source)
        db.commit()
        db.refresh(db_input_source)
        log.info(f"Database entry created for InputSource ID {db_input_source.input_source_id} (original name: '{original_file_name}', type: '{file_extension}', file: '{actual_saved_path}')")
    except Exception as e:
        log.error(f"Database error creating entry for file '{file.filename}' (path: '{actual_saved_path}', project {project_id}): {e}", exc_info=True)
        if actual_saved_path:
            try:
                # Ensure file_utils.remove_file is available and correct
                await file_utils.remove_file(actual_saved_path) 
                log.info(f"Cleaned up file '{actual_saved_path}' after DB error.")
            except FileRemoveError as remove_e:
                log.error(f"Failed to cleanup file '{actual_saved_path}' after DB error: {remove_e.args[0]} - Original: {remove_e.original_exception}")
            except Exception as general_remove_e:
                log.error(f"Unexpected error during cleanup of file '{actual_saved_path}': {general_remove_e}", exc_info=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Could not create database entry for file: {file.filename}. Please try again or contact support if the issue persists."
        )
    
    return db_input_source

async def get_input_source_by_id(db: Session, input_source_id: int) -> Optional[InputSource]:
    log.debug(f"Fetching input source by ID: {input_source_id}")
    return db.query(InputSource).filter(InputSource.input_source_id == input_source_id).first()

async def delete_source_file_and_entry(db: Session, input_source_id: int) -> InputSourceResponseSchema:
    log.info(f"Attempting to delete input source file with ID: {input_source_id}")
    db_input_source = await get_input_source_by_id(db, input_source_id) 

    if not db_input_source:
        log.warning(f"Input source file with ID {input_source_id} not found for deletion.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Input source file not found")

    file_path_to_delete = db_input_source.file_path_or_identifier
    project_id_for_log = db_input_source.project_id
    original_filename_for_log = db_input_source.original_filename

    # It's good practice to capture the state for the response *before* deletion.
    deleted_object_data = InputSourceResponseSchema.model_validate(db_input_source)

    try:
        db.delete(db_input_source)
        db.commit()
        log.info(f"Successfully deleted database entry for InputSource ID {input_source_id} (project ID: {project_id_for_log}, original name: '{original_filename_for_log}', path: '{file_path_to_delete}').")
    except Exception as e:
        db.rollback()
        log.error(f"Database error while deleting InputSource ID {input_source_id}: {e}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail="Could not delete input source record from database.")

    if file_path_to_delete:
        try:
            await file_utils.remove_file(file_path_to_delete) # Assuming remove_file can be async or is sync
            log.info(f"Successfully deleted file from filesystem: '{file_path_to_delete}' for InputSource ID {input_source_id}.")
        except FileRemoveError as e:
            # Log this, but don't raise an HTTP exception if DB entry was deleted, 
            # as the primary operation (DB delete) succeeded.
            # The client might need to be informed differently or a cleanup task scheduled.
            log.error(f"FileRemoveError for InputSource ID {input_source_id}: Failed to delete file '{file_path_to_delete}'. DB record was already deleted. Error: {e.args[0]} - Original: {e.original_exception}")
        except Exception as e:
            log.error(f"Unexpected error deleting file '{file_path_to_delete}' for InputSource ID {input_source_id} after DB record deletion: {e}", exc_info=True)
    else:
        log.warning(f"No file path found for InputSource ID {input_source_id} (project ID: {project_id_for_log}); skipping file system deletion.")
    
    return deleted_object_data

async def get_source_files_by_project(db: Session, project_id: int, skip: int = 0, limit: int = 100) -> List[InputSource]:
    log.info(f"Fetching source files for project ID: {project_id} with skip: {skip}, limit: {limit}")
    source_files = db.query(InputSource)\
                     .filter(InputSource.project_id == project_id)\
                     .order_by(InputSource.input_source_id)\
                     .offset(skip)\
                     .limit(limit)\
                     .all()
    return source_files

async def count_source_files_for_project(db: Session, project_id: int) -> int:
    log.info(f"Counting source files for project ID: {project_id}")
    count = db.query(InputSource).filter(InputSource.project_id == project_id).count()
    return count

async def get_source_file_path_and_name(db: Session, input_source_id: int) -> tuple[Optional[str], Optional[str]]:
    db_input_source = await get_input_source_by_id(db, input_source_id)
    if not db_input_source or not db_input_source.file_path_or_identifier:
        return None, None
    
    # Check if file exists on the server before returning path
    if not os.path.exists(db_input_source.file_path_or_identifier):
        log.error(f"File path '{db_input_source.file_path_or_identifier}' for input_source_id {input_source_id} does not exist on server.")
        # Optionally, update DB status here to reflect missing file if desired
        return None, None 

    return db_input_source.file_path_or_identifier, db_input_source.original_filename