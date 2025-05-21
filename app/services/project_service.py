from sqlalchemy.orm import Session
from typing import List, Optional
from app.models.project_model import Project as ProjectModel
from app.models.input_source_model import InputSource # Added
from app.schemas.project_schema import ProjectCreate, ProjectUpdate
from app.utils.logger import log
from sqlalchemy.exc import SQLAlchemyError
from app.utils import file_utils # Added for filesystem operations
import os # For os.path.exists and os.path.isdir, though file_utils might encapsulate this
import pathlib # For path manipulation, assuming file_utils returns Path objects

async def create_project(db: Session, project: ProjectCreate) -> ProjectModel:
    db_project = ProjectModel(project_name=project.project_name, description=project.description)
    try:
        # Ensure the project directory is created when the project is created
        # This makes directory cleanup more predictable if project creation succeeds but later file uploads fail.
        file_utils.ensure_project_upload_directory_exists(project.project_name)
        log.info(f"Ensured upload directory exists for project: {project.project_name}")

        db.add(db_project)
        db.commit()
        db.refresh(db_project)
        log.info(f"Project '{db_project.project_name}' created with ID {db_project.project_id}.")
    except file_utils.FileOperationError as e_dir: # Catch error from ensure_project_upload_directory_exists
        # No db.rollback() needed here as project not yet added to session or committed
        log.error(f"Failed to create upload directory for project '{project.project_name}': {e_dir}")
        raise # Re-raise to be caught by router or higher level handler
    except SQLAlchemyError as e:
        db.rollback()
        log.error(f"Error creating project '{project.project_name}': {e}")
        # Potentially attempt to clean up the directory if it was created
        try:
            project_dir_to_clean = file_utils.get_project_upload_dir_path(project.project_name)
            if project_dir_to_clean.exists() and project_dir_to_clean.is_dir():
                await file_utils.remove_directory_recursively(str(project_dir_to_clean))
                log.info(f"Cleaned up project directory '{project_dir_to_clean}' after failed DB creation for '{project.project_name}'.")
        except Exception as e_cleanup:
            log.error(f"Error during cleanup of project directory for '{project.project_name}' after DB error: {e_cleanup}")
        raise
    return db_project

async def get_projects(db: Session, skip: int = 0, limit: int = 100) -> List[ProjectModel]:
    log.debug(f"Fetching projects with skip: {skip}, limit: {limit}")
    return db.query(ProjectModel).offset(skip).limit(limit).all()

async def get_project_by_id(db: Session, project_id: int) -> Optional[ProjectModel]:
    log.debug(f"Fetching project by ID: {project_id}")
    return db.query(ProjectModel).filter(ProjectModel.project_id == project_id).first()

async def get_project_by_name(db: Session, project_name: str) -> Optional[ProjectModel]:
    log.debug(f"Fetching project by name: {project_name}")
    return db.query(ProjectModel).filter(ProjectModel.project_name == project_name).first()

async def update_project(db: Session, project_id: int, project_update: ProjectUpdate) -> Optional[ProjectModel]:
    db_project = await get_project_by_id(db, project_id)
    if not db_project:
        log.warning(f"Project with ID {project_id} not found for update.")
        return None
    
    original_project_name = db_project.project_name
    update_data = project_update.model_dump(exclude_unset=True)
    
    name_changed = 'project_name' in update_data and update_data['project_name'] != original_project_name

    for key, value in update_data.items():
        setattr(db_project, key, value)
    
    try:
        db.add(db_project)
        db.commit()
        db.refresh(db_project)
        log.info(f"Project ID {project_id} updated successfully in DB.")

        if name_changed and project_update.project_name:
            log.info(f"Project name changed from '{original_project_name}' to '{project_update.project_name}'. Attempting to rename directory.")
            try:
                await file_utils.rename_project_directory(original_project_name, project_update.project_name)
                log.info(f"Successfully renamed project directory from '{original_project_name}' to '{project_update.project_name}'.")
            except file_utils.FileOperationError as e_rename: # Assuming a specific error for this
                log.error(f"Failed to rename project directory from '{original_project_name}' to '{project_update.project_name}': {e_rename}. DB change is committed.")
                # This is a partial failure state. The DB is updated, but the directory name is not.
                # You might want to add a system to flag this for manual review or attempt a rollback of the name in DB.
            except Exception as e_rename_unexpected:
                 log.error(f"Unexpected error renaming project directory for project ID {project_id}: {e_rename_unexpected}", exc_info=True)


    except SQLAlchemyError as e:
        db.rollback()
        log.error(f"Error updating project ID {project_id} in DB: {e}")
        raise
    return db_project

async def delete_project(db: Session, project_id: int) -> Optional[ProjectModel]:
    db_project = await get_project_by_id(db, project_id)
    if not db_project:
        log.warning(f"Project with ID {project_id} not found for deletion.")
        return None

    project_name_for_log_and_delete = db_project.project_name # Store before potential deletion from DB

    # Step 1: Get list of files to delete (paths) before deleting DB records
    # InputSource records will be deleted by cascade from the database,
    # but their files need to be explicitly removed.
    input_sources_to_delete_files_for = db.query(InputSource.file_path_or_identifier, InputSource.input_source_id)\
                                          .filter(InputSource.project_id == project_id)\
                                          .all()
    
    files_deleted_count = 0
    files_failed_to_delete_count = 0

    for file_path, input_source_id_for_log in input_sources_to_delete_files_for:
        if file_path:
            try:
                log.info(f"Attempting to delete file '{file_path}' for InputSource ID {input_source_id_for_log} as part of project '{project_name_for_log_and_delete}' deletion.")
                await file_utils.remove_file(file_path) # Assumes file_utils.remove_file is async or handled
                log.info(f"Successfully deleted file '{file_path}' for InputSource ID {input_source_id_for_log}.")
                files_deleted_count += 1
            except file_utils.FileRemoveError as e: # Catch specific error from file_utils
                log.error(f"Failed to delete file '{file_path}' for InputSource ID {input_source_id_for_log} during project deletion: {e.args[0]} - Original: {getattr(e, 'original_exception', None)}")
                files_failed_to_delete_count += 1
            except Exception as e_file_remove: # Catch any other unexpected errors
                log.error(f"Unexpected error deleting file '{file_path}' for InputSource ID {input_source_id_for_log}: {e_file_remove}", exc_info=True)
                files_failed_to_delete_count += 1
        else:
            log.warning(f"InputSource ID {input_source_id_for_log} for project '{project_name_for_log_and_delete}' has no file_path_or_identifier. Skipping file deletion for this entry.")

    if files_failed_to_delete_count > 0:
        log.warning(f"During deletion of project '{project_name_for_log_and_delete}' (ID: {project_id}), {files_failed_to_delete_count} files could not be deleted. Check previous logs.")
        # Decide if this should prevent DB deletion or directory deletion.
        # For now, we proceed but log the issue.

    # Step 2: Delete the project database record (this will cascade to InputSource records)
    try:
        db.delete(db_project)
        db.commit()
        log.info(f"Project database record ID {project_id} ('{project_name_for_log_and_delete}') and its associated InputSource records deleted successfully.")
    except SQLAlchemyError as e:
        db.rollback()
        log.error(f"Error deleting project database record ID {project_id} ('{project_name_for_log_and_delete}'): {e}")
        # If DB deletion fails, we should not proceed with directory removal.
        raise # Re-raise to indicate failure. The files attempted for deletion might be an issue.
    
    # Step 3: Delete the project's base directory from the filesystem
    project_directory_path_to_delete: Optional[pathlib.Path] = None
    try:
        # This function should return the Path object for the project's directory
        project_directory_path_to_delete = file_utils.get_project_upload_dir_path(project_name_for_log_and_delete)
        
        if project_directory_path_to_delete and project_directory_path_to_delete.exists() and project_directory_path_to_delete.is_dir():
            log.info(f"Attempting to delete project directory: '{project_directory_path_to_delete}' for project '{project_name_for_log_and_delete}'")
            # This function should recursively delete the directory
            await file_utils.remove_directory_recursively(str(project_directory_path_to_delete))
            log.info(f"Successfully deleted project directory: '{project_directory_path_to_delete}' for project '{project_name_for_log_and_delete}'")
        elif project_directory_path_to_delete and not project_directory_path_to_delete.exists():
            log.warning(f"Project directory '{project_directory_path_to_delete}' for project '{project_name_for_log_and_delete}' not found. Skipping deletion.")
        elif project_directory_path_to_delete and not project_directory_path_to_delete.is_dir():
            log.warning(f"Path '{project_directory_path_to_delete}' for project '{project_name_for_log_and_delete}' is not a directory. Skipping deletion.")
        else: # project_directory_path_to_delete is None
            log.warning(f"Could not determine project directory path for project '{project_name_for_log_and_delete}'. Skipping directory deletion.")

    except file_utils.DirectoryRemoveError as e: # Catch specific error from file_utils
        log.error(f"Failed to delete project directory for '{project_name_for_log_and_delete}' at '{project_directory_path_to_delete}': {e.args[0]} - Original: {getattr(e, 'original_exception', None)}")
        # The DB record is already deleted. This is a cleanup failure.
        # The function will still return the (now detached) db_project object.
    except Exception as e_dir_remove: # Catch any other unexpected errors
        log.error(f"Unexpected error deleting project directory for '{project_name_for_log_and_delete}' at '{project_directory_path_to_delete}': {e_dir_remove}", exc_info=True)

    return db_project # Return the (now detached) project model instance

async def count_projects(db: Session) -> int:
    log.debug("Counting total projects.")
    return db.query(ProjectModel).count()

