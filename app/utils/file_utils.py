import os
import shutil
import uuid
import re 
from fastapi import UploadFile
from app.utils.logger import log
import pathlib
import asyncio 

FILE_STORAGE_PATH = "temp_uploads" 

class FileOperationError(Exception):
    """Base class for file operation errors in this utility."""
    def __init__(self, message="File operation failed.", original_exception=None):
        super().__init__(message)
        self.original_exception = original_exception

class FileSaveError(FileOperationError):
    """Error during file saving."""
    def __init__(self, message="Failed to save file.", original_exception=None):
        super().__init__(message)
        self.original_exception = original_exception

class FileRemoveError(FileOperationError):
    """Error during file removal."""
    def __init__(self, message="Failed to remove file.", original_exception=None):
        super().__init__(message)
        self.original_exception = original_exception

class DirectoryOperationError(FileOperationError):
    """Base class for directory-specific operation errors."""
    pass

class DirectoryCreateError(DirectoryOperationError):
    """Error during directory creation."""
    def __init__(self, message="Failed to create directory.", original_exception=None):
        super().__init__(message)
        self.original_exception = original_exception

class DirectoryRemoveError(DirectoryOperationError):
    """Error during directory removal."""
    def __init__(self, message="Failed to remove directory.", original_exception=None):
        super().__init__(message)
        self.original_exception = original_exception

class DirectoryRenameError(DirectoryOperationError):
    """Error during directory renaming."""
    def __init__(self, message="Failed to rename directory.", original_exception=None):
        super().__init__(message)
        self.original_exception = original_exception


def _sanitize_directory_name(name: str) -> str:
    """Sanitizes a string to be used as a directory name component."""
    if not name:
        return "" 
    name = str(name) # Ensure it's a string
    name = re.sub(r'[<>:"/\\|?*\s]', '_', name)
    name = name.strip('._-')
    name = name[:50] 
    if not name:
        return f"dir_{uuid.uuid4().hex[:8]}"
    return name

def get_project_upload_dir_path(project_name_for_folder: str) -> pathlib.Path:
    """Returns the Path object for a project's specific upload directory."""
    base_upload_dir = pathlib.Path(FILE_STORAGE_PATH)
    sanitized_project_folder_name = _sanitize_directory_name(project_name_for_folder)
    
    if not sanitized_project_folder_name:
        # This case should ideally be prevented by project name validation
        # or a more robust fallback in _sanitize_directory_name
        log.error(f"Cannot determine directory for an empty sanitized project name from original: '{project_name_for_folder}'")
        # Fallback to a generic name if sanitization results in empty, though _sanitize_directory_name tries to prevent this.
        sanitized_project_folder_name = f"invalid_project_name_{uuid.uuid4().hex[:8]}"
        
    return base_upload_dir / sanitized_project_folder_name

def ensure_project_upload_directory_exists(project_name_for_folder: str) -> str:
    """Ensures the base and project-specific upload directories exist, returns project-specific path as string."""
    base_upload_dir = pathlib.Path(FILE_STORAGE_PATH)
    if not base_upload_dir.exists():
        try:
            base_upload_dir.mkdir(parents=True, exist_ok=True)
            log.info(f"Created base upload directory: {base_upload_dir}")
        except OSError as e:
            log.error(f"Could not create base upload directory {base_upload_dir}: {e}")
            raise DirectoryCreateError(f"Server configuration error: Unable to create base upload directory.", original_exception=e) from e

    project_specific_dir = get_project_upload_dir_path(project_name_for_folder)

    if not project_specific_dir.exists():
        try:
            project_specific_dir.mkdir(parents=True, exist_ok=True)
            log.info(f"Created project-specific upload directory: {project_specific_dir}")
        except OSError as e:
            log.error(f"Could not create project-specific upload directory {project_specific_dir}: {e}")
            raise DirectoryCreateError(f"Server configuration error: Unable to create project directory '{project_specific_dir.name}'.", original_exception=e) from e
    return str(project_specific_dir)

async def save_upload_file(file: UploadFile, project_name_for_folder: str, file_path_segment: str) -> str:
    """
    Saves an uploaded file.
    'file_path_segment' is used to determine the final filename and any subdirectories within the project folder.
    """
    project_directory_path = pathlib.Path(ensure_project_upload_directory_exists(project_name_for_folder))

    path_parts = [part for part in re.split(r'[/\\]', file_path_segment) if part and part != "." and part != ".."]
    
    target_sub_dirs_parts = []
    base_filename_from_segment = ""

    if not path_parts:
        log.warning(f"User defined path segment ('{file_path_segment}') is empty or invalid. Using original filename in project root.")
        original_filename = file.filename or f"file_{uuid.uuid4().hex[:8]}.dat"
        base_filename_from_segment = "".join(c if c.isalnum() or c in ['.', '_', '-'] else '_' for c in pathlib.Path(original_filename).name)
        if not base_filename_from_segment:
             base_filename_from_segment = f"file_{uuid.uuid4().hex[:8]}{pathlib.Path(original_filename).suffix}"
    else:
        sanitized_path_components = [_sanitize_directory_name(part) for part in path_parts]
        valid_sanitized_components = [comp for comp in sanitized_path_components if comp]

        if not valid_sanitized_components:
            log.warning(f"User defined path segment ('{file_path_segment}') sanitized to empty components. Using original filename in project root.")
            original_filename = file.filename or f"file_{uuid.uuid4().hex[:8]}.dat"
            base_filename_from_segment = "".join(c if c.isalnum() or c in ['.', '_', '-'] else '_' for c in pathlib.Path(original_filename).name)
        elif len(valid_sanitized_components) == 1:
            base_filename_from_segment = valid_sanitized_components[0]
        else:
            target_sub_dirs_parts = valid_sanitized_components[:-1]
            base_filename_from_segment = valid_sanitized_components[-1]

    current_target_dir = project_directory_path
    if target_sub_dirs_parts:
        current_target_dir = project_directory_path.joinpath(*target_sub_dirs_parts)
        if not current_target_dir.exists():
            try:
                current_target_dir.mkdir(parents=True, exist_ok=True)
                log.info(f"Created subdirectory specified in path segment: {current_target_dir}")
            except OSError as e:
                log.error(f"Could not create subdirectory {current_target_dir} from path segment: {e}")
                raise DirectoryCreateError(f"Unable to create directory structure from path segment '{file_path_segment}'.", original_exception=e) from e
    
    actual_storage_path = current_target_dir / base_filename_from_segment

    if actual_storage_path.exists():
        user_visible_target_path = pathlib.Path(*target_sub_dirs_parts) / base_filename_from_segment
        log.error(f"File path conflict in project '{project_name_for_folder}': '{actual_storage_path}' (derived from user segment '{file_path_segment}') already exists.")
        raise FileSaveError(
            f"The file path '{str(user_visible_target_path)}' (derived from your provided segment '{file_path_segment}') already exists in project '{project_name_for_folder}'. Please provide a unique path identifier."
        )

    try:
        # Run synchronous file I/O in a thread pool
        loop = asyncio.get_event_loop()
        await loop.run_in_executor(None, shutil.copyfileobj, file.file, open(actual_storage_path, "wb"))
        log.info(f"Successfully saved uploaded file '{file.filename}' to '{actual_storage_path}' (project: '{project_name_for_folder}', user segment: '{file_path_segment}')")
    except IOError as e:
        log.error(f"IOError saving file '{file.filename}' to '{actual_storage_path}': {e}")
        if actual_storage_path.exists():
            try:
                await remove_file(str(actual_storage_path)) # Use async remove
            except FileRemoveError: pass
        raise FileSaveError(f"Could not save file: {file.filename}. IO Error.", original_exception=e) from e
    except Exception as e:
        log.error(f"Unexpected error saving file '{file.filename}' to '{actual_storage_path}': {e}")
        if actual_storage_path.exists():
            try:
                await remove_file(str(actual_storage_path)) # Use async remove
            except FileRemoveError: pass
        raise FileSaveError(f"An unexpected error occurred while saving file: {file.filename}.", original_exception=e) from e
    finally:
        if file.file and not file.file.closed:
            try:
                await file.close()
            except Exception as e:
                log.warning(f"Error closing uploaded file stream for '{file.filename}': {e}")
            
    return str(actual_storage_path)

async def remove_file(file_path: str):
    """Removes a file from the filesystem asynchronously."""
    path_obj = pathlib.Path(file_path)
    if not path_obj.exists():
        log.warning(f"Attempted to remove non-existent file: {file_path}")
        return # Not an error to try to remove a non-existent file in this context

    try:
        loop = asyncio.get_event_loop()
        await loop.run_in_executor(None, os.remove, path_obj)
        log.info(f"Successfully removed file: {file_path}")
    except OSError as e:
        log.error(f"OSError removing file '{file_path}': {e}")
        raise FileRemoveError(f"Could not remove file: {file_path}. OS Error.", original_exception=e) from e
    except Exception as e: # Catch any other unexpected errors
        log.error(f"Unexpected error removing file '{file_path}': {e}", exc_info=True)
        raise FileRemoveError(f"An unexpected error occurred while removing file: {file_path}.", original_exception=e) from e

async def remove_directory_recursively(dir_path_str: str):
    """Removes a directory and all its contents asynchronously."""
    dir_path = pathlib.Path(dir_path_str)
    if not dir_path.exists():
        log.warning(f"Attempted to remove non-existent directory: {dir_path}")
        return
    if not dir_path.is_dir():
        log.error(f"Path '{dir_path}' is not a directory. Cannot remove recursively.")
        raise DirectoryRemoveError(f"Path '{dir_path}' is not a directory.")

    try:
        loop = asyncio.get_event_loop()
        await loop.run_in_executor(None, shutil.rmtree, dir_path)
        log.info(f"Successfully removed directory recursively: {dir_path}")
    except OSError as e:
        log.error(f"OSError removing directory recursively '{dir_path}': {e}")
        raise DirectoryRemoveError(f"Could not remove directory: {dir_path}. OS Error.", original_exception=e) from e
    except Exception as e:
        log.error(f"Unexpected error removing directory recursively '{dir_path}': {e}", exc_info=True)
        raise DirectoryRemoveError(f"An unexpected error occurred while removing directory: {dir_path}.", original_exception=e) from e

async def rename_project_directory(old_project_name: str, new_project_name: str):
    """Renames a project's upload directory asynchronously."""
    old_dir_path = get_project_upload_dir_path(old_project_name)
    new_dir_path = get_project_upload_dir_path(new_project_name)

    if not old_dir_path.exists() or not old_dir_path.is_dir():
        log.warning(f"Old project directory '{old_dir_path}' for '{old_project_name}' not found or not a directory. Cannot rename.")
        # Depending on strictness, this could be an error or just a warning.
        # If the old dir doesn't exist, there's nothing to rename.
        # If it's not a dir, it's a problem.
        if old_dir_path.exists() and not old_dir_path.is_dir():
             raise DirectoryRenameError(f"Source path '{old_dir_path}' is not a directory.")
        return # Or raise an error if old directory must exist

    if new_dir_path.exists():
        log.error(f"Cannot rename project directory: target directory '{new_dir_path}' for new name '{new_project_name}' already exists.")
        raise DirectoryRenameError(f"Target directory '{new_dir_path}' already exists. Cannot rename.")

    try:
        loop = asyncio.get_event_loop()
        await loop.run_in_executor(None, os.rename, old_dir_path, new_dir_path)
        log.info(f"Successfully renamed project directory from '{old_dir_path}' to '{new_dir_path}'.")
    except OSError as e:
        log.error(f"OSError renaming directory from '{old_dir_path}' to '{new_dir_path}': {e}")
        raise DirectoryRenameError(f"Could not rename directory. OS Error.", original_exception=e) from e
    except Exception as e:
        log.error(f"Unexpected error renaming directory from '{old_dir_path}' to '{new_dir_path}': {e}", exc_info=True)
        raise DirectoryRenameError(f"An unexpected error occurred while renaming directory.", original_exception=e) from e
