from fastapi import APIRouter, Depends, UploadFile, File, HTTPException, status, Form
from fastapi.responses import FileResponse
from sqlalchemy.orm import Session
from typing import List

from app.config.db_config import get_db
from app.schemas.input_source_schema import InputSourceResponseSchema 
from app.schemas.response_schema import Response, ListResponse
from app.services import file_service, project_service
from app.utils.logger import log

router = APIRouter(
    prefix="/files", 
    tags=["Files"],   
    responses={
        404: {"description": "Not found"},
        409: {"description": "Conflict - e.g., file path already exists"},
        500: {"description": "Internal Server Error"}
    },
)


@router.post("/{project_id}/upload_source_files/", response_model=ListResponse[InputSourceResponseSchema])
async def upload_source_files(
    project_id: int,
    files: List[UploadFile] = File(...), 
    source_types: List[str] = Form(...),
    m204_db_file_names: List[str] = Form(...), # New: For PARMLIB to M204 DB file mapping
    db: Session = Depends(get_db)
):
    if not files:
        log.warning(f"Upload attempt for project {project_id} with no files provided.")
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="No files provided for upload.")

    if not (len(files) == len(source_types) == len(m204_db_file_names)):
        log.warning(f"Upload attempt for project {project_id}: Mismatch between number of files ({len(files)}), source_types ({len(source_types)}), and m204_db_file_names ({len(m204_db_file_names)}).")
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="The number of files, source types, and M204 DB file names must all match.")

    log.info(f"Received {len(files)} files, {len(source_types)} source types, and {len(m204_db_file_names)} M204 DB file names for upload to project_id: {project_id}.")
    
    successful_uploads_data: List[InputSourceResponseSchema] = []
    errors_occurred: List[str] = []

    for i, single_file in enumerate(files):
        current_source_type = source_types[i]
        current_m204_db_name = m204_db_file_names[i]

        if not single_file.filename:
            log.warning(f"Upload attempt for project {project_id} included a file with no filename (source type provided: '{current_source_type}').")
            errors_occurred.append(f"A file (intended type: '{current_source_type}') was provided without a filename and was skipped.")
            continue
        
        if not current_source_type:
            log.warning(f"Upload attempt for project {project_id}, file '{single_file.filename}': No source type provided.")
            errors_occurred.append(f"File '{single_file.filename}': No source type was provided.")
            continue

        # Use the file's own name as the path segment (can be adjusted if needed)
        file_path_segment_from_filename = single_file.filename
            
        try:
            log.info(f"Processing file: '{single_file.filename}' (type: '{current_source_type}', M204 DB Name: '{current_m204_db_name or 'N/A'}') for project_id: {project_id}")
            db_input_source_model = await file_service.save_uploaded_file_and_create_db_entry(
                db=db, 
                project_id=project_id, 
                file=single_file,
                user_defined_path_segment_for_file=file_path_segment_from_filename,
                source_type=current_source_type,
                m204_db_file_name=current_m204_db_name
            )
            input_source_data = InputSourceResponseSchema.model_validate(db_input_source_model)
            successful_uploads_data.append(input_source_data)
            log.info(f"Successfully processed upload for project_id: {project_id}, file: '{single_file.filename}', type: '{current_source_type}', input_source_id: {input_source_data.input_source_id}, stored_path: '{input_source_data.file_path_or_identifier}'")
        except HTTPException as he:
            log.error(f"HTTPException during upload of file '{single_file.filename}' (type: '{current_source_type}') for project {project_id}: {he.detail}")
            errors_occurred.append(f"File '{single_file.filename}' (type: '{current_source_type}'): {he.detail}")
        except Exception as e:
            log.error(f"Unexpected error during upload of file '{single_file.filename}' (type: '{current_source_type}') for project {project_id}: {str(e)}", exc_info=True)
            errors_occurred.append(f"File '{single_file.filename}' (type: '{current_source_type}'): An unexpected error occurred.")

    if not successful_uploads_data and errors_occurred:
        log.error(f"All file uploads failed for project {project_id}. First error: {errors_occurred[0]}")
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=f"All file uploads failed. First error: {errors_occurred[0]}")
    
    message = f"Processed {len(files)} file(s). {len(successful_uploads_data)} uploaded successfully."
    if errors_occurred:
        message += f" {len(errors_occurred)} file(s) failed to upload. Errors: {'; '.join(errors_occurred)}"
        log.warning(f"Partial success for upload to project {project_id}: {message}")

    return ListResponse[InputSourceResponseSchema](
        message=message,
        data=successful_uploads_data,
        total=len(successful_uploads_data), 
        skip=0, 
        limit=len(files) 
    )



@router.delete("/source_files/{input_source_id}", response_model=Response[InputSourceResponseSchema])
async def delete_source_file(
    input_source_id: int,
    db: Session = Depends(get_db)
):
    log.info(f"Received request to delete input source file with ID: {input_source_id}")
    
    deleted_input_source_data = await file_service.delete_source_file_and_entry(
        db=db, 
        input_source_id=input_source_id
    )
    
    log.info(f"Successfully processed deletion request for input source ID: {input_source_id}, original name: '{deleted_input_source_data.original_filename}', file path: '{deleted_input_source_data.file_path_or_identifier}'")
    return Response[InputSourceResponseSchema](message=f"Input source ID {input_source_id} and its associated file deleted successfully.", data=deleted_input_source_data)

@router.get("/{project_id}/source_files/", response_model=ListResponse[InputSourceResponseSchema])
async def list_source_files_for_project(
    project_id: int,
    db: Session = Depends(get_db),
    skip: int = 0,
    limit: int = 100
):
    log.info(f"Received request to list source files for project ID: {project_id}, skip: {skip}, limit: {limit}")
    
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"Project with ID {project_id} not found when trying to list source files.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    source_file_models = await file_service.get_source_files_by_project(
        db, project_id=project_id, skip=skip, limit=limit
    )
    
    source_files_data: List[InputSourceResponseSchema] = [InputSourceResponseSchema.model_validate(sf) for sf in source_file_models]
    
    total_items = await file_service.count_source_files_for_project(db, project_id=project_id)

    if not source_files_data and total_items == 0 :
        log.info(f"No source files found for project ID: {project_id}")
        empty_data: List[InputSourceResponseSchema] = []
        return ListResponse[InputSourceResponseSchema](
            message="No source files found for this project.", 
            data=empty_data, 
            total=0, 
            skip=skip, 
            limit=limit
        )
        
    log.info(f"Found {len(source_files_data)} source files for project ID: {project_id} on this page. Total items for project: {total_items}")
    return ListResponse[InputSourceResponseSchema](
        message="Source files retrieved successfully.", 
        data=source_files_data,
        total=total_items,
        skip=skip,
        limit=limit
    )

@router.get("/source_files/{input_source_id}/content")
async def view_source_file_content(
    input_source_id: int,
    db: Session = Depends(get_db)
):
    """
    Retrieves and returns the content of a specific source file.
    """
    log.info(f"Received request to view content of input source file ID: {input_source_id}")
    
    file_path, original_filename = await file_service.get_source_file_path_and_name(db, input_source_id)
    
    if not file_path or not original_filename:
        log.warning(f"File path or original filename not found for input source ID {input_source_id}, or file does not exist on server.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Source file not found or path is invalid.")
        
    log.info(f"Streaming file '{original_filename}' from path '{file_path}' for input source ID {input_source_id}.")
    return FileResponse(path=file_path, filename=original_filename)