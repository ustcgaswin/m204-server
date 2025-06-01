from fastapi import APIRouter, Depends, HTTPException, status, Body
from sqlalchemy.orm import Session
from typing import List, Optional

from app.config.db_config import get_db
from app.schemas.requirement_document_schema import (
    RequirementDocumentResponseSchema,
    GenerateRequirementsRequestSchema
    # RequirementGenerationOptionsSchema is part of GenerateRequirementsRequestSchema
)
from app.schemas.response_schema import Response
from app.services import requirement_document_service
from app.services import project_service # To check if project exists
from app.utils.logger import log

router = APIRouter(
    prefix="/requirements",
    tags=["Requirement Documents"],
    responses={
        404: {"description": "Not found"},
        500: {"description": "Internal Server Error"}
    },
)

@router.post(
    "/projects/{project_id}/generate-document",
    response_model=Response[RequirementDocumentResponseSchema],
    summary="Generate a Requirements Document for a Project",
    description="Triggers the generation of a comprehensive requirements document in Markdown format for the specified project using an LLM, based on the analyzed data in the database. The generated document is saved."
)
async def generate_and_save_requirements_document_for_project(
    project_id: int,
    request_body: GenerateRequirementsRequestSchema = Body(default_factory=GenerateRequirementsRequestSchema),
    db: Session = Depends(get_db)
):
    log.info(f"Received request to generate requirements document for project ID: {project_id} with options: {request_body.options.model_dump_json()}")

    # Ensure project exists
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"Project with ID {project_id} not found for requirements document generation.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    try:
        generated_doc_data = await requirement_document_service.generate_and_save_project_requirements_document(
            db=db,
            project_id=project_id,
            options=request_body.options,
            custom_prompt_section=request_body.custom_prompt_section
        )
        log.info(f"Successfully generated and saved requirements document ID {generated_doc_data.requirement_document_id} for project ID: {project_id}")
        # The generated_doc_data from the service now includes 'generation_options_used' populated with the input 'options'
        return Response[RequirementDocumentResponseSchema](
            message="Requirements document generated and saved successfully.",
            data=generated_doc_data
        )
    except HTTPException as he:
        log.error(f"HTTPException during requirements document generation for project {project_id}: {he.detail}", exc_info=True)
        raise he
    except Exception as e:
        log.error(f"Unexpected error during requirements document generation for project {project_id}: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"An unexpected error occurred while generating the requirements document: {str(e)}"
        )

@router.get(
    "/projects/{project_id}/latest-document",
    response_model=Response[Optional[RequirementDocumentResponseSchema]],
    summary="Get the Latest Generated Requirements Document for a Project"
)
async def get_latest_requirements_document_for_project(
    project_id: int,
    db: Session = Depends(get_db)
):
    log.info(f"Request to retrieve latest requirements document for project ID: {project_id}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    document = await requirement_document_service.get_latest_requirement_document_by_project_id(db, project_id)
    if not document:
        return Response[Optional[RequirementDocumentResponseSchema]](
            message="No requirements document found for this project.",
            data=None
        )

    return Response[Optional[RequirementDocumentResponseSchema]](
        message="Latest requirements document retrieved successfully.",
        data=document # This will include 'generation_options_used' if populated by the service
    )

@router.get(
    "/{requirement_document_id}",
    response_model=Response[RequirementDocumentResponseSchema],
    summary="Get a Specific Generated Requirements Document by ID"
)
async def get_requirements_document_by_id_route(
    requirement_document_id: int,
    db: Session = Depends(get_db)
):
    log.info(f"Request to retrieve requirements document ID: {requirement_document_id}")
    document = await requirement_document_service.get_requirement_document_by_id(db, requirement_document_id)
    if not document:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Requirements document with ID {requirement_document_id} not found.")

    return Response[RequirementDocumentResponseSchema](
        message="Requirements document retrieved successfully.",
        data=document # This will include 'generation_options_used' if populated by the service
    )

@router.get(
    "/projects/{project_id}/all-documents",
    response_model=Response[List[RequirementDocumentResponseSchema]],
    summary="List All Generated Requirements Documents for a Project"
)
async def list_all_requirements_documents_for_project(
    project_id: int,
    db: Session = Depends(get_db)
):
    log.info(f"Request to list all requirements documents for project ID: {project_id}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    documents = await requirement_document_service.get_all_requirement_documents_by_project_id(db, project_id)

    return Response[List[RequirementDocumentResponseSchema]](
        message=f"Retrieved {len(documents)} requirements documents for project ID {project_id}.",
        data=documents # Each item will include 'generation_options_used' if populated by the service
    )