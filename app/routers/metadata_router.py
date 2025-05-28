from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from typing import List

from app.config.db_config import get_db
from app.services import metadata_service, project_service, file_service
from app.schemas.input_source_schema import InputSourceResponseSchema
from app.schemas.m204_analysis_schema import (
    M204ProcedureResponseSchema, M204ProcedureUpdateSchema,
    M204VariableResponseSchema, M204VariableUpdateSchema,
    M204FileResponseSchema, M204FileUpdateSchema,
    M204FieldResponseSchema, M204FieldUpdateSchema
)
from app.schemas.response_schema import ListResponse
from app.utils.logger import log

router = APIRouter(
    prefix="/projects/{project_id}/metadata",
    tags=["Project Metadata"],
    responses={
        404: {"description": "Not found (e.g., Project, M204File, M204Variable not found)"},
        500: {"description": "Internal Server Error"}
    },
)

@router.get("/source_files/all", response_model=ListResponse[InputSourceResponseSchema])
async def list_all_project_source_files(
    project_id: int,
    db: Session = Depends(get_db),
    skip: int = 0,
    limit: int = 100
):
    log.info(f"MetadataRouter: Listing ALL source files for project ID: {project_id}, skip: {skip}, limit: {limit}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    source_file_models = await file_service.get_source_files_by_project(
        db, project_id=project_id, skip=skip, limit=limit
    )
    total_items = await file_service.count_source_files_for_project(db, project_id=project_id)

    source_files_data: List[InputSourceResponseSchema] = [InputSourceResponseSchema.model_validate(sf) for sf in source_file_models]

    if not source_files_data and total_items == 0:
        empty_data: List[InputSourceResponseSchema] = []
        return ListResponse[InputSourceResponseSchema](message="No source files found for this project.", data=empty_data, total=0, skip=skip, limit=limit)

    return ListResponse[InputSourceResponseSchema](
        message="All source files retrieved successfully.",
        data=source_files_data,
        total=total_items,
        skip=skip,
        limit=limit
    )

@router.get("/source_files/m204_db_definitions", response_model=ListResponse[InputSourceResponseSchema])
async def list_project_m204_db_definition_files(
    project_id: int,
    db: Session = Depends(get_db),
    skip: int = 0,
    limit: int = 100
):
    log.info(f"MetadataRouter: Listing InputSource files that DEFINE M204 DB files for project ID: {project_id}, skip: {skip}, limit: {limit}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found when listing M204 DB definition files.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    source_file_models = await metadata_service.get_m204_db_definition_files_by_project(
        db, project_id=project_id, skip=skip, limit=limit
    )
    total_items = await metadata_service.count_m204_db_definition_files_for_project(db, project_id=project_id)

    source_files_data: List[InputSourceResponseSchema] = [InputSourceResponseSchema.model_validate(sf) for sf in source_file_models]

    if not source_files_data and total_items == 0:
        empty_data: List[InputSourceResponseSchema] = []
        return ListResponse[InputSourceResponseSchema](message="No InputSource files defining M204 DB files found for this project.", data=empty_data, total=0, skip=skip, limit=limit)

    return ListResponse[InputSourceResponseSchema](
        message="InputSource files defining M204 DB files retrieved successfully.",
        data=source_files_data,
        total=total_items,
        skip=skip,
        limit=limit
    )

@router.get("/m204_files/databases", response_model=ListResponse[M204FileResponseSchema])
async def list_project_m204_database_files(
    project_id: int,
    db: Session = Depends(get_db),
    skip: int = 0,
    limit: int = 100
):
    log.info(f"MetadataRouter: Listing M204 Database File entries (is_db_file=True) with fields for project ID: {project_id}, skip: {skip}, limit: {limit}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found when listing M204 Database File entries.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    m204_file_models = await metadata_service.get_m204_database_files_by_project(
        db, project_id=project_id, skip=skip, limit=limit
    )
    total_items = await metadata_service.count_m204_database_files_for_project(db, project_id=project_id)

    m204_files_data: List[M204FileResponseSchema] = [M204FileResponseSchema.model_validate(mf) for mf in m204_file_models]

    if not m204_files_data and total_items == 0:
        empty_data: List[M204FileResponseSchema] = []
        return ListResponse[M204FileResponseSchema](message="No M204 Database File entries found for this project.", data=empty_data, total=0, skip=skip, limit=limit)

    return ListResponse[M204FileResponseSchema](
        message="M204 Database File entries with fields retrieved successfully.",
        data=m204_files_data,
        total=total_items,
        skip=skip,
        limit=limit
    )

@router.get("/m204_files/other", response_model=ListResponse[M204FileResponseSchema])
async def list_project_other_m204_files(
    project_id: int,
    db: Session = Depends(get_db),
    skip: int = 0,
    limit: int = 100
):
    log.info(f"MetadataRouter: Listing other (non-DB) M204File entries with image statements for project ID: {project_id}, skip: {skip}, limit: {limit}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found when listing other M204File entries.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    m204_file_models = await metadata_service.get_other_m204_files_by_project(
        db, project_id=project_id, skip=skip, limit=limit
    )
    total_items = await metadata_service.count_other_m204_files_for_project(db, project_id=project_id)

    m204_files_data: List[M204FileResponseSchema] = [M204FileResponseSchema.model_validate(mf) for mf in m204_file_models]

    if not m204_files_data and total_items == 0:
        empty_data: List[M204FileResponseSchema] = []
        return ListResponse[M204FileResponseSchema](message="No other (non-DB) M204File entries found for this project.", data=empty_data, total=0, skip=skip, limit=limit)

    return ListResponse[M204FileResponseSchema](
        message="Other (non-DB) M204File entries with image statements retrieved successfully.",
        data=m204_files_data,
        total=total_items,
        skip=skip,
        limit=limit
    )


@router.get("/procedures/", response_model=ListResponse[M204ProcedureResponseSchema])
async def list_project_procedures(
    project_id: int,
    db: Session = Depends(get_db),
    skip: int = 0,
    limit: int = 100
):
    log.info(f"MetadataRouter: Listing procedures for project ID: {project_id}, skip: {skip}, limit: {limit}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    procedures = await metadata_service.get_procedures_by_project(
        db, project_id=project_id, skip=skip, limit=limit
    )
    total_items = await metadata_service.count_procedures_for_project(db, project_id=project_id)

    procedure_data: List[M204ProcedureResponseSchema] = [M204ProcedureResponseSchema.model_validate(p) for p in procedures]

    if not procedure_data and total_items == 0:
        empty_data: List[M204ProcedureResponseSchema] = []
        return ListResponse[M204ProcedureResponseSchema](message="No procedures found for this project.", data=empty_data, total=0, skip=skip, limit=limit)

    return ListResponse[M204ProcedureResponseSchema](
        message="Procedures retrieved successfully.",
        data=procedure_data,
        total=total_items,
        skip=skip,
        limit=limit
    )

@router.get("/variables/", response_model=ListResponse[M204VariableResponseSchema])
async def list_project_variables(
    project_id: int,
    db: Session = Depends(get_db),
    skip: int = 0,
    limit: int = 100
):
    log.info(f"MetadataRouter: Listing M204 variables for project ID: {project_id}, skip: {skip}, limit: {limit}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    variables = await metadata_service.get_variables_by_project(
        db, project_id=project_id, skip=skip, limit=limit
    )
    total_items = await metadata_service.count_variables_for_project(db, project_id=project_id)

    variable_data: List[M204VariableResponseSchema] = [M204VariableResponseSchema.model_validate(v) for v in variables]

    if not variable_data and total_items == 0:
        empty_data: List[M204VariableResponseSchema] = []
        return ListResponse[M204VariableResponseSchema](message="No M204 variables found for this project.", data=empty_data, total=0, skip=skip, limit=limit)

    return ListResponse[M204VariableResponseSchema](
        message="M204 variables retrieved successfully.",
        data=variable_data,
        total=total_items,
        skip=skip,
        limit=limit
    )

# --- PUT routes for editing M204 File, Field, Variable, and Procedure details ---

@router.put("/m204_files/{m204_file_id}", response_model=M204FileResponseSchema)
async def update_m204_file(
    project_id: int,
    m204_file_id: int,
    file_update_data: M204FileUpdateSchema,
    db: Session = Depends(get_db)
):
    log.info(f"MetadataRouter: Updating M204File ID: {m204_file_id} for project ID: {project_id}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found for M204File update.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    try:
        updated_m204_file = await metadata_service.update_m204_file_details(
            db, project_id=project_id, m204_file_id=m204_file_id, update_data=file_update_data
        )
    except Exception as e:
        log.error(f"MetadataRouter: Error updating M204File ID {m204_file_id}: {e}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Failed to update M204File: {str(e)}")

    if not updated_m204_file:
        log.warning(f"MetadataRouter: M204File with ID {m204_file_id} not found or update failed for project {project_id}.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"M204File with ID {m204_file_id} not found in project {project_id}, or no update performed.")

    return M204FileResponseSchema.model_validate(updated_m204_file)


@router.put("/m204_fields/{m204_field_id}", response_model=M204FieldResponseSchema)
async def update_m204_field(
    project_id: int,
    m204_field_id: int,
    field_update_data: M204FieldUpdateSchema,
    db: Session = Depends(get_db)
):
    log.info(f"MetadataRouter: Updating M204Field ID: {m204_field_id} for project ID: {project_id}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found for M204Field update.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    try:
        updated_m204_field = await metadata_service.update_m204_field_details(
            db, project_id=project_id, m204_field_id=m204_field_id, update_data=field_update_data
        )
    except Exception as e:
        log.error(f"MetadataRouter: Error updating M204Field ID {m204_field_id}: {e}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Failed to update M204Field: {str(e)}")

    if not updated_m204_field:
        log.warning(f"MetadataRouter: M204Field with ID {m204_field_id} not found or update failed for project {project_id}.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"M204Field with ID {m204_field_id} not found in project {project_id}, or no update performed.")

    return M204FieldResponseSchema.model_validate(updated_m204_field)


@router.put("/variables/{variable_id}", response_model=M204VariableResponseSchema)
async def update_m204_variable(
    project_id: int,
    variable_id: int,
    variable_update_data: M204VariableUpdateSchema,
    db: Session = Depends(get_db)
):
    log.info(f"MetadataRouter: Updating M204Variable ID: {variable_id} for project ID: {project_id}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found for M204Variable update.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    try:
        updated_variable = await metadata_service.update_m204_variable_details(
            db, project_id=project_id, variable_id=variable_id, update_data=variable_update_data
        )
    except Exception as e:
        log.error(f"MetadataRouter: Error updating M204Variable ID {variable_id}: {e}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Failed to update M204Variable: {str(e)}")

    if not updated_variable:
        log.warning(f"MetadataRouter: M204Variable with ID {variable_id} not found or update failed for project {project_id}.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"M204Variable with ID {variable_id} not found in project {project_id}, or no update performed.")

    return M204VariableResponseSchema.model_validate(updated_variable)


@router.put("/procedures/{procedure_id}", response_model=M204ProcedureResponseSchema)
async def update_procedure(
    project_id: int,
    procedure_id: int,
    procedure_update_data: M204ProcedureUpdateSchema,
    db: Session = Depends(get_db)
):
    log.info(f"MetadataRouter: Updating Procedure ID: {procedure_id} for project ID: {project_id}")
    db_project = await project_service.get_project_by_id(db, project_id=project_id)
    if not db_project:
        log.warning(f"MetadataRouter: Project with ID {project_id} not found for Procedure update.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Project with ID {project_id} not found.")

    try:
        updated_procedure = await metadata_service.update_procedure_details(
            db, project_id=project_id, procedure_id=procedure_id, update_data=procedure_update_data
        )
    except Exception as e:
        log.error(f"MetadataRouter: Error updating Procedure ID {procedure_id}: {e}", exc_info=True)
        raise HTTPException(status_code=status.HTTP_500_INTERNAL_SERVER_ERROR, detail=f"Failed to update Procedure: {str(e)}")

    if not updated_procedure:
        log.warning(f"MetadataRouter: Procedure with ID {procedure_id} not found or update failed for project {project_id}.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail=f"Procedure with ID {procedure_id} not found in project {project_id}, or no update performed.")

    return M204ProcedureResponseSchema.model_validate(updated_procedure)