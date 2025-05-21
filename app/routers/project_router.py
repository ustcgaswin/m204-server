from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from typing import List # Added for empty_data type hint consistency

from app.schemas import project_schema
from app.schemas.response_schema import Response, ListResponse
from app.services import project_service
from app.config.db_config import get_db
from app.utils.logger import log
# Import your SQLAlchemy Project model. Adjust the import path and model name as per your project structure.
# For example, if your model is in 'app.models.project_model' and named 'Project':
from app.models.project_model import Project as ProjectDbModel 

router = APIRouter(
    prefix="/projects",
    tags=["Projects"],
    responses={404: {"description": "Not found"}},
)

@router.post("/", response_model=Response[project_schema.Project], status_code=status.HTTP_201_CREATED)
async def create_new_project(
    project: project_schema.ProjectCreate,
    db: Session = Depends(get_db)
):
    log.info(f"Attempting to create project: {project.project_name}")
    db_project_by_name = await project_service.get_project_by_name(db, project_name=project.project_name)
    # Check if db_project_by_name is an instance of your SQLAlchemy Project model
    if isinstance(db_project_by_name, ProjectDbModel):
        log.warning(f"Project with name '{project.project_name}' already exists.")
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Project name already registered")
    
    created_project_model = await project_service.create_project(db=db, project=project)
    project_data = project_schema.Project.model_validate(created_project_model)
    log.info(f"Project '{project_data.project_name}' created successfully with ID: {project_data.project_id}")
    return Response[project_schema.Project](message="Project created successfully.", data=project_data)

@router.get("/", response_model=ListResponse[project_schema.Project])
async def read_projects(
    skip: int = 0,
    limit: int = 100,
    db: Session = Depends(get_db)
):
    log.info(f"Fetching projects with skip: {skip}, limit: {limit}")
    project_models = await project_service.get_projects(db, skip=skip, limit=limit)
    # Assuming project_models is List[ProjectDbModel]
    projects_data: List[project_schema.Project] = [project_schema.Project.model_validate(p) for p in project_models] # Explicit type for projects_data
    
    total_items = await project_service.count_projects(db)
    
    log.info(f"Found {len(projects_data)} projects for current page. Total projects: {total_items}.")
    if not projects_data and total_items == 0:
        empty_data: List[project_schema.Project] = [] # Use List from typing
        return ListResponse[project_schema.Project](
            message="No projects found.", 
            data=empty_data,
            total=0,
            skip=skip,
            limit=limit
        )

    return ListResponse[project_schema.Project](
        message="Projects retrieved successfully.", 
        data=projects_data,
        total=total_items,
        skip=skip,
        limit=limit
    )

@router.get("/{project_id}", response_model=Response[project_schema.Project])
async def read_project(
    project_id: int,
    db: Session = Depends(get_db)
):
    log.info(f"Fetching project with ID: {project_id}")
    db_project_model = await project_service.get_project_by_id(db, project_id=project_id)
    if db_project_model is None: # This check is fine
        log.warning(f"Project with ID {project_id} not found.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Project not found")
    
    project_data = project_schema.Project.model_validate(db_project_model)
    log.info(f"Project '{project_data.project_name}' (ID: {project_id}) found.")
    return Response[project_schema.Project](message="Project retrieved successfully.", data=project_data)

@router.put("/{project_id}", response_model=Response[project_schema.Project])
async def update_existing_project(
    project_id: int,
    project: project_schema.ProjectUpdate,
    db: Session = Depends(get_db)
):
    log.info(f"Attempting to update project with ID: {project_id}")
    if project.project_name is not None:
        existing_project_with_name = await project_service.get_project_by_name(db, project.project_name)
        # Check if existing_project_with_name is an instance of your SQLAlchemy Project model
        if isinstance(existing_project_with_name, ProjectDbModel) and existing_project_with_name.project_id != project_id:
            log.warning(f"Cannot update project ID {project_id}: another project with name '{project.project_name}' already exists.")
            raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Project name already in use by another project")

    updated_project_model = await project_service.update_project(db=db, project_id=project_id, project_update=project)
    if updated_project_model is None: # This check is fine
        log.warning(f"Project with ID {project_id} not found for update.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Project not found")
    
    project_data = project_schema.Project.model_validate(updated_project_model)
    log.info(f"Project '{project_data.project_name}' (ID: {project_id}) updated successfully.")
    return Response[project_schema.Project](message="Project updated successfully.", data=project_data)

@router.delete("/{project_id}", response_model=Response[project_schema.Project])
async def delete_existing_project(
    project_id: int,
    db: Session = Depends(get_db)
):
    log.info(f"Attempting to delete project with ID: {project_id}")
    deleted_project_model = await project_service.delete_project(db=db, project_id=project_id)
    if deleted_project_model is None: # This check is fine
        log.warning(f"Project with ID {project_id} not found for deletion.")
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Project not found")
    
    project_data = project_schema.Project.model_validate(deleted_project_model)
    log.info(f"Project '{project_data.project_name}' (ID: {project_id}) deleted successfully.")
    return Response[project_schema.Project](message="Project deleted successfully.", data=project_data)