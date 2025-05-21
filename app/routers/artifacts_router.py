from fastapi import APIRouter, Depends, HTTPException, Path
from sqlalchemy.orm import Session
from typing import List 
# StreamingResponse and io are no longer needed for this endpoint's primary response
# import uuid # Not directly used in this specific route's signature or body now

from app.config.db_config import get_db
from app.services.artifacts_service import ArtifactsService 
from app.schemas.artifacts_schema import InputSourceArtifacts # Import the correct response model
from app.utils.logger import log 

router = APIRouter(
    prefix="/artifacts",
    tags=["Artifacts"],
    responses={404: {"description": "Not found"}},
)

# Dependency function to get the artifacts service
def get_artifacts_service(db: Session = Depends(get_db)) -> ArtifactsService:
    return ArtifactsService(db=db)

@router.post(
    "/generate/project/{project_id}",
    response_model=List[InputSourceArtifacts], # Set the correct response model
    summary="Generate Artifacts for all M204 Files in a Project as JSON",
    description=(
        "Generates COBOL, JCL, and Unit Test file contents for all M204 files associated with the given Project ID.\n"
        "The generated file contents are returned as a JSON response."
    )
)
async def generate_artifacts_for_project_route(
    project_id: int = Path(..., description="The ID of the Project for which to generate artifacts.", gt=0),
    artifacts_service: ArtifactsService = Depends(get_artifacts_service)
) -> List[InputSourceArtifacts]:
    """
    Triggers the generation of various artifacts for all M204 files within a specified project.
    Returns a JSON list containing all generated artifact contents.
    - **project_id**: The unique identifier of the project.
    """
    try:
        log.info(f"Received request to generate artifacts for project ID: {project_id} as JSON.")
        
        # The service now returns List[InputSourceArtifacts] directly
        project_artifacts: List[InputSourceArtifacts] = await artifacts_service.generate_artifacts_for_project(project_id)
        
        if not project_artifacts:
            log.info(f"No artifacts generated or no M204 input sources found for project {project_id}.")
            # The service already logs if no M204 sources are found and returns an empty list.
            # FastAPI will return an empty JSON array, which is appropriate.
        
        return project_artifacts
    except HTTPException as http_exc:
        log.error(f"HTTPException during artifact JSON generation for project {project_id}: {http_exc.detail}")
        raise http_exc
    except Exception as e:
        log.exception(f"Unexpected error during artifact JSON generation for project {project_id}:") 
        raise HTTPException(
            status_code=500,
            detail=f"An unexpected error occurred during artifact JSON generation for project {project_id}."
        )