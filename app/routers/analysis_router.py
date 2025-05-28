from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session
from typing import List

from app.config.db_config import get_db
from app.services import analysis_service # Updated import
from app.schemas.analysis_schema import UnifiedAnalysisReportSchema
from app.utils.logger import log
from app.models.input_source_model import InputSource

router = APIRouter(
    prefix="/analysis",
    tags=["Detailed Analysis"],
    responses={
        404: {"description": "Not found"},
        400: {"description": "Bad Request"},
        409: {"description": "Conflict - e.g. file missing for analysis"},
        500: {"description": "Internal Server Error"}
    },
)

@router.post("/source-file/{input_source_id}", response_model=UnifiedAnalysisReportSchema)
async def trigger_source_file_detailed_analysis(
    input_source_id: int,
    db: Session = Depends(get_db)
):
    """
    Triggers a detailed analysis of a source file identified by its input_source_id.
    The type of analysis performed depends on the file's extension/type.
    """
    log.info(f"ANALYSIS_ROUTER: Received request for detailed analysis of input_source_id: {input_source_id}")
    try:
        # Call the new analysis service
        analysis_report = await analysis_service.perform_source_file_analysis(db, input_source_id)
        return analysis_report
    except HTTPException as he:
        # Service layer should ideally log specific errors. Router can re-raise.
        log.warning(f"ANALYSIS_ROUTER: HTTPException for input_source_id {input_source_id}: {he.detail}")
        raise he
    except Exception as e:
        # Catch-all for unexpected errors not converted to HTTPException by the service
        log.error(f"ANALYSIS_ROUTER: Unexpected error in analysis router for {input_source_id}: {e}", exc_info=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="An unexpected server error occurred while processing the analysis request."
        )

# --- Project Level Ordered Analysis ---

def get_analysis_order_key(input_source: InputSource):
    """Helper function to determine the sort order for analysis based on source type."""
    source_type = input_source.source_type.lower() if input_source.source_type else "unknown"
    # Order: M204 source types -> PARMLIB -> JCL -> Others
    if source_type in ["m204", "online", "batch", "include", "screen", "map", "subroutine", "userlanguage", "src", "s", "user", "proc"]:
        return 0  # M204 source files and similar types first
    elif source_type == "parmlib":
        return 1  # Then PARMLIB files
    elif source_type == "jcl":
        return 2  # Then JCL files
    else:
        return 3  # Other types last


@router.post("/project/{project_id}/analyze-ordered", response_model=List[UnifiedAnalysisReportSchema], tags=["Project Analysis"])
async def trigger_project_ordered_analysis(
    project_id: int,
    db: Session = Depends(get_db)
):
    """
    Triggers a detailed analysis of all source files within a given project,
    processed in a dependency-aware order (e.g., M204 source -> PARMLIB -> JCL).
    """
    log.info(f"ANALYSIS_ROUTER: Received request for ordered analysis of project_id: {project_id}")

    project_input_sources = db.query(InputSource).filter(InputSource.project_id == project_id).all()

    if not project_input_sources:
        log.warning(f"ANALYSIS_ROUTER: No input sources found for project_id: {project_id}")
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"No input sources found for project_id: {project_id}"
        )

    # Sort input sources by type to ensure correct processing order
    sorted_input_sources = sorted(project_input_sources, key=get_analysis_order_key)

    analysis_reports: List[UnifiedAnalysisReportSchema] = []

    log.info(f"ANALYSIS_ROUTER: Processing {len(sorted_input_sources)} files for project {project_id} in the following order:")
    for i, source in enumerate(sorted_input_sources):
        log.info(f"  {i+1}. ID: {source.input_source_id}, Name: {source.original_filename}, Type: {source.source_type}, Status: {source.analysis_status}")

    for input_source_to_analyze in sorted_input_sources:
        try:
            log.info(f"ANALYSIS_ROUTER: Starting analysis for input_source_id: {input_source_to_analyze.input_source_id} ({input_source_to_analyze.original_filename}) in project {project_id}")
            # Call the new analysis service
            report = await analysis_service.perform_source_file_analysis(db, input_source_to_analyze.input_source_id)
            analysis_reports.append(report)
            log.info(f"ANALYSIS_ROUTER: Finished analysis for input_source_id: {input_source_to_analyze.input_source_id}, Status: {report.analysis_status}")
        except HTTPException as he:
            log.error(f"ANALYSIS_ROUTER: HTTPException during analysis of input_source_id {input_source_to_analyze.input_source_id} in project {project_id}: {he.detail}", exc_info=True)
            error_report = UnifiedAnalysisReportSchema(
                input_source_id=input_source_to_analyze.input_source_id,
                original_filename=input_source_to_analyze.original_filename,
                file_type_processed=input_source_to_analyze.source_type.lower() if input_source_to_analyze.source_type else "unknown",
                analysis_status="analysis_failed", # Reflects the outcome of this specific file's analysis attempt
                message=f"Analysis failed: {he.detail}",
                details=None,
                errors=[str(he.detail)]
            )
            analysis_reports.append(error_report)
            # Continue with other files
        except Exception as e:
            log.error(f"ANALYSIS_ROUTER: Unexpected error during analysis of input_source_id {input_source_to_analyze.input_source_id} in project {project_id}: {e}", exc_info=True)
            unexpected_error_report = UnifiedAnalysisReportSchema(
                input_source_id=input_source_to_analyze.input_source_id,
                original_filename=input_source_to_analyze.original_filename,
                file_type_processed=input_source_to_analyze.source_type.lower() if input_source_to_analyze.source_type else "unknown",
                analysis_status="analysis_failed",
                message="An unexpected server error occurred during this file's analysis.",
                details=None,
                errors=[str(e)]
            )
            analysis_reports.append(unexpected_error_report)
            # Continue with other files

    log.info(f"ANALYSIS_ROUTER: Completed ordered analysis for project_id: {project_id}. Generated {len(analysis_reports)} reports.")
    return analysis_reports