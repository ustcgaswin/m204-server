from fastapi import APIRouter, Depends, HTTPException, status, UploadFile, File, Form
from sqlalchemy.orm import Session
from typing import List

from app.config.db_config import get_db
from app.services import analysis_service, file_service
from app.schemas.analysis_schema import UnifiedAnalysisReportSchema,MermaidFixRequestSchema,MermaidFixResponseSchema
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
    Triggers a detailed analysis of a single source file.
    The type of analysis performed depends on the file's type.
    Note: This performs analysis in isolation. For complete, cross-file context
    (like VSAM enhancement), use the project-level ordered analysis endpoint.
    """
    log.info(f"ANALYSIS_ROUTER: Received request for detailed analysis of input_source_id: {input_source_id}")
    try:
        analysis_report = await analysis_service.perform_source_file_analysis(db, input_source_id)
        return analysis_report
    except HTTPException as he:
        log.warning(f"ANALYSIS_ROUTER: HTTPException for input_source_id {input_source_id}: {he.detail}")
        raise he
    except Exception as e:
        log.error(f"ANALYSIS_ROUTER: Unexpected error in analysis router for {input_source_id}: {e}", exc_info=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="An unexpected server error occurred while processing the analysis request."
        )

@router.post("/project/{project_id}/upload_and_analyze_parmlib", response_model=UnifiedAnalysisReportSchema, tags=["Project Analysis"])
async def upload_and_analyze_parmlib_file(
    project_id: int,
    m204_db_file_name: str = Form(...),
    file: UploadFile = File(...),
    db: Session = Depends(get_db)
):
    """
    Uploads a single PARMLIB file, associates it with a project and an M204 DB file name,
    and immediately performs a detailed analysis.
    Note: For full VSAM enhancement, run the project-level analysis afterwards.
    """
    log.info(
        f"ANALYSIS_ROUTER: Received request to upload and analyze PARMLIB file "
        f"'{file.filename}' for project {project_id}, associating with DB file "
        f"'{m204_db_file_name}'."
    )

    if not file.filename:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="A file with a filename must be provided.")

    try:
        # Step 1: Save the file and create the InputSource entry.
        input_source = await file_service.save_uploaded_file_and_create_db_entry(
            db=db,
            project_id=project_id,
            file=file,
            user_defined_path_segment_for_file=file.filename,
            source_type="parmlib",
            m204_db_file_name=m204_db_file_name
        )
        log.info(
            f"ANALYSIS_ROUTER: PARMLIB file '{file.filename}' saved as InputSource ID "
            f"{input_source.input_source_id}. Proceeding to analysis."
        )

        # Step 2: Trigger the detailed analysis using the main orchestrator.
        analysis_report = await analysis_service.perform_source_file_analysis(
            db, input_source.input_source_id
        )
        
        log.info(
            f"ANALYSIS_ROUTER: Successfully completed analysis for uploaded PARMLIB "
            f"file '{file.filename}' (InputSource ID: {input_source.input_source_id})."
        )
        try:
            log.info(f"ANALYSIS_ROUTER: Triggering project-wide VSAM enhancement for project {project_id} after PARMLIB analysis.")
            await analysis_service.enhance_project_vsam_suggestions(project_id)
            log.info(f"ANALYSIS_ROUTER: Successfully completed project-wide VSAM enhancement for project {project_id}.")
        except Exception as e_enhance:
            # Log the error but don't fail the entire request, as the primary analysis succeeded.
            log.error(f"ANALYSIS_ROUTER: VSAM enhancement step failed for project {project_id} after PARMLIB analysis: {e_enhance}", exc_info=True)

        return analysis_report

    except HTTPException as he:
        log.warning(
            f"ANALYSIS_ROUTER: HTTPException during PARMLIB upload/analysis for "
            f"project {project_id}: {he.detail}"
        )
        raise he
    except Exception as e:
        log.error(
            f"ANALYSIS_ROUTER: Unexpected error during PARMLIB upload/analysis for "
            f"project {project_id}: {e}", exc_info=True
        )
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="An unexpected server error occurred during the PARMLIB upload and analysis process."
        )

# --- Project Level Ordered Analysis ---

def get_analysis_order_key(input_source: InputSource):
    """Helper function to determine the sort order for analysis based on source type."""
    source_type = input_source.source_type.lower() if input_source.source_type else "unknown"
    # Order: PARMLIB -> M204 source types -> JCL -> Others
    if source_type == "parmlib":
        return 0  # First, process PARMLIB files for base file definitions.
    elif source_type in ["m204", "online", "batch", "include", "screen", "map", "subroutine", "userlanguage", "src", "s", "user", "proc"]:
        return 1  # Second, process all M204 source to find OPEN statements and IMAGEs.
    elif source_type == "jcl":
        return 2  # Third, process JCL to link physical DSNs using the now-existing OPEN statements.
    else:
        return 3  # Other types last


@router.post("/project/{project_id}/analyze-ordered", response_model=List[UnifiedAnalysisReportSchema], tags=["Project Analysis"])
async def trigger_project_ordered_analysis(
    project_id: int,
    db: Session = Depends(get_db)
):
    """
    Triggers a detailed analysis of all source files within a given project,
    processed in a dependency-aware order. After all files are analyzed, a
    final project-wide VSAM enhancement step is performed on all identified DB files.
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
            report = await analysis_service.perform_source_file_analysis(db, input_source_to_analyze.input_source_id)
            analysis_reports.append(report)
            log.info(f"ANALYSIS_ROUTER: Finished analysis for input_source_id: {input_source_to_analyze.input_source_id}, Status: {report.analysis_status}")
        except HTTPException as he:
            log.error(f"ANALYSIS_ROUTER: HTTPException during analysis of input_source_id {input_source_to_analyze.input_source_id} in project {project_id}: {he.detail}", exc_info=True)
            error_report = UnifiedAnalysisReportSchema(
                input_source_id=input_source_to_analyze.input_source_id,
                original_filename=input_source_to_analyze.original_filename,
                file_type_processed=input_source_to_analyze.source_type.lower() if input_source_to_analyze.source_type else "unknown",
                analysis_status="analysis_failed",
                message=f"Analysis failed: {he.detail}",
                details=None,
                errors=[str(he.detail)]
            )
            analysis_reports.append(error_report)
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

    log.info(f"ANALYSIS_ROUTER: Completed ordered analysis for project_id: {project_id}. Generated {len(analysis_reports)} reports.")

    # --- NEW: Final, project-wide VSAM enhancement step ---
    log.info(f"ANALYSIS_ROUTER: Initiating final project-wide VSAM enhancement for project {project_id}.")
    try:
        await analysis_service.enhance_project_vsam_suggestions(project_id)
        log.info(f"ANALYSIS_ROUTER: Successfully completed project-wide VSAM enhancement for project {project_id}.")
    except Exception as e_vsam_enhance:
        log.error(f"ANAL.YSIS_ROUTER: Project-wide VSAM enhancement step failed for project {project_id}: {e_vsam_enhance}", exc_info=True)
        # This error occurs after individual file reports are generated, so we log it
        # but don't alter the reports. A separate status endpoint could reflect this.

    return analysis_reports


@router.post("/fix-mermaid", response_model=MermaidFixResponseSchema, tags=["Utilities"])
async def fix_mermaid_diagram_endpoint(
    request: MermaidFixRequestSchema
):
    """
    Accepts a Mermaid diagram string and an error message, then uses an LLM
    to correct the diagram's syntax.
    """
    log.info("ANALYSIS_ROUTER: Received request to fix a Mermaid diagram.")
    try:
        fixed_code = await analysis_service.fix_mermaid_diagram(request)
        return MermaidFixResponseSchema(fixed_mermaid_code=fixed_code)
    except HTTPException as he:
        log.warning(f"ANALYSIS_ROUTER: HTTPException during Mermaid fix: {he.detail}")
        raise he
    except Exception as e:
        log.error(f"ANALYSIS_ROUTER: Unexpected error during Mermaid fix: {e}", exc_info=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="An unexpected server error occurred while fixing the Mermaid diagram."
        )