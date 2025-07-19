from pydantic import BaseModel
from typing import List, Optional, Union

from app.schemas.m204_analysis_schema import M204AnalysisResultDataSchema
from app.schemas.generic_analysis_schema import GenericAnalysisResultDataSchema

class UnifiedAnalysisReportSchema(BaseModel):
    input_source_id: int
    original_filename: Optional[str] = None
    file_type_processed: str
    analysis_status: str
    message: str
    details: Optional[Union[M204AnalysisResultDataSchema, GenericAnalysisResultDataSchema, None]] = None
    errors: List[str] = []

    class Config:
        from_attributes = True


class MermaidFixRequestSchema(BaseModel):
    """Schema for the request to fix a Mermaid diagram."""
    mermaid_code: str
    error_message: str

class MermaidFixResponseSchema(BaseModel):
    """Schema for the response containing the fixed Mermaid diagram and explanation."""
    fixed_mermaid_code: str
    detected_issue: str
    fix_explanation: str