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