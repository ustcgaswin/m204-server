from pydantic import BaseModel
from typing import List, Optional, Dict, Any
from datetime import datetime

# --- DD Statement Schemas (for JCL) ---
class DDStatementBaseSchema(BaseModel):
    job_name: Optional[str] = None
    step_name: Optional[str] = None
    dd_name: str
    dsn: Optional[str] = None
    disposition: Optional[str] = None
    line_number_start: Optional[int] = None
    line_number_end: Optional[int] = None
    raw_statement_text: Optional[str] = None
    parameters_json: Optional[Dict[str, Any]] = None

class DDStatementCreateSchema(DDStatementBaseSchema):
    project_id: int
    input_source_id: int # The JCL file's ID

class DDStatementResponseSchema(DDStatementBaseSchema):
    dd_statement_id: int
    project_id: int
    input_source_id: int
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True

# --- Generic Analysis Result Data Schema ---
# This schema is used as the 'details' field in UnifiedAnalysisReportSchema
# for JCL and PARMLIB analyses.
class GenericAnalysisResultDataSchema(BaseModel):
    dd_statements_found: List[DDStatementResponseSchema] = []
    # For PARMLIB, you might add specific fields here or use a more tailored schema
    # e.g., parmlib_file_definitions_found: List[SomeParmLibFileDefSchema] = []
    summary: Optional[str] = None