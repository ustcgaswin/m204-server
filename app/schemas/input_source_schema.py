from pydantic import BaseModel, Field
from typing import  Optional # Ensure List, Dict, Any are imported if used by commented out lines
from datetime import datetime
# from .m204_analysis_schema import M204ProcedureResponseSchema, M204FileResponseSchema, M204VariableResponseSchema, M204ProcedureCallResponseSchema 
# from .generic_analysis_schema import DDStatementResponseSchema
# from .artifacts_schema import CobolOutputSchema, JclOutputSchema, UnitTestOutputSchema


class InputSourceBaseSchema(BaseModel): 
    project_id: int
    original_filename: Optional[str] = None
    source_type: Optional[str] = None
    file_path_or_identifier: str 
    analysis_status: Optional[str] = Field(default="pending")
    jcl_detailed_description: Optional[str] = None 
    m204_detailed_description: Optional[str] = None # New field

    class Config:
        from_attributes = True

class InputSourceCreateSchema(InputSourceBaseSchema):
    pass 

class InputSourceUpdateSchema(BaseModel):
    original_filename: Optional[str] = None
    source_type: Optional[str] = None
    file_path_or_identifier: Optional[str] = None 
    analysis_status: Optional[str] = None
    last_analyzed_timestamp: Optional[datetime] = None
    error_message: Optional[str] = None
    jcl_detailed_description: Optional[str] = None 
    m204_detailed_description: Optional[str] = None # New field for updates

    class Config:
        from_attributes = True


class InputSourceResponseSchema(BaseModel):
    input_source_id: int
    project_id: int
    original_filename: Optional[str] = None
    source_type: Optional[str] = None
    file_path_or_identifier: str 
    analysis_status: Optional[str] = None
    last_analyzed_timestamp: Optional[datetime] = None
    error_message: Optional[str] = None
    jcl_detailed_description: Optional[str] = None 
    m204_detailed_description: Optional[str] = None # New field
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True

class InputSourceMinimalResponseSchema(BaseModel):
    input_source_id: int
    project_id: int
    original_filename: Optional[str] = None
    source_type: Optional[str] = None
    file_path_or_identifier: str 
    analysis_status: Optional[str] = None
    jcl_detailed_description: Optional[str] = None 
    m204_detailed_description: Optional[str] = None # New field
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True