from pydantic import BaseModel
from typing import Optional
from datetime import datetime

class InputSourceBaseSchema(BaseModel):
    project_id: int
    original_filename: Optional[str] = None # Add here if it can be part of base/creation
    source_type: Optional[str] = None
    analysis_status: Optional[str] = "pending"

class InputSourceCreateSchema(InputSourceBaseSchema):
    pass

class InputSourceUpdateSchema(BaseModel):
    original_filename: Optional[str] = None # Add if updatable
    source_type: Optional[str] = None
    analysis_status: Optional[str] = None
    last_analyzed_timestamp: Optional[datetime] = None
    error_message: Optional[str] = None

class InputSourceResponseSchema(BaseModel):
    input_source_id: int
    project_id: int
    original_filename: Optional[str] = None # Add to response
    source_type: Optional[str] = None
    file_path_or_identifier: Optional[str] = None
    analysis_status: Optional[str] = None
    last_analyzed_timestamp: Optional[datetime] = None
    error_message: Optional[str] = None
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True