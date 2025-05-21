from pydantic import BaseModel
from typing import Optional
from datetime import datetime

class ProjectBase(BaseModel):
    project_name: str
    description: Optional[str] = None
    overall_status: Optional[str] = "New"

class ProjectCreate(ProjectBase):
    pass

class ProjectUpdate(ProjectBase):
    project_name: Optional[str] = None
    description: Optional[str] = None
    overall_status: Optional[str] = None

class ProjectInDBBase(ProjectBase):
    project_id: int
    created_at: datetime  # Changed from creation_date
    updated_at: datetime  # Added updated_at

    class Config:
        from_attributes = True # Replaces orm_mode in Pydantic v2

# Schema for reading/returning project data
class Project(ProjectInDBBase): 
    pass

# You might also want a schema for listing multiple projects
class ProjectListResponse(BaseModel):
    projects: list[Project]
    total_count: int