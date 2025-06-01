from pydantic import BaseModel, Field
from typing import Optional, Dict, Any
from datetime import datetime

class RequirementGenerationOptionsSchema(BaseModel):
    """
    Options to control the content and structure of the generated requirements document.
    """
    include_project_overview: bool = Field(default=True, description="Include project name and description.")
    include_procedures: bool = Field(default=True, description="Include details of M204 procedures.")
    include_procedure_summaries: bool = Field(default=True, description="Include AI-generated summaries for procedures (if applicable to prompt).")
    include_procedure_variables: bool = Field(default=True, description="List variables within each procedure's section.")
    include_files: bool = Field(default=True, description="Include details of M204 files and their fields.")
    include_global_variables: bool = Field(default=True, description="Include a section for global/public M204 variables.")
    include_jcl_dd_statements: bool = Field(default=True, description="Include JCL DD statements related to the project.")
    # include_image_statements: bool = Field(default=True, description="Include IMAGE statements found.") # Removed
    include_procedure_calls: bool = Field(default=True, description="Include a section on procedure call relationships/diagram (text-based).")

    class Config:
        from_attributes = True

class RequirementDocumentBaseSchema(BaseModel):
    document_title: str = Field(default="Project Requirements Document")
    markdown_content: str
    generation_options_json: Optional[Dict[str, Any]] = None

class RequirementDocumentCreateSchema(RequirementDocumentBaseSchema):
    project_id: int
    # generation_options_json will be populated from RequirementGenerationOptionsSchema.model_dump()

class RequirementDocumentResponseSchema(RequirementDocumentBaseSchema):
    requirement_document_id: int
    project_id: int
    created_at: datetime
    updated_at: datetime
    generation_options_used: Optional[RequirementGenerationOptionsSchema] = None # To show what options were used

    class Config:
        from_attributes = True

class GenerateRequirementsRequestSchema(BaseModel):
    options: RequirementGenerationOptionsSchema = Field(default_factory=RequirementGenerationOptionsSchema)
    custom_prompt_section: Optional[str] = Field(default=None, description="Optional custom instructions or sections to add to the LLM prompt for document generation.")