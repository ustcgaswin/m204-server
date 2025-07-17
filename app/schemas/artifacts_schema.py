from pydantic import BaseModel, Field
from typing import List, Optional, Literal
import uuid

from app.models.UsedVariable import UsedVariable

# --- Grouped Used Variable Schema ---

class UsedVariableGroup(BaseModel):
    group_name: str = Field(..., description="The COBOL group/record name (01-level).")
    variables: List[UsedVariable] = Field(default_factory=list, description="Variables belonging to this group.")

# --- Pydantic Schemas for individual artifact types ---

class CobolOutputSchema(BaseModel):
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    input_source_id: int
    file_name: str
    content: str
    artifact_type: str = "cobol"

    class Config:
        from_attributes = True

class JclOutputSchema(BaseModel):
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    input_source_id: int
    file_name: str
    content: str
    jcl_purpose: Literal["general", "vsam"]
    artifact_type: str

    class Config:
        from_attributes = True

class UnitTestOutputSchema(BaseModel):
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    input_source_id: int
    file_name: str
    content: str
    artifact_type: str = "unit_test"

    class Config:
        from_attributes = True

# --- COBOL LLM Output Schemas ---

class CobolParagraph(BaseModel):
    paragraph_name: str
    cobol_code: str

class M204ProcedureToCobolParagraphsOutput(BaseModel):
    m204_procedure_name: str
    paragraphs: List[CobolParagraph]
    # For backward compatibility, keep used_variables, but prefer grouped_used_variables if present
    used_variables: List[UsedVariable] = Field(default_factory=list)
    grouped_used_variables: Optional[List[UsedVariableGroup]] = Field(
        default=None,
        description="List of variable groups for working-storage generation. If present, use this for grouping."
    )
    standalone_variables: Optional[List[UsedVariable]] = Field(
        default=None,
        description="Variables not belonging to any group (01-level variables)."
    )
    comments: Optional[str] = None

class MainLoopToCobolOutput(BaseModel):
    paragraphs: List[CobolParagraph]
    used_variables: List[UsedVariable] = Field(default_factory=list)
    grouped_used_variables: Optional[List[UsedVariableGroup]] = Field(
        default=None,
        description="List of variable groups for working-storage generation. If present, use this for grouping."
    )
    standalone_variables: Optional[List[UsedVariable]] = Field(
        default=None,
        description="Variables not belonging to any group (01-level variables)."
    )
    comments: Optional[str] = None

# --- Schemas for API Responses and Internal Service Structures ---

class GeneratedFileContent(BaseModel):
    file_name: str
    content: str
    artifact_type: str

class InputSourceArtifacts(BaseModel):
    input_source_id: int
    input_source_original_filename: Optional[str] = None
    generated_files: List[GeneratedFileContent] = Field(default_factory=list)

class GeneratedArtifactsResponse(BaseModel):
    input_source_id: int
    cobol_files: List[CobolOutputSchema] = Field(default_factory=list)
    jcl_files: List[JclOutputSchema] = Field(default_factory=list)
    unit_test_files: List[UnitTestOutputSchema] = Field(default_factory=list)

    class Config:
        from_attributes = True

class ArtifactGenerationRequest(BaseModel):
    pass