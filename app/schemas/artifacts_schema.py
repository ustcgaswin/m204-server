from pydantic import BaseModel, Field
from typing import List, Optional, Literal
import uuid

# --- Pydantic Schemas for individual artifact types ---
# These were previously in app/models/*_output_model.py

class CobolOutputSchema(BaseModel): # Renamed to avoid confusion if an ORM model is also named CobolOutput
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    input_source_id: int
    file_name: str
    content: str
    artifact_type: str = "cobol"

    class Config:
        from_attributes = True

class JclOutputSchema(BaseModel): # Renamed
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    input_source_id: int
    file_name: str
    content: str
    jcl_purpose: Literal["general", "vsam"]
    artifact_type: str # Will be "jcl_general" or "jcl_vsam"

    class Config:
        from_attributes = True

class UnitTestOutputSchema(BaseModel): # Renamed
    id: uuid.UUID = Field(default_factory=uuid.uuid4)
    input_source_id: int
    file_name: str
    content: str # Will be plain text
    artifact_type: str = "unit_test"

    class Config:
        from_attributes = True

# --- Schemas for API Responses and Internal Service Structures ---

# Schema for individual file content in the final API response
class GeneratedFileContent(BaseModel):
    file_name: str
    content: str
    artifact_type: str # e.g., "cobol", "jcl_general", "jcl_vsam", "unit_test", "error"

# Schema for artifacts related to a single input source for the API response
class InputSourceArtifacts(BaseModel):
    input_source_id: int
    input_source_original_filename: Optional[str] = None
    generated_files: List[GeneratedFileContent] = Field(default_factory=list)

# Internal response schema for the _generate_artifacts_for_single_input_source method
# This will use the schemas defined above.
class GeneratedArtifactsResponse(BaseModel):
    input_source_id: int
    cobol_files: List[CobolOutputSchema] = Field(default_factory=list)
    jcl_files: List[JclOutputSchema] = Field(default_factory=list)
    unit_test_files: List[UnitTestOutputSchema] = Field(default_factory=list)

    class Config:
        from_attributes = True

class ArtifactGenerationRequest(BaseModel):
    pass