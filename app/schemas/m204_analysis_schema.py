from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
from datetime import datetime

# --- Test Case Schema (as it might be part of the procedure's data) ---
class TestCaseSchema(BaseModel):
    test_case_id: str
    description: str
    preconditions: Optional[List[str]] = None
    inputs: Dict[str, Any]
    expected_outputs: Dict[str, Any]
    expected_behavior_description: str

# --- Image Statement Schemas ---
class ImageStatementBaseSchema(BaseModel):
    line_number: int
    image_content: str
    referenced_m204_logical_name: Optional[str] = None # Renamed from referenced_ddname

class ImageStatementCreateSchema(ImageStatementBaseSchema):
    project_id: int
    input_source_id: int

class ImageStatementResponseSchema(ImageStatementBaseSchema):
    image_statement_id: int
    project_id: int
    input_source_id: int
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True

# --- M204 Variable Schemas (Forward declaration for M204ProcedureResponseSchema) ---
class M204VariableBaseSchema(BaseModel):
    variable_name: str
    variable_type: Optional[str] = None
    scope: Optional[str] = None
    attributes: Optional[Dict[str, Any]] = None
    definition_line_number: Optional[int] = None
    cobol_mapped_variable_name: Optional[str] = None

class M204VariableResponseSchema(M204VariableBaseSchema):
    variable_id: int
    project_id: int
    input_source_id: int
    procedure_id: Optional[int] = None
    procedure_name: Optional[str] = None 
    created_at: datetime
    updated_at: datetime
    class Config: 
        from_attributes = True

# --- Procedure Schemas ---
class M204ProcedureBaseSchema(BaseModel):
    m204_proc_name: str
    m204_proc_type: Optional[str] = None
    m204_parameters_string: Optional[str] = None
    parsed_parameters_json: Optional[Dict[str, Any]] = None
    start_line_in_source: Optional[int] = None
    end_line_in_source: Optional[int] = None
    procedure_content: Optional[str] = None
    summary: Optional[str] = None
    target_cobol_program_name: Optional[str] = None
    # For creation, this will be a list of dictionaries.
    # This matches the attribute in the ORM model.
    suggested_test_cases_json: Optional[List[Dict[str, Any]]] = None

class M204ProcedureCreateSchema(M204ProcedureBaseSchema):
    project_id: int
    input_source_id: int

class M204ProcedureUpdateSchema(BaseModel):
    m204_proc_type: Optional[str] = None
    m204_parameters_string: Optional[str] = None
    parsed_parameters_json: Optional[Dict[str, Any]] = None
    start_line_in_source: Optional[int] = None
    end_line_in_source: Optional[int] = None
    procedure_content: Optional[str] = None
    summary: Optional[str] = None
    target_cobol_program_name: Optional[str] = None
    suggested_test_cases_json: Optional[List[Dict[str, Any]]] = None
    is_subroutine: Optional[bool] = None
    is_public: Optional[bool] = None

class M204ProcedureResponseSchema(M204ProcedureBaseSchema):
    proc_id: int
    project_id: int
    input_source_id: int
    is_runnable_main: Optional[bool] = None # Kept as per original schema provided
    created_at: datetime
    updated_at: datetime
    variables: List[M204VariableResponseSchema] = Field(default_factory=list) 
    
    # Override the inherited 'suggested_test_cases_json' field.
    # For responses, Pydantic will parse the list of dicts from the ORM model
    # (sourced from the 'suggested_test_cases_json' attribute)
    # into a list of TestCaseSchema objects.
    # The field name in the output JSON will remain 'suggested_test_cases_json'.
    suggested_test_cases_json: Optional[List[TestCaseSchema]] = None

    class Config: 
        from_attributes = True

# --- M204 Field Schemas (renamed from M204DefineField for consistency) ---
class M204FieldBaseSchema(BaseModel):
    field_name: str
    attributes_text: Optional[str] = None
    attributes_json: Optional[Dict[str, Any]] = None
    definition_line_number: Optional[int] = None
    target_vsam_key_order: Optional[int] = None
    target_vsam_data_type: Optional[str] = None
    target_vsam_length: Optional[int] = None
    is_primary_key_component: Optional[bool] = None

class M204FieldCreateSchema(M204FieldBaseSchema):
    project_id: int
    m204_file_id: Optional[int] = None
    defined_in_input_source_id: int

class M204FieldUpdateSchema(BaseModel):
    attributes_text: Optional[str] = None
    attributes_json: Optional[Dict[str, Any]] = None
    target_vsam_key_order: Optional[int] = None
    target_vsam_data_type: Optional[str] = None
    target_vsam_length: Optional[int] = None
    # Note: is_primary_key_component excluded as requested

class M204FieldResponseSchema(M204FieldBaseSchema):
    m204_field_id: int
    project_id: int
    m204_file_id: Optional[int] = None
    defined_in_input_source_id: int
    created_at: datetime
    updated_at: datetime
    class Config:
        from_attributes = True

# --- Backward compatibility aliases ---
M204DefineFieldBaseSchema = M204FieldBaseSchema
M204DefineFieldCreateSchema = M204FieldCreateSchema
M204DefineFieldResponseSchema = M204FieldResponseSchema

# --- M204 File Schemas ---
class M204FileBaseSchema(BaseModel):
    m204_file_name: str # This is the DDNAME
    m204_logical_dataset_name: Optional[str] = None # Added: M204 logical name
    m204_attributes: Optional[str] = None
    defined_at_line: Optional[int] = None
    is_db_file: Optional[bool] = None
    target_vsam_dataset_name: Optional[str] = None
    target_vsam_type: Optional[str] = None
    primary_key_field_name: Optional[str] = None

class M204FileCreateSchema(M204FileBaseSchema):
    project_id: int
    defined_in_input_source_id: Optional[int] = None

class M204FileUpdateSchema(BaseModel):
    m204_logical_dataset_name: Optional[str] = None # Added for updates
    m204_attributes: Optional[str] = None
    is_db_file: Optional[bool] = None
    target_vsam_dataset_name: Optional[str] = None
    target_vsam_type: Optional[str] = None
    primary_key_field_name: Optional[str] = None

class M204FileResponseSchema(M204FileBaseSchema):
    m204_file_id: int
    project_id: int
    defined_in_input_source_id: Optional[int] = None
    created_at: datetime
    updated_at: datetime
    fields: List[M204FieldResponseSchema] = Field(default_factory=list)
    image_statements: List[ImageStatementResponseSchema] = Field(default_factory=list)
    class Config:
        from_attributes = True

# --- M204 Variable Schemas (Full definition) ---
# M204VariableBaseSchema and M204VariableResponseSchema are already defined above.

class M204VariableCreateSchema(M204VariableBaseSchema):
    project_id: int
    input_source_id: int
    procedure_id: Optional[int] = None

class M204VariableUpdateSchema(BaseModel):
    cobol_mapped_variable_name: Optional[str] = None
    variable_type: Optional[str] = None
    scope: Optional[str] = None
    attributes: Optional[Dict[str, Any]] = None

# --- M204 Procedure Call Schemas ---
class M204ProcedureCallBaseSchema(BaseModel):
    called_procedure_name: str
    line_number: int
    is_external: Optional[bool] = None

class M204ProcedureCallCreateSchema(M204ProcedureCallBaseSchema):
    project_id: int
    calling_input_source_id: int
    calling_procedure_id: Optional[int] = None

class M204ProcedureCallResponseSchema(M204ProcedureCallBaseSchema):
    call_id: int
    project_id: int
    calling_input_source_id: int
    calling_procedure_id: Optional[int] = None
    resolved_procedure_id: Optional[int] = None
    created_at: datetime
    updated_at: datetime
    class Config:
        from_attributes = True

# --- M204 Analysis Result Data Schema ---
class M204AnalysisResultDataSchema(BaseModel):
    procedures_found: List[M204ProcedureResponseSchema] = Field(default_factory=list)
    defined_files_found: List[M204FileResponseSchema] = Field(default_factory=list)
    defined_fields_found: List[M204FieldResponseSchema] = Field(default_factory=list)
    variables_found: List[M204VariableResponseSchema] = Field(default_factory=list)
    procedure_calls_found: List[M204ProcedureCallResponseSchema] = Field(default_factory=list)
    image_statements_found: List[ImageStatementResponseSchema] = Field(default_factory=list)

class M204AnalysisResponseSchema(BaseModel):
    input_source_id: int
    message: str
    analysis_details: Optional[M204AnalysisResultDataSchema] = None
    errors: List[str] = Field(default_factory=list)