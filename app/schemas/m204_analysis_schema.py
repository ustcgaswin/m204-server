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

# --- M204 Variable Schemas (Forward declaration for M204ProcedureResponseSchema) ---
class M204VariableBaseSchema(BaseModel):
    variable_name: str
    variable_type: Optional[str] = None
    scope: Optional[str] = None
    attributes: Optional[Dict[str, Any]] = None # Renamed from attributes_json for consistency
    definition_line_number: Optional[int] = None
    # Renamed from suggested_cobol_name to be more generic if other targets are considered
    cobol_mapped_variable_name: Optional[str] = None # Was suggested_cobol_name
    cobol_variable_type: Optional[str] = None # e.g., PIC X(10), PIC 9(5)V99
    # is_explicitly_defined: Optional[bool] = None # Removed as it's not in the model and context implies true


class M204VariableResponseSchema(M204VariableBaseSchema):
    variable_id: int # Changed from m204_variable_id
    project_id: int
    input_source_id: int
    procedure_id: Optional[int] = None
    procedure_name: Optional[str] = None # Property from the model
    created_at: datetime
    updated_at: datetime
    # cobol_variable_type is inherited from M204VariableBaseSchema

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
    target_cobol_function_name: Optional[str] = None # MODIFIED HERE
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
    target_cobol_function_name: Optional[str] = None # MODIFIED HERE
    suggested_test_cases_json: Optional[List[Dict[str, Any]]] = None
    is_subroutine: Optional[bool] = None # Added from model
    is_public: Optional[bool] = None # Added from model

class M204ProcedureResponseSchema(M204ProcedureBaseSchema):
    proc_id: int
    project_id: int
    input_source_id: int
    is_runnable_main: Optional[bool] = None
    created_at: datetime
    updated_at: datetime
    variables: List[M204VariableResponseSchema] = Field(default_factory=list) # From relationship
    suggested_test_cases_json: Optional[List[TestCaseSchema]] = None # Parsed from JSON

    class Config:
        from_attributes = True

# --- M204 File Schemas ---
class M204FileBaseSchema(BaseModel):
    m204_file_name: str # This is the DDNAME or derived name
    m204_logical_dataset_name: Optional[str] = None # M204 logical name (e.g., from DEFINE DATASET or IMAGE)
    # defined_at_line: Optional[int] = None # Covered by definition_line_number_start/end
    definition_line_number_start: Optional[int] = None
    definition_line_number_end: Optional[int] = None
    m204_attributes: Optional[str] = None # Raw attributes from DEFINE DATASET
    is_db_file: Optional[bool] = None
    file_definition_json: Optional[Dict[str, Any]] = None
    target_vsam_dataset_name: Optional[str] = None
    target_vsam_type: Optional[str] = None
    primary_key_field_name: Optional[str] = None

class M204FileCreateSchema(M204FileBaseSchema):
    project_id: int
    defined_in_input_source_id: Optional[int] = None

class M204FileUpdateSchema(BaseModel):
    m204_logical_dataset_name: Optional[str] = None
    definition_line_number_start: Optional[int] = None
    definition_line_number_end: Optional[int] = None
    m204_attributes: Optional[str] = None
    is_db_file: Optional[bool] = None
    file_definition_json: Optional[Dict[str, Any]] = None
    target_vsam_dataset_name: Optional[str] = None
    target_vsam_type: Optional[str] = None
    primary_key_field_name: Optional[str] = None

class M204FileResponseSchema(M204FileBaseSchema):
    m204_file_id: int
    project_id: int
    defined_in_input_source_id: Optional[int] = None
    created_at: datetime
    updated_at: datetime

    class Config:
        from_attributes = True

# --- M204 Variable Schemas (Full definition) ---
# M204VariableBaseSchema and M204VariableResponseSchema are already defined above.

class M204VariableCreateSchema(M204VariableBaseSchema):
    project_id: int
    input_source_id: int
    procedure_id: Optional[int] = None
    # cobol_variable_type is inherited from M204VariableBaseSchema

class M204VariableUpdateSchema(BaseModel):
    cobol_mapped_variable_name: Optional[str] = None
    cobol_variable_type: Optional[str] = None # e.g., PIC X(10), PIC 9(5)V99
    variable_type: Optional[str] = None
    scope: Optional[str] = None
    attributes: Optional[Dict[str, Any]] = None
    # is_explicitly_defined: Optional[bool] = None # Removed


# --- M204 Procedure Call Schemas ---
class M204ProcedureCallBaseSchema(BaseModel):
    called_procedure_name: str
    line_number: int
    is_external: Optional[bool] = None
    # call_arguments_string: Optional[str] = None # Removed as per model simplification
    # parsed_arguments_json: Optional[Dict[str, Any]] = None # Removed as per model simplification

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
    # defined_fields_found: List[M204FieldResponseSchema] = Field(default_factory=list) # Removed
    variables_found: List[M204VariableResponseSchema] = Field(default_factory=list)
    procedure_calls_found: List[M204ProcedureCallResponseSchema] = Field(default_factory=list)

class M204AnalysisResponseSchema(BaseModel):
    input_source_id: int
    message: str
    analysis_details: Optional[M204AnalysisResultDataSchema] = None
    errors: List[str] = Field(default_factory=list)