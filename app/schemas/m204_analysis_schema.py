from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
from datetime import datetime

# --- Test Case Schema (optional, not used in current pipeline) ---
class TestCaseSchema(BaseModel):
    test_case_id: str
    description: str
    preconditions: Optional[List[str]] = None
    inputs: Dict[str, Any]
    expected_outputs: Dict[str, Any]
    expected_behavior_description: str

# --- M204 Variable Schemas ---
class M204VariableBaseSchema(BaseModel):
    variable_name: str
    variable_type: Optional[str] = None
    scope: Optional[str] = None
    attributes: Optional[Dict[str, Any]] = None
    definition_line_number: Optional[int] = None
    cobol_mapped_variable_name: Optional[str] = None
    cobol_variable_type: Optional[str] = None

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

class M204VariableCreateSchema(M204VariableBaseSchema):
    project_id: int
    input_source_id: int
    procedure_id: Optional[int] = None

class M204VariableUpdateSchema(BaseModel):
    cobol_mapped_variable_name: Optional[str] = None
    cobol_variable_type: Optional[str] = None
    variable_type: Optional[str] = None
    scope: Optional[str] = None
    attributes: Optional[Dict[str, Any]] = None

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
    target_cobol_function_name: Optional[str] = None

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
    target_cobol_function_name: Optional[str] = None
    is_subroutine: Optional[bool] = None
    is_public: Optional[bool] = None

class M204ProcedureResponseSchema(M204ProcedureBaseSchema):
    proc_id: int
    project_id: int
    input_source_id: int
    is_runnable_main: Optional[bool] = None
    created_at: datetime
    updated_at: datetime
    variables: List[M204VariableResponseSchema] = Field(default_factory=list)

    class Config:
        from_attributes = True

# --- M204 File Schemas ---
class M204FileBaseSchema(BaseModel):
    m204_file_name: str
    m204_logical_dataset_name: Optional[str] = None
    definition_line_number_start: Optional[int] = None
    definition_line_number_end: Optional[int] = None
    m204_attributes: Optional[str] = None
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

# --- M204 OPEN Statement Schemas ---
class M204OpenStatementBaseSchema(BaseModel):
    m204_file_name: str
    line_number: Optional[int] = None

class M204OpenStatementCreateSchema(M204OpenStatementBaseSchema):
    project_id: int
    input_source_id: int

class M204OpenStatementResponseSchema(M204OpenStatementBaseSchema):
    open_statement_id: int
    project_id: int
    input_source_id: int

    class Config:
        from_attributes = True

# --- M204 Analysis Result Data Schema ---
class M204AnalysisResultDataSchema(BaseModel):
    procedures_found: List[M204ProcedureResponseSchema] = Field(default_factory=list)
    defined_files_found: List[M204FileResponseSchema] = Field(default_factory=list)
    variables_found: List[M204VariableResponseSchema] = Field(default_factory=list)
    procedure_calls_found: List[M204ProcedureCallResponseSchema] = Field(default_factory=list)
    open_statements_found: List[M204OpenStatementResponseSchema] = Field(default_factory=list)

class M204AnalysisResponseSchema(BaseModel):
    input_source_id: int
    message: str
    analysis_details: Optional[M204AnalysisResultDataSchema] = None
    errors: List[str] = Field(default_factory=list)