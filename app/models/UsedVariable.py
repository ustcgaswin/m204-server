from pydantic import BaseModel, Field

class UsedVariable(BaseModel):
    cobol_variable_name: str = Field(..., description="The COBOL variable name as used in the generated code.")
    cobol_variable_type: str = Field(..., description="COBOL PIC or type, as used in the code.")
    usage_context: str = Field(..., description="Brief description of how the variable is used in the COBOL code.")