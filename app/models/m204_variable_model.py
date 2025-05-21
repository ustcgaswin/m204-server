from sqlalchemy import Column, Integer, String, Text, ForeignKey, DateTime, Boolean
from sqlalchemy.sql import func
from sqlalchemy.types import JSON
from sqlalchemy.orm import relationship
from app.config.db_config import Base
from typing import Optional # Added for type hinting

class M204Variable(Base):
    __tablename__ = "m204_variables"

    variable_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=False, index=True)
    procedure_id = Column(Integer, ForeignKey("procedures.proc_id"), nullable=True, index=True) # Can be null if defined outside a procedure

    variable_name = Column(String(255), nullable=False, index=True)
    variable_type = Column(String(50), nullable=True) # e.g., "%scalar_defined", "%scalar_used", "$list_used"
    scope = Column(String(50), nullable=True) # e.g., "LOCAL", "PUBLIC", "PRIVATE", "GLOBAL"
    attributes = Column(JSON, nullable=True) # For storing definition attributes like (LEN=10, TYPE=BINARY)
    definition_line_number = Column(Integer, nullable=True)
    cobol_mapped_variable_name = Column(String(255), nullable=True) # Suggested COBOL name

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    project = relationship("Project") # Assuming Project model exists and has a relationship back
    input_source = relationship("InputSource", back_populates="m204_variables_defined", foreign_keys=[input_source_id])
    procedure = relationship("Procedure", back_populates="variables_in_procedure", foreign_keys=[procedure_id], lazy="selectin")

    @property
    def procedure_name(self) -> Optional[str]:
        if self.procedure:
            return self.procedure.m204_proc_name
        return None

    def __repr__(self):
        return f"<M204Variable(id={self.variable_id}, name='{self.variable_name}', scope='{self.scope}', proc_id={self.procedure_id})>"

# Ensure InputSource model has:
# m204_variables_defined = relationship("M204Variable", back_populates="input_source", foreign_keys="[M204Variable.input_source_id]")

# Ensure Procedure model has:
# variables_in_procedure = relationship("M204Variable", back_populates="procedure", foreign_keys="[M204Variable.procedure_id]")