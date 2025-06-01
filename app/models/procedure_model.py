# filepath: c:\Users\284713\Documents\client_projects\experian\server\app\models\procedure_model.py
from sqlalchemy import Column, Integer, String, Text, ForeignKey, Boolean, DateTime
from sqlalchemy.sql import func
from sqlalchemy.types import JSON 
from sqlalchemy.orm import relationship 
from app.config.db_config import Base

class Procedure(Base):
    __tablename__ = "procedures"

    proc_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    m204_proc_name = Column(String(255), nullable=False, index=True)
    m204_proc_type = Column(String(50), nullable=True)
    m204_parameters_string = Column(Text, nullable=True)
    parsed_parameters_json = Column(JSON, nullable=True) 
    input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=False, index=True)
    start_line_in_source = Column(Integer, nullable=True)
    end_line_in_source = Column(Integer, nullable=True) 
    procedure_content = Column(Text, nullable=True) 
    target_cobol_function_name = Column(String(255), nullable=True) # MODIFIED HERE
    summary = Column(Text, nullable=True)
    suggested_test_cases_json = Column(JSON, nullable=True) # New field for storing test cases
    is_subroutine = Column(Boolean, default=False)
    is_public = Column(Boolean, default=False)

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    project = relationship("Project", back_populates="procedures")
    input_source = relationship("InputSource", back_populates="procedures_defined", foreign_keys=[input_source_id])

    calls_made = relationship("ProcedureCall", back_populates="calling_procedure", cascade="all, delete-orphan", lazy="selectin", foreign_keys="[ProcedureCall.calling_procedure_id]")
    calls_received = relationship("ProcedureCall", back_populates="resolved_procedure", cascade="all, delete-orphan", lazy="selectin", foreign_keys="[ProcedureCall.resolved_procedure_id]") 
    
    variables_in_procedure = relationship("M204Variable", back_populates="procedure", cascade="all, delete-orphan", lazy="selectin", foreign_keys="[M204Variable.procedure_id]")

    def __repr__(self):
        return f"<Procedure(proc_id={self.proc_id}, name='{self.m204_proc_name}', source_id={self.input_source_id})>"