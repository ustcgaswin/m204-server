# filepath: c:\Users\284713\Documents\client_projects\experian\server\app\models\procedure_call_model.py
from sqlalchemy import Column, Integer, String, ForeignKey, DateTime, Boolean
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship # Import relationship
from app.config.db_config import Base

class ProcedureCall(Base):
    __tablename__ = "procedure_calls"

    call_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    calling_input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=False, index=True)
    calling_procedure_id = Column(Integer, ForeignKey("procedures.proc_id"), nullable=True, index=True)
    
    called_procedure_name = Column(String(255), nullable=False, index=True)
    line_number = Column(Integer, nullable=False)
    
    is_external = Column(Boolean, nullable=True, index=True) 
    resolved_procedure_id = Column(Integer, ForeignKey("procedures.proc_id"), nullable=True, index=True)

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    project = relationship("Project", back_populates="procedure_calls")
    calling_input_source = relationship("InputSource", back_populates="procedure_calls_made_in", foreign_keys=[calling_input_source_id])
    calling_procedure = relationship("Procedure", back_populates="calls_made", foreign_keys=[calling_procedure_id])
    resolved_procedure = relationship("Procedure", back_populates="calls_received", foreign_keys=[resolved_procedure_id])

    def __repr__(self):
        return f"<ProcedureCall(id={self.call_id}, name='{self.called_procedure_name}', line={self.line_number})>"