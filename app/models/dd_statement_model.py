# filepath: c:\Users\284713\Documents\client_projects\experian\server\app\models\dd_statement_model.py
from sqlalchemy import Column, Integer, String, Text, ForeignKey, JSON, DateTime
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship # Import relationship
from app.config.db_config import Base

class DDStatement(Base):
    __tablename__ = "dd_statements"

    dd_statement_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=False, index=True) 
    
    job_name = Column(String(255), nullable=True, index=True)
    step_name = Column(String(255), nullable=True, index=True)
    dd_name = Column(String(255), nullable=False, index=True)
    
    dsn = Column(String(1024), nullable=True) 
    disposition = Column(String(255), nullable=True) 
    
    line_number_start = Column(Integer, nullable=True)
    line_number_end = Column(Integer, nullable=True)
    raw_statement_text = Column(Text, nullable=True) 
    
    parameters_json = Column(JSON, nullable=True) 

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    project = relationship("Project", back_populates="dd_statements")
    input_source = relationship("InputSource", back_populates="dd_statements_in", foreign_keys=[input_source_id])

    def __repr__(self):
        return f"<DDStatement(id={self.dd_statement_id}, dd_name='{self.dd_name}', job_name='{self.job_name}', step_name='{self.step_name}')>"