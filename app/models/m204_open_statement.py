from sqlalchemy import Column, Integer, String, ForeignKey
from sqlalchemy.orm import relationship
from app.config.db_config import Base

class M204OpenStatement(Base):
    __tablename__ = "m204_open_statements"

    open_statement_id = Column(Integer, primary_key=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id", ondelete='CASCADE'), nullable=False, index=True)
    input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id", ondelete='CASCADE'), nullable=False, index=True)
    m204_file_name = Column(String(255), nullable=False, index=True)  # The file/DDNAME referenced in OPEN
    line_number = Column(Integer, nullable=True)
    procedure_id = Column(Integer, ForeignKey("procedures.proc_id", ondelete='SET NULL'), nullable=True, index=True)  # If inside a procedure

    # Relationships
    input_source = relationship("InputSource", back_populates="m204_open_statements", foreign_keys=[input_source_id])
    # Optionally add: project = relationship("Project", back_populates="m204_open_statements", foreign_keys=[project_id])
    # Optionally add: procedure = relationship("Procedure", back_populates="m204_open_statements", foreign_keys=[procedure_id])

    def __repr__(self):
        return f"<M204OpenStatement(id={self.open_statement_id}, file='{self.m204_file_name}', line={self.line_number}, input_source_id={self.input_source_id})>"