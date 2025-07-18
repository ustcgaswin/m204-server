from sqlalchemy import Column, Integer, String, DateTime, ForeignKey, Text
from sqlalchemy.sql import func 
from sqlalchemy.orm import relationship 
from app.config.db_config import Base 


class InputSource(Base):
    __tablename__ = "input_sources"

    input_source_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    
    original_filename = Column(String(255), nullable=True) 
    source_type = Column(String(100), nullable=True)
    file_path_or_identifier = Column(String(1024), nullable=False, unique=True) 
    analysis_status = Column(String(50), default="pending")

    m204_db_file_name_association = Column(String(255), nullable=True)

    
    last_analyzed_timestamp = Column(DateTime(timezone=True), nullable=True)
    error_message = Column(Text, nullable=True)
    jcl_detailed_description = Column(Text, nullable=True) 
    m204_detailed_description = Column(Text, nullable=True) # New field for M204 detailed description
    main_processing_loop_content = Column(Text, nullable=True) # For M204 main loop

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    project = relationship("Project", back_populates="input_sources")

    # Relationships from InputSource to its dependents (e.g., Procedures defined in this source)
    procedures_defined = relationship("Procedure", back_populates="input_source", cascade="all, delete-orphan", lazy="selectin", foreign_keys="[Procedure.input_source_id]")
    m204_files_defined = relationship("M204File", back_populates="defined_in_source", cascade="all, delete-orphan", lazy="selectin", foreign_keys="[M204File.defined_in_input_source_id]")
    m204_variables_defined = relationship("M204Variable", back_populates="input_source", cascade="all, delete-orphan", lazy="selectin", foreign_keys="[M204Variable.input_source_id]")
    procedure_calls_made_in = relationship("ProcedureCall", back_populates="calling_input_source", cascade="all, delete-orphan", lazy="selectin", foreign_keys="[ProcedureCall.calling_input_source_id]")
    dd_statements_in = relationship("DDStatement", back_populates="input_source", cascade="all, delete-orphan", lazy="selectin", foreign_keys="[DDStatement.input_source_id]")

    # New: Relationship to M204OpenStatement
    m204_open_statements = relationship(
        "M204OpenStatement",
        back_populates="input_source",
        cascade="all, delete-orphan",
        lazy="selectin",
        foreign_keys="[M204OpenStatement.input_source_id]"
    )

    # Relationships to generated artifacts
    generated_cobol_artifacts = relationship(
        "GeneratedCobolArtifact", 
        back_populates="input_source", 
        cascade="all, delete-orphan",
        lazy="selectin" 
    )
    generated_jcl_artifacts = relationship(
        "GeneratedJclArtifact", 
        back_populates="input_source", 
        cascade="all, delete-orphan",
        lazy="selectin"
    )
    generated_unit_test_artifacts = relationship(
        "GeneratedUnitTestArtifact", 
        back_populates="input_source", 
        cascade="all, delete-orphan",
        lazy="selectin"
    )

    def __repr__(self):
        return f"<InputSource(id={self.input_source_id}, project_id={self.project_id}, original_name='{self.original_filename}', path='{self.file_path_or_identifier}', status='{self.analysis_status}')>"