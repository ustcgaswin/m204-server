from sqlalchemy import Column, Integer, String, ForeignKey, DateTime, Boolean
from sqlalchemy.sql import func
from sqlalchemy.types import JSON
from sqlalchemy.orm import relationship
from app.config.db_config import Base

class M204File(Base):
    __tablename__ = "m204_files"

    m204_file_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    
    m204_file_name = Column(String(255), nullable=False, index=True) # DDNAME for JCL or PARMLIB file name
    m204_logical_dataset_name = Column(String(255), nullable=True, index=True) # M204 logical name (e.g., from DEFINE DATASET or IMAGE)
    
    defined_in_input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=True, index=True)
    definition_line_number_start = Column(Integer, nullable=True) # Start line of the definition block (e.g., DEFINE DATASET, IMAGE)
    definition_line_number_end = Column(Integer, nullable=True)   # End line of the definition block
    
    is_db_file = Column(Boolean, nullable=True, default=None) # True if it's an M204 Database File (e.g., defined in PARMLIB)
    
    # Stores the full DEFINE DATASET statement or similar raw M204 attributes
    m204_attributes = Column(String, nullable=True) 
    
    # Single JSON field to store structured file definitions:
    # For DB files (from PARMLIB): {"file_type": "db_file", "source": "parmlib", "file_name": "X", "fields": {...}, "root_level_commands": [...]}
    # For flat files (from IMAGE): {"file_type": "flat_file", "source": "image_statement", "image_definitions": [{"image_name": "Y", "fields": [...]}]}
    # For mixed (both PARMLIB and IMAGE): {"file_type": "mixed", "db_file_definition": {...}, "flat_file_definition": {...}}
    file_definition_json = Column(JSON, nullable=True)
    
    # VSAM migration target fields
    target_vsam_dataset_name = Column(String(255), nullable=True) # Suggested VSAM DSN
    target_vsam_type = Column(String(50), nullable=True) # Suggested VSAM type (KSDS, ESDS, etc.)
    primary_key_field_name = Column(String(255), nullable=True) # Comma-separated if composite, derived from vsam_suggestions in JSON
    
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    # Relationships
    project = relationship("Project", back_populates="m204_files")
    defined_in_source = relationship("InputSource", back_populates="m204_files_defined", foreign_keys=[defined_in_input_source_id])

    def __repr__(self):
        return f"<M204File(id={self.m204_file_id}, name='{self.m204_file_name}', logical_name='{self.m204_logical_dataset_name}', is_db={self.is_db_file}, project_id={self.project_id})>"