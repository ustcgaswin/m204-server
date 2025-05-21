# filepath: c:\Users\284713\Documents\client_projects\experian\server\app\models\m204_file_field_model.py
from sqlalchemy import Column, Integer, String, Text, ForeignKey, DateTime, Boolean
from sqlalchemy.sql import func
from sqlalchemy.types import JSON
from sqlalchemy.orm import relationship # Import relationship
from app.config.db_config import Base

class M204Field(Base):
    __tablename__ = "m204_fields"

    m204_field_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True) # Added project_id
    m204_file_id = Column(Integer, ForeignKey("m204_files.m204_file_id"), nullable=True, index=True)
    defined_in_input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=True, index=True) # PARMLIB source

    field_name = Column(String(255), nullable=False, index=True)
    attributes_text = Column(Text, nullable=True)
    attributes_json = Column(JSON, nullable=True)
    definition_line_number = Column(Integer, nullable=True)
    
    # VSAM specific attributes (if this field becomes part of a VSAM key or record structure)
    target_vsam_key_order = Column(Integer, nullable=True) # e.g., 1 for primary part of key
    target_vsam_data_type = Column(String(50), nullable=True)
    target_vsam_length = Column(Integer, nullable=True)
    is_primary_key_component = Column(Boolean, default=False)

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    # Relationship back to M204File
    m204_file = relationship("M204File", back_populates="fields", foreign_keys=[m204_file_id])
    # Relationship back to InputSource (if defined in PARMLIB)
    defined_in_input_source = relationship("InputSource", back_populates="m204_fields_defined_in", foreign_keys=[defined_in_input_source_id])
    # Relationship to Project (if you want to query fields by project directly)
    project = relationship("Project") # This assumes Project model will have a 'm204_fields' backref if needed for direct project-to-field query.
                                      # For cascade delete, if M204Field is always tied to M204File or InputSource,
                                      # deleting those parents will handle the cascade.
                                      # If a field can exist standalone linked only to a project, then Project needs a m204_fields relationship with cascade.
                                      # Given the current structure, M204Field is deleted when M204File or InputSource is deleted.

    def __repr__(self):
        return f"<M204Field(id={self.m204_field_id}, name='{self.field_name}', file_id={self.m204_file_id})>"