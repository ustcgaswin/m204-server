from sqlalchemy import Column, Integer, String, ForeignKey, DateTime, JSON
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship
from app.config.db_config import Base

class M204ImageDefinition(Base):
    __tablename__ = "m204_image_definitions"

    image_definition_id = Column(Integer, primary_key=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id", ondelete='CASCADE'), nullable=False, index=True)
    input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id", ondelete='SET NULL'), nullable=True, index=True)
    
    image_name = Column(String(255), nullable=False, index=True)
    
    start_line_number = Column(Integer, nullable=True)
    end_line_number = Column(Integer, nullable=True)
    
    # This will store the parsed fields from _parse_image_definition
    fields_json = Column(JSON, nullable=True)

    created_at = Column(DateTime(timezone=True), server_default=func.now(), nullable=False)
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now(), nullable=False)

    project = relationship("Project")
    input_source = relationship("InputSource")

    def __repr__(self):
        return f"<M204ImageDefinition(id={self.image_definition_id}, name='{self.image_name}')>"