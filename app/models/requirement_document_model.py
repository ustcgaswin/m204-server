from sqlalchemy import Column, Integer, String, Text, ForeignKey, DateTime
from sqlalchemy.sql import func
from sqlalchemy.types import JSON
from sqlalchemy.orm import relationship
from app.config.db_config import Base

class RequirementDocument(Base):
    __tablename__ = "requirement_documents"

    requirement_document_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    
    document_title = Column(String(512), nullable=False, default="Project Requirements Document")
    markdown_content = Column(Text, nullable=False)
    generation_options_json = Column(JSON, nullable=True) # To store the options used for generation

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    # Relationship to Project
    project = relationship("Project", back_populates="requirement_documents")

    def __repr__(self):
        return f"<RequirementDocument(id={self.requirement_document_id}, project_id={self.project_id}, title='{self.document_title}')>"