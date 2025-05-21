from sqlalchemy import Column, Integer, String, Text, ForeignKey, DateTime
from sqlalchemy.sql import func 
from sqlalchemy.orm import relationship
from app.config.db_config import Base

class ImageStatement(Base):
    __tablename__ = "image_statements"

    image_statement_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=False, index=True)
    
    line_number = Column(Integer, nullable=False)
    image_content = Column(Text, nullable=False) 
    referenced_m204_logical_name = Column(String(255), nullable=True, index=True) # Renamed from referenced_ddname

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    project = relationship("Project", back_populates="image_statements")
    input_source = relationship("InputSource", back_populates="image_statements_in", foreign_keys=[input_source_id])

    def __repr__(self):
        return f"<ImageStatement(id={self.image_statement_id}, source_id={self.input_source_id}, line={self.line_number}, logical_name='{self.referenced_m204_logical_name}')>"