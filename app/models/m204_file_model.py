from sqlalchemy import Column, Integer, String, Text, ForeignKey, DateTime, Boolean, and_
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship, foreign
from app.config.db_config import Base
# Ensure ImageStatement is imported if it's used in type hints or directly
from app.models.image_statement_model import ImageStatement # This line might be needed if not already present

class M204File(Base):
    __tablename__ = "m204_files"

    m204_file_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_id = Column(Integer, ForeignKey("projects.project_id"), nullable=False, index=True)
    
    m204_file_name = Column(String(255), nullable=False, index=True) # DDNAME for JCL
    m204_logical_dataset_name = Column(String(255), nullable=True, index=True) # M204 logical name (e.g., MORTGAGE.INPUT)
    
    defined_in_input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=True, index=True)
    defined_at_line = Column(Integer, nullable=True)
    
    m204_attributes = Column(Text, nullable=True) 
    is_db_file = Column(Boolean, nullable=True, default=None) 
    
    target_vsam_dataset_name = Column(String(255), nullable=True)
    target_vsam_type = Column(String(50), nullable=True) 
    primary_key_field_name = Column(String(255), nullable=True) 
    
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    project = relationship("Project", back_populates="m204_files")
    defined_in_source = relationship("InputSource", back_populates="m204_files_defined", foreign_keys=[defined_in_input_source_id])
    
    fields = relationship(
        "M204Field", 
        back_populates="m204_file", 
        foreign_keys="[M204Field.m204_file_id]", 
        cascade="all, delete-orphan",
        lazy="selectin"
    ) 
    
    image_statements = relationship(
        "ImageStatement", # Make sure ImageStatement is imported or resolvable
        primaryjoin=lambda: and_(
            M204File.m204_logical_dataset_name == foreign(ImageStatement.referenced_m204_logical_name), # Updated join
            M204File.project_id == foreign(ImageStatement.project_id)
        ),
        uselist=True,
        viewonly=True, 
        lazy="selectin"
    )

    def __repr__(self):
        return f"<M204File(id={self.m204_file_id}, name='{self.m204_file_name}', logical_name='{self.m204_logical_dataset_name}', project_id={self.project_id})>"