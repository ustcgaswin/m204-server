from sqlalchemy import Column, Integer, String, DateTime, Text
from sqlalchemy.sql import func
from sqlalchemy.orm import relationship # Import relationship
from app.config.db_config import Base

class Project(Base):
    __tablename__ = "projects"

    project_id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    project_name = Column(String(255), nullable=False, unique=True, index=True)
    description = Column(Text, nullable=True)
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())
    overall_status = Column(String(50), nullable=True, default="New")

    # Define relationships with cascade delete
    # One Project can have many InputSources
    input_sources = relationship("InputSource", back_populates="project", cascade="all, delete-orphan", lazy="selectin")
    
    # One Project can have many Procedures
    procedures = relationship("Procedure", back_populates="project", cascade="all, delete-orphan", lazy="selectin")
    
    # One Project can have many M204Files
    m204_files = relationship("M204File", back_populates="project", cascade="all, delete-orphan", lazy="selectin")
    
    # One Project can have many M204Variables (if they are directly linked to project and not just through procedure/input_source)
    # If M204Variable is always tied to an InputSource or Procedure, this might not be needed here,
    # as deleting the InputSource/Procedure would cascade to its variables.
    # For direct project-level variables (if any):
    # m204_variables = relationship("M204Variable", back_populates="project", cascade="all, delete-orphan", lazy="selectin")

    # One Project can have many ProcedureCalls
    procedure_calls = relationship("ProcedureCall", back_populates="project", cascade="all, delete-orphan", lazy="selectin")

    # One Project can have many ImageStatements # Removed
    # image_statements = relationship("ImageStatement", back_populates="project", cascade="all, delete-orphan", lazy="selectin") # Removed

    # One Project can have many DDStatements
    dd_statements = relationship("DDStatement", back_populates="project", cascade="all, delete-orphan", lazy="selectin")
    

    requirement_documents = relationship("RequirementDocument", back_populates="project", cascade="all, delete-orphan")
    
    # Note: M204Field is related to M204File, so deleting M204File will handle its fields if that relationship has cascade.

    def __repr__(self):
        return f"<Project(project_id={self.project_id}, project_name='{self.project_name}', status='{self.overall_status}')>"