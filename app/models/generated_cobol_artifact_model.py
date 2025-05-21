from sqlalchemy import Column, Integer, String, Text, ForeignKey, DateTime, func
from sqlalchemy.dialects.postgresql import UUID # Or your DB's UUID type if not PostgreSQL, or use String for UUID
import uuid as py_uuid
from app.config.db_config import Base # Assuming your Base is here
from sqlalchemy.orm import relationship

class GeneratedCobolArtifact(Base):
    __tablename__ = "generated_cobol_artifacts"

    id = Column(UUID(as_uuid=True), primary_key=True, default=py_uuid.uuid4)
    input_source_id = Column(Integer, ForeignKey("input_sources.input_source_id"), nullable=False, index=True)
    
    file_name = Column(String(255), nullable=False)
    content = Column(Text, nullable=False)
    artifact_type = Column(String(50), nullable=False, default="cobol")
    
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), server_default=func.now(), onupdate=func.now())

    input_source = relationship("InputSource") # Add back_populates in InputSource model if needed

    def __repr__(self):
        return f"<GeneratedCobolArtifact(id={self.id}, file_name='{self.file_name}', input_source_id={self.input_source_id})>"