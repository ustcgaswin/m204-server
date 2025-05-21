from pydantic_settings import BaseSettings
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy.ext.declarative import declarative_base


class Settings(BaseSettings): # This Settings class is specific to db_config
    DATABASE_URL: str

    class Config:
        env_file = ".env" # Both config files will read .env
        env_file_encoding = 'utf-8' 
        extra = 'ignore' 

settings = Settings() # This 'settings' instance holds DATABASE_URL

engine = create_engine(settings.DATABASE_URL) # Uses the local 'settings.DATABASE_URL'
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)
Base = declarative_base()

# Dependency to get DB session
def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()