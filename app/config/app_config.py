from pydantic_settings import BaseSettings
from pydantic import root_validator # For Pydantic v1 style validators used by BaseSettings
import os

class Settings(BaseSettings):
    # General Application Settings
    APP_NAME: str = "M204 to Cobol+VSAM Converter"
    APP_VERSION: str = "0.1.0"
    ENV: str = "prod"  # Default environment
    DEBUG: bool = False # Initial default, will be overridden by validator if ENV is dev

    @root_validator(skip_on_failure=True) # Runs after individual field validation
    @classmethod
    def set_debug_based_on_env(cls, values):
        """
        Set DEBUG to True if ENV is 'dev'.
        Otherwise, DEBUG retains its value (from .env or the initial default).
        """
        env = values.get('ENV')
        if env == "dev":
            values['DEBUG'] = True
        # If ENV is not 'dev', DEBUG will keep whatever value it was assigned
        # (either from an explicit .env setting or the class default of False).
        return values

    class Config:
        env_file = ".env"
        env_file_encoding = 'utf-8'
        extra = 'ignore'

settings = Settings()