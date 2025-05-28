from pydantic_settings import BaseSettings
from pydantic import model_validator # Import model_validator for Pydantic V2


class Settings(BaseSettings):
    # General Application Settings
    APP_NAME: str = "M204 to Cobol+VSAM Converter"
    APP_VERSION: str = "0.1.0"
    ENV: str = "prod"  # Default environment
    DEBUG: bool = False # Initial default, will be overridden by validator if ENV is dev

    @model_validator(mode='after') # Use model_validator with mode 'after'
    def set_debug_based_on_env(self) -> 'Settings':
        """
        Set DEBUG to True if ENV is 'dev'.
        Otherwise, DEBUG retains its value (from .env or the initial default).
        """
        if self.ENV == "dev":
            self.DEBUG = True
        # If ENV is not 'dev', DEBUG will keep whatever value it was assigned
        # (either from an explicit .env setting or the class default of False).
        return self

    class Config:
        env_file = ".env"
        env_file_encoding = 'utf-8'
        extra = 'ignore'

settings = Settings()