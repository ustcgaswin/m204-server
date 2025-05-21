import logging
import sys
import os
from logging.handlers import RotatingFileHandler
from datetime import datetime # Import datetime
from app.config.app_config import settings # Import settings from app_config

LOGS_DIR = "logs"
# LOG_FILE will be defined dynamically in setup_logger

def setup_logger():
    # Create logs directory if it doesn't exist
    if not os.path.exists(LOGS_DIR):
        os.makedirs(LOGS_DIR)

    # Generate log filename with timestamp
    # e.g., app_2025-05-16_14-30-00.log
    timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    log_file_name = f"app_{timestamp}.log"
    log_file_path = os.path.join(LOGS_DIR, log_file_name)

    # Define log format
    log_format = "%(asctime)s - %(levelname)s - [%(filename)s:%(lineno)d] - %(name)s - %(message)s"
    
    # Get the root logger
    logger = logging.getLogger() 

    # Remove any existing handlers to avoid duplicate logs
    if logger.hasHandlers():
        logger.handlers.clear()

    # Console Handler
    console_handler = logging.StreamHandler(sys.stdout)
    console_formatter = logging.Formatter(log_format)
    console_handler.setFormatter(console_formatter)
    logger.addHandler(console_handler)

    # File Handler (RotatingFileHandler to manage log file size)
    # Rotates the log file when it reaches 10MB, keeping up to 5 backup files for this specific timestamped log.
    file_handler = RotatingFileHandler(log_file_path, maxBytes=10*1024*1024, backupCount=5, encoding='utf-8')
    file_formatter = logging.Formatter(log_format)
    file_handler.setFormatter(file_formatter)
    logger.addHandler(file_handler)

    # Set log levels based on environment
    if settings.ENV == "dev":
        logger.setLevel(logging.DEBUG)
        console_handler.setLevel(logging.DEBUG)
        file_handler.setLevel(logging.DEBUG)
        logging.getLogger("uvicorn.access").setLevel(logging.INFO)
        logging.getLogger("sqlalchemy.engine").setLevel(logging.WARNING) 
        print(f"Logger configured for DEVELOPMENT environment. Logging to console and {log_file_path}")
    elif settings.ENV == "prod":
        logger.setLevel(logging.INFO)
        console_handler.setLevel(logging.INFO) 
        file_handler.setLevel(logging.INFO)
        logging.getLogger("uvicorn.access").setLevel(logging.WARNING)
        logging.getLogger("sqlalchemy.engine").setLevel(logging.WARNING)
        print(f"Logger configured for PRODUCTION environment. Logging to console and {log_file_path}")
    else: 
        logger.setLevel(logging.INFO)
        console_handler.setLevel(logging.INFO)
        file_handler.setLevel(logging.INFO)
        print(f"Logger configured for {settings.ENV.upper()} environment with INFO level. Logging to console and {log_file_path}")

    return logger

# Initialize and export a logger instance for easy import in other modules
log = setup_logger()