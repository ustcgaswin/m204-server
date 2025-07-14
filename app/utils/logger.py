import logging
import sys
import os
from logging.handlers import RotatingFileHandler
from datetime import datetime
from app.config.app_config import settings

LOGS_DIR = "logs"

# List of logger names whose output you want to suppress or
# elevate to WARNING. Edit this list as needed.
SUPPRESSED_LOGGERS = [
    "httpcore",
    "httpx",
    "urllib3",
    "requests",
    "openai._base_client",
]

def setup_logger():
    # Create logs directory if it doesn't exist
    if not os.path.exists(LOGS_DIR):
        os.makedirs(LOGS_DIR)

    # Generate timestamped log filename
    timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    log_file_name = f"app_{timestamp}.log"
    log_file_path = os.path.join(LOGS_DIR, log_file_name)

    # Define log format
    log_format = (
        "%(asctime)s - %(levelname)s - [%(filename)s:%(lineno)d] - "
        "%(name)s - %(message)s"
    )

    # Get the root logger and clear existing handlers
    logger = logging.getLogger()
    if logger.hasHandlers():
        logger.handlers.clear()

    # Console handler
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setFormatter(logging.Formatter(log_format))
    logger.addHandler(console_handler)

    # File handler (rotates at 10MB, keeps 5 backups)
    file_handler = RotatingFileHandler(
        log_file_path,
        maxBytes=10 * 1024 * 1024,
        backupCount=5,
        encoding="utf-8",
    )
    file_handler.setFormatter(logging.Formatter(log_format))
    logger.addHandler(file_handler)

    # Helper to suppress noisy loggers
    def suppress_loggers(level=logging.WARNING):
        for name in SUPPRESSED_LOGGERS:
            logging.getLogger(name).setLevel(level)

    # Environment-specific configuration
    env = settings.ENV.lower()
    if env == "dev":
        logger.setLevel(logging.DEBUG)
        console_handler.setLevel(logging.DEBUG)
        file_handler.setLevel(logging.DEBUG)

        logging.getLogger("uvicorn.access").setLevel(logging.INFO)
        logging.getLogger("sqlalchemy.engine").setLevel(logging.WARNING)

        suppress_loggers(logging.WARNING)

        print(
            f"Logger configured for DEVELOPMENT. "
            f"Writing to console and {log_file_path}"
        )
    elif env == "prod":
        logger.setLevel(logging.INFO)
        console_handler.setLevel(logging.INFO)
        file_handler.setLevel(logging.INFO)

        logging.getLogger("uvicorn.access").setLevel(logging.WARNING)
        logging.getLogger("sqlalchemy.engine").setLevel(logging.WARNING)

        suppress_loggers(logging.WARNING)

        print(
            f"Logger configured for PRODUCTION. "
            f"Writing to console and {log_file_path}"
        )
    else:
        # Fallback for other environments
        logger.setLevel(logging.INFO)
        console_handler.setLevel(logging.INFO)
        file_handler.setLevel(logging.INFO)

        suppress_loggers(logging.WARNING)

        print(
            f"Logger configured for {settings.ENV.upper()}. "
            f"Writing to console and {log_file_path}"
        )

    return logger

# Initialize and export a logger instance for easy import elsewhere
log = setup_logger()