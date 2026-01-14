#!/usr/bin/env python3
"""
grey_gpu_optimizer/logging_config.py - Structured Logging Configuration

This module provides structured JSON logging for the Grey GPU Optimizer.
All logs are emitted in JSON format for easy parsing by external tools.

Features:
- JSON-formatted log entries with timestamps
- Separate console and file handlers
- Rotating log files for daemon operation
- Log levels: DEBUG, INFO, WARNING, ERROR
- Event-type categorization for telemetry

Usage:
    from grey_gpu_optimizer.logging_config import setup_logging, get_json_logger
    
    setup_logging(verbose=True)
    logger = get_json_logger()
    logger.info(json.dumps({"event": "gpu_detected", "vendor": "nvidia"}))
"""

from __future__ import annotations

import json
import logging
import os
import sys
from datetime import datetime, timezone
from logging.handlers import RotatingFileHandler
from pathlib import Path
from typing import Any


# =============================================================================
# Configuration
# =============================================================================

LOG_DIR = Path.home() / ".grey_optimizer" / "logs"
DEFAULT_LOG_FILE = LOG_DIR / "gpu_optimizer.log"
JSON_LOG_FILE = LOG_DIR / "gpu_optimizer.jsonl"

MAX_LOG_SIZE = 10 * 1024 * 1024  # 10 MB
MAX_LOG_BACKUPS = 5


# =============================================================================
# Custom JSON Formatter
# =============================================================================

class JSONFormatter(logging.Formatter):
    """
    Format log records as JSON objects.
    
    Each log entry is a single JSON object on one line (JSONL format).
    """
    
    def format(self, record: logging.LogRecord) -> str:
        """Format the log record as JSON."""
        log_entry = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "level": record.levelname,
            "logger": record.name,
            "message": record.getMessage(),
            "module": record.module,
            "function": record.funcName,
            "line": record.lineno,
        }
        
        # Add exception info if present
        if record.exc_info:
            log_entry["exception"] = self.formatException(record.exc_info)
        
        # Add extra fields if present
        if hasattr(record, "extra_data"):
            log_entry["extra"] = record.extra_data
        
        return json.dumps(log_entry, default=str)


class ConsoleFormatter(logging.Formatter):
    """
    Human-readable console formatter with colors.
    """
    
    COLORS = {
        "DEBUG": "\033[36m",    # Cyan
        "INFO": "\033[32m",     # Green
        "WARNING": "\033[33m",  # Yellow
        "ERROR": "\033[31m",    # Red
        "CRITICAL": "\033[35m", # Magenta
        "RESET": "\033[0m",
    }
    
    def format(self, record: logging.LogRecord) -> str:
        """Format with optional colors."""
        if sys.stdout.isatty():
            color = self.COLORS.get(record.levelname, "")
            reset = self.COLORS["RESET"]
            record.levelname = f"{color}{record.levelname}{reset}"
        
        return super().format(record)


# =============================================================================
# JSON Logger Adapter
# =============================================================================

class JSONLoggerAdapter(logging.LoggerAdapter):
    """
    Logger adapter that adds extra context to JSON logs.
    """
    
    def process(self, msg: str, kwargs: Any) -> tuple[str, Any]:
        """Process the message with extra context."""
        if "extra" not in kwargs:
            kwargs["extra"] = {}
        kwargs["extra"]["extra_data"] = self.extra
        return msg, kwargs


# =============================================================================
# Setup Functions
# =============================================================================

def setup_logging(
    verbose: bool = False,
    log_file: Path | None = None,
    enable_json_log: bool = True
) -> None:
    """
    Configure logging for the application.
    
    Args:
        verbose: Enable DEBUG level logging
        log_file: Custom log file path
        enable_json_log: Enable JSONL log file for telemetry
    """
    # Create log directory
    LOG_DIR.mkdir(parents=True, exist_ok=True)
    
    # Determine log level
    level = logging.DEBUG if verbose else logging.INFO
    
    # Get root logger for grey_gpu_optimizer
    root_logger = logging.getLogger("grey_gpu_optimizer")
    root_logger.setLevel(level)
    
    # Clear existing handlers
    root_logger.handlers = []
    
    # Console handler with human-readable format
    console_handler = logging.StreamHandler(sys.stdout)
    console_handler.setLevel(level)
    console_formatter = ConsoleFormatter(
        "%(asctime)s [%(levelname)s] %(name)s: %(message)s",
        datefmt="%H:%M:%S"
    )
    console_handler.setFormatter(console_formatter)
    root_logger.addHandler(console_handler)
    
    # File handler with rotation
    file_path = log_file or DEFAULT_LOG_FILE
    file_handler = RotatingFileHandler(
        file_path,
        maxBytes=MAX_LOG_SIZE,
        backupCount=MAX_LOG_BACKUPS
    )
    file_handler.setLevel(logging.DEBUG)  # Always log DEBUG to file
    file_formatter = logging.Formatter(
        "%(asctime)s [%(levelname)s] %(name)s.%(funcName)s:%(lineno)d - %(message)s"
    )
    file_handler.setFormatter(file_formatter)
    root_logger.addHandler(file_handler)
    
    # JSON log file for telemetry
    if enable_json_log:
        json_handler = RotatingFileHandler(
            JSON_LOG_FILE,
            maxBytes=MAX_LOG_SIZE,
            backupCount=MAX_LOG_BACKUPS
        )
        json_handler.setLevel(logging.INFO)
        json_handler.setFormatter(JSONFormatter())
        
        # Add to a separate "telemetry" logger
        telemetry_logger = logging.getLogger("grey_gpu_optimizer.telemetry")
        telemetry_logger.setLevel(logging.INFO)
        telemetry_logger.addHandler(json_handler)
        telemetry_logger.propagate = False


def get_logger(name: str = "grey_gpu_optimizer") -> logging.Logger:
    """Get a logger instance."""
    return logging.getLogger(name)


def get_json_logger(extra: dict[str, Any] | None = None) -> logging.Logger:
    """
    Get a logger configured for JSON telemetry output.
    
    Args:
        extra: Extra context to include in all log entries
    
    Returns:
        Logger instance configured for JSON output
    """
    logger = logging.getLogger("grey_gpu_optimizer.telemetry")
    
    # Ensure the telemetry logger is set up
    if not logger.handlers:
        LOG_DIR.mkdir(parents=True, exist_ok=True)
        
        json_handler = RotatingFileHandler(
            JSON_LOG_FILE,
            maxBytes=MAX_LOG_SIZE,
            backupCount=MAX_LOG_BACKUPS
        )
        json_handler.setLevel(logging.INFO)
        json_handler.setFormatter(JSONFormatter())
        logger.addHandler(json_handler)
        logger.setLevel(logging.INFO)
        logger.propagate = False
    
    if extra:
        return JSONLoggerAdapter(logger, extra)  # type: ignore
    
    return logger


def log_event(
    event_type: str,
    data: dict[str, Any],
    logger: logging.Logger | None = None
) -> None:
    """
    Log a structured event.
    
    This is a convenience function for logging structured events
    in JSON format.
    
    Args:
        event_type: Type of event (e.g., "gpu_detected", "plan_applied")
        data: Event data dictionary
        logger: Optional logger instance
    
    Example:
        >>> log_event("gpu_detected", {
        ...     "vendor": "nvidia",
        ...     "model": "RTX 4090",
        ...     "vram_mb": 24576
        ... })
    """
    if logger is None:
        logger = get_json_logger()
    
    event = {
        "event_type": event_type,
        "timestamp": datetime.now(timezone.utc).isoformat(),
        **data
    }
    
    logger.info(json.dumps(event, default=str))


# =============================================================================
# Utility Functions
# =============================================================================

def get_log_files() -> list[Path]:
    """Get list of log files."""
    if not LOG_DIR.exists():
        return []
    
    return sorted(
        f for f in LOG_DIR.iterdir()
        if f.is_file() and f.suffix in (".log", ".jsonl")
    )


def tail_log(
    log_file: Path | None = None,
    lines: int = 50
) -> list[str]:
    """
    Read the last N lines from a log file.
    
    Args:
        log_file: Path to log file (defaults to main log)
        lines: Number of lines to read
    
    Returns:
        List of log lines
    """
    file_path = log_file or DEFAULT_LOG_FILE
    
    if not file_path.exists():
        return []
    
    with open(file_path) as f:
        all_lines = f.readlines()
    
    return all_lines[-lines:]


def parse_json_log(
    log_file: Path | None = None,
    event_type: str | None = None
) -> list[dict[str, Any]]:
    """
    Parse JSON log file and optionally filter by event type.
    
    Args:
        log_file: Path to JSONL log file
        event_type: Filter by event type (optional)
    
    Returns:
        List of parsed log entries
    """
    file_path = log_file or JSON_LOG_FILE
    
    if not file_path.exists():
        return []
    
    entries = []
    with open(file_path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
                if event_type is None or entry.get("event_type") == event_type:
                    entries.append(entry)
            except json.JSONDecodeError:
                continue
    
    return entries
