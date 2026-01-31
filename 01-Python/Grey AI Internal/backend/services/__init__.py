"""
Services Package
================
Contains business logic services that are called by routes.

Services Structure:
    - parser.py: File parsing (CSV, text)
    - ai_service.py: Ollama LLM integration
    - persistence.py: Database CRUD operations

Services are responsible for:
- Data processing and transformation
- External API calls (e.g., Ollama)
- Database operations (via persistence)
- Complex business logic

Services should NOT directly handle HTTP concerns (that's for routes).

Usage:
    from services.persistence import PersistenceService
    from services.parser import ParserService
    from services.ai_service import AIService
"""

# Export persistence service and convenience functions
from services.persistence import (
    PersistenceService,
    RecordNotFoundError,
    DuplicateRecordError,
    save_upload,
    save_metrics,
    save_ai_output,
    fetch_history,
)

# Export parser service
from services.parser import ParserService

# Export AI service
from services.ai_service import AIService

__all__ = [
    # Persistence
    "PersistenceService",
    "RecordNotFoundError", 
    "DuplicateRecordError",
    "save_upload",
    "save_metrics",
    "save_ai_output",
    "fetch_history",
    # Parser
    "ParserService",
    # AI
    "AIService",
]
