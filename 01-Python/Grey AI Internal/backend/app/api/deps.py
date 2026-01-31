"""Dependency injection for API routes."""

from typing import AsyncGenerator

from sqlalchemy.ext.asyncio import AsyncSession

from app.models.database import async_session_maker
from app.services.file_service import FileService
from app.services.parser_service import ParserService
from app.services.analysis_service import AnalysisService


async def get_db() -> AsyncGenerator[AsyncSession, None]:
    """Get database session."""
    async with async_session_maker() as session:
        try:
            yield session
        finally:
            await session.close()


async def get_file_service() -> FileService:
    """Get file service instance."""
    return FileService()


async def get_parser_service() -> ParserService:
    """Get parser service instance."""
    return ParserService()


async def get_analysis_service() -> AnalysisService:
    """Get analysis service instance."""
    return AnalysisService()
