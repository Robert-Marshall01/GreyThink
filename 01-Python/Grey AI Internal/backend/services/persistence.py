"""
Persistence Service
===================
Provides CRUD operations for database entities.
Abstracts away SQLAlchemy operations for cleaner route handlers.

Architecture:
    Routes → Persistence Service → Database Models
    
    This layer handles:
    - Query construction
    - Transaction management (via session)
    - Error handling for database operations
    - ORM → Dict/Schema conversion

Usage:
    from services.persistence import PersistenceService
    
    async def my_route(db: AsyncSession = Depends(get_db_session)):
        service = PersistenceService(db)
        upload = await service.create_upload(data)
        return upload

Design Decisions:
    - Instance-based service (takes session in constructor)
    - Returns ORM objects (let routes handle serialization)
    - Raises exceptions for routes to catch and handle
    - Supports optional filtering and pagination
"""

from datetime import datetime, timezone
from typing import Optional, List, Dict, Any, Tuple

from sqlalchemy import select, update, delete, func, and_, or_
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import selectinload

from models.database import Upload, Metrics, AIOutput, User


# =============================================================================
# Custom Exceptions
# =============================================================================

class RecordNotFoundError(Exception):
    """Raised when a requested record doesn't exist."""
    def __init__(self, model: str, id: int):
        self.model = model
        self.id = id
        super().__init__(f"{model} with id={id} not found")


class DuplicateRecordError(Exception):
    """Raised when trying to create a duplicate record."""
    pass


# =============================================================================
# Persistence Service
# =============================================================================

class PersistenceService:
    """
    Database persistence service providing CRUD operations.
    
    Each instance is bound to a single database session.
    Session management (commit/rollback) is handled by the caller or dependency.
    
    Attributes:
        session: The async database session
    
    Example:
        async with async_session_maker() as session:
            svc = PersistenceService(session)
            upload = await svc.create_upload(...)
            await session.commit()
    """
    
    def __init__(self, session: AsyncSession):
        """
        Initialize service with database session.
        
        Args:
            session: AsyncSession from SQLAlchemy async session maker
        """
        self.session = session
    
    # =========================================================================
    # User Operations (Optional - for session tracking)
    # =========================================================================
    
    async def create_user(
        self,
        username: str,
        email: str,
        display_name: Optional[str] = None
    ) -> User:
        """
        Create a new user.
        
        Args:
            username: Unique username
            email: Unique email address
            display_name: Optional display name
        
        Returns:
            Created User object
        
        Raises:
            DuplicateRecordError: If username or email already exists
        """
        user = User(
            username=username,
            email=email,
            display_name=display_name
        )
        self.session.add(user)
        await self.session.flush()  # Get the ID without committing
        return user
    
    async def get_user_by_id(self, user_id: int) -> Optional[User]:
        """Get user by ID."""
        result = await self.session.execute(
            select(User).where(User.id == user_id)
        )
        return result.scalar_one_or_none()
    
    async def get_user_by_username(self, username: str) -> Optional[User]:
        """Get user by username."""
        result = await self.session.execute(
            select(User).where(User.username == username)
        )
        return result.scalar_one_or_none()
    
    async def update_user_login(self, user_id: int) -> None:
        """Update user's last login timestamp."""
        await self.session.execute(
            update(User)
            .where(User.id == user_id)
            .values(last_login_at=datetime.now(timezone.utc))
        )
    
    # =========================================================================
    # Upload Operations
    # =========================================================================
    
    async def create_upload(
        self,
        filename: str,
        file_path: str,
        file_type: str,
        file_size: int,
        mime_type: Optional[str] = None,
        user_id: Optional[int] = None
    ) -> Upload:
        """
        Save a new upload record.
        
        This is the first step in the data pipeline:
        Upload → Metrics → AIOutput
        
        Args:
            filename: Original filename from client
            file_path: Path where file is stored
            file_type: File extension (csv, txt, json)
            file_size: Size in bytes
            mime_type: Optional MIME type
            user_id: Optional associated user
        
        Returns:
            Created Upload object with ID
        
        Example:
            upload = await svc.create_upload(
                filename="sales_2024.csv",
                file_path="/uploads/abc123.csv",
                file_type="csv",
                file_size=1024000
            )
            print(f"Created upload {upload.id}")
        """
        upload = Upload(
            filename=filename,
            file_path=file_path,
            file_type=file_type,
            file_size=file_size,
            mime_type=mime_type,
            user_id=user_id,
            status="uploaded"
        )
        self.session.add(upload)
        await self.session.flush()
        return upload
    
    async def get_upload_by_id(
        self, 
        upload_id: int,
        include_metrics: bool = False,
        include_ai_output: bool = False
    ) -> Optional[Upload]:
        """
        Get upload by ID with optional eager loading.
        
        Args:
            upload_id: The upload ID
            include_metrics: Whether to eagerly load metrics
            include_ai_output: Whether to eagerly load AI output
        
        Returns:
            Upload object or None if not found
        """
        query = select(Upload).where(Upload.id == upload_id)
        
        # Add eager loading if requested
        options = []
        if include_metrics:
            options.append(selectinload(Upload.metrics))
        if include_ai_output:
            options.append(selectinload(Upload.ai_output))
        
        if options:
            query = query.options(*options)
        
        result = await self.session.execute(query)
        return result.scalar_one_or_none()
    
    async def get_upload_with_all_data(self, upload_id: int) -> Optional[Upload]:
        """
        Get upload with all related data (metrics and AI output).
        
        Convenience method for fetching complete upload details.
        """
        return await self.get_upload_by_id(
            upload_id,
            include_metrics=True,
            include_ai_output=True
        )
    
    async def update_upload_status(
        self,
        upload_id: int,
        status: str,
        error_message: Optional[str] = None
    ) -> None:
        """
        Update upload processing status.
        
        Args:
            upload_id: The upload ID
            status: New status (uploaded, parsing, parsed, analyzing, analyzed, failed)
            error_message: Optional error message if status is 'failed'
        """
        values = {"status": status}
        if error_message is not None:
            values["error_message"] = error_message
        
        await self.session.execute(
            update(Upload)
            .where(Upload.id == upload_id)
            .values(**values)
        )
    
    async def list_uploads(
        self,
        user_id: Optional[int] = None,
        status: Optional[str] = None,
        limit: int = 50,
        offset: int = 0
    ) -> Tuple[List[Upload], int]:
        """
        List uploads with filtering and pagination.
        
        Args:
            user_id: Optional filter by user
            status: Optional filter by status
            limit: Maximum records to return (default 50)
            offset: Records to skip (for pagination)
        
        Returns:
            Tuple of (list of uploads, total count)
        """
        # Build filter conditions
        conditions = []
        if user_id is not None:
            conditions.append(Upload.user_id == user_id)
        if status is not None:
            conditions.append(Upload.status == status)
        
        where_clause = and_(*conditions) if conditions else True
        
        # Get total count
        count_query = select(func.count(Upload.id)).where(where_clause)
        total = (await self.session.execute(count_query)).scalar()
        
        # Get paginated results
        query = (
            select(Upload)
            .where(where_clause)
            .order_by(Upload.created_at.desc())
            .limit(limit)
            .offset(offset)
        )
        
        result = await self.session.execute(query)
        uploads = list(result.scalars().all())
        
        return uploads, total
    
    async def delete_upload(self, upload_id: int) -> bool:
        """
        Delete an upload and all related records.
        
        Due to CASCADE, this also deletes associated Metrics and AIOutput.
        
        Args:
            upload_id: The upload ID to delete
        
        Returns:
            True if deleted, False if not found
        """
        result = await self.session.execute(
            delete(Upload).where(Upload.id == upload_id)
        )
        return result.rowcount > 0
    
    # =========================================================================
    # Metrics Operations
    # =========================================================================
    
    async def create_metrics(
        self,
        upload_id: int,
        row_count: int,
        column_count: int = 0,
        parsed_data: Optional[Dict[str, Any]] = None,
        columns_json: Optional[Dict[str, Any]] = None,
        stats_json: Optional[Dict[str, Any]] = None,
        preview_json: Optional[List[Dict[str, Any]]] = None
    ) -> Metrics:
        """
        Save parsed metrics for an upload.
        
        Called after file parsing is complete.
        Updates upload status to 'parsed'.
        
        Args:
            upload_id: Associated upload ID
            row_count: Number of data rows
            column_count: Number of columns (0 for text files)
            parsed_data: Full parsed data structure (optional, can be large)
            columns_json: Column metadata (names, types, etc.)
            stats_json: Statistical summaries per column
            preview_json: First N rows for display
        
        Returns:
            Created Metrics object
        
        Example:
            metrics = await svc.create_metrics(
                upload_id=upload.id,
                row_count=1500,
                column_count=12,
                columns_json={...},
                preview_json=[{...}, {...}]
            )
        """
        metrics = Metrics(
            upload_id=upload_id,
            row_count=row_count,
            column_count=column_count,
            parsed_data=parsed_data,
            columns_json=columns_json,
            stats_json=stats_json,
            preview_json=preview_json
        )
        self.session.add(metrics)
        
        # Update upload status
        await self.update_upload_status(upload_id, "parsed")
        
        await self.session.flush()
        return metrics
    
    async def get_metrics_by_upload_id(self, upload_id: int) -> Optional[Metrics]:
        """
        Get metrics for an upload.
        
        Args:
            upload_id: The upload ID
        
        Returns:
            Metrics object or None
        """
        result = await self.session.execute(
            select(Metrics).where(Metrics.upload_id == upload_id)
        )
        return result.scalar_one_or_none()
    
    async def update_metrics(
        self,
        upload_id: int,
        **updates
    ) -> None:
        """
        Update metrics fields.
        
        Args:
            upload_id: The upload ID
            **updates: Fields to update
        """
        await self.session.execute(
            update(Metrics)
            .where(Metrics.upload_id == upload_id)
            .values(**updates)
        )
    
    # =========================================================================
    # AI Output Operations
    # =========================================================================
    
    async def create_ai_output(
        self,
        upload_id: int,
        ai_summary: str,
        model_used: str,
        ai_recommendations: Optional[Dict[str, Any]] = None,
        insights_json: Optional[List[str]] = None,
        prompt_tokens: Optional[int] = None,
        completion_tokens: Optional[int] = None
    ) -> AIOutput:
        """
        Save AI-generated analysis output.
        
        Called after Ollama analysis is complete.
        Updates upload status to 'analyzed'.
        
        Args:
            upload_id: Associated upload ID
            ai_summary: Executive summary text
            model_used: Name of LLM model (e.g., llama3, mistral)
            ai_recommendations: Structured recommendations (JSON)
            insights_json: List of insight strings
            prompt_tokens: Input token count (if available)
            completion_tokens: Output token count (if available)
        
        Returns:
            Created AIOutput object
        
        Example:
            ai_output = await svc.create_ai_output(
                upload_id=upload.id,
                ai_summary="The sales data shows a 15% increase...",
                model_used="llama3",
                insights_json=["Trend 1...", "Pattern 2..."],
                ai_recommendations={"priority": [...]}
            )
        """
        ai_output = AIOutput(
            upload_id=upload_id,
            ai_summary=ai_summary,
            ai_recommendations=ai_recommendations,
            insights_json=insights_json,
            model_used=model_used,
            prompt_tokens=prompt_tokens,
            completion_tokens=completion_tokens
        )
        self.session.add(ai_output)
        
        # Update upload status
        await self.update_upload_status(upload_id, "analyzed")
        
        await self.session.flush()
        return ai_output
    
    async def get_ai_output_by_upload_id(self, upload_id: int) -> Optional[AIOutput]:
        """
        Get AI output for an upload.
        
        Args:
            upload_id: The upload ID
        
        Returns:
            AIOutput object or None
        """
        result = await self.session.execute(
            select(AIOutput).where(AIOutput.upload_id == upload_id)
        )
        return result.scalar_one_or_none()
    
    async def update_ai_output(
        self,
        upload_id: int,
        **updates
    ) -> None:
        """
        Update AI output fields.
        
        Args:
            upload_id: The upload ID
            **updates: Fields to update
        """
        await self.session.execute(
            update(AIOutput)
            .where(AIOutput.upload_id == upload_id)
            .values(**updates)
        )
    
    # =========================================================================
    # History & Reporting Queries
    # =========================================================================
    
    async def get_upload_history(
        self,
        upload_id: int
    ) -> Dict[str, Any]:
        """
        Get complete history for an upload.
        
        Returns all data related to an upload including:
        - Upload metadata
        - Parsed metrics
        - AI insights
        
        Args:
            upload_id: The upload ID
        
        Returns:
            Dictionary with upload, metrics, and ai_output keys
        
        Raises:
            RecordNotFoundError: If upload doesn't exist
        """
        upload = await self.get_upload_with_all_data(upload_id)
        
        if not upload:
            raise RecordNotFoundError("Upload", upload_id)
        
        return {
            "upload": upload,
            "metrics": upload.metrics,
            "ai_output": upload.ai_output,
            "has_metrics": upload.metrics is not None,
            "has_ai_output": upload.ai_output is not None
        }
    
    async def get_recent_uploads(
        self,
        limit: int = 10,
        include_analyzed_only: bool = False
    ) -> List[Upload]:
        """
        Get most recent uploads.
        
        Args:
            limit: Maximum number of uploads
            include_analyzed_only: Only return fully analyzed uploads
        
        Returns:
            List of Upload objects
        """
        query = select(Upload).order_by(Upload.created_at.desc()).limit(limit)
        
        if include_analyzed_only:
            query = query.where(Upload.status == "analyzed")
        
        result = await self.session.execute(query)
        return list(result.scalars().all())
    
    async def get_upload_stats(self) -> Dict[str, Any]:
        """
        Get aggregate statistics about uploads.
        
        Returns:
            Dictionary with counts by status, total files, etc.
        """
        # Count by status
        status_query = (
            select(Upload.status, func.count(Upload.id))
            .group_by(Upload.status)
        )
        status_result = await self.session.execute(status_query)
        status_counts = dict(status_result.all())
        
        # Total file size
        size_query = select(func.sum(Upload.file_size))
        total_size = (await self.session.execute(size_query)).scalar() or 0
        
        # Total count
        total_query = select(func.count(Upload.id))
        total_count = (await self.session.execute(total_query)).scalar()
        
        return {
            "total_uploads": total_count,
            "total_size_bytes": total_size,
            "by_status": status_counts
        }


# =============================================================================
# Convenience Functions (for simple use cases)
# =============================================================================

async def save_upload(
    session: AsyncSession,
    filename: str,
    file_path: str,
    file_type: str,
    file_size: int,
    **kwargs
) -> Upload:
    """
    Convenience function to save an upload.
    
    For use without instantiating PersistenceService.
    """
    svc = PersistenceService(session)
    return await svc.create_upload(filename, file_path, file_type, file_size, **kwargs)


async def save_metrics(
    session: AsyncSession,
    upload_id: int,
    row_count: int,
    **kwargs
) -> Metrics:
    """
    Convenience function to save metrics.
    """
    svc = PersistenceService(session)
    return await svc.create_metrics(upload_id, row_count, **kwargs)


async def save_ai_output(
    session: AsyncSession,
    upload_id: int,
    ai_summary: str,
    model_used: str,
    **kwargs
) -> AIOutput:
    """
    Convenience function to save AI output.
    """
    svc = PersistenceService(session)
    return await svc.create_ai_output(upload_id, ai_summary, model_used, **kwargs)


async def fetch_history(
    session: AsyncSession,
    upload_id: int
) -> Dict[str, Any]:
    """
    Convenience function to fetch upload history.
    """
    svc = PersistenceService(session)
    return await svc.get_upload_history(upload_id)
