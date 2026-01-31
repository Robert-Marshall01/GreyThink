"""File upload and management service."""

import os
import uuid
from pathlib import Path
from typing import Optional

from fastapi import UploadFile
from sqlalchemy.ext.asyncio import AsyncSession

from app.config import get_settings
from app.models.orm import Upload


class FileService:
    """Handles file upload, validation, and storage."""
    
    def __init__(self):
        self.settings = get_settings()
    
    def _validate_file(self, file: UploadFile) -> None:
        """Validate file type and size."""
        # Check file extension
        if file.filename:
            ext = Path(file.filename).suffix.lower()
            if ext not in self.settings.allowed_extensions_list:
                raise ValueError(
                    f"File type '{ext}' not allowed. Allowed: {self.settings.allowed_extensions_list}"
                )
        else:
            raise ValueError("Filename is required")
    
    def _generate_safe_filename(self, original_filename: str) -> str:
        """Generate a unique, safe filename."""
        ext = Path(original_filename).suffix.lower()
        unique_id = uuid.uuid4().hex[:12]
        safe_name = "".join(c if c.isalnum() or c in "-_." else "_" for c in Path(original_filename).stem)
        return f"{safe_name}_{unique_id}{ext}"
    
    async def save_file(self, file: UploadFile, db: AsyncSession) -> Upload:
        """
        Validate and save uploaded file.
        
        Args:
            file: Uploaded file from request
            db: Database session
            
        Returns:
            Upload ORM object
            
        Raises:
            ValueError: If file validation fails
        """
        # Validate
        self._validate_file(file)
        
        # Read content and check size
        content = await file.read()
        file_size = len(content)
        
        if file_size > self.settings.max_file_size_bytes:
            raise ValueError(
                f"File size ({file_size / 1024 / 1024:.2f}MB) exceeds limit "
                f"({self.settings.max_file_size_mb}MB)"
            )
        
        if file_size == 0:
            raise ValueError("File is empty")
        
        # Generate safe filename and path
        safe_filename = self._generate_safe_filename(file.filename)
        file_path = self.settings.upload_path / safe_filename
        
        # Write file to disk
        with open(file_path, "wb") as f:
            f.write(content)
        
        # Determine file type
        ext = Path(file.filename).suffix.lower()
        file_type = ext.lstrip(".")
        
        # Create database record
        upload = Upload(
            filename=file.filename,
            file_path=str(file_path),
            file_type=file_type,
            file_size=file_size,
            status="uploaded",
        )
        
        db.add(upload)
        await db.commit()
        await db.refresh(upload)
        
        return upload
    
    async def get_file_content(self, upload: Upload) -> str:
        """Read file content from disk."""
        with open(upload.file_path, "r", encoding="utf-8", errors="replace") as f:
            return f.read()
    
    async def delete_file(self, upload: Upload, db: AsyncSession) -> None:
        """Delete file from disk and database."""
        # Remove from disk
        if os.path.exists(upload.file_path):
            os.remove(upload.file_path)
        
        # Remove from database
        await db.delete(upload)
        await db.commit()
