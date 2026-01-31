"""
Upload Routes
=============
Handles file upload operations for CSV and text files.

Complete Flow:
1. User uploads file → validate → save to disk
2. Parse file → extract metrics → save to DB (Metrics model)
3. Return structured response with metrics

Endpoints:
    POST   /api/upload/          - Upload new file
    GET    /api/upload/          - List all uploads
    GET    /api/upload/{id}      - Get upload details
    DELETE /api/upload/{id}      - Delete upload
"""

import os
import uuid
from pathlib import Path
from typing import List, Optional

from fastapi import APIRouter, File, UploadFile, HTTPException, status, Depends, Query
from sqlalchemy.ext.asyncio import AsyncSession

# Services
from services.parser import ParserService
from services.persistence import PersistenceService

# Models
from models.database import get_db_session
from models.schemas import (
    UploadResponse, 
    UploadDetail, 
    MetricsResponse,
    UploadHistory,
    UploadHistoryItem
)


# -----------------------------------------------------------------------------
# Router Configuration
# -----------------------------------------------------------------------------

router = APIRouter(tags=["upload"])

# Configuration
UPLOAD_DIR = Path(os.getenv("UPLOAD_DIR", "./uploads"))
MAX_FILE_SIZE_MB = int(os.getenv("MAX_FILE_SIZE_MB", "10"))
ALLOWED_EXTENSIONS = {".csv", ".txt"}

# Ensure upload directory exists
UPLOAD_DIR.mkdir(parents=True, exist_ok=True)


# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

def validate_file_extension(filename: str) -> str:
    """
    Validate that the file has an allowed extension.
    Returns the extension if valid, raises HTTPException otherwise.
    """
    ext = Path(filename).suffix.lower()
    if ext not in ALLOWED_EXTENSIONS:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"File type '{ext}' not allowed. Allowed: {list(ALLOWED_EXTENSIONS)}"
        )
    return ext


def generate_safe_filename(original_filename: str) -> str:
    """
    Generate a unique, filesystem-safe filename.
    Preserves original extension but adds UUID to prevent collisions.
    """
    ext = Path(original_filename).suffix.lower()
    stem = Path(original_filename).stem
    # Sanitize: keep only alphanumeric, dash, underscore
    safe_stem = "".join(c if c.isalnum() or c in "-_" else "_" for c in stem)[:50]
    unique_id = uuid.uuid4().hex[:8]
    return f"{safe_stem}_{unique_id}{ext}"


# -----------------------------------------------------------------------------
# POST /api/upload/ - Upload File
# -----------------------------------------------------------------------------

@router.post("/", response_model=UploadResponse, status_code=status.HTTP_201_CREATED)
async def upload_file(
    file: UploadFile = File(..., description="CSV or TXT file to upload"),
    db: AsyncSession = Depends(get_db_session)
):
    """
    Upload a CSV or text file for analysis.
    
    Complete Flow:
    --------------
    1. Validate file type (.csv, .txt) and size (max 10MB)
    2. Save file to disk with unique filename
    3. Parse file content and extract metrics
    4. Store Upload record in database
    5. Store Metrics record in database
    6. Return response with upload info and basic metrics
    
    Example Response:
    -----------------
    ```json
    {
        "id": 1,
        "filename": "sales_data.csv",
        "file_type": "csv",
        "file_size": 102400,
        "status": "parsed",
        "row_count": 1500,
        "column_count": 8,
        "created_at": "2026-01-30T10:30:00Z",
        "has_metrics": true,
        "has_insights": false,
        "message": "File uploaded and parsed successfully"
    }
    ```
    """
    # -------------------------------------------------------------------------
    # Step 1: Validate file
    # -------------------------------------------------------------------------
    
    # Check extension
    file_ext = validate_file_extension(file.filename)
    
    # Read content
    content = await file.read()
    file_size = len(content)
    
    # Validate size
    max_size_bytes = MAX_FILE_SIZE_MB * 1024 * 1024
    if file_size > max_size_bytes:
        raise HTTPException(
            status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE,
            detail=f"File size ({file_size / 1024 / 1024:.2f}MB) exceeds limit ({MAX_FILE_SIZE_MB}MB)"
        )
    
    # Validate not empty
    if file_size == 0:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="File is empty"
        )
    
    # -------------------------------------------------------------------------
    # Step 2: Save file to disk
    # -------------------------------------------------------------------------
    
    safe_filename = generate_safe_filename(file.filename)
    file_path = UPLOAD_DIR / safe_filename
    
    with open(file_path, "wb") as f:
        f.write(content)
    
    # -------------------------------------------------------------------------
    # Step 3: Parse file and extract metrics
    # -------------------------------------------------------------------------
    
    parser = ParserService()
    file_type = file_ext.lstrip(".")  # "csv" or "txt"
    
    try:
        metrics = parser.parse(
            content=content.decode("utf-8", errors="replace"),
            file_type=file_type
        )
    except Exception as e:
        # Clean up file on parse failure
        file_path.unlink(missing_ok=True)
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail=f"Failed to parse file: {str(e)}"
        )
    
    # -------------------------------------------------------------------------
    # Step 4: Store Upload record in database
    # -------------------------------------------------------------------------
    
    persistence = PersistenceService(db)
    
    try:
        upload = await persistence.create_upload(
            filename=file.filename,
            file_path=str(file_path),
            file_type=file_type,
            file_size=file_size,
            mime_type=file.content_type
        )
        
        # -------------------------------------------------------------------------
        # Step 5: Store Metrics record in database
        # -------------------------------------------------------------------------
        
        db_metrics = await persistence.create_metrics(
            upload_id=upload.id,
            row_count=metrics.row_count,
            column_count=metrics.column_count,
            parsed_data=metrics.to_dict(),
            columns_json={"columns": [c.__dict__ for c in metrics.columns]},
            stats_json=metrics.stats,
            preview_json=metrics.preview
        )
        
    except Exception as e:
        # Clean up file on database failure
        file_path.unlink(missing_ok=True)
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Failed to save to database: {str(e)}"
        )
    
    # -------------------------------------------------------------------------
    # Step 6: Return response
    # -------------------------------------------------------------------------
    
    return UploadResponse(
        id=upload.id,
        filename=file.filename,
        file_type=file_type,
        file_size=file_size,
        status=upload.status,
        row_count=metrics.row_count,
        column_count=metrics.column_count,
        created_at=upload.created_at,
        has_metrics=True,
        has_insights=False,
        message="File uploaded and parsed successfully"
    )


# -----------------------------------------------------------------------------
# GET /api/upload/ - List Uploads
# -----------------------------------------------------------------------------

@router.get("/", response_model=UploadHistory)
async def list_uploads(
    page: int = Query(1, ge=1, description="Page number"),
    page_size: int = Query(20, ge=1, le=100, description="Items per page"),
    status_filter: Optional[str] = Query(None, description="Filter by status"),
    db: AsyncSession = Depends(get_db_session)
):
    """
    List all uploaded files with pagination.
    
    Query Parameters:
    -----------------
    - page: Page number (default: 1)
    - page_size: Items per page (default: 20, max: 100)
    - status_filter: Optional status filter (uploaded, parsed, analyzed)
    
    Example Response:
    -----------------
    ```json
    {
        "items": [
            {
                "id": 1,
                "filename": "sales_data.csv",
                "file_type": "csv",
                "status": "analyzed",
                "row_count": 1500,
                "has_insights": true,
                "created_at": "2026-01-30T10:30:00Z"
            }
        ],
        "total": 42,
        "page": 1,
        "page_size": 20,
        "has_more": true
    }
    ```
    """
    persistence = PersistenceService(db)
    
    offset = (page - 1) * page_size
    uploads, total = await persistence.list_uploads(
        status=status_filter,
        limit=page_size,
        offset=offset
    )
    
    # Transform to response format
    items = [
        UploadHistoryItem(
            id=u.id,
            filename=u.filename,
            file_type=u.file_type,
            status=u.status,
            row_count=u.metrics.row_count if u.metrics else None,
            has_insights=u.ai_output is not None,
            created_at=u.created_at
        )
        for u in uploads
    ]
    
    return UploadHistory(
        items=items,
        total=total,
        page=page,
        page_size=page_size,
        has_more=(page * page_size) < total
    )


# -----------------------------------------------------------------------------
# GET /api/upload/{upload_id} - Get Upload Details
# -----------------------------------------------------------------------------

@router.get("/{upload_id}", response_model=UploadDetail)
async def get_upload(
    upload_id: int,
    db: AsyncSession = Depends(get_db_session)
):
    """
    Get detailed information about a specific upload.
    Includes full metrics and analysis status.
    
    Example Response:
    -----------------
    ```json
    {
        "id": 1,
        "filename": "sales_data.csv",
        "file_type": "csv",
        "file_size": 102400,
        "status": "analyzed",
        "row_count": 1500,
        "column_count": 8,
        "created_at": "2026-01-30T10:30:00Z",
        "has_metrics": true,
        "has_insights": true,
        "metrics": {
            "id": 1,
            "upload_id": 1,
            "row_count": 1500,
            "column_count": 8,
            "columns": [...],
            "stats": {...},
            "preview": [...]
        }
    }
    ```
    """
    persistence = PersistenceService(db)
    
    upload = await persistence.get_upload_with_all_data(upload_id)
    
    if not upload:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Upload with id={upload_id} not found"
        )
    
    # Build metrics response if available
    metrics_response = None
    if upload.metrics:
        m = upload.metrics
        columns = None
        if m.columns_json and "columns" in m.columns_json:
            columns = m.columns_json["columns"]
        
        metrics_response = MetricsResponse(
            id=m.id,
            upload_id=m.upload_id,
            row_count=m.row_count,
            column_count=m.column_count,
            columns=columns,
            stats=m.stats_json,
            preview=m.preview_json,
            created_at=m.created_at
        )
    
    return UploadDetail(
        id=upload.id,
        filename=upload.filename,
        file_type=upload.file_type,
        file_size=upload.file_size,
        status=upload.status,
        row_count=upload.metrics.row_count if upload.metrics else None,
        column_count=upload.metrics.column_count if upload.metrics else None,
        created_at=upload.created_at,
        has_metrics=upload.metrics is not None,
        has_insights=upload.ai_output is not None,
        metrics=metrics_response,
        error_message=upload.error_message
    )


# -----------------------------------------------------------------------------
# DELETE /api/upload/{upload_id} - Delete Upload
# -----------------------------------------------------------------------------

@router.delete("/{upload_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_upload(
    upload_id: int,
    db: AsyncSession = Depends(get_db_session)
):
    """
    Delete an upload and its associated file and records.
    
    Deletes:
    - Upload record (CASCADE deletes Metrics and AIOutput)
    - Physical file from disk
    """
    persistence = PersistenceService(db)
    
    # Get upload to find file path
    upload = await persistence.get_upload_by_id(upload_id)
    
    if not upload:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Upload with id={upload_id} not found"
        )
    
    # Delete file from disk
    file_path = Path(upload.file_path)
    file_path.unlink(missing_ok=True)
    
    # Delete from database (CASCADE handles related records)
    await persistence.delete_upload(upload_id)
