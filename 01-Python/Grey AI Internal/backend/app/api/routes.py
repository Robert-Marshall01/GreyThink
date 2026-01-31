"""API routes for the application."""

from typing import List

from fastapi import APIRouter, Depends, HTTPException, UploadFile, File, status
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select

from app.api.deps import get_db, get_file_service, get_parser_service, get_analysis_service
from app.models.schemas import (
    UploadResponse,
    UploadDetail,
    ParsedMetricsResponse,
    InsightsResponse,
    AnalyzeRequest,
)
from app.models.orm import Upload, ParsedMetrics, Insight
from app.services.file_service import FileService
from app.services.parser_service import ParserService
from app.services.analysis_service import AnalysisService

router = APIRouter()


@router.post("/upload", response_model=UploadResponse, status_code=status.HTTP_201_CREATED)
async def upload_file(
    file: UploadFile = File(...),
    db: AsyncSession = Depends(get_db),
    file_service: FileService = Depends(get_file_service),
    parser_service: ParserService = Depends(get_parser_service),
):
    """
    Upload a CSV or text file for analysis.
    
    - Validates file type and size
    - Stores file on disk
    - Parses and extracts initial metrics
    - Returns upload ID for further operations
    """
    # Validate and save file
    try:
        upload = await file_service.save_file(file, db)
    except ValueError as e:
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail=str(e))
    
    # Parse file and extract metrics
    try:
        metrics = await parser_service.parse_file(upload, db)
    except Exception as e:
        # Update upload status to failed
        upload.status = "parse_failed"
        await db.commit()
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail=f"Failed to parse file: {str(e)}"
        )
    
    return UploadResponse(
        id=upload.id,
        filename=upload.filename,
        file_type=upload.file_type,
        status=upload.status,
        row_count=metrics.row_count if metrics else None,
        column_count=metrics.column_count if metrics else None,
        message="File uploaded and parsed successfully",
    )


@router.get("/uploads", response_model=List[UploadResponse])
async def list_uploads(
    db: AsyncSession = Depends(get_db),
    skip: int = 0,
    limit: int = 50,
):
    """List all uploaded files with basic info."""
    result = await db.execute(
        select(Upload).order_by(Upload.created_at.desc()).offset(skip).limit(limit)
    )
    uploads = result.scalars().all()
    
    responses = []
    for upload in uploads:
        # Get metrics if available
        metrics_result = await db.execute(
            select(ParsedMetrics).where(ParsedMetrics.upload_id == upload.id)
        )
        metrics = metrics_result.scalar_one_or_none()
        
        responses.append(UploadResponse(
            id=upload.id,
            filename=upload.filename,
            file_type=upload.file_type,
            status=upload.status,
            row_count=metrics.row_count if metrics else None,
            column_count=metrics.column_count if metrics else None,
            created_at=upload.created_at,
        ))
    
    return responses


@router.get("/uploads/{upload_id}", response_model=UploadDetail)
async def get_upload(
    upload_id: int,
    db: AsyncSession = Depends(get_db),
):
    """Get detailed information about an upload including parsed metrics."""
    result = await db.execute(select(Upload).where(Upload.id == upload_id))
    upload = result.scalar_one_or_none()
    
    if not upload:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Upload not found")
    
    # Get parsed metrics
    metrics_result = await db.execute(
        select(ParsedMetrics).where(ParsedMetrics.upload_id == upload_id)
    )
    metrics = metrics_result.scalar_one_or_none()
    
    # Get insights if available
    insights_result = await db.execute(
        select(Insight).where(Insight.metrics_id == metrics.id if metrics else -1)
    )
    insight = insights_result.scalar_one_or_none()
    
    return UploadDetail(
        id=upload.id,
        filename=upload.filename,
        file_type=upload.file_type,
        file_size=upload.file_size,
        status=upload.status,
        created_at=upload.created_at,
        metrics=ParsedMetricsResponse(
            row_count=metrics.row_count,
            column_count=metrics.column_count,
            columns=metrics.columns_json,
            stats=metrics.stats_json,
            preview=metrics.preview_json,
        ) if metrics else None,
        has_insights=insight is not None,
    )


@router.post("/analyze/{upload_id}", response_model=InsightsResponse)
async def analyze_upload(
    upload_id: int,
    request: AnalyzeRequest = None,
    db: AsyncSession = Depends(get_db),
    analysis_service: AnalysisService = Depends(get_analysis_service),
):
    """
    Trigger AI-powered analysis on an uploaded file.
    
    - Sends parsed data to local Ollama model
    - Generates summary, insights, and recommendations
    - Stores results in database
    """
    # Get upload
    result = await db.execute(select(Upload).where(Upload.id == upload_id))
    upload = result.scalar_one_or_none()
    
    if not upload:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Upload not found")
    
    # Get parsed metrics
    metrics_result = await db.execute(
        select(ParsedMetrics).where(ParsedMetrics.upload_id == upload_id)
    )
    metrics = metrics_result.scalar_one_or_none()
    
    if not metrics:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="File has not been parsed yet"
        )
    
    # Run AI analysis
    try:
        custom_prompt = request.custom_prompt if request else None
        insight = await analysis_service.analyze(metrics, db, custom_prompt=custom_prompt)
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail=f"AI analysis failed: {str(e)}. Ensure Ollama is running."
        )
    
    # Update upload status
    upload.status = "analyzed"
    await db.commit()
    
    return InsightsResponse(
        id=insight.id,
        upload_id=upload_id,
        summary=insight.summary,
        insights=insight.insights_json,
        recommendations=insight.recommendations,
        model_used=insight.model_used,
        created_at=insight.created_at,
    )


@router.get("/insights/{upload_id}", response_model=InsightsResponse)
async def get_insights(
    upload_id: int,
    db: AsyncSession = Depends(get_db),
):
    """Get AI-generated insights for an upload."""
    # Get metrics first to find insight
    metrics_result = await db.execute(
        select(ParsedMetrics).where(ParsedMetrics.upload_id == upload_id)
    )
    metrics = metrics_result.scalar_one_or_none()
    
    if not metrics:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="No parsed metrics found for this upload"
        )
    
    # Get insight
    insight_result = await db.execute(
        select(Insight).where(Insight.metrics_id == metrics.id)
    )
    insight = insight_result.scalar_one_or_none()
    
    if not insight:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="No insights generated yet. Call /api/analyze/{id} first."
        )
    
    return InsightsResponse(
        id=insight.id,
        upload_id=upload_id,
        summary=insight.summary,
        insights=insight.insights_json,
        recommendations=insight.recommendations,
        model_used=insight.model_used,
        created_at=insight.created_at,
    )
