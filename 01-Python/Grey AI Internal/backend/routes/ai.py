"""
AI Routes
=========
Handles AI inference operations using local Ollama LLM.

Complete Flow:
1. Fetch upload and metrics from database
2. Build context prompt with data summary
3. Call Ollama for AI inference (with retry + fallback)
4. Parse and structure the AI response
5. Store insights in database (AIOutput model)
6. Return structured insights to frontend

Error Handling:
- Automatic retry on transient failures
- Fallback to smaller models if primary fails
- Clean JSON error responses for all failure modes

Endpoints:
    POST /api/ai/analyze/{upload_id}  - Trigger AI analysis
    POST /api/ai/infer                - Raw inference endpoint
    GET  /api/ai/status               - Check Ollama status
    GET  /api/ai/insights/{upload_id} - Get stored insights
"""

import logging
from datetime import datetime
from typing import Optional

from fastapi import APIRouter, HTTPException, status, Depends, Body
from fastapi.responses import JSONResponse
from sqlalchemy.ext.asyncio import AsyncSession

# Services
from services.ai_service import AIService, AIInferenceError
from services.parser import ParserService
from services.persistence import PersistenceService, RecordNotFoundError

# Models
from models.database import get_db_session
from models.schemas import (
    AIRequest,
    AIResponse,
    AnalyzeRequest,
    InsightsResponse
)


# -----------------------------------------------------------------------------
# Logging Configuration
# -----------------------------------------------------------------------------

logger = logging.getLogger(__name__)


# -----------------------------------------------------------------------------
# Router Configuration
# -----------------------------------------------------------------------------

router = APIRouter(tags=["ai"])

# Initialize AI service (singleton pattern for connection reuse)
ai_service = AIService()


# -----------------------------------------------------------------------------
# POST /api/ai/analyze/{upload_id} - Trigger AI Analysis
# -----------------------------------------------------------------------------

@router.post("/analyze/{upload_id}", response_model=InsightsResponse)
async def analyze_upload(
    upload_id: int,
    request: Optional[AnalyzeRequest] = Body(default=None),
    db: AsyncSession = Depends(get_db_session)
):
    """
    Trigger AI-powered analysis on an uploaded file.
    
    Complete Flow:
    --------------
    1. Fetch upload and metrics from database
    2. Build data summary for AI context
    3. Call Ollama with analysis prompt
    4. Parse structured response (summary, insights, recommendations)
    5. Store AIOutput in database
    6. Update upload status to "analyzed"
    7. Return structured insights
    
    Example Request:
    ----------------
    ```json
    {
        "custom_prompt": "Focus on sales trends and identify top performers"
    }
    ```
    
    Example Response:
    -----------------
    ```json
    {
        "id": 1,
        "upload_id": 1,
        "summary": "The sales data shows strong Q4 performance with a 15% increase...",
        "insights": [
            "Revenue peaked in December with $120K in sales",
            "North region outperformed others by 23%",
            "Product A showed consistent growth throughout the year"
        ],
        "recommendations": {
            "actions": [
                "Increase inventory for Product A in Q1",
                "Expand North region sales team",
                "Investigate West region performance drop"
            ],
            "priority": "high"
        },
        "model_used": "llama3",
        "created_at": "2026-01-30T10:35:00Z"
    }
    ```
    """
    persistence = PersistenceService(db)
    
    # -------------------------------------------------------------------------
    # Step 1: Fetch upload and metrics from database
    # -------------------------------------------------------------------------
    
    try:
        history = await persistence.get_upload_history(upload_id)
    except RecordNotFoundError:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Upload with id={upload_id} not found"
        )
    
    upload = history["upload"]
    metrics = history["metrics"]
    
    if not metrics:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Upload has no parsed metrics. File may not have been parsed."
        )
    
    # -------------------------------------------------------------------------
    # Step 2: Build data summary for AI context
    # -------------------------------------------------------------------------
    
    # Reconstruct ParsedMetrics object for summary generation
    from services.parser import ParsedMetrics, ColumnInfo
    
    columns = []
    if metrics.columns_json and "columns" in metrics.columns_json:
        for col in metrics.columns_json["columns"]:
            columns.append(ColumnInfo(
                name=col.get("name", ""),
                dtype=col.get("dtype", "unknown"),
                non_null_count=col.get("non_null_count", 0),
                null_count=col.get("null_count", 0)
            ))
    
    parsed_metrics = ParsedMetrics(
        row_count=metrics.row_count,
        column_count=metrics.column_count,
        columns=columns,
        stats=metrics.stats_json or {},
        preview=metrics.preview_json or []
    )
    
    # Generate data summary for AI
    parser = ParserService()
    data_summary = parser.get_data_summary(parsed_metrics)
    
    # -------------------------------------------------------------------------
    # Step 3: Call Ollama for AI analysis
    # -------------------------------------------------------------------------
    
    custom_prompt = request.custom_prompt if request else None
    
    try:
        # Update status to analyzing
        await persistence.update_upload_status(upload_id, "analyzing")
        
        logger.info(f"Starting AI analysis for upload_id={upload_id}")
        
        # Call AI service (includes retry and fallback logic)
        result = await ai_service.analyze_data(
            data_summary=data_summary,
            custom_prompt=custom_prompt
        )
        
        logger.info(f"AI analysis completed for upload_id={upload_id}: "
                    f"model={result.get('model_used')}, "
                    f"fallback={result.get('fallback_used', False)}")
        
    except AIInferenceError as e:
        # Structured AI error - return clean JSON response
        error_info = e.ai_error.to_dict()
        logger.error(f"AI analysis failed for upload_id={upload_id}: {error_info}")
        
        await persistence.update_upload_status(
            upload_id, 
            "failed", 
            f"AI inference failed: {error_info['details']}"
        )
        
        # Return structured error response
        return JSONResponse(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            content={
                "error": "AI inference failed",
                "details": error_info["details"],
                "model_attempted": error_info["model_attempted"],
                "fallback_attempted": error_info.get("fallback_attempted"),
                "retry_count": error_info["retry_count"],
                "suggestion": "Ensure Ollama is running and the model is available. "
                              "Try: ollama pull llama3 && ollama serve"
            }
        )
        
    except ConnectionError as e:
        logger.error(f"Ollama connection failed for upload_id={upload_id}: {e}")
        await persistence.update_upload_status(upload_id, "failed", str(e))
        
        return JSONResponse(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            content={
                "error": "AI inference failed",
                "details": str(e),
                "suggestion": "Ensure Ollama is running: ollama serve"
            }
        )
        
    except Exception as e:
        logger.exception(f"Unexpected error during AI analysis for upload_id={upload_id}")
        await persistence.update_upload_status(upload_id, "failed", str(e))
        
        return JSONResponse(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            content={
                "error": "AI inference failed",
                "details": f"Unexpected error: {type(e).__name__}: {str(e)}"
            }
        )
    
    # -------------------------------------------------------------------------
    # Step 4: Store AIOutput in database
    # -------------------------------------------------------------------------
    
    # Get actual model used (may differ from configured if fallback was used)
    model_used = result.get("model_used", ai_service.model)
    
    # Check if AI output already exists (re-analysis case)
    existing = await persistence.get_ai_output_by_upload_id(upload_id)
    
    if existing:
        # Update existing record
        await persistence.update_ai_output(
            upload_id=upload_id,
            ai_summary=result["summary"],
            ai_recommendations={"actions": result["recommendations"].split(". ") if result["recommendations"] else []},
            insights_json=result["insights"],
            model_used=model_used
        )
        ai_output = await persistence.get_ai_output_by_upload_id(upload_id)
    else:
        # Create new record
        ai_output = await persistence.create_ai_output(
            upload_id=upload_id,
            ai_summary=result["summary"],
            model_used=model_used,
            ai_recommendations={"actions": result["recommendations"].split(". ") if result["recommendations"] else []},
            insights_json=result["insights"]
        )
    
    # Update upload status
    await persistence.update_upload_status(upload_id, "analyzed")
    
    # -------------------------------------------------------------------------
    # Step 5: Return structured response
    # -------------------------------------------------------------------------
    
    return InsightsResponse(
        id=ai_output.id,
        upload_id=upload_id,
        ai_summary=ai_output.ai_summary,
        insights_json=ai_output.insights_json,
        ai_recommendations=ai_output.ai_recommendations,
        model_used=ai_output.model_used,
        created_at=ai_output.created_at
    )


# -----------------------------------------------------------------------------
# GET /api/ai/insights/{upload_id} - Get Stored Insights
# -----------------------------------------------------------------------------

@router.get("/insights/{upload_id}", response_model=InsightsResponse)
async def get_insights(
    upload_id: int,
    db: AsyncSession = Depends(get_db_session)
):
    """
    Retrieve previously generated insights for an upload.
    
    Returns cached AI analysis without re-running the model.
    Use POST /analyze/{upload_id} to trigger new analysis.
    """
    persistence = PersistenceService(db)
    
    ai_output = await persistence.get_ai_output_by_upload_id(upload_id)
    
    if not ai_output:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"No insights found for upload id={upload_id}. Run POST /api/ai/analyze/{upload_id} first."
        )
    
    return InsightsResponse(
        id=ai_output.id,
        upload_id=upload_id,
        ai_summary=ai_output.ai_summary,
        insights_json=ai_output.insights_json,
        ai_recommendations=ai_output.ai_recommendations,
        model_used=ai_output.model_used,
        created_at=ai_output.created_at
    )


# -----------------------------------------------------------------------------
# POST /api/ai/infer - Raw Inference
# -----------------------------------------------------------------------------

@router.post("/infer", response_model=AIResponse)
async def raw_inference(request: AIRequest):
    """
    Direct inference endpoint for custom prompts.
    
    Useful for ad-hoc queries not tied to a specific upload.
    Does not store results in database.
    Includes automatic retry and fallback to smaller models.
    
    Example Request:
    ----------------
    ```json
    {
        "prompt": "Explain the concept of data normalization",
        "system_prompt": "You are a helpful data science tutor"
    }
    ```
    
    Example Error Response:
    -----------------------
    ```json
    {
        "error": "AI inference failed",
        "details": "Model runner has unexpectedly stopped",
        "model_attempted": "llama3",
        "fallback_attempted": "mistral",
        "retry_count": 4
    }
    ```
    """
    logger.info(f"Raw inference request: prompt_length={len(request.prompt)}")
    
    # Use run_llm for retry and fallback support
    result = await ai_service.run_llm(
        prompt=request.prompt,
        system_prompt=request.system_prompt
    )
    
    if result["success"]:
        return AIResponse(
            response=result["response"],
            model=result["model_used"],
            prompt_tokens=None,
            completion_tokens=None
        )
    else:
        # Return structured error response
        error_info = result["error"]
        return JSONResponse(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            content={
                "error": "AI inference failed",
                "details": error_info["details"],
                "model_attempted": error_info["model_attempted"],
                "fallback_attempted": error_info.get("fallback_attempted"),
                "retry_count": error_info["retry_count"]
            }
        )


# -----------------------------------------------------------------------------
# GET /api/ai/status - Check AI Service Status
# -----------------------------------------------------------------------------

@router.get("/status")
async def ai_status():
    """
    Check if AI service (Ollama) is available and responding.
    
    Example Response:
    -----------------
    ```json
    {
        "available": true,
        "configured_model": "llama3",
        "ollama_url": "http://localhost:11434",
        "available_models": ["llama3", "mistral", "phi3"]
    }
    ```
    """
    is_available = await ai_service.health_check()
    available_models = await ai_service.list_models() if is_available else []
    
    return {
        "available": is_available,
        "configured_model": ai_service.model,
        "ollama_url": ai_service.base_url,
        "available_models": available_models,
        "status": "healthy" if is_available else "unavailable"
    }


# -----------------------------------------------------------------------------
# POST /api/ai/analyze-organizations/{upload_id} - Industry-Specific Analysis
# -----------------------------------------------------------------------------

@router.post("/analyze-organizations/{upload_id}")
async def analyze_organizations(
    upload_id: int,
    db: AsyncSession = Depends(get_db_session)
):
    """
    Analyze organization data with industry-specific, adaptive recommendations.
    
    This endpoint transforms generic AI recommendations into structured,
    actionable insights tailored to each organization's profile.
    
    Features:
    ---------
    - Industry-specific recommendations (Apparel, Financial, Food, Tech, etc.)
    - Company maturity adaptation (older → modernization, newer → scaling)
    - Size-based strategy (small → agility, large → global expansion)
    - Structured JSON output for each organization
    
    Example Response:
    -----------------
    ```json
    {
        "organizations": [
            {
                "organization_name": "Nike Inc",
                "industry": "Apparel",
                "founded_year": 1964,
                "employee_count": 75400,
                "website_quality": "high",
                "recommendations": [
                    "Optimize e-commerce checkout flow with abandoned cart recovery",
                    "Invest in high-quality visual branding and lifestyle photography",
                    "Prioritize global brand consistency across markets",
                    "Implement operational efficiency through process automation"
                ]
            }
        ],
        "summary": "Analysis of 10 organizations across industries...",
        "total_analyzed": 10,
        "model_used": "llama3"
    }
    ```
    """
    persistence = PersistenceService(db)
    
    # Fetch upload and metrics
    try:
        history = await persistence.get_upload_history(upload_id)
    except RecordNotFoundError:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Upload with id={upload_id} not found"
        )
    
    upload = history["upload"]
    metrics = history["metrics"]
    
    if not metrics:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Upload has no parsed metrics"
        )
    
    # Extract column names and preview data
    columns = []
    if metrics.columns_json and "columns" in metrics.columns_json:
        columns = [col.get("name", "") for col in metrics.columns_json["columns"]]
    
    preview_data = metrics.preview_json or []
    
    if not preview_data:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No preview data available for organization analysis"
        )
    
    # Build data summary
    from services.parser import ParsedMetrics, ColumnInfo
    
    column_infos = []
    if metrics.columns_json and "columns" in metrics.columns_json:
        for col in metrics.columns_json["columns"]:
            column_infos.append(ColumnInfo(
                name=col.get("name", ""),
                dtype=col.get("dtype", "unknown"),
                non_null_count=col.get("non_null_count", 0),
                null_count=col.get("null_count", 0)
            ))
    
    parsed_metrics = ParsedMetrics(
        row_count=metrics.row_count,
        column_count=metrics.column_count,
        columns=column_infos,
        stats=metrics.stats_json or {},
        preview=preview_data
    )
    
    parser = ParserService()
    data_summary = parser.get_data_summary(parsed_metrics)
    
    logger.info(f"Starting organization analysis for upload_id={upload_id}")
    
    try:
        # Call the specialized organization analysis
        result = await ai_service.analyze_organizations(
            data_summary=data_summary,
            preview_data=preview_data,
            columns=columns
        )
        
        logger.info(f"Organization analysis completed: {result['total_analyzed']} orgs analyzed")
        
        return result
        
    except AIInferenceError as e:
        error_info = e.ai_error.to_dict()
        logger.error(f"Organization analysis failed: {error_info}")
        
        return JSONResponse(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            content={
                "error": "AI inference failed",
                "details": error_info["details"],
                "suggestion": "Ensure Ollama is running with a model available"
            }
        )
    except Exception as e:
        logger.exception(f"Unexpected error in organization analysis")
        return JSONResponse(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            content={
                "error": "Analysis failed",
                "details": str(e)
            }
        )


@router.post("/narrative-insights/{upload_id}")
async def narrative_insights(
    upload_id: int,
    db: AsyncSession = Depends(get_db_session)
):
    """
    Generate human-style narrative insights for uploaded data.
    
    Transforms analytical findings into consultative, actionable narratives
    that feel like advice from a senior strategist.
    """
    persistence = PersistenceService(db)
    
    # Fetch upload and metrics
    try:
        history = await persistence.get_upload_history(upload_id)
    except RecordNotFoundError:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Upload with id={upload_id} not found"
        )
    
    upload = history["upload"]
    metrics = history["metrics"]
    
    if not metrics:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Upload has no parsed metrics"
        )
    
    # Extract column names and preview data
    columns = []
    if metrics.columns_json and "columns" in metrics.columns_json:
        columns = [col.get("name", "") for col in metrics.columns_json["columns"]]
    
    preview_data = metrics.preview_json or []
    
    if not preview_data:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No preview data available for narrative analysis"
        )
    
    # Build data summary
    from services.parser import ParsedMetrics, ColumnInfo
    
    column_infos = []
    if metrics.columns_json and "columns" in metrics.columns_json:
        for col in metrics.columns_json["columns"]:
            column_infos.append(ColumnInfo(
                name=col.get("name", ""),
                dtype=col.get("dtype", "unknown"),
                non_null_count=col.get("non_null_count", 0),
                null_count=col.get("null_count", 0)
            ))
    
    parsed_metrics = ParsedMetrics(
        row_count=metrics.row_count,
        column_count=metrics.column_count,
        columns=column_infos,
        preview=preview_data
    )
    
    parser = ParserService()
    data_summary = parser.get_data_summary(parsed_metrics)
    
    try:
        # Generate narrative insights
        ai_service = AIService()
        insights = await ai_service.generate_narrative_insights(
            data_summary=data_summary,
            preview_data=preview_data,
            columns=columns,
            num_insights=4  # Generate 2-4 insights as per spec
        )
        
        return JSONResponse(
            status_code=status.HTTP_200_OK,
            content={
                "upload_id": upload_id,
                "narrative_insights": insights.get("insights", []),
                "recommendations": insights.get("recommendations", {}),
                "top_recommendations": insights.get("top_recommendations", []),
                "executive_summary": insights.get("executive_summary", ""),
                "narrative": insights.get("narrative", ""),
                "model_used": insights.get("model_used", "unknown"),
                "generated_at": datetime.utcnow().isoformat()
            }
        )
        
    except AIInferenceError as e:
        error_info = e.to_dict()
        logger.error(f"AI inference error in narrative insights: {error_info}")
        return JSONResponse(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            content={
                "error": "AI inference failed",
                "details": error_info["details"],
                "suggestion": "Ensure Ollama is running with a model available"
            }
        )
    except Exception as e:
        logger.exception(f"Unexpected error in narrative insights generation")
        return JSONResponse(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            content={
                "error": "Narrative generation failed",
                "details": str(e)
            }
        )
