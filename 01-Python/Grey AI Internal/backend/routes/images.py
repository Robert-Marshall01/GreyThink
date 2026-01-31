"""
Image Generation API Routes
============================
FastAPI endpoints for AI-powered image generation using Stable Diffusion.

Endpoints:
- POST /api/images/generate       - Text-to-image generation
- POST /api/images/transform      - Image-to-image transformation
- GET  /api/images/status         - Check SD service status
- GET  /api/images/styles         - Get available styles
- GET  /api/images/presets        - Get quality presets
- GET  /api/images/generated/{path} - Serve generated images
"""

from fastapi import APIRouter, HTTPException, UploadFile, File, Form
from fastapi.responses import FileResponse
from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any
from pathlib import Path
import base64

from services.image_service import (
    generate_image_text2img,
    generate_image_img2img,
    check_sd_status,
    list_available_models,
    get_available_styles,
    get_quality_presets,
    ImageStyle,
    QualityPreset,
    ImageGenerationError,
    SDServiceUnavailable,
    GENERATED_IMAGES_DIR,
)

router = APIRouter(prefix="/api/images", tags=["images"])


# =============================================================================
# Request/Response Models
# =============================================================================

class Text2ImageRequest(BaseModel):
    """Request model for text-to-image generation."""
    prompt: str = Field(..., min_length=1, max_length=1000, description="Text description of the image")
    negative_prompt: Optional[str] = Field(None, max_length=500, description="Things to avoid")
    style: str = Field("photorealistic", description="Style preset")
    quality: str = Field("balanced", description="Quality preset (fast/balanced/quality)")
    width: int = Field(1024, ge=512, le=2048, description="Image width")
    height: int = Field(1024, ge=512, le=2048, description="Image height")
    seed: int = Field(-1, description="Random seed (-1 for random)")
    sampler: str = Field("DPM++ 2M Karras", description="Sampling method")


class Image2ImageRequest(BaseModel):
    """Request model for image-to-image transformation."""
    image_base64: str = Field(..., description="Base64 encoded source image")
    prompt: str = Field(..., min_length=1, max_length=1000, description="Transformation prompt")
    negative_prompt: Optional[str] = Field(None, max_length=500)
    style: str = Field("photorealistic")
    quality: str = Field("balanced")
    denoising_strength: float = Field(0.75, ge=0.1, le=1.0, description="How much to change (0.1-1.0)")
    width: Optional[int] = Field(None, ge=512, le=2048)
    height: Optional[int] = Field(None, ge=512, le=2048)
    seed: int = Field(-1)
    sampler: str = Field("DPM++ 2M Karras")


class ImageResponse(BaseModel):
    """Response model for generated images."""
    success: bool
    image_id: str
    image_url: str
    image_base64: str
    metadata: Dict[str, Any]
    domain: str = "Art"


class SDStatusResponse(BaseModel):
    """Response for SD service status."""
    available: bool
    current_model: Optional[str] = None
    models: List[str] = []
    samplers: List[str] = []
    error: Optional[str] = None


# =============================================================================
# Endpoints
# =============================================================================

@router.get("/status", response_model=SDStatusResponse)
async def get_sd_status():
    """
    Check Stable Diffusion service availability.
    
    Returns current model, available models, and samplers.
    """
    status = await check_sd_status()
    return SDStatusResponse(**status)


@router.get("/styles")
async def get_styles() -> List[Dict[str, str]]:
    """Get available style presets for image generation."""
    return get_available_styles()


@router.get("/presets")
async def get_presets() -> List[Dict[str, Any]]:
    """Get available quality presets (fast/balanced/quality)."""
    return get_quality_presets()


@router.get("/models")
async def get_models() -> List[Dict[str, str]]:
    """Get list of available Stable Diffusion models."""
    models = await list_available_models()
    if not models:
        return [{"name": "stable-diffusion-xl", "title": "Stable Diffusion XL (default)"}]
    return models


@router.post("/generate", response_model=ImageResponse)
async def generate_image(request: Text2ImageRequest):
    """
    Generate an image from a text prompt.
    
    Uses Stable Diffusion XL (or configured model) to create images.
    
    Example prompts:
    - "A serene mountain landscape at sunset with snow-capped peaks"
    - "Portrait of a cyberpunk warrior in neon city"
    - "Abstract geometric patterns in blue and gold"
    """
    try:
        # Parse style enum
        try:
            style = ImageStyle(request.style.lower())
        except ValueError:
            style = ImageStyle.PHOTOREALISTIC
        
        # Parse quality enum
        try:
            quality = QualityPreset(request.quality.lower())
        except ValueError:
            quality = QualityPreset.BALANCED
        
        result = await generate_image_text2img(
            prompt=request.prompt,
            negative_prompt=request.negative_prompt,
            style=style,
            quality=quality,
            width=request.width,
            height=request.height,
            seed=request.seed,
            sampler=request.sampler,
        )
        
        return ImageResponse(
            success=True,
            image_id=result["image_id"],
            image_url=result["image_url"],
            image_base64=result["image_base64"],
            metadata=result["metadata"],
            domain="Art",
        )
        
    except SDServiceUnavailable as e:
        raise HTTPException(
            status_code=503,
            detail={
                "error": "Stable Diffusion service unavailable",
                "message": str(e),
                "hint": "Start the stable-diffusion container: docker-compose up -d stable-diffusion",
            }
        )
    except ImageGenerationError as e:
        raise HTTPException(status_code=500, detail={"error": "Generation failed", "message": str(e)})
    except Exception as e:
        raise HTTPException(status_code=500, detail={"error": "Unexpected error", "message": str(e)})


@router.post("/transform", response_model=ImageResponse)
async def transform_image(request: Image2ImageRequest):
    """
    Transform an existing image using AI.
    
    Upload an image and provide a prompt describing the desired transformation.
    The denoising_strength controls how much the image changes (0.1 = subtle, 1.0 = major).
    """
    try:
        # Parse style enum
        try:
            style = ImageStyle(request.style.lower())
        except ValueError:
            style = ImageStyle.PHOTOREALISTIC
        
        # Parse quality enum
        try:
            quality = QualityPreset(request.quality.lower())
        except ValueError:
            quality = QualityPreset.BALANCED
        
        result = await generate_image_img2img(
            image_base64=request.image_base64,
            prompt=request.prompt,
            negative_prompt=request.negative_prompt,
            style=style,
            quality=quality,
            denoising_strength=request.denoising_strength,
            width=request.width,
            height=request.height,
            seed=request.seed,
            sampler=request.sampler,
        )
        
        return ImageResponse(
            success=True,
            image_id=result["image_id"],
            image_url=result["image_url"],
            image_base64=result["image_base64"],
            metadata=result["metadata"],
            domain="Art",
        )
        
    except SDServiceUnavailable as e:
        raise HTTPException(
            status_code=503,
            detail={
                "error": "Stable Diffusion service unavailable",
                "message": str(e),
                "hint": "Start the stable-diffusion container: docker-compose up -d stable-diffusion",
            }
        )
    except ImageGenerationError as e:
        raise HTTPException(status_code=500, detail={"error": "Transformation failed", "message": str(e)})
    except Exception as e:
        raise HTTPException(status_code=500, detail={"error": "Unexpected error", "message": str(e)})


@router.post("/upload-transform")
async def upload_and_transform(
    file: UploadFile = File(...),
    prompt: str = Form(...),
    negative_prompt: Optional[str] = Form(None),
    style: str = Form("photorealistic"),
    quality: str = Form("balanced"),
    denoising_strength: float = Form(0.75),
):
    """
    Upload an image file and transform it.
    
    Alternative to /transform that accepts multipart form data.
    """
    # Validate file type
    if not file.content_type or not file.content_type.startswith("image/"):
        raise HTTPException(status_code=400, detail="File must be an image")
    
    # Read and encode image
    content = await file.read()
    image_base64 = base64.b64encode(content).decode("utf-8")
    
    # Create request and delegate
    request = Image2ImageRequest(
        image_base64=image_base64,
        prompt=prompt,
        negative_prompt=negative_prompt,
        style=style,
        quality=quality,
        denoising_strength=denoising_strength,
    )
    
    return await transform_image(request)


@router.get("/generated/{filename}")
async def serve_generated_image(filename: str):
    """Serve a generated image file."""
    filepath = GENERATED_IMAGES_DIR / filename
    
    if not filepath.exists():
        raise HTTPException(status_code=404, detail="Image not found")
    
    return FileResponse(
        filepath,
        media_type="image/png",
        headers={"Cache-Control": "public, max-age=86400"},
    )
