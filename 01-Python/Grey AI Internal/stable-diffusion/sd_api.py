"""
Lightweight Stable Diffusion API
================================
Simple FastAPI wrapper around Hugging Face diffusers.
Provides AUTOMATIC1111-compatible endpoints for text2img and img2img.

Designed to be much lighter and more reliable than the full WebUI.
"""

import os
import io
import base64
import json
import time
from datetime import datetime
from typing import Optional, List
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
import torch
from PIL import Image

# Configuration
MODEL_ID = os.getenv("SD_MODEL_ID", "runwayml/stable-diffusion-v1-5")
USE_CPU = os.getenv("USE_CPU", "true").lower() == "true"
DEVICE = "cpu" if USE_CPU else "cuda"
TORCH_DTYPE = torch.float32 if USE_CPU else torch.float16

# Global pipeline reference
pipeline = None
img2img_pipeline = None


def load_pipelines():
    """Load Stable Diffusion pipelines."""
    global pipeline, img2img_pipeline
    
    print(f"Loading Stable Diffusion model: {MODEL_ID}")
    print(f"Device: {DEVICE}, Dtype: {TORCH_DTYPE}")
    
    from diffusers import StableDiffusionPipeline, StableDiffusionImg2ImgPipeline
    
    # Load text2img pipeline
    pipeline = StableDiffusionPipeline.from_pretrained(
        MODEL_ID,
        torch_dtype=TORCH_DTYPE,
        safety_checker=None,  # Disable NSFW filter for speed
    ).to(DEVICE)
    
    # Enable memory optimizations for CPU
    if USE_CPU:
        pipeline.enable_attention_slicing()
    
    # Create img2img pipeline from components
    img2img_pipeline = StableDiffusionImg2ImgPipeline(
        vae=pipeline.vae,
        text_encoder=pipeline.text_encoder,
        tokenizer=pipeline.tokenizer,
        unet=pipeline.unet,
        scheduler=pipeline.scheduler,
        safety_checker=None,
        feature_extractor=None,
    ).to(DEVICE)
    
    if USE_CPU:
        img2img_pipeline.enable_attention_slicing()
    
    print("✅ Stable Diffusion loaded successfully")


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Load model on startup."""
    load_pipelines()
    yield
    print("Shutting down Stable Diffusion API")


app = FastAPI(
    title="Stable Diffusion API",
    description="Lightweight SD API compatible with AUTOMATIC1111",
    version="1.0.0",
    lifespan=lifespan,
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# =============================================================================
# Request/Response Models (AUTOMATIC1111 compatible)
# =============================================================================

class Txt2ImgRequest(BaseModel):
    prompt: str = ""
    negative_prompt: str = ""
    steps: int = Field(default=20, ge=1, le=100)
    cfg_scale: float = Field(default=7.0, ge=1.0, le=30.0)
    width: int = Field(default=512, ge=256, le=1024)
    height: int = Field(default=512, ge=256, le=1024)
    seed: int = -1
    batch_size: int = 1
    n_iter: int = 1
    sampler_name: str = "euler"


class Img2ImgRequest(BaseModel):
    init_images: List[str] = []
    prompt: str = ""
    negative_prompt: str = ""
    steps: int = Field(default=20, ge=1, le=100)
    cfg_scale: float = Field(default=7.0, ge=1.0, le=30.0)
    denoising_strength: float = Field(default=0.75, ge=0.0, le=1.0)
    width: Optional[int] = None
    height: Optional[int] = None
    seed: int = -1
    batch_size: int = 1
    n_iter: int = 1
    sampler_name: str = "euler"


class GenerationResponse(BaseModel):
    images: List[str]
    info: str
    parameters: dict


# =============================================================================
# Helper Functions
# =============================================================================

def image_to_base64(image: Image.Image) -> str:
    """Convert PIL Image to base64 string."""
    buffer = io.BytesIO()
    image.save(buffer, format="PNG")
    return base64.b64encode(buffer.getvalue()).decode("utf-8")


def base64_to_image(b64_string: str) -> Image.Image:
    """Convert base64 string to PIL Image."""
    # Handle data URL prefix
    if "," in b64_string:
        b64_string = b64_string.split(",")[1]
    image_data = base64.b64decode(b64_string)
    return Image.open(io.BytesIO(image_data)).convert("RGB")


def get_generator(seed: int) -> torch.Generator:
    """Get random generator with optional seed."""
    generator = torch.Generator(device=DEVICE)
    if seed == -1:
        seed = generator.seed()
    else:
        generator.manual_seed(seed)
    return generator, seed


# =============================================================================
# Endpoints
# =============================================================================

@app.get("/health")
async def health_check():
    """Health check endpoint."""
    return {"status": "healthy", "model": MODEL_ID, "device": DEVICE}


@app.get("/sdapi/v1/options")
async def get_options():
    """Get current options (AUTOMATIC1111 compatible)."""
    return {
        "sd_model_checkpoint": MODEL_ID,
        "sd_model_name": MODEL_ID.split("/")[-1],
    }


@app.get("/sdapi/v1/sd-models")
async def get_models():
    """Get available models (AUTOMATIC1111 compatible)."""
    return [
        {
            "title": MODEL_ID,
            "model_name": MODEL_ID.split("/")[-1],
            "hash": "abc123",
            "sha256": "abc123",
        }
    ]


@app.get("/sdapi/v1/samplers")
async def get_samplers():
    """Get available samplers (AUTOMATIC1111 compatible)."""
    return [
        {"name": "euler", "aliases": ["euler"]},
        {"name": "euler_a", "aliases": ["euler_ancestral"]},
        {"name": "ddim", "aliases": ["ddim"]},
        {"name": "pndm", "aliases": ["pndm"]},
    ]


@app.post("/sdapi/v1/txt2img", response_model=GenerationResponse)
async def txt2img(request: Txt2ImgRequest):
    """Generate image from text prompt."""
    if pipeline is None:
        raise HTTPException(status_code=503, detail="Model not loaded yet")
    
    try:
        # Prepare generator
        generator, actual_seed = get_generator(request.seed)
        
        # Ensure dimensions are multiples of 8
        width = (request.width // 8) * 8
        height = (request.height // 8) * 8
        
        start_time = time.time()
        
        # Generate image
        result = pipeline(
            prompt=request.prompt,
            negative_prompt=request.negative_prompt or None,
            num_inference_steps=request.steps,
            guidance_scale=request.cfg_scale,
            width=width,
            height=height,
            generator=generator,
        )
        
        generation_time = time.time() - start_time
        
        # Convert result to base64
        images = [image_to_base64(img) for img in result.images]
        
        # Build info JSON (AUTOMATIC1111 format)
        info = {
            "prompt": request.prompt,
            "negative_prompt": request.negative_prompt,
            "steps": request.steps,
            "cfg_scale": request.cfg_scale,
            "width": width,
            "height": height,
            "seed": actual_seed,
            "sampler": request.sampler_name,
            "sd_model_name": MODEL_ID.split("/")[-1],
            "generation_time": generation_time,
        }
        
        return GenerationResponse(
            images=images,
            info=json.dumps(info),
            parameters=request.dict(),
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/sdapi/v1/img2img", response_model=GenerationResponse)
async def img2img(request: Img2ImgRequest):
    """Transform an existing image."""
    if img2img_pipeline is None:
        raise HTTPException(status_code=503, detail="Model not loaded yet")
    
    if not request.init_images:
        raise HTTPException(status_code=400, detail="No init_images provided")
    
    try:
        # Load init image
        init_image = base64_to_image(request.init_images[0])
        
        # Resize if dimensions specified
        if request.width and request.height:
            width = (request.width // 8) * 8
            height = (request.height // 8) * 8
            init_image = init_image.resize((width, height), Image.LANCZOS)
        else:
            # Ensure dimensions are multiples of 8
            w, h = init_image.size
            width = (w // 8) * 8
            height = (h // 8) * 8
            if width != w or height != h:
                init_image = init_image.resize((width, height), Image.LANCZOS)
        
        # Prepare generator
        generator, actual_seed = get_generator(request.seed)
        
        start_time = time.time()
        
        # Generate image
        result = img2img_pipeline(
            prompt=request.prompt,
            negative_prompt=request.negative_prompt or None,
            image=init_image,
            num_inference_steps=request.steps,
            guidance_scale=request.cfg_scale,
            strength=request.denoising_strength,
            generator=generator,
        )
        
        generation_time = time.time() - start_time
        
        # Convert result to base64
        images = [image_to_base64(img) for img in result.images]
        
        # Build info JSON
        info = {
            "prompt": request.prompt,
            "negative_prompt": request.negative_prompt,
            "steps": request.steps,
            "cfg_scale": request.cfg_scale,
            "width": width,
            "height": height,
            "seed": actual_seed,
            "denoising_strength": request.denoising_strength,
            "sampler": request.sampler_name,
            "sd_model_name": MODEL_ID.split("/")[-1],
            "generation_time": generation_time,
        }
        
        return GenerationResponse(
            images=images,
            info=json.dumps(info),
            parameters=request.dict(),
        )
        
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# Run Server
# =============================================================================

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=7860)
