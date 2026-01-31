"""
Image Generation Service
========================
Integrates with Stable Diffusion (AUTOMATIC1111 WebUI API) for:
- Text-to-image generation
- Image-to-image transformation

Supports multiple open-source models:
- Stable Diffusion XL (default)
- Kandinsky 3.0
- DeepFloyd IF
- PixArt

Uses the AUTOMATIC1111 stable-diffusion-webui API format.
"""

import os
import base64
import httpx
import uuid
import json
from datetime import datetime
from pathlib import Path
from typing import Optional, Dict, Any, List, Tuple
from dataclasses import dataclass, asdict
from enum import Enum

# Configuration
SD_BASE_URL = os.getenv("SD_BASE_URL", "http://stable-diffusion:7860")
SD_TIMEOUT = int(os.getenv("SD_TIMEOUT", "600"))  # 10 min timeout for CPU inference
GENERATED_IMAGES_DIR = Path("uploads/generated")


class ImageStyle(str, Enum):
    """Available image generation styles."""
    PHOTOREALISTIC = "photorealistic"
    ARTISTIC = "artistic"
    ABSTRACT = "abstract"
    ANIME = "anime"
    DIGITAL_ART = "digital_art"
    OIL_PAINTING = "oil_painting"
    WATERCOLOR = "watercolor"
    PENCIL_SKETCH = "pencil_sketch"


class QualityPreset(str, Enum):
    """Speed vs quality tradeoff presets."""
    FAST = "fast"           # ~10 steps, faster
    BALANCED = "balanced"   # ~25 steps, good balance
    QUALITY = "quality"     # ~50 steps, highest quality


@dataclass
class ImageMetadata:
    """Metadata for generated images."""
    id: str
    prompt: str
    negative_prompt: Optional[str]
    style: str
    quality_preset: str
    model: str
    width: int
    height: int
    steps: int
    cfg_scale: float
    seed: int
    sampler: str
    generated_at: str
    generation_time_ms: int
    filepath: str
    
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


# Style prompt modifiers
STYLE_MODIFIERS = {
    ImageStyle.PHOTOREALISTIC: "photorealistic, 8k uhd, high resolution, detailed, sharp focus, professional photography",
    ImageStyle.ARTISTIC: "artistic, creative, expressive brushstrokes, imaginative composition",
    ImageStyle.ABSTRACT: "abstract art, non-representational, geometric shapes, bold colors",
    ImageStyle.ANIME: "anime style, vibrant colors, detailed eyes, manga illustration",
    ImageStyle.DIGITAL_ART: "digital art, concept art, trending on artstation, highly detailed",
    ImageStyle.OIL_PAINTING: "oil painting, classical art style, visible brushstrokes, rich colors",
    ImageStyle.WATERCOLOR: "watercolor painting, soft edges, flowing colors, artistic",
    ImageStyle.PENCIL_SKETCH: "pencil sketch, black and white, detailed linework, shading",
}

# Quality preset configurations
QUALITY_CONFIGS = {
    QualityPreset.FAST: {"steps": 10, "cfg_scale": 7.0},
    QualityPreset.BALANCED: {"steps": 25, "cfg_scale": 7.5},
    QualityPreset.QUALITY: {"steps": 50, "cfg_scale": 8.0},
}

# Default negative prompt to avoid common issues
DEFAULT_NEGATIVE_PROMPT = (
    "blurry, low quality, distorted, deformed, ugly, bad anatomy, "
    "bad proportions, watermark, signature, text, logo, cropped, "
    "out of frame, worst quality, low resolution, jpeg artifacts"
)


class ImageGenerationError(Exception):
    """Custom exception for image generation failures."""
    pass


class SDServiceUnavailable(Exception):
    """Raised when Stable Diffusion service is not available."""
    pass


async def check_sd_status() -> Dict[str, Any]:
    """
    Check if Stable Diffusion service is available.
    
    Returns:
        Dict with status info including available models
    """
    try:
        async with httpx.AsyncClient(timeout=10.0) as client:
            # Check API availability
            response = await client.get(f"{SD_BASE_URL}/sdapi/v1/options")
            if response.status_code == 200:
                options = response.json()
                
                # Get available models
                models_response = await client.get(f"{SD_BASE_URL}/sdapi/v1/sd-models")
                models = []
                if models_response.status_code == 200:
                    models = [m.get("title", m.get("model_name")) for m in models_response.json()]
                
                # Get samplers
                samplers_response = await client.get(f"{SD_BASE_URL}/sdapi/v1/samplers")
                samplers = []
                if samplers_response.status_code == 200:
                    samplers = [s.get("name") for s in samplers_response.json()]
                
                return {
                    "available": True,
                    "current_model": options.get("sd_model_checkpoint", "unknown"),
                    "models": models[:10],  # Limit to first 10
                    "samplers": samplers[:10],
                    "base_url": SD_BASE_URL,
                }
            else:
                return {"available": False, "error": f"API returned {response.status_code}"}
    except httpx.ConnectError:
        return {"available": False, "error": "Cannot connect to Stable Diffusion service"}
    except httpx.TimeoutException:
        return {"available": False, "error": "Connection timeout"}
    except Exception as e:
        return {"available": False, "error": str(e)}


def _ensure_output_dir() -> Path:
    """Ensure the generated images directory exists."""
    GENERATED_IMAGES_DIR.mkdir(parents=True, exist_ok=True)
    return GENERATED_IMAGES_DIR


def _apply_style_to_prompt(prompt: str, style: ImageStyle) -> str:
    """Apply style modifiers to the prompt."""
    modifier = STYLE_MODIFIERS.get(style, "")
    if modifier:
        return f"{prompt}, {modifier}"
    return prompt


def _save_image(image_base64: str, image_id: str) -> Tuple[str, Path]:
    """
    Save base64 image to disk.
    
    Returns:
        Tuple of (relative_path, absolute_path)
    """
    output_dir = _ensure_output_dir()
    filename = f"{image_id}.png"
    filepath = output_dir / filename
    
    # Decode and save
    image_data = base64.b64decode(image_base64)
    with open(filepath, "wb") as f:
        f.write(image_data)
    
    return f"generated/{filename}", filepath


async def generate_image_text2img(
    prompt: str,
    negative_prompt: Optional[str] = None,
    style: ImageStyle = ImageStyle.PHOTOREALISTIC,
    quality: QualityPreset = QualityPreset.BALANCED,
    width: int = 1024,
    height: int = 1024,
    seed: int = -1,
    sampler: str = "DPM++ 2M Karras",
) -> Dict[str, Any]:
    """
    Generate an image from a text prompt using Stable Diffusion.
    
    Args:
        prompt: Text description of the image to generate
        negative_prompt: Things to avoid in the image
        style: Style preset to apply
        quality: Speed vs quality tradeoff
        width: Image width (should be multiple of 8)
        height: Image height (should be multiple of 8)
        seed: Random seed (-1 for random)
        sampler: Sampling method
    
    Returns:
        Dict with image path, metadata, and base64 preview
    """
    # Validate dimensions
    width = min(max(width, 512), 2048)
    height = min(max(height, 512), 2048)
    width = (width // 8) * 8
    height = (height // 8) * 8
    
    # Get quality config
    quality_config = QUALITY_CONFIGS.get(quality, QUALITY_CONFIGS[QualityPreset.BALANCED])
    
    # Apply style to prompt
    styled_prompt = _apply_style_to_prompt(prompt, style)
    
    # Use default negative if not provided
    final_negative = negative_prompt or DEFAULT_NEGATIVE_PROMPT
    
    # Build request payload (AUTOMATIC1111 API format)
    payload = {
        "prompt": styled_prompt,
        "negative_prompt": final_negative,
        "steps": quality_config["steps"],
        "cfg_scale": quality_config["cfg_scale"],
        "width": width,
        "height": height,
        "seed": seed,
        "sampler_name": sampler,
        "batch_size": 1,
        "n_iter": 1,
    }
    
    start_time = datetime.now()
    
    try:
        async with httpx.AsyncClient(timeout=SD_TIMEOUT) as client:
            response = await client.post(
                f"{SD_BASE_URL}/sdapi/v1/txt2img",
                json=payload,
            )
            
            if response.status_code != 200:
                error_detail = response.text[:200] if response.text else "Unknown error"
                raise ImageGenerationError(f"SD API error ({response.status_code}): {error_detail}")
            
            result = response.json()
            
    except httpx.ConnectError:
        raise SDServiceUnavailable("Stable Diffusion service is not running. Start the SD container.")
    except httpx.TimeoutException:
        raise ImageGenerationError(f"Image generation timed out after {SD_TIMEOUT}s")
    
    generation_time = int((datetime.now() - start_time).total_seconds() * 1000)
    
    # Extract image and info
    images = result.get("images", [])
    if not images:
        raise ImageGenerationError("No image returned from SD API")
    
    image_base64 = images[0]
    
    # Parse generation info
    info = {}
    try:
        info = json.loads(result.get("info", "{}"))
    except json.JSONDecodeError:
        pass
    
    # Generate unique ID and save image
    image_id = str(uuid.uuid4())[:8]
    relative_path, absolute_path = _save_image(image_base64, image_id)
    
    # Build metadata
    metadata = ImageMetadata(
        id=image_id,
        prompt=prompt,
        negative_prompt=negative_prompt,
        style=style.value,
        quality_preset=quality.value,
        model=info.get("sd_model_name", "stable-diffusion-xl"),
        width=width,
        height=height,
        steps=quality_config["steps"],
        cfg_scale=quality_config["cfg_scale"],
        seed=info.get("seed", seed),
        sampler=sampler,
        generated_at=datetime.now().isoformat(),
        generation_time_ms=generation_time,
        filepath=relative_path,
    )
    
    return {
        "success": True,
        "image_id": image_id,
        "image_url": f"/api/images/{relative_path}",
        "image_base64": image_base64,
        "metadata": metadata.to_dict(),
    }


async def generate_image_img2img(
    image_base64: str,
    prompt: str,
    negative_prompt: Optional[str] = None,
    style: ImageStyle = ImageStyle.PHOTOREALISTIC,
    quality: QualityPreset = QualityPreset.BALANCED,
    denoising_strength: float = 0.75,
    width: Optional[int] = None,
    height: Optional[int] = None,
    seed: int = -1,
    sampler: str = "DPM++ 2M Karras",
) -> Dict[str, Any]:
    """
    Transform an existing image using Stable Diffusion.
    
    Args:
        image_base64: Base64 encoded source image
        prompt: Text description of desired transformation
        negative_prompt: Things to avoid
        style: Style preset
        quality: Speed vs quality tradeoff
        denoising_strength: How much to change the image (0-1, higher = more change)
        width: Output width (None to preserve aspect ratio)
        height: Output height (None to preserve aspect ratio)
        seed: Random seed
        sampler: Sampling method
    
    Returns:
        Dict with transformed image path and metadata
    """
    # Get quality config
    quality_config = QUALITY_CONFIGS.get(quality, QUALITY_CONFIGS[QualityPreset.BALANCED])
    
    # Apply style to prompt
    styled_prompt = _apply_style_to_prompt(prompt, style)
    
    # Use default negative if not provided
    final_negative = negative_prompt or DEFAULT_NEGATIVE_PROMPT
    
    # Build request payload
    payload = {
        "init_images": [image_base64],
        "prompt": styled_prompt,
        "negative_prompt": final_negative,
        "denoising_strength": max(0.1, min(1.0, denoising_strength)),
        "steps": quality_config["steps"],
        "cfg_scale": quality_config["cfg_scale"],
        "seed": seed,
        "sampler_name": sampler,
        "batch_size": 1,
        "n_iter": 1,
    }
    
    # Add dimensions if specified
    if width:
        payload["width"] = (min(max(width, 512), 2048) // 8) * 8
    if height:
        payload["height"] = (min(max(height, 512), 2048) // 8) * 8
    
    start_time = datetime.now()
    
    try:
        async with httpx.AsyncClient(timeout=SD_TIMEOUT) as client:
            response = await client.post(
                f"{SD_BASE_URL}/sdapi/v1/img2img",
                json=payload,
            )
            
            if response.status_code != 200:
                error_detail = response.text[:200] if response.text else "Unknown error"
                raise ImageGenerationError(f"SD API error ({response.status_code}): {error_detail}")
            
            result = response.json()
            
    except httpx.ConnectError:
        raise SDServiceUnavailable("Stable Diffusion service is not running. Start the SD container.")
    except httpx.TimeoutException:
        raise ImageGenerationError(f"Image transformation timed out after {SD_TIMEOUT}s")
    
    generation_time = int((datetime.now() - start_time).total_seconds() * 1000)
    
    # Extract image
    images = result.get("images", [])
    if not images:
        raise ImageGenerationError("No image returned from SD API")
    
    output_base64 = images[0]
    
    # Parse generation info
    info = {}
    try:
        info = json.loads(result.get("info", "{}"))
    except json.JSONDecodeError:
        pass
    
    # Generate unique ID and save image
    image_id = str(uuid.uuid4())[:8]
    relative_path, absolute_path = _save_image(output_base64, image_id)
    
    # Build metadata
    metadata = ImageMetadata(
        id=image_id,
        prompt=prompt,
        negative_prompt=negative_prompt,
        style=style.value,
        quality_preset=quality.value,
        model=info.get("sd_model_name", "stable-diffusion-xl"),
        width=info.get("width", width or 1024),
        height=info.get("height", height or 1024),
        steps=quality_config["steps"],
        cfg_scale=quality_config["cfg_scale"],
        seed=info.get("seed", seed),
        sampler=sampler,
        generated_at=datetime.now().isoformat(),
        generation_time_ms=generation_time,
        filepath=relative_path,
    )
    
    return {
        "success": True,
        "image_id": image_id,
        "image_url": f"/api/images/{relative_path}",
        "image_base64": output_base64,
        "metadata": metadata.to_dict(),
        "mode": "img2img",
        "denoising_strength": denoising_strength,
    }


async def list_available_models() -> List[Dict[str, str]]:
    """Get list of available SD models."""
    try:
        async with httpx.AsyncClient(timeout=10.0) as client:
            response = await client.get(f"{SD_BASE_URL}/sdapi/v1/sd-models")
            if response.status_code == 200:
                return [
                    {"name": m.get("model_name", "unknown"), "title": m.get("title", "unknown")}
                    for m in response.json()
                ]
            return []
    except Exception:
        return []


def get_available_styles() -> List[Dict[str, str]]:
    """Get list of available style presets."""
    return [
        {"id": style.value, "name": style.value.replace("_", " ").title()}
        for style in ImageStyle
    ]


def get_quality_presets() -> List[Dict[str, Any]]:
    """Get list of quality presets with descriptions."""
    return [
        {
            "id": QualityPreset.FAST.value,
            "name": "Fast",
            "description": "Quick generation (~10 steps)",
            "steps": 10,
        },
        {
            "id": QualityPreset.BALANCED.value,
            "name": "Balanced",
            "description": "Good balance (~25 steps)",
            "steps": 25,
        },
        {
            "id": QualityPreset.QUALITY.value,
            "name": "Quality",
            "description": "Highest quality (~50 steps)",
            "steps": 50,
        },
    ]
