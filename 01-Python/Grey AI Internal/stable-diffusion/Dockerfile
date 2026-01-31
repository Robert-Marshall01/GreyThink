# Lightweight Stable Diffusion API
# =================================
# Simple REST API wrapper around Hugging Face diffusers
# Much smaller and more reliable than AUTOMATIC1111 WebUI

FROM python:3.10-slim

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install Python dependencies
RUN pip install --no-cache-dir \
    fastapi==0.109.0 \
    uvicorn[standard]==0.27.0 \
    diffusers==0.25.0 \
    transformers==4.36.0 \
    accelerate==0.25.0 \
    torch==2.1.2 \
    safetensors==0.4.1 \
    pillow==10.2.0 \
    python-multipart==0.0.9 \
    huggingface-hub==0.20.3 \
    numpy==1.26.4

# Copy API code
COPY sd_api.py .

# Pre-download a small model on build (optional - comment out to download at runtime)
# RUN python -c "from diffusers import StableDiffusionPipeline; StableDiffusionPipeline.from_pretrained('runwayml/stable-diffusion-v1-5', torch_dtype='float32')"

# Expose API port
EXPOSE 7860

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=5 \
    CMD curl -f http://localhost:7860/health || exit 1

# Run API server
CMD ["python", "sd_api.py"]
