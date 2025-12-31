# syntax=docker/dockerfile:1
# SPDX-License-Identifier: MIT
# Dockerfile for python_ai_2 - Flan-T5 Chatbot

# =============================================================================
# Stage 1: Builder - Install dependencies and download model
# =============================================================================
FROM python:3.11-slim AS builder

# Set environment variables
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements and install Python dependencies
COPY requirements.txt .
RUN pip install --user --no-warn-script-location -r requirements.txt

# =============================================================================
# Stage 2: Production - Minimal runtime image
# =============================================================================
FROM python:3.11-slim AS production

# Labels for container metadata
LABEL maintainer="GreyThink" \
      version="1.0" \
      description="Flan-T5 Chatbot - CLI AI assistant using google/flan-t5-base"

# Set environment variables
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PATH="/home/appuser/.local/bin:$PATH" \
    # Model configuration (can be overridden at runtime)
    MODEL_NAME="google/flan-t5-base" \
    MAX_LENGTH="128" \
    DEVICE="" \
    # Hugging Face cache directory
    HF_HOME="/app/.cache/huggingface" \
    TRANSFORMERS_CACHE="/app/.cache/huggingface/transformers"

# Create non-root user for security
RUN groupadd --gid 1000 appuser && \
    useradd --uid 1000 --gid 1000 --create-home --shell /bin/bash appuser

WORKDIR /app

# Copy Python packages from builder stage
COPY --from=builder /root/.local /home/appuser/.local

# Copy application source code
COPY --chown=appuser:appuser python_ai_2.py .
COPY --chown=appuser:appuser requirements.txt .

# Create cache directory for Hugging Face models
RUN mkdir -p /app/.cache/huggingface && \
    chown -R appuser:appuser /app/.cache

# Switch to non-root user
USER appuser

# Health check - verify Python and dependencies are working
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD python -c "import torch; from transformers import AutoTokenizer; print('OK')" || exit 1

# Default command - run in sample mode for container testing
# Override with: docker run <image> python python_ai_2.py --run-sample
CMD ["python", "python_ai_2.py", "--run-sample"]
