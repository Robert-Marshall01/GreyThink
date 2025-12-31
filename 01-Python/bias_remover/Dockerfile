# syntax=docker/dockerfile:1

# ============================================
# Bias Remover - Production Dockerfile
# Python CLI Application
# ============================================

# Use slim Python image for minimal footprint
FROM python:3.11-slim-bookworm AS base

# Set Python environment variables
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONFAULTHANDLER=1

# Create non-root user for security
RUN groupadd --gid 1000 appgroup && \
    useradd --uid 1000 --gid appgroup --shell /bin/bash --create-home appuser

# Set working directory
WORKDIR /app

# Copy application code
COPY --chown=appuser:appgroup bias_remover.py .

# Switch to non-root user
USER appuser

# Set the entrypoint
ENTRYPOINT ["python", "bias_remover.py"]
