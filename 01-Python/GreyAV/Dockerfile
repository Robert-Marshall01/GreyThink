# ============================================================================
# GreyAV - Advanced Antivirus/EDR System
# Production-Grade Multi-Stage Dockerfile
# ============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Builder - Install dependencies and prepare the application
# -----------------------------------------------------------------------------
FROM python:3.12-slim-bookworm AS builder

# Set build-time labels
LABEL maintainer="GreyAV Team"
LABEL stage="builder"

# Prevent Python from writing bytecode and enable unbuffered output
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    libc-dev \
    libffi-dev \
    && rm -rf /var/lib/apt/lists/*

# Create virtual environment
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Upgrade pip and install optional dependencies
RUN pip install --upgrade pip setuptools wheel

# Install optional Python dependencies for enhanced functionality
# These are optional but recommended for full feature support
RUN pip install \
    psutil>=5.9.0 \
    watchdog>=3.0.0 \
    || echo "Optional dependencies installed (some may have failed)"

# -----------------------------------------------------------------------------
# Stage 2: Production - Minimal runtime image
# -----------------------------------------------------------------------------
FROM python:3.12-slim-bookworm AS production

# Metadata labels following OCI image spec
LABEL org.opencontainers.image.title="GreyAV" \
      org.opencontainers.image.description="Advanced Antivirus/EDR System" \
      org.opencontainers.image.version="1.0.0" \
      org.opencontainers.image.vendor="GreyAV Project" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://github.com/greyav/greyav"

# Environment configuration
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONFAULTHANDLER=1 \
    # Application configuration
    GREYAV_HOME=/app \
    GREYAV_DATA=/app/data \
    GREYAV_QUARANTINE=/app/quarantine \
    GREYAV_LOGS=/app/logs \
    # Timezone (configurable via env)
    TZ=UTC

# Install runtime dependencies, create user, and set up directories
# Combined into single RUN for smaller image layers
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Process utilities (for process scanning)
    procps \
    # Network utilities (for network scanning)
    iproute2 \
    net-tools \
    # File utilities
    file \
    # Security utilities
    libcap2-bin \
    # Timezone data
    tzdata \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    # Create non-root user for security
    # Note: AV operations may require elevated privileges in some scenarios
    && groupadd --gid 1000 greyav \
    && useradd --uid 1000 --gid greyav --shell /bin/bash --create-home greyav \
    # Create application directories
    && mkdir -p /app/data /app/quarantine /app/logs /app/reports \
    && chown -R greyav:greyav /app

# Copy virtual environment from builder
COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Set working directory
WORKDIR /app

# Copy application source code
# Copy Python modules
COPY --chown=greyav:greyav *.py ./

# Copy configuration files
COPY --chown=greyav:greyav signatures.json exclusions.json ./

# Copy optional JSON configs if they exist (using wildcard)
COPY --chown=greyav:greyav *.json ./

# Set restrictive permissions on quarantine directory
RUN chmod 700 /app/quarantine

# Create volume mount points for persistent data
VOLUME ["/app/data", "/app/quarantine", "/app/logs", "/app/reports"]

# Health check - verify the application can be imported
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD python -c "import greyav; print('GreyAV OK')" || exit 1

# Switch to non-root user (comment out if root access needed for scanning)
# USER greyav

# Default entrypoint - allows flexible command execution
ENTRYPOINT ["python", "greyav.py"]

# Default command - show help
CMD ["--help"]
