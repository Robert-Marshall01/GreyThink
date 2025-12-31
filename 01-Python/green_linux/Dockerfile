# syntax=docker/dockerfile:1

# =============================================================================
# PowerApp - Multi-stage Dockerfile
# =============================================================================
# This Dockerfile creates a production-ready container for the PowerApp CLI
# and testing environment. The GTK GUI requires X11/Wayland forwarding for
# interactive use.
#
# Build:   docker build -t powerapp:latest .
# Run CLI: docker run --rm powerapp:latest python -m powerapp.cli.power_reader --help
# Run Tests: docker run --rm powerapp:latest pytest
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Base image with system dependencies
# -----------------------------------------------------------------------------
FROM python:3.12-slim-bookworm AS base

# Prevent Python from writing pyc files and enable unbuffered output
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PYTHONFAULTHANDLER=1

# Install system dependencies for PyGObject/GTK4 and general utilities
RUN apt-get update && apt-get install -y --no-install-recommends \
    # GTK4 and GObject introspection
    libgirepository1.0-dev \
    gir1.2-gtk-4.0 \
    libgtk-4-1 \
    libcairo2-dev \
    pkg-config \
    # Required for pycairo wheel building
    libcairo2 \
    # Clean up apt cache
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# -----------------------------------------------------------------------------
# Stage 2: Builder - Install Python dependencies
# -----------------------------------------------------------------------------
FROM base AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gcc \
    python3-dev \
    && rm -rf /var/lib/apt/lists/*

# Create virtual environment
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Upgrade pip and install wheel
RUN pip install --no-cache-dir --upgrade pip setuptools wheel

# Copy dependency files first (better layer caching)
COPY requirements.txt requirements-dev.txt pyproject.toml ./

# Install production dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Install PyGObject (requires system libs from base stage)
RUN pip install --no-cache-dir PyGObject pycairo

# Install development dependencies (for testing in container)
RUN pip install --no-cache-dir -r requirements-dev.txt

# Install optional ML dependencies
RUN pip install --no-cache-dir \
    scikit-learn>=1.2 \
    onnx>=1.15 \
    onnxruntime>=1.15 \
    || echo "ML dependencies optional - continuing without them"

# -----------------------------------------------------------------------------
# Stage 3: Production runtime
# -----------------------------------------------------------------------------
FROM base AS production

# Create non-root user for security
RUN groupadd --gid 1000 powerapp \
    && useradd --uid 1000 --gid powerapp --shell /bin/bash --create-home powerapp

# Copy virtual environment from builder
COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Set working directory
WORKDIR /app

# Copy application code
COPY --chown=powerapp:powerapp . .

# Install the package and create XDG directories in a single layer
RUN pip install --no-cache-dir -e . \
    && mkdir -p /home/powerapp/.config/powerapp \
    && mkdir -p /home/powerapp/.local/state/powerapp \
    && chown -R powerapp:powerapp /home/powerapp

# Set XDG environment variables
ENV XDG_CONFIG_HOME=/home/powerapp/.config \
    XDG_STATE_HOME=/home/powerapp/.local/state \
    HOME=/home/powerapp

# Switch to non-root user
USER powerapp

# Health check - verify CLI module loads
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD python -c "from powerapp.cli import power_reader; print('OK')" || exit 1

# Default command: run the CLI power reader with mock data
CMD ["python", "-m", "powerapp.cli.power_reader", "--pretty"]

# -----------------------------------------------------------------------------
# Stage 4: Development/Testing image
# -----------------------------------------------------------------------------
FROM production AS development

USER root

# Install additional dev tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    vim-tiny \
    && rm -rf /var/lib/apt/lists/*

USER powerapp

# Override health check for dev
HEALTHCHECK NONE

# Default command for development: run tests
CMD ["pytest", "-v", "--tb=short"]

# -----------------------------------------------------------------------------
# Stage 5: GUI image (requires X11/Wayland forwarding)
# -----------------------------------------------------------------------------
FROM production AS gui

USER root

# Install additional X11/display dependencies for GUI
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Additional GUI dependencies
    adwaita-icon-theme \
    fonts-dejavu-core \
    libxkbcommon0 \
    # D-Bus for GTK
    dbus-x11 \
    && rm -rf /var/lib/apt/lists/*

USER powerapp

# For GUI, we need DISPLAY environment variable
# Run with: docker run -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix powerapp:gui
ENV DISPLAY=:0

# Default command for GUI
CMD ["python", "-m", "powerapp.gtk.main"]
