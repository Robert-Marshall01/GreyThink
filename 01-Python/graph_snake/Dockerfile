# =============================================================================
# Multi-stage Dockerfile for graph-plotter
# Optimized for headless CLI mode (--nogui)
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build stage - install dependencies and build the package
# -----------------------------------------------------------------------------
FROM python:3.11-slim AS builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# Copy dependency files first for better layer caching
COPY requirements.txt pyproject.toml setup.cfg ./

# Create virtual environment and install dependencies
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir -r requirements.txt

# Copy source code and install the package
COPY graph_plotter.py ./
RUN pip install --no-cache-dir .

# -----------------------------------------------------------------------------
# Stage 2: Production stage - minimal runtime image
# -----------------------------------------------------------------------------
FROM python:3.11-slim AS production

# Labels for container metadata
LABEL maintainer="RobertCMarshall2007@outlook.com" \
      version="0.1.0" \
      description="Graph Plotter - CLI tool for generating plots" \
      org.opencontainers.image.source="https://github.com/Robert-Marshall01/GreyThink"

WORKDIR /app

# Install runtime dependencies for matplotlib (headless backend) and create non-root user
RUN apt-get update && apt-get install -y --no-install-recommends \
    libfreetype6 \
    libpng16-16 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && groupadd --gid 1000 appgroup \
    && useradd --uid 1000 --gid appgroup --shell /bin/bash --create-home appuser

# Copy virtual environment from builder stage
COPY --from=builder /opt/venv /opt/venv

# Copy application source
COPY --chown=appuser:appgroup graph_plotter.py ./
COPY --chown=appuser:appgroup LICENSE README.md ./

# Set environment variables
ENV PATH="/opt/venv/bin:$PATH" \
    PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    # Use non-interactive matplotlib backend for headless mode
    MPLBACKEND=Agg

# Create output directory with proper permissions
RUN mkdir -p /app/output && chown -R appuser:appgroup /app/output

# Switch to non-root user
USER appuser

# Set default output directory as volume mount point
VOLUME ["/app/output"]

# Health check (verify the CLI is working)
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD python -c "from graph_plotter import parse_numbers; parse_numbers('1 2 3')" || exit 1

# Default entrypoint - run graph-plotter in headless mode
ENTRYPOINT ["python", "graph_plotter.py", "--nogui"]

# Default command shows help
CMD ["--help"]
