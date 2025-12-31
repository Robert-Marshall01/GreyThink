# =============================================================================
# Fortran Calculus Demos - Production Dockerfile
# Multi-stage build for optimized image size
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Stage
# -----------------------------------------------------------------------------
FROM ubuntu:24.04 AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    gfortran \
    make \
    libgomp1 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy only source files needed for build
COPY Makefile fortran_calculus.f90 ./

# Build with optimizations and OpenMP support
RUN gfortran -fopenmp -std=f2008 -ffree-line-length-none -O2 -fdefault-real-8 \
    -o fortran_calculus.exe fortran_calculus.f90

# -----------------------------------------------------------------------------
# Stage 2: Runtime Stage (Fortran only - minimal)
# -----------------------------------------------------------------------------
FROM ubuntu:24.04 AS runtime-minimal

# Install only runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgfortran5 \
    libgomp1 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user for security
RUN groupadd --gid 1000 calculus \
    && useradd --uid 1000 --gid calculus --shell /bin/bash --create-home calculus

WORKDIR /app

# Copy compiled binary from builder
COPY --from=builder /build/fortran_calculus.exe ./

# Create output directories
RUN mkdir -p /app/CSVs /app/PNGs \
    && chown -R calculus:calculus /app

# Switch to non-root user
USER calculus

# Default command - run all demos
ENTRYPOINT ["./fortran_calculus.exe"]
CMD []

# -----------------------------------------------------------------------------
# Stage 3: Full Runtime (with Python visualization)
# -----------------------------------------------------------------------------
FROM ubuntu:24.04 AS runtime-full

# Install runtime dependencies + Python
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgfortran5 \
    libgomp1 \
    ca-certificates \
    python3 \
    python3-pip \
    python3-venv \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user for security
RUN groupadd --gid 1000 calculus \
    && useradd --uid 1000 --gid calculus --shell /bin/bash --create-home calculus

WORKDIR /app

# Copy compiled binary from builder
COPY --from=builder /build/fortran_calculus.exe ./

# Copy Python visualization scripts and requirements
COPY requirements.txt plot_demo.py plot_results.py ./
COPY --chown=calculus:calculus CSVs/ ./CSVs/

# Create virtual environment and install Python dependencies
RUN python3 -m venv /app/venv \
    && /app/venv/bin/pip install --no-cache-dir --upgrade pip \
    && /app/venv/bin/pip install --no-cache-dir -r requirements.txt

# Create output directories
RUN mkdir -p /app/PNGs \
    && chown -R calculus:calculus /app

# Add venv to PATH
ENV PATH="/app/venv/bin:$PATH"
ENV PYTHONUNBUFFERED=1

# Switch to non-root user
USER calculus

# Default command
ENTRYPOINT ["./fortran_calculus.exe"]
CMD []

# =============================================================================
# Build targets:
#   Minimal (Fortran only):  docker build --target runtime-minimal -t fortran-calculus:minimal .
#   Full (with Python):      docker build --target runtime-full -t fortran-calculus:full .
#   Default:                 docker build -t fortran-calculus .
# =============================================================================
