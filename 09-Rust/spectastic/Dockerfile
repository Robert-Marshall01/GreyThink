# syntax=docker/dockerfile:1

# =============================================================================
# Spectastic - Multi-stage Dockerfile for Rust GUI Application
# =============================================================================
# This Dockerfile builds the spectastic system inspector application.
# Note: This is a GUI application requiring display access (X11/Wayland).
# For containerized execution, X11 forwarding or virtual framebuffer is needed.
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Environment
# -----------------------------------------------------------------------------
FROM rust:1.83-bookworm AS builder

# Install build dependencies for GUI libraries (egui/eframe)
RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config \
    libx11-dev \
    libxcursor-dev \
    libxrandr-dev \
    libxi-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libxinerama-dev \
    libwayland-dev \
    libxkbcommon-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    && rm -rf /var/lib/apt/lists/*

# Create a non-root user for building
RUN useradd -m -u 1000 builder
WORKDIR /app

# Copy dependency manifests first for better layer caching
COPY --chown=builder:builder Cargo.toml Cargo.lock* ./

# Create a dummy main.rs to build dependencies (improves caching)
RUN mkdir -p src && \
    echo 'fn main() { println!("dummy"); }' > src/main.rs && \
    chown -R builder:builder .

USER builder

# Build dependencies only (cached layer)
RUN cargo build --release && \
    rm -rf src target/release/deps/spectastic* target/release/spectastic*

# Copy actual source code
COPY --chown=builder:builder src ./src

# Build the actual application
RUN cargo build --release && \
    strip target/release/spectastic

# -----------------------------------------------------------------------------
# Stage 2: Runtime Environment
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies for GUI and system info access
RUN apt-get update && apt-get install -y --no-install-recommends \
    libx11-6 \
    libxcursor1 \
    libxrandr2 \
    libxi6 \
    libgl1 \
    libglu1-mesa \
    libxinerama1 \
    libwayland-client0 \
    libxkbcommon0 \
    libfontconfig1 \
    libfreetype6 \
    # Required for sysinfo to work properly
    procps \
    # CA certificates for any potential network operations
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user for runtime
RUN groupadd -r spectastic && useradd -r -g spectastic -u 1000 spectastic

# Create app directory
WORKDIR /app

# Copy the built binary from builder stage
COPY --from=builder --chown=spectastic:spectastic /app/target/release/spectastic /app/spectastic

# Set proper permissions
RUN chmod +x /app/spectastic

# Switch to non-root user
USER spectastic

# Environment variables for GUI support
ENV DISPLAY=:0
ENV XDG_RUNTIME_DIR=/tmp/runtime-spectastic

# Labels for container metadata
LABEL org.opencontainers.image.title="Spectastic" \
      org.opencontainers.image.description="Minimal GUI system inspector showing CPU, memory, disks, network and hardware details" \
      org.opencontainers.image.version="0.1.0" \
      org.opencontainers.image.vendor="GreyThink" \
      org.opencontainers.image.source="https://github.com/GreyThink" \
      org.opencontainers.image.licenses="MIT"

# Health check (process running)
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD pgrep -x spectastic || exit 1

# Entry point
ENTRYPOINT ["/app/spectastic"]
