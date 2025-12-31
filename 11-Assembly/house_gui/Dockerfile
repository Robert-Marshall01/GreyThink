# =============================================================================
# Dockerfile for house_gui - x86-64 Assembly SDL2 GUI Application
# Multi-stage build for optimized image size
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Stage
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    nasm \
    gcc \
    libc6-dev \
    libsdl2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy source files
COPY house_gui.asm .

# Assemble and link
RUN nasm -f elf64 house_gui.asm -o house_gui.o \
    && gcc -no-pie house_gui.o -o house_gui -lSDL2 \
    && strip house_gui

# -----------------------------------------------------------------------------
# Stage 2: Runtime Stage
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies and create non-root user
RUN apt-get update && apt-get install -y --no-install-recommends \
    libsdl2-2.0-0 \
    libx11-6 \
    libxext6 \
    libxrender1 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && groupadd -r appgroup \
    && useradd -r -g appgroup -d /app -s /sbin/nologin appuser

# Set working directory
WORKDIR /app

# Copy built binary from builder stage
COPY --from=builder /build/house_gui .

# Change ownership to non-root user
RUN chown -R appuser:appgroup /app

# Switch to non-root user
USER appuser

# Set environment variables for SDL2 display
ENV SDL_VIDEODRIVER=x11
ENV DISPLAY=:0

# Labels for container metadata
LABEL maintainer="Robert-Marshall01" \
      version="0.1.0" \
      description="House GUI - NASM + SDL2 Assembly Demo" \
      org.opencontainers.image.source="house_gui.asm"

# Default command
CMD ["./house_gui"]
