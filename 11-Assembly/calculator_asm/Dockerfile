# =============================================================================
# Dockerfile for calculator_asm - x86 Assembly Calculator
# Multi-stage build for minimal production image
# Version: 0.2.0
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build stage - compile the assembly source
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS builder

# Install build dependencies for 32-bit x86 assembly
RUN apt-get update && apt-get install -y --no-install-recommends \
    nasm \
    binutils \
    libc6-dev-i386 \
    && rm -rf /var/lib/apt/lists/*

# Create build directory
WORKDIR /build

# Copy only the source file needed for build
COPY calculator_asm.asm .

# Assemble and link for Linux i386 (32-bit)
# Using elf32 format with int 0x80 syscalls and _start entry point
RUN nasm -f elf32 calculator_asm.asm -o calculator_asm.o && \
    ld -m elf_i386 calculator_asm.o -o calculator_asm && \
    chmod +x calculator_asm

# -----------------------------------------------------------------------------
# Stage 2: Runtime stage - minimal image with only the binary
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install 32-bit runtime support and create non-root user in single layer
RUN apt-get update && apt-get install -y --no-install-recommends \
    libc6-i386 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && groupadd --gid 1000 calculator \
    && useradd --uid 1000 --gid calculator --shell /bin/bash --create-home calculator \
    && mkdir -p /app \
    && chown -R calculator:calculator /app

# Set working directory
WORKDIR /app

# Copy the compiled binary from builder stage
COPY --from=builder --chown=calculator:calculator /build/calculator_asm /app/calculator_asm

# Switch to non-root user
USER calculator

# Set the entrypoint to the calculator binary
# Note: This is an interactive CLI application requiring TTY
ENTRYPOINT ["/app/calculator_asm"]

# Metadata labels
LABEL maintainer="GreyThink" \
      version="0.2.0" \
      description="x86 Assembly Calculator - Interactive CLI" \
      org.opencontainers.image.source="https://github.com/greythink/calculator_asm" \
      org.opencontainers.image.title="Calculator ASM" \
      org.opencontainers.image.version="0.2.0"
