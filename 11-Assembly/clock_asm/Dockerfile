# =============================================================================
# Dockerfile for clock_asm - Linux x86-64 Assembly Digital Clock
# Multi-stage build: Build with NASM, run as minimal static binary
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build stage - Compile assembly with NASM and link with ld
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    nasm \
    binutils \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy assembly source
COPY clock_asm.asm .

# Assemble and link
# -f elf64: Output ELF64 format for Linux x86-64
# ld with no dynamic linking flags produces a static binary
RUN nasm -f elf64 clock_asm.asm -o clock_asm.o \
    && ld clock_asm.o -o clock_asm \
    && chmod +x clock_asm

# -----------------------------------------------------------------------------
# Stage 2: Runtime stage - Minimal image with only the binary
# -----------------------------------------------------------------------------
FROM scratch

# Labels for container metadata
LABEL org.opencontainers.image.title="clock_asm" \
      org.opencontainers.image.description="Linux x86-64 digital clock using pure syscalls" \
      org.opencontainers.image.authors="Robert-Marshall01" \
      org.opencontainers.image.source="https://github.com/Robert-Marshall01/GreyThink" \
      org.opencontainers.image.licenses="MIT"

# Copy the static binary from builder stage
COPY --from=builder /build/clock_asm /clock_asm

# No ports to expose (console application)
# No environment variables required
# No volumes needed

# Run as the binary directly (no shell needed)
# Note: scratch image has no user management, binary runs as container user
ENTRYPOINT ["/clock_asm"]
