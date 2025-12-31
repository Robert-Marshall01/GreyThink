# Dockerfile for teddy_blink_asm
# Multi-stage build for x86 32-bit Linux assembly program

# =============================================================================
# Stage 1: Build stage
# =============================================================================
FROM debian:bookworm-slim AS builder

# Install build dependencies for 32-bit assembly
RUN apt-get update && apt-get install -y --no-install-recommends \
    nasm \
    binutils \
    libc6-dev-i386 \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy assembly source file
COPY teddy_blink_asm.asm .

# Assemble and link the program (32-bit ELF)
RUN nasm -f elf teddy_blink_asm.asm -o teddy_blink_asm.o && \
    ld -m elf_i386 -o teddy_blink_asm teddy_blink_asm.o

# =============================================================================
# Stage 2: Runtime stage (minimal)
# =============================================================================
FROM debian:bookworm-slim AS runtime

# Install 32-bit runtime libraries and create non-root user
RUN apt-get update && apt-get install -y --no-install-recommends \
    libc6-i386 \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /var/cache/apt/* \
    && groupadd --gid 1000 teddyuser \
    && useradd --uid 1000 --gid 1000 --shell /bin/bash --create-home teddyuser

# Set working directory
WORKDIR /app

# Copy the compiled binary from builder stage
COPY --from=builder /build/teddy_blink_asm .

# Change ownership to non-root user
RUN chown teddyuser:teddyuser /app/teddy_blink_asm && \
    chmod +x /app/teddy_blink_asm

# Switch to non-root user
USER teddyuser

# Set terminal environment for ANSI escape codes
ENV TERM=xterm-256color

# Labels for metadata
LABEL maintainer="Robert-Marshall01" \
      description="Animated ASCII teddy bear with blinking eyes" \
      version="1.0.0"

# Run the teddy blink animation
CMD ["./teddy_blink_asm"]
