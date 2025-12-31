# Dockerfile for keypress_asm
# Multi-stage build for x86 Assembly (NASM/ELF32) application

# =============================================================================
# Stage 1: Build Stage
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

# Copy source file
COPY keypress_asm.asm .

# Assemble and link the program
RUN nasm -f elf32 keypress_asm.asm -o keypress_asm.o \
    && ld -m elf_i386 keypress_asm.o -o keypress_asm

# =============================================================================
# Stage 2: Runtime Stage
# =============================================================================
FROM debian:bookworm-slim AS runtime

# Install minimal 32-bit runtime support
RUN apt-get update && apt-get install -y --no-install-recommends \
    libc6-i386 \
    && rm -rf /var/lib/apt/lists/* \
    && rm -rf /tmp/* /var/tmp/*

# Create non-root user for security
RUN groupadd --gid 1000 appgroup \
    && useradd --uid 1000 --gid appgroup --shell /bin/false appuser

# Set working directory
WORKDIR /app

# Copy the compiled binary from builder stage
COPY --from=builder /build/keypress_asm .

# Change ownership to non-root user
RUN chown appuser:appgroup /app/keypress_asm \
    && chmod +x /app/keypress_asm

# Switch to non-root user
USER appuser

# Set entrypoint to the compiled binary
ENTRYPOINT ["./keypress_asm"]

# Labels for container metadata
LABEL maintainer="Robert-Marshall01" \
      description="Minimal Assembly keypress demonstration" \
      version="1.0.0" \
      org.opencontainers.image.source="https://github.com/Robert-Marshall01/keypress_asm"
