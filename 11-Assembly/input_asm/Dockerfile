# =============================================================================
# Dockerfile for input_asm - 32-bit x86 Assembly Application
# Multi-stage build for minimal production image
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Stage
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS builder

# Install build dependencies for 32-bit assembly
RUN apt-get update && apt-get install -y --no-install-recommends \
    nasm \
    binutils \
    libc6-dev-i386 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root build user
RUN useradd -m -s /bin/bash builder
WORKDIR /build

# Copy source files and set ownership
COPY input_asm.asm Makefile ./
RUN chown -R builder:builder /build

# Switch to non-root user for build
USER builder

# Assemble and link the 32-bit ELF binary
RUN nasm -f elf32 input_asm.asm -o input_asm.o && \
    ld -m elf_i386 input_asm.o -o input_asm && \
    rm -f input_asm.o

# -----------------------------------------------------------------------------
# Stage 2: Runtime Stage (Minimal)
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install minimal 32-bit runtime support and create non-root runtime user
RUN apt-get update && apt-get install -y --no-install-recommends \
    libc6-i386 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && groupadd -r appgroup \
    && useradd -r -g appgroup -d /app -s /sbin/nologin appuser

# Create application directory
WORKDIR /app

# Copy the compiled binary from builder stage
COPY --from=builder --chown=appuser:appgroup /build/input_asm ./input_asm

# Ensure binary is executable
RUN chmod +x /app/input_asm

# Switch to non-root user
USER appuser

# Health check - verify binary exists and is executable
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD test -x /app/input_asm || exit 1

# Set the entrypoint to the compiled binary
ENTRYPOINT ["/app/input_asm"]

# Labels for container metadata
LABEL maintainer="GreyThink" \
      version="0.2.0" \
      description="32-bit x86 Assembly application - number classifier" \
      org.opencontainers.image.source="https://github.com/greythink/input_asm" \
      org.opencontainers.image.title="input_asm" \
      org.opencontainers.image.description="Simple NASM 32-bit example that classifies numbers"
