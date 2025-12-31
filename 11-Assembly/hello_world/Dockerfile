# =============================================================================
# Dockerfile for x86 Assembly Hello World
# Multi-stage build for minimal production image
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Stage
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS builder

# Install build tools
RUN apt-get update && apt-get install -y --no-install-recommends \
    nasm \
    binutils \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy source files
COPY hello_world.asm .

# Assemble and link the program
# Using 32-bit ELF format since the code uses int 0x80 syscalls
RUN nasm -f elf32 hello_world.asm -o hello_world.o \
    && ld -m elf_i386 hello_world.o -o hello_world

# -----------------------------------------------------------------------------
# Stage 2: Runtime Stage (minimal)
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install 32-bit runtime libraries and create non-root user
RUN apt-get update && apt-get install -y --no-install-recommends \
    libc6-i386 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && groupadd --gid 1000 appgroup \
    && useradd --uid 1000 --gid appgroup --shell /bin/false appuser

# Set working directory
WORKDIR /app

# Copy the compiled binary from builder stage
COPY --from=builder /build/hello_world .

# Change ownership to non-root user
RUN chown appuser:appgroup /app/hello_world \
    && chmod 755 /app/hello_world

# Switch to non-root user
USER appuser

# Set labels for container metadata
LABEL maintainer="GreyThink" \
      version="1.0" \
      description="x86 Assembly Hello World application" \
      org.opencontainers.image.source="https://github.com/greythink/hello_world"

# Run the application
CMD ["./hello_world"]
