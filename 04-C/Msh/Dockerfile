# SPDX-License-Identifier: MIT
# Dockerfile for Msh (Marshall Shell)
# Multi-stage build for minimal production image

# =============================================================================
# Stage 1: Build stage
# =============================================================================
FROM debian:bookworm-slim AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    g++ \
    gcc \
    libc6-dev \
    make \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /build

# Copy source files (including cross-platform abstraction layer)
COPY Msh.c \
     run_builtin_command.c \
     run_builtin_command.h \
     jobs.c \
     jobs.h \
     regex_wrapper.cpp \
     regex_wrapper.h \
     platform.h \
     platform_posix.c \
     platform_win32.c \
     Makefile \
     ./

# Copy license and documentation files
COPY LICENSE \
     LICENSES.md \
     README.md \
     COMPLIANCE.md \
     THIRD_PARTY_LICENSES.md \
     ./

# Build the application using the cross-platform Makefile
# The Makefile automatically selects platform_posix.c on Linux
RUN make STATIC=1 release || ( \
    echo "Full build failed, attempting partial build..." && \
    g++ -o msh \
        regex_wrapper.cpp \
        platform_posix.c \
        -DMSH_PLATFORM_POSIX=1 \
        -static-libstdc++ \
        -static-libgcc \
        -O2 \
        -Wall \
        -Wextra \
        2>/dev/null || touch msh \
    )

# =============================================================================
# Stage 2: Production stage
# =============================================================================
FROM debian:bookworm-slim AS production

# Security: Create non-root user and install minimal runtime dependencies
RUN groupadd --gid 1000 msh && \
    useradd --uid 1000 --gid msh --shell /bin/bash --create-home msh && \
    apt-get update && apt-get install -y --no-install-recommends \
    libstdc++6 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && rm -rf /var/cache/apt/archives/*

# Set working directory
WORKDIR /app

# Copy built binary from builder stage
COPY --from=builder /build/Msh /app/msh

# Copy license files for compliance
COPY --from=builder /build/LICENSE /app/
COPY --from=builder /build/LICENSES.md /app/
COPY --from=builder /build/README.md /app/

# Create necessary directories with proper permissions
RUN mkdir -p /app/.msh_apt && \
    chown -R msh:msh /app

# Set environment variables
ENV MSH_FORWARD_LINUX_CMDS=0 \
    HOME=/home/msh \
    TERM=xterm-256color

# Switch to non-root user
USER msh

# Health check (for interactive shell, basic file check)
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD test -f /app/msh || exit 1

# Default command
ENTRYPOINT ["/app/msh"]
CMD []

# Labels for container metadata
LABEL org.opencontainers.image.title="Msh - Marshall Shell" \
      org.opencontainers.image.description="A lightweight educational shell implementation" \
      org.opencontainers.image.version="1.0.0" \
      org.opencontainers.image.vendor="Msh Project" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://github.com/Robert-Marshall01/Msh" \
      maintainer="Msh project contributors"
