# =============================================================================
# Dockerfile for password_checker - Rust CLI Application
# Multi-stage build for minimal production image
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Builder
# -----------------------------------------------------------------------------
FROM rust:1.83-slim-bookworm AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Create a non-root user for building
RUN useradd --create-home --uid 1000 builder
WORKDIR /home/builder/app

# Copy manifest files first for dependency caching
COPY --chown=builder:builder Cargo.toml Cargo.lock* ./

# Create dummy source to cache dependencies
RUN mkdir -p src/bin && \
    echo "fn main() {}" > src/main.rs && \
    echo "pub fn dummy() {}" > src/lib.rs && \
    echo "fn main() {}" > src/bin/gui.rs

# Build dependencies only (cached layer)
RUN cargo build --release --bin password_checker 2>/dev/null || true

# Remove dummy source files
RUN rm -rf src

# Copy actual source code
COPY --chown=builder:builder src ./src

# Touch source files to invalidate cache and rebuild with actual code
RUN touch src/main.rs src/lib.rs

# Build the CLI binary only (GUI requires X11 dependencies)
RUN cargo build --release --bin password_checker && \
    strip /home/builder/app/target/release/password_checker

# -----------------------------------------------------------------------------
# Stage 2: Runtime (minimal distroless-like image)
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install minimal runtime dependencies and create non-root user
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && useradd --create-home --uid 1000 --shell /bin/sh appuser

# Set working directory
WORKDIR /app

# Copy the compiled binary from builder stage
COPY --from=builder --chown=appuser:appuser \
    /home/builder/app/target/release/password_checker /app/password_checker

# Switch to non-root user
USER appuser

# Set the entrypoint to the CLI binary
ENTRYPOINT ["/app/password_checker"]

# Default command shows help
CMD ["--help"]

# -----------------------------------------------------------------------------
# Metadata labels
# -----------------------------------------------------------------------------
LABEL org.opencontainers.image.title="password_checker" \
      org.opencontainers.image.description="Password strength checker CLI tool" \
      org.opencontainers.image.version="0.1.0" \
      org.opencontainers.image.authors="Robert-Marshall01 <RobertCMarshall2007@outlook.com>" \
      org.opencontainers.image.source="https://github.com/Robert-Marshall01/GreyThink" \
      org.opencontainers.image.licenses="MIT"
