# ==============================================================================
# Quick Maths - Multi-stage Dockerfile
# A production-grade container for a C++11 console application
# ==============================================================================

# ------------------------------------------------------------------------------
# Stage 1: Build Stage
# ------------------------------------------------------------------------------
FROM gcc:13-bookworm AS builder

WORKDIR /build

# Copy source files
COPY quick_maths.cpp .

# Compile the application with optimizations
RUN g++ -std=c++11 -O2 -Wall -Wextra -o quick_maths quick_maths.cpp

# ------------------------------------------------------------------------------
# Stage 2: Runtime Stage (minimal image)
# ------------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install minimal runtime dependencies and create non-root user
RUN apt-get update && apt-get install -y --no-install-recommends \
    libstdc++6 \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && groupadd -r quickmaths && useradd -r -g quickmaths quickmaths

WORKDIR /app

# Copy compiled binary from builder stage
COPY --from=builder /build/quick_maths .

# Set ownership
RUN chown -R quickmaths:quickmaths /app

# Switch to non-root user
USER quickmaths

# Labels for container metadata
LABEL maintainer="developer" \
      version="1.0" \
      description="Quick Maths - A simple math game in C++"

# Run the application
# Note: This is an interactive console app, run with: docker run -it quick_maths
ENTRYPOINT ["./quick_maths"]
