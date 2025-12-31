# =============================================================================
# Dockerfile for Math RPG (C++ Console Application)
# Multi-stage build for optimized image size
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Stage
# -----------------------------------------------------------------------------
FROM gcc:13-bookworm AS builder

LABEL maintainer="Robert Marshall <RobertCMarshall2007@outlook.com>"

# Set working directory
WORKDIR /build

# Copy source files
COPY math_rpg.cpp .
COPY tests/ ./tests/

# Compile the main application with optimizations
RUN g++ -std=c++17 -O2 -static -Wall -Wextra -o math_rpg math_rpg.cpp

# Compile tests
RUN g++ -std=c++17 -O2 -static -Wall -Wextra -o test_math tests/test_math.cpp

# Verify the build
RUN ./test_math

# -----------------------------------------------------------------------------
# Stage 2: Runtime Stage (minimal image)
# -----------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

LABEL maintainer="Robert Marshall <RobertCMarshall2007@outlook.com>"
LABEL description="Math RPG - A C++ trigonometry quiz game"
LABEL version="1.0.0"

# Install minimal runtime dependencies (none needed for static build)
# Add ca-certificates for any future HTTPS needs
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get clean

# Create non-root user for security
RUN groupadd --gid 1000 mathuser && \
    useradd --uid 1000 --gid mathuser --shell /bin/bash --create-home mathuser

# Set working directory
WORKDIR /app

# Copy compiled binary from builder stage
COPY --from=builder /build/math_rpg .
COPY --from=builder /build/test_math .

# Copy documentation
COPY README.md .
COPY LICENSE .

# Set ownership to non-root user
RUN chown -R mathuser:mathuser /app

# Switch to non-root user
USER mathuser

# Default command runs the game
# Note: This is an interactive CLI app, run with: docker run -it math-rpg
ENTRYPOINT ["./math_rpg"]

# Default args (can be overridden to run sample mode)
# Example: docker run math-rpg --sample 12345 10
CMD []
