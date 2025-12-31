# =============================================================================
# Student Gradebook - Production Dockerfile
# Multi-stage build for minimal image size
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Stage
# -----------------------------------------------------------------------------
FROM gcc:13-bookworm AS builder

WORKDIR /build

# Copy source code
COPY student_gradebook.c .

# Compile with optimizations and static linking for portability
RUN gcc -std=c99 -O2 -Wall -Wextra \
    -static \
    -o student_gradebook \
    student_gradebook.c

# -----------------------------------------------------------------------------
# Stage 2: Production Stage
# Using scratch for minimal image (static binary)
# -----------------------------------------------------------------------------
FROM scratch AS production

# Copy the statically linked binary
COPY --from=builder /build/student_gradebook /student_gradebook

# Set the entrypoint
ENTRYPOINT ["/student_gradebook"]

# -----------------------------------------------------------------------------
# Stage 3: Debug/Development Stage (alternative)
# Use this stage if you need a shell for debugging
# -----------------------------------------------------------------------------
FROM alpine:3.19 AS debug

# Create non-root user for security
RUN addgroup -g 1000 appgroup && \
    adduser -u 1000 -G appgroup -D -h /app appuser

WORKDIR /app

# Copy the binary from builder
COPY --from=builder /build/student_gradebook .

# Set ownership
RUN chown -R appuser:appgroup /app

# Switch to non-root user
USER appuser

# Set the entrypoint
ENTRYPOINT ["./student_gradebook"]
