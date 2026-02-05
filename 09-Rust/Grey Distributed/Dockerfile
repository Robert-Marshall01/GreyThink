# =============================================================================
# Grey Distributed - Multi-stage Dockerfile
# =============================================================================
#
# This Dockerfile builds the Grey Distributed daemon (greyd) in a multi-stage
# build for minimal image size and security.
#
# Build:
#   docker build -t grey-distributed:latest .
#   docker build --build-arg VERSION=v1.0.0 -t grey-distributed:v1.0.0 .
#
# Run:
#   docker run -p 8080:8080 -p 9090:9090 grey-distributed:latest
#
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build the Go binaries
# -----------------------------------------------------------------------------
FROM golang:1.22-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    git \
    ca-certificates \
    tzdata \
    make

# Build arguments for versioning
ARG VERSION=dev
ARG COMMIT=unknown
ARG BUILD_TIME=unknown

# Set working directory
WORKDIR /build

# Copy go module files first for better layer caching
COPY go.mod go.sum* ./

# Download dependencies
RUN go mod download

# Copy source code
COPY . .

# Build greyd binary
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build \
    -trimpath \
    -ldflags "-s -w \
        -X main.Version=${VERSION} \
        -X main.Commit=${COMMIT} \
        -X main.BuildTime=${BUILD_TIME}" \
    -o /build/bin/greyd \
    ./cmd/greyd

# Build greyctl binary
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build \
    -trimpath \
    -ldflags "-s -w \
        -X main.Version=${VERSION} \
        -X main.Commit=${COMMIT} \
        -X main.BuildTime=${BUILD_TIME}" \
    -o /build/bin/greyctl \
    ./cmd/greyctl

# -----------------------------------------------------------------------------
# Stage 2: Create minimal runtime image
# -----------------------------------------------------------------------------
FROM alpine:3.19 AS runtime

# Install runtime dependencies
RUN apk add --no-cache \
    ca-certificates \
    tzdata \
    curl \
    && rm -rf /var/cache/apk/*

# Create non-root user for security
RUN addgroup -g 1000 grey && \
    adduser -u 1000 -G grey -h /home/grey -D grey

# Create necessary directories
RUN mkdir -p /etc/grey /var/lib/grey/data /var/lib/grey/work /tmp && \
    chown -R grey:grey /etc/grey /var/lib/grey

# Copy binaries from builder
COPY --from=builder /build/bin/greyd /usr/local/bin/greyd
COPY --from=builder /build/bin/greyctl /usr/local/bin/greyctl

# Ensure binaries are executable
RUN chmod +x /usr/local/bin/greyd /usr/local/bin/greyctl

# Copy timezone data
COPY --from=builder /usr/share/zoneinfo /usr/share/zoneinfo

# Switch to non-root user
USER grey
WORKDIR /home/grey

# Health check endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8080/health/live || exit 1

# Expose ports
# 8080 - HTTP API
# 9090 - gRPC API
# 7070 - Raft consensus
# 7071 - Gossip protocol (UDP)
EXPOSE 8080 9090 7070 7071/udp

# Default command
ENTRYPOINT ["/usr/local/bin/greyd"]
CMD ["--help"]
