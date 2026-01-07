# =============================================================================
# DOCKERFILE - Local Development Container
# =============================================================================
# Multi-stage build for efficient, secure container image
# Stage 1: Build environment
# Stage 2: Production-ready runtime
#
# Build: docker build -t sample-app:latest .
# Run:   docker run -p 8080:80 sample-app:latest

# -----------------------------------------------------------------------------
# Stage 1: Build Stage (if needed for compiled apps)
# -----------------------------------------------------------------------------
# Uncomment and modify for applications requiring build steps (Node.js, Go, etc.)
# FROM node:20-alpine AS builder
# WORKDIR /app
# COPY package*.json ./
# RUN npm ci --only=production
# COPY . .
# RUN npm run build

# -----------------------------------------------------------------------------
# Stage 2: Production Runtime
# -----------------------------------------------------------------------------
# SECURITY: Use specific version with digest for reproducibility
# To get digest: docker pull nginx:1.25-alpine && docker inspect --format='{{index .RepoDigests 0}}' nginx:1.25-alpine
FROM nginx:1.25.3-alpine

# Metadata labels for container identification
LABEL maintainer="platform-team"
LABEL version="1.0.0"
LABEL description="Sample application for local Kubernetes development"

# SECURITY: Install security updates and configure nginx permissions
# Combined into single RUN to reduce layers
RUN apk update && apk upgrade --no-cache && rm -rf /var/cache/apk/* && \
    chown -R nginx:nginx /var/cache/nginx && \
    chown -R nginx:nginx /var/log/nginx && \
    chown -R nginx:nginx /etc/nginx/conf.d && \
    touch /var/run/nginx.pid && \
    chown -R nginx:nginx /var/run/nginx.pid

# Copy custom nginx configuration (optional)
# COPY nginx.conf /etc/nginx/nginx.conf

# Copy application files
# For static sites:
COPY --chown=nginx:nginx ./public /usr/share/nginx/html

# Expose port 80 for HTTP traffic
# This is the port the container listens on internally
EXPOSE 80

# Switch to non-root user for security
USER nginx

# Health check for container orchestration
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:80/ || exit 1

# Start nginx in foreground mode
CMD ["nginx", "-g", "daemon off;"]
