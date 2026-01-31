# =============================================================================
# Grey AI Internal - Frontend Dockerfile
# =============================================================================
# Multi-stage build for production-optimized React application.
# Stage 1: Build React app with Vite
# Stage 2: Serve with nginx
#
# Build: docker build -t grey-ai-frontend .
# Run:   docker run -p 3000:80 grey-ai-frontend
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build
# -----------------------------------------------------------------------------
FROM node:18-alpine as builder

WORKDIR /app

# Build-time argument for API URL (baked into the build)
ARG VITE_API_URL=http://localhost:8000
ENV VITE_API_URL=$VITE_API_URL

# Install dependencies first (better caching)
COPY package.json package-lock.json* ./
RUN npm ci --silent

# Copy source code
COPY . .

# Build the production bundle
RUN npm run build


# -----------------------------------------------------------------------------
# Stage 2: Production (nginx)
# -----------------------------------------------------------------------------
FROM nginx:alpine as production

# Labels
LABEL maintainer="Grey AI Internal Team"
LABEL description="React frontend for AI-powered data insights"
LABEL version="1.0.0"

# Remove default nginx content
RUN rm -rf /usr/share/nginx/html/*

# Copy built assets from builder
COPY --from=builder /app/dist /usr/share/nginx/html

# Custom nginx configuration for SPA routing
COPY nginx.conf /etc/nginx/conf.d/default.conf

# Create non-root user (nginx image includes nginx user)
RUN chown -R nginx:nginx /usr/share/nginx/html && \
    chown -R nginx:nginx /var/cache/nginx && \
    chown -R nginx:nginx /var/log/nginx && \
    touch /var/run/nginx.pid && \
    chown -R nginx:nginx /var/run/nginx.pid

# Expose port
EXPOSE 80

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:80/ || exit 1

# Start nginx in foreground
CMD ["nginx", "-g", "daemon off;"]
