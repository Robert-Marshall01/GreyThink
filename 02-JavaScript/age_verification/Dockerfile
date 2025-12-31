# ============================================
# Age Verification CLI Application
# Node.js Interactive Terminal Application
# ============================================

# Stage 1: Build stage
FROM node:20-alpine AS builder

# Set working directory
WORKDIR /app

# Copy package files first for better layer caching
COPY package*.json ./

# Install dependencies (production only)
RUN npm ci --only=production && npm cache clean --force

# Stage 2: Production stage
FROM node:20-alpine AS production

# Add labels for better maintainability
LABEL maintainer="Robert Marshall"
LABEL description="Age Verification CLI Application"
LABEL version="1.0.0"

# Create non-root user for security
RUN addgroup -g 1001 -S nodejs && \
    adduser -S nodejs -u 1001 -G nodejs

# Set working directory
WORKDIR /app

# Copy node_modules from builder stage
COPY --from=builder /app/node_modules ./node_modules

# Copy application source files
COPY --chown=nodejs:nodejs age_verification.js ./
COPY --chown=nodejs:nodejs LICENSE ./

# Switch to non-root user
USER nodejs

# Set environment variables
ENV NODE_ENV=production

# Run the application interactively
# Note: This is a CLI app that requires TTY input
CMD ["node", "age_verification.js"]
