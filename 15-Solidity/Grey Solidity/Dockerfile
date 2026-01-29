# Grey Solidity - Docker Image
# Multi-stage build for optimized production image

# ============================================
# Stage 1: Build Stage
# ============================================
FROM node:20-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git python3 make g++

WORKDIR /app

# Copy package files first for better layer caching
COPY package*.json ./

# Install all dependencies (including devDependencies for compilation)
RUN npm ci

# Copy source files
COPY contracts/ ./contracts/
COPY scripts/ ./scripts/
COPY test/ ./test/
COPY hardhat.config.js ./

# Compile contracts
RUN npm run compile

# ============================================
# Stage 2: Production Stage
# ============================================
FROM node:20-alpine AS production

# Add labels for container metadata
LABEL org.opencontainers.image.title="Grey Solidity"
LABEL org.opencontainers.image.description="Full-scale Solidity smart contract ecosystem"
LABEL org.opencontainers.image.version="2.0.0"
LABEL org.opencontainers.image.source="https://github.com/grey-solidity/grey-solidity"

# Create non-root user for security
RUN addgroup -g 1001 hardhat && \
    adduser -u 1001 -G hardhat -s /bin/sh -D hardhat

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install production dependencies only
RUN npm ci --omit=dev && npm cache clean --force

# Copy compiled artifacts from builder stage
COPY --from=builder /app/artifacts/ ./artifacts/
COPY --from=builder /app/cache/ ./cache/

# Copy source files
COPY contracts/ ./contracts/
COPY scripts/ ./scripts/
COPY hardhat.config.js ./

# Change ownership to non-root user
RUN chown -R hardhat:hardhat /app

USER hardhat

# Expose Hardhat node port
EXPOSE 8545

# Health check for Hardhat node
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:8545 || exit 1

# Default command: start Hardhat node
CMD ["npx", "hardhat", "node", "--hostname", "0.0.0.0"]

# ============================================
# Stage 3: Development Stage
# ============================================
FROM node:20-alpine AS development

# Install build dependencies
RUN apk add --no-cache git python3 make g++

WORKDIR /app

# Copy package files
COPY package*.json ./

# Install all dependencies
RUN npm ci

# Copy all source files
COPY . .

# Expose Hardhat node port
EXPOSE 8545

# Development command with hot reload support via volume mount
CMD ["npx", "hardhat", "node", "--hostname", "0.0.0.0"]

# ============================================
# Stage 4: Test Stage
# ============================================
FROM development AS test

# Run tests
CMD ["npm", "run", "test"]
