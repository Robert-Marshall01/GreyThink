# =============================================================================
# Java Search Engine - Production Dockerfile
# Multi-stage build for minimal image size
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build
# -----------------------------------------------------------------------------
FROM eclipse-temurin:21-jdk-alpine AS builder

# Install build dependencies
RUN apk add --no-cache bash

WORKDIR /app

# Copy source code
COPY src/ ./src/

# Create output directory and compile
RUN mkdir -p out && \
    javac -d out \
        src/main/java/searchengine/Document.java \
        src/main/java/searchengine/SearchResult.java \
        src/main/java/searchengine/Tokenizer.java \
        src/main/java/searchengine/InvertedIndex.java \
        src/main/java/searchengine/SearchEngine.java \
        src/main/java/searchengine/WebSearchEngine.java \
        src/main/java/searchengine/Main.java

# -----------------------------------------------------------------------------
# Stage 2: Runtime (CLI Mode)
# -----------------------------------------------------------------------------
FROM eclipse-temurin:21-jre-alpine AS runtime

# Labels for container metadata
LABEL maintainer="Java Search Team" \
      version="1.0.0" \
      description="Java Search Engine - Local document indexing and web search" \
      org.opencontainers.image.source="https://github.com/example/java-search"

# Create non-root user for security
RUN addgroup -g 1001 -S searchuser && \
    adduser -u 1001 -S searchuser -G searchuser

WORKDIR /app

# Copy compiled classes from builder
COPY --from=builder /app/out ./out
COPY README.md ./

# Change ownership to non-root user
RUN chown -R searchuser:searchuser /app

# Switch to non-root user
USER searchuser

# Set Java options for containers
ENV JAVA_OPTS="-XX:+UseContainerSupport -XX:MaxRAMPercentage=75.0 -XX:+UseG1GC"

# Health check using a simple Java version check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD java -version || exit 1

# Default entrypoint - runs the interactive CLI
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -cp out searchengine.Main"]

# -----------------------------------------------------------------------------
# Stage 3: Web Search API (Optional - for headless web search)
# -----------------------------------------------------------------------------
FROM eclipse-temurin:21-jre-alpine AS websearch

LABEL maintainer="Java Search Team" \
      version="1.0.0" \
      description="Java Web Search Engine - Wikipedia and StackExchange search"

# Create non-root user
RUN addgroup -g 1001 -S searchuser && \
    adduser -u 1001 -S searchuser -G searchuser

WORKDIR /app

# Copy compiled classes from builder
COPY --from=builder /app/out ./out

# Change ownership
RUN chown -R searchuser:searchuser /app

USER searchuser

ENV JAVA_OPTS="-XX:+UseContainerSupport -XX:MaxRAMPercentage=75.0 -XX:+UseG1GC"

# Run web search with query passed as argument
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -cp out searchengine.WebSearchEngine \"$@\"", "--"]
CMD ["Java programming"]
