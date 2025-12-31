# =============================================================================
# MoneyJava - Dockerfile
# Java Swing Desktop Application Container
# =============================================================================
# Multi-stage build for optimized image size
# Requires X11 forwarding for GUI display
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build Stage
# -----------------------------------------------------------------------------
FROM eclipse-temurin:21-jdk-jammy AS builder

# Set working directory
WORKDIR /app

# Copy source files
COPY *.java ./

# Compile all Java source files
RUN javac -d /app/classes *.java

# -----------------------------------------------------------------------------
# Stage 2: Runtime Stage
# -----------------------------------------------------------------------------
FROM eclipse-temurin:21-jre-jammy AS runtime

# Labels for container metadata
LABEL maintainer="MoneyJava Developer" \
      version="1.0" \
      description="MoneyJava - Personal Finance Manager (Java Swing GUI)"

# Install X11 libraries, create user, set up directories in single layer
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxext6 \
    libxrender1 \
    libxtst6 \
    libxi6 \
    libfreetype6 \
    fontconfig \
    procps \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && groupadd -r appgroup \
    && useradd -r -g appgroup -d /app -s /sbin/nologin appuser

# Set working directory
WORKDIR /app

# Copy compiled classes from builder stage
COPY --from=builder /app/classes /app/classes

# Create data directory and set ownership
RUN mkdir -p /app/data && chown -R appuser:appgroup /app

# Switch to non-root user
USER appuser

# Set Java options for better container behavior
ENV JAVA_OPTS="-XX:+UseContainerSupport -XX:MaxRAMPercentage=75.0"

# Working directory for data persistence
WORKDIR /app/data

# Entry point - run the GUI application
# Note: Requires X11 forwarding (DISPLAY env var) to display GUI
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -cp /app/classes LoginSignupGUI"]
