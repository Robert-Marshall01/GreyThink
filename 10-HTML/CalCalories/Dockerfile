# Production-grade Dockerfile for CalCalories Flask Application
# Python 3.11 slim image for smaller size and security

# Stage 1: Build stage
FROM python:3.11-slim AS builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements first for layer caching
COPY requirements.txt .

# Create virtual environment and install dependencies
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir -r requirements.txt

# Stage 2: Production stage
FROM python:3.11-slim AS production

# Set environment variables
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PATH="/opt/venv/bin:$PATH" \
    FLASK_ENV=production \
    FLASK_APP=app.py

WORKDIR /app

# Create non-root user for security
RUN groupadd --gid 1000 appgroup && \
    useradd --uid 1000 --gid appgroup --shell /bin/bash --create-home appuser

# Copy virtual environment from builder stage
COPY --from=builder /opt/venv /opt/venv

# Copy application files
COPY --chown=appuser:appgroup app.py calcal.py calcal.sql ./
COPY --chown=appuser:appgroup calcal_home.html calcal_settings.html calcal_signin.html calcal_usersetup.html ./
COPY --chown=appuser:appgroup calcal_css.css calcal_js.js ./

# Create directory for SQLite database with proper permissions
RUN mkdir -p /app/data && chown -R appuser:appgroup /app/data

# Switch to non-root user
USER appuser

# Expose Flask port
EXPOSE 5000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:5000/')" || exit 1

# Run with gunicorn for production
CMD ["gunicorn", "--bind", "0.0.0.0:5000", "--workers", "2", "--threads", "4", "--timeout", "120", "app:app"]
