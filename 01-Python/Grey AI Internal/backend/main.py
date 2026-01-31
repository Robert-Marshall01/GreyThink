"""
Grey AI Internal - FastAPI Backend Entrypoint
==============================================
Main application entry point that initializes FastAPI, configures middleware,
and registers all route modules. This is the single source of truth for app config.
"""

import os
from contextlib import asynccontextmanager

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

# Import route modules
from routes import upload, ai, images

# Import database initialization
from models.database import init_database


# -----------------------------------------------------------------------------
# Application Lifespan Handler
# -----------------------------------------------------------------------------
# Handles startup and shutdown events for the application.
# Database tables are created on startup.

@asynccontextmanager
async def lifespan(app: FastAPI):
    """
    Application lifespan context manager.
    - Startup: Initialize database tables
    - Shutdown: Cleanup resources if needed
    """
    # Startup
    print("🚀 Starting Grey AI Internal backend...")
    await init_database()
    print("✅ Database initialized")
    yield
    # Shutdown
    print("👋 Shutting down Grey AI Internal backend...")


# -----------------------------------------------------------------------------
# FastAPI Application Instance
# -----------------------------------------------------------------------------

app = FastAPI(
    title="Grey AI Internal",
    description="AI-powered data insights tool using local LLM via Ollama",
    version="1.0.0",
    lifespan=lifespan,
)


# -----------------------------------------------------------------------------
# CORS Middleware Configuration
# -----------------------------------------------------------------------------
# Allow frontend (React dev server) to communicate with backend.
# In production, restrict origins to your actual domain.

ALLOWED_ORIGINS = os.getenv(
    "CORS_ORIGINS", 
    "http://localhost:5173,http://localhost:3000"
).split(",")

app.add_middleware(
    CORSMiddleware,
    allow_origins=ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# -----------------------------------------------------------------------------
# Health Check Endpoint
# -----------------------------------------------------------------------------
# Simple endpoint to verify the API is running.
# Useful for load balancers, container orchestration, and monitoring.

@app.get("/health", tags=["Health"])
async def health_check():
    """
    Health check endpoint.
    Returns OK status if the server is running properly.
    """
    return {
        "status": "healthy",
        "service": "Grey AI Internal",
        "version": "1.0.0"
    }


# -----------------------------------------------------------------------------
# Route Registration
# -----------------------------------------------------------------------------
# Register all route modules with their prefixes.
# Each module handles a specific domain of functionality.

app.include_router(
    upload.router,
    prefix="/api/upload",
    tags=["Upload"]
)

app.include_router(
    ai.router,
    prefix="/api/ai",
    tags=["AI Inference"]
)

app.include_router(
    images.router,
    # Note: images.router already has /api/images prefix
    tags=["Image Generation"]
)


# -----------------------------------------------------------------------------
# Development Server
# -----------------------------------------------------------------------------
# Run directly with: python main.py
# For production, use: uvicorn main:app --host 0.0.0.0 --port 8000

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=8000,
        reload=True  # Auto-reload on code changes (dev only)
    )
