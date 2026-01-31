# =============================================================================
# Grey AI Internal - Test Configuration (conftest.py)
# =============================================================================
# Shared pytest fixtures for both unit and integration tests.
# Provides database sessions, sample data, and mock objects.
#
# Usage:
#   pytest                      # Run all tests
#   pytest tests/unit           # Run unit tests only
#   pytest tests/integration    # Run integration tests only
#   pytest -v                   # Verbose output
#   pytest -x                   # Stop on first failure
# =============================================================================

import asyncio
import os
import sys
import tempfile
from pathlib import Path
from typing import AsyncGenerator, Dict, Any, Generator

import pytest
import pytest_asyncio
from sqlalchemy.ext.asyncio import AsyncSession, create_async_engine, async_sessionmaker

# Add backend to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from models.database import Base, Upload, Metrics, AIOutput, User


# =============================================================================
# Event Loop Configuration
# =============================================================================

@pytest.fixture(scope="session")
def event_loop() -> Generator[asyncio.AbstractEventLoop, None, None]:
    """
    Create an event loop for the entire test session.
    This allows async fixtures to work across multiple tests.
    """
    loop = asyncio.new_event_loop()
    yield loop
    loop.close()


# =============================================================================
# Database Fixtures
# =============================================================================

@pytest_asyncio.fixture(scope="function")
async def test_db_engine():
    """
    Create a temporary SQLite database engine for testing.
    Uses an in-memory database for speed.
    
    Scope: function - creates fresh database for each test.
    """
    # Use in-memory SQLite for fast, isolated tests
    engine = create_async_engine(
        "sqlite+aiosqlite:///:memory:",
        echo=False,
        future=True,
    )
    
    # Create all tables
    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)
    
    yield engine
    
    # Cleanup
    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.drop_all)
    await engine.dispose()


@pytest_asyncio.fixture(scope="function")
async def db_session(test_db_engine) -> AsyncGenerator[AsyncSession, None]:
    """
    Provide a database session for tests.
    Each test gets a fresh session with automatic rollback.
    
    Usage:
        async def test_something(db_session):
            upload = Upload(filename="test.csv", ...)
            db_session.add(upload)
            await db_session.commit()
    """
    async_session = async_sessionmaker(
        test_db_engine,
        class_=AsyncSession,
        expire_on_commit=False,
        autoflush=False,
    )
    
    async with async_session() as session:
        yield session
        # Rollback any uncommitted changes
        await session.rollback()


@pytest_asyncio.fixture
async def sample_upload(db_session: AsyncSession) -> Upload:
    """
    Create and return a sample Upload record in the database.
    Useful for tests that need an existing upload.
    """
    upload = Upload(
        filename="test_data.csv",
        file_path="/tmp/test_data.csv",
        file_type="csv",
        file_size=1024,
        status="parsed",
        mime_type="text/csv"
    )
    db_session.add(upload)
    await db_session.commit()
    await db_session.refresh(upload)
    return upload


@pytest_asyncio.fixture
async def sample_upload_with_metrics(db_session: AsyncSession, sample_upload: Upload) -> Upload:
    """
    Create an Upload with associated Metrics record.
    """
    metrics = Metrics(
        upload_id=sample_upload.id,
        row_count=100,
        column_count=5,
        parsed_data={"rows": 100, "cols": 5},
        columns_json={
            "columns": [
                {"name": "id", "dtype": "int64", "non_null_count": 100, "null_count": 0},
                {"name": "name", "dtype": "object", "non_null_count": 100, "null_count": 0},
                {"name": "value", "dtype": "float64", "non_null_count": 95, "null_count": 5},
            ]
        },
        stats_json={
            "value": {"min": 0.0, "max": 100.0, "mean": 50.0, "median": 48.5}
        },
        preview_json=[
            {"id": 1, "name": "Item A", "value": 42.5},
            {"id": 2, "name": "Item B", "value": 67.3},
        ]
    )
    db_session.add(metrics)
    await db_session.commit()
    await db_session.refresh(sample_upload)
    return sample_upload


@pytest_asyncio.fixture
async def sample_upload_with_insights(
    db_session: AsyncSession, 
    sample_upload_with_metrics: Upload
) -> Upload:
    """
    Create an Upload with Metrics and AIOutput.
    Represents a fully analyzed upload.
    """
    ai_output = AIOutput(
        upload_id=sample_upload_with_metrics.id,
        ai_summary="The data shows strong performance in Q4 with a 15% increase.",
        ai_recommendations={
            "actions": [
                "Increase inventory for top-performing items",
                "Investigate low-performing regions"
            ]
        },
        insights_json=[
            "Revenue peaked in December",
            "North region outperformed by 23%",
            "Product A shows consistent growth"
        ],
        model_used="llama3"
    )
    db_session.add(ai_output)
    sample_upload_with_metrics.status = "analyzed"
    await db_session.commit()
    await db_session.refresh(sample_upload_with_metrics)
    return sample_upload_with_metrics


# =============================================================================
# Sample File Content Fixtures
# =============================================================================

@pytest.fixture
def sample_csv_content() -> str:
    """
    Sample CSV content for parser tests.
    Contains numeric, string, and null values.
    """
    return """id,name,value,category,date
1,Product A,100.50,Electronics,2026-01-15
2,Product B,75.25,Clothing,2026-01-16
3,Product C,200.00,Electronics,2026-01-17
4,Product D,,Home,2026-01-18
5,Product E,50.00,Clothing,2026-01-19
6,Product F,150.75,Electronics,2026-01-20
7,Product G,89.99,Home,2026-01-21
8,Product H,120.00,Clothing,2026-01-22
9,Product I,175.50,Electronics,2026-01-23
10,Product J,95.00,Home,2026-01-24"""


@pytest.fixture
def sample_csv_bytes(sample_csv_content: str) -> bytes:
    """Sample CSV as bytes (for file upload simulation)."""
    return sample_csv_content.encode("utf-8")


@pytest.fixture
def sample_text_content() -> str:
    """
    Sample text content for parser tests.
    Represents a simple text file with multiple lines.
    """
    return """This is a sample text file.
It contains multiple lines of content.
Each line will be parsed as a row.
The parser should extract line and word counts.
This is useful for testing text file handling."""


@pytest.fixture
def sample_text_bytes(sample_text_content: str) -> bytes:
    """Sample text as bytes."""
    return sample_text_content.encode("utf-8")


@pytest.fixture
def large_csv_content() -> str:
    """
    Larger CSV for performance and edge case testing.
    100 rows with varied data.
    """
    header = "id,name,value,category,quantity"
    rows = [header]
    categories = ["Electronics", "Clothing", "Home", "Food", "Books"]
    for i in range(1, 101):
        category = categories[i % len(categories)]
        value = 10.0 + (i * 1.5)
        rows.append(f"{i},Item_{i},{value:.2f},{category},{i * 2}")
    return "\n".join(rows)


@pytest.fixture
def malformed_csv_content() -> str:
    """Malformed CSV for error handling tests."""
    return """id,name,value
1,Product A,100
2,Product B
3,Product C,200,extra_field
4,Product D,"""


@pytest.fixture
def empty_csv_content() -> str:
    """Empty CSV (header only) for edge case tests."""
    return "id,name,value"


# =============================================================================
# Mock AI Response Fixtures
# =============================================================================

@pytest.fixture
def mock_ai_response() -> Dict[str, Any]:
    """
    Mock response from AIService.analyze_data().
    Used to test integration without actual Ollama calls.
    """
    return {
        "summary": "The dataset reveals strong quarterly growth patterns with notable seasonal variations. Key metrics show a 15% improvement over the previous period.",
        "insights": [
            "Revenue increased 15% quarter-over-quarter",
            "North region leads with 23% market share",
            "Product category A shows consistent upward trend",
            "Customer retention improved to 78%",
            "Seasonal peak observed in December"
        ],
        "recommendations": "Focus marketing efforts on high-performing regions. Consider expanding Product A inventory. Investigate underperforming segments for optimization opportunities.",
        "created_at": "2026-01-30T10:30:00Z"
    }


@pytest.fixture
def mock_ollama_generate_response() -> Dict[str, Any]:
    """
    Mock response from Ollama /api/generate endpoint.
    Simulates actual Ollama API response structure.
    """
    return {
        "model": "llama3",
        "created_at": "2026-01-30T10:30:00.123456Z",
        "response": """SUMMARY:
The data shows strong performance metrics with notable growth trends.

INSIGHTS:
- Revenue increased by 15% quarter-over-quarter
- Top 3 products account for 60% of sales
- Customer acquisition cost decreased by 10%

RECOMMENDATIONS:
Focus on scaling the top-performing product lines. Consider expanding into adjacent market segments. Monitor the customer acquisition funnel for optimization opportunities.""",
        "done": True,
        "total_duration": 5000000000,
        "prompt_eval_count": 50,
        "eval_count": 150
    }


# =============================================================================
# Temporary File Fixtures
# =============================================================================

@pytest.fixture
def temp_upload_dir() -> Generator[Path, None, None]:
    """
    Provide a temporary directory for file uploads.
    Automatically cleans up after test.
    """
    with tempfile.TemporaryDirectory() as tmpdir:
        upload_path = Path(tmpdir) / "uploads"
        upload_path.mkdir()
        yield upload_path


@pytest.fixture
def temp_csv_file(temp_upload_dir: Path, sample_csv_content: str) -> Path:
    """
    Create a temporary CSV file for upload testing.
    Returns the path to the file.
    """
    file_path = temp_upload_dir / "test_data.csv"
    file_path.write_text(sample_csv_content)
    return file_path


# =============================================================================
# Service Fixtures
# =============================================================================

@pytest.fixture
def parser_service():
    """Provide a ParserService instance for testing."""
    from services.parser import ParserService
    return ParserService()


@pytest.fixture
def ai_service():
    """
    Provide an AIService instance for testing.
    Note: Real tests should mock the HTTP calls.
    """
    from services.ai_service import AIService
    return AIService()


@pytest.fixture
def persistence_service(db_session: AsyncSession):
    """Provide a PersistenceService with test database session."""
    from services.persistence import PersistenceService
    return PersistenceService(db_session)


# =============================================================================
# FastAPI Test Client
# =============================================================================

@pytest_asyncio.fixture
async def test_client(db_session: AsyncSession):
    """
    Provide an async test client for API testing.
    Overrides the database dependency with test session.
    """
    from fastapi.testclient import TestClient
    from httpx import AsyncClient, ASGITransport
    from main import app
    from models.database import get_db_session
    
    # Override database dependency
    async def override_get_db():
        yield db_session
    
    app.dependency_overrides[get_db_session] = override_get_db
    
    # Create async client
    async with AsyncClient(
        transport=ASGITransport(app=app),
        base_url="http://test"
    ) as client:
        yield client
    
    # Clean up override
    app.dependency_overrides.clear()


# =============================================================================
# Environment Configuration
# =============================================================================

@pytest.fixture(autouse=True)
def set_test_environment(monkeypatch):
    """
    Set test-specific environment variables.
    Runs automatically for all tests.
    """
    monkeypatch.setenv("DATABASE_URL", "sqlite+aiosqlite:///:memory:")
    monkeypatch.setenv("OLLAMA_BASE_URL", "http://localhost:11434")
    monkeypatch.setenv("OLLAMA_MODEL", "llama3")
    monkeypatch.setenv("DEBUG", "true")
    monkeypatch.setenv("UPLOAD_DIR", "/tmp/test_uploads")
    monkeypatch.setenv("MAX_FILE_SIZE_MB", "10")
