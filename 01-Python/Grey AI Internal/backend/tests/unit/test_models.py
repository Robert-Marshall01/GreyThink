# =============================================================================
# Unit Tests - Database Models & Persistence
# =============================================================================
# Tests for ORM models (Upload, Metrics, AIOutput) and PersistenceService.
# Validates CRUD operations, relationships, and database constraints.
#
# Run: pytest tests/unit/test_models.py -v
# =============================================================================

import pytest
from datetime import datetime, timezone
from sqlalchemy import select
from sqlalchemy.ext.asyncio import AsyncSession

from models.database import Upload, Metrics, AIOutput, User, utc_now
from services.persistence import PersistenceService, RecordNotFoundError


class TestUtcNowFunction:
    """Tests for the utc_now utility function."""
    
    def test_utc_now_returns_datetime(self):
        """Test that utc_now returns a datetime object."""
        result = utc_now()
        assert isinstance(result, datetime)
    
    def test_utc_now_is_timezone_aware(self):
        """Test that utc_now returns timezone-aware datetime."""
        result = utc_now()
        assert result.tzinfo is not None
        assert result.tzinfo == timezone.utc


class TestUploadModel:
    """Tests for the Upload ORM model."""
    
    @pytest.mark.asyncio
    async def test_create_upload(self, db_session: AsyncSession):
        """
        Test basic upload creation.
        Verifies all required fields are saved correctly.
        """
        # Arrange
        upload = Upload(
            filename="test.csv",
            file_path="/uploads/test.csv",
            file_type="csv",
            file_size=1024,
            status="uploaded"
        )
        
        # Act
        db_session.add(upload)
        await db_session.commit()
        await db_session.refresh(upload)
        
        # Assert
        assert upload.id is not None
        assert upload.filename == "test.csv"
        assert upload.file_type == "csv"
        assert upload.file_size == 1024
        assert upload.status == "uploaded"
        assert upload.created_at is not None
    
    @pytest.mark.asyncio
    async def test_upload_default_status(self, db_session: AsyncSession):
        """
        Test that upload status defaults to 'uploaded'.
        """
        # Arrange
        upload = Upload(
            filename="test.txt",
            file_path="/uploads/test.txt",
            file_type="txt",
            file_size=512
        )
        
        # Act
        db_session.add(upload)
        await db_session.commit()
        
        # Assert
        assert upload.status == "uploaded"
    
    @pytest.mark.asyncio
    async def test_upload_optional_fields(self, db_session: AsyncSession):
        """
        Test that optional fields can be null.
        """
        # Arrange
        upload = Upload(
            filename="minimal.csv",
            file_path="/uploads/minimal.csv",
            file_type="csv",
            file_size=256
        )
        
        # Act
        db_session.add(upload)
        await db_session.commit()
        
        # Assert
        assert upload.user_id is None
        assert upload.mime_type is None
        assert upload.error_message is None
    
    @pytest.mark.asyncio
    async def test_upload_with_user_association(self, db_session: AsyncSession):
        """
        Test upload with user foreign key relationship.
        """
        # Arrange
        user = User(
            username="testuser",
            email="test@example.com"
        )
        db_session.add(user)
        await db_session.commit()
        
        upload = Upload(
            filename="user_file.csv",
            file_path="/uploads/user_file.csv",
            file_type="csv",
            file_size=1024,
            user_id=user.id
        )
        db_session.add(upload)
        await db_session.commit()
        
        # Assert
        assert upload.user_id == user.id
    
    @pytest.mark.asyncio
    async def test_upload_repr(self, db_session: AsyncSession):
        """Test the __repr__ method for debugging."""
        # Arrange
        upload = Upload(
            filename="repr_test.csv",
            file_path="/test",
            file_type="csv",
            file_size=100
        )
        db_session.add(upload)
        await db_session.commit()
        
        # Act
        repr_str = repr(upload)
        
        # Assert
        assert "Upload" in repr_str
        assert "repr_test.csv" in repr_str


class TestMetricsModel:
    """Tests for the Metrics ORM model."""
    
    @pytest.mark.asyncio
    async def test_create_metrics(
        self, 
        db_session: AsyncSession, 
        sample_upload: Upload
    ):
        """
        Test basic metrics creation linked to an upload.
        """
        # Arrange
        metrics = Metrics(
            upload_id=sample_upload.id,
            row_count=100,
            column_count=5
        )
        
        # Act
        db_session.add(metrics)
        await db_session.commit()
        await db_session.refresh(metrics)
        
        # Assert
        assert metrics.id is not None
        assert metrics.upload_id == sample_upload.id
        assert metrics.row_count == 100
        assert metrics.column_count == 5
    
    @pytest.mark.asyncio
    async def test_metrics_json_fields(
        self, 
        db_session: AsyncSession, 
        sample_upload: Upload
    ):
        """
        Test that JSON fields store and retrieve correctly.
        """
        # Arrange
        columns_data = {
            "columns": [
                {"name": "id", "dtype": "int64"},
                {"name": "name", "dtype": "object"}
            ]
        }
        stats_data = {
            "id": {"min": 1, "max": 100, "mean": 50.5}
        }
        preview_data = [
            {"id": 1, "name": "Test"}
        ]
        
        metrics = Metrics(
            upload_id=sample_upload.id,
            row_count=100,
            column_count=2,
            columns_json=columns_data,
            stats_json=stats_data,
            preview_json=preview_data
        )
        
        # Act
        db_session.add(metrics)
        await db_session.commit()
        await db_session.refresh(metrics)
        
        # Assert
        assert metrics.columns_json == columns_data
        assert metrics.stats_json == stats_data
        assert metrics.preview_json == preview_data
    
    @pytest.mark.asyncio
    async def test_metrics_upload_relationship(
        self, 
        db_session: AsyncSession, 
        sample_upload_with_metrics: Upload
    ):
        """
        Test the metrics-upload relationship navigation.
        """
        # Act - Query metrics and navigate to upload
        result = await db_session.execute(
            select(Metrics).where(Metrics.upload_id == sample_upload_with_metrics.id)
        )
        metrics = result.scalar_one()
        
        # Assert
        assert metrics.upload_id == sample_upload_with_metrics.id


class TestAIOutputModel:
    """Tests for the AIOutput ORM model."""
    
    @pytest.mark.asyncio
    async def test_create_ai_output(
        self, 
        db_session: AsyncSession, 
        sample_upload: Upload
    ):
        """
        Test basic AI output creation.
        """
        # Arrange
        ai_output = AIOutput(
            upload_id=sample_upload.id,
            ai_summary="Test summary of the data analysis.",
            model_used="llama3"
        )
        
        # Act
        db_session.add(ai_output)
        await db_session.commit()
        await db_session.refresh(ai_output)
        
        # Assert
        assert ai_output.id is not None
        assert ai_output.upload_id == sample_upload.id
        assert ai_output.ai_summary == "Test summary of the data analysis."
        assert ai_output.model_used == "llama3"
    
    @pytest.mark.asyncio
    async def test_ai_output_with_full_data(
        self, 
        db_session: AsyncSession, 
        sample_upload: Upload
    ):
        """
        Test AI output with all optional fields populated.
        """
        # Arrange
        ai_output = AIOutput(
            upload_id=sample_upload.id,
            ai_summary="Comprehensive analysis summary.",
            ai_recommendations={
                "actions": [
                    "Action item 1",
                    "Action item 2"
                ],
                "priority": "high"
            },
            insights_json=[
                "Insight about trend 1",
                "Insight about pattern 2",
                "Insight about anomaly 3"
            ],
            model_used="llama3",
            prompt_tokens=150,
            completion_tokens=250
        )
        
        # Act
        db_session.add(ai_output)
        await db_session.commit()
        await db_session.refresh(ai_output)
        
        # Assert
        assert ai_output.ai_recommendations["actions"][0] == "Action item 1"
        assert len(ai_output.insights_json) == 3
        assert ai_output.prompt_tokens == 150


class TestUserModel:
    """Tests for the User ORM model."""
    
    @pytest.mark.asyncio
    async def test_create_user(self, db_session: AsyncSession):
        """Test basic user creation."""
        # Arrange
        user = User(
            username="testuser",
            email="test@example.com",
            display_name="Test User"
        )
        
        # Act
        db_session.add(user)
        await db_session.commit()
        await db_session.refresh(user)
        
        # Assert
        assert user.id is not None
        assert user.username == "testuser"
        assert user.email == "test@example.com"
        assert user.is_active is True
    
    @pytest.mark.asyncio
    async def test_user_unique_username(self, db_session: AsyncSession):
        """Test that duplicate usernames are rejected."""
        # Arrange
        user1 = User(username="unique", email="user1@example.com")
        user2 = User(username="unique", email="user2@example.com")
        
        db_session.add(user1)
        await db_session.commit()
        
        # Act & Assert
        db_session.add(user2)
        with pytest.raises(Exception):  # IntegrityError
            await db_session.commit()


class TestPersistenceServiceUploads:
    """Tests for PersistenceService upload operations."""
    
    @pytest.mark.asyncio
    async def test_create_upload(
        self, 
        persistence_service: PersistenceService
    ):
        """Test creating an upload through the persistence service."""
        # Act
        upload = await persistence_service.create_upload(
            filename="persistence_test.csv",
            file_path="/uploads/persistence_test.csv",
            file_type="csv",
            file_size=2048
        )
        
        # Assert
        assert upload.id is not None
        assert upload.filename == "persistence_test.csv"
        assert upload.status == "uploaded"
    
    @pytest.mark.asyncio
    async def test_get_upload_by_id(
        self, 
        persistence_service: PersistenceService,
        sample_upload: Upload
    ):
        """Test retrieving an upload by ID."""
        # Act
        retrieved = await persistence_service.get_upload_by_id(sample_upload.id)
        
        # Assert
        assert retrieved is not None
        assert retrieved.id == sample_upload.id
        assert retrieved.filename == sample_upload.filename
    
    @pytest.mark.asyncio
    async def test_get_upload_not_found(
        self, 
        persistence_service: PersistenceService
    ):
        """Test that non-existent upload returns None."""
        # Act
        result = await persistence_service.get_upload_by_id(99999)
        
        # Assert
        assert result is None
    
    @pytest.mark.asyncio
    async def test_update_upload_status(
        self, 
        persistence_service: PersistenceService,
        sample_upload: Upload
    ):
        """Test updating upload status."""
        # Act
        await persistence_service.update_upload_status(
            sample_upload.id,
            "analyzed"
        )
        
        # Assert
        updated = await persistence_service.get_upload_by_id(sample_upload.id)
        assert updated.status == "analyzed"
    
    @pytest.mark.asyncio
    async def test_update_upload_status_with_error(
        self, 
        persistence_service: PersistenceService,
        sample_upload: Upload
    ):
        """Test updating upload status with error message."""
        # Act
        await persistence_service.update_upload_status(
            sample_upload.id,
            "failed",
            error_message="Parse error occurred"
        )
        
        # Assert
        updated = await persistence_service.get_upload_by_id(sample_upload.id)
        assert updated.status == "failed"
        assert updated.error_message == "Parse error occurred"
    
    @pytest.mark.asyncio
    async def test_list_uploads(
        self, 
        persistence_service: PersistenceService,
        db_session: AsyncSession
    ):
        """Test listing uploads with pagination."""
        # Arrange - create multiple uploads
        for i in range(5):
            upload = Upload(
                filename=f"file_{i}.csv",
                file_path=f"/uploads/file_{i}.csv",
                file_type="csv",
                file_size=100 * i
            )
            db_session.add(upload)
        await db_session.commit()
        
        # Act
        uploads, total = await persistence_service.list_uploads(
            limit=3,
            offset=0
        )
        
        # Assert
        assert len(uploads) == 3
        assert total == 5
    
    @pytest.mark.asyncio
    async def test_list_uploads_with_status_filter(
        self, 
        persistence_service: PersistenceService,
        db_session: AsyncSession
    ):
        """Test filtering uploads by status."""
        # Arrange
        upload1 = Upload(
            filename="parsed.csv",
            file_path="/test",
            file_type="csv",
            file_size=100,
            status="parsed"
        )
        upload2 = Upload(
            filename="analyzed.csv",
            file_path="/test2",
            file_type="csv",
            file_size=100,
            status="analyzed"
        )
        db_session.add_all([upload1, upload2])
        await db_session.commit()
        
        # Act
        uploads, total = await persistence_service.list_uploads(status="analyzed")
        
        # Assert
        assert total >= 1
        assert all(u.status == "analyzed" for u in uploads)
    
    @pytest.mark.asyncio
    async def test_delete_upload(
        self, 
        persistence_service: PersistenceService,
        sample_upload: Upload
    ):
        """Test deleting an upload."""
        # Arrange
        upload_id = sample_upload.id
        
        # Act
        await persistence_service.delete_upload(upload_id)
        
        # Assert
        deleted = await persistence_service.get_upload_by_id(upload_id)
        assert deleted is None


class TestPersistenceServiceMetrics:
    """Tests for PersistenceService metrics operations."""
    
    @pytest.mark.asyncio
    async def test_create_metrics(
        self, 
        persistence_service: PersistenceService,
        sample_upload: Upload
    ):
        """Test creating metrics for an upload."""
        # Act
        metrics = await persistence_service.create_metrics(
            upload_id=sample_upload.id,
            row_count=250,
            column_count=8,
            columns_json={"columns": [{"name": "test"}]},
            stats_json={"test": {"min": 0}},
            preview_json=[{"test": 1}]
        )
        
        # Assert
        assert metrics.id is not None
        assert metrics.upload_id == sample_upload.id
        assert metrics.row_count == 250
    
    @pytest.mark.asyncio
    async def test_get_metrics_by_upload_id(
        self, 
        persistence_service: PersistenceService,
        sample_upload_with_metrics: Upload
    ):
        """Test retrieving metrics by upload ID."""
        # Act
        metrics = await persistence_service.get_metrics_by_upload_id(
            sample_upload_with_metrics.id
        )
        
        # Assert
        assert metrics is not None
        assert metrics.row_count == 100


class TestPersistenceServiceAIOutput:
    """Tests for PersistenceService AI output operations."""
    
    @pytest.mark.asyncio
    async def test_create_ai_output(
        self, 
        persistence_service: PersistenceService,
        sample_upload: Upload
    ):
        """Test creating AI output for an upload."""
        # Act
        ai_output = await persistence_service.create_ai_output(
            upload_id=sample_upload.id,
            ai_summary="Test AI summary",
            model_used="llama3"
        )
        
        # Assert
        assert ai_output.id is not None
        assert ai_output.ai_summary == "Test AI summary"
    
    @pytest.mark.asyncio
    async def test_get_ai_output_by_upload_id(
        self, 
        persistence_service: PersistenceService,
        sample_upload_with_insights: Upload
    ):
        """Test retrieving AI output by upload ID."""
        # Act
        ai_output = await persistence_service.get_ai_output_by_upload_id(
            sample_upload_with_insights.id
        )
        
        # Assert
        assert ai_output is not None
        assert ai_output.model_used == "llama3"
    
    @pytest.mark.asyncio
    async def test_update_ai_output(
        self, 
        persistence_service: PersistenceService,
        sample_upload_with_insights: Upload
    ):
        """Test updating existing AI output."""
        # Act
        await persistence_service.update_ai_output(
            upload_id=sample_upload_with_insights.id,
            ai_summary="Updated summary",
            model_used="mistral"
        )
        
        # Assert
        updated = await persistence_service.get_ai_output_by_upload_id(
            sample_upload_with_insights.id
        )
        assert updated.ai_summary == "Updated summary"


class TestPersistenceServiceUploadHistory:
    """Tests for getting full upload history with related data."""
    
    @pytest.mark.asyncio
    async def test_get_upload_history(
        self, 
        persistence_service: PersistenceService,
        sample_upload_with_insights: Upload
    ):
        """Test getting complete upload history with all related records."""
        # Act
        history = await persistence_service.get_upload_history(
            sample_upload_with_insights.id
        )
        
        # Assert
        assert "upload" in history
        assert "metrics" in history
        assert "ai_output" in history
        assert history["upload"].id == sample_upload_with_insights.id
        assert history["metrics"] is not None
        assert history["ai_output"] is not None
    
    @pytest.mark.asyncio
    async def test_get_upload_history_not_found(
        self, 
        persistence_service: PersistenceService
    ):
        """Test that non-existent upload raises error."""
        # Act & Assert
        with pytest.raises(RecordNotFoundError):
            await persistence_service.get_upload_history(99999)
    
    @pytest.mark.asyncio
    async def test_get_upload_with_all_data(
        self, 
        persistence_service: PersistenceService,
        sample_upload_with_insights: Upload
    ):
        """Test getting upload with eager-loaded relationships."""
        # Act
        upload = await persistence_service.get_upload_with_all_data(
            sample_upload_with_insights.id
        )
        
        # Assert
        assert upload is not None
        assert upload.metrics is not None
        assert upload.ai_output is not None


class TestCascadeDeletes:
    """Tests for cascade delete behavior."""
    
    @pytest.mark.asyncio
    async def test_delete_upload_cascades_to_metrics(
        self, 
        db_session: AsyncSession,
        sample_upload_with_metrics: Upload
    ):
        """
        Test that deleting an upload also deletes its metrics.
        Cascade should handle related records automatically.
        """
        # Arrange
        upload_id = sample_upload_with_metrics.id
        
        # Verify metrics exist
        result = await db_session.execute(
            select(Metrics).where(Metrics.upload_id == upload_id)
        )
        assert result.scalar_one_or_none() is not None
        
        # Act - delete the upload
        await db_session.delete(sample_upload_with_metrics)
        await db_session.commit()
        
        # Assert - metrics should also be deleted
        result = await db_session.execute(
            select(Metrics).where(Metrics.upload_id == upload_id)
        )
        assert result.scalar_one_or_none() is None
    
    @pytest.mark.asyncio
    async def test_delete_upload_cascades_to_ai_output(
        self, 
        db_session: AsyncSession,
        sample_upload_with_insights: Upload
    ):
        """
        Test that deleting an upload also deletes its AI output.
        """
        # Arrange
        upload_id = sample_upload_with_insights.id
        
        # Verify AI output exists
        result = await db_session.execute(
            select(AIOutput).where(AIOutput.upload_id == upload_id)
        )
        assert result.scalar_one_or_none() is not None
        
        # Act
        await db_session.delete(sample_upload_with_insights)
        await db_session.commit()
        
        # Assert
        result = await db_session.execute(
            select(AIOutput).where(AIOutput.upload_id == upload_id)
        )
        assert result.scalar_one_or_none() is None
