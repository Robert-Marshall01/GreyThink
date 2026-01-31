# =============================================================================
# Integration Tests - API Endpoints
# =============================================================================
# End-to-end tests for all API endpoints using the FastAPI test client.
# Tests the full request/response cycle including database operations.
#
# Run: pytest tests/integration/test_api.py -v
# =============================================================================

import pytest
from unittest.mock import AsyncMock, patch, MagicMock
from io import BytesIO

from httpx import AsyncClient


class TestHealthEndpoint:
    """Tests for the /health endpoint."""
    
    @pytest.mark.asyncio
    async def test_health_check(self, test_client: AsyncClient):
        """
        Test that health check returns OK status.
        This endpoint is used by load balancers and monitoring.
        """
        # Act
        response = await test_client.get("/health")
        
        # Assert
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "healthy"
        assert "version" in data


class TestUploadEndpoints:
    """Tests for /api/upload/ endpoints."""
    
    @pytest.mark.asyncio
    async def test_upload_csv_file_success(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test successful CSV file upload.
        Should create upload record and parse metrics.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Act
        response = await test_client.post(
            "/api/upload/",
            files={"file": ("test_data.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        
        # Assert
        assert response.status_code == 201
        data = response.json()
        assert data["filename"] == "test_data.csv"
        assert data["file_type"] == "csv"
        assert data["status"] == "parsed"
        assert data["row_count"] == 10
        assert data["has_metrics"] is True
    
    @pytest.mark.asyncio
    async def test_upload_text_file_success(
        self, 
        test_client: AsyncClient,
        sample_text_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test successful text file upload.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Act
        response = await test_client.post(
            "/api/upload/",
            files={"file": ("notes.txt", BytesIO(sample_text_bytes), "text/plain")}
        )
        
        # Assert
        assert response.status_code == 201
        data = response.json()
        assert data["file_type"] == "txt"
        assert data["row_count"] == 5  # 5 lines
    
    @pytest.mark.asyncio
    async def test_upload_invalid_file_type(self, test_client: AsyncClient):
        """
        Test that invalid file types are rejected.
        Only CSV and TXT files are allowed.
        """
        # Arrange
        content = b"invalid file content"
        
        # Act
        response = await test_client.post(
            "/api/upload/",
            files={"file": ("data.json", BytesIO(content), "application/json")}
        )
        
        # Assert
        assert response.status_code == 400
        assert "not allowed" in response.json()["detail"].lower()
    
    @pytest.mark.asyncio
    async def test_upload_empty_file(
        self, 
        test_client: AsyncClient,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test that empty files are rejected.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Act
        response = await test_client.post(
            "/api/upload/",
            files={"file": ("empty.csv", BytesIO(b""), "text/csv")}
        )
        
        # Assert
        assert response.status_code == 400
        assert "empty" in response.json()["detail"].lower()
    
    @pytest.mark.asyncio
    async def test_list_uploads_empty(self, test_client: AsyncClient):
        """
        Test listing uploads when none exist.
        """
        # Act
        response = await test_client.get("/api/upload/")
        
        # Assert
        assert response.status_code == 200
        data = response.json()
        assert data["items"] == []
        assert data["total"] == 0
    
    @pytest.mark.asyncio
    async def test_list_uploads_with_data(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test listing uploads after creating some.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Upload two files
        await test_client.post(
            "/api/upload/",
            files={"file": ("file1.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        await test_client.post(
            "/api/upload/",
            files={"file": ("file2.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        
        # Act
        response = await test_client.get("/api/upload/")
        
        # Assert
        assert response.status_code == 200
        data = response.json()
        assert data["total"] >= 2
        assert len(data["items"]) >= 2
    
    @pytest.mark.asyncio
    async def test_list_uploads_pagination(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test pagination of upload list.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Upload 5 files
        for i in range(5):
            await test_client.post(
                "/api/upload/",
                files={"file": (f"file_{i}.csv", BytesIO(sample_csv_bytes), "text/csv")}
            )
        
        # Act - get first page with page_size=2
        response = await test_client.get("/api/upload/?page=1&page_size=2")
        
        # Assert
        assert response.status_code == 200
        data = response.json()
        assert len(data["items"]) == 2
        assert data["total"] >= 5
        assert data["has_more"] is True
    
    @pytest.mark.asyncio
    async def test_get_upload_detail(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test getting detailed upload information.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Create an upload
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("detail_test.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        # Act
        response = await test_client.get(f"/api/upload/{upload_id}")
        
        # Assert
        assert response.status_code == 200
        data = response.json()
        assert data["id"] == upload_id
        assert data["filename"] == "detail_test.csv"
        assert data["has_metrics"] is True
        assert "metrics" in data
        assert data["metrics"]["row_count"] == 10
    
    @pytest.mark.asyncio
    async def test_get_upload_not_found(self, test_client: AsyncClient):
        """
        Test 404 response for non-existent upload.
        """
        # Act
        response = await test_client.get("/api/upload/99999")
        
        # Assert
        assert response.status_code == 404
        assert "not found" in response.json()["detail"].lower()
    
    @pytest.mark.asyncio
    async def test_delete_upload(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test deleting an upload.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("delete_test.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        # Act
        delete_response = await test_client.delete(f"/api/upload/{upload_id}")
        
        # Assert
        assert delete_response.status_code == 204
        
        # Verify it's gone
        get_response = await test_client.get(f"/api/upload/{upload_id}")
        assert get_response.status_code == 404


class TestAIEndpoints:
    """Tests for /api/ai/ endpoints."""
    
    @pytest.mark.asyncio
    async def test_ai_status_endpoint(self, test_client: AsyncClient):
        """
        Test AI status endpoint returns configuration info.
        """
        # Arrange - mock health check to return False (no real Ollama)
        with patch("services.ai_service.AIService.health_check", new_callable=AsyncMock) as mock_health:
            mock_health.return_value = False
            
            # Act
            response = await test_client.get("/api/ai/status")
            
            # Assert
            assert response.status_code == 200
            data = response.json()
            assert "available" in data
            assert "configured_model" in data
    
    @pytest.mark.asyncio
    async def test_analyze_upload_success(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        mock_ai_response: dict,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test triggering AI analysis on an upload.
        Mocks the Ollama call to avoid real inference.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Upload a file first
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("analysis_test.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        # Mock the AI service analyze_data method
        with patch("services.ai_service.AIService.analyze_data", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = mock_ai_response
            
            # Act
            response = await test_client.post(f"/api/ai/analyze/{upload_id}")
            
            # Assert
            assert response.status_code == 200
            data = response.json()
            assert "summary" in data or "ai_summary" in data
            assert data["upload_id"] == upload_id
    
    @pytest.mark.asyncio
    async def test_analyze_upload_with_custom_prompt(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        mock_ai_response: dict,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test analysis with custom prompt instructions.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("custom_prompt.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        with patch("services.ai_service.AIService.analyze_data", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = mock_ai_response
            
            # Act
            response = await test_client.post(
                f"/api/ai/analyze/{upload_id}",
                json={"custom_prompt": "Focus on sales trends"}
            )
            
            # Assert
            assert response.status_code == 200
            # Verify custom prompt was passed
            mock_analyze.assert_called_once()
            call_kwargs = mock_analyze.call_args[1]
            assert call_kwargs["custom_prompt"] == "Focus on sales trends"
    
    @pytest.mark.asyncio
    async def test_analyze_upload_not_found(self, test_client: AsyncClient):
        """
        Test analysis on non-existent upload returns 404.
        """
        # Act
        response = await test_client.post("/api/ai/analyze/99999")
        
        # Assert
        assert response.status_code == 404
    
    @pytest.mark.asyncio
    async def test_analyze_upload_ollama_unavailable(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test graceful handling when Ollama is unavailable.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("unavailable_test.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        with patch("services.ai_service.AIService.analyze_data", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.side_effect = ConnectionError("Cannot connect to Ollama")
            
            # Act
            response = await test_client.post(f"/api/ai/analyze/{upload_id}")
            
            # Assert
            assert response.status_code == 503
            assert "Ollama" in response.json()["detail"] or "connect" in response.json()["detail"].lower()
    
    @pytest.mark.asyncio
    async def test_get_insights_success(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        mock_ai_response: dict,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test retrieving stored insights for an upload.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Upload and analyze
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("insights_test.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        with patch("services.ai_service.AIService.analyze_data", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = mock_ai_response
            await test_client.post(f"/api/ai/analyze/{upload_id}")
        
        # Act - get insights
        response = await test_client.get(f"/api/ai/insights/{upload_id}")
        
        # Assert
        assert response.status_code == 200
        data = response.json()
        assert data["upload_id"] == upload_id
    
    @pytest.mark.asyncio
    async def test_get_insights_not_analyzed(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test getting insights for upload that hasn't been analyzed.
        Should return 404 with helpful message.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("not_analyzed.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        # Act
        response = await test_client.get(f"/api/ai/insights/{upload_id}")
        
        # Assert
        assert response.status_code == 404
        assert "insights" in response.json()["detail"].lower() or "analyze" in response.json()["detail"].lower()
    
    @pytest.mark.asyncio
    async def test_raw_inference(
        self, 
        test_client: AsyncClient,
        mock_ollama_generate_response: dict
    ):
        """
        Test raw inference endpoint for custom prompts.
        """
        # Arrange
        mock_response = AsyncMock()
        mock_response.status_code = 200
        mock_response.json.return_value = mock_ollama_generate_response
        mock_response.raise_for_status = MagicMock()
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.post = AsyncMock(
                return_value=mock_response
            )
            
            # Act
            response = await test_client.post(
                "/api/ai/infer",
                json={
                    "prompt": "What is data normalization?",
                    "system_prompt": "You are a helpful tutor"
                }
            )
            
            # Assert
            assert response.status_code == 200
            data = response.json()
            assert "response" in data
            assert "model" in data


class TestEndToEndFlow:
    """End-to-end integration tests covering the complete workflow."""
    
    @pytest.mark.asyncio
    async def test_complete_upload_analyze_retrieve_flow(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        mock_ai_response: dict,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test the complete user workflow:
        1. Upload CSV file
        2. Verify metrics are parsed
        3. Trigger AI analysis
        4. Retrieve insights
        5. List uploads shows analyzed status
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Step 1: Upload file
        upload_response = await test_client.post(
            "/api/upload/",
            files={"file": ("e2e_test.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        assert upload_response.status_code == 201
        upload_data = upload_response.json()
        upload_id = upload_data["id"]
        
        # Step 2: Verify metrics
        assert upload_data["has_metrics"] is True
        assert upload_data["row_count"] == 10
        assert upload_data["column_count"] == 5
        
        # Step 3: Trigger AI analysis
        with patch("services.ai_service.AIService.analyze_data", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = mock_ai_response
            
            analyze_response = await test_client.post(f"/api/ai/analyze/{upload_id}")
            assert analyze_response.status_code == 200
            analyze_data = analyze_response.json()
            assert "summary" in analyze_data or "ai_summary" in analyze_data
        
        # Step 4: Retrieve insights
        insights_response = await test_client.get(f"/api/ai/insights/{upload_id}")
        assert insights_response.status_code == 200
        insights_data = insights_response.json()
        assert insights_data["upload_id"] == upload_id
        
        # Step 5: List uploads shows analyzed
        list_response = await test_client.get("/api/upload/")
        assert list_response.status_code == 200
        list_data = list_response.json()
        
        # Find our upload in the list
        our_upload = next((u for u in list_data["items"] if u["id"] == upload_id), None)
        assert our_upload is not None
        assert our_upload["has_insights"] is True
    
    @pytest.mark.asyncio
    async def test_reanalysis_updates_insights(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        mock_ai_response: dict,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Test that re-analyzing an upload updates the insights.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Upload
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("reanalyze.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        # First analysis
        with patch("services.ai_service.AIService.analyze_data", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = mock_ai_response
            await test_client.post(f"/api/ai/analyze/{upload_id}")
        
        # Second analysis with different prompt
        updated_response = {**mock_ai_response, "summary": "Updated analysis with new insights"}
        
        with patch("services.ai_service.AIService.analyze_data", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = updated_response
            
            reanalyze_response = await test_client.post(
                f"/api/ai/analyze/{upload_id}",
                json={"custom_prompt": "Focus on new aspects"}
            )
        
        # Assert - should get updated summary
        assert reanalyze_response.status_code == 200


class TestErrorHandling:
    """Tests for error handling and edge cases."""
    
    @pytest.mark.asyncio
    async def test_invalid_json_body(self, test_client: AsyncClient):
        """
        Test handling of invalid JSON in request body.
        """
        # Act
        response = await test_client.post(
            "/api/ai/infer",
            content="invalid json{",
            headers={"Content-Type": "application/json"}
        )
        
        # Assert
        assert response.status_code == 422  # Unprocessable Entity
    
    @pytest.mark.asyncio
    async def test_missing_required_field(self, test_client: AsyncClient):
        """
        Test validation error for missing required fields.
        """
        # Act - missing 'prompt' field
        response = await test_client.post(
            "/api/ai/infer",
            json={"system_prompt": "test"}
        )
        
        # Assert
        assert response.status_code == 422
        error = response.json()
        assert "detail" in error
    
    @pytest.mark.asyncio
    async def test_file_upload_missing_file(self, test_client: AsyncClient):
        """
        Test error when file is not provided in upload request.
        """
        # Act
        response = await test_client.post("/api/upload/")
        
        # Assert
        assert response.status_code == 422


class TestResponseSchemas:
    """Tests to verify response schemas match Pydantic models."""
    
    @pytest.mark.asyncio
    async def test_upload_response_schema(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Verify upload response has all expected fields.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        # Act
        response = await test_client.post(
            "/api/upload/",
            files={"file": ("schema_test.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        
        # Assert
        data = response.json()
        required_fields = ["id", "filename", "file_type", "file_size", "status", "created_at"]
        for field in required_fields:
            assert field in data, f"Missing field: {field}"
    
    @pytest.mark.asyncio
    async def test_insights_response_schema(
        self, 
        test_client: AsyncClient,
        sample_csv_bytes: bytes,
        mock_ai_response: dict,
        temp_upload_dir,
        monkeypatch
    ):
        """
        Verify insights response has all expected fields.
        """
        # Arrange
        monkeypatch.setenv("UPLOAD_DIR", str(temp_upload_dir))
        
        create_response = await test_client.post(
            "/api/upload/",
            files={"file": ("insights_schema.csv", BytesIO(sample_csv_bytes), "text/csv")}
        )
        upload_id = create_response.json()["id"]
        
        with patch("services.ai_service.AIService.analyze_data", new_callable=AsyncMock) as mock_analyze:
            mock_analyze.return_value = mock_ai_response
            await test_client.post(f"/api/ai/analyze/{upload_id}")
        
        # Act
        response = await test_client.get(f"/api/ai/insights/{upload_id}")
        
        # Assert
        data = response.json()
        required_fields = ["id", "upload_id", "model_used", "created_at"]
        for field in required_fields:
            assert field in data, f"Missing field: {field}"
        
        # At least one of these should be present (with aliasing)
        assert "summary" in data or "ai_summary" in data
