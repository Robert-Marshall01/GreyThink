# =============================================================================
# Unit Tests - AI Service
# =============================================================================
# Tests for the AIService class which handles Ollama LLM integration.
# Uses mocking to avoid actual Ollama calls during testing.
#
# Run: pytest tests/unit/test_ai_service.py -v
# =============================================================================

import pytest
from unittest.mock import AsyncMock, patch, MagicMock
from typing import Dict, Any

from services.ai_service import AIService, SYSTEM_PROMPTS, EXAMPLE_PROMPTS


class TestAIServiceConfiguration:
    """Tests for AIService initialization and configuration."""
    
    def test_default_configuration(self, ai_service: AIService):
        """
        Test that AIService initializes with correct defaults.
        Defaults should match environment variables or fallbacks.
        """
        # Assert
        assert ai_service.base_url == "http://localhost:11434"
        assert ai_service.model == "llama3"
        assert ai_service.timeout == 120.0
    
    def test_custom_environment_configuration(self, monkeypatch):
        """
        Test that AIService respects environment variables.
        """
        # Arrange
        monkeypatch.setenv("OLLAMA_BASE_URL", "http://custom:11434")
        monkeypatch.setenv("OLLAMA_MODEL", "mistral")
        monkeypatch.setenv("OLLAMA_TIMEOUT", "60")
        
        # Act
        service = AIService()
        
        # Assert
        assert service.base_url == "http://custom:11434"
        assert service.model == "mistral"
        assert service.timeout == 60.0


class TestAIServiceGenerate:
    """Tests for the generate() method which calls Ollama API."""
    
    @pytest.mark.asyncio
    async def test_generate_success(
        self, 
        ai_service: AIService,
        mock_ollama_generate_response: Dict[str, Any]
    ):
        """
        Test successful generation call.
        Mocks the HTTP call to Ollama.
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
            result = await ai_service.generate(
                prompt="What are the key trends?",
                system_prompt="You are a data analyst."
            )
            
            # Assert
            assert isinstance(result, str)
            assert len(result) > 0
            assert "SUMMARY" in result or "Revenue" in result
    
    @pytest.mark.asyncio
    async def test_generate_with_custom_temperature(
        self, 
        ai_service: AIService,
        mock_ollama_generate_response: Dict[str, Any]
    ):
        """
        Test that temperature parameter is passed correctly.
        Lower temperature = more deterministic output.
        """
        # Arrange
        captured_payload = {}
        
        mock_response = AsyncMock()
        mock_response.status_code = 200
        mock_response.json.return_value = mock_ollama_generate_response
        mock_response.raise_for_status = MagicMock()
        
        async def capture_post(url, json):
            captured_payload.update(json)
            return mock_response
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.post = capture_post
            
            # Act
            await ai_service.generate(
                prompt="Test prompt",
                temperature=0.3
            )
            
            # Assert
            assert captured_payload["options"]["temperature"] == 0.3
    
    @pytest.mark.asyncio
    async def test_generate_connection_error(self, ai_service: AIService):
        """
        Test that connection errors are handled gracefully.
        Should raise ConnectionError with helpful message.
        """
        # Arrange
        import httpx
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.post = AsyncMock(
                side_effect=httpx.ConnectError("Connection refused")
            )
            
            # Act & Assert
            with pytest.raises(ConnectionError) as exc_info:
                await ai_service.generate(prompt="Test")
            
            assert "Cannot connect to Ollama" in str(exc_info.value)
    
    @pytest.mark.asyncio
    async def test_generate_timeout_error(self, ai_service: AIService):
        """
        Test that timeout errors are handled gracefully.
        """
        # Arrange
        import httpx
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.post = AsyncMock(
                side_effect=httpx.TimeoutException("Request timed out")
            )
            
            # Act & Assert
            with pytest.raises(RuntimeError) as exc_info:
                await ai_service.generate(prompt="Test")
            
            assert "timed out" in str(exc_info.value)


class TestAIServiceAnalyzeData:
    """Tests for the analyze_data() method with structured output."""
    
    @pytest.mark.asyncio
    async def test_analyze_data_returns_structured_output(
        self, 
        ai_service: AIService,
        mock_ollama_generate_response: Dict[str, Any]
    ):
        """
        Test that analyze_data returns properly structured insights.
        Should contain summary, insights list, and recommendations.
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
            result = await ai_service.analyze_data(
                data_summary="Sales data with 100 rows and 5 columns",
                custom_prompt=None
            )
            
            # Assert
            assert isinstance(result, dict)
            assert "summary" in result
            assert "insights" in result
            assert "recommendations" in result
            assert "created_at" in result
            assert isinstance(result["insights"], list)
    
    @pytest.mark.asyncio
    async def test_analyze_data_with_custom_prompt(
        self, 
        ai_service: AIService,
        mock_ollama_generate_response: Dict[str, Any]
    ):
        """
        Test that custom prompts are included in the analysis.
        """
        # Arrange
        captured_prompt = []
        
        mock_response = AsyncMock()
        mock_response.status_code = 200
        mock_response.json.return_value = mock_ollama_generate_response
        mock_response.raise_for_status = MagicMock()
        
        async def capture_post(url, json):
            captured_prompt.append(json.get("prompt", ""))
            return mock_response
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.post = capture_post
            
            # Act
            await ai_service.analyze_data(
                data_summary="Test data",
                custom_prompt="Focus on regional performance"
            )
            
            # Assert
            assert any("Focus on regional performance" in p for p in captured_prompt)
    
    @pytest.mark.asyncio
    async def test_analyze_data_with_analysis_mode(
        self, 
        ai_service: AIService,
        mock_ollama_generate_response: Dict[str, Any]
    ):
        """
        Test that different analysis modes use different system prompts.
        """
        # Arrange
        captured_systems = []
        
        mock_response = AsyncMock()
        mock_response.status_code = 200
        mock_response.json.return_value = mock_ollama_generate_response
        mock_response.raise_for_status = MagicMock()
        
        async def capture_post(url, json):
            captured_systems.append(json.get("system", ""))
            return mock_response
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.post = capture_post
            
            # Act - test different modes
            await ai_service.analyze_data(
                data_summary="Test",
                analysis_mode="trends"
            )
            
            # Assert
            # Should use trends-specific system prompt
            assert any("trend" in s.lower() for s in captured_systems)


class TestAIServiceParsing:
    """Tests for response parsing logic."""
    
    def test_parse_analysis_response_complete(self, ai_service: AIService):
        """
        Test parsing of well-formatted AI response.
        """
        # Arrange
        response = """SUMMARY:
The data shows strong growth patterns.

INSIGHTS:
- Revenue increased 15%
- Customer retention improved
- New markets opened

RECOMMENDATIONS:
Focus on scaling successful regions."""
        
        # Act
        result = ai_service._parse_analysis_response(response)
        
        # Assert
        assert "strong growth" in result["summary"].lower()
        assert len(result["insights"]) >= 2
        assert "scaling" in result["recommendations"].lower() or len(result["recommendations"]) > 0
    
    def test_parse_analysis_response_handles_variations(self, ai_service: AIService):
        """
        Test that parser handles different formatting variations.
        LLMs may format sections differently.
        """
        # Arrange - different format
        response = """Executive Summary: Performance metrics improved overall.

Key Findings:
* Finding one about revenue
* Finding two about customers
* Finding three about operations

Action Items:
Implement the suggested improvements."""
        
        # Act
        result = ai_service._parse_analysis_response(response)
        
        # Assert - should still extract content
        assert isinstance(result, dict)
        assert "summary" in result
        assert "insights" in result
        assert "recommendations" in result
    
    def test_parse_analysis_response_fallback(self, ai_service: AIService):
        """
        Test fallback behavior when parsing fails.
        Should use raw response as summary.
        """
        # Arrange - unstructured response
        response = "This is just a raw text response without proper sections."
        
        # Act
        result = ai_service._parse_analysis_response(response)
        
        # Assert
        assert result["summary"] != ""
        # Should have some fallback content
        assert "This is just" in result["summary"] or len(result["insights"]) > 0


class TestAIServiceSpecializedMethods:
    """Tests for specialized analysis methods."""
    
    @pytest.mark.asyncio
    async def test_analyze_trends(
        self, 
        ai_service: AIService,
        mock_ollama_generate_response: Dict[str, Any]
    ):
        """
        Test the analyze_trends convenience method.
        Should use trends-specific prompts.
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
            result = await ai_service.analyze_trends("Test data summary")
            
            # Assert
            assert isinstance(result, dict)
            assert "summary" in result
    
    @pytest.mark.asyncio
    async def test_detect_anomalies(
        self, 
        ai_service: AIService,
        mock_ollama_generate_response: Dict[str, Any]
    ):
        """
        Test the detect_anomalies convenience method.
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
            result = await ai_service.detect_anomalies("Test data summary")
            
            # Assert
            assert isinstance(result, dict)
    
    @pytest.mark.asyncio
    async def test_suggest_optimizations(
        self, 
        ai_service: AIService,
        mock_ollama_generate_response: Dict[str, Any]
    ):
        """
        Test the suggest_optimizations convenience method.
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
            result = await ai_service.suggest_optimizations("Test data summary")
            
            # Assert
            assert isinstance(result, dict)


class TestAIServiceHealthCheck:
    """Tests for health check and utility methods."""
    
    @pytest.mark.asyncio
    async def test_health_check_success(self, ai_service: AIService):
        """
        Test health check returns True when Ollama is available.
        """
        # Arrange
        mock_response = AsyncMock()
        mock_response.status_code = 200
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.get = AsyncMock(
                return_value=mock_response
            )
            
            # Act
            result = await ai_service.health_check()
            
            # Assert
            assert result is True
    
    @pytest.mark.asyncio
    async def test_health_check_failure(self, ai_service: AIService):
        """
        Test health check returns False when Ollama is unavailable.
        """
        # Arrange
        import httpx
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.get = AsyncMock(
                side_effect=httpx.ConnectError("Connection refused")
            )
            
            # Act
            result = await ai_service.health_check()
            
            # Assert
            assert result is False
    
    @pytest.mark.asyncio
    async def test_list_models_success(self, ai_service: AIService):
        """
        Test listing available models.
        """
        # Arrange
        mock_response = AsyncMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {
            "models": [
                {"name": "llama3"},
                {"name": "mistral"},
                {"name": "phi3"}
            ]
        }
        mock_response.raise_for_status = MagicMock()
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.get = AsyncMock(
                return_value=mock_response
            )
            
            # Act
            result = await ai_service.list_models()
            
            # Assert
            assert "llama3" in result
            assert "mistral" in result
    
    @pytest.mark.asyncio
    async def test_list_models_empty_on_error(self, ai_service: AIService):
        """
        Test that list_models returns empty list on error.
        """
        # Arrange
        import httpx
        
        with patch("httpx.AsyncClient") as mock_client:
            mock_client.return_value.__aenter__.return_value.get = AsyncMock(
                side_effect=httpx.ConnectError("Error")
            )
            
            # Act
            result = await ai_service.list_models()
            
            # Assert
            assert result == []
    
    def test_get_example_prompts(self, ai_service: AIService):
        """
        Test that example prompts are returned correctly.
        """
        # Act
        prompts = ai_service.get_example_prompts()
        
        # Assert
        assert isinstance(prompts, dict)
        assert "summarize_trends" in prompts
        assert "detect_anomalies" in prompts
        assert "suggest_optimizations" in prompts
        assert "executive_summary" in prompts


class TestPromptConstants:
    """Tests for prompt template constants."""
    
    def test_system_prompts_exist(self):
        """Test that all expected system prompts are defined."""
        assert "default" in SYSTEM_PROMPTS
        assert "trends" in SYSTEM_PROMPTS
        assert "anomalies" in SYSTEM_PROMPTS
        assert "optimization" in SYSTEM_PROMPTS
    
    def test_system_prompts_not_empty(self):
        """Test that system prompts have content."""
        for key, prompt in SYSTEM_PROMPTS.items():
            assert len(prompt) > 50, f"System prompt '{key}' is too short"
    
    def test_example_prompts_exist(self):
        """Test that all expected example prompts are defined."""
        assert "summarize_trends" in EXAMPLE_PROMPTS
        assert "detect_anomalies" in EXAMPLE_PROMPTS
        assert "suggest_optimizations" in EXAMPLE_PROMPTS
        assert "executive_summary" in EXAMPLE_PROMPTS
    
    def test_example_prompts_not_empty(self):
        """Test that example prompts have content."""
        for key, prompt in EXAMPLE_PROMPTS.items():
            assert len(prompt) > 50, f"Example prompt '{key}' is too short"
