"""Ollama API client for local LLM inference."""

from typing import Optional, Dict, Any

import httpx

from app.config import get_settings


class OllamaClient:
    """Client for interacting with local Ollama instance."""
    
    def __init__(self):
        self.settings = get_settings()
        self.base_url = self.settings.ollama_base_url
        self.model = self.settings.ollama_model
    
    async def generate(
        self,
        prompt: str,
        system_prompt: Optional[str] = None,
        model: Optional[str] = None,
        temperature: float = 0.7,
        max_tokens: int = 2000,
    ) -> str:
        """
        Generate a response from the Ollama model.
        
        Args:
            prompt: User prompt/query
            system_prompt: Optional system instructions
            model: Model to use (defaults to configured model)
            temperature: Sampling temperature (0-1)
            max_tokens: Maximum tokens in response
            
        Returns:
            Generated text response
            
        Raises:
            Exception: If Ollama request fails
        """
        model = model or self.model
        
        # Build request payload
        payload: Dict[str, Any] = {
            "model": model,
            "prompt": prompt,
            "stream": False,
            "options": {
                "temperature": temperature,
                "num_predict": max_tokens,
            },
        }
        
        if system_prompt:
            payload["system"] = system_prompt
        
        # Make request to Ollama
        async with httpx.AsyncClient(timeout=120.0) as client:
            try:
                response = await client.post(
                    f"{self.base_url}/api/generate",
                    json=payload,
                )
                response.raise_for_status()
                
                result = response.json()
                return result.get("response", "")
                
            except httpx.ConnectError:
                raise Exception(
                    f"Cannot connect to Ollama at {self.base_url}. "
                    "Ensure Ollama is running: `ollama serve`"
                )
            except httpx.TimeoutException:
                raise Exception(
                    "Ollama request timed out. The model may be loading or "
                    "processing a large request."
                )
            except httpx.HTTPStatusError as e:
                raise Exception(f"Ollama returned error: {e.response.text}")
    
    async def chat(
        self,
        messages: list,
        model: Optional[str] = None,
        temperature: float = 0.7,
    ) -> str:
        """
        Chat completion with conversation history.
        
        Args:
            messages: List of {"role": "user|assistant|system", "content": "..."}
            model: Model to use
            temperature: Sampling temperature
            
        Returns:
            Assistant response
        """
        model = model or self.model
        
        payload = {
            "model": model,
            "messages": messages,
            "stream": False,
            "options": {
                "temperature": temperature,
            },
        }
        
        async with httpx.AsyncClient(timeout=120.0) as client:
            try:
                response = await client.post(
                    f"{self.base_url}/api/chat",
                    json=payload,
                )
                response.raise_for_status()
                
                result = response.json()
                return result.get("message", {}).get("content", "")
                
            except httpx.ConnectError:
                raise Exception(
                    f"Cannot connect to Ollama at {self.base_url}. "
                    "Ensure Ollama is running."
                )
    
    async def list_models(self) -> list:
        """List available models in Ollama."""
        async with httpx.AsyncClient(timeout=30.0) as client:
            try:
                response = await client.get(f"{self.base_url}/api/tags")
                response.raise_for_status()
                
                result = response.json()
                return [m.get("name") for m in result.get("models", [])]
                
            except Exception:
                return []
    
    async def health_check(self) -> bool:
        """Check if Ollama is running and responsive."""
        async with httpx.AsyncClient(timeout=5.0) as client:
            try:
                response = await client.get(f"{self.base_url}/api/tags")
                return response.status_code == 200
            except Exception:
                return False
