"""AI-powered analysis service."""

from typing import Optional

from sqlalchemy.ext.asyncio import AsyncSession

from app.config import get_settings
from app.models.orm import ParsedMetrics, Insight
from app.services.parser_service import ParserService
from app.ai.ollama_client import OllamaClient
from app.ai.prompts import get_analysis_prompt


class AnalysisService:
    """Handles AI-powered data analysis using Ollama."""
    
    def __init__(self):
        self.settings = get_settings()
        self.ollama = OllamaClient()
        self.parser = ParserService()
    
    async def analyze(
        self,
        metrics: ParsedMetrics,
        db: AsyncSession,
        custom_prompt: Optional[str] = None,
    ) -> Insight:
        """
        Perform AI analysis on parsed data.
        
        Args:
            metrics: ParsedMetrics ORM object
            db: Database session
            custom_prompt: Optional custom analysis prompt
            
        Returns:
            Insight ORM object with AI-generated analysis
        """
        # Generate data summary for AI context
        data_summary = self.parser.get_data_summary(metrics)
        
        # Build the analysis prompt
        prompt = get_analysis_prompt(data_summary, custom_prompt)
        
        # Call Ollama for analysis
        response = await self.ollama.generate(
            prompt=prompt,
            system_prompt=(
                "You are an expert data analyst. Analyze the provided data and generate "
                "actionable insights. Be concise, specific, and practical. Focus on patterns, "
                "anomalies, and recommendations that would be valuable for business decisions."
            ),
        )
        
        # Parse the AI response
        parsed_response = self._parse_ai_response(response)
        
        # Create insight record
        insight = Insight(
            metrics_id=metrics.id,
            summary=parsed_response["summary"],
            insights_json=parsed_response["insights"],
            recommendations=parsed_response["recommendations"],
            model_used=self.settings.ollama_model,
        )
        
        db.add(insight)
        await db.commit()
        await db.refresh(insight)
        
        return insight
    
    def _parse_ai_response(self, response: str) -> dict:
        """
        Parse AI response into structured components.
        
        Expected format from AI:
        SUMMARY: ...
        INSIGHTS:
        - insight 1
        - insight 2
        RECOMMENDATIONS: ...
        """
        result = {
            "summary": "",
            "insights": [],
            "recommendations": "",
        }
        
        # Simple parsing - look for section markers
        current_section = "summary"
        lines = response.strip().split("\n")
        
        for line in lines:
            line_lower = line.lower().strip()
            
            if line_lower.startswith("summary:"):
                current_section = "summary"
                content = line.split(":", 1)[1].strip() if ":" in line else ""
                if content:
                    result["summary"] = content
            elif line_lower.startswith("insights:") or line_lower.startswith("key insights:"):
                current_section = "insights"
            elif line_lower.startswith("recommendations:") or line_lower.startswith("recommendation:"):
                current_section = "recommendations"
                content = line.split(":", 1)[1].strip() if ":" in line else ""
                if content:
                    result["recommendations"] = content
            elif line.strip():
                # Add content to current section
                if current_section == "summary":
                    result["summary"] += " " + line.strip()
                elif current_section == "insights":
                    # Handle bullet points
                    cleaned = line.strip().lstrip("-•*").strip()
                    if cleaned:
                        result["insights"].append(cleaned)
                elif current_section == "recommendations":
                    result["recommendations"] += " " + line.strip()
        
        # Cleanup
        result["summary"] = result["summary"].strip()
        result["recommendations"] = result["recommendations"].strip()
        
        # Fallback if parsing didn't work well
        if not result["summary"] and not result["insights"]:
            result["summary"] = response[:500] if len(response) > 500 else response
            result["insights"] = ["AI analysis completed. See summary for details."]
        
        return result
