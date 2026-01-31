"""
AI Service
==========
Handles communication with local Ollama LLM for AI inference.
Isolates all AI-related logic and provides a clean interface for routes.

Features:
    - Automatic retry on failure (configurable attempts)
    - Fallback to smaller models when primary model fails
    - Structured error responses for clean API handling
    - Comprehensive logging for debugging

Ollama Setup:
    1. Install Ollama: https://ollama.ai/download
    2. Pull a model: ollama pull llama3
    3. Start server: ollama serve
    4. Set OLLAMA_MODEL env var to your preferred model

Supported Models:
    - llama3 (default, recommended)
    - mistral (faster, good for quick analysis)
    - phi3 (lightweight, fastest)
    - codellama (code-focused)

Example Prompts:
    - Trend Analysis: "Identify trends and patterns in the data"
    - Anomaly Detection: "Highlight any anomalies or outliers"
    - Optimization: "Suggest optimizations based on the data"
"""

import os
import logging
from datetime import datetime, timezone
from typing import Optional, List, Dict, Any
from dataclasses import dataclass

import httpx


# -----------------------------------------------------------------------------
# Logging Configuration
# -----------------------------------------------------------------------------

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

# Add console handler if not already configured
if not logger.handlers:
    handler = logging.StreamHandler()
    handler.setFormatter(logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    ))
    logger.addHandler(handler)


# -----------------------------------------------------------------------------
# Error Classes
# -----------------------------------------------------------------------------

@dataclass
class AIError:
    """
    Structured error response for AI failures.
    Provides clean JSON-serializable error information.
    """
    error: str
    details: str
    model_attempted: str
    fallback_attempted: Optional[str] = None
    retry_count: int = 0
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "error": self.error,
            "details": self.details,
            "model_attempted": self.model_attempted,
            "fallback_attempted": self.fallback_attempted,
            "retry_count": self.retry_count
        }


class AIInferenceError(Exception):
    """
    Custom exception for AI inference failures.
    Carries structured error information for API responses.
    """
    def __init__(self, ai_error: AIError):
        self.ai_error = ai_error
        super().__init__(ai_error.error)


# -----------------------------------------------------------------------------
# Fallback Model Configuration
# -----------------------------------------------------------------------------

# Ordered list of fallback models (from larger to smaller)
# If primary model fails, try the next one in the list
FALLBACK_MODELS = [
    "llama3",      # Primary default
    "mistral",     # Good balance of speed/quality
    "phi3",        # Lightweight, fastest
    "llama3:8b",   # Smaller llama variant
    "tinyllama",   # Ultra-lightweight fallback
]


# -----------------------------------------------------------------------------
# Prompt Templates
# -----------------------------------------------------------------------------

# System prompts for different analysis modes
SYSTEM_PROMPTS = {
    "default": (
        "You are an expert data analyst. Analyze the provided data and generate "
        "actionable insights. Be concise, specific, and practical. Focus on patterns, "
        "anomalies, and recommendations that would be valuable for business decisions. "
        "Format your response with clear sections: SUMMARY, INSIGHTS, RECOMMENDATIONS."
    ),
    "trends": (
        "You are a trend analysis specialist. Focus on identifying temporal patterns, "
        "growth trajectories, seasonal variations, and emerging trends in the data. "
        "Provide specific percentages and comparisons where possible."
    ),
    "anomalies": (
        "You are an anomaly detection expert. Focus on identifying outliers, unusual patterns, "
        "data quality issues, and values that deviate significantly from expectations. "
        "Explain why each anomaly matters and potential causes."
    ),
    "optimization": (
        "You are a business optimization consultant. Focus on identifying inefficiencies, "
        "cost reduction opportunities, performance improvements, and actionable recommendations "
        "to optimize operations based on the data."
    ),
}

# Example analysis prompts that can be used as templates
EXAMPLE_PROMPTS = {
    "summarize_trends": """
Analyze the dataset and identify key trends:
1. What are the main patterns over time?
2. Are there any seasonal variations?
3. What is the overall direction (growth/decline)?
4. Which categories/segments show the strongest trends?
""",
    "detect_anomalies": """
Examine the data for anomalies and outliers:
1. Identify any values that deviate significantly from the norm
2. Look for unexpected patterns or breaks in continuity
3. Highlight potential data quality issues
4. Suggest which anomalies warrant further investigation
""",
    "suggest_optimizations": """
Based on the data, suggest optimizations:
1. What inefficiencies can you identify?
2. Where are the biggest opportunities for improvement?
3. What actions would have the highest impact?
4. Prioritize recommendations by effort vs. impact
""",
    "executive_summary": """
Provide an executive summary suitable for stakeholders:
1. Key metrics and their current state
2. Most important findings (positive and negative)
3. Critical action items
4. Recommended next steps
""",
}


# -----------------------------------------------------------------------------
# AI Service Class
# -----------------------------------------------------------------------------

class AIService:
    """
    Service for interacting with local Ollama instance.
    Handles prompt building, API calls, and response parsing.
    
    Features:
        - Automatic retry on transient failures
        - Fallback to smaller models when primary fails
        - Structured error responses
        - Comprehensive logging
    
    Usage:
        service = AIService()
        
        # Check if Ollama is running
        if await service.health_check():
            result = await service.analyze_data(data_summary)
    """
    
    def __init__(self):
        """Initialize AI service with configuration from environment."""
        self.base_url = os.getenv("OLLAMA_BASE_URL", "http://localhost:11434")
        self.model = os.getenv("OLLAMA_MODEL", "llama3")
        self.timeout = float(os.getenv("OLLAMA_TIMEOUT", "120"))
        self.max_retries = int(os.getenv("OLLAMA_MAX_RETRIES", "2"))
        self.enable_fallback = os.getenv("OLLAMA_ENABLE_FALLBACK", "true").lower() == "true"
        
        logger.info(f"AIService initialized: model={self.model}, url={self.base_url}, "
                    f"timeout={self.timeout}s, max_retries={self.max_retries}, "
                    f"fallback_enabled={self.enable_fallback}")
    
    # -------------------------------------------------------------------------
    # Core LLM Runner with Retry and Fallback
    # -------------------------------------------------------------------------
    
    async def run_llm(
        self,
        prompt: str,
        model: Optional[str] = None,
        system_prompt: Optional[str] = None,
        temperature: float = 0.7,
        max_tokens: int = 2000
    ) -> Dict[str, Any]:
        """
        Run LLM inference with automatic retry and fallback.
        
        This is the core method that handles:
        1. Primary model attempt with retries
        2. Automatic fallback to smaller models on failure
        3. Structured error responses if all attempts fail
        
        Args:
            prompt: User prompt/query
            model: Model to use (defaults to configured model)
            system_prompt: Optional system instructions
            temperature: Sampling temperature (0-1)
            max_tokens: Maximum tokens in response
            
        Returns:
            Dict with:
            - success: bool
            - response: str (if success)
            - model_used: str (actual model that generated response)
            - error: AIError dict (if failure)
            
        Example:
            result = await service.run_llm(
                prompt="Analyze this data...",
                model="llama3"
            )
            if result["success"]:
                print(result["response"])
            else:
                print(result["error"])
        """
        target_model = model or self.model
        last_error = None
        models_tried = []
        total_retries = 0
        
        # Build list of models to try (primary + fallbacks)
        models_to_try = [target_model]
        if self.enable_fallback:
            # Add fallback models that aren't already the primary
            for fallback in FALLBACK_MODELS:
                if fallback != target_model and fallback not in models_to_try:
                    models_to_try.append(fallback)
        
        logger.info(f"Starting LLM inference. Primary model: {target_model}, "
                    f"Fallback chain: {models_to_try[1:] if len(models_to_try) > 1 else 'disabled'}")
        logger.debug(f"Prompt (first 200 chars): {prompt[:200]}...")
        
        # Try each model in the chain
        for model_name in models_to_try:
            models_tried.append(model_name)
            
            # Retry loop for current model
            for attempt in range(1, self.max_retries + 1):
                total_retries += 1
                
                logger.info(f"Attempting inference: model={model_name}, attempt={attempt}/{self.max_retries}")
                
                try:
                    response = await self._call_ollama(
                        prompt=prompt,
                        model=model_name,
                        system_prompt=system_prompt,
                        temperature=temperature,
                        max_tokens=max_tokens
                    )
                    
                    # Success!
                    logger.info(f"Inference successful: model={model_name}, "
                                f"response_length={len(response)}")
                    
                    return {
                        "success": True,
                        "response": response,
                        "model_used": model_name,
                        "attempts": total_retries,
                        "fallback_used": model_name != target_model
                    }
                    
                except httpx.ConnectError as e:
                    # Connection error - Ollama not running, don't retry
                    error_msg = f"Cannot connect to Ollama at {self.base_url}"
                    logger.error(f"{error_msg}: {e}")
                    last_error = AIError(
                        error="AI inference failed",
                        details=f"{error_msg}. Ensure Ollama is running: ollama serve",
                        model_attempted=model_name,
                        retry_count=total_retries
                    )
                    # Connection error affects all models, so break completely
                    break
                    
                except httpx.TimeoutException as e:
                    # Timeout - might succeed with retry or smaller model
                    error_msg = f"Request timed out after {self.timeout}s"
                    logger.warning(f"{error_msg} (model={model_name}, attempt={attempt})")
                    last_error = AIError(
                        error="AI inference failed",
                        details=f"{error_msg}. Try a smaller model or increase OLLAMA_TIMEOUT.",
                        model_attempted=model_name,
                        retry_count=total_retries
                    )
                    continue  # Retry same model
                    
                except httpx.HTTPStatusError as e:
                    # HTTP error from Ollama
                    try:
                        error_body = e.response.json()
                        error_detail = error_body.get("error", str(e))
                    except Exception:
                        error_detail = e.response.text
                    
                    logger.error(f"Ollama HTTP error: {error_detail} (model={model_name})")
                    
                    # Check for specific recoverable errors
                    if "model runner has unexpectedly stopped" in error_detail.lower():
                        # Model crashed - try fallback model
                        logger.warning(f"Model runner crashed for {model_name}, trying fallback...")
                        last_error = AIError(
                            error="AI inference failed",
                            details=f"Model runner crashed: {error_detail}",
                            model_attempted=model_name,
                            retry_count=total_retries
                        )
                        break  # Move to next model in fallback chain
                        
                    elif "model" in error_detail.lower() and "not found" in error_detail.lower():
                        # Model not installed - skip to next model
                        logger.warning(f"Model {model_name} not found, trying fallback...")
                        last_error = AIError(
                            error="AI inference failed",
                            details=f"Model not found: {model_name}",
                            model_attempted=model_name,
                            retry_count=total_retries
                        )
                        break  # Move to next model in fallback chain
                        
                    else:
                        # Other HTTP error - retry
                        last_error = AIError(
                            error="AI inference failed",
                            details=error_detail,
                            model_attempted=model_name,
                            retry_count=total_retries
                        )
                        continue  # Retry same model
                        
                except Exception as e:
                    # Unexpected error
                    error_msg = f"Unexpected error: {type(e).__name__}: {str(e)}"
                    logger.exception(f"Unexpected error during inference: {e}")
                    last_error = AIError(
                        error="AI inference failed",
                        details=error_msg,
                        model_attempted=model_name,
                        retry_count=total_retries
                    )
                    continue  # Retry same model
            
            # If we're here, all retries for current model failed
            # Check if it was a connection error (affects all models)
            if isinstance(last_error, AIError) and "connect" in last_error.details.lower():
                break  # Don't try other models
        
        # All models and retries exhausted
        logger.error(f"All inference attempts failed. Models tried: {models_tried}, "
                     f"Total attempts: {total_retries}")
        
        if last_error:
            last_error.fallback_attempted = models_tried[-1] if len(models_tried) > 1 else None
        else:
            last_error = AIError(
                error="AI inference failed",
                details="Unknown error occurred",
                model_attempted=target_model,
                retry_count=total_retries
            )
        
        return {
            "success": False,
            "error": last_error.to_dict(),
            "models_tried": models_tried,
            "attempts": total_retries
        }
    
    async def _call_ollama(
        self,
        prompt: str,
        model: str,
        system_prompt: Optional[str] = None,
        temperature: float = 0.7,
        max_tokens: int = 2000
    ) -> str:
        """
        Make a single call to Ollama API.
        
        This is a low-level method called by run_llm().
        Raises exceptions on failure for run_llm() to handle.
        """
        payload = {
            "model": model,
            "prompt": prompt,
            "stream": False,
            "options": {
                "temperature": temperature,
                "num_predict": max_tokens,
            }
        }
        
        if system_prompt:
            payload["system"] = system_prompt
        
        async with httpx.AsyncClient(timeout=self.timeout) as client:
            response = await client.post(
                f"{self.base_url}/api/generate",
                json=payload
            )
            response.raise_for_status()
            result = response.json()
            return result.get("response", "")
    
    # -------------------------------------------------------------------------
    # Core Generation Method (Legacy - Now Uses run_llm)
    # -------------------------------------------------------------------------
    
    async def generate(
        self,
        prompt: str,
        system_prompt: Optional[str] = None,
        temperature: float = 0.7,
        max_tokens: int = 2000
    ) -> str:
        """
        Generate a response from the Ollama model.
        
        Now uses run_llm() internally for retry and fallback support.
        
        Args:
            prompt: User prompt/query
            system_prompt: Optional system instructions
            temperature: Sampling temperature (0-1, lower = more deterministic)
            max_tokens: Maximum tokens in response
            
        Returns:
            Generated text response
            
        Raises:
            ConnectionError: If Ollama is not available
            AIInferenceError: If generation fails after all retries
            
        Example:
            response = await service.generate(
                prompt="What are the key trends in this sales data?",
                system_prompt="You are a sales analyst.",
                temperature=0.5
            )
        """
        result = await self.run_llm(
            prompt=prompt,
            system_prompt=system_prompt,
            temperature=temperature,
            max_tokens=max_tokens
        )
        
        if result["success"]:
            return result["response"]
        else:
            # Raise structured error for callers to handle
            error_data = result["error"]
            raise AIInferenceError(AIError(
                error=error_data["error"],
                details=error_data["details"],
                model_attempted=error_data["model_attempted"],
                fallback_attempted=error_data.get("fallback_attempted"),
                retry_count=error_data["retry_count"]
            ))
    
    # -------------------------------------------------------------------------
    # Data Analysis Method
    # -------------------------------------------------------------------------
    
    async def analyze_data(
        self,
        data_summary: str,
        custom_prompt: Optional[str] = None,
        analysis_mode: str = "default"
    ) -> Dict[str, Any]:
        """
        Analyze data and generate structured insights.
        
        Uses run_llm() for automatic retry and fallback support.
        
        Args:
            data_summary: Human-readable summary of the data
            custom_prompt: Optional custom instructions for analysis
            analysis_mode: Type of analysis (default, trends, anomalies, optimization)
            
        Returns:
            Dictionary with:
            - summary: Executive summary text
            - insights: List of key insights
            - recommendations: Actionable recommendations
            - model_used: Model that generated the response
            - fallback_used: Whether a fallback model was used
            - created_at: Timestamp
            
        Raises:
            AIInferenceError: If all models and retries fail
            
        Example:
            result = await service.analyze_data(
                data_summary="Sales data with 1500 rows...",
                custom_prompt="Focus on regional performance",
                analysis_mode="trends"
            )
        """
        # Build the analysis prompt
        prompt = self._build_analysis_prompt(data_summary, custom_prompt)
        
        # Get system prompt for the analysis mode
        system_prompt = SYSTEM_PROMPTS.get(analysis_mode, SYSTEM_PROMPTS["default"])
        
        logger.info(f"Starting data analysis: mode={analysis_mode}, "
                    f"data_summary_length={len(data_summary)}")
        
        # Run LLM with retry and fallback
        result = await self.run_llm(
            prompt=prompt,
            system_prompt=system_prompt,
            temperature=0.7
        )
        
        if not result["success"]:
            # All attempts failed - raise structured error
            error_data = result["error"]
            logger.error(f"Data analysis failed: {error_data}")
            raise AIInferenceError(AIError(
                error=error_data["error"],
                details=error_data["details"],
                model_attempted=error_data["model_attempted"],
                fallback_attempted=error_data.get("fallback_attempted"),
                retry_count=error_data["retry_count"]
            ))
        
        # Parse structured response
        parsed = self._parse_analysis_response(result["response"])
        parsed["created_at"] = datetime.now(timezone.utc)
        parsed["model_used"] = result["model_used"]
        parsed["fallback_used"] = result.get("fallback_used", False)
        
        logger.info(f"Data analysis successful: model={result['model_used']}, "
                    f"fallback_used={result.get('fallback_used', False)}, "
                    f"insights_count={len(parsed['insights'])}")
        
        return parsed
    
    def _build_analysis_prompt(
        self,
        data_summary: str,
        custom_prompt: Optional[str] = None
    ) -> str:
        """
        Build the analysis prompt with data context.
        
        Includes:
        - Data overview and statistics
        - Sample rows for context
        - Structured output format instructions
        """
        prompt = f"""Analyze the following data and provide insights:

=== DATA OVERVIEW ===
{data_summary}

=== ANALYSIS REQUEST ===

Please provide your analysis in the following format:

SUMMARY:
Write a brief executive summary (2-3 sentences) covering the most important findings.

INSIGHTS:
- [Insight 1]: Description of first key finding
- [Insight 2]: Description of second key finding
- [Insight 3]: Description of third key finding
(List 3-5 key findings, patterns, or anomalies with specific numbers where possible)

RECOMMENDATIONS:
Based on your analysis, provide actionable recommendations for the business. 
Include specific next steps and priority level.

=== FOCUS AREAS ===
1. Notable patterns or trends in the data
2. Any anomalies, outliers, or data quality issues
3. Potential business implications
4. Specific, actionable optimization opportunities"""

        if custom_prompt:
            prompt += f"\n\n=== ADDITIONAL INSTRUCTIONS ===\n{custom_prompt}"
        
        return prompt
    
    def _parse_analysis_response(self, response: str) -> Dict[str, Any]:
        """
        Parse AI response into structured components.
        Handles variations in formatting from the LLM.
        """
        result = {
            "summary": "",
            "insights": [],
            "recommendations": ""
        }
        
        current_section = "summary"
        lines = response.strip().split("\n")
        
        for line in lines:
            line_lower = line.lower().strip()
            
            # Detect section headers
            if any(h in line_lower for h in ["summary:", "executive summary:"]):
                current_section = "summary"
                content = line.split(":", 1)[1].strip() if ":" in line else ""
                if content:
                    result["summary"] = content
                    
            elif any(h in line_lower for h in ["insights:", "key insights:", "key findings:"]):
                current_section = "insights"
                
            elif any(h in line_lower for h in ["recommendations:", "recommendation:", "action items:"]):
                current_section = "recommendations"
                content = line.split(":", 1)[1].strip() if ":" in line else ""
                if content:
                    result["recommendations"] = content
                    
            elif line.strip():
                # Add content to current section
                if current_section == "summary":
                    result["summary"] += " " + line.strip()
                elif current_section == "insights":
                    # Handle various bullet point styles
                    cleaned = line.strip().lstrip("-•*0123456789.[]").strip()
                    if cleaned and len(cleaned) > 10:  # Filter noise
                        result["insights"].append(cleaned)
                elif current_section == "recommendations":
                    result["recommendations"] += " " + line.strip()
        
        # Cleanup whitespace
        result["summary"] = result["summary"].strip()
        result["recommendations"] = result["recommendations"].strip()
        
        # Limit insights to top 5 most relevant
        result["insights"] = result["insights"][:5]
        
        # Fallback if parsing didn't extract meaningful content
        if not result["summary"] and not result["insights"]:
            result["summary"] = response[:500].strip() + "..." if len(response) > 500 else response.strip()
            result["insights"] = ["Analysis completed. See summary for details."]
            result["recommendations"] = "Review the summary above for insights."
        
        return result
    
    # -------------------------------------------------------------------------
    # Specialized Analysis Methods
    # -------------------------------------------------------------------------
    
    async def analyze_trends(self, data_summary: str) -> Dict[str, Any]:
        """Analyze data for trends and patterns."""
        return await self.analyze_data(
            data_summary=data_summary,
            custom_prompt=EXAMPLE_PROMPTS["summarize_trends"],
            analysis_mode="trends"
        )
    
    async def detect_anomalies(self, data_summary: str) -> Dict[str, Any]:
        """Analyze data for anomalies and outliers."""
        return await self.analyze_data(
            data_summary=data_summary,
            custom_prompt=EXAMPLE_PROMPTS["detect_anomalies"],
            analysis_mode="anomalies"
        )
    
    async def suggest_optimizations(self, data_summary: str) -> Dict[str, Any]:
        """Analyze data for optimization opportunities."""
        return await self.analyze_data(
            data_summary=data_summary,
            custom_prompt=EXAMPLE_PROMPTS["suggest_optimizations"],
            analysis_mode="optimization"
        )
    
    # -------------------------------------------------------------------------
    # Organization-Specific Analysis (Industry-Aware Recommendations)
    # -------------------------------------------------------------------------
    
    # Industry-specific recommendation templates
    INDUSTRY_RECOMMENDATIONS = {
        "apparel": [
            "Optimize e-commerce checkout flow with abandoned cart recovery sequences",
            "Invest in high-quality visual branding and lifestyle photography",
            "Implement virtual try-on or size recommendation AI tools",
            "Develop influencer partnership programs for social commerce",
            "Create personalized product recommendations based on browsing history",
        ],
        "fashion": [
            "Optimize e-commerce checkout flow with abandoned cart recovery sequences",
            "Invest in high-quality visual branding and lifestyle photography",
            "Implement virtual try-on or size recommendation AI tools",
            "Develop influencer partnership programs for social commerce",
            "Launch seasonal collection preview campaigns for VIP customers",
        ],
        "financial": [
            "Strengthen trust signals with security certifications and compliance badges",
            "Implement multi-factor authentication and secure UX patterns",
            "Add transparent fee disclosure and comparison calculators",
            "Develop educational content marketing for financial literacy",
            "Enhance mobile banking experience with biometric login",
        ],
        "banking": [
            "Strengthen trust signals with security certifications and compliance badges",
            "Implement real-time fraud detection and customer alerts",
            "Add transparent fee disclosure and comparison calculators",
            "Develop educational content marketing for financial literacy",
            "Create seamless omnichannel experience (branch + mobile + web)",
        ],
        "insurance": [
            "Simplify quote generation with intuitive multi-step forms",
            "Add claims tracking dashboards with real-time status updates",
            "Implement chatbots for 24/7 policy questions and support",
            "Develop personalized coverage recommendation engines",
            "Strengthen trust signals with customer testimonials and ratings",
        ],
        "food": [
            "Highlight sustainability initiatives and carbon footprint transparency",
            "Implement supply chain transparency with origin tracking",
            "Develop farm-to-table storytelling for brand authenticity",
            "Create subscription boxes for recurring revenue and loyalty",
            "Optimize local SEO for 'near me' food-related searches",
        ],
        "beverage": [
            "Highlight sustainability initiatives and packaging recycling programs",
            "Implement supply chain transparency with ingredient sourcing",
            "Develop direct-to-consumer e-commerce for premium products",
            "Create limited edition releases for brand excitement",
            "Partner with restaurants/bars for on-premise visibility",
        ],
        "technology": [
            "Prioritize website performance (sub-2s load times) for technical audience",
            "Implement interactive product demos and sandbox environments",
            "Develop developer documentation and API reference portals",
            "Create technical blog content for SEO and thought leadership",
            "Build community forums or Discord for user engagement",
        ],
        "software": [
            "Implement freemium or trial-based acquisition funnels",
            "Create interactive product tours and onboarding sequences",
            "Develop comprehensive documentation with code examples",
            "Build integration marketplace for ecosystem growth",
            "Prioritize performance monitoring and uptime transparency",
        ],
        "healthcare": [
            "Ensure HIPAA compliance and display trust certifications",
            "Implement patient portal with secure messaging",
            "Develop telehealth capabilities with easy scheduling",
            "Create educational health content for organic traffic",
            "Optimize for local SEO and 'near me' healthcare searches",
        ],
        "retail": [
            "Implement omnichannel inventory visibility (buy online, pick up in store)",
            "Develop loyalty program with personalized rewards",
            "Create shoppable social media content and live shopping events",
            "Optimize mobile shopping experience for 60%+ mobile traffic",
            "Implement dynamic pricing based on demand and inventory",
        ],
        "manufacturing": [
            "Develop digital product catalogs with 3D visualization",
            "Implement B2B e-commerce portal for repeat orders",
            "Create case studies showcasing operational efficiency gains",
            "Invest in IoT-enabled predictive maintenance marketing",
            "Build thought leadership content around Industry 4.0",
        ],
        "default": [
            "Improve website performance and mobile responsiveness",
            "Develop content marketing strategy for organic traffic",
            "Implement data-driven personalization across touchpoints",
            "Build email nurture sequences for lead conversion",
            "Invest in customer success and retention programs",
        ],
    }
    
    def _get_industry_recommendations(self, industry: str) -> List[str]:
        """Get industry-specific recommendations based on category."""
        industry_lower = industry.lower() if industry else ""
        
        # Match against known industries
        for key in self.INDUSTRY_RECOMMENDATIONS:
            if key in industry_lower:
                return self.INDUSTRY_RECOMMENDATIONS[key]
        
        return self.INDUSTRY_RECOMMENDATIONS["default"]
    
    def _get_company_profile_recommendations(
        self,
        founded_year: Optional[int],
        employee_count: Optional[int]
    ) -> List[str]:
        """Generate recommendations based on company maturity and size."""
        recommendations = []
        current_year = datetime.now().year
        
        # Age-based recommendations
        if founded_year:
            company_age = current_year - founded_year
            if company_age > 20:
                recommendations.extend([
                    "Prioritize digital transformation and legacy system modernization",
                    "Develop mobile-first customer experiences to match modern expectations",
                    "Consider brand refresh to maintain relevance with younger demographics",
                ])
            elif company_age > 10:
                recommendations.extend([
                    "Optimize existing digital channels for conversion rate improvement",
                    "Invest in customer data platform for unified insights",
                ])
            else:
                recommendations.extend([
                    "Focus on scalable infrastructure to support rapid growth",
                    "Develop thought leadership content to establish industry authority",
                    "Build strategic partnerships for market expansion",
                ])
        
        # Size-based recommendations
        if employee_count:
            if employee_count < 100:
                recommendations.extend([
                    "Leverage agility to outmaneuver larger competitors with niche focus",
                    "Invest in targeted, high-ROI marketing channels over broad campaigns",
                    "Build direct customer relationships for competitive advantage",
                ])
            elif employee_count < 500:
                recommendations.extend([
                    "Implement marketing automation to scale personalization",
                    "Develop departmental alignment between sales and marketing",
                ])
            elif employee_count < 5000:
                recommendations.extend([
                    "Invest in enterprise-grade analytics and attribution modeling",
                    "Develop regional marketing strategies for market-specific messaging",
                ])
            else:
                recommendations.extend([
                    "Prioritize global brand consistency across markets",
                    "Implement operational efficiency through process automation",
                    "Develop sustainability initiatives for ESG reporting",
                    "Consider M&A opportunities for capability expansion",
                ])
        
        return recommendations
    
    def _assess_website_quality(self, metrics: Dict[str, Any]) -> str:
        """Assess website quality based on available metrics."""
        # This would normally check page speed, mobile responsiveness, etc.
        # For now, we'll use a heuristic based on data completeness
        if not metrics:
            return "unknown"
        
        # Assume data with more columns indicates richer data = better website tracking
        column_count = metrics.get("column_count", 0)
        row_count = metrics.get("row_count", 0)
        
        if column_count >= 10 and row_count >= 100:
            return "high"
        elif column_count >= 5 and row_count >= 20:
            return "medium"
        else:
            return "low"
    
    async def analyze_organizations(
        self,
        data_summary: str,
        preview_data: List[Dict[str, Any]],
        columns: List[str],
        custom_prompt: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Analyze organization data with industry-specific, adaptive recommendations.
        
        This method:
        1. Detects organization metadata (industry, founded year, employee count)
        2. Generates tailored recommendations based on industry vertical
        3. Adapts advice based on company maturity and size
        4. Returns structured JSON for each organization
        
        Args:
            data_summary: Human-readable summary of the data
            preview_data: List of row dictionaries from the CSV
            columns: List of column names
            custom_prompt: Optional additional instructions
            
        Returns:
            Dictionary with:
            - organizations: List of organization analyses
            - summary: Overall dataset summary
            - model_used: Model that generated insights
        """
        logger.info(f"Starting organization analysis: {len(preview_data)} organizations")
        
        # Detect relevant columns
        columns_lower = [c.lower() for c in columns]
        name_col = next((c for c, cl in zip(columns, columns_lower) 
                         if any(k in cl for k in ["name", "organization", "company"])), None)
        industry_col = next((c for c, cl in zip(columns, columns_lower) 
                             if "industry" in cl), None)
        founded_col = next((c for c, cl in zip(columns, columns_lower) 
                            if any(k in cl for k in ["founded", "year", "established"])), None)
        employee_col = next((c for c, cl in zip(columns, columns_lower) 
                             if any(k in cl for k in ["employee", "size", "headcount"])), None)
        
        logger.info(f"Detected columns: name={name_col}, industry={industry_col}, "
                    f"founded={founded_col}, employees={employee_col}")
        
        # Process each organization
        organizations = []
        for row in preview_data[:10]:  # Limit to first 10 for performance
            org_name = str(row.get(name_col, "Unknown")) if name_col else "Unknown"
            industry = str(row.get(industry_col, "")) if industry_col else ""
            
            # Parse founded year
            founded_year = None
            if founded_col and row.get(founded_col):
                try:
                    founded_year = int(str(row[founded_col])[:4])
                except (ValueError, TypeError):
                    pass
            
            # Parse employee count
            employee_count = None
            if employee_col and row.get(employee_col):
                try:
                    emp_str = str(row[employee_col]).replace(",", "").replace("+", "")
                    employee_count = int(emp_str)
                except (ValueError, TypeError):
                    pass
            
            # Generate recommendations
            industry_recs = self._get_industry_recommendations(industry)
            profile_recs = self._get_company_profile_recommendations(founded_year, employee_count)
            
            # Combine and deduplicate
            all_recs = industry_recs[:3] + profile_recs[:2]
            
            organizations.append({
                "organization_name": org_name,
                "industry": industry or "Unknown",
                "founded_year": founded_year,
                "employee_count": employee_count,
                "website_quality": self._assess_website_quality(row),
                "recommendations": all_recs,
            })
        
        # Generate AI summary for the overall dataset
        summary_prompt = f"""Analyze this organization dataset and provide a brief summary:

{data_summary}

Focus on:
1. Industry distribution patterns
2. Company maturity (age) trends
3. Size segmentation insights
4. Overall digital optimization opportunities

Keep response under 200 words."""

        system_prompt = (
            "You are a business strategy consultant analyzing company data. "
            "Provide actionable insights focused on digital transformation and growth opportunities. "
            "Be specific and avoid generic advice."
        )
        
        result = await self.run_llm(
            prompt=summary_prompt,
            system_prompt=system_prompt,
            temperature=0.6
        )
        
        ai_summary = result.get("response", "") if result["success"] else "Analysis unavailable"
        
        return {
            "organizations": organizations,
            "summary": ai_summary,
            "total_analyzed": len(organizations),
            "model_used": result.get("model_used", self.model),
            "created_at": datetime.now(timezone.utc),
        }
    
    # -------------------------------------------------------------------------
    # Human-Style Narrative Analysis
    # -------------------------------------------------------------------------
    
    async def generate_narrative_insights(
        self,
        data_summary: str,
        preview_data: List[Dict[str, Any]],
        columns: List[str],
        num_insights: int = 3
    ) -> Dict[str, Any]:
        """
        Generate consultative, human-style narrative insights from organization data.
        
        Produces insights that feel like advice from a senior strategist:
        - Conversational language that sounds like a trusted advisor
        - Structured insights with: Key Finding → Why It Matters → What To Do → Priority
        - Adaptive recommendations based on industry, company age, and size
        - Both narrative paragraphs and structured JSON for APIs
        
        Args:
            data_summary: Human-readable summary of the data
            preview_data: List of row dictionaries from the CSV
            columns: List of column names
            num_insights: Number of insights to generate (default: 3)
            
        Returns:
            Dictionary with:
            - narrative: Human-readable report text
            - insights: List of structured insight objects
            - executive_summary: One-paragraph overview
            - model_used: Model that generated the response
        """
        logger.info(f"Generating narrative insights for {len(preview_data)} organizations")
        
        # Build rich context about the dataset
        context = self._build_narrative_context(preview_data, columns)
        
        # Build rule-based insights from the data first (reliable fallback)
        rule_based_insights = self._generate_rule_based_insights(preview_data, columns)
        
        # Generate clean executive summary from rule-based insights (more reliable than small LLMs)
        executive_summary = self._generate_executive_summary(rule_based_insights, context)
        
        # Generate categorized recommendations
        categorized_recommendations = self._categorize_recommendations(rule_based_insights, context)
        
        return {
            "narrative": f"Executive Briefing\n\n{executive_summary}\n\n" + "\n\n".join(
                f"**{ins['insight']}**\n{ins['implication']}\n\nRecommendation: {ins['recommendation']} (Priority: {ins['priority']})"
                for ins in rule_based_insights
            ),
            "insights": rule_based_insights,
            "executive_summary": executive_summary,
            "recommendations": categorized_recommendations,
            "top_recommendations": self._extract_top_recommendations(rule_based_insights),
            "model_used": "rule-based",
            "created_at": datetime.now(timezone.utc)
        }
    
    def _generate_rule_based_insights(
        self,
        preview_data: List[Dict[str, Any]],
        columns: List[str]
    ) -> List[Dict[str, Any]]:
        """
        Generate structured insights based on data patterns using deterministic rules.
        More reliable than LLM for smaller models.
        """
        from collections import Counter
        insights = []
        columns_lower = [c.lower() for c in columns]
        
        # Analyze founding years
        founded_col = next((c for c, cl in zip(columns, columns_lower) 
                            if any(k in cl for k in ["founded", "year", "established"])), None)
        current_year = datetime.now().year
        ages = []
        if founded_col:
            for row in preview_data:
                try:
                    year = int(str(row.get(founded_col, ""))[:4])
                    if 1700 <= year <= current_year:
                        ages.append(current_year - year)
                except:
                    pass
        
        # Analyze company sizes
        employee_col = next((c for c, cl in zip(columns, columns_lower) 
                             if any(k in cl for k in ["employee", "size", "headcount"])), None)
        sizes = []
        if employee_col:
            for row in preview_data:
                try:
                    emp_str = str(row.get(employee_col, "")).replace(",", "").replace("+", "")
                    sizes.append(int(emp_str))
                except:
                    pass
        
        # Analyze industries
        industry_col = next((c for c, cl in zip(columns, columns_lower) 
                             if "industry" in cl), None)
        industries = []
        if industry_col:
            industries = [str(row.get(industry_col, "")).split("/")[0].strip() 
                         for row in preview_data if row.get(industry_col)]
        
        # Detect tech stack / cloud / framework / creative mentions
        all_text = " ".join(str(v) for row in preview_data for v in row.values()).lower()
        
        cloud_detected = any(c in all_text for c in ["aws", "azure", "gcp", "cloud", "kubernetes", "docker"])
        ml_detected = any(c in all_text for c in ["machine learning", "ml", "ai", "tensorflow", "pytorch", "llm", "model"])
        data_detected = any(c in all_text for c in ["data", "analytics", "warehouse", "pipeline", "etl", "spark"])
        security_detected = any(c in all_text for c in ["security", "compliance", "gdpr", "soc2", "hipaa", "encryption"])
        
        # Creative AI domain detection
        art_detected = any(c in all_text for c in ["design", "image", "visual", "creative", "brand", "logo", "graphics", "midjourney", "dall-e", "stable diffusion"])
        writing_detected = any(c in all_text for c in ["content", "copywriting", "blog", "article", "documentation", "marketing copy", "seo content", "chatgpt", "claude"])
        code_detected = any(c in all_text for c in ["developer", "software", "github", "api", "programming", "code", "copilot", "cursor", "engineering"])
        audio_detected = any(c in all_text for c in ["audio", "podcast", "voice", "speech", "music", "transcription", "tts", "text-to-speech", "elevenlabs"])
        
        # Generate insights based on patterns
        
        # Insight 1: Company maturity
        if ages:
            avg_age = sum(ages) / len(ages)
            if avg_age > 50:
                insights.append({
                    "insight": f"These are predominantly legacy organizations, averaging {avg_age:.0f} years old",
                    "implication": "Older companies often face challenges with digital transformation, attracting younger talent, and modernizing legacy systems. Without proactive investment, they risk losing ground to more agile competitors.",
                    "recommendation": "Conduct a digital maturity assessment. Refresh the career pages and social media to showcase innovation. Consider reverse mentoring programs where younger employees help leadership understand digital trends.",
                    "priority": "High",
                    "domain": "Applied AI"
                })
            elif avg_age > 25:
                insights.append({
                    "insight": f"A mix of established and maturing companies, averaging {avg_age:.0f} years old",
                    "implication": "These organizations have built solid market positions but need to balance maintaining their core business while investing in growth and innovation.",
                    "recommendation": "Focus on operational efficiency while exploring adjacent market opportunities. Invest in employee development to retain institutional knowledge while bringing in fresh perspectives.",
                    "priority": "Medium",
                    "domain": "BI"
                })
            else:
                insights.append({
                    "insight": f"Relatively young organizations, averaging {avg_age:.0f} years old",
                    "implication": "Younger companies have agility advantages but may lack established processes and brand recognition. Scaling too fast without infrastructure can create operational challenges.",
                    "recommendation": "Prioritize building scalable systems and processes now. Invest in employer branding and SEO to build market presence. Consider strategic partnerships with established players.",
                    "priority": "High",
                    "domain": "Infra"
                })
        
        # Insight 2: Company size
        if sizes:
            avg_size = sum(sizes) / len(sizes)
            large_count = sum(1 for s in sizes if s >= 5000)
            small_count = sum(1 for s in sizes if s < 500)
            
            if large_count > len(sizes) * 0.4:
                insights.append({
                    "insight": f"Heavy concentration of large enterprises ({large_count} with 5,000+ employees)",
                    "implication": "Large organizations face unique challenges: maintaining culture at scale, coordinating across divisions, and avoiding bureaucratic slowdown. They also have resources for significant digital investments.",
                    "recommendation": "Focus on culture alignment initiatives and cross-functional collaboration tools. Large companies should leverage their resources for comprehensive digital transformation rather than piecemeal improvements.",
                    "priority": "Medium",
                    "domain": "Governance"
                })
            elif small_count > len(sizes) * 0.5:
                insights.append({
                    "insight": f"Predominantly smaller organizations ({small_count} under 500 employees)",
                    "implication": "Small companies have agility but limited resources. They need to be strategic about where they invest and may benefit from partnerships or outsourcing for specialized capabilities.",
                    "recommendation": "Prioritize high-impact, low-cost digital marketing (SEO, content, social). Consider fractional hires or agencies for specialized skills. Focus on targeted recruitment in key roles.",
                    "priority": "High",
                    "domain": "BI"
                })
        
        # Insight 3: Industry focus
        if industries:
            industry_counts = Counter(industries)
            top_industry = industry_counts.most_common(1)[0] if industry_counts else None
            
            traditional = ["Manufacturing", "Retail", "Food", "Insurance", "Banking"]
            is_traditional = any(t.lower() in (top_industry[0] if top_industry else "").lower() for t in traditional)
            
            if is_traditional and ages and sum(ages)/len(ages) > 40:
                insights.append({
                    "insight": f"Traditional industry players (led by {top_industry[0] if top_industry else 'various sectors'}) facing modernization pressure",
                    "implication": "Traditional industries are experiencing rapid disruption from digital-native competitors. Companies that don't modernize risk being outpaced by more agile market entrants.",
                    "recommendation": "Accelerate digital transformation initiatives. Update customer-facing systems (website, mobile, online ordering). Invest in analytics to compete on data-driven insights with newer competitors.",
                    "priority": "High",
                    "domain": "Applied AI"
                })
            elif top_industry:
                insights.append({
                    "insight": f"Industry concentration in {top_industry[0]} ({top_industry[1]} organizations)",
                    "implication": "Industry-specific expertise and best practices can be leveraged across this cohort. Common challenges likely include talent competition within the sector.",
                    "recommendation": "Benchmark against industry leaders for digital presence and employer branding. Consider industry-specific training and development programs. Build partnerships within the ecosystem.",
                    "priority": "Medium",
                    "domain": "BI"
                })
        
        # Insight 4: Tech stack / AI readiness (based on detected patterns)
        if ml_detected:
            insights.append({
                "insight": "AI/ML capabilities detected in the dataset",
                "implication": "Organizations with ML investments need robust MLOps practices to scale from experimentation to production. Model governance, monitoring, and retraining pipelines become critical.",
                "recommendation": "Implement MLOps best practices: version control for models, automated retraining pipelines, model monitoring dashboards, and A/B testing infrastructure.",
                "priority": "High",
                "domain": "MLOps"
            })
        elif cloud_detected:
            insights.append({
                "insight": "Cloud infrastructure adoption indicated",
                "implication": "Cloud-first organizations can leverage managed services to accelerate innovation but need strong governance to control costs and security.",
                "recommendation": "Establish FinOps practices for cost optimization. Implement infrastructure-as-code (Terraform/Pulumi) for reproducibility. Set up cloud security posture management.",
                "priority": "Medium",
                "domain": "Infra"
            })
        elif data_detected:
            insights.append({
                "insight": "Data infrastructure or analytics focus detected",
                "implication": "Data-driven organizations require robust pipelines, governance, and self-service analytics to democratize insights across teams.",
                "recommendation": "Invest in a modern data stack (ELT over ETL). Implement data catalogs for discoverability. Enable self-service BI tools for business users.",
                "priority": "Medium",
                "domain": "Data"
            })
        
        # Insight 5: Security/Compliance (if detected)
        if security_detected:
            insights.append({
                "insight": "Security and compliance requirements identified",
                "implication": "Regulated industries face increasing scrutiny. Non-compliance can result in fines, reputational damage, and operational disruption.",
                "recommendation": "Conduct a security audit and gap analysis. Implement automated compliance monitoring. Train staff on security best practices and incident response.",
                "priority": "High",
                "domain": "Security"
            })
        
        # Insight 6: Creative AI domains
        if code_detected and len(insights) < 5:
            insights.append({
                "insight": "Software development or engineering focus detected",
                "implication": "Development teams can dramatically accelerate delivery with AI-assisted coding tools. Code quality, review efficiency, and onboarding speed all improve with proper tooling.",
                "recommendation": "Adopt AI coding assistants (GitHub Copilot, Cursor). Implement automated code review with AI. Use AI for documentation generation and test case creation.",
                "priority": "High",
                "domain": "Code"
            })
        
        if writing_detected and len(insights) < 5:
            insights.append({
                "insight": "Content creation or marketing focus detected",
                "implication": "AI-powered writing tools can 10x content velocity while maintaining brand voice. SEO optimization, A/B testing copy, and personalization become scalable.",
                "recommendation": "Implement AI writing assistants for first drafts. Create brand voice guidelines for AI tools. Use AI for SEO optimization and content repurposing across channels.",
                "priority": "Medium",
                "domain": "Writing"
            })
        
        if art_detected and len(insights) < 5:
            insights.append({
                "insight": "Visual design or creative needs identified",
                "implication": "Generative AI is transforming creative workflows. Teams can iterate faster on concepts, generate variations at scale, and reduce time-to-market for visual assets.",
                "recommendation": "Explore AI image generation for concept work and mockups. Train teams on prompt engineering for visual AI. Establish guidelines for AI-generated asset usage and attribution.",
                "priority": "Medium",
                "domain": "Art"
            })
        
        if audio_detected and len(insights) < 5:
            insights.append({
                "insight": "Audio or voice technology needs detected",
                "implication": "Voice AI enables new interfaces and content formats. Podcasts, voice assistants, and audio content can be produced at scale with AI voice synthesis.",
                "recommendation": "Evaluate text-to-speech solutions for content accessibility. Consider AI-powered transcription for meetings and calls. Explore voice cloning for consistent brand voice.",
                "priority": "Medium",
                "domain": "Audio"
            })
        
        # Ensure we have at least 2 insights
        if len(insights) < 2:
            insights.append({
                "insight": f"Dataset includes {len(preview_data)} organizations across diverse characteristics",
                "implication": "A diverse portfolio requires tailored strategies rather than one-size-fits-all approaches. Understanding the unique needs of each segment is critical.",
                "recommendation": "Segment the organizations by size, age, and industry to develop targeted strategies. Prioritize quick wins for immediate impact while planning longer-term transformation initiatives.",
                "priority": "Medium",
                "domain": "BI"
            })
        
        return insights[:6]  # Return 2-6 insights for maximum domain breadth
    
    def _extract_top_recommendations(self, insights: List[Dict[str, Any]]) -> List[str]:
        """
        Extract top actionable recommendations from insights.
        Returns a prioritized list of clear next steps.
        """
        recommendations = []
        
        # Sort by priority (High first)
        priority_order = {"high": 0, "medium": 1, "low": 2}
        sorted_insights = sorted(
            insights, 
            key=lambda x: priority_order.get(x.get("priority", "medium").lower(), 1)
        )
        
        for insight in sorted_insights:
            rec = insight.get("recommendation", "").strip()
            if rec and len(rec) > 20:
                # Extract first actionable sentence if recommendation is long
                first_action = rec.split(". ")[0]
                if first_action and not first_action.endswith("."):
                    first_action += "."
                first_action = first_action.replace("..", ".")
                recommendations.append(first_action)
        
        return recommendations[:4]  # Return top 4 recommendations
    
    def _categorize_recommendations(
        self, 
        insights: List[Dict[str, Any]], 
        context: str
    ) -> Dict[str, List[Dict[str, str]]]:
        """
        Categorize recommendations into data quality, business strategy, industry opportunities,
        and technical opportunities across AI domains.
        """
        data_quality = []
        business_strategy = []
        industry_opportunities = []
        technical_opportunities = []
        
        # Check for data quality issues in context
        if "future dates" in context.lower() or "invalid" in context.lower():
            data_quality.append({
                "recommendation": "Review and correct date fields with future or invalid values",
                "priority": "High",
                "domain": "Data"
            })
        
        if "missing" in context.lower():
            data_quality.append({
                "recommendation": "Fill in missing data points to improve analysis accuracy",
                "priority": "Medium",
                "domain": "Data"
            })
        
        # Always recommend data validation
        data_quality.append({
            "recommendation": "Establish regular data quality audits to maintain accuracy over time",
            "priority": "Low",
            "domain": "Governance"
        })
        
        # Technical and creative AI domains for categorization
        tech_domains = ["AI/ML", "MLOps", "Security", "Infra", "Data", "Code", "Writing", "Art", "Audio"]
        
        # Extract from insights based on domain
        for insight in insights:
            rec = insight.get("recommendation", "")
            priority = insight.get("priority", "Medium")
            domain = insight.get("domain", "BI")
            insight_text = insight.get("insight", "").lower()
            
            first_rec = (rec.split(". ")[0] + ".").replace("..", ".") if rec else ""
            
            # Route based on domain
            if domain in tech_domains:
                technical_opportunities.append({
                    "recommendation": first_rec,
                    "priority": priority,
                    "domain": domain
                })
            elif any(k in insight_text for k in ["industry", "sector", "traditional", "modernization"]):
                industry_opportunities.append({
                    "recommendation": first_rec,
                    "priority": priority,
                    "domain": domain
                })
            else:
                business_strategy.append({
                    "recommendation": first_rec,
                    "priority": priority,
                    "domain": domain
                })
        
        return {
            "data_quality": data_quality[:3],
            "business_strategy": business_strategy[:3],
            "industry_opportunities": industry_opportunities[:3],
            "technical_opportunities": technical_opportunities[:4]
        }
    
    def _generate_executive_summary(self, insights: List[Dict[str, Any]], context: str) -> str:
        """
        Generate an executive summary from parsed insights if the LLM didn't provide one.
        Creates a consultative, actionable summary paragraph.
        """
        if not insights:
            return "Analysis complete. Review the detailed findings above for actionable recommendations."
        
        # Count priorities
        high_priority = [ins for ins in insights if ins.get("priority", "").lower() == "high"]
        
        # Extract the most important finding
        main_finding = ""
        if high_priority:
            main_finding = high_priority[0].get("insight", "")
        elif insights:
            main_finding = insights[0].get("insight", "")
        
        # Get the first sentence of the main finding (cleaner summary)
        if main_finding:
            first_sentence = main_finding.split(". ")[0]
            if len(first_sentence) > 150:
                first_sentence = first_sentence[:147] + "..."
        else:
            first_sentence = "Several important patterns emerged from this data"
        
        # Build actionable summary
        summary = f"This analysis reveals {len(insights)} key insights. {first_sentence}. "
        
        if high_priority:
            area_word = "area" if len(high_priority) == 1 else "areas"
            summary += f"We've identified {len(high_priority)} high-priority {area_word} requiring immediate attention. "
        
        # Add call to action
        recommendations = [ins.get("recommendation", "") for ins in insights if ins.get("recommendation")]
        if recommendations:
            summary += "Review the detailed recommendations above for specific next steps on digital optimization and business strategy."
        
        return summary
    
    def _build_narrative_context(
        self,
        preview_data: List[Dict[str, Any]],
        columns: List[str]
    ) -> str:
        """
        Build rich, human-readable context from the data for narrative generation.
        
        Extracts patterns, trends, and notable observations to feed to the LLM.
        """
        if not preview_data:
            return "No data available for analysis."
        
        from collections import Counter
        context_parts = []
        data_quality_issues = []  # Track data quality problems across all fields
        columns_lower = [c.lower() for c in columns]
        
        # -----------------------------------------------------------------
        # 1. Industry Distribution & Trends
        # -----------------------------------------------------------------
        industry_col = next((c for c, cl in zip(columns, columns_lower) 
                             if "industry" in cl), None)
        industry_insights = []
        top_industries = []
        if industry_col:
            industries = [str(row.get(industry_col, "")).split("/")[0].strip() 
                         for row in preview_data if row.get(industry_col)]
            if industries:
                industry_counts = Counter(industries)
                top_industries = industry_counts.most_common(5)
                total = len(industries)
                
                # Describe industry mix
                if len(industry_counts) == 1:
                    industry_insights.append(f"All organizations are in {top_industries[0][0]}")
                else:
                    top_pct = (top_industries[0][1] / total) * 100
                    if top_pct > 50:
                        industry_insights.append(f"{top_industries[0][0]} dominates ({top_pct:.0f}% of organizations)")
                    else:
                        top_3 = [f"{ind}" for ind, _ in top_industries[:3]]
                        industry_insights.append(f"Industry mix: primarily {', '.join(top_3)}")
                
                # Identify traditional vs. modern industries
                traditional = ["Manufacturing", "Retail", "Food", "Insurance", "Banking", "Utilities"]
                modern = ["Technology", "Software", "AI", "SaaS", "Fintech", "E-commerce"]
                trad_count = sum(c for ind, c in industry_counts.items() 
                                if any(t.lower() in ind.lower() for t in traditional))
                mod_count = sum(c for ind, c in industry_counts.items() 
                               if any(m.lower() in ind.lower() for m in modern))
                
                if trad_count > mod_count * 2:
                    industry_insights.append("Strong lean toward traditional/legacy industries")
                elif mod_count > trad_count * 2:
                    industry_insights.append("Predominantly modern/tech-forward companies")
        
        if industry_insights:
            context_parts.append("INDUSTRY PATTERNS:\n" + "\n".join(f"- {i}" for i in industry_insights))
        
        # -----------------------------------------------------------------
        # 2. Company Age & Maturity Analysis (with data quality checks)
        # -----------------------------------------------------------------
        founded_col = next((c for c, cl in zip(columns, columns_lower) 
                            if any(k in cl for k in ["founded", "year", "established"])), None)
        age_insights = []
        avg_age = None
        if founded_col:
            current_year = datetime.now().year
            ages = []
            decades = Counter()
            future_dates = 0
            invalid_dates = 0
            very_old = 0  # Before 1800
            
            for row in preview_data:
                try:
                    year = int(str(row.get(founded_col, ""))[:4])
                    
                    # Data quality checks
                    if year > current_year:
                        future_dates += 1
                        continue  # Skip invalid future dates
                    elif year < 1800:
                        very_old += 1
                        # Include but flag
                    
                    age = current_year - year
                    ages.append(age)
                    decade = (year // 10) * 10
                    decades[decade] += 1
                except (ValueError, TypeError):
                    invalid_dates += 1
            
            # Report data quality issues
            if future_dates > 0:
                data_quality_issues.append(f"DATA QUALITY: {future_dates} records have founding dates in the future - these appear to be data entry errors")
            if invalid_dates > len(preview_data) * 0.1:
                data_quality_issues.append(f"DATA QUALITY: {invalid_dates} records have missing or invalid founding year data")
            if very_old > 0:
                data_quality_issues.append(f"NOTE: {very_old} organizations founded before 1800 - verify historical accuracy")
            
            if ages:
                avg_age = sum(ages) / len(ages)
                oldest = max(ages)
                youngest = min(ages)
                
                # Age distribution insight
                if avg_age > 75:
                    age_insights.append(f"These are legacy organizations - averaging {avg_age:.0f} years old")
                    age_insights.append("Many likely face digital transformation challenges and succession planning needs")
                elif avg_age > 40:
                    age_insights.append(f"Established players with deep market roots (avg {avg_age:.0f} years)")
                    age_insights.append("Likely have strong brand equity but may need modernization")
                elif avg_age > 15:
                    age_insights.append(f"Maturing companies in growth phase (avg {avg_age:.0f} years)")
                else:
                    age_insights.append(f"Young, emerging organizations (avg {avg_age:.0f} years)")
                    age_insights.append("Likely focused on scaling and market capture")
                
                # Founding trend
                if decades:
                    recent = sum(v for k, v in decades.items() if k >= 2000)
                    older = sum(v for k, v in decades.items() if k < 2000)
                    if older > recent * 3:
                        age_insights.append("Most companies founded before 2000 - suggests legacy-heavy dataset")
                    elif recent > older * 2:
                        age_insights.append("Predominantly post-2000 companies - newer market entrants")
                
                age_insights.append(f"Age range: {youngest} to {oldest} years")
        
        if age_insights:
            context_parts.append("COMPANY MATURITY:\n" + "\n".join(f"- {i}" for i in age_insights))
        
        # -----------------------------------------------------------------
        # 3. Company Size & Scale Analysis
        # -----------------------------------------------------------------
        employee_col = next((c for c, cl in zip(columns, columns_lower) 
                             if any(k in cl for k in ["employee", "size", "headcount", "staff"])), None)
        size_insights = []
        if employee_col:
            sizes = []
            for row in preview_data:
                try:
                    emp_str = str(row.get(employee_col, ""))
                    # Handle range formats like "1000-5000"
                    if "-" in emp_str:
                        parts = emp_str.split("-")
                        emp = (int(parts[0].replace(",", "")) + int(parts[1].replace(",", ""))) // 2
                    else:
                        emp = int(emp_str.replace(",", "").replace("+", "").strip())
                    sizes.append(emp)
                except (ValueError, TypeError, IndexError):
                    pass
            
            if sizes:
                avg_size = sum(sizes) / len(sizes)
                median_size = sorted(sizes)[len(sizes)//2]
                
                micro = sum(1 for s in sizes if s < 50)
                small = sum(1 for s in sizes if 50 <= s < 500)
                medium = sum(1 for s in sizes if 500 <= s < 5000)
                large = sum(1 for s in sizes if s >= 5000)
                enterprise = sum(1 for s in sizes if s >= 50000)
                
                # Dominant size category
                total = len(sizes)
                if enterprise > total * 0.3:
                    size_insights.append(f"Heavy concentration of enterprise-scale organizations ({enterprise} with 50k+ employees)")
                elif large > total * 0.4:
                    size_insights.append(f"Predominantly large enterprises ({large} with 5,000+ employees)")
                elif small + micro > total * 0.5:
                    size_insights.append(f"Mostly smaller organizations ({small + micro} under 500 employees)")
                else:
                    size_insights.append("Diverse mix across company sizes")
                
                # Size distribution detail
                size_insights.append(f"Breakdown: {micro} micro (<50), {small} small, {medium} mid-size, {large} large")
                
                # Strategic implications
                if avg_size > 10000:
                    size_insights.append("Large company focus suggests B2B enterprise solutions may be relevant")
                elif avg_size < 200:
                    size_insights.append("Small company focus - agility and cost-efficiency are likely priorities")
        
        if size_insights:
            context_parts.append("COMPANY SIZE:\n" + "\n".join(f"- {i}" for i in size_insights))
        
        # -----------------------------------------------------------------
        # 4. Cross-Pattern Analysis
        # -----------------------------------------------------------------
        cross_insights = []
        
        # Old + Large = Legacy enterprise transformation opportunity
        if avg_age and avg_age > 50 and size_insights and "large" in " ".join(size_insights).lower():
            cross_insights.append("PATTERN: Old + large companies often need digital transformation and cultural change initiatives")
        
        # Young + Small = Startup growth patterns
        if avg_age and avg_age < 20 and size_insights and "small" in " ".join(size_insights).lower():
            cross_insights.append("PATTERN: Young + small suggests startup cohort - focus on scaling and market validation")
        
        # Traditional industry + Old = Modernization urgency
        if top_industries and industry_insights and "traditional" in " ".join(industry_insights).lower():
            if avg_age and avg_age > 40:
                cross_insights.append("PATTERN: Traditional industries with aging companies face pressing modernization needs")
        
        if cross_insights:
            context_parts.append("CROSS-PATTERN INSIGHTS:\n" + "\n".join(f"- {i}" for i in cross_insights))
        
        # -----------------------------------------------------------------
        # 5. Data Quality Issues (if any were found)
        # -----------------------------------------------------------------
        if data_quality_issues:
            context_parts.append("DATA QUALITY FLAGS:\n" + "\n".join(f"⚠️ {i}" for i in data_quality_issues))
        
        # -----------------------------------------------------------------
        # 6. Sample Organizations
        # -----------------------------------------------------------------
        name_col = next((c for c, cl in zip(columns, columns_lower) 
                         if any(k in cl for k in ["name", "organization", "company"])), None)
        if name_col:
            sample_names = [str(row.get(name_col, "")) for row in preview_data[:5] if row.get(name_col)]
            if sample_names:
                context_parts.append(f"SAMPLE ORGANIZATIONS: {', '.join(sample_names)}")
        
        return "\n\n".join(context_parts) if context_parts else "Dataset contains organizational information."
    
    def _parse_narrative_response(self, response: str) -> Dict[str, Any]:
        """
        Parse AI narrative response into structured format.
        
        Handles both structured (KEY INSIGHT: ...) and freeform narrative formats.
        Returns both the raw narrative and parsed insight objects.
        """
        result = {
            "narrative": response,
            "insights": [],
            "executive_summary": ""
        }
        
        lines = response.split("\n")
        current_insight = None
        current_field = None
        executive_started = False
        recommendations_started = False
        summary_lines = []
        recommendations_list = []
        
        for i, line in enumerate(lines):
            line_stripped = line.strip()
            line_lower = line_stripped.lower()
            
            # Skip empty lines unless we're building a field
            if not line_stripped:
                if current_field and current_insight:
                    current_field = None  # End of current field
                continue
            
            # Check for recommendations summary section
            if any(marker in line_lower for marker in ["recommendations summary:", "key recommendations:", "next steps:"]):
                # Save current insight before starting recommendations
                if current_insight and current_insight.get("finding"):
                    result["insights"].append(current_insight)
                    current_insight = None
                
                recommendations_started = True
                executive_started = False
                continue
            
            # Check for executive summary section
            if any(marker in line_lower for marker in ["executive summary:", "in summary:", "overall:"]):
                # Save current insight before starting summary
                if current_insight and current_insight.get("finding"):
                    result["insights"].append(current_insight)
                    current_insight = None
                
                recommendations_started = False
                executive_started = True
                # Extract content after the colon
                if ":" in line_stripped:
                    content = line_stripped.split(":", 1)[1].strip()
                    if content:
                        summary_lines.append(content)
                continue
            
            # Collect recommendations
            if recommendations_started:
                # Stop if we hit executive summary or new insight
                if any(marker in line_lower for marker in ["executive summary:", "key insight:"]):
                    recommendations_started = False
                elif line_stripped.startswith("-") or line_stripped.startswith("•") or line_stripped.startswith("*"):
                    rec = line_stripped.lstrip("-•* ").strip()
                    if rec:
                        recommendations_list.append(rec)
                    continue
                elif line_stripped and len(line_stripped) > 10:
                    recommendations_list.append(line_stripped)
                    continue
            
            # If we've started executive summary, collect lines
            if executive_started:
                # Stop if we hit a new section marker
                if any(marker in line_lower for marker in ["key insight:", "insight 1:", "insight 2:", "recommendations"]):
                    executive_started = False
                else:
                    summary_lines.append(line_stripped)
                    continue
            
            # Detect KEY INSIGHT marker (starts a new insight)
            if any(marker in line_lower for marker in ["key insight:", "insight:", "finding:"]):
                # Save previous insight
                if current_insight and current_insight.get("finding"):
                    result["insights"].append(current_insight)
                
                # Start new insight
                current_insight = {
                    "finding": "",
                    "why_it_matters": "",
                    "recommendation": "",
                    "priority": "Medium"
                }
                content = line_stripped.split(":", 1)[1].strip() if ":" in line_stripped else ""
                current_insight["finding"] = content
                current_field = "finding"
                continue
            
            # Detect WHY IT MATTERS marker
            if any(marker in line_lower for marker in ["why it matters:", "why this matters:", "business implication:", "implication:", "significance:"]):
                if current_insight:
                    content = line_stripped.split(":", 1)[1].strip() if ":" in line_stripped else ""
                    current_insight["why_it_matters"] = content
                    current_field = "why_it_matters"
                continue
            
            # Detect WHAT TO DO marker
            if any(marker in line_lower for marker in ["what to do:", "recommendation:", "action:", "next step:", "our advice:"]):
                if current_insight:
                    content = line_stripped.split(":", 1)[1].strip() if ":" in line_stripped else ""
                    current_insight["recommendation"] = content
                    current_field = "recommendation"
                continue
            
            # Detect PRIORITY marker
            if "priority:" in line_lower:
                if current_insight:
                    content = line_stripped.split(":", 1)[1].strip() if ":" in line_stripped else ""
                    if "high" in content.lower():
                        current_insight["priority"] = "High"
                    elif "low" in content.lower():
                        current_insight["priority"] = "Low"
                    else:
                        current_insight["priority"] = "Medium"
                    current_field = None
                continue
            
            # Continuation of current field
            if current_insight and current_field and line_stripped:
                current_val = current_insight.get(current_field, "")
                if current_val:
                    current_insight[current_field] = current_val + " " + line_stripped
                else:
                    current_insight[current_field] = line_stripped
        
        # Don't forget the last insight
        if current_insight and current_insight.get("finding"):
            result["insights"].append(current_insight)
        
        # Build executive summary from collected lines
        if summary_lines:
            result["executive_summary"] = " ".join(summary_lines).strip()
        # Note: If no summary was found, the parent function will generate one
        
        # Add recommendations list if captured
        if recommendations_list:
            result["top_recommendations"] = recommendations_list[:5]  # Limit to top 5
        
        # If parsing didn't extract structured insights, try alternate parsing
        if not result["insights"]:
            result["insights"] = self._fallback_parse_narrative(response)
        
        # Clean up insights
        for insight in result["insights"]:
            insight["finding"] = self._clean_insight_text(insight.get("finding", ""))
            insight["why_it_matters"] = self._clean_insight_text(insight.get("why_it_matters", ""))
            insight["recommendation"] = self._clean_insight_text(insight.get("recommendation", ""))
        
        # Remove empty or low-quality insights
        result["insights"] = [
            ins for ins in result["insights"] 
            if ins.get("finding") and len(ins.get("finding", "")) > 10 and not ins.get("finding", "").isdigit()
        ]
        
        # If we still don't have good insights, extract from narrative
        if not result["insights"] or all(len(ins.get("finding", "")) < 20 for ins in result["insights"]):
            result["insights"] = self._extract_insights_from_narrative(response)
        
        return result
    
    def _clean_insight_text(self, text: str) -> str:
        """Clean up insight text by removing artifacts and formatting issues."""
        if not text:
            return ""
        
        # Remove leading numbers like "1." or "1:"
        import re
        text = re.sub(r'^\d+[\.\:\)]\s*', '', text.strip())
        
        # Remove common prefixes
        prefixes_to_remove = [
            "key insight:", "insight:", "finding:", 
            "why it matters:", "business implication:",
            "what to do:", "recommendation:", "action:",
            "priority:", "---", "==="
        ]
        text_lower = text.lower()
        for prefix in prefixes_to_remove:
            if text_lower.startswith(prefix):
                text = text[len(prefix):].strip()
                text_lower = text.lower()
        
        # Remove excessive newlines and clean up
        text = re.sub(r'\n+', ' ', text)
        text = re.sub(r'\s+', ' ', text)
        
        return text.strip()
    
    def _extract_insights_from_narrative(self, response: str) -> List[Dict[str, Any]]:
        """
        Extract structured insights from a freeform narrative response.
        
        Uses heuristics to identify key statements and recommendations.
        """
        import re
        insights = []
        
        # Look for sentences that contain actionable or insight-like content
        sentences = re.split(r'[.!?]\s+', response)
        
        finding_keywords = ["most companies", "majority", "we found", "we're seeing", "data shows", 
                          "pattern", "trend", "notably", "interestingly", "key finding"]
        matter_keywords = ["this means", "this suggests", "impact", "challenge", "opportunity",
                         "struggle", "face", "need", "important because", "matters because"]
        action_keywords = ["should", "consider", "recommend", "focus on", "prioritize", 
                         "invest in", "update", "modernize", "improve", "develop"]
        
        current_insight = {"finding": "", "why_it_matters": "", "recommendation": "", "priority": "Medium"}
        
        for sentence in sentences:
            sentence = sentence.strip()
            if len(sentence) < 20:
                continue
            
            sentence_lower = sentence.lower()
            
            # Classify sentence
            if any(kw in sentence_lower for kw in finding_keywords) and not current_insight["finding"]:
                current_insight["finding"] = sentence
            elif any(kw in sentence_lower for kw in matter_keywords) and not current_insight["why_it_matters"]:
                current_insight["why_it_matters"] = sentence
            elif any(kw in sentence_lower for kw in action_keywords) and not current_insight["recommendation"]:
                current_insight["recommendation"] = sentence
                
                # Infer priority
                if any(word in sentence_lower for word in ["urgent", "immediate", "critical", "high priority"]):
                    current_insight["priority"] = "High"
                elif any(word in sentence_lower for word in ["eventually", "consider", "long-term", "optional"]):
                    current_insight["priority"] = "Low"
                
                # Save insight if we have at least finding and recommendation
                if current_insight["finding"] and current_insight["recommendation"]:
                    insights.append(current_insight.copy())
                    current_insight = {"finding": "", "why_it_matters": "", "recommendation": "", "priority": "Medium"}
        
        # If we have a partial insight, add it
        if current_insight["finding"]:
            insights.append(current_insight)
        
        # Return up to 3 insights, or fallback
        if insights:
            return insights[:3]
        
        return [{
            "finding": "Analysis complete - see narrative for details",
            "why_it_matters": "The full narrative above contains detailed business insights",
            "recommendation": "Review the executive summary and narrative for actionable next steps",
            "priority": "Medium"
        }]
    
    def _fallback_parse_narrative(self, response: str) -> List[Dict[str, Any]]:
        """
        Fallback parser for narrative responses that don't follow the structured format.
        
        Attempts to extract insights by looking for paragraph patterns.
        """
        insights = []
        
        # Split into paragraphs
        paragraphs = [p.strip() for p in response.split("\n\n") if p.strip()]
        
        for para in paragraphs:
            # Skip very short paragraphs or section headers
            if len(para) < 50 or para.startswith("===") or para.startswith("---"):
                continue
            
            # Skip if it looks like the executive summary
            if any(marker in para.lower()[:50] for marker in ["summary", "overall", "in conclusion"]):
                continue
            
            # Create an insight from the paragraph
            sentences = para.split(". ")
            
            if len(sentences) >= 2:
                insight = {
                    "finding": sentences[0].strip() + ".",
                    "why_it_matters": ". ".join(sentences[1:3]).strip(),
                    "recommendation": sentences[-1].strip() if len(sentences) > 3 else "Review the full analysis for specific recommendations.",
                    "priority": "Medium"
                }
                
                # Infer priority from keywords
                para_lower = para.lower()
                if any(word in para_lower for word in ["urgent", "critical", "immediate", "essential", "priority"]):
                    insight["priority"] = "High"
                elif any(word in para_lower for word in ["consider", "optional", "eventually", "long-term"]):
                    insight["priority"] = "Low"
                
                insights.append(insight)
        
        # Limit to 3 insights
        return insights[:3] if insights else [{
            "finding": "Analysis provided in narrative format above",
            "why_it_matters": "See the detailed narrative for business implications",
            "recommendation": "Review the full narrative for actionable recommendations",
            "priority": "Medium"
        }]
    
    # -------------------------------------------------------------------------
    # Utility Methods
    # -------------------------------------------------------------------------
    
    async def health_check(self) -> bool:
        """
        Check if Ollama is running and responsive.
        
        Returns:
            True if Ollama is available, False otherwise
        """
        async with httpx.AsyncClient(timeout=5.0) as client:
            try:
                response = await client.get(f"{self.base_url}/api/tags")
                return response.status_code == 200
            except Exception:
                return False
    
    async def list_models(self) -> List[str]:
        """
        List available models in Ollama.
        
        Returns:
            List of model names, empty list if unavailable
        """
        async with httpx.AsyncClient(timeout=10.0) as client:
            try:
                response = await client.get(f"{self.base_url}/api/tags")
                response.raise_for_status()
                result = response.json()
                return [m.get("name", "") for m in result.get("models", [])]
            except Exception:
                return []
    
    async def chat(
        self,
        messages: List[Dict[str, str]],
        temperature: float = 0.7
    ) -> str:
        """
        Chat completion with conversation history.
        
        Args:
            messages: List of {"role": "user|assistant|system", "content": "..."}
            temperature: Sampling temperature
            
        Returns:
            Assistant response text
            
        Example:
            response = await service.chat([
                {"role": "system", "content": "You are a data analyst."},
                {"role": "user", "content": "What patterns do you see?"},
            ])
        """
        payload = {
            "model": self.model,
            "messages": messages,
            "stream": False,
            "options": {
                "temperature": temperature,
            }
        }
        
        async with httpx.AsyncClient(timeout=self.timeout) as client:
            try:
                response = await client.post(
                    f"{self.base_url}/api/chat",
                    json=payload
                )
                response.raise_for_status()
                result = response.json()
                return result.get("message", {}).get("content", "")
            except httpx.ConnectError:
                raise ConnectionError(
                    f"Cannot connect to Ollama at {self.base_url}."
                )
    
    def get_example_prompts(self) -> Dict[str, str]:
        """
        Get example prompts for UI display.
        
        Returns:
            Dictionary of prompt name to prompt text
        """
        return EXAMPLE_PROMPTS.copy()
