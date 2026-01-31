"""Prompt templates for AI analysis."""

from typing import Optional


def get_analysis_prompt(data_summary: str, custom_prompt: Optional[str] = None) -> str:
    """
    Generate the analysis prompt for the AI model.
    
    Args:
        data_summary: Parsed data summary text
        custom_prompt: Optional user-provided custom instructions
        
    Returns:
        Complete prompt string
    """
    base_prompt = f"""Analyze the following data and provide insights:

{data_summary}

Please provide your analysis in the following format:

SUMMARY:
A brief executive summary of the data (2-3 sentences).

INSIGHTS:
- Key insight 1
- Key insight 2
- Key insight 3
(List 3-5 key findings, patterns, or anomalies)

RECOMMENDATIONS:
Actionable recommendations based on the data analysis.

Focus on:
1. Notable patterns or trends
2. Any anomalies or outliers
3. Correlations between data points
4. Potential business implications
5. Specific, actionable next steps"""

    if custom_prompt:
        base_prompt += f"""

ADDITIONAL INSTRUCTIONS:
{custom_prompt}"""
    
    return base_prompt


def get_summary_prompt(text: str) -> str:
    """Generate a summarization prompt for text content."""
    return f"""Summarize the following text content concisely:

{text}

Provide:
1. A brief summary (2-3 sentences)
2. Key points extracted
3. Any action items or important dates mentioned"""


def get_comparison_prompt(data1_summary: str, data2_summary: str) -> str:
    """Generate a comparison prompt for two datasets."""
    return f"""Compare the following two datasets:

DATASET 1:
{data1_summary}

DATASET 2:
{data2_summary}

Provide:
1. Key differences between the datasets
2. Common patterns
3. Which dataset shows better metrics (if applicable)
4. Recommendations based on the comparison"""


def get_trend_prompt(data_summary: str, time_column: Optional[str] = None) -> str:
    """Generate a trend analysis prompt."""
    time_context = f" using '{time_column}' as the time reference" if time_column else ""
    
    return f"""Analyze trends in the following data{time_context}:

{data_summary}

Provide:
1. Overall trend direction (increasing, decreasing, stable)
2. Any seasonal patterns
3. Notable inflection points
4. Forecast or prediction if sufficient data
5. Factors that might explain the trends"""
