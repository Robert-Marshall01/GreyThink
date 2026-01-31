"""Utility functions."""

import re
from datetime import datetime
from typing import Any, Dict, List


def sanitize_filename(filename: str) -> str:
    """Remove potentially dangerous characters from filename."""
    # Remove path separators and null bytes
    filename = re.sub(r'[/\\:\x00]', '', filename)
    # Remove leading/trailing spaces and dots
    filename = filename.strip('. ')
    # Limit length
    if len(filename) > 200:
        filename = filename[:200]
    return filename or "unnamed_file"


def format_file_size(size_bytes: int) -> str:
    """Format file size in human-readable format."""
    for unit in ['B', 'KB', 'MB', 'GB']:
        if size_bytes < 1024:
            return f"{size_bytes:.1f} {unit}"
        size_bytes /= 1024
    return f"{size_bytes:.1f} TB"


def truncate_string(s: str, max_length: int = 100, suffix: str = "...") -> str:
    """Truncate string to max length with suffix."""
    if len(s) <= max_length:
        return s
    return s[:max_length - len(suffix)] + suffix


def safe_json_serialize(obj: Any) -> Any:
    """Make objects JSON serializable."""
    if isinstance(obj, datetime):
        return obj.isoformat()
    elif isinstance(obj, (set, frozenset)):
        return list(obj)
    elif hasattr(obj, '__dict__'):
        return obj.__dict__
    return str(obj)


def extract_numeric_columns(data: List[Dict[str, Any]]) -> List[str]:
    """Extract column names that contain numeric values."""
    if not data:
        return []
    
    numeric_cols = []
    first_row = data[0]
    
    for key, value in first_row.items():
        if isinstance(value, (int, float)) and not isinstance(value, bool):
            numeric_cols.append(key)
    
    return numeric_cols


def calculate_basic_stats(values: List[float]) -> Dict[str, float]:
    """Calculate basic statistics for a list of numbers."""
    if not values:
        return {}
    
    sorted_values = sorted(values)
    n = len(sorted_values)
    
    return {
        "min": min(values),
        "max": max(values),
        "mean": sum(values) / n,
        "median": sorted_values[n // 2] if n % 2 else (sorted_values[n // 2 - 1] + sorted_values[n // 2]) / 2,
    }
