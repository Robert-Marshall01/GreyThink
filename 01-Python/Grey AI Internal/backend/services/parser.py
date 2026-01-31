"""
Parser Service
==============
Handles parsing of uploaded files (CSV/text) into structured metrics.
Extracts statistics, column info, and preview data for downstream use.
"""

import csv
import io
import json
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional

# Optional: pandas for robust CSV parsing (falls back to csv module if not available)
try:
    import pandas as pd
    HAS_PANDAS = True
except ImportError:
    HAS_PANDAS = False


# -----------------------------------------------------------------------------
# Data Classes for Parsed Results
# -----------------------------------------------------------------------------

@dataclass
class ColumnInfo:
    """Information about a single column in the data."""
    name: str
    dtype: str
    non_null_count: int = 0
    null_count: int = 0


@dataclass
class ParsedMetrics:
    """
    Structured metrics extracted from a parsed file.
    Used as the return type for parse operations.
    """
    row_count: int
    column_count: int
    columns: List[ColumnInfo] = field(default_factory=list)
    stats: Dict[str, Dict[str, Any]] = field(default_factory=dict)
    preview: List[Dict[str, Any]] = field(default_factory=list)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            "row_count": self.row_count,
            "column_count": self.column_count,
            "columns": [
                {"name": c.name, "dtype": c.dtype, "non_null_count": c.non_null_count, "null_count": c.null_count}
                for c in self.columns
            ],
            "stats": self.stats,
            "preview": self.preview
        }


# -----------------------------------------------------------------------------
# Parser Service Class
# -----------------------------------------------------------------------------

class ParserService:
    """
    Service for parsing uploaded files into structured metrics.
    Supports CSV and plain text files.
    """
    
    # Number of rows to include in preview
    PREVIEW_ROWS = 10
    
    def parse(self, content: str, file_type: str) -> ParsedMetrics:
        """
        Parse file content and extract metrics.
        
        Args:
            content: Raw file content as string
            file_type: Type of file ("csv" or "txt")
            
        Returns:
            ParsedMetrics object with extracted data
            
        Raises:
            ValueError: If file type is unsupported or parsing fails
        """
        if file_type == "csv":
            return self._parse_csv(content)
        elif file_type == "txt":
            return self._parse_text(content)
        else:
            raise ValueError(f"Unsupported file type: {file_type}")
    
    def _parse_csv(self, content: str) -> ParsedMetrics:
        """
        Parse CSV content using pandas (if available) or stdlib csv.
        Extracts column info, statistics, and preview rows.
        """
        if HAS_PANDAS:
            return self._parse_csv_pandas(content)
        else:
            return self._parse_csv_stdlib(content)
    
    def _parse_csv_pandas(self, content: str) -> ParsedMetrics:
        """Parse CSV using pandas for robust handling and statistics."""
        df = pd.read_csv(io.StringIO(content))
        
        # Extract column information
        columns = []
        for col in df.columns:
            columns.append(ColumnInfo(
                name=str(col),
                dtype=str(df[col].dtype),
                non_null_count=int(df[col].notna().sum()),
                null_count=int(df[col].isna().sum())
            ))
        
        # Compute statistics for numeric columns
        stats = {}
        numeric_cols = df.select_dtypes(include=["number"]).columns
        for col in numeric_cols:
            stats[col] = {
                "min": float(df[col].min()) if pd.notna(df[col].min()) else None,
                "max": float(df[col].max()) if pd.notna(df[col].max()) else None,
                "mean": float(df[col].mean()) if pd.notna(df[col].mean()) else None,
                "median": float(df[col].median()) if pd.notna(df[col].median()) else None,
                "std": float(df[col].std()) if pd.notna(df[col].std()) else None,
            }
        
        # Get preview rows (convert to JSON-safe format)
        preview = df.head(self.PREVIEW_ROWS).to_dict(orient="records")
        preview = json.loads(json.dumps(preview, default=str))
        
        return ParsedMetrics(
            row_count=len(df),
            column_count=len(df.columns),
            columns=columns,
            stats=stats,
            preview=preview
        )
    
    def _parse_csv_stdlib(self, content: str) -> ParsedMetrics:
        """Fallback CSV parser using Python's csv module."""
        reader = csv.DictReader(io.StringIO(content))
        rows = list(reader)
        
        if not rows:
            return ParsedMetrics(row_count=0, column_count=0)
        
        # Extract column names
        columns = [
            ColumnInfo(name=col, dtype="string")
            for col in reader.fieldnames or []
        ]
        
        # Preview rows
        preview = rows[:self.PREVIEW_ROWS]
        
        return ParsedMetrics(
            row_count=len(rows),
            column_count=len(columns),
            columns=columns,
            stats={},  # Basic parser doesn't compute stats
            preview=preview
        )
    
    def _parse_text(self, content: str) -> ParsedMetrics:
        """
        Parse plain text content.
        Extracts line count, word count, and preview lines.
        """
        lines = content.strip().split("\n")
        line_count = len(lines)
        word_count = len(content.split())
        char_count = len(content)
        
        # Build preview
        preview = [
            {"line": i + 1, "content": line}
            for i, line in enumerate(lines[:self.PREVIEW_ROWS])
        ]
        
        # Text stats
        stats = {
            "text_metrics": {
                "word_count": word_count,
                "char_count": char_count,
                "avg_line_length": char_count / line_count if line_count > 0 else 0
            }
        }
        
        return ParsedMetrics(
            row_count=line_count,
            column_count=1,  # Text treated as single column
            columns=[ColumnInfo(name="content", dtype="text")],
            stats=stats,
            preview=preview
        )
    
    def get_data_summary(self, metrics: ParsedMetrics) -> str:
        """
        Generate a human-readable summary of parsed data.
        Used as context for AI analysis.
        
        Args:
            metrics: ParsedMetrics object
            
        Returns:
            Formatted string summary for AI context
        """
        summary_parts = [
            "Data Overview:",
            f"- Rows: {metrics.row_count}",
            f"- Columns: {metrics.column_count}",
        ]
        
        # Add column info
        if metrics.columns:
            summary_parts.append("\nColumns:")
            for col in metrics.columns:
                summary_parts.append(f"  - {col.name} ({col.dtype})")
        
        # Add statistics
        if metrics.stats:
            summary_parts.append("\nStatistics:")
            for col_name, col_stats in metrics.stats.items():
                if isinstance(col_stats, dict):
                    stat_str = ", ".join(
                        f"{k}={v:.2f}" if isinstance(v, float) else f"{k}={v}"
                        for k, v in col_stats.items()
                        if v is not None
                    )
                    summary_parts.append(f"  - {col_name}: {stat_str}")
        
        # Add preview
        if metrics.preview:
            summary_parts.append(f"\nSample Data (first {len(metrics.preview)} rows):")
            for row in metrics.preview[:5]:
                summary_parts.append(f"  {json.dumps(row, default=str)}")
        
        return "\n".join(summary_parts)
