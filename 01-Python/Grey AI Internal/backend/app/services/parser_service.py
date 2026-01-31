"""Data parsing and metrics extraction service."""

import csv
import io
import json
from typing import Dict, List, Any, Optional

import pandas as pd
from sqlalchemy.ext.asyncio import AsyncSession

from app.models.orm import Upload, ParsedMetrics


class ParserService:
    """Handles parsing of uploaded files and metric extraction."""
    
    PREVIEW_ROWS = 10  # Number of rows to include in preview
    
    async def parse_file(self, upload: Upload, db: AsyncSession) -> ParsedMetrics:
        """
        Parse uploaded file and extract metrics.
        
        Args:
            upload: Upload ORM object
            db: Database session
            
        Returns:
            ParsedMetrics ORM object
        """
        # Read file content
        with open(upload.file_path, "r", encoding="utf-8", errors="replace") as f:
            content = f.read()
        
        # Parse based on file type
        if upload.file_type == "csv":
            metrics_data = self._parse_csv(content)
        elif upload.file_type == "txt":
            metrics_data = self._parse_text(content)
        else:
            raise ValueError(f"Unsupported file type: {upload.file_type}")
        
        # Create metrics record
        metrics = ParsedMetrics(
            upload_id=upload.id,
            row_count=metrics_data["row_count"],
            column_count=metrics_data["column_count"],
            columns_json=metrics_data.get("columns"),
            stats_json=metrics_data.get("stats"),
            preview_json=metrics_data.get("preview"),
        )
        
        # Update upload status
        upload.status = "parsed"
        
        db.add(metrics)
        await db.commit()
        await db.refresh(metrics)
        
        return metrics
    
    def _parse_csv(self, content: str) -> Dict[str, Any]:
        """Parse CSV content and extract metrics."""
        # Use pandas for robust CSV parsing
        df = pd.read_csv(io.StringIO(content))
        
        # Extract column information
        columns = []
        for col in df.columns:
            col_info = {
                "name": col,
                "dtype": str(df[col].dtype),
                "non_null_count": int(df[col].notna().sum()),
                "null_count": int(df[col].isna().sum()),
            }
            columns.append(col_info)
        
        # Compute basic stats for numeric columns
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
        
        # Get preview (first N rows)
        preview = df.head(self.PREVIEW_ROWS).to_dict(orient="records")
        # Convert any non-serializable values
        preview = json.loads(json.dumps(preview, default=str))
        
        return {
            "row_count": len(df),
            "column_count": len(df.columns),
            "columns": columns,
            "stats": stats,
            "preview": preview,
        }
    
    def _parse_text(self, content: str) -> Dict[str, Any]:
        """Parse text file and extract metrics."""
        lines = content.strip().split("\n")
        
        # Basic text metrics
        word_count = len(content.split())
        char_count = len(content)
        line_count = len(lines)
        
        # Try to detect if it's structured data (e.g., key: value pairs)
        structured_data = []
        for line in lines[:self.PREVIEW_ROWS]:
            if ":" in line:
                parts = line.split(":", 1)
                if len(parts) == 2:
                    structured_data.append({
                        "key": parts[0].strip(),
                        "value": parts[1].strip()
                    })
        
        stats = {
            "word_count": word_count,
            "char_count": char_count,
            "avg_line_length": char_count / line_count if line_count > 0 else 0,
        }
        
        # Preview is first N lines
        preview = [{"line": i + 1, "content": line} for i, line in enumerate(lines[:self.PREVIEW_ROWS])]
        
        return {
            "row_count": line_count,
            "column_count": 1,  # Text files treated as single column
            "columns": [{"name": "content", "dtype": "text"}],
            "stats": stats,
            "preview": preview,
        }
    
    def get_data_summary(self, metrics: ParsedMetrics) -> str:
        """Generate a text summary of the parsed data for AI context."""
        summary_parts = [
            f"Data Overview:",
            f"- Rows: {metrics.row_count}",
            f"- Columns: {metrics.column_count}",
        ]
        
        if metrics.columns_json:
            summary_parts.append("\nColumns:")
            for col in metrics.columns_json:
                summary_parts.append(f"  - {col.get('name', 'unknown')} ({col.get('dtype', 'unknown')})")
        
        if metrics.stats_json:
            summary_parts.append("\nStatistics:")
            for col_name, col_stats in metrics.stats_json.items():
                if isinstance(col_stats, dict):
                    stat_str = ", ".join(
                        f"{k}={v:.2f}" if isinstance(v, float) else f"{k}={v}"
                        for k, v in col_stats.items() if v is not None
                    )
                    summary_parts.append(f"  - {col_name}: {stat_str}")
                else:
                    summary_parts.append(f"  - {col_name}: {col_stats}")
        
        if metrics.preview_json:
            summary_parts.append(f"\nSample Data (first {len(metrics.preview_json)} rows):")
            for row in metrics.preview_json[:5]:  # Limit to 5 for AI context
                summary_parts.append(f"  {json.dumps(row, default=str)}")
        
        return "\n".join(summary_parts)
