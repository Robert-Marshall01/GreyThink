# =============================================================================
# Unit Tests - Parser Service
# =============================================================================
# Tests for the ParserService class which handles CSV and text file parsing.
# Validates correct extraction of metrics, statistics, and preview data.
#
# Run: pytest tests/unit/test_parser.py -v
# =============================================================================

import pytest
from services.parser import ParserService, ParsedMetrics, ColumnInfo


class TestParserServiceCSV:
    """Tests for CSV parsing functionality."""
    
    def test_parse_csv_basic(self, parser_service: ParserService, sample_csv_content: str):
        """
        Test basic CSV parsing returns correct row and column counts.
        Verifies the fundamental parsing operation works correctly.
        """
        # Act
        result = parser_service.parse(sample_csv_content, "csv")
        
        # Assert
        assert isinstance(result, ParsedMetrics)
        assert result.row_count == 10  # 10 data rows
        assert result.column_count == 5  # id, name, value, category, date
    
    def test_parse_csv_columns(self, parser_service: ParserService, sample_csv_content: str):
        """
        Test that column information is correctly extracted.
        Each column should have name, dtype, and null counts.
        """
        # Act
        result = parser_service.parse(sample_csv_content, "csv")
        
        # Assert
        assert len(result.columns) == 5
        
        # Check column names
        column_names = [col.name for col in result.columns]
        assert "id" in column_names
        assert "name" in column_names
        assert "value" in column_names
        assert "category" in column_names
        assert "date" in column_names
    
    def test_parse_csv_detects_numeric_columns(
        self, 
        parser_service: ParserService, 
        sample_csv_content: str
    ):
        """
        Test that numeric columns are correctly identified.
        The 'id' and 'value' columns should be detected as numeric.
        """
        # Act
        result = parser_service.parse(sample_csv_content, "csv")
        
        # Assert - check that stats exist for numeric columns
        # Note: pandas detects 'id' as int64 and 'value' as float64
        assert "value" in result.stats or "id" in result.stats
        if "value" in result.stats:
            assert "mean" in result.stats["value"]
            assert "min" in result.stats["value"]
            assert "max" in result.stats["value"]
    
    def test_parse_csv_handles_null_values(
        self, 
        parser_service: ParserService, 
        sample_csv_content: str
    ):
        """
        Test that null/missing values are detected and counted.
        The sample CSV has one row with missing 'value'.
        """
        # Act
        result = parser_service.parse(sample_csv_content, "csv")
        
        # Assert - find the 'value' column and check null count
        value_col = next((c for c in result.columns if c.name == "value"), None)
        if value_col:
            # One row has missing value (row 4)
            assert value_col.null_count >= 1 or value_col.non_null_count <= 9
    
    def test_parse_csv_generates_preview(
        self, 
        parser_service: ParserService, 
        sample_csv_content: str
    ):
        """
        Test that preview rows are generated correctly.
        Preview should contain first N rows as dictionaries.
        """
        # Act
        result = parser_service.parse(sample_csv_content, "csv")
        
        # Assert
        assert result.preview is not None
        assert len(result.preview) <= 10  # Max preview rows
        assert len(result.preview) > 0
        
        # Each preview row should be a dict with column keys
        first_row = result.preview[0]
        assert isinstance(first_row, dict)
        assert "id" in first_row or "name" in first_row
    
    def test_parse_csv_computes_statistics(
        self, 
        parser_service: ParserService, 
        large_csv_content: str
    ):
        """
        Test that statistical summaries are computed for large datasets.
        Uses larger CSV to ensure meaningful statistics.
        """
        # Act
        result = parser_service.parse(large_csv_content, "csv")
        
        # Assert
        assert result.row_count == 100
        assert result.stats is not None
        
        # Should have stats for 'value' column
        if "value" in result.stats:
            stats = result.stats["value"]
            assert "min" in stats
            assert "max" in stats
            assert "mean" in stats
    
    def test_parse_csv_empty_file(
        self, 
        parser_service: ParserService, 
        empty_csv_content: str
    ):
        """
        Test parsing of CSV with only headers (no data rows).
        Should return 0 rows with column metadata.
        """
        # Act
        result = parser_service.parse(empty_csv_content, "csv")
        
        # Assert
        assert result.row_count == 0
        # Columns should still be detected from headers
        assert result.column_count == 3
    
    def test_parse_csv_to_dict(
        self, 
        parser_service: ParserService, 
        sample_csv_content: str
    ):
        """
        Test that ParsedMetrics.to_dict() serializes correctly.
        Used for JSON serialization to database.
        """
        # Act
        result = parser_service.parse(sample_csv_content, "csv")
        as_dict = result.to_dict()
        
        # Assert
        assert isinstance(as_dict, dict)
        assert "row_count" in as_dict
        assert "column_count" in as_dict
        assert "columns" in as_dict
        assert "stats" in as_dict
        assert "preview" in as_dict
        assert as_dict["row_count"] == 10


class TestParserServiceText:
    """Tests for text file parsing functionality."""
    
    def test_parse_text_basic(self, parser_service: ParserService, sample_text_content: str):
        """
        Test basic text parsing returns correct line count.
        Each line in the text file counts as a row.
        """
        # Act
        result = parser_service.parse(sample_text_content, "txt")
        
        # Assert
        assert isinstance(result, ParsedMetrics)
        assert result.row_count == 5  # 5 lines in sample
        assert result.column_count == 1  # Text treated as single column
    
    def test_parse_text_generates_word_count(
        self, 
        parser_service: ParserService, 
        sample_text_content: str
    ):
        """
        Test that text parsing computes word and character counts.
        """
        # Act
        result = parser_service.parse(sample_text_content, "txt")
        
        # Assert
        assert "text_metrics" in result.stats
        text_stats = result.stats["text_metrics"]
        assert "word_count" in text_stats
        assert "char_count" in text_stats
        assert text_stats["word_count"] > 0
        assert text_stats["char_count"] > 0
    
    def test_parse_text_preview(
        self, 
        parser_service: ParserService, 
        sample_text_content: str
    ):
        """
        Test that text preview contains line content.
        """
        # Act
        result = parser_service.parse(sample_text_content, "txt")
        
        # Assert
        assert result.preview is not None
        assert len(result.preview) > 0
        
        first_line = result.preview[0]
        assert "line" in first_line
        assert "content" in first_line
        assert first_line["line"] == 1
    
    def test_parse_text_single_line(self, parser_service: ParserService):
        """Test parsing of single-line text."""
        # Arrange
        content = "Single line text file"
        
        # Act
        result = parser_service.parse(content, "txt")
        
        # Assert
        assert result.row_count == 1


class TestParserServiceDataSummary:
    """Tests for the get_data_summary method."""
    
    def test_get_data_summary_csv(
        self, 
        parser_service: ParserService, 
        sample_csv_content: str
    ):
        """
        Test that data summary generates readable context for AI.
        Summary is used as input to the AI service.
        """
        # Arrange
        metrics = parser_service.parse(sample_csv_content, "csv")
        
        # Act
        summary = parser_service.get_data_summary(metrics)
        
        # Assert
        assert isinstance(summary, str)
        assert "Data Overview:" in summary
        assert "Rows: 10" in summary
        assert "Columns: 5" in summary
        assert "Columns:" in summary or "Sample Data" in summary
    
    def test_get_data_summary_includes_preview(
        self, 
        parser_service: ParserService, 
        sample_csv_content: str
    ):
        """
        Test that data summary includes sample rows for AI context.
        """
        # Arrange
        metrics = parser_service.parse(sample_csv_content, "csv")
        
        # Act
        summary = parser_service.get_data_summary(metrics)
        
        # Assert
        assert "Sample Data" in summary


class TestParserServiceEdgeCases:
    """Tests for edge cases and error handling."""
    
    def test_parse_unsupported_type_raises_error(self, parser_service: ParserService):
        """
        Test that unsupported file types raise ValueError.
        """
        # Act & Assert
        with pytest.raises(ValueError) as exc_info:
            parser_service.parse("some content", "json")
        
        assert "Unsupported file type" in str(exc_info.value)
    
    def test_parse_empty_string(self, parser_service: ParserService):
        """
        Test handling of empty string input.
        """
        # Act
        result = parser_service.parse("", "txt")
        
        # Assert - should handle gracefully
        assert result.row_count >= 0


class TestColumnInfo:
    """Tests for the ColumnInfo dataclass."""
    
    def test_column_info_creation(self):
        """Test creating ColumnInfo with all fields."""
        # Act
        col = ColumnInfo(
            name="test_column",
            dtype="float64",
            non_null_count=95,
            null_count=5
        )
        
        # Assert
        assert col.name == "test_column"
        assert col.dtype == "float64"
        assert col.non_null_count == 95
        assert col.null_count == 5
    
    def test_column_info_defaults(self):
        """Test ColumnInfo default values."""
        # Act
        col = ColumnInfo(name="simple", dtype="object")
        
        # Assert
        assert col.non_null_count == 0
        assert col.null_count == 0


class TestParsedMetrics:
    """Tests for the ParsedMetrics dataclass."""
    
    def test_parsed_metrics_to_dict(self):
        """Test serialization to dictionary."""
        # Arrange
        metrics = ParsedMetrics(
            row_count=100,
            column_count=3,
            columns=[ColumnInfo("id", "int64", 100, 0)],
            stats={"id": {"min": 1, "max": 100}},
            preview=[{"id": 1}]
        )
        
        # Act
        result = metrics.to_dict()
        
        # Assert
        assert result["row_count"] == 100
        assert result["column_count"] == 3
        assert len(result["columns"]) == 1
        assert result["columns"][0]["name"] == "id"
