"""
GreyAV Core Scanner Tests
"""

import pytest
from pathlib import Path


class TestGreyAVImport:
    """Test that GreyAV can be imported correctly."""
    
    def test_import_greyav(self):
        """Test main module import."""
        import greyav
        assert hasattr(greyav, 'GreyAV')
    
    def test_greyav_class_exists(self):
        """Test GreyAV class can be instantiated."""
        from greyav import GreyAV
        av = GreyAV()
        assert av is not None
    
    def test_version_defined(self):
        """Test version is defined."""
        from greyav import GreyAV
        av = GreyAV()
        assert hasattr(av, 'VERSION')
        assert av.VERSION is not None


class TestFileScanning:
    """Test file scanning functionality."""
    
    def test_scan_clean_file(self, greyav_instance, clean_file):
        """Test that clean files are not flagged."""
        result = greyav_instance.scan_file(str(clean_file))
        assert result is not None
        # Clean file should not be flagged as threat
        assert result.get('threat_detected', False) is False
    
    def test_scan_nonexistent_file(self, greyav_instance, temp_dir):
        """Test scanning a file that doesn't exist."""
        fake_path = temp_dir / "nonexistent.txt"
        result = greyav_instance.scan_file(str(fake_path))
        # Should handle gracefully
        assert result is not None
    
    def test_scan_eicar_detection(self, greyav_instance, eicar_file):
        """Test EICAR test file is detected."""
        result = greyav_instance.scan_file(str(eicar_file))
        assert result is not None
        # EICAR should be detected as a threat
        assert result.get('threat_detected', False) is True


class TestHeuristicAnalysis:
    """Test heuristic analysis functionality."""
    
    def test_heuristic_clean_file(self, greyav_instance, clean_file):
        """Test heuristic analysis on clean file."""
        result = greyav_instance.heuristic_analysis(str(clean_file))
        assert result is not None
        assert 'score' in result
        # Clean file should have low score
        assert result['score'] < 50
    
    def test_heuristic_suspicious_file(self, greyav_instance, suspicious_file):
        """Test heuristic analysis detects suspicious patterns."""
        result = greyav_instance.heuristic_analysis(str(suspicious_file))
        assert result is not None
        assert 'score' in result
        # Suspicious file should have elevated score
        assert result['score'] > 0


class TestQuarantine:
    """Test quarantine functionality."""
    
    def test_quarantine_directory_exists(self, greyav_instance):
        """Test quarantine directory is accessible."""
        assert hasattr(greyav_instance, 'quarantine_dir')
    
    def test_list_quarantine(self, greyav_instance):
        """Test listing quarantined files."""
        # Should not raise an exception
        files = greyav_instance.list_quarantine()
        assert isinstance(files, (list, tuple))


class TestHashCalculation:
    """Test file hash calculation."""
    
    def test_calculate_hash(self, greyav_instance, clean_file):
        """Test hash calculation returns valid hash."""
        file_hash = greyav_instance.calculate_hash(str(clean_file))
        assert file_hash is not None
        # SHA256 hash should be 64 characters
        assert len(file_hash) == 64
        # Should be hexadecimal
        assert all(c in '0123456789abcdef' for c in file_hash.lower())
    
    def test_hash_consistency(self, greyav_instance, clean_file):
        """Test that same file produces same hash."""
        hash1 = greyav_instance.calculate_hash(str(clean_file))
        hash2 = greyav_instance.calculate_hash(str(clean_file))
        assert hash1 == hash2


class TestConfiguration:
    """Test configuration management."""
    
    def test_config_exists(self, greyav_instance):
        """Test configuration is accessible."""
        assert hasattr(greyav_instance, 'config') or hasattr(greyav_instance, 'get_config')
    
    def test_signatures_loaded(self, greyav_instance):
        """Test signatures are loaded."""
        assert hasattr(greyav_instance, 'signatures')


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
