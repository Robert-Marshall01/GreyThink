#!/usr/bin/env python3
"""
tests/test_debug_spec.py - Unit Tests for Spec Integrity Verification

Tests for debug_gpu_optimizer.verify_spec_integrity() function.
Verifies detection of missing fields, defaulted values, and suggested fixes.
"""

from __future__ import annotations

import json
import pytest
from pathlib import Path

# Import module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent))

from debug_gpu_optimizer import (
    verify_spec_integrity,
    REQUIRED_SPEC_FIELDS,
)


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def complete_spec():
    """A complete spec with all fields properly filled."""
    return {
        "vendor": "nvidia",
        "model": "GeForce RTX 3080",
        "vram_total_mb": 10240,
        "vram_free_mb": 9000,
        "compute_capability": "8.6",
        "cuda_version": "12.1",
        "num_sm": 68,
        "pci_bandwidth": "16 GT/s",
        "clock_mhz": 1710,
        "thermal_limits": {"throttle_c": 83, "shutdown_c": 100},
    }


@pytest.fixture
def partial_spec():
    """A partial spec with some fields missing or defaulted."""
    return {
        "vendor": "nvidia",
        "model": "GeForce RTX 3070",
        "vram_total_mb": 8192,
        "vram_free_mb": 6144,
        "compute_capability": "8.6",
        "cuda_version": "",  # Defaulted/empty
        "num_sm": 0,  # Defaulted to 0
        "pci_bandwidth": "",  # Missing
        "clock_mhz": 0,  # Defaulted to 0
        "thermal_limits": {},  # Empty
    }


@pytest.fixture
def minimal_spec():
    """Minimal spec with only basic detection."""
    return {
        "vendor": "nvidia",
        "model": "unknown",
        "vram_total_mb": 0,
    }


@pytest.fixture
def empty_spec():
    """Completely empty spec."""
    return {}


# =============================================================================
# Test: Complete Spec
# =============================================================================

class TestCompleteSpec:
    """Tests for complete spec verification."""
    
    def test_no_missing_fields(self, complete_spec):
        """Complete spec should have no missing fields."""
        result = verify_spec_integrity(complete_spec)
        
        assert result["missing_fields"] == []
    
    def test_high_confidence_score(self, complete_spec):
        """Complete spec should have high confidence score."""
        result = verify_spec_integrity(complete_spec)
        
        assert result["confidence_score"] >= 0.8
    
    def test_no_defaulted_fields(self, complete_spec):
        """Complete spec should have no defaulted fields."""
        result = verify_spec_integrity(complete_spec)
        
        assert result["defaulted_fields"] == []
    
    def test_no_suggested_fixes(self, complete_spec):
        """Complete spec should need no fixes."""
        result = verify_spec_integrity(complete_spec)
        
        assert len(result["suggested_fixes"]) == 0


# =============================================================================
# Test: Partial Spec
# =============================================================================

class TestPartialSpec:
    """Tests for partial spec verification."""
    
    def test_detects_defaulted_fields(self, partial_spec):
        """Should detect fields with default values."""
        result = verify_spec_integrity(partial_spec)
        
        # These fields have empty/0 values which are defaults
        assert "cuda_version" in result["defaulted_fields"]
        assert "num_sm" in result["defaulted_fields"]
        assert "clock_mhz" in result["defaulted_fields"]
    
    def test_provides_suggested_fixes(self, partial_spec):
        """Should provide fixes for defaulted fields."""
        result = verify_spec_integrity(partial_spec)
        
        # Should have fix suggestions for defaulted fields
        for field in result["defaulted_fields"]:
            assert field in result["suggested_fixes"]
    
    def test_medium_confidence_score(self, partial_spec):
        """Partial spec should have medium confidence."""
        result = verify_spec_integrity(partial_spec)
        
        # Not complete (< 0.9) but not empty (> 0.4)
        assert 0.4 < result["confidence_score"] < 0.9
    
    def test_warnings_generated(self, partial_spec):
        """Should generate warnings for defaults."""
        result = verify_spec_integrity(partial_spec)
        
        assert len(result["warnings"]) > 0


# =============================================================================
# Test: Minimal Spec
# =============================================================================

class TestMinimalSpec:
    """Tests for minimal spec verification."""
    
    def test_detects_missing_fields(self, minimal_spec):
        """Should detect all missing fields."""
        result = verify_spec_integrity(minimal_spec)
        
        # vram_free_mb, compute_capability, etc. should be missing
        assert len(result["missing_fields"]) > 0
    
    def test_low_confidence_score(self, minimal_spec):
        """Minimal spec should have low confidence."""
        result = verify_spec_integrity(minimal_spec)
        
        assert result["confidence_score"] < 0.5
    
    def test_vram_zero_detected_as_default(self, minimal_spec):
        """vram_total_mb=0 should be detected as default."""
        result = verify_spec_integrity(minimal_spec)
        
        assert "vram_total_mb" in result["defaulted_fields"]
    
    def test_severe_warning_added(self, minimal_spec):
        """Should add severe warning for low confidence."""
        result = verify_spec_integrity(minimal_spec)
        
        severe_warnings = [w for w in result["warnings"] if "SEVERE" in w]
        assert len(severe_warnings) > 0


# =============================================================================
# Test: Empty Spec
# =============================================================================

class TestEmptySpec:
    """Tests for empty spec verification."""
    
    def test_all_fields_missing(self, empty_spec):
        """Empty spec should have all required fields missing."""
        result = verify_spec_integrity(empty_spec)
        
        assert len(result["missing_fields"]) == len(REQUIRED_SPEC_FIELDS)
    
    def test_zero_confidence(self, empty_spec):
        """Empty spec should have very low confidence."""
        result = verify_spec_integrity(empty_spec)
        
        assert result["confidence_score"] < 0.1
    
    def test_fixes_for_all_fields(self, empty_spec):
        """Should provide fixes for all missing fields."""
        result = verify_spec_integrity(empty_spec)
        
        for field in REQUIRED_SPEC_FIELDS:
            assert field in result["suggested_fixes"]


# =============================================================================
# Test: Validation Logic
# =============================================================================

class TestValidationLogic:
    """Tests for specific validation rules."""
    
    def test_vram_too_low_warning(self):
        """Should warn when vram_total_mb seems too low."""
        spec = {"vram_total_mb": 512}  # Suspiciously low
        result = verify_spec_integrity(spec)
        
        vram_warnings = [w for w in result["warnings"] if "vram_total_mb" in w and "low" in w.lower()]
        assert len(vram_warnings) > 0
    
    def test_vram_too_high_warning(self):
        """Should warn when vram_total_mb seems too high."""
        spec = {"vram_total_mb": 500000}  # Impossibly high
        result = verify_spec_integrity(spec)
        
        vram_warnings = [w for w in result["warnings"] if "vram_total_mb" in w and "high" in w.lower()]
        assert len(vram_warnings) > 0
    
    def test_vram_free_greater_than_total_inconsistency(self):
        """Should detect when free > total."""
        spec = {
            "vram_total_mb": 8192,
            "vram_free_mb": 16384,  # More than total!
        }
        result = verify_spec_integrity(spec)
        
        inconsistency_warnings = [w for w in result["warnings"] if "INCONSISTENCY" in w]
        assert len(inconsistency_warnings) > 0
    
    def test_nvidia_without_cuda_inconsistency(self):
        """Should detect NVIDIA vendor without CUDA info."""
        spec = {
            "vendor": "nvidia",
            "cuda_version": "",
            "compute_capability": "",
        }
        result = verify_spec_integrity(spec)
        
        inconsistency_warnings = [w for w in result["warnings"] if "NVIDIA" in w and "CUDA" in w]
        assert len(inconsistency_warnings) > 0
    
    def test_compute_capability_format(self):
        """Should warn about malformed compute_capability."""
        spec = {"compute_capability": "8_6"}  # Should be "8.6"
        result = verify_spec_integrity(spec)
        
        format_warnings = [w for w in result["warnings"] if "format" in w.lower()]
        assert len(format_warnings) > 0


# =============================================================================
# Test: Field Details
# =============================================================================

class TestFieldDetails:
    """Tests for field_details output."""
    
    def test_field_details_structure(self, complete_spec):
        """Should include detailed info for each field."""
        result = verify_spec_integrity(complete_spec)
        
        for field_name in REQUIRED_SPEC_FIELDS:
            assert field_name in result["field_details"]
            details = result["field_details"][field_name]
            assert "present" in details
            assert "value" in details
            assert "is_default" in details
            assert "importance" in details
    
    def test_field_present_flag(self, partial_spec):
        """Should correctly mark present/missing fields."""
        result = verify_spec_integrity(partial_spec)
        
        assert result["field_details"]["vendor"]["present"] is True
        assert result["field_details"]["vendor"]["value"] == "nvidia"


# =============================================================================
# Test: Edge Cases
# =============================================================================

class TestEdgeCases:
    """Tests for edge cases."""
    
    def test_none_values(self):
        """Should handle None values gracefully."""
        spec = {"vendor": None, "vram_total_mb": None}
        result = verify_spec_integrity(spec)
        
        # Should treat None as default/missing
        assert "vendor" in result["defaulted_fields"]
    
    def test_negative_vram(self):
        """Should handle negative VRAM values."""
        spec = {"vram_free_mb": -100}
        result = verify_spec_integrity(spec)
        
        negative_warnings = [w for w in result["warnings"] if "negative" in w.lower()]
        assert len(negative_warnings) > 0
    
    def test_extra_fields_ignored(self, complete_spec):
        """Should ignore extra fields not in requirements."""
        complete_spec["extra_field"] = "value"
        result = verify_spec_integrity(complete_spec)
        
        # Should still pass with high confidence
        assert result["confidence_score"] >= 0.8


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
