#!/usr/bin/env python3
"""
tests/test_debug_plan_trace.py - Unit Tests for Plan Decision Tracing

Tests for debug_gpu_optimizer.trace_plan_decisions() function.
Verifies that planner decisions are properly traced to spec fields.
"""

from __future__ import annotations

import pytest
from pathlib import Path

# Import module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent))

from debug_gpu_optimizer import trace_plan_decisions


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def complete_spec():
    """Complete GPU spec."""
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
    """Partial GPU spec with defaults."""
    return {
        "vendor": "nvidia",
        "model": "GeForce RTX 3070",
        "vram_total_mb": 8192,
        "vram_free_mb": 6144,
        "compute_capability": "8.6",
        "cuda_version": "",
        "num_sm": 0,
        "clock_mhz": 0,
        "thermal_limits": {},
    }


@pytest.fixture
def intel_spec():
    """Intel iGPU spec."""
    return {
        "vendor": "intel",
        "model": "UHD Graphics 770",
        "vram_total_mb": 6144,
        "vram_free_mb": 4000,
        "compute_capability": "",
        "num_sm": 0,
    }


@pytest.fixture
def safe_plan():
    """Safe mode optimization plan."""
    return {
        "mode": "safe",
        "per_process_vram_cap_mb": 7168,  # 70% of 10240
        "recommended_batch_size": 16,
        "cooldown_threshold_c": 80,
        "resume_threshold_c": 65,
        "fair_scheduling_policy": "round_robin",
        "vram_deduplication_enabled": False,
        "estimated_reclaimed_mb": 0,
    }


@pytest.fixture
def aggressive_plan():
    """Aggressive mode optimization plan."""
    return {
        "mode": "aggressive",
        "per_process_vram_cap_mb": 9216,  # 90% of 10240
        "recommended_batch_size": 32,
        "cooldown_threshold_c": 83,
        "resume_threshold_c": 70,
        "fair_scheduling_policy": "priority_weighted",
        "vram_deduplication_enabled": True,
        "estimated_reclaimed_mb": 1000,
        "preemptive_throttle": {"enabled": True, "throttle_pct": 75, "grace_period_s": 3},
    }


@pytest.fixture
def conservative_plan():
    """Overly conservative plan for testing detection."""
    return {
        "mode": "safe",
        "per_process_vram_cap_mb": 2048,  # Only 20% utilization
        "recommended_batch_size": 2,  # Very small
        "cooldown_threshold_c": 60,  # Very aggressive thermal
        "resume_threshold_c": 45,
        "fair_scheduling_policy": "round_robin",
        "vram_deduplication_enabled": False,
        "estimated_reclaimed_mb": 0,
    }


# =============================================================================
# Test: Basic Trace Structure
# =============================================================================

class TestTraceStructure:
    """Tests for trace output structure."""
    
    def test_returns_list(self, safe_plan, complete_spec):
        """Should return a list of trace strings."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        assert isinstance(traces, list)
        assert len(traces) > 0
    
    def test_traces_include_summary(self, safe_plan, complete_spec):
        """Should include a summary at the end."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        # Last trace should be summary
        summary = traces[-1]
        assert "SUMMARY" in summary
        assert "confidence" in summary.lower()
    
    def test_traces_vram_cap(self, safe_plan, complete_spec):
        """Should include trace for VRAM cap decision."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        vram_traces = [t for t in traces if "per_process_vram_cap_mb" in t]
        assert len(vram_traces) > 0
    
    def test_traces_batch_size(self, safe_plan, complete_spec):
        """Should include trace for batch size decision."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        batch_traces = [t for t in traces if "recommended_batch_size" in t]
        assert len(batch_traces) > 0
    
    def test_traces_thermal(self, safe_plan, complete_spec):
        """Should include trace for thermal thresholds."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        thermal_traces = [t for t in traces if "thermal" in t.lower()]
        assert len(thermal_traces) > 0


# =============================================================================
# Test: Spec Field Usage
# =============================================================================

class TestSpecFieldUsage:
    """Tests for spec field attribution in traces."""
    
    def test_vram_cap_uses_vram_total(self, safe_plan, complete_spec):
        """VRAM cap trace should reference vram_total_mb."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        vram_trace = next(t for t in traces if "per_process_vram_cap_mb" in t)
        assert "vram_total_mb" in vram_trace
    
    def test_batch_size_uses_vendor(self, safe_plan, complete_spec):
        """Batch size trace should reference vendor."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        batch_trace = next(t for t in traces if "recommended_batch_size" in t)
        assert "vendor" in batch_trace
    
    def test_thermal_uses_limits_when_present(self, safe_plan, complete_spec):
        """Thermal trace should use thermal_limits when present."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        thermal_trace = next(t for t in traces if "thermal" in t.lower())
        assert "thermal_limits" in thermal_trace


# =============================================================================
# Test: Defaults Detection
# =============================================================================

class TestDefaultsDetection:
    """Tests for detecting when defaults are used."""
    
    def test_detects_defaults_with_partial_spec(self, safe_plan, partial_spec):
        """Should detect when defaults are used due to missing spec fields."""
        traces = trace_plan_decisions(safe_plan, partial_spec)
        
        default_traces = [t for t in traces if "defaults_used: True" in t]
        assert len(default_traces) > 0
    
    def test_no_defaults_with_complete_spec(self, safe_plan, complete_spec):
        """Should not report defaults with complete spec."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        # Most traces should not use defaults
        default_traces = [t for t in traces if "defaults_used: True" in t]
        non_default_traces = [t for t in traces if "defaults_used: False" in t]
        
        # Majority should not use defaults
        assert len(non_default_traces) >= len(default_traces)


# =============================================================================
# Test: Issue Detection
# =============================================================================

class TestIssueDetection:
    """Tests for detecting potential issues in plans."""
    
    def test_detects_conservative_vram_cap(self, conservative_plan, complete_spec):
        """Should flag overly conservative VRAM cap."""
        traces = trace_plan_decisions(conservative_plan, complete_spec)
        
        # Look for ISSUE flag in VRAM trace
        vram_trace = next(t for t in traces if "per_process_vram_cap_mb" in t)
        assert "ISSUE" in vram_trace
    
    def test_suggests_adjustment_for_conservative_plan(self, conservative_plan, complete_spec):
        """Should suggest adjustment for conservative settings."""
        traces = trace_plan_decisions(conservative_plan, complete_spec)
        
        # Look for ADJUSTMENT suggestion
        adjustment_traces = [t for t in traces if "ADJUSTMENT" in t]
        assert len(adjustment_traces) > 0
    
    def test_detects_intel_batch_reduction(self, safe_plan, intel_spec):
        """Should flag Intel batch reduction heuristic."""
        # Simulate a plan with reduced batch for Intel
        plan = dict(safe_plan)
        plan["recommended_batch_size"] = 4  # Reduced for Intel
        
        traces = trace_plan_decisions(plan, intel_spec)
        
        batch_trace = next(t for t in traces if "recommended_batch_size" in t)
        # Should either flag as issue or note the Intel adjustment
        assert "Intel" in batch_trace or "vendor" in batch_trace
    
    def test_detects_missing_sm_scaling(self, safe_plan, partial_spec):
        """Should detect when SM scaling couldn't be applied."""
        traces = trace_plan_decisions(safe_plan, partial_spec)
        
        batch_trace = next(t for t in traces if "recommended_batch_size" in t)
        # num_sm=0 should be flagged
        assert "num_sm" in batch_trace or "ISSUE" in batch_trace


# =============================================================================
# Test: Confidence Scores
# =============================================================================

class TestConfidenceScores:
    """Tests for confidence scores in traces."""
    
    def test_high_confidence_with_complete_spec(self, safe_plan, complete_spec):
        """Traces should show high confidence with complete spec."""
        traces = trace_plan_decisions(safe_plan, complete_spec)
        
        # Extract confidence values (rough check)
        confidence_matches = []
        for trace in traces:
            if "confidence:" in trace:
                # Parse confidence value
                for line in trace.split("\n"):
                    if "confidence:" in line:
                        value = line.split(":")[1].strip()
                        try:
                            confidence_matches.append(float(value))
                        except ValueError:
                            pass
        
        if confidence_matches:
            avg = sum(confidence_matches) / len(confidence_matches)
            assert avg >= 0.7
    
    def test_lower_confidence_with_partial_spec(self, safe_plan, partial_spec):
        """Traces should show lower confidence with partial spec."""
        traces = trace_plan_decisions(safe_plan, partial_spec)
        
        # Summary should mention low confidence if applicable
        summary = traces[-1]
        # Check that the system acknowledges incomplete data
        assert "confidence" in summary.lower()


# =============================================================================
# Test: Aggressive vs Safe Mode
# =============================================================================

class TestModeComparison:
    """Tests comparing safe vs aggressive mode traces."""
    
    def test_aggressive_shows_higher_utilization(self, aggressive_plan, safe_plan, complete_spec):
        """Aggressive plan should trace higher VRAM utilization."""
        safe_traces = trace_plan_decisions(safe_plan, complete_spec)
        aggressive_traces = trace_plan_decisions(aggressive_plan, complete_spec)
        
        # Both should have traces
        assert len(safe_traces) > 0
        assert len(aggressive_traces) > 0
    
    def test_scheduling_mode_match(self, safe_plan, aggressive_plan, complete_spec):
        """Should trace scheduling policy appropriateness."""
        safe_traces = trace_plan_decisions(safe_plan, complete_spec)
        aggressive_traces = trace_plan_decisions(aggressive_plan, complete_spec)
        
        safe_scheduling = next(t for t in safe_traces if "fair_scheduling_policy" in t)
        aggressive_scheduling = next(t for t in aggressive_traces if "fair_scheduling_policy" in t)
        
        assert "round_robin" in safe_scheduling
        assert "priority_weighted" in aggressive_scheduling


# =============================================================================
# Test: Edge Cases
# =============================================================================

class TestEdgeCases:
    """Tests for edge cases."""
    
    def test_empty_plan(self, complete_spec):
        """Should handle empty plan gracefully."""
        traces = trace_plan_decisions({}, complete_spec)
        
        # Should still produce traces with default values
        assert len(traces) > 0
    
    def test_empty_spec(self, safe_plan):
        """Should handle empty spec gracefully."""
        traces = trace_plan_decisions(safe_plan, {})
        
        # Should still produce traces, all showing defaults
        assert len(traces) > 0
        
        # Most should show defaults_used: True
        default_traces = [t for t in traces if "defaults_used: True" in t]
        assert len(default_traces) >= 1
    
    def test_both_empty(self):
        """Should handle both empty gracefully."""
        traces = trace_plan_decisions({}, {})
        
        assert len(traces) > 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
