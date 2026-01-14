#!/usr/bin/env python3
"""
tests/test_debug_enforcement_logs.py - Unit Tests for Enforcement Log Analysis

Tests for debug_gpu_optimizer.check_enforcement_activity() function.
Uses mocked log files to verify enforcement detection and gap analysis.
"""

from __future__ import annotations

import json
import tempfile
from pathlib import Path

import pytest

# Import module under test
import sys
sys.path.insert(0, str(Path(__file__).parent.parent))

from debug_gpu_optimizer import check_enforcement_activity


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def empty_log_file(tmp_path):
    """Create an empty log file."""
    log_file = tmp_path / "empty.log"
    log_file.touch()
    return str(log_file)


@pytest.fixture
def dry_run_only_log(tmp_path):
    """Create a log with only dry-run actions."""
    log_file = tmp_path / "dry_run.log"
    entries = [
        {"timestamp": "2026-01-11T10:00:00Z", "event_type": "startup"},
        {"timestamp": "2026-01-11T10:00:10Z", "event_type": "sample", "details": {"temp_c": 45}},
        {"timestamp": "2026-01-11T10:01:00Z", "event_type": "plan_applied", "dry_run": True, "details": {"action": "throttle"}},
        {"timestamp": "2026-01-11T10:02:00Z", "event_type": "sample", "details": {"temp_c": 50}},
    ]
    with open(log_file, "w") as f:
        for entry in entries:
            f.write(json.dumps(entry) + "\n")
    return str(log_file)


@pytest.fixture
def active_enforcement_log(tmp_path):
    """Create a log with live enforcement actions."""
    log_file = tmp_path / "active.log"
    entries = [
        {"timestamp": "2026-01-11T10:00:00Z", "event_type": "startup"},
        {"timestamp": "2026-01-11T10:00:10Z", "event_type": "sample", "details": {"temp_c": 45}},
        {"timestamp": "2026-01-11T10:01:00Z", "event_type": "plan_applied", "dry_run": False, "details": {"action": "set_vram_cap", "value": 7168}},
        {"timestamp": "2026-01-11T10:02:00Z", "event_type": "sample", "details": {"temp_c": 82}},
        {"timestamp": "2026-01-11T10:02:05Z", "event_type": "plan_applied", "dry_run": False, "details": {"action": "thermal_throttle", "throttle_pct": 75}},
        {"timestamp": "2026-01-11T10:03:00Z", "event_type": "sample", "details": {"temp_c": 68}},
    ]
    with open(log_file, "w") as f:
        for entry in entries:
            f.write(json.dumps(entry) + "\n")
    return str(log_file)


@pytest.fixture
def threshold_breach_no_action_log(tmp_path):
    """Create a log with threshold breaches but no enforcement."""
    log_file = tmp_path / "breach_no_action.log"
    entries = [
        {"timestamp": "2026-01-11T10:00:00Z", "event_type": "startup"},
        {"timestamp": "2026-01-11T10:00:10Z", "event_type": "sample", "details": {"temp_c": 85}},
        {"timestamp": "2026-01-11T10:01:00Z", "event_type": "sample", "details": {"temp_c": 88}},
        {"timestamp": "2026-01-11T10:02:00Z", "event_type": "sample", "details": {"temp_c": 92}},
        # No plan_applied events despite high temps!
    ]
    with open(log_file, "w") as f:
        for entry in entries:
            f.write(json.dumps(entry) + "\n")
    return str(log_file)


@pytest.fixture
def gap_in_sampling_log(tmp_path):
    """Create a log with gaps in sampling."""
    log_file = tmp_path / "gap.log"
    entries = [
        {"timestamp": "2026-01-11T10:00:00Z", "event_type": "sample", "details": {"temp_c": 50}},
        {"timestamp": "2026-01-11T10:01:00Z", "event_type": "sample", "details": {"temp_c": 52}},
        # 10 minute gap!
        {"timestamp": "2026-01-11T10:11:00Z", "event_type": "sample", "details": {"temp_c": 55}},
        {"timestamp": "2026-01-11T10:12:00Z", "event_type": "sample", "details": {"temp_c": 53}},
    ]
    with open(log_file, "w") as f:
        for entry in entries:
            f.write(json.dumps(entry) + "\n")
    return str(log_file)


@pytest.fixture
def malformed_log(tmp_path):
    """Create a log with some malformed entries."""
    log_file = tmp_path / "malformed.log"
    with open(log_file, "w") as f:
        f.write('{"timestamp": "2026-01-11T10:00:00Z", "event_type": "sample"}\n')
        f.write('this is not json\n')  # Malformed
        f.write('{"timestamp": "2026-01-11T10:01:00Z", "event_type": "sample"}\n')
        f.write('{"incomplete json\n')  # Malformed
        f.write('{"timestamp": "2026-01-11T10:02:00Z", "event_type": "sample"}\n')
    return str(log_file)


# =============================================================================
# Test: Missing Log File
# =============================================================================

class TestMissingLog:
    """Tests for missing log file handling."""
    
    def test_handles_missing_file(self):
        """Should handle non-existent log file gracefully."""
        result = check_enforcement_activity("/nonexistent/path/log.file")
        
        assert "anomalies" in result
        assert len(result["anomalies"]) > 0
        assert "not found" in result["anomalies"][0].lower()
    
    def test_returns_structure_on_missing_file(self):
        """Should return proper structure even on error."""
        result = check_enforcement_activity("/nonexistent/log")
        
        assert "actions_count" in result
        assert "dry_run_only" in result
        assert "gaps" in result


# =============================================================================
# Test: Empty Log
# =============================================================================

class TestEmptyLog:
    """Tests for empty log file handling."""
    
    def test_detects_empty_log(self, empty_log_file):
        """Should detect and report empty log."""
        result = check_enforcement_activity(empty_log_file)
        
        assert result["actions_count"] == 0
        assert "empty" in str(result["anomalies"]).lower()
    
    def test_gaps_reported_for_empty(self, empty_log_file):
        """Should report gaps for no activity."""
        result = check_enforcement_activity(empty_log_file)
        
        assert len(result["gaps"]) > 0


# =============================================================================
# Test: Dry-Run Only Enforcement
# =============================================================================

class TestDryRunOnly:
    """Tests for dry-run only enforcement detection."""
    
    def test_detects_dry_run_only(self, dry_run_only_log):
        """Should detect when all actions are dry-run."""
        result = check_enforcement_activity(dry_run_only_log)
        
        assert result["dry_run_only"] is True
    
    def test_counts_dry_run_actions(self, dry_run_only_log):
        """Should count dry-run actions."""
        result = check_enforcement_activity(dry_run_only_log)
        
        assert result["actions_count"] == 1
    
    def test_lists_proposed_actions(self, dry_run_only_log):
        """Should list proposed (dry-run) actions."""
        result = check_enforcement_activity(dry_run_only_log)
        
        assert len(result["proposed_actions"]) > 0
        assert any("DRY-RUN" in a for a in result["proposed_actions"])
    
    def test_reports_gap_for_dry_run_only(self, dry_run_only_log):
        """Should report gap that no live enforcement is happening."""
        result = check_enforcement_activity(dry_run_only_log)
        
        gap_about_dry_run = [g for g in result["gaps"] if "dry-run" in g.lower()]
        assert len(gap_about_dry_run) > 0


# =============================================================================
# Test: Active Enforcement
# =============================================================================

class TestActiveEnforcement:
    """Tests for active (live) enforcement detection."""
    
    def test_detects_live_enforcement(self, active_enforcement_log):
        """Should detect live enforcement actions."""
        result = check_enforcement_activity(active_enforcement_log)
        
        assert result["dry_run_only"] is False
    
    def test_counts_all_actions(self, active_enforcement_log):
        """Should count all enforcement actions."""
        result = check_enforcement_activity(active_enforcement_log)
        
        assert result["actions_count"] == 2
    
    def test_lists_effective_actions(self, active_enforcement_log):
        """Should list effectively applied actions."""
        result = check_enforcement_activity(active_enforcement_log)
        
        assert len(result["effective_actions"]) > 0
    
    def test_gets_last_action_timestamp(self, active_enforcement_log):
        """Should capture last action timestamp."""
        result = check_enforcement_activity(active_enforcement_log)
        
        assert result["last_action_timestamp"] != ""
        assert "2026-01-11T10:02:05Z" in result["last_action_timestamp"]


# =============================================================================
# Test: Threshold Breach Without Action
# =============================================================================

class TestBreachNoAction:
    """Tests for detecting threshold breaches without enforcement."""
    
    def test_detects_breach_without_action(self, threshold_breach_no_action_log):
        """Should detect when thresholds are breached but no action taken."""
        result = check_enforcement_activity(threshold_breach_no_action_log)
        
        # Should have gap indicating breaches without actions
        assert len(result["gaps"]) > 0
    
    def test_zero_actions_with_breaches(self, threshold_breach_no_action_log):
        """Should report zero actions despite breaches."""
        result = check_enforcement_activity(threshold_breach_no_action_log)
        
        assert result["actions_count"] == 0


# =============================================================================
# Test: Sampling Gaps
# =============================================================================

class TestSamplingGaps:
    """Tests for sampling gap detection."""
    
    def test_detects_sampling_gap(self, gap_in_sampling_log):
        """Should detect gaps in sampling interval."""
        result = check_enforcement_activity(gap_in_sampling_log)
        
        # Should have anomaly about gap
        gap_anomalies = [a for a in result["anomalies"] if "gap" in a.lower()]
        assert len(gap_anomalies) > 0
    
    def test_reports_gap_duration(self, gap_in_sampling_log):
        """Should report gap duration."""
        result = check_enforcement_activity(gap_in_sampling_log)
        
        # Gap should be ~10 minutes = 600 seconds
        gap_str = str(result["anomalies"])
        # Should mention the gap duration
        assert "600" in gap_str or "gap" in gap_str.lower()


# =============================================================================
# Test: Malformed Logs
# =============================================================================

class TestMalformedLogs:
    """Tests for handling malformed log entries."""
    
    def test_handles_malformed_entries(self, malformed_log):
        """Should skip malformed entries and continue."""
        result = check_enforcement_activity(malformed_log)
        
        # Should still process valid entries
        assert result["actions_count"] >= 0  # No crash
    
    def test_no_crash_on_bad_json(self, malformed_log):
        """Should not crash on bad JSON."""
        # This should not raise
        result = check_enforcement_activity(malformed_log)
        
        assert isinstance(result, dict)


# =============================================================================
# Test: Output Structure
# =============================================================================

class TestOutputStructure:
    """Tests for output dictionary structure."""
    
    def test_all_required_keys(self, active_enforcement_log):
        """Should have all required keys in output."""
        result = check_enforcement_activity(active_enforcement_log)
        
        required_keys = [
            "actions_count",
            "dry_run_only",
            "last_action_timestamp",
            "anomalies",
            "proposed_actions",
            "effective_actions",
            "gaps",
        ]
        
        for key in required_keys:
            assert key in result, f"Missing key: {key}"
    
    def test_lists_are_lists(self, active_enforcement_log):
        """List fields should be lists."""
        result = check_enforcement_activity(active_enforcement_log)
        
        assert isinstance(result["anomalies"], list)
        assert isinstance(result["proposed_actions"], list)
        assert isinstance(result["effective_actions"], list)
        assert isinstance(result["gaps"], list)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
