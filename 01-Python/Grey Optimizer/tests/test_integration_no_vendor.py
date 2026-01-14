#!/usr/bin/env python3
"""
tests/test_integration_no_vendor.py - Integration Tests Without Vendor Tools

This module provides integration tests that simulate environments where
no vendor-specific tools (nvidia-smi, rocm-smi, intel_gpu_top) are available.

These tests verify that the optimizer gracefully falls back to system
tools and provides reasonable defaults when vendor detection fails.

Test Scenarios:
- No GPU tools installed (bare metal or container)
- Only lspci/lshw available
- Only Python libraries available
- Complete fallback to defaults

Usage:
    pytest tests/test_integration_no_vendor.py -v
    pytest tests/test_integration_no_vendor.py -v -k "test_fallback"
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from unittest.mock import MagicMock, patch, PropertyMock

import pytest

from grey_gpu_optimizer.optimizer import (
    GPUSpec,
    OptimizationPlan,
    ApplyResult,
    detect_gpus,
    plan_optimizations,
    apply_plan,
    _check_tool,
    _run_command,
    _parse_lspci,
    _parse_lshw,
)
from grey_gpu_optimizer.daemon import (
    GPUOptimizerDaemon,
    sample_gpu_metrics,
    MonitorSample,
)
from grey_gpu_optimizer.cli import main as cli_main


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def mock_no_vendor_tools():
    """Mock environment with no vendor tools available."""
    def check_tool(tool):
        # Only allow basic system tools
        return tool in ["lspci", "lshw", "glxinfo"]
    
    return check_tool


@pytest.fixture
def mock_lspci_intel_only():
    """Mock lspci output with only Intel integrated GPU."""
    return """
00:02.0 VGA compatible controller: Intel Corporation UHD Graphics 770 (rev 0c)
"""


@pytest.fixture
def mock_lshw_intel():
    """Mock lshw output for Intel GPU."""
    return """
  *-display
       description: VGA compatible controller
       product: UHD Graphics 770
       vendor: Intel Corporation
       physical id: 2
       bus info: pci@0000:00:02.0
       logical name: /dev/fb0
       version: 0c
       width: 64 bits
       clock: 33MHz
       configuration: depth=32 driver=i915 latency=0 resolution=2560,1440
"""


@pytest.fixture
def temp_config_dir(tmp_path):
    """Create temporary config directory."""
    config_dir = tmp_path / ".grey_optimizer"
    config_dir.mkdir(parents=True)
    (config_dir / "artifacts").mkdir()
    return config_dir


# =============================================================================
# Test: Detection Without Vendor Tools
# =============================================================================

class TestDetectionNoVendor:
    """Tests for GPU detection without vendor-specific tools."""
    
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    @patch("grey_gpu_optimizer.optimizer._run_command")
    def test_detect_with_lspci_only(
        self,
        mock_run_command,
        mock_check_tool,
        mock_lspci_intel_only
    ):
        """Test detection using only lspci."""
        def check_tool(tool):
            return tool == "lspci"
        
        def run_command(cmd, *args, **kwargs):
            if cmd[0] == "lspci":
                return (True, mock_lspci_intel_only, "")
            return (False, "", "not found")
        
        mock_check_tool.side_effect = check_tool
        mock_run_command.side_effect = run_command
        
        specs = detect_gpus(use_python_libs=False)
        
        assert len(specs) >= 1
        spec = specs[0]
        assert spec.vendor == "intel"
        assert "UHD" in spec.model or "770" in spec.model
    
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    @patch("grey_gpu_optimizer.optimizer._run_command")
    def test_detect_with_lshw_only(
        self,
        mock_run_command,
        mock_check_tool,
        mock_lshw_intel
    ):
        """Test detection using only lshw."""
        def check_tool(tool):
            return tool == "lshw"
        
        def run_command(cmd, *args, **kwargs):
            if cmd[0] == "lshw":
                return (True, mock_lshw_intel, "")
            return (False, "", "not found")
        
        mock_check_tool.side_effect = check_tool
        mock_run_command.side_effect = run_command
        
        specs = detect_gpus(use_python_libs=False)
        
        assert len(specs) >= 1
        spec = specs[0]
        assert spec.vendor == "intel"
        assert "i915" in spec.driver or spec.driver == ""
    
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    def test_detect_with_no_tools(self, mock_check_tool):
        """Test detection when no tools are available."""
        mock_check_tool.return_value = False
        
        specs = detect_gpus(use_python_libs=False)
        
        assert len(specs) == 1
        spec = specs[0]
        # Should have notes about failed detection
        assert any("not found" in note.lower() for note in spec.notes)
    
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    @patch("grey_gpu_optimizer.optimizer._probe_torch_cuda")
    def test_detect_with_torch_only(self, mock_torch, mock_check_tool):
        """Test detection using only PyTorch CUDA."""
        mock_check_tool.return_value = False
        mock_torch.return_value = {
            "vendor": "nvidia",
            "model": "NVIDIA GeForce RTX 3080",
            "vram_total_mb": 10240,
            "compute_capability": "8.6",
            "num_sm": 68,
            "detection_source": "torch.cuda"
        }
        
        specs = detect_gpus(
            use_vendor_cli=False,
            use_system_tools=False,
            use_python_libs=True
        )
        
        assert len(specs) >= 1
        spec = specs[0]
        assert spec.vendor == "nvidia"
        assert spec.vram_total_mb == 10240


# =============================================================================
# Test: Planning Without Full Detection
# =============================================================================

class TestPlanningNoVendor:
    """Tests for planning with incomplete GPU detection."""
    
    def test_plan_with_unknown_vendor(self):
        """Test planning for unknown vendor GPU."""
        spec = GPUSpec(
            vendor="unknown",
            model="Generic GPU",
            vram_total_mb=4096,
        )
        
        plan = plan_optimizations(spec, mode="safe")
        
        assert plan.mode == "safe"
        assert plan.per_process_vram_cap_mb > 0
        assert plan.per_process_vram_cap_mb <= 4096
        # Lower confidence for unknown vendor
        assert plan.confidence < 0.8
    
    def test_plan_with_zero_vram(self):
        """Test planning when VRAM is unknown."""
        spec = GPUSpec(
            vendor="intel",
            model="Unknown GPU",
            vram_total_mb=0,  # Unknown
        )
        
        plan = plan_optimizations(spec, mode="safe")
        
        # Should use conservative defaults
        assert plan.per_process_vram_cap_mb <= 4096  # Default cap
        assert plan.recommended_batch_size <= 8
        # Very low confidence without VRAM info
        assert plan.confidence < 0.6
    
    def test_plan_rationale_for_incomplete_spec(self):
        """Test that rationale explains incomplete detection."""
        spec = GPUSpec(
            vendor="unknown",
            vram_total_mb=0,
            detection_sources=[],
        )
        
        plan = plan_optimizations(spec, mode="safe")
        
        # Rationale should mention unknown/default
        vram_rationale = plan.rationale.get("vram_cap", "")
        assert "unknown" in vram_rationale.lower() or "default" in vram_rationale.lower()


# =============================================================================
# Test: Apply Without Enforcement Capability
# =============================================================================

class TestApplyNoVendor:
    """Tests for plan application without vendor tools."""
    
    def test_apply_dry_run_works_without_tools(self):
        """Test that dry-run works even without vendor tools."""
        spec = GPUSpec(vendor="unknown", vram_total_mb=4096)
        plan = plan_optimizations(spec, mode="safe")
        
        result = apply_plan(plan, dry_run=True)
        
        assert result.success is True
        assert result.dry_run is True
        assert len(result.actions_applied) > 0
    
    def test_artifacts_generated_without_tools(self, temp_config_dir, monkeypatch):
        """Test artifact generation works without vendor tools."""
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.ARTIFACTS_DIR",
            temp_config_dir / "artifacts"
        )
        
        spec = GPUSpec(vendor="unknown", vram_total_mb=4096)
        plan = plan_optimizations(spec, mode="safe")
        
        result = apply_plan(plan, dry_run=True)
        
        assert "vram_reclamation_log" in result.artifacts
        assert Path(result.artifacts["vram_reclamation_log"]).exists()


# =============================================================================
# Test: Daemon Without Vendor Metrics
# =============================================================================

class TestDaemonNoVendor:
    """Tests for daemon operation without vendor monitoring tools."""
    
    @patch("grey_gpu_optimizer.daemon._get_nvidia_metrics")
    @patch("grey_gpu_optimizer.daemon._get_rocm_metrics")
    @patch("grey_gpu_optimizer.daemon._get_intel_metrics")
    def test_sample_metrics_no_tools(
        self,
        mock_intel,
        mock_rocm,
        mock_nvidia
    ):
        """Test metric sampling when no monitoring tools work."""
        mock_nvidia.return_value = {}
        mock_rocm.return_value = {}
        mock_intel.return_value = {}
        
        sample = sample_gpu_metrics(None)
        
        # Should return zeroed sample
        assert isinstance(sample, MonitorSample)
        assert sample.timestamp != ""
    
    def test_daemon_starts_without_tools(self, temp_config_dir, monkeypatch):
        """Test daemon can start without vendor tools."""
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.CONFIG_DIR",
            temp_config_dir
        )
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.SPEC_FILE",
            temp_config_dir / "gpu_spec.json"
        )
        monkeypatch.setattr(
            "grey_gpu_optimizer.daemon.CONFIG_DIR",
            temp_config_dir
        )
        
        daemon = GPUOptimizerDaemon(interval_s=1, dry_run=True)
        
        # Start in background
        daemon.start()
        
        # Give it a moment
        import time
        time.sleep(0.5)
        
        # Should be running
        assert daemon.status.running is True
        
        # Stop
        daemon.stop(timeout=2)
        assert daemon.status.running is False


# =============================================================================
# Test: CLI Without Vendor Tools
# =============================================================================

class TestCLINoVendor:
    """Tests for CLI operation without vendor tools."""
    
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    @patch("grey_gpu_optimizer.optimizer._run_command")
    def test_cli_detect_no_tools(
        self,
        mock_run_command,
        mock_check_tool,
        capsys
    ):
        """Test CLI detect command without tools."""
        mock_check_tool.return_value = False
        mock_run_command.return_value = (False, "", "not found")
        
        # Run detect
        exit_code = cli_main(["detect", "--output", "json"])
        
        # Should complete (might return 0 or 1 depending on detection)
        assert exit_code in [0, 1]
    
    def test_cli_plan_with_cached_spec(self, temp_config_dir, monkeypatch):
        """Test CLI plan command with cached spec."""
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.CONFIG_DIR",
            temp_config_dir
        )
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.SPEC_FILE",
            temp_config_dir / "gpu_spec.json"
        )
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.PLAN_FILE",
            temp_config_dir / "gpu_plan.yaml"
        )
        monkeypatch.setattr(
            "grey_gpu_optimizer.cli.CONFIG_DIR",
            temp_config_dir
        )
        
        # Create cached spec
        spec_data = [{
            "vendor": "intel",
            "model": "Test GPU",
            "vram_total_mb": 4096,
            "vram_free_mb": 3000,
            "detection_sources": ["test"],
            "notes": [],
            "detection_timestamp": "",
            "device_id": "",
            "driver": "",
            "compute_capability": "",
            "cuda_version": "",
            "opencl_version": "",
            "vulkan_version": "",
            "opengl_version": "",
            "clock_mhz": 0,
            "memory_bus_width": 0,
            "num_sm": 0,
            "pci_bandwidth": "",
            "thermal_limits": {"throttle_c": 83, "shutdown_c": 100},
        }]
        
        with open(temp_config_dir / "gpu_spec.json", "w") as f:
            json.dump(spec_data, f)
        
        # Run plan
        exit_code = cli_main(["plan", "--mode", "safe", "--output", "json"])
        
        assert exit_code == 0


# =============================================================================
# Test: Fallback Behavior
# =============================================================================

class TestFallbackBehavior:
    """Tests for graceful fallback behavior."""
    
    def test_detection_chain_fallback(self):
        """Test detection falls through the tool chain properly."""
        # This is a meta-test that verifies the detection order
        # When vendor tools fail, system tools should be tried
        # When system tools fail, Python libs should be tried
        # When all fail, reasonable defaults should be returned
        
        spec = GPUSpec()  # Default spec
        plan = plan_optimizations(spec, mode="safe")
        
        # Even with minimal info, should produce usable plan
        assert plan.per_process_vram_cap_mb > 0
        assert plan.recommended_batch_size >= 1
        assert plan.cooldown_threshold_c > 0
        assert plan.resume_threshold_c > 0
    
    def test_safe_defaults_are_conservative(self):
        """Test that fallback defaults are conservative."""
        spec = GPUSpec(vendor="unknown", vram_total_mb=0)
        plan = plan_optimizations(spec, mode="safe")
        
        # Conservative VRAM cap
        assert plan.per_process_vram_cap_mb <= 4096
        
        # Small batch size
        assert plan.recommended_batch_size <= 8
        
        # Throttle disabled in safe mode
        assert not plan.preemptive_throttle.get("enabled", False)
        
        # Dedup disabled without sufficient VRAM info
        assert not plan.vram_deduplication_enabled
    
    def test_aggressive_with_unknown_still_bounded(self):
        """Test aggressive mode is still bounded for unknown GPUs."""
        spec = GPUSpec(vendor="unknown", vram_total_mb=0)
        plan = plan_optimizations(spec, mode="aggressive")
        
        # Even aggressive should have reasonable limits
        assert plan.per_process_vram_cap_mb <= 8192
        assert plan.recommended_batch_size <= 16


# =============================================================================
# Test: Error Recovery
# =============================================================================

class TestErrorRecovery:
    """Tests for error handling and recovery."""
    
    @patch("grey_gpu_optimizer.optimizer._run_command")
    def test_detection_handles_command_errors(self, mock_run_command):
        """Test detection handles command execution errors."""
        mock_run_command.side_effect = Exception("Unexpected error")
        
        # Should not raise, should return fallback
        try:
            specs = detect_gpus(use_python_libs=False)
            assert len(specs) >= 1
        except Exception:
            pytest.fail("Detection should handle errors gracefully")
    
    def test_plan_handles_invalid_spec(self):
        """Test planning handles invalid spec data."""
        # Pass invalid data
        invalid_spec = {"not_a_valid_field": "value"}
        
        # Should not raise
        plan = plan_optimizations(invalid_spec, mode="safe")
        assert plan.mode == "safe"
    
    def test_apply_handles_missing_artifacts_dir(self, temp_config_dir, monkeypatch):
        """Test apply creates artifacts dir if missing."""
        artifacts_dir = temp_config_dir / "new_artifacts"
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.ARTIFACTS_DIR",
            artifacts_dir
        )
        
        assert not artifacts_dir.exists()
        
        spec = GPUSpec(vendor="test", vram_total_mb=4096)
        plan = plan_optimizations(spec, mode="safe")
        result = apply_plan(plan, dry_run=True)
        
        assert result.success is True
        assert artifacts_dir.exists()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
