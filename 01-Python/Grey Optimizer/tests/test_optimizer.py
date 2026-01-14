#!/usr/bin/env python3
"""
tests/test_optimizer.py - Unit Tests for Grey GPU Optimizer

This module provides pytest-based unit tests for the core optimizer
functionality including detection, planning, and enforcement.

Test Categories:
- Detection tests with mocked CLI outputs
- Planning tests for various GPU configurations
- Apply tests with dry-run validation
- Edge case and error handling tests

Usage:
    pytest tests/test_optimizer.py -v
    pytest tests/test_optimizer.py -v -k "test_detect"
"""

from __future__ import annotations

import json
import os
import tempfile
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

# Import modules under test
from grey_gpu_optimizer.optimizer import (
    GPUSpec,
    OptimizationPlan,
    ApplyResult,
    detect_gpus,
    plan_optimizations,
    apply_plan,
    save_spec,
    load_spec,
    save_plan,
    load_plan,
    _run_command,
    _parse_nvidia_smi,
    _parse_lspci,
    _parse_glxinfo,
    _compute_spec_hash,
)


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def mock_nvidia_smi_output():
    """Mock nvidia-smi output for an RTX 3080."""
    return """
+-----------------------------------------------------------------------------+
| NVIDIA-SMI 525.85.05    Driver Version: 525.85.05    CUDA Version: 12.0     |
|-------------------------------+----------------------+----------------------+
| GPU  Name        Persistence-M| Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp  Perf  Pwr:Usage/Cap|         Memory-Usage | GPU-Util  Compute M. |
|                               |                      |               MIG M. |
|===============================+======================+======================|
|   0  NVIDIA GeForce ...  Off  | 00000000:01:00.0 Off |                  N/A |
| 30%   45C    P8    15W / 320W |    512MiB / 10240MiB |      0%      Default |
|                               |                      |                  N/A |
+-------------------------------+----------------------+----------------------+
"""


@pytest.fixture
def mock_nvidia_smi_query_output():
    """Mock nvidia-smi --query-gpu output."""
    return "NVIDIA GeForce RTX 3080, 10240, 9728, 525.85.05, 1710"


@pytest.fixture
def mock_lspci_output():
    """Mock lspci output."""
    return """
00:02.0 VGA compatible controller: Intel Corporation Device 4680 (rev 0c)
01:00.0 VGA compatible controller: NVIDIA Corporation GA102 [GeForce RTX 3080] (rev a1)
"""


@pytest.fixture
def mock_glxinfo_output():
    """Mock glxinfo output."""
    return """
name of display: :0
display: :0  screen: 0
OpenGL vendor string: NVIDIA Corporation
OpenGL renderer string: NVIDIA GeForce RTX 3080/PCIe/SSE2
OpenGL version string: 4.6.0 NVIDIA 525.85.05
"""


@pytest.fixture
def sample_gpu_spec():
    """Create a sample GPUSpec for testing."""
    return GPUSpec(
        vendor="nvidia",
        model="GeForce RTX 3080",
        device_id="01:00.0",
        driver="525.85.05",
        vram_total_mb=10240,
        vram_free_mb=9728,
        compute_capability="8.6",
        cuda_version="12.0",
        detection_sources=["nvidia-smi", "lspci"],
    )


@pytest.fixture
def sample_intel_spec():
    """Create a sample Intel GPUSpec for testing."""
    return GPUSpec(
        vendor="intel",
        model="UHD Graphics 770",
        device_id="00:02.0",
        driver="i915",
        vram_total_mb=6144,
        vram_free_mb=4096,
        opengl_version="4.6",
        detection_sources=["lspci", "glxinfo"],
    )


@pytest.fixture
def temp_config_dir(tmp_path):
    """Create a temporary config directory."""
    config_dir = tmp_path / ".grey_optimizer"
    config_dir.mkdir(parents=True)
    return config_dir


# =============================================================================
# Test: Command Runner
# =============================================================================

class TestCommandRunner:
    """Tests for _run_command utility."""
    
    def test_run_command_success(self):
        """Test successful command execution."""
        success, stdout, stderr = _run_command(["echo", "hello"])
        assert success is True
        assert "hello" in stdout
    
    def test_run_command_not_found(self):
        """Test command not found handling."""
        success, stdout, stderr = _run_command(["nonexistent_command_xyz"])
        assert success is False
        assert "not found" in stderr.lower() or "error" in stderr.lower()
    
    def test_run_command_timeout(self):
        """Test command timeout handling."""
        success, stdout, stderr = _run_command(["sleep", "10"], timeout=1)
        assert success is False
        assert "timeout" in stderr.lower()


# =============================================================================
# Test: CLI Output Parsing
# =============================================================================

class TestCLIParsing:
    """Tests for CLI output parsing functions."""
    
    def test_parse_nvidia_smi(self, mock_nvidia_smi_output):
        """Test nvidia-smi output parsing."""
        result = _parse_nvidia_smi(mock_nvidia_smi_output)
        
        assert result.get("vram_total_mb") == 10240
        assert result.get("driver") == "525.85.05"
        assert result.get("cuda_version") == "12.0"
        assert result.get("current_temp_c") == 45
    
    def test_parse_lspci(self, mock_lspci_output):
        """Test lspci output parsing."""
        results = _parse_lspci(mock_lspci_output)
        
        assert len(results) == 2
        
        # Find NVIDIA entry
        nvidia = next((r for r in results if r.get("vendor") == "nvidia"), None)
        assert nvidia is not None
        assert "RTX 3080" in nvidia.get("model", "")
    
    def test_parse_glxinfo(self, mock_glxinfo_output):
        """Test glxinfo output parsing."""
        result = _parse_glxinfo(mock_glxinfo_output)
        
        assert result.get("vendor") == "nvidia"
        assert result.get("opengl_version") == "4.6"
        assert "RTX 3080" in result.get("model", "")


# =============================================================================
# Test: GPU Detection
# =============================================================================

class TestGPUDetection:
    """Tests for detect_gpus function."""
    
    @patch("grey_gpu_optimizer.optimizer._run_command")
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    def test_detect_nvidia_gpu(
        self,
        mock_check_tool,
        mock_run_command,
        mock_nvidia_smi_query_output
    ):
        """Test NVIDIA GPU detection with mocked nvidia-smi."""
        mock_check_tool.return_value = True
        mock_run_command.return_value = (True, mock_nvidia_smi_query_output, "")
        
        specs = detect_gpus(
            use_system_tools=False,
            use_python_libs=False
        )
        
        assert len(specs) >= 1
        spec = specs[0]
        assert spec.vendor == "nvidia"
    
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    def test_detect_no_tools(self, mock_check_tool):
        """Test detection when no tools are available."""
        mock_check_tool.return_value = False
        
        specs = detect_gpus(use_python_libs=False)
        
        assert len(specs) == 1
        assert "No GPU detected" in str(specs[0].notes) or specs[0].vendor == "unknown"


# =============================================================================
# Test: GPUSpec
# =============================================================================

class TestGPUSpec:
    """Tests for GPUSpec dataclass."""
    
    def test_to_dict(self, sample_gpu_spec):
        """Test conversion to dictionary."""
        data = sample_gpu_spec.to_dict()
        
        assert isinstance(data, dict)
        assert data["vendor"] == "nvidia"
        assert data["vram_total_mb"] == 10240
    
    def test_from_dict(self):
        """Test creation from dictionary."""
        data = {
            "vendor": "amd",
            "model": "RX 6800",
            "vram_total_mb": 16384,
        }
        
        spec = GPUSpec.from_dict(data)
        
        assert spec.vendor == "amd"
        assert spec.model == "RX 6800"
        assert spec.vram_total_mb == 16384
    
    def test_default_values(self):
        """Test default values for GPUSpec."""
        spec = GPUSpec()
        
        assert spec.vendor == "unknown"
        assert spec.model == "unknown"
        assert spec.vram_total_mb == 0


# =============================================================================
# Test: Plan Optimizations
# =============================================================================

class TestPlanOptimizations:
    """Tests for plan_optimizations function."""
    
    def test_safe_plan_small_vram(self, sample_intel_spec):
        """Test safe mode planning for small VRAM GPU."""
        plan = plan_optimizations(sample_intel_spec, mode="safe")
        
        assert plan.mode == "safe"
        assert plan.per_process_vram_cap_mb < sample_intel_spec.vram_total_mb
        assert plan.recommended_batch_size <= 8  # Conservative for Intel
        assert plan.confidence > 0
    
    def test_safe_plan_large_vram(self, sample_gpu_spec):
        """Test safe mode planning for large VRAM GPU."""
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        
        assert plan.mode == "safe"
        assert plan.per_process_vram_cap_mb <= sample_gpu_spec.vram_total_mb * 0.75
        assert plan.recommended_batch_size >= 8
    
    def test_aggressive_plan(self, sample_gpu_spec):
        """Test aggressive mode planning."""
        safe_plan = plan_optimizations(sample_gpu_spec, mode="safe")
        aggressive_plan = plan_optimizations(sample_gpu_spec, mode="aggressive")
        
        assert aggressive_plan.mode == "aggressive"
        assert aggressive_plan.per_process_vram_cap_mb > safe_plan.per_process_vram_cap_mb
        assert aggressive_plan.recommended_batch_size >= safe_plan.recommended_batch_size
    
    def test_plan_has_rationale(self, sample_gpu_spec):
        """Test that plans include rationale strings."""
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        
        assert len(plan.rationale) > 0
        assert "vram_cap" in plan.rationale
        assert "batch_size" in plan.rationale
    
    def test_plan_from_dict(self):
        """Test planning from dict input."""
        spec_dict = {
            "vendor": "nvidia",
            "model": "Test GPU",
            "vram_total_mb": 8192,
        }
        
        plan = plan_optimizations(spec_dict, mode="safe")
        
        assert plan.mode == "safe"
        assert plan.per_process_vram_cap_mb > 0
    
    def test_plan_spec_hash(self, sample_gpu_spec):
        """Test that plan includes spec hash."""
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        
        assert len(plan.spec_hash) == 16
        assert plan.spec_hash == _compute_spec_hash(sample_gpu_spec)


# =============================================================================
# Test: Apply Plan
# =============================================================================

class TestApplyPlan:
    """Tests for apply_plan function."""
    
    def test_dry_run_default(self, sample_gpu_spec):
        """Test that dry-run is default."""
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        result = apply_plan(plan)
        
        assert result.dry_run is True
        assert result.success is True
        assert all("[DRY-RUN]" in a for a in result.actions_applied)
    
    def test_dry_run_explicit(self, sample_gpu_spec):
        """Test explicit dry-run mode."""
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        result = apply_plan(plan, dry_run=True)
        
        assert result.dry_run is True
        assert len(result.actions_applied) > 0
    
    def test_live_requires_consent(self, sample_gpu_spec):
        """Test that live mode requires consent and confirm."""
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        
        # No consent
        result = apply_plan(plan, dry_run=False)
        assert result.success is False
        assert len(result.errors) > 0
        
        # Only consent
        result = apply_plan(plan, dry_run=False, consent=True)
        assert result.success is False
        
        # Only confirm
        result = apply_plan(plan, dry_run=False, confirm=True)
        assert result.success is False
    
    def test_live_with_consent_and_confirm(self, sample_gpu_spec):
        """Test live mode with both flags."""
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        result = apply_plan(plan, dry_run=False, consent=True, confirm=True)
        
        assert result.dry_run is False
        assert result.success is True
        assert all("[DRY-RUN]" not in a for a in result.actions_applied)
    
    def test_artifacts_generated(self, sample_gpu_spec, tmp_path):
        """Test that artifacts are generated."""
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        result = apply_plan(plan, dry_run=True)
        
        assert "vram_reclamation_log" in result.artifacts
        assert "thermal_signature_csv" in result.artifacts
        assert "artifact_report_json" in result.artifacts


# =============================================================================
# Test: Persistence
# =============================================================================

class TestPersistence:
    """Tests for spec and plan persistence."""
    
    def test_save_and_load_spec(self, sample_gpu_spec, temp_config_dir, monkeypatch):
        """Test saving and loading GPU spec."""
        # Patch CONFIG_DIR
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.CONFIG_DIR",
            temp_config_dir
        )
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.SPEC_FILE",
            temp_config_dir / "gpu_spec.json"
        )
        
        # Save
        save_spec([sample_gpu_spec])
        
        # Load
        loaded = load_spec()
        
        assert loaded is not None
        assert len(loaded) == 1
        assert loaded[0].vendor == sample_gpu_spec.vendor
        assert loaded[0].vram_total_mb == sample_gpu_spec.vram_total_mb
    
    def test_load_spec_not_found(self, temp_config_dir, monkeypatch):
        """Test loading when no spec file exists."""
        monkeypatch.setattr(
            "grey_gpu_optimizer.optimizer.SPEC_FILE",
            temp_config_dir / "nonexistent.json"
        )
        
        loaded = load_spec()
        assert loaded is None


# =============================================================================
# Test: Edge Cases
# =============================================================================

class TestEdgeCases:
    """Tests for edge cases and error handling."""
    
    def test_zero_vram_spec(self):
        """Test planning with zero VRAM."""
        spec = GPUSpec(vendor="unknown", vram_total_mb=0)
        plan = plan_optimizations(spec, mode="safe")
        
        assert plan.per_process_vram_cap_mb > 0  # Should use defaults
        assert plan.confidence < 0.5  # Low confidence
    
    def test_unknown_vendor(self):
        """Test planning with unknown vendor."""
        spec = GPUSpec(vendor="unknown", vram_total_mb=8192)
        plan = plan_optimizations(spec, mode="safe")
        
        assert plan.per_process_vram_cap_mb > 0
        assert plan.recommended_batch_size > 0
    
    def test_plan_from_empty_dict(self):
        """Test planning from empty dict."""
        plan = plan_optimizations({}, mode="safe")
        
        assert plan.mode == "safe"
        assert plan.per_process_vram_cap_mb > 0


# =============================================================================
# Test: Integration
# =============================================================================

class TestIntegration:
    """Integration tests for full workflow."""
    
    def test_detect_plan_apply_workflow(self, sample_gpu_spec, temp_config_dir, monkeypatch):
        """Test complete detect -> plan -> apply workflow."""
        # Patch config dir
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
            "grey_gpu_optimizer.optimizer.ARTIFACTS_DIR",
            temp_config_dir / "artifacts"
        )
        
        # Save spec (simulating detection)
        save_spec([sample_gpu_spec])
        
        # Plan
        plan = plan_optimizations(sample_gpu_spec, mode="safe")
        assert plan.confidence > 0.5
        
        # Apply (dry-run)
        result = apply_plan(plan, dry_run=True)
        assert result.success is True
        assert len(result.artifacts) > 0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
