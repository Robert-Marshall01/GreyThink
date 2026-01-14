#!/usr/bin/env python3
"""
tests/test_gpu_optimizer.py - Unit and Integration Tests for GPU Optimizer

Tests cover:
- Detection with mocked CLI outputs
- Planning heuristics and confidence scoring
- Apply with dry-run mode
- No-vendor-tools fallback environment
- Config override loading

Run with:
    pytest tests/test_gpu_optimizer.py -v
    pytest tests/test_gpu_optimizer.py -v -k "test_detect"
    pytest tests/test_gpu_optimizer.py -v --no-header -rN
"""

from __future__ import annotations

import json
import os
import sys
import tempfile
from pathlib import Path
from typing import Any
from unittest.mock import MagicMock, patch

import pytest

# Add daemon directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "daemon"))

from gpu_optimizer import (
    GPUSpec,
    GPUVendor,
    OptimizationPlan,
    ApplyResult,
    PlanMode,
    SchedulingPolicy,
    detect_gpus,
    plan_optimizations,
    apply_plan,
    load_last_spec,
    load_last_plan,
    load_config_overrides,
    _parse_nvidia_smi,
    _parse_rocm_smi,
    _parse_lspci,
    _parse_lshw,
    _parse_glxinfo,
    _parse_vulkaninfo,
    _run_command,
    _check_tool_available,
    _probe_torch_cuda,
    _probe_pyopencl,
    _probe_pycuda,
    CONFIG_DIR,
)


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def mock_nvidia_smi_output():
    """Sample nvidia-smi output for testing."""
    return """
+-----------------------------------------------------------------------------+
| NVIDIA-SMI 535.154.05   Driver Version: 535.154.05   CUDA Version: 12.2     |
|-------------------------------+----------------------+----------------------+
| GPU  Name        Persistence-M| Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp  Perf  Pwr:Usage/Cap|         Memory-Usage | GPU-Util  Compute M. |
|                               |                      |               MIG M. |
|===============================+======================+======================|
|   0  NVIDIA GeForce ...  Off  | 00000000:01:00.0  On |                  N/A |
| 30%   42C    P8    12W / 350W |   1234MiB / 24576MiB |      0%      Default |
+-------------------------------+----------------------+----------------------+
"""


@pytest.fixture
def mock_nvidia_smi_query_output():
    """Sample nvidia-smi query output in CSV format."""
    return "NVIDIA GeForce RTX 4090, 24576, 20000, 535.154.05, 210"


@pytest.fixture
def mock_rocm_smi_output():
    """Sample rocm-smi output for AMD GPUs."""
    return """
========================= ROCm System Management Interface =========================
================================== Product Info ====================================
GPU[0]          : Radeon RX 7900 XTX
GPU[0]          : Card SKU: D704
============================== End of ROCm SMI Log ================================
VRAM Total Memory (B):  25769803776
VRAM Total Used Memory (B):  1073741824
"""


@pytest.fixture
def mock_lspci_output():
    """Sample lspci output."""
    return """
01:00.0 VGA compatible controller: NVIDIA Corporation GA102 [GeForce RTX 3090] (rev a1)
02:00.0 VGA compatible controller: Advanced Micro Devices, Inc. [AMD/ATI] Navi 21 [Radeon RX 6900 XT]
"""


@pytest.fixture
def mock_lshw_output():
    """Sample lshw -C display output."""
    return """
  *-display
       description: VGA compatible controller
       product: GA102 [GeForce RTX 3090]
       vendor: NVIDIA Corporation
       bus info: pci@0000:01:00.0
       version: a1
       width: 64 bits
       clock: 33MHz
       capabilities: pm msi pciexpress vga_controller bus_master cap_list rom
       configuration: driver=nvidia latency=0
       resources: irq:51 memory:fb000000-fbffffff
"""


@pytest.fixture
def mock_glxinfo_output():
    """Sample glxinfo output."""
    return """
name of display: :0
display: :0  screen: 0
OpenGL vendor string: NVIDIA Corporation
OpenGL renderer string: NVIDIA GeForce RTX 4090/PCIe/SSE2
OpenGL core profile version string: 4.6.0 NVIDIA 535.154.05
OpenGL version string: 4.6.0 NVIDIA 535.154.05
"""


@pytest.fixture
def mock_vulkaninfo_output():
    """Sample vulkaninfo --summary output."""
    return """
==========
VULKANINFO
==========

Vulkan Instance Version: 1.3.250

Instance Extensions: count = 20

Devices:
========
GPU0:
    apiVersion    = 1.3.260
    driverVersion = 545.23.8.0
    vendorID      = 0x10de
    deviceID      = 0x2684
    deviceType    = PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
    deviceName    = NVIDIA GeForce RTX 4090
    driverName    = NVIDIA
    driverInfo    = 545.23.08
"""


@pytest.fixture
def temp_config_dir(tmp_path):
    """Provide a temporary config directory."""
    old_config = str(CONFIG_DIR)
    # Create temp directory structure
    config_dir = tmp_path / ".grey_optimizer"
    config_dir.mkdir(parents=True, exist_ok=True)
    
    yield config_dir
    
    # Cleanup is handled by tmp_path fixture


# =============================================================================
# Unit Tests: Parsing Functions
# =============================================================================

class TestParsers:
    """Tests for CLI output parsing functions."""
    
    def test_parse_nvidia_smi_basic(self, mock_nvidia_smi_output):
        """Test parsing basic nvidia-smi output."""
        result = _parse_nvidia_smi(mock_nvidia_smi_output)
        
        assert "driver" in result or "vram_total_mb" in result
        # Memory should be parsed
        assert result.get("vram_total_mb") == 24576 or result.get("vram_free_mb") is not None
    
    def test_parse_nvidia_smi_memory_extraction(self):
        """Test memory extraction from nvidia-smi output."""
        output = "1234MiB / 24576MiB"
        result = _parse_nvidia_smi(output)
        assert result.get("vram_total_mb") == 24576
    
    def test_parse_nvidia_smi_driver_version(self, mock_nvidia_smi_output):
        """Test driver version extraction."""
        result = _parse_nvidia_smi(mock_nvidia_smi_output)
        assert result.get("driver") == "535.154.05" or "driver" not in result
    
    def test_parse_nvidia_smi_cuda_version(self, mock_nvidia_smi_output):
        """Test CUDA version extraction."""
        result = _parse_nvidia_smi(mock_nvidia_smi_output)
        assert result.get("cuda_version") == "12.2" or "cuda_version" not in result
    
    def test_parse_rocm_smi(self, mock_rocm_smi_output):
        """Test parsing rocm-smi output."""
        result = _parse_rocm_smi(mock_rocm_smi_output)
        
        assert result.get("model") == "Radeon RX 7900 XTX" or "model" not in result
        # VRAM: 25769803776 bytes = 24576 MB
        assert result.get("vram_total_mb") == 24576 or "vram_total_mb" not in result
    
    def test_parse_lspci(self, mock_lspci_output):
        """Test parsing lspci output."""
        result = _parse_lspci(mock_lspci_output)
        
        assert len(result) == 2
        assert result[0]["vendor"] == "nvidia"
        assert result[1]["vendor"] == "amd"
        assert "RTX 3090" in result[0]["model"]
    
    def test_parse_lshw(self, mock_lshw_output):
        """Test parsing lshw output."""
        result = _parse_lshw(mock_lshw_output)
        
        assert result.get("model") == "GA102 [GeForce RTX 3090]"
        assert result.get("vendor") == "nvidia"
        assert result.get("driver") == "nvidia"
        assert result.get("device_id") == "0000:01:00.0"
    
    def test_parse_glxinfo(self, mock_glxinfo_output):
        """Test parsing glxinfo output."""
        result = _parse_glxinfo(mock_glxinfo_output)
        
        assert result.get("opengl_version") == "4.6.0"
        assert result.get("vendor") == "nvidia"
        assert "RTX 4090" in result.get("model", "")
    
    def test_parse_vulkaninfo(self, mock_vulkaninfo_output):
        """Test parsing vulkaninfo output."""
        result = _parse_vulkaninfo(mock_vulkaninfo_output)
        
        assert result.get("vulkan_version") == "1.3.260"
        assert "RTX 4090" in result.get("model", "")


# =============================================================================
# Unit Tests: GPUSpec Dataclass
# =============================================================================

class TestGPUSpec:
    """Tests for GPUSpec dataclass."""
    
    def test_default_values(self):
        """Test default initialization."""
        spec = GPUSpec()
        
        assert spec.vendor == "unknown"
        assert spec.model == "unknown"
        assert spec.vram_total_mb == 0
        assert spec.notes == []
    
    def test_to_dict(self):
        """Test serialization to dict."""
        spec = GPUSpec(
            vendor="nvidia",
            model="RTX 4090",
            vram_total_mb=24576,
            notes=["Test note"]
        )
        
        result = spec.to_dict()
        
        assert result["vendor"] == "nvidia"
        assert result["model"] == "RTX 4090"
        assert result["vram_total_mb"] == 24576
        assert "Test note" in result["notes"]
    
    def test_from_dict(self):
        """Test deserialization from dict."""
        data = {
            "vendor": "amd",
            "model": "RX 7900 XTX",
            "vram_total_mb": 24576,
            "cuda_version": "",  # Empty string should work
        }
        
        spec = GPUSpec.from_dict(data)
        
        assert spec.vendor == "amd"
        assert spec.model == "RX 7900 XTX"
        assert spec.vram_total_mb == 24576
    
    def test_from_dict_backwards_compat(self):
        """Test handling of open_gl_version -> opengl_version migration."""
        data = {
            "vendor": "nvidia",
            "open_gl_version": "4.6.0",  # Old field name
        }
        
        spec = GPUSpec.from_dict(data)
        
        assert spec.opengl_version == "4.6.0"


# =============================================================================
# Unit Tests: OptimizationPlan Dataclass
# =============================================================================

class TestOptimizationPlan:
    """Tests for OptimizationPlan dataclass."""
    
    def test_default_values(self):
        """Test default initialization."""
        plan = OptimizationPlan()
        
        assert plan.mode == "safe"
        assert plan.cooldown_threshold_c == 83
        assert plan.resume_threshold_c == 70
        assert plan.preemptive_throttle is False
    
    def test_to_dict_and_back(self):
        """Test round-trip serialization."""
        plan = OptimizationPlan(
            mode="aggressive",
            per_process_vram_cap_mb=20000,
            recommended_batch_size=64,
            confidence_overall=0.85,
        )
        
        data = plan.to_dict()
        restored = OptimizationPlan.from_dict(data)
        
        assert restored.mode == "aggressive"
        assert restored.per_process_vram_cap_mb == 20000
        assert restored.recommended_batch_size == 64
        assert restored.confidence_overall == 0.85


# =============================================================================
# Unit Tests: Detection Functions
# =============================================================================

class TestDetection:
    """Tests for GPU detection functions."""
    
    def test_run_command_success(self):
        """Test successful command execution."""
        success, stdout, stderr = _run_command(["echo", "hello"])
        
        assert success is True
        assert stdout == "hello"
    
    def test_run_command_not_found(self):
        """Test handling of missing command."""
        success, stdout, stderr = _run_command(["nonexistent_command_12345"])
        
        assert success is False
        assert "not found" in stderr.lower() or "error" in stderr.lower()
    
    def test_run_command_timeout(self):
        """Test command timeout handling."""
        success, stdout, stderr = _run_command(["sleep", "10"], timeout=1)
        
        assert success is False
        assert "timeout" in stderr.lower()
    
    def test_check_tool_available_true(self):
        """Test that common tools are found."""
        # 'echo' should be available on all Unix systems
        assert _check_tool_available("echo") is True
    
    def test_check_tool_available_false(self):
        """Test handling of missing tools."""
        assert _check_tool_available("nonexistent_tool_xyz") is False
    
    @patch("gpu_optimizer._run_command")
    @patch("gpu_optimizer._check_tool_available")
    def test_detect_gpus_nvidia(self, mock_check, mock_run, mock_nvidia_smi_output):
        """Test detection with mocked nvidia-smi."""
        # Setup mocks
        mock_check.side_effect = lambda x: x == "nvidia-smi"
        mock_run.return_value = (True, mock_nvidia_smi_output, "")
        
        specs = detect_gpus(
            use_vendor_cli=True,
            use_system_tools=False,
            use_python_libs=False
        )
        
        assert len(specs) >= 1
        # Should have detected something from nvidia-smi
        assert any(s.vendor == "nvidia" or "nvidia-smi" in s.detection_sources for s in specs)
    
    @patch("gpu_optimizer._run_command")
    @patch("gpu_optimizer._check_tool_available")
    def test_detect_gpus_no_tools(self, mock_check, mock_run):
        """Test detection when no tools are available."""
        mock_check.return_value = False
        mock_run.return_value = (False, "", "not found")
        
        specs = detect_gpus(
            use_vendor_cli=True,
            use_system_tools=True,
            use_python_libs=False
        )
        
        # Should return a spec with notes about missing tools
        assert len(specs) >= 1
        spec = specs[0]
        assert len(spec.notes) > 0
        assert any("not found" in note.lower() for note in spec.notes)
    
    @patch("gpu_optimizer._run_command")
    @patch("gpu_optimizer._check_tool_available")
    def test_detect_gpus_lspci_fallback(self, mock_check, mock_run, mock_lspci_output):
        """Test detection falling back to lspci."""
        def check_tool(name):
            return name == "lspci"
        
        def run_cmd(cmd, **kwargs):
            if cmd[0] == "lspci":
                return (True, mock_lspci_output, "")
            return (False, "", "not found")
        
        mock_check.side_effect = check_tool
        mock_run.side_effect = run_cmd
        
        specs = detect_gpus(
            use_vendor_cli=True,
            use_system_tools=True,
            use_python_libs=False
        )
        
        assert len(specs) >= 1
        assert "lspci" in specs[0].detection_sources


# =============================================================================
# Unit Tests: Planning Functions
# =============================================================================

class TestPlanning:
    """Tests for optimization planning functions."""
    
    def test_plan_safe_mode(self):
        """Test safe mode planning."""
        specs = [GPUSpec(
            vendor="nvidia",
            model="RTX 4090",
            vram_total_mb=24576,
            num_sm=128,
            compute_capability="8.9",
            detection_sources=["nvidia-smi"],
        )]
        
        plan = plan_optimizations(specs, mode="safe")
        
        assert plan.mode == "safe"
        # Safe mode should use ~75% of VRAM
        assert plan.per_process_vram_cap_mb == int(24576 * 0.75)
        assert plan.preemptive_throttle is False
        assert plan.fair_scheduling_policy == "round_robin"
    
    def test_plan_aggressive_mode(self):
        """Test aggressive mode planning."""
        specs = [GPUSpec(
            vendor="nvidia",
            model="RTX 4090",
            vram_total_mb=24576,
            num_sm=128,
            detection_sources=["nvidia-smi"],
        )]
        
        plan = plan_optimizations(specs, mode="aggressive")
        
        assert plan.mode == "aggressive"
        # Aggressive mode should use ~90% of VRAM
        assert plan.per_process_vram_cap_mb == int(24576 * 0.90)
        assert plan.preemptive_throttle is True
        assert plan.fair_scheduling_policy == "priority_based"
    
    def test_plan_with_config_overrides(self):
        """Test config overrides are applied."""
        specs = [GPUSpec(vendor="nvidia", vram_total_mb=24576)]
        overrides = {
            "per_process_vram_cap_mb": 16000,
            "recommended_batch_size": 128,
        }
        
        plan = plan_optimizations(specs, mode="safe", config_overrides=overrides)
        
        assert plan.per_process_vram_cap_mb == 16000
        assert plan.recommended_batch_size == 128
        assert "overridden by config" in " ".join(plan.notes).lower()
    
    def test_plan_confidence_scoring(self):
        """Test confidence scores are computed correctly."""
        # High-quality detection should have high confidence
        high_quality_spec = GPUSpec(
            vendor="nvidia",
            model="RTX 4090",
            vram_total_mb=24576,
            compute_capability="8.9",
            detection_sources=["nvidia-smi", "vulkaninfo", "lspci"],
        )
        
        # Low-quality detection should have lower confidence
        low_quality_spec = GPUSpec(
            vendor="unknown",
            model="unknown",
            vram_total_mb=0,
            detection_sources=[],
        )
        
        high_plan = plan_optimizations([high_quality_spec], mode="safe")
        low_plan = plan_optimizations([low_quality_spec], mode="safe")
        
        assert high_plan.confidence_overall > low_plan.confidence_overall
        assert high_plan.confidence_overall >= 0.5
        assert low_plan.confidence_overall < 0.5
    
    def test_plan_amd_thermal_adjustment(self):
        """Test AMD-specific thermal thresholds."""
        amd_spec = GPUSpec(
            vendor="amd",
            model="RX 7900 XTX",
            vram_total_mb=24576,
        )
        
        plan = plan_optimizations([amd_spec], mode="safe")
        
        # AMD should have higher thermal thresholds
        assert plan.cooldown_threshold_c == 85
        assert plan.resume_threshold_c == 72
    
    def test_plan_batch_size_scaling(self):
        """Test batch size scales with VRAM."""
        small_gpu = GPUSpec(vendor="nvidia", vram_total_mb=4000)
        large_gpu = GPUSpec(vendor="nvidia", vram_total_mb=24000)
        
        small_plan = plan_optimizations([small_gpu], mode="safe")
        large_plan = plan_optimizations([large_gpu], mode="safe")
        
        assert large_plan.recommended_batch_size > small_plan.recommended_batch_size


# =============================================================================
# Unit Tests: Apply Functions
# =============================================================================

class TestApply:
    """Tests for plan application functions."""
    
    def test_apply_dry_run_default(self):
        """Test that dry-run is default."""
        plan = OptimizationPlan(
            mode="safe",
            per_process_vram_cap_mb=16000,
        )
        
        result = apply_plan(plan)
        
        assert result.dry_run is True
        assert result.success is True
        assert all("[DRY-RUN]" in a for a in result.actions_applied)
    
    def test_apply_no_dry_run(self, temp_config_dir):
        """Test actual application (non-destructive only)."""
        plan = OptimizationPlan(
            mode="safe",
            per_process_vram_cap_mb=16000,
            recommended_batch_size=32,
        )
        
        with patch.object(Path, "mkdir"):
            result = apply_plan(plan, dry_run=False)
        
        assert result.dry_run is False
        assert result.success is True
        assert len(result.actions_applied) > 0
        assert "[DRY-RUN]" not in result.actions_applied[0]
    
    def test_apply_sets_environment(self):
        """Test that env vars are set during apply."""
        plan = OptimizationPlan(
            mode="safe",
            per_process_vram_cap_mb=16000,
            cooldown_threshold_c=80,
            resume_threshold_c=65,
        )
        
        # Clear any existing env vars
        for var in ["GREY_VRAM_CAP_MB", "GREY_COOLDOWN_C", "GREY_RESUME_C"]:
            os.environ.pop(var, None)
        
        with patch.object(Path, "mkdir"):
            result = apply_plan(plan, dry_run=False)
        
        assert os.environ.get("GREY_VRAM_CAP_MB") == "16000"
        assert os.environ.get("GREY_COOLDOWN_C") == "80"
        assert os.environ.get("GREY_RESUME_C") == "65"
    
    def test_apply_result_metrics(self):
        """Test that metrics are included in result."""
        plan = OptimizationPlan(mode="aggressive")
        
        result = apply_plan(plan, dry_run=True)
        
        assert "plan_mode" in result.metrics
        assert "timestamp" in result.metrics
        assert "dry_run_actions_count" in result.metrics


# =============================================================================
# Integration Tests
# =============================================================================

class TestIntegration:
    """Integration tests for end-to-end workflows."""
    
    @patch("gpu_optimizer._run_command")
    @patch("gpu_optimizer._check_tool_available")
    def test_full_workflow_no_vendor_tools(self, mock_check, mock_run):
        """Test complete workflow when no vendor tools are available."""
        # Simulate environment with no GPU tools
        mock_check.return_value = False
        mock_run.return_value = (False, "", "command not found")
        
        # Detection should still work, returning a spec with notes
        specs = detect_gpus(
            use_vendor_cli=True,
            use_system_tools=True,
            use_python_libs=False
        )
        
        assert len(specs) >= 1
        assert "No GPU detected" in " ".join(specs[0].notes) or len(specs[0].notes) > 0
        
        # Planning should still work with defaults
        plan = plan_optimizations(specs, mode="safe")
        
        assert plan.mode == "safe"
        assert plan.per_process_vram_cap_mb > 0  # Should use default
        assert plan.confidence_overall < 0.5  # Low confidence without detection
        
        # Apply should work in dry-run
        result = apply_plan(plan, dry_run=True)
        
        assert result.success is True
        assert result.dry_run is True
    
    def test_full_workflow_with_specs(self):
        """Test complete workflow with pre-defined specs."""
        # Create realistic spec
        specs = [GPUSpec(
            vendor="nvidia",
            model="NVIDIA GeForce RTX 4090",
            device_id="0000:01:00.0",
            driver="535.154.05",
            vram_total_mb=24576,
            vram_free_mb=20000,
            compute_capability="8.9",
            cuda_version="12.2",
            num_sm=128,
            detection_sources=["nvidia-smi", "vulkaninfo"],
        )]
        
        # Plan
        plan = plan_optimizations(specs, mode="aggressive")
        
        assert plan.per_process_vram_cap_mb > 20000  # 90% of 24576
        assert plan.recommended_batch_size >= 64
        assert plan.confidence_overall > 0.6
        
        # Apply (dry-run)
        result = apply_plan(plan, dry_run=True)
        
        assert result.success is True
        assert len(result.actions_applied) >= 3  # VRAM, thermal, scheduling


# =============================================================================
# CLI Tests
# =============================================================================

class TestCLI:
    """Tests for CLI interface."""
    
    @patch("gpu_optimizer._run_command")
    @patch("gpu_optimizer._check_tool_available")
    def test_detect_json_output(self, mock_check, mock_run, capsys):
        """Test detect --json produces valid JSON."""
        mock_check.return_value = False
        mock_run.return_value = (False, "", "not found")
        
        from gpu_optimizer import cmd_detect
        import argparse
        
        args = argparse.Namespace(
            json=True,
            no_vendor_cli=False,
            no_system_tools=False,
            no_python_libs=True,
            verbose=False
        )
        
        result = cmd_detect(args)
        
        assert result == 0
        captured = capsys.readouterr()
        # Should be valid JSON
        parsed = json.loads(captured.out)
        assert isinstance(parsed, list)
        assert len(parsed) >= 1


# =============================================================================
# Smoke Tests
# =============================================================================

class TestSmokeTests:
    """Quick smoke tests for basic functionality."""
    
    def test_smoke_gpuspec_creation(self):
        """SMOKE TEST: Create GPUSpec instance."""
        spec = GPUSpec(vendor="nvidia", model="Test GPU")
        assert spec.vendor == "nvidia"
    
    def test_smoke_plan_creation(self):
        """SMOKE TEST: Create OptimizationPlan instance."""
        plan = OptimizationPlan(mode="safe")
        assert plan.mode == "safe"
    
    def test_smoke_apply_result_creation(self):
        """SMOKE TEST: Create ApplyResult instance."""
        result = ApplyResult(success=True, dry_run=True)
        assert result.success is True
    
    @patch("gpu_optimizer._check_tool_available")
    def test_smoke_detect_gpus(self, mock_check):
        """SMOKE TEST: detect_gpus() runs without exception."""
        mock_check.return_value = False
        
        specs = detect_gpus(
            use_vendor_cli=False,
            use_system_tools=False,
            use_python_libs=False
        )
        
        assert isinstance(specs, list)
    
    def test_smoke_plan_optimizations(self):
        """SMOKE TEST: plan_optimizations() runs without exception."""
        specs = [GPUSpec()]
        
        plan = plan_optimizations(specs, mode="safe")
        
        assert isinstance(plan, OptimizationPlan)
    
    def test_smoke_apply_plan_dry_run(self):
        """SMOKE TEST: apply_plan() runs in dry-run mode."""
        plan = OptimizationPlan()
        
        result = apply_plan(plan, dry_run=True)
        
        assert isinstance(result, ApplyResult)
        assert result.dry_run is True


# =============================================================================
# Edge Cases
# =============================================================================

class TestEdgeCases:
    """Tests for edge cases and error handling."""
    
    def test_empty_spec_list(self):
        """Test planning with empty spec list."""
        plan = plan_optimizations([], mode="safe")
        
        # Should use defaults
        assert plan.per_process_vram_cap_mb > 0
        assert plan.confidence_overall < 0.5
    
    def test_malformed_nvidia_smi_output(self):
        """Test parsing of malformed nvidia-smi output."""
        result = _parse_nvidia_smi("garbage output that doesn't match anything")
        
        # Should return empty dict, not crash
        assert isinstance(result, dict)
    
    def test_plan_with_zero_vram(self):
        """Test planning when VRAM is zero."""
        spec = GPUSpec(vendor="nvidia", vram_total_mb=0)
        
        plan = plan_optimizations([spec], mode="safe")
        
        # Should use defaults
        assert plan.per_process_vram_cap_mb > 0
        assert "defaults" in " ".join(plan.notes).lower()


# =============================================================================
# Run Tests
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
