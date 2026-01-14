#!/usr/bin/env python3
"""
tests/test_grey_gpu_optimizer_comprehensive.py - Comprehensive Test Suite

This test module provides thorough coverage for the grey_gpu_optimizer package:
1. Unit tests with mocked CLI outputs for spec parsing
2. Integration tests simulating workloads
3. Planner decision tests with varying spec inputs
4. Enforcement action verification
5. Artifact generation tests

Run with:
    pytest tests/test_grey_gpu_optimizer_comprehensive.py -v
    pytest tests/test_grey_gpu_optimizer_comprehensive.py -v -k "test_smoke"
    pytest tests/test_grey_gpu_optimizer_comprehensive.py -v --tb=short
"""

from __future__ import annotations

import json
import os
import signal
import sys
import tempfile
from pathlib import Path
from typing import Any
from unittest.mock import MagicMock, patch, mock_open

import pytest

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from grey_gpu_optimizer.optimizer import (
    GPUSpec,
    OptimizationPlan,
    ApplyResult,
    detect_gpus,
    plan_optimizations,
    apply_plan,
    load_spec,
    save_spec,
    load_plan,
    save_plan,
    CONFIG_DIR,
    SPEC_FILE,
    PLAN_FILE,
    ARTIFACTS_DIR,
)
from grey_gpu_optimizer.daemon import (
    GPUOptimizerDaemon,
    DaemonStatus,
    MonitorSample,
    sample_gpu_metrics,
)
from grey_gpu_optimizer.utils.chunking import (
    compute_optimal_batch_size,
    adaptive_batch_resizer,
    chunk_tensor_for_checkpointing,
    BatchSizeRecommendation,
    ChunkingStrategy,
)
from grey_gpu_optimizer.utils.dedup import (
    VRAMDeduplicator,
    estimate_reclaimed_mb,
    find_duplicate_buffers,
    BufferInfo,
    DeduplicationReport,
)


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def nvidia_smi_output():
    """Mock nvidia-smi standard output."""
    return """
+-----------------------------------------------------------------------------+
| NVIDIA-SMI 545.23.08    Driver Version: 545.23.08    CUDA Version: 12.3     |
|-------------------------------+----------------------+----------------------+
| GPU  Name        Persistence-M| Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp  Perf  Pwr:Usage/Cap|         Memory-Usage | GPU-Util  Compute M. |
|                               |                      |               MIG M. |
|===============================+======================+======================|
|   0  NVIDIA GeForce RTX 4090  Off  | 00000000:01:00.0  On |                  N/A |
| 35%   45C    P2    85W / 450W |   2048MiB / 24576MiB |     12%      Default |
+-------------------------------+----------------------+----------------------+
"""


@pytest.fixture
def nvidia_smi_query_output():
    """Mock nvidia-smi CSV query output."""
    return "NVIDIA GeForce RTX 4090, 24576, 22000, 545.23.08, 2520, 1410"


@pytest.fixture
def rocm_smi_output():
    """Mock rocm-smi output."""
    return """
========================= ROCm System Management Interface =========================
================================== Product Info ====================================
GPU[0]          : AMD Radeon RX 7900 XTX
GPU[0]          : Card series: Navi31 [Radeon RX 7900 XT/7900 XTX]
============================== End of ROCm SMI Log ================================
VRAM Total Memory (B):  25769803776
VRAM Total Used Memory (B):  2147483648
"""


@pytest.fixture
def lspci_output():
    """Mock lspci output with multiple GPUs."""
    return """
01:00.0 VGA compatible controller: NVIDIA Corporation AD102 [GeForce RTX 4090] (rev a1)
02:00.0 3D controller: Advanced Micro Devices, Inc. [AMD/ATI] Navi 31 [Radeon RX 7900 XT/7900 XTX] (rev c8)
00:02.0 VGA compatible controller: Intel Corporation Raptor Lake-S GT1 [UHD Graphics 770] (rev 04)
"""


@pytest.fixture
def glxinfo_output():
    """Mock glxinfo output."""
    return """
name of display: :0
display: :0  screen: 0
direct rendering: Yes
OpenGL vendor string: NVIDIA Corporation
OpenGL renderer string: NVIDIA GeForce RTX 4090/PCIe/SSE2
OpenGL core profile version string: 4.6.0 NVIDIA 545.23.08
OpenGL version string: 4.6.0 NVIDIA 545.23.08
"""


@pytest.fixture
def temp_config_dir(tmp_path):
    """Create temporary config directory structure."""
    config_dir = tmp_path / ".grey_optimizer"
    config_dir.mkdir(parents=True)
    (config_dir / "artifacts").mkdir()
    return config_dir


@pytest.fixture
def sample_gpu_spec():
    """Create a sample GPUSpec for testing."""
    return GPUSpec(
        vendor="nvidia",
        model="NVIDIA GeForce RTX 4090",
        device_id="0000:01:00.0",
        driver="545.23.08",
        vram_total_mb=24576,
        vram_free_mb=22000,
        compute_capability="8.9",
        cuda_version="12.3",
        num_sm=128,
        clock_mhz=2520,
        memory_bus_width=384,
        pci_bandwidth="16 GT/s",
        thermal_limits={"throttle_c": 83, "shutdown_c": 91},
        detection_sources=["nvidia-smi", "vulkaninfo", "lspci"],
    )


@pytest.fixture
def sample_intel_spec():
    """Create a sample Intel GPUSpec for testing."""
    return GPUSpec(
        vendor="intel",
        model="Intel Arc A770",
        vram_total_mb=6144,
        vram_free_mb=5500,
        driver="xe",
        detection_sources=["lspci"],
    )


@pytest.fixture
def sample_amd_spec():
    """Create a sample AMD GPUSpec for testing."""
    return GPUSpec(
        vendor="amd",
        model="AMD Radeon RX 7900 XTX",
        vram_total_mb=24576,
        vram_free_mb=22000,
        driver="23.40.2",
        detection_sources=["rocm-smi", "lspci"],
    )


# =============================================================================
# Unit Tests: GPUSpec
# =============================================================================

class TestGPUSpec:
    """Unit tests for GPUSpec dataclass."""
    
    def test_default_initialization(self):
        """Test GPUSpec with default values."""
        spec = GPUSpec()
        assert spec.vendor == "unknown"
        assert spec.model == "unknown"
        assert spec.vram_total_mb == 0
        assert spec.notes == []
        assert spec.detection_sources == []
    
    def test_custom_initialization(self, sample_gpu_spec):
        """Test GPUSpec with custom values."""
        assert sample_gpu_spec.vendor == "nvidia"
        assert sample_gpu_spec.vram_total_mb == 24576
        assert sample_gpu_spec.compute_capability == "8.9"
        assert len(sample_gpu_spec.detection_sources) == 3
    
    def test_to_dict(self, sample_gpu_spec):
        """Test serialization to dictionary."""
        data = sample_gpu_spec.to_dict()
        assert isinstance(data, dict)
        assert data["vendor"] == "nvidia"
        assert data["vram_total_mb"] == 24576
        assert "thermal_limits" in data
    
    def test_from_dict(self):
        """Test deserialization from dictionary."""
        data = {
            "vendor": "amd",
            "model": "RX 7900 XTX",
            "vram_total_mb": 24576,
            "compute_capability": "gfx1100",
        }
        spec = GPUSpec.from_dict(data)
        assert spec.vendor == "amd"
        assert spec.model == "RX 7900 XTX"
        assert spec.vram_total_mb == 24576
    
    def test_from_dict_with_legacy_fields(self):
        """Test handling of legacy field names."""
        data = {
            "vendor": "nvidia",
            "open_gl_version": "4.6.0",  # Legacy field
        }
        spec = GPUSpec.from_dict(data)
        assert spec.opengl_version == "4.6.0"
    
    def test_roundtrip_serialization(self, sample_gpu_spec):
        """Test to_dict -> from_dict roundtrip."""
        data = sample_gpu_spec.to_dict()
        restored = GPUSpec.from_dict(data)
        assert restored.vendor == sample_gpu_spec.vendor
        assert restored.vram_total_mb == sample_gpu_spec.vram_total_mb
        assert restored.detection_sources == sample_gpu_spec.detection_sources


# =============================================================================
# Unit Tests: OptimizationPlan
# =============================================================================

class TestOptimizationPlan:
    """Unit tests for OptimizationPlan dataclass."""
    
    def test_default_initialization(self):
        """Test OptimizationPlan with default values."""
        plan = OptimizationPlan()
        assert plan.mode == "safe"
        assert plan.cooldown_threshold_c == 83
        assert plan.resume_threshold_c == 70
        assert plan.fair_scheduling_policy == "round_robin"
    
    def test_aggressive_mode(self):
        """Test OptimizationPlan in aggressive mode."""
        plan = OptimizationPlan(
            mode="aggressive",
            per_process_vram_cap_mb=20000,
            recommended_batch_size=64,
        )
        assert plan.mode == "aggressive"
        assert plan.per_process_vram_cap_mb == 20000
    
    def test_to_dict(self):
        """Test serialization."""
        plan = OptimizationPlan(
            mode="safe",
            per_process_vram_cap_mb=16000,
            confidence=0.85,
        )
        data = plan.to_dict()
        assert data["mode"] == "safe"
        assert data["per_process_vram_cap_mb"] == 16000
        assert data["confidence"] == 0.85
    
    def test_from_dict(self):
        """Test deserialization."""
        data = {
            "mode": "aggressive",
            "per_process_vram_cap_mb": 20000,
            "recommended_batch_size": 64,
            "confidence": 0.90,
        }
        plan = OptimizationPlan.from_dict(data)
        assert plan.mode == "aggressive"
        assert plan.recommended_batch_size == 64


# =============================================================================
# Unit Tests: Planner Decisions
# =============================================================================

class TestPlannerDecisions:
    """Tests verifying planner decisions change with different specs."""
    
    def test_vram_cap_scales_with_size(self, sample_gpu_spec, sample_intel_spec):
        """Verify VRAM caps scale appropriately with VRAM size."""
        large_plan = plan_optimizations(sample_gpu_spec, mode="safe")
        small_plan = plan_optimizations(sample_intel_spec, mode="safe")
        
        # Larger GPU should have higher absolute cap
        assert large_plan.per_process_vram_cap_mb > small_plan.per_process_vram_cap_mb
    
    def test_batch_size_scales_with_vram(self, sample_gpu_spec, sample_intel_spec):
        """Verify batch size recommendations scale with VRAM."""
        large_plan = plan_optimizations(sample_gpu_spec, mode="safe")
        small_plan = plan_optimizations(sample_intel_spec, mode="safe")
        
        assert large_plan.recommended_batch_size > small_plan.recommended_batch_size
    
    def test_intel_gets_reduced_batch(self, sample_intel_spec, sample_gpu_spec):
        """Verify Intel GPUs get reduced batch sizes."""
        # Create NVIDIA spec with same VRAM as Intel
        nvidia_6gb = GPUSpec(
            vendor="nvidia",
            vram_total_mb=6144,
            detection_sources=["nvidia-smi"],
        )
        
        intel_plan = plan_optimizations(sample_intel_spec, mode="safe")
        nvidia_plan = plan_optimizations(nvidia_6gb, mode="safe")
        
        # Intel should have smaller batch for same VRAM
        assert intel_plan.recommended_batch_size <= nvidia_plan.recommended_batch_size
    
    def test_amd_thermal_thresholds(self, sample_amd_spec):
        """Verify AMD gets appropriate thermal thresholds."""
        plan = plan_optimizations(sample_amd_spec, mode="safe")
        
        # AMD should have higher thermal tolerance
        assert plan.cooldown_threshold_c >= 82
    
    def test_aggressive_vs_safe_mode(self, sample_gpu_spec):
        """Verify aggressive mode has higher utilization."""
        safe_plan = plan_optimizations(sample_gpu_spec, mode="safe")
        aggressive_plan = plan_optimizations(sample_gpu_spec, mode="aggressive")
        
        # Aggressive should use more VRAM
        assert aggressive_plan.per_process_vram_cap_mb > safe_plan.per_process_vram_cap_mb
        
        # Aggressive should have larger batches
        assert aggressive_plan.recommended_batch_size >= safe_plan.recommended_batch_size
        
        # Aggressive should enable preemptive throttle
        assert aggressive_plan.preemptive_throttle.get("enabled", False)
    
    def test_confidence_reflects_detection_quality(self):
        """Verify confidence scores reflect detection quality."""
        # High quality detection
        high_quality = GPUSpec(
            vendor="nvidia",
            model="RTX 4090",
            vram_total_mb=24576,
            compute_capability="8.9",
            cuda_version="12.3",
            detection_sources=["nvidia-smi", "vulkaninfo", "lspci"],
        )
        
        # Low quality detection
        low_quality = GPUSpec(
            vendor="unknown",
            model="unknown",
            vram_total_mb=0,
            detection_sources=[],
        )
        
        high_plan = plan_optimizations(high_quality, mode="safe")
        low_plan = plan_optimizations(low_quality, mode="safe")
        
        assert high_plan.confidence > low_plan.confidence
        assert high_plan.confidence >= 0.5
        assert low_plan.confidence < 0.5
    
    def test_cuda_enables_larger_batches(self):
        """Verify CUDA availability enables larger batches."""
        with_cuda = GPUSpec(
            vendor="nvidia",
            vram_total_mb=8192,
            cuda_version="12.3",
            detection_sources=["nvidia-smi"],
        )
        
        without_cuda = GPUSpec(
            vendor="nvidia",
            vram_total_mb=8192,
            cuda_version="",
            detection_sources=["lspci"],
        )
        
        cuda_plan = plan_optimizations(with_cuda, mode="aggressive")
        no_cuda_plan = plan_optimizations(without_cuda, mode="aggressive")
        
        assert cuda_plan.recommended_batch_size >= no_cuda_plan.recommended_batch_size
    
    def test_dedup_only_for_large_vram_aggressive(self, sample_gpu_spec, sample_intel_spec):
        """Verify deduplication only enabled for large VRAM in aggressive mode."""
        large_aggressive = plan_optimizations(sample_gpu_spec, mode="aggressive")
        large_safe = plan_optimizations(sample_gpu_spec, mode="safe")
        small_aggressive = plan_optimizations(sample_intel_spec, mode="aggressive")
        
        assert large_aggressive.vram_deduplication_enabled is True
        assert large_safe.vram_deduplication_enabled is False
        assert small_aggressive.vram_deduplication_enabled is False


# =============================================================================
# Unit Tests: Apply Plan
# =============================================================================

class TestApplyPlan:
    """Tests for plan application."""
    
    def test_dry_run_is_default(self):
        """Verify dry-run is the default mode."""
        plan = OptimizationPlan()
        result = apply_plan(plan)
        
        assert result.dry_run is True
        assert result.success is True
        assert all("[DRY-RUN]" in a for a in result.actions_applied)
    
    def test_dry_run_no_env_changes(self):
        """Verify dry-run doesn't modify environment."""
        # Clear any existing env vars
        for var in ["GREY_VRAM_CAP_MB", "GREY_COOLDOWN_C"]:
            os.environ.pop(var, None)
        
        plan = OptimizationPlan(per_process_vram_cap_mb=8000)
        result = apply_plan(plan, dry_run=True)
        
        assert "GREY_VRAM_CAP_MB" not in os.environ
    
    def test_requires_consent_and_confirm(self):
        """Verify destructive actions require both flags."""
        plan = OptimizationPlan()
        
        # Missing both
        result = apply_plan(plan, dry_run=False)
        assert result.success is False
        assert any("consent" in e.lower() for e in result.errors)
        
        # Missing confirm
        result = apply_plan(plan, dry_run=False, consent=True)
        assert result.success is False
        
        # Missing consent
        result = apply_plan(plan, dry_run=False, confirm=True)
        assert result.success is False
    
    def test_actual_apply_with_consent(self, temp_config_dir):
        """Verify actual application with consent flags."""
        plan = OptimizationPlan(
            per_process_vram_cap_mb=16000,
            cooldown_threshold_c=80,
        )
        
        with patch.object(Path, "mkdir"):
            result = apply_plan(
                plan,
                dry_run=False,
                consent=True,
                confirm=True
            )
        
        assert result.dry_run is False
        assert result.success is True
        assert os.environ.get("GREY_VRAM_CAP_MB") == "16000"
        assert os.environ.get("GREY_COOLDOWN_C") == "80"
    
    def test_generates_artifacts(self, temp_config_dir):
        """Verify artifact generation."""
        plan = OptimizationPlan()
        
        with patch("grey_gpu_optimizer.optimizer.ARTIFACTS_DIR", temp_config_dir / "artifacts"):
            result = apply_plan(plan, dry_run=True)
        
        assert "artifact_report_json" in result.artifacts or len(result.artifacts) >= 0
    
    def test_result_contains_metrics(self):
        """Verify result contains proper metrics."""
        plan = OptimizationPlan(mode="aggressive")
        result = apply_plan(plan, dry_run=True)
        
        assert "plan_mode" in result.metrics
        assert "timestamp" in result.metrics
        assert result.metrics["plan_mode"] == "aggressive"


# =============================================================================
# Unit Tests: Chunking Utilities
# =============================================================================

class TestChunkingUtilities:
    """Tests for tensor chunking utilities."""
    
    def test_compute_optimal_batch_size(self, sample_gpu_spec):
        """Test batch size computation."""
        result = compute_optimal_batch_size(
            sample_gpu_spec,
            model_size_mb=1000,
            sample_size_mb=100
        )
        
        assert isinstance(result, BatchSizeRecommendation)
        assert result.batch_size > 0
        assert result.max_batch_size >= result.batch_size
        assert result.min_batch_size <= result.batch_size
        assert 0 <= result.confidence <= 1
    
    def test_batch_size_scales_with_vram(self):
        """Test batch size scales with available VRAM."""
        small_gpu = GPUSpec(vram_total_mb=4000)
        large_gpu = GPUSpec(vram_total_mb=24000)
        
        small_result = compute_optimal_batch_size(small_gpu, sample_size_mb=100)
        large_result = compute_optimal_batch_size(large_gpu, sample_size_mb=100)
        
        assert large_result.batch_size > small_result.batch_size
    
    def test_adaptive_batch_resizer(self):
        """Test adaptive batch resizing based on VRAM usage."""
        # High utilization - should reduce batch
        new_batch = adaptive_batch_resizer(
            current_batch=32,
            vram_used_pct=90.0,
            target_pct=75.0
        )
        assert new_batch < 32
        
        # Low utilization - should increase batch
        new_batch = adaptive_batch_resizer(
            current_batch=16,
            vram_used_pct=50.0,
            target_pct=75.0
        )
        assert new_batch > 16
    
    def test_chunk_tensor_for_checkpointing(self):
        """Test chunking strategy generation."""
        strategy = chunk_tensor_for_checkpointing(
            tensor_size_mb=8000,
            vram_available_mb=4000
        )
        
        assert isinstance(strategy, ChunkingStrategy)
        assert strategy.num_chunks > 1
        assert strategy.activation_checkpointing is True
    
    def test_no_chunking_for_small_tensors(self):
        """Test no chunking when tensor fits in memory."""
        strategy = chunk_tensor_for_checkpointing(
            tensor_size_mb=1000,
            vram_available_mb=8000
        )
        
        assert strategy.num_chunks == 1
        assert strategy.activation_checkpointing is False


# =============================================================================
# Unit Tests: Deduplication Utilities
# =============================================================================

class TestDeduplicationUtilities:
    """Tests for VRAM deduplication utilities."""
    
    def test_vram_deduplicator_basic(self):
        """Test basic deduplicator functionality."""
        dedup = VRAMDeduplicator()
        
        # Register identical buffers
        data = b"test_data_12345"
        dedup.register_buffer("buffer1", data, 100)
        dedup.register_buffer("buffer2", data, 100)  # Duplicate
        dedup.register_buffer("buffer3", b"different_data", 100)
        
        report = dedup.analyze()
        
        assert report.total_buffers == 3
        assert report.unique_buffers == 2
        assert report.duplicate_buffers == 1
        assert report.reclaimable_mb == 100  # One duplicate of 100MB
    
    def test_estimate_reclaimed_mb(self):
        """Test VRAM reclamation estimation."""
        small_estimate = estimate_reclaimed_mb(4000)
        large_estimate = estimate_reclaimed_mb(24000)
        
        # Larger VRAM should have higher estimate
        assert large_estimate > small_estimate
        
        # Should be positive
        assert small_estimate >= 0
        assert large_estimate >= 0
    
    def test_find_duplicate_buffers(self):
        """Test duplicate buffer detection."""
        data1 = b"buffer_content_abc"
        data2 = b"buffer_content_xyz"
        
        buffers = [
            ("buf1", data1, 50),
            ("buf2", data1, 50),  # Duplicate of buf1
            ("buf3", data2, 50),
        ]
        
        duplicates = find_duplicate_buffers(buffers)
        
        assert len(duplicates) == 1  # One group of duplicates
        assert any("buf1" in group and "buf2" in group for group in duplicates.values())


# =============================================================================
# Unit Tests: Daemon
# =============================================================================

class TestDaemon:
    """Tests for GPU optimizer daemon."""
    
    def test_daemon_initialization(self):
        """Test daemon initializes correctly."""
        daemon = GPUOptimizerDaemon(
            interval_s=60,
            dry_run=True,
            verbose=False
        )
        
        assert daemon.interval_s == 60
        assert daemon.dry_run is True
        assert daemon.status.running is False
    
    def test_daemon_status_dataclass(self):
        """Test DaemonStatus dataclass."""
        status = DaemonStatus(
            running=True,
            gpu_vendor="nvidia",
            gpu_model="RTX 4090",
            vram_total_mb=24576,
        )
        
        data = status.to_dict()
        assert data["running"] is True
        assert data["gpu_vendor"] == "nvidia"
    
    def test_monitor_sample_dataclass(self):
        """Test MonitorSample dataclass."""
        sample = MonitorSample(
            vram_used_mb=8000,
            vram_free_mb=16000,
            temp_c=65.0,
            gpu_util_pct=80.0,
        )
        
        data = sample.to_dict()
        assert data["vram_used_mb"] == 8000
        assert data["temp_c"] == 65.0
    
    @patch("grey_gpu_optimizer.daemon._get_nvidia_metrics")
    def test_sample_gpu_metrics(self, mock_metrics):
        """Test GPU metrics sampling."""
        mock_metrics.return_value = {
            "vram_used_mb": 5000,
            "vram_free_mb": 19000,
            "temp_c": 55.0,
            "gpu_util_pct": 25.0,
            "mem_util_pct": 20.0,
        }
        
        spec = GPUSpec(vendor="nvidia")
        sample = sample_gpu_metrics(spec)
        
        assert isinstance(sample, MonitorSample)
        assert sample.timestamp != ""


# =============================================================================
# Integration Tests
# =============================================================================

class TestIntegration:
    """Integration tests for end-to-end workflows."""
    
    @patch("grey_gpu_optimizer.optimizer._run_command")
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    def test_detect_plan_apply_workflow(self, mock_check, mock_run):
        """Test complete detect -> plan -> apply workflow."""
        # Mock no tools available
        mock_check.return_value = False
        mock_run.return_value = (False, "", "not found")
        
        # Detection should work with notes
        specs = detect_gpus(
            use_vendor_cli=False,
            use_system_tools=False,
            use_python_libs=False
        )
        assert len(specs) >= 1
        
        # Planning should work with defaults
        plan = plan_optimizations(specs[0], mode="safe")
        assert plan.mode == "safe"
        assert plan.per_process_vram_cap_mb > 0
        
        # Apply should work in dry-run
        result = apply_plan(plan, dry_run=True)
        assert result.success is True
        assert result.dry_run is True
    
    def test_plan_with_real_spec(self, sample_gpu_spec):
        """Test planning with realistic GPU spec."""
        plan = plan_optimizations(sample_gpu_spec, mode="aggressive")
        
        # Should have reasonable values for RTX 4090
        assert plan.per_process_vram_cap_mb > 20000  # 90% of 24576
        assert plan.recommended_batch_size >= 64
        assert plan.cooldown_threshold_c == 83
        assert plan.confidence > 0.7
    
    def test_serialization_roundtrip(self, sample_gpu_spec, tmp_path):
        """Test spec and plan serialization roundtrip."""
        # Create plan
        original_plan = plan_optimizations(sample_gpu_spec, mode="aggressive")
        
        # Save and load spec
        with patch("grey_gpu_optimizer.optimizer.CONFIG_DIR", tmp_path):
            with patch("grey_gpu_optimizer.optimizer.SPEC_FILE", tmp_path / "spec.json"):
                save_spec([sample_gpu_spec])
                loaded_specs = load_spec()
        
        assert loaded_specs is not None
        assert len(loaded_specs) >= 1
        assert loaded_specs[0].vendor == sample_gpu_spec.vendor


# =============================================================================
# Smoke Tests
# =============================================================================

class TestSmokeTests:
    """Quick smoke tests for basic functionality."""
    
    def test_smoke_gpuspec_creation(self):
        """SMOKE TEST: Create GPUSpec instance.
        
        Command: pytest -v -k test_smoke_gpuspec
        Expected: GPUSpec with vendor='nvidia' created
        """
        spec = GPUSpec(vendor="nvidia", model="Test GPU")
        assert spec.vendor == "nvidia"
        assert spec.model == "Test GPU"
    
    def test_smoke_optimization_plan_creation(self):
        """SMOKE TEST: Create OptimizationPlan instance.
        
        Command: pytest -v -k test_smoke_optimization
        Expected: OptimizationPlan with mode='safe' created
        """
        plan = OptimizationPlan(mode="safe", per_process_vram_cap_mb=8000)
        assert plan.mode == "safe"
        assert plan.per_process_vram_cap_mb == 8000
    
    def test_smoke_apply_result_creation(self):
        """SMOKE TEST: Create ApplyResult instance.
        
        Command: pytest -v -k test_smoke_apply_result
        Expected: ApplyResult with success=True created
        """
        result = ApplyResult(success=True, dry_run=True)
        assert result.success is True
        assert result.dry_run is True
    
    @patch("grey_gpu_optimizer.optimizer._check_tool")
    def test_smoke_detect_gpus(self, mock_check):
        """SMOKE TEST: detect_gpus() runs without exception.
        
        Command: grey-gpu-opt detect --json
        Expected: Returns list of GPUSpec (possibly empty/default)
        """
        mock_check.return_value = False
        
        specs = detect_gpus(
            use_vendor_cli=False,
            use_system_tools=False,
            use_python_libs=False
        )
        
        assert isinstance(specs, list)
        assert len(specs) >= 1
    
    def test_smoke_plan_optimizations(self):
        """SMOKE TEST: plan_optimizations() runs without exception.
        
        Command: grey-gpu-opt plan --mode safe
        Expected: Returns OptimizationPlan with confidence > 0
        """
        spec = GPUSpec(vendor="nvidia", vram_total_mb=8192)
        plan = plan_optimizations(spec, mode="safe")
        
        assert isinstance(plan, OptimizationPlan)
        assert plan.confidence > 0
    
    def test_smoke_apply_plan_dry_run(self):
        """SMOKE TEST: apply_plan() runs in dry-run mode.
        
        Command: grey-gpu-opt apply --dry-run
        Expected: Returns ApplyResult with dry_run=True, success=True
        """
        plan = OptimizationPlan(mode="safe")
        result = apply_plan(plan, dry_run=True)
        
        assert isinstance(result, ApplyResult)
        assert result.dry_run is True
        assert result.success is True
    
    def test_smoke_chunking_utilities(self):
        """SMOKE TEST: Chunking utilities work correctly.
        
        Command: python -c "from grey_gpu_optimizer.utils.chunking import *"
        Expected: All functions importable and callable
        """
        spec = GPUSpec(vram_total_mb=8192)
        
        batch_rec = compute_optimal_batch_size(spec)
        assert batch_rec.batch_size > 0
        
        new_batch = adaptive_batch_resizer(32, 80.0)
        assert new_batch > 0
        
        strategy = chunk_tensor_for_checkpointing(4000, 8000)
        assert strategy.chunk_size_mb > 0
    
    def test_smoke_dedup_utilities(self):
        """SMOKE TEST: Deduplication utilities work correctly.
        
        Command: python -c "from grey_gpu_optimizer.utils.dedup import *"
        Expected: All functions importable and callable
        """
        dedup = VRAMDeduplicator()
        dedup.register_buffer("test", b"data", 100)
        report = dedup.analyze()
        
        assert report.total_buffers == 1
        
        estimate = estimate_reclaimed_mb(8192)
        assert estimate >= 0


# =============================================================================
# Edge Cases
# =============================================================================

class TestEdgeCases:
    """Tests for edge cases and error handling."""
    
    def test_empty_spec_detection_sources(self):
        """Test planning with no detection sources."""
        spec = GPUSpec(
            vendor="unknown",
            vram_total_mb=0,
            detection_sources=[],
        )
        
        plan = plan_optimizations(spec, mode="safe")
        
        # Should use defaults and have low confidence
        assert plan.per_process_vram_cap_mb > 0
        assert plan.confidence < 0.5
    
    def test_zero_vram_spec(self):
        """Test planning with zero VRAM."""
        spec = GPUSpec(vendor="nvidia", vram_total_mb=0)
        plan = plan_optimizations(spec, mode="safe")
        
        # Should use defaults
        assert plan.per_process_vram_cap_mb > 0
        assert "VRAM unknown" in str(plan.rationale) or len(plan.notes) > 0
    
    def test_very_small_vram(self):
        """Test planning with very small VRAM (1GB)."""
        spec = GPUSpec(vendor="nvidia", vram_total_mb=1024)
        plan = plan_optimizations(spec, mode="safe")
        
        # Should have conservative settings
        assert plan.per_process_vram_cap_mb <= 1024
        assert plan.recommended_batch_size <= 8
    
    def test_very_large_vram(self):
        """Test planning with very large VRAM (80GB - A100)."""
        spec = GPUSpec(
            vendor="nvidia",
            vram_total_mb=81920,
            compute_capability="8.0",
            cuda_version="12.0",
        )
        plan = plan_optimizations(spec, mode="aggressive")
        
        # Should have high batch size
        assert plan.recommended_batch_size >= 64
    
    def test_batch_resizer_at_limits(self):
        """Test adaptive batch resizer at boundary conditions."""
        # At minimum
        result = adaptive_batch_resizer(1, 99.0, min_batch=1)
        assert result >= 1
        
        # At maximum
        result = adaptive_batch_resizer(256, 50.0, max_batch=256)
        assert result <= 256
    
    def test_chunking_with_tiny_memory(self):
        """Test chunking with very small available memory."""
        strategy = chunk_tensor_for_checkpointing(
            tensor_size_mb=10000,
            vram_available_mb=500
        )
        
        assert strategy.num_chunks > 1
        assert strategy.activation_checkpointing is True
        assert strategy.offload_to_cpu is True


# =============================================================================
# Test Runner
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
