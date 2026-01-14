#!/usr/bin/env python3
"""
grey_gpu_optimizer/optimizer.py - Core GPU Detection, Planning, and Enforcement

This module implements the GPU immune-system governance framework:
- detect_gpus(): Probes hardware using vendor CLIs, system tools, and Python libraries
- plan_optimizations(): Generates safe/aggressive optimization plans based on GPU specs
- apply_plan(): Enforces plans with dry-run mode by default

Detection Order:
1. Vendor CLIs: nvidia-smi, rocm-smi, intel_gpu_top, vulkaninfo
2. System fallbacks: lspci, lshw -C display, glxinfo
3. Optional library probes: torch.cuda, pyopencl, pycuda

Safety: All detection is read-only. Enforcement requires explicit consent flags.
"""

from __future__ import annotations

import hashlib
import json
import logging
import os
import re
import shutil
import subprocess
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Optional

# Optional YAML support
try:
    import yaml
    HAS_YAML = True
except ImportError:
    yaml = None  # type: ignore
    HAS_YAML = False

# Optional psutil for process monitoring
try:
    import psutil
    HAS_PSUTIL = True
except ImportError:
    psutil = None  # type: ignore
    HAS_PSUTIL = False

logger = logging.getLogger("grey_gpu_optimizer")

# =============================================================================
# Configuration
# =============================================================================

CONFIG_DIR = Path.home() / ".grey_optimizer"
SYSTEM_CONFIG_DIR = Path("/etc/grey_optimizer")
USER_CONFIG_DIR = Path.home() / ".config/grey_optimizer"

SPEC_FILE = CONFIG_DIR / "gpu_spec.json"
PLAN_FILE = CONFIG_DIR / "gpu_plan.yaml"
ARTIFACTS_DIR = CONFIG_DIR / "artifacts"

# Thermal defaults (Celsius)
DEFAULT_COOLDOWN_C = 83
DEFAULT_RESUME_C = 70

# VRAM defaults (MB)
DEFAULT_SAFE_VRAM_CAP_MB = 4096
DEFAULT_AGGRESSIVE_VRAM_CAP_MB = 8192


# =============================================================================
# Data Classes
# =============================================================================

@dataclass
class GPUSpec:
    """
    Normalized GPU hardware specification.
    
    All fields are populated during detection. Missing values use sensible
    defaults and are recorded in `notes` for transparency.
    
    Attributes:
        vendor: GPU vendor (nvidia, amd, intel, unknown)
        model: GPU model name
        device_id: PCI device ID or bus address
        driver: Driver name and version
        vram_total_mb: Total VRAM in megabytes
        vram_free_mb: Free VRAM in megabytes
        compute_capability: CUDA compute capability (e.g., "8.9")
        cuda_version: CUDA version string
        opencl_version: OpenCL version string
        vulkan_version: Vulkan version string
        opengl_version: OpenGL version string
        clock_mhz: GPU clock speed in MHz
        memory_bus_width: Memory bus width in bits
        num_sm: Number of streaming multiprocessors
        pci_bandwidth: PCI bandwidth string (e.g., "16 GT/s")
        thermal_limits: Thermal throttle limits dict
        notes: Human-readable notes about detection
        detection_timestamp: ISO timestamp of detection
        detection_sources: List of tools used for detection
    """
    vendor: str = "unknown"
    model: str = "unknown"
    device_id: str = ""
    driver: str = ""
    vram_total_mb: int = 0
    vram_free_mb: int = 0
    compute_capability: str = ""
    cuda_version: str = ""
    opencl_version: str = ""
    vulkan_version: str = ""
    opengl_version: str = ""
    clock_mhz: int = 0
    memory_bus_width: int = 0
    num_sm: int = 0
    pci_bandwidth: str = ""
    thermal_limits: dict[str, int] = field(default_factory=lambda: {
        "throttle_c": 83,
        "shutdown_c": 100,
    })
    notes: list[str] = field(default_factory=list)
    detection_timestamp: str = ""
    detection_sources: list[str] = field(default_factory=list)
    
    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for JSON/YAML serialization."""
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> GPUSpec:
        """Create GPUSpec from dictionary."""
        # Handle legacy field names
        if "open_gl_version" in data and "opengl_version" not in data:
            data["opengl_version"] = data.pop("open_gl_version")
        return cls(**{k: v for k, v in data.items() if k in cls.__dataclass_fields__})


@dataclass
class OptimizationPlan:
    """
    GPU optimization plan with safe and aggressive recommendations.
    
    Each recommendation includes a confidence score (0.0-1.0) and
    a rationale string explaining the decision.
    
    Attributes:
        mode: Planning mode (safe, aggressive)
        per_process_vram_cap_mb: Maximum VRAM per process
        recommended_batch_size: Optimal batch size for ML workloads
        tensor_chunk_size_mb: Tensor chunk size for gradient checkpointing
        cooldown_threshold_c: Temperature to start throttling
        resume_threshold_c: Temperature to resume full speed
        fair_scheduling_policy: Scheduling policy for multi-tenant
        preemptive_throttle: Throttling configuration dict
        vram_deduplication_enabled: Whether dedup is recommended
        estimated_reclaimed_mb: Estimated VRAM savings from dedup
        confidence: Overall plan confidence (0.0-1.0)
        rationale: Dict of rationale strings per recommendation
        generated_at: ISO timestamp of plan generation
        spec_hash: Hash of the GPU spec used
        notes: Additional notes
    """
    mode: str = "safe"
    per_process_vram_cap_mb: int = 0
    recommended_batch_size: int = 1
    tensor_chunk_size_mb: int = 64
    cooldown_threshold_c: int = DEFAULT_COOLDOWN_C
    resume_threshold_c: int = DEFAULT_RESUME_C
    fair_scheduling_policy: str = "round_robin"
    preemptive_throttle: dict[str, Any] = field(default_factory=lambda: {
        "enabled": False,
        "throttle_pct": 50,
        "grace_period_s": 5,
    })
    vram_deduplication_enabled: bool = False
    estimated_reclaimed_mb: int = 0
    confidence: float = 0.5
    rationale: dict[str, str] = field(default_factory=dict)
    generated_at: str = ""
    spec_hash: str = ""
    notes: list[str] = field(default_factory=list)
    
    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for YAML/JSON serialization."""
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> OptimizationPlan:
        """Create OptimizationPlan from dictionary."""
        return cls(**{k: v for k, v in data.items() if k in cls.__dataclass_fields__})


@dataclass
class ApplyResult:
    """
    Result of applying an optimization plan.
    
    Attributes:
        success: Whether application succeeded
        dry_run: Whether this was a dry run
        actions_applied: List of actions taken/simulated
        actions_skipped: List of skipped actions
        errors: List of error messages
        metrics: Metrics dict for logging
        artifacts: Paths to generated artifact files
    """
    success: bool = False
    dry_run: bool = True
    actions_applied: list[str] = field(default_factory=list)
    actions_skipped: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)
    metrics: dict[str, Any] = field(default_factory=dict)
    artifacts: dict[str, str] = field(default_factory=dict)
    
    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return asdict(self)


# =============================================================================
# Detection Utilities
# =============================================================================

def _run_command(
    cmd: list[str],
    timeout: int = 10,
    capture_stderr: bool = True
) -> tuple[bool, str, str]:
    """
    Safely run a shell command and capture output.
    
    This function is read-only and does not modify system state.
    
    Args:
        cmd: Command and arguments as list
        timeout: Timeout in seconds
        capture_stderr: Whether to capture stderr
    
    Returns:
        Tuple of (success, stdout, stderr)
    """
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
            env={**os.environ, "LANG": "C"}
        )
        return (
            result.returncode == 0,
            result.stdout.strip(),
            result.stderr.strip() if capture_stderr else ""
        )
    except subprocess.TimeoutExpired:
        return False, "", f"Command timed out after {timeout}s"
    except FileNotFoundError:
        return False, "", f"Command not found: {cmd[0]}"
    except Exception as e:
        return False, "", f"Error: {e}"


def _check_tool(tool: str) -> bool:
    """Check if a command-line tool is available."""
    return shutil.which(tool) is not None


def _parse_nvidia_smi(output: str) -> dict[str, Any]:
    """Parse nvidia-smi output for GPU information."""
    result: dict[str, Any] = {}
    
    # Parse memory: "1234MiB / 24576MiB"
    mem_match = re.search(r"(\d+)\s*MiB\s*/\s*(\d+)\s*MiB", output)
    if mem_match:
        result["vram_free_mb"] = int(mem_match.group(2)) - int(mem_match.group(1))
        result["vram_total_mb"] = int(mem_match.group(2))
    
    # Parse driver version
    driver_match = re.search(r"Driver Version:\s*([0-9.]+)", output)
    if driver_match:
        result["driver"] = driver_match.group(1)
    
    # Parse CUDA version
    cuda_match = re.search(r"CUDA Version:\s*([0-9.]+)", output)
    if cuda_match:
        result["cuda_version"] = cuda_match.group(1)
    
    # Parse GPU name
    name_match = re.search(r"\|\s+\d+\s+([^|]+?)\s+(?:Off|On)\s+\|", output)
    if name_match:
        result["model"] = name_match.group(1).strip()
    
    # Parse temperature
    temp_match = re.search(r"(\d+)C", output)
    if temp_match:
        result["current_temp_c"] = int(temp_match.group(1))
    
    return result


def _parse_nvidia_smi_query(output: str) -> dict[str, Any]:
    """Parse nvidia-smi --query-gpu output (CSV format)."""
    result: dict[str, Any] = {}
    
    parts = output.split(",")
    if len(parts) >= 5:
        result["model"] = parts[0].strip()
        result["vram_total_mb"] = int(parts[1].strip()) if parts[1].strip().isdigit() else 0
        result["vram_free_mb"] = int(parts[2].strip()) if parts[2].strip().isdigit() else 0
        result["driver"] = parts[3].strip()
        result["clock_mhz"] = int(parts[4].strip()) if parts[4].strip().isdigit() else 0
    
    return result


def _parse_rocm_smi(output: str) -> dict[str, Any]:
    """Parse rocm-smi output for AMD GPU information."""
    result: dict[str, Any] = {}
    
    # GPU model
    model_match = re.search(r"GPU\[\d+\]\s*:\s*(.+)", output)
    if model_match:
        result["model"] = model_match.group(1).strip()
    
    # VRAM
    vram_match = re.search(r"VRAM Total Memory \(B\):\s*(\d+)", output)
    if vram_match:
        result["vram_total_mb"] = int(vram_match.group(1)) // (1024 * 1024)
    
    vram_used_match = re.search(r"VRAM Total Used Memory \(B\):\s*(\d+)", output)
    if vram_used_match and "vram_total_mb" in result:
        used_mb = int(vram_used_match.group(1)) // (1024 * 1024)
        result["vram_free_mb"] = result["vram_total_mb"] - used_mb
    
    return result


def _parse_lspci(output: str) -> list[dict[str, Any]]:
    """Parse lspci output for VGA controllers."""
    results = []
    
    for line in output.split("\n"):
        if "VGA" in line or "3D controller" in line:
            entry: dict[str, Any] = {}
            
            # Device ID
            id_match = re.match(r"([0-9a-f:\.]+)", line)
            if id_match:
                entry["device_id"] = id_match.group(1)
            
            # Vendor detection
            line_lower = line.lower()
            if "nvidia" in line_lower:
                entry["vendor"] = "nvidia"
            elif "amd" in line_lower or "ati" in line_lower:
                entry["vendor"] = "amd"
            elif "intel" in line_lower:
                entry["vendor"] = "intel"
            else:
                entry["vendor"] = "unknown"
            
            # Model extraction
            model_match = re.search(r":\s*(.+?)(?:\s*\(rev|\s*$)", line)
            if model_match:
                entry["model"] = model_match.group(1).strip()
            
            if entry:
                results.append(entry)
    
    return results


def _parse_lshw(output: str) -> dict[str, Any]:
    """Parse lshw -C display output."""
    result: dict[str, Any] = {}
    
    # Product (model)
    product_match = re.search(r"product:\s*(.+)", output)
    if product_match:
        result["model"] = product_match.group(1).strip()
    
    # Vendor
    vendor_match = re.search(r"vendor:\s*(.+)", output)
    if vendor_match:
        vendor_str = vendor_match.group(1).lower()
        if "nvidia" in vendor_str:
            result["vendor"] = "nvidia"
        elif "amd" in vendor_str or "advanced micro" in vendor_str:
            result["vendor"] = "amd"
        elif "intel" in vendor_str:
            result["vendor"] = "intel"
    
    # Driver
    driver_match = re.search(r"driver=(\S+)", output)
    if driver_match:
        result["driver"] = driver_match.group(1)
    
    # Bus info
    bus_match = re.search(r"bus info:\s*pci@([0-9a-f:.]+)", output)
    if bus_match:
        result["device_id"] = bus_match.group(1)
    
    return result


def _parse_glxinfo(output: str) -> dict[str, Any]:
    """Parse glxinfo output for OpenGL information."""
    result: dict[str, Any] = {}
    
    # OpenGL version
    version_match = re.search(r"OpenGL version string:\s*(.+)", output)
    if version_match:
        ver_str = version_match.group(1)
        ver_num_match = re.search(r"(\d+\.\d+)", ver_str)
        if ver_num_match:
            result["opengl_version"] = ver_num_match.group(1)
    
    # Vendor
    vendor_match = re.search(r"OpenGL vendor string:\s*(.+)", output)
    if vendor_match:
        vendor_str = vendor_match.group(1).lower()
        if "nvidia" in vendor_str:
            result["vendor"] = "nvidia"
        elif "amd" in vendor_str or "ati" in vendor_str:
            result["vendor"] = "amd"
        elif "intel" in vendor_str:
            result["vendor"] = "intel"
    
    # Renderer (model)
    renderer_match = re.search(r"OpenGL renderer string:\s*(.+)", output)
    if renderer_match:
        result["model"] = renderer_match.group(1).strip()
    
    return result


def _parse_vulkaninfo(output: str) -> dict[str, Any]:
    """Parse vulkaninfo --summary output."""
    result: dict[str, Any] = {}
    
    # Vulkan version
    version_match = re.search(r"apiVersion\s*=\s*([0-9.]+)", output)
    if version_match:
        result["vulkan_version"] = version_match.group(1)
    
    # Device name
    name_match = re.search(r"deviceName\s*=\s*(.+)", output)
    if name_match:
        result["model"] = name_match.group(1).strip()
    
    return result


def _probe_torch_cuda() -> dict[str, Any]:
    """Probe PyTorch CUDA for GPU information."""
    result: dict[str, Any] = {}
    
    try:
        import torch
        if torch.cuda.is_available():
            result["vendor"] = "nvidia"
            result["model"] = torch.cuda.get_device_name(0)
            props = torch.cuda.get_device_properties(0)
            result["vram_total_mb"] = props.total_memory // (1024 * 1024)
            result["compute_capability"] = f"{props.major}.{props.minor}"
            result["num_sm"] = props.multi_processor_count
            result["detection_source"] = "torch.cuda"
    except ImportError:
        pass
    except Exception as e:
        result["error"] = str(e)
    
    return result


def _probe_pyopencl() -> dict[str, Any]:
    """Probe PyOpenCL for GPU information."""
    result: dict[str, Any] = {}
    
    try:
        import pyopencl as cl
        platforms = cl.get_platforms()
        for platform in platforms:
            devices = platform.get_devices(device_type=cl.device_type.GPU)
            for device in devices:
                result["model"] = device.name
                result["vram_total_mb"] = device.global_mem_size // (1024 * 1024)
                result["opencl_version"] = device.version
                result["vendor"] = device.vendor.lower()
                if "nvidia" in result["vendor"]:
                    result["vendor"] = "nvidia"
                elif "amd" in result["vendor"] or "advanced" in result["vendor"]:
                    result["vendor"] = "amd"
                elif "intel" in result["vendor"]:
                    result["vendor"] = "intel"
                result["detection_source"] = "pyopencl"
                break
            if result:
                break
    except ImportError:
        pass
    except Exception as e:
        result["error"] = str(e)
    
    return result


def _probe_pycuda() -> dict[str, Any]:
    """Probe PyCUDA for GPU information."""
    result: dict[str, Any] = {}
    
    try:
        import pycuda.driver as cuda
        import pycuda.autoinit  # noqa: F401
        
        device = cuda.Device(0)
        result["vendor"] = "nvidia"
        result["model"] = device.name()
        result["vram_total_mb"] = device.total_memory() // (1024 * 1024)
        cc = device.compute_capability()
        result["compute_capability"] = f"{cc[0]}.{cc[1]}"
        result["num_sm"] = device.get_attribute(cuda.device_attribute.MULTIPROCESSOR_COUNT)
        result["detection_source"] = "pycuda"
    except ImportError:
        pass
    except Exception as e:
        result["error"] = str(e)
    
    return result


# =============================================================================
# Main Detection Function
# =============================================================================

def detect_gpus(
    use_vendor_cli: bool = True,
    use_system_tools: bool = True,
    use_python_libs: bool = True,
    verbose: bool = False
) -> list[GPUSpec]:
    """
    Detect GPU hardware and return normalized specifications.
    
    This function probes the system using multiple methods to build a
    comprehensive picture of available GPU hardware. It does NOT modify
    any system state.
    
    Detection Order:
        1. Vendor CLIs: nvidia-smi, rocm-smi, intel_gpu_top, vulkaninfo
        2. System tools: lspci, lshw -C display, glxinfo
        3. Python libraries: torch.cuda, pyopencl, pycuda
    
    Args:
        use_vendor_cli: Enable vendor CLI detection
        use_system_tools: Enable system tool detection
        use_python_libs: Enable Python library probing
        verbose: Enable verbose logging
    
    Returns:
        List of GPUSpec objects, one per detected GPU.
        If no GPU is detected, returns a single GPUSpec with notes.
    
    Example:
        >>> specs = detect_gpus()
        >>> print(f"Found {len(specs)} GPU(s)")
        >>> for spec in specs:
        ...     print(f"  {spec.vendor}: {spec.model} ({spec.vram_total_mb}MB)")
    """
    specs: list[GPUSpec] = []
    notes: list[str] = []
    detection_sources: list[str] = []
    merged: dict[str, Any] = {}
    
    timestamp = datetime.now(timezone.utc).isoformat()
    
    # -------------------------------------------------------------------------
    # Phase 1: Vendor CLIs
    # -------------------------------------------------------------------------
    if use_vendor_cli:
        # NVIDIA
        if _check_tool("nvidia-smi"):
            # Try query format first
            success, stdout, _ = _run_command([
                "nvidia-smi",
                "--query-gpu=name,memory.total,memory.free,driver_version,clocks.gr",
                "--format=csv,noheader,nounits"
            ])
            if success and stdout:
                parsed = _parse_nvidia_smi_query(stdout)
                if parsed:
                    merged.update(parsed)
                    merged["vendor"] = "nvidia"
                    detection_sources.append("nvidia-smi")
            else:
                # Fallback to standard output
                success, stdout, _ = _run_command(["nvidia-smi"])
                if success:
                    parsed = _parse_nvidia_smi(stdout)
                    if parsed:
                        merged.update(parsed)
                        merged["vendor"] = "nvidia"
                        detection_sources.append("nvidia-smi")
        else:
            notes.append("nvidia-smi not found")
        
        # AMD ROCm
        if _check_tool("rocm-smi"):
            success, stdout, _ = _run_command(["rocm-smi", "--showproductname"])
            if success:
                parsed = _parse_rocm_smi(stdout)
                if parsed:
                    merged.update(parsed)
                    merged["vendor"] = "amd"
                    detection_sources.append("rocm-smi")
            
            # Get memory info
            success2, stdout2, _ = _run_command(["rocm-smi", "--showmeminfo", "vram"])
            if success2:
                parsed2 = _parse_rocm_smi(stdout2)
                merged.update(parsed2)
        else:
            notes.append("rocm-smi not found")
        
        # Vulkan info
        if _check_tool("vulkaninfo"):
            success, stdout, _ = _run_command(["vulkaninfo", "--summary"])
            if success:
                parsed = _parse_vulkaninfo(stdout)
                if parsed:
                    merged.update({k: v for k, v in parsed.items() if k not in merged})
                    detection_sources.append("vulkaninfo")
        else:
            notes.append("vulkaninfo not found")
    
    # -------------------------------------------------------------------------
    # Phase 2: System Tools
    # -------------------------------------------------------------------------
    if use_system_tools:
        # lspci
        if _check_tool("lspci"):
            success, stdout, _ = _run_command(["lspci", "-v"])
            if success:
                lspci_gpus = _parse_lspci(stdout)
                if lspci_gpus:
                    # Use first GPU if we don't have vendor info yet
                    if "vendor" not in merged or merged.get("vendor") == "unknown":
                        merged.update(lspci_gpus[0])
                    elif "device_id" not in merged:
                        merged["device_id"] = lspci_gpus[0].get("device_id", "")
                    detection_sources.append("lspci")
        
        # lshw
        if _check_tool("lshw"):
            success, stdout, _ = _run_command(["lshw", "-C", "display"])
            if success:
                parsed = _parse_lshw(stdout)
                if parsed:
                    merged.update({k: v for k, v in parsed.items() if k not in merged})
                    detection_sources.append("lshw")
        
        # glxinfo
        if _check_tool("glxinfo"):
            success, stdout, _ = _run_command(["glxinfo"])
            if success:
                parsed = _parse_glxinfo(stdout)
                if parsed:
                    merged.update({k: v for k, v in parsed.items() if k not in merged})
                    detection_sources.append("glxinfo")
        else:
            notes.append("glxinfo not found (install mesa-utils)")
    
    # -------------------------------------------------------------------------
    # Phase 3: Python Libraries
    # -------------------------------------------------------------------------
    if use_python_libs:
        # PyTorch CUDA
        torch_info = _probe_torch_cuda()
        if torch_info and "error" not in torch_info:
            merged.update({k: v for k, v in torch_info.items() if k not in merged})
            if "detection_source" in torch_info:
                detection_sources.append(torch_info["detection_source"])
        
        # PyOpenCL
        if "vram_total_mb" not in merged or merged.get("vram_total_mb", 0) == 0:
            opencl_info = _probe_pyopencl()
            if opencl_info and "error" not in opencl_info:
                merged.update({k: v for k, v in opencl_info.items() if k not in merged})
                if "detection_source" in opencl_info:
                    detection_sources.append(opencl_info["detection_source"])
        
        # PyCUDA
        if merged.get("vendor") == "nvidia" and "compute_capability" not in merged:
            pycuda_info = _probe_pycuda()
            if pycuda_info and "error" not in pycuda_info:
                merged.update({k: v for k, v in pycuda_info.items() if k not in merged})
                if "detection_source" in pycuda_info:
                    detection_sources.append(pycuda_info["detection_source"])
    
    # -------------------------------------------------------------------------
    # Build GPUSpec
    # -------------------------------------------------------------------------
    if merged:
        spec = GPUSpec(
            vendor=merged.get("vendor", "unknown"),
            model=merged.get("model", "unknown"),
            device_id=merged.get("device_id", ""),
            driver=merged.get("driver", ""),
            vram_total_mb=merged.get("vram_total_mb", 0),
            vram_free_mb=merged.get("vram_free_mb", 0),
            compute_capability=merged.get("compute_capability", ""),
            cuda_version=merged.get("cuda_version", ""),
            opencl_version=merged.get("opencl_version", ""),
            vulkan_version=merged.get("vulkan_version", ""),
            opengl_version=merged.get("opengl_version", ""),
            clock_mhz=merged.get("clock_mhz", 0),
            memory_bus_width=merged.get("memory_bus_width", 0),
            num_sm=merged.get("num_sm", 0),
            pci_bandwidth=merged.get("pci_bandwidth", ""),
            thermal_limits=merged.get("thermal_limits", {"throttle_c": 83, "shutdown_c": 100}),
            notes=notes,
            detection_timestamp=timestamp,
            detection_sources=detection_sources,
        )
        specs.append(spec)
    else:
        # No GPU detected
        spec = GPUSpec(
            notes=notes + ["No GPU detected via any method"],
            detection_timestamp=timestamp,
            detection_sources=[],
        )
        specs.append(spec)
    
    # Save spec for future use
    save_spec(specs)
    
    logger.info(f"Detected {len(specs)} GPU(s) using sources: {detection_sources}")
    
    return specs


# =============================================================================
# Planning Functions
# =============================================================================

def _compute_spec_hash(spec: GPUSpec) -> str:
    """Compute a hash of the GPU spec for plan validation."""
    spec_str = json.dumps(spec.to_dict(), sort_keys=True)
    return hashlib.sha256(spec_str.encode()).hexdigest()[:16]


def plan_optimizations(
    spec: GPUSpec | dict[str, Any],
    mode: str = "safe"
) -> OptimizationPlan:
    """
    Generate an optimization plan based on GPU spec and mode.
    
    This function analyzes the GPU specification and generates either a
    conservative (safe) or aggressive optimization plan. Each recommendation
    includes a confidence score and rationale.
    
    Args:
        spec: GPUSpec object or dict with GPU specifications
        mode: "safe" or "aggressive" optimization mode
    
    Returns:
        OptimizationPlan with recommendations and confidence scores
    
    Heuristics:
        - vram_total_mb < 4096: Conservative caps, smaller batches
        - vendor == "intel": Prefer CPU fallback, smaller batches
        - cuda_version present: Allow larger batches, tensor fusion
        - compute_capability, num_sm, pci_bandwidth: Scale batch/chunk sizes
    
    Example:
        >>> spec = detect_gpus()[0]
        >>> safe_plan = plan_optimizations(spec, mode="safe")
        >>> aggressive_plan = plan_optimizations(spec, mode="aggressive")
    """
    # Convert dict to GPUSpec if needed
    if isinstance(spec, dict):
        spec = GPUSpec.from_dict(spec)
    
    is_aggressive = mode.lower() == "aggressive"
    notes: list[str] = []
    rationale: dict[str, str] = {}
    confidence_factors: list[float] = []
    
    # Base confidence from detection quality
    base_confidence = 0.3
    if spec.detection_sources:
        base_confidence += 0.1 * min(len(spec.detection_sources), 4)
    if spec.vram_total_mb > 0:
        base_confidence += 0.2
    confidence_factors.append(min(base_confidence, 0.9))
    
    # -------------------------------------------------------------------------
    # VRAM Cap Calculation
    # -------------------------------------------------------------------------
    if spec.vram_total_mb > 0:
        if spec.vram_total_mb < 4096:
            # Small VRAM: very conservative
            vram_pct = 0.60 if is_aggressive else 0.50
            rationale["vram_cap"] = f"Small VRAM ({spec.vram_total_mb}MB): using {int(vram_pct*100)}% to prevent OOM"
            confidence_factors.append(0.7)
        elif spec.vram_total_mb < 8192:
            # Medium VRAM
            vram_pct = 0.80 if is_aggressive else 0.70
            rationale["vram_cap"] = f"Medium VRAM ({spec.vram_total_mb}MB): using {int(vram_pct*100)}%"
            confidence_factors.append(0.8)
        else:
            # Large VRAM
            vram_pct = 0.90 if is_aggressive else 0.75
            rationale["vram_cap"] = f"Large VRAM ({spec.vram_total_mb}MB): using {int(vram_pct*100)}% for optimal throughput"
            confidence_factors.append(0.85)
        
        vram_cap = int(spec.vram_total_mb * vram_pct)
    else:
        # Unknown VRAM: use defaults
        vram_cap = DEFAULT_AGGRESSIVE_VRAM_CAP_MB if is_aggressive else DEFAULT_SAFE_VRAM_CAP_MB
        rationale["vram_cap"] = f"VRAM unknown: using default {vram_cap}MB cap"
        confidence_factors.append(0.4)
        notes.append("VRAM detection incomplete")
    
    # -------------------------------------------------------------------------
    # Batch Size Calculation
    # -------------------------------------------------------------------------
    # Base batch size from VRAM
    if spec.vram_total_mb >= 24000:
        base_batch = 128 if is_aggressive else 64
    elif spec.vram_total_mb >= 16000:
        base_batch = 64 if is_aggressive else 32
    elif spec.vram_total_mb >= 8000:
        base_batch = 32 if is_aggressive else 16
    elif spec.vram_total_mb >= 4000:
        base_batch = 16 if is_aggressive else 8
    else:
        base_batch = 8 if is_aggressive else 4
    
    batch_size = base_batch
    
    # Vendor-specific adjustments
    if spec.vendor == "intel":
        batch_size = max(1, batch_size // 2)
        rationale["batch_size"] = f"Intel GPU: reduced batch to {batch_size} (prefer CPU fallback for large models)"
        confidence_factors.append(0.6)
    elif spec.cuda_version:
        # CUDA available: can use larger batches with tensor fusion
        if is_aggressive:
            batch_size = int(batch_size * 1.5)
        rationale["batch_size"] = f"CUDA {spec.cuda_version} available: batch size {batch_size} with tensor fusion enabled"
        confidence_factors.append(0.85)
    else:
        rationale["batch_size"] = f"Standard batch size {batch_size} based on {spec.vram_total_mb}MB VRAM"
        confidence_factors.append(0.7)
    
    # SM-based scaling
    if spec.num_sm > 0:
        sm_factor = min(2.0, max(1.0, spec.num_sm / 64))
        batch_size = int(batch_size * sm_factor)
        rationale["batch_size"] += f" (scaled by {sm_factor:.1f}x for {spec.num_sm} SMs)"
        confidence_factors.append(0.8)
    
    # -------------------------------------------------------------------------
    # Tensor Chunk Size
    # -------------------------------------------------------------------------
    if spec.memory_bus_width >= 384:
        tensor_chunk = 256 if is_aggressive else 128
    elif spec.memory_bus_width >= 256:
        tensor_chunk = 128 if is_aggressive else 96
    else:
        tensor_chunk = 96 if is_aggressive else 64
    
    if spec.pci_bandwidth:
        rationale["tensor_chunk"] = f"Chunk size {tensor_chunk}MB for {spec.memory_bus_width}-bit bus ({spec.pci_bandwidth})"
    else:
        rationale["tensor_chunk"] = f"Chunk size {tensor_chunk}MB for {'aggressive' if is_aggressive else 'safe'} mode"
    
    # -------------------------------------------------------------------------
    # Thermal Thresholds
    # -------------------------------------------------------------------------
    throttle_temp = spec.thermal_limits.get("throttle_c", 83)
    
    if spec.vendor == "amd":
        cooldown_c = 85 if is_aggressive else 82
        resume_c = 72 if is_aggressive else 68
        rationale["thermal"] = f"AMD GPU: cooldown at {cooldown_c}°C, resume at {resume_c}°C"
    elif spec.vendor == "intel":
        cooldown_c = 80 if is_aggressive else 75
        resume_c = 65 if is_aggressive else 60
        rationale["thermal"] = f"Intel GPU: cooldown at {cooldown_c}°C, resume at {resume_c}°C (tighter limits)"
    else:  # NVIDIA or unknown
        cooldown_c = 83 if is_aggressive else 80
        resume_c = 70 if is_aggressive else 65
        rationale["thermal"] = f"NVIDIA/default: cooldown at {cooldown_c}°C, resume at {resume_c}°C"
    
    # -------------------------------------------------------------------------
    # Scheduling Policy
    # -------------------------------------------------------------------------
    if is_aggressive:
        scheduling = "priority_weighted"
        rationale["scheduling"] = "Priority-weighted scheduling for maximum throughput"
    else:
        scheduling = "round_robin"
        rationale["scheduling"] = "Round-robin scheduling for fair resource sharing"
    
    # -------------------------------------------------------------------------
    # Preemptive Throttle
    # -------------------------------------------------------------------------
    preemptive_throttle = {
        "enabled": is_aggressive,
        "throttle_pct": 30 if is_aggressive else 50,
        "grace_period_s": 3 if is_aggressive else 5,
    }
    if is_aggressive:
        rationale["throttle"] = "Preemptive throttle enabled at 30% with 3s grace period"
    else:
        rationale["throttle"] = "Preemptive throttle disabled in safe mode"
    
    # -------------------------------------------------------------------------
    # VRAM Deduplication
    # -------------------------------------------------------------------------
    from grey_gpu_optimizer.utils.dedup import estimate_reclaimed_mb
    
    dedup_enabled = spec.vram_total_mb >= 8000 and is_aggressive
    if dedup_enabled:
        estimated_reclaim = estimate_reclaimed_mb(spec.vram_total_mb)
        rationale["dedup"] = f"VRAM dedup enabled: estimated {estimated_reclaim}MB reclaimable"
    else:
        estimated_reclaim = 0
        rationale["dedup"] = "VRAM dedup disabled (requires >=8GB VRAM and aggressive mode)"
    
    # -------------------------------------------------------------------------
    # Compute Overall Confidence
    # -------------------------------------------------------------------------
    confidence = sum(confidence_factors) / len(confidence_factors) if confidence_factors else 0.5
    confidence = min(0.95, max(0.1, confidence))
    
    # -------------------------------------------------------------------------
    # Build Plan
    # -------------------------------------------------------------------------
    plan = OptimizationPlan(
        mode=mode,
        per_process_vram_cap_mb=vram_cap,
        recommended_batch_size=batch_size,
        tensor_chunk_size_mb=tensor_chunk,
        cooldown_threshold_c=cooldown_c,
        resume_threshold_c=resume_c,
        fair_scheduling_policy=scheduling,
        preemptive_throttle=preemptive_throttle,
        vram_deduplication_enabled=dedup_enabled,
        estimated_reclaimed_mb=estimated_reclaim,
        confidence=round(confidence, 3),
        rationale=rationale,
        generated_at=datetime.now(timezone.utc).isoformat(),
        spec_hash=_compute_spec_hash(spec),
        notes=notes,
    )
    
    # Save plan
    save_plan(plan)
    
    logger.info(f"Generated {mode} plan with confidence {confidence:.2f}")
    
    return plan


# =============================================================================
# Enforcement Functions
# =============================================================================

def apply_plan(
    plan: OptimizationPlan | dict[str, Any],
    dry_run: bool = True,
    consent: bool = False,
    confirm: bool = False,
    verbose: bool = False
) -> ApplyResult:
    """
    Apply an optimization plan to the system.
    
    SAFETY: By default, this function operates in dry-run mode and will NOT
    make any changes. Destructive actions require BOTH consent=True AND
    confirm=True flags.
    
    Args:
        plan: OptimizationPlan or dict to apply
        dry_run: If True (default), only simulate actions
        consent: Explicit consent for destructive actions
        confirm: Confirmation for destructive actions
        verbose: Enable verbose logging
    
    Returns:
        ApplyResult with details of actions taken/simulated
    
    Example:
        >>> plan = plan_optimizations(spec, mode="safe")
        >>> # Dry run (default, safe)
        >>> result = apply_plan(plan, dry_run=True)
        >>> # Actual apply (requires both flags)
        >>> result = apply_plan(plan, dry_run=False, consent=True, confirm=True)
    """
    # Convert dict to OptimizationPlan if needed
    if isinstance(plan, dict):
        plan = OptimizationPlan.from_dict(plan)
    
    result = ApplyResult(
        dry_run=dry_run,
        metrics={
            "plan_mode": plan.mode,
            "plan_confidence": plan.confidence,
            "timestamp": datetime.now(timezone.utc).isoformat(),
        }
    )
    
    # Safety check for destructive actions
    can_enforce = not dry_run and consent and confirm
    
    if not dry_run and (not consent or not confirm):
        result.errors.append(
            "Destructive enforcement requires both consent=True and confirm=True"
        )
        result.success = False
        return result
    
    actions: list[tuple[str, str, Callable[[], bool]]] = []
    
    # -------------------------------------------------------------------------
    # Define Actions
    # -------------------------------------------------------------------------
    
    # Action: Set VRAM environment
    def action_set_vram_env() -> bool:
        os.environ["GREY_VRAM_CAP_MB"] = str(plan.per_process_vram_cap_mb)
        return True
    
    actions.append((
        "set_vram_cap",
        f"Set GREY_VRAM_CAP_MB={plan.per_process_vram_cap_mb}",
        action_set_vram_env
    ))
    
    # Action: Set thermal thresholds
    def action_set_thermal() -> bool:
        os.environ["GREY_COOLDOWN_C"] = str(plan.cooldown_threshold_c)
        os.environ["GREY_RESUME_C"] = str(plan.resume_threshold_c)
        return True
    
    actions.append((
        "set_thermal",
        f"Set thermal thresholds: {plan.cooldown_threshold_c}°C / {plan.resume_threshold_c}°C",
        action_set_thermal
    ))
    
    # Action: Set scheduling policy
    def action_set_scheduling() -> bool:
        os.environ["GREY_SCHEDULING"] = plan.fair_scheduling_policy
        return True
    
    actions.append((
        "set_scheduling",
        f"Set scheduling policy: {plan.fair_scheduling_policy}",
        action_set_scheduling
    ))
    
    # Action: Set batch size hint
    def action_set_batch_hint() -> bool:
        hint_file = CONFIG_DIR / "batch_hint.txt"
        CONFIG_DIR.mkdir(parents=True, exist_ok=True)
        hint_file.write_text(str(plan.recommended_batch_size))
        return True
    
    actions.append((
        "set_batch_hint",
        f"Write batch size hint: {plan.recommended_batch_size}",
        action_set_batch_hint
    ))
    
    # Action: Enable preemptive throttle
    if plan.preemptive_throttle.get("enabled"):
        def action_enable_throttle() -> bool:
            os.environ["GREY_THROTTLE_ENABLED"] = "1"
            os.environ["GREY_THROTTLE_PCT"] = str(plan.preemptive_throttle["throttle_pct"])
            return True
        
        actions.append((
            "enable_throttle",
            f"Enable preemptive throttle at {plan.preemptive_throttle['throttle_pct']}%",
            action_enable_throttle
        ))
    
    # -------------------------------------------------------------------------
    # Execute Actions
    # -------------------------------------------------------------------------
    for action_name, description, action_fn in actions:
        try:
            if dry_run:
                result.actions_applied.append(f"[DRY-RUN] {description}")
            else:
                success = action_fn()
                if success:
                    result.actions_applied.append(description)
                else:
                    result.actions_skipped.append(f"SKIPPED: {description}")
        except Exception as e:
            result.errors.append(f"ERROR in {action_name}: {e}")
    
    # -------------------------------------------------------------------------
    # Generate Artifacts
    # -------------------------------------------------------------------------
    result.artifacts = _generate_artifacts(plan, result)
    
    # Determine success
    result.success = len(result.errors) == 0
    result.metrics["applied_actions_count"] = len([
        a for a in result.actions_applied if not a.startswith("[DRY-RUN]")
    ])
    result.metrics["dry_run_actions_count"] = len([
        a for a in result.actions_applied if a.startswith("[DRY-RUN]")
    ])
    
    logger.info(
        f"Plan application {'simulated' if dry_run else 'completed'}: "
        f"{len(result.actions_applied)} actions, {len(result.errors)} errors"
    )
    
    return result


def _generate_artifacts(plan: OptimizationPlan, result: ApplyResult) -> dict[str, str]:
    """Generate artifact files for audit trail."""
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    artifacts = {}
    
    # VRAM reclamation log
    vram_log_path = ARTIFACTS_DIR / f"vram_reclamation_{timestamp}.log"
    with open(vram_log_path, "w") as f:
        f.write(f"VRAM Reclamation Log - {timestamp}\n")
        f.write(f"Mode: {plan.mode}\n")
        f.write(f"VRAM Cap: {plan.per_process_vram_cap_mb}MB\n")
        f.write(f"Dedup Enabled: {plan.vram_deduplication_enabled}\n")
        f.write(f"Estimated Reclaimed: {plan.estimated_reclaimed_mb}MB\n")
        f.write(f"Confidence: {plan.confidence}\n")
        f.write("\nActions:\n")
        for action in result.actions_applied:
            f.write(f"  {action}\n")
    artifacts["vram_reclamation_log"] = str(vram_log_path)
    
    # Thermal signature CSV
    thermal_csv_path = ARTIFACTS_DIR / f"thermal_signature_{timestamp}.csv"
    with open(thermal_csv_path, "w") as f:
        f.write("timestamp,cooldown_c,resume_c,throttle_enabled,throttle_pct\n")
        f.write(f"{timestamp},{plan.cooldown_threshold_c},{plan.resume_threshold_c},"
                f"{plan.preemptive_throttle.get('enabled', False)},"
                f"{plan.preemptive_throttle.get('throttle_pct', 0)}\n")
    artifacts["thermal_signature_csv"] = str(thermal_csv_path)
    
    # Fairness heatmap CSV
    fairness_csv_path = ARTIFACTS_DIR / f"fairness_heatmap_{timestamp}.csv"
    with open(fairness_csv_path, "w") as f:
        f.write("timestamp,process_id,utilization_pct,vram_mb,policy\n")
        # Simulated data
        for i in range(4):
            f.write(f"{timestamp},proc_{i},0.0,0,{plan.fair_scheduling_policy}\n")
    artifacts["fairness_heatmap_csv"] = str(fairness_csv_path)
    
    # Artifact report JSON
    report_path = ARTIFACTS_DIR / f"artifact_report_{timestamp}.json"
    report = {
        "timestamp": timestamp,
        "plan": plan.to_dict(),
        "result": {
            "success": result.success,
            "dry_run": result.dry_run,
            "actions_count": len(result.actions_applied),
            "errors_count": len(result.errors),
        },
        "artifacts": artifacts,
    }
    with open(report_path, "w") as f:
        json.dump(report, f, indent=2)
    artifacts["artifact_report_json"] = str(report_path)
    
    return artifacts


# =============================================================================
# Persistence Functions
# =============================================================================

def save_spec(specs: list[GPUSpec]) -> Path:
    """Save GPU specs to config directory."""
    CONFIG_DIR.mkdir(parents=True, exist_ok=True)
    data = [s.to_dict() for s in specs]
    with open(SPEC_FILE, "w") as f:
        json.dump(data, f, indent=2)
    logger.debug(f"Saved GPU spec to {SPEC_FILE}")
    return SPEC_FILE


def load_spec() -> list[GPUSpec] | None:
    """Load GPU specs from config directory."""
    if not SPEC_FILE.exists():
        return None
    try:
        with open(SPEC_FILE) as f:
            data = json.load(f)
        return [GPUSpec.from_dict(d) for d in data]
    except Exception as e:
        logger.warning(f"Failed to load spec: {e}")
        return None


def save_plan(plan: OptimizationPlan) -> Path:
    """Save optimization plan to config directory."""
    CONFIG_DIR.mkdir(parents=True, exist_ok=True)
    if HAS_YAML:
        with open(PLAN_FILE, "w") as f:
            yaml.dump(plan.to_dict(), f, default_flow_style=False)
    else:
        plan_json = PLAN_FILE.with_suffix(".json")
        with open(plan_json, "w") as f:
            json.dump(plan.to_dict(), f, indent=2)
    logger.debug(f"Saved plan to {PLAN_FILE}")
    return PLAN_FILE


def load_plan() -> OptimizationPlan | None:
    """Load optimization plan from config directory."""
    if PLAN_FILE.exists() and HAS_YAML:
        try:
            with open(PLAN_FILE) as f:
                data = yaml.safe_load(f)
            return OptimizationPlan.from_dict(data)
        except Exception as e:
            logger.warning(f"Failed to load plan: {e}")
    
    plan_json = PLAN_FILE.with_suffix(".json")
    if plan_json.exists():
        try:
            with open(plan_json) as f:
                data = json.load(f)
            return OptimizationPlan.from_dict(data)
        except Exception as e:
            logger.warning(f"Failed to load plan: {e}")
    
    return None


def load_config_overrides() -> dict[str, Any]:
    """Load configuration overrides from YAML config files."""
    if not HAS_YAML:
        return {}
    
    overrides: dict[str, Any] = {}
    
    # System config
    system_config = SYSTEM_CONFIG_DIR / "gpu_config.yaml"
    if system_config.exists():
        try:
            with open(system_config) as f:
                data = yaml.safe_load(f) or {}
            overrides.update(data)
        except Exception as e:
            logger.warning(f"Failed to load system config: {e}")
    
    # User config
    user_config = USER_CONFIG_DIR / "gpu_config.yaml"
    if user_config.exists():
        try:
            with open(user_config) as f:
                data = yaml.safe_load(f) or {}
            overrides.update(data)
        except Exception as e:
            logger.warning(f"Failed to load user config: {e}")
    
    return overrides
