#!/usr/bin/env python3
"""
gpu_optimizer.py - GPU Hardware Detection, Planning, and Optimization Module

Detects GPU hardware and driver capabilities, produces a normalized JSON spec,
computes safe and aggressive optimization plans, and exposes a non-destructive
enforcement API with dry-run mode.

Detection order:
1. Vendor CLIs (nvidia-smi, rocm-smi, intel_gpu_top, vulkaninfo)
2. System tools (lspci, lshw -C display, glxinfo)
3. Optional library probes (torch.cuda, pyopencl, pycuda)

Safety: No forced kills by default. Throttling and scheduling recommendations only.
Destructive actions require explicit consent flag.

Usage:
    python gpu_optimizer.py detect --json
    python gpu_optimizer.py plan --output plan.yaml --mode safe
    python gpu_optimizer.py plan --mode aggressive

API:
    from gpu_optimizer import detect_gpus, plan_optimizations, apply_plan
    specs = detect_gpus()
    plan = plan_optimizations(specs, mode="safe")
    result = apply_plan(plan, dry_run=True)
"""

from __future__ import annotations

import argparse
import hashlib
import json
import logging
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from enum import Enum
from pathlib import Path
from typing import Any, Callable, Optional, TypedDict

# Optional YAML support
try:
    import yaml
    HAS_YAML = True
except ImportError:
    yaml = None  # type: ignore
    HAS_YAML = False

# -----------------------------------------------------------------------------
# Configuration and Constants
# -----------------------------------------------------------------------------

CONFIG_DIR = Path.home() / ".grey_optimizer"
LAST_SPEC_FILE = CONFIG_DIR / "last_gpu_spec.json"
LAST_PLAN_FILE = CONFIG_DIR / "last_gpu_plan.yaml"
CONFIG_FILE = CONFIG_DIR / "gpu_config.yaml"
LOG_FILE = CONFIG_DIR / "gpu_optimizer.log"

# Confidence thresholds for planning
CONFIDENCE_HIGH = 0.85
CONFIDENCE_MEDIUM = 0.60
CONFIDENCE_LOW = 0.35

# Default temperature thresholds (Celsius)
DEFAULT_COOLDOWN_THRESHOLD_C = 83
DEFAULT_RESUME_THRESHOLD_C = 70

# Default VRAM caps (MB)
DEFAULT_SAFE_VRAM_CAP_MB = 4096
DEFAULT_AGGRESSIVE_VRAM_CAP_MB = 8192


# -----------------------------------------------------------------------------
# Logging Setup
# -----------------------------------------------------------------------------

def setup_logging(verbose: bool = False) -> logging.Logger:
    """Configure structured JSON logging."""
    CONFIG_DIR.mkdir(parents=True, exist_ok=True)
    
    logger = logging.getLogger("gpu_optimizer")
    logger.setLevel(logging.DEBUG if verbose else logging.INFO)
    
    # File handler with JSON format
    file_handler = logging.FileHandler(LOG_FILE)
    file_handler.setLevel(logging.DEBUG)
    
    class JSONFormatter(logging.Formatter):
        def format(self, record: logging.LogRecord) -> str:
            log_entry = {
                "timestamp": datetime.now(timezone.utc).isoformat(),
                "level": record.levelname,
                "module": record.module,
                "function": record.funcName,
                "message": record.getMessage(),
            }
            if hasattr(record, "metrics"):
                log_entry["metrics"] = record.metrics
            return json.dumps(log_entry)
    
    file_handler.setFormatter(JSONFormatter())
    logger.addHandler(file_handler)
    
    # Console handler
    if verbose:
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.DEBUG)
        console_handler.setFormatter(
            logging.Formatter("%(levelname)s: %(message)s")
        )
        logger.addHandler(console_handler)
    
    return logger


logger = setup_logging()


# -----------------------------------------------------------------------------
# Type Definitions
# -----------------------------------------------------------------------------

class GPUVendor(Enum):
    """Supported GPU vendors."""
    NVIDIA = "nvidia"
    AMD = "amd"
    INTEL = "intel"
    UNKNOWN = "unknown"


class PlanMode(Enum):
    """Optimization plan modes."""
    SAFE = "safe"
    AGGRESSIVE = "aggressive"


class SchedulingPolicy(Enum):
    """Fair scheduling policies for GPU workloads."""
    ROUND_ROBIN = "round_robin"
    PRIORITY_BASED = "priority_based"
    VRAM_WEIGHTED = "vram_weighted"
    FIFO = "fifo"


@dataclass
class GPUSpec:
    """
    Normalized GPU specification.
    
    All fields are optional to handle partial detection scenarios.
    The 'notes' field accumulates warnings and detection metadata.
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
    notes: list[str] = field(default_factory=list)
    
    # Detection metadata
    detection_timestamp: str = ""
    detection_sources: list[str] = field(default_factory=list)
    
    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> GPUSpec:
        """Create from dictionary."""
        # Handle field name mapping for backwards compatibility
        if "open_gl_version" in data and "opengl_version" not in data:
            data["opengl_version"] = data.pop("open_gl_version")
        return cls(**{k: v for k, v in data.items() if k in cls.__dataclass_fields__})


@dataclass
class OptimizationPlan:
    """
    GPU optimization plan with safe and aggressive recommendations.
    
    All values include confidence scores to indicate reliability.
    Includes VRAM deduplication estimates, fairness scheduling, and rationale strings.
    """
    mode: str = "safe"
    per_process_vram_cap_mb: int = 0
    recommended_batch_size: int = 1
    tensor_chunk_size_mb: int = 64
    cooldown_threshold_c: int = DEFAULT_COOLDOWN_THRESHOLD_C
    resume_threshold_c: int = DEFAULT_RESUME_THRESHOLD_C
    fair_scheduling_policy: str = SchedulingPolicy.ROUND_ROBIN.value
    preemptive_throttle: bool = False
    
    # VRAM deduplication and compression estimates
    estimated_reclaimed_mb: int = 0
    dedup_buffer_count: int = 0
    compression_ratio: float = 1.0
    
    # Multi-tenant fairness scheduling
    fairness_weight_per_process: float = 1.0
    max_concurrent_kernels: int = 4
    utilization_heatmap_enabled: bool = False
    
    # Confidence scores (0.0 - 1.0)
    confidence_vram_cap: float = 0.0
    confidence_batch_size: float = 0.0
    confidence_thermal: float = 0.0
    confidence_dedup: float = 0.0
    confidence_overall: float = 0.0
    
    # Rationale strings explaining each decision
    rationale: dict[str, str] = field(default_factory=dict)
    
    # Plan metadata
    generated_at: str = ""
    gpu_specs_hash: str = ""
    notes: list[str] = field(default_factory=list)
    
    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for YAML/JSON serialization."""
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> OptimizationPlan:
        """Create from dictionary."""
        return cls(**{k: v for k, v in data.items() if k in cls.__dataclass_fields__})


@dataclass
class ApplyResult:
    """Result of applying an optimization plan."""
    success: bool = False
    dry_run: bool = True
    actions_applied: list[str] = field(default_factory=list)
    actions_skipped: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)
    metrics: dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return asdict(self)


# -----------------------------------------------------------------------------
# Detection Utilities
# -----------------------------------------------------------------------------

def _run_command(
    cmd: list[str],
    timeout: int = 10,
    capture_stderr: bool = True
) -> tuple[bool, str, str]:
    """
    Safely run a shell command and capture output.
    
    Returns:
        Tuple of (success, stdout, stderr)
    
    Note: Never executes arbitrary commands; only predefined safe commands.
    """
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
            env={**os.environ, "LANG": "C"}  # Force consistent output format
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
        return False, "", f"Error running command: {e}"


def _check_tool_available(tool: str) -> bool:
    """Check if a command-line tool is available."""
    return shutil.which(tool) is not None


def _parse_nvidia_smi(output: str) -> dict[str, Any]:
    """
    Parse nvidia-smi output for GPU information.
    
    Handles both query format and standard output.
    """
    info: dict[str, Any] = {}
    
    # Try to parse CSV query format first
    # nvidia-smi --query-gpu=name,memory.total,memory.free,driver_version,... --format=csv,noheader
    lines = output.strip().split("\n")
    
    for line in lines:
        # Parse key-value pairs from various nvidia-smi output formats
        if "Driver Version:" in line:
            match = re.search(r"Driver Version:\s*(\S+)", line)
            if match:
                info["driver"] = match.group(1)
        
        if "CUDA Version:" in line:
            match = re.search(r"CUDA Version:\s*(\S+)", line)
            if match:
                info["cuda_version"] = match.group(1)
        
        # Product name from query format
        if "name" not in info and not line.startswith("|"):
            # First non-header line might be GPU name in CSV format
            parts = [p.strip() for p in line.split(",")]
            if len(parts) >= 1 and not parts[0].startswith("="):
                info["model"] = parts[0]
    
    # Parse memory information
    memory_match = re.search(r"(\d+)\s*MiB\s*/\s*(\d+)\s*MiB", output)
    if memory_match:
        info["vram_free_mb"] = int(memory_match.group(1))
        info["vram_total_mb"] = int(memory_match.group(2))
    
    # Parse clock speed
    clock_match = re.search(r"Graphics\s*:\s*(\d+)\s*MHz", output)
    if clock_match:
        info["clock_mhz"] = int(clock_match.group(1))
    
    # Parse SM count (from detailed query)
    sm_match = re.search(r"SM\s*Count\s*:\s*(\d+)", output, re.IGNORECASE)
    if sm_match:
        info["num_sm"] = int(sm_match.group(1))
    
    return info


def _parse_rocm_smi(output: str) -> dict[str, Any]:
    """Parse rocm-smi output for AMD GPU information."""
    info: dict[str, Any] = {}
    
    # GPU model
    model_match = re.search(r"GPU\[\d+\]\s*:\s*(.*)", output)
    if model_match:
        info["model"] = model_match.group(1).strip()
    
    # VRAM
    vram_match = re.search(r"VRAM\s+Total\s+Memory\s+\(B\)\s*:\s*(\d+)", output)
    if vram_match:
        info["vram_total_mb"] = int(vram_match.group(1)) // (1024 * 1024)
    
    vram_used_match = re.search(r"VRAM\s+Total\s+Used\s+Memory\s+\(B\)\s*:\s*(\d+)", output)
    if vram_used_match and "vram_total_mb" in info:
        used_mb = int(vram_used_match.group(1)) // (1024 * 1024)
        info["vram_free_mb"] = info["vram_total_mb"] - used_mb
    
    # Clock speed
    clock_match = re.search(r"GPU\s+Clock\s+Level\s*:.*?(\d+)Mhz", output, re.IGNORECASE)
    if clock_match:
        info["clock_mhz"] = int(clock_match.group(1))
    
    return info


def _parse_lspci(output: str) -> list[dict[str, Any]]:
    """Parse lspci output for GPU devices."""
    gpus: list[dict[str, Any]] = []
    
    # Look for VGA compatible controllers and 3D controllers
    pattern = r"([0-9a-f:.]+)\s+(VGA compatible controller|3D controller):\s*(.*)"
    
    for match in re.finditer(pattern, output, re.IGNORECASE):
        pci_id = match.group(1)
        desc = match.group(3)
        
        gpu_info: dict[str, Any] = {
            "device_id": pci_id,
            "model": desc.strip(),
        }
        
        # Detect vendor from description
        desc_lower = desc.lower()
        if "nvidia" in desc_lower:
            gpu_info["vendor"] = GPUVendor.NVIDIA.value
        elif "amd" in desc_lower or "radeon" in desc_lower:
            gpu_info["vendor"] = GPUVendor.AMD.value
        elif "intel" in desc_lower:
            gpu_info["vendor"] = GPUVendor.INTEL.value
        else:
            gpu_info["vendor"] = GPUVendor.UNKNOWN.value
        
        gpus.append(gpu_info)
    
    return gpus


def _parse_lshw(output: str) -> dict[str, Any]:
    """Parse lshw -C display output."""
    info: dict[str, Any] = {}
    
    # Product name
    product_match = re.search(r"product:\s*(.*)", output)
    if product_match:
        info["model"] = product_match.group(1).strip()
    
    # Vendor
    vendor_match = re.search(r"vendor:\s*(.*)", output)
    if vendor_match:
        vendor_str = vendor_match.group(1).lower()
        if "nvidia" in vendor_str:
            info["vendor"] = GPUVendor.NVIDIA.value
        elif "amd" in vendor_str or "advanced micro" in vendor_str:
            info["vendor"] = GPUVendor.AMD.value
        elif "intel" in vendor_str:
            info["vendor"] = GPUVendor.INTEL.value
    
    # Bus info (PCI ID)
    bus_match = re.search(r"bus info:\s*pci@([\w:.]+)", output)
    if bus_match:
        info["device_id"] = bus_match.group(1)
    
    # Memory size
    memory_match = re.search(r"size:\s*(\d+)\s*(M|G)iB", output, re.IGNORECASE)
    if memory_match:
        size = int(memory_match.group(1))
        unit = memory_match.group(2).upper()
        info["vram_total_mb"] = size * 1024 if unit == "G" else size
    
    # Driver
    driver_match = re.search(r"configuration:.*driver=(\S+)", output)
    if driver_match:
        info["driver"] = driver_match.group(1)
    
    return info


def _parse_glxinfo(output: str) -> dict[str, Any]:
    """Parse glxinfo output for OpenGL information."""
    info: dict[str, Any] = {}
    
    # OpenGL version
    gl_version_match = re.search(r"OpenGL version string:\s*(.*)", output)
    if gl_version_match:
        version_str = gl_version_match.group(1).strip()
        # Extract version number (e.g., "4.6.0" from "4.6.0 NVIDIA 535.154.05")
        version_num = re.search(r"(\d+\.\d+\.?\d*)", version_str)
        if version_num:
            info["opengl_version"] = version_num.group(1)
    
    # OpenGL renderer (GPU model)
    renderer_match = re.search(r"OpenGL renderer string:\s*(.*)", output)
    if renderer_match:
        info["model"] = renderer_match.group(1).strip()
    
    # Vendor
    vendor_match = re.search(r"OpenGL vendor string:\s*(.*)", output)
    if vendor_match:
        vendor_str = vendor_match.group(1).lower()
        if "nvidia" in vendor_str:
            info["vendor"] = GPUVendor.NVIDIA.value
        elif "amd" in vendor_str or "ati" in vendor_str:
            info["vendor"] = GPUVendor.AMD.value
        elif "intel" in vendor_str:
            info["vendor"] = GPUVendor.INTEL.value
    
    return info


def _parse_vulkaninfo(output: str) -> dict[str, Any]:
    """Parse vulkaninfo output."""
    info: dict[str, Any] = {}
    
    # Vulkan API version
    api_version_match = re.search(r"apiVersion\s*=\s*(\d+\.\d+\.?\d*)", output)
    if api_version_match:
        info["vulkan_version"] = api_version_match.group(1)
    
    # Device name
    device_match = re.search(r"deviceName\s*=\s*(.*)", output)
    if device_match:
        info["model"] = device_match.group(1).strip()
    
    return info


def _probe_torch_cuda() -> dict[str, Any]:
    """Probe GPU via PyTorch CUDA if available."""
    info: dict[str, Any] = {}
    
    try:
        import torch
        if torch.cuda.is_available():
            device_count = torch.cuda.device_count()
            if device_count > 0:
                # Get info for first GPU
                props = torch.cuda.get_device_properties(0)
                info["model"] = props.name
                info["vram_total_mb"] = props.total_memory // (1024 * 1024)
                info["num_sm"] = props.multi_processor_count
                info["compute_capability"] = f"{props.major}.{props.minor}"
                info["vendor"] = GPUVendor.NVIDIA.value
                
                # Current free memory
                free, total = torch.cuda.mem_get_info(0)
                info["vram_free_mb"] = free // (1024 * 1024)
                
                # CUDA version
                info["cuda_version"] = torch.version.cuda or ""
    except ImportError:
        pass  # PyTorch not available
    except Exception as e:
        info["notes"] = [f"torch.cuda probe error: {e}"]
    
    return info


def _probe_pyopencl() -> dict[str, Any]:
    """Probe GPU via PyOpenCL if available."""
    info: dict[str, Any] = {}
    
    try:
        import pyopencl as cl
        platforms = cl.get_platforms()
        for platform in platforms:
            devices = platform.get_devices(device_type=cl.device_type.GPU)
            for device in devices:
                info["model"] = device.name
                info["vram_total_mb"] = device.global_mem_size // (1024 * 1024)
                
                # OpenCL version
                version_match = re.search(r"OpenCL\s+(\d+\.\d+)", device.version)
                if version_match:
                    info["opencl_version"] = version_match.group(1)
                
                # Detect vendor
                vendor_lower = device.vendor.lower()
                if "nvidia" in vendor_lower:
                    info["vendor"] = GPUVendor.NVIDIA.value
                elif "amd" in vendor_lower:
                    info["vendor"] = GPUVendor.AMD.value
                elif "intel" in vendor_lower:
                    info["vendor"] = GPUVendor.INTEL.value
                
                # Only get first GPU
                break
            if info:
                break
    except ImportError:
        pass  # PyOpenCL not available
    except Exception as e:
        info["notes"] = [f"pyopencl probe error: {e}"]
    
    return info


def _probe_pycuda() -> dict[str, Any]:
    """Probe GPU via PyCUDA if available."""
    info: dict[str, Any] = {}
    
    try:
        import pycuda.driver as cuda
        import pycuda.autoinit  # noqa: F401 - required for initialization
        
        device = cuda.Device(0)
        info["model"] = device.name()
        info["vram_total_mb"] = device.total_memory() // (1024 * 1024)
        info["compute_capability"] = f"{device.compute_capability()[0]}.{device.compute_capability()[1]}"
        info["num_sm"] = device.get_attribute(cuda.device_attribute.MULTIPROCESSOR_COUNT)
        info["vendor"] = GPUVendor.NVIDIA.value
        
        # Memory bus width
        info["memory_bus_width"] = device.get_attribute(
            cuda.device_attribute.GLOBAL_MEMORY_BUS_WIDTH
        )
    except ImportError:
        pass  # PyCUDA not available
    except Exception as e:
        info["notes"] = [f"pycuda probe error: {e}"]
    
    return info


# -----------------------------------------------------------------------------
# Main Detection Function
# -----------------------------------------------------------------------------

def detect_gpus(
    use_vendor_cli: bool = True,
    use_system_tools: bool = True,
    use_python_libs: bool = True,
    verbose: bool = False
) -> list[GPUSpec]:
    """
    Detect GPU hardware and driver capabilities.
    
    Detection is performed in order of reliability:
    1. Vendor CLIs (nvidia-smi, rocm-smi, intel_gpu_top, vulkaninfo)
    2. System tools (lspci, lshw, glxinfo)
    3. Python libraries (torch.cuda, pyopencl, pycuda)
    
    Args:
        use_vendor_cli: Enable vendor-specific CLI detection
        use_system_tools: Enable system tool detection (lspci, lshw)
        use_python_libs: Enable Python library probes
        verbose: Enable verbose logging
    
    Returns:
        List of GPUSpec objects, one per detected GPU
    
    Example:
        >>> specs = detect_gpus()
        >>> for spec in specs:
        ...     print(f"{spec.vendor}: {spec.model} ({spec.vram_total_mb} MB)")
    """
    if verbose:
        global logger
        logger = setup_logging(verbose=True)
    
    detected: list[GPUSpec] = []
    detection_sources: list[str] = []
    notes: list[str] = []
    
    # Aggregate info from multiple sources
    merged_info: dict[str, Any] = {}
    
    # ---------------------------------------------------------------------
    # Phase 1: Vendor CLIs (most reliable)
    # ---------------------------------------------------------------------
    if use_vendor_cli:
        # NVIDIA: nvidia-smi
        if _check_tool_available("nvidia-smi"):
            success, stdout, stderr = _run_command([
                "nvidia-smi",
                "--query-gpu=name,memory.total,memory.free,driver_version,clocks.gr",
                "--format=csv,noheader,nounits"
            ])
            if success and stdout:
                detection_sources.append("nvidia-smi")
                nvidia_info = _parse_nvidia_smi(stdout)
                nvidia_info["vendor"] = GPUVendor.NVIDIA.value
                merged_info.update(nvidia_info)
            
            # Also try standard format for more details
            success, stdout, _ = _run_command(["nvidia-smi"])
            if success and stdout:
                nvidia_info = _parse_nvidia_smi(stdout)
                # Only update fields not already set
                for k, v in nvidia_info.items():
                    if k not in merged_info or not merged_info[k]:
                        merged_info[k] = v
        else:
            notes.append("nvidia-smi not found")
        
        # AMD: rocm-smi
        if _check_tool_available("rocm-smi"):
            success, stdout, stderr = _run_command(["rocm-smi", "--showproductname", "--showmeminfo", "vram"])
            if success and stdout:
                detection_sources.append("rocm-smi")
                amd_info = _parse_rocm_smi(stdout)
                amd_info["vendor"] = GPUVendor.AMD.value
                for k, v in amd_info.items():
                    if k not in merged_info or not merged_info[k]:
                        merged_info[k] = v
        else:
            notes.append("rocm-smi not found")
        
        # Intel: intel_gpu_top (requires root, may fail)
        if _check_tool_available("intel_gpu_top"):
            notes.append("intel_gpu_top available but requires root privileges")
        
        # Vulkan info
        if _check_tool_available("vulkaninfo"):
            success, stdout, _ = _run_command(["vulkaninfo", "--summary"], timeout=15)
            if success and stdout:
                detection_sources.append("vulkaninfo")
                vulkan_info = _parse_vulkaninfo(stdout)
                for k, v in vulkan_info.items():
                    if k not in merged_info or not merged_info[k]:
                        merged_info[k] = v
        else:
            notes.append("vulkaninfo not found")
    
    # ---------------------------------------------------------------------
    # Phase 2: System tools (fallback)
    # ---------------------------------------------------------------------
    if use_system_tools:
        # lspci
        if _check_tool_available("lspci"):
            success, stdout, _ = _run_command(["lspci", "-v"])
            if success and stdout:
                detection_sources.append("lspci")
                lspci_gpus = _parse_lspci(stdout)
                if lspci_gpus:
                    # Use first GPU info as fallback
                    for k, v in lspci_gpus[0].items():
                        if k not in merged_info or not merged_info[k]:
                            merged_info[k] = v
        else:
            notes.append("lspci not found")
        
        # lshw (may require root)
        if _check_tool_available("lshw"):
            success, stdout, stderr = _run_command(["lshw", "-C", "display", "-numeric"])
            if success and stdout:
                detection_sources.append("lshw")
                lshw_info = _parse_lshw(stdout)
                for k, v in lshw_info.items():
                    if k not in merged_info or not merged_info[k]:
                        merged_info[k] = v
            elif "WARNING: you should run" in stderr:
                notes.append("lshw requires root for full hardware info")
        else:
            notes.append("lshw not found")
        
        # glxinfo
        if _check_tool_available("glxinfo"):
            success, stdout, _ = _run_command(["glxinfo"], timeout=5)
            if success and stdout:
                detection_sources.append("glxinfo")
                glx_info = _parse_glxinfo(stdout)
                for k, v in glx_info.items():
                    if k not in merged_info or not merged_info[k]:
                        merged_info[k] = v
        else:
            notes.append("glxinfo not found (install mesa-utils)")
    
    # ---------------------------------------------------------------------
    # Phase 3: Python library probes (optional)
    # ---------------------------------------------------------------------
    if use_python_libs:
        # PyTorch CUDA
        torch_info = _probe_torch_cuda()
        if torch_info:
            detection_sources.append("torch.cuda")
            for k, v in torch_info.items():
                if k != "notes" and (k not in merged_info or not merged_info[k]):
                    merged_info[k] = v
            if "notes" in torch_info:
                notes.extend(torch_info["notes"])
        
        # PyOpenCL
        opencl_info = _probe_pyopencl()
        if opencl_info:
            detection_sources.append("pyopencl")
            for k, v in opencl_info.items():
                if k != "notes" and (k not in merged_info or not merged_info[k]):
                    merged_info[k] = v
            if "notes" in opencl_info:
                notes.extend(opencl_info["notes"])
        
        # PyCUDA
        pycuda_info = _probe_pycuda()
        if pycuda_info:
            detection_sources.append("pycuda")
            for k, v in pycuda_info.items():
                if k != "notes" and (k not in merged_info or not merged_info[k]):
                    merged_info[k] = v
            if "notes" in pycuda_info:
                notes.extend(pycuda_info["notes"])
    
    # ---------------------------------------------------------------------
    # Build GPUSpec from merged info
    # ---------------------------------------------------------------------
    if merged_info:
        spec = GPUSpec(
            vendor=merged_info.get("vendor", "unknown"),
            model=merged_info.get("model", "unknown"),
            device_id=merged_info.get("device_id", ""),
            driver=merged_info.get("driver", ""),
            vram_total_mb=merged_info.get("vram_total_mb", 0),
            vram_free_mb=merged_info.get("vram_free_mb", 0),
            compute_capability=merged_info.get("compute_capability", ""),
            cuda_version=merged_info.get("cuda_version", ""),
            opencl_version=merged_info.get("opencl_version", ""),
            vulkan_version=merged_info.get("vulkan_version", ""),
            opengl_version=merged_info.get("opengl_version", ""),
            clock_mhz=merged_info.get("clock_mhz", 0),
            memory_bus_width=merged_info.get("memory_bus_width", 0),
            num_sm=merged_info.get("num_sm", 0),
            pci_bandwidth=merged_info.get("pci_bandwidth", ""),
            notes=notes,
            detection_timestamp=datetime.now(timezone.utc).isoformat(),
            detection_sources=detection_sources,
        )
        detected.append(spec)
        
        # Log metrics
        logger.info(
            "GPU detection completed",
            extra={
                "metrics": {
                    "detection_success": True,
                    "sources_used": len(detection_sources),
                    "gpus_found": 1,
                }
            }
        )
    else:
        notes.append("No GPU detected by any method")
        # Return a spec with notes even if nothing detected
        spec = GPUSpec(
            notes=notes,
            detection_timestamp=datetime.now(timezone.utc).isoformat(),
            detection_sources=[],
        )
        detected.append(spec)
        
        logger.warning(
            "No GPU detected",
            extra={
                "metrics": {
                    "detection_success": False,
                    "sources_used": 0,
                    "gpus_found": 0,
                }
            }
        )
    
    # Save last spec
    _save_spec(detected)
    
    return detected


def _save_spec(specs: list[GPUSpec]) -> None:
    """Persist specs to config directory."""
    try:
        CONFIG_DIR.mkdir(parents=True, exist_ok=True)
        with open(LAST_SPEC_FILE, "w") as f:
            json.dump([s.to_dict() for s in specs], f, indent=2)
    except Exception as e:
        logger.warning(f"Failed to save spec: {e}")


def load_last_spec() -> list[GPUSpec]:
    """Load the last saved GPU spec."""
    try:
        if LAST_SPEC_FILE.exists():
            with open(LAST_SPEC_FILE) as f:
                data = json.load(f)
            return [GPUSpec.from_dict(d) for d in data]
    except Exception as e:
        logger.warning(f"Failed to load spec: {e}")
    return []


# -----------------------------------------------------------------------------
# Planning Functions
# -----------------------------------------------------------------------------

def _compute_spec_hash(specs: list[GPUSpec]) -> str:
    """Compute a hash of GPU specs for plan validation."""
    spec_str = json.dumps([s.to_dict() for s in specs], sort_keys=True)
    return hashlib.sha256(spec_str.encode()).hexdigest()[:16]


def plan_optimizations(
    specs: list[GPUSpec],
    mode: str = "safe",
    config_overrides: dict[str, Any] | None = None
) -> OptimizationPlan:
    """
    Generate an optimization plan based on GPU specs and mode.
    
    Args:
        specs: List of GPUSpec objects from detect_gpus()
        mode: "safe" or "aggressive" optimization mode
        config_overrides: Optional dict to override computed values
    
    Returns:
        OptimizationPlan with recommendations and confidence scores
    
    Heuristics:
    - Safe mode: Conservative VRAM caps, lower batch sizes, no preemptive throttle
    - Aggressive mode: Higher VRAM utilization, larger batches, preemptive throttle
    
    Example:
        >>> specs = detect_gpus()
        >>> plan = plan_optimizations(specs, mode="safe")
        >>> print(f"VRAM cap: {plan.per_process_vram_cap_mb} MB (confidence: {plan.confidence_vram_cap:.2f})")
    """
    plan_mode = PlanMode.SAFE if mode.lower() == "safe" else PlanMode.AGGRESSIVE
    notes: list[str] = []
    
    # Use first GPU spec (extend for multi-GPU later)
    spec = specs[0] if specs else GPUSpec()
    
    # Base confidence from detection quality
    base_confidence = 0.3
    if spec.detection_sources:
        base_confidence += 0.1 * len(spec.detection_sources)
    if spec.vram_total_mb > 0:
        base_confidence += 0.2
    if spec.compute_capability:
        base_confidence += 0.1
    base_confidence = min(base_confidence, 1.0)
    
    # ---------------------------------------------------------------------
    # VRAM Cap Calculation
    # ---------------------------------------------------------------------
    if spec.vram_total_mb > 0:
        if plan_mode == PlanMode.SAFE:
            # Reserve 25% for system and other apps
            vram_cap = int(spec.vram_total_mb * 0.75)
        else:
            # Aggressive: use up to 90%
            vram_cap = int(spec.vram_total_mb * 0.90)
        confidence_vram = base_confidence + 0.15
    else:
        # Fallback defaults
        vram_cap = DEFAULT_SAFE_VRAM_CAP_MB if plan_mode == PlanMode.SAFE else DEFAULT_AGGRESSIVE_VRAM_CAP_MB
        confidence_vram = 0.3
        notes.append("VRAM cap based on defaults (detection incomplete)")
    
    confidence_vram = min(confidence_vram, 1.0)
    
    # ---------------------------------------------------------------------
    # Batch Size Calculation
    # ---------------------------------------------------------------------
    # Heuristic: batch size scales with available VRAM and SM count
    if spec.vram_total_mb >= 24000:  # 24GB+ (A100, 4090, etc.)
        base_batch = 64
    elif spec.vram_total_mb >= 16000:  # 16GB (A6000, 3090, etc.)
        base_batch = 32
    elif spec.vram_total_mb >= 8000:  # 8GB
        base_batch = 16
    elif spec.vram_total_mb >= 4000:  # 4GB
        base_batch = 8
    else:
        base_batch = 4
    
    # Adjust for mode
    if plan_mode == PlanMode.AGGRESSIVE:
        batch_size = base_batch * 2
    else:
        batch_size = base_batch
    
    # SM-based adjustment
    if spec.num_sm > 0:
        # More SMs can handle larger batches
        sm_factor = max(1.0, spec.num_sm / 64)  # Normalize to ~64 SMs
        batch_size = int(batch_size * min(sm_factor, 2.0))
        confidence_batch = base_confidence + 0.2
    else:
        confidence_batch = base_confidence
        notes.append("Batch size estimated without SM count")
    
    confidence_batch = min(confidence_batch, 1.0)
    
    # ---------------------------------------------------------------------
    # Tensor Chunk Size
    # ---------------------------------------------------------------------
    # Chunk size for gradient checkpointing and memory management
    if spec.memory_bus_width >= 384:  # High-bandwidth memory
        tensor_chunk = 128
    elif spec.memory_bus_width >= 256:
        tensor_chunk = 96
    else:
        tensor_chunk = 64
    
    if plan_mode == PlanMode.AGGRESSIVE:
        tensor_chunk = int(tensor_chunk * 1.5)
    
    # ---------------------------------------------------------------------
    # Thermal Thresholds
    # ---------------------------------------------------------------------
    # Use defaults but adjust for vendor (AMD tends to run hotter)
    cooldown_c = DEFAULT_COOLDOWN_THRESHOLD_C
    resume_c = DEFAULT_RESUME_THRESHOLD_C
    
    if spec.vendor == GPUVendor.AMD.value:
        cooldown_c = 85  # AMD GPUs have higher thermal limits
        resume_c = 72
    elif spec.vendor == GPUVendor.INTEL.value:
        cooldown_c = 80  # Intel iGPUs throttle earlier
        resume_c = 65
    
    # Confidence in thermal depends on vendor detection
    confidence_thermal = base_confidence if spec.vendor != "unknown" else 0.4
    
    # ---------------------------------------------------------------------
    # Scheduling Policy
    # ---------------------------------------------------------------------
    if plan_mode == PlanMode.AGGRESSIVE:
        scheduling = SchedulingPolicy.PRIORITY_BASED.value
    else:
        scheduling = SchedulingPolicy.ROUND_ROBIN.value
    
    # Preemptive throttle only in aggressive mode
    preemptive_throttle = plan_mode == PlanMode.AGGRESSIVE
    
    # ---------------------------------------------------------------------
    # VRAM Deduplication & Compression Estimates
    # ---------------------------------------------------------------------
    # Simulate duplicate buffer detection based on VRAM size
    # Typical ML workloads have 10-20% duplicate tensors
    if spec.vram_total_mb > 0:
        dedup_ratio = 0.15 if plan_mode == PlanMode.AGGRESSIVE else 0.10
        estimated_reclaimed = int(spec.vram_total_mb * dedup_ratio)
        dedup_buffer_count = max(1, estimated_reclaimed // 64)  # Assume 64MB avg buffer
        compression_ratio = 1.3 if plan_mode == PlanMode.AGGRESSIVE else 1.15
        confidence_dedup = base_confidence * 0.7  # Lower confidence for estimates
    else:
        dedup_ratio = 0.0
        estimated_reclaimed = 0
        dedup_buffer_count = 0
        compression_ratio = 1.0
        confidence_dedup = 0.2
    
    # ---------------------------------------------------------------------
    # Multi-tenant Fairness Scheduling
    # ---------------------------------------------------------------------
    if spec.num_sm > 0:
        max_concurrent = min(8, max(2, spec.num_sm // 16))
    else:
        max_concurrent = 4  # Default
    
    fairness_weight = 1.0  # Equal weight for all processes by default
    utilization_heatmap = plan_mode == PlanMode.AGGRESSIVE  # Only in aggressive mode
    
    # ---------------------------------------------------------------------
    # Build Rationale Strings
    # ---------------------------------------------------------------------
    rationale: dict[str, str] = {
        "vram_cap": f"Using {75 if plan_mode == PlanMode.SAFE else 90}% of detected {spec.vram_total_mb}MB VRAM to {'reserve headroom for system' if plan_mode == PlanMode.SAFE else 'maximize utilization'}",
        "batch_size": f"Batch size {batch_size} based on {spec.vram_total_mb}MB VRAM {'and ' + str(spec.num_sm) + ' SMs' if spec.num_sm > 0 else ''}",
        "tensor_chunk": f"Chunk size {tensor_chunk}MB optimized for {'high-bandwidth' if spec.memory_bus_width >= 256 else 'standard'} memory bus",
        "thermal": f"Thermal thresholds set for {spec.vendor.upper()} GPU: cooldown at {cooldown_c}°C, resume at {resume_c}°C",
        "scheduling": f"{'Priority-based' if scheduling == SchedulingPolicy.PRIORITY_BASED.value else 'Round-robin'} scheduling for {'aggressive throughput' if plan_mode == PlanMode.AGGRESSIVE else 'fair resource sharing'}",
        "dedup": f"Estimated {estimated_reclaimed}MB reclaimable from ~{dedup_buffer_count} duplicate buffers ({int(dedup_ratio*100)}% dedup rate)",
    }
    
    # ---------------------------------------------------------------------
    # Apply config overrides
    # ---------------------------------------------------------------------
    if config_overrides:
        if "per_process_vram_cap_mb" in config_overrides:
            vram_cap = config_overrides["per_process_vram_cap_mb"]
            notes.append("VRAM cap overridden by config")
            rationale["vram_cap"] = f"VRAM cap {vram_cap}MB set by user configuration override"
        if "recommended_batch_size" in config_overrides:
            batch_size = config_overrides["recommended_batch_size"]
            notes.append("Batch size overridden by config")
            rationale["batch_size"] = f"Batch size {batch_size} set by user configuration override"
        if "cooldown_threshold_c" in config_overrides:
            cooldown_c = config_overrides["cooldown_threshold_c"]
        if "resume_threshold_c" in config_overrides:
            resume_c = config_overrides["resume_threshold_c"]
    
    # Overall confidence is average of components
    confidence_overall = (confidence_vram + confidence_batch + confidence_thermal + confidence_dedup) / 4
    
    plan = OptimizationPlan(
        mode=mode,
        per_process_vram_cap_mb=vram_cap,
        recommended_batch_size=batch_size,
        tensor_chunk_size_mb=tensor_chunk,
        cooldown_threshold_c=cooldown_c,
        resume_threshold_c=resume_c,
        fair_scheduling_policy=scheduling,
        preemptive_throttle=preemptive_throttle,
        # VRAM deduplication
        estimated_reclaimed_mb=estimated_reclaimed,
        dedup_buffer_count=dedup_buffer_count,
        compression_ratio=round(compression_ratio, 2),
        # Fairness scheduling
        fairness_weight_per_process=fairness_weight,
        max_concurrent_kernels=max_concurrent,
        utilization_heatmap_enabled=utilization_heatmap,
        # Confidence scores
        confidence_vram_cap=round(confidence_vram, 3),
        confidence_batch_size=round(confidence_batch, 3),
        confidence_thermal=round(confidence_thermal, 3),
        confidence_dedup=round(confidence_dedup, 3),
        confidence_overall=round(confidence_overall, 3),
        # Rationale
        rationale=rationale,
        # Metadata
        generated_at=datetime.now(timezone.utc).isoformat(),
        gpu_specs_hash=_compute_spec_hash(specs),
        notes=notes,
    )
    
    # Save plan
    _save_plan(plan)
    
    # Log metrics
    logger.info(
        "Optimization plan generated",
        extra={
            "metrics": {
                "plan_mode": mode,
                "plan_confidence": confidence_overall,
                "vram_cap_mb": vram_cap,
                "batch_size": batch_size,
            }
        }
    )
    
    return plan


def _save_plan(plan: OptimizationPlan) -> None:
    """Persist plan to config directory."""
    try:
        CONFIG_DIR.mkdir(parents=True, exist_ok=True)
        if HAS_YAML:
            with open(LAST_PLAN_FILE, "w") as f:
                yaml.dump(plan.to_dict(), f, default_flow_style=False)
        else:
            # Fallback to JSON if YAML not available
            json_path = LAST_PLAN_FILE.with_suffix(".json")
            with open(json_path, "w") as f:
                json.dump(plan.to_dict(), f, indent=2)
    except Exception as e:
        logger.warning(f"Failed to save plan: {e}")


def load_last_plan() -> OptimizationPlan | None:
    """Load the last saved optimization plan."""
    try:
        if LAST_PLAN_FILE.exists() and HAS_YAML:
            with open(LAST_PLAN_FILE) as f:
                data = yaml.safe_load(f)
            return OptimizationPlan.from_dict(data)
        # Try JSON fallback
        json_path = LAST_PLAN_FILE.with_suffix(".json")
        if json_path.exists():
            with open(json_path) as f:
                data = json.load(f)
            return OptimizationPlan.from_dict(data)
    except Exception as e:
        logger.warning(f"Failed to load plan: {e}")
    return None


def load_config_overrides() -> dict[str, Any]:
    """Load user configuration overrides from YAML config file."""
    if not HAS_YAML:
        return {}
    
    try:
        if CONFIG_FILE.exists():
            with open(CONFIG_FILE) as f:
                return yaml.safe_load(f) or {}
    except Exception as e:
        logger.warning(f"Failed to load config: {e}")
    return {}


# -----------------------------------------------------------------------------
# Enforcement Functions
# -----------------------------------------------------------------------------

def apply_plan(
    plan: OptimizationPlan,
    dry_run: bool = True,
    destructive: bool = False,
    verbose: bool = False
) -> ApplyResult:
    """
    Apply an optimization plan to the system.
    
    SAFETY: By default, this function operates in dry-run mode and will NOT
    make any changes. Destructive actions (killing processes, etc.) require
    both dry_run=False AND destructive=True.
    
    Args:
        plan: OptimizationPlan to apply
        dry_run: If True (default), only simulate actions
        destructive: If True, allow destructive actions (requires dry_run=False)
        verbose: Enable verbose logging
    
    Returns:
        ApplyResult with details of actions taken/simulated
    
    Example:
        >>> plan = plan_optimizations(specs, mode="safe")
        >>> result = apply_plan(plan, dry_run=True)
        >>> print(f"Would apply {len(result.actions_applied)} actions")
        
        # To actually apply (non-destructive only):
        >>> result = apply_plan(plan, dry_run=False)
        
        # To allow destructive actions (use with extreme caution):
        >>> result = apply_plan(plan, dry_run=False, destructive=True)
    """
    result = ApplyResult(
        dry_run=dry_run,
        metrics={
            "plan_mode": plan.mode,
            "timestamp": datetime.now(timezone.utc).isoformat(),
        }
    )
    
    actions: list[tuple[str, str, Callable[[], bool]]] = []
    
    # ---------------------------------------------------------------------
    # Define available actions
    # ---------------------------------------------------------------------
    
    # Action: Set environment variables for VRAM limiting
    def action_set_vram_env() -> bool:
        """Set CUDA_VISIBLE_DEVICES limits (non-destructive)."""
        # This would set env vars for child processes
        # In dry-run, we just validate the plan values
        if plan.per_process_vram_cap_mb > 0:
            os.environ["GREY_VRAM_CAP_MB"] = str(plan.per_process_vram_cap_mb)
            return True
        return False
    
    actions.append((
        "set_vram_environment",
        f"Set GREY_VRAM_CAP_MB={plan.per_process_vram_cap_mb}",
        action_set_vram_env
    ))
    
    # Action: Configure thermal monitoring thresholds
    def action_set_thermal_thresholds() -> bool:
        """Configure thermal thresholds (non-destructive)."""
        os.environ["GREY_COOLDOWN_C"] = str(plan.cooldown_threshold_c)
        os.environ["GREY_RESUME_C"] = str(plan.resume_threshold_c)
        return True
    
    actions.append((
        "set_thermal_thresholds",
        f"Set thermal thresholds: cooldown={plan.cooldown_threshold_c}°C, resume={plan.resume_threshold_c}°C",
        action_set_thermal_thresholds
    ))
    
    # Action: Configure scheduling policy
    def action_set_scheduling() -> bool:
        """Set scheduling policy (non-destructive)."""
        os.environ["GREY_SCHEDULING_POLICY"] = plan.fair_scheduling_policy
        return True
    
    actions.append((
        "set_scheduling_policy",
        f"Set scheduling policy: {plan.fair_scheduling_policy}",
        action_set_scheduling
    ))
    
    # Action: Enable preemptive throttle (if aggressive mode)
    if plan.preemptive_throttle:
        def action_enable_throttle() -> bool:
            """Enable preemptive throttling (non-destructive flag)."""
            os.environ["GREY_PREEMPTIVE_THROTTLE"] = "1"
            return True
        
        actions.append((
            "enable_preemptive_throttle",
            "Enable preemptive throttling for aggressive mode",
            action_enable_throttle
        ))
    
    # Action: Write batch size recommendation
    def action_write_batch_hint() -> bool:
        """Write recommended batch size to hint file (non-destructive)."""
        hint_file = CONFIG_DIR / "batch_size_hint.txt"
        if not dry_run:
            CONFIG_DIR.mkdir(parents=True, exist_ok=True)
            with open(hint_file, "w") as f:
                f.write(str(plan.recommended_batch_size))
        return True
    
    actions.append((
        "write_batch_hint",
        f"Write batch size hint: {plan.recommended_batch_size}",
        action_write_batch_hint
    ))
    
    # ---------------------------------------------------------------------
    # Execute or simulate actions
    # ---------------------------------------------------------------------
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
    
    # Determine overall success
    result.success = len(result.errors) == 0
    result.metrics["applied_actions_count"] = len([
        a for a in result.actions_applied if not a.startswith("[DRY-RUN]")
    ])
    result.metrics["dry_run_actions_count"] = len([
        a for a in result.actions_applied if a.startswith("[DRY-RUN]")
    ])
    
    # Log metrics
    logger.info(
        "Plan application completed",
        extra={
            "metrics": {
                "success": result.success,
                "dry_run": dry_run,
                "actions_applied": len(result.actions_applied),
                "errors": len(result.errors),
            }
        }
    )
    
    return result


# -----------------------------------------------------------------------------
# Artifact Output Functions
# -----------------------------------------------------------------------------

ARTIFACTS_DIR = CONFIG_DIR / "artifacts"


def write_vram_reclamation_log(plan: OptimizationPlan, specs: list[GPUSpec]) -> Path:
    """Write VRAM reclamation log as JSON artifact."""
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    artifact_path = ARTIFACTS_DIR / f"vram_reclamation_{timestamp}.json"
    
    artifact = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "plan_mode": plan.mode,
        "gpu_count": len(specs),
        "total_vram_mb": sum(s.vram_total_mb for s in specs),
        "estimated_reclaimed_mb": plan.estimated_reclaimed_mb,
        "dedup_buffer_count": plan.dedup_buffer_count,
        "compression_ratio": plan.compression_ratio,
        "confidence": plan.confidence_dedup,
        "rationale": plan.rationale.get("dedup", ""),
        "gpu_specs": [s.to_dict() for s in specs],
    }
    
    with open(artifact_path, "w") as f:
        json.dump(artifact, f, indent=2)
    
    logger.info(f"VRAM reclamation log written: {artifact_path}")
    return artifact_path


def write_thermal_signature_csv(plan: OptimizationPlan, specs: list[GPUSpec]) -> Path:
    """Write thermal signature CSV artifact."""
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    artifact_path = ARTIFACTS_DIR / f"thermal_signature_{timestamp}.csv"
    
    with open(artifact_path, "w") as f:
        f.write("timestamp,gpu_index,vendor,model,cooldown_threshold_c,resume_threshold_c,current_temp_c,throttle_active\n")
        for i, spec in enumerate(specs):
            # Simulated thermal data - in real daemon this would be live data
            f.write(f"{datetime.now(timezone.utc).isoformat()},{i},{spec.vendor},{spec.model},"
                    f"{plan.cooldown_threshold_c},{plan.resume_threshold_c},0,false\n")
    
    logger.info(f"Thermal signature CSV written: {artifact_path}")
    return artifact_path


def write_fairness_heatmap_csv(plan: OptimizationPlan, process_count: int = 4) -> Path:
    """Write fairness scheduling heatmap CSV artifact."""
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    artifact_path = ARTIFACTS_DIR / f"fairness_heatmap_{timestamp}.csv"
    
    with open(artifact_path, "w") as f:
        f.write("timestamp,process_id,utilization_pct,vram_used_mb,kernel_count,weight\n")
        for pid in range(process_count):
            # Simulated utilization data - in real daemon this would be live data
            f.write(f"{datetime.now(timezone.utc).isoformat()},proc_{pid},"
                    f"0.0,0,0,{plan.fairness_weight_per_process}\n")
    
    logger.info(f"Fairness heatmap CSV written: {artifact_path}")
    return artifact_path


def write_artifact_report(plan: OptimizationPlan, specs: list[GPUSpec]) -> Path:
    """Write comprehensive artifact report JSON."""
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    artifact_path = ARTIFACTS_DIR / f"artifact_report_{timestamp}.json"
    
    # Write sub-artifacts first
    vram_log = write_vram_reclamation_log(plan, specs)
    thermal_csv = write_thermal_signature_csv(plan, specs)
    fairness_csv = write_fairness_heatmap_csv(plan)
    
    report = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "plan": plan.to_dict(),
        "gpu_specs": [s.to_dict() for s in specs],
        "artifacts": {
            "vram_reclamation_log": str(vram_log),
            "thermal_signature_csv": str(thermal_csv),
            "fairness_heatmap_csv": str(fairness_csv),
        },
        "summary": {
            "total_vram_mb": sum(s.vram_total_mb for s in specs),
            "estimated_reclaimed_mb": plan.estimated_reclaimed_mb,
            "optimization_mode": plan.mode,
            "overall_confidence": plan.confidence_overall,
        }
    }
    
    with open(artifact_path, "w") as f:
        json.dump(report, f, indent=2)
    
    logger.info(f"Artifact report written: {artifact_path}")
    return artifact_path


# -----------------------------------------------------------------------------
# Daemon Loop
# -----------------------------------------------------------------------------

class GPUOptimizerDaemon:
    """
    Daemon for continuous GPU monitoring and optimization.
    
    Loads GPU spec at startup, monitors VRAM usage, thermal oscillations,
    and tensor loads, applying the plan in dry-run mode by default.
    """
    
    def __init__(
        self,
        interval_seconds: float = 30.0,
        dry_run: bool = True,
        consent: bool = False,
        verbose: bool = False
    ):
        self.interval = interval_seconds
        self.dry_run = dry_run
        self.consent = consent
        self.verbose = verbose
        self.running = False
        self.specs: list[GPUSpec] = []
        self.plan: OptimizationPlan | None = None
        self._logger = logging.getLogger("gpu_optimizer.daemon")
    
    def start(self) -> None:
        """Start the daemon loop."""
        self._logger.info("GPU Optimizer Daemon starting...")
        
        # Load GPU spec at startup
        self.specs = detect_gpus(verbose=self.verbose)
        if not self.specs:
            self._logger.error("No GPUs detected. Cannot start daemon.")
            return
        
        self._logger.info(f"Detected {len(self.specs)} GPU(s)")
        for i, spec in enumerate(self.specs):
            self._logger.info(f"  GPU {i}: {spec.vendor} {spec.model} ({spec.vram_total_mb}MB)")
        
        # Generate initial plan
        self.plan = plan_optimizations(self.specs, mode="safe")
        self._logger.info(f"Initial plan: VRAM cap {self.plan.per_process_vram_cap_mb}MB, "
                         f"batch size {self.plan.recommended_batch_size}")
        
        # Write initial artifacts
        write_artifact_report(self.plan, self.specs)
        
        self.running = True
        self._run_loop()
    
    def stop(self) -> None:
        """Stop the daemon loop."""
        self._logger.info("GPU Optimizer Daemon stopping...")
        self.running = False
    
    def _run_loop(self) -> None:
        """Main daemon monitoring loop."""
        import time
        
        iteration = 0
        while self.running:
            try:
                iteration += 1
                self._log_structured("daemon_tick", {
                    "iteration": iteration,
                    "dry_run": self.dry_run,
                })
                
                # Monitor current state
                self._monitor_vram()
                self._monitor_thermal()
                self._monitor_utilization()
                
                # Apply plan (dry-run by default)
                if self.plan:
                    result = apply_plan(
                        self.plan,
                        dry_run=self.dry_run,
                        destructive=self.consent,
                        verbose=self.verbose
                    )
                    
                    if not result.success:
                        self._logger.warning(f"Plan application had errors: {result.errors}")
                
                # Write periodic artifacts (every 10 iterations)
                if iteration % 10 == 0 and self.plan:
                    write_artifact_report(self.plan, self.specs)
                
                time.sleep(self.interval)
                
            except KeyboardInterrupt:
                self._logger.info("Daemon interrupted by user")
                self.stop()
                break
            except Exception as e:
                self._logger.error(f"Daemon error: {e}")
                time.sleep(self.interval)
    
    def _monitor_vram(self) -> None:
        """Monitor VRAM usage across GPUs."""
        # Re-detect to get current VRAM usage
        # In production, this would use nvidia-smi/rocm-smi for live stats
        self._log_structured("vram_monitor", {
            "gpu_count": len(self.specs),
            "estimated_reclaimed_mb": self.plan.estimated_reclaimed_mb if self.plan else 0,
        })
    
    def _monitor_thermal(self) -> None:
        """Monitor thermal state and oscillations."""
        # Would poll nvidia-smi/rocm-smi for temperature
        self._log_structured("thermal_monitor", {
            "cooldown_threshold": self.plan.cooldown_threshold_c if self.plan else 0,
            "resume_threshold": self.plan.resume_threshold_c if self.plan else 0,
        })
    
    def _monitor_utilization(self) -> None:
        """Monitor GPU utilization for fairness heatmap."""
        self._log_structured("utilization_monitor", {
            "max_concurrent_kernels": self.plan.max_concurrent_kernels if self.plan else 0,
            "heatmap_enabled": self.plan.utilization_heatmap_enabled if self.plan else False,
        })
    
    def _log_structured(self, event: str, details: dict[str, Any]) -> None:
        """Write structured JSON log entry."""
        log_entry = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "event": event,
            "details": details,
        }
        self._logger.debug(json.dumps(log_entry))


# -----------------------------------------------------------------------------
# CLI Interface
# -----------------------------------------------------------------------------

def cmd_detect(args: argparse.Namespace) -> int:
    """Handle 'detect' command."""
    specs = detect_gpus(
        use_vendor_cli=not args.no_vendor_cli,
        use_system_tools=not args.no_system_tools,
        use_python_libs=not args.no_python_libs,
        verbose=args.verbose
    )
    
    if args.json:
        output = json.dumps([s.to_dict() for s in specs], indent=2)
        print(output)
    else:
        for i, spec in enumerate(specs):
            print(f"\n=== GPU {i} ===")
            print(f"Vendor:       {spec.vendor}")
            print(f"Model:        {spec.model}")
            print(f"VRAM:         {spec.vram_total_mb} MB (free: {spec.vram_free_mb} MB)")
            print(f"Driver:       {spec.driver or 'N/A'}")
            print(f"CUDA:         {spec.cuda_version or 'N/A'}")
            print(f"Compute:      {spec.compute_capability or 'N/A'}")
            print(f"OpenGL:       {spec.opengl_version or 'N/A'}")
            print(f"Vulkan:       {spec.vulkan_version or 'N/A'}")
            print(f"OpenCL:       {spec.opencl_version or 'N/A'}")
            print(f"Clock:        {spec.clock_mhz} MHz" if spec.clock_mhz else "Clock:        N/A")
            print(f"SMs:          {spec.num_sm}" if spec.num_sm else "SMs:          N/A")
            print(f"Sources:      {', '.join(spec.detection_sources) or 'none'}")
            if spec.notes:
                print(f"Notes:        {'; '.join(spec.notes)}")
    
    return 0


def cmd_plan(args: argparse.Namespace) -> int:
    """Handle 'plan' command."""
    # Load specs
    specs = load_last_spec()
    if not specs or not specs[0].detection_sources:
        print("No GPU spec found. Running detection first...")
        specs = detect_gpus(verbose=args.verbose)
    
    # Load config overrides
    overrides = load_config_overrides()
    
    # Generate plan
    plan = plan_optimizations(specs, mode=args.mode, config_overrides=overrides)
    
    # Output
    if args.output:
        output_path = Path(args.output)
        if output_path.suffix in (".yaml", ".yml") and HAS_YAML:
            with open(output_path, "w") as f:
                yaml.dump(plan.to_dict(), f, default_flow_style=False)
        else:
            with open(output_path, "w") as f:
                json.dump(plan.to_dict(), f, indent=2)
        print(f"Plan saved to: {output_path}")
    else:
        print(f"\n=== Optimization Plan ({plan.mode.upper()} mode) ===")
        print(f"VRAM Cap:           {plan.per_process_vram_cap_mb} MB (confidence: {plan.confidence_vram_cap:.2f})")
        print(f"Batch Size:         {plan.recommended_batch_size} (confidence: {plan.confidence_batch_size:.2f})")
        print(f"Tensor Chunk:       {plan.tensor_chunk_size_mb} MB")
        print(f"Cooldown Temp:      {plan.cooldown_threshold_c}°C")
        print(f"Resume Temp:        {plan.resume_threshold_c}°C")
        print(f"Scheduling:         {plan.fair_scheduling_policy}")
        print(f"Preemptive Throttle:{' enabled' if plan.preemptive_throttle else ' disabled'}")
        print(f"Overall Confidence: {plan.confidence_overall:.2f}")
        if plan.notes:
            print(f"Notes:              {'; '.join(plan.notes)}")
    
    return 0


def cmd_apply(args: argparse.Namespace) -> int:
    """Handle 'apply' command."""
    # Load plan
    plan = load_last_plan()
    if not plan:
        print("No plan found. Generate one first with: gpu_optimizer.py plan")
        return 1
    
    # Confirm if not dry-run
    if not args.dry_run and not args.yes:
        print(f"This will apply the {plan.mode} optimization plan.")
        print("Actions to be taken:")
        for note in plan.notes:
            print(f"  - {note}")
        response = input("\nProceed? [y/N]: ").strip().lower()
        if response != "y":
            print("Aborted.")
            return 1
    
    # Apply
    result = apply_plan(
        plan,
        dry_run=args.dry_run,
        destructive=args.destructive,
        verbose=args.verbose
    )
    
    # Output
    if args.json:
        print(json.dumps(result.to_dict(), indent=2))
    else:
        mode_str = "[DRY-RUN] " if result.dry_run else ""
        print(f"\n{mode_str}=== Apply Result ===")
        print(f"Success: {result.success}")
        print(f"Actions:")
        for action in result.actions_applied:
            print(f"  ✓ {action}")
        for action in result.actions_skipped:
            print(f"  ⊘ {action}")
        if result.errors:
            print(f"Errors:")
            for error in result.errors:
                print(f"  ✗ {error}")
    
    return 0 if result.success else 1


def cmd_status(args: argparse.Namespace) -> int:
    """Handle 'status' command - show current GPU and plan status."""
    # Load last spec
    specs = load_last_spec()
    plan = load_last_plan()
    
    if args.json:
        status = {
            "gpu_detected": len(specs) > 0 if specs else False,
            "gpu_count": len(specs) if specs else 0,
            "gpus": [s.to_dict() for s in specs] if specs else [],
            "plan_exists": plan is not None,
            "plan": plan.to_dict() if plan else None,
            "config_dir": str(CONFIG_DIR),
            "artifacts_dir": str(ARTIFACTS_DIR),
        }
        print(json.dumps(status, indent=2))
    else:
        print("\n=== Grey GPU Optimizer Status ===\n")
        
        if specs:
            print(f"GPUs Detected: {len(specs)}")
            for i, spec in enumerate(specs):
                print(f"  [{i}] {spec.vendor.upper()} {spec.model}")
                print(f"      VRAM: {spec.vram_total_mb}MB, Driver: {spec.driver or 'N/A'}")
        else:
            print("GPUs Detected: None (run 'detect' first)")
        
        print()
        
        if plan:
            print(f"Active Plan: {plan.mode.upper()} mode")
            print(f"  VRAM Cap:     {plan.per_process_vram_cap_mb}MB")
            print(f"  Batch Size:   {plan.recommended_batch_size}")
            print(f"  Thermal:      {plan.cooldown_threshold_c}°C / {plan.resume_threshold_c}°C")
            print(f"  Confidence:   {plan.confidence_overall:.1%}")
            print(f"  Est. Reclaim: {plan.estimated_reclaimed_mb}MB")
            print(f"  Generated:    {plan.generated_at}")
        else:
            print("Active Plan: None (run 'plan' first)")
        
        print()
        print(f"Config Dir:    {CONFIG_DIR}")
        print(f"Artifacts Dir: {ARTIFACTS_DIR}")
        
        # Check for artifacts
        if ARTIFACTS_DIR.exists():
            artifacts = list(ARTIFACTS_DIR.glob("*.json")) + list(ARTIFACTS_DIR.glob("*.csv"))
            print(f"Artifacts:     {len(artifacts)} files")
        else:
            print("Artifacts:     0 files")
    
    return 0


def cmd_start_daemon(args: argparse.Namespace) -> int:
    """Handle 'start-daemon' command - start continuous monitoring daemon."""
    print("Starting GPU Optimizer Daemon...")
    print(f"  Interval:    {args.interval}s")
    print(f"  Dry-run:     {'yes' if args.dry_run else 'NO - LIVE MODE'}")
    print(f"  Consent:     {'yes' if args.consent else 'no'}")
    print()
    
    if not args.dry_run and not args.consent:
        print("WARNING: Non-dry-run mode requires --consent --confirm flags.")
        print("Running in dry-run mode instead.")
        args.dry_run = True
    
    if not args.dry_run:
        print("=" * 60)
        print("WARNING: LIVE MODE - Changes will be applied to the system!")
        print("=" * 60)
        if not args.confirm:
            response = input("Type 'yes' to confirm: ").strip().lower()
            if response != "yes":
                print("Aborted.")
                return 1
    
    daemon = GPUOptimizerDaemon(
        interval_seconds=args.interval,
        dry_run=args.dry_run,
        consent=args.consent,
        verbose=args.verbose
    )
    
    try:
        daemon.start()
    except KeyboardInterrupt:
        print("\nDaemon stopped by user.")
    
    return 0


def main() -> int:
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        prog="gpu_optimizer",
        description="GPU Hardware Detection, Planning, and Optimization",
        epilog="""
Examples:
  %(prog)s detect --json
  %(prog)s plan --mode safe --output plan.yaml
  %(prog)s plan --mode aggressive
  %(prog)s apply --dry-run
  %(prog)s apply --yes
  %(prog)s status
  %(prog)s start-daemon --interval 30
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("-v", "--verbose", action="store_true", help="Enable verbose logging")
    
    subparsers = parser.add_subparsers(dest="command", required=True)
    
    # detect subcommand
    detect_parser = subparsers.add_parser("detect", help="Detect GPU hardware")
    detect_parser.add_argument("--json", action="store_true", help="Output as JSON")
    detect_parser.add_argument("--no-vendor-cli", action="store_true", help="Skip vendor CLI detection")
    detect_parser.add_argument("--no-system-tools", action="store_true", help="Skip system tool detection")
    detect_parser.add_argument("--no-python-libs", action="store_true", help="Skip Python library probes")
    detect_parser.set_defaults(func=cmd_detect)
    
    # plan subcommand
    plan_parser = subparsers.add_parser("plan", help="Generate optimization plan")
    plan_parser.add_argument("--mode", choices=["safe", "aggressive"], default="safe", help="Optimization mode")
    plan_parser.add_argument("--output", "-o", help="Output file (YAML or JSON)")
    plan_parser.set_defaults(func=cmd_plan)
    
    # apply subcommand
    apply_parser = subparsers.add_parser("apply", help="Apply optimization plan")
    apply_parser.add_argument("--dry-run", action="store_true", default=True, help="Simulate only (default)")
    apply_parser.add_argument("--no-dry-run", dest="dry_run", action="store_false", help="Actually apply changes")
    apply_parser.add_argument("--destructive", action="store_true", help="Allow destructive actions")
    apply_parser.add_argument("--yes", "-y", action="store_true", help="Skip confirmation")
    apply_parser.add_argument("--json", action="store_true", help="Output as JSON")
    apply_parser.set_defaults(func=cmd_apply)
    
    # status subcommand
    status_parser = subparsers.add_parser("status", help="Show GPU and plan status")
    status_parser.add_argument("--json", action="store_true", help="Output as JSON")
    status_parser.set_defaults(func=cmd_status)
    
    # start-daemon subcommand
    daemon_parser = subparsers.add_parser("start-daemon", help="Start continuous monitoring daemon")
    daemon_parser.add_argument("--interval", type=float, default=30.0, help="Monitoring interval in seconds")
    daemon_parser.add_argument("--dry-run", action="store_true", default=True, help="Simulate only (default)")
    daemon_parser.add_argument("--no-dry-run", dest="dry_run", action="store_false", help="Apply changes for real")
    daemon_parser.add_argument("--consent", action="store_true", help="Consent to destructive actions")
    daemon_parser.add_argument("--confirm", action="store_true", help="Skip confirmation prompt")
    daemon_parser.set_defaults(func=cmd_start_daemon)
    
    args = parser.parse_args()
    
    if args.verbose:
        global logger
        logger = setup_logging(verbose=True)
    
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
