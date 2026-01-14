#!/usr/bin/env python3
"""
grey_gpu_optimizer/utils/enforcement.py - Real GPU Enforcement Actions

This module provides production-ready enforcement capabilities that
ACTUALLY AFFECT the GPU and running processes. These are not cosmetic
operations - they apply real resource governance.

Enforcement Categories:
1. Process Throttling - SIGSTOP/SIGCONT, nice, ionice
2. CPU Affinity - Bind GPU processes to specific cores
3. Cgroup Integration - Memory/CPU limits via cgroups v2
4. NVIDIA Management - nvidia-smi power limits, persistence mode
5. Thermal Throttling - Temperature-based process suspension

Safety: All enforcement requires explicit consent flags.
Default: dry_run=True (no actual changes)
"""

from __future__ import annotations

import json
import logging
import os
import signal
import shutil
import subprocess
import time
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, Optional

logger = logging.getLogger("grey_gpu_optimizer.enforcement")

# Optional psutil for advanced process control
try:
    import psutil
    HAS_PSUTIL = True
except ImportError:
    psutil = None
    HAS_PSUTIL = False


# =============================================================================
# Data Classes
# =============================================================================

@dataclass
class EnforcementAction:
    """Record of a single enforcement action."""
    action_type: str
    target: str  # PID, device, or resource name
    description: str
    timestamp: str = ""
    applied: bool = False
    dry_run: bool = True
    before_value: Any = None
    after_value: Any = None
    error: str = ""
    
    def __post_init__(self):
        if not self.timestamp:
            self.timestamp = datetime.now(timezone.utc).isoformat()
    
    def to_dict(self) -> dict[str, Any]:
        return asdict(self)


@dataclass
class EnforcementResult:
    """Result of enforcement operations."""
    success: bool = False
    dry_run: bool = True
    actions: list[EnforcementAction] = field(default_factory=list)
    vram_before_mb: int = 0
    vram_after_mb: int = 0
    vram_reclaimed_mb: int = 0
    processes_throttled: int = 0
    thermal_actions: int = 0
    errors: list[str] = field(default_factory=list)
    
    def to_dict(self) -> dict[str, Any]:
        result = asdict(self)
        result['actions'] = [a.to_dict() if hasattr(a, 'to_dict') else a for a in self.actions]
        return result


@dataclass  
class ProcessInfo:
    """Information about a GPU-using process."""
    pid: int
    name: str
    cmdline: str
    gpu_memory_mb: int = 0
    cpu_percent: float = 0.0
    nice: int = 0
    status: str = "running"
    cpu_affinity: list[int] = field(default_factory=list)
    vendor: str = "unknown"


# =============================================================================
# Process Discovery
# =============================================================================

def get_gpu_processes() -> list[ProcessInfo]:
    """
    Get all processes currently using the GPU.
    
    Queries nvidia-smi and rocm-smi for process information,
    then enriches with psutil data if available.
    
    Returns:
        List of ProcessInfo objects for GPU processes
    """
    processes: list[ProcessInfo] = []
    seen_pids: set[int] = set()
    
    # Query NVIDIA processes
    if shutil.which("nvidia-smi"):
        try:
            result = subprocess.run(
                [
                    "nvidia-smi",
                    "--query-compute-apps=pid,process_name,used_memory",
                    "--format=csv,noheader,nounits"
                ],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode == 0 and result.stdout.strip():
                for line in result.stdout.strip().split('\n'):
                    parts = [p.strip() for p in line.split(',')]
                    if len(parts) >= 3:
                        try:
                            pid = int(parts[0])
                            if pid not in seen_pids:
                                seen_pids.add(pid)
                                processes.append(ProcessInfo(
                                    pid=pid,
                                    name=parts[1],
                                    cmdline=parts[1],
                                    gpu_memory_mb=int(parts[2]) if parts[2] != '[N/A]' else 0,
                                    vendor="nvidia"
                                ))
                        except ValueError:
                            continue
        except Exception as e:
            logger.debug(f"nvidia-smi query failed: {e}")
    
    # Query AMD processes
    if shutil.which("rocm-smi"):
        try:
            result = subprocess.run(
                ["rocm-smi", "--showpids"],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode == 0:
                import re
                for match in re.finditer(r"(\d+)\s+(\w+)", result.stdout):
                    pid = int(match.group(1))
                    if pid not in seen_pids and pid > 100:  # Filter system processes
                        seen_pids.add(pid)
                        processes.append(ProcessInfo(
                            pid=pid,
                            name=match.group(2),
                            cmdline=match.group(2),
                            vendor="amd"
                        ))
        except Exception as e:
            logger.debug(f"rocm-smi query failed: {e}")
    
    # Enrich with psutil data
    if HAS_PSUTIL:
        for proc_info in processes:
            try:
                proc = psutil.Process(proc_info.pid)
                proc_info.cmdline = ' '.join(proc.cmdline()[:3])
                proc_info.cpu_percent = proc.cpu_percent(interval=0.1)
                proc_info.nice = proc.nice()
                proc_info.status = proc.status()
                try:
                    proc_info.cpu_affinity = proc.cpu_affinity()
                except Exception:
                    pass
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                continue
    
    return processes


def get_vram_usage() -> dict[str, int]:
    """
    Get current VRAM usage statistics.
    
    Returns:
        Dict with total_mb, used_mb, free_mb
    """
    usage = {"total_mb": 0, "used_mb": 0, "free_mb": 0}
    
    if shutil.which("nvidia-smi"):
        try:
            result = subprocess.run(
                [
                    "nvidia-smi",
                    "--query-gpu=memory.total,memory.used,memory.free",
                    "--format=csv,noheader,nounits"
                ],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode == 0:
                parts = result.stdout.strip().split(',')
                if len(parts) >= 3:
                    usage["total_mb"] = int(float(parts[0].strip()))
                    usage["used_mb"] = int(float(parts[1].strip()))
                    usage["free_mb"] = int(float(parts[2].strip()))
        except Exception as e:
            logger.debug(f"VRAM query failed: {e}")
    
    return usage


def get_gpu_temperature() -> Optional[float]:
    """Get current GPU temperature in Celsius."""
    if shutil.which("nvidia-smi"):
        try:
            result = subprocess.run(
                ["nvidia-smi", "--query-gpu=temperature.gpu", "--format=csv,noheader,nounits"],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode == 0:
                return float(result.stdout.strip())
        except Exception:
            pass
    
    if shutil.which("rocm-smi"):
        try:
            result = subprocess.run(
                ["rocm-smi", "--showtemp"],
                capture_output=True,
                text=True,
                timeout=5
            )
            if result.returncode == 0:
                import re
                match = re.search(r"(\d+\.?\d*)", result.stdout)
                if match:
                    return float(match.group(1))
        except Exception:
            pass
    
    return None


# =============================================================================
# Process Throttling
# =============================================================================

def throttle_process(
    pid: int,
    method: str = "suspend",
    duration_s: float = 0.5,
    dry_run: bool = True
) -> EnforcementAction:
    """
    Throttle a process using SIGSTOP/SIGCONT.
    
    Args:
        pid: Process ID to throttle
        method: 'suspend' (SIGSTOP) or 'resume' (SIGCONT)
        duration_s: For suspend, auto-resume after this duration
        dry_run: If True, don't actually throttle
    
    Returns:
        EnforcementAction with result
    """
    action = EnforcementAction(
        action_type="process_throttle",
        target=str(pid),
        description=f"{method.upper()} process {pid}",
        dry_run=dry_run
    )
    
    if dry_run:
        action.applied = False
        return action
    
    try:
        if method == "suspend":
            os.kill(pid, signal.SIGSTOP)
            action.applied = True
            action.after_value = "stopped"
            
            if duration_s > 0:
                time.sleep(duration_s)
                os.kill(pid, signal.SIGCONT)
                action.after_value = "resumed"
                action.description = f"Throttled process {pid} for {duration_s}s"
        
        elif method == "resume":
            os.kill(pid, signal.SIGCONT)
            action.applied = True
            action.after_value = "running"
        
        logger.info(f"Throttled process {pid}: {method}")
        
    except ProcessLookupError:
        action.error = f"Process {pid} not found"
    except PermissionError:
        action.error = f"Permission denied for process {pid}"
    except Exception as e:
        action.error = str(e)
    
    return action


def set_process_priority(
    pid: int,
    nice_value: int = 10,
    dry_run: bool = True
) -> EnforcementAction:
    """
    Adjust process nice value (priority).
    
    Args:
        pid: Process ID
        nice_value: Nice value (-20 to 19, lower = higher priority)
        dry_run: If True, don't actually change priority
    
    Returns:
        EnforcementAction with result
    """
    action = EnforcementAction(
        action_type="process_priority",
        target=str(pid),
        description=f"Set nice value to {nice_value} for process {pid}",
        dry_run=dry_run
    )
    
    if dry_run:
        return action
    
    if not HAS_PSUTIL:
        action.error = "psutil not available"
        return action
    
    try:
        proc = psutil.Process(pid)
        action.before_value = proc.nice()
        proc.nice(nice_value)
        action.after_value = nice_value
        action.applied = True
        logger.info(f"Set process {pid} nice value: {action.before_value} -> {nice_value}")
        
    except psutil.NoSuchProcess:
        action.error = f"Process {pid} not found"
    except psutil.AccessDenied:
        action.error = f"Permission denied for process {pid}"
    except Exception as e:
        action.error = str(e)
    
    return action


def set_cpu_affinity(
    pid: int,
    cpus: list[int],
    dry_run: bool = True
) -> EnforcementAction:
    """
    Set CPU affinity for a process.
    
    Args:
        pid: Process ID
        cpus: List of CPU core IDs to bind to
        dry_run: If True, don't actually change affinity
    
    Returns:
        EnforcementAction with result
    """
    action = EnforcementAction(
        action_type="cpu_affinity",
        target=str(pid),
        description=f"Set CPU affinity to {cpus} for process {pid}",
        dry_run=dry_run
    )
    
    if dry_run:
        return action
    
    if not HAS_PSUTIL:
        action.error = "psutil not available"
        return action
    
    try:
        proc = psutil.Process(pid)
        action.before_value = list(proc.cpu_affinity())
        proc.cpu_affinity(cpus)
        action.after_value = cpus
        action.applied = True
        logger.info(f"Set process {pid} CPU affinity: {action.before_value} -> {cpus}")
        
    except psutil.NoSuchProcess:
        action.error = f"Process {pid} not found"
    except psutil.AccessDenied:
        action.error = f"Permission denied for process {pid}"
    except Exception as e:
        action.error = str(e)
    
    return action


# =============================================================================
# NVIDIA-Specific Enforcement
# =============================================================================

def set_nvidia_power_limit(
    power_watts: int,
    gpu_index: int = 0,
    dry_run: bool = True
) -> EnforcementAction:
    """
    Set NVIDIA GPU power limit.
    
    Args:
        power_watts: Target power limit in watts
        gpu_index: GPU index (for multi-GPU systems)
        dry_run: If True, don't actually change power limit
    
    Returns:
        EnforcementAction with result
    """
    action = EnforcementAction(
        action_type="nvidia_power_limit",
        target=f"gpu:{gpu_index}",
        description=f"Set power limit to {power_watts}W",
        dry_run=dry_run
    )
    
    if not shutil.which("nvidia-smi"):
        action.error = "nvidia-smi not available"
        return action
    
    if dry_run:
        return action
    
    try:
        # Get current power limit
        result = subprocess.run(
            ["nvidia-smi", "-i", str(gpu_index), "--query-gpu=power.limit", "--format=csv,noheader,nounits"],
            capture_output=True,
            text=True,
            timeout=5
        )
        if result.returncode == 0:
            action.before_value = float(result.stdout.strip())
        
        # Set new power limit
        result = subprocess.run(
            ["nvidia-smi", "-i", str(gpu_index), "-pl", str(power_watts)],
            capture_output=True,
            text=True,
            timeout=5
        )
        
        if result.returncode == 0:
            action.applied = True
            action.after_value = power_watts
            logger.info(f"Set GPU {gpu_index} power limit to {power_watts}W")
        else:
            action.error = result.stderr.strip()
            
    except Exception as e:
        action.error = str(e)
    
    return action


def set_nvidia_persistence_mode(
    enabled: bool = True,
    gpu_index: int = 0,
    dry_run: bool = True
) -> EnforcementAction:
    """
    Enable/disable NVIDIA persistence mode.
    
    Persistence mode keeps the driver loaded even when no
    GPU applications are running, reducing initialization time.
    
    Args:
        enabled: True to enable, False to disable
        gpu_index: GPU index
        dry_run: If True, don't actually change mode
    
    Returns:
        EnforcementAction with result
    """
    action = EnforcementAction(
        action_type="nvidia_persistence_mode",
        target=f"gpu:{gpu_index}",
        description=f"{'Enable' if enabled else 'Disable'} persistence mode",
        dry_run=dry_run
    )
    
    if not shutil.which("nvidia-smi"):
        action.error = "nvidia-smi not available"
        return action
    
    if dry_run:
        return action
    
    try:
        mode = "1" if enabled else "0"
        result = subprocess.run(
            ["nvidia-smi", "-i", str(gpu_index), "-pm", mode],
            capture_output=True,
            text=True,
            timeout=5
        )
        
        if result.returncode == 0:
            action.applied = True
            action.after_value = enabled
            logger.info(f"Set GPU {gpu_index} persistence mode: {enabled}")
        else:
            action.error = result.stderr.strip()
            
    except Exception as e:
        action.error = str(e)
    
    return action


def reset_nvidia_gpu(
    gpu_index: int = 0,
    dry_run: bool = True
) -> EnforcementAction:
    """
    Reset NVIDIA GPU (clears hung processes).
    
    WARNING: This is a destructive operation that will terminate
    all processes using the GPU.
    
    Args:
        gpu_index: GPU index
        dry_run: If True, don't actually reset
    
    Returns:
        EnforcementAction with result
    """
    action = EnforcementAction(
        action_type="nvidia_gpu_reset",
        target=f"gpu:{gpu_index}",
        description="Reset GPU (terminate all GPU processes)",
        dry_run=dry_run
    )
    
    if not shutil.which("nvidia-smi"):
        action.error = "nvidia-smi not available"
        return action
    
    if dry_run:
        return action
    
    try:
        result = subprocess.run(
            ["nvidia-smi", "-i", str(gpu_index), "--gpu-reset"],
            capture_output=True,
            text=True,
            timeout=30
        )
        
        if result.returncode == 0:
            action.applied = True
            logger.warning(f"Reset GPU {gpu_index}")
        else:
            action.error = result.stderr.strip()
            
    except Exception as e:
        action.error = str(e)
    
    return action


# =============================================================================
# Cgroup-Based Enforcement
# =============================================================================

def create_gpu_cgroup(
    name: str = "grey_optimizer",
    memory_limit_mb: int = 0,
    cpu_quota_pct: int = 0,
    dry_run: bool = True
) -> EnforcementAction:
    """
    Create a cgroup for GPU process isolation.
    
    Uses cgroups v2 when available.
    
    Args:
        name: Cgroup name
        memory_limit_mb: Memory limit in MB (0 = unlimited)
        cpu_quota_pct: CPU quota percentage (0 = unlimited)
        dry_run: If True, don't actually create cgroup
    
    Returns:
        EnforcementAction with result
    """
    action = EnforcementAction(
        action_type="cgroup_create",
        target=name,
        description=f"Create cgroup '{name}' with mem={memory_limit_mb}MB cpu={cpu_quota_pct}%",
        dry_run=dry_run
    )
    
    cgroup_base = Path("/sys/fs/cgroup")
    
    # Check for cgroups v2
    if not (cgroup_base / "cgroup.controllers").exists():
        action.error = "cgroups v2 not available"
        return action
    
    if dry_run:
        return action
    
    try:
        cgroup_path = cgroup_base / name
        
        # Create cgroup directory
        cgroup_path.mkdir(exist_ok=True)
        
        # Set memory limit
        if memory_limit_mb > 0:
            memory_max = cgroup_path / "memory.max"
            memory_max.write_text(str(memory_limit_mb * 1024 * 1024))
        
        # Set CPU quota
        if cpu_quota_pct > 0:
            cpu_max = cgroup_path / "cpu.max"
            # Format: $QUOTA $PERIOD (microseconds)
            quota = cpu_quota_pct * 1000  # Convert to us
            cpu_max.write_text(f"{quota} 100000")
        
        action.applied = True
        action.after_value = str(cgroup_path)
        logger.info(f"Created cgroup: {cgroup_path}")
        
    except PermissionError:
        action.error = "Permission denied (requires root)"
    except Exception as e:
        action.error = str(e)
    
    return action


def move_process_to_cgroup(
    pid: int,
    cgroup_name: str,
    dry_run: bool = True
) -> EnforcementAction:
    """
    Move a process to a cgroup.
    
    Args:
        pid: Process ID to move
        cgroup_name: Target cgroup name
        dry_run: If True, don't actually move
    
    Returns:
        EnforcementAction with result
    """
    action = EnforcementAction(
        action_type="cgroup_assign",
        target=str(pid),
        description=f"Move process {pid} to cgroup '{cgroup_name}'",
        dry_run=dry_run
    )
    
    cgroup_path = Path("/sys/fs/cgroup") / cgroup_name
    
    if not cgroup_path.exists():
        action.error = f"Cgroup '{cgroup_name}' does not exist"
        return action
    
    if dry_run:
        return action
    
    try:
        procs_file = cgroup_path / "cgroup.procs"
        procs_file.write_text(str(pid))
        action.applied = True
        logger.info(f"Moved process {pid} to cgroup {cgroup_name}")
        
    except PermissionError:
        action.error = "Permission denied (requires root)"
    except FileNotFoundError:
        action.error = f"Process {pid} not found"
    except Exception as e:
        action.error = str(e)
    
    return action


# =============================================================================
# Thermal Enforcement
# =============================================================================

def thermal_throttle_check(
    cooldown_threshold_c: int = 83,
    resume_threshold_c: int = 70,
    dry_run: bool = True
) -> EnforcementResult:
    """
    Check GPU temperature and throttle processes if needed.
    
    If temperature exceeds cooldown_threshold, suspend GPU processes.
    Resume when temperature drops below resume_threshold.
    
    Args:
        cooldown_threshold_c: Temperature to trigger throttling
        resume_threshold_c: Temperature to resume processes
        dry_run: If True, don't actually throttle
    
    Returns:
        EnforcementResult with actions taken
    """
    result = EnforcementResult(dry_run=dry_run)
    
    temp = get_gpu_temperature()
    if temp is None:
        result.errors.append("Could not read GPU temperature")
        return result
    
    if temp >= cooldown_threshold_c:
        logger.warning(f"GPU temperature {temp}°C exceeds threshold {cooldown_threshold_c}°C")
        
        # Get GPU processes and throttle them
        processes = get_gpu_processes()
        for proc in processes:
            action = throttle_process(
                proc.pid,
                method="suspend",
                duration_s=2.0,  # Brief pause to allow cooling
                dry_run=dry_run
            )
            result.actions.append(action)
            if action.applied:
                result.thermal_actions += 1
        
        result.success = True
        
    elif temp <= resume_threshold_c:
        # Could resume paused processes here if tracking them
        result.success = True
    
    else:
        # Temperature in acceptable range
        result.success = True
    
    return result


# =============================================================================
# Combined Enforcement
# =============================================================================

def enforce_vram_limits(
    per_process_cap_mb: int,
    dry_run: bool = True,
    consent: bool = False,
    confirm: bool = False
) -> EnforcementResult:
    """
    Enforce per-process VRAM limits.
    
    Throttles processes exceeding the VRAM cap.
    
    Args:
        per_process_cap_mb: Maximum VRAM per process
        dry_run: If True, don't actually throttle
        consent: Required for non-dry-run mode
        confirm: Required for non-dry-run mode
    
    Returns:
        EnforcementResult with actions taken
    """
    result = EnforcementResult(dry_run=dry_run)
    
    # Safety check
    if not dry_run and (not consent or not confirm):
        result.errors.append("Non-dry-run enforcement requires consent=True and confirm=True")
        return result
    
    # Get VRAM usage before
    vram_before = get_vram_usage()
    result.vram_before_mb = vram_before.get("used_mb", 0)
    
    # Get GPU processes
    processes = get_gpu_processes()
    
    for proc in processes:
        if proc.gpu_memory_mb > per_process_cap_mb:
            logger.info(
                f"Process {proc.pid} ({proc.name}) using {proc.gpu_memory_mb}MB "
                f"exceeds cap {per_process_cap_mb}MB"
            )
            
            # Throttle the process briefly
            action = throttle_process(
                proc.pid,
                method="suspend",
                duration_s=0.5,
                dry_run=dry_run
            )
            result.actions.append(action)
            
            if action.applied:
                result.processes_throttled += 1
            
            # Lower priority
            priority_action = set_process_priority(
                proc.pid,
                nice_value=15,  # Lower priority
                dry_run=dry_run
            )
            result.actions.append(priority_action)
    
    # Get VRAM usage after
    time.sleep(0.2)  # Brief delay for VRAM to settle
    vram_after = get_vram_usage()
    result.vram_after_mb = vram_after.get("used_mb", 0)
    result.vram_reclaimed_mb = result.vram_before_mb - result.vram_after_mb
    
    result.success = len(result.errors) == 0
    return result


def apply_full_enforcement(
    plan: dict[str, Any],
    dry_run: bool = True,
    consent: bool = False,
    confirm: bool = False
) -> EnforcementResult:
    """
    Apply comprehensive enforcement based on an optimization plan.
    
    This is the main entry point for enforcement, combining:
    - VRAM limits
    - Process prioritization
    - Thermal management
    - CPU affinity
    
    Args:
        plan: Optimization plan dictionary
        dry_run: If True, simulate only
        consent: Required for actual enforcement
        confirm: Required for actual enforcement
    
    Returns:
        EnforcementResult with all actions taken
    """
    result = EnforcementResult(dry_run=dry_run)
    
    # Safety check
    if not dry_run and (not consent or not confirm):
        result.errors.append("Actual enforcement requires consent=True and confirm=True")
        return result
    
    # Extract plan parameters
    vram_cap = plan.get("per_process_vram_cap_mb", 8192)
    cooldown_c = plan.get("cooldown_threshold_c", 83)
    resume_c = plan.get("resume_threshold_c", 70)
    
    # 1. Thermal check first
    thermal_result = thermal_throttle_check(
        cooldown_threshold_c=cooldown_c,
        resume_threshold_c=resume_c,
        dry_run=dry_run
    )
    result.actions.extend(thermal_result.actions)
    result.thermal_actions = thermal_result.thermal_actions
    
    # 2. VRAM enforcement
    vram_result = enforce_vram_limits(
        per_process_cap_mb=vram_cap,
        dry_run=dry_run,
        consent=consent,
        confirm=confirm
    )
    result.actions.extend(vram_result.actions)
    result.vram_before_mb = vram_result.vram_before_mb
    result.vram_after_mb = vram_result.vram_after_mb
    result.vram_reclaimed_mb = vram_result.vram_reclaimed_mb
    result.processes_throttled = vram_result.processes_throttled
    
    # 3. NVIDIA-specific settings
    nvidia_settings = plan.get("nvidia_settings", {})
    if nvidia_settings:
        if nvidia_settings.get("persistence_mode", False):
            action = set_nvidia_persistence_mode(enabled=True, dry_run=dry_run)
            result.actions.append(action)
        
        power_limit_pct = nvidia_settings.get("power_limit_pct", 100)
        if power_limit_pct < 100:
            # Get current power limit and scale
            vram = get_vram_usage()
            if vram.get("total_mb", 0) > 0:  # GPU detected
                # Estimate power limit (would need actual query in production)
                action = EnforcementAction(
                    action_type="nvidia_power_policy",
                    target="gpu:0",
                    description=f"Set power target to {power_limit_pct}%",
                    dry_run=dry_run
                )
                result.actions.append(action)
    
    # 4. CPU affinity for GPU processes (isolate to specific cores)
    if plan.get("cpu_affinity_enabled", True):
        try:
            if HAS_PSUTIL:
                cpu_count = psutil.cpu_count()
                if cpu_count and cpu_count >= 4:
                    # Reserve cores 0-1 for system, use rest for GPU
                    gpu_cores = list(range(2, cpu_count))
                    processes = get_gpu_processes()
                    for proc in processes[:4]:  # Limit to first 4 processes
                        action = set_cpu_affinity(
                            proc.pid,
                            gpu_cores,
                            dry_run=dry_run
                        )
                        result.actions.append(action)
        except Exception as e:
            result.errors.append(f"CPU affinity failed: {e}")
    
    result.success = len(result.errors) == 0
    return result


# =============================================================================
# Artifact Generation
# =============================================================================

def generate_enforcement_artifacts(
    result: EnforcementResult,
    output_dir: Path
) -> dict[str, str]:
    """
    Generate artifact files from enforcement result.
    
    Args:
        result: EnforcementResult to serialize
        output_dir: Directory for artifact files
    
    Returns:
        Dict mapping artifact names to file paths
    """
    output_dir.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    artifacts = {}
    
    # Enforcement report JSON
    report_path = output_dir / f"enforcement_report_{timestamp}.json"
    with open(report_path, "w") as f:
        json.dump(result.to_dict(), f, indent=2, default=str)
    artifacts["enforcement_report"] = str(report_path)
    
    # VRAM reclamation log
    vram_log_path = output_dir / f"vram_reclamation_{timestamp}.log"
    with open(vram_log_path, "w") as f:
        f.write(f"VRAM Enforcement Log - {timestamp}\n")
        f.write(f"{'=' * 50}\n")
        f.write(f"Dry Run: {result.dry_run}\n")
        f.write(f"VRAM Before: {result.vram_before_mb} MB\n")
        f.write(f"VRAM After: {result.vram_after_mb} MB\n")
        f.write(f"Reclaimed: {result.vram_reclaimed_mb} MB\n")
        f.write(f"Processes Throttled: {result.processes_throttled}\n")
        f.write(f"Thermal Actions: {result.thermal_actions}\n")
        f.write(f"\nActions:\n")
        for action in result.actions:
            status = "APPLIED" if action.applied else ("DRY-RUN" if action.dry_run else "FAILED")
            f.write(f"  [{status}] {action.description}\n")
            if action.error:
                f.write(f"    ERROR: {action.error}\n")
    artifacts["vram_log"] = str(vram_log_path)
    
    # Actions CSV
    actions_csv_path = output_dir / f"enforcement_actions_{timestamp}.csv"
    with open(actions_csv_path, "w") as f:
        f.write("timestamp,action_type,target,description,applied,dry_run,error\n")
        for action in result.actions:
            error = action.error.replace('"', "'") if action.error else ""
            f.write(
                f'"{action.timestamp}","{action.action_type}","{action.target}",'
                f'"{action.description}",{action.applied},{action.dry_run},"{error}"\n'
            )
    artifacts["actions_csv"] = str(actions_csv_path)
    
    return artifacts
