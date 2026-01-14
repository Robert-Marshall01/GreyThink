#!/usr/bin/env python3
"""
grey_gpu_optimizer/daemon.py - GPU Optimizer Daemon

This module provides a daemon that continuously monitors GPU resources,
applies optimization plans, and emits structured JSON logs for telemetry.

Features:
- Loads GPU spec at startup (or triggers detection)
- Monitors VRAM usage, temperature, and GPU processes
- Applies optimization plans in dry_run=True mode by default
- Emits structured JSON logs for external consumption
- Graceful shutdown on SIGTERM/SIGINT

Usage:
    from grey_gpu_optimizer.daemon import GPUOptimizerDaemon
    daemon = GPUOptimizerDaemon(interval_s=30)
    daemon.run()  # Blocks until shutdown
"""

from __future__ import annotations

import json
import logging
import os
import signal
import time
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from threading import Event, Thread
from typing import Any, Callable

from grey_gpu_optimizer.optimizer import (
    GPUSpec,
    OptimizationPlan,
    ApplyResult,
    detect_gpus,
    plan_optimizations,
    apply_plan,
    load_spec,
    load_plan,
    CONFIG_DIR,
    ARTIFACTS_DIR,
)
from grey_gpu_optimizer.logging_config import setup_logging, get_json_logger

# Optional psutil for resource monitoring
try:
    import psutil
    HAS_PSUTIL = True
except ImportError:
    psutil = None  # type: ignore
    HAS_PSUTIL = False

logger = logging.getLogger("grey_gpu_optimizer.daemon")


# =============================================================================
# Data Classes
# =============================================================================

@dataclass
class DaemonStatus:
    """Current status of the daemon."""
    running: bool = False
    start_time: str = ""
    last_check: str = ""
    check_count: int = 0
    gpu_vendor: str = "unknown"
    gpu_model: str = "unknown"
    vram_total_mb: int = 0
    vram_free_mb: int = 0
    current_temp_c: float = 0.0
    plan_mode: str = ""
    plan_confidence: float = 0.0
    last_actions: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)
    
    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return asdict(self)


@dataclass
class MonitorSample:
    """Single sample from monitoring."""
    timestamp: str = ""
    vram_used_mb: int = 0
    vram_free_mb: int = 0
    temp_c: float = 0.0
    gpu_util_pct: float = 0.0
    mem_util_pct: float = 0.0
    process_count: int = 0
    
    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return asdict(self)


# =============================================================================
# GPU Monitoring Utilities
# =============================================================================

def _get_nvidia_metrics() -> dict[str, Any]:
    """Get current metrics from nvidia-smi."""
    import subprocess
    import shutil
    
    if not shutil.which("nvidia-smi"):
        return {}
    
    try:
        result = subprocess.run(
            ["nvidia-smi", "--query-gpu=memory.used,memory.free,temperature.gpu,"
             "utilization.gpu,utilization.memory",
             "--format=csv,noheader,nounits"],
            capture_output=True,
            text=True,
            timeout=5
        )
        if result.returncode == 0:
            parts = result.stdout.strip().split(",")
            if len(parts) >= 5:
                return {
                    "vram_used_mb": int(parts[0].strip()),
                    "vram_free_mb": int(parts[1].strip()),
                    "temp_c": float(parts[2].strip()),
                    "gpu_util_pct": float(parts[3].strip()),
                    "mem_util_pct": float(parts[4].strip()),
                }
    except Exception:
        pass
    
    return {}


def _get_rocm_metrics() -> dict[str, Any]:
    """Get current metrics from rocm-smi."""
    import subprocess
    import shutil
    
    if not shutil.which("rocm-smi"):
        return {}
    
    try:
        # Get temperature
        result = subprocess.run(
            ["rocm-smi", "--showtemp"],
            capture_output=True,
            text=True,
            timeout=5
        )
        metrics: dict[str, Any] = {}
        if result.returncode == 0:
            import re
            temp_match = re.search(r"(\d+\.?\d*)c", result.stdout, re.IGNORECASE)
            if temp_match:
                metrics["temp_c"] = float(temp_match.group(1))
        
        # Get memory
        result = subprocess.run(
            ["rocm-smi", "--showmeminfo", "vram"],
            capture_output=True,
            text=True,
            timeout=5
        )
        if result.returncode == 0:
            import re
            total_match = re.search(r"Total Memory \(B\):\s*(\d+)", result.stdout)
            used_match = re.search(r"Total Used Memory \(B\):\s*(\d+)", result.stdout)
            if total_match and used_match:
                total_mb = int(total_match.group(1)) // (1024 * 1024)
                used_mb = int(used_match.group(1)) // (1024 * 1024)
                metrics["vram_used_mb"] = used_mb
                metrics["vram_free_mb"] = total_mb - used_mb
        
        return metrics
    except Exception:
        pass
    
    return {}


def _get_intel_metrics() -> dict[str, Any]:
    """Get current metrics from Intel tools."""
    # Intel GPU monitoring is limited; return empty for now
    # TODO: Implement intel_gpu_top parsing if available
    return {}


def _count_gpu_processes() -> int:
    """Count processes using GPU."""
    if not HAS_PSUTIL:
        return 0
    
    count = 0
    gpu_keywords = ["cuda", "gpu", "nvidia", "rocm", "intel_gpu"]
    
    try:
        for proc in psutil.process_iter(["name", "cmdline"]):
            try:
                name = (proc.info.get("name") or "").lower()
                cmdline = " ".join(proc.info.get("cmdline") or []).lower()
                if any(kw in name or kw in cmdline for kw in gpu_keywords):
                    count += 1
            except (psutil.NoSuchProcess, psutil.AccessDenied):
                pass
    except Exception:
        pass
    
    return count


def sample_gpu_metrics(spec: GPUSpec | None = None) -> MonitorSample:
    """
    Sample current GPU metrics.
    
    Args:
        spec: Optional GPUSpec to determine which monitoring tools to use
    
    Returns:
        MonitorSample with current metrics
    """
    sample = MonitorSample(
        timestamp=datetime.now(timezone.utc).isoformat()
    )
    
    vendor = spec.vendor if spec else "unknown"
    
    # Try vendor-specific metrics
    metrics: dict[str, Any] = {}
    if vendor == "nvidia" or not metrics:
        metrics = _get_nvidia_metrics()
    if not metrics and vendor == "amd":
        metrics = _get_rocm_metrics()
    if not metrics and vendor == "intel":
        metrics = _get_intel_metrics()
    
    if metrics:
        sample.vram_used_mb = metrics.get("vram_used_mb", 0)
        sample.vram_free_mb = metrics.get("vram_free_mb", 0)
        sample.temp_c = metrics.get("temp_c", 0.0)
        sample.gpu_util_pct = metrics.get("gpu_util_pct", 0.0)
        sample.mem_util_pct = metrics.get("mem_util_pct", 0.0)
    
    sample.process_count = _count_gpu_processes()
    
    return sample


# =============================================================================
# Daemon Class
# =============================================================================

class GPUOptimizerDaemon:
    """
    GPU Optimizer Daemon for continuous monitoring and optimization.
    
    The daemon runs in a loop, sampling GPU metrics and applying
    optimization plans as needed. By default, all actions are dry-run
    to ensure safety.
    
    Attributes:
        interval_s: Seconds between monitoring cycles
        dry_run: If True, simulate actions only
        verbose: Enable verbose logging
    
    Example:
        >>> daemon = GPUOptimizerDaemon(interval_s=30)
        >>> daemon.start()  # Start in background thread
        >>> # ... do other work ...
        >>> daemon.stop()
    
        >>> # Or run in foreground (blocking)
        >>> daemon.run()
    """
    
    def __init__(
        self,
        interval_s: int = 30,
        dry_run: bool = True,
        verbose: bool = False,
        consent: bool = False,
        confirm: bool = False,
    ):
        """
        Initialize the daemon.
        
        Args:
            interval_s: Monitoring interval in seconds
            dry_run: If True, simulate all actions
            verbose: Enable verbose logging
            consent: Explicit consent for destructive actions
            confirm: Confirmation for destructive actions
        """
        self.interval_s = interval_s
        self.dry_run = dry_run
        self.verbose = verbose
        self.consent = consent
        self.confirm = confirm
        
        self._shutdown_event = Event()
        self._thread: Thread | None = None
        self._status = DaemonStatus()
        self._spec: GPUSpec | None = None
        self._plan: OptimizationPlan | None = None
        self._json_logger = get_json_logger()
        
        # History for trend analysis
        self._samples: list[MonitorSample] = []
        self._max_samples = 100
    
    @property
    def status(self) -> DaemonStatus:
        """Get current daemon status."""
        return self._status
    
    def _log_event(self, event_type: str, data: dict[str, Any]) -> None:
        """Log a structured JSON event."""
        event = {
            "timestamp": datetime.now(timezone.utc).isoformat(),
            "event_type": event_type,
            "daemon_id": f"grey_gpu_{os.getpid()}",
            **data
        }
        self._json_logger.info(json.dumps(event))
    
    def _load_or_detect_spec(self) -> GPUSpec | None:
        """Load cached spec or trigger fresh detection."""
        specs = load_spec()
        if specs and len(specs) > 0:
            self._log_event("spec_loaded", {"source": "cache"})
            return specs[0]
        
        logger.info("No cached spec found, running detection...")
        specs = detect_gpus()
        if specs and len(specs) > 0:
            self._log_event("spec_loaded", {"source": "detection"})
            return specs[0]
        
        return None
    
    def _load_or_generate_plan(self, spec: GPUSpec) -> OptimizationPlan | None:
        """Load cached plan or generate new one."""
        plan = load_plan()
        if plan:
            self._log_event("plan_loaded", {"source": "cache", "mode": plan.mode})
            return plan
        
        logger.info("No cached plan found, generating safe plan...")
        plan = plan_optimizations(spec, mode="safe")
        self._log_event("plan_loaded", {"source": "generated", "mode": plan.mode})
        return plan
    
    def _check_thermal_threshold(self, sample: MonitorSample) -> bool:
        """
        Check if we need to apply thermal throttling.
        
        Returns True if we're above the cooldown threshold.
        """
        if not self._plan:
            return False
        
        if sample.temp_c >= self._plan.cooldown_threshold_c:
            logger.warning(
                f"Thermal threshold reached: {sample.temp_c}°C >= "
                f"{self._plan.cooldown_threshold_c}°C"
            )
            return True
        
        return False
    
    def _check_vram_threshold(self, sample: MonitorSample) -> bool:
        """
        Check if VRAM usage exceeds plan cap.
        
        Returns True if we should intervene.
        """
        if not self._plan or not self._spec:
            return False
        
        if self._spec.vram_total_mb == 0:
            return False
        
        vram_used = self._spec.vram_total_mb - sample.vram_free_mb
        
        if vram_used >= self._plan.per_process_vram_cap_mb:
            logger.warning(
                f"VRAM threshold reached: {vram_used}MB >= "
                f"{self._plan.per_process_vram_cap_mb}MB cap"
            )
            return True
        
        return False
    
    def _monitoring_loop(self) -> None:
        """Main monitoring loop."""
        self._status.start_time = datetime.now(timezone.utc).isoformat()
        self._status.running = True
        
        # Initial setup
        self._spec = self._load_or_detect_spec()
        if self._spec:
            self._status.gpu_vendor = self._spec.vendor
            self._status.gpu_model = self._spec.model
            self._status.vram_total_mb = self._spec.vram_total_mb
            self._plan = self._load_or_generate_plan(self._spec)
            if self._plan:
                self._status.plan_mode = self._plan.mode
                self._status.plan_confidence = self._plan.confidence
        
        self._log_event("daemon_started", {
            "interval_s": self.interval_s,
            "dry_run": self.dry_run,
            "gpu_vendor": self._status.gpu_vendor,
            "gpu_model": self._status.gpu_model,
        })
        
        while not self._shutdown_event.is_set():
            try:
                self._monitoring_cycle()
            except Exception as e:
                logger.exception(f"Error in monitoring cycle: {e}")
                self._status.errors.append(str(e))
                self._log_event("error", {"message": str(e)})
            
            # Wait for next cycle or shutdown
            self._shutdown_event.wait(self.interval_s)
        
        self._status.running = False
        self._log_event("daemon_stopped", {
            "check_count": self._status.check_count,
            "errors_count": len(self._status.errors),
        })
    
    def _monitoring_cycle(self) -> None:
        """Single monitoring cycle."""
        self._status.check_count += 1
        self._status.last_check = datetime.now(timezone.utc).isoformat()
        
        # Sample metrics
        sample = sample_gpu_metrics(self._spec)
        self._samples.append(sample)
        if len(self._samples) > self._max_samples:
            self._samples = self._samples[-self._max_samples:]
        
        self._status.vram_free_mb = sample.vram_free_mb
        self._status.current_temp_c = sample.temp_c
        
        self._log_event("sample", sample.to_dict())
        
        # Check thresholds
        thermal_breach = self._check_thermal_threshold(sample)
        vram_breach = self._check_vram_threshold(sample)
        
        if thermal_breach or vram_breach:
            if self._plan:
                # Apply plan (dry-run by default)
                result = apply_plan(
                    self._plan,
                    dry_run=self.dry_run,
                    consent=self.consent,
                    confirm=self.confirm,
                    verbose=self.verbose
                )
                
                self._status.last_actions = result.actions_applied[-5:]
                
                self._log_event("plan_applied", {
                    "dry_run": result.dry_run,
                    "success": result.success,
                    "actions_count": len(result.actions_applied),
                    "thermal_breach": thermal_breach,
                    "vram_breach": vram_breach,
                })
                
                if not result.success:
                    for error in result.errors:
                        self._status.errors.append(error)
    
    def start(self) -> None:
        """Start the daemon in a background thread."""
        if self._thread and self._thread.is_alive():
            logger.warning("Daemon is already running")
            return
        
        self._shutdown_event.clear()
        self._thread = Thread(target=self._monitoring_loop, daemon=True)
        self._thread.start()
        logger.info("Daemon started in background")
    
    def stop(self, timeout: float = 10.0) -> None:
        """Stop the daemon gracefully."""
        if not self._thread:
            return
        
        logger.info("Stopping daemon...")
        self._shutdown_event.set()
        self._thread.join(timeout=timeout)
        
        if self._thread.is_alive():
            logger.warning("Daemon did not stop within timeout")
        else:
            logger.info("Daemon stopped")
        
        self._thread = None
    
    def run(self) -> None:
        """
        Run the daemon in the foreground (blocking).
        
        Use start() and stop() for background operation.
        """
        # Set up signal handlers
        def handle_signal(signum: int, frame: Any) -> None:
            logger.info(f"Received signal {signum}, shutting down...")
            self._shutdown_event.set()
        
        signal.signal(signal.SIGTERM, handle_signal)
        signal.signal(signal.SIGINT, handle_signal)
        
        logger.info(f"Starting daemon in foreground (interval={self.interval_s}s)")
        self._monitoring_loop()
    
    def get_samples(self, last_n: int = 10) -> list[MonitorSample]:
        """Get recent monitoring samples."""
        return self._samples[-last_n:]
    
    def write_status_file(self) -> Path:
        """Write current status to a file for external consumption."""
        status_file = CONFIG_DIR / "daemon_status.json"
        CONFIG_DIR.mkdir(parents=True, exist_ok=True)
        
        with open(status_file, "w") as f:
            json.dump(self._status.to_dict(), f, indent=2)
        
        return status_file


# =============================================================================
# Module-level Convenience Functions
# =============================================================================

_daemon_instance: GPUOptimizerDaemon | None = None


def get_daemon() -> GPUOptimizerDaemon:
    """Get the global daemon instance."""
    global _daemon_instance
    if _daemon_instance is None:
        _daemon_instance = GPUOptimizerDaemon()
    return _daemon_instance


def start_daemon(
    interval_s: int = 30,
    dry_run: bool = True,
    verbose: bool = False
) -> GPUOptimizerDaemon:
    """
    Start the global daemon instance.
    
    Args:
        interval_s: Monitoring interval in seconds
        dry_run: If True, simulate actions only
        verbose: Enable verbose logging
    
    Returns:
        The daemon instance
    """
    global _daemon_instance
    
    if _daemon_instance and _daemon_instance.status.running:
        logger.warning("Daemon is already running")
        return _daemon_instance
    
    _daemon_instance = GPUOptimizerDaemon(
        interval_s=interval_s,
        dry_run=dry_run,
        verbose=verbose
    )
    _daemon_instance.start()
    return _daemon_instance


def stop_daemon(timeout: float = 10.0) -> None:
    """Stop the global daemon instance."""
    global _daemon_instance
    if _daemon_instance:
        _daemon_instance.stop(timeout=timeout)


def daemon_status() -> DaemonStatus:
    """Get the status of the global daemon instance."""
    if _daemon_instance:
        return _daemon_instance.status
    return DaemonStatus(running=False)
