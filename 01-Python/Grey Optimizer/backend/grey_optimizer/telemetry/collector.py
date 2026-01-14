"""
Telemetry Collector

Central telemetry collection that aggregates CPU, RAM, and Disk
metrics from various sources (/proc, cgroups, etc.)

Safety: All operations are read-only. No system state is modified.
"""

import asyncio
import logging
from datetime import datetime
from typing import Dict, Any, Optional, List
from dataclasses import dataclass, field
from collections import deque

from ..config import TelemetryConfig
from .cpu import CPUTelemetry
from .ram import RAMTelemetry
from .disk import DiskTelemetry

logger = logging.getLogger(__name__)


@dataclass
class MetricsSnapshot:
    """A point-in-time capture of all system metrics."""
    timestamp: datetime
    cpu: Dict[str, Any]
    ram: Dict[str, Any]
    disk: Dict[str, Any]
    processes: List[Dict[str, Any]] = field(default_factory=list)


class TelemetryCollector:
    """
    Aggregates telemetry from all subsystems.
    
    Principle: "Measure twice, cut once" - We collect comprehensive
    metrics to make informed enforcement decisions.
    """
    
    def __init__(self, config: TelemetryConfig):
        """
        Initialize the collector with configuration.
        
        Args:
            config: Telemetry configuration settings
        """
        self.config = config
        
        # Initialize subsystem collectors
        self.cpu = CPUTelemetry()
        self.ram = RAMTelemetry()
        self.disk = DiskTelemetry()
        
        # Metrics history for baseline calculation
        self._history: deque = deque(maxlen=int(
            config.history_retention / config.sample_interval
        ))
        
        # Cached baseline
        self._baseline: Optional[Dict[str, Any]] = None
        
        # Lock for thread-safe operations
        self._lock = asyncio.Lock()
        
        logger.info("Telemetry collector initialized")
    
    async def collect(self) -> MetricsSnapshot:
        """
        Collect current metrics from all subsystems.
        
        Returns:
            MetricsSnapshot with current system state
        """
        async with self._lock:
            try:
                # Collect from all subsystems concurrently
                cpu_task = asyncio.create_task(self.cpu.collect())
                ram_task = asyncio.create_task(self.ram.collect())
                disk_task = asyncio.create_task(self.disk.collect())
                
                cpu_metrics, ram_metrics, disk_metrics = await asyncio.gather(
                    cpu_task, ram_task, disk_task
                )
                
                # Collect process-level metrics if enabled
                processes = []
                if self.config.track_processes:
                    processes = await self._collect_process_metrics()
                
                # Create snapshot
                snapshot = MetricsSnapshot(
                    timestamp=datetime.utcnow(),
                    cpu=cpu_metrics,
                    ram=ram_metrics,
                    disk=disk_metrics,
                    processes=processes[:self.config.max_tracked_processes]
                )
                
                # Add to history
                self._history.append(snapshot)
                
                return snapshot
                
            except Exception as e:
                logger.error(f"Collection error: {e}")
                raise
    
    async def get_current(self) -> Dict[str, Any]:
        """
        Get the most recent metrics as a dictionary.
        
        Returns:
            Dictionary with current CPU, RAM, and Disk metrics
        """
        if not self._history:
            snapshot = await self.collect()
        else:
            snapshot = self._history[-1]
        
        return {
            "timestamp": snapshot.timestamp.isoformat(),
            "cpu": snapshot.cpu,
            "ram": snapshot.ram,
            "disk": snapshot.disk,
            "process_count": len(snapshot.processes)
        }
    
    async def get_baseline(self) -> Dict[str, Any]:
        """
        Calculate baseline metrics from warmup period.
        
        The baseline represents the system's "normal" state before
        any optimization is applied. We use the average of metrics
        collected during the warmup period.
        
        Returns:
            Dictionary with averaged baseline metrics
        """
        if not self._history:
            raise ValueError("No metrics collected yet")
        
        # Calculate averages across all snapshots
        snapshots = list(self._history)
        
        cpu_usage_sum = sum(s.cpu.get("usage_percent", 0) for s in snapshots)
        ram_used_sum = sum(s.ram.get("used_mb", 0) for s in snapshots)
        disk_read_sum = sum(s.disk.get("read_mbs", 0) for s in snapshots)
        disk_write_sum = sum(s.disk.get("write_mbs", 0) for s in snapshots)
        
        count = len(snapshots)
        
        self._baseline = {
            "timestamp": datetime.utcnow().isoformat(),
            "sample_count": count,
            "cpu": {
                "usage_percent": cpu_usage_sum / count if count > 0 else 0,
                "core_count": snapshots[-1].cpu.get("core_count", 1) if snapshots else 1
            },
            "ram": {
                "used_mb": ram_used_sum / count if count > 0 else 0,
                "total_mb": snapshots[-1].ram.get("total_mb", 0) if snapshots else 0
            },
            "disk": {
                "read_mbs": disk_read_sum / count if count > 0 else 0,
                "write_mbs": disk_write_sum / count if count > 0 else 0
            }
        }
        
        logger.info(f"Baseline calculated from {count} samples")
        return self._baseline
    
    async def get_meminfo(self) -> Dict[str, Any]:
        """
        Get current memory info for watchdog checks.
        
        Returns:
            Dictionary with memory statistics
        """
        return await self.ram.collect()
    
    async def _collect_process_metrics(self) -> List[Dict[str, Any]]:
        """
        Collect per-process metrics for top consumers.
        
        Returns:
            List of process metrics sorted by resource usage
        """
        import os
        
        processes = []
        
        try:
            for pid in os.listdir("/proc"):
                if not pid.isdigit():
                    continue
                
                try:
                    pid_int = int(pid)
                    
                    # Read process name
                    with open(f"/proc/{pid}/comm", "r") as f:
                        comm = f.read().strip()
                    
                    # Read process status for memory
                    rss_kb = 0
                    try:
                        with open(f"/proc/{pid}/status", "r") as f:
                            for line in f:
                                if line.startswith("VmRSS:"):
                                    rss_kb = int(line.split()[1])
                                    break
                    except (FileNotFoundError, PermissionError):
                        pass
                    
                    # Read CPU stats
                    cpu_time = 0
                    try:
                        with open(f"/proc/{pid}/stat", "r") as f:
                            parts = f.read().split()
                            if len(parts) > 14:
                                utime = int(parts[13])
                                stime = int(parts[14])
                                cpu_time = utime + stime
                    except (FileNotFoundError, PermissionError):
                        pass
                    
                    processes.append({
                        "pid": pid_int,
                        "name": comm,
                        "rss_mb": rss_kb / 1024,
                        "cpu_time": cpu_time
                    })
                    
                except (FileNotFoundError, PermissionError, ValueError):
                    continue
            
            # Sort by RSS (memory) descending
            processes.sort(key=lambda p: p["rss_mb"], reverse=True)
            
        except Exception as e:
            logger.debug(f"Process collection error: {e}")
        
        return processes
    
    def get_history(self, seconds: int = 60) -> List[Dict[str, Any]]:
        """
        Get historical metrics for the specified duration.
        
        Args:
            seconds: How many seconds of history to return
            
        Returns:
            List of metric snapshots
        """
        # Calculate how many samples to return
        samples = int(seconds / self.config.sample_interval)
        
        history = list(self._history)[-samples:]
        
        return [
            {
                "timestamp": s.timestamp.isoformat(),
                "cpu_percent": s.cpu.get("usage_percent", 0),
                "ram_mb": s.ram.get("used_mb", 0),
                "disk_read_mbs": s.disk.get("read_mbs", 0),
                "disk_write_mbs": s.disk.get("write_mbs", 0)
            }
            for s in history
        ]
