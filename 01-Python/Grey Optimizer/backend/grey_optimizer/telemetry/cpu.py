"""
CPU Telemetry Collection

Collects CPU metrics from /proc/stat and cgroup cpu.stat.

Safety: Read-only operations. No system state modified.
"""

import asyncio
import logging
import os
from typing import Dict, Any, Optional

logger = logging.getLogger(__name__)


class CPUTelemetry:
    """
    Collects CPU usage metrics from the Linux kernel.
    
    Uses /proc/stat for system-wide CPU and per-cgroup cpu.stat
    when available. Calculates usage percentages between samples.
    """
    
    def __init__(self):
        """Initialize CPU telemetry collector."""
        # Previous values for delta calculation
        self._prev_total: int = 0
        self._prev_idle: int = 0
        self._prev_timestamp: float = 0
        
        # Detect number of CPU cores
        self._core_count = os.cpu_count() or 1
        
        logger.debug(f"CPU telemetry initialized ({self._core_count} cores)")
    
    async def collect(self) -> Dict[str, Any]:
        """
        Collect current CPU metrics.
        
        Reads /proc/stat to calculate overall CPU usage as a
        percentage. The first call returns 0 since we need two
        samples to calculate a delta.
        
        Returns:
            Dictionary with CPU metrics:
            - usage_percent: Overall CPU usage (0-100)
            - core_count: Number of CPU cores
            - per_core: List of per-core usage (if available)
        """
        try:
            # Read /proc/stat
            with open("/proc/stat", "r") as f:
                lines = f.readlines()
            
            # Parse the first line (aggregate CPU)
            cpu_line = lines[0]
            values = cpu_line.split()[1:]  # Skip "cpu" prefix
            
            # CPU time values (in jiffies, typically 1/100th of a second)
            # user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice
            user = int(values[0])
            nice = int(values[1])
            system = int(values[2])
            idle = int(values[3])
            iowait = int(values[4]) if len(values) > 4 else 0
            irq = int(values[5]) if len(values) > 5 else 0
            softirq = int(values[6]) if len(values) > 6 else 0
            steal = int(values[7]) if len(values) > 7 else 0
            
            # Calculate totals
            idle_time = idle + iowait
            non_idle = user + nice + system + irq + softirq + steal
            total = idle_time + non_idle
            
            # Calculate usage percentage
            usage_percent = 0.0
            if self._prev_total > 0:
                total_delta = total - self._prev_total
                idle_delta = idle_time - self._prev_idle
                
                if total_delta > 0:
                    usage_percent = ((total_delta - idle_delta) / total_delta) * 100
                    usage_percent = max(0.0, min(100.0, usage_percent))
            
            # Store for next calculation
            self._prev_total = total
            self._prev_idle = idle_time
            
            # Parse per-core usage
            per_core = await self._collect_per_core(lines[1:])
            
            # Get load average
            load_avg = await self._get_load_average()
            
            return {
                "usage_percent": round(usage_percent, 2),
                "core_count": self._core_count,
                "per_core": per_core,
                "load_average": load_avg,
                "user_percent": round((user / total) * 100, 2) if total > 0 else 0,
                "system_percent": round((system / total) * 100, 2) if total > 0 else 0,
                "iowait_percent": round((iowait / total) * 100, 2) if total > 0 else 0
            }
            
        except Exception as e:
            logger.error(f"CPU collection error: {e}")
            return {
                "usage_percent": 0.0,
                "core_count": self._core_count,
                "error": str(e)
            }
    
    async def _collect_per_core(self, lines: list) -> list:
        """
        Parse per-core CPU usage from /proc/stat lines.
        
        Returns:
            List of per-core usage percentages
        """
        per_core = []
        
        for line in lines:
            if not line.startswith("cpu"):
                break
                
            try:
                values = line.split()[1:]
                if len(values) < 4:
                    continue
                    
                user = int(values[0])
                nice = int(values[1])
                system = int(values[2])
                idle = int(values[3])
                
                total = user + nice + system + idle
                if total > 0:
                    usage = ((total - idle) / total) * 100
                    per_core.append(round(usage, 2))
                else:
                    per_core.append(0.0)
                    
            except (ValueError, IndexError):
                continue
        
        return per_core
    
    async def _get_load_average(self) -> Dict[str, float]:
        """
        Get system load averages from /proc/loadavg.
        
        Returns:
            Dictionary with 1, 5, and 15 minute load averages
        """
        try:
            with open("/proc/loadavg", "r") as f:
                parts = f.read().split()
                
            return {
                "1min": float(parts[0]),
                "5min": float(parts[1]),
                "15min": float(parts[2])
            }
        except Exception:
            return {"1min": 0.0, "5min": 0.0, "15min": 0.0}
    
    async def get_cgroup_stats(self, cgroup_path: str) -> Optional[Dict[str, Any]]:
        """
        Get CPU stats for a specific cgroup v2.
        
        Args:
            cgroup_path: Path to cgroup (e.g., /sys/fs/cgroup/grey-optimizer)
            
        Returns:
            Dictionary with cgroup CPU stats or None if not available
        """
        try:
            cpu_stat_path = f"{cgroup_path}/cpu.stat"
            
            if not os.path.exists(cpu_stat_path):
                return None
            
            with open(cpu_stat_path, "r") as f:
                lines = f.readlines()
            
            stats = {}
            for line in lines:
                parts = line.strip().split()
                if len(parts) == 2:
                    stats[parts[0]] = int(parts[1])
            
            return {
                "usage_usec": stats.get("usage_usec", 0),
                "user_usec": stats.get("user_usec", 0),
                "system_usec": stats.get("system_usec", 0),
                "nr_periods": stats.get("nr_periods", 0),
                "nr_throttled": stats.get("nr_throttled", 0),
                "throttled_usec": stats.get("throttled_usec", 0)
            }
            
        except Exception as e:
            logger.debug(f"cgroup CPU stats error: {e}")
            return None
