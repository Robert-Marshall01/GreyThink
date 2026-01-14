"""
RAM Telemetry Collection

Collects memory metrics from /proc/meminfo and cgroup memory stats.

Safety: Read-only operations. No system state modified.
"""

import asyncio
import logging
import os
from typing import Dict, Any, Optional

logger = logging.getLogger(__name__)


class RAMTelemetry:
    """
    Collects memory usage metrics from the Linux kernel.
    
    Reads /proc/meminfo for system-wide memory stats and
    cgroup memory.current for per-cgroup limits.
    """
    
    def __init__(self):
        """Initialize RAM telemetry collector."""
        logger.debug("RAM telemetry initialized")
    
    async def collect(self) -> Dict[str, Any]:
        """
        Collect current memory metrics.
        
        Reads /proc/meminfo to get comprehensive memory statistics.
        
        Returns:
            Dictionary with memory metrics:
            - total_mb: Total physical memory
            - used_mb: Used memory (excluding buffers/cache)
            - available_mb: Available memory
            - cached_mb: Cached memory
            - buffers_mb: Buffer memory
            - swap_total_mb: Total swap
            - swap_used_mb: Used swap
        """
        try:
            meminfo = {}
            
            with open("/proc/meminfo", "r") as f:
                for line in f:
                    parts = line.split()
                    if len(parts) >= 2:
                        key = parts[0].rstrip(":")
                        value = int(parts[1])  # Value in kB
                        meminfo[key] = value
            
            # Extract key metrics (all values in kB, convert to MB)
            total_kb = meminfo.get("MemTotal", 0)
            free_kb = meminfo.get("MemFree", 0)
            available_kb = meminfo.get("MemAvailable", 0)
            buffers_kb = meminfo.get("Buffers", 0)
            cached_kb = meminfo.get("Cached", 0)
            swap_total_kb = meminfo.get("SwapTotal", 0)
            swap_free_kb = meminfo.get("SwapFree", 0)
            slab_kb = meminfo.get("Slab", 0)
            
            # Calculate used memory (excluding buffers/cache)
            used_kb = total_kb - available_kb
            
            # Get KSM statistics if available
            ksm_stats = await self._get_ksm_stats()
            
            return {
                "total_mb": round(total_kb / 1024, 2),
                "used_mb": round(used_kb / 1024, 2),
                "free_mb": round(free_kb / 1024, 2),
                "available_mb": round(available_kb / 1024, 2),
                "cached_mb": round(cached_kb / 1024, 2),
                "buffers_mb": round(buffers_kb / 1024, 2),
                "slab_mb": round(slab_kb / 1024, 2),
                "swap_total_mb": round(swap_total_kb / 1024, 2),
                "swap_used_mb": round((swap_total_kb - swap_free_kb) / 1024, 2),
                "usage_percent": round((used_kb / total_kb) * 100, 2) if total_kb > 0 else 0,
                "ksm": ksm_stats
            }
            
        except Exception as e:
            logger.error(f"RAM collection error: {e}")
            return {
                "total_mb": 0,
                "used_mb": 0,
                "available_mb": 0,
                "error": str(e)
            }
    
    async def _get_ksm_stats(self) -> Optional[Dict[str, Any]]:
        """
        Get Kernel Same-page Merging statistics.
        
        KSM can significantly reduce memory for similar pages.
        
        Returns:
            Dictionary with KSM stats or None if KSM not available
        """
        try:
            ksm_path = "/sys/kernel/mm/ksm"
            
            if not os.path.exists(ksm_path):
                return None
            
            def read_ksm_file(name: str) -> int:
                try:
                    with open(f"{ksm_path}/{name}", "r") as f:
                        return int(f.read().strip())
                except (FileNotFoundError, ValueError, PermissionError):
                    return 0
            
            # KSM statistics
            run = read_ksm_file("run")
            pages_shared = read_ksm_file("pages_shared")
            pages_sharing = read_ksm_file("pages_sharing")
            pages_unshared = read_ksm_file("pages_unshared")
            full_scans = read_ksm_file("full_scans")
            
            # Calculate memory savings (page size is typically 4KB)
            page_size_kb = 4
            savings_mb = (pages_sharing - pages_shared) * page_size_kb / 1024
            
            return {
                "enabled": run == 1,
                "pages_shared": pages_shared,
                "pages_sharing": pages_sharing,
                "pages_unshared": pages_unshared,
                "full_scans": full_scans,
                "savings_mb": round(max(0, savings_mb), 2)
            }
            
        except Exception as e:
            logger.debug(f"KSM stats error: {e}")
            return None
    
    async def get_cgroup_stats(self, cgroup_path: str) -> Optional[Dict[str, Any]]:
        """
        Get memory stats for a specific cgroup v2.
        
        Args:
            cgroup_path: Path to cgroup (e.g., /sys/fs/cgroup/grey-optimizer)
            
        Returns:
            Dictionary with cgroup memory stats or None if not available
        """
        try:
            if not os.path.exists(cgroup_path):
                return None
            
            stats = {}
            
            # Current memory usage
            current_path = f"{cgroup_path}/memory.current"
            if os.path.exists(current_path):
                with open(current_path, "r") as f:
                    stats["current_bytes"] = int(f.read().strip())
                    stats["current_mb"] = round(stats["current_bytes"] / (1024 * 1024), 2)
            
            # Memory limit
            max_path = f"{cgroup_path}/memory.max"
            if os.path.exists(max_path):
                with open(max_path, "r") as f:
                    value = f.read().strip()
                    if value == "max":
                        stats["limit_mb"] = None  # No limit
                    else:
                        stats["limit_bytes"] = int(value)
                        stats["limit_mb"] = round(stats["limit_bytes"] / (1024 * 1024), 2)
            
            # Memory events (OOMs, etc.)
            events_path = f"{cgroup_path}/memory.events"
            if os.path.exists(events_path):
                with open(events_path, "r") as f:
                    for line in f:
                        parts = line.strip().split()
                        if len(parts) == 2:
                            stats[f"event_{parts[0]}"] = int(parts[1])
            
            return stats
            
        except Exception as e:
            logger.debug(f"cgroup memory stats error: {e}")
            return None
    
    async def get_process_memory(self, pid: int) -> Optional[Dict[str, Any]]:
        """
        Get detailed memory info for a specific process.
        
        Uses /proc/[pid]/smaps for accurate memory accounting.
        
        Args:
            pid: Process ID
            
        Returns:
            Dictionary with process memory stats or None if not available
        """
        try:
            status_path = f"/proc/{pid}/status"
            
            with open(status_path, "r") as f:
                status = {}
                for line in f:
                    parts = line.split(":")
                    if len(parts) == 2:
                        key = parts[0].strip()
                        value = parts[1].strip().split()[0]
                        try:
                            status[key] = int(value)
                        except ValueError:
                            status[key] = value
            
            return {
                "vm_peak_mb": round(status.get("VmPeak", 0) / 1024, 2),
                "vm_size_mb": round(status.get("VmSize", 0) / 1024, 2),
                "vm_rss_mb": round(status.get("VmRSS", 0) / 1024, 2),
                "vm_shared_mb": round(status.get("RssAnon", 0) / 1024, 2),
                "vm_data_mb": round(status.get("VmData", 0) / 1024, 2),
                "vm_stack_mb": round(status.get("VmStk", 0) / 1024, 2),
            }
            
        except Exception as e:
            logger.debug(f"Process {pid} memory error: {e}")
            return None
