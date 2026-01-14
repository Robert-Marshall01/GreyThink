"""
Disk Telemetry Collection

Collects disk I/O metrics from /proc/diskstats and filesystem stats.

Safety: Read-only operations. No system state modified.
"""

import asyncio
import logging
import os
import time
from typing import Dict, Any, Optional, List
from pathlib import Path

logger = logging.getLogger(__name__)


class DiskTelemetry:
    """
    Collects disk I/O and storage metrics.
    
    Uses /proc/diskstats for I/O statistics and statfs
    for filesystem usage.
    """
    
    def __init__(self):
        """Initialize disk telemetry collector."""
        # Previous values for rate calculation
        self._prev_stats: Dict[str, Dict[str, int]] = {}
        self._prev_timestamp: float = 0
        
        logger.debug("Disk telemetry initialized")
    
    async def collect(self) -> Dict[str, Any]:
        """
        Collect current disk metrics.
        
        Returns:
            Dictionary with disk metrics:
            - read_mbs: Read throughput in MB/s
            - write_mbs: Write throughput in MB/s
            - read_iops: Read operations per second
            - write_iops: Write operations per second
            - devices: Per-device statistics
            - filesystems: Filesystem usage
        """
        try:
            current_time = time.time()
            
            # Read /proc/diskstats
            disk_stats = await self._read_diskstats()
            
            # Calculate rates
            read_mbs = 0.0
            write_mbs = 0.0
            read_iops = 0.0
            write_iops = 0.0
            devices = []
            
            time_delta = current_time - self._prev_timestamp if self._prev_timestamp > 0 else 1
            
            for device, stats in disk_stats.items():
                # Skip loop and ram devices
                if device.startswith(("loop", "ram", "dm-")):
                    continue
                
                prev = self._prev_stats.get(device, {})
                
                # Calculate deltas (sector size is typically 512 bytes)
                sector_size = 512
                
                if prev:
                    read_sectors_delta = stats["read_sectors"] - prev.get("read_sectors", 0)
                    write_sectors_delta = stats["write_sectors"] - prev.get("write_sectors", 0)
                    read_ios_delta = stats["read_ios"] - prev.get("read_ios", 0)
                    write_ios_delta = stats["write_ios"] - prev.get("write_ios", 0)
                    
                    device_read_mbs = (read_sectors_delta * sector_size) / (1024 * 1024) / time_delta
                    device_write_mbs = (write_sectors_delta * sector_size) / (1024 * 1024) / time_delta
                    device_read_iops = read_ios_delta / time_delta
                    device_write_iops = write_ios_delta / time_delta
                    
                    read_mbs += device_read_mbs
                    write_mbs += device_write_mbs
                    read_iops += device_read_iops
                    write_iops += device_write_iops
                    
                    devices.append({
                        "name": device,
                        "read_mbs": round(device_read_mbs, 2),
                        "write_mbs": round(device_write_mbs, 2),
                        "read_iops": round(device_read_iops, 2),
                        "write_iops": round(device_write_iops, 2)
                    })
            
            # Store for next calculation
            self._prev_stats = disk_stats
            self._prev_timestamp = current_time
            
            # Get filesystem usage
            filesystems = await self._get_filesystem_usage()
            
            return {
                "read_mbs": round(read_mbs, 2),
                "write_mbs": round(write_mbs, 2),
                "read_iops": round(read_iops, 2),
                "write_iops": round(write_iops, 2),
                "devices": devices,
                "filesystems": filesystems,
                "total_usage_gb": sum(fs["used_gb"] for fs in filesystems),
                "total_available_gb": sum(fs["available_gb"] for fs in filesystems)
            }
            
        except Exception as e:
            logger.error(f"Disk collection error: {e}")
            return {
                "read_mbs": 0.0,
                "write_mbs": 0.0,
                "error": str(e)
            }
    
    async def _read_diskstats(self) -> Dict[str, Dict[str, int]]:
        """
        Parse /proc/diskstats for all devices.
        
        Returns:
            Dictionary mapping device name to stats
        """
        stats = {}
        
        try:
            with open("/proc/diskstats", "r") as f:
                for line in f:
                    parts = line.split()
                    if len(parts) < 14:
                        continue
                    
                    device = parts[2]
                    
                    # Fields from /proc/diskstats
                    # https://www.kernel.org/doc/Documentation/iostats.txt
                    stats[device] = {
                        "read_ios": int(parts[3]),        # reads completed
                        "read_merges": int(parts[4]),     # reads merged
                        "read_sectors": int(parts[5]),    # sectors read
                        "read_time_ms": int(parts[6]),    # time reading (ms)
                        "write_ios": int(parts[7]),       # writes completed
                        "write_merges": int(parts[8]),    # writes merged
                        "write_sectors": int(parts[9]),   # sectors written
                        "write_time_ms": int(parts[10]),  # time writing (ms)
                        "io_in_progress": int(parts[11]), # I/Os currently in progress
                        "io_time_ms": int(parts[12]),     # time doing I/Os (ms)
                        "weighted_io_time": int(parts[13])  # weighted time doing I/Os
                    }
        except Exception as e:
            logger.debug(f"diskstats error: {e}")
        
        return stats
    
    async def _get_filesystem_usage(self) -> List[Dict[str, Any]]:
        """
        Get usage statistics for mounted filesystems.
        
        Returns:
            List of filesystem usage dictionaries
        """
        filesystems = []
        
        try:
            # Read mounted filesystems
            with open("/proc/mounts", "r") as f:
                mounts = f.readlines()
            
            for mount in mounts:
                parts = mount.split()
                if len(parts) < 4:
                    continue
                
                device = parts[0]
                mountpoint = parts[1]
                fstype = parts[2]
                
                # Skip virtual filesystems
                if fstype in ("proc", "sysfs", "devtmpfs", "tmpfs", "cgroup", 
                             "cgroup2", "debugfs", "securityfs", "pstore",
                             "bpf", "tracefs", "hugetlbfs", "mqueue"):
                    continue
                
                # Skip if not a real device
                if not device.startswith("/dev/"):
                    continue
                
                try:
                    # Get filesystem stats
                    stat = os.statvfs(mountpoint)
                    
                    total_bytes = stat.f_blocks * stat.f_frsize
                    free_bytes = stat.f_bfree * stat.f_frsize
                    available_bytes = stat.f_bavail * stat.f_frsize
                    used_bytes = total_bytes - free_bytes
                    
                    filesystems.append({
                        "device": device,
                        "mountpoint": mountpoint,
                        "fstype": fstype,
                        "total_gb": round(total_bytes / (1024 ** 3), 2),
                        "used_gb": round(used_bytes / (1024 ** 3), 2),
                        "available_gb": round(available_bytes / (1024 ** 3), 2),
                        "usage_percent": round((used_bytes / total_bytes) * 100, 2) if total_bytes > 0 else 0
                    })
                    
                except (OSError, PermissionError):
                    continue
                    
        except Exception as e:
            logger.debug(f"Filesystem usage error: {e}")
        
        return filesystems
    
    async def get_cgroup_io_stats(self, cgroup_path: str) -> Optional[Dict[str, Any]]:
        """
        Get I/O stats for a specific cgroup v2.
        
        Args:
            cgroup_path: Path to cgroup
            
        Returns:
            Dictionary with cgroup I/O stats or None if not available
        """
        try:
            io_stat_path = f"{cgroup_path}/io.stat"
            
            if not os.path.exists(io_stat_path):
                return None
            
            stats = {}
            
            with open(io_stat_path, "r") as f:
                for line in f:
                    parts = line.strip().split()
                    if len(parts) < 2:
                        continue
                    
                    device = parts[0]
                    device_stats = {}
                    
                    for part in parts[1:]:
                        if "=" in part:
                            key, value = part.split("=")
                            device_stats[key] = int(value)
                    
                    stats[device] = device_stats
            
            return stats
            
        except Exception as e:
            logger.debug(f"cgroup IO stats error: {e}")
            return None
    
    async def get_file_info(self, path: str) -> Optional[Dict[str, Any]]:
        """
        Get detailed information about a specific file.
        
        Used for deduplication analysis and sparse file detection.
        
        Args:
            path: Path to file
            
        Returns:
            Dictionary with file info or None if not accessible
        """
        try:
            stat = os.stat(path)
            
            # Check if file is sparse
            is_sparse = stat.st_blocks * 512 < stat.st_size
            
            return {
                "path": path,
                "size_bytes": stat.st_size,
                "size_mb": round(stat.st_size / (1024 * 1024), 2),
                "blocks": stat.st_blocks,
                "block_size": stat.st_blksize,
                "is_sparse": is_sparse,
                "sparse_savings_mb": round((stat.st_size - stat.st_blocks * 512) / (1024 * 1024), 2) if is_sparse else 0,
                "hardlinks": stat.st_nlink,
                "inode": stat.st_ino
            }
            
        except Exception as e:
            logger.debug(f"File info error for {path}: {e}")
            return None
