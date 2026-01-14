"""
Memory Diagnostics Module

Comprehensive memory analysis including:
- /proc/meminfo parsing
- /proc/[pid]/smaps analysis for RSS/PSS
- cgroup memory.current and memory.stat
- Before/after snapshot mechanism
- Memory leak detection

Safety: Read-only operations. No system state modified.
All data collected is stored in the SQLite audit database.
"""

import asyncio
import json
import logging
import os
import re
import time
from dataclasses import dataclass, field, asdict
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, Optional, List, Tuple

logger = logging.getLogger(__name__)


@dataclass
class ProcessMemory:
    """Memory metrics for a single process."""
    pid: int
    name: str
    rss_kb: int = 0          # Resident Set Size
    pss_kb: int = 0          # Proportional Set Size (shared pages divided)
    uss_kb: int = 0          # Unique Set Size (private memory only)
    swap_kb: int = 0         # Swap usage
    shared_clean_kb: int = 0
    shared_dirty_kb: int = 0
    private_clean_kb: int = 0
    private_dirty_kb: int = 0
    referenced_kb: int = 0   # Recently accessed pages
    anonymous_kb: int = 0    # Anonymous (heap/stack) memory
    
    @property
    def rss_mb(self) -> float:
        return round(self.rss_kb / 1024, 2)
    
    @property
    def pss_mb(self) -> float:
        return round(self.pss_kb / 1024, 2)
    
    @property
    def uss_mb(self) -> float:
        return round(self.uss_kb / 1024, 2)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "pid": self.pid,
            "name": self.name,
            "rss_kb": self.rss_kb,
            "rss_mb": self.rss_mb,
            "pss_kb": self.pss_kb,
            "pss_mb": self.pss_mb,
            "uss_kb": self.uss_kb,
            "uss_mb": self.uss_mb,
            "swap_kb": self.swap_kb,
            "shared_clean_kb": self.shared_clean_kb,
            "shared_dirty_kb": self.shared_dirty_kb,
            "private_clean_kb": self.private_clean_kb,
            "private_dirty_kb": self.private_dirty_kb,
            "referenced_kb": self.referenced_kb,
            "anonymous_kb": self.anonymous_kb,
        }


@dataclass
class MemorySnapshot:
    """
    Point-in-time memory snapshot.
    
    Captures system-wide and per-process memory metrics
    for before/after comparison.
    """
    timestamp: str
    timestamp_epoch: float
    
    # System-wide metrics from /proc/meminfo
    meminfo: Dict[str, int] = field(default_factory=dict)
    
    # Derived system metrics
    total_mb: float = 0
    used_mb: float = 0
    available_mb: float = 0
    cached_mb: float = 0
    swap_used_mb: float = 0
    
    # Per-process metrics
    processes: List[ProcessMemory] = field(default_factory=list)
    
    # cgroup metrics
    cgroups: Dict[str, Dict[str, Any]] = field(default_factory=dict)
    
    # KSM stats
    ksm: Optional[Dict[str, Any]] = None
    
    # Process list for tracking
    process_list: List[int] = field(default_factory=list)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "timestamp": self.timestamp,
            "timestamp_epoch": self.timestamp_epoch,
            "system": {
                "total_mb": self.total_mb,
                "used_mb": self.used_mb,
                "available_mb": self.available_mb,
                "cached_mb": self.cached_mb,
                "swap_used_mb": self.swap_used_mb,
                "meminfo": self.meminfo,
            },
            "processes": [p.to_dict() for p in self.processes],
            "cgroups": self.cgroups,
            "ksm": self.ksm,
            "process_count": len(self.processes),
        }
    
    def total_rss_mb(self) -> float:
        """Total RSS across all tracked processes."""
        return sum(p.rss_mb for p in self.processes)
    
    def total_pss_mb(self) -> float:
        """Total PSS across all tracked processes."""
        return sum(p.pss_mb for p in self.processes)


@dataclass
class MemoryComparison:
    """
    Comparison between two memory snapshots.
    
    Calculates savings and identifies which processes
    contributed to memory reduction.
    """
    before: MemorySnapshot
    after: MemorySnapshot
    
    @property
    def duration_seconds(self) -> float:
        return self.after.timestamp_epoch - self.before.timestamp_epoch
    
    @property
    def system_used_delta_mb(self) -> float:
        """Change in system-wide used memory (negative = reduction)."""
        return self.after.used_mb - self.before.used_mb
    
    @property
    def system_available_delta_mb(self) -> float:
        """Change in available memory (positive = improvement)."""
        return self.after.available_mb - self.before.available_mb
    
    @property
    def total_rss_delta_mb(self) -> float:
        """Change in total RSS (negative = reduction)."""
        return self.after.total_rss_mb() - self.before.total_rss_mb()
    
    @property
    def total_pss_delta_mb(self) -> float:
        """Change in total PSS (negative = reduction)."""
        return self.after.total_pss_mb() - self.before.total_pss_mb()
    
    @property
    def reduction_percent(self) -> float:
        """Percentage reduction in total RSS."""
        before_rss = self.before.total_rss_mb()
        if before_rss <= 0:
            return 0
        return round((-self.total_rss_delta_mb / before_rss) * 100, 2)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "duration_seconds": self.duration_seconds,
            "before": self.before.to_dict(),
            "after": self.after.to_dict(),
            "deltas": {
                "system_used_delta_mb": self.system_used_delta_mb,
                "system_available_delta_mb": self.system_available_delta_mb,
                "total_rss_delta_mb": self.total_rss_delta_mb,
                "total_pss_delta_mb": self.total_pss_delta_mb,
                "reduction_percent": self.reduction_percent,
            }
        }


@dataclass
class LeakCandidate:
    """
    Process suspected of memory leaking.
    
    Tracks RSS/PSS growth over a time window.
    """
    pid: int
    name: str
    samples: List[Tuple[float, int, int]] = field(default_factory=list)  # (timestamp, rss_kb, pss_kb)
    
    def add_sample(self, timestamp: float, rss_kb: int, pss_kb: int):
        """Add a memory sample."""
        self.samples.append((timestamp, rss_kb, pss_kb))
    
    def is_leaking(self, min_samples: int = 5, growth_threshold_kb: int = 10240) -> bool:
        """
        Check if process shows steady memory growth.
        
        A leak is detected if:
        1. We have at least min_samples
        2. RSS has grown by at least growth_threshold_kb
        3. Growth is monotonic (each sample >= previous)
        
        Args:
            min_samples: Minimum samples before detecting leak
            growth_threshold_kb: Minimum growth to consider a leak (default 10MB)
        
        Returns:
            True if leak detected
        """
        if len(self.samples) < min_samples:
            return False
        
        # Check monotonic growth
        rss_values = [s[1] for s in self.samples]
        for i in range(1, len(rss_values)):
            if rss_values[i] < rss_values[i-1]:
                return False  # Not monotonic
        
        # Check total growth
        growth = rss_values[-1] - rss_values[0]
        return growth >= growth_threshold_kb
    
    def growth_rate_kb_per_sec(self) -> float:
        """Calculate average growth rate in KB/second."""
        if len(self.samples) < 2:
            return 0
        
        first = self.samples[0]
        last = self.samples[-1]
        
        time_delta = last[0] - first[0]
        if time_delta <= 0:
            return 0
        
        rss_delta = last[1] - first[1]
        return rss_delta / time_delta
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "pid": self.pid,
            "name": self.name,
            "sample_count": len(self.samples),
            "growth_rate_kb_per_sec": round(self.growth_rate_kb_per_sec(), 2),
            "is_leaking": self.is_leaking(),
            "first_rss_kb": self.samples[0][1] if self.samples else 0,
            "last_rss_kb": self.samples[-1][1] if self.samples else 0,
        }


class MemoryDiagnostics:
    """
    Comprehensive memory diagnostics collector.
    
    Provides:
    - System-wide memory metrics from /proc/meminfo
    - Per-process RSS/PSS/USS from /proc/[pid]/smaps
    - cgroup memory stats from memory.current, memory.stat
    - Before/after snapshot comparison
    - Memory leak detection
    
    All operations are read-only and safe.
    """
    
    CGROUP_V2_ROOT = "/sys/fs/cgroup"
    KSM_PATH = "/sys/kernel/mm/ksm"
    
    def __init__(self, database=None):
        """
        Initialize memory diagnostics.
        
        Args:
            database: Optional Database instance for storing results
        """
        self.database = database
        self._leak_tracker: Dict[int, LeakCandidate] = {}
        self._snapshots: List[MemorySnapshot] = []
        logger.info("Memory diagnostics initialized")
    
    async def collect_meminfo(self) -> Dict[str, int]:
        """
        Parse /proc/meminfo into a dictionary.
        
        Returns:
            Dict mapping metric names to values in KB
        """
        meminfo = {}
        try:
            with open("/proc/meminfo", "r") as f:
                for line in f:
                    parts = line.split()
                    if len(parts) >= 2:
                        key = parts[0].rstrip(":")
                        value = int(parts[1])
                        meminfo[key] = value
        except Exception as e:
            logger.error(f"Failed to read /proc/meminfo: {e}")
        
        return meminfo
    
    async def collect_process_smaps(self, pid: int) -> Optional[ProcessMemory]:
        """
        Parse /proc/[pid]/smaps_rollup for memory metrics.
        
        smaps_rollup provides aggregated PSS/USS values without
        parsing the full smaps file (faster and more efficient).
        
        Falls back to /proc/[pid]/smaps if rollup not available.
        
        Args:
            pid: Process ID
            
        Returns:
            ProcessMemory with RSS/PSS/USS metrics
        """
        try:
            # Get process name
            name = "unknown"
            comm_path = f"/proc/{pid}/comm"
            if os.path.exists(comm_path):
                with open(comm_path, "r") as f:
                    name = f.read().strip()
            
            proc_mem = ProcessMemory(pid=pid, name=name)
            
            # Try smaps_rollup first (faster, kernel 4.14+)
            rollup_path = f"/proc/{pid}/smaps_rollup"
            if os.path.exists(rollup_path):
                await self._parse_smaps_rollup(rollup_path, proc_mem)
            else:
                # Fall back to full smaps parsing
                smaps_path = f"/proc/{pid}/smaps"
                if os.path.exists(smaps_path):
                    await self._parse_smaps_full(smaps_path, proc_mem)
                else:
                    # Last resort: use /proc/[pid]/status
                    await self._parse_proc_status(pid, proc_mem)
            
            return proc_mem
            
        except (PermissionError, FileNotFoundError) as e:
            logger.debug(f"Cannot read smaps for PID {pid}: {e}")
            return None
        except Exception as e:
            logger.debug(f"Error reading smaps for PID {pid}: {e}")
            return None
    
    async def _parse_smaps_rollup(self, path: str, proc_mem: ProcessMemory):
        """Parse smaps_rollup file."""
        with open(path, "r") as f:
            for line in f:
                parts = line.split()
                if len(parts) >= 2:
                    key = parts[0].rstrip(":")
                    value = int(parts[1])
                    
                    if key == "Rss":
                        proc_mem.rss_kb = value
                    elif key == "Pss":
                        proc_mem.pss_kb = value
                    elif key == "Shared_Clean":
                        proc_mem.shared_clean_kb = value
                    elif key == "Shared_Dirty":
                        proc_mem.shared_dirty_kb = value
                    elif key == "Private_Clean":
                        proc_mem.private_clean_kb = value
                    elif key == "Private_Dirty":
                        proc_mem.private_dirty_kb = value
                    elif key == "Referenced":
                        proc_mem.referenced_kb = value
                    elif key == "Anonymous":
                        proc_mem.anonymous_kb = value
                    elif key == "Swap":
                        proc_mem.swap_kb = value
        
        # USS = Private_Clean + Private_Dirty
        proc_mem.uss_kb = proc_mem.private_clean_kb + proc_mem.private_dirty_kb
    
    async def _parse_smaps_full(self, path: str, proc_mem: ProcessMemory):
        """Parse full smaps file (slower)."""
        with open(path, "r") as f:
            for line in f:
                parts = line.split()
                if len(parts) >= 2:
                    key = parts[0].rstrip(":")
                    try:
                        value = int(parts[1])
                    except ValueError:
                        continue
                    
                    # Accumulate values across all mappings
                    if key == "Rss":
                        proc_mem.rss_kb += value
                    elif key == "Pss":
                        proc_mem.pss_kb += value
                    elif key == "Shared_Clean":
                        proc_mem.shared_clean_kb += value
                    elif key == "Shared_Dirty":
                        proc_mem.shared_dirty_kb += value
                    elif key == "Private_Clean":
                        proc_mem.private_clean_kb += value
                    elif key == "Private_Dirty":
                        proc_mem.private_dirty_kb += value
                    elif key == "Referenced":
                        proc_mem.referenced_kb += value
                    elif key == "Anonymous":
                        proc_mem.anonymous_kb += value
                    elif key == "Swap":
                        proc_mem.swap_kb += value
        
        proc_mem.uss_kb = proc_mem.private_clean_kb + proc_mem.private_dirty_kb
    
    async def _parse_proc_status(self, pid: int, proc_mem: ProcessMemory):
        """Fallback: parse /proc/[pid]/status for basic RSS."""
        status_path = f"/proc/{pid}/status"
        with open(status_path, "r") as f:
            for line in f:
                if line.startswith("VmRSS:"):
                    parts = line.split()
                    if len(parts) >= 2:
                        proc_mem.rss_kb = int(parts[1])
                        proc_mem.pss_kb = proc_mem.rss_kb  # Approximation
    
    async def collect_cgroup_memory(self, cgroup_path: str) -> Dict[str, Any]:
        """
        Collect memory stats from a cgroup v2.
        
        Reads:
        - memory.current: Current memory usage
        - memory.max: Memory limit
        - memory.stat: Detailed breakdown
        - memory.events: OOM events
        
        Args:
            cgroup_path: Full path to cgroup directory
            
        Returns:
            Dict with cgroup memory stats
        """
        stats = {"path": cgroup_path, "available": False}
        
        if not os.path.exists(cgroup_path):
            return stats
        
        stats["available"] = True
        
        try:
            # memory.current
            current_path = f"{cgroup_path}/memory.current"
            if os.path.exists(current_path):
                with open(current_path, "r") as f:
                    stats["current_bytes"] = int(f.read().strip())
                    stats["current_mb"] = round(stats["current_bytes"] / (1024 * 1024), 2)
            
            # memory.max
            max_path = f"{cgroup_path}/memory.max"
            if os.path.exists(max_path):
                with open(max_path, "r") as f:
                    value = f.read().strip()
                    if value == "max":
                        stats["limit_bytes"] = None
                        stats["limit_mb"] = None
                    else:
                        stats["limit_bytes"] = int(value)
                        stats["limit_mb"] = round(stats["limit_bytes"] / (1024 * 1024), 2)
            
            # memory.stat
            stat_path = f"{cgroup_path}/memory.stat"
            if os.path.exists(stat_path):
                stats["stat"] = {}
                with open(stat_path, "r") as f:
                    for line in f:
                        parts = line.strip().split()
                        if len(parts) == 2:
                            stats["stat"][parts[0]] = int(parts[1])
            
            # memory.events
            events_path = f"{cgroup_path}/memory.events"
            if os.path.exists(events_path):
                stats["events"] = {}
                with open(events_path, "r") as f:
                    for line in f:
                        parts = line.strip().split()
                        if len(parts) == 2:
                            stats["events"][parts[0]] = int(parts[1])
            
        except Exception as e:
            logger.debug(f"Error reading cgroup {cgroup_path}: {e}")
            stats["error"] = str(e)
        
        return stats
    
    async def collect_ksm_stats(self) -> Optional[Dict[str, Any]]:
        """Collect Kernel Same-page Merging statistics."""
        if not os.path.exists(self.KSM_PATH):
            return None
        
        try:
            stats = {}
            
            def read_ksm(name: str) -> int:
                try:
                    with open(f"{self.KSM_PATH}/{name}", "r") as f:
                        return int(f.read().strip())
                except (FileNotFoundError, ValueError, PermissionError):
                    return 0
            
            stats["run"] = read_ksm("run")
            stats["enabled"] = stats["run"] == 1
            stats["pages_shared"] = read_ksm("pages_shared")
            stats["pages_sharing"] = read_ksm("pages_sharing")
            stats["pages_unshared"] = read_ksm("pages_unshared")
            stats["pages_volatile"] = read_ksm("pages_volatile")
            stats["full_scans"] = read_ksm("full_scans")
            stats["sleep_millisecs"] = read_ksm("sleep_millisecs")
            stats["pages_to_scan"] = read_ksm("pages_to_scan")
            
            # Calculate savings (page size = 4KB typically)
            page_size_kb = 4
            sharing_pages = stats["pages_sharing"] - stats["pages_shared"]
            stats["savings_kb"] = max(0, sharing_pages * page_size_kb)
            stats["savings_mb"] = round(stats["savings_kb"] / 1024, 2)
            
            return stats
            
        except Exception as e:
            logger.debug(f"Error reading KSM stats: {e}")
            return None
    
    async def take_snapshot(
        self,
        target_pids: Optional[List[int]] = None,
        include_all_processes: bool = False,
        cgroup_paths: Optional[List[str]] = None,
    ) -> MemorySnapshot:
        """
        Take a point-in-time memory snapshot.
        
        Args:
            target_pids: Specific PIDs to track (if None, uses include_all_processes)
            include_all_processes: If True and no target_pids, collect all user processes
            cgroup_paths: Optional cgroup paths to monitor
        
        Returns:
            MemorySnapshot with all collected metrics
        """
        now = time.time()
        snapshot = MemorySnapshot(
            timestamp=datetime.utcnow().isoformat(),
            timestamp_epoch=now,
        )
        
        # Collect /proc/meminfo
        snapshot.meminfo = await self.collect_meminfo()
        
        # Derive system metrics
        total_kb = snapshot.meminfo.get("MemTotal", 0)
        available_kb = snapshot.meminfo.get("MemAvailable", 0)
        cached_kb = snapshot.meminfo.get("Cached", 0)
        swap_total_kb = snapshot.meminfo.get("SwapTotal", 0)
        swap_free_kb = snapshot.meminfo.get("SwapFree", 0)
        
        snapshot.total_mb = round(total_kb / 1024, 2)
        snapshot.available_mb = round(available_kb / 1024, 2)
        snapshot.used_mb = round((total_kb - available_kb) / 1024, 2)
        snapshot.cached_mb = round(cached_kb / 1024, 2)
        snapshot.swap_used_mb = round((swap_total_kb - swap_free_kb) / 1024, 2)
        
        # Collect per-process metrics
        if target_pids:
            pids = target_pids
        elif include_all_processes:
            pids = self._get_all_pids()
        else:
            pids = []
        
        snapshot.process_list = pids
        
        for pid in pids:
            proc_mem = await self.collect_process_smaps(pid)
            if proc_mem:
                snapshot.processes.append(proc_mem)
        
        # Collect cgroup metrics
        if cgroup_paths:
            for cg_path in cgroup_paths:
                stats = await self.collect_cgroup_memory(cg_path)
                snapshot.cgroups[cg_path] = stats
        
        # Collect KSM stats
        snapshot.ksm = await self.collect_ksm_stats()
        
        # Store in history
        self._snapshots.append(snapshot)
        
        logger.debug(f"Memory snapshot: used={snapshot.used_mb}MB, procs={len(snapshot.processes)}")
        
        return snapshot
    
    def _get_all_pids(self) -> List[int]:
        """Get all user-space process PIDs."""
        pids = []
        try:
            for entry in os.listdir("/proc"):
                if entry.isdigit():
                    pid = int(entry)
                    # Skip kernel threads (no smaps)
                    if os.path.exists(f"/proc/{pid}/smaps"):
                        pids.append(pid)
        except Exception as e:
            logger.debug(f"Error listing /proc: {e}")
        return sorted(pids)
    
    async def compare_snapshots(
        self,
        before: MemorySnapshot,
        after: MemorySnapshot,
    ) -> MemoryComparison:
        """
        Compare two memory snapshots.
        
        Args:
            before: Snapshot taken before enforcement
            after: Snapshot taken after enforcement
        
        Returns:
            MemoryComparison with deltas and reduction percentage
        """
        return MemoryComparison(before=before, after=after)
    
    async def update_leak_tracker(self, snapshot: MemorySnapshot):
        """
        Update leak tracker with new snapshot data.
        
        Call periodically to detect memory leaks.
        
        Args:
            snapshot: Recent memory snapshot
        """
        now = snapshot.timestamp_epoch
        
        for proc in snapshot.processes:
            if proc.pid not in self._leak_tracker:
                self._leak_tracker[proc.pid] = LeakCandidate(
                    pid=proc.pid,
                    name=proc.name,
                )
            
            self._leak_tracker[proc.pid].add_sample(
                timestamp=now,
                rss_kb=proc.rss_kb,
                pss_kb=proc.pss_kb,
            )
    
    def get_leak_candidates(
        self,
        min_samples: int = 5,
        growth_threshold_kb: int = 10240,
    ) -> List[LeakCandidate]:
        """
        Get processes showing memory leak patterns.
        
        Args:
            min_samples: Minimum samples before detecting leak
            growth_threshold_kb: Minimum growth to consider a leak
        
        Returns:
            List of LeakCandidate objects for leaking processes
        """
        leaks = []
        for candidate in self._leak_tracker.values():
            if candidate.is_leaking(min_samples, growth_threshold_kb):
                leaks.append(candidate)
        
        # Sort by growth rate descending
        leaks.sort(key=lambda c: c.growth_rate_kb_per_sec(), reverse=True)
        return leaks
    
    def clear_leak_tracker(self):
        """Clear leak tracking data."""
        self._leak_tracker.clear()
    
    async def generate_report(
        self,
        target_pids: Optional[List[int]] = None,
        include_all_processes: bool = True,
        cgroup_paths: Optional[List[str]] = None,
        format: str = "text",
    ) -> str:
        """
        Generate a comprehensive memory diagnostics report.
        
        Args:
            target_pids: Specific PIDs to analyze
            include_all_processes: Include all user processes
            cgroup_paths: cgroup paths to analyze
            format: "text" for human-readable, "json" for structured
        
        Returns:
            Formatted report string
        """
        snapshot = await self.take_snapshot(
            target_pids=target_pids,
            include_all_processes=include_all_processes,
            cgroup_paths=cgroup_paths,
        )
        
        if format == "json":
            return json.dumps(snapshot.to_dict(), indent=2)
        
        # Human-readable text format
        lines = []
        lines.append("=" * 60)
        lines.append("GREY OPTIMIZER - MEMORY DIAGNOSTICS REPORT")
        lines.append(f"Timestamp: {snapshot.timestamp}")
        lines.append("=" * 60)
        lines.append("")
        
        # System overview
        lines.append("SYSTEM MEMORY")
        lines.append("-" * 40)
        lines.append(f"  Total:     {snapshot.total_mb:>10.1f} MB")
        lines.append(f"  Used:      {snapshot.used_mb:>10.1f} MB")
        lines.append(f"  Available: {snapshot.available_mb:>10.1f} MB")
        lines.append(f"  Cached:    {snapshot.cached_mb:>10.1f} MB")
        lines.append(f"  Swap Used: {snapshot.swap_used_mb:>10.1f} MB")
        lines.append("")
        
        # KSM
        if snapshot.ksm:
            lines.append("KERNEL SAME-PAGE MERGING (KSM)")
            lines.append("-" * 40)
            lines.append(f"  Enabled:   {'Yes' if snapshot.ksm['enabled'] else 'No'}")
            lines.append(f"  Pages Shared: {snapshot.ksm['pages_shared']}")
            lines.append(f"  Pages Sharing: {snapshot.ksm['pages_sharing']}")
            lines.append(f"  Savings:   {snapshot.ksm['savings_mb']:>10.1f} MB")
            lines.append("")
        
        # Top processes by RSS
        if snapshot.processes:
            lines.append("TOP PROCESSES BY RSS")
            lines.append("-" * 60)
            lines.append(f"  {'PID':>8}  {'RSS MB':>10}  {'PSS MB':>10}  NAME")
            
            # Sort by RSS descending
            sorted_procs = sorted(
                snapshot.processes,
                key=lambda p: p.rss_kb,
                reverse=True,
            )[:20]
            
            for proc in sorted_procs:
                lines.append(f"  {proc.pid:>8}  {proc.rss_mb:>10.1f}  {proc.pss_mb:>10.1f}  {proc.name}")
            
            lines.append("")
            lines.append(f"Total RSS tracked: {snapshot.total_rss_mb():.1f} MB")
            lines.append(f"Total PSS tracked: {snapshot.total_pss_mb():.1f} MB")
            lines.append("")
        
        # Cgroups
        if snapshot.cgroups:
            lines.append("CGROUP MEMORY")
            lines.append("-" * 40)
            for path, stats in snapshot.cgroups.items():
                if stats.get("available"):
                    current = stats.get("current_mb", 0)
                    limit = stats.get("limit_mb", "unlimited")
                    lines.append(f"  {path}")
                    lines.append(f"    Current: {current} MB, Limit: {limit}")
                    if stats.get("events"):
                        oom_kill = stats["events"].get("oom_kill", 0)
                        if oom_kill > 0:
                            lines.append(f"    OOM Kills: {oom_kill}")
            lines.append("")
        
        # Leak candidates
        leaks = self.get_leak_candidates()
        if leaks:
            lines.append("POTENTIAL MEMORY LEAKS")
            lines.append("-" * 40)
            for leak in leaks[:10]:
                rate = leak.growth_rate_kb_per_sec()
                lines.append(f"  PID {leak.pid} ({leak.name}): +{rate:.1f} KB/sec")
            lines.append("")
        
        lines.append("=" * 60)
        
        return "\n".join(lines)
    
    async def save_to_database(self, snapshot: MemorySnapshot):
        """
        Save snapshot to SQLite audit database.
        
        Args:
            snapshot: Memory snapshot to save
        """
        if not self.database:
            logger.debug("No database configured for memory diagnostics")
            return
        
        try:
            # Store as JSON in audit log
            await self.database.log_event(
                event_type="memory_snapshot",
                data=snapshot.to_dict(),
            )
            logger.debug("Memory snapshot saved to database")
        except Exception as e:
            logger.error(f"Failed to save memory snapshot: {e}")
