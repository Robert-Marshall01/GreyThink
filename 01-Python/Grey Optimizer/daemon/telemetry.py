"""
Grey Optimizer - Telemetry Snapshot Module

Captures detailed memory snapshots for baseline/post comparison:
- /proc/meminfo for system-wide metrics
- /proc/[pid]/smaps for per-process RSS/PSS
- cgroup memory.current/memory.stat
- KSM stats from /sys/kernel/mm/ksm/*
- Writes JSON snapshots to SQLite audit database

This module provides the evidence needed to prove RAM reclamation.
"""

import asyncio
import json
import logging
import os
import re
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple
import platform

logger = logging.getLogger(__name__)

# Artifact directory for JSON snapshots
ARTIFACTS_DIR = Path(os.environ.get(
    "GREY_ARTIFACTS_DIR",
    "/var/lib/grey-optimizer/artifacts"
))

# KSM sysfs path
KSM_PATH = Path("/sys/kernel/mm/ksm")


@dataclass
class ProcessMemory:
    """Memory statistics for a single process."""
    pid: int
    name: str
    rss: int = 0           # Resident Set Size (bytes)
    pss: int = 0           # Proportional Set Size (bytes)
    uss: int = 0           # Unique Set Size (bytes)
    swap: int = 0          # Swap usage (bytes)
    shared_clean: int = 0
    shared_dirty: int = 0
    private_clean: int = 0
    private_dirty: int = 0
    referenced: int = 0
    anonymous: int = 0
    lazy_free: int = 0
    
    @property
    def rss_mb(self) -> float:
        return self.rss / (1024 * 1024)
    
    @property
    def pss_mb(self) -> float:
        return self.pss / (1024 * 1024)
    
    @property
    def uss_mb(self) -> float:
        return self.uss / (1024 * 1024)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "pid": self.pid,
            "name": self.name,
            "rss": self.rss,
            "rss_mb": round(self.rss_mb, 2),
            "pss": self.pss,
            "pss_mb": round(self.pss_mb, 2),
            "uss": self.uss,
            "uss_mb": round(self.uss_mb, 2),
            "swap": self.swap,
            "shared_clean": self.shared_clean,
            "shared_dirty": self.shared_dirty,
            "private_clean": self.private_clean,
            "private_dirty": self.private_dirty,
            "referenced": self.referenced,
            "anonymous": self.anonymous,
        }


@dataclass
class MemorySnapshot:
    """Point-in-time memory snapshot."""
    timestamp: datetime
    source: str = "unknown"
    action_id: Optional[str] = None  # Links snapshot to a specific action
    
    # System-wide metrics (bytes)
    mem_total: int = 0
    mem_free: int = 0
    mem_available: int = 0
    buffers: int = 0
    cached: int = 0
    swap_cached: int = 0
    active: int = 0
    inactive: int = 0
    swap_total: int = 0
    swap_free: int = 0
    dirty: int = 0
    slab: int = 0
    sreclaimable: int = 0
    shmem: int = 0
    
    # Calculated metrics
    mem_used: int = 0
    
    # Per-process metrics
    processes: List[ProcessMemory] = field(default_factory=list)
    
    # cgroup metrics
    cgroup_memory_current: Dict[str, int] = field(default_factory=dict)
    cgroup_memory_max: Dict[str, int] = field(default_factory=dict)
    
    # KSM metrics (Linux only)
    ksm_stats: Dict[str, Any] = field(default_factory=dict)
    
    @property
    def total_mb(self) -> float:
        return self.mem_total / (1024 * 1024)
    
    @property
    def used_mb(self) -> float:
        return self.mem_used / (1024 * 1024)
    
    @property
    def available_mb(self) -> float:
        return self.mem_available / (1024 * 1024)
    
    @property
    def total_process_rss(self) -> int:
        return sum(p.rss for p in self.processes)
    
    @property
    def total_process_pss(self) -> int:
        return sum(p.pss for p in self.processes)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "timestamp": self.timestamp.isoformat(),
            "source": self.source,
            "action_id": self.action_id,
            "system": {
                "total_mb": round(self.total_mb, 2),
                "used_mb": round(self.used_mb, 2),
                "available_mb": round(self.available_mb, 2),
                "buffers_mb": round(self.buffers / (1024 * 1024), 2),
                "cached_mb": round(self.cached / (1024 * 1024), 2),
                "swap_total_mb": round(self.swap_total / (1024 * 1024), 2),
                "swap_free_mb": round(self.swap_free / (1024 * 1024), 2),
                "dirty_mb": round(self.dirty / (1024 * 1024), 2),
                "slab_mb": round(self.slab / (1024 * 1024), 2),
            },
            "processes": [p.to_dict() for p in self.processes],
            "cgroups": {
                "current": self.cgroup_memory_current,
                "max": self.cgroup_memory_max,
            },
            "ksm": self.ksm_stats,
            "totals": {
                "process_rss_mb": round(self.total_process_rss / (1024 * 1024), 2),
                "process_pss_mb": round(self.total_process_pss / (1024 * 1024), 2),
            },
        }


@dataclass  
class SnapshotComparison:
    """Comparison between two snapshots (baseline vs post)."""
    baseline: MemorySnapshot
    post: MemorySnapshot
    
    @property
    def system_reduction_bytes(self) -> int:
        """Total system memory reduction in bytes."""
        return self.baseline.mem_used - self.post.mem_used
    
    @property
    def system_reduction_mb(self) -> float:
        """Total system memory reduction in MB."""
        return self.system_reduction_bytes / (1024 * 1024)
    
    @property
    def system_reduction_percent(self) -> float:
        """Percentage of memory reduced."""
        if self.baseline.mem_used == 0:
            return 0
        return (self.system_reduction_bytes / self.baseline.mem_used) * 100
    
    @property
    def cache_reduction_bytes(self) -> int:
        """Cache reduction in bytes."""
        return (self.baseline.cached + self.baseline.buffers) - \
               (self.post.cached + self.post.buffers)
    
    @property
    def process_rss_reduction(self) -> int:
        """Total RSS reduction across processes."""
        return self.baseline.total_process_rss - self.post.total_process_rss
    
    @property
    def process_pss_reduction(self) -> int:
        """Total PSS reduction across processes."""
        return self.baseline.total_process_pss - self.post.total_process_pss
    
    def get_per_process_comparison(self) -> List[Dict[str, Any]]:
        """Get per-process memory changes."""
        baseline_by_pid = {p.pid: p for p in self.baseline.processes}
        post_by_pid = {p.pid: p for p in self.post.processes}
        
        changes = []
        all_pids = set(baseline_by_pid.keys()) | set(post_by_pid.keys())
        
        for pid in all_pids:
            baseline_proc = baseline_by_pid.get(pid)
            post_proc = post_by_pid.get(pid)
            
            if baseline_proc and post_proc:
                rss_change = baseline_proc.rss - post_proc.rss
                pss_change = baseline_proc.pss - post_proc.pss
                
                changes.append({
                    "pid": pid,
                    "name": baseline_proc.name,
                    "status": "running",
                    "rss_baseline_mb": round(baseline_proc.rss_mb, 2),
                    "rss_post_mb": round(post_proc.rss_mb, 2),
                    "rss_change_mb": round(rss_change / (1024 * 1024), 2),
                    "pss_baseline_mb": round(baseline_proc.pss_mb, 2),
                    "pss_post_mb": round(post_proc.pss_mb, 2),
                    "pss_change_mb": round(pss_change / (1024 * 1024), 2),
                })
            elif baseline_proc:
                changes.append({
                    "pid": pid,
                    "name": baseline_proc.name,
                    "status": "exited",
                    "rss_baseline_mb": round(baseline_proc.rss_mb, 2),
                    "rss_post_mb": 0,
                    "rss_change_mb": round(baseline_proc.rss_mb, 2),
                })
            else:
                changes.append({
                    "pid": pid,
                    "name": post_proc.name,
                    "status": "new",
                    "rss_baseline_mb": 0,
                    "rss_post_mb": round(post_proc.rss_mb, 2),
                    "rss_change_mb": round(-post_proc.rss_mb, 2),
                })
        
        # Sort by RSS reduction (largest first)
        changes.sort(key=lambda x: x.get("rss_change_mb", 0), reverse=True)
        return changes
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "baseline_timestamp": self.baseline.timestamp.isoformat(),
            "post_timestamp": self.post.timestamp.isoformat(),
            "duration_seconds": (self.post.timestamp - self.baseline.timestamp).total_seconds(),
            "summary": {
                "system_reduction_mb": round(self.system_reduction_mb, 2),
                "system_reduction_percent": round(self.system_reduction_percent, 2),
                "cache_reduction_mb": round(self.cache_reduction_bytes / (1024 * 1024), 2),
                "process_rss_reduction_mb": round(self.process_rss_reduction / (1024 * 1024), 2),
                "process_pss_reduction_mb": round(self.process_pss_reduction / (1024 * 1024), 2),
            },
            "per_process": self.get_per_process_comparison(),
        }


class TelemetrySnapshotter:
    """
    Captures memory telemetry snapshots from /proc and cgroups.
    
    Thread-safe and async-friendly.
    """
    
    def __init__(
        self,
        target_pids: Optional[List[int]] = None,
        track_all_processes: bool = False,
        min_rss_mb: float = 10.0,
    ):
        """
        Initialize the snapshotter.
        
        Args:
            target_pids: Specific PIDs to track (optional)
            track_all_processes: If True, track all processes
            min_rss_mb: Minimum RSS to include a process (when tracking all)
        """
        self.target_pids = set(target_pids) if target_pids else set()
        self.track_all_processes = track_all_processes
        self.min_rss_mb = min_rss_mb
        self._smaps_regex = re.compile(r"^([A-Za-z_]+):\s+(\d+)\s+kB", re.MULTILINE)
    
    async def take_snapshot(self, source: str = "manual") -> MemorySnapshot:
        """
        Take a complete memory snapshot.
        
        Args:
            source: Label for this snapshot (e.g., "baseline", "post")
            
        Returns:
            MemorySnapshot with all collected data
        """
        snapshot = MemorySnapshot(
            timestamp=datetime.utcnow(),
            source=source,
        )
        
        # Collect system-wide metrics
        await self._collect_meminfo(snapshot)
        
        # Collect per-process metrics
        await self._collect_process_memory(snapshot)
        
        # Collect cgroup metrics
        await self._collect_cgroup_memory(snapshot)
        
        return snapshot
    
    async def _collect_meminfo(self, snapshot: MemorySnapshot) -> None:
        """Collect system-wide memory metrics from /proc/meminfo."""
        try:
            content = await asyncio.to_thread(self._read_meminfo)
            
            for line in content.split("\n"):
                if not line:
                    continue
                    
                parts = line.split()
                if len(parts) < 2:
                    continue
                
                key = parts[0].rstrip(":")
                value = int(parts[1]) * 1024  # Convert kB to bytes
                
                if key == "MemTotal":
                    snapshot.mem_total = value
                elif key == "MemFree":
                    snapshot.mem_free = value
                elif key == "MemAvailable":
                    snapshot.mem_available = value
                elif key == "Buffers":
                    snapshot.buffers = value
                elif key == "Cached":
                    snapshot.cached = value
                elif key == "SwapCached":
                    snapshot.swap_cached = value
                elif key == "Active":
                    snapshot.active = value
                elif key == "Inactive":
                    snapshot.inactive = value
                elif key == "SwapTotal":
                    snapshot.swap_total = value
                elif key == "SwapFree":
                    snapshot.swap_free = value
                elif key == "Dirty":
                    snapshot.dirty = value
                elif key == "Slab":
                    snapshot.slab = value
                elif key == "SReclaimable":
                    snapshot.sreclaimable = value
                elif key == "Shmem":
                    snapshot.shmem = value
            
            # Calculate used memory
            snapshot.mem_used = snapshot.mem_total - snapshot.mem_available
            
        except Exception as e:
            logger.error(f"Failed to read /proc/meminfo: {e}")
    
    def _read_meminfo(self) -> str:
        """Read /proc/meminfo synchronously."""
        try:
            with open("/proc/meminfo", "r") as f:
                return f.read()
        except FileNotFoundError:
            return ""
    
    async def _collect_process_memory(self, snapshot: MemorySnapshot) -> None:
        """Collect per-process memory from /proc/[pid]/smaps."""
        pids_to_collect = set()
        
        if self.target_pids:
            pids_to_collect = self.target_pids.copy()
        
        if self.track_all_processes or not self.target_pids:
            try:
                all_pids = await asyncio.to_thread(self._get_all_pids)
                pids_to_collect.update(all_pids)
            except Exception as e:
                logger.error(f"Failed to list processes: {e}")
        
        # Collect memory for each PID (in parallel with limit)
        sem = asyncio.Semaphore(20)  # Limit concurrent file reads
        
        async def collect_one(pid: int) -> Optional[ProcessMemory]:
            async with sem:
                return await self._collect_pid_memory(pid)
        
        tasks = [collect_one(pid) for pid in pids_to_collect]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        for result in results:
            if isinstance(result, ProcessMemory):
                # Apply RSS filter
                if result.rss_mb >= self.min_rss_mb:
                    snapshot.processes.append(result)
            elif isinstance(result, Exception):
                # Silently skip (process may have exited)
                pass
        
        # Sort by RSS descending
        snapshot.processes.sort(key=lambda p: p.rss, reverse=True)
    
    def _get_all_pids(self) -> List[int]:
        """Get all process PIDs from /proc."""
        pids = []
        try:
            for entry in os.listdir("/proc"):
                if entry.isdigit():
                    pids.append(int(entry))
        except Exception:
            pass
        return pids
    
    async def _collect_pid_memory(self, pid: int) -> Optional[ProcessMemory]:
        """Collect memory stats for a single PID."""
        try:
            proc = ProcessMemory(pid=pid, name="")
            
            # Get process name
            comm_path = f"/proc/{pid}/comm"
            try:
                with open(comm_path, "r") as f:
                    proc.name = f.read().strip()
            except (FileNotFoundError, PermissionError):
                proc.name = f"pid-{pid}"
            
            # Parse smaps for detailed memory info
            smaps_path = f"/proc/{pid}/smaps_rollup"
            fallback_smaps = f"/proc/{pid}/smaps"
            
            content = ""
            try:
                with open(smaps_path, "r") as f:
                    content = f.read()
            except FileNotFoundError:
                try:
                    with open(fallback_smaps, "r") as f:
                        content = f.read()
                except (FileNotFoundError, PermissionError):
                    # Fall back to status
                    return await self._collect_pid_status(pid, proc)
            except PermissionError:
                return await self._collect_pid_status(pid, proc)
            
            # Parse smaps content
            for match in self._smaps_regex.finditer(content):
                key = match.group(1)
                value = int(match.group(2)) * 1024  # kB to bytes
                
                if key == "Rss":
                    proc.rss = value
                elif key == "Pss":
                    proc.pss = value
                elif key == "Shared_Clean":
                    proc.shared_clean = value
                elif key == "Shared_Dirty":
                    proc.shared_dirty = value
                elif key == "Private_Clean":
                    proc.private_clean = value
                elif key == "Private_Dirty":
                    proc.private_dirty = value
                elif key == "Referenced":
                    proc.referenced = value
                elif key == "Anonymous":
                    proc.anonymous = value
                elif key == "Swap":
                    proc.swap = value
                elif key == "LazyFree":
                    proc.lazy_free = value
            
            # Calculate USS (Unique Set Size)
            proc.uss = proc.private_clean + proc.private_dirty
            
            return proc
            
        except (ProcessLookupError, FileNotFoundError, PermissionError):
            return None
        except Exception as e:
            logger.debug(f"Failed to collect memory for PID {pid}: {e}")
            return None
    
    async def _collect_pid_status(self, pid: int, proc: ProcessMemory) -> Optional[ProcessMemory]:
        """Fallback: collect basic memory from /proc/[pid]/status."""
        try:
            status_path = f"/proc/{pid}/status"
            with open(status_path, "r") as f:
                for line in f:
                    if line.startswith("VmRSS:"):
                        proc.rss = int(line.split()[1]) * 1024
                    elif line.startswith("VmSwap:"):
                        proc.swap = int(line.split()[1]) * 1024
            return proc
        except Exception:
            return None
    
    async def _collect_cgroup_memory(self, snapshot: MemorySnapshot) -> None:
        """Collect cgroup memory statistics."""
        cgroup_root = Path("/sys/fs/cgroup")
        grey_cgroups = cgroup_root / "grey-optimizer"
        
        if not grey_cgroups.exists():
            return
        
        try:
            for cg_path in grey_cgroups.iterdir():
                if not cg_path.is_dir():
                    continue
                
                name = cg_path.name
                
                # Read memory.current
                current_path = cg_path / "memory.current"
                if current_path.exists():
                    try:
                        with open(current_path) as f:
                            snapshot.cgroup_memory_current[name] = int(f.read().strip())
                    except (ValueError, PermissionError):
                        pass
                
                # Read memory.max
                max_path = cg_path / "memory.max"
                if max_path.exists():
                    try:
                        with open(max_path) as f:
                            val = f.read().strip()
                            snapshot.cgroup_memory_max[name] = (
                                -1 if val == "max" else int(val)
                            )
                    except (ValueError, PermissionError):
                        pass
                        
        except Exception as e:
            logger.debug(f"Failed to read cgroup memory: {e}")
    
    def compare(self, baseline: MemorySnapshot, post: MemorySnapshot) -> SnapshotComparison:
        """
        Compare two snapshots to calculate memory reduction.
        
        Args:
            baseline: Snapshot before enforcement
            post: Snapshot after enforcement
            
        Returns:
            SnapshotComparison with detailed reduction metrics
        """
        return SnapshotComparison(baseline=baseline, post=post)


class SnapshotStore:
    """
    Persists snapshots to SQLite database for audit trail.
    """
    
    def __init__(self, db_path: Path):
        """
        Initialize the snapshot store.
        
        Args:
            db_path: Path to SQLite database
        """
        self.db_path = db_path
        self._initialized = False
    
    async def initialize(self) -> None:
        """Initialize the database schema."""
        import aiosqlite
        
        async with aiosqlite.connect(self.db_path) as db:
            await db.execute("""
                CREATE TABLE IF NOT EXISTS snapshots (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    timestamp TEXT NOT NULL,
                    source TEXT NOT NULL,
                    snapshot_json TEXT NOT NULL,
                    created_at TEXT DEFAULT CURRENT_TIMESTAMP
                )
            """)
            
            await db.execute("""
                CREATE TABLE IF NOT EXISTS comparisons (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    baseline_id INTEGER NOT NULL,
                    post_id INTEGER NOT NULL,
                    comparison_json TEXT NOT NULL,
                    created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (baseline_id) REFERENCES snapshots(id),
                    FOREIGN KEY (post_id) REFERENCES snapshots(id)
                )
            """)
            
            await db.execute("""
                CREATE INDEX IF NOT EXISTS idx_snapshots_timestamp 
                ON snapshots(timestamp)
            """)
            
            await db.execute("""
                CREATE INDEX IF NOT EXISTS idx_snapshots_source 
                ON snapshots(source)
            """)
            
            await db.commit()
        
        self._initialized = True
    
    async def save_snapshot(self, snapshot: MemorySnapshot) -> int:
        """
        Save a snapshot to the database.
        
        Returns:
            The snapshot ID
        """
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute(
                """INSERT INTO snapshots (timestamp, source, snapshot_json)
                   VALUES (?, ?, ?)""",
                (
                    snapshot.timestamp.isoformat(),
                    snapshot.source,
                    json.dumps(snapshot.to_dict()),
                )
            )
            await db.commit()
            return cursor.lastrowid
    
    async def save_comparison(
        self,
        baseline_id: int,
        post_id: int,
        comparison: SnapshotComparison,
    ) -> int:
        """
        Save a comparison to the database.
        
        Returns:
            The comparison ID
        """
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            cursor = await db.execute(
                """INSERT INTO comparisons (baseline_id, post_id, comparison_json)
                   VALUES (?, ?, ?)""",
                (baseline_id, post_id, json.dumps(comparison.to_dict()))
            )
            await db.commit()
            return cursor.lastrowid
    
    async def get_recent_snapshots(self, limit: int = 10) -> List[Dict[str, Any]]:
        """Get recent snapshots."""
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            cursor = await db.execute(
                """SELECT id, timestamp, source 
                   FROM snapshots 
                   ORDER BY timestamp DESC 
                   LIMIT ?""",
                (limit,)
            )
            rows = await cursor.fetchall()
            return [dict(row) for row in rows]
    
    async def get_snapshot(self, snapshot_id: int) -> Optional[Dict[str, Any]]:
        """Get a snapshot by ID."""
        import aiosqlite
        
        if not self._initialized:
            await self.initialize()
        
        async with aiosqlite.connect(self.db_path) as db:
            db.row_factory = aiosqlite.Row
            cursor = await db.execute(
                "SELECT * FROM snapshots WHERE id = ?",
                (snapshot_id,)
            )
            row = await cursor.fetchone()
            if row:
                result = dict(row)
                result["snapshot"] = json.loads(result["snapshot_json"])
                return result
            return None


# ═══════════════════════════════════════════════════════════════════════════════
# Snapshot Helper Functions (Required by Project Spec)
# ═══════════════════════════════════════════════════════════════════════════════

def _read_ksm_stats() -> Dict[str, Any]:
    """Read current KSM statistics from sysfs."""
    stats = {}
    if platform.system().lower() != "linux" or not KSM_PATH.exists():
        return stats
    
    for key in ["run", "pages_to_scan", "sleep_millisecs", "pages_shared", 
                "pages_sharing", "pages_unshared", "pages_volatile", "full_scans"]:
        path = KSM_PATH / key
        if path.exists():
            try:
                value = path.read_text().strip()
                # Convert numeric values
                try:
                    stats[key] = int(value)
                except ValueError:
                    stats[key] = value
            except PermissionError:
                stats[key] = None
    
    # Calculate estimated savings
    if stats.get("pages_shared") and stats.get("pages_sharing"):
        page_size = os.sysconf("SC_PAGESIZE") if hasattr(os, 'sysconf') else 4096
        stats["estimated_savings_bytes"] = stats["pages_shared"] * page_size
        stats["estimated_savings_mb"] = round(stats["estimated_savings_bytes"] / (1024 * 1024), 2)
    
    return stats


def _read_smaps_for_pid(pid: int) -> Dict[str, Any]:
    """Read detailed memory info from /proc/[pid]/smaps."""
    summary = {
        "rss_kb": 0,
        "pss_kb": 0, 
        "uss_kb": 0,
        "anonymous_kb": 0,
        "swap_kb": 0,
        "shared_clean_kb": 0,
        "shared_dirty_kb": 0,
        "private_clean_kb": 0,
        "private_dirty_kb": 0,
    }
    
    try:
        with open(f"/proc/{pid}/smaps", "r") as f:
            for line in f:
                if line.startswith("Rss:"):
                    summary["rss_kb"] += int(line.split()[1])
                elif line.startswith("Pss:"):
                    summary["pss_kb"] += int(line.split()[1])
                elif line.startswith("Anonymous:"):
                    summary["anonymous_kb"] += int(line.split()[1])
                elif line.startswith("Swap:"):
                    summary["swap_kb"] += int(line.split()[1])
                elif line.startswith("Shared_Clean:"):
                    summary["shared_clean_kb"] += int(line.split()[1])
                elif line.startswith("Shared_Dirty:"):
                    summary["shared_dirty_kb"] += int(line.split()[1])
                elif line.startswith("Private_Clean:"):
                    summary["private_clean_kb"] += int(line.split()[1])
                elif line.startswith("Private_Dirty:"):
                    summary["private_dirty_kb"] += int(line.split()[1])
        
        # USS = Private memory only
        summary["uss_kb"] = summary["private_clean_kb"] + summary["private_dirty_kb"]
        
    except (FileNotFoundError, PermissionError):
        pass
    
    return summary


async def snapshot_before(
    action_id: str,
    include_processes: bool = True,
    target_pids: Optional[List[int]] = None,
    database: Optional[Any] = None,
) -> MemorySnapshot:
    """
    Capture a baseline memory snapshot before an action.
    
    Args:
        action_id: Unique identifier for this action (links before/after)
        include_processes: Whether to capture per-process metrics
        target_pids: Optional list of specific PIDs to capture
        database: Optional audit database to register the snapshot
    
    Returns:
        MemorySnapshot with current state
    
    Side Effects:
        - Writes JSON snapshot to artifacts/ directory
        - Registers entry in SQLite audit if database provided
    """
    snapshotter = TelemetrySnapshotter()
    snapshot = await snapshotter.capture(
        source=f"before_{action_id}",
        include_processes=include_processes,
        target_pids=target_pids,
    )
    snapshot.action_id = action_id
    
    # Add KSM stats
    snapshot.ksm_stats = _read_ksm_stats()
    
    # Write to artifacts directory
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    artifact_path = ARTIFACTS_DIR / f"baseline_{action_id}_{datetime.now(timezone.utc).strftime('%Y%m%d_%H%M%S')}.json"
    
    artifact_data = {
        "type": "baseline_snapshot",
        "action_id": action_id,
        "snapshot": snapshot.to_dict(),
        "artifact_path": str(artifact_path),
    }
    
    artifact_path.write_text(json.dumps(artifact_data, indent=2))
    logger.info(f"Baseline snapshot saved: {artifact_path}")
    
    # Register in audit database if provided
    if database:
        try:
            await database.log_action(
                action="snapshot_before",
                subsystem="telemetry",
                parameters={"action_id": action_id},
                outcome="success",
                details={"artifact_path": str(artifact_path)},
            )
        except Exception as e:
            logger.warning(f"Could not log to audit database: {e}")
    
    return snapshot


async def snapshot_after(
    action_id: str,
    baseline: Optional[MemorySnapshot] = None,
    include_processes: bool = True,
    target_pids: Optional[List[int]] = None,
    database: Optional[Any] = None,
) -> Tuple[MemorySnapshot, Optional[SnapshotComparison]]:
    """
    Capture a post-action memory snapshot and compare to baseline.
    
    Args:
        action_id: Unique identifier (same as snapshot_before)
        baseline: Optional baseline snapshot for comparison
        include_processes: Whether to capture per-process metrics
        target_pids: Optional list of specific PIDs to capture
        database: Optional audit database
    
    Returns:
        Tuple of (post_snapshot, comparison) where comparison is None if no baseline
    
    Side Effects:
        - Writes JSON snapshot and comparison to artifacts/
        - Registers entries in SQLite audit if database provided
    """
    snapshotter = TelemetrySnapshotter()
    snapshot = await snapshotter.capture(
        source=f"after_{action_id}",
        include_processes=include_processes,
        target_pids=target_pids,
    )
    snapshot.action_id = action_id
    
    # Add KSM stats
    snapshot.ksm_stats = _read_ksm_stats()
    
    # Write post snapshot
    ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now(timezone.utc).strftime('%Y%m%d_%H%M%S')
    artifact_path = ARTIFACTS_DIR / f"post_{action_id}_{timestamp}.json"
    
    artifact_data = {
        "type": "post_snapshot",
        "action_id": action_id,
        "snapshot": snapshot.to_dict(),
        "artifact_path": str(artifact_path),
    }
    
    artifact_path.write_text(json.dumps(artifact_data, indent=2))
    logger.info(f"Post snapshot saved: {artifact_path}")
    
    # Create comparison if baseline provided
    comparison = None
    if baseline:
        comparison = SnapshotComparison(baseline=baseline, post=snapshot)
        
        comparison_path = ARTIFACTS_DIR / f"comparison_{action_id}_{timestamp}.json"
        comparison_data = {
            "type": "snapshot_comparison",
            "action_id": action_id,
            "comparison": comparison.to_dict(),
            "savings": {
                "system_reduction_mb": round(comparison.system_reduction_mb, 2),
                "system_reduction_percent": round(comparison.system_reduction_percent, 2),
                "cache_reduction_mb": round(comparison.cache_reduction_bytes / (1024 * 1024), 2),
                "process_rss_reduction_mb": round(comparison.process_rss_reduction / (1024 * 1024), 2),
                "process_pss_reduction_mb": round(comparison.process_pss_reduction / (1024 * 1024), 2),
            },
        }
        comparison_path.write_text(json.dumps(comparison_data, indent=2))
        logger.info(f"Comparison saved: {comparison_path}")
    
    # Register in audit database
    if database:
        try:
            await database.log_action(
                action="snapshot_after",
                subsystem="telemetry",
                parameters={"action_id": action_id},
                outcome="success",
                details={
                    "artifact_path": str(artifact_path),
                    "has_comparison": comparison is not None,
                },
            )
        except Exception as e:
            logger.warning(f"Could not log to audit database: {e}")
    
    return snapshot, comparison