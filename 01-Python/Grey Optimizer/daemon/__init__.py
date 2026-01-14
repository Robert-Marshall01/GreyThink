"""
Grey Optimizer Daemon Package

Provides the core daemon functionality for adaptive RAM reclamation:

Modules:
- grey_daemon: Main daemon orchestrator
- memory_reclaim: RAM reclamation strategies (cgroup, madvise, KSM, zram, drop_caches)
- telemetry: Memory snapshot collection from /proc and cgroups
- audit: Append-only SQLite audit database with HMAC checkpoints
- health: Health monitoring and automatic rollback

Usage:
    # As a module
    python -m daemon --simulation
    
    # Programmatically
    from daemon.grey_daemon import GreyDaemon, DaemonConfig
    config = DaemonConfig(simulation=True)
    daemon = GreyDaemon(config)
    await daemon.start()
"""

from .grey_daemon import GreyDaemon, DaemonConfig
from .memory_reclaim import MemoryReclaimer, ReclaimResult
from .telemetry import TelemetrySnapshotter, MemorySnapshot, SnapshotComparison
from .audit import AuditDatabase, Checkpoint
from .health import HealthWatchdog, QuickHealthCheck, HealthStatus

__all__ = [
    "GreyDaemon",
    "DaemonConfig", 
    "MemoryReclaimer",
    "ReclaimResult",
    "TelemetrySnapshotter",
    "MemorySnapshot",
    "SnapshotComparison",
    "AuditDatabase",
    "Checkpoint",
    "HealthWatchdog",
    "QuickHealthCheck",
    "HealthStatus",
]

__version__ = "2.0.0"
