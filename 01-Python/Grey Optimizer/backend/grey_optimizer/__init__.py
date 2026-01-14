"""
Grey Optimizer - Production-ready Linux resource optimizer

This package provides the core orchestration daemon for enforcing
measurable reductions in CPU, RAM, and Disk usage on Linux hosts.

Architecture:
- telemetry/   : Metrics collection from /proc, cgroups
- policy/      : Enforcement policy engine
- enforcement/ : Python wrappers for C modules
- api/         : REST + WebSocket API
- persistence/ : SQLite storage for logs and proofs

Safety Principle: "Only use the bits needed, drop the bloat."
"""

__version__ = "1.0.0"
__author__ = "Grey Optimizer Team"

from .config import Config
from .daemon import GreyOptimizerDaemon

__all__ = ["Config", "GreyOptimizerDaemon", "__version__"]
