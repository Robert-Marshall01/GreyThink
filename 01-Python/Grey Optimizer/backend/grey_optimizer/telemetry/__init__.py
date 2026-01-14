"""
Telemetry Collection Package

Collects metrics from /proc filesystem and cgroup interfaces
for CPU, RAM, and Disk subsystems.

All collection is read-only and safe to run without privileges,
though some metrics require root for full accuracy.
"""

from .collector import TelemetryCollector
from .cpu import CPUTelemetry
from .ram import RAMTelemetry
from .disk import DiskTelemetry

__all__ = ["TelemetryCollector", "CPUTelemetry", "RAMTelemetry", "DiskTelemetry"]
