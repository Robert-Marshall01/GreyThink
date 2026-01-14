"""
Policy Engine Package

Evaluates telemetry data and makes enforcement decisions
based on configured policies and thresholds.

Safety: Policy decisions are recommendations - enforcement
is handled separately with additional safety checks.
"""

from .engine import PolicyEngine, PolicyDecision

__all__ = ["PolicyEngine", "PolicyDecision"]
