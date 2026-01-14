"""
Enforcement Package

Executes policy decisions by calling appropriate C modules
or system APIs. Manages cgroups, scheduler settings, and
disk operations.

Safety: All enforcement actions are validated, logged, and
reversible where possible. Destructive actions require consent.
"""

from .manager import EnforcementManager, EnforcementResult

__all__ = ["EnforcementManager", "EnforcementResult"]
