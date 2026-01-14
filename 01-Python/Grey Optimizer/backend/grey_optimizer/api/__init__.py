"""
API Package

REST and WebSocket API for the Grey Optimizer dashboard
and external integrations.

Provides:
- Real-time metrics via WebSocket
- Configuration management
- Enforcement control
- Proof artifact export
"""

from .server import APIServer

__all__ = ["APIServer"]
