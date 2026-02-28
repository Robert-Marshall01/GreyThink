"""
Grey Math — Plugin System.

Extensible plugin architecture supporting:
- Custom IR types and operators
- Solver plugins (symbolic, numeric)
- Rewrite rule packs
- Domain module extensions
- Visualizer plugins
- Hot-reloading with dependency management
"""

from greymath.plugins.base import (
    Plugin,
    PluginType,
    PluginMetadata,
    PluginCapability,
    PluginManager,
)
from greymath.plugins.loader import PluginLoader

__all__ = [
    "Plugin",
    "PluginType",
    "PluginMetadata",
    "PluginCapability",
    "PluginManager",
    "PluginLoader",
]
