"""
Experimental Plugin System — Plugin registration and lifecycle for
extending the experimental subsystem.

Allows third-party or user plugins to register:
- New manifold types and geometric structures
- Custom operator classes
- Stochastic process definitions
- PDE solvers
- Category constructs
- Architecture components
- Numeric solvers and diagnostics

Each plugin must declare:
- Capabilities (what it provides)
- Dependencies (what it requires)
- Stability level
- Required IR extensions
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional, Type

from greymath.experimental.mode import StabilityLevel

logger = logging.getLogger(__name__)


# ─── Plugin Capability ──────────────────────────────────────────────────────

class PluginCapability(Enum):
    """What a plugin provides."""
    MANIFOLD = auto()
    OPERATOR = auto()
    STOCHASTIC_PROCESS = auto()
    PDE_SOLVER = auto()
    CATEGORY_CONSTRUCT = auto()
    ARCHITECTURE_COMPONENT = auto()
    NUMERIC_SOLVER = auto()
    SYMBOLIC_RULE = auto()
    DIAGNOSTIC = auto()
    IR_EXTENSION = auto()


# ─── Plugin Descriptor ──────────────────────────────────────────────────────

@dataclass
class ExperimentalPluginDescriptor:
    """Describes an experimental plugin's metadata and requirements."""
    name: str
    version: str = "0.1.0"
    author: str = ""
    description: str = ""
    stability_level: StabilityLevel = StabilityLevel.ALPHA
    capabilities: list[PluginCapability] = field(default_factory=list)
    dependencies: list[str] = field(default_factory=list)
    required_ir_extensions: list[str] = field(default_factory=list)
    tags: list[str] = field(default_factory=list)

    def __repr__(self) -> str:
        caps = ", ".join(c.name for c in self.capabilities)
        return f"PluginDesc({self.name} v{self.version}, [{caps}])"


# ─── Plugin Interface ───────────────────────────────────────────────────────

class ExperimentalPlugin:
    """
    Base class for experimental plugins.

    Subclass and implement the lifecycle methods:
    - on_register: called when plugin is registered
    - on_activate: called when entering ExperimentalMode
    - on_deactivate: called when leaving ExperimentalMode
    - on_unregister: called when plugin is removed
    """

    def __init__(self) -> None:
        self._descriptor: Optional[ExperimentalPluginDescriptor] = None

    @property
    def descriptor(self) -> ExperimentalPluginDescriptor:
        if self._descriptor is None:
            self._descriptor = self.describe()
        return self._descriptor

    def describe(self) -> ExperimentalPluginDescriptor:
        """Return the plugin descriptor. Override in subclasses."""
        return ExperimentalPluginDescriptor(name=self.__class__.__name__)

    def on_register(self, registry: "PluginRegistry") -> None:
        """Called when the plugin is registered."""
        pass

    def on_activate(self) -> None:
        """Called when experimental mode is entered."""
        pass

    def on_deactivate(self) -> None:
        """Called when experimental mode is exited."""
        pass

    def on_unregister(self) -> None:
        """Called when the plugin is removed."""
        pass

    def get_exports(self) -> dict[str, Any]:
        """
        Return a dict of named exports (classes, functions, etc.)
        that this plugin makes available.
        """
        return {}

    def __repr__(self) -> str:
        return f"Plugin({self.descriptor.name})"


# ─── Plugin Registry ────────────────────────────────────────────────────────

class PluginRegistry:
    """
    Central registry for experimental plugins.

    Handles registration, dependency resolution, activation lifecycle,
    and capability queries.
    """

    def __init__(self) -> None:
        self._plugins: dict[str, ExperimentalPlugin] = {}
        self._active: set[str] = set()
        self._exports: dict[str, dict[str, Any]] = {}

    def register(self, plugin: ExperimentalPlugin) -> None:
        """Register a plugin."""
        desc = plugin.descriptor
        name = desc.name

        if name in self._plugins:
            raise ValueError(f"Plugin '{name}' is already registered")

        # Check dependencies
        for dep in desc.dependencies:
            if dep not in self._plugins:
                raise ValueError(
                    f"Plugin '{name}' requires '{dep}' which is not registered. "
                    f"Register dependencies first."
                )

        self._plugins[name] = plugin
        self._exports[name] = plugin.get_exports()
        plugin.on_register(self)
        logger.info(f"[PluginRegistry] Registered: {name} ({desc.stability_level.name})")

    def unregister(self, name: str) -> None:
        """Unregister a plugin."""
        if name not in self._plugins:
            raise ValueError(f"Plugin '{name}' is not registered")

        # Check if any active plugins depend on this one
        for other_name, other_plugin in self._plugins.items():
            if other_name != name and name in other_plugin.descriptor.dependencies:
                if other_name in self._active:
                    raise ValueError(
                        f"Cannot unregister '{name}': "
                        f"active plugin '{other_name}' depends on it"
                    )

        if name in self._active:
            self.deactivate(name)

        self._plugins[name].on_unregister()
        del self._plugins[name]
        del self._exports[name]
        logger.info(f"[PluginRegistry] Unregistered: {name}")

    def activate(self, name: str) -> None:
        """Activate a plugin (called when entering experimental mode)."""
        if name not in self._plugins:
            raise ValueError(f"Plugin '{name}' is not registered")
        if name in self._active:
            return

        # Activate dependencies first
        plugin = self._plugins[name]
        for dep in plugin.descriptor.dependencies:
            if dep not in self._active:
                self.activate(dep)

        plugin.on_activate()
        self._active.add(name)

    def deactivate(self, name: str) -> None:
        """Deactivate a plugin."""
        if name not in self._active:
            return
        self._plugins[name].on_deactivate()
        self._active.discard(name)

    def activate_all(self) -> None:
        """Activate all registered plugins."""
        for name in self._plugins:
            self.activate(name)

    def deactivate_all(self) -> None:
        """Deactivate all active plugins."""
        for name in list(self._active):
            self.deactivate(name)

    def get_plugin(self, name: str) -> ExperimentalPlugin:
        """Get a plugin by name."""
        if name not in self._plugins:
            raise ValueError(f"Plugin '{name}' is not registered")
        return self._plugins[name]

    def get_exports(self, name: str) -> dict[str, Any]:
        """Get a plugin's exports."""
        if name not in self._exports:
            raise ValueError(f"Plugin '{name}' is not registered")
        return self._exports[name]

    def query_by_capability(
        self, capability: PluginCapability
    ) -> list[ExperimentalPlugin]:
        """Find all plugins that provide a given capability."""
        return [
            p for p in self._plugins.values()
            if capability in p.descriptor.capabilities
        ]

    def query_by_stability(
        self, max_level: StabilityLevel = StabilityLevel.BETA
    ) -> list[ExperimentalPlugin]:
        """Find plugins at or below a given stability level."""
        level_order = {
            StabilityLevel.STABLE: 0,
            StabilityLevel.BETA: 1,
            StabilityLevel.ALPHA: 2,
            StabilityLevel.UNSTABLE: 3,
            StabilityLevel.DANGEROUS: 4,
        }
        max_val = level_order.get(max_level, 4)
        return [
            p for p in self._plugins.values()
            if level_order.get(p.descriptor.stability_level, 4) <= max_val
        ]

    def list_plugins(self) -> list[dict[str, Any]]:
        """List all registered plugins with their status."""
        result = []
        for name, plugin in self._plugins.items():
            desc = plugin.descriptor
            result.append({
                "name": name,
                "version": desc.version,
                "stability": desc.stability_level.name,
                "capabilities": [c.name for c in desc.capabilities],
                "active": name in self._active,
                "dependencies": desc.dependencies,
            })
        return result

    @property
    def n_registered(self) -> int:
        return len(self._plugins)

    @property
    def n_active(self) -> int:
        return len(self._active)

    def __repr__(self) -> str:
        return f"PluginRegistry({self.n_registered} plugins, {self.n_active} active)"


# ─── Global Registry ────────────────────────────────────────────────────────

_global_registry: Optional[PluginRegistry] = None


def get_plugin_registry() -> PluginRegistry:
    """Get the global plugin registry (lazily initialized)."""
    global _global_registry
    if _global_registry is None:
        _global_registry = PluginRegistry()
    return _global_registry


def reset_plugin_registry() -> None:
    """Reset the global plugin registry (for testing)."""
    global _global_registry
    if _global_registry is not None:
        _global_registry.deactivate_all()
    _global_registry = None
