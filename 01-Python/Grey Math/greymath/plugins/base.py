"""
Grey Math — Plugin Base Classes and Registry.

Defines the plugin protocol, metadata, capabilities, and lifecycle management.
"""

from __future__ import annotations

import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional, Protocol, runtime_checkable

logger = logging.getLogger(__name__)


class PluginType(Enum):
    """Categories of plugins."""
    IR_TYPE = auto()          # New mathematical types
    OPERATOR = auto()         # New expression operators
    SOLVER = auto()           # Numeric/symbolic solvers
    REWRITE_RULES = auto()    # Rewrite rule packs
    DOMAIN = auto()           # Domain module extensions
    VISUALIZER = auto()       # Visualization backends
    EXPORTER = auto()         # Export formats (LaTeX, Lean, Coq)
    DATA_SOURCE = auto()      # External data connectors
    TRANSFORMER = auto()      # Expression transformers


class PluginCapability(Enum):
    """Fine-grained capabilities a plugin can declare."""
    SYMBOLIC_SIMPLIFY = auto()
    SYMBOLIC_DIFFERENTIATE = auto()
    NUMERIC_SOLVE = auto()
    NUMERIC_OPTIMIZE = auto()
    MATRIX_DECOMPOSE = auto()
    ODE_INTEGRATE = auto()
    STOCHASTIC_SAMPLE = auto()
    VISUALIZE_2D = auto()
    VISUALIZE_3D = auto()
    EXPORT_LATEX = auto()
    EXPORT_LEAN = auto()
    EXPORT_COQ = auto()
    CUSTOM_TYPE = auto()
    REWRITE_RULE = auto()


@dataclass
class PluginMetadata:
    """Metadata describing a plugin."""
    name: str
    version: str
    author: str = ""
    description: str = ""
    plugin_type: PluginType = PluginType.DOMAIN
    capabilities: list[PluginCapability] = field(default_factory=list)
    dependencies: list[str] = field(default_factory=list)  # Other plugin names
    python_requires: str = ">=3.11"
    tags: list[str] = field(default_factory=list)
    homepage: str = ""
    license: str = ""


@runtime_checkable
class PluginHooks(Protocol):
    """Protocol for optional lifecycle hooks."""

    def on_load(self) -> None: ...
    def on_unload(self) -> None: ...
    def on_session_start(self, session_id: str) -> None: ...
    def on_session_end(self, session_id: str) -> None: ...


class Plugin(ABC):
    """
    Base class for all Grey Math plugins.

    Subclass this to create a plugin. Implement `setup()` to register
    types, rules, solvers, etc. with the host system.
    """

    def __init__(self) -> None:
        self._enabled: bool = True
        self._loaded: bool = False

    @property
    @abstractmethod
    def metadata(self) -> PluginMetadata:
        """Return plugin metadata."""
        ...

    @abstractmethod
    def setup(self, host: "PluginHost") -> None:
        """
        Called when the plugin is loaded.
        Use the host object to register types, rules, solvers, etc.
        """
        ...

    def teardown(self) -> None:
        """Called when the plugin is unloaded. Override for cleanup."""
        pass

    @property
    def enabled(self) -> bool:
        return self._enabled

    def enable(self) -> None:
        self._enabled = True

    def disable(self) -> None:
        self._enabled = False

    def on_load(self) -> None:
        """Hook called after plugin is loaded."""
        pass

    def on_unload(self) -> None:
        """Hook called before plugin is unloaded."""
        pass

    def on_session_start(self, session_id: str) -> None:
        """Hook called when an evaluation session starts."""
        pass

    def on_session_end(self, session_id: str) -> None:
        """Hook called when an evaluation session ends."""
        pass


class PluginHost:
    """
    Interface provided to plugins during setup for registering extensions.
    Acts as the bridge between plugins and the core system.
    """

    def __init__(self) -> None:
        self._types: dict[str, type] = {}
        self._operators: dict[str, Callable] = {}
        self._rewrite_rules: list[Any] = []
        self._solvers: dict[str, Callable] = {}
        self._visualizers: dict[str, Callable] = {}
        self._exporters: dict[str, Callable] = {}
        self._functions: dict[str, Callable] = {}
        self._hooks: dict[str, list[Callable]] = {}

    def register_type(self, name: str, type_cls: type) -> None:
        """Register a new IR type."""
        self._types[name] = type_cls
        logger.info(f"Plugin registered type: {name}")

    def register_operator(self, name: str, handler: Callable) -> None:
        """Register a new expression operator."""
        self._operators[name] = handler
        logger.info(f"Plugin registered operator: {name}")

    def register_rewrite_rule(self, rule: Any) -> None:
        """Register a rewrite rule."""
        self._rewrite_rules.append(rule)
        logger.info(f"Plugin registered rewrite rule")

    def register_solver(self, name: str, solver: Callable) -> None:
        """Register a solver function."""
        self._solvers[name] = solver
        logger.info(f"Plugin registered solver: {name}")

    def register_visualizer(self, name: str, visualizer: Callable) -> None:
        """Register a visualization backend."""
        self._visualizers[name] = visualizer
        logger.info(f"Plugin registered visualizer: {name}")

    def register_exporter(self, name: str, exporter: Callable) -> None:
        """Register an export format handler."""
        self._exporters[name] = exporter
        logger.info(f"Plugin registered exporter: {name}")

    def register_function(self, name: str, func: Callable) -> None:
        """Register a function available in the evaluation environment."""
        self._functions[name] = func
        logger.info(f"Plugin registered function: {name}")

    def register_hook(self, event: str, handler: Callable) -> None:
        """Register a hook for a lifecycle event."""
        self._hooks.setdefault(event, []).append(handler)

    @property
    def types(self) -> dict[str, type]:
        return dict(self._types)

    @property
    def operators(self) -> dict[str, Callable]:
        return dict(self._operators)

    @property
    def rewrite_rules(self) -> list:
        return list(self._rewrite_rules)

    @property
    def solvers(self) -> dict[str, Callable]:
        return dict(self._solvers)

    @property
    def functions(self) -> dict[str, Callable]:
        return dict(self._functions)


class PluginManager:
    """
    Manages plugin lifecycle: discovery, loading, dependency resolution,
    enabling/disabling, and unloading.
    """

    def __init__(self) -> None:
        self._plugins: dict[str, Plugin] = {}
        self._load_order: list[str] = []
        self._host = PluginHost()

    @property
    def host(self) -> PluginHost:
        return self._host

    def register(self, plugin: Plugin) -> None:
        """Register a plugin instance."""
        meta = plugin.metadata
        name = meta.name

        if name in self._plugins:
            raise ValueError(f"Plugin already registered: {name}")

        # Check dependencies
        for dep in meta.dependencies:
            if dep not in self._plugins:
                raise ValueError(
                    f"Plugin '{name}' requires '{dep}' which is not loaded"
                )

        self._plugins[name] = plugin
        logger.info(f"Plugin registered: {name} v{meta.version}")

    def load(self, name: str) -> None:
        """Load and initialize a registered plugin."""
        plugin = self._plugins.get(name)
        if plugin is None:
            raise ValueError(f"Plugin not found: {name}")

        if plugin._loaded:
            logger.warning(f"Plugin already loaded: {name}")
            return

        # Ensure dependencies are loaded
        for dep in plugin.metadata.dependencies:
            dep_plugin = self._plugins.get(dep)
            if dep_plugin and not dep_plugin._loaded:
                self.load(dep)

        plugin.setup(self._host)
        plugin._loaded = True
        plugin.on_load()
        self._load_order.append(name)
        logger.info(f"Plugin loaded: {name}")

    def unload(self, name: str) -> None:
        """Unload a plugin."""
        plugin = self._plugins.get(name)
        if plugin is None:
            return

        # Check for dependents
        for other_name, other_plugin in self._plugins.items():
            if other_name != name and other_plugin._loaded:
                if name in other_plugin.metadata.dependencies:
                    raise ValueError(
                        f"Cannot unload '{name}': plugin '{other_name}' depends on it"
                    )

        plugin.on_unload()
        plugin.teardown()
        plugin._loaded = False
        if name in self._load_order:
            self._load_order.remove(name)
        logger.info(f"Plugin unloaded: {name}")

    def load_all(self) -> None:
        """Load all registered plugins in dependency order."""
        loaded = set()
        to_load = list(self._plugins.keys())

        while to_load:
            progress = False
            for name in list(to_load):
                plugin = self._plugins[name]
                deps = set(plugin.metadata.dependencies)
                if deps.issubset(loaded):
                    self.load(name)
                    loaded.add(name)
                    to_load.remove(name)
                    progress = True

            if not progress:
                raise ValueError(
                    f"Circular dependency detected among: {to_load}"
                )

    def unload_all(self) -> None:
        """Unload all plugins in reverse order."""
        for name in reversed(self._load_order[:]):
            self.unload(name)

    def get_plugin(self, name: str) -> Plugin | None:
        return self._plugins.get(name)

    def list_plugins(self) -> list[PluginMetadata]:
        return [p.metadata for p in self._plugins.values()]

    def list_loaded(self) -> list[str]:
        return [n for n, p in self._plugins.items() if p._loaded]

    def list_capabilities(self) -> dict[str, list[PluginCapability]]:
        return {
            name: plugin.metadata.capabilities
            for name, plugin in self._plugins.items()
            if plugin._loaded
        }

    def find_by_capability(self, cap: PluginCapability) -> list[Plugin]:
        return [
            p for p in self._plugins.values()
            if p._loaded and cap in p.metadata.capabilities
        ]

    def notify_session_start(self, session_id: str) -> None:
        for name in self._load_order:
            self._plugins[name].on_session_start(session_id)

    def notify_session_end(self, session_id: str) -> None:
        for name in reversed(self._load_order):
            self._plugins[name].on_session_end(session_id)
