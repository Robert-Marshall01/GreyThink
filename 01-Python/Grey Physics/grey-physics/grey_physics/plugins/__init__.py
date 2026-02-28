"""
Grey Physics — Plugin System

Extensible plugin architecture:
  - Register new field types, operators, domains, solvers
  - Hook system for pre/post processing
  - Plugin discovery and loading
  - Dependency resolution
"""

from __future__ import annotations

import importlib
import inspect
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Type


class PluginMeta:
    """Metadata for a plugin."""

    def __init__(self, name: str, version: str = "0.1.0",
                 author: str = "", description: str = "",
                 dependencies: Optional[List[str]] = None):
        self.name = name
        self.version = version
        self.author = author
        self.description = description
        self.dependencies = dependencies or []


class PluginBase:
    """Base class for all Grey Physics plugins."""

    meta: PluginMeta = PluginMeta("unnamed")

    def on_load(self) -> None:
        """Called when the plugin is loaded."""
        pass

    def on_unload(self) -> None:
        """Called when the plugin is unloaded."""
        pass


class HookRegistry:
    """Registry for plugin hooks — allows plugins to inject behaviour."""

    def __init__(self):
        self._hooks: Dict[str, List[Callable]] = {}

    def register(self, hook_name: str, callback: Callable) -> None:
        """Register a callback for a named hook."""
        if hook_name not in self._hooks:
            self._hooks[hook_name] = []
        self._hooks[hook_name].append(callback)

    def unregister(self, hook_name: str, callback: Callable) -> None:
        if hook_name in self._hooks:
            self._hooks[hook_name] = [
                cb for cb in self._hooks[hook_name] if cb is not callback
            ]

    def trigger(self, hook_name: str, *args, **kwargs) -> List[Any]:
        """Trigger all callbacks for a hook. Returns list of results."""
        results = []
        for cb in self._hooks.get(hook_name, []):
            results.append(cb(*args, **kwargs))
        return results

    def available_hooks(self) -> List[str]:
        return list(self._hooks.keys())


class ComponentRegistry:
    """Registry for domain components (solvers, fields, operators)."""

    def __init__(self):
        self._components: Dict[str, Dict[str, Any]] = {
            "solver": {},
            "field_type": {},
            "operator": {},
            "domain": {},
            "visualization": {},
        }

    def register(self, category: str, name: str, component: Any) -> None:
        if category not in self._components:
            self._components[category] = {}
        self._components[category][name] = component

    def get(self, category: str, name: str) -> Any:
        return self._components.get(category, {}).get(name)

    def list_category(self, category: str) -> List[str]:
        return list(self._components.get(category, {}).keys())

    def list_categories(self) -> List[str]:
        return list(self._components.keys())


class PluginManager:
    """Manages plugin lifecycle and dependencies."""

    def __init__(self):
        self.plugins: Dict[str, PluginBase] = {}
        self.hooks = HookRegistry()
        self.components = ComponentRegistry()
        self._load_order: List[str] = []

    def register_plugin(self, plugin: PluginBase) -> None:
        """Register and load a plugin instance."""
        name = plugin.meta.name

        # Check dependencies
        for dep in plugin.meta.dependencies:
            if dep not in self.plugins:
                raise RuntimeError(
                    f"Plugin '{name}' requires '{dep}' which is not loaded"
                )

        self.plugins[name] = plugin
        self._load_order.append(name)
        plugin.on_load()

    def unregister_plugin(self, name: str) -> None:
        """Unregister and unload a plugin."""
        if name in self.plugins:
            # Check no other plugin depends on this
            for other_name, other_plugin in self.plugins.items():
                if name in other_plugin.meta.dependencies:
                    raise RuntimeError(
                        f"Cannot unload '{name}': required by '{other_name}'"
                    )
            self.plugins[name].on_unload()
            del self.plugins[name]
            self._load_order.remove(name)

    def load_from_module(self, module_path: str) -> None:
        """Load plugins from a Python module path.

        The module should define a class inheriting from PluginBase.
        """
        module = importlib.import_module(module_path)
        for name, obj in inspect.getmembers(module, inspect.isclass):
            if issubclass(obj, PluginBase) and obj is not PluginBase:
                instance = obj()
                self.register_plugin(instance)

    def load_from_directory(self, directory: str) -> None:
        """Load all plugins from a directory of .py files."""
        path = Path(directory)
        if not path.exists():
            return
        for py_file in sorted(path.glob("*.py")):
            if py_file.name.startswith("_"):
                continue
            spec = importlib.util.spec_from_file_location(
                py_file.stem, str(py_file)
            )
            if spec and spec.loader:
                module = importlib.util.module_from_spec(spec)
                try:
                    spec.loader.exec_module(module)
                    for name, obj in inspect.getmembers(module, inspect.isclass):
                        if issubclass(obj, PluginBase) and obj is not PluginBase:
                            instance = obj()
                            self.register_plugin(instance)
                except Exception as e:
                    print(f"Warning: Failed to load plugin from {py_file}: {e}")

    def list_plugins(self) -> List[Dict[str, str]]:
        return [
            {
                "name": p.meta.name,
                "version": p.meta.version,
                "author": p.meta.author,
                "description": p.meta.description,
            }
            for p in self.plugins.values()
        ]


# ============================================================
# Built-in hooks
# ============================================================

HOOK_PRE_SIMULATE = "pre_simulate"
HOOK_POST_SIMULATE = "post_simulate"
HOOK_PRE_SOLVE = "pre_solve"
HOOK_POST_SOLVE = "post_solve"
HOOK_ON_ERROR = "on_error"
HOOK_ON_ENERGY_DRIFT = "on_energy_drift"


# ============================================================
# Example plugin template
# ============================================================

class ExamplePlugin(PluginBase):
    """Example plugin demonstrating the plugin API."""

    meta = PluginMeta(
        name="example_plugin",
        version="0.1.0",
        author="Grey Physics",
        description="Example plugin showing the plugin architecture.",
    )

    def on_load(self) -> None:
        print(f"[{self.meta.name}] Loaded.")

    def on_unload(self) -> None:
        print(f"[{self.meta.name}] Unloaded.")
