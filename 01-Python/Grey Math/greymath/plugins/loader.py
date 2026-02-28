"""
Grey Math — Plugin Loader.

Discovers and loads plugins from:
- Entry points (setuptools/pyproject.toml)
- Plugin directories  
- Explicit module paths
"""

from __future__ import annotations

import importlib
import inspect
import logging
import sys
from pathlib import Path
from typing import Any

from greymath.plugins.base import Plugin, PluginManager

logger = logging.getLogger(__name__)


class PluginLoader:
    """
    Discovers and loads Grey Math plugins from various sources.
    """

    def __init__(self, manager: PluginManager) -> None:
        self._manager = manager

    def load_from_module(self, module_path: str) -> list[str]:
        """
        Load plugins from a Python module path.
        
        Scans the module for classes that subclass Plugin and
        registers them with the plugin manager.
        
        Returns list of plugin names loaded.
        """
        loaded: list[str] = []
        try:
            module = importlib.import_module(module_path)
        except ImportError as e:
            logger.error(f"Failed to import plugin module {module_path}: {e}")
            return loaded

        for name, obj in inspect.getmembers(module, inspect.isclass):
            if issubclass(obj, Plugin) and obj is not Plugin:
                try:
                    instance = obj()
                    self._manager.register(instance)
                    loaded.append(instance.metadata.name)
                except Exception as e:
                    logger.error(f"Failed to register plugin {name}: {e}")

        return loaded

    def load_from_directory(self, directory: str | Path) -> list[str]:
        """
        Scan a directory for Python files containing plugins.
        
        Each .py file is imported and scanned for Plugin subclasses.
        """
        loaded: list[str] = []
        dir_path = Path(directory)

        if not dir_path.is_dir():
            logger.warning(f"Plugin directory not found: {dir_path}")
            return loaded

        # Add directory to sys.path temporarily
        str_path = str(dir_path)
        if str_path not in sys.path:
            sys.path.insert(0, str_path)

        for py_file in sorted(dir_path.glob("*.py")):
            if py_file.name.startswith("_"):
                continue

            module_name = py_file.stem
            try:
                module = importlib.import_module(module_name)
                for name, obj in inspect.getmembers(module, inspect.isclass):
                    if issubclass(obj, Plugin) and obj is not Plugin:
                        try:
                            instance = obj()
                            self._manager.register(instance)
                            loaded.append(instance.metadata.name)
                        except Exception as e:
                            logger.error(f"Failed to register plugin {name}: {e}")
            except Exception as e:
                logger.error(f"Failed to load plugin file {py_file}: {e}")

        return loaded

    def load_from_entry_points(self, group: str = "greymath.plugins") -> list[str]:
        """
        Discover plugins registered as package entry points.
        
        Plugins register in pyproject.toml:
            [project.entry-points."greymath.plugins"]
            my_plugin = "my_package.plugin:MyPlugin"
        """
        loaded: list[str] = []

        try:
            if sys.version_info >= (3, 12):
                from importlib.metadata import entry_points
                eps = entry_points(group=group)
            else:
                from importlib.metadata import entry_points
                all_eps = entry_points()
                eps = all_eps.get(group, [])  # type: ignore
        except Exception as e:
            logger.error(f"Failed to read entry points: {e}")
            return loaded

        for ep in eps:
            try:
                plugin_cls = ep.load()
                if inspect.isclass(plugin_cls) and issubclass(plugin_cls, Plugin):
                    instance = plugin_cls()
                    self._manager.register(instance)
                    loaded.append(instance.metadata.name)
                    logger.info(f"Loaded plugin from entry point: {ep.name}")
            except Exception as e:
                logger.error(f"Failed to load entry point {ep.name}: {e}")

        return loaded

    def load_from_config(self, config: dict[str, Any]) -> list[str]:
        """
        Load plugins specified in a configuration dict.
        
        Config format:
            {
                "plugins": [
                    {"module": "my_package.plugin"},
                    {"directory": "/path/to/plugins/"},
                    {"entry_points": "greymath.plugins"},
                ]
            }
        """
        loaded: list[str] = []

        for spec in config.get("plugins", []):
            if "module" in spec:
                loaded.extend(self.load_from_module(spec["module"]))
            elif "directory" in spec:
                loaded.extend(self.load_from_directory(spec["directory"]))
            elif "entry_points" in spec:
                loaded.extend(self.load_from_entry_points(spec["entry_points"]))

        return loaded

    def reload_plugin(self, name: str) -> bool:
        """
        Hot-reload a plugin by unloading and reloading it.
        """
        plugin = self._manager.get_plugin(name)
        if plugin is None:
            logger.error(f"Plugin not found: {name}")
            return False

        try:
            self._manager.unload(name)

            # Get the module the plugin was defined in
            module = inspect.getmodule(type(plugin))
            if module is not None:
                importlib.reload(module)

                # Find the updated class
                for cls_name, obj in inspect.getmembers(module, inspect.isclass):
                    if issubclass(obj, Plugin) and obj is not Plugin:
                        new_instance = obj()
                        if new_instance.metadata.name == name:
                            # Re-register
                            self._manager._plugins[name] = new_instance
                            self._manager.load(name)
                            logger.info(f"Plugin hot-reloaded: {name}")
                            return True

            logger.error(f"Could not find plugin class after reload: {name}")
            return False

        except Exception as e:
            logger.error(f"Failed to reload plugin {name}: {e}")
            return False
