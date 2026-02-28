"""
Grey Physics — IDE (Terminal-Based Interface)

A rich terminal user interface for the physics engine:
  - Dashboard with system overview
  - Model Editor panel
  - Simulation runner
  - Phase-space / field visualiser (text-based)
  - REPL / Console
  - Inspector panel
  - System browser

Built with Rich for immediate cross-platform terminal rendering.
For full GUI, use the optional Textual TUI (requires `pip install textual`).
"""

from __future__ import annotations

import sys
from typing import Any, Callable, Dict, List, Optional, Tuple

try:
    from rich.console import Console
    from rich.table import Table
    from rich.panel import Panel
    from rich.layout import Layout
    from rich.text import Text
    from rich.live import Live
    from rich.progress import Progress, SpinnerColumn, TextColumn
    HAS_RICH = True
except ImportError:
    HAS_RICH = False


class GreyPhysicsIDE:
    """Terminal-based IDE for Grey Physics.

    Provides a REPL-like interface with inspection, visualization,
    and simulation control.
    """

    def __init__(self):
        if HAS_RICH:
            self.console = Console()
        else:
            self.console = None
        self.models: Dict[str, Any] = {}
        self.results: Dict[str, Any] = {}
        self.history: List[str] = []

    def banner(self) -> None:
        """Display startup banner."""
        banner_text = """
╔══════════════════════════════════════════════════════════╗
║                    GREY PHYSICS v0.1.0                  ║
║         Research-Grade Physics & Mathematics Engine     ║
║                                                         ║
║  Domains: Mechanics │ EM │ Fluids │ Quantum │ Relativity║
║           Field Theory │ Chaos │ Experimental           ║
║                                                         ║
║  Type 'help' for commands, 'quit' to exit               ║
╚══════════════════════════════════════════════════════════╝
"""
        if self.console:
            self.console.print(banner_text, style="bold cyan")
        else:
            print(banner_text)

    def show_status(self) -> None:
        """Show system status panel."""
        if not HAS_RICH:
            print(f"Models loaded: {len(self.models)}")
            print(f"Results cached: {len(self.results)}")
            return

        table = Table(title="System Status")
        table.add_column("Property", style="cyan")
        table.add_column("Value", style="green")
        table.add_row("Models loaded", str(len(self.models)))
        table.add_row("Results cached", str(len(self.results)))
        table.add_row("History entries", str(len(self.history)))
        self.console.print(table)

    def show_models(self) -> None:
        """List loaded models."""
        if not self.models:
            self._print("No models loaded.")
            return

        if HAS_RICH:
            table = Table(title="Loaded Models")
            table.add_column("Name", style="cyan")
            table.add_column("Type", style="green")
            table.add_column("Details", style="yellow")
            for name, model in self.models.items():
                table.add_row(name, type(model).__name__,
                              str(getattr(model, 'name', '')))
            self.console.print(table)
        else:
            for name, model in self.models.items():
                print(f"  {name}: {type(model).__name__}")

    def load_model(self, name: str, model: Any) -> None:
        """Load a model into the IDE."""
        self.models[name] = model
        self._print(f"Model '{name}' loaded ({type(model).__name__})")

    def inspect(self, name: str) -> None:
        """Inspect a loaded model or result."""
        obj = self.models.get(name) or self.results.get(name)
        if obj is None:
            self._print(f"'{name}' not found.")
            return

        if HAS_RICH:
            table = Table(title=f"Inspector: {name}")
            table.add_column("Attribute", style="cyan")
            table.add_column("Value", style="green")
            for attr in dir(obj):
                if attr.startswith('_'):
                    continue
                val = getattr(obj, attr, None)
                if callable(val):
                    table.add_row(attr, f"method({len(val.__code__.co_varnames)-1} args)")
                else:
                    val_str = str(val)
                    if len(val_str) > 80:
                        val_str = val_str[:77] + "..."
                    table.add_row(attr, val_str)
            self.console.print(table)
        else:
            print(f"--- {name} ({type(obj).__name__}) ---")
            for attr in dir(obj):
                if not attr.startswith('_'):
                    print(f"  {attr}: {type(getattr(obj, attr, None)).__name__}")

    def run_simulation(self, name: str, **kwargs) -> Any:
        """Run a simulation on a loaded model."""
        model = self.models.get(name)
        if model is None:
            self._print(f"Model '{name}' not found.")
            return None

        if not hasattr(model, 'simulate'):
            self._print(f"Model '{name}' has no simulate() method.")
            return None

        self._print(f"Running simulation on '{name}'...")

        if HAS_RICH:
            with Progress(
                SpinnerColumn(),
                TextColumn("[progress.description]{task.description}"),
                console=self.console,
            ) as progress:
                task = progress.add_task("Simulating...", total=None)
                result = model.simulate(**kwargs)
                progress.update(task, completed=True)
        else:
            result = model.simulate(**kwargs)

        result_name = f"{name}_result"
        self.results[result_name] = result
        self._print(f"Simulation complete. Result stored as '{result_name}'")
        return result

    def show_help(self) -> None:
        """Display help."""
        help_text = {
            "status": "Show system status",
            "models": "List loaded models",
            "load <name> <model>": "Load a model by name",
            "inspect <name>": "Inspect a model or result",
            "run <name> [kwargs]": "Run simulation",
            "results": "List simulation results",
            "help": "Show this help",
            "quit / exit": "Exit the IDE",
        }

        if HAS_RICH:
            table = Table(title="Grey Physics IDE — Commands")
            table.add_column("Command", style="cyan")
            table.add_column("Description", style="green")
            for cmd, desc in help_text.items():
                table.add_row(cmd, desc)
            self.console.print(table)
        else:
            print("Commands:")
            for cmd, desc in help_text.items():
                print(f"  {cmd:30s} {desc}")

    def repl(self) -> None:
        """Start the REPL loop."""
        self.banner()

        while True:
            try:
                if self.console:
                    line = input("grey> ")
                else:
                    line = input("grey> ")
            except (EOFError, KeyboardInterrupt):
                self._print("\nGoodbye.")
                break

            line = line.strip()
            if not line:
                continue

            self.history.append(line)

            if line in ("quit", "exit"):
                self._print("Goodbye.")
                break
            elif line == "help":
                self.show_help()
            elif line == "status":
                self.show_status()
            elif line == "models":
                self.show_models()
            elif line == "results":
                self._show_results()
            elif line.startswith("inspect "):
                self.inspect(line.split(None, 1)[1])
            elif line.startswith("run "):
                self.run_simulation(line.split(None, 1)[1])
            else:
                # Try to evaluate as Python expression in context
                try:
                    result = eval(line, {"models": self.models,
                                         "results": self.results})
                    self._print(str(result))
                except Exception as e:
                    self._print(f"Error: {e}")

    def _show_results(self) -> None:
        if not self.results:
            self._print("No results.")
            return
        for name, result in self.results.items():
            rtype = type(result).__name__
            if isinstance(result, dict):
                keys = list(result.keys())
                self._print(f"  {name}: dict with keys {keys}")
            else:
                self._print(f"  {name}: {rtype}")

    def _print(self, msg: str) -> None:
        if self.console:
            self.console.print(msg)
        else:
            print(msg)


# ============================================================
# Visualization helpers (text-based)
# ============================================================

class TextVisualizer:
    """Simple text-based visualizations for terminal output."""

    @staticmethod
    def sparkline(data: list, width: int = 40) -> str:
        """Unicode sparkline from data."""
        blocks = " ▁▂▃▄▅▆▇█"
        if not data:
            return ""
        mn, mx = min(data), max(data)
        rng = mx - mn if mx != mn else 1
        return "".join(
            blocks[int((v - mn) / rng * 8)] for v in data[:width]
        )

    @staticmethod
    def ascii_plot(x: list, y: list,
                   width: int = 60, height: int = 20,
                   title: str = "") -> str:
        """Simple ASCII scatter/line plot."""
        if not x or not y:
            return ""

        x_min, x_max = min(x), max(x)
        y_min, y_max = min(y), max(y)
        x_range = x_max - x_min if x_max != x_min else 1
        y_range = y_max - y_min if y_max != y_min else 1

        canvas = [[' '] * width for _ in range(height)]

        for xi, yi in zip(x, y):
            col = int((xi - x_min) / x_range * (width - 1))
            row = height - 1 - int((yi - y_min) / y_range * (height - 1))
            col = max(0, min(width - 1, col))
            row = max(0, min(height - 1, row))
            canvas[row][col] = '●'

        lines = []
        if title:
            lines.append(title.center(width + 6))
        lines.append(f"{y_max:8.3g} ┤{''.join(canvas[0])}")
        for i in range(1, height - 1):
            lines.append(f"{'':8s} │{''.join(canvas[i])}")
        lines.append(f"{y_min:8.3g} ┤{''.join(canvas[-1])}")
        lines.append(f"{'':8s} └{'─' * width}")
        lines.append(f"{x_min:8.3g}{' ' * (width - 16)}{x_max:8.3g}")

        return "\n".join(lines)

    @staticmethod
    def heatmap(data, width: int = 40, height: int = 20) -> str:
        """Simple text heatmap using Unicode block characters."""
        import numpy as np
        arr = np.asarray(data)
        blocks = " ░▒▓█"

        # Downsample
        if arr.shape[0] > height or arr.shape[1] > width:
            from numpy import ndarray
            ry = max(1, arr.shape[0] // height)
            rx = max(1, arr.shape[1] // width)
            arr = arr[::ry, ::rx][:height, :width]

        mn, mx = arr.min(), arr.max()
        rng = mx - mn if mx != mn else 1

        lines = []
        for row in arr:
            line = ""
            for val in row:
                idx = int((val - mn) / rng * (len(blocks) - 1))
                line += blocks[idx]
            lines.append(line)

        return "\n".join(lines)


def main():
    """Entry point for the Grey Physics IDE."""
    ide = GreyPhysicsIDE()
    ide.repl()


if __name__ == "__main__":
    main()
