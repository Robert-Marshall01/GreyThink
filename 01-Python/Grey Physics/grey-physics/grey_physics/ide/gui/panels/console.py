"""
Grey Physics GUI — Interactive Console Panel

An interactive Python REPL with the grey_physics environment pre-loaded.
Allows direct interaction with the physics engine, creating models,
running computations, and inspecting objects.
"""

from __future__ import annotations

import code
import io
import sys
import traceback
import tkinter as tk
from typing import Any, Dict

from grey_physics.ide.gui import theme as T
from grey_physics.ide.gui.widgets import StyledFrame, PanelFrame


BANNER = """\
Grey Physics Interactive Console
Python {pyver} | Grey Physics {gpver}
──────────────────────────────────────────
Loaded modules:
  np          → numpy
  ir          → grey_physics.core.ir
  symbolic    → grey_physics.core.symbolic
  numeric     → grey_physics.core.numeric
  domains     → grey_physics.domains

Type expressions to evaluate. Use ↑/↓ for history.
"""


class ConsolePanel(StyledFrame):
    """Interactive Python console with grey_physics pre-loaded."""

    def __init__(self, parent: tk.Widget, app: Any):
        super().__init__(parent)
        self.app = app
        self._history: list[str] = []
        self._history_idx: int = -1
        self._namespace: Dict[str, Any] = {}
        self._build_ui()
        self._init_namespace()
        self._print_banner()

    def _build_ui(self) -> None:
        # ── Output area ───────────────────────────────────────────
        output_frame = StyledFrame(self)
        output_frame.pack(fill="both", expand=True, pady=(0, T.PAD_SMALL))

        self._output = tk.Text(
            output_frame,
            bg="#11111b", fg=T.FG_PRIMARY,
            insertbackground=T.FG_PRIMARY,
            font=T.FONT_MONO, relief="flat",
            highlightthickness=0,
            selectbackground=T.HIGHLIGHT,
            selectforeground=T.FG_PRIMARY,
            wrap="word", state="disabled",
        )
        scrollbar = tk.Scrollbar(output_frame, command=self._output.yview,
                                 bg=T.BG_TERTIARY, troughcolor="#11111b",
                                 highlightthickness=0)
        self._output.configure(yscrollcommand=scrollbar.set)
        self._output.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

        # Tag styles
        self._output.tag_configure("prompt", foreground=T.ACCENT)
        self._output.tag_configure("input", foreground=T.FG_PRIMARY)
        self._output.tag_configure("output", foreground=T.SUCCESS)
        self._output.tag_configure("error", foreground=T.ERROR)
        self._output.tag_configure("banner", foreground=T.FG_MUTED)

        # ── Input area ────────────────────────────────────────────
        input_frame = StyledFrame(self, bg="#11111b")
        input_frame.pack(fill="x")

        prompt_label = tk.Label(input_frame, text=">>> ", bg="#11111b",
                                fg=T.ACCENT, font=T.FONT_MONO)
        prompt_label.pack(side="left")

        self._input = tk.Entry(
            input_frame, bg="#11111b", fg=T.FG_PRIMARY,
            insertbackground=T.FG_PRIMARY, font=T.FONT_MONO,
            relief="flat", highlightthickness=0,
        )
        self._input.pack(side="left", fill="x", expand=True)
        self._input.bind("<Return>", self._on_enter)
        self._input.bind("<Up>", self._on_history_up)
        self._input.bind("<Down>", self._on_history_down)
        self._input.focus_set()

    def _init_namespace(self) -> None:
        """Set up the console namespace with grey_physics imports."""
        ns: Dict[str, Any] = {"__name__": "__console__"}

        try:
            import numpy as np_
            ns["np"] = np_
            ns["numpy"] = np_
        except ImportError:
            pass

        try:
            import grey_physics
            ns["grey_physics"] = grey_physics
        except ImportError:
            pass

        try:
            from grey_physics.core import ir
            ns["ir"] = ir
            # Import commonly used IR types directly
            from grey_physics.core.ir import (
                ScalarField, VectorField, TensorField,
                Spacetime, Metric, Lagrangian, Hamiltonian,
            )
            ns["ScalarField"] = ScalarField
            ns["VectorField"] = VectorField
            ns["TensorField"] = TensorField
            ns["Spacetime"] = Spacetime
            ns["Metric"] = Metric
            ns["Lagrangian"] = Lagrangian
            ns["Hamiltonian"] = Hamiltonian
        except ImportError:
            pass

        try:
            from grey_physics.core import symbolic
            ns["symbolic"] = symbolic
            from grey_physics.core.symbolic import SymbolicEngine
            ns["SymbolicEngine"] = SymbolicEngine
        except ImportError:
            pass

        try:
            from grey_physics.core import numeric
            ns["numeric"] = numeric
            from grey_physics.core.numeric import ODESolver
            ns["ODESolver"] = ODESolver
        except ImportError:
            pass

        try:
            from grey_physics import domains
            ns["domains"] = domains
        except ImportError:
            pass

        # Provide access to app models and results
        ns["models"] = self.app.models
        ns["results"] = self.app.results

        self._namespace = ns

    def _print_banner(self) -> None:
        import grey_physics
        banner = BANNER.format(
            pyver=sys.version.split()[0],
            gpver=grey_physics.__version__,
        )
        self._append_output(banner, "banner")

    def _on_enter(self, event: Any) -> None:
        line = self._input.get().rstrip()
        self._input.delete(0, "end")

        if not line:
            self._append_output(">>> \n", "prompt")
            return

        # Add to history
        if not self._history or self._history[-1] != line:
            self._history.append(line)
        self._history_idx = -1

        # Show the input
        self._append_output(">>> ", "prompt")
        self._append_output(line + "\n", "input")

        # Execute
        self._execute(line)

    def _on_history_up(self, event: Any) -> None:
        if not self._history:
            return
        if self._history_idx == -1:
            self._history_idx = len(self._history) - 1
        elif self._history_idx > 0:
            self._history_idx -= 1

        self._input.delete(0, "end")
        self._input.insert(0, self._history[self._history_idx])

    def _on_history_down(self, event: Any) -> None:
        if not self._history:
            return
        if self._history_idx == -1:
            return
        if self._history_idx < len(self._history) - 1:
            self._history_idx += 1
            self._input.delete(0, "end")
            self._input.insert(0, self._history[self._history_idx])
        else:
            self._history_idx = -1
            self._input.delete(0, "end")

    def _execute(self, source: str) -> None:
        """Execute a line of Python in the console namespace."""
        stdout_capture = io.StringIO()
        stderr_capture = io.StringIO()

        old_stdout = sys.stdout
        old_stderr = sys.stderr

        try:
            sys.stdout = stdout_capture
            sys.stderr = stderr_capture

            # Try eval first (expression → show result)
            try:
                compiled = compile(source, "<console>", "eval")
                result = eval(compiled, self._namespace)
                if result is not None:
                    # Capture the repr output
                    result_str = repr(result)
                    self._namespace["_"] = result
            except SyntaxError:
                # Fall back to exec (statement)
                result = None
                result_str = None
                compiled = compile(source, "<console>", "exec")
                exec(compiled, self._namespace)

        except Exception:
            result_str = None
            traceback.print_exc(file=stderr_capture)

        finally:
            sys.stdout = old_stdout
            sys.stderr = old_stderr

        # Show stdout
        stdout_text = stdout_capture.getvalue()
        if stdout_text:
            self._append_output(stdout_text, "output")

        # Show eval result
        if result_str is not None:
            self._append_output(result_str + "\n", "output")

        # Show errors
        stderr_text = stderr_capture.getvalue()
        if stderr_text:
            self._append_output(stderr_text, "error")

    def _append_output(self, text: str, tag: str = "output") -> None:
        self._output.configure(state="normal")
        self._output.insert("end", text, tag)
        self._output.see("end")
        self._output.configure(state="disabled")
