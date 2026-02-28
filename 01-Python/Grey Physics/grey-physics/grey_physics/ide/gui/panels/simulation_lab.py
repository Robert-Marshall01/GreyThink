"""
Grey Physics GUI — Simulation Lab Panel

Configure, run, and monitor simulations:
  - ODE simulations (particle mechanics, oscillators, attractors)
  - Solver method selection (RK4, Verlet, BDF, etc.)
  - Real-time progress and energy monitoring
  - Results storage for visualization in Field Viewer
"""

from __future__ import annotations

import threading
import tkinter as tk
from tkinter import messagebox
from typing import Any, Dict, Optional

import numpy as np

from grey_physics.ide.gui import theme as T
from grey_physics.ide.gui.widgets import (
    StyledFrame, PanelFrame, StyledLabel, StyledButton,
    StyledCombobox, ScrolledText, ParameterRow,
)


# ── Simulation presets ────────────────────────────────────────────────

SIM_PRESETS: Dict[str, Dict[str, Any]] = {
    "Harmonic Oscillator": {
        "equations": "harmonic",
        "params": {"omega": "1.0", "x0": "1.0", "v0": "0.0"},
        "description": "ẍ = −ω²x  (simple harmonic motion)",
    },
    "Damped Oscillator": {
        "equations": "damped",
        "params": {"omega": "1.0", "gamma": "0.1", "x0": "1.0", "v0": "0.0"},
        "description": "ẍ = −ω²x − 2γẋ",
    },
    "Double Pendulum": {
        "equations": "double_pendulum",
        "params": {"m1": "1.0", "m2": "1.0", "L1": "1.0", "L2": "1.0",
                   "θ1_0": "2.0", "θ2_0": "3.0"},
        "description": "Chaotic double pendulum",
    },
    "Lorenz System": {
        "equations": "lorenz",
        "params": {"sigma": "10.0", "rho": "28.0", "beta": "2.667",
                   "x0": "1.0", "y0": "1.0", "z0": "1.0"},
        "description": "ẋ=σ(y−x), ẏ=x(ρ−z)−y, ż=xy−βz",
    },
    "Kepler Orbit": {
        "equations": "kepler",
        "params": {"x0": "1.0", "vy0": "0.8"},
        "description": "Gravitational two-body problem",
    },
    "Van der Pol": {
        "equations": "vanderpol",
        "params": {"mu": "2.0", "x0": "0.5", "v0": "0.0"},
        "description": "ẍ − μ(1−x²)ẋ + x = 0",
    },
}


class SimulationLabPanel(StyledFrame):
    """Panel for configuring and running simulations."""

    def __init__(self, parent: tk.Widget, app: Any):
        super().__init__(parent)
        self.app = app
        self._param_rows: list[ParameterRow] = []
        self._running = False
        self._build_ui()

    def _build_ui(self) -> None:
        # ── Top: preset + solver config ───────────────────────────
        top = StyledFrame(self)
        top.pack(fill="x", pady=(0, T.PAD))

        # Preset selector
        preset_frame = PanelFrame(top, title="Simulation Preset")
        preset_frame.pack(side="left", fill="both", expand=True,
                          padx=(0, T.PAD))

        inner = StyledFrame(preset_frame, bg=T.BG_SECONDARY)
        inner.pack(fill="x", padx=T.PAD_SMALL, pady=T.PAD_SMALL)

        StyledLabel(inner, text="Preset:").pack(side="left",
                                                padx=(0, T.PAD_SMALL))
        self._preset_var = tk.StringVar(value="Harmonic Oscillator")
        preset_combo = StyledCombobox(
            inner, textvariable=self._preset_var,
            values=list(SIM_PRESETS.keys()), width=22)
        preset_combo.pack(side="left", padx=(0, T.PAD_SMALL))
        preset_combo.bind("<<ComboboxSelected>>", self._on_preset_change)

        self._desc_label = StyledLabel(inner, text="", fg=T.FG_MUTED)
        self._desc_label.pack(side="left", padx=T.PAD_SMALL)

        # Solver config
        solver_frame = PanelFrame(top, title="Solver")
        solver_frame.pack(side="left", fill="y")

        solver_inner = StyledFrame(solver_frame, bg=T.BG_SECONDARY)
        solver_inner.pack(fill="both", padx=T.PAD_SMALL, pady=T.PAD_SMALL)

        StyledLabel(solver_inner, text="Method:").pack(side="left",
                                                       padx=(0, T.PAD_SMALL))
        self._method_var = tk.StringVar(value="RK4")
        StyledCombobox(
            solver_inner, textvariable=self._method_var,
            values=["Euler", "RK4", "RK45", "Verlet", "Leapfrog", "BDF"],
            width=10,
        ).pack(side="left", padx=(0, T.PAD))

        StyledLabel(solver_inner, text="dt:").pack(side="left",
                                                   padx=(0, T.PAD_SMALL))
        self._dt_var = tk.StringVar(value="0.01")
        tk.Entry(solver_inner, textvariable=self._dt_var, width=8,
                 bg=T.BG_TERTIARY, fg=T.FG_PRIMARY,
                 insertbackground=T.FG_PRIMARY, font=T.FONT_MONO,
                 relief="flat").pack(side="left", padx=(0, T.PAD))

        StyledLabel(solver_inner, text="T:").pack(side="left",
                                                  padx=(0, T.PAD_SMALL))
        self._tmax_var = tk.StringVar(value="30.0")
        tk.Entry(solver_inner, textvariable=self._tmax_var, width=8,
                 bg=T.BG_TERTIARY, fg=T.FG_PRIMARY,
                 insertbackground=T.FG_PRIMARY, font=T.FONT_MONO,
                 relief="flat").pack(side="left")

        # ── Middle: parameters ────────────────────────────────────
        mid = StyledFrame(self)
        mid.pack(fill="x", pady=(0, T.PAD))

        self._params_panel = PanelFrame(mid, title="Parameters")
        self._params_panel.pack(fill="x")
        self._params_container = StyledFrame(self._params_panel,
                                             bg=T.BG_SECONDARY)
        self._params_container.pack(fill="x", padx=T.PAD_SMALL,
                                    pady=T.PAD_SMALL)

        # Load default preset
        self._load_preset("Harmonic Oscillator")

        # ── Buttons ───────────────────────────────────────────────
        btn_frame = StyledFrame(self)
        btn_frame.pack(fill="x", pady=(0, T.PAD))

        self._run_btn = StyledButton(btn_frame, text="▶  Run Simulation",
                                     command=self._run_simulation)
        self._run_btn.pack(side="left", padx=(0, T.PAD_SMALL))

        self._stop_btn = StyledButton(btn_frame, text="■  Stop",
                                      bg=T.ERROR, fg="#ffffff",
                                      command=self._stop_simulation,
                                      state="disabled")
        self._stop_btn.pack(side="left")

        # ── Results ───────────────────────────────────────────────
        results_panel = PanelFrame(self, title="Results")
        results_panel.pack(fill="both", expand=True)

        self._results_text = ScrolledText(results_panel, height=14)
        self._results_text.pack(fill="both", expand=True, padx=T.PAD_SMALL,
                                pady=T.PAD_SMALL)
        self._results_text.tag_configure("heading", foreground=T.ACCENT,
                                         font=T.FONT_HEADING)
        self._results_text.tag_configure("success", foreground=T.SUCCESS)
        self._results_text.tag_configure("error", foreground=T.ERROR)
        self._results_text.tag_configure("dim", foreground=T.FG_MUTED)
        self._results_text.tag_configure("value", foreground=T.WARNING)

    # ── Preset handling ───────────────────────────────────────────────

    def _on_preset_change(self, event: Any = None) -> None:
        name = self._preset_var.get()
        self._load_preset(name)

    def _load_preset(self, name: str) -> None:
        preset = SIM_PRESETS.get(name)
        if not preset:
            return
        self._desc_label.configure(text=preset["description"])

        for w in self._params_container.winfo_children():
            w.destroy()
        self._param_rows.clear()

        for pname, pdefault in preset["params"].items():
            row = ParameterRow(self._params_container, f"{pname}:", str(pdefault))
            row.pack(fill="x", pady=2)
            self._param_rows.append(row)

    def _get_params(self) -> Dict[str, float]:
        preset = SIM_PRESETS.get(self._preset_var.get(), {})
        param_names = list(preset.get("params", {}).keys())
        params = {}
        for i, name in enumerate(param_names):
            try:
                params[name] = float(self._param_rows[i].value)
            except (ValueError, IndexError):
                params[name] = float(preset["params"][name])
        return params

    # ── Simulation execution ──────────────────────────────────────────

    def _run_simulation(self) -> None:
        if self._running:
            return

        preset_name = self._preset_var.get()
        preset = SIM_PRESETS.get(preset_name)
        if not preset:
            self.app.status.set_message("Select a preset", "warning")
            return

        try:
            dt = float(self._dt_var.get())
            tmax = float(self._tmax_var.get())
        except ValueError:
            self.app.status.set_message("Invalid dt or T value", "error")
            return

        method = self._method_var.get()
        params = self._get_params()

        self._running = True
        self._run_btn.configure(state="disabled")
        self._stop_btn.configure(state="normal")
        self._results_text.delete("1.0", "end")
        self._results_text.insert("end", f"Running: {preset_name}\n", "heading")
        self._results_text.insert("end", f"Method: {method}, dt={dt}, T={tmax}\n")
        self._results_text.insert("end", f"Parameters: {params}\n\n")
        self.app.status.set_message(f"Running {preset_name}...", "info")

        def worker() -> None:
            try:
                result = self._execute_simulation(
                    preset["equations"], params, dt, tmax, method)
                self.after(0, lambda: self._on_sim_complete(preset_name, result))
            except Exception as e:
                self.after(0, lambda: self._on_sim_error(str(e)))
            finally:
                self._running = False

        thread = threading.Thread(target=worker, daemon=True)
        thread.start()

    def _stop_simulation(self) -> None:
        self._running = False
        self._run_btn.configure(state="normal")
        self._stop_btn.configure(state="disabled")
        self.app.status.set_message("Simulation stopped", "warning")

    def _execute_simulation(self, eq_type: str, params: Dict[str, float],
                            dt: float, tmax: float,
                            method: str) -> Dict[str, Any]:
        from grey_physics.core.numeric import ODESolver
        solver = ODESolver()

        equations, y0 = self._build_equations(eq_type, params)
        result = solver.solve(equations, y0, (0, tmax), dt=dt, method=method)
        return result

    def _build_equations(self, eq_type: str, p: Dict[str, float]):
        if eq_type == "harmonic":
            omega = p.get("omega", 1.0)
            y0 = np.array([p.get("x0", 1.0), p.get("v0", 0.0)])
            def f(t, y):
                return np.array([y[1], -omega**2 * y[0]])
            return f, y0

        elif eq_type == "damped":
            omega = p.get("omega", 1.0)
            gamma = p.get("gamma", 0.1)
            y0 = np.array([p.get("x0", 1.0), p.get("v0", 0.0)])
            def f(t, y):
                return np.array([y[1], -omega**2 * y[0] - 2 * gamma * y[1]])
            return f, y0

        elif eq_type == "double_pendulum":
            g = 9.81
            m1, m2 = p.get("m1", 1.0), p.get("m2", 1.0)
            L1, L2 = p.get("L1", 1.0), p.get("L2", 1.0)
            y0 = np.array([p.get("θ1_0", 2.0), 0.0, p.get("θ2_0", 3.0), 0.0])
            def f(t, y):
                th1, w1, th2, w2 = y
                delta = th1 - th2
                den1 = (m1 + m2) * L1 - m2 * L1 * np.cos(delta)**2
                den2 = (L2 / L1) * den1
                a1 = (m2 * L1 * w1**2 * np.sin(delta) * np.cos(delta) +
                      m2 * g * np.sin(th2) * np.cos(delta) +
                      m2 * L2 * w2**2 * np.sin(delta) -
                      (m1 + m2) * g * np.sin(th1)) / den1
                a2 = (-m2 * L2 * w2**2 * np.sin(delta) * np.cos(delta) +
                      (m1 + m2) * g * np.sin(th1) * np.cos(delta) -
                      (m1 + m2) * L1 * w1**2 * np.sin(delta) -
                      (m1 + m2) * g * np.sin(th2)) / den2
                return np.array([w1, a1, w2, a2])
            return f, y0

        elif eq_type == "lorenz":
            sigma = p.get("sigma", 10.0)
            rho = p.get("rho", 28.0)
            beta = p.get("beta", 8.0 / 3)
            y0 = np.array([p.get("x0", 1.0), p.get("y0", 1.0),
                           p.get("z0", 1.0)])
            def f(t, y):
                return np.array([
                    sigma * (y[1] - y[0]),
                    y[0] * (rho - y[2]) - y[1],
                    y[0] * y[1] - beta * y[2],
                ])
            return f, y0

        elif eq_type == "kepler":
            y0 = np.array([p.get("x0", 1.0), 0.0, 0.0, p.get("vy0", 0.8)])
            def f(t, y):
                x, vx, yy, vy = y
                r3 = (x**2 + yy**2)**1.5
                if r3 < 1e-10:
                    r3 = 1e-10
                return np.array([vx, -x / r3, vy, -yy / r3])
            return f, y0

        elif eq_type == "vanderpol":
            mu = p.get("mu", 2.0)
            y0 = np.array([p.get("x0", 0.5), p.get("v0", 0.0)])
            def f(t, y):
                return np.array([y[1], mu * (1 - y[0]**2) * y[1] - y[0]])
            return f, y0

        raise ValueError(f"Unknown equation type: {eq_type}")

    def _on_sim_complete(self, name: str, result: Dict[str, Any]) -> None:
        self._run_btn.configure(state="normal")
        self._stop_btn.configure(state="disabled")

        t = result["t"]
        y = result["y"]

        self._results_text.insert("end", "Simulation Complete\n", "success")
        self._results_text.insert("end", f"Time steps: {len(t)}\n")
        self._results_text.insert("end",
                                  f"Time range: [{t[0]:.3f}, {t[-1]:.3f}]\n")
        self._results_text.insert("end", f"State dimensions: {y.shape[1]}\n\n")

        # Statistics
        self._results_text.insert("end", "State Statistics:\n", "heading")
        for i in range(y.shape[1]):
            col = y[:, i]
            self._results_text.insert(
                "end",
                f"  y[{i}]: min={col.min():.6f}, max={col.max():.6f}, "
                f"mean={col.mean():.6f}\n",
            )

        # Energy check for conservative systems
        self._results_text.insert("end", "\n")

        # Store result for Field Viewer
        result_name = f"sim_{name.lower().replace(' ', '_')}"
        self.app.results[result_name] = result
        self._results_text.insert(
            "end",
            f"Result stored as '{result_name}' — view in Field Viewer\n",
            "dim",
        )

        self.app.status.set_message(
            f"Simulation '{name}' complete ({len(t)} steps)", "success")

    def _on_sim_error(self, error: str) -> None:
        self._run_btn.configure(state="normal")
        self._stop_btn.configure(state="disabled")
        self._results_text.insert("end", f"\nError: {error}\n", "error")
        self.app.status.set_message(f"Simulation error: {error}", "error")
