"""
Grey Physics GUI — Field Viewer Panel

Visualize scalar fields, vector fields, and simulation results
using matplotlib embedded in the tkinter window.
"""

from __future__ import annotations

import tkinter as tk
from typing import Any, Dict, Optional

import numpy as np

from grey_physics.ide.gui import theme as T
from grey_physics.ide.gui.widgets import (
    StyledFrame, PanelFrame, StyledLabel, StyledButton,
    StyledCombobox, ParameterRow,
)

try:
    import matplotlib
    matplotlib.use("TkAgg")
    from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk
    from matplotlib.figure import Figure
    HAS_MPL = True
except ImportError:
    HAS_MPL = False


class FieldViewerPanel(StyledFrame):
    """Panel for visualizing fields and data."""

    def __init__(self, parent: tk.Widget, app: Any):
        super().__init__(parent)
        self.app = app
        self._build_ui()

    def _build_ui(self) -> None:
        if not HAS_MPL:
            msg = StyledLabel(
                self, text="matplotlib is required for visualization.\n"
                           "Install with: pip install matplotlib",
                fg=T.WARNING, bg=T.BG_PRIMARY, font=T.FONT_HEADING)
            msg.pack(expand=True)
            return

        # ── Controls bar ──────────────────────────────────────────
        controls = PanelFrame(self, title="Visualization Controls")
        controls.pack(fill="x", padx=0, pady=(0, T.PAD))

        ctrl_inner = StyledFrame(controls, bg=T.BG_SECONDARY)
        ctrl_inner.pack(fill="x", padx=T.PAD_SMALL, pady=T.PAD_SMALL)

        # Plot type selector
        StyledLabel(ctrl_inner, text="Plot Type:").pack(side="left",
                                                        padx=(0, T.PAD_SMALL))
        self._plot_type = tk.StringVar(value="Scalar Field 2D")
        plot_types = [
            "Scalar Field 2D", "Scalar Field 3D",
            "Vector Field 2D", "Phase Portrait",
            "Wavefunction |ψ|²", "Energy Landscape",
            "Lorenz Attractor", "Time Series",
        ]
        combo = StyledCombobox(ctrl_inner, textvariable=self._plot_type,
                               values=plot_types, width=20)
        combo.pack(side="left", padx=(0, T.PAD))

        # Model selector
        StyledLabel(ctrl_inner, text="Model:").pack(side="left",
                                                    padx=(0, T.PAD_SMALL))
        self._model_var = tk.StringVar(value="(built-in demo)")
        self._model_combo = StyledCombobox(ctrl_inner,
                                           textvariable=self._model_var,
                                           values=["(built-in demo)"],
                                           width=20)
        self._model_combo.pack(side="left", padx=(0, T.PAD))

        StyledButton(ctrl_inner, text="Plot",
                     command=self._do_plot).pack(side="left", padx=(0, T.PAD_SMALL))
        StyledButton(ctrl_inner, text="Clear", bg=T.BG_ACTIVE,
                     fg=T.FG_PRIMARY,
                     command=self._clear_plot).pack(side="left")

        # ── Matplotlib canvas ─────────────────────────────────────
        canvas_frame = StyledFrame(self)
        canvas_frame.pack(fill="both", expand=True)

        self._fig = Figure(figsize=(8, 5), dpi=100,
                           facecolor=T.BG_PRIMARY, edgecolor=T.BORDER)
        self._ax = self._fig.add_subplot(111)
        self._style_axes(self._ax)
        self._ax.set_title("Grey Physics — Field Viewer",
                           color=T.FG_PRIMARY, fontsize=12)
        self._ax.text(0.5, 0.5, "Select a plot type and click Plot",
                      ha="center", va="center", color=T.FG_MUTED,
                      fontsize=14, transform=self._ax.transAxes)

        self._canvas = FigureCanvasTkAgg(self._fig, master=canvas_frame)
        self._canvas.draw()
        self._canvas.get_tk_widget().pack(fill="both", expand=True)

        # Toolbar
        toolbar_frame = StyledFrame(canvas_frame)
        toolbar_frame.pack(fill="x")
        self._toolbar = NavigationToolbar2Tk(self._canvas, toolbar_frame)
        self._toolbar.update()

    def _style_axes(self, ax: Any) -> None:
        """Apply dark theme to matplotlib axes."""
        ax.set_facecolor(T.BG_SECONDARY)
        ax.tick_params(colors=T.FG_MUTED, labelsize=9)
        for spine in ax.spines.values():
            spine.set_color(T.BORDER)
        ax.xaxis.label.set_color(T.FG_SECONDARY)
        ax.yaxis.label.set_color(T.FG_SECONDARY)

    def refresh_model_list(self) -> None:
        """Update the model dropdown from app.models."""
        names = ["(built-in demo)"] + list(self.app.models.keys())
        if hasattr(self, "_model_combo"):
            self._model_combo.configure(values=names)

    def _do_plot(self) -> None:
        if not HAS_MPL:
            return

        plot_type = self._plot_type.get()
        self._fig.clear()

        try:
            if plot_type == "Scalar Field 2D":
                self._plot_scalar_2d()
            elif plot_type == "Scalar Field 3D":
                self._plot_scalar_3d()
            elif plot_type == "Vector Field 2D":
                self._plot_vector_2d()
            elif plot_type == "Phase Portrait":
                self._plot_phase_portrait()
            elif plot_type == "Wavefunction |ψ|²":
                self._plot_wavefunction()
            elif plot_type == "Energy Landscape":
                self._plot_energy_landscape()
            elif plot_type == "Lorenz Attractor":
                self._plot_lorenz()
            elif plot_type == "Time Series":
                self._plot_time_series()
            else:
                ax = self._fig.add_subplot(111)
                self._style_axes(ax)
                ax.text(0.5, 0.5, f"Unknown plot type: {plot_type}",
                        ha="center", va="center", color=T.ERROR,
                        transform=ax.transAxes)

            self._fig.tight_layout()
            self._canvas.draw()
            self.app.status.set_message(f"Plotted: {plot_type}", "success")

        except Exception as e:
            ax = self._fig.add_subplot(111)
            self._style_axes(ax)
            ax.text(0.5, 0.5, f"Error: {e}", ha="center", va="center",
                    color=T.ERROR, transform=ax.transAxes, fontsize=11)
            self._canvas.draw()
            self.app.status.set_message(f"Plot error: {e}", "error")

    def _clear_plot(self) -> None:
        if not HAS_MPL:
            return
        self._fig.clear()
        ax = self._fig.add_subplot(111)
        self._style_axes(ax)
        ax.set_title("Grey Physics — Field Viewer",
                     color=T.FG_PRIMARY, fontsize=12)
        self._canvas.draw()

    # ── Plot implementations ──────────────────────────────────────────

    def _plot_scalar_2d(self) -> None:
        ax = self._fig.add_subplot(111)
        self._style_axes(ax)

        x = np.linspace(-5, 5, 200)
        y = np.linspace(-5, 5, 200)
        X, Y = np.meshgrid(x, y)

        # Demo: Gaussian wave packet
        Z = np.exp(-(X**2 + Y**2) / 2) * np.cos(3 * X) * np.cos(3 * Y)

        im = ax.pcolormesh(X, Y, Z, cmap="RdBu_r", shading="auto")
        self._fig.colorbar(im, ax=ax, label="φ(x, y)")
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        ax.set_title("Scalar Field φ(x, y)", color=T.FG_PRIMARY)
        ax.set_aspect("equal")

    def _plot_scalar_3d(self) -> None:
        ax = self._fig.add_subplot(111, projection="3d")
        ax.set_facecolor(T.BG_SECONDARY)

        x = np.linspace(-4, 4, 100)
        y = np.linspace(-4, 4, 100)
        X, Y = np.meshgrid(x, y)
        Z = np.exp(-(X**2 + Y**2) / 3) * np.sin(2 * X) * np.cos(2 * Y)

        ax.plot_surface(X, Y, Z, cmap="viridis", alpha=0.9,
                        edgecolor="none")
        ax.set_xlabel("x", color=T.FG_SECONDARY)
        ax.set_ylabel("y", color=T.FG_SECONDARY)
        ax.set_zlabel("φ", color=T.FG_SECONDARY)
        ax.set_title("Scalar Field (3D Surface)", color=T.FG_PRIMARY)
        ax.tick_params(colors=T.FG_MUTED)

    def _plot_vector_2d(self) -> None:
        ax = self._fig.add_subplot(111)
        self._style_axes(ax)

        x = np.linspace(-3, 3, 16)
        y = np.linspace(-3, 3, 16)
        X, Y = np.meshgrid(x, y)

        # Rotating + source field
        U = -Y + 0.3 * X
        V = X + 0.3 * Y
        mag = np.sqrt(U**2 + V**2)
        mag[mag == 0] = 1

        ax.quiver(X, Y, U / mag, V / mag, mag, cmap="plasma", scale=25)
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        ax.set_title("Vector Field V(x, y)", color=T.FG_PRIMARY)
        ax.set_aspect("equal")

    def _plot_phase_portrait(self) -> None:
        ax = self._fig.add_subplot(111)
        self._style_axes(ax)

        # Harmonic oscillator phase portrait
        from grey_physics.core.numeric import ODESolver
        solver = ODESolver()

        lines_data = []
        for E in [0.5, 1.0, 2.0, 3.0, 4.0]:
            q0 = np.sqrt(2 * E)
            result = solver.solve(
                lambda t, y: np.array([y[1], -y[0]]),
                np.array([q0, 0.0]),
                (0, 2 * np.pi + 0.1),
                dt=0.02,
                method="RK4",
            )
            lines_data.append(result["y"])

        for yd in lines_data:
            ax.plot(yd[:, 0], yd[:, 1], color=T.ACCENT, alpha=0.7, lw=1.2)

        ax.set_xlabel("q (position)")
        ax.set_ylabel("p (momentum)")
        ax.set_title("Phase Portrait — Harmonic Oscillator",
                     color=T.FG_PRIMARY)
        ax.set_aspect("equal")
        ax.grid(True, alpha=0.2, color=T.FG_MUTED)

    def _plot_wavefunction(self) -> None:
        ax = self._fig.add_subplot(111)
        self._style_axes(ax)

        from grey_physics.domains.quantum import SchrodingerSolver1D

        def harmonic_V(x: np.ndarray) -> np.ndarray:
            return 0.5 * x**2

        solver = SchrodingerSolver1D(N=512, L=20.0, V=harmonic_V)
        psi0 = np.exp(-0.5 * (solver.x - 2)**2) * np.exp(2j * solver.x)
        result = solver.evolve(psi0, dt=0.005, n_steps=400, save_every=40)

        for i in range(len(result["t"])):
            alpha = 0.3 + 0.7 * (i / len(result["t"]))
            ax.plot(result["x"], result["prob"][i],
                    alpha=alpha, lw=1.2, color=T.ACCENT)

        # Overlay potential
        ax2 = ax.twinx()
        ax2.plot(solver.x, harmonic_V(solver.x), "--",
                 color=T.WARNING, alpha=0.5, lw=1)
        ax2.set_ylabel("V(x)", color=T.WARNING)
        ax2.tick_params(colors=T.FG_MUTED)

        ax.set_xlabel("x")
        ax.set_ylabel("|ψ(x)|²")
        ax.set_title("Quantum Wavefunction Evolution",
                     color=T.FG_PRIMARY)
        ax.grid(True, alpha=0.2, color=T.FG_MUTED)

    def _plot_energy_landscape(self) -> None:
        ax = self._fig.add_subplot(111)
        self._style_axes(ax)

        x = np.linspace(-3, 3, 300)
        y = np.linspace(-3, 3, 300)
        X, Y = np.meshgrid(x, y)

        # Double-well potential
        V = (X**2 - 1)**2 + Y**2

        cs = ax.contourf(X, Y, V, levels=20, cmap="inferno")
        ax.contour(X, Y, V, levels=20, colors=T.FG_MUTED, linewidths=0.3)
        self._fig.colorbar(cs, ax=ax, label="V(x, y)")
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        ax.set_title("Energy Landscape — Double Well",
                     color=T.FG_PRIMARY)
        ax.set_aspect("equal")

    def _plot_lorenz(self) -> None:
        ax = self._fig.add_subplot(111, projection="3d")
        ax.set_facecolor(T.BG_SECONDARY)

        from grey_physics.domains.chaos import StrangeAttractor
        attractor = StrangeAttractor.lorenz()
        result = attractor.simulate(
            np.array([1.0, 1.0, 1.0]),
            t_span=(0, 40),
            dt=0.005,
        )
        traj = result["trajectory"]
        t = result["t"]

        # Color by time
        for i in range(0, len(traj) - 1, 10):
            end = min(i + 11, len(traj))
            color_val = i / len(traj)
            ax.plot(traj[i:end, 0], traj[i:end, 1], traj[i:end, 2],
                    color=matplotlib.cm.plasma(color_val), lw=0.5, alpha=0.8)

        ax.set_xlabel("x", color=T.FG_SECONDARY)
        ax.set_ylabel("y", color=T.FG_SECONDARY)
        ax.set_zlabel("z", color=T.FG_SECONDARY)
        ax.set_title("Lorenz Attractor", color=T.FG_PRIMARY)
        ax.tick_params(colors=T.FG_MUTED)

    def _plot_time_series(self) -> None:
        ax = self._fig.add_subplot(111)
        self._style_axes(ax)

        # Demo: coupled oscillators
        from grey_physics.core.numeric import ODESolver
        solver = ODESolver()

        k1, k2, k12 = 1.0, 1.5, 0.3

        def coupled_osc(t: float, y: np.ndarray) -> np.ndarray:
            x1, v1, x2, v2 = y
            a1 = -k1 * x1 - k12 * (x1 - x2)
            a2 = -k2 * x2 - k12 * (x2 - x1)
            return np.array([v1, a1, v2, a2])

        result = solver.solve(coupled_osc,
                              np.array([1.0, 0.0, 0.0, 0.0]),
                              (0, 30), dt=0.02, method="RK4")

        ax.plot(result["t"], result["y"][:, 0], color=T.ACCENT,
                lw=1.2, label="x₁(t)")
        ax.plot(result["t"], result["y"][:, 2], color=T.WARNING,
                lw=1.2, label="x₂(t)")
        ax.set_xlabel("Time")
        ax.set_ylabel("Displacement")
        ax.set_title("Coupled Oscillators", color=T.FG_PRIMARY)
        ax.legend(facecolor=T.BG_TERTIARY, edgecolor=T.BORDER,
                  labelcolor=T.FG_PRIMARY)
        ax.grid(True, alpha=0.2, color=T.FG_MUTED)
