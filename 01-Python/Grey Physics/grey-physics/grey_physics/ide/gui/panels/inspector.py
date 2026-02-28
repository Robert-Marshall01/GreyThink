"""
Grey Physics GUI — Symmetry Inspector Panel

Analyze symmetries and conserved quantities of physical models:
  - Identify symmetries of Lagrangians
  - Compute Noether charges and conservation laws
  - Display symmetry generators and transformation rules
  - Derive equations of motion via the symbolic engine
"""

from __future__ import annotations

import tkinter as tk
from typing import Any, Dict, List, Optional

from grey_physics.ide.gui import theme as T
from grey_physics.ide.gui.widgets import (
    StyledFrame, PanelFrame, StyledLabel, StyledButton,
    StyledCombobox, ScrolledText,
)


class InspectorPanel(StyledFrame):
    """Panel for inspecting symmetries and derived quantities."""

    def __init__(self, parent: tk.Widget, app: Any):
        super().__init__(parent)
        self.app = app
        self._build_ui()

    def _build_ui(self) -> None:
        # ── Controls ──────────────────────────────────────────────
        controls = PanelFrame(self, title="Inspector Controls")
        controls.pack(fill="x", pady=(0, T.PAD))

        ctrl_inner = StyledFrame(controls, bg=T.BG_SECONDARY)
        ctrl_inner.pack(fill="x", padx=T.PAD_SMALL, pady=T.PAD_SMALL)

        StyledLabel(ctrl_inner, text="Model:").pack(side="left",
                                                    padx=(0, T.PAD_SMALL))
        self._model_var = tk.StringVar(value="(select)")
        self._model_combo = StyledCombobox(
            ctrl_inner, textvariable=self._model_var,
            values=["(select)"], width=22)
        self._model_combo.pack(side="left", padx=(0, T.PAD))

        StyledLabel(ctrl_inner, text="Action:").pack(side="left",
                                                     padx=(0, T.PAD_SMALL))
        self._action_var = tk.StringVar(value="Derive Equations")
        StyledCombobox(
            ctrl_inner, textvariable=self._action_var,
            values=[
                "Derive Equations",
                "Find Symmetries",
                "Conservation Laws",
                "Canonical Form",
                "Full Inspection",
            ],
            width=20,
        ).pack(side="left", padx=(0, T.PAD))

        StyledButton(ctrl_inner, text="Analyze",
                     command=self._analyze).pack(side="left")

        # ── Output ────────────────────────────────────────────────
        output_panel = PanelFrame(self, title="Analysis Results")
        output_panel.pack(fill="both", expand=True)

        self._output = ScrolledText(output_panel, height=20)
        self._output.pack(fill="both", expand=True, padx=T.PAD_SMALL,
                          pady=T.PAD_SMALL)
        self._output.tag_configure("heading", foreground=T.ACCENT,
                                   font=T.FONT_HEADING)
        self._output.tag_configure("subheading", foreground=T.ACCENT_DIM,
                                   font=T.FONT_BODY)
        self._output.tag_configure("success", foreground=T.SUCCESS)
        self._output.tag_configure("error", foreground=T.ERROR)
        self._output.tag_configure("dim", foreground=T.FG_MUTED)
        self._output.tag_configure("math", foreground=T.WARNING,
                                   font=T.FONT_MONO)

        # Initial content
        self._output.insert("end", "Symmetry & Conservation Inspector\n",
                            "heading")
        self._output.insert("end", "─" * 50 + "\n", "dim")
        self._output.insert(
            "end",
            "Create a model in the Model Editor, then select it here\n"
            "to derive equations of motion, find symmetries, and\n"
            "compute conserved quantities via Noether's theorem.\n",
        )

    def refresh_model_list(self) -> None:
        names = ["(select)"] + list(self.app.models.keys())
        if hasattr(self, "_model_combo"):
            self._model_combo.configure(values=names)

    def _analyze(self) -> None:
        model_name = self._model_var.get()
        if model_name == "(select)" or model_name not in self.app.models:
            self.app.status.set_message("Select a model first", "warning")
            return

        model = self.app.models[model_name]
        action = self._action_var.get()

        self._output.delete("1.0", "end")

        try:
            if action == "Derive Equations":
                self._derive_equations(model_name, model)
            elif action == "Find Symmetries":
                self._find_symmetries(model_name, model)
            elif action == "Conservation Laws":
                self._conservation_laws(model_name, model)
            elif action == "Canonical Form":
                self._canonical_form(model_name, model)
            elif action == "Full Inspection":
                self._full_inspection(model_name, model)

            self.app.status.set_message(
                f"Analysis complete: {action} on '{model_name}'", "success")

        except Exception as e:
            self._output.insert("end", f"\nError during analysis:\n", "heading")
            self._output.insert("end", f"  {e}\n", "error")
            self.app.status.set_message(f"Analysis error: {e}", "error")

    def _derive_equations(self, name: str, model: Any) -> None:
        from grey_physics.core.symbolic import SymbolicEngine
        engine = SymbolicEngine()

        self._output.insert("end", f"Equations of Motion — {name}\n",
                            "heading")
        self._output.insert("end", "─" * 50 + "\n\n", "dim")

        try:
            eqs = engine.derive_equations(model)
            self._output.insert("end", f"Derived {len(eqs)} equation(s):\n\n",
                                "subheading")
            for i, eq in enumerate(eqs):
                self._output.insert("end", f"  EOM[{i}]:  ", "dim")
                self._output.insert("end", f"{eq} = 0\n\n", "math")
        except TypeError as e:
            self._output.insert("end", f"Cannot derive equations for this "
                                f"model type ({type(model).__name__})\n", "dim")
            self._output.insert("end", f"Detail: {e}\n", "error")

    def _find_symmetries(self, name: str, model: Any) -> None:
        from grey_physics.core.ir.symmetry import SymmetryType

        self._output.insert("end", f"Symmetry Analysis — {name}\n", "heading")
        self._output.insert("end", "─" * 50 + "\n\n", "dim")

        # Identify common symmetries based on model properties
        symmetries_found = []

        if hasattr(model, "is_density"):
            if model.is_density:
                self._output.insert("end", "Field theory Lagrangian density\n\n",
                                    "subheading")
            else:
                self._output.insert("end", "Particle Lagrangian\n\n",
                                    "subheading")

        if hasattr(model, "expr"):
            free_syms = model.expr.free_symbols()
            self._output.insert("end", "Free symbols: ", "dim")
            self._output.insert("end", f"{free_syms}\n\n", "math")

            # Check for time-translation symmetry
            if hasattr(model, "time_var"):
                t_name = model.time_var.name if hasattr(model.time_var, "name") \
                    else "t"
                if t_name not in free_syms:
                    symmetries_found.append(
                        ("Time Translation", SymmetryType.TRANSLATION,
                         "∂/∂t", "Energy (Hamiltonian)"))
                    self._output.insert("end",
                                        "✓ Time-translation invariance detected\n",
                                        "success")
                    self._output.insert("end",
                                        "  → Conserved: Energy (Hamiltonian)\n\n")

            # Check for coordinate symmetries
            if hasattr(model, "generalized_coords"):
                for q in model.generalized_coords:
                    q_name = q.name if hasattr(q, "name") else str(q)
                    if q_name not in free_syms:
                        symmetries_found.append(
                            (f"{q_name}-Translation", SymmetryType.TRANSLATION,
                             f"∂/∂{q_name}",
                             f"Conjugate momentum p_{q_name}"))
                        self._output.insert(
                            "end",
                            f"✓ {q_name}-translation symmetry detected\n",
                            "success")
                        self._output.insert(
                            "end",
                            f"  → Conserved: Conjugate momentum p_{q_name}\n\n")

        if not symmetries_found:
            self._output.insert("end",
                                "No obvious symmetries detected from "
                                "expression analysis.\n", "dim")
            self._output.insert("end",
                                "Use the symbolic engine for deeper analysis.\n")

        self._output.insert("end", f"\nTotal symmetries found: "
                            f"{len(symmetries_found)}\n", "subheading")

    def _conservation_laws(self, name: str, model: Any) -> None:
        from grey_physics.core.symbolic import SymbolicEngine
        engine = SymbolicEngine()

        self._output.insert("end",
                            f"Conservation Laws — {name}\n", "heading")
        self._output.insert("end", "─" * 50 + "\n\n", "dim")

        try:
            laws = engine.compute_conserved_quantities(model)
            if laws:
                self._output.insert("end",
                                    f"Found {len(laws)} conservation law(s):\n\n",
                                    "subheading")
                for i, law in enumerate(laws):
                    self._output.insert("end", f"  [{i}] ", "dim")
                    self._output.insert("end", f"{law}\n", "math")
            else:
                self._output.insert("end",
                                    "No conservation laws computed.\n", "dim")
                self._output.insert(
                    "end",
                    "Noether's theorem requires a Lagrangian with "
                    "identified symmetries.\n")
        except Exception as e:
            self._output.insert("end", f"Error: {e}\n", "error")

    def _canonical_form(self, name: str, model: Any) -> None:
        self._output.insert("end", f"Canonical Form — {name}\n", "heading")
        self._output.insert("end", "─" * 50 + "\n\n", "dim")

        if hasattr(model, "canonical_form"):
            self._output.insert("end", "Canonical representation:\n",
                                "subheading")
            self._output.insert("end", f"  {model.canonical_form()}\n\n",
                                "math")

        self._output.insert("end", "Object type: ", "dim")
        self._output.insert("end", f"{type(model).__name__}\n\n")

        if hasattr(model, "name"):
            self._output.insert("end", f"Name: {model.name}\n")
        if hasattr(model, "metadata"):
            m = model.metadata
            if m.domain:
                self._output.insert("end", f"Domain: {m.domain.name}\n")
            if m.symmetries:
                self._output.insert("end",
                                    f"Symmetries: {m.symmetries}\n")
            if m.stability:
                self._output.insert("end",
                                    f"Stability: {m.stability.name}\n")

    def _full_inspection(self, name: str, model: Any) -> None:
        self._output.insert("end", f"Full Inspection — {name}\n", "heading")
        self._output.insert("end", "═" * 50 + "\n\n", "dim")

        self._canonical_form(name, model)
        self._output.insert("end", "\n")
        self._derive_equations(name, model)
        self._output.insert("end", "\n")
        self._find_symmetries(name, model)
        self._output.insert("end", "\n")
        self._conservation_laws(name, model)
