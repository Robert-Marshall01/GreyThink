"""
Grey Physics GUI — Model Editor Panel

Create, inspect, and manage physics models:
  - Lagrangians (particle + field)
  - Hamiltonians
  - Spacetimes / Metrics
  - Scalar / Vector / Tensor fields
  - Symmetries
"""

from __future__ import annotations

import tkinter as tk
from tkinter import ttk, messagebox
from typing import Any, Dict, List, Optional

from grey_physics.ide.gui import theme as T
from grey_physics.ide.gui.widgets import (
    StyledFrame, PanelFrame, StyledLabel, StyledEntry, StyledButton,
    StyledCombobox, ScrolledText, ParameterRow,
)


# ── Model templates ───────────────────────────────────────────────────

MODEL_TEMPLATES: Dict[str, Dict[str, Any]] = {
    "Free Particle": {
        "type": "lagrangian",
        "factory": "free_particle",
        "params": {"mass": "1.0", "dim": "3"},
        "description": "L = ½m|v|²",
    },
    "Harmonic Oscillator": {
        "type": "lagrangian",
        "factory": "harmonic_oscillator",
        "params": {"mass": "1.0", "omega": "1.0", "dim": "1"},
        "description": "L = ½mq̇² − ½mω²q²",
    },
    "Klein-Gordon Field": {
        "type": "lagrangian",
        "factory": "klein_gordon",
        "params": {"mass": "1.0"},
        "description": "ℒ = ½(∂μφ)(∂ᵘφ) − ½m²φ²",
    },
    "Electromagnetic (Maxwell)": {
        "type": "lagrangian",
        "factory": "electromagnetic",
        "params": {},
        "description": "ℒ = −¼ FμνFᵘᵛ",
    },
    "Einstein-Hilbert": {
        "type": "lagrangian",
        "factory": "gravitational_einstein_hilbert",
        "params": {},
        "description": "ℒ = (1/16πG) R √(−g)",
    },
    "Kepler Problem": {
        "type": "hamiltonian",
        "factory": "kepler",
        "params": {"mass": "1.0", "k": "1.0"},
        "description": "H = p²/(2m) − k/|r|",
    },
    "H.O. Hamiltonian": {
        "type": "hamiltonian",
        "factory": "harmonic_oscillator",
        "params": {"mass": "1.0", "omega": "1.0"},
        "description": "H = p²/(2m) + ½mω²q²",
    },
    "Minkowski Spacetime": {
        "type": "spacetime",
        "factory": "minkowski",
        "params": {"dim": "4"},
        "description": "η = diag(−1,+1,+1,+1)",
    },
    "Schwarzschild Spacetime": {
        "type": "spacetime",
        "factory": "schwarzschild",
        "params": {"mass": "1.0"},
        "description": "Schwarzschild black hole",
    },
    "Scalar Field": {
        "type": "field",
        "factory": "scalar",
        "params": {"name": "φ"},
        "description": "φ(x) : M → ℝ",
    },
    "Vector Field": {
        "type": "field",
        "factory": "vector",
        "params": {"name": "V", "dim": "3"},
        "description": "Vᵘ(x) on ℝⁿ",
    },
}


class ModelEditorPanel(StyledFrame):
    """Panel for creating and inspecting physics models."""

    def __init__(self, parent: tk.Widget, app: Any):
        super().__init__(parent)
        self.app = app
        self._param_rows: List[ParameterRow] = []
        self._build_ui()

    def _build_ui(self) -> None:
        # ── Left: template selector ───────────────────────────────
        left = PanelFrame(self, title="Model Templates")
        left.pack(side="left", fill="y", padx=(0, T.PAD), pady=0)

        self._template_list = tk.Listbox(
            left, bg=T.BG_TERTIARY, fg=T.FG_PRIMARY,
            font=T.FONT_BODY, selectbackground=T.ACCENT,
            selectforeground=T.BG_PRIMARY, relief="flat",
            highlightthickness=0, width=26, activestyle="none",
        )
        for name in MODEL_TEMPLATES:
            self._template_list.insert("end", name)
        self._template_list.pack(fill="both", expand=True, pady=(0, T.PAD_SMALL))
        self._template_list.bind("<<ListboxSelect>>", self._on_template_select)

        # ── Right: model editor ───────────────────────────────────
        right = StyledFrame(self)
        right.pack(side="left", fill="both", expand=True)

        # Description
        self._desc_label = StyledLabel(right, text="Select a model template",
                                       font=T.FONT_HEADING, bg=T.BG_PRIMARY,
                                       anchor="w")
        self._desc_label.pack(fill="x", pady=(0, T.PAD))

        # Description detail
        self._desc_detail = StyledLabel(right, text="", bg=T.BG_PRIMARY,
                                        fg=T.FG_SECONDARY, anchor="w")
        self._desc_detail.pack(fill="x", pady=(0, T.PAD))

        # Parameters frame
        self._params_frame = PanelFrame(right, title="Parameters")
        self._params_frame.pack(fill="x", pady=(0, T.PAD))

        self._params_container = StyledFrame(self._params_frame,
                                             bg=T.BG_SECONDARY)
        self._params_container.pack(fill="x", padx=T.PAD_SMALL,
                                    pady=T.PAD_SMALL)

        # Model name
        self._name_row = ParameterRow(self._params_container,
                                      "Model Name:", "my_model")
        self._name_row.pack(fill="x", pady=2)

        # Dynamic parameter rows go here
        self._dynamic_params = StyledFrame(self._params_container,
                                           bg=T.BG_SECONDARY)
        self._dynamic_params.pack(fill="x")

        # Action buttons
        btn_frame = StyledFrame(right, bg=T.BG_PRIMARY)
        btn_frame.pack(fill="x", pady=(0, T.PAD))

        StyledButton(btn_frame, text="Create Model",
                     command=self._create_model).pack(side="left", padx=(0, T.PAD_SMALL))
        StyledButton(btn_frame, text="Clear",
                     command=self._clear, bg=T.BG_ACTIVE,
                     fg=T.FG_PRIMARY).pack(side="left")

        # Model info output
        info_panel = PanelFrame(right, title="Model Info")
        info_panel.pack(fill="both", expand=True)

        self._info_text = ScrolledText(info_panel, height=12)
        self._info_text.pack(fill="both", expand=True, padx=T.PAD_SMALL,
                             pady=T.PAD_SMALL)
        self._info_text.tag_configure("heading", foreground=T.ACCENT,
                                      font=T.FONT_HEADING)
        self._info_text.tag_configure("success", foreground=T.SUCCESS)
        self._info_text.tag_configure("error", foreground=T.ERROR)
        self._info_text.tag_configure("dim", foreground=T.FG_MUTED)

    # ── Event handlers ────────────────────────────────────────────────

    def _on_template_select(self, event: Any) -> None:
        sel = self._template_list.curselection()
        if not sel:
            return
        name = self._template_list.get(sel[0])
        tmpl = MODEL_TEMPLATES[name]

        self._desc_label.configure(text=name)
        self._desc_detail.configure(text=tmpl["description"])

        # Clear old dynamic params
        for w in self._dynamic_params.winfo_children():
            w.destroy()
        self._param_rows.clear()

        # Add new params
        for pname, pdefault in tmpl["params"].items():
            row = ParameterRow(self._dynamic_params, f"{pname}:", str(pdefault))
            row.pack(fill="x", pady=2)
            self._param_rows.append(row)

    def _create_model(self) -> None:
        sel = self._template_list.curselection()
        if not sel:
            self.app.status.set_message("Select a template first", "warning")
            return

        template_name = self._template_list.get(sel[0])
        tmpl = MODEL_TEMPLATES[template_name]
        model_name = self._name_row.value.strip() or "model"

        try:
            model = self._build_model(tmpl, model_name)
            self.app.models[model_name] = model

            # Show info
            self._info_text.delete("1.0", "end")
            self._info_text.insert("end", f"Model Created: {model_name}\n",
                                   "heading")
            self._info_text.insert("end", f"Type: {tmpl['type']}\n")
            self._info_text.insert("end", f"Template: {template_name}\n")
            self._info_text.insert("end", f"Description: {tmpl['description']}\n\n")

            # Show canonical form
            if hasattr(model, "canonical_form"):
                self._info_text.insert("end", "Canonical Form:\n", "heading")
                self._info_text.insert("end", f"  {model.canonical_form()}\n\n")

            # Show expression
            if hasattr(model, "expr"):
                self._info_text.insert("end", "Expression:\n", "heading")
                self._info_text.insert("end", f"  {model.expr}\n\n")

            # Show fields
            if hasattr(model, "fields") and model.fields:
                self._info_text.insert("end", "Fields:\n", "heading")
                for f in model.fields:
                    self._info_text.insert("end", f"  {f}\n")
                self._info_text.insert("end", "\n")

            # Show metric components
            if hasattr(model, "metric") and model.metric is not None:
                self._info_text.insert("end", "Metric Components:\n", "heading")
                m = model.metric
                for i in range(m.dim):
                    for j in range(m.dim):
                        comp = m[i, j]
                        if str(comp) != "0.0":
                            self._info_text.insert(
                                "end", f"  g[{i},{j}] = {comp}\n")
                self._info_text.insert("end", "\n")

            self._info_text.insert("end", "✓ Model added to workspace\n",
                                   "success")
            self.app.status.set_message(
                f"Created model '{model_name}' ({template_name})", "success")
            self.app.refresh_model_lists()

        except Exception as e:
            self._info_text.delete("1.0", "end")
            self._info_text.insert("end", f"Error creating model:\n", "heading")
            self._info_text.insert("end", f"  {e}\n", "error")
            self.app.status.set_message(f"Error: {e}", "error")

    def _build_model(self, tmpl: Dict[str, Any], name: str) -> Any:
        """Build a model from a template and current parameter values."""
        from grey_physics.core.ir.mechanics import Lagrangian, Hamiltonian
        from grey_physics.core.ir.spacetime import Spacetime
        from grey_physics.core.ir.fields import ScalarField, VectorField

        params = {}
        param_defs = list(tmpl["params"].keys())
        for i, pname in enumerate(param_defs):
            raw = self._param_rows[i].value if i < len(self._param_rows) else \
                tmpl["params"][pname]
            # Try numeric conversion
            try:
                val = int(raw)
            except ValueError:
                try:
                    val = float(raw)
                except ValueError:
                    val = raw
            params[pname] = val

        mtype = tmpl["type"]
        factory = tmpl["factory"]

        if mtype == "lagrangian":
            if factory == "klein_gordon":
                field = ScalarField("φ")
                return Lagrangian.klein_gordon(field, mass=params.get("mass", 0.0))
            elif factory == "free_particle":
                return Lagrangian.free_particle(
                    mass=params.get("mass", 1.0),
                    dim=int(params.get("dim", 3)))
            elif factory == "harmonic_oscillator":
                return Lagrangian.harmonic_oscillator(
                    mass=params.get("mass", 1.0),
                    omega=params.get("omega", 1.0),
                    dim=int(params.get("dim", 1)))
            elif factory == "electromagnetic":
                return Lagrangian.electromagnetic()
            elif factory == "gravitational_einstein_hilbert":
                return Lagrangian.gravitational_einstein_hilbert()

        elif mtype == "hamiltonian":
            if factory == "kepler":
                return Hamiltonian.kepler(
                    mass=params.get("mass", 1.0),
                    k=params.get("k", 1.0))
            elif factory == "harmonic_oscillator":
                return Hamiltonian.harmonic_oscillator(
                    mass=params.get("mass", 1.0),
                    omega=params.get("omega", 1.0))

        elif mtype == "spacetime":
            if factory == "minkowski":
                return Spacetime.minkowski(dim=int(params.get("dim", 4)))
            elif factory == "schwarzschild":
                return Spacetime.schwarzschild(mass=params.get("mass", 1.0))

        elif mtype == "field":
            fname = params.get("name", "φ")
            if factory == "scalar":
                return ScalarField(fname)
            elif factory == "vector":
                return VectorField(fname, dim=int(params.get("dim", 3)))

        raise ValueError(f"Unknown template: {mtype}/{factory}")

    def _clear(self) -> None:
        self._info_text.delete("1.0", "end")
        self._desc_label.configure(text="Select a model template")
        self._desc_detail.configure(text="")
        for w in self._dynamic_params.winfo_children():
            w.destroy()
        self._param_rows.clear()
        self._template_list.selection_clear(0, "end")
