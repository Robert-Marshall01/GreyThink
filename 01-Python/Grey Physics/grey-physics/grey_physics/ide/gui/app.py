"""
Grey Physics GUI — Main Application Window

The top-level tkinter application that hosts all panels in a
tabbed notebook interface with a menu bar and status bar.
"""

from __future__ import annotations

import sys
import tkinter as tk
from tkinter import ttk, messagebox, filedialog
from typing import Any, Dict

from grey_physics.ide.gui import theme as T
from grey_physics.ide.gui.widgets import StatusBar, StyledFrame


class GreyPhysicsApp(tk.Tk):
    """Main Grey Physics GUI application."""

    def __init__(self) -> None:
        super().__init__()

        # ── Application state ─────────────────────────────────────
        self.models: Dict[str, Any] = {}
        self.results: Dict[str, Any] = {}

        # ── Window setup ──────────────────────────────────────────
        self.title("Grey Physics — Physics & Mathematics IDE")
        self.geometry("1280x800")
        self.minsize(960, 600)
        self.configure(bg=T.BG_PRIMARY)

        # Attempt to set icon (non-critical)
        try:
            self.tk.call("wm", "iconphoto", self._w,
                         tk.PhotoImage(data=""))
        except Exception:
            pass

        # ── ttk theme ─────────────────────────────────────────────
        self._configure_ttk_style()

        # ── Menu bar ──────────────────────────────────────────────
        self._build_menu()

        # ── Main layout ──────────────────────────────────────────
        main_frame = StyledFrame(self)
        main_frame.pack(fill="both", expand=True)

        # ── Tab notebook ──────────────────────────────────────────
        self._notebook = ttk.Notebook(main_frame, style="Dark.TNotebook")
        self._notebook.pack(fill="both", expand=True, padx=T.PAD,
                            pady=(T.PAD, 0))

        # Import and create panels (lazy to handle missing deps)
        self._create_panels()

        # ── Status bar ────────────────────────────────────────────
        self.status = StatusBar(self)
        self.status.pack(fill="x", side="bottom")
        self.status.set_right("Grey Physics v0.1.0")

        # ── Key bindings ──────────────────────────────────────────
        self.bind("<Control-q>", lambda e: self._on_quit())
        self.bind("<Control-n>", lambda e: self._notebook.select(0))
        self.bind("<F5>", lambda e: self._notebook.select(2))

        self.protocol("WM_DELETE_WINDOW", self._on_quit)

    def _configure_ttk_style(self) -> None:
        """Configure the ttk style for a dark theme."""
        style = ttk.Style(self)
        style.theme_use("clam")

        # Notebook tabs
        style.configure("Dark.TNotebook", background=T.BG_PRIMARY,
                        borderwidth=0)
        style.configure("Dark.TNotebook.Tab",
                        background=T.BG_SECONDARY,
                        foreground=T.FG_SECONDARY,
                        padding=[16, 6],
                        font=T.FONT_BODY)
        style.map("Dark.TNotebook.Tab",
                  background=[("selected", T.BG_ACTIVE),
                              ("active", T.BG_TERTIARY)],
                  foreground=[("selected", T.FG_PRIMARY),
                              ("active", T.FG_PRIMARY)])

        # Combobox
        style.configure("TCombobox",
                        fieldbackground=T.BG_TERTIARY,
                        background=T.BG_TERTIARY,
                        foreground=T.FG_PRIMARY,
                        selectbackground=T.HIGHLIGHT,
                        selectforeground=T.FG_PRIMARY)
        style.map("TCombobox",
                  fieldbackground=[("readonly", T.BG_TERTIARY)],
                  foreground=[("readonly", T.FG_PRIMARY)])

    def _build_menu(self) -> None:
        """Build the application menu bar."""
        menubar = tk.Menu(self, bg=T.BG_SECONDARY, fg=T.FG_PRIMARY,
                          activebackground=T.ACCENT,
                          activeforeground=T.BG_PRIMARY,
                          font=T.FONT_BODY, relief="flat")

        # File menu
        file_menu = tk.Menu(menubar, tearoff=0, bg=T.BG_SECONDARY,
                            fg=T.FG_PRIMARY, activebackground=T.ACCENT,
                            activeforeground=T.BG_PRIMARY, font=T.FONT_BODY)
        file_menu.add_command(label="New Model      Ctrl+N",
                              command=lambda: self._notebook.select(0))
        file_menu.add_separator()
        file_menu.add_command(label="Export Results...",
                              command=self._export_results)
        file_menu.add_separator()
        file_menu.add_command(label="Quit           Ctrl+Q",
                              command=self._on_quit)
        menubar.add_cascade(label="File", menu=file_menu)

        # View menu
        view_menu = tk.Menu(menubar, tearoff=0, bg=T.BG_SECONDARY,
                            fg=T.FG_PRIMARY, activebackground=T.ACCENT,
                            activeforeground=T.BG_PRIMARY, font=T.FONT_BODY)
        view_menu.add_command(label="Model Editor",
                              command=lambda: self._notebook.select(0))
        view_menu.add_command(label="Field Viewer",
                              command=lambda: self._notebook.select(1))
        view_menu.add_command(label="Simulation Lab   F5",
                              command=lambda: self._notebook.select(2))
        view_menu.add_command(label="Inspector",
                              command=lambda: self._notebook.select(3))
        view_menu.add_command(label="Console",
                              command=lambda: self._notebook.select(4))
        menubar.add_cascade(label="View", menu=view_menu)

        # Physics menu
        physics_menu = tk.Menu(menubar, tearoff=0, bg=T.BG_SECONDARY,
                               fg=T.FG_PRIMARY, activebackground=T.ACCENT,
                               activeforeground=T.BG_PRIMARY, font=T.FONT_BODY)
        physics_menu.add_command(label="Quick: Harmonic Oscillator",
                                 command=lambda: self._quick_model("Harmonic Oscillator"))
        physics_menu.add_command(label="Quick: Klein-Gordon Field",
                                 command=lambda: self._quick_model("Klein-Gordon Field"))
        physics_menu.add_command(label="Quick: Lorenz Attractor",
                                 command=lambda: self._quick_sim("Lorenz System"))
        physics_menu.add_separator()
        physics_menu.add_command(label="Quick: Minkowski Spacetime",
                                 command=lambda: self._quick_model("Minkowski Spacetime"))
        physics_menu.add_command(label="Quick: Schwarzschild",
                                 command=lambda: self._quick_model("Schwarzschild Spacetime"))
        menubar.add_cascade(label="Physics", menu=physics_menu)

        # Help menu
        help_menu = tk.Menu(menubar, tearoff=0, bg=T.BG_SECONDARY,
                            fg=T.FG_PRIMARY, activebackground=T.ACCENT,
                            activeforeground=T.BG_PRIMARY, font=T.FONT_BODY)
        help_menu.add_command(label="About Grey Physics",
                              command=self._show_about)
        help_menu.add_command(label="Keyboard Shortcuts",
                              command=self._show_shortcuts)
        menubar.add_cascade(label="Help", menu=help_menu)

        self.config(menu=menubar)

    def _create_panels(self) -> None:
        """Instantiate all panels and add them to the notebook."""
        from grey_physics.ide.gui.panels.model_editor import ModelEditorPanel
        from grey_physics.ide.gui.panels.field_viewer import FieldViewerPanel
        from grey_physics.ide.gui.panels.simulation_lab import SimulationLabPanel
        from grey_physics.ide.gui.panels.inspector import InspectorPanel
        from grey_physics.ide.gui.panels.console import ConsolePanel

        # Model Editor
        self._model_editor = ModelEditorPanel(self._notebook, self)
        self._notebook.add(self._model_editor, text="  Model Editor  ")

        # Field Viewer
        self._field_viewer = FieldViewerPanel(self._notebook, self)
        self._notebook.add(self._field_viewer, text="  Field Viewer  ")

        # Simulation Lab
        self._sim_lab = SimulationLabPanel(self._notebook, self)
        self._notebook.add(self._sim_lab, text="  Simulation Lab  ")

        # Inspector
        self._inspector = InspectorPanel(self._notebook, self)
        self._notebook.add(self._inspector, text="  Inspector  ")

        # Console
        self._console = ConsolePanel(self._notebook, self)
        self._notebook.add(self._console, text="  Console  ")

    def refresh_model_lists(self) -> None:
        """Notify all panels that the model list has changed."""
        if hasattr(self, "_field_viewer"):
            self._field_viewer.refresh_model_list()
        if hasattr(self, "_inspector"):
            self._inspector.refresh_model_list()

    # ── Quick actions ─────────────────────────────────────────────────

    def _quick_model(self, template_name: str) -> None:
        """Quickly create a model from a template and switch to editor."""
        from grey_physics.ide.gui.panels.model_editor import MODEL_TEMPLATES
        tmpl = MODEL_TEMPLATES.get(template_name)
        if not tmpl:
            return

        # Build with default params
        model_name = template_name.lower().replace(" ", "_")
        try:
            editor = self._model_editor
            # Select the template in the list
            items = editor._template_list.get(0, "end")
            for i, item in enumerate(items):
                if item == template_name:
                    editor._template_list.selection_clear(0, "end")
                    editor._template_list.selection_set(i)
                    editor._template_list.see(i)
                    editor._on_template_select(None)
                    break
            self._notebook.select(0)
            self.status.set_message(
                f"Template '{template_name}' loaded — click Create Model",
                "info")
        except Exception as e:
            self.status.set_message(f"Error: {e}", "error")

    def _quick_sim(self, preset_name: str) -> None:
        """Switch to sim lab with a preset loaded."""
        try:
            self._sim_lab._preset_var.set(preset_name)
            self._sim_lab._on_preset_change()
            self._notebook.select(2)
            self.status.set_message(
                f"Preset '{preset_name}' loaded — click Run", "info")
        except Exception as e:
            self.status.set_message(f"Error: {e}", "error")

    # ── File operations ───────────────────────────────────────────────

    def _export_results(self) -> None:
        """Export simulation results to a NumPy .npz file."""
        if not self.results:
            messagebox.showinfo("Export", "No simulation results to export.")
            return

        filepath = filedialog.asksaveasfilename(
            defaultextension=".npz",
            filetypes=[("NumPy archive", "*.npz"), ("All files", "*.*")],
            title="Export Results",
        )
        if not filepath:
            return

        try:
            import numpy as np
            save_dict = {}
            for name, result in self.results.items():
                for key, val in result.items():
                    if isinstance(val, np.ndarray):
                        save_dict[f"{name}_{key}"] = val
            np.savez(filepath, **save_dict)
            self.status.set_message(f"Results exported to {filepath}",
                                    "success")
        except Exception as e:
            messagebox.showerror("Export Error", str(e))

    # ── Dialogs ───────────────────────────────────────────────────────

    def _show_about(self) -> None:
        import grey_physics
        messagebox.showinfo(
            "About Grey Physics",
            f"Grey Physics v{grey_physics.__version__}\n\n"
            "A PhD-level Physics & Mathematics\n"
            "Operating System and IDE\n\n"
            "Features:\n"
            "• Unified IR for physics & math\n"
            "• Symbolic engine (Euler-Lagrange, Noether)\n"
            "• Numeric engine (ODE/PDE solvers)\n"
            "• 7 physics domains\n"
            "• Interactive visualization\n\n"
            "Python " + sys.version.split()[0],
        )

    def _show_shortcuts(self) -> None:
        messagebox.showinfo(
            "Keyboard Shortcuts",
            "Ctrl+N     Switch to Model Editor\n"
            "Ctrl+Q     Quit\n"
            "F5         Switch to Simulation Lab\n"
            "↑/↓        Console history navigation\n"
            "Enter      Execute console command\n",
        )

    def _on_quit(self) -> None:
        self.destroy()
