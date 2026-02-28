"""
Grey Physics — GUI Application

A tkinter-based graphical user interface for the Grey Physics engine.
Provides:
  - Model Editor: create and edit Lagrangians, Hamiltonians, fields
  - Field Viewer: visualize scalar/vector fields with matplotlib
  - Simulation Lab: configure and run simulations
  - Symmetry Inspector: analyse symmetries and conservation laws
  - Interactive Console: Python REPL with grey_physics context
"""

from __future__ import annotations


def launch() -> None:
    """Launch the Grey Physics GUI application."""
    from grey_physics.ide.gui.app import GreyPhysicsApp
    app = GreyPhysicsApp()
    app.mainloop()
