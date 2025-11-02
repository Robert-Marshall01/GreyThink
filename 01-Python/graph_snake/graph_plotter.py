#!/usr/bin/env python3
"""graph_plotter.py - simple GUI plotter

This file provides a small tkinter-based GUI that places input fields on the
left and an interactive matplotlib plot on the right. It accepts comma or
whitespace-separated numeric values for X, Y and optional Z (used for size or
color in scatter plots). The GUI supports multiple plot types (Line, Scatter,
Bar and more: Histogram, Box, Hexbin, Pie, Area) and allows saving the
rendered plot as a PNG.

Usage:
    python graph_plotter.py

Requirements: tkinter (stdlib), numpy, matplotlib
"""

from __future__ import annotations
__version__ = "0.1.0"
import sys
import math
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from typing import List, Optional

import numpy as np
from matplotlib.figure import Figure
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk
import argparse
import os
import webbrowser
import textwrap
from mpl_toolkits.axes_grid1 import make_axes_locatable


def parse_numbers(text: str) -> List[float]:
    """Parse a string of numbers separated by commas/whitespace into a list of floats.

    Accepts: "1,2,3", "1 2 3", "1, 2 3" etc.
    Raises ValueError on invalid tokens.
    """
    if not text:
        return []
    # normalize separators
    cleaned = text.replace("\n", " ").replace("\t", " ").replace(",", " ")
    parts = [p for p in cleaned.split(" ") if p != ""]
    values: List[float] = []
    for p in parts:
        try:
            # allow simple float parsing, reject non-numeric
            v = float(p)
            if math.isfinite(v):
                values.append(v)
            else:
                raise ValueError(f"Non-finite number: {p}")
        except ValueError as exc:
            raise ValueError(f"Could not parse token '{p}' as number") from exc
    return values


def parse_labels(text: str) -> List[str]:
    """Parse a string of tokens separated by commas/whitespace into a list of labels.

    Unlike parse_numbers this preserves tokens as strings and won't attempt to
    convert to numeric types. Empty input -> [].
    """
    if not text:
        return []
    cleaned = text.replace("\n", " ").replace("\t", " ").replace(",", " ")
    parts = [p for p in cleaned.split(" ") if p != ""]
    return parts


def render_plot(ax, figure, plot_type: str, x: np.ndarray, y: np.ndarray, z: Optional[np.ndarray] = None, raw_x_text: str = "") -> None:
    """Render a plot onto the provided axes using the same logic as the GUI.

    This helper is factored out so the CLI (nogui) mode can reuse plotting
    behavior without instantiating the GUI.
    """
    ax.clear()
    # Remove any existing 'counts' colorbar we previously attached so repeated
    # calls (e.g. clicking Plot multiple times) don't append more colorbars.
    if hasattr(figure, '_counts_colorbar'):
        cb = getattr(figure, '_counts_colorbar', None)
        try:
            # Remove the colorbar axes if it's still attached to the figure.
            # Use Figure.delaxes which ensures the figure layout is updated.
            if cb is not None and hasattr(cb, 'ax') and cb.ax in getattr(figure, 'axes', []):
                try:
                    figure.delaxes(cb.ax)
                except Exception:
                    try:
                        cb.ax.remove()
                    except Exception:
                        pass
            # Remove the colorbar artist itself
            try:
                cb.remove()
            except Exception:
                pass
        finally:
            # Clear stored reference
            try:
                delattr(figure, '_counts_colorbar')
            except Exception:
                if hasattr(figure, '_counts_colorbar'):
                    del figure._counts_colorbar
        # Recompute layout to collapse any empty space left by removed axes
        try:
            figure.tight_layout()
        except Exception:
            pass
    try:
        if plot_type == "Line":
            order = np.argsort(x)
            ax.plot(x[order], y[order], marker="o")
            ax.set_xlabel("X")
            ax.set_ylabel("Y")
            ax.set_title("Line Plot")
        elif plot_type == "Scatter":
            if z is not None and z.size:
                colors = z
                sizes = 20 + (np.abs(z - np.mean(z)) / (np.ptp(z) if np.ptp(z) != 0 else 1.0)) * 180
                sc = ax.scatter(x, y, c=colors, s=sizes, cmap="viridis", alpha=0.8, edgecolor="k")
                # Add/reuse a colorbar for scatter when Z is provided. Use the
                # existing dedicated colorbar axes if present to avoid creating
                # new axes that leave empty columns.
                cax = None
                if hasattr(figure, '_counts_colorbar_ax') and getattr(figure, '_counts_colorbar_ax') in getattr(figure, 'axes', []):
                    cax = getattr(figure, '_counts_colorbar_ax')
                    try:
                        cax.cla()
                    except Exception:
                        pass
                else:
                    try:
                        divider = make_axes_locatable(ax)
                        cax = divider.append_axes("right", size="5%", pad=0.05)
                    except Exception:
                        cax = None

                if cax is not None:
                    cb = figure.colorbar(sc, cax=cax)
                    figure._counts_colorbar_ax = cax
                    figure._counts_colorbar = cb
                else:
                    cb = figure.colorbar(sc, ax=ax)
                    figure._counts_colorbar = cb
                # set color limits so vmax equals max(z)
                try:
                    cb.mappable.set_clim(vmin=float(np.min(z)), vmax=float(np.max(z)))
                except Exception:
                    pass
                cb.set_label('value')
            else:
                ax.scatter(x, y, s=40, color="#2b8cbe", alpha=0.8, edgecolor="k")
            ax.set_xlabel("X")
            ax.set_ylabel("Y")
            ax.set_title("Scatter Plot")
        elif plot_type == "Bar":
            indices = np.arange(len(x))
            ax.bar(indices, y, color="#3a7bd5", alpha=0.9)
            ax.set_xticks(indices)
            try:
                x_labels = [str(int(v)) if float(v).is_integer() else str(v) for v in x]
            except Exception:
                x_labels = [str(v) for v in x]
            ax.set_xticklabels(x_labels, rotation=45, ha="right")
            ax.set_xlabel("X")
            ax.set_ylabel("Y")
            ax.set_title("Bar Plot")
        elif plot_type == "Histogram":
            data = y if y.size > 0 else x
            if data.size == 0:
                raise ValueError("Histogram requires numeric data in X or Y.")
            ax.hist(data, bins='auto', color="#6a9fb5", edgecolor='k', alpha=0.9)
            ax.set_xlabel("Value")
            ax.set_ylabel("Count")
            ax.set_title("Histogram")
        elif plot_type == "Box":
            data = y if y.size > 0 else x
            if data.size == 0:
                raise ValueError("Box plot requires numeric data in X or Y.")
            ax.boxplot(data, vert=True, patch_artist=True)
            ax.set_title("Box Plot")
            ax.set_ylabel("Value")
        elif plot_type == "Hexbin":
            if x.size == 0 or y.size == 0:
                raise ValueError("Hexbin requires both X and Y numeric values.")
            # If Z values supplied, use them as C to aggregate per-hex (use max)
            # and set the colorbar max to the maximum Z value. Otherwise color
            # by counts as usual.
            use_z = (z is not None and getattr(z, 'size', 0) > 0)
            if use_z:
                # aggregate z per bin using max so the color reflects the max z
                hb = ax.hexbin(x, y, C=z, reduce_C_function=np.max, gridsize=30, cmap='viridis')
            else:
                hb = ax.hexbin(x, y, gridsize=30, cmap='viridis')
            ax.set_xlabel("X")
            ax.set_ylabel("Y")
            ax.set_title("Hexbin 2D Histogram")
            # Create/reuse a dedicated colorbar axes to avoid creating new
            # figure axes on each call (which caused the empty column).
            cax = None
            if hasattr(figure, '_counts_colorbar_ax') and getattr(figure, '_counts_colorbar_ax') in getattr(figure, 'axes', []):
                cax = getattr(figure, '_counts_colorbar_ax')
                # clear the axes so colorbar can be redrawn
                try:
                    cax.cla()
                except Exception:
                    pass
            else:
                # create a new cax to the right of ax
                try:
                    divider = make_axes_locatable(ax)
                    cax = divider.append_axes("right", size="5%", pad=0.05)
                except Exception:
                    # fallback: let figure create the colorbar axes
                    cax = None

            # create colorbar in the dedicated axes if available
            if cax is not None:
                cb = figure.colorbar(hb, cax=cax)
                try:
                    figure._counts_colorbar_ax = cax
                    figure._counts_colorbar = cb
                except Exception:
                    figure._counts_colorbar_ax = cax
                    figure._counts_colorbar = cb
            else:
                cb = figure.colorbar(hb, ax=ax)
                try:
                    figure._counts_colorbar = cb
                except Exception:
                    figure._counts_colorbar = cb
            # If using Z, ensure colorbar range uses max(z) as requested
            if use_z:
                try:
                    vmax = float(np.max(z))
                    cb.mappable.set_clim(vmin=float(np.min(z)), vmax=vmax)
                except Exception:
                    pass
                cb.set_label('value')
            else:
                cb.set_label('counts')
        elif plot_type == "Pie":
            labels = parse_labels(raw_x_text)
            sizes = y if y.size > 0 else x
            if sizes.size == 0:
                raise ValueError("Pie chart requires numeric sizes in Y (or X).")
            if labels and len(labels) != len(sizes):
                labels = []
            if not labels:
                labels = [str(i) for i in range(1, len(sizes) + 1)]
            ax.pie(sizes, labels=labels, autopct='%1.1f%%', startangle=90, colors=None)
            ax.axis('equal')
            ax.set_title("Pie Chart")
        elif plot_type == "Area":
            order = np.argsort(x)
            ax.fill_between(x[order], y[order], color="#8cc6d9", alpha=0.7)
            ax.plot(x[order], y[order], marker="", color="#2b8cbe")
            ax.set_xlabel("X")
            ax.set_ylabel("Y")
            ax.set_title("Area Chart")
        else:
            raise ValueError(f"Unknown plot type: {plot_type}")
    except Exception:
        # Re-raise for callers to handle consistently
        raise


class GraphPlotterApp(tk.Tk):
    def __init__(self) -> None:
        super().__init__()
        self.title("Graph Plotter")
        self.geometry("1000x600")

        # top-level frames
        self.left_frame = ttk.Frame(self, width=320)
        self.right_frame = ttk.Frame(self)
        self.left_frame.pack(side=tk.LEFT, fill=tk.Y, padx=8, pady=8)
        self.right_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=8, pady=8)

        # UI build
        self._build_left()
        self._build_right()

        # Keyboard shortcuts to improve usability
        self.bind('<Control-p>', lambda e: self.on_plot())
        self.bind('<Control-s>', lambda e: self.on_save())
        self.bind('<Control-o>', lambda e: self.on_load_csv())
        self.bind('<Escape>', lambda e: self.on_clear())

    def _build_left(self) -> None:
        lbl = ttk.Label(self.left_frame, text="Input (comma or space separated)")
        lbl.pack(anchor=tk.W, pady=(0, 6))

        # X
        ttk.Label(self.left_frame, text="X values:").pack(anchor=tk.W)
        self.x_text = tk.Text(self.left_frame, height=3, width=40)
        self.x_text.pack(fill=tk.X)

        # Y
        ttk.Label(self.left_frame, text="Y values:").pack(anchor=tk.W, pady=(6, 0))
        self.y_text = tk.Text(self.left_frame, height=3, width=40)
        self.y_text.pack(fill=tk.X)

        # Z (optional)
        ttk.Label(self.left_frame, text="Z values (optional - size/color for scatter):").pack(anchor=tk.W, pady=(6, 0))
        self.z_text = tk.Text(self.left_frame, height=3, width=40)
        self.z_text.pack(fill=tk.X)

        # Plot type
        ttk.Label(self.left_frame, text="Plot type:").pack(anchor=tk.W, pady=(6, 0))
        # Added more plot types: Histogram (uses Y or X), Box (Y), Hexbin (X+Y),
        # Pie (labels from X, sizes from Y), Area (filled line by X/Y)
        self.plot_type = ttk.Combobox(
            self.left_frame,
            values=["Line", "Scatter", "Bar", "Histogram", "Box", "Hexbin", "Pie", "Area"],
            state="readonly",
        )
        self.plot_type.current(0)
        self.plot_type.pack(fill=tk.X)

        # Buttons
        btn_frame = ttk.Frame(self.left_frame)
        btn_frame.pack(fill=tk.X, pady=(10, 0))
        self.plot_btn = ttk.Button(btn_frame, text="Plot", command=self.on_plot)
        self.plot_btn.pack(side=tk.LEFT, padx=(0, 6))
        self.clear_btn = ttk.Button(btn_frame, text="Clear", command=self.on_clear)
        self.clear_btn.pack(side=tk.LEFT, padx=(0, 6))
        self.save_btn = ttk.Button(btn_frame, text="Save PNG", command=self.on_save)
        self.save_btn.pack(side=tk.LEFT)

        # Load CSV
        self.load_btn = ttk.Button(self.left_frame, text="Load CSV", command=self.on_load_csv)
        self.load_btn.pack(fill=tk.X, pady=(8, 0))

        # Sample / presets and options
        sample_frame = ttk.Frame(self.left_frame)
        sample_frame.pack(fill=tk.X, pady=(6, 0))
        self.sample_btn = ttk.Button(sample_frame, text="Fill Sample", command=self.fill_sample)
        self.sample_btn.pack(side=tk.LEFT, padx=(0, 6))
        # Open after save option
        self.open_after_save_var = tk.BooleanVar(value=False)
        ttk.Checkbutton(sample_frame, text="Open after save", variable=self.open_after_save_var).pack(side=tk.LEFT)

        # Status
        self.status_var = tk.StringVar(value="Ready")
        ttk.Label(self.left_frame, textvariable=self.status_var, foreground="blue").pack(anchor=tk.W, pady=(8, 0))

    def fill_sample(self) -> None:
        """Populate the input fields with a small sample dataset for quick trials."""
        sample_x = "1, 2, 3, 4, 5"
        sample_y = "2, 3, 5, 4, 6"
        sample_z = "10, 20, 30, 40, 50"
        self.x_text.delete("1.0", tk.END)
        self.x_text.insert(tk.END, sample_x)
        self.y_text.delete("1.0", tk.END)
        self.y_text.insert(tk.END, sample_y)
        self.z_text.delete("1.0", tk.END)
        self.z_text.insert(tk.END, sample_z)
        self.plot_type.set("Line")
        self.status_var.set("Sample data loaded")

    def _build_right(self) -> None:
        # Matplotlib figure
        self.figure = Figure(figsize=(6, 5), dpi=100)
        self.ax = self.figure.add_subplot(111)

        self.canvas = FigureCanvasTkAgg(self.figure, master=self.right_frame)
        self.canvas.draw()
        self.canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

        toolbar = NavigationToolbar2Tk(self.canvas, self.right_frame)
        toolbar.update()
        toolbar.pack(fill=tk.X)

    def on_plot(self) -> None:
        try:
            x_vals = parse_numbers(self.x_text.get("1.0", tk.END).strip())
            y_vals = parse_numbers(self.y_text.get("1.0", tk.END).strip())
            z_vals = parse_numbers(self.z_text.get("1.0", tk.END).strip())
        except ValueError as exc:
            messagebox.showerror("Parse error", str(exc))
            return

        if len(x_vals) == 0 or len(y_vals) == 0:
            messagebox.showerror("Input error", "X and Y must contain at least one number each.")
            return

        if len(x_vals) != len(y_vals):
            messagebox.showerror("Input error", "X and Y must have the same length.")
            return

        if z_vals and len(z_vals) != len(x_vals):
            messagebox.showerror("Input error", "Z must be empty or the same length as X/Y.")
            return

        x = np.array(x_vals)
        y = np.array(y_vals)
        z = np.array(z_vals) if z_vals else None

        plot_type = self.plot_type.get()
        raw_x = self.x_text.get("1.0", tk.END).strip()
        try:
            render_plot(self.ax, self.figure, plot_type, x, y, z, raw_x)
        except Exception as exc:
            messagebox.showerror("Plot error", f"An error occurred while plotting: {exc}")
            return

        self.figure.tight_layout()
        self.canvas.draw()
        self.status_var.set("Plotted")

    def on_clear(self) -> None:
        self.x_text.delete("1.0", tk.END)
        self.y_text.delete("1.0", tk.END)
        self.z_text.delete("1.0", tk.END)
        self.ax.clear()
        # remove any persisted colorbar reference and its axes/artists
        if hasattr(self.figure, '_counts_colorbar'):
            cb = getattr(self.figure, '_counts_colorbar', None)
            try:
                if cb is not None and hasattr(cb, 'ax') and cb.ax in getattr(self.figure, 'axes', []):
                    try:
                                self.figure.delaxes(cb.ax)
                    except Exception:
                        try:
                            cb.ax.remove()
                        except Exception:
                            pass
                try:
                    cb.remove()
                except Exception:
                    pass
            finally:
                try:
                    delattr(self.figure, '_counts_colorbar')
                except Exception:
                    if hasattr(self.figure, '_counts_colorbar'):
                        del self.figure._counts_colorbar
        self.canvas.draw()
        self.status_var.set("Cleared")

    def on_save(self) -> None:
        path = filedialog.asksaveasfilename(defaultextension=".png", filetypes=[("PNG image", "*.png")])
        if not path:
            return
        try:
            self.figure.savefig(path, bbox_inches="tight")
            self.status_var.set(f"Saved: {path}")
            # Optionally open file after saving for convenience
            if getattr(self, 'open_after_save_var', None) and self.open_after_save_var.get():
                try:
                    # Windows: startfile, otherwise fallback to webbrowser
                    if hasattr(os, 'startfile'):
                        os.startfile(path)
                    else:
                        webbrowser.open('file://' + os.path.abspath(path))
                except Exception:
                    # ignore errors opening the file
                    pass
        except Exception as exc:
            messagebox.showerror("Save error", f"Could not save file: {exc}")

    def on_load_csv(self) -> None:
        path = filedialog.askopenfilename(filetypes=[("CSV", "*.csv"), ("All files", "*")])
        if not path:
            return
        try:
            data = np.genfromtxt(path, delimiter=",", dtype=float)
        except Exception as exc:
            messagebox.showerror("Load error", f"Could not load CSV: {exc}")
            return
        if data.ndim == 1:
            # single column
            self.x_text.delete("1.0", tk.END)
            self.x_text.insert(tk.END, ", ".join(map(str, data.tolist())))
            self.status_var.set("Loaded single-column CSV into X")
            return
        # assume columns are X, Y, (Z)
        cols = data.T
        self.x_text.delete("1.0", tk.END)
        self.x_text.insert(tk.END, ", ".join(map(str, cols[0].tolist())))
        self.y_text.delete("1.0", tk.END)
        self.y_text.insert(tk.END, ", ".join(map(str, cols[1].tolist())))
        if cols.shape[0] >= 3:
            self.z_text.delete("1.0", tk.END)
            self.z_text.insert(tk.END, ", ".join(map(str, cols[2].tolist())))
        self.status_var.set(f"Loaded: {path}")


def main() -> None:
    parser = argparse.ArgumentParser(description="Graph Plotter - GUI and headless plotting helper")
    parser.add_argument("--nogui", action="store_true", help="Run in headless mode (no GUI). Requires --x and --y and --output")
    parser.add_argument("--x", type=str, help="X values (comma or space separated)")
    parser.add_argument("--y", type=str, help="Y values (comma or space separated)")
    parser.add_argument("--z", type=str, default="", help="Optional Z values for scatter (comma or space separated)")
    parser.add_argument("--plot-type", type=str, default="Line", help="Plot type (Line, Scatter, Bar, Histogram, Box, Hexbin, Pie, Area)")
    parser.add_argument("--output", type=str, default="plot.png", help="Output PNG path for headless mode")
    parser.add_argument("--open", action="store_true", help="If set in headless mode, open the output file after saving")
    parser.add_argument("--demo", action="store_true", help="Launch the GUI with sample data prefilled")
    args = parser.parse_args()

    if args.nogui:
        if not (args.x and args.y):
            parser.error("--nogui requires --x and --y to be provided")
        try:
            x_vals = parse_numbers(args.x)
            y_vals = parse_numbers(args.y)
            z_vals = parse_numbers(args.z) if args.z else []
        except ValueError as exc:
            print(f"Error parsing numbers: {exc}", file=sys.stderr)
            sys.exit(2)
        x = np.array(x_vals)
        y = np.array(y_vals)
        z = np.array(z_vals) if z_vals else None
        fig = Figure(figsize=(6, 4), dpi=100)
        ax = fig.add_subplot(111)
        try:
            render_plot(ax, fig, args.plot_type, x, y, z, args.x)
        except Exception as exc:
            print(f"Error while rendering plot: {exc}", file=sys.stderr)
            sys.exit(3)
        fig.tight_layout()
        try:
            fig.savefig(args.output, bbox_inches="tight")
            print(f"Saved plot to {args.output}")
            if args.open:
                try:
                    if hasattr(os, 'startfile'):
                        os.startfile(args.output)
                    else:
                        webbrowser.open('file://' + os.path.abspath(args.output))
                except Exception:
                    pass
        except Exception as exc:
            print(f"Could not save output: {exc}", file=sys.stderr)
            sys.exit(4)
        return

    # Launch GUI
    app = GraphPlotterApp()
    if args.demo:
        app.fill_sample()
    # If user provided a plot type via CLI, select it in the UI for convenience
    if args.plot_type:
        try:
            app.plot_type.set(args.plot_type)
        except Exception:
            pass
    app.mainloop()


if __name__ == "__main__":
    main()
