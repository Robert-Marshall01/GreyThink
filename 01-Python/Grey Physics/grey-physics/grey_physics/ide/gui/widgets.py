"""
Grey Physics GUI — Reusable widgets.
"""

from __future__ import annotations

import tkinter as tk
from tkinter import ttk
from typing import Any, Callable, List, Optional

from grey_physics.ide.gui import theme as T


# ── Styled Frame ───────────────────────────────────────────────────────

class StyledFrame(tk.Frame):
    """A Frame pre-configured with the dark theme."""

    def __init__(self, parent: tk.Widget, **kw: Any):
        kw.setdefault("bg", T.BG_PRIMARY)
        kw.setdefault("highlightthickness", 0)
        super().__init__(parent, **kw)


class PanelFrame(tk.LabelFrame):
    """A labelled panel with themed styling."""

    def __init__(self, parent: tk.Widget, title: str = "", **kw: Any):
        kw.setdefault("bg", T.BG_SECONDARY)
        kw.setdefault("fg", T.ACCENT)
        kw.setdefault("font", T.FONT_HEADING)
        kw.setdefault("padx", T.PAD)
        kw.setdefault("pady", T.PAD_SMALL)
        kw.setdefault("bd", 1)
        kw.setdefault("relief", "groove")
        super().__init__(parent, text=f"  {title}  ", **kw)


# ── Styled Label ──────────────────────────────────────────────────────

class StyledLabel(tk.Label):
    def __init__(self, parent: tk.Widget, **kw: Any):
        kw.setdefault("bg", T.BG_SECONDARY)
        kw.setdefault("fg", T.FG_PRIMARY)
        kw.setdefault("font", T.FONT_BODY)
        super().__init__(parent, **kw)


# ── Styled Entry ──────────────────────────────────────────────────────

class StyledEntry(tk.Entry):
    def __init__(self, parent: tk.Widget, **kw: Any):
        kw.setdefault("bg", T.BG_TERTIARY)
        kw.setdefault("fg", T.FG_PRIMARY)
        kw.setdefault("insertbackground", T.FG_PRIMARY)
        kw.setdefault("font", T.FONT_MONO)
        kw.setdefault("relief", "flat")
        kw.setdefault("highlightthickness", 1)
        kw.setdefault("highlightcolor", T.ACCENT)
        kw.setdefault("highlightbackground", T.BORDER)
        super().__init__(parent, **kw)


# ── Styled Button ─────────────────────────────────────────────────────

class StyledButton(tk.Button):
    def __init__(self, parent: tk.Widget, **kw: Any):
        kw.setdefault("bg", T.ACCENT)
        kw.setdefault("fg", "#1e1e2e")
        kw.setdefault("activebackground", T.ACCENT_DIM)
        kw.setdefault("activeforeground", "#ffffff")
        kw.setdefault("font", T.FONT_BODY)
        kw.setdefault("relief", "flat")
        kw.setdefault("padx", T.PAD)
        kw.setdefault("pady", T.PAD_SMALL)
        kw.setdefault("cursor", "hand2")
        super().__init__(parent, **kw)


# ── Styled Text ───────────────────────────────────────────────────────

class StyledText(tk.Text):
    def __init__(self, parent: tk.Widget, **kw: Any):
        kw.setdefault("bg", T.BG_TERTIARY)
        kw.setdefault("fg", T.FG_PRIMARY)
        kw.setdefault("insertbackground", T.FG_PRIMARY)
        kw.setdefault("font", T.FONT_MONO)
        kw.setdefault("relief", "flat")
        kw.setdefault("highlightthickness", 1)
        kw.setdefault("highlightcolor", T.ACCENT)
        kw.setdefault("highlightbackground", T.BORDER)
        kw.setdefault("selectbackground", T.HIGHLIGHT)
        kw.setdefault("selectforeground", T.FG_PRIMARY)
        kw.setdefault("wrap", "word")
        super().__init__(parent, **kw)


# ── Scrollable Text with scrollbar ────────────────────────────────────

class ScrolledText(StyledFrame):
    """A themed text widget with a scrollbar."""

    def __init__(self, parent: tk.Widget, **kw: Any):
        frame_kw = {k: kw.pop(k) for k in list(kw) if k in ("bg",)}
        super().__init__(parent, **frame_kw)

        self.text = StyledText(self, **kw)
        scrollbar = tk.Scrollbar(self, command=self.text.yview,
                                 bg=T.BG_TERTIARY, troughcolor=T.BG_SECONDARY,
                                 highlightthickness=0)
        self.text.configure(yscrollcommand=scrollbar.set)

        self.text.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

    def insert(self, *args: Any, **kw: Any) -> None:
        self.text.insert(*args, **kw)

    def delete(self, *args: Any, **kw: Any) -> None:
        self.text.delete(*args, **kw)

    def get(self, *args: Any, **kw: Any) -> str:
        return self.text.get(*args, **kw)

    def see(self, index: str) -> None:
        self.text.see(index)

    def configure_text(self, **kw: Any) -> None:
        self.text.configure(**kw)

    def tag_configure(self, *args: Any, **kw: Any) -> None:
        self.text.tag_configure(*args, **kw)

    def bind_text(self, *args: Any, **kw: Any) -> None:
        self.text.bind(*args, **kw)


# ── Dropdown / Combobox ───────────────────────────────────────────────

class StyledCombobox(ttk.Combobox):
    def __init__(self, parent: tk.Widget, **kw: Any):
        kw.setdefault("state", "readonly")
        kw.setdefault("font", T.FONT_BODY)
        super().__init__(parent, **kw)


# ── StatusBar ─────────────────────────────────────────────────────────

class StatusBar(tk.Frame):
    """A bottom status bar."""

    def __init__(self, parent: tk.Widget):
        super().__init__(parent, bg=T.BG_SECONDARY, height=T.STATUS_HEIGHT)
        self.pack_propagate(False)

        self._label = tk.Label(self, text="Ready", bg=T.BG_SECONDARY,
                               fg=T.FG_SECONDARY, font=T.FONT_SMALL,
                               anchor="w", padx=T.PAD)
        self._label.pack(side="left", fill="x", expand=True)

        self._right = tk.Label(self, text="", bg=T.BG_SECONDARY,
                               fg=T.FG_MUTED, font=T.FONT_SMALL,
                               anchor="e", padx=T.PAD)
        self._right.pack(side="right")

    def set_message(self, msg: str, level: str = "info") -> None:
        color = {
            "info": T.FG_SECONDARY,
            "success": T.SUCCESS,
            "warning": T.WARNING,
            "error": T.ERROR,
        }.get(level, T.FG_SECONDARY)
        self._label.configure(text=msg, fg=color)

    def set_right(self, text: str) -> None:
        self._right.configure(text=text)


# ── ParameterRow ──────────────────────────────────────────────────────

class ParameterRow(StyledFrame):
    """A labelled parameter input row."""

    def __init__(self, parent: tk.Widget, label: str,
                 default: str = "", width: int = 12):
        super().__init__(parent, bg=T.BG_SECONDARY)
        StyledLabel(self, text=label, width=14, anchor="e").pack(
            side="left", padx=(0, T.PAD_SMALL))
        self.var = tk.StringVar(value=default)
        self.entry = StyledEntry(self, textvariable=self.var, width=width)
        self.entry.pack(side="left", fill="x", expand=True)

    @property
    def value(self) -> str:
        return self.var.get()

    @value.setter
    def value(self, v: str) -> None:
        self.var.set(v)
