"""
Grey Physics GUI — Theme and styling constants.
"""

from __future__ import annotations

# ── Colour palette ─────────────────────────────────────────────────────
BG_PRIMARY    = "#1e1e2e"   # main background
BG_SECONDARY  = "#2a2a3c"   # panels / sidebars
BG_TERTIARY   = "#33334d"   # input fields, frames
BG_ACTIVE     = "#3d3d5c"   # hovered / active items
ACCENT        = "#7aa2f7"   # blue accent
ACCENT_DIM    = "#5c7ec9"   # dimmed accent
SUCCESS       = "#9ece6a"   # green
WARNING       = "#e0af68"   # amber
ERROR         = "#f7768e"   # red
FG_PRIMARY    = "#c0caf5"   # main text
FG_SECONDARY  = "#9aa5ce"   # secondary text
FG_MUTED      = "#565f89"   # muted / placeholder text
BORDER        = "#3b3b54"   # borders
HIGHLIGHT     = "#414868"   # selection highlight

# ── Fonts ──────────────────────────────────────────────────────────────
FONT_FAMILY   = "Helvetica"
MONO_FAMILY   = "Courier"

FONT_TITLE    = (FONT_FAMILY, 14, "bold")
FONT_HEADING  = (FONT_FAMILY, 12, "bold")
FONT_BODY     = (FONT_FAMILY, 11)
FONT_SMALL    = (FONT_FAMILY, 10)
FONT_MONO     = (MONO_FAMILY, 11)
FONT_MONO_SM  = (MONO_FAMILY, 10)

# ── Dimensions ─────────────────────────────────────────────────────────
PAD           = 8
PAD_SMALL     = 4
PAD_LARGE     = 16
SIDEBAR_WIDTH = 240
STATUS_HEIGHT = 28


def apply_theme(widget) -> None:
    """Apply the dark theme to a top-level tkinter widget tree."""
    try:
        widget.configure(bg=BG_PRIMARY)
    except Exception:
        pass
