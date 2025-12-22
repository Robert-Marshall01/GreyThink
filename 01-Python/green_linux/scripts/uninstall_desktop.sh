#!/usr/bin/env bash
set -euo pipefail

resolve_home() {
  local fallback="${HOME:-}"
  if command -v getent >/dev/null 2>&1; then
    local entry
    entry="$(getent passwd "$(id -un)" 2>/dev/null || true)"
    if [[ -n "$entry" ]]; then
      local home_dir
      home_dir="$(cut -d: -f6 <<<"$entry" || true)"
      if [[ -n "$home_dir" ]]; then
        printf '%s' "$home_dir"
        return
      fi
    fi
  fi
  printf '%s' "$fallback"
}

CANONICAL_HOME="$(resolve_home)"
CANONICAL_HOME="${CANONICAL_HOME:-${HOME:-}}"
if [[ -n "${XDG_DATA_HOME:-}" && "$XDG_DATA_HOME" != "$CANONICAL_HOME/.local/share" ]]; then
  if [[ "$XDG_DATA_HOME" == */snap/* ]]; then
    DATA_HOME="$CANONICAL_HOME/.local/share"
  else
    DATA_HOME="$XDG_DATA_HOME"
  fi
else
  DATA_HOME="$CANONICAL_HOME/.local/share"
fi
DEST_DIR="$DATA_HOME/applications"
DESKTOP="$DEST_DIR/powerapp.desktop"
RUN_SCRIPT="$CANONICAL_HOME/.local/bin/powerapp-run"
ICON_PATH="$CANONICAL_HOME/.local/share/icons/hicolor/scalable/apps/powerapp.svg"
PNG_PATH="$CANONICAL_HOME/.local/share/icons/hicolor/128x128/apps/powerapp.png"

if [[ ! -e "$DESKTOP" && ! -e "$RUN_SCRIPT" && ! -e "$ICON_PATH" && ! -e "$PNG_PATH" ]]; then
  echo "Nothing to remove (no desktop entry, wrapper, or icon found)."
  exit 0
fi

if [[ -e "$DESKTOP" ]]; then
  rm -f "$DESKTOP"
  echo "Removed $DESKTOP"
fi
if [[ -e "$RUN_SCRIPT" ]]; then
  rm -f "$RUN_SCRIPT"
  echo "Removed $RUN_SCRIPT"
fi
if [[ -e "$ICON_PATH" ]]; then
  rm -f "$ICON_PATH"
  echo "Removed $ICON_PATH"
fi
if [[ -e "$PNG_PATH" ]]; then
  rm -f "$PNG_PATH"
  echo "Removed $PNG_PATH"
fi

# Try to update desktop database and icon cache
if command -v update-desktop-database >/dev/null 2>&1; then
  update-desktop-database "${DEST_DIR}" >/dev/null 2>&1 || true
fi
if command -v gtk-update-icon-cache >/dev/null 2>&1; then
  gtk-update-icon-cache "$CANONICAL_HOME/.local/share/icons/hicolor" >/dev/null 2>&1 || true
fi

echo "Uninstall complete."
