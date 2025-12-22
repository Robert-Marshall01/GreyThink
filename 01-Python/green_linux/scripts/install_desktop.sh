#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: install_desktop.sh [options]

Options:
  --dry-run             only print the installer actions without writing files
  --download-url URL    fetch a release archive and install the extracted tree
  --install-dir DIR     install the app under DIR instead of XDG_DATA_HOME/powerapp
  --help                show this message and exit
USAGE
}

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

info() {
  printf 'powerapp-install: %s\n' "$*"
}

DRY=0
DOWNLOAD_URL=""
INSTALL_DIR=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run)
      DRY=1
      shift
      ;;
    --download-url)
      if [[ $# -lt 2 ]]; then
        echo "--download-url requires an argument" >&2
        usage >&2
        exit 1
      fi
      DOWNLOAD_URL="$2"
      shift 2
      ;;
    --install-dir)
      if [[ $# -lt 2 ]]; then
        echo "--install-dir requires an argument" >&2
        usage >&2
        exit 1
      fi
      INSTALL_DIR="$2"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 1
      ;;
  esac
done

TMP_DOWNLOAD_DIR=""
cleanup_temp() {
  if [[ -n "${TMP_DOWNLOAD_DIR:-}" && -d "$TMP_DOWNLOAD_DIR" ]]; then
    rm -rf "$TMP_DOWNLOAD_DIR"
  fi
}
trap cleanup_temp EXIT

download_and_extract() {
  local url="$1"
  local dest_dir
  dest_dir="$(mktemp -d "${TMPDIR:-/tmp}/powerapp-install.XXXXXX")"
  TMP_DOWNLOAD_DIR="$dest_dir"
  local archive="$dest_dir/archive"
  info "Downloading $url"
  if command -v curl >/dev/null 2>&1; then
    curl -fL "$url" -o "$archive"
  elif command -v wget >/dev/null 2>&1; then
    wget -qO "$archive" "$url"
  else
    echo "curl or wget is required to download $url" >&2
    exit 1
  fi
  local mime
  mime="$(file --brief --mime-type "$archive" 2>/dev/null || true)"
  local extract_dir="$dest_dir/extracted"
  mkdir -p "$extract_dir"
  case "$mime" in
    application/zip)
      if ! command -v unzip >/dev/null 2>&1; then
        echo "unzip is required to extract $archive" >&2
        exit 1
      fi
      unzip -q "$archive" -d "$extract_dir"
      ;;
    application/x-gzip|application/gzip|application/x-xz|application/x-bzip2|application/x-tar|"")
      tar -xf "$archive" -C "$extract_dir"
      ;;
    *)
      if tar -xf "$archive" -C "$extract_dir" >/dev/null 2>&1; then
        :
      elif unzip -q "$archive" -d "$extract_dir" >/dev/null 2>&1; then
        :
      else
        echo "Unable to extract $archive" >&2
        exit 1
      fi
      ;;
  esac
  local top_dir
  top_dir="$(find "$extract_dir" -mindepth 1 -maxdepth 1 -type d | head -n 1)"
  if [[ -n "$top_dir" ]]; then
    printf '%s' "$top_dir"
  else
    printf '%s' "$extract_dir"
  fi
}

sync_source() {
  local src="$1"
  local dest="$2"
  if [[ "$src" == "$dest" ]]; then
    return
  fi
  mkdir -p "$dest"
  if command -v rsync >/dev/null 2>&1; then
    rsync -a --delete --delete-excluded \
      --exclude='.git' \
      --exclude='.venv' \
      --exclude='__pycache__' \
      "$src/" "$dest/"
  else
    rm -rf "$dest"
    mkdir -p "$dest"
    cp -a "$src/." "$dest/"
  fi
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_TREE="$(cd "$SCRIPT_DIR/.." && pwd)"
SOURCE_ROOT="$SOURCE_TREE"

if [[ -n "$DOWNLOAD_URL" ]]; then
  if [[ $DRY -eq 1 ]]; then
    info "Dry run: would download $DOWNLOAD_URL and install from its extracted tree"
  else
    SOURCE_ROOT="$(download_and_extract "$DOWNLOAD_URL")"
  fi
fi

SOURCE_ROOT="$(cd "$SOURCE_ROOT" && pwd -P)"
if [[ ! -d "$SOURCE_ROOT" ]]; then
  echo "Could not find source tree: $SOURCE_ROOT" >&2
  exit 1
fi

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

APP_INSTALL_DIR="${INSTALL_DIR:-$DATA_HOME/powerapp}"
mkdir -p "$APP_INSTALL_DIR"
APP_INSTALL_DIR="$(cd "$APP_INSTALL_DIR" && pwd -P)"

DESKTOP_DIR="${CANONICAL_HOME}/Desktop"
if command -v xdg-user-dir >/dev/null 2>&1; then
  XDG_DESKTOP_DIR="$(xdg-user-dir DESKTOP 2>/dev/null || true)"
  if [[ -n "$XDG_DESKTOP_DIR" ]]; then
    if [[ "$XDG_DESKTOP_DIR" == "~" ]]; then
      XDG_DESKTOP_DIR="$CANONICAL_HOME"
    elif [[ "$XDG_DESKTOP_DIR" == "~/"* ]]; then
      XDG_DESKTOP_DIR="$CANONICAL_HOME/${XDG_DESKTOP_DIR:2}"
    fi
    DESKTOP_DIR="$XDG_DESKTOP_DIR"
  fi
fi
DESKTOP_ENTRY_DEST="$DESKTOP_DIR/powerapp.desktop"

if [[ $DRY -eq 1 ]]; then
  info "Dry run: would sync $SOURCE_ROOT to $APP_INSTALL_DIR"
else
  sync_source "$SOURCE_ROOT" "$APP_INSTALL_DIR"
fi

INSTALL_ROOT="$APP_INSTALL_DIR"

SRC_DESKTOP="$SCRIPT_DIR/powerapp.desktop"
ICON_SRC="$SCRIPT_DIR/assets/powerapp.svg"
PNG_SRC="$SCRIPT_DIR/assets/powerapp-128.png"

DEST_DIR="$DATA_HOME/applications"
mkdir -p "$DEST_DIR"

BIN_DIR="${CANONICAL_HOME}/.local/bin"
mkdir -p "$BIN_DIR"
RUN_SCRIPT="$BIN_DIR/powerapp-run"

escaped_runner="${RUN_SCRIPT//\\/\\\\}"
escaped_runner="${escaped_runner//&/\&}"
escaped_runner="${escaped_runner//|/\|}"
EXEC_REPLACEMENT="Exec=$escaped_runner"

cat > "$RUN_SCRIPT" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
REPO_ROOT="__INSTALL_ROOT__"
VENV_PY="$REPO_ROOT/.venv/bin/python"
if [[ -x "/usr/bin/python3" ]]; then
  SYS_PY="/usr/bin/python3"
else
  SYS_PY="$(command -v python3 || command -v python || true)"
fi

_check_gi() {
  "${1}" -c "import gi" >/dev/null 2>&1
}

PY=""
if [[ -x "$VENV_PY" ]]; then
  if _check_gi "$VENV_PY"; then
    PY="$VENV_PY"
  elif [[ -n "$SYS_PY" ]] && _check_gi "$SYS_PY"; then
    PY="$SYS_PY"
    mkdir -p "$HOME/.cache/powerapp" 2>/dev/null || true
    echo "$(date) - venv exists but gi not found, using system python ($PY)" >> "$HOME/.cache/powerapp/launch.log" 2>/dev/null || true
  fi
else
  if [[ -n "$SYS_PY" ]] && _check_gi "$SYS_PY"; then
    PY="$SYS_PY"
  fi
fi

if [[ -z "$PY" ]]; then
  MSG="PowerApp failed to start: missing PyGObject/GTK (install 'python3-gi gir1.2-gtk-4.0') or run the dev launcher ('./scripts/run_gui.sh')."
  if command -v notify-send >/dev/null 2>&1; then
    notify-send "PowerApp: Start failed" "$MSG"
  elif command -v zenity >/dev/null 2>&1; then
    zenity --error --title="PowerApp: Start failed" --text="$MSG"
  fi
  mkdir -p "$HOME/.cache/powerapp" 2>/dev/null || true
  echo "$(date) - Launch failed: gi not importable in venv or system python" >> "$HOME/.cache/powerapp/launch.log" 2>/dev/null || true
  exit 1
fi

exec "$PY" -m powerapp.gtk.main "$@"
SH

sed -i "s|__INSTALL_ROOT__|$INSTALL_ROOT|g" "$RUN_SCRIPT"
chmod +x "$RUN_SCRIPT"

ICON_DEST_DIR="$CANONICAL_HOME/.local/share/icons/hicolor/scalable/apps"
mkdir -p "$ICON_DEST_DIR"
ICON_DEST=""
if [[ -f "$ICON_SRC" ]]; then
  ICON_DEST="$ICON_DEST_DIR/powerapp.svg"
  if [[ $DRY -eq 1 ]]; then
    info "Dry run: would copy $ICON_SRC to $ICON_DEST"
  else
    cp "$ICON_SRC" "$ICON_DEST"
  fi
fi

PNG_DEST_DIR="$CANONICAL_HOME/.local/share/icons/hicolor/128x128/apps"
mkdir -p "$PNG_DEST_DIR"
if [[ -f "$PNG_SRC" ]]; then
  if [[ $DRY -eq 1 ]]; then
    info "Dry run: would copy $PNG_SRC to $PNG_DEST_DIR/powerapp.png"
  else
    cp "$PNG_SRC" "$PNG_DEST_DIR/powerapp.png"
  fi
fi

if [[ $DRY -eq 1 ]]; then
  info "Dry run: would write desktop entry to $DEST_DIR/powerapp.desktop pointing at $RUN_SCRIPT"
  info "Dry run: would also drop a clickable desktop icon at $DESKTOP_ENTRY_DEST"
  exit 0
fi

if [[ -n "$ICON_DEST" ]]; then
  sed -e "s|Exec=\"\${HOME}/.local/bin/powerapp-run\"|$EXEC_REPLACEMENT|" \
      -e "s|Icon=|Icon=powerapp|" "$SRC_DESKTOP" > "$DEST_DIR/powerapp.desktop"
else
  sed "s|Exec=\"\${HOME}/.local/bin/powerapp-run\"|$EXEC_REPLACEMENT|" "$SRC_DESKTOP" > "$DEST_DIR/powerapp.desktop"
fi

chmod 644 "$DEST_DIR/powerapp.desktop"

if command -v update-desktop-database >/dev/null 2>&1; then
  update-desktop-database "$DEST_DIR" >/dev/null 2>&1 || true
fi

if [[ -n "$DESKTOP_DIR" ]]; then
  mkdir -p "$DESKTOP_DIR"
  DESKTOP_DIR="$(cd "$DESKTOP_DIR" && pwd -P)"
  DESKTOP_ENTRY_DEST="$DESKTOP_DIR/powerapp.desktop"
  cp "$DEST_DIR/powerapp.desktop" "$DESKTOP_ENTRY_DEST"
  chmod 755 "$DESKTOP_ENTRY_DEST"
  info "Installed clickable desktop icon at $DESKTOP_ENTRY_DEST"
fi

info "Installed $DEST_DIR/powerapp.desktop"
info "App files are stored under $APP_INSTALL_DIR"
