#!/usr/bin/env bash
set -euo pipefail

# Lightweight helper to run the PowerApp GUI with basic environment checks.
# Usage:
#   chmod +x scripts/run_gui.sh
#   ./scripts/run_gui.sh [--no-gtk-check] [args...]

NO_GTK_CHECK=0
if [[ "${1:-}" == "--no-gtk-check" ]]; then
  NO_GTK_CHECK=1
  shift
fi

# Prefer activated venv, then .venv, then system python
if [[ -n "${VIRTUAL_ENV:-}" && -x "$VIRTUAL_ENV/bin/python" ]]; then
  PY="$VIRTUAL_ENV/bin/python"
elif [[ -x ".venv/bin/python" ]]; then
  PY=".venv/bin/python"
else
  if command -v python3 >/dev/null 2>&1; then
    PY="$(command -v python3)"
  else
    PY="$(command -v python)"
  fi
fi

echo "Using python: $PY"

if [[ $NO_GTK_CHECK -eq 0 ]]; then
  echo "Checking PyGObject / GTK availability..."
  set +e
  "$PY" - <<'PY' >/dev/null 2>&1
try:
    import gi
    gi.require_version('Gtk', '4.0')
except Exception as e:
    raise SystemExit(2)
PY
  rc=$?
  set -e
  if [[ $rc -ne 0 ]]; then
    echo "Error: PyGObject / GTK 4.0 not available in this Python environment." >&2
    echo "On Debian/Ubuntu install: sudo apt install python3-gi gir1.2-gtk-4.0" >&2
    echo "Or activate your project's virtualenv that has PyGObject available." >&2
    exit 2
  fi
fi

# Ensure project root is on PYTHONPATH so 'python -m powerapp.gtk.main' can import the package
proj_root="$(cd "$(dirname "$0")/.." && pwd)"
if [[ -z "${PYTHONPATH:-}" ]]; then
  export PYTHONPATH="$proj_root"
else
  export PYTHONPATH="$proj_root:$PYTHONPATH"
fi
echo "Using PYTHONPATH: $PYTHONPATH"

# Finally, run the GUI module
exec "$PY" -m powerapp.gtk.main "$@"
