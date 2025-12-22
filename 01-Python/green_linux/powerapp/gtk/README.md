PowerApp GNOME/GTK Prototype

**Status:** Prototype / Proof‑of‑Concept — intended as a forkable starter for experimentation and development. We used AI-assisted tooling to accelerate prototyping; core application flows are covered by tests and key paths have human review where practical. There are documented limitations and targeted TODOs (see **Known issues**); contributions, reviews, and test improvements are welcome.

**What works:**
- GTK UI and main application flow
- Power sampling with mock mode for testing on systems without hardware measurement
- Carbon intensity display with refresh functionality
- Postponement suggestions with proper error messaging and task management
- Low-carbon window simulation and visualization with timezone-aware displays
- CSV export with timezone conversion and proper formatting
- Export preview with sample validation
- Applied postponements tracking and management
- Desktop launcher and wrapper scripts (when PyGObject is available)
- Included lightweight window predictor model for local inference
- CLI and helper scripts for development and testing

**Known issues / limitations:**
- Settings persistence and validation needs refinement (some edge cases in save/load)
- The repository `.venv` may not include PyGObject; the launcher may fall back to the system Python (see troubleshooting)
- Some features are partial or experimental (per-app legend behavior, automatic profile application)
- Some code was AI-assisted and may contain inaccuracies or incomplete implementations; critical paths should be reviewed before production use
- Test coverage is incomplete in some modules — see `tests/` for current state
- Sandboxed environments (Flatpak/Snap) may limit functionality

Run locally (developer machine):

Install for development (recommended)

- Create & activate a virtual environment:

  python -m venv .venv
  source .venv/bin/activate

- Install the package and developer extras:

  pip install -e .[dev]

Optional: ML toolchain (ONNX / sklearn) for model export and quant tests:

  pip install -e .[ml]

How to verify (quick smoke tests):

- Run the full test suite (recommended after `pip install -e .[dev]`):

  pytest -q

- Quick GUI smoke test (requires PyGObject):

  python -m powerapp.gtk.main

- ML/ONNX checks (optional, requires `pip install -e .[ml]`):

  pytest tests/test_onnx_model.py -q || echo "ONNX tests skipped or require extras"

1) Ensure GTK and PyGObject are installed (Ubuntu):
   sudo apt install python3-gi gir1.2-gtk-4.0

2) Run the prototype:
   python3 -m powerapp.gtk.main

Convenience script
- Make the run script executable and use it to run the app (it checks for a venv and GTK availability):

  chmod +x ../../scripts/run_gui.sh
  ./scripts/run_gui.sh

You can pass `--no-gtk-check` to skip the PyGObject/GTK availability check if you already know your environment is configured.

Create a desktop launcher (optional)
- To install a user-level desktop shortcut that launches the app via a small wrapper script:

  chmod +x ../../scripts/install_desktop.sh
  ./scripts/install_desktop.sh

Uninstalling the desktop entry
- To remove the installed desktop entry, wrapper and icon use:

  chmod +x ../../scripts/uninstall_desktop.sh
  ./scripts/uninstall_desktop.sh

This will remove `~/.local/share/applications/powerapp.desktop`, `~/.local/bin/powerapp-run` and the installed SVG icon in `~/.local/share/icons/hicolor/scalable/apps`.

- To preview the actions without installing: `./scripts/install_desktop.sh --dry-run`
- This will copy `scripts/powerapp.desktop` to `~/.local/share/applications`, install the new Power+Leaf icon to `~/.local/share/icons/hicolor/scalable/apps/powerapp.svg`, and create a wrapper `~/.local/bin/powerapp-run` that picks the project `.venv` if available.

### If the desktop icon doesn't start PowerApp ⚠️
If clicking the desktop icon does not launch the app, try the following troubleshooting steps to see errors and fix common problems:

- Run the wrapper directly in a terminal to observe output and errors:

  ```sh
  ~/.local/bin/powerapp-run
  # or from the repo root:
  ./scripts/run_gui.sh
  ```

- Check the launcher and logs:

  ```sh
  cat ~/.local/share/applications/powerapp.desktop   # ensure Exec= points to the wrapper
  ls -l ~/.local/bin/powerapp-run                    # ensure it's executable
  tail -n 200 ~/.cache/powerapp/launch.log           # inspect recent launch errors and diagnostics
  ```

- Common fixes:
  - Install PyGObject/GTK: `sudo apt install python3-gi gir1.2-gtk-4.0`
  - Make wrapper executable: `chmod +x ~/.local/bin/powerapp-run`
  - Update desktop DB (if icon or menu entries are stale): `update-desktop-database ~/.local/share/applications`

- For development you can also run the app directly:

  ```sh
  python3 -m powerapp.gtk.main
  ```

This troubleshooting section helps when the desktop icon does not launch due to missing dependencies, PATH, or permission issues.

Run if clicking on the app icon fails to render the GUI:

```
cd ~/Desktop/green_linux
source .venv/bin/activate
pip install PyGObject
./scripts/install_desktop.sh
```
run directly in the repo directory to start the app
======================================================================================================
./scripts/run_gui.sh
======================================================================================================

Before clicking the icon again, confirm the log under `~/.cache/powerapp/launch.log` reports `Starting PowerApp with /home/.../.venv/bin/python` instead of `/usr/bin/python3`.

Preview: the icon is a green leaf shaped mark with an embedded power glyph to emphasize "energy-aware power insights" and scales well for desktop menus.
Notes:
- The prototype calls `powerapp.cli.power_reader.get_sample()` to fetch measurements.
- RAPL sampling may require root or the privileged helper; upower will work on laptops with proper battery support.
- This is an early prototype; styling, graphs, and actionable buttons will be added next.

## Limitations & Known Issues ⚠️

- **Experimental / Incomplete:** Some features are still prototypes and may be unavailable or only partially functional in this repository (e.g., visualizations, chatbot, per-app legend behavior, and automatic profile application).
- **External requirements:** Several features depend on external system services or permissions (power-profiles daemon, systemd user timers, calendar/EDS access, RAPL/root helpers). If these services or privileges are missing, the related features will not work.
- **Sandboxing caveats:** Functionality may be limited in sandboxed environments (Flatpak, Snap) where system integrations and helper tools are restricted. If something appears to “not work”, check logs and system permissions first.
- **Development note:** We're actively iterating; behave as an early-stage prototype and feel free to open issues with logs and reproduction steps.