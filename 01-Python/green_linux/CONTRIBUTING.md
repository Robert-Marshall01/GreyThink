Contributing guidelines

Developer setup (short)

> See `docs/HELP.md` for a more complete developer setup and environment notes.

1) Create & activate a venv

   python -m venv .venv
   source .venv/bin/activate

2) Install package and developer extras

   pip install -e .[dev]

Optional: ML toolchain (ONNX / sklearn)

   pip install -e .[ml]

Note about GUI / PyGObject

PyGObject (the `gi` bindings) is typically provided by the OS package manager. On Debian/Ubuntu, install:

   sudo apt install python3-gi gir1.2-gtk-4.0

Recommended local files (do not commit)

- Virtual environments: `.venv/`, `.env/`, `venv/` — keep these local and add them to `.gitignore`.
- Local caches and logs: `.cache/`, `*.log`.
- Editor and OS files: `.vscode/`, `.idea/`, `*.swp`, `.DS_Store`.

Please read the project README and follow its development setup instructions before contributing.

Visual testing (golden images)

- Purpose: We track a small set of golden images for UI components (currently the simulator mini-timeline) to detect visual regressions that might affect UX.

- Regenerating the golden locally:
  1. Ensure dev dependencies are installed:
     python -m pip install -r requirements-dev.txt
     (pycairo may require system packages: pkg-config libcairo2-dev python3-dev)
  2. Start Xvfb if running headless (Linux):
     Xvfb :99 -screen 0 1280x1024x24 & export DISPLAY=:99
  3. Generate the golden image and metadata:
     python scripts/generate_golden_simulator_image.py
  4. Review the generated files under `tests/golden/` and commit them with a clear message.

- What to include in a PR that changes visuals:
  - If a change intentionally modifies visuals, regenerate the golden image and commit both the PNG and the JSON metadata in `tests/golden/` and add a short note in your PR description saying "Updated simulator golden after visual change".
  - If the visual change is unintentional, CI will flag a mismatch; please investigate and either revert the change or update the golden as above after confirmation.

- CI behavior:
  - On pull requests that touch drawing-related files (e.g., `powerapp/gtk/`), CI will regenerate the golden and validate structural metadata (per-app bars) first; if that fails it will fall back to a pixel diff and attach a diff artifact.

Thanks for keeping visuals intentional and reviewed!