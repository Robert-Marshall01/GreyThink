# Graph Plotter

This project now provides a small GUI plotter that places input fields on the
left and the interactive plot on the right. It accepts comma- or
whitespace-separated numeric values for X, Y and an optional Z column (Z is
used for color/size in scatter plots).

Requirements
- Python 3.8+
- Install dependencies:

```powershell
python -m pip install -r requirements.txt
```

Create a virtual environment (recommended)

It's best practice to create a per-project virtual environment rather than committing one to the repository. Below are platform-friendly commands you can run from PowerShell (Windows) or a POSIX shell (macOS / Linux).

Windows (PowerShell)

```powershell
# create the environment in a .venv folder
python -m venv .venv

# activate the venv (PowerShell)
# If activation is blocked by policy you can run PowerShell as admin and
# run: Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
& .\.venv\Scripts\Activate.ps1

# upgrade pip and install dependencies
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

POSIX (macOS / Linux)

```bash
# create and activate
python3 -m venv .venv
source .venv/bin/activate

# upgrade pip and install dependencies
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

Notes
- Don't commit `.venv/` to git — it's machine- and platform-specific. Add `.venv/` to `.gitignore` (there is a todo in the project to ensure this).
- Deactivate the environment with `deactivate` when finished.
 - Linux/macOS compatibility has not been tested for this GUI; the POSIX commands above are provided as a guideline and may need adjustment on your system.

Run the GUI

```powershell
python graph_plotter.py
```

Headless / CLI usage

You can also run the plotter in a headless (non-GUI) mode. This is useful for
quick scripting or CI. Example:

```powershell
python graph_plotter.py --nogui --x "1 2 3" --y "2 4 1" --plot-type Scatter --output out.png
```

Use `--open` to open the saved image automatically (on supported platforms):

```powershell
python graph_plotter.py --nogui --x "1 2 3" --y "2 4 1" --output out.png --open
```

Installable package

You can install the project locally and get a `graph-plotter` command:

```powershell
python -m pip install -e .
graph-plotter --help
```

Version

This project exposes a module version at the top of `graph_plotter.py` as
`__version__ = "0.1.0"`.

Publishing (optional)

If you'd like to publish this package to PyPI, follow these steps locally:

1. Install build tools:

```powershell
python -m pip install --upgrade build twine
```

2. Build distributions:

```powershell
python -m build
```

3. Upload to PyPI (recommended to use an API token):

```powershell
python -m twine upload dist/*
```

GitHub Actions release workflow

I added a GitHub Actions workflow (`.github/workflows/release.yml`) that will build and upload to PyPI when you push a tag that starts with `v` (for example `v0.1.0`). To enable automatic publishing, create a repository secret named `PYPI_API_TOKEN` with your PyPI token value.

Note: I did not publish to PyPI for you. If you want me to prepare a sample tag or help create the token and release, tell me and I'll prepare the steps.

Usage
- Enter values for X and Y (comma or space separated) in the left panel. Z is
  optional and is used for color/size in scatter plots.
- Choose the plot type (Line, Scatter, Bar) and click "Plot".
  New plot types: Histogram, Box, Hexbin, Pie, and Area are also available
  from the Plot type drop-down. Histogram uses Y (or X) as the data. Box shows
  a boxplot for numeric data. Hexbin produces a 2D density plot of X vs Y.
  Pie uses labels from X and sizes from Y. Area fills the line between X and
  Y.
- Click "Save PNG" to save the current plot.
- You can also load a CSV with columns X,Y(,Z) via the "Load CSV" button.

Notes
- The GUI uses tkinter and a matplotlib canvas embedded in the application.
- For large datasets prefer saving to CSV and loading into the GUI rather
  than pasting thousands of values into the text fields.
- If you need 3D plots or Excel-style formula parsing, tell me and I can
  extend the app further.
 - This application is very basic and may contain bugs. Use it for
   experimentation and expect rough edges; please report any issues you
   encounter.
