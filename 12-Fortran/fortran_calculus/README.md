# Fortran Calculus Demos — Visualization Utilities

This repository contains a large, monolithic Fortran demo program `fortran_calculus.f90` (builds to `fortran_calculus.exe`) plus lightweight Python utilities to visualize CSV output. The code is intended primarily for educational and showcase purposes: short demos, numerical experiments, and illustrative diagnostics.

## Quick overview

- Purpose: educational demos of numerical calculus (derivatives, integrals, ODE integrators, fractional derivatives, stochastic demos, spectral methods, and many more).
- Structure: the main program contains many small demo subroutines that print or write CSV files. Use `--demo NAME` to run a single demo.
- Visualization: `plot_demo.py` reads CSV files produced by the Fortran program and generates PNGs.

## Requirements

- Fortran compiler: `gfortran` (recommended) or any Fortran 2008-compatible compiler.

  - Recommended `gfortran` version: 9.0 or later. Quad precision support and behavior may vary by compiler.

- Python 3.8+ for plotting utilities.

- Python dependencies (see `requirements.txt`):

  - `numpy` ~= 1.20
  - `matplotlib` ~= 3.3
  - `pandas` (optional) ~= 1.2

These versions are suggestions that were current when the project was prepared. The Python tools are optional — the Fortran demos can be run without Python.

## Build & run (examples)

Compile with `gfortran` (Windows PowerShell example):

```powershell
# compile (simple example)
gfortran -fopenmp -std=f2008 -ffree-line-length-none -O2 fortran_calculus.f90 -o fortran_calculus.exe

# run the full demo (very verbose)
.\fortran_calculus.exe

# run a single demo by name (recommended for classroom use)
.\fortran_calculus.exe --demo rk4_decay
```

Notes:

- Quad precision (if used by routines in the code) is compiler-dependent. On `gfortran` you may need `-fdefault-real-8` or `-fdefault-real-16` or link `-lquadmath` depending on your setup. If you rely on quad checks and see strange behavior, try different compiler flags or consult your compiler docs.

## Visualization (optional)

Install Python dependencies and run the plotting helper:

```powershell
python -m pip install -r requirements.txt
python plot_demo.py --all
```

You can also run specific plots (see `plot_demo.py --help`).

## Educational / Usage Note

This repository is intended primarily for education, demonstration, and exploration. The code collects many small demos and diagnostics to illustrate numerical ideas — it is not intended as production numerical software.

The program's output and code are verbose and may require some computer skills (command-line familiarity, basic scripting or Fortran knowledge) to use effectively.

Because the repository contains many dense demos and can produce large, verbose output and CSV files, running the full program is not recommended on legacy or resource-constrained hardware — it may use significant CPU, memory, or disk I/O. Prefer running individual demos via `--demo` on older machines.

Note: not every part of this program has been exhaustively tested. Some demos or functions may contain bugs, numerical instabilities, or incorrect implementations — please verify outputs before using them for teaching or research.

## AI generation / modification note & disclaimer

- Parts of the codebase and accompanying text (comments, helper formulas, and some implementations) were generated or modified with the assistance of an AI tool.
- The author is not a professional mathematician; the formulas, implementations, and numerical choices may contain simplifications or mistakes. The code is provided as illustrative material and should not be used as a substitute for a carefully vetted numerical library or as authoritative mathematical proof.
- If you use this code for teaching or research, please review and verify the math and numerical behavior for your specific use case. Corrections and improvements are welcome.

- Because parts of this repository were generated or modified by an AI tool, the code and accompanying text may contain bugs or unintended errors; please review, test, and verify outputs before relying on them.

## Contributing

If you'd like the repository to be more classroom-friendly I recommend the following small improvements (I can help):

- Split demos into topic modules (`derivatives`, `integration`, `odes`, `stochastic`, ...)
- Add short lesson drivers that run small, focused examples
- Add unit tests for core functions
- Add a `--list` flag to print available demos concisely

I'm open to suggestions on how to improve the program — feel free to open an issue or a pull request with ideas, fixes, or enhancements.

## License

This repository contains original code and AI-assisted content.

## Legal / Attribution

- Mathematical formulas, identities, and algorithms (theory) are not subject to copyright; copyright applies to specific expressions, text, diagrams, and source code that implement or describe those ideas.
- The code in this repository is licensed under the MIT License (see `LICENSE`). By default that applies to code you (the author) wrote or substantially edited.
- Third‑party software: the Python packages listed in `requirements.txt` (for example `numpy`, `matplotlib`, `pandas`, `scipy`) are third‑party dependencies and carry their own licenses. Those packages are not included in this source tree (except via a committed virtual environment). Consult each package's upstream license for reuse terms.
- If you copied or adapted code, text, or examples from external sources (web posts, textbooks, other repos), please add short attribution near the relevant function and include the original source and license in a `THIRD_PARTY_NOTICES.md` or in an issue.
- Note: a committed `.venv` or other packaged virtual environment can contain many third‑party license files; it is best practice to remove `.venv` from the repository and add it to `.gitignore` to avoid bundling external packages unintentionally.
- Patents and other rights: algorithms can sometimes be subject to patents or other IP — this README does not analyze patent risk. If you plan to commercialize or redistribute widely, perform appropriate legal review.

If you want, I can create a `THIRD_PARTY_NOTICES.md` that lists the Python dependencies and where to find their licenses, and prepare a `.gitignore` update to exclude `.venv` from the repo. Which would you prefer?

### IP note

No patent claims are known to the author for the code in this repository. However, the public availability of the code does not guarantee that specific implementations or optimizations are free of patent encumbrances. If you plan to redistribute or use this code in a commercial product, consider an IP review or legal advice.
