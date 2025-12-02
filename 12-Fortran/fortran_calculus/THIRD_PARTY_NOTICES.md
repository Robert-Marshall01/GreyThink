# THIRD-PARTY NOTICES

This project references a small set of third-party Python packages used by the optional plotting and data utilities. These packages are not bundled in this repository; consult each project's PyPI page or source repository for the exact license text and version-specific terms.

Notes

- The exact license text and terms may vary by package version. To inspect the license for an installed package, run `pip show <package>` or consult the package's GitHub/PyPI page.
- This repository previously contained a committed virtual environment (`.venv`). We recommend removing `.venv/` from the repo and keeping it listed in `.gitignore` to avoid embedding third-party packages and their files.
- If you (the maintainer) copied material from external sources into the Fortran source files, add explicit attribution and license information here or in `README.md`.

If you'd like, I can attempt to collect the license files for the packages installed in a local `.venv` (if present) and append them here with exact version numbers. Tell me if you want that.

This project uses or references the following third-party Python packages for optional plotting and data work. The Fortran source is original to this repository unless otherwise noted. The packages below are not bundled in this repository (they are installed via `pip`); check each upstream project for the exact license text and version-specific terms.

## Python dependencies (from `requirements.txt`)

- numpy
  - PyPI: [numpy on PyPI](https://pypi.org/project/numpy/)
  - License information: see the project page above (NumPy is generally BSD-style licensed)

- matplotlib
  - PyPI: [matplotlib on PyPI](https://pypi.org/project/matplotlib/)
  - License information: see the project page above (Matplotlib uses its own license; consult upstream docs)

- pandas
  - PyPI: [pandas on PyPI](https://pypi.org/project/pandas/)
  - License information: see the project page above (pandas uses a BSD-style license)

- scipy
  - PyPI: [scipy on PyPI](https://pypi.org/project/scipy/)
  - License information: see the project page above (SciPy is BSD-licensed)

## Notes and recommendations

- The exact license terms can vary by package version. If you require precise licensing information, consult the package metadata for the version you install (for example: `pip show numpy`) or check the project's repository or PyPI page.
- This repository previously contained a committed virtual environment (`.venv`). Committed virtual environments embed copies of many third-party packages (and their license files) into the repo; we recommend removing `.venv/` from the repository and using `.gitignore` to avoid re-committing it.
- If you (the maintainer) recall copying code or text from other external sources into the Fortran files, please add explicit attribution here or in `README.md` and provide links and license information.

If you'd like, I can attempt to automatically collect each installed package's license text (if `.venv` is present) or add direct license links for specific versions — tell me which you prefer.
