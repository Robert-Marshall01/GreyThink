# Contributing

Thank you for your interest in contributing to this collection of Fortran calculus demos. This document explains how to run the code, report problems, and submit improvements.

1. Run and reproduce
   - Build the Fortran program with a Fortran-2008-capable compiler (example using `gfortran`):

     ```powershell
     gfortran -fopenmp -std=f2008 -ffree-line-length-none -O2 fortran_calculus.f90 -o fortran_calculus.exe
     ```

   - Run a small demo rather than the whole program for quicker feedback:

     ```powershell
     .\fortran_calculus.exe --demo rk4_decay
     ```

2. Reporting issues
   - Open an issue with a concise title and the following information:
     - Steps to reproduce (commands and options used)
     - Expected behavior
     - Actual behavior (include error messages or output snippets)
     - Environment: OS, compiler and version (`gfortran --version`), and Python version if relevant
     - Any relevant log files or CSV outputs (attach if small; for large files, provide a short sample)

3. Submitting changes (pull requests)
   - Fork the repository and create a descriptive branch name (e.g. `fix/romberg-index` or `feat/lesson-01`).
   - Keep changes focused and small; prefer one logical change per PR.
   - Include a short description of what the change does and why (tests or sample outputs help reviewers).
   - If your change modifies output formats or CSV headers, document the change in `README.md`.

4. Coding style and notes
   - The project targets Fortran 2008 free-form source. Prefer readable names and avoid obfuscated one-letter variables in new code.
   - Use `selected_real_kind` or `iso_fortran_env` kinds consistently when adding numerical routines.
   - Keep demos self-contained: minimize external dependencies and avoid writing large binary files by default.

5. Tests and verification
   - There is no full automated test suite yet. When adding or changing core numerical functions, please include a short validation program or a small test driver that checks results against known analytic values.

6. Documentation and examples
   - Improve `README.md` when you add or change demos: include compile/run examples and sample output when feasible.

7. Licensing, attribution, and AI assistance
   - This repository is licensed under the MIT License (see `LICENSE`). If you contribute, your contribution will be covered by the same license.
   - If you adapt code or text from other sources, add a short attribution comment near the code and report the source in an issue or a `THIRD_PARTY_NOTICES.md` file.
   - Some content in this repository was assisted by AI tools. When contributing AI-generated content, please indicate this in the PR description.

8. IP and patents
   - The author is not aware of any patent claims covering the material included here. Public posting does not guarantee freedom from patents — if you plan to use code in a commercial product, consider an IP review.

9. Contact and etiquette
   - Be respectful and constructive. If you are unsure about a change, open an issue first to discuss the idea.

Thanks again — contributions that improve clarity, correctness, reproducibility, and teaching value are very welcome.
