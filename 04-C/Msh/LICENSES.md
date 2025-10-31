# LICENSES and Third-Party Attributions

This repository (Msh) includes third-party components and is distributed under terms described below.

## Included upstream components

- `upstream_bash/` — GNU Bash sources included verbatim. License: GNU General Public License version 3 (GPL-3.0-or-later).
 - `upstream_bash/` — GNU Bash sources included verbatim. License: GNU General Public License version 3 (GPL-3.0-or-later).
  - The full license text is included at `upstream_bash/COPYING`.
  - A short vendor notice is included at `upstream_bash/NOTICE`.

Clarification: The GNU Bash sources under `upstream_bash/` are upstream code maintained by the Free Software Foundation and contributors. The Msh project did not author GNU Bash. Per-file modification notices will be present only for files that have been changed in this repository.

## Project files

The files in the top-level of this repository (for example `Msh.c`, `run_builtin_command.c`, `jobs.c`, `regex_wrapper.*`) are part of the Msh project and are licensed under the MIT License. The canonical MIT text is provided at the repository root in `LICENSE`. Per-file SPDX short identifiers are present in source files and should be consulted for machine-readable tooling.

## Corresponding Source and Distribution


If you distribute compiled binaries that include GPL-licensed code from `upstream_bash/`, you must provide the Corresponding Source in one of the ways permitted by GPLv3 section 6 (for example, provide the full source tree alongside the distribution or provide a written offer / public URL where the source is available).

See `SOURCE_OFFER.txt` for a ready-to-use written-offer template and guidance for providing the Corresponding Source when distributing object code or binaries.


## GFDL reference

One source file (`Msh.c`) contains a short printed reference to the GNU Bash manual / GFDL. That single-line citation does not, by itself, import GFDL obligations. If you include longer manual text under the GFDL, follow the GFDL terms for that material.

## Recommended repository practices (already applied)

- Keep `upstream_bash/COPYING` and any upstream per-file copyright headers intact (do not remove or alter them).
- Keep `upstream_bash/NOTICE` (added) and `vendor-README.md` (added) for provenance.
- Provide a top-level `LICENSES.md` (this file) describing included components and where to find source.
- Add SPDX short identifiers and small copyright/notice headers to your own source files (done in this commit for main project files).
 - Add SPDX short identifiers and small copyright/notice headers to your own source files (done in this commit for main project files and many tests/scripts).

## Audit artifacts

- A token-based similarity audit has been run and the final report is available at `similarity_report_final.json` (k=8, w=3). The scan excluded the `upstream_bash/` vendor directory and shows only one small overlap with test data (`TEST/BBBBB.txt`).

Note: I added SPDX identifiers to a set of source and test files and updated `SOURCE_OFFER.txt` to prefer including the Corresponding Source in this repository. If you want, I can run a repo-wide SPDX insertion pass for every recognized source file (this can be done next).

## Binaries in the repository

I found pre-built Windows executables (*.exe) in the repository root (for example `Msh.exe`, `Msh_build.exe`, `less.exe`, `kill.exe`, and test binaries). If you distribute these binaries (or allow others to download them), GPLv3 requires you to provide the Corresponding Source as well (either included directly or via a written offer/public URL). Options:

- Remove compiled binaries from the public repository and provide only source code.
- If keeping binaries, ensure `SOURCE_OFFER.txt` contains a working URL or contact and that the Corresponding Source is available there (or include the source in the same release).

Keeping binaries in the repo is fine for private development, but for public distribution make sure the Corresponding Source is provided in a compliant way.


## Questions or legal certainty

This document provides practical guidance, not legal advice. For binding legal interpretations about distribution obligations or license compatibility, consult a qualified attorney.
