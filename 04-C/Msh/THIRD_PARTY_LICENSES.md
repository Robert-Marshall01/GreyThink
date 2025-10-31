THIRD-PARTY LICENSES
====================

This file lists third-party/vendor code included in this repository and the licensing obligations that apply when you redistribute or distribute binaries that include these components.

1) upstream_bash/
------------------
- Path: `upstream_bash/`
- License: GNU General Public License version 3, 29 June 2007 ("GPLv3") — "GPLv3 or later" (GPLv3+).
- Full text: `upstream_bash/COPYING` (included verbatim in this repo).
- Notes and obligations:
  - Keep `upstream_bash/COPYING` and all per-file copyright and license headers intact when redistributing or
    producing binary/object distributions that include any code from `upstream_bash/`.
  - If you distribute binaries that include GPL-covered code, you must provide the Corresponding Source for the GPL-covered parts (GPLv3 §6). Options include:
    - include the full `upstream_bash/` source tree with your release (recommended), or
    - provide a written offer (machine-readable) that supplies the Corresponding Source on request, or
    - provide a public, stable URL that hosts the Corresponding Source and ensure long-term availability.
  - If you modify upstream files, mark the files as modified and preserve the original copyright/license headers.

  - Provenance: `upstream_bash/` is included verbatim from the GNU Bash distribution. The Msh project does not claim authorship of GNU Bash or its upstream files. If specific files have been modified locally, those files must contain explicit per-file modification notices.

2) upstream_bash/lib/readline/COPYING
------------------------------------
- The `lib/readline` module (inside `upstream_bash/`) contains code with its own COPYING file; treat it as part of the GPL-covered upstream tree.

3) Other third-party items discovered
------------------------------------
- This repository contains a small number of helper utilities and test materials. Where a third-party license is present, it should be in the file header or adjacent LICENSE/COPYING file. See `LICENSES.md` and `vendor-README.md` for additional vendor notes.

Project's own code license
-------------------------
- Path: project root files and most `*.c`, `*.h`, `*.cpp` in the top-level — licensed under the MIT License (see `LICENSE`). The MIT license is GPL-compatible; when you combine MIT-licensed code with GPLv3+ code, the combined work is distributed under the GPL terms for GPL-covered parts.

Contact / Corresponding Source
------------------------------
- Preferred: include the `upstream_bash/` source tree in any distribution that contains GPL-covered binaries.
- If you provide a written offer instead of including source directly, use the wording in `COMPLIANCE.md` and include accurate contact information.

Notes
-----
- Do not remove or modify per-file upstream headers inside `upstream_bash/`.
- If you discover additional third-party code not listed here, add an entry for it with license, path, and obligations.

Last updated: 2025-10-31
