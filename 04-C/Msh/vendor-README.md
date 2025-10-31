# Vendor/Upstream notice

This repository includes an upstream copy of the GNU Bash sources under
the directory `upstream_bash/`.

- License: GNU General Public License v3.0 or later (GPLv3+). See
  `upstream_bash/COPYING` and per-file headers for the full license text.
- Purpose: The `upstream_bash/` subtree contains the original Bash source
  code, examples, and tests that are being kept in-tree for reference or
  local testing.

Disclaimer: The GNU Bash sources in `upstream_bash/` are upstream code maintained by the Free Software Foundation and contributors; they are not authored by the Msh project. Unless a specific file includes an explicit "Modifications" notice, assume the files in `upstream_bash/` are verbatim upstream sources.

Notes from automated checks performed on this repository:

- A token-level similarity scan was run comparing repository files to the
  upstream Bash corpus. The scan produced a single finding (see
  `similarity_report.json`) showing token overlap in `TEST/BBBBB.txt` versus
  `upstream_bash/y.tab.c`. Manual inspection of `TEST/BBBBB.txt` indicates
  it is a short user/test text file and not a verbatim upstream source file.

Recommendations for redistribution:

1. Keep `upstream_bash/` and its `COPYING` file intact when distributing
   this project (these files are licensed under GPLv3+).
2. If you distribute binaries that include upstream Bash code, ensure you
   provide the corresponding source or a written offer as required by GPLv3.
3. If you find files outside `upstream_bash/` that derive from upstream
   Bash sources, either restore the upstream headers and comply with the
   GPL, or remove/replace those files.

If you want, I can help with packaging notes or produce a short
`LICENSES.md` that enumerates third-party licenses used in this repo.
