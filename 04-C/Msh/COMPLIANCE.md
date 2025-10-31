COMPLIANCE AND RELEASE CHECKLIST
================================

Purpose
-------
This file explains the minimum actions maintainers must take to comply with GPL obligations for the bundled GNU Bash sources in `upstream_bash/` and to make binary distributions legally compliant.

Key facts
---------
- `upstream_bash/` is a verbatim upstream copy of GNU Bash and is licensed under GPLv3+ (see `upstream_bash/COPYING`).
- Project top-level code is MIT-licensed (see `LICENSE`). MIT is GPL-compatible but does not remove GPL obligations for upstream code.
 - Maintainer contact: RobertCMarshall2007@outlook.com

Clarification: The code in `upstream_bash/` is upstream GNU Bash and is not authored by the Msh project. If any upstream file has been modified in this repository, that file must contain an explicit "Modifications" notice describing the change and the author.

Before publishing a release (source or binary)
--------------------------------------------
Follow this checklist before tagging/publishing any release that includes pre-built binaries or that bundles `upstream_bash/` code.

1) Preserve upstream license files and per-file headers
   - Ensure `upstream_bash/COPYING` remains included and unmodified in the release.
   - Ensure all files inside `upstream_bash/` retain their original per-file copyright/license headers.

2) Corresponding Source for GPL parts
   - If you include the GPL-covered binaries in a release, provide the Corresponding Source for those binaries. Preferred options:
     A) include `upstream_bash/` (the full source tree) in the release artifact (recommended), or
     B) include a written offer with a durable contact (email/postal) that allows recipients to obtain the Corresponding Source, or
     C) publish a stable downloadable URL that hosts the Corresponding Source and include that URL in the release materials.

    - Sample written-offer wording (use the maintainer contact listed above):

       "A complete machine-readable copy of the Corresponding Source for the GPL-covered parts of this distribution is available from the distributor. The Corresponding Source will be provided on physical media for the cost of distribution, or by download from a stable URL. Contact: RobertCMarshall2007@outlook.com"

   - Note: if you choose option B or C, make sure the contact URL or email is accurate and monitored for at least the time you promise availability.

   - Practical note: this repository now contains a compressed archive of the Corresponding Source for convenience. The archive is named `CORRESPONDING_SOURCE_upstream_bash.zip` and lives at the repository root. You may include that archive directly in any release that bundles pre-built binaries to satisfy the "include Corresponding Source" requirement.

3) Include `SOURCE_OFFER.txt` and `THIRD_PARTY_LICENSES.md`
   - Ensure both files are present in the release root and that `SOURCE_OFFER.txt` contains the correct maintainer contact (email or URL). `THIRD_PARTY_LICENSES.md` should be included for quick auditing.

4) If you modified any upstream file
   - Add a clear "Modifications" note to the top of the file or in a CHANGES/NEWS file indicating what you changed and when.
   - Do not remove or alter upstream copyright lines.

5) Binary distributions hosted online
   - If you publish pre-built binaries (for example, Windows `.exe` files) on a website or release page, you must either include Corresponding Source in the release or provide the written offer text and contact in the release notes and a machine-readable file.

6) Recommended repository practice
   - Keep `upstream_bash/` as a separate vendor directory (already done) and list it in `THIRD_PARTY_LICENSES.md`.
   - Keep `LICENSE` (MIT), `LICENSES.md`, `SOURCE_OFFER.txt`, `THIRD_PARTY_LICENSES.md`, `COMPLIANCE.md`, and `upstream_bash/COPYING` together at the repository root so they are trivially discoverable.
   - Optionally add SPDX short identifiers to your own source files (e.g., `SPDX-License-Identifier: MIT`) to help automated scanners.

7) Audit and verification
   - Before release, grep for instances of removed upstream headers or missing COPYING files. If any upstream header was accidentally removed, restore it before releasing.

8) If you need help
   - I can (if you want me to):
     - run an automated scan to find files missing upstream headers or likely copied from Bash, or
     - add SPDX headers to your own files in bulk (requires confirmation), or
     - craft a source tarball layout that meets GPLv3 §6 options.

Last updated: 2025-10-31
