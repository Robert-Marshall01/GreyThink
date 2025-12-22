# Translation workflow (PowerApp)

This file documents the minimal workflow for extracting translatable strings and managing `.po` files for PowerApp.

Prerequisites
-------------
- Recommended: GNU gettext utilities (`xgettext`, `msginit`, `msgmerge`, `msgfmt`).
- If they are unavailable, `scripts/generate_pot.py` contains a fallback extractor.

Common commands (Makefile)
--------------------------
- Generate a template POT (strings to translate):

    make extract-pot

  This writes `locale/powerapp.pot` using `xgettext` if present, otherwise a best-effort Python extractor.

- Initialize a new language (example: Spanish `es`):

    make init-po lang=es

  This uses `msginit` if available, otherwise it copies the POT to `locale/es/LC_MESSAGES/powerapp.po` for a manual start.

- Update existing `.po` files after extracting new strings:

    make update-po

  This uses `msgmerge` to merge changes from the POT into each existing PO.

- Compile `.po` files to binary `.mo` files that gettext uses at runtime:

    make compile-mo

  This uses `msgfmt` when available. If `msgfmt` is not installed, the Makefile will report it and skip compilation.

Where to place translations
---------------------------
- PO files should live at:

    locale/<lang>/LC_MESSAGES/powerapp.po

- Compiled MO files go to:

    locale/<lang>/LC_MESSAGES/powerapp.mo

Loading translations at runtime
-------------------------------
- The app calls `powerapp.i18n.setup_gettext()` at startup which binds to the `locale/` directory and installs `_()`.
- Make sure to include the `locale/` directory in packaging (e.g., `package_data` or `MANIFEST.in`).

Contributing translations
------------------------
- After creating or updating `.po` files, translate the `msgid` entries in each `.po` file.
- Once ready, run `make compile-mo` and include the `.mo` in your package or pull request.

Automation & CI
---------------
- You may include a CI job that runs `make extract-pot` and `make update-po` to keep PO files in sync (requires gettext tools). If gettext tools are missing in CI, the fallback generator can run but it is less powerful than `xgettext`.

CI check (recommended)
----------------------
- This repository includes a GitHub Action that regenerates `locale/powerapp.pot` on PRs and fails the check when the generated POT differs from the committed `locale/powerapp.pot`.
- To fix a failing check: run `make extract-pot` locally and add the updated `locale/powerapp.pot` to your branch before pushing.

If you want, I can add a small CI job that validates that `locale/powerapp.pot` includes the latest strings on PRs and that all `.po` files merge cleanly.
