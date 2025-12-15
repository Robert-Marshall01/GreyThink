# Simulated packaging layout

This file lists the recommended files that should be included when creating a
redistributable artifact (wheel, tarball, Docker image) that includes this
project and the `google/flan-t5-base` model weights.

Top-level files (recommended):

- README.md
- LICENSE
- SPDX
- NOTICE
- CITATION.md
- CITATION.bib
- python_ai_2.py
- requirements.txt
- requirements-dev.txt (if provided)
- THIRD_PARTY_LICENSES_GENERATED.md
- THIRD_PARTY_LICENSES.md
- PACKAGING_CHECKLIST.md

Licenses directory (required when bundling weights):

- licenses/COPYING.APACHE2.txt
- licenses/google-flan-t5-base.NOTICE.txt

Model-related documentation (do NOT include weights unless you will comply):

- models/README.md
- models/COPYING.APACHE2.txt (optional duplicate)
- models/google-flan-t5-base.NOTICE.txt (optional duplicate)

Notes:

- This layout ensures that any consumer of a redistributed artifact can find
  the full Apache 2.0 license text and the model NOTICE without unpacking the
  whole artifact.
- If you choose NOT to bundle weights (recommended), include `models/README.md`
  and `CITATION.md` and ensure `licenses/` is present for clarity.
