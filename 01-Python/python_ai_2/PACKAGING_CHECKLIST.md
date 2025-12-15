# Packaging checklist for distributions

When building any redistributable artifact that includes this project
(wheels, tarballs, Docker images, or other packaged binaries), follow
this checklist to maintain compliance with third-party licenses (including
the Apache 2.0 license for `google/flan-t5-base`):

1. Do not bundle model weights unless you intend to comply with redistribution
   obligations. Prefer to fetch weights at runtime using `transformers.from_pretrained`.

2. If you bundle the model weights, include the following files in the
   distributed artifact in a clearly visible location (root or `licenses/`):
   - `models/COPYING.APACHE2.txt`
   - `models/google-flan-t5-base.NOTICE.txt`

3. Include a top-level `NOTICE` file in the artifact. This repository's
   `NOTICE` at the repo root already documents the model and points to the
   license/NOTICE files.

4. Include full license texts for any third-party packages you are bundling.
   Use `THIRD_PARTY_LICENSES_GENERATED.md` (generated via `pip-licenses`) or
   include `LICENSE` files from each dependency as appropriate.

5. Ensure source files retain their original copyright and attribution
   notices. Do not remove or modify upstream copyright lines without
   marking modifications and adding a notice that you changed the file.

6. Add SPDX short identifiers in code files (e.g., `SPDX-License-Identifier: MIT`) —
   this repo includes an `SPDX` file and `python_ai_2.py` has a header.

7. For Docker images: add an `/licenses` directory in the image that contains
   all license texts and `NOTICE` files, and reference them in the image
   documentation (README or Docker labels).

8. For pip packages: include license text and `NOTICE` in the `dist/` artifact
   (e.g., include them in `package_data`), and document license info in
   `METADATA`/`PKG-INFO` if applicable.

9. Document in the `README` that model weights are not included and provide
   runtime instructions to download them via `transformers.from_pretrained(...)`.

10. If you modify any model files from the upstream distribution, add a
    prominent notice describing the changes and the date of modification.

Following this checklist will reduce the risk of accidental non‑compliance
when creating redistributable artifacts that include the model or other
third-party code.
