# Licenses for packaging

This directory contains copies of license/NOTICE files to be included in
redistributable artifacts (wheels, tarballs, Docker images) so downstream
users can easily find license texts without digging into source directories.

Files:

- `COPYING.APACHE2.txt` — Apache 2.0 license text for `google/flan-t5-base` model
- `google-flan-t5-base.NOTICE.txt` — model NOTICE

When building a binary artifact, include the entire `licenses/` directory
at the top level of the artifact (for example, `/licenses/COPYING.APACHE2.txt`).
