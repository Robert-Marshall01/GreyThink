Unreleased
----------
- ui: Improve Settings UX and dialog robustness — enable wrapping on long explanatory labels (fixes garbled text), make the Settings "Save" flow more defensive across GTK binding APIs, and add a non-blocking dialog presenter with retry/nudge/backoff to reduce empty/unmapped dialogs.
- ui: Fix Export CSV file chooser re-opening repeatedly after cancel by ensuring the nudge/re-present loop stops once the dialog has received a response.
- tests: Fix flaky / environment-dependent tests (PyGObject venv compatibility, picklability, subprocess PYTHONPATH, simulator window selection).
- telemetry: Read sample rate dynamically and write local upload files synchronously when no endpoint is configured (improves test determinism and local debugging).
- tests: Add uploader unit tests for POST behavior and sampling.

Notes: These changes are focused on test stability and developer experience; runtime telemetry behavior remains opt-in and respects `telemetry_opt_in`. When a `POWERAPP_TELEMETRY_ENDPOINT` is configured, the uploader will attempt to POST; otherwise events are written to a local debug file under `$XDG_CACHE_HOME/powerapp/telemetry_upload.jsonl` (or repo-local fallback).