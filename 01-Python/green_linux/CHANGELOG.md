Unreleased - 2025-12-16

### Added
- Integration/UI tests for calendar-aware suggestion flows (mocked EDS & ICS responses, busy-window filtering, tooltip truncation).
- Headful CI job: run GTK/PyGObject UI tests under Xvfb (with pip cache and persistent wheelhouse).
- Explainability panel in Simulator: shows top contributing features and provides counterfactual window previews with a **Preview** action; accessible and covered by headful tests.

### Changed
- Polished UI to use modern GTK4-friendly patterns:
  - Replaced deprecated dialog and widget show APIs with `present()` and modern child content handling.
  - Replaced fragile `Gtk.MessageDialog` usage with a `_show_simple_message` helper using modern Gtk.Dialog patterns.
  - Migrated `ComboBoxText` usage to id-based entries and wrapped legacy calls to avoid DeprecationWarnings.
- Improved simulator visuals: busy shading, clearer legend, and improved draw function.

### Fixed
- Silence various GTK deprecation warnings in tests and runtime by migrating to modern APIs and scoping temporary warning suppression where needed.
- Multiple stability and headful-test robustness fixes (safe fallbacks for older GTK bindings, deterministic SyncThread usage in tests).

### Notes
- Next: finalize UX copy, prepare release packaging (Snap/App Center), and add an entry to the project release notes before tagging.
