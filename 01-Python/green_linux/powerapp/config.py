"""Configuration helper for PowerApp.

Provides simple JSON-backed settings stored in XDG config directory.
"""
from pathlib import Path
import json
import os

APP_DIR_NAME = 'powerapp'
CONFIG_FILENAME = 'config.json'

DEFAULTS = {
    'provider': 'mock',  # 'mock' or 'electricitymap'
    'token': None,
    'zone': '',
    'timezone': '',
    'window_hours': 2,
    'top_k': 5,
    'forecast_hours': 48,
    'forecast_cache_ttl': 900,  # seconds
    'forecast_refresh_cooldown': 5,
    'threshold': 300.0,
    # simulator visuals
    'sim_gridlines': 4,          # number of horizontal gridlines shown in simulator
    'sim_grid_opacity': 0.06,
    'respect_calendar': False,
    'calendar_source': 'eds',  # 'eds' or 'ics'
    'calendar_ics_path': None,  # optional path to .ics file or directory
    'allow_quick_actions': False,
    'undo_duration': 6,
    'enable_ml_best_window': False,
    'ml_model_path': None,
    'telemetry_opt_in': False,
    'collect_calibration_samples': False,
    'save_diagnostics': False,
    'enable_bugreport_upload': False,
    'bugreport_upload_url': None,
    # Accessibility / UI
    'palette': 'default',  # 'default', 'high_contrast', or 'colorblind'
    # Power profile options
    'power_profile_target': 'power-saver',
    'auto_set_power_profile': False,
    # Power sampling
    'use_mock_power': False,  # True to use mock power values for testing
    'tasks': []  # list of user-defined deferrable tasks
}


def _config_path() -> Path:
    xdg = os.environ.get('XDG_CONFIG_HOME')
    if not xdg:
        xdg = str(Path.home() / '.config')
    d = Path(xdg) / APP_DIR_NAME
    d.mkdir(parents=True, exist_ok=True)
    return d / CONFIG_FILENAME


def load_settings() -> dict:
    path = _config_path()
    try:
        if path.exists():
            with path.open('r', encoding='utf-8') as fh:
                data = json.load(fh)
                # merge with defaults
                cfg = DEFAULTS.copy()
                cfg.update({k: v for k, v in data.items() if v is not None})
                return cfg
    except Exception:
        # ignore parse errors and return defaults
        pass
    return DEFAULTS.copy()


# Keep latest seen settings in-memory so multiple partial saves during a
# single run/event loop do not accidentally overwrite recent in-memory
# updates when the on-disk state is stale (e.g., in tests where
# save_settings is monkeypatched and doesn't write to disk).
_last_seen: dict = {}
_last_config_path: str = ""


def _compose_to_save(incoming: dict, persisted: dict) -> dict:
    """Compose the final settings dictionary to persist.

    The preference order is:
      - explicit values provided in `incoming` (if present and non-empty)
      - most-recent in-memory values from `_last_seen`
      - existing persisted values from `persisted`
      - DEFAULTS
    """
    def _is_specified(v):
        # Treat None and empty strings as "unspecified" so they don't
        # overwrite previously-seen valid values during partial saves.
        return (v is not None) and not (isinstance(v, str) and v == "")

    out = {}
    for k in DEFAULTS:
        if (incoming is not None) and (k in incoming) and _is_specified(incoming.get(k)):
            out[k] = incoming.get(k)
        elif k in _last_seen:
            out[k] = _last_seen.get(k)
        else:
            out[k] = persisted.get(k, DEFAULTS[k])
    return out


def save_settings(settings: dict) -> None:
    """Persist settings while preserving unspecified keys.

    If `settings` is a partial dict, merge it with the currently persisted
    settings (or defaults) so unspecified keys are not reset to defaults.

    This function also maintains a short-lived in-memory cache of the most
    recently-seen settings (`_last_seen`) so that subsequent partial saves
    in the same process prefer in-memory updates over possibly-stale on-disk
    data. This makes behavior deterministic in test runs that monkeypatch
    persistence.
    """
    global _last_seen, _last_config_path
    path = _config_path()
    
    # Clear _last_seen cache if config path changed (e.g., new test isolation)
    current_path = str(path)
    if current_path != _last_config_path:
        _last_seen = {}
        _last_config_path = current_path

    # Update in-memory view of the most recently-seen values. Ignore explicit
    # None or empty-string values from callers to preserve existing keys.
    if isinstance(settings, dict):
        for k, v in settings.items():
            if v is None:
                continue
            if isinstance(v, str) and v == "":
                # Treat empty strings as unspecified
                continue
            _last_seen[k] = v

    # base on existing persisted settings so callers can pass partial dicts
    existing = load_settings()

    to_save = _compose_to_save(settings, existing)

    # Make a snapshot of the dict we are about to persist so later logs can
    # unambiguously identify the exact object that was written.
    snapshot = dict(to_save)

    # Log snapshot and persist to disk
    _log_save_snapshot(snapshot)
    with path.open('w', encoding='utf-8') as fh:
        json.dump(snapshot, fh, indent=2)


# More verbose debug that prints the snapshot id and key overview so we can
# correlate persisted files with call-site ids printed by the UI handlers.
def _log_save_snapshot(snap: dict) -> None:
    try:
        import traceback
        print('DEBUG_SAVE_SNAPSHOT id', id(snap))
        print('DEBUG_SAVE_SNAPSHOT sim_gridlines', snap.get('sim_gridlines'))
        # Print a few targeted keys to more clearly show overwritten fields
        try:
            print('DEBUG_SAVE_SNAPSHOT timezone', snap.get('timezone'))
        except Exception:
            pass
        try:
            print('DEBUG_SAVE_SNAPSHOT ml_model_path', snap.get('ml_model_path'))
        except Exception:
            pass
        try:
            print('DEBUG_SAVE_SNAPSHOT bugreport_url', snap.get('bugreport_upload_url'))
        except Exception:
            pass
        print('DEBUG_SAVE_SNAPSHOT keys', list(snap.keys()))
        # show a short stack to see *who* invoked save_settings (3-frame context)
        for ln in traceback.format_stack(limit=5)[-5:-2]:
            for line in ln.rstrip().splitlines():
                print('DEBUG_SAVE_STACK:', line)
    except (ImportError, OSError, UnicodeEncodeError) as exc:
        # best-effort logging; tolerate I/O/encoding issues
        print('DEBUG_SAVE_SNAPSHOT logging failed:', type(exc), exc)
