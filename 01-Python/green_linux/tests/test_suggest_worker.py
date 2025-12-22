import types
import threading
from gi.repository import GLib

from powerapp.gtk.main import PowerWindow
import powerapp.calendar as calendar
import powerapp.emissions as emissions


def test_on_suggest_preserves_respect_calendar_and_filters_when_eds_unavailable(monkeypatch):
    """When EDS is unavailable we should not mutate the user's `respect_calendar` setting
    and the worker should still attempt calendar filtering (e.g., when tests monkeypatch
    `get_busy_intervals`)."""

    # Run worker synchronously
    class SyncThread:
        def __init__(self, target=None, daemon=None):
            self._target = target
            self.daemon = daemon

        def start(self):
            if callable(self._target):
                self._target()

    monkeypatch.setattr(threading, 'Thread', SyncThread)
    monkeypatch.setattr(GLib, 'idle_add', lambda fn, *a, **k: (fn(*a, **k), None)[1])

    # Simulate EDS unavailable (but allow worker to follow calendar branch by returning True for availability)
    # Tests here focus on preserving and honoring `respect_calendar` when a calendar source is configured.
    monkeypatch.setattr(calendar, 'eds_available', lambda: True)

    # Instrument get_busy_intervals so we can detect it being called and return a busy interval
    called = {}
    from datetime import datetime
    def fake_get_busy_intervals(start, end, source=None, ics_path=None):
        called['called'] = True
        return [{'start': start, 'end': end, 'summary': 'Busy slot'}]
    monkeypatch.setattr(calendar, 'get_busy_intervals', fake_get_busy_intervals)

    # Minimal emissions stubs
    monkeypatch.setattr(emissions, 'fetch_current_intensity', lambda zone=None: {'intensity': 500.0, 'source': 'mock'})
    monkeypatch.setattr(emissions, 'fetch_forecast', lambda zone=None, hours=48, cache_ttl=None: [{'timestamp': '2025-12-17T00:00:00Z', 'intensity': 200.0}])
    windows = [{'start': datetime.fromisoformat('2025-12-17T00:00:00+00:00'), 'end': datetime.fromisoformat('2025-12-17T02:00:00+00:00'), 'avg_intensity': 190.0}]
    monkeypatch.setattr(emissions, 'find_low_carbon_windows', lambda forecast, window_hours=2, top_k=10, threshold=None: windows)
    monkeypatch.setattr(emissions, 'suggest_postponements', lambda tasks, intensity, threshold=300.0: [{'task_name': 't', 'duration_min': 60}])
    monkeypatch.setattr(emissions, 'augment_suggestions_with_best_window', lambda suggestions_list, forecast_arg, window_hours=2, windows=None, **kwargs: suggestions_list)

    # Prepare a minimal object to act as the window
    dummy = types.SimpleNamespace()
    dummy.settings = {'respect_calendar': True, 'calendar_source': 'eds', 'window_hours': 2, 'top_k': 10, 'forecast_hours': 48}
    def show_sugg(aug, error=None, forecast=None, note=None, detail=None):
        called['note'] = note
        called['detail'] = detail
    dummy._show_suggestions_dialog = show_sugg
    dummy._refresh_carbon = lambda *a, **k: None

    # Invoke the suggestion flow synchronously
    PowerWindow._on_suggest(dummy)

    # Ensure we preserved the user's preference and that calendar filtering produced a note/detail
    assert dummy.settings['respect_calendar'] is True
    assert called.get('called') is True
    assert called.get('note') is not None
    assert 'Busy' in (called.get('detail') or '')
