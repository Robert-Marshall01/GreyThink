import types

from gi.repository import GLib

from powerapp.gtk.main import PowerWindow
import powerapp.calendar as calendar
import powerapp.emissions as emissions


def test_start_suggest_worker_applies_calendar_filter_and_preserves_setting(monkeypatch):
    """_start_suggest_worker should restore the original calendar opt-in when requested
    and perform calendar filtering (calling get_busy_intervals) so a filter note/detail
    gets passed to the suggestions dialog."""

    # Run worker synchronously
    class SyncThread:
        def __init__(self, target=None, daemon=None):
            self._target = target
            self.daemon = daemon

        def start(self):
            if callable(self._target):
                self._target()

    monkeypatch.setattr('threading.Thread', SyncThread)

    # Make GLib.idle_add call functions immediately
    monkeypatch.setattr(GLib, 'idle_add', lambda fn, *a, **k: (fn(*a, **k), None)[1])

    # Deterministic emissions/calendar behavior
    monkeypatch.setattr(emissions, 'fetch_current_intensity', lambda zone=None: {'intensity': 500.0, 'source': 'mock'})
    monkeypatch.setattr(emissions, 'fetch_forecast', lambda zone=None, hours=48, cache_ttl=None: [{'timestamp': '2025-12-17T00:00:00Z', 'intensity': 200.0}])
    from datetime import datetime
    windows = [{'start': datetime.fromisoformat('2025-12-17T00:00:00+00:00'), 'end': datetime.fromisoformat('2025-12-17T02:00:00+00:00'), 'avg_intensity': 190.0}]
    monkeypatch.setattr(emissions, 'find_low_carbon_windows', lambda forecast, window_hours=2, top_k=10, threshold=None: windows)
    monkeypatch.setattr(emissions, 'suggest_postponements', lambda tasks, intensity, threshold=300.0: [{'task_name': 't', 'duration_min': 60}])

    called = {}

    # Instrument get_busy_intervals so we can detect it being called and return a busy interval
    def fake_get_busy_intervals(start, end, source=None, ics_path=None):
        called['called_busy'] = True
        return [{'start': start, 'end': end, 'summary': 'Busy slot'}]

    monkeypatch.setattr(calendar, 'get_busy_intervals', fake_get_busy_intervals)

    # augment_suggestions_with_best_window attaches best window data and returns suggestions
    def fake_augment(suggestions_list, forecast_arg, window_hours=2, windows=None, **kwargs):
        called['augment_windows'] = windows
        for s in suggestions_list:
            if windows:
                s['best_window_start'] = windows[0]['start']
        return suggestions_list

    monkeypatch.setattr(emissions, 'augment_suggestions_with_best_window', fake_augment)

    # Prepare a minimal object to act as the window; start with respect_calendar False
    dummy = types.SimpleNamespace()
    dummy.settings = {'respect_calendar': False, 'calendar_source': 'eds', 'window_hours': 2, 'top_k': 10, 'forecast_hours': 48}

    def show_sugg(aug, error=None, forecast=None, note=None, detail=None):
        called['note'] = note
        called['detail'] = detail

    dummy._show_suggestions_dialog = show_sugg

    # Invoke the worker helper directly: simulate that the pre-check detected EDS unavailable
    PowerWindow._start_suggest_worker(dummy, tasks=[{'task_name': 't'}], skip_calendar_for_worker=True, orig_cal_cfg=True)

    # After run: ensure the user's original preference was restored and calendar filtering ran
    assert dummy.settings['respect_calendar'] is True
    assert called.get('called_busy') is True
    assert called.get('note') is not None
    assert 'Busy slot' in (called.get('detail') or '')
    # And augment received the filtered windows list
    assert isinstance(called.get('augment_windows'), list)
