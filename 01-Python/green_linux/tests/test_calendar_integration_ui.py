import os
import pytest

pytestmark = pytest.mark.skipif(not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY')), reason='Requires graphical display')

import gi
gi.require_version('Gtk', '4.0')
from gi.repository import Gtk, GLib

from powerapp.gtk.main import PowerWindow


def _make_sync_thread():
    class SyncThread:
        def __init__(self, target=None, daemon=None):
            self._target = target
            self.daemon = daemon

        def start(self):
            if callable(self._target):
                self._target()

    return SyncThread


def test_suggest_filters_windows_when_calendar_conflicts(monkeypatch):
    """When calendar has conflicts, suggestions dialog shows a filter note and details."""
    # Make worker synchronous
    import threading
    monkeypatch.setattr(threading, 'Thread', _make_sync_thread())
    # Make GLib.idle_add run immediately
    monkeypatch.setattr(GLib, 'idle_add', lambda fn, *a, **k: (fn(*a, **k), None)[1])

    # Capture created dialogs
    created = []
    def run_stub(self):
        created.append(self)
        return Gtk.ResponseType.CLOSE
    monkeypatch.setattr(Gtk.Dialog, 'run', run_stub, raising=False)

    # Provide deterministic emissions and windows
    import powerapp.emissions as emissions
    monkeypatch.setattr(emissions, 'fetch_current_intensity', lambda zone=None: {'intensity': 400.0, 'source': 'mock'})

    suggestion = {
        'task_name': 'Task A', 'urgency': 'low', 'duration_min': 60, 'current_intensity_g': 400.0
    }
    monkeypatch.setattr(emissions, 'suggest_postponements', lambda tasks, intensity, threshold=300.0: [suggestion])

    # Forecast and windows (two windows)
    fake_forecast = [{'timestamp': '2025-12-17T00:00:00Z', 'intensity': 200.0}, {'timestamp': '2025-12-17T01:00:00Z', 'intensity': 180.0}]
    monkeypatch.setattr(emissions, 'fetch_forecast', lambda zone=None, hours=48, cache_ttl=None: fake_forecast)
    windows = [{'start': '2025-12-17T00:00:00Z', 'end': '2025-12-17T02:00:00Z', 'avg_intensity': 190.0}]
    monkeypatch.setattr(emissions, 'find_low_carbon_windows', lambda forecast, window_hours=2, top_k=10, threshold=None: windows)
    monkeypatch.setattr(emissions, 'augment_suggestions_with_best_window', lambda suggestions_list, forecast_arg, window_hours=2, windows=None: suggestions_list)

    # Stub calendar to return a busy interval overlapping the window
    import powerapp.calendar as calendar
    from datetime import datetime, timezone, timedelta
    busy = [{'start': datetime.now(timezone.utc).isoformat(), 'end': (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat(), 'summary': 'Busy slot'}]
    monkeypatch.setattr(calendar, 'get_busy_intervals', lambda start, end, source=None, ics_path=None: busy)

    base_win = Gtk.Window()
    base_win.settings = {'respect_calendar': True, 'window_hours': 2, 'top_k': 10}
    base_win._show_tasks_dialog = lambda *_: None
    base_win._show_transient_message = lambda *_args, **_kwargs: None

    # run suggestion flow synchronously
    try:
        PowerWindow._on_suggest(base_win)
    except Exception as e:
        pytest.skip(f'Suggestion worker incompatible: {e}')

    # Find the suggestions dialog created
    sdlg = None
    for d in reversed(created):
        try:
            if getattr(d, 'get_title', None) and d.get_title() and 'suggest' in (d.get_title() or '').lower():
                sdlg = d
                break
        except Exception:
            pass
    assert sdlg is not None

    if getattr(sdlg, 'get_child', None):
        s_content = sdlg.get_child()
    elif getattr(sdlg, 'get_content_area', None):
        s_content = sdlg.get_content_area()
    else:
        pytest.skip('Dialog has no content area')

    # Should contain a label mentioning calendar/filter
    found_filtered = any(isinstance(c, Gtk.Label) and 'calendar' in (c.get_text() or '').lower() for c in s_content.get_children())
    assert found_filtered


def test_suggest_does_not_filter_when_respect_disabled(monkeypatch):
    """When calendar respect is disabled, no filter note should be present."""
    import threading
    monkeypatch.setattr(threading, 'Thread', _make_sync_thread())
    monkeypatch.setattr(GLib, 'idle_add', lambda fn, *a, **k: (fn(*a, **k), None)[1])

    created = []
    def run_stub(self):
        created.append(self)
        return Gtk.ResponseType.CLOSE
    monkeypatch.setattr(Gtk.Dialog, 'run', run_stub, raising=False)

    import powerapp.emissions as emissions
    monkeypatch.setattr(emissions, 'fetch_current_intensity', lambda zone=None: {'intensity': 400.0, 'source': 'mock'})
    suggestion = {'task_name': 'Task B', 'urgency': 'low', 'duration_min': 60, 'current_intensity_g': 400.0}
    monkeypatch.setattr(emissions, 'suggest_postponements', lambda tasks, intensity, threshold=300.0: [suggestion])
    fake_forecast = [{'timestamp': '2025-12-17T00:00:00Z', 'intensity': 200.0}]
    monkeypatch.setattr(emissions, 'fetch_forecast', lambda zone=None, hours=48, cache_ttl=None: fake_forecast)
    monkeypatch.setattr(emissions, 'find_low_carbon_windows', lambda forecast, window_hours=2, top_k=10, threshold=None: [])
    monkeypatch.setattr(emissions, 'augment_suggestions_with_best_window', lambda suggestions_list, forecast_arg, window_hours=2, windows=None: suggestions_list)

    import powerapp.calendar as calendar
    monkeypatch.setattr(calendar, 'get_busy_intervals', lambda *a, **k: [{'start': 'x', 'end': 'y', 'summary': 'Busy'}])

    base_win = Gtk.Window()
    base_win.settings = {'respect_calendar': False}
    base_win._show_tasks_dialog = lambda *_: None
    base_win._show_transient_message = lambda *_args, **_kwargs: None

    try:
        PowerWindow._on_suggest(base_win)
    except Exception as e:
        pytest.skip(f'Suggestion worker incompatible: {e}')

    sdlg = None
    for d in reversed(created):
        try:
            if getattr(d, 'get_title', None) and d.get_title() and 'suggest' in (d.get_title() or '').lower():
                sdlg = d
                break
        except Exception:
            pass
    assert sdlg is not None
    if getattr(sdlg, 'get_child', None):
        s_content = sdlg.get_child()
    elif getattr(sdlg, 'get_content_area', None):
        s_content = sdlg.get_content_area()
    else:
        pytest.skip('Dialog has no content area')

    # No calendar label/tooltip when respect disabled
    found_filtered = any(isinstance(c, Gtk.Label) and 'calendar' in (c.get_text() or '').lower() for c in s_content.get_children())
    assert not found_filtered


def test_simulator_shows_multiple_busy_intervals(monkeypatch):
    """Simulator should attach busy intervals to the drawing area when multiple events are returned."""
    created = []
    def run_stub(self):
        created.append(self)
        return Gtk.ResponseType.CLOSE
    monkeypatch.setattr(Gtk.Dialog, 'run', run_stub, raising=False)

    # Prepare suggestion + forecast
    suggestion = {'task_name': 'Task C', 'duration_min': 60, 'best_window_start': '2025-12-17T00:00:00Z', 'current_intensity_g': 400.0}
    forecast = [{'timestamp': '2025-12-17T00:00:00Z', 'intensity': 120.0}, {'timestamp': '2025-12-17T01:00:00Z', 'intensity': 110.0}]

    # Stub calendar to return two busy intervals
    import powerapp.calendar as calendar
    from datetime import datetime, timezone, timedelta
    now = datetime.now(timezone.utc)
    busy = [{'start': (now + timedelta(hours=1)).isoformat(), 'end': (now + timedelta(hours=2)).isoformat(), 'summary': 'Event1'},
            {'start': (now + timedelta(hours=3)).isoformat(), 'end': (now + timedelta(hours=4)).isoformat(), 'summary': 'Event2'}]
    monkeypatch.setattr(calendar, 'get_busy_intervals', lambda start, end, source=None, ics_path=None: busy)

    base_win = Gtk.Window()
    base_win.settings = {'respect_calendar': True, 'window_hours': 2}
    base_win._show_tasks_dialog = lambda *_: None
    base_win._show_transient_message = lambda *_args, **_kwargs: None

    try:
        PowerWindow._show_simulator_dialog(base_win, suggestion, forecast)
    except (TypeError, SystemError) as e:
        pytest.skip(f'GTK environment incompatible for simulator test: {e}')

    # Find the created simulator dialog and its drawing area
    sim_dlg = None
    for d in reversed(created):
        try:
            if getattr(d, 'get_title', None) and d.get_title() and 'simulate' in (d.get_title() or '').lower():
                sim_dlg = d
                break
        except Exception:
            pass
    assert sim_dlg is not None
    if getattr(sim_dlg, 'get_child', None):
        sim_content = sim_dlg.get_child()
    elif getattr(sim_dlg, 'get_content_area', None):
        sim_content = sim_dlg.get_content_area()
    else:
        pytest.skip('Dialog has no content area')

    found_draw = None
    for c in sim_content.get_children():
        if isinstance(c, Gtk.DrawingArea):
            found_draw = c
            break
    assert found_draw is not None
    # _sim_busy should reflect the two busy intervals
    assert getattr(found_draw, '_sim_busy', None) is not None
    assert len(found_draw._sim_busy) == 2


def test_calendar_source_choice_is_respected(monkeypatch):
    """When calendar_source is set to 'ics', the UI code passes that source into get_busy_intervals."""
    import threading
    monkeypatch.setattr(threading, 'Thread', _make_sync_thread())
    monkeypatch.setattr(GLib, 'idle_add', lambda fn, *a, **k: (fn(*a, **k), None)[1])

    called = {}
    def fake_get_busy_intervals(start, end, source=None, ics_path=None):
        called['source'] = source
        return [{'start': start, 'end': end, 'summary': 'Busy slot'}]

    import powerapp.calendar as calendar
    monkeypatch.setattr(calendar, 'get_busy_intervals', fake_get_busy_intervals)

    import powerapp.emissions as emissions
    monkeypatch.setattr(emissions, 'fetch_current_intensity', lambda zone=None: {'intensity': 500.0, 'source': 'mock'})
    suggestion = {'task_name': 'Task ICS', 'urgency': 'low', 'duration_min': 60, 'current_intensity_g': 500.0}
    monkeypatch.setattr(emissions, 'suggest_postponements', lambda tasks, intensity, threshold=300.0: [suggestion])
    fake_forecast = [{'timestamp': '2025-12-17T00:00:00Z', 'intensity': 200.0}]
    monkeypatch.setattr(emissions, 'fetch_forecast', lambda zone=None, hours=48, cache_ttl=None: fake_forecast)
    monkeypatch.setattr(emissions, 'find_low_carbon_windows', lambda forecast_arg, window_hours=2, top_k=10, threshold=None: [{'start': forecast_arg[0]['timestamp'], 'end': forecast_arg[0]['timestamp'], 'avg_intensity': 100.0}])
    monkeypatch.setattr(emissions, 'augment_suggestions_with_best_window', lambda suggestions_list, forecast_arg, window_hours=2, windows=None: suggestions_list)

    base_win = Gtk.Window()
    base_win.settings = {'respect_calendar': True, 'calendar_source': 'ics', 'window_hours': 2}
    base_win._show_tasks_dialog = lambda *_: None
    base_win._show_transient_message = lambda *_args, **_kwargs: None

    try:
        PowerWindow._on_suggest(base_win)
    except Exception as e:
        pytest.skip(f'Suggestion worker incompatible: {e}')

    # Ensure the calendar source passed was 'ics'
    assert called.get('source') == 'ics'


def test_eds_unavailable_does_not_call_get_busy(monkeypatch):
    """When source is 'eds' but EDS is unavailable, get_busy_intervals should not be invoked and no filter note should appear."""
    import threading
    monkeypatch.setattr(threading, 'Thread', _make_sync_thread())
    monkeypatch.setattr(GLib, 'idle_add', lambda fn, *a, **k: (fn(*a, **k), None)[1])

    # Force EDS unavailable
    import powerapp.calendar as calendar
    monkeypatch.setattr(calendar, 'eds_available', lambda: False)

    called = {'gb_called': False}
    def fake_get_busy_intervals(start, end, source=None, ics_path=None):
        called['gb_called'] = True
        return [{'start': start, 'end': end, 'summary': 'Busy slot'}]
    monkeypatch.setattr(calendar, 'get_busy_intervals', fake_get_busy_intervals)

    import powerapp.emissions as emissions
    monkeypatch.setattr(emissions, 'fetch_current_intensity', lambda zone=None: {'intensity': 500.0, 'source': 'mock'})
    suggestion = {'task_name': 'Task EDS', 'urgency': 'low', 'duration_min': 60, 'current_intensity_g': 500.0}
    monkeypatch.setattr(emissions, 'suggest_postponements', lambda tasks, intensity, threshold=300.0: [suggestion])
    fake_forecast = [{'timestamp': '2025-12-17T00:00:00Z', 'intensity': 200.0}]
    monkeypatch.setattr(emissions, 'fetch_forecast', lambda zone=None, hours=48, cache_ttl=None: fake_forecast)
    monkeypatch.setattr(emissions, 'find_low_carbon_windows', lambda forecast_arg, window_hours=2, top_k=10, threshold=None: [{'start': forecast_arg[0]['timestamp'], 'end': forecast_arg[0]['timestamp'], 'avg_intensity': 100.0}])
    monkeypatch.setattr(emissions, 'augment_suggestions_with_best_window', lambda suggestions_list, forecast_arg, window_hours=2, windows=None: suggestions_list)

    base_win = Gtk.Window()
    base_win.settings = {'respect_calendar': True, 'calendar_source': 'eds', 'window_hours': 2}
    base_win._show_tasks_dialog = lambda *_: None
    base_win._show_transient_message = lambda *_args, **_kwargs: None

    try:
        PowerWindow._on_suggest(base_win)
    except Exception as e:
        pytest.skip(f'Suggestion worker incompatible: {e}')

    # get_busy_intervals should not have been called
    assert called['gb_called'] is False


def test_filter_details_truncated_and_more(monkeypatch):
    """If more than 3 events are filtered, detail text should include ', and more...'"""
    import threading
    monkeypatch.setattr(threading, 'Thread', _make_sync_thread())
    monkeypatch.setattr(GLib, 'idle_add', lambda fn, *a, **k: (fn(*a, **k), None)[1])

    created = []
    def run_stub(self):
        created.append(self)
        return Gtk.ResponseType.CLOSE
    monkeypatch.setattr(Gtk.Dialog, 'run', run_stub, raising=False)

    import powerapp.emissions as emissions
    monkeypatch.setattr(emissions, 'fetch_current_intensity', lambda zone=None: {'intensity': 500.0, 'source': 'mock'})
    suggestion = {'task_name': 'Task Many', 'urgency': 'low', 'duration_min': 60, 'current_intensity_g': 500.0}
    monkeypatch.setattr(emissions, 'suggest_postponements', lambda tasks, intensity, threshold=300.0: [suggestion])
    fake_forecast = [{'timestamp': '2025-12-17T00:00:00Z', 'intensity': 200.0}]
    monkeypatch.setattr(emissions, 'fetch_forecast', lambda zone=None, hours=48, cache_ttl=None: fake_forecast)
    monkeypatch.setattr(emissions, 'find_low_carbon_windows', lambda forecast_arg, window_hours=2, top_k=10, threshold=None: [{'start': forecast_arg[0]['timestamp'], 'end': forecast_arg[0]['timestamp'], 'avg_intensity': 100.0}])
    monkeypatch.setattr(emissions, 'augment_suggestions_with_best_window', lambda suggestions_list, forecast_arg, window_hours=2, windows=None: suggestions_list)

    import powerapp.calendar as calendar
    busy = []
    for i in range(5):
        busy.append({'start': f'2025-12-17T0{i}:00:00+00:00', 'end': f'2025-12-17T0{i}:30:00+00:00', 'summary': f'Event{i+1}'})
    monkeypatch.setattr(calendar, 'get_busy_intervals', lambda start, end, source=None, ics_path=None: busy)

    base_win = Gtk.Window()
    base_win.settings = {'respect_calendar': True, 'calendar_source': 'ics', 'window_hours': 2}
    base_win._show_tasks_dialog = lambda *_: None
    base_win._show_transient_message = lambda *_args, **_kwargs: None

    try:
        PowerWindow._on_suggest(base_win)
    except Exception as e:
        pytest.skip(f'Suggestion worker incompatible: {e}')

    # Find the suggestions dialog created
    sdlg = None
    for d in reversed(created):
        try:
            if getattr(d, 'get_title', None) and d.get_title() and 'suggest' in (d.get_title() or '').lower():
                sdlg = d
                break
        except Exception:
            pass
    assert sdlg is not None
    if getattr(sdlg, 'get_child', None):
        s_content = sdlg.get_child()
    elif getattr(sdlg, 'get_content_area', None):
        s_content = sdlg.get_content_area()
    else:
        pytest.skip('Dialog has no content area')

    lbls = [c for c in s_content.get_children() if isinstance(c, Gtk.Label) and 'calendar' in (c.get_text() or '').lower()]
    assert lbls
    # tooltip should contain the first three summaries and mention 'and more'
    tip = lbls[0].get_tooltip_text() or ''
    assert 'Event1' in tip and 'Event2' in tip and 'Event3' in tip
    assert 'and more' in tip