import os
import pytest

pytest.skipif_condition = not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY'))
pytestmark = pytest.mark.skipif(pytest.skipif_condition, reason='Requires graphical display')

import gi
gi.require_version('Gtk', '4.0')
from gi.repository import Gtk


from powerapp.gtk.main import PowerWindow


def _iter_children(root):
    try:
        if getattr(root, 'get_children', None):
            return list(root.get_children())
    except Exception:
        pass
    out = []
    try:
        ch = root.get_first_child() if getattr(root, 'get_first_child', None) else None
        while ch is not None:
            out.append(ch)
            ch = ch.get_next_sibling() if getattr(ch, 'get_next_sibling', None) else None
    except Exception:
        pass
    return out


def test_timezone_saved(monkeypatch):
    win = PowerWindow(None)

    saved = {}

    def fake_save_settings(cfg):
        saved['called'] = True
        saved['last'] = dict(cfg)

    import powerapp.config as cfgmod
    monkeypatch.setattr(cfgmod, 'save_settings', fake_save_settings)

    # Replace presenter to capture the dialog and simulate OK response after setting entries
    def presenter_stub(self, dlg, on_response=None):
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
        if content is not None:
            for c in _iter_children(content):
                try:
                    if getattr(c, 'get_text', None):
                        # set all text entries to a known timezone
                        c.set_text('America/Los_Angeles')
                except Exception:
                    pass
                for g in _iter_children(c):
                    try:
                        if getattr(g, 'get_text', None):
                            g.set_text('America/Los_Angeles')
                    except Exception:
                        pass
        if on_response:
            on_response(dlg, Gtk.ResponseType.OK)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_stub)
    win._show_settings_dialog()

    assert saved.get('called', False) is True, 'save_settings was not called'
    assert saved.get('last', {}).get('timezone') == 'America/Los_Angeles'


def test_format_ts_respects_timezone_setting():
    win = PowerWindow(None)
    # set timezone to Pacific so 12:00 UTC -> 04:00 PST on Jan 1
    win.settings['timezone'] = 'America/Los_Angeles'
    s = '2025-01-01T12:00:00Z'
    out = win._format_ts_for_display(s)
    assert '04:00' in out, f'Expected localized Pacific time in output, got: {out}'


def test_suggestions_until_column_localized(monkeypatch):
    win = PowerWindow(None)
    captured = {}

    def presenter_capture(self, dlg, on_response=None):
        captured['dlg'] = dlg
        # do not call on_response; test will inspect

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_capture)

    win.settings['timezone'] = 'America/Los_Angeles'
    suggestion = {
        'task_name': 'TestTask',
        'duration_min': 60,
        'urgency': 'low',
        'estimated_saving_kgCO2_if_postponed': 0.0,
        'estimated_saving_kgCO2_best_window': 0.0,
        'best_window_start': None,
        'suggested_postpone_until': '2025-01-01T12:00:00Z',
    }

    # Instead of inspecting rendered labels (which may not be exposed by some Gtk bindings),
    # verify the formatting helper is invoked with the expected value
    calls = []
    def fake_format(self, ts):
        calls.append(ts)
        return 'ZZZ'
    monkeypatch.setattr(PowerWindow, '_format_ts_for_display', fake_format)

    win._show_suggestions_dialog([suggestion], error=None, forecast=[])

    assert calls, 'format helper was not called'
    # helper may be called for other cells (e.g., best_window_start=None), ensure we saw the suggestion value passed
    assert any(c == suggestion['suggested_postpone_until'] for c in calls if c is not None), f'Expected format helper called with suggestion ts, calls={calls}'


def test_format_ts_handles_offset_and_z():
    win = PowerWindow(None)
    # set user timezone to UTC for predictable output
    win.settings['timezone'] = 'UTC'

    s_z = '2025-01-01T12:00:00Z'  # UTC
    out_z = win._format_ts_for_display(s_z)
    assert '12:00' in out_z, f'Expected 12:00 in output for Z timestamp, got: {out_z}'

    s_off = '2025-01-01T12:00:00-07:00'  # 12:00 MST -> 19:00 UTC
    out_off = win._format_ts_for_display(s_off)
    assert '19:00' in out_off, f'Expected 19:00 UTC in output for -07:00 timestamp, got: {out_off}'

    s_naive = '2025-01-01T12:00:00'  # treated as UTC by code
    out_naive = win._format_ts_for_display(s_naive)
    assert '12:00' in out_naive, f'Expected 12:00 for naive timestamp treated as UTC, got: {out_naive}'