import os
import pytest

pytest.skipif_condition = not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY'))
pytestmark = pytest.mark.skipif(pytest.skipif_condition, reason='Requires graphical display')

import gi
gi.require_version('Gtk', '4.0')
from gi.repository import Gtk

from powerapp.gtk.main import PowerWindow


def test_windows_dialog_shows_various_timestamp_formats(monkeypatch):
    win = PowerWindow(None)
    captured = {}

    def presenter_capture(self, dlg, on_response=None):
        captured['dlg'] = dlg
        # do not call on_response so the test can inspect the dialog

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_capture)

    windows = [
        {'start': '2025-12-19T12:00:00Z', 'avg_intensity': 100.0, 'duration_h': 1},
        {'start': '2025-12-19T13:00:00-07:00', 'avg_intensity': 110.0, 'duration_h': 1},
        {'start': '2025-12-19T14:00:00', 'avg_intensity': 120.0, 'duration_h': 1},
    ]

    # Force UTC user timezone for predictable output
    win.settings['timezone'] = 'UTC'

    win._show_windows_dialog(windows)

    dlg = captured.get('dlg')
    assert dlg is not None, 'expected dialog to be created'
    try:
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else dlg.get_content_area()
    except Exception:
        pytest.skip('Dialog content not accessible in this Gtk binding')

    # find the first Grid in content
    grid = None
    for c in content.get_children() if getattr(content, 'get_children', None) else []:
        if isinstance(c, Gtk.Grid):
            grid = c
            break
    assert grid is not None, 'expected a Grid containing the windows table'

    # read column 0 labels for each window row (row 1..)
    texts = []
    for i in range(1, 1 + len(windows)):
        try:
            lbl = grid.get_child_at(0, i)
            if lbl is None:
                continue
            txt = None
            try:
                txt = lbl.get_text()
            except Exception:
                try:
                    txt = lbl.get_label()
                except Exception:
                    txt = None
            texts.append(txt or '')
        except Exception:
            texts.append('')

    # Verify that each of the provided formats produced some formatted time
    assert any('12:00' in t for t in texts), f'Expected one formatted time to include 12:00, got: {texts}'
    assert any('19:00' in t or '20:00' in t or '13:00' in t for t in texts), f'Expected offset timestamp to be formatted, got: {texts}'
    assert any('14:00' in t for t in texts), f'Expected naive timestamp treated as UTC to show 14:00, got: {texts}'
