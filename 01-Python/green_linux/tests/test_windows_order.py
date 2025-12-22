import os
import pytest

pytest.skipif_condition = not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY'))
pytestmark = pytest.mark.skipif(pytest.skipif_condition, reason='Requires graphical display')

import gi
gi.require_version('Gtk', '4.0')
from gi.repository import Gtk

from powerapp.gtk.main import PowerWindow
from datetime import datetime


def test_windows_dialog_orders_by_start_time(monkeypatch):
    win = PowerWindow(None)
    captured = {}

    def presenter_capture(self, dlg, on_response=None):
        captured['dlg'] = dlg
        # do not call on_response so the test can inspect the dialog

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_capture)

    # Provide windows out of chronological order
    windows = [
        {'start': '2025-12-21T16:00:00Z', 'avg_intensity': 190.0, 'duration_h': 2},
        {'start': '2025-12-20T18:00:00Z', 'avg_intensity': 185.0, 'duration_h': 2},
        {'start': '2025-12-20T17:00:00Z', 'avg_intensity': 187.0, 'duration_h': 2},
    ]

    # Make UI labels show the raw ISO start string so we can parse them back reliably
    monkeypatch.setattr(PowerWindow, '_format_ts_for_display', lambda self, ts: ts)

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

    # parse ISO strings into datetimes and ensure order is ascending
    dts = []
    for t in texts:
        s = t
        if s.endswith('Z'):
            s = s.replace('Z', '+00:00')
        dts.append(datetime.fromisoformat(s))

    assert dts == sorted(dts), f'Expected rows ordered by start time ascending, got: {dts}'
