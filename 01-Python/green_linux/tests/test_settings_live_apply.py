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


def test_timezone_change_applies_to_open_suggestions(monkeypatch):
    win = PowerWindow(None)

    # Show a suggestions dialog containing a single timestamp (UTC)
    suggestion = {
        'task_name': 'TZTest',
        'duration_min': 60,
        'urgency': 'low',
        'estimated_saving_kgCO2_if_postponed': 0.0,
        'estimated_saving_kgCO2_best_window': 0.0,
        'best_window_start': None,
        'suggested_postpone_until': '2025-01-01T12:00:00Z',
    }

    captured = {}

    # Capture the suggestions dialog instance so we can inspect it later
    def presenter_capture(self, dlg, on_response=None):
        captured['dlg'] = dlg
        # do not call on_response; we want the dialog to remain open while we open Settings

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_capture)

    win._show_suggestions_dialog([suggestion], error=None, forecast=[])

    assert 'dlg' in captured, 'Suggestions dialog was not presented'

    # Sanity-check: the dialog should have stored suggestions and grid and labels should carry canonical timestamps
    dlg = captured['dlg']
    assert getattr(dlg, '_suggestions', None) is not None, 'Dialog did not store _suggestions'
    assert getattr(dlg, '_suggestions_grid', None) is not None, 'Dialog did not store _suggestions_grid'
    # Ensure at least one child in the grid has an _iso_ts attribute
    has_iso = False
    grid = dlg._suggestions_grid
    details = []
    try:
        for ch in (grid.get_children() if getattr(grid, 'get_children', None) else []):
            cn = getattr(ch, '__class__', type(ch)).__name__
            lbl = None
            try:
                if getattr(ch, 'get_label', None):
                    lbl = ch.get_label()
                elif getattr(ch, 'get_text', None):
                    lbl = ch.get_text()
            except Exception:
                lbl = None
            details.append((cn, lbl, getattr(ch, '_iso_ts', None)))
            if getattr(ch, '_iso_ts', None) is not None:
                has_iso = True
                break
    except Exception:
        pass
    assert has_iso, f'No grid child had _iso_ts set; children: {details!r}'
    # Now present Settings and set timezone to America/Los_Angeles, which should trigger
    # our refresh logic that updates labels in the open suggestions dialog.
    def settings_presenter(self, dlg, on_response=None):
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
        if content is not None:
            for c in _iter_children(content):
                for g in _iter_children(c):
                    try:
                        if getattr(g, 'set_text', None):
                            try:
                                g.set_text('America/Los_Angeles')
                            except Exception:
                                pass
                    except Exception:
                        pass
        if on_response:
            on_response(dlg, Gtk.ResponseType.OK)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', settings_presenter)

    win._show_settings_dialog()

    # After saving settings, the suggestions dialog should be marked refreshed (dialog-level marker) or the labels updated
    dlg = captured['dlg']

    found = False
    # Prefer dialog-level marker attached during refresh
    try:
        tz_vals = getattr(dlg, '_tz_refreshed_values', None)
        if tz_vals:
            for v in tz_vals:
                if '04:00' in (v.get('suggested_postpone_until') or ''):
                    found = True
                    break
    except Exception:
        pass

    # Fallback: scan children for _tz_refreshed_text
    if not found:
        try:
            cont = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
            if cont is not None:
                def scan(w):
                    nonlocal found
                    try:
                        if getattr(w, 'get_children', None):
                            for ch in list(w.get_children()):
                                scan(ch)
                    except Exception:
                        pass
                    try:
                        tr = getattr(w, '_tz_refreshed_text', None)
                        if tr and '04:00' in tr:
                            found = True
                    except Exception:
                        pass
                scan(cont)
        except Exception:
            pass

    assert found, 'Expected the suggestions dialog timestamps to be updated to Pacific time (04:00) after changing timezone in Settings'
