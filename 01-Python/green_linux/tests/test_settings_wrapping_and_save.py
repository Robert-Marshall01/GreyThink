import os
import pytest

pytest.skipif_condition = not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY'))
pytestmark = pytest.mark.skipif(pytest.skipif_condition, reason='Requires graphical display')

import gi
gi.require_version('Gtk', '4.0')
from gi.repository import Gtk

from powerapp.gtk.main import PowerWindow


def _iter_children(root):
    # Helper that works with GTK4 container APIs (get_children or get_first_child chain)
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


def test_main_note_label_is_wrapped():
    win = PowerWindow(None)
    # If label wrapping not supported, skip rather than fail
    if not getattr(win.note_label, 'get_wrap', None):
        pytest.skip('Label wrap not supported in this GTK binding')
    assert win.note_label.get_wrap() is True


def test_settings_save_calls_save_settings(monkeypatch):
    win = PowerWindow(None)

    saved = {}

    def fake_save_settings(cfg):
        saved['called'] = True
        saved['last'] = dict(cfg)

    import powerapp.config as cfgmod
    monkeypatch.setattr(cfgmod, 'save_settings', fake_save_settings)

    # Replace presenter to capture the dialog and immediately simulate an OK response
    def presenter_stub(self, dlg, on_response=None):
        # try to set a few widget values so the response handler saves deterministic data
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
        if content is not None:
            for c in _iter_children(content):
                # set first Entry found to a known token
                try:
                    if getattr(c, 'get_text', None):
                        # avoid clobbering the timezone manual entry (name: timezone_entry)
                        try:
                            if getattr(c, 'get_name', None) and c.get_name() == 'timezone_entry':
                                pass
                            else:
                                c.set_text('tok-test')
                        except Exception:
                            try:
                                c.set_text('tok-test')
                            except Exception:
                                pass
                except Exception:
                    pass
                # set SpinButtons if present
                try:
                    if getattr(c, 'get_value', None) and isinstance(c.get_value(), (int, float)):
                        # set small integer values
                        try:
                            c.set_value(3)
                        except Exception:
                            try:
                                c.set_text('3')
                            except Exception:
                                pass
                except Exception:
                    pass
                # also descend into children
                for g in _iter_children(c):
                    try:
                        if getattr(g, 'get_text', None):
                            try:
                                if getattr(g, 'get_name', None) and g.get_name() == 'timezone_entry':
                                    pass
                                else:
                                    g.set_text('tok-test')
                            except Exception:
                                try:
                                    g.set_text('tok-test')
                                except Exception:
                                    pass
                    except Exception:
                        pass
                    try:
                        if getattr(g, 'set_value', None):
                            g.set_value(4)
                    except Exception:
                        pass
        # Call the response handler to trigger saving
        if on_response:
            on_response(dlg, Gtk.ResponseType.OK)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_stub)

    # Call and ensure save_settings stubbed is invoked
    win._show_settings_dialog()

    assert saved.get('called', False) is True, 'save_settings was not called'
    assert 'forecast_hours' in saved.get('last', {}), 'saved settings missing expected keys'
    assert 'window_hours' in saved.get('last', {}), 'saved settings missing expected keys'
    # token may be present as None if entry wasn't found, but ensure structure


def test_settings_privacy_label_wrapped(monkeypatch):
    win = PowerWindow(None)

    captured = {}

    def presenter_capture(self, dlg, on_response=None):
        captured['dlg'] = dlg
        # do not call on_response so test can inspect content

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_capture)

    win._show_settings_dialog()

    dlg = captured.get('dlg')
    assert dlg is not None, 'Settings dialog was not captured'
    content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
    assert content is not None, 'Settings dialog has no content area'
    # find privacy label by searching text
    privacy_lbl = None
    for c in _iter_children(content):
        for g in _iter_children(c):
            try:
                if getattr(g, 'get_label', None) and 'Privacy' in (g.get_label() or ''):
                    privacy_lbl = g
                    break
            except Exception:
                pass
        if privacy_lbl:
            break
    assert privacy_lbl is not None, 'Could not find Privacy label in Settings'
    if not getattr(privacy_lbl, 'get_wrap', None):
        pytest.skip('Label wrap not supported in this GTK binding')
    assert privacy_lbl.get_wrap() is True
