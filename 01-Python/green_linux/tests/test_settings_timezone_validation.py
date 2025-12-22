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


def test_invalid_timezone_blocks_save(monkeypatch):
    win = PowerWindow(None)

    saved = {}

    def fake_save_settings(cfg):
        saved['called'] = True

    import powerapp.config as cfgmod
    monkeypatch.setattr(cfgmod, 'save_settings', fake_save_settings)

    captured = {}
    def presenter_stub(self, dlg, on_response=None):
        # capture the dialog so the test can inspect it before destruction
        captured['dlg'] = dlg
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
        # find the manual entry and set an invalid timezone
        if content is not None:
            for c in _iter_children(content):
                for g in _iter_children(c):
                    try:
                        if getattr(g, 'set_text', None):
                            g.set_text('Not/A_Zone')
                    except Exception:
                        pass
        # Simulate pressing Save which triggers validation via on_response
        if on_response:
            on_response(dlg, Gtk.ResponseType.OK)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_stub)

    win._show_settings_dialog()

    assert saved.get('called', False) is False, 'Settings should not be saved when timezone invalid'

    # Inspect the captured dialog's inline error label
    dlg = captured.get('dlg')
    assert dlg is not None, 'Dialog was not captured'
    content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
    assert content is not None
    err_lbl = getattr(content, '_tz_error_label', None)
    assert err_lbl is not None
    text = ''
    try:
        text = err_lbl.get_label() if getattr(err_lbl, 'get_label', None) else ''
    except Exception:
        text = ''
    assert 'Invalid timezone' in text, f'Expected validation message, got: {text}'