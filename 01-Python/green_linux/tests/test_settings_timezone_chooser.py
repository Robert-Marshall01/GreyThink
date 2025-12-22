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


def test_timezone_combo_selection_saved(monkeypatch):
    win = PowerWindow(None)

    saved = {}

    def fake_save_settings(cfg):
        saved['called'] = True
        saved['last'] = dict(cfg)

    import powerapp.config as cfgmod
    monkeypatch.setattr(cfgmod, 'save_settings', fake_save_settings)

    def presenter_stub(self, dlg, on_response=None):
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
        if content is not None:
            # prefer to set the combo selection if available, but fall back to writing the entry text
            for c in _iter_children(content):
                try:
                    for g in _iter_children(c):
                        try:
                            # if ComboBoxText present, try selecting an item
                            if getattr(g, 'get_active_text', None) and getattr(g, 'set_active', None):
                                try:
                                    for i, txt in enumerate(['UTC', 'Europe/London', 'Europe/Berlin', 'America/Los_Angeles', 'America/New_York']):
                                        if txt == 'America/Los_Angeles':
                                            try:
                                                g.set_active(i)
                                            except Exception:
                                                pass
                                            break
                                except Exception:
                                    pass
                            # if Entry present, set text directly as a robust fallback
                            if getattr(g, 'set_text', None):
                                try:
                                    g.set_text('America/Los_Angeles')
                                except Exception:
                                    pass
                        except Exception:
                            pass
                except Exception:
                    pass
        if on_response:
            on_response(dlg, Gtk.ResponseType.OK)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_stub)

    win._show_settings_dialog()

    assert saved.get('called', False) is True, 'save_settings not called'
    assert saved.get('last', {}).get('timezone') == 'America/Los_Angeles'