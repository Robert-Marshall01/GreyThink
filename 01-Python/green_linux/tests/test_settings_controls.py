import pytest
import gi
import os

gi.require_version('Gtk', '4.0')
from gi.repository import Gtk

from powerapp.gtk.main import PowerWindow

pytest.skipif_condition = not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY'))
pytestmark = pytest.mark.skipif(pytest.skipif_condition, reason='Requires graphical display')


def test_palette_live_apply_to_other_windows(monkeypatch):
    win = PowerWindow(None)

    # Fake other toplevel that should receive palette updates
    class FakePaletteBox:
        def __init__(self):
            self.called = False
            self.last_idx = None

        def set_active(self, idx):
            self.called = True
            self.last_idx = idx

    other = Gtk.Window()
    other._palette_box = FakePaletteBox()

    # Ensure Gtk.Window.list_toplevels returns our fake window so _on_settings_response sees it
    monkeypatch.setattr(Gtk.Window, 'list_toplevels', lambda: [other])

    # Capture saved settings
    recorded = {}

    def fake_save_settings(cfg):
        recorded['called'] = True
        recorded['last'] = dict(cfg)

    import powerapp.config as cfgmod
    monkeypatch.setattr(cfgmod, 'save_settings', fake_save_settings)

    # Presenter: set palette combo to 'High contrast' (index 1)
    def presenter_set_palette(self, dlg, on_response=None):
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
        if content is not None:
            for c in list(content.get_children()):
                for g in (c.get_children() if getattr(c, 'get_children', None) else []):
                    try:
                        if getattr(g, 'set_active', None) and getattr(g, 'get_active', None):
                            try:
                                # heuristics: set to second option
                                g.set_active(1)
                            except Exception:
                                try:
                                    g.set_active(1)
                                except Exception:
                                    pass
                    except Exception:
                        pass
        if on_response:
            on_response(dlg, Gtk.ResponseType.OK)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_set_palette)

    win._show_settings_dialog()

    assert recorded.get('called', False) is True
    assert recorded['last'].get('palette') == 'high_contrast'

    # The other window's palette box should have been updated
    assert getattr(other, '_palette_box', None) is not None
    assert other._palette_box.called is True


def test_ml_model_path_detection_from_any_entry(monkeypatch):
    win = PowerWindow(None)

    recorded = {}

    def fake_save_settings(cfg):
        recorded['called'] = True
        recorded['last'] = dict(cfg)

    import powerapp.config as cfgmod
    monkeypatch.setattr(cfgmod, 'save_settings', fake_save_settings)

    # Presenter: set the token entry (or first Entry) to a .joblib path so the heuristic picks it up
    def presenter_set_joblib(self, dlg, on_response=None):
        # If the dialog exposes an explicit ml_entry, use it (deterministic)
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
        found = False
        if content is not None:
            # Prefer the exposed attribute set by the dialog
            try:
                ent = getattr(content, '_ml_entry', None) or getattr(dlg, '_ml_entry', None)
                if ent is not None and getattr(ent, 'set_text', None):
                    ent.set_text('my_model.joblib')
                    found = True
            except Exception:
                pass
            if not found:
                for c in list(content.get_children()):
                    try:
                        # If the child is a Grid, inspect its children for the label
                        if getattr(c, 'get_children', None):
                            for cc in list(c.get_children()):
                                try:
                                    lbl = None
                                    try:
                                        lbl = cc.get_label() if getattr(cc, 'get_label', None) else None
                                    except Exception:
                                        lbl = None
                                    if lbl and 'Model path' in lbl:
                                        # find the nearest Entry in the grid children and set it
                                        for candidate in list(c.get_children()):
                                            try:
                                                if getattr(candidate, 'set_text', None) and getattr(candidate, 'get_text', None):
                                                    candidate.set_text('my_model.joblib')
                                                    found = True
                                                    break
                                            except Exception:
                                                pass
                                        if found:
                                            break
                                except Exception:
                                    pass
                        if found:
                            break
                    except Exception:
                        pass
        # Fallback: set any first Entry found
        if not found and content is not None:
            for c in list(content.get_children()):
                try:
                    if getattr(c, 'set_text', None) and getattr(c, 'get_text', None):
                        try:
                            c.set_text('my_model.joblib')
                            found = True
                            break
                        except Exception:
                            pass
                except Exception:
                    pass
        if on_response:
            on_response(dlg, Gtk.ResponseType.OK)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_set_joblib)

    win._show_settings_dialog()

    assert recorded.get('called', False) is True
    assert recorded['last'].get('ml_model_path') == 'my_model.joblib'