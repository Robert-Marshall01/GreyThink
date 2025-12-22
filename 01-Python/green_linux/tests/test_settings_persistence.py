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


def test_settings_persists_additional_controls(monkeypatch):
    win = PowerWindow(None)

    recorded = {}

    def fake_save_settings(cfg):
        recorded['called'] = True
        recorded['last'] = dict(cfg)

    import powerapp.config as cfgmod
    monkeypatch.setattr(cfgmod, 'save_settings', fake_save_settings)

    # Presenter stub: set a variety of widget values then invoke OK
    def presenter_stub(self, dlg, on_response=None):
        content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
        if content is not None:
            for c in _iter_children(content):
                # CheckButtons: enable them
                try:
                    if getattr(c, 'set_active', None) and getattr(c, 'get_active', None):
                        try:
                            # heuristics: enable both quick actions and save diagnostics
                            c.set_active(True)
                        except Exception:
                            try:
                                c.set_active(True)
                            except Exception:
                                pass
                except Exception:
                    pass
                # SpinButtons: choose noticeable values
                try:
                    if getattr(c, 'set_value', None) and getattr(c, 'get_value', None):
                        try:
                            val = c.get_value()
                            if isinstance(val, float) and 0.0 < val < 1.0:
                                c.set_value(0.42)
                            else:
                                c.set_value(7)
                        except Exception:
                            try:
                                c.set_value(7)
                            except Exception:
                                pass
                except Exception:
                    pass
                # ComboBoxText: pick the second option (index 1) where possible
                try:
                    if getattr(c, 'set_active', None) and getattr(c, 'get_active', None):
                        try:
                            c.set_active(1)
                        except Exception:
                            try:
                                c.set_active_id('1')
                            except Exception:
                                pass
                except Exception:
                    pass
                # Descend into children
                for g in _iter_children(c):
                    try:
                        if getattr(g, 'set_active', None) and getattr(g, 'get_active', None):
                            try:
                                g.set_active(True)
                            except Exception:
                                pass
                    except Exception:
                        pass
                    try:
                        if getattr(g, 'set_value', None):
                            try:
                                val = g.get_value() if getattr(g, 'get_value', None) else None
                                if isinstance(val, float) and 0.0 < val < 1.0:
                                    g.set_value(0.33)
                                else:
                                    g.set_value(5)
                            except Exception:
                                try:
                                    g.set_value(5)
                                except Exception:
                                    pass
                    except Exception:
                        pass
        # Trigger save
        if on_response:
            on_response(dlg, Gtk.ResponseType.OK)

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_stub)

    # Add stubs to verify immediate UI effects are applied
    called = {'draw': False, 'palette': False}

    class FakeDrawArea:
        def queue_draw(self):
            called['draw'] = True
        def queue_render(self):
            called['draw'] = True

    class FakePaletteBox:
        def set_active(self, idx):
            called['palette'] = True

    # attach fakes to the window so _on_settings_response can find them
    win._active_sim_draw_area = FakeDrawArea()
    win._palette_box = FakePaletteBox()

    win._show_settings_dialog()

    assert recorded.get('called', False) is True, 'save_settings was not called'
    saved = recorded.get('last', {})
    # Ensure new keys are present
    for key in ('allow_quick_actions', 'save_diagnostics', 'power_profile_target', 'sim_gridlines', 'sim_grid_opacity', 'palette'):
        assert key in saved, f"Expected '{key}' to be saved but it was missing"

    # Check specific expected saved values (based on presenter_stub heuristics)
    assert saved.get('palette') == 'high_contrast', f"Expected palette 'high_contrast', got {saved.get('palette')!r}"
    # Gridlines should be an int and differ from default if our stub changed it
    assert isinstance(saved.get('sim_gridlines'), int), f"Expected sim_gridlines to be int, got {type(saved.get('sim_gridlines'))}"
    assert int(saved.get('sim_gridlines')) != 4, "Expected sim_gridlines to differ from default 4"
    # sim_grid_opacity should be a float and differ from default
    val_op = float(saved.get('sim_grid_opacity', 0.0))
    assert 0.0 < val_op <= 1.0, f"Expected sim_grid_opacity to be between 0.0 and 1.0, got {val_op}"
    assert abs(val_op - 0.06) > 1e-6, "Expected sim_grid_opacity to differ from default 0.06"
    assert saved.get('power_profile_target') in ('performance', 'balanced', 'power-saver', 'Balanced', 'Performance'), f"Unexpected power_profile_target: {saved.get('power_profile_target')!r}"

    # confirm immediate effects applied
    assert called['draw'] is True, 'Expected draw area to be queued for redraw after saving settings'
    assert called['palette'] is True, 'Expected palette widget to be updated after saving settings'


def test_diagnostics_save_button_sensitivity(monkeypatch):
    # Test that the diagnostics dialog's Save button sensitivity follows the `save_diagnostics` setting
    for opt in (True, False):
        win = PowerWindow(None)
        win.settings = {'save_diagnostics': opt}

        captured = {}

        def presenter_capture(self, dlg, on_response=None):
            # Capture diagnostics dialog when presented and inspect Save button sensitivity
            try:
                title = dlg.get_title() if getattr(dlg, 'get_title', None) else ''
            except Exception:
                title = ''
            if 'Diagnostics' in title:
                content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
                captured['diag'] = dlg
                captured['save_sensitive'] = None
                if content is not None:
                    # search for a button likely labelled 'Save to file' or with tooltip about diagnostics
                    def scan(container):
                        try:
                            children = list(container.get_children()) if getattr(container, 'get_children', None) else []
                        except Exception:
                            # try single-child API
                            ch = getattr(container, 'get_child', None)
                            try:
                                c0 = ch() if callable(ch) else None
                                children = [c0] if c0 is not None else []
                            except Exception:
                                children = []
                        for c in children:
                            try:
                                lbl = c.get_label() if getattr(c, 'get_label', None) else ''
                                tip = c.get_tooltip_text() if getattr(c, 'get_tooltip_text', None) else ''
                                if ('Save' in (lbl or '')) or ('diagnostics' in (tip or '').lower()):
                                    # found candidate
                                    try:
                                        captured['save_sensitive'] = bool(c.get_sensitive()) if getattr(c, 'get_sensitive', None) else None
                                        return True
                                    except Exception:
                                        pass
                            except Exception:
                                pass
                            try:
                                if scan(c):
                                    return True
                            except Exception:
                                pass
                        return False
                    scan(content)
                return

            # For settings dialog: find the Diagnostics button and invoke it to open diagnostics dialog
            content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
            if content is not None:
                try:
                    # find diag button by label 'Diagnostics'
                    for c in list(content.get_children()):
                        try:
                            for g in list(c.get_children()):
                                try:
                                    if getattr(g, 'get_label', None) and 'Diagnostics' in (g.get_label() or ''):
                                        # invoke click handler
                                        if getattr(g, '_on_clicked', None):
                                            g._on_clicked(None)
                                        else:
                                            try:
                                                g.emit('clicked')
                                            except Exception:
                                                pass
                                        return
                                except Exception:
                                    pass
                        except Exception:
                            pass
                except Exception:
                    pass

        monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_capture)

        # Show settings dialog which will in turn invoke the diagnostics flow
        win._show_settings_dialog()

        assert 'save_sensitive' in captured, 'Diagnostics dialog was not captured'
        assert captured['save_sensitive'] == bool(opt), f"Expected diagnostic Save sensitive={opt}, got {captured['save_sensitive']}"
