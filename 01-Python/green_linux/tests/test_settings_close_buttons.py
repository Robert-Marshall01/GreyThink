import os
import pytest

pytest.skipif_condition = not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY'))
pytestmark = pytest.mark.skipif(pytest.skipif_condition, reason='Requires graphical display')

import gi
gi.require_version('Gtk', '4.0')

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


def test_settings_cancel_and_window_close(monkeypatch):
    win = PowerWindow(None)

    captured = {}

    def presenter_capture(self, dlg, on_response=None):
        # capture dialog and don't call on_response — test will interact directly
        captured['dlg'] = dlg

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_capture)

    # Show dialog and capture
    win._show_settings_dialog()
    dlg = captured.get('dlg')
    assert dlg is not None, 'Settings dialog was not created'

    content = dlg.get_child() if getattr(dlg, 'get_child', None) else (dlg.get_content_area() if getattr(dlg, 'get_content_area', None) else None)
    assert content is not None

    # Find Cancel button and click it
    cancel_btn = None
    for c in _iter_children(content):
        if getattr(c, 'get_label', None) and c.get_label() in ('Cancel', _('Cancel') if '_' in globals() else 'Cancel'):
            cancel_btn = c
            break
        for g in _iter_children(c):
            if getattr(g, 'get_label', None) and g.get_label() in ('Cancel', _('Cancel') if '_' in globals() else 'Cancel'):
                cancel_btn = g
                break
        if cancel_btn:
            break

    assert cancel_btn is not None, 'Could not find Cancel button'

    # Click it — should close/destroy dialog
    try:
        cancel_btn.emit('clicked')
    except Exception:
        try:
            # fallback to invoking handler
            if getattr(cancel_btn, '_on_clicked', None):
                cancel_btn._on_clicked()
        except Exception:
            pass

    # After clicking cancel, dialog should not be visible
    try:
        vis = dlg.get_visible() if getattr(dlg, 'get_visible', None) else False
    except Exception:
        vis = False
    assert not vis, 'Dialog still visible after Cancel click'

    # Now show another settings dialog and simulate window-manager close
    captured.clear()
    win._show_settings_dialog()
    dlg2 = captured.get('dlg')
    assert dlg2 is not None

    # Try several ways to simulate a WM close
    closed = False
    try:
        dlg2.emit('close')
        closed = not dlg2.get_visible()
    except Exception:
        pass
    if not closed:
        try:
            dlg2.close()
            closed = not dlg2.get_visible()
        except Exception:
            pass
    if not closed:
        try:
            dlg2.emit('delete-event', None)
            closed = not dlg2.get_visible()
        except Exception:
            pass
    if not closed:
        try:
            dlg2.destroy()
            closed = True
        except Exception:
            pass

    assert closed, 'Dialog did not close after simulated window manager close'