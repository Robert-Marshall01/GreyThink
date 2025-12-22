import os
import pytest

pytest.skipif_condition = not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY'))
pytestmark = pytest.mark.skipif(pytest.skipif_condition, reason='Requires graphical display')

import gi
gi.require_version('Gtk', '4.0')

from powerapp.gtk.main import PowerWindow


def test_settings_skips_transient_for_on_wayland(monkeypatch):
    # Ensure we simulate Wayland session
    monkeypatch.setenv('XDG_SESSION_TYPE', 'wayland')

    captured = {}

    def presenter_capture(self, dlg, on_response=None):
        captured['dlg'] = dlg

    monkeypatch.setattr(PowerWindow, '_present_and_handle_dialog', presenter_capture)

    win = PowerWindow(None)
    win._show_settings_dialog()

    dlg = captured.get('dlg')
    assert dlg is not None

    try:
        parent = dlg.get_transient_for() if getattr(dlg, 'get_transient_for', None) else None
    except Exception:
        parent = None

    # On Wayland we expect transient_for not to be set so decorations survive
    assert parent is None, f'Expected no transient parent on Wayland, got: {parent}'
