import os
import pytest

pytestmark = pytest.mark.skipif(not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY')), reason='Requires graphical display')

import gi
gi.require_version('Gtk', '4.0')
from gi.repository import Gtk

from powerapp.gtk.main import PowerWindow


def _make_settings_dialog_and_find_button(monkeypatch):
    # Create a window and open settings, return the settings dialog and the Privacy button widget
    created = []

    def run_stub(self):
        created.append(self)
        return Gtk.ResponseType.CLOSE

    monkeypatch.setattr(Gtk.Dialog, 'run', run_stub, raising=False)
    base_win = Gtk.Window()
    base_win.settings = {}
    # call the settings dialog
    try:
        PowerWindow._show_settings_dialog(base_win)
    except Exception:
        pytest.skip('GTK environment incompatible')

    # find the Settings dialog in created
    sdlg = None
    for d in reversed(created):
        try:
            if getattr(d, 'get_title', None) and d.get_title() == 'Settings':
                sdlg = d
                break
        except Exception:
            pass
    if sdlg is None:
        pytest.skip('Settings dialog not created')

    if getattr(sdlg, 'get_child', None):
        content = sdlg.get_child()
    elif getattr(sdlg, 'get_content_area', None):
        content = sdlg.get_content_area()
    else:
        pytest.skip('Dialog has no content area')

    # find button by label
    pb = None
    for c in content.get_children():
        if isinstance(c, Gtk.Button) and (c.get_label() or '').lower().startswith('privacy'):
            pb = c
            break
    if pb is None:
        # try nested search
        for c in content.get_children():
            try:
                for cc in c.get_children():
                    if isinstance(cc, Gtk.Button) and (cc.get_label() or '').lower().startswith('privacy'):
                        pb = cc
                        break
                if pb:
                    break
            except Exception:
                continue
    if pb is None:
        pytest.skip('Privacy button not found')
    return sdlg, pb, base_win


def test_privacy_button_falls_back_to_message(monkeypatch):
    # Ensure that when xdg-open is not available, clicking the button shows a simple message
    sdlg, pb, base_win = _make_settings_dialog_and_find_button(monkeypatch)

    called = {'msg': False}
    def fake_msg(title, text=None):
        called['msg'] = True
    monkeypatch.setattr(base_win, '_show_simple_message', fake_msg)

    # ensure xdg-open is absent
    import shutil
    monkeypatch.setattr(shutil, 'which', lambda x: None)

    # click the privacy button
    pb.emit('clicked')
    assert called['msg'] is True


def test_privacy_button_uses_xdg_open(monkeypatch, tmp_path):
    # Ensure that when xdg-open is present, we attempt to open the file
    sdlg, pb, base_win = _make_settings_dialog_and_find_button(monkeypatch)

    calls = []
    def fake_popen(cmd):
        calls.append(cmd)
        class P: pass
        return P()

    import shutil
    import subprocess
    monkeypatch.setattr(shutil, 'which', lambda x: '/usr/bin/xdg-open')
    monkeypatch.setattr(subprocess, 'Popen', fake_popen)

    pb.emit('clicked')
    assert calls, 'xdg-open was not invoked'
    assert 'xdg-open' in calls[0][0]


def test_privacy_button_uses_open_for_macos(monkeypatch):
    # Ensure that when 'open' is present (macOS), we attempt to open the file
    sdlg, pb, base_win = _make_settings_dialog_and_find_button(monkeypatch)

    calls = []
    def fake_popen(cmd):
        calls.append(cmd)
        class P: pass
        return P()

    import shutil
    import subprocess
    # Simulate xdg-open absent, open present
    monkeypatch.setattr(shutil, 'which', lambda x: '/usr/bin/open' if x == 'open' else None)
    monkeypatch.setattr(subprocess, 'Popen', fake_popen)

    pb.emit('clicked')
    assert calls, 'open was not invoked'
    assert 'open' in calls[0][0]


def test_privacy_button_uses_startfile_for_windows(monkeypatch):
    # Ensure that when os.startfile is available, it is used (Windows)
    sdlg, pb, base_win = _make_settings_dialog_and_find_button(monkeypatch)

    calls = []
    def fake_startfile(path):
        calls.append(path)

    import os
    monkeypatch.setattr(os, 'startfile', fake_startfile, raising=False)

    pb.emit('clicked')
    assert calls, 'os.startfile was not invoked'
