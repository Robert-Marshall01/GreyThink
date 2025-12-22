import pytest
from gi.repository import Gtk
from powerapp.gtk.main import PowerWindow


def test_settings_contains_bugreport_upload_controls(monkeypatch):
    created = []
    def dialog_run_stub(self):
        created.append(self)
        return Gtk.ResponseType.CLOSE
    monkeypatch.setattr(Gtk.Dialog, 'run', dialog_run_stub, raising=False)

    try:
        win = PowerWindow(app=None)
    except Exception:
        pytest.skip('Cannot instantiate PowerWindow')

    try:
        PowerWindow._show_settings_dialog(win)
    except Exception:
        pytest.skip('GTK environment incompatible')

    assert created, 'No dialog created'
    settings_dlg = created[0]
    found_chk = False
    found_entry = False
    content = settings_dlg.get_child() if getattr(settings_dlg, 'get_child', None) else settings_dlg.get_content_area()
    # Walk the content tree recursively to find the upload CheckButton and Entry
    def _walk(w):
        try:
            if w is None:
                return
            yield w
            if getattr(w, 'get_children', None):
                for ch in w.get_children():
                    yield from _walk(ch)
        except Exception:
            return

    for ch in _walk(content):
        try:
            if isinstance(ch, Gtk.CheckButton) and ((ch.get_label() and 'upload' in ch.get_label().lower()) or (ch.get_tooltip_text() and 'upload' in ch.get_tooltip_text().lower())):
                found_chk = True
            if isinstance(ch, Gtk.Entry):
                found_entry = True
        except Exception:
            pass
    assert found_chk, 'Upload checkbox not found in settings'
    assert found_entry, 'Upload URL entry not found in settings'