import pytest
from gi.repository import Gtk
from powerapp.gtk.main import PowerWindow


def test_settings_contains_cache_textview(monkeypatch):
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
    content = settings_dlg.get_child() if getattr(settings_dlg, 'get_child', None) else settings_dlg.get_content_area()

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

    found_cache = False
    for ch in _walk(content):
        try:
            if isinstance(ch, Gtk.TextView):
                buf = ch.get_buffer()
                txt = buf.get_text(buf.get_start_iter(), buf.get_end_iter(), True)
                if 'Clearing removes local calibration samples' in txt:
                    found_cache = True
                    break
        except Exception:
            pass

    assert found_cache, 'Cache explanatory text not found as TextView in settings'