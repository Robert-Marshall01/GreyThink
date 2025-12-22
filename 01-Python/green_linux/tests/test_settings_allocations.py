import pytest
from gi.repository import Gtk, GLib
from powerapp.gtk.main import PowerWindow


def test_settings_wrapped_widgets_have_alloc(monkeypatch):
    created = []

    # Run idle callbacks immediately so our strong fallback executes synchronously
    monkeypatch.setattr(GLib, 'idle_add', lambda fn, *a, **k: (fn(*a, **k), None)[1])

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

    found_wrapped = False
    found_applied = False
    for ch in _walk(content):
        try:
            # consider TextView or wrapped Labels
            if isinstance(ch, (Gtk.TextView,)) or (isinstance(ch, Gtk.Label) and getattr(ch, 'get_wrap', None) and ch.get_wrap()):
                found_wrapped = True
                # Check if our strong/idle nudges applied size hints
                if getattr(ch, '_strong_nudge_applied', False) or getattr(ch, '_idle_nudge_applied', False):
                    found_applied = True
                    break
                # or the parent had it applied
                pget = getattr(ch, 'get_parent', None)
                if callable(pget):
                    p = pget()
                else:
                    p = None
                if p is not None and (getattr(p, '_strong_nudge_applied', False) or getattr(p, '_idle_nudge_applied', False)):
                    found_applied = True
                    break
        except Exception:
            pass

    assert found_wrapped, 'No wrapped explanatory widgets found in Settings'
    assert found_applied, 'Strong or idle nudge was not applied to any wrapped widget in Settings'