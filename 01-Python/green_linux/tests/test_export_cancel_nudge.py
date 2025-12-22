from gi.repository import Gtk, GLib

from powerapp.gtk.main import PowerWindow


def test_export_dialog_cancel_stops_represent(monkeypatch):
    # Make GLib.timeout_add call callbacks immediately so nudges run synchronously in the test
    def immediate_timeout(delay_ms, cb, *args, **kwargs):
        try:
            cb()
        except Exception:
            pass
        return 0

    monkeypatch.setattr(GLib, 'timeout_add', immediate_timeout)

    class FakeDlg:
        def __init__(self):
            self.present_calls = 0
            self._handler = None
            self.destroyed = False

        def connect(self, sig, cb):
            if sig == 'response':
                self._handler = cb

        def present(self):
            self.present_calls += 1

        def show(self):
            self.present_calls += 1

        def get_visible(self):
            # Simulate not mapped so the nudge would try to re-present
            return False

        def get_allocated_width(self):
            return 0

        def get_allocated_height(self):
            return 0

        def get_title(self):
            return 'Export'

        def destroy(self):
            self.destroyed = True

    win = PowerWindow(app=None)
    dlg = FakeDlg()

    # Present the dialog (this should call present at least once)
    win._present_and_handle_dialog(dlg, on_response=lambda d, r: None)
    assert dlg.present_calls >= 1

    # Simulate user cancelling the file chooser
    assert dlg._handler is not None
    dlg._handler(dlg, Gtk.ResponseType.CANCEL)

    # After cancellation the nudge machinery should not re-present the dialog any further.
    old_calls = dlg.present_calls
    # Trigger another cycle of nudges (immediate_timeout executes synchronously)
    try:
        GLib.timeout_add(100, lambda: None)
    except Exception:
        pass

    assert dlg.present_calls == old_calls
