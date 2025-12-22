from gi.repository import GLib

from powerapp.gtk.main import PowerWindow


def test_schedule_safe_destroy_respects_mapping():
    win = PowerWindow(app=None)

    class FakeDlg:
        def __init__(self):
            self.destroyed = False
        def get_visible(self):
            return True
        def get_allocated_width(self):
            return 200
        def get_allocated_height(self):
            return 100
        def destroy(self):
            self.destroyed = True

    dlg = FakeDlg()
    # run with attempts=1 and delay 0 so checks run synchronously
    win._schedule_safe_destroy(dlg, attempts=1, delay_sec=0)
    assert dlg.destroyed is False, 'Dialog should not be destroyed when visible/mapped'


def test_schedule_safe_destroy_destroys_if_not_mapped(monkeypatch):
    win = PowerWindow(app=None)

    class FakeDlg:
        def __init__(self):
            self.destroyed = False
        def get_visible(self):
            return False
        def get_allocated_width(self):
            return 0
        def get_allocated_height(self):
            return 0
        def destroy(self):
            self.destroyed = True

    # Make GLib.timeout_add_seconds execute callbacks immediately
    def immediate_timeout(delay, cb, *args, **kwargs):
        try:
            cb()
        except Exception:
            pass
        return 0

    monkeypatch.setattr(GLib, 'timeout_add_seconds', immediate_timeout)

    dlg = FakeDlg()
    win._schedule_safe_destroy(dlg, attempts=1, delay_sec=0)
    assert dlg.destroyed is True, 'Dialog should be destroyed when not mapped after attempts'
