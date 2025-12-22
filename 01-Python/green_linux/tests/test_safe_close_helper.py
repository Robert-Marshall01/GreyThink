import gi
gi.require_version('Gtk', '4.0')
from gi.repository import GLib


from powerapp.gtk.main import PowerWindow


class DummyNoResponse:
    def __init__(self):
        self.destroyed = False
    def destroy(self):
        self.destroyed = True


class DummyWithResponse:
    def __init__(self, responds_but_not_destroy=False):
        self.responded = False
        self.destroyed = False
        self._n = 0
        self._dont_destroy = responds_but_not_destroy
    def response(self, r):
        self.responded = r
    def get_visible(self):
        # simulate still visible once; then next time return False to allow scheduled destroy to run
        self._n += 1
        if self._n <= 2:
            return True
        return False
    def destroy(self):
        self.destroyed = True


def test_safe_close_destroys_without_response(monkeypatch):
    win = PowerWindow(None)
    d = DummyNoResponse()
    win._safe_close(d)
    assert d.destroyed is True, 'Dialog without response() should be destroyed immediately'


def test_safe_close_schedules_destroy_when_response_does_not_close(monkeypatch):
    win = PowerWindow(None)
    d = DummyWithResponse(responds_but_not_destroy=True)

    # Make GLib.timeout_add execute callbacks immediately for determinism
    called = {}

    def fake_timeout_add(ms, cb):
        # Call wrapper (cb returns tuple/False) and return a fake id
        try:
            cb()
        except Exception:
            pass
        return 1

    monkeypatch.setattr(GLib, 'timeout_add', fake_timeout_add)

    win._safe_close(d, response=123)
    # response should have been recorded
    assert d.responded == 123
    # scheduled destroy should have run synchronously via monkeypatched timeout_add
    assert d.destroyed is True
