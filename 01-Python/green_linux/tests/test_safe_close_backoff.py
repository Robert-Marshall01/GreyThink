import gi
gi.require_version('Gtk', '4.0')
from gi.repository import GLib


from powerapp.gtk.main import PowerWindow


class DummyWithResponseLong:
    """Dialog that reports visible for N checks before allowing destroy."""
    def __init__(self, visible_checks=4):
        self.responded = None
        self.destroyed = False
        self._n = 0
        self._visible_checks = visible_checks
    def response(self, r):
        self.responded = r
    def get_visible(self):
        self._n += 1
        return self._n <= self._visible_checks
    def get_allocated_width(self):
        return 0
    def get_allocated_height(self):
        return 0
    def destroy(self):
        self.destroyed = True


def test_safe_close_exponential_backoff(monkeypatch):
    win = PowerWindow(None)
    # This dialog will be visible for several checks, requiring multiple backoff attempts
    d = DummyWithResponseLong(visible_checks=4)

    def fake_timeout_add(ms, cb):
        try:
            # Call cb immediately to simulate timer firing synchronously in tests
            cb()
        except Exception:
            pass
        return 1

    monkeypatch.setattr(GLib, 'timeout_add', fake_timeout_add)

    win._safe_close(d, response=999)
    assert d.responded == 999
    assert d.destroyed is True
