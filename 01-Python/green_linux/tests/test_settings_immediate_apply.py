import pytest

try:
    from powerapp.gtk.main import PowerWindow
except Exception:
    PowerWindow = None


def _make_win_or_skip():
    if PowerWindow is None:
        pytest.skip('PowerWindow unavailable')
    try:
        win = PowerWindow(app=None)
    except Exception:
        pytest.skip('Cannot instantiate PowerWindow in this environment')
    return win


def test_maybe_apply_profile_calls_when_opted_in(monkeypatch):
    # Call the module helper directly
    calls = {'set': []}

    try:
        from powerapp.system import integration as si
        monkeypatch.setattr(si, 'set_power_profile', lambda p: calls['set'].append(p) or True, raising=False)
    except Exception:
        pytest.skip('System integration module unavailable')

    from powerapp.gtk.main import maybe_apply_profile

    maybe_apply_profile(prev_profile='power-saver', new_profile='performance', new_auto=True)

    assert calls['set'] == ['performance']


def test_maybe_apply_profile_no_call_when_not_opted_in(monkeypatch):
    # Call module helper directly
    calls = {'set': []}
    try:
        from powerapp.system import integration as si
        monkeypatch.setattr(si, 'set_power_profile', lambda p: calls['set'].append(p) or True, raising=False)
    except Exception:
        pytest.skip('System integration module unavailable')

    from powerapp.gtk.main import maybe_apply_profile

    maybe_apply_profile(prev_profile='power-saver', new_profile='performance', new_auto=False)

    assert calls['set'] == []


def test_maybe_apply_profile_no_call_when_same_profile(monkeypatch):
    # Call module helper directly
    calls = {'set': []}
    try:
        from powerapp.system import integration as si
        monkeypatch.setattr(si, 'set_power_profile', lambda p: calls['set'].append(p) or True, raising=False)
    except Exception:
        pytest.skip('System integration module unavailable')

    from powerapp.gtk.main import maybe_apply_profile

    maybe_apply_profile(prev_profile='performance', new_profile='performance', new_auto=True)

    assert calls['set'] == []


def test_maybe_apply_profile_handles_integration_exceptions(monkeypatch):
    """If the integration layer raises, the helper should swallow the error and not raise."""
    try:
        from powerapp.system import integration as si
    except Exception:
        pytest.skip('System integration module unavailable')

    def raise_error(p):
        raise RuntimeError('integration failed')

    monkeypatch.setattr(si, 'set_power_profile', raise_error, raising=False)

    from powerapp.gtk.main import maybe_apply_profile

    # Should not raise
    maybe_apply_profile(prev_profile='power-saver', new_profile='performance', new_auto=True)

    # nothing to assert beyond no exception — ensure we didn't call alternate code paths
    # (no-op)
