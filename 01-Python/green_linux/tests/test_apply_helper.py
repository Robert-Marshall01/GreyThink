import pytest

from powerapp.gtk.main import PowerWindow


def test_apply_postponement_changes_profile(monkeypatch, tmp_path):
    """Directly call the apply helper and verify set_power_profile is invoked when configured."""

    try:
        win = PowerWindow(app=None)
    except Exception:
        pytest.skip('Cannot instantiate PowerWindow')

    # Ensure settings indicate auto profile change
    win.settings = {}
    win._applied_suggestions = []
    win.settings['auto_set_power_profile'] = True
    win.settings['power_profile_target'] = 'performance'

    suggestion = {'task_name': 'UnitApplyTest', 'current_intensity_g': 100}
    forecast = [{'timestamp': '2025-12-21T12:00:00Z', 'intensity': 100}]
    chosen = '2025-12-21T12:00:00Z'

    # Replace simulate_postponement to be deterministic
    def fake_sim(suggestion, forecast, chosen, window_hours, current_intensity, per_app_series):
        return {'savings_kg': 1.23}

    monkeypatch.setattr('powerapp.emissions.simulate_postponement', fake_sim)

    # Build a fake integration module
    calls = {}

    class FakeSI:
        def get_current_power_profile(self):
            calls['prior'] = 'balanced'
            return 'balanced'

        def set_power_profile(self, target):
            calls['set'] = target
            return True

        def schedule_user_timer(self, unit, when, cmd):
            calls['schedule'] = (unit, when, cmd)
            return True

    si = FakeSI()

    # Ensure helper is present: attach it by showing the simulator (it binds the helper for testing)
    try:
        PowerWindow._show_simulator_dialog(win, suggestion, forecast)
    except Exception:
        # If we cannot show the simulator dialog in this environment, attempt to bind the helper directly
        if not getattr(win, '_apply_postponement', None):
            # bind a simple wrapper that calls the inner helper if present
            pytest.skip('Cannot bind helper; GTK environment incompatible')

    # Now call the helper directly with injected si_module
    applied = win._apply_postponement(suggestion, forecast, chosen=chosen, window_hours=2, profile_check=None, do_profile=True, si_module=si)

    assert applied is not None
    assert applied['task_name'] == 'UnitApplyTest'
    assert calls.get('set') == 'performance'