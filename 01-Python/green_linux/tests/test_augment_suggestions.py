from powerapp.emissions import augment_suggestions_with_best_window


def test_augment_with_forecast():
    suggestions = [
        {'task_id': 't1', 'task_name': 'A', 'urgency': 'low', 'duration_min': 60, 'current_intensity_g': 400.0, 'estimated_saving_kgCO2_if_postponed': 0.0, 'suggested_postpone_until': '2025-12-15T01:00:00Z'}
    ]
    # forecast with a clear low window (50 g) at first hour
    forecast = [
        {'timestamp': '2025-12-15T00:00:00+00:00', 'intensity': 50.0},
        {'timestamp': '2025-12-15T01:00:00+00:00', 'intensity': 100.0}
    ]
    aug = augment_suggestions_with_best_window(suggestions, forecast, window_hours=1)
    assert aug[0]['best_window_start'] is not None
    assert aug[0]['best_window_avg_intensity'] == 50.0
    # compute expected: avg_power_kw 0.05 * 1h = 0.05 kWh, delta = 400-50 = 350 g/kWh => kg = 0.05*350/1000 = 0.0175
    assert abs(aug[0]['estimated_saving_kgCO2_best_window'] - 0.0175) < 1e-9
