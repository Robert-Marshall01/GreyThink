from powerapp.emissions import augment_suggestions_with_best_window


def test_augment_uses_model_prediction(monkeypatch):
    # Single suggestion, model should be used when use_model=True
    suggestion = {'task_id': 't1', 'task_name': 'A', 'urgency': 'low', 'duration_min': 60, 'current_intensity_g': 400.0}
    suggestions = [suggestion]
    # forecast: three candidate windows
    forecast = [
        {'timestamp': '2025-12-17T00:00:00+00:00', 'intensity': 200.0},
        {'timestamp': '2025-12-17T01:00:00+00:00', 'intensity': 180.0},
        {'timestamp': '2025-12-17T02:00:00+00:00', 'intensity': 160.0},
        {'timestamp': '2025-12-17T03:00:00+00:00', 'intensity': 140.0},
        {'timestamp': '2025-12-17T04:00:00+00:00', 'intensity': 120.0},
    ]

    # Monkeypatch predict_best_windows to return a made-up best window
    def fake_predict(forecast_arg, window_hours=2, top_k=1, model_path=None, use_model=False):
        return [{'start': '2025-12-17T03:00:00+00:00', 'end': '2025-12-17T05:00:00+00:00', 'avg_intensity': 140.0}]

    import powerapp.emissions as emissions
    monkeypatch.setattr(emissions, 'predict_best_windows', fake_predict)

    aug = augment_suggestions_with_best_window(suggestions, forecast, window_hours=2, use_model=True, model_path='fake')
    assert aug[0]['best_window_start'].startswith('2025-12-17T03:00')
    assert aug[0]['best_window_avg_intensity'] == 140.0
    # compute expected saving: avg_power_kw 0.05 * dur 1h = 0.05, delta = 400 - 140 = 260 g -> kg = 0.05*260/1000 = 0.013
    assert abs(aug[0]['estimated_saving_kgCO2_best_window'] - 0.013) < 1e-6
