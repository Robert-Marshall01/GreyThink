import joblib

from powerapp.emissions import predict_best_windows, augment_suggestions_with_best_window


class FakeModel:
    def predict(self, X):
        # X is a numpy array; prefer windows with larger per-app means by returning negative sum of per-app cols
        return [-(float(x[3:].sum()) if x.shape[0] > 3 else 0.0) for x in X]


def test_predict_best_windows_respects_per_app_series(tmp_path):
    data = {'model': FakeModel(), 'app_keys': ['hotapp']}
    p = tmp_path / 'fake_model.joblib'
    joblib.dump(data, str(p))

    # forecast 6 hours
    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]
    # per_app_series has high activity at hours 02 and 03 (index 2 and 3)
    per_app_series = [
        {'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'per_app_cpu': {'hotapp': 100.0 if i in (2,3) else 0.0}} for i in range(6)
    ]

    res = predict_best_windows(forecast, window_hours=2, top_k=1, model_path=str(p), use_model=True, per_app_series=per_app_series)
    assert len(res) == 1
    # expected window should start at hour 02 (the window covering 02-03)
    assert res[0]['start'].startswith('2025-12-17T02:00')


def test_augment_suggestions_uses_per_suggestion_model(tmp_path):
    data = {'model': FakeModel(), 'app_keys': ['hotapp']}
    p = tmp_path / 'fake_model.joblib'
    joblib.dump(data, str(p))

    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]

    sug_a = {'task_id': 'a', 'task_name': 'A', 'duration_min': 60, 'current_intensity_g': 300.0, 'per_app_series': [
        {'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'per_app_cpu': {'hotapp': 100.0 if i==2 else 0.0}} for i in range(6)
    ]}
    sug_b = {'task_id': 'b', 'task_name': 'B', 'duration_min': 60, 'current_intensity_g': 300.0, 'per_app_series': [
        {'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'per_app_cpu': {'hotapp': 100.0 if i==4 else 0.0}} for i in range(6)
    ]}

    suggestions = [sug_a, sug_b]
    aug = augment_suggestions_with_best_window(suggestions, forecast, window_hours=2, use_model=True, model_path=str(p))
    assert aug[0]['best_window_start'] != aug[1]['best_window_start']
