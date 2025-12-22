from powerapp.ml.explain import explain_window_choice


def _make_forecast():
    return [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200, 180, 100, 90, 120, 160])]


def test_explain_returns_top_features():
    forecast = _make_forecast()
    suggestion = {'window_hours': 2, 'current_intensity_g': 400.0}
    per_app_series = []
    for i, p in enumerate(forecast):
        if i in (2, 3):
            per_app_series.append({'timestamp': p['timestamp'], 'per_app_cpu': {'appA': 70.0, 'appB': 30.0}})
        else:
            per_app_series.append({'timestamp': p['timestamp'], 'per_app_cpu': {'appA': 10.0, 'appB': 10.0}})

    res = explain_window_choice(suggestion, forecast, forecast[2]['timestamp'], per_app_series=per_app_series, top_k=3)
    assert isinstance(res, list)
    assert any(r['feature'].startswith('avg_intensity_delta') for r in res)
    # app features present
    assert any(r['feature'].startswith('app:appA') or r['feature'].startswith('app:appB') for r in res)
