from powerapp.emissions import find_low_carbon_windows, predict_best_windows


def test_predict_best_windows_fallback_matches_find_low_carbon_windows():
    # synthetic forecast of 6 hours with varying intensities
    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]
    fw = find_low_carbon_windows(forecast, window_hours=2, top_k=2)
    pw = predict_best_windows(forecast, window_hours=2, top_k=2, use_model=False)
    assert fw == pw


# Top-level FakeModel so it is picklable by joblib
class FakeModel:
    def predict(self, X):
        # return decreasing predictions depending on first feature (start hour) so sorting differs
        return [float(row[0]) for row in X]


def test_predict_best_windows_uses_model_when_available(monkeypatch, tmp_path):
    # Skip heavy training; create a fake model file with a simple predict
    data = {'model': FakeModel(), 'app_keys': []}
    import pytest
    pytest.importorskip('joblib')
    import joblib
    p = tmp_path / 'fake.joblib'
    joblib.dump(data, str(p))
    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]

    res = predict_best_windows(forecast, window_hours=2, top_k=2, model_path=str(p), use_model=True)
    # Since FakeModel predicts start_hour as value, the first returned should correspond to the lowest start hour (0)
    assert len(res) == 2
    assert res[0]['start'].startswith('2025-12-17T00:00')
