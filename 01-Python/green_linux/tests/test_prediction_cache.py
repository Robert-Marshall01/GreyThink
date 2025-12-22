import joblib

from powerapp.emissions import predict_best_windows


class FakeModel:
    def predict(self, X):
        return [0.0 for _ in range(len(X))]


def test_prediction_cache_avoids_reloading(monkeypatch, tmp_path):
    data = {'model': FakeModel(), 'app_keys': []}
    p = tmp_path / 'fake_model.joblib'
    joblib.dump(data, str(p))

    # wrap joblib.load to count calls
    real_load = __import__('joblib').load
    calls = {'n': 0}

    def load_wrap(path):
        calls['n'] += 1
        return real_load(path)

    monkeypatch.setattr('joblib.load', load_wrap)

    # first call should load model once
    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]
    res1 = predict_best_windows(forecast, window_hours=2, top_k=1, model_path=str(p), use_model=True)
    assert calls['n'] == 1

    # second call with same args should not trigger additional loads (cache hit)
    res2 = predict_best_windows(forecast, window_hours=2, top_k=1, model_path=str(p), use_model=True)
    assert calls['n'] == 1
    assert res1 == res2
