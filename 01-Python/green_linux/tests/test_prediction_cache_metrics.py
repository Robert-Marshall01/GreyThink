import joblib

from powerapp.emissions import predict_best_windows, get_prediction_cache_stats, reset_prediction_cache_stats


class FakeModel:
    def predict(self, X):
        return [0.0 for _ in range(len(X))]


def test_cache_metrics_hit_miss(monkeypatch, tmp_path):
    reset_prediction_cache_stats()
    data = {'model': FakeModel(), 'app_keys': []}
    p = tmp_path / 'fake_model.joblib'
    joblib.dump(data, str(p))

    # call predict twice; first is miss, second is hit
    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]
    r1 = predict_best_windows(forecast, window_hours=2, top_k=1, model_path=str(p), use_model=True)
    r2 = predict_best_windows(forecast, window_hours=2, top_k=1, model_path=str(p), use_model=True)

    stats = get_prediction_cache_stats()
    assert stats['hits'] >= 1
    assert stats['misses'] >= 1


def test_cache_max_configurable(monkeypatch, tmp_path):
    # set small cache max
    monkeypatch.setenv('POWERAPP_PRED_CACHE_MAX', '1')
    # need to reload module constants; simplest is to clear and re-import via importlib
    import importlib
    import powerapp.emissions as em
    importlib.reload(em)

    data = {'model': FakeModel(), 'app_keys': []}
    p = tmp_path / 'fake_model.joblib'
    joblib.dump(data, str(p))

    f = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]
    # two different forecasts/inputs to cause two distinct cache entries
    res1 = em.predict_best_windows(f, window_hours=2, top_k=1, model_path=str(p), use_model=True)
    f2 = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([100,120,140,160,180,200])]
    res2 = em.predict_best_windows(f2, window_hours=2, top_k=1, model_path=str(p), use_model=True)

    stats = em.get_prediction_cache_stats()
    assert stats['max_size'] == 1
    assert stats['size'] <= 1
