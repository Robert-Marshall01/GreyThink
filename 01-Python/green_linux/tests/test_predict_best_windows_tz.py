import os
import pytest

pytest.skipif_condition = not (os.environ.get('DISPLAY') or os.environ.get('WAYLAND_DISPLAY'))
# This test doesn't require display, but follow repository convention for headful tests

from powerapp.emissions import predict_best_windows


def test_predict_best_windows_preserves_offsets(monkeypatch):
    # Forecast with explicit -07:00 offsets
    forecast = [
        {'timestamp': '2025-12-20T10:00:00-07:00', 'intensity': 200.0},
        {'timestamp': '2025-12-20T11:00:00-07:00', 'intensity': 180.0},
        {'timestamp': '2025-12-20T12:00:00-07:00', 'intensity': 170.0},
    ]

    class FakeModel:
        def predict(self, X):
            # return zeros (iterable) for all candidates
            return [0.0 for _ in range(len(X))]

    def fake_joblib_load(path):
        return {'model': FakeModel(), 'app_keys': []}

    import sys
    import types
    # Use monkeypatch to temporarily inject small fakes into sys.modules so they are
    # restored after the test finishes (avoids breaking other tests that need numpy/joblib).
    monkeypatch.setitem(sys.modules, 'joblib', types.SimpleNamespace(load=fake_joblib_load))
    # Provide a minimal fake numpy so model.predict can be called with the array-like object
    monkeypatch.setitem(sys.modules, 'numpy', types.SimpleNamespace(array=lambda x: x))

    res = predict_best_windows(forecast, window_hours=2, top_k=2, use_model=True, model_path='/tmp/fake')
    assert res, 'expected some results'
    for w in res:
        s = w['start']
        assert ('-07:00' in s) or ('+00:00' in s) or s.endswith('Z'), f"Expected timezone indication in start, got: {s}"
