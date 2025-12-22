import os
import pytest
from powerapp.emissions import predict_best_windows


def test_demo_model_loaded_and_predicts():
    """Ensure the checked-in demo model can be loaded and used to predict windows."""
    p = os.path.join(os.getcwd(), 'models', 'window_predictor.joblib')
    if not os.path.exists(p):
        pytest.skip('Demo model not present in models/window_predictor.joblib')

    # small synthetic forecast
    forecast = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]

    res = predict_best_windows(forecast, window_hours=2, top_k=2, model_path=p, use_model=True)
    assert isinstance(res, list)
    assert len(res) == 2
    # each result should include 'start' and 'avg_intensity'
    assert 'start' in res[0] and 'avg_intensity' in res[0]
