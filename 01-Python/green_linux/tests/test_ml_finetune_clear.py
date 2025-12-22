import os
from powerapp.ml import fine_tune
import numpy as np

def test_clear_calibration_cache(tmp_path):
    cache = str(tmp_path / 'cal_test.npy')
    samples = np.ones((2,4), dtype=np.float32)
    fine_tune.cache_calibration_samples(samples, cache_path=cache)
    assert os.path.exists(cache)
    assert fine_tune.clear_calibration_cache(cache_path=cache) is True
    assert not os.path.exists(cache)
    # clearing again returns False
    assert fine_tune.clear_calibration_cache(cache_path=cache) is False
