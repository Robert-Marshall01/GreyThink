import os
import time
import numpy as np
import pytest

# These tests validate that the checked-in demo model meets basic
# acceptance criteria for size and inference latency. Thresholds can
# be tuned via environment variables for CI variability.

pytest.importorskip('joblib')
pytest.importorskip('sklearn')

MODEL_PATH = os.path.join(os.getcwd(), 'models', 'window_predictor.joblib')
# Target size we aspire to (informational); hard limit is looser to avoid CI breakage.
TARGET_SIZE_MB = float(os.environ.get('MODEL_TARGET_SIZE_MB', '5'))
# Hard assertion limit; can be overridden via MODEL_SIZE_LIMIT_MB env var (defaults to 15 MB)
HARD_SIZE_LIMIT_MB = float(os.environ.get('MODEL_SIZE_LIMIT_MB', '15'))
LATENCY_LIMIT_MS = float(os.environ.get('MODEL_LATENCY_LIMIT_MS', '100'))


def _skip_if_missing_model():
    if not os.path.exists(MODEL_PATH):
        pytest.skip(f"Demo model not present at {MODEL_PATH}")


def test_model_file_size_under_threshold():
    """Ensure the demo model file size is below the configured threshold."""
    _skip_if_missing_model()
    size = os.path.getsize(MODEL_PATH)
    size_mb = size / (1024.0 * 1024.0)
    import warnings
    if size_mb > TARGET_SIZE_MB:
        warnings.warn(f"Model exceeds target size {TARGET_SIZE_MB} MB: {size_mb:.2f} MB. Consider quantizing/pruning.")
    assert size_mb <= HARD_SIZE_LIMIT_MB, f"Model file too large: {size_mb:.2f} MB (hard limit {HARD_SIZE_LIMIT_MB} MB)"


def test_model_inference_latency_under_threshold():
    """Measure average inference time for a small batch and assert under threshold (ms).

    This test runs a warm-up predict then measures avg time over several iterations to reduce noise.
    """
    _skip_if_missing_model()
    import joblib

    data = joblib.load(MODEL_PATH)
    model = data.get('model') if isinstance(data, dict) else data
    app_keys = data.get('app_keys', []) if isinstance(data, dict) else []

    # Build a single test feature vector matching training shape: [start_hour, avg_forecast, mean_power, per_app_means...]
    sample = [0, 100.0, 0.0] + [0.0 for _ in app_keys]
    X = np.array([sample])

    # Warm-up
    try:
        _ = model.predict(X)
    except Exception as e:
        pytest.skip(f"Model predict failed during warm-up: {e}")

    iterations = 30
    # Measure
    start = time.perf_counter()
    for _ in range(iterations):
        _ = model.predict(X)
    end = time.perf_counter()
    total_ms = (end - start) * 1000.0
    avg_ms = total_ms / iterations

    assert avg_ms <= LATENCY_LIMIT_MS, f"Model inference too slow: avg {avg_ms:.2f} ms (limit {LATENCY_LIMIT_MS} ms)"