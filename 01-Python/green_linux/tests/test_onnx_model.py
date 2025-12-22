import os
import pytest

pytest.importorskip('onnx')
pytest.importorskip('onnxruntime')
pytest.importorskip('joblib')
import joblib

ONNX_PATH = os.path.join(os.getcwd(), 'models', 'window_predictor_small.onnx')
FP16_PATH = os.path.join(os.getcwd(), 'models', 'window_predictor_small.fp16.onnx')
SIZE_LIMIT_MB = float(os.environ.get('MODEL_SIZE_LIMIT_MB', '5'))


def test_onnx_model_exists_and_size():
    if not os.path.exists(ONNX_PATH):
        pytest.skip('Small ONNX model not present')
    size_mb = os.path.getsize(ONNX_PATH) / (1024.0 * 1024.0)
    assert size_mb <= SIZE_LIMIT_MB, f"ONNX model too large: {size_mb:.2f} MB (limit {SIZE_LIMIT_MB} MB)"


def test_onnx_model_inference():
    import onnxruntime as ort
    import joblib
    # Load app_keys from the joblib small model to determine feature dims
    joblib_p = os.path.join(os.getcwd(), 'models', 'window_predictor_small.joblib')
    if not os.path.exists(joblib_p):
        pytest.skip('Small joblib model missing (used for shape)')
    data = joblib.load(joblib_p)
    app_keys = data.get('app_keys', []) if isinstance(data, dict) else []
    n_features = 3 + len(app_keys)

    sess = ort.InferenceSession(ONNX_PATH)
    # Construct a sample input matching the declared input name
    input_name = sess.get_inputs()[0].name
    import numpy as np
    X = np.zeros((1, n_features), dtype=np.float32)
    res = sess.run(None, {input_name: X})
    # result should be a 1D array or single-element list
    assert res is not None
    # We expect one output with shape (1,)
    out = res[0]
    assert hasattr(out, 'shape')
    assert out.shape[0] == 1


def test_fp16_model_if_present():
    if not os.path.exists(FP16_PATH):
        pytest.skip('FP16 ONNX model not present')
    size_mb = os.path.getsize(FP16_PATH) / (1024.0 * 1024.0)
    assert size_mb <= SIZE_LIMIT_MB, f"FP16 ONNX model too large: {size_mb:.2f} MB (limit {SIZE_LIMIT_MB} MB)"

    import onnxruntime as ort
    sess = ort.InferenceSession(FP16_PATH)
    input_name = sess.get_inputs()[0].name
    import numpy as np
    # Build sample input using joblib shape
    data = joblib.load(os.path.join(os.getcwd(), 'models', 'window_predictor_small.joblib'))
    app_keys = data.get('app_keys', []) if isinstance(data, dict) else []
    n_features = 3 + len(app_keys)
    # FP16 model expects float16 inputs
    X = np.zeros((1, n_features), dtype=np.float16)
    res = sess.run(None, {input_name: X})
    assert res is not None
    out = res[0]
    assert hasattr(out, 'shape')
    assert out.shape[0] == 1
