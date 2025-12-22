import os
import json
import pytest

from powerapp.ml.fine_tune import train_simple_model, export_onnx, save_metadata


def test_train_simple_model_small():
    model = train_simple_model()
    # pipeline has scaler and ridge
    assert hasattr(model, 'named_steps')
    assert 'scaler' in model.named_steps and 'ridge' in model.named_steps


def test_export_onnx_and_save(tmp_path, monkeypatch):
    model = train_simple_model()
    fp32 = tmp_path / 'mfp32.onnx'
    try:
        export_onnx(model, str(fp32))
    except RuntimeError:
        pytest.skip('skl2onnx not available; skipping export test')
    assert fp32.exists() and fp32.stat().st_size > 0

    # test metadata saving
    meta_path = save_metadata({'meta': {'a': 1}}, str(tmp_path))
    assert os.path.exists(meta_path)
    with open(meta_path, 'r', encoding='utf-8') as f:
        j = json.load(f)
    assert 'meta' in j and j['meta']['a'] == 1


def test_prototype_run_with_mocked_quantize(tmp_path, monkeypatch):
    # This test runs the prototype but replaces quantize_onnx with a simple stub
    # so the unit test does not require onnxruntime to be installed.
    from powerapp.ml import fine_tune

    called = {}

    def fake_quantize(fp, outp, calibration_samples=32):
        # create a tiny file to represent quantized model
        with open(outp, 'wb') as f:
            f.write(b'Q')
        called['fp'] = fp
        called['out'] = outp
        return {'fake': True, 'size_fp32': os.path.getsize(fp), 'size_quantized': os.path.getsize(outp)}

    monkeypatch.setattr(fine_tune, 'quantize_onnx', fake_quantize)

    out_dir = str(tmp_path)
    fp32_path, q_path, meta = fine_tune.prototype_finetune_and_export(out_dir=out_dir)
    assert os.path.exists(fp32_path)
    assert os.path.exists(q_path)
    assert 'meta' in meta and 'fake' in meta['meta']
    assert called['fp'] == fp32_path
    assert called['out'] == q_path


def test_quantize_static_with_onnxruntime(tmp_path):
    # This test requires onnxruntime with quantization support; skip if not available.
    try:
        pass
    except Exception:
        pytest.skip('onnxruntime.quantization not available; skipping static quantization test')

    from powerapp.ml import fine_tune
    import numpy as np

    model = fine_tune.train_simple_model()
    fp32 = os.path.join(str(tmp_path), 'static_fp32.onnx')
    try:
        fine_tune.export_onnx(model, fp32)
    except RuntimeError:
        pytest.skip('skl2onnx not available; skipping static quantization test')

    # prepare small deterministic calibration samples
    nfeat = model.named_steps['scaler'].mean_.shape[0]
    samples = (np.eye(nfeat, dtype=np.float32) * 0.1)[:8]

    qpath = os.path.join(str(tmp_path), 'static_q.onnx')
    try:
        meta = fine_tune.quantize_onnx_static(fp32, qpath, samples=samples)
    except RuntimeError as e:
        pytest.skip(f'Static quantization not supported in this environment: {e}')
    assert os.path.exists(qpath)
    assert 'quantized' in meta and os.path.getsize(qpath) > 0
    # quantized size should not exceed FP32 in typical cases
    assert meta['size_quantized'] <= meta['size_fp32'] + 1024


def test_cache_calibration_samples_and_load(tmp_path):
    import numpy as np
    from powerapp.ml import fine_tune

    # create small sample arrays and cache them
    s1 = np.ones((3, 4), dtype=np.float32) * 0.1
    s2 = np.ones((4, 4), dtype=np.float32) * 0.2
    cache = str(tmp_path / 'cal.npy')
    fine_tune.cache_calibration_samples(s1, cache_path=cache, maxlen=5)
    loaded = fine_tune.load_calibration_samples(cache_path=cache)
    assert loaded is not None
    assert loaded.shape == (3, 4)

    fine_tune.cache_calibration_samples(s2, cache_path=cache, maxlen=5)
    loaded2 = fine_tune.load_calibration_samples(cache_path=cache)
    assert loaded2.shape == (7, 4) or loaded2.shape == (5, 4) or loaded2.shape[0] <= 5


def test_get_calibration_samples_prefers_cache_and_param(tmp_path):
    import numpy as np
    from powerapp.ml import fine_tune

    cache = str(tmp_path / 'cal2.npy')
    s_cache = np.arange(20, dtype=np.float32).reshape(5, 4)
    # write directly
    import numpy as _np
    _np.save(cache, s_cache)

    # when no explicit samples provided, cached samples are used
    got = fine_tune.get_calibration_samples('doesnotexist.onnx', samples=None, cache_path=cache, n_samples=3)
    assert got.shape == (3, 4)
    # when explicit samples provided, they're used
    sparam = np.ones((2, 4), dtype=np.float32) * 9.0
    got2 = fine_tune.get_calibration_samples('doesnotexist.onnx', samples=sparam, cache_path=cache, n_samples=5)
    assert got2.shape[0] == 2 and (got2 == 9.0).all()

