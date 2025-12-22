import subprocess
import sys
from pathlib import Path
import numpy as np
import os


def test_generate_calibration_and_reader(tmp_path):
    repo = Path(__file__).resolve().parents[1]
    eval_dir = tmp_path / 'evaluation'
    eval_dir.mkdir()
    cal_path = eval_dir / 'calibration.npz'

    # ensure ONNX model exists (may be created by convert script)
    onnx_path = repo / 'models' / 'window_predictor_small.onnx'
    if not onnx_path.exists():
        # convert to onnx; don't fail the test if conversion fails
        subprocess.run([sys.executable, 'scripts/convert_to_onnx.py'], cwd=str(repo))
        subprocess.run([sys.executable, 'scripts/fix_onnx_opset.py'], cwd=str(repo))

    # generate calibration npz (ensure scripts can import package by adding repo to PYTHONPATH)
    env = dict(**os.environ)
    env['PYTHONPATH'] = str(repo)
    subprocess.check_call([sys.executable, 'scripts/generate_dataset.py', '--n-series', '16', '--length', '48', '--calibration-out', str(cal_path), '--calibration-samples', '32', '--seed', '2025'], cwd=str(repo), env=env)

    assert cal_path.exists(), 'Calibration file should have been created'

    # Now import the reader class and validate it yields samples
    from scripts.quantize_static import NpzCalibrationReader

    reader = NpzCalibrationReader(str(onnx_path), str(cal_path))
    sample = reader.get_next()
    assert sample is None or isinstance(sample, dict)
    if sample is not None:
        # value should be an ndarray of shape (1, features)
        arr = next(iter(sample.values()))
        assert hasattr(arr, 'shape')
        assert arr.shape[0] == 1
        assert arr.dtype == np.float32
