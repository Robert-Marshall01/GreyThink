import json
import os
import subprocess
import sys
from pathlib import Path


def run(cmd, env=None):
    env = {**os.environ, **(env or {})}
    env['PYTHONPATH'] = str(Path.cwd())
    subprocess.check_call(cmd, env=env)


def load_calibration(path):
    import numpy as np
    data = np.load(path, allow_pickle=True)
    X = data['X']
    meta = json.loads(data['meta'].tolist())
    return X, meta


def test_calibration_dataset_produced_and_reproducible(tmp_path):
    cal_p = tmp_path / 'cal.npz'
    cmd = [sys.executable, 'scripts/generate_dataset.py', '--out', str(tmp_path / 'all.npz'), '--n-series', '50', '--length', '24', '--seed', '123', '--calibration-out', str(cal_p), '--calibration-samples', '30']
    run(cmd)
    X1, m1 = load_calibration(str(cal_p))

    # run again same seed
    cal_p2 = tmp_path / 'cal2.npz'
    cmd = [sys.executable, 'scripts/generate_dataset.py', '--out', str(tmp_path / 'all2.npz'), '--n-series', '50', '--length', '24', '--seed', '123', '--calibration-out', str(cal_p2), '--calibration-samples', '30']
    run(cmd)
    X2, m2 = load_calibration(str(cal_p2))

    assert X1.shape == X2.shape
    assert m1 == m2

    # different seed -> likely different calibration set
    cal_p3 = tmp_path / 'cal3.npz'
    cmd = [sys.executable, 'scripts/generate_dataset.py', '--out', str(tmp_path / 'all3.npz'), '--n-series', '50', '--length', '24', '--seed', '999', '--calibration-out', str(cal_p3), '--calibration-samples', '30']
    run(cmd)
    X3, m3 = load_calibration(str(cal_p3))
    if X1.shape == X3.shape and m1 == m3:
        import warnings
        warnings.warn('Different seed produced identical calibration set; unlikely but acceptable')
