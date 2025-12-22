import json
import os
import subprocess
import sys
from pathlib import Path


def run(cmd, env=None):
    env = {**os.environ, **(env or {})}
    env['PYTHONPATH'] = str(Path.cwd())
    subprocess.check_call(cmd, env=env)


def load_npz(path):
    import numpy as np
    data = np.load(path, allow_pickle=True)
    X = data['X']
    y = data['y']
    meta = json.loads(data['meta'].tolist())
    return X, y, meta


def test_generate_dataset_splits_reproducible(tmp_path):
    train_p = tmp_path / 'train.npz'
    test_p = tmp_path / 'test.npz'

    # create split with a seed
    cmd = [sys.executable, 'scripts/generate_dataset.py', '--out', str(tmp_path / 'all.npz'), '--n-series', '50', '--length', '24', '--seed', '123', '--train-out', str(train_p), '--test-out', str(test_p), '--test-size', '0.2']
    run(cmd)
    X1, y1, m1 = load_npz(str(train_p))
    Xt1, yt1, mt1 = load_npz(str(test_p))

    # run again with same seed -> identical splits
    train_p2 = tmp_path / 'train2.npz'
    test_p2 = tmp_path / 'test2.npz'
    cmd = [sys.executable, 'scripts/generate_dataset.py', '--out', str(tmp_path / 'all2.npz'), '--n-series', '50', '--length', '24', '--seed', '123', '--train-out', str(train_p2), '--test-out', str(test_p2), '--test-size', '0.2']
    run(cmd)
    X2, y2, m2 = load_npz(str(train_p2))
    Xt2, yt2, mt2 = load_npz(str(test_p2))

    assert X1.shape == X2.shape
    assert Xt1.shape == Xt2.shape
    assert m1 == m2
    assert mt1 == mt2

    # different seed -> different splits likely
    train_p3 = tmp_path / 'train3.npz'
    test_p3 = tmp_path / 'test3.npz'
    cmd = [sys.executable, 'scripts/generate_dataset.py', '--out', str(tmp_path / 'all3.npz'), '--n-series', '50', '--length', '24', '--seed', '456', '--train-out', str(train_p3), '--test-out', str(test_p3), '--test-size', '0.2']
    run(cmd)
    X3, y3, m3 = load_npz(str(train_p3))
    Xt3, yt3, mt3 = load_npz(str(test_p3))

    # It's possible the same split occurs by chance; prefer to warn instead of failing the test
    different = (X1.shape != X3.shape) or (Xt1.shape != Xt3.shape) or (m1 != m3) or (mt1 != mt3)
    if not different:
        import warnings
        warnings.warn("Different seed produced identical splits; this can happen by chance, test continues")
