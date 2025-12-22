import os
import sys
import subprocess
import pytest
from pathlib import Path

pytest.importorskip('sklearn')
pytest.importorskip('joblib')


def test_script_trains_model_and_writes_file(tmp_path):
    out = tmp_path / 'window_predictor.joblib'
    cmd = [sys.executable, os.path.join('scripts', 'train.py'), '--n-series', '5', '--length', '8', '--out', str(out)]
    env = {**os.environ, 'PYTHONPATH': str(Path.cwd())}
    res = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=env)
    assert res.returncode == 0, f"Train script failed: {res.stderr}"
    assert out.exists()
    # joblib load to verify structure
    import joblib
    data = joblib.load(str(out))
    assert 'model' in data
    assert 'app_keys' in data
