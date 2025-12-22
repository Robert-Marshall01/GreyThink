import subprocess
import sys
from pathlib import Path


def test_evaluate_model_runs_and_produces_metrics(tmp_path):
    out_model = tmp_path / 'model.joblib'
    out_report = tmp_path / 'report.json'
    cmd = [sys.executable, 'scripts/evaluate_model.py', '--n-series', '30', '--length', '24', '--window-hours', '2', '--n-estimators', '5', '--model-out', str(out_model), '--report-out', str(out_report)]
    env = {
        **dict()
    }
    env = {**env, **{'PYTHONPATH': str(Path.cwd())}}
    subprocess.check_call(cmd, env=env)
    assert out_model.exists()
    assert out_report.exists()
    import json
    r = json.loads(out_report.read_text())
    assert 'metrics' in r
    assert 'ndcg_model' in r['metrics']
