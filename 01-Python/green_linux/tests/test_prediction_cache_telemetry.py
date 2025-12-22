import joblib
from pathlib import Path

import importlib


# Top-level fake model so it is picklable by joblib
class FakeModel:
    def predict(self, X):
        return [0.0 for _ in range(len(X))]


def test_telemetry_written_when_opted_in(monkeypatch, tmp_path):
    # enable telemetry and set XDG cache to tmp
    monkeypatch.setenv('XDG_CACHE_HOME', str(tmp_path))
    # ensure settings reports telemetry_opt_in=True
    import powerapp.emissions as em
    import powerapp.config as cfgmod

    monkeypatch.setattr(cfgmod, 'load_settings', lambda: {'telemetry_opt_in': True})
    # reset stats and set small interval to force immediate report
    em.reset_prediction_cache_stats()
    monkeypatch.setenv('POWERAPP_PRED_CACHE_TELEMETRY_INTERVAL_SEC', '0')
    importlib.reload(em)

    data = {'model': FakeModel(), 'app_keys': []}
    p = tmp_path / 'fake_model.joblib'
    joblib.dump(data, str(p))

    f = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]

    # call predict to cause cache set and telemetry attempt
    em.predict_best_windows(f, window_hours=2, top_k=1, model_path=str(p), use_model=True)

    # check telemetry file (either XDG path telemetry_events.jsonl or telemetry_upload.jsonl or repo-local fallback)
    tele_dir = Path(str(tmp_path)) / 'powerapp'
    tele_path1 = tele_dir / 'telemetry_events.jsonl'
    tele_path1b = tele_dir / 'telemetry_upload.jsonl'
    tele_path2 = Path(__file__).resolve().parents[1] / 'run-telemetry' / 'telemetry_events.jsonl'
    assert tele_path1.exists() or tele_path1b.exists() or tele_path2.exists()
    # prefer telemetry_upload if present
    chosen = tele_path1b if tele_path1b.exists() else (tele_path1 if tele_path1.exists() else tele_path2)
    content = chosen.read_text().strip().splitlines()
    assert len(content) >= 1
    ev = content[-1]
    assert 'pred_cache_stats' in ev


def test_no_telemetry_when_opted_out(monkeypatch, tmp_path):
    monkeypatch.setenv('XDG_CACHE_HOME', str(tmp_path))
    import powerapp.emissions as em
    import powerapp.config as cfgmod

    monkeypatch.setattr(cfgmod, 'load_settings', lambda: {'telemetry_opt_in': False})
    em.reset_prediction_cache_stats()
    monkeypatch.setenv('POWERAPP_PRED_CACHE_TELEMETRY_INTERVAL_SEC', '0')
    importlib.reload(em)

    data = {'model': FakeModel(), 'app_keys': []}
    p = tmp_path / 'fake_model.joblib'
    joblib.dump(data, str(p))

    f = [{'timestamp': f'2025-12-17T0{i}:00:00+00:00', 'intensity': v} for i, v in enumerate([200,150,100,120,180,160])]

    em.predict_best_windows(f, window_hours=2, top_k=1, model_path=str(p), use_model=True)

    tele_path1 = Path(str(tmp_path)) / 'powerapp' / 'telemetry_events.jsonl'
    tele_path2 = Path(__file__).resolve().parents[1] / 'run-telemetry' / 'telemetry_events.jsonl'
    assert not tele_path1.exists()
    # fallback must also not be created when telemetry is opted out
    assert not tele_path2.exists()
