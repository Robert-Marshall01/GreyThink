from powerapp.emissions import fetch_current_intensity, suggest_postponements, suggestions_to_csv


def test_fetch_current_intensity_mock():
    r = fetch_current_intensity()
    assert 'intensity' in r and isinstance(r['intensity'], float)


def test_suggest_postponements_basic():
    tasks = [
        {'id': 't1', 'name': 'System updates', 'urgency': 'low', 'duration_min': 30},
        {'id': 't2', 'name': 'Backup', 'urgency': 'medium', 'duration_min': 120},
        {'id': 't3', 'name': 'Render', 'urgency': 'high', 'duration_min': 60},
    ]
    # Use a high intensity so suggestions are triggered
    suggestions = suggest_postponements(tasks, current_intensity=500.0, threshold=300.0)
    # should include t1 and t2 but not t3
    ids = [s['task_id'] for s in suggestions]
    assert 't1' in ids and 't2' in ids and 't3' not in ids


def test_suggestions_csv():
    tasks = [{'id': 't1', 'name': 'System updates', 'urgency': 'low', 'duration_min': 30}]
    suggestions = suggest_postponements(tasks, current_intensity=500.0, threshold=300.0)
    csv = suggestions_to_csv(suggestions)
    assert 'System updates' in csv
    assert 'estimated_saving_kgCO2_if_postponed' not in csv  # header is normalized
