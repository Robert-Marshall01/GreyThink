from powerapp.ml.dataset import generate_simulated_series, extract_window_examples


def test_extract_window_examples_basic():
    data = generate_simulated_series(n_series=2, length=6, apps=['a','b'])
    X, y, app_keys = extract_window_examples(data, window_hours=2)
    assert isinstance(X, list)
    assert isinstance(y, list)
    assert len(X) > 0
    # each feature vector should have 3 + len(app_keys) entries
    assert all(len(x) == 3 + len(app_keys) for x in X)


def test_labels_are_avg_forecast():
    data = generate_simulated_series(n_series=1, length=4, apps=['x','y'])
    X, y, _ = extract_window_examples(data, window_hours=2)
    # reconstruct avg_forecast from features (second element)
    for feat, lab in zip(X, y):
        assert abs(feat[1] - lab) < 1e-6
