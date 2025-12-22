from powerapp.ml.dataset import generate_simulated_series


def test_generate_simulated_series_seed_reproducible():
    data1 = generate_simulated_series(n_series=3, length=4, apps=['a','b'], seed=42)
    data2 = generate_simulated_series(n_series=3, length=4, apps=['a','b'], seed=42)
    assert data1 == data2, 'Same seed should produce identical outputs'


def test_generate_simulated_series_seed_differs():
    data1 = generate_simulated_series(n_series=2, length=3, apps=['x','y'], seed=1)
    data2 = generate_simulated_series(n_series=2, length=3, apps=['x','y'], seed=2)
    assert data1 != data2, 'Different seeds should produce different outputs'
