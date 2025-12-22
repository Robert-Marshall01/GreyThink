from powerapp.ml.dataset import generate_simulated_series


def test_generate_simulated_series_basic():
    data = generate_simulated_series(n_series=3, length=6, apps=['a','b'])
    assert isinstance(data, list)
    assert len(data) == 3
    for item in data:
        assert 'series' in item and 'meta' in item
        assert len(item['series']) == 6
        for s in item['series']:
            assert 'timestamp' in s and 'per_app_cpu' in s and 'total_power_w' in s
            assert set(s['per_app_cpu'].keys()) == {'a','b'}
            assert isinstance(s['total_power_w'], float)


def test_generate_values_reasonable():
    data = generate_simulated_series(n_series=1, length=4, apps=['x','y'], base_power=2.0)
    s = data[0]['series'][0]
    # CPU per app <= 100 each and total power sensible
    assert all(0.0 <= v <= 100.0 for v in s['per_app_cpu'].values())
    assert s['total_power_w'] >= 2.0
