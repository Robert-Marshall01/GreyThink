from powerapp.ml.heuristics import estimate_total_power, split_app_power


def test_estimate_total_power_increases_with_cpu():
    a = {'chrome': 20.0, 'editor': 10.0}
    t1 = estimate_total_power(a, base_power=3.0)
    a2 = {'chrome': 40.0, 'editor': 10.0}
    t2 = estimate_total_power(a2, base_power=3.0)
    assert t2 > t1


def test_estimate_total_power_handles_edge_cases():
    # negative and >100 values are clamped
    a = {'x': -10.0, 'y': 200.0}
    t = estimate_total_power(a, base_power=2.0)
    assert t >= 2.0


def test_split_app_power_sums_dynamic_portion():
    per_app = {'a': 10.0, 'b': 30.0, 'c': 0.0}
    est = estimate_total_power(per_app, base_power=1.0)
    alloc = split_app_power(est, per_app, base_power=1.0, include_base=False)
    # allocations present and numeric
    assert set(alloc.keys()) == set(per_app.keys())
    s = sum(alloc.values())
    # dynamic portion should approximate est - base_power
    assert abs(s - (est - 1.0)) < 1e-3
    # non-negative allocations
    assert all(v >= 0.0 for v in alloc.values())


def test_split_app_power_with_base_included():
    per_app = {'a': 5.0, 'b': 5.0}
    est = estimate_total_power(per_app, base_power=2.0)
    alloc = split_app_power(est, per_app, base_power=2.0, include_base=True)
    assert '__base__' in alloc and alloc['__base__'] == 2.0
    s = sum(v for k, v in alloc.items() if k != '__base__')
    assert abs(s - (est - 2.0)) < 1e-3


def test_empty_per_app_returns_empty():
    assert split_app_power(5.0, {}, base_power=1.0) == {}
