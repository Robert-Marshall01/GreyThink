from datetime import datetime, timezone, timedelta
from powerapp.emissions import compute_emissions_from_history


def iso(dt):
    return dt.replace(tzinfo=timezone.utc).isoformat()


def test_compute_emissions_simple_constant():
    t0 = datetime(2025,1,1,0,0,0, tzinfo=timezone.utc)
    # two samples 1 hour apart, both 100W
    h = [ (iso(t0), 100.0), (iso(t0 + timedelta(hours=1)), 100.0) ]
    res = compute_emissions_from_history(h)
    # energy = avg power 100W = 0.1 kW * 1h = 0.1 kWh
    assert abs(res['kwh'] - 0.1) < 1e-9
    assert abs(res['duration_h'] - 1.0) < 1e-9
    # sample_count should be 2
    assert res['sample_count'] == 2


def test_compute_emissions_with_intensity():
    t0 = datetime(2025,1,1,0,0,0, tzinfo=timezone.utc)
    h = [ (iso(t0), 100.0), (iso(t0 + timedelta(hours=1)), 100.0) ]
    res = compute_emissions_from_history(h, intensity=200.0)
    # kwh 0.1, intensity 200 g/kWh -> 0.02 kg
    assert abs(res['kg_co2'] - 0.02) < 1e-9


def test_compute_emissions_with_intensity_series():
    t0 = datetime(2025,1,1,0,0,0, tzinfo=timezone.utc)
    h = [ (iso(t0), 100.0), (iso(t0 + timedelta(hours=1)), 100.0) ]
    series = [ {'timestamp': iso(t0), 'intensity': 100.0}, {'timestamp': iso(t0 + timedelta(hours=1)), 'intensity': 200.0} ]
    res = compute_emissions_from_history(h, intensity_series=series)
    # energy 0.1 kWh, intensity at start 100 g -> 0.01 kg
    assert abs(res['kg_co2'] - 0.01) < 1e-9


def test_compute_emissions_with_no_valid_samples():
    t0 = datetime(2025,1,1,0,0,0, tzinfo=timezone.utc)
    # 40 samples but all have missing power -> should report zero energy and zero valid samples
    h = [ (iso(t0 + timedelta(seconds=i)), None) for i in range(40) ]
    res = compute_emissions_from_history(h)
    assert res['sample_count'] == 0
    assert res['kwh'] == 0.0
    assert res['duration_h'] == 0.0


def test_compute_emissions_interleaved_none():
    t0 = datetime(2025,1,1,0,0,0, tzinfo=timezone.utc)
    # interleaved missing samples but two valid ones 1s and 3s -> duration 2s
    h = [
        (iso(t0), None),
        (iso(t0 + timedelta(seconds=1)), 100.0),
        (iso(t0 + timedelta(seconds=2)), None),
        (iso(t0 + timedelta(seconds=3)), 100.0),
    ]
    res = compute_emissions_from_history(h)
    assert res['sample_count'] == 2
    assert abs(res['duration_h'] - (2.0/3600.0)) < 1e-9
    expected_kwh = (100.0 * (2.0/3600.0)) / 1000.0
    assert abs(res['kwh'] - expected_kwh) < 1e-12
