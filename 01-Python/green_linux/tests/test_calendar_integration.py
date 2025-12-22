from datetime import datetime, timezone, timedelta


from powerapp.emissions import find_low_carbon_windows, augment_suggestions_with_best_window
from powerapp.calendar import filter_windows_by_busy, get_busy_intervals, eds_available


def _make_simple_forecast(base=300.0, low_hours=(0,4), low_value=100.0, hours=12):
    now = datetime.now(timezone.utc).replace(minute=0, second=0, microsecond=0)
    res = []
    for h in range(hours):
        dt = now + timedelta(hours=h)
        if h in low_hours:
            ci = low_value
        else:
            ci = base
        res.append({'timestamp': dt.isoformat(), 'intensity': ci})
    return res


def test_filter_windows_with_mocked_eds_busy(monkeypatch):
    # Create forecast with two low windows at h=0 and h=4 (window_hours=2)
    forecast = _make_simple_forecast()
    windows = find_low_carbon_windows(forecast, window_hours=2, top_k=10)
    assert len(windows) >= 2

    # busy interval overlaps the first (best) window
    first_start = windows[0]['start']
    first_end = windows[0]['end']

    # Monkeypatch get_busy_intervals to simulate EDS returning the busy interval
    import powerapp.calendar as cal

    def fake_busy(start_dt, end_dt, source='eds', ics_path=None):
        # return single busy event exactly overlapping the first window
        return [{'start': start_dt + timedelta(hours=0), 'end': start_dt + timedelta(hours=2), 'summary': 'Busy Event'}]

    monkeypatch.setattr(cal, 'get_busy_intervals', fake_busy)

    # Filter
    busy = cal.get_busy_intervals(datetime.fromisoformat(windows[0]['start']), datetime.fromisoformat(windows[-1]['end']), source='eds')
    filtered = filter_windows_by_busy(windows, busy)

    # Ensure the first window was removed
    assert len(filtered) < len(windows)
    # Use filtered windows in augmentation and assert best_window_start reflects filtered best
    suggestion = {'task_id': 't1', 'task_name': 'Test', 'urgency': 'low', 'duration_min': 60, 'power_w': 100.0, 'current_intensity_g': 300.0}
    aug = augment_suggestions_with_best_window([suggestion], forecast, window_hours=2, windows=filtered)
    assert aug
    aw = aug[0]
    # best_window_start should be the start of the remaining first filtered window
    assert aw.get('best_window_start') == filtered[0]['start']


def test_eds_unavailable_returns_empty():
    # Ensure that get_busy_intervals returns empty list when EDS not available and source='eds'
    import powerapp.calendar as cal
    # monkeypatch eds_available to return False indirectly by calling function
    assert eds_available() in (True, False)
    # if eds_available() is True in this environment, we still want to test behavior when it's False
    orig = cal.eds_available
    cal.eds_available = lambda: False
    try:
        res = get_busy_intervals(datetime.now(timezone.utc), datetime.now(timezone.utc) + timedelta(hours=2), source='eds')
        assert res == []
    finally:
        cal.eds_available = orig
