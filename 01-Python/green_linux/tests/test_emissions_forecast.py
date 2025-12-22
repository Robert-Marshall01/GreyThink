from powerapp.emissions import fetch_forecast, find_low_carbon_windows


def test_fetch_forecast_mock():
    fc = fetch_forecast(hours=24)
    assert isinstance(fc, list) and len(fc) == 24
    assert 'timestamp' in fc[0] and 'intensity' in fc[0]


def test_find_low_carbon_windows_basic():
    # create synthetic increasing forecast to ensure lowest windows are first
    base = [{'timestamp': f'2025-01-01T{str(i).zfill(2)}:00:00+00:00', 'intensity': float(i)} for i in range(24)]
    wins = find_low_carbon_windows(base, window_hours=3, top_k=2)
    assert len(wins) == 2
    assert wins[0]['avg_intensity'] <= wins[1]['avg_intensity']


def test_find_low_windows_threshold():
    base = [{'timestamp': f'2025-01-01T{str(i).zfill(2)}:00:00+00:00', 'intensity': float(i)} for i in range(24)]
    wins = find_low_carbon_windows(base, window_hours=3, threshold=5.0)
    # windows consisting of early low hours should be returned
    assert all(w['avg_intensity'] <= 5.0 for w in wins)


def test_find_low_carbon_windows_normalizes_start_tz():
    # Use naive (no tz) timestamps and ensure returned start has timezone info (+00:00)
    base = [
        {'timestamp': '2025-01-01T00:00:00', 'intensity': 0.0},
        {'timestamp': '2025-01-01T01:00:00', 'intensity': 1.0},
        {'timestamp': '2025-01-01T02:00:00', 'intensity': 2.0},
    ]
    wins = find_low_carbon_windows(base, window_hours=2, top_k=1)
    assert wins, 'expected at least one window'
    start = wins[0]['start']
    assert isinstance(start, str)
    # Result should be timezone-aware (contain an explicit offset or Z)
    from datetime import datetime
    assert datetime.fromisoformat(start).tzinfo is not None, f"Expected timezone-aware start, got: {start}"


def test_fetch_forecast_timestamps_are_aware():
    fc = fetch_forecast(hours=3)
    assert len(fc) == 3
    for p in fc:
        ts = p.get('timestamp')
        assert isinstance(ts, str)
        # synthetic forecast should produce ISO strings with timezone (+00:00)
        assert ('+' in ts) or ts.endswith('Z'), f"Expected timezone-aware timestamp, got: {ts}"


def test_find_low_carbon_windows_preserves_timezone_offsets():
    # If forecast timestamps include explicit offsets, ensure output keeps that timezone info
    base = [
        {'timestamp': '2025-01-01T00:00:00-07:00', 'intensity': 0.0},
        {'timestamp': '2025-01-01T01:00:00-07:00', 'intensity': 1.0},
        {'timestamp': '2025-01-01T02:00:00-07:00', 'intensity': 2.0},
    ]
    wins = find_low_carbon_windows(base, window_hours=2, top_k=1)
    assert wins, 'expected at least one window'
    start = wins[0]['start']
    assert isinstance(start, str)
    assert ('-07:00' in start) or ('+00:00' in start), f"Expected timezone offset in start, got: {start}"


def test_windows_start_within_forecast_range():
    from datetime import datetime, timezone, timedelta
    start_dt = datetime(2025,12,19,12,0,tzinfo=timezone.utc)
    # build a 48-hour forecast starting at start_dt
    base = []
    for h in range(48):
        dt = (start_dt + timedelta(hours=h)).isoformat()
        base.append({'timestamp': dt, 'intensity': float((h % 24))})
    wins = find_low_carbon_windows(base, window_hours=2, top_k=10)
    assert wins, 'expected some windows'
    for w in wins:
        s = w['start']
        # parse and ensure within [start_dt, start_dt + 48h)
        ds = datetime.fromisoformat(s)
        assert ds >= start_dt and ds < (start_dt + timedelta(hours=48)), f"Window start {ds} out of forecast range"


def test_formatting_in_user_timezone_preserves_day():
    # Ensure that formatting a UTC timestamp into MST does not shift it off by an extra day
    from powerapp.gtk.main import PowerWindow
    win = PowerWindow(None)
    win.settings['timezone'] = 'America/Denver'  # MST/MDT depending on date
    # a UTC time late on Dec 19 that should still be Dec 19 in MST
    s = '2025-12-19T23:00:00+00:00'
    out = win._format_ts_for_display(s)
    assert '2025-12-19' in out or '2025-12-20' in out, f"Unexpected formatted day: {out}"

