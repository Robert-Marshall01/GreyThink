from powerapp.utils.export import history_to_csv


def test_history_to_csv():
    history = [
        ("2025-12-15T00:00:00Z", 12.345678),
        ("2025-12-15T00:00:05Z", None),
        ("2025-12-15T00:00:10Z", 6.5),
    ]
    csv = history_to_csv(history)
    lines = csv.strip().splitlines()
    assert lines[0] == 'timestamp,avg_intensity_gco2_kwh,power_w'
    assert lines[1].startswith('2025-12-15T00:00:00Z,,12.345678')
    assert lines[2] == '2025-12-15T00:00:05Z,'  # No trailing comma for None
    assert lines[3].startswith('2025-12-15T00:00:10Z,,6.500000')


def test_history_to_csv_last_minutes():
    from datetime import datetime, timezone, timedelta
    now = datetime.now(timezone.utc)
    recent = now - timedelta(minutes=5)
    old = now - timedelta(minutes=120)
    history = [
        (recent.isoformat(), 10.0),
        (old.isoformat(), 1.0),
    ]
    csv = history_to_csv(history, last_minutes=60)
    lines = csv.strip().splitlines()
    assert len(lines) == 2  # header + one recent sample
    assert lines[1].startswith(recent.isoformat())


def test_history_to_csv_with_intensity():
    history = [
        ("2025-12-15T00:00:00Z", 100.0),
        ("2025-12-15T00:00:05Z", 150.0),
    ]
    csv = history_to_csv(history, avg_intensity=450.5)
    lines = csv.strip().splitlines()
    assert lines[0] == 'timestamp,avg_intensity_gco2_kwh,power_w'
    assert '450.50' in lines[1]
    assert '450.50' in lines[2]
    assert lines[1].startswith('2025-12-15T00:00:00Z,450.50,100.000000')
    assert lines[2].startswith('2025-12-15T00:00:05Z,450.50,150.000000')


def test_history_to_csv_with_timezone_conversion():
    """Test that timestamps are converted from UTC to user's timezone."""
    history = [
        ("2025-12-15T00:00:00+00:00", 100.0),  # UTC midnight
        ("2025-12-15T12:00:00+00:00", 150.0),  # UTC noon
    ]
    # Convert to America/New_York (UTC-5)
    csv = history_to_csv(history, user_zone='America/New_York')
    lines = csv.strip().splitlines()
    assert lines[0] == 'timestamp,avg_intensity_gco2_kwh,power_w'
    # Should be converted to EST/EDT (UTC-5 or UTC-4)
    # 2025-12-15T00:00:00+00:00 -> 2025-12-14T19:00:00-05:00 (EST in December)
    assert '2025-12-14T19:00:00-05:00' in lines[1]
    # 2025-12-15T12:00:00+00:00 -> 2025-12-15T07:00:00-05:00
    assert '2025-12-15T07:00:00-05:00' in lines[2]


def test_history_to_csv_timezone_fallback():
    """Test that invalid timezone doesn't break export."""
    history = [
        ("2025-12-15T00:00:00Z", 100.0),
    ]
    # Invalid timezone should fall back to original timestamps
    csv = history_to_csv(history, user_zone='Invalid/Timezone')
    lines = csv.strip().splitlines()
    assert '2025-12-15T00:00:00Z' in lines[1]

