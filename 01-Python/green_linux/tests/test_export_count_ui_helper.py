from powerapp.utils.export import count_history_samples
from datetime import datetime, timezone, timedelta


def test_count_history_samples_no_filter():
    history = [
        ('2025-12-15T00:00:00Z', 1.0),
        ('2025-12-15T00:01:00Z', 2.0),
    ]
    assert count_history_samples(history) == 2


def test_count_history_samples_with_filter():
    now = datetime.now(timezone.utc)
    recent = (now - timedelta(minutes=5)).isoformat()
    old = (now - timedelta(minutes=120)).isoformat()
    history = [
        (recent, 10.0),
        (old, 1.0),
    ]
    assert count_history_samples(history, last_minutes=60) == 1


def test_count_history_samples_ignores_none():
    now = datetime.now(timezone.utc)
    recent = (now - timedelta(minutes=5)).isoformat()
    recent2 = (now - timedelta(minutes=10)).isoformat()
    history = [
        (recent, None),
        (recent2, 5.0),
    ]
    assert count_history_samples(history, last_minutes=60) == 1
