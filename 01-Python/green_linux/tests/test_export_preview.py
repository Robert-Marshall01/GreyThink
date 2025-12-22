from powerapp.utils.export import get_export_preview
from datetime import datetime, timezone, timedelta


def make_history(now, offsets_minutes):
    return [( (now - timedelta(minutes=o)).isoformat(), 1.0) for o in offsets_minutes]


def test_export_preview_empty():
    msg = get_export_preview([])
    assert '(no samples collected yet' in msg or 'wait a few seconds' in msg


def test_export_preview_limits_and_more():
    now = datetime.now(timezone.utc)
    # create 15 samples, 0..14 minutes ago
    history = make_history(now, list(range(15)))
    # attach example power values
    history = [(t, float(i)) for i, (t, _) in enumerate(history, start=1)]
    p = get_export_preview(history, last_minutes=60, max_items=5)
    lines = p.splitlines()
    assert len(lines) == 6  # 5 shown + '... and 10 more'
    assert lines[-1].startswith('... and ')
    # each shown line should include a value with ' W' suffix
    for ln in lines[:-1]:
        assert ' — ' in ln and ln.endswith(' W')


def test_export_preview_filters_by_minutes():
    now = datetime.now(timezone.utc)
    recent = (now - timedelta(minutes=5)).isoformat()
    old = (now - timedelta(minutes=120)).isoformat()
    history = [(recent, 1.0), (old, 2.0)]
    p = get_export_preview(history, last_minutes=60, max_items=10)
    assert '120' not in p  # old sample excluded
    assert ' W' in p  # sample values present
    assert recent.split('T')[0] in p


def test_export_preview_only_none():
    now = datetime.now(timezone.utc)
    # create 10 recent timestamps with missing power values
    history = [((now - timedelta(minutes=i)).isoformat(), None) for i in range(10)]
    p = get_export_preview(history, last_minutes=60, max_items=10)
    assert '(no valid power measurements' in p
