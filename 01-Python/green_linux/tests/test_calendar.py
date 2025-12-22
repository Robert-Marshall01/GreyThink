from datetime import datetime, timezone, timedelta
import os

from powerapp.calendar import _parse_ics_file, get_busy_intervals, filter_windows_by_busy


def _make_ics(tmpdir, start_dt):
    # create a simple ICS with an event at start_dt->start_dt+1h
    path = os.path.join(tmpdir, 'test.ics')
    dt1 = start_dt.isoformat()
    dt2 = (start_dt + timedelta(hours=1)).isoformat()
    content = f"""BEGIN:VCALENDAR
VERSION:2.0
BEGIN:VEVENT
DTSTART:{dt1}
DTEND:{dt2}
SUMMARY:Busy
END:VEVENT
END:VCALENDAR
"""
    with open(path, 'w', encoding='utf-8') as fh:
        fh.write(content)
    return path


def test_parse_ics_and_filter(tmp_path):
    start = datetime.now(timezone.utc).replace(minute=0, second=0, microsecond=0)
    ics = _make_ics(str(tmp_path), start + timedelta(hours=2))
    busy = _parse_ics_file(ics, start, start + timedelta(hours=6))
    assert len(busy) == 1
    # now build windows and filter
    windows = [
        {'start': (start + timedelta(hours=1)).isoformat(), 'end': (start + timedelta(hours=2)).isoformat()},
        {'start': (start + timedelta(hours=2)).isoformat(), 'end': (start + timedelta(hours=3)).isoformat()},
        {'start': (start + timedelta(hours=4)).isoformat(), 'end': (start + timedelta(hours=5)).isoformat()},
    ]
    filtered = filter_windows_by_busy(windows, busy)
    # middle window overlaps busy and should be removed
    assert len(filtered) == 2
    assert all(w['start'] != windows[1]['start'] for w in filtered)


def test_filter_with_z_suffix():
    # windows provided as RFC3339 'Z' terminated strings should be parsed correctly
    start = datetime.now(timezone.utc).replace(minute=0, second=0, microsecond=0)
    windows = [
        {'start': (start + timedelta(hours=1)).isoformat().replace('+00:00', 'Z'), 'end': (start + timedelta(hours=2)).isoformat().replace('+00:00', 'Z')},
        {'start': (start + timedelta(hours=2)).isoformat().replace('+00:00', 'Z'), 'end': (start + timedelta(hours=3)).isoformat().replace('+00:00', 'Z')},
    ]
    busy = [{'start': start + timedelta(hours=1, minutes=30), 'end': start + timedelta(hours=1, minutes=45), 'summary': 'Busy'}]
    filtered = filter_windows_by_busy(windows, busy)
    # first window overlaps busy and should be removed
    assert len(filtered) == 1
    assert filtered[0]['start'] == windows[1]['start']


def test_get_busy_intervals_ics_path(tmp_path):
    # ensure get_busy_intervals with explicit ics_path finds events
    start = datetime.now(timezone.utc).replace(minute=0, second=0, microsecond=0)
    ics = _make_ics(str(tmp_path), start + timedelta(hours=1))
    res = get_busy_intervals(start, start + timedelta(hours=6), source='ics', ics_path=str(tmp_path))
    assert len(res) == 1
