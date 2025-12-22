"""Calendar helpers: EDS + ICS free/busy support (MVP).

Provides a small, testable API to read calendar busy intervals from:
- Evolution Data Server (EDS) when available (best-effort)
- Local ICS files (search in XDG_CONFIG_HOME/calendar, ~/.calendar or provided path)

Exports:
- eds_available() -> bool
- get_busy_intervals(start_dt, end_dt, source='eds'|'ics', ics_path=None) -> list of {'start': dt, 'end': dt, 'summary': str}
- filter_windows_by_busy(windows, busy_intervals) -> list of windows without overlap
"""
from datetime import datetime, timezone
import os
from typing import List, Dict, Optional


def eds_available() -> bool:
    try:
        import gi
        gi.require_version('EDataServer', '1.2')
        return True
    except Exception:
        return False


def _parse_iso(dt_str: str) -> Optional[datetime]:
    try:
        # Be tolerant of RFC3339 'Z' suffix which datetime.fromisoformat doesn't accept
        if dt_str and isinstance(dt_str, str) and dt_str.endswith('Z'):
            dt_str = dt_str.replace('Z', '+00:00')
        dt = datetime.fromisoformat(dt_str)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        return dt
    except Exception:
        return None


def _scan_ics_paths(ics_path: Optional[str] = None) -> List[str]:
    paths = []
    if ics_path:
        # single file or directory
        if os.path.isdir(ics_path):
            for fn in os.listdir(ics_path):
                if fn.lower().endswith('.ics'):
                    paths.append(os.path.join(ics_path, fn))
        elif os.path.isfile(ics_path):
            paths.append(ics_path)
        return paths

    # default locations
    xdg = os.environ.get('XDG_CONFIG_HOME')
    if xdg:
        p = os.path.join(xdg, 'calendar')
        if os.path.isdir(p):
            for fn in os.listdir(p):
                if fn.lower().endswith('.ics'):
                    paths.append(os.path.join(p, fn))
    home = os.path.expanduser('~')
    p2 = os.path.join(home, '.calendar')
    if os.path.isdir(p2):
        for fn in os.listdir(p2):
            if fn.lower().endswith('.ics'):
                paths.append(os.path.join(p2, fn))
    return paths


def _parse_ics_file(path: str, start: datetime, end: datetime) -> List[Dict]:
    # Very lightweight ICS parsing: handle VEVENT with DTSTART/DTEND and SUMMARY
    res = []
    try:
        with open(path, 'r', encoding='utf-8') as fh:
            lines = [l.rstrip('\n') for l in fh]
    except Exception:
        return res

    in_event = False
    cur = {}
    for line in lines:
        if line.strip() == 'BEGIN:VEVENT':
            in_event = True
            cur = {}
            continue
        if line.strip() == 'END:VEVENT':
            in_event = False
            # evaluate
            dt1 = _parse_iso(cur.get('DTSTART') or cur.get('DTSTART;VALUE=DATE-TIME') or '')
            dt2 = _parse_iso(cur.get('DTEND') or cur.get('DTEND;VALUE=DATE-TIME') or '')
            if dt1 and dt2 and not (dt2 <= start or dt1 >= end):
                res.append({'start': dt1, 'end': dt2, 'summary': cur.get('SUMMARY', '')})
            cur = {}
            continue
        if not in_event:
            continue
        # key:value
        if ':' in line:
            k, v = line.split(':', 1)
            k = k.strip()
            v = v.strip()
            cur[k] = v
    return res


def get_busy_intervals(start: datetime, end: datetime, source: str = 'eds', ics_path: Optional[str] = None) -> List[Dict]:
    """Return busy intervals between start and end.

    source: 'eds' to attempt EDS (best-effort), 'ics' to scan local ICS files.
    If EDS is unavailable or fails, returns an empty list.
    """
    if source == 'eds':
        # best-effort EDS support; return empty list when unavailable
        if not eds_available():
            return []
        try:
            # Attempt to query EDS free/busy - best-effort and non-blocking approach
            import gi
            gi.require_version('EDataServer', '1.2')
            # A full EDS integration requires a running EDS and proper account discovery.
            # For MVP we do a best-effort and return empty if we can't perform a query.
            return []
        except Exception:
            return []
    else:
        items = []
        for p in _scan_ics_paths(ics_path):
            items.extend(_parse_ics_file(p, start, end))
        return items


def filter_windows_by_busy(windows: List[Dict], busy: List[Dict]) -> List[Dict]:
    """Return windows that do not overlap any busy interval.

    windows: list of {'start': iso or datetime, 'end': iso or datetime, ...}
    busy: list of {'start': datetime, 'end': datetime, 'summary': str}
    """
    print(f"DEBUG_FILTER: windows={windows!r} busy={busy!r}", flush=True)
    out = []
    # normalize busy to tuples
    b_intervals = []
    for b in busy:
        s = b.get('start')
        e = b.get('end')
        if isinstance(s, str):
            s = _parse_iso(s)
        if isinstance(e, str):
            e = _parse_iso(e)
        if s and e:
            b_intervals.append((s, e))
    for w in windows:
        ws = w.get('start')
        we = w.get('end')
        if isinstance(ws, str):
            ws = _parse_iso(ws)
        if isinstance(we, str):
            we = _parse_iso(we)
        if not ws or not we:
            continue
        conflict = False
        for bs, be in b_intervals:
            # overlap if ws < be and we > bs
            if ws < be and we > bs:
                conflict = True
                break
        if not conflict:
            out.append(w)
    print(f"DEBUG_FILTER_RESULT: out_len={len(out)} b_intervals_len={len(b_intervals)}", flush=True)
    return out
