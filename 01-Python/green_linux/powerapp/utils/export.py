"""Export utilities for PowerApp

Provides helpers to serialize a history of (timestamp, power) samples to CSV.
"""
from typing import Iterable, Tuple, Optional
import io
from datetime import datetime, timezone, timedelta


def _parse_timestamp(ts) -> Optional[datetime]:
    if ts is None:
        return None
    if isinstance(ts, datetime):
        return ts
    s = ts
    # handle trailing Z
    if isinstance(s, str) and s.endswith('Z'):
        s = s[:-1] + '+00:00'
    try:
        return datetime.fromisoformat(s)
    except Exception:
        return None


def history_to_csv(history: Iterable[Tuple[str, float]], last_minutes: Optional[int] = None, avg_intensity: Optional[float] = None, user_zone: Optional[str] = None) -> str:
    """Convert history (iterable of (timestamp, power_or_none)) to CSV string.

    If last_minutes is provided, only include samples within the last N minutes from now (UTC).
    If avg_intensity is provided, include it as a third column (g/kWh) for each row.
    If user_zone is provided, convert timestamps from UTC to that timezone.

    Rows: timestamp,avg_intensity_gco2_kwh,power_w
    - power is empty when None
    - avg_intensity is empty when not provided
    """
    cutoff = None
    if last_minutes is not None:
        cutoff = datetime.now(timezone.utc) - timedelta(minutes=int(last_minutes))

    # Get user's timezone for conversion
    tz = None
    if user_zone:
        try:
            from zoneinfo import ZoneInfo
            tz = ZoneInfo(user_zone)
        except Exception:
            tz = None

    out = io.StringIO()
    out.write('timestamp,avg_intensity_gco2_kwh,power_w\n')
    for ts, p in history:
        dt = _parse_timestamp(ts)
        if cutoff and (dt is None or dt < cutoff):
            continue
        
        # Convert timestamp to user's timezone if available
        ts_out = ts
        if tz and dt:
            try:
                # Ensure dt has timezone info
                if dt.tzinfo is None:
                    dt = dt.replace(tzinfo=timezone.utc)
                # Convert to user's timezone
                dt_local = dt.astimezone(tz)
                ts_out = dt_local.isoformat()
            except Exception:
                # If conversion fails, use original timestamp
                ts_out = ts
        
        intensity_str = f'{avg_intensity:.2f}' if avg_intensity is not None else ''
        if p is None:
            out.write(f'{ts_out},{intensity_str}\n')
        else:
            out.write(f'{ts_out},{intensity_str},{p:.6f}\n')
    return out.getvalue()


def write_history_to_file(
    history: Iterable[Tuple[str, float]],
    path: str,
    last_minutes: Optional[int] = None,
    zone: Optional[str] = None,
    user_zone: Optional[str] = None
) -> None:
    """Write history to CSV file, including average intensity.

    Always includes carbon intensity column. If zone is provided, attempts to
    fetch from the configured provider; otherwise falls back to mock provider.
    If user_zone is provided, timestamps are converted to that timezone.
    """
    avg_intensity = None
    try:
        from powerapp.emissions import fetch_current_intensity
        result = fetch_current_intensity(zone=zone)
        if result and 'intensity' in result:
            avg_intensity = result['intensity']
    except Exception:
        # If fetch fails, just omit intensity value (column still present)
        pass

    csv = history_to_csv(
        history,
        last_minutes=last_minutes,
        avg_intensity=avg_intensity,
        user_zone=user_zone
    )
    with open(path, 'w', encoding='utf-8') as fh:
        fh.write(csv)


def generate_suggested_filename(last_minutes: Optional[int] = None, sample_count: Optional[int] = None) -> str:
    """Create a timestamped filename.

    If last_minutes is provided, include it as '-last{N}min'.
    If sample_count is provided, append '-{N}samples' before the .csv suffix.
    """
    now = datetime.now(timezone.utc)
    base = now.strftime('power-history-%Y%m%d-%H%M%S')
    if last_minutes is not None:
        base = f"{base}-last{int(last_minutes)}min"
    if sample_count is not None:
        base = f"{base}-{int(sample_count)}samples"
    return f"{base}.csv"


def count_history_samples(history: Iterable[Tuple[str, float]], last_minutes: Optional[int] = None) -> int:
    """Return the number of *valid* samples (with power values) that would be exported given an optional last_minutes filter.

    History is an iterable of (timestamp, power_or_none) where timestamp is ISO string or datetime.
    Samples with timestamps that cannot be parsed are ignored when filtering by minutes. Samples with
    ``power`` equal to ``None`` are considered invalid and are not counted.
    """
    if last_minutes is None:
        return sum(1 for _, p in history if p is not None)
    from datetime import datetime, timezone, timedelta
    cutoff = datetime.now(timezone.utc) - timedelta(minutes=int(last_minutes))
    cnt = 0
    for ts, p in history:
        dt = _parse_timestamp(ts)
        if dt and dt >= cutoff and p is not None:
            cnt += 1
    return cnt


def get_export_preview(history: Iterable[Tuple[str, float]], last_minutes: Optional[int] = None, max_items: int = 10, user_zone: Optional[str] = None) -> str:
    """Return a short preview string listing sample timestamps and values that would be exported.

    Shows up to `max_items` most recent samples (newest last) formatted as:
      2025-12-15T00:00:00Z — 12.34 W
    If a sample has no value, it shows '—' in place of the number. If more samples exist,
    appends a "... and N more" line.
    If user_zone is provided, converts timestamps to that timezone.
    """
    from datetime import datetime, timezone, timedelta
    cutoff = None
    if last_minutes is not None:
        cutoff = datetime.now(timezone.utc) - timedelta(minutes=int(last_minutes))

    # Get user's timezone for conversion
    tz = None
    if user_zone:
        try:
            from zoneinfo import ZoneInfo
            tz = ZoneInfo(user_zone)
        except Exception:
            tz = None

    # Collect eligible items as (dt, ts_str, power)
    items = []
    for ts, p in history:
        dt = _parse_timestamp(ts)
        if dt is None:
            continue
        if cutoff and dt < cutoff:
            continue
        
        # Convert timestamp to user's timezone if available
        ts_str = ts if isinstance(ts, str) else dt.isoformat()
        if tz and dt:
            try:
                # Ensure dt has timezone info
                if dt.tzinfo is None:
                    dt_with_tz = dt.replace(tzinfo=timezone.utc)
                else:
                    dt_with_tz = dt
                # Convert to user's timezone
                dt_local = dt_with_tz.astimezone(tz)
                ts_str = dt_local.isoformat()
            except Exception:
                # If conversion fails, use original timestamp
                pass
        
        items.append((dt, ts_str, p))

    # sort by datetime ascending
    items.sort(key=lambda x: x[0])
    total = len(items)
    if total == 0:
        # Check if history itself is empty or if time filter is too restrictive
        if not list(history):
            return (
                '(no samples collected yet - '
                'wait a few seconds for app to collect data)'
            )
        else:
            return (
                '(no samples in selected time window - '
                'try increasing "Last N minutes")'
            )

    # If there are no valid power values in the selected window, show a helpful message
    valid_count = sum(1 for (_, _, p) in items if p is not None)
    if valid_count == 0:
        return '(no valid power measurements - check if power sampling is working)'

    # keep the last `max_items` samples (most recent last)
    shown = items[-max_items:]
    lines = []
    for (_, ts_str, p) in shown:
        if p is None:
            lines.append(f'{ts_str} — —')
        else:
            try:
                lines.append(f'{ts_str} — {float(p):.2f} W')
            except Exception:
                lines.append(f'{ts_str} — {p}')
    if total > len(lines):
        lines.append(f'... and {total - len(lines)} more')
    return '\n'.join(lines)

