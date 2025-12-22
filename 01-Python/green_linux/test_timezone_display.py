#!/usr/bin/env python3
"""Test timezone display formatting."""
from datetime import datetime, timezone
from zoneinfo import ZoneInfo

def format_ts_for_display(ts_str, user_tz='America/Denver'):
    """Format a timestamp string for display in the user's timezone."""
    try:
        # Parse the timestamp
        dt = datetime.fromisoformat(ts_str)
        # Convert to user timezone
        user_zone = ZoneInfo(user_tz)
        local_dt = dt.astimezone(user_zone)
        # Format as local time
        return local_dt.strftime('%Y-%m-%d %H:%M:%S %Z')
    except Exception as e:
        print(f"Error formatting {ts_str}: {e}")
        return ts_str

# Test
utc_time = '2025-12-23T01:00:00+00:00'
formatted = format_ts_for_display(utc_time)
print(f"UTC time: {utc_time}")
print(f"Formatted (Denver): {formatted}")
print()
print("Expected: 2025-12-22 18:00:00 MST (or MDT depending on DST)")
