#!/usr/bin/env python3
"""Debug script to test export preview with various scenarios."""
from datetime import datetime, timezone, timedelta
from powerapp.utils.export import get_export_preview

print("=" * 80)
print("Testing Export Preview Scenarios")
print("=" * 80)

# Scenario 1: Empty history
print("\n1. Empty history:")
print(get_export_preview([]))

# Scenario 2: History with all None values
print("\n2. History with all None power values:")
now = datetime.now(timezone.utc)
history_all_none = [
    (now.isoformat(), None),
    ((now - timedelta(seconds=5)).isoformat(), None),
    ((now - timedelta(seconds=10)).isoformat(), None),
]
print(get_export_preview(history_all_none))

# Scenario 3: History with valid values
print("\n3. History with valid power values:")
history_valid = [
    (now.isoformat(), 100.5),
    ((now - timedelta(seconds=5)).isoformat(), 105.2),
    ((now - timedelta(seconds=10)).isoformat(), 110.8),
]
print(get_export_preview(history_valid))

# Scenario 4: Mixed history (some None, some valid)
print("\n4. Mixed history (some None, some valid):")
history_mixed = [
    (now.isoformat(), 100.5),
    ((now - timedelta(seconds=5)).isoformat(), None),
    ((now - timedelta(seconds=10)).isoformat(), 110.8),
    ((now - timedelta(seconds=15)).isoformat(), None),
    ((now - timedelta(seconds=20)).isoformat(), 95.3),
]
print(get_export_preview(history_mixed))

# Scenario 5: Old samples outside time window
print("\n5. Old samples (60+ minutes ago) with last_minutes=30:")
old_time = now - timedelta(minutes=65)
history_old = [
    (old_time.isoformat(), 100.5),
    ((old_time + timedelta(seconds=5)).isoformat(), 105.2),
]
print(get_export_preview(history_old, last_minutes=30))

# Scenario 6: Recent samples within time window
print("\n6. Recent samples (within 5 minutes) with last_minutes=30:")
recent_time = now - timedelta(minutes=2)
history_recent = [
    (recent_time.isoformat(), 100.5),
    ((recent_time + timedelta(seconds=5)).isoformat(), 105.2),
    ((recent_time + timedelta(seconds=10)).isoformat(), 110.8),
]
print(get_export_preview(history_recent, last_minutes=30))

print("\n" + "=" * 80)
print("Likely causes of 'no valid samples' message:")
print("=" * 80)
print("1. All power values in history are None (sampling is failing)")
print("2. All samples are outside the selected time window")
print("3. History is empty (app hasn't collected any samples yet)")
print("\nTo diagnose:")
print("- Check if the app is showing power readings in the main UI")
print("- Try adjusting the 'Last N minutes' slider to a larger value")
print("- Wait a few seconds for the app to collect more samples")
print("=" * 80)
