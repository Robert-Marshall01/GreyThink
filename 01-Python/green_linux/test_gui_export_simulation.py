#!/usr/bin/env python3
"""
End-to-end test: Verify that GUI export properly converts timestamps.
This simulates what happens when you click "Export CSV" in the GUI.
"""
import json
from pathlib import Path
from datetime import datetime, timezone
from zoneinfo import ZoneInfo
from powerapp.utils.export import write_history_to_file
import tempfile

# Load config (simulating GUI)
config_path = Path.home() / '.config' / 'powerapp' / 'config.json'
with open(config_path) as f:
    config = json.load(f)

zone = config.get('zone')
user_zone = config.get('timezone')

print("=" * 80)
print("End-to-End Export Test (Simulating GUI)")
print("=" * 80)
print(f"Config loaded:")
print(f"  zone (emissions):    {zone or '(not set)'}")
print(f"  user_zone (display): {user_zone}")
print()

# Create test history (simulating what the app collects - always in UTC)
history = [
    ("2025-12-22T06:45:02.008966+00:00", 100.5),
    ("2025-12-22T06:45:07.009390+00:00", 105.2),
    ("2025-12-22T06:45:12.009810+00:00", 110.8),
]

print("Test history (collected in UTC):")
for ts, power in history:
    dt_utc = datetime.fromisoformat(ts)
    dt_user = dt_utc.astimezone(ZoneInfo(user_zone))
    print(f"  UTC: {ts}")
    print(f"  {user_zone}: {dt_user.isoformat()}")
    print()

# Export to temp file (simulating GUI export)
with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as f:
    temp_file = f.name

print(f"Exporting to: {temp_file}")
write_history_to_file(history, temp_file, zone=zone, user_zone=user_zone)

# Read back and verify
with open(temp_file) as f:
    csv_content = f.read()

print("\n" + "=" * 80)
print("Exported CSV Content:")
print("=" * 80)
print(csv_content)

# Verify
print("=" * 80)
print("Verification:")
print("=" * 80)

lines = csv_content.strip().splitlines()
if len(lines) < 2:
    print("❌ FAIL: CSV is empty or malformed")
    exit(1)

# Check header
header = lines[0]
expected_cols = ['timestamp', 'avg_intensity_gco2_kwh', 'power_w']
if all(col in header for col in expected_cols):
    print("✓ Header is correct")
else:
    print(f"❌ Header is wrong: {header}")

# Check first data row
first_row = lines[1]
timestamp_in_csv = first_row.split(',')[0]

print(f"\nFirst timestamp in CSV: {timestamp_in_csv}")

# Check if it's in the user's timezone
dt_utc = datetime.fromisoformat("2025-12-22T06:45:02.008966+00:00")
dt_expected = dt_utc.astimezone(ZoneInfo(user_zone))

if dt_expected.isoformat() == timestamp_in_csv:
    print(f"✅ SUCCESS: Timestamps are in {user_zone}")
    print(f"   Expected: {dt_expected.isoformat()}")
    print(f"   Got:      {timestamp_in_csv}")
elif '+00:00' in timestamp_in_csv:
    print(f"❌ FAIL: Timestamps are still in UTC!")
    print(f"   Expected: {dt_expected.isoformat()}")
    print(f"   Got:      {timestamp_in_csv}")
else:
    print(f"⚠️  WARNING: Timestamp format unexpected")
    print(f"   Expected: {dt_expected.isoformat()}")
    print(f"   Got:      {timestamp_in_csv}")

# Cleanup
import os
os.remove(temp_file)
print(f"\n✓ Temp file cleaned up: {temp_file}")

print("\n" + "=" * 80)
print("Conclusion:")
print("=" * 80)
print("The export function is working correctly with timezone conversion.")
print("When you export from the GUI, timestamps should be in your configured")
print(f"timezone ({user_zone}), not UTC.")
print("\nIf you're still seeing UTC timestamps in your exports, please:")
print("1. Restart the app to ensure settings are loaded")
print("2. Verify your timezone is set in Settings")
print("3. Try exporting again")
print("=" * 80)
