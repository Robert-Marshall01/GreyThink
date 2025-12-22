#!/usr/bin/env python3
"""Verify that CSV export uses the configured timezone."""
import json
import os
from pathlib import Path
from datetime import datetime, timezone
from zoneinfo import ZoneInfo

# Read config
config_path = Path.home() / '.config' / 'powerapp' / 'config.json'
if not config_path.exists():
    print("❌ Config file not found. Run the app first to create it.")
    exit(1)

with open(config_path) as f:
    config = json.load(f)

user_zone = config.get('timezone')
print(f"✓ Config loaded: timezone = {user_zone}")

if not user_zone:
    print("❌ No timezone configured. The timezone setup dialog should have appeared.")
    print("   Please run the app and configure your timezone.")
    exit(1)

# Test the export function
from powerapp.utils.export import history_to_csv

# Create test data with UTC timestamps (simulating what the app collects)
now_utc = datetime.now(timezone.utc)
history = [
    (now_utc.isoformat(), 100.5),
]

print(f"\n✓ Test timestamp (UTC): {now_utc.isoformat()}")

# Convert to user timezone
now_user_tz = now_utc.astimezone(ZoneInfo(user_zone))
print(f"✓ Expected in {user_zone}: {now_user_tz.isoformat()}")

# Export with timezone
csv = history_to_csv(history, user_zone=user_zone)
lines = csv.strip().splitlines()

print(f"\n✓ CSV output:\n{csv}")

# Verify
timestamp_in_csv = lines[1].split(',')[0]
print(f"\n✓ Timestamp in CSV: {timestamp_in_csv}")

if now_user_tz.isoformat() in csv:
    print("✅ SUCCESS: CSV export uses your configured timezone!")
    print(f"   The timezone offset matches {user_zone}")
else:
    print("❌ FAIL: Timezone conversion not working")
    print(f"   Expected: {now_user_tz.isoformat()}")
    print(f"   Got:      {timestamp_in_csv}")
    
# Check if it's still UTC
if '+00:00' in timestamp_in_csv and user_zone != 'UTC':
    print("⚠️  WARNING: Timestamp is still in UTC!")
    print("   This means the export is not converting to your timezone.")
