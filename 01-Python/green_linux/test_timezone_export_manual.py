#!/usr/bin/env python3
"""Manual test to verify timezone conversion in CSV export."""
from datetime import datetime, timezone
from zoneinfo import ZoneInfo
from powerapp.utils.export import history_to_csv

# Create test data with UTC timestamps
history = [
    ("2025-12-22T06:45:02.008966+00:00", 100.5),
    ("2025-12-22T06:45:07.009390+00:00", 105.2),
    ("2025-12-22T06:45:12.009810+00:00", 110.8),
]

print("=" * 80)
print("Testing Timezone Conversion in CSV Export")
print("=" * 80)

print("\nOriginal UTC timestamps:")
for ts, power in history:
    print(f"  {ts} -> {power} W")

print("\n" + "=" * 80)
print("Test 1: Export without timezone (should keep UTC)")
print("=" * 80)
csv_utc = history_to_csv(history, avg_intensity=450.5)
print(csv_utc)

print("\n" + "=" * 80)
print("Test 2: Export with America/New_York timezone")
print("=" * 80)
csv_ny = history_to_csv(history, avg_intensity=450.5, user_zone='America/New_York')
print(csv_ny)

print("\n" + "=" * 80)
print("Test 3: Export with Europe/London timezone")
print("=" * 80)
csv_london = history_to_csv(history, avg_intensity=450.5, user_zone='Europe/London')
print(csv_london)

print("\n" + "=" * 80)
print("Verification")
print("=" * 80)
if '+00:00' in csv_utc:
    print("✓ UTC export contains UTC timestamps (+00:00)")
else:
    print("✗ UTC export missing UTC timezone")

if '-05:00' in csv_ny or '-04:00' in csv_ny:
    print("✓ New York export contains EST/EDT timestamps")
else:
    print("✗ New York export missing correct timezone offset")

if '+00:00' in csv_london or '+01:00' in csv_london:
    print("✓ London export contains GMT/BST timestamps")
else:
    print("✗ London export missing correct timezone offset")

# Check for specific conversions
# 2025-12-22T06:45:02+00:00 (UTC) should be:
# - 2025-12-22T01:45:02-05:00 (EST, December is standard time)
# - 2025-12-22T06:45:02+00:00 (GMT, December is standard time)

utc_dt = datetime.fromisoformat("2025-12-22T06:45:02.008966+00:00")
ny_dt = utc_dt.astimezone(ZoneInfo('America/New_York'))
london_dt = utc_dt.astimezone(ZoneInfo('Europe/London'))

print(f"\nExpected conversions for 2025-12-22T06:45:02.008966+00:00:")
print(f"  New York:  {ny_dt.isoformat()}")
print(f"  London:    {london_dt.isoformat()}")

if ny_dt.isoformat() in csv_ny:
    print("✓ New York conversion is accurate")
else:
    print("✗ New York conversion mismatch")

if london_dt.isoformat() in csv_london:
    print("✓ London conversion is accurate")
else:
    print("✗ London conversion mismatch")

print("\n" + "=" * 80)
print("Summary")
print("=" * 80)
print("The CSV export now converts timestamps from UTC to the user's configured")
print("timezone. When you export from the GUI, the timestamps should be in your")
print("selected timezone, not UTC.")
print("\nTo verify in the GUI:")
print("1. Start the app and ensure a timezone is configured")
print("2. Export a CSV file")
print("3. Open the CSV and check the timestamp column")
print("4. The timestamps should have your timezone offset (e.g., -05:00 for EST)")
print("=" * 80)
