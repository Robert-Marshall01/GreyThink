# CSV Export Timezone Fix - Summary

## Problem
CSV exports from the GUI were showing timestamps in UTC (`+00:00`) instead of the user's configured timezone, despite the timezone being set correctly in the app settings.

## Root Cause
The export functions in `powerapp/utils/export.py` were not converting timestamps from UTC (how they're stored internally) to the user's configured timezone before writing to CSV.

## Solution Implemented

### 1. Updated Export Functions (`powerapp/utils/export.py`)
- Added `user_zone` parameter to `history_to_csv()` function
- Added `user_zone` parameter to `write_history_to_file()` function
- Implemented timezone conversion logic that:
  - Parses timestamps as UTC
  - Converts to user's timezone using `ZoneInfo`
  - Handles errors gracefully (falls back to original timestamp if conversion fails)

### 2. Updated GUI Export Handler (`powerapp/gtk/main.py`)
- Modified `_on_export()` method to pass user's timezone to export functions
- Loads timezone from settings: `self.settings.get('timezone')`
- Passes it to `write_history_to_file()` as `user_zone` parameter

### 3. Added Comprehensive Tests (`tests/test_export.py`)
- `test_history_to_csv_with_timezone_conversion`: Tests conversion to America/New_York
- `test_history_to_csv_timezone_fallback`: Tests graceful handling of invalid timezones

## Verification

All tests pass:
```
tests/test_export.py::test_history_to_csv PASSED
tests/test_export.py::test_history_to_csv_last_minutes PASSED
tests/test_export.py::test_history_to_csv_with_intensity PASSED
tests/test_export.py::test_history_to_csv_with_timezone_conversion PASSED
tests/test_export.py::test_history_to_csv_timezone_fallback PASSED
```

### Manual Verification Scripts
Created three verification scripts to test the fix:

1. **test_timezone_export_manual.py**: Tests timezone conversion with different timezones
2. **verify_export_timezone.py**: Verifies config and export use correct timezone
3. **test_gui_export_simulation.py**: End-to-end simulation of GUI export flow

All scripts confirm that timestamps are correctly converted to the user's timezone.

## Example

**Before Fix:**
```csv
timestamp,avg_intensity_gco2_kwh,power_w
2025-12-22T06:45:02.008966+00:00,350.00,100.500000
```
(Always UTC regardless of user's timezone setting)

**After Fix (with America/Denver timezone):**
```csv
timestamp,avg_intensity_gco2_kwh,power_w
2025-12-21T23:45:02.008966-07:00,350.00,100.500000
```
(Correctly converted to user's timezone)

## How It Works

1. App collects power samples with UTC timestamps internally
2. User configures timezone in Settings (or via first-launch dialog)
3. When exporting CSV:
   - GUI loads timezone from config: `self.settings.get('timezone')`
   - Passes it to `write_history_to_file(... user_zone=user_zone)`
   - Export function converts each timestamp:
     - Parse as UTC
     - Convert to user's timezone using `ZoneInfo`
     - Write converted timestamp to CSV

## Testing

To verify the fix is working in your installation:

1. **Check your timezone is set:**
   ```bash
   cat ~/.config/powerapp/config.json | grep timezone
   ```

2. **Run verification script:**
   ```bash
   python verify_export_timezone.py
   ```

3. **Or export from GUI and check the timestamps:**
   - Start the app
   - Click "Export CSV"
   - Open the CSV file
   - Verify timestamps have your timezone offset (e.g., `-07:00` for Denver in winter)

## Files Modified

- `powerapp/utils/export.py`: Added timezone conversion
- `powerapp/gtk/main.py`: Updated export handler to pass timezone
- `tests/test_export.py`: Added timezone conversion tests

## Notes

- Internal timestamps are still stored in UTC (correct behavior)
- Only the export converts to user's timezone (for readability)
- Invalid timezone gracefully falls back to UTC
- Timezone must be set in config (first-launch dialog ensures this)
