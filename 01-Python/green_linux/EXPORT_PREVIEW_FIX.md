# Export Preview "No Valid Samples" - Diagnostic and Fix

## Issue
The "Export Preview" dialog shows:
```
(no valid samples with power values for the selected window)
```

## Root Causes

This message appears when one of three conditions is met:

### 1. **App Just Started (No Samples Yet)**
- The app hasn't had time to collect power samples
- **Solution**: Wait 5-10 seconds for the app to collect data
- **New message**: `(no samples collected yet - wait a few seconds for app to collect data)`

### 2. **Time Window Too Restrictive**
- The "Last N minutes" filter (default: 60 minutes) excludes all samples
- This happens if samples are older than the selected window
- **Solution**: Increase the "Last N minutes" value
- **New message**: `(no samples in selected time window - try increasing "Last N minutes")`

### 3. **Power Sampling is Failing**
- The app is collecting samples but all power values are `None`
- This indicates an issue with power measurement (upower, RAPL, etc.)
- **Solution**: Check if power monitoring is working on your system
- **New message**: `(no valid power measurements - check if power sampling is working)`

## Fix Applied

Updated [powerapp/utils/export.py](powerapp/utils/export.py#L173-L186) to provide more specific diagnostic messages:

**Before:**
- Generic message: `(no valid samples with power values for the selected window)`
- Didn't help users understand what was wrong

**After:**
- Three specific messages that guide users to the solution:
  1. Wait for samples to be collected
  2. Increase the time window
  3. Check if power sampling is working

## Testing

All tests updated and passing:
```
tests/test_export_preview.py::test_export_preview_empty PASSED
tests/test_export_preview.py::test_export_preview_limits_and_more PASSED
tests/test_export_preview.py::test_export_preview_filters_by_minutes PASSED
tests/test_export_preview.py::test_export_preview_only_none PASSED
```

## How to Diagnose

1. **Check main UI**: Does it show power readings? If not, power sampling isn't working.

2. **Wait a few seconds**: If you just started the app, wait 5-10 seconds for samples.

3. **Adjust time window**: If samples exist but preview is empty, increase "Last N minutes".

4. **Check power monitoring**:
   ```bash
   # Test power sampling directly
   cd /home/robertmarshall/Desktop/green_linux
   python -c "from powerapp.system import powerreader as pr; print(pr.get_sample())"
   ```

## Example Scenarios

### Scenario A: Just Started App
```
Preview shows: (no samples collected yet - wait a few seconds for app to collect data)
Action: Wait 5-10 seconds and try again
```

### Scenario B: Old Samples
```
Preview shows: (no samples in selected time window - try increasing "Last N minutes")
Action: Increase the slider from 60 to 120 or higher
```

### Scenario C: Power Monitoring Issue
```
Preview shows: (no valid power measurements - check if power sampling is working)
Action: Check if upower or RAPL is working on your system
```

## Files Modified

- [powerapp/utils/export.py](powerapp/utils/export.py) - Improved diagnostic messages
- [tests/test_export_preview.py](tests/test_export_preview.py) - Updated tests for new messages

## Related

- See [TIMEZONE_FIX_SUMMARY.md](TIMEZONE_FIX_SUMMARY.md) for CSV timezone conversion fix
