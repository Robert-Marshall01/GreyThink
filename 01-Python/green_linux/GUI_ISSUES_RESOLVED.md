# PowerApp GUI Issues - RESOLVED

## Problem Summary
The GUI was not collecting samples or showing the Export Preview window.

## Root Causes Identified

### 1. **Old Instance Still Running** (PRIMARY ISSUE)
- An old PowerApp instance from the desktop launcher was still running in the background
- GTK single-instance mode caused new launches to exit immediately
- Process ID 32133 was running: `/usr/bin/python3 -m powerapp.gtk.main`

### 2. **TypeError in Preview Function**
- `self.history` is a `deque` (not a list)
- Slicing with `self.history[:3]` caused `TypeError: sequence index must be integer, not 'slice'`
- Fixed by converting to list: `history_list = list(self.history)`

## Solutions Implemented

### 1. Fixed the Preview Bug
File: `powerapp/gtk/main.py` - `_on_preview()` method
- Convert deque to list before slicing
- All `self.history` references now use `history_list` for operations that require indexing

### 2. Created Restart Script
File: `restart_powerapp.sh`
```bash
#!/bin/bash
pkill -f "python.*powerapp.gtk.main"
sleep 1
python -m powerapp.gtk.main
```

## Verification Results

### ✓ Confirmed Working:
1. **Settings loaded correctly**: `use_mock_power: True`, `timezone: America/Denver`
2. **Mock power sampling**: Generating realistic values (19-42W range)
3. **Auto-refresh timer**: Collecting samples every 5 seconds  
4. **History tracking**: Successfully storing samples in deque
5. **Export Preview**: Opens and displays samples correctly
6. **GUI responsiveness**: All buttons and controls working

### Sample Debug Output:
```
POWERWINDOW __init__ START
DEBUG: After super().__init__
DEBUG __init__: Calling _schedule()
DEBUG _schedule: Timer set, new _auto_id=10, interval=5s
DEBUG refresh(): Called, POWERAPP_MOCK_POWER=1
DEBUG _sample_worker: use_mock=True, method=mock
DEBUG _sample_worker: res={'timestamp': '2025-12-22T19:08:37.769651+00:00', 
  'method': 'mock', 'power_w': 41.35, ...}
DEBUG _handle_sample: ts=2025-12-22T19:08:37.769651+00:00, pw=41.35
DEBUG _handle_sample: history now has 1 samples
```

## How to Use

### Restart the App (Recommended Method)
```bash
./restart_powerapp.sh
```

### Manual Launch
```bash
# 1. Kill any running instances
pkill -f "python.*powerapp.gtk.main"

# 2. Launch fresh
python -m powerapp.gtk.main
```

### Check If Already Running
```bash
ps aux | grep -i powerapp | grep -v grep
```

### Enable Mock Power (if needed)
Mock power is already enabled in your settings (`use_mock_power: True`), so you don't need to set environment variables.

To verify settings:
```bash
cat ~/.config/powerapp/config.json | grep use_mock_power
```

## Expected Behavior Now

1. **On Launch**:
   - Window appears immediately (no timezone dialog since you already configured it)
   - "Method: mock" displayed
   - Power readings start appearing: "XX.XX W"
   - Auto-refresh enabled by default

2. **Sample Collection**:
   - New sample every 5 seconds
   - History counter increases: "Will export: N samples"
   - Sparkline graph updates
   - Stats show Min/Max/Avg

3. **Export Preview**:
   - Click "Preview" button
   - Dialog shows full list of samples
   - Includes timestamps and power values
   - Shows diagnostics if there are issues

4. **CSV Export**:
   - Click "Export CSV"
   - Saves to timestamped file
   - Includes average intensity column
   - All timestamps in your local timezone (America/Denver)

## Troubleshooting

### If GUI Still Won't Start:
```bash
# Check for running instances
ps aux | grep powerapp

# Kill all instances forcefully
killall -9 python3
pkill -9 -f powerapp

# Try launching with debug output
python -m powerapp.gtk.main 2>&1 | tee /tmp/powerapp.log
```

### If Samples Not Collecting:
1. Check settings: `cat ~/.config/powerapp/config.json`
2. Verify `use_mock_power: true` is present
3. Restart the app using `./restart_powerapp.sh`

### If Preview Shows "No Valid Samples":
1. Wait at least 5 seconds for first sample
2. Check "Method" label shows "mock" (not "none")
3. Check "Will export: N samples" counter is increasing

## Files Modified

- `powerapp/gtk/main.py`: Fixed deque slicing in `_on_preview()`
- `restart_powerapp.sh`: New script for easy app restart
- `test_gui_startup.py`: Diagnostic script (can be kept for future testing)

## Cleanup (Optional)

You can remove these diagnostic scripts if desired:
- `diagnose_power_sampling.py`
- `debug_export_preview.py`
- `debug_history.py`
- `test_env_gui.py`
- `diagnose_gui_full.py`
- `check_timezone_setup.py`
- `test_gui_startup.py`
- `enable_mock_power.py` (mock mode is now persisted in settings)

Keep these:
- `restart_powerapp.sh` (useful for restarting the app)
- All your launch scripts in `scripts/` directory

## Summary

**The app is now fully functional!** The main issue was an old instance blocking new launches. After killing the old process and fixing the deque slicing bug, the GUI now:
- ✓ Collects samples automatically
- ✓ Shows export preview correctly  
- ✓ Uses mock power for testing
- ✓ Exports CSV with correct timezone and intensity values

Use `./restart_powerapp.sh` whenever you need to restart the app fresh.
