# Postponement Suggestions Bug Fix

## Problem

"Postponement suggestions" always said "No suggestions at this time (intensity is low or deferrable tasks)." even when the carbon intensity was high.

## Root Cause

The issue had two parts:

1. **No tasks were defined in the config file** - The `tasks` array was empty in `~/.config/powerapp/config.json`
2. **Unclear error messaging** - The dialog showed a generic message that didn't distinguish between:
   - No tasks defined
   - Intensity too low
   - All tasks marked as high urgency

## Investigation Details

### How Postponement Suggestions Work

1. User clicks "Suggest postponements" button
2. App loads tasks from `settings['tasks']`
3. App fetches current carbon intensity
4. `suggest_postponements()` function:
   - Loops through tasks
   - Skips tasks with `urgency == 'high'`
   - Only suggests postponement when `current_intensity > threshold` (default 300 gCO2/kWh)
   - Returns empty list if no tasks match criteria

### The Bug

When `tasks = []` (empty), the for loop doesn't execute, so `suggestions = []`, which triggers the "No suggestions" message. The message was misleading because it didn't clearly state that **no tasks were defined**.

## Fix Implemented

### 1. Improved Error Messaging ([main.py:3206-3238](powerapp/gtk/main.py#L3206-L3238))

Changed the "No suggestions" dialog to provide specific feedback:

```python
if not suggestions:
    # Provide more specific messaging based on whether tasks exist
    tasks = self.settings.get('tasks', []) if hasattr(self, 'settings') else []
    if not tasks:
        msg = 'No deferrable tasks defined. Use "Manage tasks" to add tasks that can be postponed.'
    else:
        # Tasks exist but no suggestions generated - check why
        try:
            threshold = self.settings.get('threshold', 300.0) if hasattr(self, 'settings') else 300.0
            zone = self.settings.get('zone') if hasattr(self, 'settings') else None
            from powerapp.emissions import fetch_current_intensity
            intensity_data = fetch_current_intensity(zone=zone)
            current_intensity = intensity_data.get('intensity', 0)
            if current_intensity <= threshold:
                msg = f'Current carbon intensity ({current_intensity:.0f} gCO2/kWh) is below your threshold ({threshold:.0f} gCO2/kWh). No postponements recommended at this time.'
            else:
                # Intensity is high but no suggestions - all tasks must be high urgency
                msg = f'Current carbon intensity ({current_intensity:.0f} gCO2/kWh) is above threshold, but all your tasks are marked as high urgency and cannot be postponed.'
        except Exception as e:
            msg = f'No suggestions at this time (intensity is low or no deferrable tasks). Debug: {e}'
```

### 2. Enhanced Task Management Dialog ([main.py:2452-2534](powerapp/gtk/main.py#L2452-L2534))

When no tasks are defined, the "Manage tasks" dialog now shows:
- Clear title: "No tasks defined"
- Helpful instructions on how to add tasks
- Example task format with JSON syntax
- "Open Settings File" button that launches the editor

Example task format shown:
```json
"tasks": [
  {
    "id": "backup",
    "name": "System backup",
    "urgency": "low",
    "duration_min": 60,
    "power_w": 100
  }
]
```

## Testing

### Test Script: `add_sample_tasks.py`

Created a helper script to add sample tasks for testing:

```bash
python3 add_sample_tasks.py
```

This adds 3 sample tasks:
- System backup (low urgency, 60 min, 100 W)
- Export video project (medium urgency, 30 min, 150 W)
- Security update (high urgency, 15 min, 50 W)

### Verification

After adding tasks and restarting the app:

```bash
./restart_powerapp.sh
```

Click "Suggest postponements" and you should see:
- 2 suggestions (System backup + Export video project)
- Security update is NOT suggested (high urgency tasks are never suggested)
- Each suggestion shows estimated CO2 savings and recommended postponement time

### Test Cases Covered

1. **No tasks defined**: Shows "No deferrable tasks defined. Use 'Manage tasks' to add tasks..."
2. **Intensity below threshold**: Shows "Current carbon intensity (X gCO2/kWh) is below your threshold (Y gCO2/kWh)..."
3. **All tasks high urgency**: Shows "...all your tasks are marked as high urgency and cannot be postponed"
4. **Valid suggestions**: Shows list of tasks with postponement recommendations and CO2 savings

## Task Definition Format

Tasks must be defined in `~/.config/powerapp/config.json`:

```json
{
  "tasks": [
    {
      "id": "unique_task_id",
      "name": "Human-readable task name",
      "urgency": "low",  // "low", "medium", or "high" (only low/medium get suggested)
      "duration_min": 60,  // estimated task duration in minutes
      "power_w": 100  // estimated power consumption in watts (optional, defaults to 50W)
    }
  ],
  "threshold": 300.0,  // carbon intensity threshold in gCO2/kWh
  // ... other settings
}
```

### Urgency Levels

- **low**: Postponed by 6 hours when intensity is high
- **medium**: Postponed by 1 hour when intensity is high
- **high**: Never suggested for postponement

## Files Modified

- [powerapp/gtk/main.py](powerapp/gtk/main.py)
  - `_show_suggestions_dialog()` - improved error messaging (lines 3206-3238)
  - `_show_tasks_dialog()` - enhanced task management UI (lines 2452-2534)

## Files Created

- [add_sample_tasks.py](add_sample_tasks.py) - helper script to add sample tasks for testing
- [POSTPONEMENT_SUGGESTIONS_FIX.md](POSTPONEMENT_SUGGESTIONS_FIX.md) - this documentation

## User Instructions

### To Enable Postponement Suggestions

1. Click "Manage tasks" button in the GUI
2. Click "Open Settings File" button
3. Add your deferrable tasks in JSON format (see example above)
4. Save the file
5. Click "Suggest postponements" to see recommendations

### Tips

- Only add tasks with `urgency: "low"` or `urgency: "medium"` for postponement
- Set `threshold` to control when suggestions are made (default: 300 gCO2/kWh)
- Higher `power_w` values result in larger CO2 savings estimates
- Longer `duration_min` values result in larger CO2 savings estimates

## Related Documentation

- [powerapp/emissions.py](powerapp/emissions.py) - `suggest_postponements()` function
- [powerapp/config.py](powerapp/config.py) - settings management
- [docs/design-on-device-ml-per-app-estimation.md](docs/design-on-device-ml-per-app-estimation.md) - design doc mentioning postponement suggestions
