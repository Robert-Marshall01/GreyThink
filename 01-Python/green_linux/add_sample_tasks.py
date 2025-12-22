#!/usr/bin/env python3
"""Add sample tasks to PowerApp settings for testing postponement suggestions."""

import json
from pathlib import Path
import os

# Use the correct config path (config.json, not settings.json)
xdg = os.environ.get('XDG_CONFIG_HOME')
if not xdg:
    xdg = str(Path.home() / '.config')
settings_path = Path(xdg) / 'powerapp' / 'config.json'

# Load current settings
if settings_path.exists():
    with open(settings_path) as f:
        settings = json.load(f)
else:
    settings = {}

# Add sample tasks for testing
settings['tasks'] = [
    {
        "id": "system_backup",
        "name": "System backup",
        "urgency": "low",
        "duration_min": 60,
        "power_w": 100
    },
    {
        "id": "video_export",
        "name": "Export video project",
        "urgency": "medium",
        "duration_min": 30,
        "power_w": 150
    },
    {
        "id": "critical_update",
        "name": "Security update",
        "urgency": "high",
        "duration_min": 15,
        "power_w": 50
    }
]

# Save settings
settings_path.parent.mkdir(parents=True, exist_ok=True)
with open(settings_path, 'w') as f:
    json.dump(settings, f, indent=2)

print(f"Added {len(settings['tasks'])} sample tasks to {settings_path}")
print("\nTasks added:")
for task in settings['tasks']:
    print(f"  - {task['name']} ({task['urgency']} urgency, {task['duration_min']} min, {task['power_w']} W)")

print("\nNow test the postponement suggestions by clicking 'Suggest postponements' in the GUI.")
