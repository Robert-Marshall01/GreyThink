#!/usr/bin/env python3
"""Debug script to check if the GUI history is being populated with mock power values."""

import os
import sys
import time

# Enable mock mode
os.environ['POWERAPP_MOCK_POWER'] = '1'

# Import the power reader
sys.path.insert(0, os.path.dirname(__file__))
from powerapp.cli import power_reader as pr

print("Testing mock power sampling with GUI-like code...")
print(f"Environment variable POWERAPP_MOCK_POWER={os.environ.get('POWERAPP_MOCK_POWER')}")

# Simulate what the GUI does
method = 'mock' if os.environ.get('POWERAPP_MOCK_POWER') else 'auto'
print(f"Using method: {method}")

# Collect a few samples like the GUI would
history = []
for i in range(5):
    try:
        res = pr.get_sample(method=method, interval=0.5)
        print(f"Sample {i+1}: {res}")
        
        pw = res.get('power_w')
        ts = res.get('timestamp')
        
        if pw is not None:
            history.append((ts, float(pw)))
        else:
            history.append((ts, None))
            
    except Exception as e:
        print(f"Error: {e}")
        history.append((time.time(), None))

print(f"\nHistory collected: {len(history)} samples")
print("History contents:")
for ts, pw in history:
    print(f"  {ts}: {pw}")

# Now test the export preview with this history
print("\nTesting export preview...")
from powerapp.utils.export import get_export_preview

preview = get_export_preview(history, last_minutes=None, max_items=10)
print("Preview output:")
print(preview)
