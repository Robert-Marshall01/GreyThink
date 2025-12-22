#!/usr/bin/env python3
"""Simple test to verify GUI launches and captures debug output."""
import sys
import os

# Enable mock power for testing
os.environ['POWERAPP_MOCK_POWER'] = '1'

print("=" * 80)
print("TESTING GUI STARTUP")
print("=" * 80)

# 1. Check settings
print("\n1. CHECKING SETTINGS:")
from powerapp.config import load_settings, _config_path
settings = load_settings()
print(f"   Config path: {_config_path()}")
print(f"   use_mock_power: {settings.get('use_mock_power')}")
print(f"   timezone: {settings.get('timezone')}")

# 2. Test power sampling
print("\n2. TESTING POWER SAMPLING:")
from powerapp.cli import power_reader as pr
sample = pr.get_sample(method='mock', interval=0.5)
print(f"   Sample: {sample}")
if sample and sample.get('power_w'):
    print(f"   ✓ Got power: {sample['power_w']} W")
else:
    print(f"   ✗ No power value!")

# 3. Launch GUI
print("\n3. LAUNCHING GUI:")
print("   (Watch for debug output from _sample_worker and _handle_sample)")
print("   Press Ctrl+C to exit\n")

try:
    import gi
    gi.require_version('Gtk', '4.0')
    from gi.repository import Gtk
    print("DEBUG: Importing powerapp.gtk.main...")
    from powerapp.gtk.main import PowerApp
    
    print("DEBUG: Creating PowerApp instance...")
    app = PowerApp()
    print(f"DEBUG: PowerApp created: {app}")
    print(f"DEBUG: App ID: {app.get_application_id()}")
    
    print("DEBUG: Calling app.run()...")
    status = app.run(sys.argv)
    print(f"DEBUG: app.run() returned: {status}")
    sys.exit(status)
except KeyboardInterrupt:
    print("\nExited by user")
    sys.exit(0)
except Exception as e:
    print(f"\n✗ ERROR: {e}")
    import traceback
    traceback.print_exc()
    sys.exit(1)
