#!/usr/bin/env python3
"""Check if timezone setup should be shown."""
import os
from powerapp.config import load_settings

print("=" * 80)
print("TIMEZONE SETUP CHECK")
print("=" * 80)

# Check if config file exists BEFORE loading (load_settings creates directory)
xdg = os.environ.get('XDG_CONFIG_HOME', os.path.join(os.path.expanduser('~'), '.config'))
config_file = os.path.join(xdg, 'powerapp', 'config.json')
config_exists = os.path.exists(config_file)

print(f"\n1. Config file path: {config_file}")
print(f"2. Config exists: {config_exists}")

settings = load_settings()
timezone = settings.get('timezone', '').strip()

print(f"3. Timezone from settings: '{timezone}'")
print(f"4. Timezone is empty: {not timezone}")

# Show setup if config doesn't exist or timezone is empty/default
needs_setup = not config_exists or not timezone

print(f"\n5. NEEDS_SETUP: {needs_setup}")

if needs_setup:
    print("\n❌ Timezone setup dialog WILL BE SHOWN")
    print("   This blocks the main window from appearing!")
else:
    print("\n✓ Timezone already configured, main window will appear")
