#!/usr/bin/env python3
"""Diagnose why power sampling is returning None values."""
import sys
from powerapp.cli import power_reader as pr

print("=" * 80)
print("Power Sampling Diagnostic")
print("=" * 80)

# Test each method
methods = ['auto', 'upower', 'rapl', 'mock']

for method in methods:
    print(f"\n--- Testing method: {method} ---")
    try:
        result = pr.get_sample(method=method, interval=0.5)
        print(f"✓ Method: {result.get('method', 'unknown')}")
        print(f"  Power: {result.get('power_w', 'None')} W")
        print(f"  Timestamp: {result.get('timestamp', 'None')}")
        if result.get('notes'):
            print(f"  Notes: {result.get('notes')}")
        if result.get('power_w') is not None:
            print(f"  ✅ SUCCESS: {method} is working!")
    except Exception as e:
        print(f"✗ Error: {e}")

print("\n" + "=" * 80)
print("System Information")
print("=" * 80)

# Check upower
print("\nChecking upower availability:")
import subprocess
try:
    result = subprocess.run(['which', 'upower'], capture_output=True, text=True)
    if result.returncode == 0:
        print(f"✓ upower found at: {result.stdout.strip()}")
        # Get battery info
        result = subprocess.run(['upower', '-d'], capture_output=True, text=True)
        if result.returncode == 0:
            print("\nBattery devices:")
            for line in result.stdout.splitlines():
                if 'Device:' in line or 'battery' in line.lower():
                    print(f"  {line.strip()}")
        else:
            print("✗ upower command failed")
    else:
        print("✗ upower not found")
except Exception as e:
    print(f"✗ Error checking upower: {e}")

# Check RAPL
print("\nChecking RAPL (Intel RAPL energy interface):")
rapl_paths = [
    '/sys/class/powercap/intel-rapl:0/energy_uj',
    '/sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj',
]
rapl_found = False
for path in rapl_paths:
    try:
        with open(path) as f:
            val = f.read().strip()
            print(f"✓ RAPL found at: {path}")
            print(f"  Current reading: {val}")
            rapl_found = True
            break
    except Exception:
        continue

if not rapl_found:
    print("✗ RAPL interface not found")
    print("  (This is expected on non-Intel systems or VMs)")

print("\n" + "=" * 80)
print("Diagnosis Summary")
print("=" * 80)

print("\nIf all methods return None, possible causes:")
print("1. No battery detected (desktop/VM)")
print("2. upower not installed or not working")
print("3. RAPL not available (non-Intel CPU or VM)")
print("4. Permissions issue accessing power interfaces")

print("\nTo fix:")
print("1. For testing/development: Use mock provider")
print("   - In GUI: Settings → Provider → mock")
print("2. Install upower: sudo apt install upower")
print("3. Check if running in a VM (power monitoring not available)")
print("=" * 80)
