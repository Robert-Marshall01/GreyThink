#!/usr/bin/env python3
import os
import sys

print(f"Python sys.executable: {sys.executable}")
print(f"POWERAPP_MOCK_POWER: {os.environ.get('POWERAPP_MOCK_POWER')}")
print(f"All env vars with POWERAPP:")
for k, v in os.environ.items():
    if 'POWERAPP' in k:
        print(f"  {k}={v}")

# Test the import
from powerapp.cli import power_reader as pr
res = pr.get_sample(method='auto', interval=0.5)
print(f"\nget_sample result: {res}")
