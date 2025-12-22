#!/usr/bin/env python3
"""Enable mock power mode in settings for testing on systems without battery/RAPL."""

from powerapp.config import load_settings, save_settings

settings = load_settings()
print(f"Current use_mock_power: {settings.get('use_mock_power')}")

settings['use_mock_power'] = True
save_settings(settings)

print(f"Updated use_mock_power: True")
print("\nMock power mode enabled! You can now run the GUI normally and it will use fake power values.")
print("To disable, run: python -c \"from powerapp.config import save_settings; save_settings({'use_mock_power': False})\"")
