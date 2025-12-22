#!/usr/bin/env python3
"""
Comprehensive GUI diagnostics - captures all initialization and runtime issues.
Writes detailed log to /tmp/powerapp_gui_diagnostic.log
"""
import sys
import os
import traceback
from pathlib import Path

LOG_FILE = "/tmp/powerapp_gui_diagnostic.log"

def log(msg):
    """Write message to both stdout and log file."""
    print(msg, flush=True)
    try:
        with open(LOG_FILE, 'a') as f:
            f.write(msg + '\n')
    except Exception as e:
        print(f"Failed to write to log: {e}", flush=True)

def main():
    try:
        # Clear previous log
        try:
            Path(LOG_FILE).unlink(missing_ok=True)
        except Exception:
            pass
        
        log("=" * 80)
        log("POWERAPP GUI DIAGNOSTIC START")
        log("=" * 80)
        
        # 1. Check environment
        log("\n1. ENVIRONMENT:")
        log(f"   POWERAPP_MOCK_POWER: {os.getenv('POWERAPP_MOCK_POWER', 'NOT SET')}")
        log(f"   Python: {sys.version}")
        log(f"   CWD: {os.getcwd()}")
        
        # 2. Check config file
        log("\n2. CONFIG FILE:")
        from powerapp.config import _config_path, load_settings
        cfg_path = _config_path()
        log(f"   Config file: {cfg_path}")
        log(f"   Config exists: {cfg_path.exists()}")
        if cfg_path.exists():
            log(f"   Config contents:\n{cfg_path.read_text()}")
        
        # 3. Check settings values
        log("\n3. SETTINGS VALUES:")
        settings = load_settings()
        log(f"   use_mock_power: {settings.get('use_mock_power', False)}")
        log(f"   timezone: {settings.get('timezone', 'NOT SET')}")
        log(f"   first_launch_complete: {settings.get('first_launch_complete', False)}")
        
        # 4. Test power sampling
        log("\n4. POWER SAMPLING TEST:")
        try:
            from powerapp.cli.power_reader import PowerSampler
            sampler = PowerSampler()
            log(f"   PowerSampler initialized")
            log(f"   use_mock: {sampler.use_mock}")
            
            sample = sampler.read()
            log(f"   Sample result: {sample}")
            
            if sample and sample.get('power_watts'):
                log(f"   ✓ Power sampling working! Got {sample['power_watts']} W")
            else:
                log(f"   ✗ Power sampling returned None or no power_watts")
        except Exception as e:
            log(f"   ✗ Power sampling FAILED: {e}")
            log(f"   Traceback:\n{traceback.format_exc()}")
        
        # 5. Check history
        log("\n5. HISTORY CHECK:")
        try:
            from powerapp.cli.power_reader import PowerSampler
            sampler = PowerSampler()
            history = sampler.get_history()
            log(f"   History length: {len(history)}")
            if history:
                log(f"   First sample: {history[0]}")
                log(f"   Last sample: {history[-1]}")
            else:
                log(f"   History is empty")
        except Exception as e:
            log(f"   ✗ History check FAILED: {e}")
        
        # 6. Test GUI imports
        log("\n6. GUI IMPORTS:")
        try:
            import gi
            gi.require_version('Gtk', '4.0')
            from gi.repository import Gtk
            log(f"   ✓ GTK 4.0 imported successfully")
            log(f"   GTK version: {Gtk.get_major_version()}.{Gtk.get_minor_version()}.{Gtk.get_micro_version()}")
        except Exception as e:
            log(f"   ✗ GTK import FAILED: {e}")
            log(f"   Cannot proceed with GUI diagnostics")
            return 1
        
        # 7. Try to import main module
        log("\n7. MAIN MODULE IMPORT:")
        try:
            from powerapp.gtk import main as gtk_main
            log(f"   ✓ powerapp.gtk.main imported successfully")
        except Exception as e:
            log(f"   ✗ Main module import FAILED: {e}")
            log(f"   Traceback:\n{traceback.format_exc()}")
            return 1
        
        # 8. Try to create app instance
        log("\n8. APP INSTANCE CREATION:")
        try:
            app = gtk_main.PowerApplication()
            log(f"   ✓ PowerApplication instance created")
            log(f"   App ID: {app.get_application_id()}")
        except Exception as e:
            log(f"   ✗ App creation FAILED: {e}")
            log(f"   Traceback:\n{traceback.format_exc()}")
            return 1
        
        # 9. Check if window would be created
        log("\n9. WINDOW CREATION TEST:")
        log(f"   Will now attempt to run the app...")
        log(f"   If the GUI appears, check if:")
        log(f"   - Method shows 'mock' or a real power source")
        log(f"   - Samples are being collected (watch the counter)")
        log(f"   - Preview button opens a window")
        log(f"\n   Launching GUI in 3 seconds...")
        
        # Give time to read the log
        import time
        time.sleep(3)
        
        log("\n10. LAUNCHING GUI:")
        try:
            status = app.run(sys.argv)
            log(f"\n   App exited with status: {status}")
        except Exception as e:
            log(f"   ✗ App run FAILED: {e}")
            log(f"   Traceback:\n{traceback.format_exc()}")
            return 1
        
        log("\n" + "=" * 80)
        log("POWERAPP GUI DIAGNOSTIC END")
        log("=" * 80)
        return 0
        
    except Exception as e:
        log(f"\n✗ DIAGNOSTIC SCRIPT FAILED: {e}")
        log(f"Traceback:\n{traceback.format_exc()}")
        return 1

if __name__ == '__main__':
    sys.exit(main())
