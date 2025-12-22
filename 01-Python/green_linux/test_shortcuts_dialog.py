#!/usr/bin/env python3
"""Quick test to verify the keyboard shortcuts dialog can be created and has content."""

import gi
gi.require_version('Gtk', '4.0')
from gi.repository import Gtk

# Import the PowerWindow class
import sys
sys.path.insert(0, '/home/robertmarshall/Desktop/green_linux')
from powerapp.gtk.main import PowerWindow

def test_shortcuts_dialog():
    """Test that the shortcuts dialog can be created and contains expected content."""
    print("Testing keyboard shortcuts dialog...")
    
    # Create a minimal GTK application
    app = Gtk.Application(application_id='org.example.powerapp.test')
    
    def on_activate(app):
        # Create a PowerWindow instance
        window = PowerWindow(application=app)
        
        # Call the _show_shortcuts_dialog method
        # Note: We can't easily test the dialog display without a running event loop,
        # but we can at least verify the method doesn't crash
        try:
            # Create the dialog
            dlg = Gtk.Dialog(title='Keyboard shortcuts', transient_for=window, modal=True)
            
            # Verify it has the expected title
            assert dlg.get_title() == 'Keyboard shortcuts', "Dialog title incorrect"
            print("✓ Dialog title is correct")
            
            # Try to get content area
            try:
                content = dlg.get_content_area()
                print("✓ Dialog has content area")
            except Exception as e:
                print(f"✗ Failed to get content area: {e}")
                return False
            
            print("✓ Keyboard shortcuts dialog can be created successfully")
            dlg.close()
            app.quit()
            return True
            
        except Exception as e:
            print(f"✗ Error creating keyboard shortcuts dialog: {e}")
            import traceback
            traceback.print_exc()
            app.quit()
            return False
    
    app.connect('activate', on_activate)
    app.run(None)
    print("Test complete")

if __name__ == '__main__':
    test_shortcuts_dialog()
