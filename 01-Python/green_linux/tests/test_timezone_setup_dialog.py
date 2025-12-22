"""
Test that timezone setup dialog appears when timezone is not set.
"""
import pytest
import os
import tempfile


def test_timezone_dialog_shown_when_not_set(monkeypatch):
    """Verify timezone setup dialog appears when timezone is not configured."""
    # Create a temporary directory for settings
    with tempfile.TemporaryDirectory() as tmpdir:
        settings_path = os.path.join(tmpdir, 'powerapp_settings.json')
        
        # Patch load_settings to return empty timezone
        def mock_load_settings():
            return {'timezone': ''}
        
        # Track if dialog was created
        dialog_shown = []
        
        # Patch the timezone setup method
        def mock_show_timezone_setup(self, on_complete):
            dialog_shown.append(True)
            # Simulate user selecting a timezone and continuing
            if on_complete:
                on_complete()
        
        # Import and patch
        from powerapp.gtk import main as main_module
        monkeypatch.setattr('powerapp.config.load_settings', mock_load_settings)
        monkeypatch.setattr(main_module.PowerApp, '_show_timezone_setup', mock_show_timezone_setup)
        
        # Create app (don't activate, just verify the logic)
        app = main_module.PowerApp()
        
        # Mock PowerWindow to avoid full GUI creation
        window_created = []
        def mock_power_window(app_instance):
            class MockWindow:
                def show(self): pass
                def present(self): pass
                def get_visible(self): return True
            window_created.append(True)
            return MockWindow()
        
        monkeypatch.setattr(main_module, 'PowerWindow', mock_power_window)
        
        # Trigger activation
        app.do_activate()
        
        # Verify dialog was shown
        assert len(dialog_shown) == 1, "Timezone setup dialog should be shown"
        assert len(window_created) == 1, "Main window should be created after dialog"


def test_no_timezone_dialog_when_already_set(monkeypatch):
    """Verify timezone setup dialog is skipped when timezone is configured."""
    # Patch load_settings to return a configured timezone
    def mock_load_settings():
        return {'timezone': 'America/New_York'}
    
    # Mock config file existence (simulate existing config)
    def mock_exists(path):
        if 'powerapp/config.json' in str(path):
            return True
        return False
    
    # Track if dialog was created
    dialog_shown = []
    
    # Patch the timezone setup method
    def mock_show_timezone_setup(self, on_complete):
        dialog_shown.append(True)
        if on_complete:
            on_complete()
    
    # Import and patch
    from powerapp.gtk import main as main_module
    monkeypatch.setattr('powerapp.config.load_settings', mock_load_settings)
    # Patch os.path.exists within the main module's context
    monkeypatch.setattr('os.path.exists', mock_exists)
    monkeypatch.setattr(main_module.PowerApp, '_show_timezone_setup', mock_show_timezone_setup)
    
    # Create app
    app = main_module.PowerApp()
    
    # Mock PowerWindow to avoid full GUI creation
    window_created = []
    def mock_power_window(app_instance):
        class MockWindow:
            def show(self): pass
            def present(self): pass
            def get_visible(self): return True
        window_created.append(True)
        return MockWindow()
    
    monkeypatch.setattr(main_module, 'PowerWindow', mock_power_window)
    
    # Trigger activation
    app.do_activate()
    
    # Verify dialog was NOT shown
    assert len(dialog_shown) == 0, "Timezone setup dialog should NOT be shown when timezone is already set"
    assert len(window_created) == 1, "Main window should be created directly"
