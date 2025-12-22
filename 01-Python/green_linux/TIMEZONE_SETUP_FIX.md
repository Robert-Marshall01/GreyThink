# Timezone Setup Dialog Fix

## Problem
The timezone setup dialog was not appearing on first launch when the app was started from the Ubuntu GUI (desktop icon). Users would see the main PowerApp window directly instead of being prompted to set their timezone first.

## Root Cause
The `_show_timezone_setup()` method was creating a `Gtk.Dialog` without a `transient_for` parent window. GTK issued a warning:

```
GtkDialog mapped without a transient parent. This is discouraged.
```

When a dialog has no transient parent, some window managers (especially when apps are launched from GUI icons rather than terminal) may not display the dialog properly, or may display it in a way that doesn't grab user attention.

## Solution
Modified `_show_timezone_setup()` to create a small `Gtk.ApplicationWindow` as a parent before creating the dialog:

```python
# Create a simple ApplicationWindow to act as parent for the dialog
parent_win = Gtk.ApplicationWindow(application=self)
parent_win.set_title('PowerApp')
parent_win.set_default_size(100, 100)  # Small but visible size
parent_win.present()  # Must be shown/presented for some WMs to display modal dialog

dlg = Gtk.Dialog(title='Initial Setup: Select Your Timezone', 
                 transient_for=parent_win, modal=True)
```

The parent window is destroyed along with the dialog when the user clicks "Continue".

## Testing

### Automated Tests
```bash
python3 -m pytest tests/test_timezone_setup_dialog.py -v
```

Both tests should pass:
- `test_timezone_dialog_shown_when_not_set` - Verifies dialog appears when no timezone is set
- `test_no_timezone_dialog_when_already_set` - Verifies dialog is skipped when timezone is already configured

### Manual Testing (First Launch)
Use the provided test script:

```bash
./test_first_launch_gui.sh
```

This script:
1. Backs up and removes your config file (if it exists)
2. Launches the app
3. You should see the timezone setup dialog BEFORE the main window

**Expected Behavior:**
1. A small "PowerApp" window appears briefly (this is the parent)
2. The "Initial Setup: Select Your Timezone" dialog appears on top (modal)
3. Select your timezone and click "Continue"
4. Both the dialog and parent window close
5. The main PowerApp window appears

### Manual Testing (Subsequent Launches)
After setting your timezone once:

```bash
python3 -m powerapp.gtk.main
```

You should go directly to the main window WITHOUT seeing the timezone setup dialog.

### Verify Config File
After first launch, check that your timezone was saved:

```bash
cat ~/.config/powerapp/config.json
```

Should contain:
```json
{
  "timezone": "America/New_York"  # or your selected timezone
}
```

## Files Modified
- [powerapp/gtk/main.py](powerapp/gtk/main.py#L10715-L10920) - Updated `_show_timezone_setup()` method
- [test_first_launch_gui.sh](test_first_launch_gui.sh) - Created test script for manual verification

## Related Issues
- Issue was that config file logic was previously preventing timezone from being cleared via empty string
- Fix ensures robust first-launch detection by checking config file existence before loading settings
- Tests now properly mock `os.path.exists` to simulate first-launch scenarios
