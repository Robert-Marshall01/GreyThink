#!/usr/bin/env python3
import sys
import traceback
import datetime

import logging

try:
    import gi
    gi.require_version('Gtk', '4.0')
    from gi.repository import Gtk, GLib
except Exception as e:
    logging.error('GTK import failed: %s', e)
    sys.exit(2)

# Make GLib.idle_add synchronous for our checks
try:
    GLib.idle_add = lambda fn, *a, **k: (fn(*a, **k), None)[1]
except Exception:
    pass

created = []
created_dialogs = []

# Capture dialogs when presenter is used (preferred) or when run() fallback occurs

original_presenter = None

def _present_and_capture(self, dlg, on_response=None):
    try:
        created_dialogs.append(dlg)
    except Exception:
        pass
    # call the original if present
    try:
        if original_presenter:
            return original_presenter(self, dlg, on_response=on_response)
    except Exception:
        pass

# We'll wire this up after importing PowerWindow

def dialog_run_stub(self):
    created.append(self)
    title = ''
    try:
        title = self.get_title() or ''
    except Exception:
        pass
    if title == 'Settings':
        # attempt to find checkbuttons and entries and set them to test behavior
        def _find_in(container, cls):
            try:
                children = container.get_children()
            except Exception:
                return None
            for c in children:
                if isinstance(c, cls):
                    return c
                found = _find_in(c, cls)
                if found:
                    return found
            return None
        cb = None
        ent = None
        try:
            if getattr(self, 'get_child', None):
                cb = _find_in(self.get_child(), Gtk.CheckButton)
                ent = _find_in(self.get_child(), Gtk.Entry)
            elif getattr(self, 'get_content_area', None):
                cb = _find_in(self.get_content_area(), Gtk.CheckButton)
                ent = _find_in(self.get_content_area(), Gtk.Entry)
        except Exception:
            pass
        if cb is not None:
            try:
                cb.set_active(True)
            except Exception:
                pass
        if ent is not None:
            try:
                ent.set_text('/tmp/fake_model.joblib')
            except Exception:
                pass
        return Gtk.ResponseType.OK
    return Gtk.ResponseType.CLOSE

# Patch run
Gtk.Dialog.run = dialog_run_stub

from powerapp.gtk.main import PowerWindow

# Create a window and open settings
win = PowerWindow(None)
logging.info('Created PowerWindow')

# Call settings dialog
try:
    win._show_settings_dialog()
except Exception:
    traceback.print_exc()

# Inspect created dialogs
for d in created:
    try:
        title = d.get_title() if getattr(d, 'get_title', None) else '<no-title>'
    except Exception:
        title = '<err>'
    logging.info('Dialog: %s', title)
    content = None
    try:
        content = d.get_child() if getattr(d, 'get_child', None) else (d.get_content_area() if getattr(d, 'get_content_area', None) else None)
    except Exception:
        pass
    if content is None:
        logging.warning('No content area on dialog: %s', title)
        continue
    def dump(c, depth=0):
        indent = '  ' * depth
        try:
            cname = getattr(c, '__class__', type(c)).__name__
        except Exception:
            cname = 'Unknown'
        txt = ''
        try:
            if getattr(c, 'get_label', None):
                txt = c.get_label()
            elif getattr(c, 'get_text', None):
                txt = c.get_text()
        except Exception:
            txt = '<err-text>'
        safe_txt = str(txt)[:200].replace("\n", " ")
        logging.info('%s%s: %s', indent, cname, safe_txt)
        try:
            for ch in c.get_children():
                dump(ch, depth+1)
        except Exception:
            pass
    dump(content)

# Inspect main window labels near bottom
logging.info('Main window labels:')
try:
    logging.info(' stats_label: %s', win.stats_label.get_text())
except Exception:
    logging.info(' stats_label: <err>')
try:
    logging.info(' note_label: %s', win.note_label.get_text())
except Exception:
    logging.info(' note_label: <err>')

logging.info('Done %s', datetime.datetime.now())
