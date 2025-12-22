#!/usr/bin/env python3
import sys
import logging

import gi

logging.basicConfig(level=logging.INFO)
gi.require_version('Gtk', '4.0')
from gi.repository import GLib

# Ensure package imports
import os
sys.path.insert(0, os.getcwd())

from powerapp.gtk.main import PowerWindow
import tools.check_settings as cs

# Prevent saving to disk during this test
try:
    import powerapp.config as cfg
    cfg.save_settings = lambda s: logging.info('save_settings called (stub)')
except Exception:
    pass

# Ensure our presenter wrapper is installed
cs.original_presenter = PowerWindow._present_and_handle_dialog
PowerWindow._present_and_handle_dialog = cs._present_and_capture

win = PowerWindow(None)
logging.info('Created PowerWindow')

# show settings
win._show_settings_dialog()

# helper to iterate children (GTK4 compat)
def iter_children(w):
    try:
        if getattr(w,'get_children',None):
            return list(w.get_children())
    except Exception:
        pass
    out = []
    try:
        ch = w.get_first_child() if getattr(w,'get_first_child',None) else None
        while ch is not None:
            out.append(ch)
            nxt = ch.get_next_sibling() if getattr(ch,'get_next_sibling',None) else None
            ch = nxt
    except Exception:
        pass
    return out

# schedule a sequence of interactions
captured = cs.created_dialogs

# Wait for dialog to present and then interact

def _interact():
    if not captured:
        logging.info('No dialogs captured yet')
        return False
    settings = captured[0]
    logging.info('Interacting with Settings dialog')
    content = settings.get_child() if getattr(settings,'get_child',None) else (settings.get_content_area() if getattr(settings,'get_content_area',None) else None)
    if content is None:
        logging.warning('No content')
        return False
    # find Buttons of interest
    to_click = {'Diagnostics':[], 'Privacy details':[], 'Browse...':[], 'Manage samples':[], 'Cancel':[], 'Save':[]}
    # traverse grid for items
    for c in iter_children(content):
        for g in iter_children(c):
            try:
                if getattr(g,'get_label',None):
                    lbl = g.get_label()
                elif getattr(g,'get_text',None):
                    lbl = g.get_text()
                else:
                    lbl = ''
            except Exception:
                lbl = ''
            if lbl in to_click:
                to_click[lbl].append(g)
    # Click Diagnostics if present
    if to_click['Diagnostics']:
        b = to_click['Diagnostics'][0]
        logging.info('Clicking Diagnostics')
        try:
            b.emit('clicked')
        except Exception:
            try:
                b._on_clicked()
            except Exception:
                pass
    else:
        logging.warning('Diagnostics button not found')
    # Click Privacy details
    if to_click['Privacy details']:
        b = to_click['Privacy details'][0]
        logging.info('Clicking Privacy details')
        try:
            b.emit('clicked')
        except Exception:
            try:
                b._on_clicked()
            except Exception:
                pass
    else:
        logging.warning('Privacy details not found')
    # Click Browse... (ML model)
    if to_click['Browse...']:
        b = to_click['Browse...'][0]
        logging.info('Clicking Browse...')
        try:
            b.emit('clicked')
        except Exception:
            pass
    # Click Save
    if to_click['Save']:
        b = to_click['Save'][0]
        logging.info('Clicking Save')
        try:
            b.emit('clicked')
        except Exception:
            try:
                b._on_clicked()
            except Exception:
                pass
    else:
        logging.warning('Save button not found')
    # schedule a dump of captured dialogs after 300ms
    GLib.timeout_add(300, _dump_after)
    return False


def _dump_after():
    logging.info('Dumping captured dialogs after interactions:')
    for d in captured:
        try:
            t = d.get_title() if getattr(d,'get_title',None) else '<no-title>'
        except Exception:
            t = '<err>'
        logging.info(' Dialog: %s', t)
        cont = d.get_child() if getattr(d,'get_child',None) else (d.get_content_area() if getattr(d,'get_content_area',None) else None)
        if cont is None:
            logging.warning('  No content')
            continue
        for i,ch in enumerate(iter_children(cont)):
            try:
                label = ch.get_label() if getattr(ch,'get_label',None) else (ch.get_text() if getattr(ch,'get_text',None) else '')
            except Exception:
                label = ''
            logging.info('  child %d %s %s', i, getattr(ch,'__class__',type(ch)).__name__, str(label)[:200].replace('\n',' '))
    logging.info('Main note_label: %s', win.note_label.get_text())
    logging.info('Main stats_label: %s', win.stats_label.get_text())
    loop.quit()
    return False

# run loop
loop = GLib.MainLoop()
GLib.timeout_add(400, _interact)
loop.run()
logging.info('Done')
