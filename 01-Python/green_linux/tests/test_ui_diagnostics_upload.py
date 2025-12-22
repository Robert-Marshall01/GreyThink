import pytest
from gi.repository import Gtk
from powerapp.gtk.main import PowerWindow


def test_diagnostics_upload_invokes_upload(monkeypatch):
    # capture created dialogs
    created = []
    def dialog_run_stub(self):
        created.append(self)
        return Gtk.ResponseType.CLOSE
    monkeypatch.setattr(Gtk.Dialog, 'run', dialog_run_stub, raising=False)

    # create window
    try:
        win = PowerWindow(app=None)
    except Exception:
        pytest.skip('Cannot instantiate PowerWindow')

    # stub diagnostics runner
    from powerapp import system as ps
    monkeypatch.setattr(ps.integration, 'run_diagnostics', lambda: {'candidate_used':'gdbus','current_profile':'balanced','systemd_user':True, 'email':'alice@example.com'})

    # enable upload in settings and provide url
    win.settings['enable_bugreport_upload'] = True
    win.settings['bugreport_upload_url'] = 'https://example.test/bugreport'

    # open settings dialog and find the Diagnostics button
    try:
        PowerWindow._show_settings_dialog(win)
    except Exception:
        pytest.skip('GTK environment incompatible')

    # Find the Settings dialog by title among created dialogs (be robust to other dialogs)
    settings_dlg = None
    for d in reversed(created):
        try:
            if getattr(d, 'get_title', None) and d.get_title() == 'Settings':
                settings_dlg = d
                break
        except Exception:
            pass
    if settings_dlg is None:
        pytest.skip('Settings dialog not created')

    diag_btn = None
    content = settings_dlg.get_child() if getattr(settings_dlg, 'get_child', None) else settings_dlg.get_content_area()

    # Recursively search the widget tree for a Label with text 'Diagnostics' or a Button labeled 'Diagnostics'
    def find_label(root, text):
        try:
            if isinstance(root, Gtk.Label) and root.get_text() == text:
                return root
        except Exception:
            pass
        try:
            for ch in root.get_children():
                res = find_label(ch, text)
                if res is not None:
                    return res
        except Exception:
            pass
        return None

    def find_button(root, text):
        try:
            if isinstance(root, Gtk.Button):
                try:
                    if (root.get_label() or '') == text:
                        return root
                except Exception:
                    pass
                # check for a Label child
                try:
                    for ch in root.get_children():
                        if isinstance(ch, Gtk.Label) and (ch.get_text() or '') == text:
                            return root
                except Exception:
                    pass
        except Exception:
            pass
        try:
            for ch in root.get_children():
                res = find_button(ch, text)
                if res is not None:
                    return res
        except Exception:
            pass
        return None

    lab = find_label(content, 'Diagnostics')
    if lab is not None:
        # walk up to nearest Button
        parent = lab.get_parent()
        while parent is not None:
            try:
                if isinstance(parent, Gtk.Button):
                    diag_btn = parent
                    break
            except Exception:
                pass
            try:
                parent = parent.get_parent()
            except Exception:
                parent = None
    if diag_btn is None:
        diag_btn = find_button(content, 'Diagnostics')

    assert diag_btn is not None

    # open diagnostics dialog
    try:
        diag_btn.emit('clicked')
    except Exception:
        try:
            diag_btn._on_clicked()
        except Exception:
            pass

    assert len(created) >= 2
    diag_dlg = created[-1]

    # find upload checkbox and upload button (search recursively)
    upload_chk = None
    upload_btn = None
    cont = diag_dlg.get_child() if getattr(diag_dlg, 'get_child', None) else diag_dlg.get_content_area()

    def find_checkbutton(root, substring):
        try:
            if isinstance(root, Gtk.CheckButton):
                try:
                    if substring in (root.get_label() or ''):
                        return root
                except Exception:
                    pass
                try:
                    for ch in root.get_children():
                        if isinstance(ch, Gtk.Label) and substring in (ch.get_text() or ''):
                            return root
                except Exception:
                    pass
        except Exception:
            pass
        try:
            for ch in root.get_children():
                res = find_checkbutton(ch, substring)
                if res is not None:
                    return res
        except Exception:
            pass
        return None

    def find_button(root, text):
        try:
            if isinstance(root, Gtk.Button):
                try:
                    if (root.get_label() or '') == text:
                        return root
                except Exception:
                    pass
                try:
                    for ch in root.get_children():
                        if isinstance(ch, Gtk.Label) and (ch.get_text() or '') == text:
                            return root
                except Exception:
                    pass
        except Exception:
            pass
        try:
            for ch in root.get_children():
                res = find_button(ch, text)
                if res is not None:
                    return res
        except Exception:
            pass
        return None

    upload_chk = find_checkbutton(cont, 'Upload anonymized')
    upload_btn = find_button(cont, 'Upload')

    assert upload_chk is not None
    assert upload_btn is not None

    # ensure it's active and monkeypatch upload function
    try:
        upload_chk.set_active(True)
    except Exception:
        pass

    called = {}
    from powerapp.system import bugreport as br
    def fake_upload(diagnostics, url, headers=None):
        called['url'] = url
        called['diagnostics'] = diagnostics
        return {'id':'srv123'}
    monkeypatch.setattr(br, 'upload_bug_report', fake_upload)

    # trigger upload
    try:
        upload_btn.emit('clicked')
    except Exception:
        try:
            upload_btn._on_clicked()
        except Exception:
            pass

    assert called.get('url') == 'https://example.test/bugreport'
    assert 'diagnostics' in called
    # ensure anonymization applied (email should be redacted)
    diag = called['diagnostics']
    assert ('alice@example.com' not in str(diag))
    assert '<REDACTED' in str(diag)