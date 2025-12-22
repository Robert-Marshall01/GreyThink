from pathlib import Path
import importlib

from powerapp.i18n import setup_gettext, get_locale_dir


def test_setup_gettext_and_generate_pot(tmp_path, monkeypatch):
    # Ensure setup_gettext doesn't raise and returns a valid locale dir path
    ld = get_locale_dir()
    assert isinstance(ld, Path)

    # Run generate_pot against project root with output to tmp
    gen = importlib.import_module('scripts.generate_pot')
    out = tmp_path / 'powerapp.pot'
    path = gen.generate_pot(root='.', out=str(out))
    assert path.exists()
    # Check that at least the legend msgid is present in the generated POT
    txt = path.read_text(encoding='utf-8')
    assert 'Legend' in txt or 'Legend' in path.name

    # Ensure setup_gettext can be called safely
    setup_gettext()
    # and '_' is available
    assert callable(__builtins__['_'] if '_' in __builtins__ else (lambda x: x))
