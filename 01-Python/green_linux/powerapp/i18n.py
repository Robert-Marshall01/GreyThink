"""Internationalization helper for PowerApp.

Provides a safe setup_gettext() function that binds the translation domain and points
at the project's `locale/` directory. This is safe to call at import time and will
not raise if gettext or locale files are not available.
"""
from pathlib import Path
import gettext

DOMAIN = 'powerapp'


def get_locale_dir() -> Path:
    # locale directory at project root: powerapp/../locale
    here = Path(__file__).resolve().parent
    return here.parent / 'locale'


def setup_gettext(domain: str = DOMAIN, localedir: str = None):
    try:
        ld = localedir or str(get_locale_dir())
        gettext.bindtextdomain(domain, ld)
        gettext.textdomain(domain)
        # install '_' into builtins for convenience in modules that call gettext.gettext
        gettext.install(domain, ld, names=('ngettext',))
    except Exception:
        # best-effort: do not fail setup on missing binaries or directories
        try:
            # fallback: ensure _ exists and returns input
            import builtins
            builtins._ = lambda s: s
        except Exception:
            pass


# Provide a thin convenience wrapper for modules
def _t(msg: str) -> str:
    try:
        return gettext.gettext(msg)
    except Exception:
        return msg
