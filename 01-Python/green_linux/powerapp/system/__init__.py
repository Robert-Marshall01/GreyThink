# Expose system submodules for easier testing and monkeypatching
# This ensures that `powerapp.system.integration` and `powerapp.system.bugreport`
# are available as attributes on the package object.

try:
    from . import integration  # type: ignore
except Exception:
    integration = None

try:
    from . import bugreport  # type: ignore
except Exception:
    bugreport = None

__all__ = ['integration', 'bugreport']
