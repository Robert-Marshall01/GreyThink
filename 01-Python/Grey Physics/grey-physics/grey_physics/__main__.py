"""Entry point for ``python -m grey_physics``."""

from __future__ import annotations

import argparse
import sys


def main() -> None:
    parser = argparse.ArgumentParser(
        prog="grey-physics",
        description="Grey Physics — Physics & Mathematics IDE",
    )
    parser.add_argument(
        "--no-gui", action="store_true", default=False,
        help="Print help and exit instead of launching the GUI",
    )
    parser.add_argument(
        "--version", action="store_true", default=False,
        help="Show version and exit",
    )
    args = parser.parse_args()

    if args.version:
        import grey_physics
        print(f"Grey Physics v{grey_physics.__version__}")
        sys.exit(0)

    if args.no_gui:
        parser.print_help()
        sys.exit(0)

    # Default: launch the GUI
    from grey_physics.ide.gui import launch
    launch()


if __name__ == "__main__":
    main()
