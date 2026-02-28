#!/usr/bin/env python3
"""
Grey Physics — Cross-Platform Installer Builder

Detects the current platform and launches the appropriate installer or
uninstaller. Can also package installers into distributable archives.

Usage:
    python build_installers.py install   [--all-users] [--extras EXTRAS]
    python build_installers.py uninstall [--install-dir DIR]
    python build_installers.py package   [--output-dir DIR]
"""
from __future__ import annotations

import argparse
import os
import platform
import shutil
import subprocess
import sys
import tarfile
import zipfile
from pathlib import Path

# ── Constants ──────────────────────────────────────────────────────────
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent
APP_NAME = "grey-physics"


def detect_platform() -> str:
    """Return 'windows', 'linux', or 'macos'."""
    system = platform.system().lower()
    if system == "darwin":
        return "macos"
    if system == "windows":
        return "windows"
    if system == "linux":
        return "linux"
    print(f"ERROR: Unsupported platform: {system}", file=sys.stderr)
    sys.exit(1)


def run_installer(plat: str, extra_args: list[str]) -> int:
    """Run the platform-specific installer."""
    if plat == "windows":
        script = SCRIPT_DIR / "windows" / "install.ps1"
        cmd = [
            "powershell.exe", "-NoProfile", "-ExecutionPolicy", "Bypass",
            "-File", str(script),
        ] + extra_args
    elif plat == "linux":
        script = SCRIPT_DIR / "linux" / "install.sh"
        cmd = ["bash", str(script)] + extra_args
    elif plat == "macos":
        script = SCRIPT_DIR / "macos" / "install.sh"
        cmd = ["bash", str(script)] + extra_args
    else:
        print(f"ERROR: Unknown platform: {plat}", file=sys.stderr)
        return 1

    if not script.exists():
        print(f"ERROR: Installer script not found: {script}", file=sys.stderr)
        return 1

    print(f"Running {plat} installer: {script}")
    return subprocess.call(cmd)


def run_uninstaller(plat: str, extra_args: list[str]) -> int:
    """Run the platform-specific uninstaller."""
    if plat == "windows":
        script = SCRIPT_DIR / "windows" / "uninstall.ps1"
        cmd = [
            "powershell.exe", "-NoProfile", "-ExecutionPolicy", "Bypass",
            "-File", str(script),
        ] + extra_args
    elif plat == "linux":
        script = SCRIPT_DIR / "linux" / "uninstall.sh"
        cmd = ["bash", str(script)] + extra_args
    elif plat == "macos":
        script = SCRIPT_DIR / "macos" / "uninstall.sh"
        cmd = ["bash", str(script)] + extra_args
    else:
        print(f"ERROR: Unknown platform: {plat}", file=sys.stderr)
        return 1

    if not script.exists():
        print(f"ERROR: Uninstaller script not found: {script}", file=sys.stderr)
        return 1

    print(f"Running {plat} uninstaller: {script}")
    return subprocess.call(cmd)


def package_installers(output_dir: Path) -> None:
    """
    Create distributable archives for all platforms.

    Produces:
      - grey-physics-installer-windows.zip   (contains install.bat/ps1, uninstall.bat/ps1)
      - grey-physics-installer-linux.tar.gz  (contains install.sh, uninstall.sh)
      - grey-physics-installer-macos.tar.gz  (contains install.sh, uninstall.sh)
      - grey-physics-installer-all.zip       (contains everything + build_installers.py)
    """
    output_dir.mkdir(parents=True, exist_ok=True)

    platforms_config = {
        "windows": {
            "dir": SCRIPT_DIR / "windows",
            "files": ["install.ps1", "uninstall.ps1", "install.bat", "uninstall.bat"],
            "archive_type": "zip",
        },
        "linux": {
            "dir": SCRIPT_DIR / "linux",
            "files": ["install.sh", "uninstall.sh"],
            "archive_type": "tar.gz",
        },
        "macos": {
            "dir": SCRIPT_DIR / "macos",
            "files": ["install.sh", "uninstall.sh"],
            "archive_type": "tar.gz",
        },
    }

    for plat, config in platforms_config.items():
        src_dir = config["dir"]
        files = config["files"]
        archive_type = config["archive_type"]
        archive_name = f"{APP_NAME}-installer-{plat}"

        # Verify all files exist
        missing = [f for f in files if not (src_dir / f).exists()]
        if missing:
            print(f"WARNING: Missing files for {plat}: {missing}")
            continue

        if archive_type == "zip":
            archive_path = output_dir / f"{archive_name}.zip"
            with zipfile.ZipFile(archive_path, "w", zipfile.ZIP_DEFLATED) as zf:
                for f in files:
                    zf.write(src_dir / f, f"{archive_name}/{f}")
            print(f"Created: {archive_path}")

        elif archive_type == "tar.gz":
            archive_path = output_dir / f"{archive_name}.tar.gz"
            with tarfile.open(archive_path, "w:gz") as tf:
                for f in files:
                    tf.add(src_dir / f, f"{archive_name}/{f}")
            print(f"Created: {archive_path}")

    # Create all-in-one archive
    all_archive = output_dir / f"{APP_NAME}-installer-all.zip"
    with zipfile.ZipFile(all_archive, "w", zipfile.ZIP_DEFLATED) as zf:
        # Add build_installers.py
        zf.write(SCRIPT_DIR / "build_installers.py", "installers/build_installers.py")
        # Add README
        readme_path = SCRIPT_DIR / "README.md"
        if readme_path.exists():
            zf.write(readme_path, "installers/README.md")
        # Add all platform scripts
        for plat, config in platforms_config.items():
            for f in config["files"]:
                src = config["dir"] / f
                if src.exists():
                    zf.write(src, f"installers/{plat}/{f}")
    print(f"Created: {all_archive}")

    print(f"\nAll archives written to: {output_dir}")


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Grey Physics Cross-Platform Installer Builder",
    )
    subparsers = parser.add_subparsers(dest="command", help="Command to run")

    # install subcommand
    install_parser = subparsers.add_parser("install", help="Run the installer")
    install_parser.add_argument("--all-users", action="store_true",
                                help="Install for all users (requires elevated privileges)")
    install_parser.add_argument("--install-dir", help="Custom install directory")
    install_parser.add_argument("--extras", help="Optional extras: gpu, viz, ide, all, dev")

    # uninstall subcommand
    uninstall_parser = subparsers.add_parser("uninstall", help="Run the uninstaller")
    uninstall_parser.add_argument("--install-dir", help="Installation directory to remove")

    # package subcommand
    package_parser = subparsers.add_parser("package",
                                           help="Package installers into distributable archives")
    package_parser.add_argument("--output-dir", default="dist/installers",
                                help="Output directory for archives (default: dist/installers)")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return 1

    plat = detect_platform()
    print(f"Detected platform: {plat}")

    if args.command == "install":
        extra_args = []
        if args.all_users:
            extra_args.append("--all-users" if plat != "windows" else "-AllUsers")
        if args.install_dir:
            if plat == "windows":
                extra_args.extend(["-InstallDir", args.install_dir])
            else:
                extra_args.extend(["--install-dir", args.install_dir])
        if args.extras:
            if plat == "windows":
                extra_args.extend(["-Extras", args.extras])
            else:
                extra_args.extend(["--extras", args.extras])
        return run_installer(plat, extra_args)

    elif args.command == "uninstall":
        extra_args = []
        if args.install_dir:
            if plat == "windows":
                extra_args.extend(["-InstallDir", args.install_dir])
            else:
                extra_args.extend(["--install-dir", args.install_dir])
        return run_uninstaller(plat, extra_args)

    elif args.command == "package":
        package_installers(Path(args.output_dir))
        return 0

    return 1


if __name__ == "__main__":
    sys.exit(main())
