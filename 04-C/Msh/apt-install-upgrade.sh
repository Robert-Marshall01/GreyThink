#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
# Small helper script to update, upgrade and optionally install packages on Debian/Ubuntu
# Usage:
#   ./apt-install-upgrade.sh           # runs update && upgrade
#   ./apt-install-upgrade.sh pkg1 pkg2 # update, upgrade, then install pkg1 pkg2
# Requires: run as root or with sudo

set -euo pipefail

if [[ $(id -u) -ne 0 ]]; then
  echo "This script must be run as root. Re-run with sudo or as root."
  exit 1
fi

# Update package lists
echo "Updating package lists..."
apt-get update -qq

# Upgrade packages non-interactively
echo "Upgrading installed packages..."
DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::=--force-confdef -o Dpkg::Options::=--force-confold upgrade

# If packages provided, install them
if [[ $# -gt 0 ]]; then
  echo "Installing packages: $*"
  DEBIAN_FRONTEND=noninteractive apt-get -y install "$@"
fi

# Clean up
echo "Removing unused packages..."
apt-get -y autoremove
apt-get -y autoclean

echo "Done."
