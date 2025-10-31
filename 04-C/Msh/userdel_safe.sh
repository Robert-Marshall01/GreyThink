#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
# userdel_safe.sh - safe wrapper for deleting users on Linux
# Usage: ./userdel_safe.sh [-r] USERNAME
# -r : remove home directory
# This script will:
# 1) Refuse to delete the running user or root
# 2) If run as root, call /usr/sbin/userdel (or userdel on PATH)
# 3) If not root, attempt to use sudo to run userdel
# 4) If sudo is not available or user declines, offer to emulate deletion by
#    removing the username from .msh_users (local emulation file)

#!/usr/bin/env bash
set -euo pipefail

REMOVE_HOME=0
USERNAME=""
FORCE=0


usage() {
  echo "Usage: $0 [-r] [-f] USERNAME"
  echo "  -r  remove home directory"
  echo "  -f  force (skip interactive confirmation)"
  exit 2
}

while getopts ":rf" opt; do
  case "$opt" in
    r) REMOVE_HOME=1 ;;
    f) FORCE=1 ;;
    \?) usage ;;
  esac
done
shift $((OPTIND-1))

if [ $# -ne 1 ]; then
  usage
fi

USERNAME="$1"

# Protect root and current user
CURRENT_USER=$(id -un)
if [ "$USERNAME" = "root" ] || [ "$USERNAME" = "$CURRENT_USER" ]; then
  echo "Refusing to delete protected account: $USERNAME"
  exit 1
fi

# Decide remove-home option
if [ "$REMOVE_HOME" -eq 1 ]; then
  UDEL_OPTS="-r"
else
  UDEL_OPTS=""
fi

# detect userdel
USERDEL_CMD=""
if command -v userdel >/dev/null 2>&1; then
  USERDEL_CMD=$(command -v userdel)
fi

# If running as root, run userdel directly
confirm_or_abort() {
  # If forced, skip confirmation
  if [ "$FORCE" -eq 1 ]; then
    return 0
  fi
  # interactive confirmation: require typing username to proceed
  if [ -t 0 ]; then
    echo -n "Type the username to confirm deletion of '$USERNAME' (or Ctrl-C to abort): "
    read -r reply || { echo; echo "Aborted."; exit 3; }
    if [ "$reply" != "$USERNAME" ]; then
      echo "Confirmation mismatch; aborting."
      exit 3
    fi
  else
    echo "Non-interactive shell and no --force; refusing to proceed. Use -f to force." >&2
    exit 3
  fi
}

if [ "$(id -u)" -eq 0 ] && [ -n "$USERDEL_CMD" ]; then
  echo "Preparing to run system userdel for $USERNAME"
  confirm_or_abort
  if $USERDEL_CMD $UDEL_OPTS -- "$USERNAME"; then
    echo "User $USERNAME deleted (system)."
    exit 0
  else
    echo "System userdel failed." >&2
  fi
fi

# If not root, try sudo
if [ -n "$USERDEL_CMD" ]; then
  if command -v sudo >/dev/null 2>&1; then
    echo "Attempting to run 'sudo $USERDEL_CMD $UDEL_OPTS -- $USERNAME'"
    confirm_or_abort
    if sudo $USERDEL_CMD $UDEL_OPTS -- "$USERNAME"; then
      echo "User $USERNAME deleted (via sudo)."
      exit 0
    else
      echo "sudo userdel failed or was denied." >&2
    fi
  fi
fi

# Fallback: emulate deletion in .msh_users
MSH_USERS_FILE=".msh_users"
if [ -f "$MSH_USERS_FILE" ]; then
  if grep -Fxq "$USERNAME" "$MSH_USERS_FILE"; then
    echo "Removing emulated user $USERNAME from $MSH_USERS_FILE"
    confirm_or_abort
    # safe temp file via mktemp
    TMPFILE=$(mktemp "${MSH_USERS_FILE}.tmp.XXXX")
    grep -Fxv "$USERNAME" "$MSH_USERS_FILE" > "$TMPFILE" && mv "$TMPFILE" "$MSH_USERS_FILE"
    echo "Emulated user $USERNAME removed."
    exit 0
  else
    echo "No emulated entry for $USERNAME found in $MSH_USERS_FILE"
    exit 1
  fi
else
  echo "No .msh_users file found; nothing to emulate."
  exit 1
fi
