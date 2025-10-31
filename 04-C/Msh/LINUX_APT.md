# APT install & upgrade examples

This small reference shows common Debian/Ubuntu apt commands for installing and upgrading packages. Use these on Debian-based Linux distributions (Debian, Ubuntu, Mint, Pop!_OS, etc.).

Important notes
- These commands require root privileges. Prefix with `sudo` or run as root.
- On some systems `apt` is an interactive frontend; for scripting prefer `DEBIAN_FRONTEND=noninteractive apt-get` or `apt-get` with `-y`.

Common commands

- Update package lists (recommended before installing or upgrading):

  sudo apt update

- Upgrade all installed packages (safe, installs newest versions available):

  sudo apt upgrade

- Full upgrade (may remove packages to satisfy dependencies):

  sudo apt full-upgrade

- Install a package (example: curl):

  sudo apt install curl

- Install multiple packages:

  sudo apt install build-essential git curl

- Install without prompts (non-interactive):

  sudo apt -y install package-name

- Remove a package (keep config files):

  sudo apt remove package-name

- Purge a package (remove config files too):

  sudo apt purge package-name

- Autoremove unused dependencies:

  sudo apt autoremove

Scripting tips

- Non-interactive environment (avoid prompts):

  DEBIAN_FRONTEND=noninteractive sudo apt-get -yq install package-name

- Update and upgrade in one line:

  sudo apt update && sudo apt -y upgrade

Example: update lists, upgrade all packages, then install git:

  sudo apt update && sudo apt -y upgrade && sudo apt -y install git

- Isolating commands inline (the semicolon `;`)

  - Use `;` to run commands sequentially regardless of the previous command's exit status. This is useful when you want later commands to run even if an earlier command fails.

    Example — update package lists, then attempt to install `curl` even if the update fails:

      sudo apt update; sudo apt -y install curl

  - Contrast with `&&` which only runs the next command if the previous one succeeded (exit status 0). Use `;` when you explicitly want isolation between commands.

  - You can combine separators in a single line for more complex behavior. For example, run update and always try install, but only upgrade on success:

      sudo apt update; sudo apt -y install curl && sudo apt -y upgrade

License

This file is provided as-is for documentation purposes.

Forwarding from Windows `Msh`
--------------------------------
If you're running `Msh.exe` on Windows, the shell can optionally forward Linux package commands to WSL so you can type `apt` directly in `Msh` and have the command executed inside your WSL distro.

Environment variables
- `MSH_FORWARD_LINUX_CMDS` (optional): if set to `0` or `off` forwarding is disabled. If unset or set to `1`/`true`/`yes` forwarding is enabled by default.
- `MSH_WSL_DISTRO` (optional): set to the name of your WSL distro (for example `Ubuntu`) to force `wsl -d <distro> -- ...`.

Runtime toggle in `Msh`
- Use the built-in command `forward-wsl` inside `Msh` to control forwarding at runtime:
  - `forward-wsl on` — enable forwarding
  - `forward-wsl off` — disable forwarding
  - `forward-wsl status` — show current status (or just `forward-wsl`)

Notes
- Forwarding detects these commands and runs them in WSL: `apt`, `apt-get`, `dpkg`, `apt-key`, and user/group management commands (`useradd`, `adduser`, `userdel`, `deluser`, `groupadd`, `addgroup`, `groupdel`, `delgroup`).
- Ensure WSL is installed and a distro is available. You can test with `where wsl` in PowerShell or `wsl -l -v`.

User and group commands
------------------------

Below are common Linux commands for creating and removing users and groups. These are standard on Debian/Ubuntu systems; some distros provide slightly different utilities. All of these require root privileges — prefix with `sudo` or run as root.

useradd (low-level)
- Synopsis: `sudo useradd [options] USERNAME`
- Description: Low-level tool to create a new user account. It typically only creates the account and home directory (when used with `-m`) and does not set a password.
- Common options:
  - `-m` create home directory
  - `-s /bin/bash` set login shell
  - `-G group1,group2` supplementary groups
  - `-c "Full Name"` comment / GECOS
- Example:

  sudo useradd -m -s /bin/bash -c "Alice Example" -G sudo alice

- After creating with `useradd`, set an initial password with:

  sudo passwd alice

adduser (friendly wrapper)
- Synopsis: `sudo adduser USERNAME`
- Description: High-level, interactive script that wraps `useradd` and prompts for password and user information. Preferred for interactive setup on Debian/Ubuntu.
- Example:

  sudo adduser bob

  # follow prompts to set password and details

userdel (remove user)
- Synopsis: `sudo userdel [options] USERNAME`
- Description: Remove a user account. By default it removes the account but leaves the home directory and mail spool.
- Common options:
  - `-r` remove home directory and mail spool
- Example:

  sudo userdel -r olduser

deluser (friendly wrapper)
- Synopsis: `sudo deluser USERNAME`
- Description: High-level wrapper that removes a user and can also remove home directories or remove the user from a group. On Debian/Ubuntu it is often preferred for interactive use.
- Example (remove user and home):

  sudo deluser --remove-home charlie

groupadd (create group)
- Synopsis: `sudo groupadd [options] GROUPNAME`
- Description: Create a new group.
- Common options:
  - `-g GID` specify numeric group id
- Example:

  sudo groupadd developers

addgroup (friendly wrapper)
- Synopsis: `sudo addgroup GROUPNAME`
- Description: Debian/Ubuntu wrapper around `groupadd`. `addgroup` may also create a system group when used with `--system`.
- Example:

  sudo addgroup projectx

groupdel (remove group)
- Synopsis: `sudo groupdel GROUPNAME`
- Description: Remove a group from the system. Will fail if the group is still in use as a primary group by a user (some systems allow force options).
- Example:

  sudo groupdel oldgroup

delgroup (friendly wrapper)
- Synopsis: `sudo delgroup GROUPNAME`
- Description: Debian/Ubuntu wrapper to remove groups. Can be used to remove a group's files and membership in scripts.
- Example:

  sudo delgroup tempgroup

Notes and best practices
- Prefer `adduser` / `deluser` on Debian-based systems for interactive workflows. Use `useradd` / `userdel` / `groupadd` / `groupdel` for scripting when you need precise option control.
- Always check `/etc/passwd`, `/etc/shadow`, and `/etc/group` for the canonical records when debugging user/group issues.
- When scripting user creation, ensure you set a secure password (`chpasswd` can be used in scripts) and configure SSH keys or other authentication methods rather than leaving accounts with weak passwords.
