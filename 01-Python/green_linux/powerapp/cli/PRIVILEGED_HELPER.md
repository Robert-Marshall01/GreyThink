Privileged helper design (notes)

Goal
----
Allow non-root sampling of RAPL energy counters so the GUI can show system power without needing to run the whole app as root.

Options
-------
1) Systemd root helper unit + unix socket
   - Install a small root-owned service (system unit) that exposes a unix socket (e.g., /run/powerapp/rapl.sock)
   - The helper reads RAPL counters and responds to simple sample requests.
   - Communicates via simple JSON RPC or line-based protocol.
   - Installation done via post-install hook in snap or via package; privilege escalation limited to the helper.

2) Polkit + DBus activation
   - Write a system service activated via polkit requests that performs a single sample on-demand.
   - More integrated with system security model but slightly more complex to implement.

3) sudo wrapper (least preferred)
   - Use a sudoers rule to allow a specific binary to be run without password by a user or group. Simpler, but less polished.

Security notes
--------------
- Limit the helper to only read required files under /sys/devices/virtual/powercap and not expose arbitrary filesystem access.
- Provide a manifest of operations and minimize attack surface.
- Use socket ownership and file permissions to restrict access to local user sessions.

Next steps
----------
- Implement a minimal systemd helper prototype that supports 'sample' RPC and returns JSON with energy counters and timestamp.
- Create Snap hooks to install and enable the unit at snap install time (snapd endorses system services via helpers).