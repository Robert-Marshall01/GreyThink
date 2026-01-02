#!/usr/bin/env python3
"""
GreyAV Socket Intake Module

Provides multi-port network listeners for receiving threat probes and external
security events. Monitors critical ports (SSH, DHCP, HTTP, HTTPS) automatically.
Starts with GreyAV service.
"""

import socket
import threading
import logging
import signal
import sys
import json
from datetime import datetime
from pathlib import Path

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('GreyAV.SocketIntake')

# Default ports to monitor
DEFAULT_PORTS = [
    # GreyAV
    8000,
    # Remote Access
    22, 23, 2222, 3389, 5900,
    # File Transfer
    20, 21,
    # Web
    80, 443, 8080, 8443,
    # Email
    25, 110, 143,
    # DNS
    53,
    # DHCP
    67, 68,
    # Windows/SMB
    135, 139, 445,
    # SNMP
    161, 162,
    # Databases
    3306, 5432, 27017, 6379,
    # Backdoor/Exploit
    4444,
]

# Port descriptions for logging
PORT_DESCRIPTIONS = {
    # GreyAV
    8000: 'GreyAV-Primary',
    # Remote Access
    22: 'SSH',
    23: 'Telnet',
    2222: 'SSH-Alt',
    3389: 'RDP',
    5900: 'VNC',
    # File Transfer
    20: 'FTP-Data',
    21: 'FTP-Control',
    # Web
    80: 'HTTP',
    443: 'HTTPS',
    8080: 'HTTP-Alt',
    8443: 'HTTPS-Alt',
    # Email
    25: 'SMTP',
    110: 'POP3',
    143: 'IMAP',
    # DNS
    53: 'DNS',
    # DHCP
    67: 'DHCP-Server',
    68: 'DHCP-Client',
    # Windows/SMB
    135: 'MS-RPC',
    139: 'NetBIOS',
    445: 'SMB',
    # SNMP
    161: 'SNMP',
    162: 'SNMP-Trap',
    # Databases
    3306: 'MySQL',
    5432: 'PostgreSQL',
    27017: 'MongoDB',
    6379: 'Redis',
    # Backdoor/Exploit
    4444: 'Metasploit',
}

# Global state for graceful shutdown
_listener_sockets = {}  # port -> socket
_listener_threads = {}  # port -> thread
_running = False
_lock = threading.Lock()


def get_port_description(port):
    """Get human-readable description for a port."""
    return PORT_DESCRIPTIONS.get(port, f'Port-{port}')


def handle_client(conn, addr, port):
    """Handle incoming client connections."""
    try:
        data = conn.recv(4096).decode('utf-8', errors='replace')
        port_desc = get_port_description(port)
        logger.info("[GreyAV] Threat probe on %s (port %d) from %s: %s",
                    port_desc, port, addr, data[:200])

        # Log to file for persistence
        log_entry = {
            'timestamp': datetime.now().isoformat(),
            'source_ip': addr[0],
            'source_port': addr[1],
            'dest_port': port,
            'service': port_desc,
            'data': data[:1000],
            'action': 'blocked_and_logged'
        }

        log_file = Path(__file__).parent / 'socket_intake.log'
        with open(log_file, 'a') as f:
            f.write(json.dumps(log_entry) + '\n')

        conn.send(f"GreyAV: Threat on {port_desc} blocked and logged!\n".encode())
    except Exception as e:
        logger.error("Error handling client %s on port %d: %s", addr, port, e)
    finally:
        try:
            conn.close()
        except Exception:
            pass


def _create_port_listener(host, port):
    """Create a listener loop for a specific port."""
    global _running

    def listener_loop():
        global _running
        port_desc = get_port_description(port)

        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            sock.settimeout(1.0)  # Allow periodic checks for shutdown
            sock.bind((host, port))
            sock.listen(10)

            with _lock:
                _listener_sockets[port] = sock

            logger.info("[GreyAV] Socket Intake listening on %s:%d (%s)",
                        host, port, port_desc)

            while _running:
                try:
                    conn, addr = sock.accept()
                    client_thread = threading.Thread(
                        target=handle_client,
                        args=(conn, addr, port),
                        daemon=True
                    )
                    client_thread.start()
                except socket.timeout:
                    continue
                except OSError as e:
                    if _running:
                        logger.error("Socket error on port %d: %s", port, e)
                    break

        except PermissionError:
            logger.warning("[GreyAV] Permission denied for port %d (%s) - requires root",
                           port, port_desc)
        except OSError as e:
            if "Address already in use" in str(e):
                logger.warning("[GreyAV] Port %d (%s) already in use - skipping",
                               port, port_desc)
            else:
                logger.error("[GreyAV] Failed to bind on %s:%d - %s", host, port, e)
        finally:
            with _lock:
                if port in _listener_sockets:
                    try:
                        _listener_sockets[port].close()
                    except Exception:
                        pass
                    del _listener_sockets[port]
            logger.debug("[GreyAV] Listener on port %d stopped", port)

    return listener_loop


def start_listener(host='0.0.0.0', port=None, ports=None, blocking=True):
    """
    Start the socket intake listener(s).

    Args:
        host: IP address to bind to (default: 0.0.0.0)
        port: Single port number (for backwards compatibility)
        ports: List of ports to listen on (default: DEFAULT_PORTS)
        blocking: If True, blocks the calling thread. If False, runs in background.

    Returns:
        If blocking=False, returns dict of port -> thread mappings.
    """
    global _running, _listener_threads

    if _running:
        logger.warning("Socket listeners are already running")
        return _listener_threads

    # Determine which ports to listen on
    if ports is not None:
        listen_ports = list(ports)
    elif port is not None:
        listen_ports = [port]
    else:
        listen_ports = DEFAULT_PORTS.copy()

    _running = True
    started_ports = []

    # Start a listener for each port
    for p in listen_ports:
        listener_loop = _create_port_listener(host, p)
        thread = threading.Thread(
            target=listener_loop,
            daemon=True,
            name=f"GreyAV-SocketIntake-{p}"
        )
        thread.start()
        _listener_threads[p] = thread
        started_ports.append(p)

    if started_ports:
        logger.info("[GreyAV] Socket Intake started on %d ports: %s",
                    len(started_ports),
                    ", ".join(f"{p} ({get_port_description(p)})" for p in started_ports))

    if blocking:
        # Wait for all threads to finish (they won't unless stopped)
        try:
            while _running:
                # Check if any threads are still alive
                alive = any(t.is_alive() for t in _listener_threads.values())
                if not alive:
                    break
                threading.Event().wait(1.0)
        except KeyboardInterrupt:
            stop_listener()
    else:
        return _listener_threads


def stop_listener():
    """Stop all socket listeners gracefully."""
    global _running

    logger.info("[GreyAV] Stopping Socket Intake...")
    _running = False

    # Close all sockets to break accept() calls
    with _lock:
        for port, sock in list(_listener_sockets.items()):
            try:
                sock.close()
            except Exception:
                pass

    # Wait for all threads to finish
    for port, thread in list(_listener_threads.items()):
        if thread.is_alive():
            thread.join(timeout=2)

    _listener_threads.clear()
    logger.info("[GreyAV] Socket Intake stopped")


def is_running():
    """Check if any listener is currently running."""
    return _running and any(t.is_alive() for t in _listener_threads.values())


def get_active_ports():
    """Get list of ports currently being listened on."""
    with _lock:
        return list(_listener_sockets.keys())


def signal_handler(signum, _frame):
    """Handle shutdown signals gracefully."""
    logger.info("Received signal %s, shutting down...", signum)
    stop_listener()
    sys.exit(0)


if __name__ == "__main__":
    # Register signal handlers for graceful shutdown
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    # Parse optional command-line arguments
    import argparse
    parser = argparse.ArgumentParser(
        description='GreyAV Multi-Port Socket Intake Listener'
    )
    parser.add_argument('--host', default='0.0.0.0',
                        help='Host to bind to')
    parser.add_argument('--port', type=int, default=None,
                        help='Single port to listen on (legacy mode)')
    parser.add_argument('--ports', type=str, default=None,
                        help='Comma-separated list of ports to listen on')
    parser.add_argument('--all-ports', action='store_true',
                        help='Listen on all default ports (22,67,68,80,443,2222,8000)')
    args = parser.parse_args()

    # Determine ports to listen on
    if args.ports:
        ports_list = [int(p.strip()) for p in args.ports.split(',')]
    elif args.all_ports or args.port is None:
        ports_list = DEFAULT_PORTS
    else:
        ports_list = [args.port]

    print(f"[GreyAV] Starting Socket Intake on ports: {ports_list}")
    print("[GreyAV] Note: Ports < 1024 require root privileges")
    start_listener(host=args.host, ports=ports_list, blocking=True)
