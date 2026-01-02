#!/usr/bin/env python3
"""
GreyAV Main Entry Point

This is the main entry point for the GreyAV antivirus/EDR system.
It initializes and runs the core components including Socket Intake.
Runs automatically on system startup when installed as a service.
"""

import logging
import sys
import signal
import time
import os
from pathlib import Path

# Add installation directory to path
install_dir = Path(__file__).parent
sys.path.insert(0, str(install_dir))

# Configure logging
log_dir = install_dir / 'logs'
log_dir.mkdir(exist_ok=True)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(sys.stdout),
        logging.FileHandler(log_dir / 'greyav_service.log')
    ]
)

logger = logging.getLogger('GreyAV')

# Global state
running = True
socket_intake_started = False
av_instance = None
real_time_monitor = None


def signal_handler(signum, frame):
    """Handle shutdown signals gracefully."""
    global running
    logger.info("Received signal %s, initiating graceful shutdown...", signum)
    running = False


def start_real_time_protection():
    """Start real-time file system monitoring."""
    global av_instance, real_time_monitor
    
    try:
        from greyav import GreyAV, RealTimeMonitor, WATCHDOG_AVAILABLE
        
        if not WATCHDOG_AVAILABLE:
            logger.warning("Watchdog not available - real-time monitoring disabled")
            return False
        
        # Initialize AV engine
        av_instance = GreyAV()
        
        # Default paths to monitor
        monitor_paths = ['/home', '/tmp', '/var/tmp']
        existing_paths = [p for p in monitor_paths if os.path.exists(p)]
        
        if not existing_paths:
            existing_paths = [str(install_dir)]
        
        real_time_monitor = RealTimeMonitor(av_instance, existing_paths, recursive=True)
        real_time_monitor.start()
        
        logger.info("Real-time protection started for: %s", ', '.join(existing_paths))
        return True
        
    except ImportError as e:
        logger.warning("Could not start real-time protection: %s", e)
        return False
    except Exception as e:
        logger.error("Error starting real-time protection: %s", e)
        return False


def stop_real_time_protection():
    """Stop real-time monitoring."""
    global real_time_monitor
    
    if real_time_monitor:
        try:
            real_time_monitor.stop()
            logger.info("Real-time protection stopped")
        except Exception as e:
            logger.error("Error stopping real-time protection: %s", e)


def main():
    """Main entry point for GreyAV service."""
    global running, socket_intake_started
    
    # Register signal handlers
    signal.signal(signal.SIGTERM, signal_handler)
    signal.signal(signal.SIGINT, signal_handler)
    
    logger.info("=" * 60)
    logger.info("GreyAV Antivirus/EDR System Starting...")
    logger.info("=" * 60)
    logger.info("PID: %s", os.getpid())
    logger.info("Working Directory: %s", install_dir)
    
    # Track loaded modules
    modules_loaded = []
    
    # Load core modules
    try:
        from greyav import GreyAV
        modules_loaded.append("GreyAV Core")
    except ImportError as e:
        logger.error("GreyAV core module not found: %s", e)
        logger.error("Cannot start without core module")
        return 1
    
    # Load optional modules
    try:
        from behavioral_engine import BehavioralEngine
        modules_loaded.append("Behavioral Engine")
    except ImportError:
        pass
    
    try:
        from av_immune_system import AVImmuneSystem
        modules_loaded.append("AV Immune System")
    except ImportError:
        pass
    
    try:
        from auto_threat_manager import AutoThreatManager
        modules_loaded.append("Auto Threat Manager")
    except ImportError:
        pass
    
    # Start Socket Intake listener on all default ports
    try:
        from socket_intake import (
            start_listener, stop_listener, is_running,
            get_active_ports, DEFAULT_PORTS, PORT_DESCRIPTIONS
        )
        logger.info("Initializing Socket Intake for %d ports...", len(DEFAULT_PORTS))
        start_listener(host='0.0.0.0', ports=DEFAULT_PORTS, blocking=False)
        socket_intake_started = True
        
        # Give listeners time to start
        time.sleep(0.5)
        active = get_active_ports()
        failed_ports = set(DEFAULT_PORTS) - set(active)
        
        if active:
            modules_loaded.append(f"Socket Intake ({len(active)}/{len(DEFAULT_PORTS)} ports)")
            logger.info("Socket Intake ACTIVE on %d ports: %s", 
                       len(active),
                       ', '.join(f"{p}({PORT_DESCRIPTIONS.get(p, 'Unknown')})" 
                                for p in sorted(active)))
        
        if failed_ports:
            priv_failed = [p for p in failed_ports if p < 1024]
            other_failed = [p for p in failed_ports if p >= 1024]
            if priv_failed:
                logger.warning("Privileged ports require root: %s",
                              ', '.join(str(p) for p in sorted(priv_failed)))
            if other_failed:
                logger.warning("Failed to bind ports: %s",
                              ', '.join(str(p) for p in sorted(other_failed)))
        
        if len(active) == len(DEFAULT_PORTS):
            logger.info("All %d ports successfully bound", len(DEFAULT_PORTS))
            
    except ImportError:
        logger.warning("Socket Intake module not available")
    except Exception as e:
        logger.warning("Failed to start Socket Intake: %s", e)
    
    # Start real-time protection
    if start_real_time_protection():
        modules_loaded.append("Real-Time Protection")
    
    logger.info("Loaded modules: %s", ', '.join(modules_loaded) or 'None')
    logger.info("GreyAV service is running")
    logger.info("=" * 60)
    
    # Write PID file
    pid_file = install_dir / 'greyav.pid'
    try:
        pid_file.write_text(str(os.getpid()))
    except Exception:
        pass
    
    # Main service loop
    heartbeat_interval = 60  # seconds
    last_heartbeat = time.time()
    
    while running:
        try:
            time.sleep(1)
            
            # Periodic heartbeat
            if time.time() - last_heartbeat >= heartbeat_interval:
                logger.debug("Service heartbeat - uptime: %d seconds", 
                           int(time.time() - last_heartbeat))
                last_heartbeat = time.time()
                
        except Exception as e:
            logger.error("Error in main loop: %s", e)
            time.sleep(5)
    
    # Cleanup
    logger.info("Shutting down GreyAV service...")
    
    stop_real_time_protection()
    
    if socket_intake_started:
        try:
            from socket_intake import stop_listener
            stop_listener()
            logger.info("Socket Intake stopped")
        except Exception:
            pass
    
    # Remove PID file
    try:
        pid_file.unlink(missing_ok=True)
    except Exception:
        pass
    
    logger.info("GreyAV service stopped")
    return 0


if __name__ == "__main__":
    sys.exit(main())
