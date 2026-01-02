#!/usr/bin/env python3
"""
Behavioral Detection Engine for GreyAV
=======================================

A lightweight, modular behavioral detection system that uses micro-sensors
to detect suspicious patterns and a correlation engine to raise alerts.

Architecture:
    Sensors → EventBus → CorrelationEngine → Alerts

Usage:
    python3 behavioral_engine.py [--verbose] [--alert-threshold N]

Author: GreyAV Team
License: MIT
"""

import abc
import argparse
import hashlib
import logging
import math
import os
import queue
import random
import socket
import struct
import sys
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Optional, Set, Tuple

# Import centralized port management
try:
    from port_manager import get_port_manager, PortManager
    PORT_MANAGER_AVAILABLE = True
except ImportError:
    PORT_MANAGER_AVAILABLE = False
    get_port_manager = None
    PortManager = None


# =============================================================================
# Configuration
# =============================================================================

class Config:
    """Global configuration for the behavioral engine."""
    
    # Event Bus settings
    EVENT_QUEUE_SIZE: int = 10000
    EVENT_LOG_FILE: str = "behavioral_events.log"
    
    # Correlation thresholds
    HIGH_ENTROPY_THRESHOLD: int = 5      # alerts after N high-entropy writes
    HIGH_ENTROPY_WINDOW: float = 60.0    # within N seconds
    RAPID_SPAWN_THRESHOLD: int = 10      # alerts after N rapid spawns
    RAPID_SPAWN_WINDOW: float = 5.0      # within N seconds
    OUTBOUND_CONN_THRESHOLD: int = 3     # alerts after N unknown connections
    OUTBOUND_CONN_WINDOW: float = 30.0   # within N seconds
    
    # Sensor settings
    ENTROPY_THRESHOLD: float = 7.5       # bits per byte (max 8.0)
    SPAWN_RATE_THRESHOLD: float = 5.0    # processes per second
    
    # Known safe destinations (for outbound connection sensor)
    KNOWN_SAFE_IPS: Set[str] = {
        "127.0.0.1", "::1",              # localhost
        "8.8.8.8", "8.8.4.4",            # Google DNS
        "1.1.1.1", "1.0.0.1",            # Cloudflare DNS
        "9.9.9.9",                        # Quad9 DNS
        "208.67.222.222", "208.67.220.220",  # OpenDNS
    }
    
    # Extended known safe ports - loaded from PortManager if available
    KNOWN_SAFE_PORTS: Set[int] = {
        # Core network services
        22, 53, 80, 443,
        # Secure email
        465, 587, 993, 995,
        # Common web development/services
        8000, 8080, 8443,
        # Time sync
        123,
        # DHCP
        67, 68,
        # Local services
        631,   # CUPS printing
        5353,  # mDNS
        # VPN
        1194,  # OpenVPN
        51820, # WireGuard
        # Alternative SSH
        2222,
        # Common dev ports
        3000, 4200, 5000,
    }
    
    # C2/Backdoor ports for detection (suspicious outbound)
    C2_INDICATOR_PORTS: Set[int] = {
        4444, 4445,       # Metasploit
        5555, 6666,       # Common backdoors
        7777, 9999,       # Common backdoors
        12345,            # NetBus
        27374,            # SubSeven
        31337, 31338,     # Elite/Back Orifice
        50050,            # Cobalt Strike
        3333, 14444,      # Crypto mining stratum
    }
    
    @classmethod
    def load_ports_from_manager(cls):
        """Load port definitions from PortManager if available."""
        if PORT_MANAGER_AVAILABLE and get_port_manager:
            try:
                pm = get_port_manager()
                cls.KNOWN_SAFE_PORTS = pm.get_behavioral_safe_ports()
                cls.C2_INDICATOR_PORTS = pm.get_c2_ports()
            except Exception:
                pass  # Use defaults


# =============================================================================
# Event Types and Data Structures
# =============================================================================

class EventType(Enum):
    """Enumeration of behavioral event types."""
    HIGH_ENTROPY_WRITE = auto()
    RAPID_PROCESS_SPAWN = auto()
    UNEXPECTED_OUTBOUND = auto()
    MEMORY_INJECTION = auto()
    PRIVILEGE_ESCALATION = auto()
    FILE_TAMPERING = auto()
    REGISTRY_MODIFICATION = auto()
    PERSISTENCE_ATTEMPT = auto()
    LATERAL_MOVEMENT = auto()
    DATA_EXFILTRATION = auto()


class Severity(Enum):
    """Alert severity levels."""
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4


@dataclass
class Event:
    """
    Represents a behavioral event detected by a sensor.
    
    Attributes:
        event_type: The type of event detected
        sensor_name: Name of the sensor that detected this event
        timestamp: When the event was detected
        data: Additional event-specific data
        severity: Event severity level
    """
    event_type: EventType
    sensor_name: str
    timestamp: float
    data: Dict[str, Any]
    severity: Severity = Severity.MEDIUM
    
    def __str__(self) -> str:
        ts = datetime.fromtimestamp(self.timestamp).isoformat()
        return f"[{ts}] {self.sensor_name}: {self.event_type.name} ({self.severity.name})"


@dataclass
class Alert:
    """
    Represents a correlated alert raised by the correlation engine.
    
    Attributes:
        alert_id: Unique identifier for this alert
        alert_type: Type of alert (based on correlation rule)
        severity: Alert severity
        timestamp: When the alert was raised
        events: List of events that triggered this alert
        description: Human-readable description
        recommended_action: Suggested response action
    """
    alert_id: str
    alert_type: str
    severity: Severity
    timestamp: float
    events: List[Event]
    description: str
    recommended_action: str = ""
    
    def __str__(self) -> str:
        ts = datetime.fromtimestamp(self.timestamp).isoformat()
        return (f"\n{'='*60}\n"
                f"🚨 ALERT: {self.alert_type}\n"
                f"{'='*60}\n"
                f"ID: {self.alert_id}\n"
                f"Severity: {self.severity.name}\n"
                f"Time: {ts}\n"
                f"Description: {self.description}\n"
                f"Events: {len(self.events)}\n"
                f"Action: {self.recommended_action}\n"
                f"{'='*60}\n")


# =============================================================================
# Sensor Base Class
# =============================================================================

class Sensor(abc.ABC):
    """
    Abstract base class for behavioral micro-sensors.
    
    Each sensor is a small, single-purpose module that detects one
    suspicious behavioral pattern. Sensors emit events to the event bus
    when suspicious activity is detected.
    
    Subclasses must implement:
        - _monitor_loop(): The main monitoring logic
        
    Attributes:
        name: Unique identifier for this sensor
        description: Human-readable description of what this sensor detects
        enabled: Whether the sensor is currently active
    """
    
    def __init__(self, name: str, description: str = ""):
        """
        Initialize the sensor.
        
        Args:
            name: Unique identifier for this sensor
            description: Human-readable description
        """
        self.name = name
        self.description = description
        self.enabled = True
        self._event_bus: Optional['EventBus'] = None
        self._thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()
        self._logger = logging.getLogger(f"Sensor.{name}")
    
    def register(self, event_bus: 'EventBus') -> None:
        """
        Register this sensor with an event bus.
        
        Args:
            event_bus: The event bus to send events to
        """
        self._event_bus = event_bus
        self._logger.debug(f"Registered with event bus")
    
    def start(self) -> None:
        """
        Begin monitoring in a background thread.
        
        This starts the sensor's monitoring loop in a separate thread,
        allowing multiple sensors to run concurrently.
        """
        if self._thread and self._thread.is_alive():
            self._logger.warning("Sensor already running")
            return
        
        self._stop_event.clear()
        self._thread = threading.Thread(
            target=self._run,
            name=f"Sensor-{self.name}",
            daemon=True
        )
        self._thread.start()
        self._logger.info(f"Started monitoring")
    
    def stop(self) -> None:
        """Stop the sensor's monitoring loop."""
        self._stop_event.set()
        if self._thread:
            self._thread.join(timeout=5.0)
            self._logger.info("Stopped monitoring")
    
    def emit(self, event_type: EventType, data: Dict[str, Any],
             severity: Severity = Severity.MEDIUM) -> None:
        """
        Emit an event to the event bus.
        
        This is a helper method for sensors to easily send events
        to the registered event bus.
        
        Args:
            event_type: The type of event detected
            data: Additional event-specific data
            severity: Event severity level
        """
        if not self._event_bus:
            self._logger.error("Cannot emit event: no event bus registered")
            return
        
        event = Event(
            event_type=event_type,
            sensor_name=self.name,
            timestamp=time.time(),
            data=data,
            severity=severity
        )
        
        self._event_bus.publish(event)
        self._logger.debug(f"Emitted {event_type.name}")
    
    def _run(self) -> None:
        """Internal run method that wraps the monitoring loop."""
        try:
            self._monitor_loop()
        except Exception as e:
            self._logger.error(f"Error in monitoring loop: {e}")
    
    @abc.abstractmethod
    def _monitor_loop(self) -> None:
        """
        Main monitoring loop implementation.
        
        Subclasses must implement this method to provide the actual
        monitoring logic. The loop should check self._stop_event
        periodically to allow graceful shutdown.
        """
        pass


# =============================================================================
# Micro-Sensors Implementation
# =============================================================================

class HighEntropyWriteSensor(Sensor):
    """
    Detects file writes with high entropy content.
    
    High entropy in written data may indicate encryption (ransomware)
    or compressed/obfuscated malware payloads.
    
    This is a simulation that generates mock file write events
    for demonstration purposes.
    """
    
    def __init__(self, watch_dirs: Optional[List[str]] = None,
                 entropy_threshold: float = Config.ENTROPY_THRESHOLD):
        """
        Initialize the high entropy write sensor.
        
        Args:
            watch_dirs: Directories to monitor (default: /tmp)
            entropy_threshold: Minimum entropy to flag (0-8 bits/byte)
        """
        super().__init__(
            name="HighEntropyWriteSensor",
            description="Detects file writes with high entropy content"
        )
        self.watch_dirs = watch_dirs or ["/tmp"]
        self.entropy_threshold = entropy_threshold
        self._file_hashes: Dict[str, str] = {}
    
    @staticmethod
    def calculate_entropy(data: bytes) -> float:
        """
        Calculate Shannon entropy of data.
        
        Args:
            data: Bytes to analyze
            
        Returns:
            Entropy in bits per byte (0-8)
        """
        if not data:
            return 0.0
        
        # Count byte frequencies
        freq = defaultdict(int)
        for byte in data:
            freq[byte] += 1
        
        # Calculate entropy
        length = len(data)
        entropy = 0.0
        for count in freq.values():
            if count > 0:
                p = count / length
                entropy -= p * math.log2(p)
        
        return entropy
    
    def _monitor_loop(self) -> None:
        """Monitor for high-entropy file writes."""
        self._logger.info(f"Monitoring directories: {self.watch_dirs}")
        
        while not self._stop_event.is_set():
            # Simulate checking for file writes
            # In production, use inotify or similar
            
            # Generate simulated events for demonstration
            if random.random() < 0.1:  # 10% chance per cycle
                self._simulate_file_write()
            
            self._stop_event.wait(1.0)
    
    def _simulate_file_write(self) -> None:
        """Simulate a file write event for testing."""
        # Generate random data with varying entropy
        if random.random() < 0.3:  # 30% chance of high entropy
            # High entropy (encrypted/compressed)
            data = os.urandom(1024)
        else:
            # Low entropy (normal text)
            data = b"Normal file content with low entropy. " * 25
        
        entropy = self.calculate_entropy(data)
        filepath = f"/tmp/simulated_file_{random.randint(1000, 9999)}.dat"
        
        if entropy >= self.entropy_threshold:
            self.emit(
                event_type=EventType.HIGH_ENTROPY_WRITE,
                data={
                    "filepath": filepath,
                    "entropy": round(entropy, 2),
                    "size": len(data),
                    "threshold": self.entropy_threshold
                },
                severity=Severity.HIGH if entropy > 7.8 else Severity.MEDIUM
            )


class RapidProcessSpawnSensor(Sensor):
    """
    Detects rapid process spawning patterns.
    
    Rapid process creation may indicate fork bombs, malware propagation,
    or process injection attacks.
    
    This is a simulation that generates mock process events
    for demonstration purposes.
    """
    
    def __init__(self, rate_threshold: float = Config.SPAWN_RATE_THRESHOLD):
        """
        Initialize the rapid process spawn sensor.
        
        Args:
            rate_threshold: Processes per second to trigger detection
        """
        super().__init__(
            name="RapidProcessSpawnSensor",
            description="Detects rapid process spawning patterns"
        )
        self.rate_threshold = rate_threshold
        self._spawn_times: List[float] = []
        self._window_size = 5.0  # seconds
    
    def _monitor_loop(self) -> None:
        """Monitor for rapid process spawning."""
        self._logger.info(f"Monitoring process spawns (threshold: {self.rate_threshold}/s)")
        
        while not self._stop_event.is_set():
            # Simulate process monitoring
            # In production, use /proc or audit subsystem
            
            # Generate simulated events for demonstration
            self._simulate_process_activity()
            
            self._stop_event.wait(0.5)
    
    def _simulate_process_activity(self) -> None:
        """Simulate process spawn events for testing."""
        current_time = time.time()
        
        # Clean old entries
        self._spawn_times = [
            t for t in self._spawn_times 
            if current_time - t < self._window_size
        ]
        
        # Simulate spawns with occasional bursts
        if random.random() < 0.2:  # 20% chance of activity
            # Simulate 1-15 spawns
            num_spawns = random.randint(1, 15) if random.random() < 0.1 else 1
            
            for _ in range(num_spawns):
                self._spawn_times.append(current_time)
            
            # Calculate current rate
            rate = len(self._spawn_times) / self._window_size
            
            if rate >= self.rate_threshold:
                self.emit(
                    event_type=EventType.RAPID_PROCESS_SPAWN,
                    data={
                        "rate": round(rate, 2),
                        "threshold": self.rate_threshold,
                        "window_size": self._window_size,
                        "spawn_count": len(self._spawn_times),
                        "sample_pids": [random.randint(1000, 65535) for _ in range(min(5, num_spawns))]
                    },
                    severity=Severity.HIGH if rate > self.rate_threshold * 2 else Severity.MEDIUM
                )


class UnexpectedOutboundConnectionSensor(Sensor):
    """
    Detects outbound connections to unknown destinations.
    
    Unexpected network connections may indicate C2 communication,
    data exfiltration, or malware beacon activity.
    
    This is a simulation that generates mock network events
    for demonstration purposes.
    """
    
    def __init__(self, known_safe_ips: Optional[Set[str]] = None,
                 known_safe_ports: Optional[Set[int]] = None):
        """
        Initialize the unexpected outbound connection sensor.
        
        Args:
            known_safe_ips: Set of known safe IP addresses
            known_safe_ports: Set of known safe ports
        """
        super().__init__(
            name="UnexpectedOutboundSensor",
            description="Detects outbound connections to unknown destinations"
        )
        self.known_safe_ips = known_safe_ips or Config.KNOWN_SAFE_IPS
        self.known_safe_ports = known_safe_ports or Config.KNOWN_SAFE_PORTS
        self._connection_history: List[Tuple[str, int, float]] = []
    
    def _monitor_loop(self) -> None:
        """Monitor for unexpected outbound connections."""
        self._logger.info("Monitoring outbound connections")
        
        while not self._stop_event.is_set():
            # Simulate connection monitoring
            # In production, use netstat/ss or packet capture
            
            # Generate simulated events for demonstration
            self._simulate_connection_activity()
            
            self._stop_event.wait(2.0)
    
    def _simulate_connection_activity(self) -> None:
        """Simulate network connection events for testing."""
        if random.random() < 0.3:  # 30% chance of connection
            # Generate random destination
            if random.random() < 0.2:  # 20% chance of suspicious
                # Suspicious destination
                dest_ip = f"{random.randint(1, 223)}.{random.randint(0, 255)}.{random.randint(0, 255)}.{random.randint(1, 254)}"
                dest_port = random.choice([4444, 5555, 6666, 8080, 9999, 31337])
            else:
                # Known safe destination
                dest_ip = random.choice(list(self.known_safe_ips))
                dest_port = random.choice(list(self.known_safe_ports))
            
            # Check if suspicious
            is_suspicious = (
                dest_ip not in self.known_safe_ips or
                dest_port not in self.known_safe_ports
            )
            
            if is_suspicious:
                # Determine severity based on port
                if dest_port in {4444, 5555, 31337}:  # Common C2 ports
                    severity = Severity.CRITICAL
                elif dest_port not in self.known_safe_ports:
                    severity = Severity.HIGH
                else:
                    severity = Severity.MEDIUM
                
                self.emit(
                    event_type=EventType.UNEXPECTED_OUTBOUND,
                    data={
                        "dest_ip": dest_ip,
                        "dest_port": dest_port,
                        "protocol": "TCP",
                        "process": f"suspicious_proc_{random.randint(100, 999)}",
                        "pid": random.randint(1000, 65535),
                        "known_safe_ip": dest_ip in self.known_safe_ips,
                        "known_safe_port": dest_port in self.known_safe_ports
                    },
                    severity=severity
                )


# =============================================================================
# Event Bus
# =============================================================================

class EventBus:
    """
    Central event bus for receiving and routing behavioral events.
    
    The event bus receives events from registered sensors, logs them,
    and forwards them to the correlation engine for analysis.
    
    Features:
        - Thread-safe event queue
        - Event logging with timestamps
        - Sensor registration and management
        - Event forwarding to correlation engine
    """
    
    def __init__(self, log_file: Optional[str] = None):
        """
        Initialize the event bus.
        
        Args:
            log_file: Optional path to event log file
        """
        self._sensors: Dict[str, Sensor] = {}
        self._correlation_engine: Optional['CorrelationEngine'] = None
        self._event_queue: queue.Queue = queue.Queue(maxsize=Config.EVENT_QUEUE_SIZE)
        self._processor_thread: Optional[threading.Thread] = None
        self._stop_event = threading.Event()
        self._logger = logging.getLogger("EventBus")
        self._event_count = 0
        self._start_time = time.time()
        
        # Event logging
        self._log_file = log_file or Config.EVENT_LOG_FILE
        self._event_log_handler: Optional[logging.FileHandler] = None
        self._setup_event_logging()
    
    def _setup_event_logging(self) -> None:
        """Setup dedicated event logging."""
        self._event_logger = logging.getLogger("EventLog")
        self._event_logger.setLevel(logging.INFO)
        
        try:
            handler = logging.FileHandler(self._log_file)
            handler.setFormatter(logging.Formatter(
                '%(asctime)s|%(message)s',
                datefmt='%Y-%m-%d %H:%M:%S'
            ))
            self._event_logger.addHandler(handler)
            self._event_log_handler = handler
        except Exception as e:
            self._logger.warning(f"Could not setup event log file: {e}")
    
    def register_sensor(self, sensor: Sensor) -> None:
        """
        Register a sensor with the event bus.
        
        Args:
            sensor: The sensor to register
        """
        if sensor.name in self._sensors:
            self._logger.warning(f"Sensor {sensor.name} already registered")
            return
        
        sensor.register(self)
        self._sensors[sensor.name] = sensor
        self._logger.info(f"Registered sensor: {sensor.name}")
    
    def set_correlation_engine(self, engine: 'CorrelationEngine') -> None:
        """
        Set the correlation engine for event processing.
        
        Args:
            engine: The correlation engine to use
        """
        self._correlation_engine = engine
        self._logger.info("Correlation engine connected")
    
    def publish(self, event: Event) -> None:
        """
        Publish an event to the bus.
        
        This method is thread-safe and can be called from any sensor.
        
        Args:
            event: The event to publish
        """
        try:
            self._event_queue.put_nowait(event)
            self._event_count += 1
        except queue.Full:
            self._logger.warning("Event queue full, dropping event")
    
    def start(self) -> None:
        """Start the event bus and all registered sensors."""
        self._logger.info("Starting event bus...")
        
        # Start event processor
        self._stop_event.clear()
        self._processor_thread = threading.Thread(
            target=self._process_events,
            name="EventBus-Processor",
            daemon=True
        )
        self._processor_thread.start()
        
        # Start all sensors
        for sensor in self._sensors.values():
            if sensor.enabled:
                sensor.start()
        
        self._logger.info(f"Started {len(self._sensors)} sensors")
    
    def stop(self) -> None:
        """Stop the event bus and all sensors."""
        self._logger.info("Stopping event bus...")
        
        # Stop all sensors
        for sensor in self._sensors.values():
            sensor.stop()
        
        # Stop event processor
        self._stop_event.set()
        if self._processor_thread:
            self._processor_thread.join(timeout=5.0)
        
        # Cleanup
        if self._event_log_handler:
            self._event_log_handler.close()
        
        self._logger.info("Event bus stopped")
    
    def _process_events(self) -> None:
        """Process events from the queue."""
        while not self._stop_event.is_set():
            try:
                event = self._event_queue.get(timeout=0.5)
                self._handle_event(event)
            except queue.Empty:
                continue
            except Exception as e:
                self._logger.error(f"Error processing event: {e}")
    
    def _handle_event(self, event: Event) -> None:
        """
        Handle a single event.
        
        Args:
            event: The event to handle
        """
        # Log the event
        self._event_logger.info(
            f"{event.event_type.name}|{event.sensor_name}|"
            f"{event.severity.name}|{event.data}"
        )
        
        # Forward to correlation engine
        if self._correlation_engine:
            self._correlation_engine.handle_event(event)
    
    def get_stats(self) -> Dict[str, Any]:
        """Get event bus statistics."""
        runtime = time.time() - self._start_time
        return {
            "event_count": self._event_count,
            "events_per_second": self._event_count / runtime if runtime > 0 else 0,
            "runtime_seconds": runtime,
            "queue_size": self._event_queue.qsize(),
            "sensor_count": len(self._sensors),
            "active_sensors": sum(1 for s in self._sensors.values() if s.enabled)
        }


# =============================================================================
# Correlation Engine
# =============================================================================

class CorrelationEngine:
    """
    Correlation engine for analyzing behavioral events.
    
    The correlation engine maintains event counters and applies rules
    to detect patterns that indicate threats. When thresholds are met,
    alerts are raised.
    
    Features:
        - Time-windowed event counting
        - Configurable detection rules
        - Alert generation and management
        - Statistics tracking
    """
    
    def __init__(self, alert_callback: Optional[Callable[[Alert], None]] = None):
        """
        Initialize the correlation engine.
        
        Args:
            alert_callback: Optional callback function for alerts
        """
        self._event_windows: Dict[EventType, List[Tuple[float, Event]]] = defaultdict(list)
        self._alert_callback = alert_callback or self._default_alert_handler
        self._alerts: List[Alert] = []
        self._alert_count = 0
        self._event_count = 0
        self._lock = threading.Lock()
        self._logger = logging.getLogger("CorrelationEngine")
        
        # Configure rules
        self._rules = self._build_rules()
    
    def _build_rules(self) -> List[Dict[str, Any]]:
        """Build the correlation rules."""
        return [
            {
                "name": "High Entropy Write Burst",
                "event_type": EventType.HIGH_ENTROPY_WRITE,
                "threshold": Config.HIGH_ENTROPY_THRESHOLD,
                "window": Config.HIGH_ENTROPY_WINDOW,
                "severity": Severity.HIGH,
                "description": "Multiple high-entropy file writes detected (possible ransomware)",
                "action": "Investigate files, consider isolating system"
            },
            {
                "name": "Process Spawn Storm",
                "event_type": EventType.RAPID_PROCESS_SPAWN,
                "threshold": Config.RAPID_SPAWN_THRESHOLD,
                "window": Config.RAPID_SPAWN_WINDOW,
                "severity": Severity.CRITICAL,
                "description": "Rapid process spawning detected (possible fork bomb or malware)",
                "action": "Kill process tree, investigate parent process"
            },
            {
                "name": "Suspicious Outbound Activity",
                "event_type": EventType.UNEXPECTED_OUTBOUND,
                "threshold": Config.OUTBOUND_CONN_THRESHOLD,
                "window": Config.OUTBOUND_CONN_WINDOW,
                "severity": Severity.HIGH,
                "description": "Multiple connections to unknown destinations (possible C2 activity)",
                "action": "Block network, analyze traffic, investigate process"
            }
        ]
    
    def handle_event(self, event: Event) -> None:
        """
        Handle an incoming event.
        
        This method is called by the event bus for each event received.
        It updates counters and checks correlation rules.
        
        Args:
            event: The event to handle
        """
        with self._lock:
            self._event_count += 1
            current_time = time.time()
            
            # Add event to window
            self._event_windows[event.event_type].append((current_time, event))
            
            # Check rules for this event type
            self._check_rules(event.event_type, current_time)
    
    def _check_rules(self, event_type: EventType, current_time: float) -> None:
        """
        Check correlation rules for an event type.
        
        Args:
            event_type: The type of event to check rules for
            current_time: Current timestamp
        """
        for rule in self._rules:
            if rule["event_type"] != event_type:
                continue
            
            window = rule["window"]
            threshold = rule["threshold"]
            
            # Get events in window
            events = self._event_windows[event_type]
            
            # Clean old events
            events = [(t, e) for t, e in events if current_time - t <= window]
            self._event_windows[event_type] = events
            
            # Check threshold
            if len(events) >= threshold:
                self._raise_alert(rule, [e for _, e in events])
                
                # Clear window to prevent duplicate alerts
                self._event_windows[event_type] = []
    
    def _raise_alert(self, rule: Dict[str, Any], events: List[Event]) -> None:
        """
        Raise an alert based on a triggered rule.
        
        Args:
            rule: The rule that triggered the alert
            events: The events that triggered the rule
        """
        self._alert_count += 1
        
        alert = Alert(
            alert_id=f"ALERT-{self._alert_count:06d}",
            alert_type=rule["name"],
            severity=rule["severity"],
            timestamp=time.time(),
            events=events,
            description=rule["description"],
            recommended_action=rule["action"]
        )
        
        self._alerts.append(alert)
        self._logger.warning(f"Alert raised: {rule['name']}")
        
        # Call alert callback
        self._alert_callback(alert)
    
    def _default_alert_handler(self, alert: Alert) -> None:
        """Default alert handler that prints to stdout."""
        print(alert)
    
    def add_rule(self, name: str, event_type: EventType, threshold: int,
                 window: float, severity: Severity, description: str,
                 action: str = "") -> None:
        """
        Add a custom correlation rule.
        
        Args:
            name: Rule name
            event_type: Event type to correlate
            threshold: Number of events to trigger alert
            window: Time window in seconds
            severity: Alert severity
            description: Rule description
            action: Recommended action
        """
        self._rules.append({
            "name": name,
            "event_type": event_type,
            "threshold": threshold,
            "window": window,
            "severity": severity,
            "description": description,
            "action": action
        })
        self._logger.info(f"Added rule: {name}")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get correlation engine statistics."""
        with self._lock:
            return {
                "event_count": self._event_count,
                "alert_count": self._alert_count,
                "rule_count": len(self._rules),
                "active_windows": {
                    k.name: len(v) for k, v in self._event_windows.items() if v
                }
            }
    
    def get_alerts(self) -> List[Alert]:
        """Get all raised alerts."""
        return self._alerts.copy()


# =============================================================================
# Main Application
# =============================================================================

class BehavioralEngine:
    """
    Main behavioral detection engine application.
    
    This class orchestrates the sensors, event bus, and correlation
    engine to provide a complete behavioral detection system.
    """
    
    def __init__(self, verbose: bool = False):
        """
        Initialize the behavioral engine.
        
        Args:
            verbose: Enable verbose logging
        """
        self._setup_logging(verbose)
        self._logger = logging.getLogger("BehavioralEngine")
        
        # Initialize components
        self.event_bus = EventBus()
        self.correlation_engine = CorrelationEngine(
            alert_callback=self._handle_alert
        )
        self.event_bus.set_correlation_engine(self.correlation_engine)
        
        # Create sensors
        self.sensors: List[Sensor] = [
            HighEntropyWriteSensor(),
            RapidProcessSpawnSensor(),
            UnexpectedOutboundConnectionSensor()
        ]
        
        # Register sensors
        for sensor in self.sensors:
            self.event_bus.register_sensor(sensor)
        
        self._running = False
        self._alert_handlers: List[Callable[[Alert], None]] = []
    
    def _setup_logging(self, verbose: bool) -> None:
        """Setup logging configuration."""
        level = logging.DEBUG if verbose else logging.INFO
        logging.basicConfig(
            level=level,
            format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
    
    def _handle_alert(self, alert: Alert) -> None:
        """Handle an alert from the correlation engine."""
        for handler in self._alert_handlers:
            try:
                handler(alert)
            except Exception as e:
                self._logger.error(f"Alert handler error: {e}")
    
    def add_alert_handler(self, handler: Callable[[Alert], None]) -> None:
        """Add a custom alert handler."""
        self._alert_handlers.append(handler)
    
    def add_sensor(self, sensor: Sensor) -> None:
        """
        Add a custom sensor to the engine.
        
        Args:
            sensor: The sensor to add
        """
        self.sensors.append(sensor)
        self.event_bus.register_sensor(sensor)
        
        if self._running:
            sensor.start()
    
    def start(self) -> None:
        """Start the behavioral detection engine."""
        self._logger.info("=" * 60)
        self._logger.info("Starting Behavioral Detection Engine")
        self._logger.info("=" * 60)
        
        self.event_bus.start()
        self._running = True
        
        self._logger.info("Engine started. Monitoring for suspicious behavior...")
    
    def stop(self) -> None:
        """Stop the behavioral detection engine."""
        self._logger.info("Stopping Behavioral Detection Engine...")
        self.event_bus.stop()
        self._running = False
        self._logger.info("Engine stopped")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get comprehensive statistics."""
        return {
            "event_bus": self.event_bus.get_stats(),
            "correlation": self.correlation_engine.get_stats(),
            "alerts": len(self.correlation_engine.get_alerts())
        }
    
    def run_interactive(self, duration: Optional[float] = None) -> None:
        """
        Run the engine interactively.
        
        Args:
            duration: Optional duration in seconds (None = run until Ctrl+C)
        """
        self.start()
        
        try:
            start_time = time.time()
            while True:
                if duration and (time.time() - start_time) >= duration:
                    break
                
                # Print periodic stats
                time.sleep(10)
                stats = self.get_stats()
                print(f"\n[Stats] Events: {stats['event_bus']['event_count']}, "
                      f"Alerts: {stats['alerts']}, "
                      f"Queue: {stats['event_bus']['queue_size']}")
                
        except KeyboardInterrupt:
            print("\nShutting down...")
        finally:
            self.stop()
            self._print_summary()
    
    def _print_summary(self) -> None:
        """Print final summary."""
        stats = self.get_stats()
        alerts = self.correlation_engine.get_alerts()
        
        print("\n" + "=" * 60)
        print("BEHAVIORAL DETECTION ENGINE - SUMMARY")
        print("=" * 60)
        print(f"Total Events Processed: {stats['correlation']['event_count']}")
        print(f"Total Alerts Raised: {len(alerts)}")
        print(f"Runtime: {stats['event_bus']['runtime_seconds']:.1f} seconds")
        print(f"Events/Second: {stats['event_bus']['events_per_second']:.2f}")
        
        if alerts:
            print("\nALERT SUMMARY:")
            for alert in alerts:
                print(f"  - [{alert.severity.name}] {alert.alert_type}")
        
        print("=" * 60)


def main():
    """Main entry point for the behavioral detection engine."""
    parser = argparse.ArgumentParser(
        description="Behavioral Detection Engine for GreyAV",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                    Run with default settings
  %(prog)s --verbose          Enable verbose logging
  %(prog)s --duration 60      Run for 60 seconds
  %(prog)s --entropy 7.0      Set entropy threshold
        """
    )
    
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Enable verbose logging"
    )
    parser.add_argument(
        "-d", "--duration",
        type=float,
        default=None,
        help="Run duration in seconds (default: indefinite)"
    )
    parser.add_argument(
        "--entropy",
        type=float,
        default=Config.ENTROPY_THRESHOLD,
        help=f"Entropy threshold (default: {Config.ENTROPY_THRESHOLD})"
    )
    parser.add_argument(
        "--spawn-rate",
        type=float,
        default=Config.SPAWN_RATE_THRESHOLD,
        help=f"Spawn rate threshold (default: {Config.SPAWN_RATE_THRESHOLD})"
    )
    
    args = parser.parse_args()
    
    # Update config from args
    Config.ENTROPY_THRESHOLD = args.entropy
    Config.SPAWN_RATE_THRESHOLD = args.spawn_rate
    
    # Create and run engine
    engine = BehavioralEngine(verbose=args.verbose)
    
    print("""
    ╔══════════════════════════════════════════════════════════╗
    ║     BEHAVIORAL DETECTION ENGINE - GreyAV v3.0            ║
    ║                                                          ║
    ║  Monitoring for:                                         ║
    ║    • High entropy file writes (ransomware detection)     ║
    ║    • Rapid process spawning (fork bombs, malware)        ║
    ║    • Unexpected outbound connections (C2 activity)       ║
    ║                                                          ║
    ║  Press Ctrl+C to stop                                    ║
    ╚══════════════════════════════════════════════════════════╝
    """)
    
    engine.run_interactive(duration=args.duration)


if __name__ == "__main__":
    main()
