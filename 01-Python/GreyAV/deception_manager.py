"""
Adaptive Deception Layer for GreyAV EDR System.

This module implements honeypot management and deception-based threat detection.
It creates and monitors decoy artifacts to detect malicious activity with high confidence.
"""

import uuid
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Set

# Configure module logger
logger = logging.getLogger(__name__)


@dataclass
class DeceptionEvent:
    """
    Represents a deception-triggered security event.
    
    Emitted when a suspicious process interacts with a honeypot artifact,
    providing high-confidence indication of malicious activity.
    
    Attributes:
        event_id: Unique identifier for this deception event.
        artifact_type: Type of honeypot ("file", "registry", "process", "network").
        artifact_name: Name or path of the honeypot artifact.
        pid: Process ID that interacted with the honeypot.
        timestamp: When the interaction was detected.
        description: Human-readable description of the event.
    """
    event_id: str
    artifact_type: str
    artifact_name: str
    pid: int
    timestamp: datetime
    description: str
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert the event to a dictionary for serialization."""
        return {
            "event_id": self.event_id,
            "artifact_type": self.artifact_type,
            "artifact_name": self.artifact_name,
            "pid": self.pid,
            "timestamp": self.timestamp.isoformat(),
            "description": self.description
        }


@dataclass
class Event:
    """
    Generic event from the EDR pipeline.
    
    This is a simplified representation of events flowing through the system.
    Used for interaction detection with honeypots.
    
    Attributes:
        event_type: Type of event (e.g., "file_access", "network_connection").
        pid: Process ID that generated the event.
        metadata: Additional event details including target paths/resources.
    """
    event_type: str
    pid: int
    metadata: Dict[str, Any] = field(default_factory=dict)


class DeceptionManager:
    """
    Manages honeypot artifacts and detects interactions with them.
    
    The DeceptionManager creates and tracks various types of honeypot artifacts
    (files, registry keys, processes, network endpoints) and monitors the event
    pipeline for any interactions with these decoys. When an interaction is
    detected, a high-confidence DeceptionEvent is emitted.
    
    Attributes:
        honeypots: Registry of all honeypot artifacts organized by type.
        consumers: List of callback functions to receive DeceptionEvents.
    """
    
    # Valid honeypot artifact types
    ARTIFACT_TYPES = {"file", "registry", "process", "network"}
    
    def __init__(self) -> None:
        """Initialize the DeceptionManager with empty honeypot registries."""
        self._honeypots: Dict[str, Set[str]] = {
            "file": set(),
            "registry": set(),
            "process": set(),
            "network": set()
        }
        self._consumers: List[Callable[[DeceptionEvent], None]] = []
        self._active: bool = True
        logger.info("DeceptionManager initialized")
    
    @property
    def honeypots(self) -> Dict[str, Set[str]]:
        """Get a copy of the honeypot registry."""
        return {k: v.copy() for k, v in self._honeypots.items()}
    
    def register_consumer(self, callback: Callable[[DeceptionEvent], None]) -> None:
        """
        Register a consumer callback for DeceptionEvents.
        
        Args:
            callback: Function to be called when a DeceptionEvent is emitted.
        """
        self._consumers.append(callback)
        logger.debug(f"Registered consumer: {callback.__name__ if hasattr(callback, '__name__') else callback}")
    
    def unregister_consumer(self, callback: Callable[[DeceptionEvent], None]) -> bool:
        """
        Unregister a consumer callback.
        
        Args:
            callback: The callback function to remove.
            
        Returns:
            True if the callback was found and removed, False otherwise.
        """
        try:
            self._consumers.remove(callback)
            return True
        except ValueError:
            return False
    
    def _emit_event(self, event: DeceptionEvent) -> None:
        """
        Emit a DeceptionEvent to all registered consumers.
        
        Args:
            event: The DeceptionEvent to emit.
        """
        logger.warning(f"DECEPTION TRIGGERED: {event.artifact_type} honeypot '{event.artifact_name}' "
                      f"accessed by PID {event.pid}")
        for consumer in self._consumers:
            try:
                consumer(event)
            except Exception as e:
                logger.error(f"Error in consumer callback: {e}")
    
    def create_honeypot_file(self, path: str) -> None:
        """
        Create a honeypot file artifact.
        
        Creates a decoy file that appears valuable to attackers (e.g., passwords.txt,
        credentials.xlsx). Any access to this file indicates potential malicious activity.
        
        Args:
            path: The filesystem path for the honeypot file.
            
        Note:
            In production, this would create an actual file with enticing content.
            Current implementation registers the path for monitoring.
        """
        self._honeypots["file"].add(path)
        logger.info(f"Created honeypot file: {path}")
        # TODO: In production, create actual file with decoy content
        # Example: passwords.txt, credentials.xlsx, private_keys.pem
    
    def create_honeypot_registry(self, key: str) -> None:
        """
        Create a honeypot registry key artifact.
        
        Creates a decoy registry key that mimics sensitive configuration.
        Access to this key indicates potential reconnaissance or credential harvesting.
        
        Args:
            key: The registry key path (e.g., "HKLM\\SOFTWARE\\Honeypot\\Credentials").
            
        Note:
            Registry honeypots are Windows-specific. On other platforms,
            this registers the key for cross-platform event matching.
        """
        self._honeypots["registry"].add(key)
        logger.info(f"Created honeypot registry key: {key}")
        # TODO: In production on Windows, create actual registry key
    
    def create_honeypot_process(self, name: str) -> None:
        """
        Create a honeypot process artifact.
        
        Registers a process name as a honeypot. Any attempt to interact with,
        inject into, or terminate this process indicates malicious activity.
        
        Args:
            name: The process name to monitor (e.g., "lsass_backup.exe").
            
        Note:
            In production, this might spawn a decoy process that mimics
            high-value targets like LSASS or security software.
        """
        self._honeypots["process"].add(name)
        logger.info(f"Created honeypot process: {name}")
        # TODO: In production, spawn actual decoy process
    
    def create_honeypot_network(self, port: int) -> None:
        """
        Create a honeypot network endpoint.
        
        Opens a listening port that mimics a valuable service (e.g., SSH, RDP, SMB).
        Any connection attempt indicates potential lateral movement or scanning.
        
        Args:
            port: The port number to monitor (e.g., 2222 mimicking SSH).
            
        Note:
            In production, this would bind to the port and accept connections.
            Current implementation registers the port for event matching.
        """
        port_str = str(port)
        self._honeypots["network"].add(port_str)
        logger.info(f"Created honeypot network endpoint on port: {port}")
        # TODO: In production, bind socket and accept connections
    
    def remove_honeypot(self, artifact_type: str, artifact_name: str) -> bool:
        """
        Remove a honeypot artifact from monitoring.
        
        Args:
            artifact_type: Type of artifact ("file", "registry", "process", "network").
            artifact_name: The name/path of the artifact to remove.
            
        Returns:
            True if the artifact was found and removed, False otherwise.
        """
        if artifact_type not in self.ARTIFACT_TYPES:
            logger.error(f"Invalid artifact type: {artifact_type}")
            return False
        
        try:
            self._honeypots[artifact_type].discard(artifact_name)
            logger.info(f"Removed honeypot {artifact_type}: {artifact_name}")
            return True
        except KeyError:
            return False
    
    def is_honeypot(self, artifact_type: str, artifact_name: str) -> bool:
        """
        Check if a given artifact is a registered honeypot.
        
        Args:
            artifact_type: Type of artifact to check.
            artifact_name: Name/path of the artifact.
            
        Returns:
            True if the artifact is a registered honeypot.
        """
        if artifact_type not in self.ARTIFACT_TYPES:
            return False
        return artifact_name in self._honeypots[artifact_type]
    
    def monitor_interactions(self, event: Event) -> Optional[DeceptionEvent]:
        """
        Monitor an event for honeypot interactions.
        
        Analyzes incoming pipeline events to detect when a process interacts
        with a honeypot artifact. When detected, creates and emits a high-confidence
        DeceptionEvent.
        
        Args:
            event: The pipeline event to analyze.
            
        Returns:
            A DeceptionEvent if a honeypot interaction was detected, None otherwise.
            
        Note:
            Interaction detection uses event.metadata["target"] to identify
            the resource being accessed. The event_type helps determine
            which honeypot registry to check.
        """
        if not self._active:
            return None
        
        target = event.metadata.get("target", "")
        if not target:
            return None
        
        # Determine artifact type based on event type
        artifact_type = self._map_event_to_artifact_type(event.event_type)
        if not artifact_type:
            return None
        
        # Check if target matches any honeypot
        target_str = str(target)
        
        # For files, also check if any honeypot path is contained in the target
        if artifact_type == "file":
            for honeypot_path in self._honeypots["file"]:
                if honeypot_path in target_str or target_str in honeypot_path:
                    return self._create_deception_event(
                        artifact_type="file",
                        artifact_name=honeypot_path,
                        pid=event.pid,
                        event_type=event.event_type
                    )
        
        # For other types, check exact match
        if target_str in self._honeypots.get(artifact_type, set()):
            return self._create_deception_event(
                artifact_type=artifact_type,
                artifact_name=target_str,
                pid=event.pid,
                event_type=event.event_type
            )
        
        return None
    
    def _map_event_to_artifact_type(self, event_type: str) -> Optional[str]:
        """
        Map an event type to the corresponding artifact type.
        
        Args:
            event_type: The type of pipeline event.
            
        Returns:
            The corresponding honeypot artifact type, or None if unmapped.
        """
        mapping = {
            "file_access": "file",
            "file_read": "file",
            "file_write": "file",
            "file_delete": "file",
            "file_open": "file",
            "registry_access": "registry",
            "registry_read": "registry",
            "registry_write": "registry",
            "registry_query": "registry",
            "process_access": "process",
            "process_inject": "process",
            "process_terminate": "process",
            "process_create": "process",
            "network_connect": "network",
            "network_listen": "network",
            "network_accept": "network",
        }
        return mapping.get(event_type.lower())
    
    def _create_deception_event(
        self,
        artifact_type: str,
        artifact_name: str,
        pid: int,
        event_type: str
    ) -> DeceptionEvent:
        """
        Create and emit a DeceptionEvent.
        
        Args:
            artifact_type: Type of honeypot that was triggered.
            artifact_name: Name/path of the honeypot.
            pid: Process ID that triggered the honeypot.
            event_type: The original event type that caused the trigger.
            
        Returns:
            The created DeceptionEvent.
        """
        description = (
            f"Process {pid} performed '{event_type}' on {artifact_type} "
            f"honeypot '{artifact_name}'. High confidence malicious activity detected."
        )
        
        deception_event = DeceptionEvent(
            event_id=str(uuid.uuid4()),
            artifact_type=artifact_type,
            artifact_name=artifact_name,
            pid=pid,
            timestamp=datetime.now(),
            description=description
        )
        
        self._emit_event(deception_event)
        return deception_event
    
    def get_stats(self) -> Dict[str, int]:
        """
        Get statistics about registered honeypots.
        
        Returns:
            Dictionary with counts of each honeypot type.
        """
        return {k: len(v) for k, v in self._honeypots.items()}
    
    def activate(self) -> None:
        """Activate honeypot monitoring."""
        self._active = True
        logger.info("DeceptionManager activated")
    
    def deactivate(self) -> None:
        """Deactivate honeypot monitoring (honeypots remain registered)."""
        self._active = False
        logger.info("DeceptionManager deactivated")
    
    def is_active(self) -> bool:
        """Check if monitoring is active."""
        return self._active
    
    def clear_all_honeypots(self) -> None:
        """Remove all registered honeypots."""
        for artifact_type in self._honeypots:
            self._honeypots[artifact_type].clear()
        logger.info("All honeypots cleared")


# Module-level convenience functions for pipeline integration

_default_manager: Optional[DeceptionManager] = None


def get_default_manager() -> DeceptionManager:
    """
    Get or create the default DeceptionManager instance.
    
    Returns:
        The singleton DeceptionManager instance.
    """
    global _default_manager
    if _default_manager is None:
        _default_manager = DeceptionManager()
    return _default_manager


def setup_default_honeypots(manager: Optional[DeceptionManager] = None) -> DeceptionManager:
    """
    Set up a standard set of honeypot artifacts.
    
    Creates common honeypot files, registry keys, and network endpoints
    that are attractive to attackers.
    
    Args:
        manager: Optional DeceptionManager instance. Uses default if None.
        
    Returns:
        The configured DeceptionManager instance.
    """
    if manager is None:
        manager = get_default_manager()
    
    # Honeypot files - appear valuable to attackers
    honeypot_files = [
        "/etc/shadow.bak",
        "/root/.ssh/id_rsa.backup",
        "/var/www/html/wp-config.php.bak",
        "/home/admin/passwords.txt",
        "/opt/secrets/api_keys.json",
        "C:\\Users\\Administrator\\Desktop\\passwords.xlsx",
        "C:\\Windows\\System32\\config\\SAM.bak",
    ]
    
    for path in honeypot_files:
        manager.create_honeypot_file(path)
    
    # Honeypot registry keys (Windows)
    honeypot_registry = [
        "HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\RunOnce\\Backup",
        "HKCU\\SOFTWARE\\Honeypot\\Credentials",
        "HKLM\\SYSTEM\\CurrentControlSet\\Services\\HoneypotSvc",
    ]
    
    for key in honeypot_registry:
        manager.create_honeypot_registry(key)
    
    # Honeypot processes
    honeypot_processes = [
        "lsass_debug.exe",
        "svchost_backup.exe",
        "admin_console.exe",
    ]
    
    for proc in honeypot_processes:
        manager.create_honeypot_process(proc)
    
    # Honeypot network ports - mimic valuable services
    honeypot_ports = [
        2222,   # Fake SSH
        3390,   # Fake RDP
        8443,   # Fake HTTPS admin
        27017,  # Fake MongoDB
    ]
    
    for port in honeypot_ports:
        manager.create_honeypot_network(port)
    
    logger.info(f"Default honeypots configured: {manager.get_stats()}")
    return manager


if __name__ == "__main__":
    # Demo/test usage
    logging.basicConfig(level=logging.DEBUG)
    
    # Create manager and set up honeypots
    manager = setup_default_honeypots()
    
    # Register a consumer
    def alert_handler(event: DeceptionEvent) -> None:
        print(f"[ALERT] {event.description}")
    
    manager.register_consumer(alert_handler)
    
    # Simulate events
    print("\n--- Simulating events ---\n")
    
    # This should NOT trigger (normal file access)
    normal_event = Event(
        event_type="file_access",
        pid=1234,
        metadata={"target": "/var/log/syslog"}
    )
    result = manager.monitor_interactions(normal_event)
    print(f"Normal event result: {result}")
    
    # This SHOULD trigger (honeypot file access)
    malicious_event = Event(
        event_type="file_read",
        pid=5678,
        metadata={"target": "/home/admin/passwords.txt"}
    )
    result = manager.monitor_interactions(malicious_event)
    print(f"Malicious event result: {result}")
    
    # Show stats
    print(f"\nHoneypot statistics: {manager.get_stats()}")
