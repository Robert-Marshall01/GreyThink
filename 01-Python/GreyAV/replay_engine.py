"""
Behavioral Memory Replay Engine for GreyAV EDR System.

This module stores normalized events in a replay buffer and enables
re-running past events through updated detection rules to identify
previously undetected malicious behavior.
"""

from collections import deque
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Optional
import uuid


@dataclass
class Event:
    """
    Represents a normalized/enriched system event for replay analysis.
    
    Attributes:
        event_id: Unique identifier for this event.
        event_type: Type of event (e.g., 'file_write', 'process_spawn').
        process_id: Process ID that generated the event.
        process_name: Name of the process.
        timestamp: When the event occurred (Unix timestamp).
        source: Source entity (file, process, user).
        target: Target entity affected by the event.
        severity: Severity score (0.0 to 1.0).
        metadata: Additional event-specific data.
    """
    event_id: str
    event_type: str
    process_id: int
    process_name: str
    timestamp: float
    source: str = ""
    target: str = ""
    severity: float = 0.5
    metadata: dict = field(default_factory=dict)

    @staticmethod
    def generate_id() -> str:
        """Generate a unique event ID."""
        return f"EVT-{uuid.uuid4().hex[:12].upper()}"

    def to_datetime(self) -> datetime:
        """Convert timestamp to datetime object."""
        return datetime.fromtimestamp(self.timestamp)


@dataclass
class RetroAlert:
    """
    Alert object emitted when replay analysis detects malicious behavior.
    
    RetroAlerts indicate that new detection logic has identified suspicious
    patterns in historical event data that were not caught in real-time.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        process_id: Process ID associated with the alert.
        events_involved: List of events that triggered the detection.
        description: Human-readable description of the threat.
        timestamp: When the alert was generated.
        detected_by: Name of the rule or engine that made the detection.
    """
    alert_id: str
    process_id: int
    events_involved: list[Event]
    description: str
    timestamp: datetime
    detected_by: str

    @staticmethod
    def generate_id() -> str:
        """Generate a unique alert ID."""
        return f"RA-{uuid.uuid4().hex[:12].upper()}"


class ReplayBuffer:
    """
    Circular buffer for storing normalized events for replay analysis.
    
    The buffer maintains a fixed-size collection of recent events that
    can be replayed through updated detection rules.
    
    Attributes:
        events: Double-ended queue of stored events.
        max_size: Maximum number of events to retain.
    """

    DEFAULT_MAX_SIZE: int = 10000

    def __init__(self, max_size: int = DEFAULT_MAX_SIZE) -> None:
        """
        Initialize the replay buffer.
        
        Args:
            max_size: Maximum number of events to store.
        """
        self.max_size: int = max_size
        self.events: deque[Event] = deque(maxlen=max_size)
        self._event_index: dict[str, Event] = {}  # event_id → Event
        self._process_index: dict[int, list[str]] = {}  # process_id → event_ids

    def add_event(self, event: Event) -> None:
        """
        Add an event to the replay buffer.
        
        If the buffer is at capacity, the oldest event is automatically
        removed to make room.
        
        Args:
            event: The event to store.
        """
        # Check if we need to remove old event from indices
        if len(self.events) >= self.max_size:
            oldest = self.events[0]
            self._remove_from_indices(oldest)
        
        # Add to buffer
        self.events.append(event)
        
        # Update indices
        self._event_index[event.event_id] = event
        
        if event.process_id not in self._process_index:
            self._process_index[event.process_id] = []
        self._process_index[event.process_id].append(event.event_id)

    def _remove_from_indices(self, event: Event) -> None:
        """Remove an event from internal indices."""
        self._event_index.pop(event.event_id, None)
        
        if event.process_id in self._process_index:
            if event.event_id in self._process_index[event.process_id]:
                self._process_index[event.process_id].remove(event.event_id)
            if not self._process_index[event.process_id]:
                del self._process_index[event.process_id]

    def get_events(self, since: Optional[float] = None) -> list[Event]:
        """
        Retrieve events from the buffer.
        
        Args:
            since: Optional Unix timestamp to filter events.
                   If provided, only returns events after this time.
                   
        Returns:
            List of events, optionally filtered by timestamp.
        """
        if since is None:
            return list(self.events)
        
        return [e for e in self.events if e.timestamp >= since]

    def get_events_by_process(self, process_id: int) -> list[Event]:
        """
        Get all events for a specific process.
        
        Args:
            process_id: Process ID to filter by.
            
        Returns:
            List of events for the specified process.
        """
        event_ids = self._process_index.get(process_id, [])
        return [self._event_index[eid] for eid in event_ids if eid in self._event_index]

    def get_events_by_type(self, event_type: str) -> list[Event]:
        """
        Get all events of a specific type.
        
        Args:
            event_type: Event type to filter by.
            
        Returns:
            List of events matching the type.
        """
        return [e for e in self.events if e.event_type == event_type]

    def get_event_by_id(self, event_id: str) -> Optional[Event]:
        """
        Get a specific event by its ID.
        
        Args:
            event_id: Event ID to look up.
            
        Returns:
            Event if found, None otherwise.
        """
        return self._event_index.get(event_id)

    def get_events_in_range(
        self, 
        start_time: float, 
        end_time: float
    ) -> list[Event]:
        """
        Get events within a time range.
        
        Args:
            start_time: Start of time range (Unix timestamp).
            end_time: End of time range (Unix timestamp).
            
        Returns:
            List of events within the range.
        """
        return [
            e for e in self.events 
            if start_time <= e.timestamp <= end_time
        ]

    def clear(self) -> None:
        """Clear all events from the buffer."""
        self.events.clear()
        self._event_index.clear()
        self._process_index.clear()

    def __len__(self) -> int:
        """Return the number of events in the buffer."""
        return len(self.events)

    def __iter__(self):
        """Iterate over events in the buffer."""
        return iter(self.events)


# Type alias for replay rules
ReplayRule = Callable[[Event, list[Event]], Optional[RetroAlert]]


class ReplayEngine:
    """
    Engine for replaying historical events through detection rules.
    
    The ReplayEngine maintains a collection of detection rules and can
    replay buffered events to identify previously undetected threats
    using updated or newly added detection logic.
    
    Attributes:
        buffer: ReplayBuffer containing historical events.
        rules: List of detection rule functions.
    """

    def __init__(self, buffer: Optional[ReplayBuffer] = None) -> None:
        """
        Initialize the replay engine.
        
        Args:
            buffer: Optional ReplayBuffer instance. Creates new one if not provided.
        """
        self.buffer: ReplayBuffer = buffer or ReplayBuffer()
        self.rules: list[ReplayRule] = []
        self._rule_names: dict[int, str] = {}  # rule_id (hash) → name
        self._alert_subscribers: list[Callable[[RetroAlert], None]] = []

    def add_rule(
        self, 
        rule: ReplayRule, 
        name: Optional[str] = None
    ) -> None:
        """
        Add a detection rule to the engine.
        
        Rules are functions that take an event and context (list of all events)
        and return a RetroAlert if malicious behavior is detected.
        
        Args:
            rule: Detection rule function.
            name: Optional human-readable name for the rule.
        """
        self.rules.append(rule)
        rule_id = id(rule)
        self._rule_names[rule_id] = name or f"Rule_{len(self.rules)}"

    def remove_rule(self, rule: ReplayRule) -> bool:
        """
        Remove a detection rule from the engine.
        
        Args:
            rule: The rule to remove.
            
        Returns:
            True if rule was removed, False if not found.
        """
        if rule in self.rules:
            self.rules.remove(rule)
            self._rule_names.pop(id(rule), None)
            return True
        return False

    def get_rule_name(self, rule: ReplayRule) -> str:
        """
        Get the name of a rule.
        
        Args:
            rule: The rule to look up.
            
        Returns:
            Rule name string.
        """
        return self._rule_names.get(id(rule), "unknown")

    def replay_all(self) -> list[RetroAlert]:
        """
        Replay all events in the buffer through all rules.
        
        Returns:
            List of RetroAlerts generated during replay.
        """
        return self._replay_events(self.buffer.get_events())

    def replay_since(self, timestamp: float) -> list[RetroAlert]:
        """
        Replay events since a given timestamp.
        
        Args:
            timestamp: Unix timestamp to start replay from.
            
        Returns:
            List of RetroAlerts generated during replay.
        """
        events = self.buffer.get_events(since=timestamp)
        return self._replay_events(events)

    def replay_range(
        self, 
        start_time: float, 
        end_time: float
    ) -> list[RetroAlert]:
        """
        Replay events within a time range.
        
        Args:
            start_time: Start of time range (Unix timestamp).
            end_time: End of time range (Unix timestamp).
            
        Returns:
            List of RetroAlerts generated during replay.
        """
        events = self.buffer.get_events_in_range(start_time, end_time)
        return self._replay_events(events)

    def replay_process(self, process_id: int) -> list[RetroAlert]:
        """
        Replay all events for a specific process.
        
        Args:
            process_id: Process ID to replay.
            
        Returns:
            List of RetroAlerts for the process.
        """
        events = self.buffer.get_events_by_process(process_id)
        return self._replay_events(events)

    def _replay_events(self, events: list[Event]) -> list[RetroAlert]:
        """
        Internal method to replay a list of events through rules.
        
        Args:
            events: List of events to replay.
            
        Returns:
            List of RetroAlerts generated.
        """
        alerts: list[RetroAlert] = []
        all_events = self.buffer.get_events()  # Full context
        
        for event in events:
            for rule in self.rules:
                try:
                    alert = rule(event, all_events)
                    if alert:
                        # Set detected_by if not already set
                        if not alert.detected_by:
                            alert.detected_by = self.get_rule_name(rule)
                        alerts.append(alert)
                        self._notify_subscribers(alert)
                except Exception as e:
                    print(f"[ReplayEngine] Rule execution failed: {e}")
        
        return alerts

    def generate_timeline(
        self, 
        process_id: int,
        include_related: bool = False
    ) -> list[Event]:
        """
        Generate a forensic timeline of events for a process.
        
        Args:
            process_id: Process ID to generate timeline for.
            include_related: If True, include events from child/parent processes.
            
        Returns:
            List of events sorted by timestamp.
        """
        events = self.buffer.get_events_by_process(process_id)
        
        if include_related:
            # Look for related processes in metadata
            related_pids: set[int] = {process_id}
            
            for event in events:
                if 'parent_pid' in event.metadata:
                    related_pids.add(event.metadata['parent_pid'])
                if 'child_pid' in event.metadata:
                    related_pids.add(event.metadata['child_pid'])
            
            # Gather events from related processes
            for pid in related_pids:
                if pid != process_id:
                    events.extend(self.buffer.get_events_by_process(pid))
        
        # Sort by timestamp
        return sorted(events, key=lambda e: e.timestamp)

    def generate_attack_timeline(
        self, 
        events: list[Event]
    ) -> dict[str, Any]:
        """
        Generate a structured attack timeline from events.
        
        Args:
            events: List of events to analyze.
            
        Returns:
            Dictionary containing timeline with phases and details.
        """
        if not events:
            return {"phases": [], "summary": "No events to analyze"}
        
        sorted_events = sorted(events, key=lambda e: e.timestamp)
        
        # Group events into phases
        phases: list[dict[str, Any]] = []
        current_phase: list[Event] = []
        phase_gap_threshold = 60.0  # 60 seconds
        
        for event in sorted_events:
            if current_phase:
                time_gap = event.timestamp - current_phase[-1].timestamp
                if time_gap > phase_gap_threshold:
                    # Start new phase
                    phases.append(self._summarize_phase(current_phase))
                    current_phase = []
            current_phase.append(event)
        
        if current_phase:
            phases.append(self._summarize_phase(current_phase))
        
        return {
            "phases": phases,
            "total_events": len(sorted_events),
            "time_span": sorted_events[-1].timestamp - sorted_events[0].timestamp,
            "start_time": datetime.fromtimestamp(sorted_events[0].timestamp).isoformat(),
            "end_time": datetime.fromtimestamp(sorted_events[-1].timestamp).isoformat(),
            "processes_involved": list({e.process_id for e in sorted_events}),
            "summary": f"Attack timeline with {len(phases)} phases over {len(sorted_events)} events"
        }

    def _summarize_phase(self, events: list[Event]) -> dict[str, Any]:
        """
        Summarize a phase of events.
        
        Args:
            events: Events in this phase.
            
        Returns:
            Phase summary dictionary.
        """
        event_types = [e.event_type for e in events]
        max_severity = max(e.severity for e in events)
        
        return {
            "start_time": datetime.fromtimestamp(events[0].timestamp).isoformat(),
            "end_time": datetime.fromtimestamp(events[-1].timestamp).isoformat(),
            "duration": events[-1].timestamp - events[0].timestamp,
            "event_count": len(events),
            "event_types": list(set(event_types)),
            "max_severity": max_severity,
            "events": events,
        }

    def subscribe(self, callback: Callable[[RetroAlert], None]) -> None:
        """
        Subscribe to retro alerts.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self._alert_subscribers.append(callback)

    def unsubscribe(self, callback: Callable[[RetroAlert], None]) -> None:
        """
        Unsubscribe from retro alerts.
        
        Args:
            callback: Previously registered callback function.
        """
        if callback in self._alert_subscribers:
            self._alert_subscribers.remove(callback)

    def _notify_subscribers(self, alert: RetroAlert) -> None:
        """
        Notify all subscribers of a new alert.
        
        Args:
            alert: The alert to broadcast.
        """
        for callback in self._alert_subscribers:
            try:
                callback(alert)
            except Exception as e:
                print(f"[ReplayEngine] Subscriber notification failed: {e}")

    def get_stats(self) -> dict[str, Any]:
        """
        Get statistics about the replay engine.
        
        Returns:
            Dictionary containing engine metrics.
        """
        return {
            "buffer_size": len(self.buffer),
            "buffer_max_size": self.buffer.max_size,
            "rule_count": len(self.rules),
            "rule_names": [self.get_rule_name(r) for r in self.rules],
            "unique_processes": len(self.buffer._process_index),
        }


# Example detection rules

def detect_ransomware_staging(
    event: Event, 
    context: list[Event]
) -> Optional[RetroAlert]:
    """
    Detect potential ransomware staging behavior.
    
    Looks for patterns of rapid file enumeration followed by encryption-like activity.
    
    Args:
        event: Current event being analyzed.
        context: All events in buffer for correlation.
        
    Returns:
        RetroAlert if ransomware staging detected, None otherwise.
    """
    if event.event_type not in ('file_write', 'file_modify'):
        return None
    
    # Look for rapid file operations from same process
    process_events = [
        e for e in context 
        if e.process_id == event.process_id 
        and e.event_type in ('file_read', 'file_write', 'file_modify')
        and abs(e.timestamp - event.timestamp) < 60  # Within 60 seconds
    ]
    
    # Check for suspicious patterns
    if len(process_events) >= 10:
        read_count = sum(1 for e in process_events if e.event_type == 'file_read')
        write_count = sum(1 for e in process_events if e.event_type in ('file_write', 'file_modify'))
        
        # Pattern: many reads followed by writes (encryption behavior)
        if read_count >= 5 and write_count >= 5:
            return RetroAlert(
                alert_id=RetroAlert.generate_id(),
                process_id=event.process_id,
                events_involved=process_events,
                description=(
                    f"Potential ransomware staging detected: Process {event.process_name} "
                    f"performed {read_count} reads and {write_count} writes in rapid succession"
                ),
                timestamp=datetime.now(),
                detected_by="detect_ransomware_staging"
            )
    
    return None


def detect_credential_access(
    event: Event, 
    context: list[Event]
) -> Optional[RetroAlert]:
    """
    Detect potential credential access attempts.
    
    Looks for access to sensitive credential stores or files.
    
    Args:
        event: Current event being analyzed.
        context: All events in buffer for correlation.
        
    Returns:
        RetroAlert if credential access detected, None otherwise.
    """
    credential_patterns = [
        '/etc/shadow',
        '/etc/passwd',
        '.ssh/id_rsa',
        'credentials',
        'password',
        'SAM',
        'SYSTEM',
        'SECURITY',
        'lsass',
    ]
    
    target_lower = event.target.lower()
    
    for pattern in credential_patterns:
        if pattern.lower() in target_lower:
            return RetroAlert(
                alert_id=RetroAlert.generate_id(),
                process_id=event.process_id,
                events_involved=[event],
                description=(
                    f"Credential access detected: Process {event.process_name} "
                    f"accessed sensitive target: {event.target}"
                ),
                timestamp=datetime.now(),
                detected_by="detect_credential_access"
            )
    
    return None


def detect_lateral_movement(
    event: Event, 
    context: list[Event]
) -> Optional[RetroAlert]:
    """
    Detect potential lateral movement attempts.
    
    Looks for network connections to internal hosts with suspicious patterns.
    
    Args:
        event: Current event being analyzed.
        context: All events in buffer for correlation.
        
    Returns:
        RetroAlert if lateral movement detected, None otherwise.
    """
    if event.event_type != 'network_connection':
        return None
    
    # Look for multiple internal connections from same process
    process_net_events = [
        e for e in context
        if e.process_id == event.process_id
        and e.event_type == 'network_connection'
        and e.metadata.get('is_internal', False)
    ]
    
    if len(process_net_events) >= 3:
        unique_targets = {e.target for e in process_net_events}
        
        if len(unique_targets) >= 3:
            return RetroAlert(
                alert_id=RetroAlert.generate_id(),
                process_id=event.process_id,
                events_involved=process_net_events,
                description=(
                    f"Potential lateral movement: Process {event.process_name} "
                    f"connected to {len(unique_targets)} internal hosts"
                ),
                timestamp=datetime.now(),
                detected_by="detect_lateral_movement"
            )
    
    return None


# Pipeline integration
def create_replay_handler(engine: ReplayEngine) -> Callable[[Event], None]:
    """
    Create an event handler for pipeline integration.
    
    This factory creates a handler that stores events in the replay buffer.
    
    Args:
        engine: The ReplayEngine instance to use.
        
    Returns:
        Event handler function compatible with pipeline subscription.
    """
    def handler(event: Event) -> None:
        engine.buffer.add_event(event)
    
    return handler


if __name__ == "__main__":
    import time
    
    # Demo usage
    engine = ReplayEngine()
    
    # Add detection rules
    engine.add_rule(detect_ransomware_staging, "Ransomware Staging Detection")
    engine.add_rule(detect_credential_access, "Credential Access Detection")
    engine.add_rule(detect_lateral_movement, "Lateral Movement Detection")
    
    # Subscribe to alerts
    def alert_handler(alert: RetroAlert) -> None:
        print(f"\n[RETRO ALERT] {alert.alert_id}")
        print(f"  Process: {alert.process_id}")
        print(f"  Detected by: {alert.detected_by}")
        print(f"  Events: {len(alert.events_involved)}")
        print(f"  {alert.description}")
    
    engine.subscribe(alert_handler)
    
    # Simulate some events
    base_time = time.time() - 300  # 5 minutes ago
    
    events = [
        Event(Event.generate_id(), "file_read", 1234, "suspicious.exe", base_time, "/home/user", "/etc/passwd"),
        Event(Event.generate_id(), "file_read", 1234, "suspicious.exe", base_time + 1, "/home/user", "/etc/shadow"),
        Event(Event.generate_id(), "file_read", 5678, "ransomware.bin", base_time + 10, "", "file1.docx"),
        Event(Event.generate_id(), "file_read", 5678, "ransomware.bin", base_time + 11, "", "file2.docx"),
        Event(Event.generate_id(), "file_read", 5678, "ransomware.bin", base_time + 12, "", "file3.docx"),
        Event(Event.generate_id(), "file_read", 5678, "ransomware.bin", base_time + 13, "", "file4.docx"),
        Event(Event.generate_id(), "file_read", 5678, "ransomware.bin", base_time + 14, "", "file5.docx"),
        Event(Event.generate_id(), "file_write", 5678, "ransomware.bin", base_time + 15, "", "file1.docx.enc"),
        Event(Event.generate_id(), "file_write", 5678, "ransomware.bin", base_time + 16, "", "file2.docx.enc"),
        Event(Event.generate_id(), "file_write", 5678, "ransomware.bin", base_time + 17, "", "file3.docx.enc"),
        Event(Event.generate_id(), "file_write", 5678, "ransomware.bin", base_time + 18, "", "file4.docx.enc"),
        Event(Event.generate_id(), "file_write", 5678, "ransomware.bin", base_time + 19, "", "file5.docx.enc"),
    ]
    
    # Add events to buffer
    for event in events:
        engine.buffer.add_event(event)
    
    print(f"Buffer contains {len(engine.buffer)} events")
    print(f"Engine stats: {engine.get_stats()}")
    
    # Replay all events
    print("\nReplaying all events...")
    alerts = engine.replay_all()
    print(f"\nTotal alerts: {len(alerts)}")
    
    # Generate timeline for a process
    print("\nGenerating timeline for ransomware process (5678)...")
    timeline = engine.generate_timeline(5678)
    print(f"Timeline has {len(timeline)} events")
    
    # Generate structured attack timeline
    attack_timeline = engine.generate_attack_timeline(timeline)
    print(f"Attack summary: {attack_timeline['summary']}")
