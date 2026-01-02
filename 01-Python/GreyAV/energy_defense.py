"""
Adaptive Energy-Aware Defense Engine for GreyAV EDR System.

This module monitors system resources and dynamically adjusts detection
depth and rule execution based on resource availability, ensuring critical
defenses remain active even under heavy system load.
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Optional
import random
import uuid


@dataclass
class Event:
    """
    Represents a system event to be evaluated for resource-aware defense.
    
    Attributes:
        event_id: Unique identifier for this event.
        event_type: Type of event (e.g., 'file_write', 'process_spawn').
        process_id: Process ID that generated the event.
        process_name: Name of the process.
        severity: Severity score (0.0 to 1.0).
        timestamp: When the event occurred.
        metadata: Additional event-specific data.
    """
    event_id: str
    event_type: str
    process_id: int
    process_name: str
    severity: float = 0.5
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: dict = field(default_factory=dict)

    @staticmethod
    def generate_id() -> str:
        """Generate a unique event ID."""
        return f"EVT-{uuid.uuid4().hex[:12].upper()}"


@dataclass
class ResourceDefenseAlert:
    """
    Alert object emitted when suspicious resource usage aligns with events.
    
    This alert indicates that a process is consuming abnormal resources
    in conjunction with potentially malicious behavior.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        process_id: Process ID causing the resource spike.
        resource_type: Type of resource affected ('cpu', 'memory', 'disk', 'battery').
        usage_percent: Current usage percentage of the resource.
        description: Human-readable description of the alert.
        timestamp: When the alert was generated.
    """
    alert_id: str
    process_id: int
    resource_type: str
    usage_percent: float
    description: str
    timestamp: datetime = field(default_factory=datetime.now)

    @staticmethod
    def generate_id() -> str:
        """Generate a unique alert ID."""
        return f"RDA-{uuid.uuid4().hex[:12].upper()}"


class ResourceMonitor:
    """
    Monitor for system resource usage.
    
    Provides methods to query current CPU, memory, disk I/O, and battery
    levels. This implementation uses mock values but can be extended to
    use actual system metrics via psutil or platform-specific APIs.
    """

    def __init__(self, use_mock: bool = True) -> None:
        """
        Initialize the resource monitor.
        
        Args:
            use_mock: If True, use mock values. If False, attempt real monitoring.
        """
        self._use_mock = use_mock
        self._mock_cpu = 30.0
        self._mock_memory = 45.0
        self._mock_disk = 20.0
        self._mock_battery = 100.0
        self._history: dict[str, list[tuple[datetime, float]]] = {
            'cpu': [],
            'memory': [],
            'disk': [],
            'battery': [],
        }

    def get_cpu_usage(self) -> float:
        """
        Get current CPU usage percentage.
        
        Returns:
            CPU usage as percentage (0.0 to 100.0).
        """
        if self._use_mock:
            # Simulate varying CPU usage
            self._mock_cpu = max(0.0, min(100.0, self._mock_cpu + random.uniform(-5, 5)))
            usage = self._mock_cpu
        else:
            # Placeholder for real implementation
            # Could use: psutil.cpu_percent(interval=0.1)
            usage = 50.0
        
        self._record_usage('cpu', usage)
        return usage

    def get_memory_usage(self) -> float:
        """
        Get current memory usage percentage.
        
        Returns:
            Memory usage as percentage (0.0 to 100.0).
        """
        if self._use_mock:
            self._mock_memory = max(0.0, min(100.0, self._mock_memory + random.uniform(-3, 3)))
            usage = self._mock_memory
        else:
            # Placeholder for real implementation
            # Could use: psutil.virtual_memory().percent
            usage = 60.0
        
        self._record_usage('memory', usage)
        return usage

    def get_disk_io(self) -> float:
        """
        Get current disk I/O utilization percentage.
        
        Returns:
            Disk I/O as percentage (0.0 to 100.0).
        """
        if self._use_mock:
            self._mock_disk = max(0.0, min(100.0, self._mock_disk + random.uniform(-10, 10)))
            usage = self._mock_disk
        else:
            # Placeholder for real implementation
            # Could use: psutil.disk_io_counters() with rate calculation
            usage = 25.0
        
        self._record_usage('disk', usage)
        return usage

    def get_battery_level(self) -> float:
        """
        Get current battery level percentage.
        
        Returns:
            Battery level as percentage (0.0 to 100.0).
            Returns 100.0 if no battery is present (AC power).
        """
        if self._use_mock:
            # Simulate slow battery drain
            self._mock_battery = max(0.0, self._mock_battery - random.uniform(0, 0.1))
            usage = self._mock_battery
        else:
            # Placeholder for real implementation
            # Could use: psutil.sensors_battery()
            usage = 100.0  # Assume AC power
        
        self._record_usage('battery', usage)
        return usage

    def get_all_metrics(self) -> dict[str, float]:
        """
        Get all resource metrics at once.
        
        Returns:
            Dictionary with all resource usage values.
        """
        return {
            'cpu': self.get_cpu_usage(),
            'memory': self.get_memory_usage(),
            'disk': self.get_disk_io(),
            'battery': self.get_battery_level(),
        }

    def _record_usage(self, resource_type: str, value: float) -> None:
        """Record usage value in history."""
        self._history[resource_type].append((datetime.now(), value))
        # Keep only last 100 entries
        if len(self._history[resource_type]) > 100:
            self._history[resource_type] = self._history[resource_type][-100:]

    def get_average_usage(self, resource_type: str, samples: int = 10) -> float:
        """
        Get average usage over recent samples.
        
        Args:
            resource_type: Type of resource.
            samples: Number of recent samples to average.
            
        Returns:
            Average usage percentage.
        """
        history = self._history.get(resource_type, [])
        if not history:
            return 0.0
        
        recent = history[-samples:]
        return sum(v for _, v in recent) / len(recent)

    def set_mock_values(
        self, 
        cpu: Optional[float] = None,
        memory: Optional[float] = None,
        disk: Optional[float] = None,
        battery: Optional[float] = None
    ) -> None:
        """
        Set mock values for testing.
        
        Args:
            cpu: Mock CPU usage percentage.
            memory: Mock memory usage percentage.
            disk: Mock disk I/O percentage.
            battery: Mock battery level percentage.
        """
        if cpu is not None:
            self._mock_cpu = cpu
        if memory is not None:
            self._mock_memory = memory
        if disk is not None:
            self._mock_disk = disk
        if battery is not None:
            self._mock_battery = battery


# Type alias for defense rules
DefenseRule = Callable[[Event], Optional[ResourceDefenseAlert]]


class EnergyDefenseEngine:
    """
    Adaptive energy-aware defense engine.
    
    Monitors system resources and dynamically adjusts detection depth
    and rule execution based on resource availability. Prioritizes
    critical defenses under heavy load or low battery conditions.
    
    Attributes:
        monitor: ResourceMonitor instance for querying system metrics.
        thresholds: Dictionary mapping resource types to alert thresholds.
    """

    # Default resource thresholds (percentage)
    DEFAULT_THRESHOLDS: dict[str, float] = {
        'cpu': 80.0,
        'memory': 75.0,
        'disk': 85.0,
        'battery': 20.0,  # Alert when below this level
    }

    # Event types that are considered high-priority
    CRITICAL_EVENT_TYPES: set[str] = {
        'privilege_escalation',
        'process_injection',
        'credential_theft',
        'ransomware_behavior',
        'rootkit_detected',
        'lateral_movement',
    }

    # Event types associated with resource abuse
    RESOURCE_ABUSE_TYPES: set[str] = {
        'cryptomining',
        'resource_exhaustion',
        'fork_bomb',
        'memory_leak',
        'disk_flood',
    }

    def __init__(
        self, 
        monitor: Optional[ResourceMonitor] = None,
        thresholds: Optional[dict[str, float]] = None
    ) -> None:
        """
        Initialize the energy defense engine.
        
        Args:
            monitor: Optional ResourceMonitor instance.
            thresholds: Optional custom thresholds for alerts.
        """
        self.monitor: ResourceMonitor = monitor or ResourceMonitor()
        self.thresholds: dict[str, float] = thresholds or dict(self.DEFAULT_THRESHOLDS)
        
        self._rules: dict[str, list[DefenseRule]] = {
            'high': [],
            'medium': [],
            'low': [],
        }
        self._current_defense_level: str = 'high'
        self._alert_subscribers: list[Callable[[ResourceDefenseAlert], None]] = []
        self._level_change_subscribers: list[Callable[[str, str], None]] = []

    def evaluate_event(self, event: Event) -> Optional[ResourceDefenseAlert]:
        """
        Evaluate an event for resource-related suspicious behavior.
        
        Checks if the event coincides with abnormal resource usage that
        might indicate malicious activity (e.g., cryptomining, resource
        exhaustion attacks).
        
        Args:
            event: The event to evaluate.
            
        Returns:
            ResourceDefenseAlert if suspicious, None otherwise.
        """
        metrics = self.monitor.get_all_metrics()
        
        # Check each resource against thresholds
        alerts: list[ResourceDefenseAlert] = []
        
        for resource_type, usage in metrics.items():
            threshold = self.thresholds.get(resource_type, 80.0)
            
            # For battery, alert when BELOW threshold (low battery)
            if resource_type == 'battery':
                is_critical = usage < threshold
            else:
                is_critical = usage > threshold
            
            if is_critical:
                # Check if event is suspicious given high resource usage
                if self._is_suspicious_resource_event(event, resource_type):
                    alert = self._create_resource_alert(
                        event, resource_type, usage
                    )
                    alerts.append(alert)
        
        # Return the most severe alert (highest usage)
        if alerts:
            alerts.sort(key=lambda a: a.usage_percent, reverse=True)
            best_alert = alerts[0]
            self._notify_alert_subscribers(best_alert)
            return best_alert
        
        return None

    def _is_suspicious_resource_event(
        self, 
        event: Event, 
        resource_type: str
    ) -> bool:
        """
        Determine if an event is suspicious given the resource type.
        
        Args:
            event: The event to check.
            resource_type: The resource experiencing high usage.
            
        Returns:
            True if the combination is suspicious.
        """
        # High severity events are always suspicious with high resource usage
        if event.severity >= 0.7:
            return True
        
        # Known resource abuse event types
        if event.event_type in self.RESOURCE_ABUSE_TYPES:
            return True
        
        # CPU-specific suspicious patterns
        if resource_type == 'cpu':
            suspicious_types = {'cryptomining', 'password_cracking', 'hash_computation'}
            if event.event_type in suspicious_types:
                return True
        
        # Memory-specific suspicious patterns
        if resource_type == 'memory':
            suspicious_types = {'memory_allocation', 'buffer_overflow', 'heap_spray'}
            if event.event_type in suspicious_types:
                return True
        
        # Disk-specific suspicious patterns
        if resource_type == 'disk':
            suspicious_types = {'file_write', 'file_encrypt', 'mass_deletion'}
            if event.event_type in suspicious_types:
                return True
        
        return False

    def _create_resource_alert(
        self, 
        event: Event, 
        resource_type: str, 
        usage: float
    ) -> ResourceDefenseAlert:
        """
        Create a ResourceDefenseAlert for an event.
        
        Args:
            event: The triggering event.
            resource_type: Type of resource affected.
            usage: Current usage percentage.
            
        Returns:
            ResourceDefenseAlert object.
        """
        severity = "CRITICAL" if usage > 90.0 else "HIGH" if usage > 80.0 else "ELEVATED"
        
        description = (
            f"{severity}: Process '{event.process_name}' (PID: {event.process_id}) "
            f"triggered {event.event_type} event while {resource_type} usage is at "
            f"{usage:.1f}%. Possible resource abuse or attack in progress."
        )
        
        return ResourceDefenseAlert(
            alert_id=ResourceDefenseAlert.generate_id(),
            process_id=event.process_id,
            resource_type=resource_type,
            usage_percent=usage,
            description=description,
            timestamp=datetime.now()
        )

    def adjust_defense_level(self) -> str:
        """
        Adjust the defense level based on current resource state.
        
        Defense levels:
        - 'high': Full detection, all rules active
        - 'medium': Reduced detection, skip expensive rules
        - 'low': Minimal detection, only critical rules
        
        Returns:
            Current defense level ('high', 'medium', or 'low').
        """
        metrics = self.monitor.get_all_metrics()
        
        cpu = metrics['cpu']
        memory = metrics['memory']
        battery = metrics['battery']
        
        old_level = self._current_defense_level
        
        # Determine new level based on resource constraints
        if battery < 10.0:
            # Critical battery - minimal defense only
            new_level = 'low'
        elif cpu > 90.0 or memory > 90.0:
            # Extreme resource pressure - reduce to low
            new_level = 'low'
        elif cpu > 75.0 or memory > 80.0 or battery < 30.0:
            # Moderate pressure - reduce to medium
            new_level = 'medium'
        else:
            # Resources available - full defense
            new_level = 'high'
        
        self._current_defense_level = new_level
        
        # Notify subscribers of level change
        if new_level != old_level:
            self._notify_level_change(old_level, new_level)
        
        return new_level

    def get_defense_level(self) -> str:
        """
        Get the current defense level without recalculating.
        
        Returns:
            Current defense level.
        """
        return self._current_defense_level

    def add_rule(
        self, 
        rule: DefenseRule, 
        priority: str = 'medium'
    ) -> None:
        """
        Add a defense rule at a specific priority level.
        
        Args:
            rule: The detection rule function.
            priority: Priority level ('high', 'medium', 'low').
        """
        if priority not in self._rules:
            priority = 'medium'
        self._rules[priority].append(rule)

    def remove_rule(self, rule: DefenseRule) -> bool:
        """
        Remove a defense rule from all priority levels.
        
        Args:
            rule: The rule to remove.
            
        Returns:
            True if rule was found and removed.
        """
        for level_rules in self._rules.values():
            if rule in level_rules:
                level_rules.remove(rule)
                return True
        return False

    def run_rules(self, event: Event) -> list[ResourceDefenseAlert]:
        """
        Run detection rules based on current defense level.
        
        Rules are executed selectively depending on available resources:
        - 'high': Run all rules (high, medium, low priority)
        - 'medium': Run high and medium priority rules only
        - 'low': Run only high priority (critical) rules
        
        Args:
            event: The event to analyze.
            
        Returns:
            List of alerts generated by rules.
        """
        level = self.adjust_defense_level()
        alerts: list[ResourceDefenseAlert] = []
        
        # Determine which rule sets to execute
        rules_to_run: list[DefenseRule] = []
        
        if level == 'high':
            rules_to_run.extend(self._rules['high'])
            rules_to_run.extend(self._rules['medium'])
            rules_to_run.extend(self._rules['low'])
        elif level == 'medium':
            rules_to_run.extend(self._rules['high'])
            rules_to_run.extend(self._rules['medium'])
        else:  # low
            rules_to_run.extend(self._rules['high'])
        
        # Always run critical event check regardless of level
        if event.event_type in self.CRITICAL_EVENT_TYPES:
            # Force full rule execution for critical events
            rules_to_run = []
            rules_to_run.extend(self._rules['high'])
            rules_to_run.extend(self._rules['medium'])
            rules_to_run.extend(self._rules['low'])
        
        # Execute rules
        for rule in rules_to_run:
            try:
                alert = rule(event)
                if alert:
                    alerts.append(alert)
                    self._notify_alert_subscribers(alert)
            except Exception as e:
                print(f"[EnergyDefenseEngine] Rule execution failed: {e}")
        
        # Also check for resource-based alerts
        resource_alert = self.evaluate_event(event)
        if resource_alert:
            alerts.append(resource_alert)
        
        return alerts

    def subscribe_alerts(
        self, 
        callback: Callable[[ResourceDefenseAlert], None]
    ) -> None:
        """
        Subscribe to resource defense alerts.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self._alert_subscribers.append(callback)

    def unsubscribe_alerts(
        self, 
        callback: Callable[[ResourceDefenseAlert], None]
    ) -> None:
        """
        Unsubscribe from resource defense alerts.
        
        Args:
            callback: Previously registered callback function.
        """
        if callback in self._alert_subscribers:
            self._alert_subscribers.remove(callback)

    def subscribe_level_changes(
        self, 
        callback: Callable[[str, str], None]
    ) -> None:
        """
        Subscribe to defense level change notifications.
        
        Args:
            callback: Function(old_level, new_level) to call on changes.
        """
        self._level_change_subscribers.append(callback)

    def unsubscribe_level_changes(
        self, 
        callback: Callable[[str, str], None]
    ) -> None:
        """
        Unsubscribe from defense level changes.
        
        Args:
            callback: Previously registered callback function.
        """
        if callback in self._level_change_subscribers:
            self._level_change_subscribers.remove(callback)

    def _notify_alert_subscribers(self, alert: ResourceDefenseAlert) -> None:
        """Notify all alert subscribers."""
        for callback in self._alert_subscribers:
            try:
                callback(alert)
            except Exception as e:
                print(f"[EnergyDefenseEngine] Alert subscriber failed: {e}")

    def _notify_level_change(self, old_level: str, new_level: str) -> None:
        """Notify all level change subscribers."""
        for callback in self._level_change_subscribers:
            try:
                callback(old_level, new_level)
            except Exception as e:
                print(f"[EnergyDefenseEngine] Level change subscriber failed: {e}")

    def get_status(self) -> dict[str, Any]:
        """
        Get current engine status and metrics.
        
        Returns:
            Dictionary containing engine state and resource metrics.
        """
        metrics = self.monitor.get_all_metrics()
        
        return {
            'defense_level': self._current_defense_level,
            'resources': metrics,
            'thresholds': dict(self.thresholds),
            'rule_counts': {
                level: len(rules) 
                for level, rules in self._rules.items()
            },
            'resources_critical': {
                resource: (
                    usage < self.thresholds.get(resource, 20.0) 
                    if resource == 'battery' 
                    else usage > self.thresholds.get(resource, 80.0)
                )
                for resource, usage in metrics.items()
            },
        }

    def set_threshold(self, resource_type: str, threshold: float) -> None:
        """
        Set the alert threshold for a resource type.
        
        Args:
            resource_type: Type of resource ('cpu', 'memory', 'disk', 'battery').
            threshold: Threshold percentage for alerts.
        """
        self.thresholds[resource_type] = threshold


# Pipeline integration handler
def create_energy_defense_handler(
    engine: EnergyDefenseEngine
) -> Callable[[Event], list[ResourceDefenseAlert]]:
    """
    Create an event handler for pipeline integration.
    
    This factory creates a handler that evaluates events through the
    energy defense engine.
    
    Args:
        engine: The EnergyDefenseEngine instance to use.
        
    Returns:
        Event handler function compatible with pipeline subscription.
    """
    def handler(event: Event) -> list[ResourceDefenseAlert]:
        return engine.run_rules(event)
    
    return handler


# Example defense rules

def detect_cryptomining(event: Event) -> Optional[ResourceDefenseAlert]:
    """
    Detect potential cryptomining activity.
    
    Args:
        event: Event to analyze.
        
    Returns:
        ResourceDefenseAlert if cryptomining suspected.
    """
    suspicious_patterns = ['xmrig', 'minerd', 'cryptonight', 'stratum']
    
    process_lower = event.process_name.lower()
    for pattern in suspicious_patterns:
        if pattern in process_lower:
            return ResourceDefenseAlert(
                alert_id=ResourceDefenseAlert.generate_id(),
                process_id=event.process_id,
                resource_type='cpu',
                usage_percent=0.0,  # Will be updated by engine
                description=f"Potential cryptominer detected: {event.process_name}",
                timestamp=datetime.now()
            )
    
    return None


def detect_fork_bomb(event: Event) -> Optional[ResourceDefenseAlert]:
    """
    Detect potential fork bomb attack.
    
    Args:
        event: Event to analyze.
        
    Returns:
        ResourceDefenseAlert if fork bomb suspected.
    """
    if event.event_type == 'process_spawn':
        spawn_rate = event.metadata.get('spawn_rate', 0)
        if spawn_rate > 100:  # More than 100 spawns per second
            return ResourceDefenseAlert(
                alert_id=ResourceDefenseAlert.generate_id(),
                process_id=event.process_id,
                resource_type='cpu',
                usage_percent=0.0,
                description=f"Fork bomb detected: {event.process_name} spawning at {spawn_rate}/sec",
                timestamp=datetime.now()
            )
    
    return None


def detect_disk_flood(event: Event) -> Optional[ResourceDefenseAlert]:
    """
    Detect disk flood attack (filling disk space).
    
    Args:
        event: Event to analyze.
        
    Returns:
        ResourceDefenseAlert if disk flood suspected.
    """
    if event.event_type in ('file_write', 'file_create'):
        write_rate = event.metadata.get('write_rate_mb', 0)
        if write_rate > 500:  # More than 500MB/sec
            return ResourceDefenseAlert(
                alert_id=ResourceDefenseAlert.generate_id(),
                process_id=event.process_id,
                resource_type='disk',
                usage_percent=0.0,
                description=f"Disk flood detected: {event.process_name} writing at {write_rate}MB/sec",
                timestamp=datetime.now()
            )
    
    return None


if __name__ == "__main__":
    # Demo usage
    engine = EnergyDefenseEngine()
    
    # Add detection rules at different priorities
    engine.add_rule(detect_cryptomining, 'high')
    engine.add_rule(detect_fork_bomb, 'high')
    engine.add_rule(detect_disk_flood, 'medium')
    
    # Subscribe to alerts
    def alert_handler(alert: ResourceDefenseAlert) -> None:
        print(f"\n[RESOURCE ALERT] {alert.alert_id}")
        print(f"  Process: {alert.process_id}")
        print(f"  Resource: {alert.resource_type} @ {alert.usage_percent:.1f}%")
        print(f"  {alert.description}")
    
    engine.subscribe_alerts(alert_handler)
    
    # Subscribe to level changes
    def level_handler(old_level: str, new_level: str) -> None:
        print(f"\n[DEFENSE LEVEL] Changed from '{old_level}' to '{new_level}'")
    
    engine.subscribe_level_changes(level_handler)
    
    # Set mock values to simulate high load
    engine.monitor.set_mock_values(cpu=85.0, memory=70.0, battery=50.0)
    
    print("Initial status:", engine.get_status())
    
    # Simulate events
    events = [
        Event(
            Event.generate_id(), 
            "process_spawn", 
            1234, 
            "normal_app",
            severity=0.2
        ),
        Event(
            Event.generate_id(), 
            "file_write", 
            5678, 
            "suspicious_writer",
            severity=0.8
        ),
        Event(
            Event.generate_id(), 
            "process_spawn", 
            9999, 
            "xmrig_miner",
            severity=0.6
        ),
    ]
    
    print("\nProcessing events...")
    for event in events:
        print(f"\n→ Event: {event.event_type} from {event.process_name}")
        alerts = engine.run_rules(event)
        print(f"  Defense level: {engine.get_defense_level()}")
        print(f"  Alerts generated: {len(alerts)}")
    
    # Simulate low battery
    print("\n\nSimulating low battery...")
    engine.monitor.set_mock_values(battery=8.0)
    level = engine.adjust_defense_level()
    print(f"Defense level after low battery: {level}")
    
    print("\nFinal status:", engine.get_status())
