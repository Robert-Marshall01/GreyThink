"""
Adaptive Trust Fabric for GreyAV EDR System.

This module maintains dynamic trust scores for system entities (users, processes, files)
and propagates trust changes across related entities to detect compromised assets.
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Optional
import uuid


@dataclass
class Event:
    """
    Represents a system event that may affect trust scores.
    
    Attributes:
        event_type: Type of event (e.g., 'privilege_escalation', 'file_tampering').
        entity_id: The primary entity involved in this event.
        entity_type: Type of entity ('user', 'process', 'file').
        severity: Severity score (0.0 to 1.0).
        timestamp: When the event occurred.
        metadata: Additional event-specific data.
    """
    event_type: str
    entity_id: str
    entity_type: str = "process"
    severity: float = 0.5
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: dict = field(default_factory=dict)


@dataclass
class TrustAlert:
    """
    Alert object emitted when an entity's trust score falls below threshold.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        entity_type: Type of entity ('user', 'process', 'file').
        entity_id: Identifier of the affected entity.
        old_score: Trust score before the change.
        new_score: Trust score after the change.
        description: Human-readable description of the trust change.
        timestamp: When the alert was generated.
    """
    alert_id: str
    entity_type: str
    entity_id: str
    old_score: float
    new_score: float
    description: str
    timestamp: datetime = field(default_factory=datetime.now)

    @staticmethod
    def generate_id() -> str:
        """Generate a unique alert ID."""
        return f"TA-{uuid.uuid4().hex[:12].upper()}"


class TrustFabric:
    """
    Adaptive Trust Fabric for maintaining and propagating trust scores.
    
    The fabric tracks trust scores for all system entities and their relationships,
    allowing trust degradation to propagate across connected entities when
    suspicious activity is detected.
    
    Attributes:
        trust_scores: Dictionary mapping entity IDs to trust scores (0.0-1.0).
        relationships: Dictionary mapping entity IDs to lists of related entity IDs.
    """

    # Default trust score for new entities
    DEFAULT_TRUST_SCORE: float = 0.8

    # Threshold below which alerts are generated
    ALERT_THRESHOLD: float = 0.3

    # Critical threshold for immediate action
    CRITICAL_THRESHOLD: float = 0.1

    # Event type to trust delta mappings (negative values reduce trust)
    EVENT_TRUST_DELTAS: dict[str, float] = {
        # Severe trust reduction events
        'privilege_escalation': -0.35,
        'credential_theft': -0.40,
        'process_injection': -0.35,
        'rootkit_detected': -0.50,
        'ransomware_behavior': -0.45,
        
        # Moderate trust reduction events
        'suspicious_lineage': -0.20,
        'file_tampering': -0.25,
        'unauthorized_access': -0.25,
        'defense_evasion': -0.20,
        'lateral_movement': -0.30,
        
        # Minor trust reduction events
        'anomalous_behavior': -0.10,
        'policy_violation': -0.15,
        'failed_authentication': -0.10,
        'unusual_network_activity': -0.15,
        
        # Trust restoration events (positive deltas)
        'verified_signature': 0.10,
        'successful_scan': 0.05,
        'admin_approval': 0.20,
        'behavioral_baseline_match': 0.08,
    }

    # Propagation factors by relationship type
    PROPAGATION_FACTORS: dict[str, float] = {
        'user_to_process': 0.7,
        'process_to_file': 0.5,
        'process_to_child': 0.8,
        'file_to_process': 0.4,
        'default': 0.5,
    }

    def __init__(self) -> None:
        """Initialize an empty trust fabric."""
        self.trust_scores: dict[str, float] = {}
        self.relationships: dict[str, list[str]] = {}
        self._entity_types: dict[str, str] = {}  # entity_id → entity_type
        self._alert_subscribers: list[Callable[[TrustAlert], None]] = []
        self._trust_history: dict[str, list[tuple[datetime, float, str]]] = {}

    def set_initial_trust(
        self, 
        entity_id: str, 
        score: float, 
        entity_type: str = "process"
    ) -> None:
        """
        Set the initial trust score for an entity.
        
        Args:
            entity_id: Unique identifier for the entity.
            score: Initial trust score (0.0 to 1.0).
            entity_type: Type of entity ('user', 'process', 'file').
        """
        clamped_score = max(0.0, min(1.0, score))
        self.trust_scores[entity_id] = clamped_score
        self._entity_types[entity_id] = entity_type
        
        # Initialize relationship list if not exists
        if entity_id not in self.relationships:
            self.relationships[entity_id] = []
        
        # Initialize trust history
        if entity_id not in self._trust_history:
            self._trust_history[entity_id] = []
        self._trust_history[entity_id].append(
            (datetime.now(), clamped_score, "initial_trust_set")
        )

    def get_trust(self, entity_id: str) -> float:
        """
        Get the current trust score for an entity.
        
        Args:
            entity_id: Entity identifier.
            
        Returns:
            Trust score, or DEFAULT_TRUST_SCORE if entity not found.
        """
        return self.trust_scores.get(entity_id, self.DEFAULT_TRUST_SCORE)

    def get_entity_type(self, entity_id: str) -> str:
        """
        Get the type of an entity.
        
        Args:
            entity_id: Entity identifier.
            
        Returns:
            Entity type string.
        """
        return self._entity_types.get(entity_id, "unknown")

    def add_relationship(
        self, 
        entity_id: str, 
        related_id: str, 
        bidirectional: bool = False
    ) -> None:
        """
        Add a relationship between two entities.
        
        Args:
            entity_id: Primary entity identifier.
            related_id: Related entity identifier.
            bidirectional: If True, add relationship in both directions.
        """
        # Ensure both entities exist with default trust
        if entity_id not in self.trust_scores:
            self.set_initial_trust(entity_id, self.DEFAULT_TRUST_SCORE)
        if related_id not in self.trust_scores:
            self.set_initial_trust(related_id, self.DEFAULT_TRUST_SCORE)
        
        # Add forward relationship
        if entity_id not in self.relationships:
            self.relationships[entity_id] = []
        if related_id not in self.relationships[entity_id]:
            self.relationships[entity_id].append(related_id)
        
        # Add reverse relationship if bidirectional
        if bidirectional:
            if related_id not in self.relationships:
                self.relationships[related_id] = []
            if entity_id not in self.relationships[related_id]:
                self.relationships[related_id].append(entity_id)

    def remove_entity(self, entity_id: str) -> None:
        """
        Remove an entity and all its relationships from the fabric.
        
        Args:
            entity_id: Entity identifier to remove.
        """
        self.trust_scores.pop(entity_id, None)
        self._entity_types.pop(entity_id, None)
        self.relationships.pop(entity_id, None)
        self._trust_history.pop(entity_id, None)
        
        # Remove from other entities' relationship lists
        for related_list in self.relationships.values():
            if entity_id in related_list:
                related_list.remove(entity_id)

    def update_trust(
        self, 
        entity_id: str, 
        delta: float, 
        reason: str
    ) -> Optional[TrustAlert]:
        """
        Update the trust score for an entity by a delta value.
        
        Args:
            entity_id: Entity identifier.
            delta: Change in trust score (negative reduces trust).
            reason: Description of why trust is being updated.
            
        Returns:
            TrustAlert if score falls below threshold, None otherwise.
        """
        # Get current score or initialize with default
        old_score = self.trust_scores.get(entity_id, self.DEFAULT_TRUST_SCORE)
        
        # Ensure entity exists
        if entity_id not in self.trust_scores:
            self.set_initial_trust(entity_id, self.DEFAULT_TRUST_SCORE)
        
        # Calculate new score (clamped to [0, 1])
        new_score = max(0.0, min(1.0, old_score + delta))
        self.trust_scores[entity_id] = new_score
        
        # Record in history
        if entity_id not in self._trust_history:
            self._trust_history[entity_id] = []
        self._trust_history[entity_id].append((datetime.now(), new_score, reason))
        
        # Keep history bounded
        if len(self._trust_history[entity_id]) > 100:
            self._trust_history[entity_id] = self._trust_history[entity_id][-100:]
        
        # Check if alert should be generated
        alert = None
        if new_score < self.ALERT_THRESHOLD and old_score >= self.ALERT_THRESHOLD:
            # Crossed below threshold
            alert = self._create_alert(entity_id, old_score, new_score, reason)
        elif new_score < self.CRITICAL_THRESHOLD and old_score >= self.CRITICAL_THRESHOLD:
            # Crossed into critical zone
            alert = self._create_alert(
                entity_id, old_score, new_score, 
                f"CRITICAL: {reason}"
            )
        
        if alert:
            self._notify_subscribers(alert)
        
        return alert

    def _create_alert(
        self, 
        entity_id: str, 
        old_score: float, 
        new_score: float, 
        reason: str
    ) -> TrustAlert:
        """
        Create a TrustAlert for an entity.
        
        Args:
            entity_id: Entity identifier.
            old_score: Previous trust score.
            new_score: Current trust score.
            reason: Reason for trust change.
            
        Returns:
            TrustAlert object.
        """
        entity_type = self._entity_types.get(entity_id, "unknown")
        severity = "CRITICAL" if new_score < self.CRITICAL_THRESHOLD else "WARNING"
        
        description = (
            f"{severity}: Trust score for {entity_type} '{entity_id}' "
            f"dropped from {old_score:.2f} to {new_score:.2f}. "
            f"Reason: {reason}"
        )
        
        return TrustAlert(
            alert_id=TrustAlert.generate_id(),
            entity_type=entity_type,
            entity_id=entity_id,
            old_score=old_score,
            new_score=new_score,
            description=description,
            timestamp=datetime.now()
        )

    def propagate_trust(
        self, 
        entity_id: str, 
        factor: float = 0.5,
        visited: Optional[set[str]] = None
    ) -> list[TrustAlert]:
        """
        Propagate trust changes to related entities.
        
        When an entity's trust score changes significantly, this method
        propagates a portion of that change to related entities.
        
        Args:
            entity_id: Entity whose trust should propagate.
            factor: Propagation factor (0.0 to 1.0) controlling decay.
            visited: Set of already-visited entities (for recursion control).
            
        Returns:
            List of TrustAlerts generated during propagation.
        """
        if visited is None:
            visited = set()
        
        if entity_id in visited:
            return []
        
        visited.add(entity_id)
        alerts: list[TrustAlert] = []
        
        source_score = self.get_trust(entity_id)
        related_entities = self.relationships.get(entity_id, [])
        
        for related_id in related_entities:
            if related_id in visited:
                continue
            
            related_score = self.get_trust(related_id)
            
            # Calculate trust delta based on source entity's trust
            # If source has low trust, reduce related entity's trust
            if source_score < self.DEFAULT_TRUST_SCORE:
                trust_deficit = self.DEFAULT_TRUST_SCORE - source_score
                delta = -trust_deficit * factor
                
                if abs(delta) > 0.01:  # Only propagate meaningful changes
                    alert = self.update_trust(
                        related_id, 
                        delta, 
                        f"Trust propagation from {entity_id}"
                    )
                    if alert:
                        alerts.append(alert)
                    
                    # Recursive propagation with decaying factor
                    if factor > 0.1:
                        sub_alerts = self.propagate_trust(
                            related_id, 
                            factor * 0.5, 
                            visited
                        )
                        alerts.extend(sub_alerts)
        
        return alerts

    def handle_event(self, event: Event) -> list[TrustAlert]:
        """
        Handle a system event and update trust accordingly.
        
        This method processes events, adjusts trust scores based on event type,
        and propagates trust changes to related entities.
        
        Args:
            event: The system event to process.
            
        Returns:
            List of TrustAlerts generated from this event.
        """
        alerts: list[TrustAlert] = []
        entity_id = event.entity_id
        
        # Ensure entity exists
        if entity_id not in self.trust_scores:
            self.set_initial_trust(
                entity_id, 
                self.DEFAULT_TRUST_SCORE, 
                event.entity_type
            )
        
        # Get base delta from event type
        base_delta = self.EVENT_TRUST_DELTAS.get(event.event_type, 0.0)
        
        # Scale delta by event severity
        scaled_delta = base_delta * event.severity
        
        # Apply additional modifiers from metadata
        if event.metadata.get('repeated_offense', False):
            scaled_delta *= 1.5  # Increase penalty for repeat offenders
        
        if event.metadata.get('verified_source', False):
            scaled_delta *= 0.5  # Reduce penalty if source is verified
        
        # Update trust for the primary entity
        if abs(scaled_delta) > 0.001:
            reason = f"{event.event_type} (severity: {event.severity:.2f})"
            alert = self.update_trust(entity_id, scaled_delta, reason)
            if alert:
                alerts.append(alert)
            
            # Propagate trust changes if significant
            if abs(scaled_delta) > 0.1:
                propagation_factor = self._get_propagation_factor(event.entity_type)
                prop_alerts = self.propagate_trust(entity_id, propagation_factor)
                alerts.extend(prop_alerts)
        
        return alerts

    def _get_propagation_factor(self, entity_type: str) -> float:
        """
        Get the propagation factor for an entity type.
        
        Args:
            entity_type: Type of entity.
            
        Returns:
            Propagation factor.
        """
        factor_key = f"{entity_type}_to_process"
        return self.PROPAGATION_FACTORS.get(
            factor_key, 
            self.PROPAGATION_FACTORS['default']
        )

    def subscribe(self, callback: Callable[[TrustAlert], None]) -> None:
        """
        Subscribe to trust alerts.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self._alert_subscribers.append(callback)

    def unsubscribe(self, callback: Callable[[TrustAlert], None]) -> None:
        """
        Unsubscribe from trust alerts.
        
        Args:
            callback: Previously registered callback function.
        """
        if callback in self._alert_subscribers:
            self._alert_subscribers.remove(callback)

    def _notify_subscribers(self, alert: TrustAlert) -> None:
        """
        Notify all subscribers of a new alert.
        
        Args:
            alert: The alert to broadcast.
        """
        for callback in self._alert_subscribers:
            try:
                callback(alert)
            except Exception as e:
                print(f"[TrustFabric] Subscriber notification failed: {e}")

    def get_low_trust_entities(
        self, 
        threshold: Optional[float] = None
    ) -> list[tuple[str, float]]:
        """
        Get all entities with trust scores below a threshold.
        
        Args:
            threshold: Trust score threshold (default: ALERT_THRESHOLD).
            
        Returns:
            List of (entity_id, trust_score) tuples sorted by score ascending.
        """
        if threshold is None:
            threshold = self.ALERT_THRESHOLD
        
        low_trust = [
            (entity_id, score)
            for entity_id, score in self.trust_scores.items()
            if score < threshold
        ]
        
        return sorted(low_trust, key=lambda x: x[1])

    def get_trust_history(
        self, 
        entity_id: str
    ) -> list[tuple[datetime, float, str]]:
        """
        Get the trust score history for an entity.
        
        Args:
            entity_id: Entity identifier.
            
        Returns:
            List of (timestamp, score, reason) tuples.
        """
        return self._trust_history.get(entity_id, [])

    def get_fabric_stats(self) -> dict[str, Any]:
        """
        Get statistics about the current trust fabric state.
        
        Returns:
            Dictionary containing fabric metrics.
        """
        scores = list(self.trust_scores.values())
        
        return {
            "entity_count": len(self.trust_scores),
            "relationship_count": sum(
                len(related) for related in self.relationships.values()
            ),
            "avg_trust_score": sum(scores) / len(scores) if scores else 0.0,
            "min_trust_score": min(scores) if scores else 0.0,
            "max_trust_score": max(scores) if scores else 0.0,
            "low_trust_count": len(self.get_low_trust_entities()),
            "critical_trust_count": len([
                s for s in scores if s < self.CRITICAL_THRESHOLD
            ]),
        }

    def reset_trust(self, entity_id: str) -> None:
        """
        Reset an entity's trust score to the default value.
        
        Args:
            entity_id: Entity identifier.
        """
        if entity_id in self.trust_scores:
            old_score = self.trust_scores[entity_id]
            self.trust_scores[entity_id] = self.DEFAULT_TRUST_SCORE
            
            if entity_id in self._trust_history:
                self._trust_history[entity_id].append(
                    (datetime.now(), self.DEFAULT_TRUST_SCORE, "trust_reset")
                )

    def clear(self) -> None:
        """Reset the fabric to an empty state."""
        self.trust_scores.clear()
        self.relationships.clear()
        self._entity_types.clear()
        self._trust_history.clear()

    def to_dict(self) -> dict[str, Any]:
        """
        Serialize the fabric to a dictionary for persistence.
        
        Returns:
            Dictionary representation of the fabric.
        """
        return {
            "trust_scores": dict(self.trust_scores),
            "relationships": {k: list(v) for k, v in self.relationships.items()},
            "entity_types": dict(self._entity_types),
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "TrustFabric":
        """
        Deserialize a fabric from a dictionary.
        
        Args:
            data: Dictionary representation of the fabric.
            
        Returns:
            Reconstructed TrustFabric instance.
        """
        fabric = cls()
        fabric.trust_scores = dict(data.get("trust_scores", {}))
        fabric.relationships = {
            k: list(v) for k, v in data.get("relationships", {}).items()
        }
        fabric._entity_types = dict(data.get("entity_types", {}))
        return fabric


# Pipeline integration handler
def create_trust_fabric_handler(fabric: TrustFabric) -> Callable[[Event], list[TrustAlert]]:
    """
    Create an event handler for pipeline integration.
    
    This factory creates a handler that can be subscribed to an event pipeline
    to automatically update the trust fabric when events occur.
    
    Args:
        fabric: The TrustFabric instance to update.
        
    Returns:
        Event handler function compatible with pipeline subscription.
    """
    def handler(event: Event) -> list[TrustAlert]:
        return fabric.handle_event(event)
    
    return handler


if __name__ == "__main__":
    # Demo usage
    fabric = TrustFabric()
    
    # Set up some entities with relationships
    fabric.set_initial_trust("user_admin", 0.9, "user")
    fabric.set_initial_trust("process_nginx", 0.85, "process")
    fabric.set_initial_trust("process_shell", 0.75, "process")
    fabric.set_initial_trust("/etc/passwd", 0.95, "file")
    
    # Establish relationships
    fabric.add_relationship("user_admin", "process_nginx")
    fabric.add_relationship("process_nginx", "process_shell")
    fabric.add_relationship("process_shell", "/etc/passwd")
    
    # Subscribe to alerts
    def alert_handler(alert: TrustAlert) -> None:
        print(f"\n[TRUST ALERT] {alert.alert_id}")
        print(f"  Entity: {alert.entity_type} - {alert.entity_id}")
        print(f"  Score: {alert.old_score:.2f} → {alert.new_score:.2f}")
        print(f"  {alert.description}")
    
    fabric.subscribe(alert_handler)
    
    # Simulate suspicious events
    events = [
        Event("suspicious_lineage", "process_shell", "process", 0.8),
        Event("privilege_escalation", "process_shell", "process", 0.9),
        Event("file_tampering", "/etc/passwd", "file", 0.7),
    ]
    
    print("Processing events...")
    for event in events:
        print(f"\n→ Event: {event.event_type} on {event.entity_id}")
        alerts = fabric.handle_event(event)
        print(f"  Generated {len(alerts)} alert(s)")
    
    print(f"\nFabric stats: {fabric.get_fabric_stats()}")
    
    print("\nLow trust entities:")
    for entity_id, score in fabric.get_low_trust_entities():
        print(f"  {entity_id}: {score:.2f}")
