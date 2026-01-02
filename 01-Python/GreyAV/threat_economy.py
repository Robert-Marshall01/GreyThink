"""
Predictive Threat Economy Engine for GreyAV EDR System.

This module models the economic aspects of cyber attacks, calculating
attacker costs, defender costs, and ROI to identify economically viable
attack paths and defense imbalances.
"""

import uuid
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, Dict, Any, List, Tuple, Callable

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


@dataclass
class Event:
    """
    Represents a system event for economic analysis.
    
    Attributes:
        event_id: Unique identifier for the event.
        event_type: Type of event (process_spawn, file_write, network_connect, etc.).
        source: The entity initiating the event.
        target: The entity affected by the event.
        timestamp: When the event occurred.
        metadata: Additional context data.
    """
    event_id: str
    event_type: str
    source: str
    target: str
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class EconomyAlert:
    """
    Emitted when critical economic imbalances are detected.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        process_id: Process or entity being analyzed.
        attacker_cost: Estimated cost for the attacker.
        defender_cost: Estimated cost for the defender.
        roi: Return on investment ratio (attacker_cost / defender_cost).
        description: Human-readable description of the economic analysis.
        timestamp: When the alert was generated.
        risk_level: Severity level (low, medium, high, critical).
        recommended_actions: Suggested defense actions.
        metadata: Additional context about the analysis.
    """
    alert_id: str
    process_id: str
    attacker_cost: float
    defender_cost: float
    roi: float
    description: str
    timestamp: datetime = field(default_factory=datetime.now)
    risk_level: str = "medium"
    recommended_actions: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class AttackPath:
    """
    Represents a potential attack path with economic analysis.
    
    Attributes:
        path_id: Unique identifier for this path.
        events: Sequence of events in the attack path.
        total_attacker_cost: Cumulative attacker cost.
        total_defender_cost: Cumulative defender cost.
        viability_score: How economically viable this path is (0.0 - 1.0).
        blocked_at: Point where defense is most effective.
    """
    path_id: str
    events: List[str]
    total_attacker_cost: float
    total_defender_cost: float
    viability_score: float
    blocked_at: Optional[str] = None


class ThreatEconomyEngine:
    """
    Predictive Threat Economy Engine for cost-benefit analysis.
    
    Models attacker and defender costs for security events, calculates
    ROI of defense actions, and forecasts economically viable attack paths.
    
    Attributes:
        cost_models: Dictionary mapping event types to attacker costs.
        defense_models: Dictionary mapping defense actions to defender costs.
        roi_threshold: ROI threshold for triggering alerts.
        subscribers: Callbacks notified when economy alerts are generated.
    """
    
    # Default attacker cost models (higher = more expensive for attacker)
    DEFAULT_ATTACKER_COSTS: Dict[str, float] = {
        # Execution
        "process_spawn": 0.1,
        "script_execution": 0.15,
        "powershell_execution": 0.2,
        
        # Persistence
        "file_write": 0.2,
        "registry_write": 0.25,
        "scheduled_task": 0.3,
        "service_install": 0.35,
        "bootkit_install": 0.9,
        
        # Privilege Escalation
        "privilege_escalation": 0.7,
        "token_manipulation": 0.6,
        "exploit_execution": 0.8,
        
        # Defense Evasion
        "process_injection": 0.5,
        "file_delete": 0.1,
        "log_clear": 0.2,
        "timestomp": 0.3,
        "rootkit_install": 0.85,
        
        # Credential Access
        "credential_dump": 0.6,
        "keylog": 0.4,
        "brute_force": 0.3,
        
        # Discovery
        "system_info": 0.05,
        "network_scan": 0.15,
        "process_enum": 0.05,
        
        # Lateral Movement
        "remote_execution": 0.5,
        "pass_the_hash": 0.55,
        "rdp_session": 0.3,
        
        # Collection
        "file_read": 0.1,
        "file_copy": 0.15,
        "archive_create": 0.2,
        "screen_capture": 0.25,
        
        # Exfiltration
        "data_upload": 0.4,
        "dns_exfil": 0.5,
        "steganography": 0.6,
        
        # Command and Control
        "network_connect": 0.15,
        "dns_query": 0.1,
        "encrypted_channel": 0.25,
        
        # Impact
        "encrypt": 0.7,
        "file_modify": 0.2,
        "wipe": 0.8,
        "ddos": 0.4,
    }
    
    # Default defender cost models (higher = more expensive for defender)
    DEFAULT_DEFENDER_COSTS: Dict[str, float] = {
        # Monitoring (low cost)
        "monitor": 0.05,
        "log": 0.02,
        "alert": 0.1,
        
        # Analysis (medium cost)
        "analyze": 0.2,
        "investigate": 0.3,
        "correlate": 0.25,
        "hunt": 0.4,
        
        # Containment (medium-high cost)
        "isolate": 0.35,
        "quarantine": 0.3,
        "block_network": 0.25,
        "kill_process": 0.15,
        
        # Eradication (high cost)
        "remove_malware": 0.4,
        "patch": 0.5,
        "reimage": 0.7,
        "restore_backup": 0.6,
        
        # Recovery (high cost)
        "recover_files": 0.5,
        "rebuild_system": 0.8,
        "validate_integrity": 0.35,
        
        # Prevention (investment cost)
        "update_signatures": 0.15,
        "deploy_rule": 0.2,
        "train_model": 0.45,
        "harden_system": 0.55,
        
        # Incident Response (very high cost)
        "incident_response": 0.7,
        "forensics": 0.6,
        "legal_action": 0.9,
        "notify_users": 0.4,
    }
    
    # ROI thresholds for risk levels
    ROI_THRESHOLDS: Dict[str, float] = {
        "low": 0.5,
        "medium": 1.0,
        "high": 2.0,
        "critical": 5.0,
    }
    
    def __init__(
        self,
        cost_models: Optional[Dict[str, float]] = None,
        defense_models: Optional[Dict[str, float]] = None,
        roi_threshold: float = 1.5
    ):
        """
        Initialize the Threat Economy Engine.
        
        Args:
            cost_models: Custom attacker cost models.
            defense_models: Custom defender cost models.
            roi_threshold: ROI threshold for triggering alerts.
        """
        self.cost_models: Dict[str, float] = cost_models or self.DEFAULT_ATTACKER_COSTS.copy()
        self.defense_models: Dict[str, float] = defense_models or self.DEFAULT_DEFENDER_COSTS.copy()
        self.roi_threshold: float = roi_threshold
        self.subscribers: List[Callable[[EconomyAlert], None]] = []
        self._alert_history: List[EconomyAlert] = []
        self._attack_paths: List[AttackPath] = []
        logger.info("ThreatEconomyEngine initialized")
    
    def subscribe(self, callback: Callable[[EconomyAlert], None]) -> None:
        """
        Subscribe to economy alert notifications.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self.subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[EconomyAlert], None]) -> None:
        """
        Unsubscribe from economy alert notifications.
        
        Args:
            callback: Previously registered callback to remove.
        """
        if callback in self.subscribers:
            self.subscribers.remove(callback)
    
    def _notify_subscribers(self, alert: EconomyAlert) -> None:
        """Notify all subscribers of a new alert."""
        for callback in self.subscribers:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def estimate_attacker_cost(self, events: List[Event]) -> float:
        """
        Estimate the total cost for an attacker to execute events.
        
        Higher costs indicate more sophisticated/expensive attacks
        requiring more resources, skills, or time.
        
        Args:
            events: List of events to analyze.
            
        Returns:
            Total attacker cost (sum of individual event costs).
        """
        if not events:
            return 0.0
        
        total_cost = 0.0
        
        for event in events:
            base_cost = self.cost_models.get(event.event_type, 0.1)
            
            # Apply modifiers based on metadata
            modifier = 1.0
            
            # Stealth increases cost
            if event.metadata.get("stealthy", False):
                modifier *= 1.3
            
            # Persistence increases cost
            if event.metadata.get("persistent", False):
                modifier *= 1.2
            
            # Encrypted traffic increases cost
            if event.metadata.get("encrypted", False):
                modifier *= 1.15
            
            # Zero-day exploits are expensive
            if event.metadata.get("zero_day", False):
                modifier *= 2.0
            
            total_cost += base_cost * modifier
        
        logger.debug(f"Attacker cost for {len(events)} events: {total_cost:.3f}")
        return total_cost
    
    def estimate_defender_cost(self, actions: List[str]) -> float:
        """
        Estimate the total cost for a defender to execute actions.
        
        Higher costs indicate more resource-intensive defense operations
        requiring more personnel, time, or infrastructure.
        
        Args:
            actions: List of defense actions to analyze.
            
        Returns:
            Total defender cost (sum of individual action costs).
        """
        if not actions:
            return 0.0
        
        total_cost = 0.0
        
        for action in actions:
            cost = self.defense_models.get(action, 0.1)
            total_cost += cost
        
        logger.debug(f"Defender cost for {len(actions)} actions: {total_cost:.3f}")
        return total_cost
    
    def calculate_roi(self, attacker_cost: float, defender_cost: float) -> float:
        """
        Calculate the ROI ratio for an attack/defense scenario.
        
        ROI = attacker_cost / defender_cost
        
        Interpretation:
        - ROI > 1: Attack is expensive relative to defense (good for defender)
        - ROI < 1: Defense is expensive relative to attack (bad for defender)
        - ROI = 1: Balanced costs
        
        Args:
            attacker_cost: Total attacker cost.
            defender_cost: Total defender cost.
            
        Returns:
            ROI ratio (attacker_cost / defender_cost).
        """
        if defender_cost <= 0:
            return float('inf') if attacker_cost > 0 else 1.0
        
        roi = attacker_cost / defender_cost
        return round(roi, 4)
    
    def _determine_risk_level(self, roi: float) -> str:
        """
        Determine risk level based on ROI.
        
        Args:
            roi: The calculated ROI.
            
        Returns:
            Risk level string.
        """
        # Lower ROI = worse for defender
        if roi < self.ROI_THRESHOLDS["low"]:
            return "critical"  # Defense too expensive
        elif roi < self.ROI_THRESHOLDS["medium"]:
            return "high"
        elif roi < self.ROI_THRESHOLDS["high"]:
            return "medium"
        else:
            return "low"  # Attack expensive relative to defense
    
    def _recommend_actions(
        self,
        events: List[Event],
        current_actions: List[str],
        roi: float
    ) -> List[str]:
        """
        Recommend defense actions based on economic analysis.
        
        Args:
            events: The attack events.
            current_actions: Current defense actions.
            roi: Calculated ROI.
            
        Returns:
            List of recommended actions.
        """
        recommendations = []
        
        if roi < 0.5:
            # Defense is too expensive - optimize
            recommendations.append("reduce_monitoring_scope")
            recommendations.append("automate_response")
            recommendations.append("deploy_deception")
        elif roi < 1.0:
            # Slightly unfavorable - balance needed
            recommendations.append("enhance_detection")
            recommendations.append("update_signatures")
        else:
            # Favorable - maintain or invest
            if "monitor" not in current_actions:
                recommendations.append("monitor")
            if any(e.event_type in ["privilege_escalation", "credential_dump"] for e in events):
                recommendations.append("isolate")
            if any(e.event_type in ["encrypt", "wipe"] for e in events):
                recommendations.append("quarantine")
        
        # Add event-specific recommendations
        event_types = {e.event_type for e in events}
        
        if "data_upload" in event_types or "dns_exfil" in event_types:
            recommendations.append("block_network")
        
        if "process_injection" in event_types:
            recommendations.append("kill_process")
        
        if "credential_dump" in event_types:
            recommendations.append("rotate_credentials")
        
        return list(set(recommendations))
    
    def analyze(
        self,
        events: List[Event],
        actions: List[str]
    ) -> EconomyAlert:
        """
        Analyze events and defense actions, generating an economy alert.
        
        Calculates attacker and defender costs, computes ROI, and generates
        an alert with recommendations if thresholds are exceeded.
        
        Args:
            events: List of attack events to analyze.
            actions: List of defense actions being taken.
            
        Returns:
            EconomyAlert with economic analysis results.
        """
        # Calculate costs
        attacker_cost = self.estimate_attacker_cost(events)
        defender_cost = self.estimate_defender_cost(actions)
        roi = self.calculate_roi(attacker_cost, defender_cost)
        
        # Determine risk level
        risk_level = self._determine_risk_level(roi)
        
        # Get recommendations
        recommendations = self._recommend_actions(events, actions, roi)
        
        # Determine process/entity ID
        process_id = events[0].source if events else "unknown"
        
        # Build description
        if roi < self.roi_threshold:
            description = (
                f"Economic imbalance detected. ROI: {roi:.2f} (threshold: {self.roi_threshold}). "
                f"Attacker cost: {attacker_cost:.2f}, Defender cost: {defender_cost:.2f}. "
                f"Defense operations are relatively expensive - consider optimization."
            )
        else:
            description = (
                f"Economic analysis complete. ROI: {roi:.2f}. "
                f"Attacker cost: {attacker_cost:.2f}, Defender cost: {defender_cost:.2f}. "
                f"Defense posture is economically favorable."
            )
        
        # Create alert
        alert = EconomyAlert(
            alert_id=str(uuid.uuid4()),
            process_id=process_id,
            attacker_cost=attacker_cost,
            defender_cost=defender_cost,
            roi=roi,
            description=description,
            risk_level=risk_level,
            recommended_actions=recommendations,
            metadata={
                "event_count": len(events),
                "action_count": len(actions),
                "event_types": [e.event_type for e in events],
                "actions": actions,
            },
        )
        
        # Store alert
        self._alert_history.append(alert)
        
        # Notify if threshold exceeded
        if roi < self.roi_threshold:
            self._notify_subscribers(alert)
            logger.warning(f"Economy alert: ROI {roi:.2f} below threshold {self.roi_threshold}")
        else:
            logger.info(f"Economy analysis: ROI {roi:.2f} is acceptable")
        
        return alert
    
    def forecast_attack_path(
        self,
        events: List[Event],
        max_steps: int = 10
    ) -> AttackPath:
        """
        Forecast economically viable attack path extensions.
        
        Predicts what attack steps are likely based on current events
        and economic viability.
        
        Args:
            events: Current attack events.
            max_steps: Maximum forecast steps.
            
        Returns:
            AttackPath with viability analysis.
        """
        current_cost = self.estimate_attacker_cost(events)
        event_types = [e.event_type for e in events]
        
        # Predict next likely steps based on attack patterns
        predictions = self._predict_next_steps(events)
        
        # Calculate path viability
        predicted_events = []
        for step in predictions[:max_steps]:
            predicted_events.append(
                Event(
                    event_id=f"predicted-{len(predicted_events)}",
                    event_type=step,
                    source="predicted",
                    target="predicted",
                )
            )
        
        predicted_cost = self.estimate_attacker_cost(predicted_events)
        total_attacker_cost = current_cost + predicted_cost
        
        # Estimate defender cost to counter
        defense_actions = self._estimate_required_defense(event_types + predictions)
        total_defender_cost = self.estimate_defender_cost(defense_actions)
        
        # Calculate viability (higher = more viable for attacker)
        if total_attacker_cost > 0:
            viability = 1.0 - (total_attacker_cost / (total_attacker_cost + total_defender_cost))
        else:
            viability = 0.5
        
        # Find optimal blocking point
        blocked_at = self._find_optimal_block_point(event_types + predictions)
        
        attack_path = AttackPath(
            path_id=str(uuid.uuid4()),
            events=event_types + predictions,
            total_attacker_cost=total_attacker_cost,
            total_defender_cost=total_defender_cost,
            viability_score=viability,
            blocked_at=blocked_at,
        )
        
        self._attack_paths.append(attack_path)
        
        logger.info(
            f"Attack path forecast: {len(predictions)} predicted steps, "
            f"viability: {viability:.2f}"
        )
        
        return attack_path
    
    def _predict_next_steps(self, events: List[Event]) -> List[str]:
        """
        Predict likely next attack steps based on current events.
        
        Uses attack chain patterns to predict progression.
        """
        predictions = []
        event_types = {e.event_type for e in events}
        
        # Attack chain patterns
        patterns = {
            "process_spawn": ["file_write", "network_connect", "privilege_escalation"],
            "privilege_escalation": ["credential_dump", "process_injection", "remote_execution"],
            "credential_dump": ["pass_the_hash", "remote_execution", "lateral_movement"],
            "file_write": ["scheduled_task", "service_install", "registry_write"],
            "network_connect": ["dns_query", "data_upload", "encrypted_channel"],
            "file_read": ["archive_create", "file_copy", "data_upload"],
            "encrypt": ["wipe", "network_connect"],
        }
        
        for event_type in event_types:
            if event_type in patterns:
                for next_step in patterns[event_type]:
                    if next_step not in event_types and next_step not in predictions:
                        predictions.append(next_step)
        
        return predictions[:5]  # Limit predictions
    
    def _estimate_required_defense(self, event_types: List[str]) -> List[str]:
        """
        Estimate required defense actions for given attack events.
        """
        defense_map = {
            "process_spawn": ["monitor", "analyze"],
            "privilege_escalation": ["alert", "investigate", "isolate"],
            "credential_dump": ["alert", "incident_response", "rotate_credentials"],
            "data_upload": ["block_network", "investigate"],
            "encrypt": ["quarantine", "incident_response", "recover_files"],
            "process_injection": ["kill_process", "investigate"],
            "remote_execution": ["isolate", "incident_response"],
        }
        
        actions = set()
        for event_type in event_types:
            if event_type in defense_map:
                actions.update(defense_map[event_type])
        
        return list(actions) or ["monitor", "analyze"]
    
    def _find_optimal_block_point(self, event_types: List[str]) -> Optional[str]:
        """
        Find the optimal point to block an attack chain.
        
        Returns the event type where blocking provides best ROI.
        """
        if not event_types:
            return None
        
        best_point = None
        best_value = 0.0
        
        for i, event_type in enumerate(event_types):
            # Cost to reach this point
            cost_to_here = sum(
                self.cost_models.get(et, 0.1)
                for et in event_types[:i+1]
            )
            
            # Remaining attack value
            remaining_cost = sum(
                self.cost_models.get(et, 0.1)
                for et in event_types[i+1:]
            )
            
            # Block value = cost already spent + remaining attack prevented
            block_value = cost_to_here + remaining_cost * 1.5
            
            if block_value > best_value:
                best_value = block_value
                best_point = event_type
        
        return best_point
    
    def get_cost_efficiency(self, action: str) -> Dict[str, float]:
        """
        Get cost efficiency metrics for a defense action.
        
        Args:
            action: Defense action to analyze.
            
        Returns:
            Dictionary with efficiency metrics.
        """
        defender_cost = self.defense_models.get(action, 0.1)
        
        # Calculate how many attack types this action can counter
        countered_attacks = []
        for event_type, attacker_cost in self.cost_models.items():
            if self._action_counters_event(action, event_type):
                countered_attacks.append((event_type, attacker_cost))
        
        total_countered_value = sum(cost for _, cost in countered_attacks)
        
        return {
            "action": action,
            "defender_cost": defender_cost,
            "attacks_countered": len(countered_attacks),
            "total_attack_value_countered": total_countered_value,
            "efficiency_ratio": total_countered_value / defender_cost if defender_cost > 0 else 0,
        }
    
    def _action_counters_event(self, action: str, event_type: str) -> bool:
        """Check if a defense action can counter an event type."""
        counters = {
            "quarantine": ["encrypt", "wipe", "file_modify"],
            "kill_process": ["process_spawn", "process_injection", "keylog"],
            "block_network": ["data_upload", "network_connect", "dns_exfil"],
            "isolate": ["lateral_movement", "remote_execution", "pass_the_hash"],
            "update_signatures": ["file_write", "script_execution"],
        }
        
        return event_type in counters.get(action, [])
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get engine statistics.
        
        Returns:
            Dictionary with engine statistics.
        """
        if not self._alert_history:
            return {
                "total_alerts": 0,
                "avg_roi": 0,
                "attack_paths_analyzed": len(self._attack_paths),
            }
        
        rois = [a.roi for a in self._alert_history]
        risk_counts = {}
        for alert in self._alert_history:
            risk_counts[alert.risk_level] = risk_counts.get(alert.risk_level, 0) + 1
        
        return {
            "total_alerts": len(self._alert_history),
            "avg_roi": sum(rois) / len(rois),
            "min_roi": min(rois),
            "max_roi": max(rois),
            "risk_distribution": risk_counts,
            "attack_paths_analyzed": len(self._attack_paths),
        }
    
    def get_alert_history(
        self,
        risk_level: Optional[str] = None,
        min_roi: Optional[float] = None,
        max_roi: Optional[float] = None
    ) -> List[EconomyAlert]:
        """
        Get filtered alert history.
        
        Args:
            risk_level: Filter by risk level.
            min_roi: Minimum ROI filter.
            max_roi: Maximum ROI filter.
            
        Returns:
            List of matching alerts.
        """
        results = self._alert_history
        
        if risk_level:
            results = [a for a in results if a.risk_level == risk_level]
        
        if min_roi is not None:
            results = [a for a in results if a.roi >= min_roi]
        
        if max_roi is not None:
            results = [a for a in results if a.roi <= max_roi]
        
        return results
    
    def clear(self) -> None:
        """Clear alert history and attack paths."""
        self._alert_history.clear()
        self._attack_paths.clear()
        logger.info("ThreatEconomyEngine cleared")


# Factory function for pipeline integration
def create_economy_engine(
    roi_threshold: float = 1.5
) -> ThreatEconomyEngine:
    """
    Factory function to create a ThreatEconomyEngine instance.
    
    Args:
        roi_threshold: ROI threshold for triggering alerts.
        
    Returns:
        New ThreatEconomyEngine instance.
    """
    return ThreatEconomyEngine(roi_threshold=roi_threshold)


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    engine = create_economy_engine(roi_threshold=1.0)
    
    # Subscribe to alerts
    def on_alert(alert: EconomyAlert):
        print(f">>> ECONOMY ALERT [{alert.risk_level.upper()}]")
        print(f"    ROI: {alert.roi:.2f}")
        print(f"    Attacker Cost: {alert.attacker_cost:.2f}")
        print(f"    Defender Cost: {alert.defender_cost:.2f}")
        print(f"    Recommendations: {', '.join(alert.recommended_actions)}\n")
    
    engine.subscribe(on_alert)
    
    # Scenario 1: Simple reconnaissance (low attacker cost)
    print("=== Scenario 1: Reconnaissance ===\n")
    recon_events = [
        Event("e1", "system_info", "attacker.exe", "system"),
        Event("e2", "process_enum", "attacker.exe", "system"),
        Event("e3", "network_scan", "attacker.exe", "192.168.1.0/24"),
    ]
    recon_actions = ["monitor", "log", "alert"]
    alert1 = engine.analyze(recon_events, recon_actions)
    print(f"Result: {alert1.description}\n")
    
    # Scenario 2: Sophisticated attack (high attacker cost)
    print("=== Scenario 2: Sophisticated Attack ===\n")
    apt_events = [
        Event("e4", "exploit_execution", "browser.exe", "system", 
              metadata={"zero_day": True}),
        Event("e5", "privilege_escalation", "payload.exe", "system"),
        Event("e6", "credential_dump", "mimikatz.exe", "lsass.exe"),
        Event("e7", "pass_the_hash", "attacker.exe", "dc01.corp.local"),
    ]
    apt_actions = ["isolate", "incident_response", "forensics"]
    alert2 = engine.analyze(apt_events, apt_actions)
    print(f"Result: {alert2.description}\n")
    
    # Scenario 3: Ransomware (imbalanced defense cost)
    print("=== Scenario 3: Ransomware (High Defense Cost) ===\n")
    ransomware_events = [
        Event("e8", "process_spawn", "malware.exe", "crypto.exe"),
        Event("e9", "encrypt", "crypto.exe", "documents/"),
        Event("e10", "file_modify", "crypto.exe", "ransom_note.txt"),
    ]
    ransomware_actions = [
        "quarantine", "incident_response", "recover_files", 
        "rebuild_system", "forensics", "notify_users"
    ]
    alert3 = engine.analyze(ransomware_events, ransomware_actions)
    print(f"Result: {alert3.description}\n")
    
    # Forecast attack path
    print("=== Attack Path Forecast ===\n")
    path = engine.forecast_attack_path(apt_events)
    print(f"Path ID: {path.path_id}")
    print(f"Events: {' -> '.join(path.events[:8])}...")
    print(f"Viability Score: {path.viability_score:.2f}")
    print(f"Optimal Block Point: {path.blocked_at}")
    print(f"Total Attacker Cost: {path.total_attacker_cost:.2f}")
    print(f"Total Defender Cost: {path.total_defender_cost:.2f}")
    
    # Cost efficiency analysis
    print("\n=== Defense Action Efficiency ===\n")
    for action in ["quarantine", "block_network", "kill_process", "monitor"]:
        efficiency = engine.get_cost_efficiency(action)
        print(f"{action}:")
        print(f"  Cost: {efficiency['defender_cost']:.2f}")
        print(f"  Attacks Countered: {efficiency['attacks_countered']}")
        print(f"  Efficiency Ratio: {efficiency['efficiency_ratio']:.2f}")
    
    # Statistics
    print("\n=== Engine Statistics ===")
    stats = engine.get_statistics()
    print(f"Total Alerts: {stats['total_alerts']}")
    print(f"Average ROI: {stats['avg_roi']:.2f}")
    print(f"Risk Distribution: {stats['risk_distribution']}")
