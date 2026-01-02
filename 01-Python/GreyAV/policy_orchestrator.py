"""
Autonomous Policy Orchestration Engine for GreyAV EDR System.

This module maintains defense policies, dynamically adjusts them based on
risk scores and alerts, enforces policies on processes/files/network flows,
and provides full audit logging for compliance and forensics.
"""

import uuid
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, Dict, Any, List, Callable

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


@dataclass
class Alert:
    """
    Represents an incoming security alert from the detection pipeline.
    
    Attributes:
        alert_id: Unique identifier for the alert.
        source: Origin of the alert (e.g., 'behavioral_engine', 'signature_scan').
        entity: The entity involved (process, file path, network address).
        risk_score: Numeric risk score (0.0 - 1.0).
        description: Human-readable description of the alert.
        metadata: Additional context data.
    """
    alert_id: str
    source: str
    entity: str
    risk_score: float
    description: str
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class PolicyAlert:
    """
    Emitted when a policy is auto-adjusted by the orchestrator.
    
    Attributes:
        alert_id: Unique identifier for this policy change alert.
        policy_name: Name of the policy that was modified.
        old_action: Previous action (e.g., 'allow', 'monitor').
        new_action: New action (e.g., 'block', 'quarantine').
        reason: Explanation for the policy adjustment.
        timestamp: When the policy change occurred.
    """
    alert_id: str
    policy_name: str
    old_action: str
    new_action: str
    reason: str
    timestamp: datetime = field(default_factory=datetime.now)


class PolicyStore:
    """
    Persistent store for defense policies.
    
    Manages a dictionary of policy names mapped to their current actions.
    Actions include: 'allow', 'block', 'quarantine', 'monitor', 'alert'.
    
    Attributes:
        policies: Dictionary mapping policy names to actions.
    """
    
    def __init__(self, initial_policies: Optional[Dict[str, str]] = None):
        """
        Initialize the policy store.
        
        Args:
            initial_policies: Optional dictionary of initial policies.
        """
        self.policies: Dict[str, str] = initial_policies or self._default_policies()
    
    def _default_policies(self) -> Dict[str, str]:
        """Return default policy configurations."""
        return {
            "unknown_process": "monitor",
            "suspicious_network": "alert",
            "malware_detected": "quarantine",
            "unauthorized_access": "block",
            "file_modification": "monitor",
            "privilege_escalation": "block",
            "data_exfiltration": "block",
            "ransomware_behavior": "quarantine",
            "cryptominer_detected": "block",
            "lateral_movement": "alert",
        }
    
    def get_policy(self, name: str) -> str:
        """
        Retrieve the current action for a policy.
        
        Args:
            name: The policy name to look up.
            
        Returns:
            The action string, or 'monitor' if policy not found.
        """
        return self.policies.get(name, "monitor")
    
    def set_policy(self, name: str, action: str) -> None:
        """
        Set or update a policy action.
        
        Args:
            name: The policy name.
            action: The action to set (allow, block, quarantine, monitor, alert).
        """
        valid_actions = {"allow", "block", "quarantine", "monitor", "alert"}
        if action not in valid_actions:
            raise ValueError(f"Invalid action '{action}'. Must be one of {valid_actions}")
        self.policies[name] = action
        logger.debug(f"Policy '{name}' set to '{action}'")
    
    def list_policies(self) -> Dict[str, str]:
        """
        List all current policies.
        
        Returns:
            Dictionary of all policy names and their actions.
        """
        return dict(self.policies)
    
    def remove_policy(self, name: str) -> bool:
        """
        Remove a policy from the store.
        
        Args:
            name: The policy name to remove.
            
        Returns:
            True if policy was removed, False if it didn't exist.
        """
        if name in self.policies:
            del self.policies[name]
            return True
        return False


class PolicyOrchestrator:
    """
    Autonomous Policy Orchestration Engine.
    
    Dynamically adjusts security policies based on incoming alerts and risk scores.
    Enforces policies on processes, files, and network flows, and maintains
    a complete audit trail of all policy changes.
    
    Attributes:
        store: The PolicyStore instance for managing policies.
        thresholds: Risk score thresholds for policy escalation.
        audit_trail: List of all PolicyAlert objects for audit.
        subscribers: Callbacks to notify on policy changes.
    """
    
    # Default risk thresholds for policy escalation
    DEFAULT_THRESHOLDS: Dict[str, float] = {
        "escalate_to_alert": 0.3,
        "escalate_to_block": 0.6,
        "escalate_to_quarantine": 0.8,
    }
    
    # Action severity order for escalation logic
    ACTION_SEVERITY: Dict[str, int] = {
        "allow": 0,
        "monitor": 1,
        "alert": 2,
        "block": 3,
        "quarantine": 4,
    }
    
    def __init__(
        self,
        store: Optional[PolicyStore] = None,
        thresholds: Optional[Dict[str, float]] = None
    ):
        """
        Initialize the Policy Orchestrator.
        
        Args:
            store: PolicyStore instance. Creates default if not provided.
            thresholds: Custom risk thresholds. Uses defaults if not provided.
        """
        self.store: PolicyStore = store or PolicyStore()
        self.thresholds: Dict[str, float] = thresholds or self.DEFAULT_THRESHOLDS.copy()
        self.audit_trail: List[PolicyAlert] = []
        self.subscribers: List[Callable[[PolicyAlert], None]] = []
        logger.info("PolicyOrchestrator initialized")
    
    def subscribe(self, callback: Callable[[PolicyAlert], None]) -> None:
        """
        Subscribe to policy change notifications.
        
        Args:
            callback: Function to call when a policy is adjusted.
        """
        self.subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[PolicyAlert], None]) -> None:
        """
        Unsubscribe from policy change notifications.
        
        Args:
            callback: Previously registered callback to remove.
        """
        if callback in self.subscribers:
            self.subscribers.remove(callback)
    
    def _notify_subscribers(self, policy_alert: PolicyAlert) -> None:
        """Notify all subscribers of a policy change."""
        for callback in self.subscribers:
            try:
                callback(policy_alert)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def _determine_action_for_risk(self, risk_score: float) -> str:
        """
        Determine the appropriate action based on risk score.
        
        Args:
            risk_score: Risk score between 0.0 and 1.0.
            
        Returns:
            Recommended action string.
        """
        if risk_score >= self.thresholds.get("escalate_to_quarantine", 0.8):
            return "quarantine"
        elif risk_score >= self.thresholds.get("escalate_to_block", 0.6):
            return "block"
        elif risk_score >= self.thresholds.get("escalate_to_alert", 0.3):
            return "alert"
        return "monitor"
    
    def _should_escalate(self, current_action: str, new_action: str) -> bool:
        """
        Check if new action is more severe than current action.
        
        Args:
            current_action: Current policy action.
            new_action: Proposed new action.
            
        Returns:
            True if escalation should occur.
        """
        current_severity = self.ACTION_SEVERITY.get(current_action, 0)
        new_severity = self.ACTION_SEVERITY.get(new_action, 0)
        return new_severity > current_severity
    
    def evaluate_alert(self, alert: Alert) -> Optional[PolicyAlert]:
        """
        Evaluate an incoming alert and adjust policies if needed.
        
        Analyzes the alert's risk score against configured thresholds and
        escalates the relevant policy if the risk warrants it.
        
        Args:
            alert: The incoming security alert to evaluate.
            
        Returns:
            PolicyAlert if a policy was adjusted, None otherwise.
        """
        # Determine which policy applies to this alert
        policy_name = self._map_alert_to_policy(alert)
        current_action = self.store.get_policy(policy_name)
        recommended_action = self._determine_action_for_risk(alert.risk_score)
        
        # Only escalate, never de-escalate automatically
        if self._should_escalate(current_action, recommended_action):
            reason = (
                f"Risk score {alert.risk_score:.2f} from {alert.source} "
                f"exceeded threshold. Alert: {alert.description}"
            )
            return self.adjust_policy(policy_name, recommended_action, reason)
        
        logger.debug(
            f"Alert {alert.alert_id} evaluated, no policy change needed "
            f"(current: {current_action}, recommended: {recommended_action})"
        )
        return None
    
    def _map_alert_to_policy(self, alert: Alert) -> str:
        """
        Map an alert to the relevant policy name.
        
        Args:
            alert: The alert to map.
            
        Returns:
            Policy name that applies to this alert.
        """
        # Map common alert sources to policies
        source_policy_map = {
            "behavioral_engine": "unknown_process",
            "network_monitor": "suspicious_network",
            "signature_scan": "malware_detected",
            "access_control": "unauthorized_access",
            "file_integrity": "file_modification",
            "privilege_monitor": "privilege_escalation",
            "dlp_engine": "data_exfiltration",
            "ransomware_detector": "ransomware_behavior",
            "cryptominer_detector": "cryptominer_detected",
            "lateral_movement_detector": "lateral_movement",
        }
        
        # Check metadata for explicit policy reference
        if "policy" in alert.metadata:
            return alert.metadata["policy"]
        
        return source_policy_map.get(alert.source, "unknown_process")
    
    def adjust_policy(
        self,
        name: str,
        new_action: str,
        reason: str
    ) -> PolicyAlert:
        """
        Adjust a policy and emit a PolicyAlert.
        
        Args:
            name: Name of the policy to adjust.
            new_action: New action to set.
            reason: Explanation for the adjustment.
            
        Returns:
            PolicyAlert documenting the change.
        """
        old_action = self.store.get_policy(name)
        self.store.set_policy(name, new_action)
        
        policy_alert = PolicyAlert(
            alert_id=str(uuid.uuid4()),
            policy_name=name,
            old_action=old_action,
            new_action=new_action,
            reason=reason,
        )
        
        # Audit log the change
        self.audit_log(policy_alert)
        
        # Notify subscribers
        self._notify_subscribers(policy_alert)
        
        logger.warning(
            f"Policy adjusted: '{name}' changed from '{old_action}' to '{new_action}'. "
            f"Reason: {reason}"
        )
        
        return policy_alert
    
    def enforce_policy(self, entity: str, policy_name: str) -> None:
        """
        Enforce a policy on a specific entity.
        
        This method applies the policy action to the given entity
        (process, file, or network flow).
        
        Args:
            entity: The entity to enforce the policy on (PID, path, IP:port).
            policy_name: Name of the policy to enforce.
        """
        action = self.store.get_policy(policy_name)
        
        # Stub implementation - in production, this would integrate with
        # system APIs to actually block/quarantine/etc.
        enforcement_actions = {
            "allow": self._enforce_allow,
            "monitor": self._enforce_monitor,
            "alert": self._enforce_alert,
            "block": self._enforce_block,
            "quarantine": self._enforce_quarantine,
        }
        
        enforcer = enforcement_actions.get(action, self._enforce_monitor)
        enforcer(entity, policy_name)
        
        logger.info(f"Enforced policy '{policy_name}' ({action}) on entity: {entity}")
    
    def _enforce_allow(self, entity: str, policy_name: str) -> None:
        """Allow action - no restrictions on entity."""
        print(f"[ENFORCE:ALLOW] Entity '{entity}' is allowed per policy '{policy_name}'")
    
    def _enforce_monitor(self, entity: str, policy_name: str) -> None:
        """Monitor action - log activity but don't restrict."""
        print(f"[ENFORCE:MONITOR] Monitoring entity '{entity}' per policy '{policy_name}'")
    
    def _enforce_alert(self, entity: str, policy_name: str) -> None:
        """Alert action - generate alert and continue monitoring."""
        print(f"[ENFORCE:ALERT] Alert raised for entity '{entity}' per policy '{policy_name}'")
    
    def _enforce_block(self, entity: str, policy_name: str) -> None:
        """Block action - prevent entity from executing/connecting."""
        print(f"[ENFORCE:BLOCK] Blocking entity '{entity}' per policy '{policy_name}'")
    
    def _enforce_quarantine(self, entity: str, policy_name: str) -> None:
        """Quarantine action - isolate entity for analysis."""
        print(f"[ENFORCE:QUARANTINE] Quarantining entity '{entity}' per policy '{policy_name}'")
    
    def audit_log(self, policy_alert: PolicyAlert) -> None:
        """
        Log a policy change for audit purposes.
        
        Maintains an in-memory audit trail and logs to the configured logger.
        
        Args:
            policy_alert: The PolicyAlert to log.
        """
        self.audit_trail.append(policy_alert)
        
        log_entry = (
            f"[AUDIT] Policy Change | "
            f"ID: {policy_alert.alert_id} | "
            f"Policy: {policy_alert.policy_name} | "
            f"Action: {policy_alert.old_action} -> {policy_alert.new_action} | "
            f"Reason: {policy_alert.reason} | "
            f"Timestamp: {policy_alert.timestamp.isoformat()}"
        )
        logger.info(log_entry)
        print(log_entry)
    
    def get_audit_trail(
        self,
        policy_name: Optional[str] = None,
        since: Optional[datetime] = None
    ) -> List[PolicyAlert]:
        """
        Retrieve audit trail entries.
        
        Args:
            policy_name: Filter by policy name (optional).
            since: Filter entries after this timestamp (optional).
            
        Returns:
            List of PolicyAlert objects matching the filters.
        """
        results = self.audit_trail
        
        if policy_name:
            results = [p for p in results if p.policy_name == policy_name]
        
        if since:
            results = [p for p in results if p.timestamp >= since]
        
        return results
    
    def reset_policy(self, name: str, reason: str = "Manual reset") -> PolicyAlert:
        """
        Reset a policy to its default action (monitor).
        
        Args:
            name: Policy name to reset.
            reason: Reason for the reset.
            
        Returns:
            PolicyAlert documenting the reset.
        """
        return self.adjust_policy(name, "monitor", reason)
    
    def bulk_evaluate(self, alerts: List[Alert]) -> List[PolicyAlert]:
        """
        Evaluate multiple alerts in batch.
        
        Args:
            alerts: List of alerts to evaluate.
            
        Returns:
            List of PolicyAlerts for any policies that were adjusted.
        """
        policy_alerts = []
        for alert in alerts:
            result = self.evaluate_alert(alert)
            if result:
                policy_alerts.append(result)
        return policy_alerts


# Convenience function for pipeline integration
def create_orchestrator(
    initial_policies: Optional[Dict[str, str]] = None,
    thresholds: Optional[Dict[str, float]] = None
) -> PolicyOrchestrator:
    """
    Factory function to create a configured PolicyOrchestrator.
    
    Args:
        initial_policies: Custom initial policies.
        thresholds: Custom risk thresholds.
        
    Returns:
        Configured PolicyOrchestrator instance.
    """
    store = PolicyStore(initial_policies)
    return PolicyOrchestrator(store=store, thresholds=thresholds)


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    # Create orchestrator with default settings
    orchestrator = create_orchestrator()
    
    # Subscribe to policy changes
    def on_policy_change(alert: PolicyAlert):
        print(f">>> Subscriber notified: {alert.policy_name} is now {alert.new_action}")
    
    orchestrator.subscribe(on_policy_change)
    
    # Simulate incoming alerts
    test_alerts = [
        Alert(
            alert_id="alert-001",
            source="behavioral_engine",
            entity="/usr/bin/suspicious_process",
            risk_score=0.45,
            description="Unusual process behavior detected"
        ),
        Alert(
            alert_id="alert-002",
            source="network_monitor",
            entity="192.168.1.100:4444",
            risk_score=0.75,
            description="Suspicious outbound connection to known C2 server"
        ),
        Alert(
            alert_id="alert-003",
            source="ransomware_detector",
            entity="/home/user/documents",
            risk_score=0.92,
            description="Rapid file encryption detected"
        ),
    ]
    
    print("=== Evaluating Alerts ===\n")
    for alert in test_alerts:
        print(f"Processing alert: {alert.alert_id} (risk: {alert.risk_score})")
        result = orchestrator.evaluate_alert(alert)
        if result:
            print(f"  -> Policy changed!\n")
        else:
            print(f"  -> No policy change needed\n")
    
    print("\n=== Enforcing Policies ===\n")
    orchestrator.enforce_policy("/usr/bin/suspicious_process", "unknown_process")
    orchestrator.enforce_policy("192.168.1.100:4444", "suspicious_network")
    
    print("\n=== Current Policies ===")
    for name, action in orchestrator.store.list_policies().items():
        print(f"  {name}: {action}")
    
    print("\n=== Audit Trail ===")
    for entry in orchestrator.get_audit_trail():
        print(f"  [{entry.timestamp.strftime('%H:%M:%S')}] {entry.policy_name}: "
              f"{entry.old_action} -> {entry.new_action}")
