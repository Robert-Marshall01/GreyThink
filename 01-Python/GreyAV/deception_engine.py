"""
Cognitive Deception Engine for GreyAV EDR System.

This module implements a dynamic deception layer that deploys decoys
(honeypots, honeytokens, canary files) to detect and analyze attacker behavior.
"""

import uuid
import random
import logging
import threading
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, Dict, Any, List, Callable, Set
from enum import Enum

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


class DecoyType(Enum):
    """Types of decoys that can be deployed."""
    FILE = "file"
    PROCESS = "process"
    REGISTRY = "registry"
    NETWORK = "network"
    CREDENTIAL = "credential"
    SERVICE = "service"


class AttackerTactic(Enum):
    """Common attacker tactics for strategy adaptation."""
    RECONNAISSANCE = "reconnaissance"
    CREDENTIAL_ACCESS = "credential_access"
    LATERAL_MOVEMENT = "lateral_movement"
    EXFILTRATION = "exfiltration"
    PERSISTENCE = "persistence"
    PRIVILEGE_ESCALATION = "privilege_escalation"


@dataclass
class DeceptionAlert:
    """
    Emitted when an attacker interacts with a decoy.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        decoy_type: Type of decoy ("file", "process", "registry", "network").
        decoy_name: Name/identifier of the triggered decoy.
        attacker_action: Action performed by the attacker.
        description: Human-readable description.
        timestamp: When the interaction occurred.
        severity: Alert severity level (1-10).
        source_info: Information about the attacker source.
        metadata: Additional context.
    """
    alert_id: str
    decoy_type: str
    decoy_name: str
    attacker_action: str
    description: str
    timestamp: datetime = field(default_factory=datetime.now)
    severity: int = 5
    source_info: Dict[str, Any] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class Decoy:
    """
    Represents a deployed decoy.
    
    Attributes:
        name: Unique name of the decoy.
        decoy_type: Type of decoy.
        path: Virtual path or location.
        created_at: When the decoy was created.
        interactions: Count of attacker interactions.
        is_active: Whether the decoy is currently active.
        config: Decoy-specific configuration.
    """
    name: str
    decoy_type: str
    path: str
    created_at: datetime = field(default_factory=datetime.now)
    interactions: int = 0
    is_active: bool = True
    config: Dict[str, Any] = field(default_factory=dict)


class DecoyManager:
    """
    Manages the lifecycle of decoys.
    
    Responsible for creating, removing, and listing decoys
    across different types (files, processes, registry, network).
    """
    
    # Templates for different decoy types
    DECOY_TEMPLATES: Dict[str, List[Dict[str, Any]]] = {
        "file": [
            {"name": "passwords.txt", "path": "/home/{user}/Documents/passwords.txt"},
            {"name": "backup.sql", "path": "/var/backups/database_backup.sql"},
            {"name": "id_rsa", "path": "/home/{user}/.ssh/id_rsa"},
            {"name": "aws_credentials", "path": "/home/{user}/.aws/credentials"},
            {"name": "wallet.dat", "path": "/home/{user}/.bitcoin/wallet.dat"},
            {"name": "secrets.env", "path": "/opt/app/.env.secrets"},
            {"name": "admin_backup.zip", "path": "C:\\Users\\Admin\\Desktop\\admin_backup.zip"},
            {"name": "payroll.xlsx", "path": "C:\\Shares\\HR\\payroll_2024.xlsx"},
        ],
        "process": [
            {"name": "sshd_honeypot", "command": "/usr/sbin/sshd -p 2222"},
            {"name": "mysql_decoy", "command": "mysqld --port=3307"},
            {"name": "rdp_honeypot", "command": "xrdp --port=3390"},
            {"name": "smb_honeypot", "command": "samba --port=4450"},
            {"name": "ftp_decoy", "command": "vsftpd --listen-port=2121"},
        ],
        "registry": [
            {"name": "RunOnce_Decoy", "path": "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\RunOnce\\BackupService"},
            {"name": "Services_Decoy", "path": "HKLM\\SYSTEM\\CurrentControlSet\\Services\\DecoyService"},
            {"name": "Uninstall_Decoy", "path": "HKLM\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\FakeApp"},
            {"name": "Credentials_Decoy", "path": "HKCU\\Software\\Credentials\\AdminPassword"},
        ],
        "network": [
            {"name": "http_honeypot", "endpoint": "0.0.0.0:8080", "protocol": "http"},
            {"name": "https_admin", "endpoint": "0.0.0.0:8443", "protocol": "https"},
            {"name": "ssh_trap", "endpoint": "0.0.0.0:22222", "protocol": "ssh"},
            {"name": "telnet_decoy", "endpoint": "0.0.0.0:2323", "protocol": "telnet"},
            {"name": "dns_honeypot", "endpoint": "0.0.0.0:5353", "protocol": "dns"},
        ],
        "credential": [
            {"name": "admin_token", "type": "api_key", "value": "sk-fake-api-key-decoy"},
            {"name": "db_password", "type": "password", "value": "Super$ecret123!"},
            {"name": "aws_key", "type": "access_key", "value": "AKIAIOSFODNN7EXAMPLE"},
        ],
        "service": [
            {"name": "backup_service", "service_name": "BackupAgentSvc"},
            {"name": "monitoring_agent", "service_name": "MonitoringSvc"},
            {"name": "update_service", "service_name": "AutoUpdateSvc"},
        ],
    }
    
    def __init__(self):
        """Initialize the DecoyManager."""
        self._decoys: Dict[str, Decoy] = {}
        self._lock = threading.Lock()
        logger.info("DecoyManager initialized")
    
    def create_decoy(self, decoy_type: str, name: str, **kwargs) -> Decoy:
        """
        Create a new decoy.
        
        Args:
            decoy_type: Type of decoy ("file", "process", "registry", "network").
            name: Unique name for the decoy.
            **kwargs: Additional configuration for the decoy.
            
        Returns:
            The created Decoy object.
            
        Raises:
            ValueError: If decoy with name already exists.
        """
        with self._lock:
            if name in self._decoys:
                raise ValueError(f"Decoy '{name}' already exists")
            
            # Find template or use custom path
            path = kwargs.get("path", f"/decoys/{decoy_type}/{name}")
            
            # Look for matching template
            templates = self.DECOY_TEMPLATES.get(decoy_type, [])
            for template in templates:
                if template.get("name") == name:
                    path = template.get("path", template.get("endpoint", path))
                    kwargs.update({k: v for k, v in template.items() if k not in kwargs})
                    break
            
            decoy = Decoy(
                name=name,
                decoy_type=decoy_type,
                path=path,
                config=kwargs,
            )
            
            self._decoys[name] = decoy
            logger.info(f"Created {decoy_type} decoy: {name} at {path}")
            
            return decoy
    
    def remove_decoy(self, name: str) -> bool:
        """
        Remove a decoy.
        
        Args:
            name: Name of the decoy to remove.
            
        Returns:
            True if removed, False if not found.
        """
        with self._lock:
            if name in self._decoys:
                decoy = self._decoys.pop(name)
                logger.info(f"Removed decoy: {name} (type: {decoy.decoy_type})")
                return True
            return False
    
    def get_decoy(self, name: str) -> Optional[Decoy]:
        """
        Get a decoy by name.
        
        Args:
            name: Name of the decoy.
            
        Returns:
            Decoy object or None if not found.
        """
        return self._decoys.get(name)
    
    def list_decoys(self, decoy_type: Optional[str] = None) -> List[str]:
        """
        List all decoy names.
        
        Args:
            decoy_type: Optional filter by type.
            
        Returns:
            List of decoy names.
        """
        with self._lock:
            if decoy_type:
                return [
                    name for name, decoy in self._decoys.items()
                    if decoy.decoy_type == decoy_type
                ]
            return list(self._decoys.keys())
    
    def list_decoys_detailed(self, decoy_type: Optional[str] = None) -> List[Decoy]:
        """
        List all decoys with full details.
        
        Args:
            decoy_type: Optional filter by type.
            
        Returns:
            List of Decoy objects.
        """
        with self._lock:
            if decoy_type:
                return [
                    decoy for decoy in self._decoys.values()
                    if decoy.decoy_type == decoy_type
                ]
            return list(self._decoys.values())
    
    def activate_decoy(self, name: str) -> bool:
        """Activate a decoy."""
        if name in self._decoys:
            self._decoys[name].is_active = True
            return True
        return False
    
    def deactivate_decoy(self, name: str) -> bool:
        """Deactivate a decoy."""
        if name in self._decoys:
            self._decoys[name].is_active = False
            return True
        return False
    
    def record_interaction(self, name: str) -> None:
        """Record an interaction with a decoy."""
        if name in self._decoys:
            self._decoys[name].interactions += 1
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get decoy statistics."""
        with self._lock:
            stats = {
                "total_decoys": len(self._decoys),
                "active_decoys": sum(1 for d in self._decoys.values() if d.is_active),
                "by_type": {},
                "total_interactions": sum(d.interactions for d in self._decoys.values()),
            }
            
            for decoy in self._decoys.values():
                if decoy.decoy_type not in stats["by_type"]:
                    stats["by_type"][decoy.decoy_type] = 0
                stats["by_type"][decoy.decoy_type] += 1
            
            return stats
    
    def clear(self) -> int:
        """Remove all decoys. Returns count of removed decoys."""
        with self._lock:
            count = len(self._decoys)
            self._decoys.clear()
            logger.info(f"Cleared {count} decoys")
            return count


class DeceptionEngine:
    """
    Cognitive Deception Engine for active threat detection.
    
    Deploys and manages decoys, monitors attacker interactions,
    and adapts deception strategies based on observed behavior.
    
    Attributes:
        decoys: Dictionary mapping decoy names to metadata.
        manager: DecoyManager instance.
        subscribers: List of alert callbacks.
    """
    
    # Attacker action severity mapping
    ACTION_SEVERITY: Dict[str, int] = {
        "read_file": 4,
        "write_file": 6,
        "delete_file": 7,
        "execute_file": 8,
        "modify_registry": 7,
        "read_registry": 4,
        "connect_endpoint": 5,
        "scan_port": 3,
        "enumerate_process": 4,
        "inject_process": 9,
        "access_credential": 9,
        "exfiltrate_data": 10,
        "lateral_movement": 8,
        "privilege_escalation": 9,
    }
    
    # Strategy adaptation rules
    ADAPTATION_RULES: Dict[str, List[str]] = {
        "reconnaissance": ["file", "network", "service"],
        "credential_access": ["credential", "file", "registry"],
        "lateral_movement": ["network", "process", "service"],
        "exfiltration": ["file", "network"],
        "persistence": ["registry", "service", "file"],
        "privilege_escalation": ["registry", "process", "credential"],
    }
    
    def __init__(self, auto_deploy: bool = True):
        """
        Initialize the Deception Engine.
        
        Args:
            auto_deploy: Whether to automatically deploy initial decoys.
        """
        self.decoys: Dict[str, Dict[str, Any]] = {}
        self.manager = DecoyManager()
        self.subscribers: List[Callable[[DeceptionAlert], None]] = []
        self._alert_history: List[DeceptionAlert] = []
        self._attacker_profiles: Dict[str, Dict[str, Any]] = {}
        self._current_strategy: str = "balanced"
        self._running: bool = False
        self._monitor_thread: Optional[threading.Thread] = None
        
        if auto_deploy:
            self._deploy_initial_decoys()
        
        logger.info("DeceptionEngine initialized")
    
    def _deploy_initial_decoys(self) -> None:
        """Deploy a balanced set of initial decoys."""
        initial_decoys = [
            ("file", "passwords.txt"),
            ("file", "backup.sql"),
            ("network", "ssh_trap"),
            ("network", "http_honeypot"),
            ("registry", "RunOnce_Decoy"),
            ("credential", "admin_token"),
        ]
        
        for decoy_type, name in initial_decoys:
            try:
                self.deploy_decoy(decoy_type, name)
            except Exception as e:
                logger.warning(f"Failed to deploy initial decoy {name}: {e}")
    
    def subscribe(self, callback: Callable[[DeceptionAlert], None]) -> None:
        """
        Subscribe to deception alert notifications.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self.subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[DeceptionAlert], None]) -> None:
        """
        Unsubscribe from deception alert notifications.
        
        Args:
            callback: Previously registered callback.
        """
        if callback in self.subscribers:
            self.subscribers.remove(callback)
    
    def _notify_subscribers(self, alert: DeceptionAlert) -> None:
        """Notify all subscribers of a new alert."""
        for callback in self.subscribers:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def deploy_decoy(self, decoy_type: str, name: str, **kwargs) -> Decoy:
        """
        Deploy a new decoy.
        
        Args:
            decoy_type: Type of decoy ("file", "process", "registry", "network").
            name: Unique name for the decoy.
            **kwargs: Additional configuration.
            
        Returns:
            The deployed Decoy object.
        """
        decoy = self.manager.create_decoy(decoy_type, name, **kwargs)
        
        # Store in local dict for quick access
        self.decoys[name] = {
            "type": decoy_type,
            "path": decoy.path,
            "created_at": decoy.created_at.isoformat(),
            "config": decoy.config,
        }
        
        logger.info(f"Deployed {decoy_type} decoy: {name}")
        return decoy
    
    def remove_decoy(self, name: str) -> bool:
        """
        Remove a deployed decoy.
        
        Args:
            name: Name of the decoy to remove.
            
        Returns:
            True if removed, False otherwise.
        """
        if self.manager.remove_decoy(name):
            self.decoys.pop(name, None)
            return True
        return False
    
    def monitor_interaction(
        self,
        attacker_action: str,
        decoy_name: str,
        source_info: Optional[Dict[str, Any]] = None
    ) -> DeceptionAlert:
        """
        Monitor and log an attacker interaction with a decoy.
        
        Args:
            attacker_action: Action performed (e.g., "read_file", "connect_endpoint").
            decoy_name: Name of the decoy that was accessed.
            source_info: Information about the attacker source.
            
        Returns:
            DeceptionAlert describing the interaction.
        """
        decoy = self.manager.get_decoy(decoy_name)
        
        if decoy is None:
            # Unknown decoy - could be a new detection opportunity
            decoy_type = "unknown"
            path = f"/unknown/{decoy_name}"
        else:
            decoy_type = decoy.decoy_type
            path = decoy.path
            self.manager.record_interaction(decoy_name)
        
        # Determine severity
        severity = self.ACTION_SEVERITY.get(attacker_action, 5)
        
        # Build description
        description = (
            f"Attacker interaction detected: {attacker_action} on {decoy_type} decoy "
            f"'{decoy_name}' at {path}"
        )
        
        # Create alert
        alert = DeceptionAlert(
            alert_id=str(uuid.uuid4()),
            decoy_type=decoy_type,
            decoy_name=decoy_name,
            attacker_action=attacker_action,
            description=description,
            severity=severity,
            source_info=source_info or {},
            metadata={
                "path": path,
                "decoy_config": decoy.config if decoy else {},
                "interaction_count": decoy.interactions if decoy else 0,
            },
        )
        
        # Store alert
        self._alert_history.append(alert)
        
        # Update attacker profile
        self._update_attacker_profile(alert)
        
        # Notify subscribers
        self._notify_subscribers(alert)
        
        logger.warning(
            f"DECEPTION ALERT: {attacker_action} on {decoy_name} "
            f"(severity: {severity})"
        )
        
        return alert
    
    def _update_attacker_profile(self, alert: DeceptionAlert) -> None:
        """Update attacker profile based on interaction."""
        source_ip = alert.source_info.get("ip", "unknown")
        
        if source_ip not in self._attacker_profiles:
            self._attacker_profiles[source_ip] = {
                "first_seen": datetime.now(),
                "interactions": [],
                "tactics": set(),
                "severity_score": 0,
            }
        
        profile = self._attacker_profiles[source_ip]
        profile["interactions"].append({
            "action": alert.attacker_action,
            "decoy": alert.decoy_name,
            "timestamp": alert.timestamp.isoformat(),
        })
        profile["severity_score"] += alert.severity
        profile["last_seen"] = datetime.now()
        
        # Infer tactic
        tactic = self._infer_tactic(alert.attacker_action)
        if tactic:
            profile["tactics"].add(tactic)
    
    def _infer_tactic(self, action: str) -> Optional[str]:
        """Infer attacker tactic from action."""
        tactic_mapping = {
            "read_file": "reconnaissance",
            "write_file": "persistence",
            "delete_file": "defense_evasion",
            "execute_file": "execution",
            "modify_registry": "persistence",
            "read_registry": "reconnaissance",
            "connect_endpoint": "command_and_control",
            "scan_port": "reconnaissance",
            "enumerate_process": "discovery",
            "inject_process": "defense_evasion",
            "access_credential": "credential_access",
            "exfiltrate_data": "exfiltration",
            "lateral_movement": "lateral_movement",
            "privilege_escalation": "privilege_escalation",
        }
        return tactic_mapping.get(action)
    
    def adapt_strategy(self, attacker_behavior: str) -> List[str]:
        """
        Adapt deception strategy based on observed attacker behavior.
        
        Adjusts decoy deployment to counter identified tactics.
        
        Args:
            attacker_behavior: Observed tactic/behavior pattern.
            
        Returns:
            List of actions taken.
        """
        actions = []
        
        # Get recommended decoy types for this behavior
        recommended_types = self.ADAPTATION_RULES.get(
            attacker_behavior,
            ["file", "network"]
        )
        
        logger.info(f"Adapting strategy for behavior: {attacker_behavior}")
        
        for decoy_type in recommended_types:
            templates = self.manager.DECOY_TEMPLATES.get(decoy_type, [])
            
            for template in templates:
                name = template.get("name", "")
                
                # Skip if already deployed
                if name in self.decoys:
                    continue
                
                # Deploy new decoy
                try:
                    self.deploy_decoy(decoy_type, name)
                    actions.append(f"Deployed {decoy_type} decoy: {name}")
                    
                    # Limit deployments per adaptation
                    if len(actions) >= 3:
                        break
                except Exception as e:
                    logger.warning(f"Failed to deploy adaptive decoy: {e}")
            
            if len(actions) >= 3:
                break
        
        # Update current strategy
        self._current_strategy = attacker_behavior
        
        logger.info(f"Strategy adapted: {len(actions)} new decoys deployed")
        return actions
    
    def auto_adapt(self) -> List[str]:
        """
        Automatically adapt based on recent attacker profiles.
        
        Returns:
            List of actions taken.
        """
        actions = []
        
        # Analyze recent attacker tactics
        all_tactics: Set[str] = set()
        for profile in self._attacker_profiles.values():
            all_tactics.update(profile.get("tactics", set()))
        
        # Adapt for most common tactics
        for tactic in all_tactics:
            if tactic in self.ADAPTATION_RULES:
                new_actions = self.adapt_strategy(tactic)
                actions.extend(new_actions)
        
        return actions
    
    def run(self, interval: float = 60.0) -> None:
        """
        Continuously deploy and monitor decoys.
        
        Runs in a background thread, periodically checking
        for adaptation opportunities.
        
        Args:
            interval: Check interval in seconds.
        """
        if self._running:
            logger.warning("DeceptionEngine is already running")
            return
        
        self._running = True
        
        def monitor_loop():
            logger.info("DeceptionEngine monitor started")
            
            while self._running:
                try:
                    # Check for needed adaptations
                    if self._attacker_profiles:
                        self.auto_adapt()
                    
                    # Rotate some decoys periodically
                    self._rotate_decoys()
                    
                except Exception as e:
                    logger.error(f"Monitor loop error: {e}")
                
                # Sleep in small increments to allow quick shutdown
                for _ in range(int(interval)):
                    if not self._running:
                        break
                    time.sleep(1)
            
            logger.info("DeceptionEngine monitor stopped")
        
        self._monitor_thread = threading.Thread(target=monitor_loop, daemon=True)
        self._monitor_thread.start()
    
    def stop(self) -> None:
        """Stop the monitoring loop."""
        self._running = False
        if self._monitor_thread:
            self._monitor_thread.join(timeout=5.0)
            self._monitor_thread = None
        logger.info("DeceptionEngine stopped")
    
    def _rotate_decoys(self) -> None:
        """Rotate decoys to maintain unpredictability."""
        # Get least-interacted decoys
        decoys = self.manager.list_decoys_detailed()
        
        if len(decoys) < 5:
            return
        
        # Find cold decoys (no interactions)
        cold_decoys = [d for d in decoys if d.interactions == 0]
        
        # Randomly rotate one cold decoy
        if cold_decoys and random.random() < 0.1:
            old_decoy = random.choice(cold_decoys)
            
            # Remove old
            self.remove_decoy(old_decoy.name)
            
            # Deploy new of same type
            templates = self.manager.DECOY_TEMPLATES.get(old_decoy.decoy_type, [])
            if templates:
                new_template = random.choice(templates)
                if new_template["name"] not in self.decoys:
                    try:
                        self.deploy_decoy(old_decoy.decoy_type, new_template["name"])
                        logger.info(f"Rotated decoy: {old_decoy.name} -> {new_template['name']}")
                    except Exception:
                        pass
    
    def simulate_attack(
        self,
        actions: Optional[List[str]] = None,
        source_ip: str = "192.168.1.100"
    ) -> List[DeceptionAlert]:
        """
        Simulate attacker interactions for testing.
        
        Args:
            actions: List of actions to simulate.
            source_ip: Simulated attacker IP.
            
        Returns:
            List of generated alerts.
        """
        if actions is None:
            actions = ["read_file", "scan_port", "access_credential"]
        
        alerts = []
        decoy_names = self.manager.list_decoys()
        
        if not decoy_names:
            logger.warning("No decoys deployed for simulation")
            return alerts
        
        for action in actions:
            decoy_name = random.choice(decoy_names)
            alert = self.monitor_interaction(
                attacker_action=action,
                decoy_name=decoy_name,
                source_info={"ip": source_ip, "simulated": True},
            )
            alerts.append(alert)
        
        return alerts
    
    def get_alert_history(
        self,
        decoy_type: Optional[str] = None,
        min_severity: int = 0
    ) -> List[DeceptionAlert]:
        """
        Get alert history with optional filters.
        
        Args:
            decoy_type: Filter by decoy type.
            min_severity: Minimum severity filter.
            
        Returns:
            List of matching alerts.
        """
        results = self._alert_history
        
        if decoy_type:
            results = [a for a in results if a.decoy_type == decoy_type]
        
        if min_severity > 0:
            results = [a for a in results if a.severity >= min_severity]
        
        return results
    
    def get_attacker_profiles(self) -> Dict[str, Dict[str, Any]]:
        """Get all attacker profiles."""
        # Convert sets to lists for serialization
        profiles = {}
        for ip, profile in self._attacker_profiles.items():
            profiles[ip] = {
                **profile,
                "tactics": list(profile.get("tactics", set())),
            }
        return profiles
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get engine statistics.
        
        Returns:
            Dictionary with engine statistics.
        """
        decoy_stats = self.manager.get_statistics()
        
        return {
            **decoy_stats,
            "current_strategy": self._current_strategy,
            "is_running": self._running,
            "total_alerts": len(self._alert_history),
            "unique_attackers": len(self._attacker_profiles),
            "alerts_by_severity": self._count_by_severity(),
            "alerts_by_type": self._count_by_type(),
        }
    
    def _count_by_severity(self) -> Dict[str, int]:
        """Count alerts by severity level."""
        counts = {"low": 0, "medium": 0, "high": 0, "critical": 0}
        for alert in self._alert_history:
            if alert.severity <= 3:
                counts["low"] += 1
            elif alert.severity <= 5:
                counts["medium"] += 1
            elif alert.severity <= 7:
                counts["high"] += 1
            else:
                counts["critical"] += 1
        return counts
    
    def _count_by_type(self) -> Dict[str, int]:
        """Count alerts by decoy type."""
        counts: Dict[str, int] = {}
        for alert in self._alert_history:
            counts[alert.decoy_type] = counts.get(alert.decoy_type, 0) + 1
        return counts
    
    def clear(self) -> None:
        """Clear all history and reset."""
        self._alert_history.clear()
        self._attacker_profiles.clear()
        self.manager.clear()
        self.decoys.clear()
        logger.info("DeceptionEngine cleared")


# Factory function for pipeline integration
def create_deception_engine(auto_deploy: bool = True) -> DeceptionEngine:
    """
    Factory function to create a DeceptionEngine instance.
    
    Args:
        auto_deploy: Whether to deploy initial decoys.
        
    Returns:
        New DeceptionEngine instance.
    """
    return DeceptionEngine(auto_deploy=auto_deploy)


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    print("=== Cognitive Deception Engine Demo ===\n")
    
    engine = create_deception_engine()
    
    # Subscribe to alerts
    def on_alert(alert: DeceptionAlert):
        print(f"\n>>> DECEPTION ALERT [{alert.decoy_type.upper()}]")
        print(f"    Decoy: {alert.decoy_name}")
        print(f"    Action: {alert.attacker_action}")
        print(f"    Severity: {alert.severity}/10")
        print(f"    Description: {alert.description}\n")
    
    engine.subscribe(on_alert)
    
    # Show deployed decoys
    print("=== Deployed Decoys ===")
    for name in engine.manager.list_decoys():
        decoy = engine.manager.get_decoy(name)
        if decoy:
            print(f"  [{decoy.decoy_type}] {name}: {decoy.path}")
    
    # Deploy additional decoy
    print("\n=== Deploying Additional Decoy ===")
    engine.deploy_decoy("network", "telnet_decoy")
    
    # Simulate attacker interactions
    print("\n=== Simulating Attacker Activity ===")
    
    # Attacker 1: Reconnaissance
    engine.monitor_interaction(
        "read_file", "passwords.txt",
        source_info={"ip": "10.0.0.50", "process": "explorer.exe"}
    )
    
    # Attacker 1: Credential access
    engine.monitor_interaction(
        "access_credential", "admin_token",
        source_info={"ip": "10.0.0.50", "process": "mimikatz.exe"}
    )
    
    # Attacker 2: Network scanning
    engine.monitor_interaction(
        "scan_port", "ssh_trap",
        source_info={"ip": "192.168.1.200", "tool": "nmap"}
    )
    
    # Attacker 2: Connection attempt
    engine.monitor_interaction(
        "connect_endpoint", "http_honeypot",
        source_info={"ip": "192.168.1.200", "user_agent": "curl/7.64.1"}
    )
    
    # Adapt strategy
    print("\n=== Adapting Strategy ===")
    actions = engine.adapt_strategy("credential_access")
    for action in actions:
        print(f"  {action}")
    
    # Statistics
    print("\n=== Engine Statistics ===")
    stats = engine.get_statistics()
    print(f"Total Decoys: {stats['total_decoys']}")
    print(f"Active Decoys: {stats['active_decoys']}")
    print(f"Total Alerts: {stats['total_alerts']}")
    print(f"Unique Attackers: {stats['unique_attackers']}")
    print(f"Current Strategy: {stats['current_strategy']}")
    
    # Attacker profiles
    print("\n=== Attacker Profiles ===")
    profiles = engine.get_attacker_profiles()
    for ip, profile in profiles.items():
        print(f"  {ip}:")
        print(f"    Interactions: {len(profile['interactions'])}")
        print(f"    Tactics: {profile['tactics']}")
        print(f"    Severity Score: {profile['severity_score']}")
    
    print("\n=== Demo Complete ===")
