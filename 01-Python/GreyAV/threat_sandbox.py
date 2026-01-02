"""
Adaptive Threat Simulation Sandbox for GreyAV EDR System.

This module provides a controlled environment for safely executing
suspicious binaries, capturing their behaviors, and detecting malicious
patterns across varied environment conditions.
"""

import uuid
import random
import hashlib
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, Dict, Any, List, Set, Callable
from pathlib import Path

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


@dataclass
class SandboxAlert:
    """
    Emitted when malicious patterns are confirmed in sandbox execution.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        binary_name: Name of the analyzed binary.
        behaviors: List of captured behaviors during execution.
        risk_score: Calculated risk score (0.0 - 1.0).
        description: Human-readable description of the analysis.
        timestamp: When the alert was generated.
        binary_hash: SHA256 hash of the binary.
        profiles_tested: Environment profiles used during analysis.
        execution_time: Simulated execution time in seconds.
        metadata: Additional context about the analysis.
    """
    alert_id: str
    binary_name: str
    behaviors: List[str]
    risk_score: float
    description: str
    timestamp: datetime = field(default_factory=datetime.now)
    binary_hash: str = ""
    profiles_tested: List[str] = field(default_factory=list)
    execution_time: float = 0.0
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ExecutionResult:
    """
    Result of a single sandbox execution.
    
    Attributes:
        profile: Environment profile used.
        behaviors: Captured behaviors.
        syscalls: System calls made.
        files_created: Files created during execution.
        files_modified: Files modified during execution.
        registry_changes: Registry modifications.
        network_activity: Network connections/requests.
        execution_time: Time taken for execution.
        exit_code: Simulated exit code.
    """
    profile: str
    behaviors: List[str]
    syscalls: List[str] = field(default_factory=list)
    files_created: List[str] = field(default_factory=list)
    files_modified: List[str] = field(default_factory=list)
    registry_changes: List[str] = field(default_factory=list)
    network_activity: List[str] = field(default_factory=list)
    execution_time: float = 0.0
    exit_code: int = 0


class ThreatSandbox:
    """
    Adaptive Threat Simulation Sandbox for malware analysis.
    
    Provides a controlled environment for executing suspicious binaries,
    capturing system behaviors, and detecting malicious patterns across
    multiple environment configurations.
    
    Attributes:
        environment_profiles: Dictionary of environment profile configurations.
        logs: List of captured behavior logs.
        current_binary: Currently loaded binary path.
        subscribers: Callbacks notified when alerts are generated.
    """
    
    # Default environment profiles
    DEFAULT_PROFILES: Dict[str, Dict[str, Any]] = {
        "Windows10_Admin": {
            "os": "Windows",
            "os_version": "10.0.19041",
            "privilege": "Administrator",
            "network": "connected",
            "security_software": ["Defender"],
            "common_apps": ["Office", "Chrome", "Outlook"],
        },
        "Windows10_User": {
            "os": "Windows",
            "os_version": "10.0.19041",
            "privilege": "User",
            "network": "connected",
            "security_software": ["Defender"],
            "common_apps": ["Chrome", "Notepad"],
        },
        "Windows11_Admin": {
            "os": "Windows",
            "os_version": "10.0.22000",
            "privilege": "Administrator",
            "network": "connected",
            "security_software": ["Defender", "SmartScreen"],
            "common_apps": ["Office", "Edge", "Teams"],
        },
        "Windows11_User": {
            "os": "Windows",
            "os_version": "10.0.22000",
            "privilege": "User",
            "network": "connected",
            "security_software": ["Defender", "SmartScreen"],
            "common_apps": ["Edge", "Notepad"],
        },
        "Windows_Isolated": {
            "os": "Windows",
            "os_version": "10.0.19041",
            "privilege": "User",
            "network": "disconnected",
            "security_software": [],
            "common_apps": [],
        },
        "Linux_Root": {
            "os": "Linux",
            "os_version": "5.15.0",
            "privilege": "root",
            "network": "connected",
            "security_software": ["AppArmor"],
            "common_apps": ["bash", "python3"],
        },
        "Linux_User": {
            "os": "Linux",
            "os_version": "5.15.0",
            "privilege": "user",
            "network": "connected",
            "security_software": ["AppArmor"],
            "common_apps": ["bash", "python3"],
        },
    }
    
    # Behavior categories and their risk weights
    BEHAVIOR_RISK_WEIGHTS: Dict[str, float] = {
        # High risk behaviors
        "process_injection": 0.9,
        "credential_access": 0.85,
        "privilege_escalation": 0.85,
        "rootkit_installation": 0.95,
        "bootkit_modification": 0.95,
        "ransomware_encryption": 0.95,
        "data_exfiltration": 0.8,
        "c2_communication": 0.75,
        "defense_evasion": 0.7,
        "persistence_mechanism": 0.65,
        
        # Medium risk behaviors
        "suspicious_network": 0.5,
        "file_system_modification": 0.4,
        "registry_modification": 0.45,
        "process_creation": 0.3,
        "script_execution": 0.35,
        "scheduled_task": 0.5,
        "service_modification": 0.55,
        
        # Low risk behaviors
        "file_read": 0.1,
        "system_discovery": 0.15,
        "environment_query": 0.1,
        "normal_network": 0.05,
        "user_interaction": 0.05,
    }
    
    # Simulated malicious behavior patterns
    MALICIOUS_PATTERNS: List[List[str]] = [
        # Ransomware pattern
        ["process_creation", "file_system_modification", "ransomware_encryption", "persistence_mechanism"],
        # Credential stealer pattern
        ["process_creation", "credential_access", "c2_communication", "data_exfiltration"],
        # RAT pattern
        ["process_creation", "persistence_mechanism", "c2_communication", "system_discovery"],
        # Dropper pattern
        ["file_system_modification", "process_creation", "defense_evasion", "persistence_mechanism"],
        # Rootkit pattern
        ["privilege_escalation", "rootkit_installation", "defense_evasion", "persistence_mechanism"],
    ]
    
    # Benign behavior patterns
    BENIGN_PATTERNS: List[List[str]] = [
        ["file_read", "environment_query", "normal_network"],
        ["user_interaction", "file_read", "file_system_modification"],
        ["process_creation", "normal_network", "file_read"],
    ]
    
    def __init__(
        self,
        environment_profiles: Optional[Dict[str, Dict[str, Any]]] = None
    ):
        """
        Initialize the Threat Sandbox.
        
        Args:
            environment_profiles: Custom environment profiles.
        """
        self.environment_profiles = environment_profiles or self.DEFAULT_PROFILES.copy()
        self.logs: List[Dict[str, Any]] = []
        self.current_binary: Optional[str] = None
        self.current_binary_hash: str = ""
        self.subscribers: List[Callable[[SandboxAlert], None]] = []
        self._alert_history: List[SandboxAlert] = []
        self._execution_results: List[ExecutionResult] = []
        logger.info("ThreatSandbox initialized with %d profiles", len(self.environment_profiles))
    
    def subscribe(self, callback: Callable[[SandboxAlert], None]) -> None:
        """
        Subscribe to sandbox alert notifications.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self.subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[SandboxAlert], None]) -> None:
        """
        Unsubscribe from sandbox alert notifications.
        
        Args:
            callback: Previously registered callback to remove.
        """
        if callback in self.subscribers:
            self.subscribers.remove(callback)
    
    def _notify_subscribers(self, alert: SandboxAlert) -> None:
        """Notify all subscribers of a new alert."""
        for callback in self.subscribers:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def _compute_binary_hash(self, path: str) -> str:
        """
        Compute SHA256 hash of binary.
        
        For simulation, generates a deterministic hash from the path.
        """
        return hashlib.sha256(path.encode()).hexdigest()
    
    def load_binary(self, path: str) -> None:
        """
        Load a binary for sandbox analysis.
        
        Args:
            path: Path to the binary file.
        """
        self.current_binary = path
        self.current_binary_hash = self._compute_binary_hash(path)
        
        # Log the load operation
        self.logs.append({
            "action": "load_binary",
            "path": path,
            "hash": self.current_binary_hash,
            "timestamp": datetime.now().isoformat(),
        })
        
        logger.info(f"Loaded binary: {path} (hash: {self.current_binary_hash[:16]}...)")
    
    def _simulate_syscalls(self, profile: Dict[str, Any]) -> List[str]:
        """Simulate system calls based on profile."""
        base_syscalls = ["NtCreateFile", "NtReadFile", "NtQuerySystemInformation"]
        
        if profile.get("privilege") in ["Administrator", "root"]:
            base_syscalls.extend(["NtLoadDriver", "NtSetSystemInformation"])
        
        if profile.get("network") == "connected":
            base_syscalls.extend(["NtCreateSection", "NtConnectPort"])
        
        return base_syscalls
    
    def _simulate_file_operations(self, profile: Dict[str, Any]) -> tuple:
        """Simulate file operations."""
        created = []
        modified = []
        
        # Simulate based on OS
        if profile.get("os") == "Windows":
            temp_paths = ["C:\\Windows\\Temp\\", "C:\\Users\\Public\\"]
            created = [f"{random.choice(temp_paths)}tmp_{uuid.uuid4().hex[:8]}.dat"]
            if random.random() > 0.5:
                modified = ["C:\\Windows\\System32\\config\\SAM"]
        else:
            temp_paths = ["/tmp/", "/var/tmp/"]
            created = [f"{random.choice(temp_paths)}.tmp_{uuid.uuid4().hex[:8]}"]
            if random.random() > 0.5:
                modified = ["/etc/passwd"]
        
        return created, modified
    
    def _simulate_registry_changes(self, profile: Dict[str, Any]) -> List[str]:
        """Simulate registry changes (Windows only)."""
        if profile.get("os") != "Windows":
            return []
        
        changes = []
        
        if random.random() > 0.6:
            changes.append("HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run")
        
        if random.random() > 0.8:
            changes.append("HKLM\\SYSTEM\\CurrentControlSet\\Services")
        
        return changes
    
    def _simulate_network_activity(self, profile: Dict[str, Any]) -> List[str]:
        """Simulate network activity."""
        if profile.get("network") == "disconnected":
            return ["connection_failed:timeout"]
        
        activity = []
        
        # Simulate DNS queries
        domains = ["update.microsoft.com", "cdn.example.com", "api.suspicious-domain.xyz"]
        activity.append(f"dns_query:{random.choice(domains)}")
        
        # Simulate connections
        if random.random() > 0.5:
            ip = f"{random.randint(1, 255)}.{random.randint(0, 255)}.{random.randint(0, 255)}.{random.randint(1, 255)}"
            port = random.choice([80, 443, 8080, 4444, 31337])
            activity.append(f"connect:{ip}:{port}")
        
        return activity
    
    def _determine_behaviors(
        self,
        profile: Dict[str, Any],
        syscalls: List[str],
        files_created: List[str],
        files_modified: List[str],
        registry_changes: List[str],
        network_activity: List[str]
    ) -> List[str]:
        """
        Determine behavior categories from execution artifacts.
        
        Simulates behavior detection based on profile and artifacts.
        """
        behaviors = []
        
        # Analyze file operations
        if files_created:
            behaviors.append("file_system_modification")
        
        if files_modified:
            if any("SAM" in f or "passwd" in f for f in files_modified):
                behaviors.append("credential_access")
            else:
                behaviors.append("file_system_modification")
        
        # Analyze registry
        if registry_changes:
            behaviors.append("registry_modification")
            if any("Run" in r or "Services" in r for r in registry_changes):
                behaviors.append("persistence_mechanism")
        
        # Analyze network
        for activity in network_activity:
            if "suspicious" in activity or "4444" in activity or "31337" in activity:
                behaviors.append("c2_communication")
                behaviors.append("suspicious_network")
            elif "connection_failed" in activity:
                behaviors.append("defense_evasion")  # Anti-sandbox behavior
            else:
                behaviors.append("normal_network")
        
        # Analyze syscalls
        if "NtLoadDriver" in syscalls:
            behaviors.append("rootkit_installation")
        
        # Simulate additional behaviors based on binary characteristics
        if self.current_binary:
            binary_name = Path(self.current_binary).name.lower()
            
            # Heuristic: suspicious names
            if any(x in binary_name for x in ["ransom", "crypt", "lock"]):
                behaviors.append("ransomware_encryption")
            
            if any(x in binary_name for x in ["inject", "hook", "hollow"]):
                behaviors.append("process_injection")
            
            if any(x in binary_name for x in ["mimikatz", "lazagne", "creds"]):
                behaviors.append("credential_access")
        
        # Add random variation for simulation
        if random.random() > 0.7:
            behaviors.append(random.choice(["process_creation", "script_execution", "system_discovery"]))
        
        # Privilege-dependent behaviors
        if profile.get("privilege") in ["Administrator", "root"]:
            if random.random() > 0.8:
                behaviors.append("privilege_escalation")
        
        return list(set(behaviors))  # Deduplicate
    
    def execute(self, profile: str) -> List[str]:
        """
        Execute the loaded binary under a specific environment profile.
        
        Simulates execution and captures behaviors.
        
        Args:
            profile: Name of the environment profile to use.
            
        Returns:
            List of captured behavior strings.
        """
        if not self.current_binary:
            logger.warning("No binary loaded for execution")
            return []
        
        if profile not in self.environment_profiles:
            logger.error(f"Unknown profile: {profile}")
            return []
        
        profile_config = self.environment_profiles[profile]
        
        # Simulate execution start
        start_time = datetime.now()
        
        # Capture artifacts
        syscalls = self._simulate_syscalls(profile_config)
        files_created, files_modified = self._simulate_file_operations(profile_config)
        registry_changes = self._simulate_registry_changes(profile_config)
        network_activity = self._simulate_network_activity(profile_config)
        
        # Determine behaviors
        behaviors = self._determine_behaviors(
            profile_config,
            syscalls,
            files_created,
            files_modified,
            registry_changes,
            network_activity
        )
        
        # Calculate execution time (simulated)
        execution_time = random.uniform(0.5, 5.0)
        
        # Create execution result
        result = ExecutionResult(
            profile=profile,
            behaviors=behaviors,
            syscalls=syscalls,
            files_created=files_created,
            files_modified=files_modified,
            registry_changes=registry_changes,
            network_activity=network_activity,
            execution_time=execution_time,
            exit_code=0 if random.random() > 0.1 else 1,
        )
        
        self._execution_results.append(result)
        
        # Log execution
        self.logs.append({
            "action": "execute",
            "binary": self.current_binary,
            "profile": profile,
            "behaviors": behaviors,
            "execution_time": execution_time,
            "timestamp": datetime.now().isoformat(),
        })
        
        logger.info(
            f"Executed {self.current_binary} under {profile}: "
            f"{len(behaviors)} behaviors captured in {execution_time:.2f}s"
        )
        
        return behaviors
    
    def analyze_behaviors(self, behaviors: List[str]) -> SandboxAlert:
        """
        Analyze captured behaviors and generate a SandboxAlert.
        
        Computes risk score based on behavior patterns and weights.
        
        Args:
            behaviors: List of behavior strings to analyze.
            
        Returns:
            SandboxAlert with analysis results.
        """
        if not behaviors:
            return SandboxAlert(
                alert_id=str(uuid.uuid4()),
                binary_name=Path(self.current_binary).name if self.current_binary else "unknown",
                behaviors=[],
                risk_score=0.0,
                description="No behaviors captured during analysis",
                binary_hash=self.current_binary_hash,
            )
        
        # Calculate risk score from behavior weights
        risk_scores = [
            self.BEHAVIOR_RISK_WEIGHTS.get(b, 0.1)
            for b in behaviors
        ]
        
        # Use weighted average with bonus for multiple high-risk behaviors
        base_score = sum(risk_scores) / len(risk_scores)
        high_risk_count = sum(1 for s in risk_scores if s >= 0.7)
        bonus = min(high_risk_count * 0.1, 0.3)
        
        risk_score = min(base_score + bonus, 1.0)
        
        # Detect known malicious patterns
        detected_patterns = []
        behavior_set = set(behaviors)
        
        for i, pattern in enumerate(self.MALICIOUS_PATTERNS):
            pattern_set = set(pattern)
            match_ratio = len(behavior_set & pattern_set) / len(pattern_set)
            if match_ratio >= 0.5:
                pattern_names = ["Ransomware", "Credential Stealer", "RAT", "Dropper", "Rootkit"]
                detected_patterns.append((pattern_names[i], match_ratio))
        
        # Build description
        binary_name = Path(self.current_binary).name if self.current_binary else "unknown"
        
        if detected_patterns:
            pattern_desc = ", ".join(f"{p[0]} ({p[1]*100:.0f}% match)" for p in detected_patterns)
            description = (
                f"Malicious behavior detected in '{binary_name}'. "
                f"Risk: {risk_score:.2f}. "
                f"Matched patterns: {pattern_desc}. "
                f"Behaviors: {', '.join(behaviors[:5])}{'...' if len(behaviors) > 5 else ''}"
            )
        elif risk_score >= 0.5:
            description = (
                f"Suspicious behavior detected in '{binary_name}'. "
                f"Risk: {risk_score:.2f}. "
                f"Behaviors: {', '.join(behaviors[:5])}{'...' if len(behaviors) > 5 else ''}"
            )
        else:
            description = (
                f"Analysis complete for '{binary_name}'. "
                f"Risk: {risk_score:.2f}. No significant threats detected."
            )
        
        # Create alert
        profiles_tested = list(set(r.profile for r in self._execution_results))
        total_time = sum(r.execution_time for r in self._execution_results)
        
        alert = SandboxAlert(
            alert_id=str(uuid.uuid4()),
            binary_name=binary_name,
            behaviors=behaviors,
            risk_score=risk_score,
            description=description,
            binary_hash=self.current_binary_hash,
            profiles_tested=profiles_tested,
            execution_time=total_time,
            metadata={
                "detected_patterns": [p[0] for p in detected_patterns],
                "pattern_confidence": {p[0]: p[1] for p in detected_patterns},
                "high_risk_behavior_count": high_risk_count,
            },
        )
        
        return alert
    
    def run(self, path: str, profiles: Optional[List[str]] = None) -> SandboxAlert:
        """
        Run complete sandbox analysis on a binary.
        
        Loads the binary, executes under multiple profiles, analyzes
        behaviors, and returns a comprehensive alert.
        
        Args:
            path: Path to the binary to analyze.
            profiles: List of profiles to test. Uses all if not specified.
            
        Returns:
            SandboxAlert with complete analysis results.
        """
        # Reset execution results for new analysis
        self._execution_results.clear()
        
        # Load binary
        self.load_binary(path)
        
        # Determine profiles to use
        if profiles is None:
            # Use a subset of profiles for efficiency
            profiles = ["Windows10_Admin", "Windows10_User", "Windows_Isolated"]
        
        # Execute under each profile
        all_behaviors: Set[str] = set()
        
        for profile in profiles:
            if profile in self.environment_profiles:
                behaviors = self.execute(profile)
                all_behaviors.update(behaviors)
        
        # Analyze combined behaviors
        alert = self.analyze_behaviors(list(all_behaviors))
        
        # Store and notify
        self._alert_history.append(alert)
        
        if alert.risk_score >= 0.5:
            self._notify_subscribers(alert)
            logger.warning(f"Sandbox alert generated: {alert.binary_name} (risk: {alert.risk_score:.2f})")
        else:
            logger.info(f"Sandbox analysis complete: {alert.binary_name} (risk: {alert.risk_score:.2f})")
        
        return alert
    
    def add_profile(self, name: str, config: Dict[str, Any]) -> None:
        """
        Add a new environment profile.
        
        Args:
            name: Profile name.
            config: Profile configuration dictionary.
        """
        self.environment_profiles[name] = config
        logger.info(f"Added environment profile: {name}")
    
    def remove_profile(self, name: str) -> bool:
        """
        Remove an environment profile.
        
        Args:
            name: Profile name to remove.
            
        Returns:
            True if removed, False if not found.
        """
        if name in self.environment_profiles:
            del self.environment_profiles[name]
            return True
        return False
    
    def get_execution_logs(self, binary_hash: Optional[str] = None) -> List[Dict[str, Any]]:
        """
        Get execution logs, optionally filtered by binary hash.
        
        Args:
            binary_hash: Filter by specific binary hash.
            
        Returns:
            List of log entries.
        """
        if binary_hash:
            return [log for log in self.logs if log.get("hash") == binary_hash]
        return self.logs
    
    def get_alert_history(
        self,
        min_risk: float = 0.0,
        pattern: Optional[str] = None
    ) -> List[SandboxAlert]:
        """
        Get alert history with optional filters.
        
        Args:
            min_risk: Minimum risk score filter.
            pattern: Filter by detected pattern name.
            
        Returns:
            List of matching alerts.
        """
        results = self._alert_history
        
        if min_risk > 0:
            results = [a for a in results if a.risk_score >= min_risk]
        
        if pattern:
            results = [
                a for a in results
                if pattern in a.metadata.get("detected_patterns", [])
            ]
        
        return results
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get sandbox statistics.
        
        Returns:
            Dictionary with sandbox statistics.
        """
        if not self._alert_history:
            return {
                "total_analyses": 0,
                "total_executions": len(self._execution_results),
                "profiles_available": list(self.environment_profiles.keys()),
            }
        
        risk_scores = [a.risk_score for a in self._alert_history]
        pattern_counts: Dict[str, int] = {}
        
        for alert in self._alert_history:
            for pattern in alert.metadata.get("detected_patterns", []):
                pattern_counts[pattern] = pattern_counts.get(pattern, 0) + 1
        
        return {
            "total_analyses": len(self._alert_history),
            "total_executions": len(self._execution_results),
            "avg_risk_score": sum(risk_scores) / len(risk_scores),
            "max_risk_score": max(risk_scores),
            "malicious_count": sum(1 for r in risk_scores if r >= 0.7),
            "suspicious_count": sum(1 for r in risk_scores if 0.5 <= r < 0.7),
            "clean_count": sum(1 for r in risk_scores if r < 0.5),
            "pattern_distribution": pattern_counts,
            "profiles_available": list(self.environment_profiles.keys()),
        }
    
    def clear(self) -> None:
        """Clear all logs and history."""
        self.logs.clear()
        self._alert_history.clear()
        self._execution_results.clear()
        self.current_binary = None
        self.current_binary_hash = ""
        logger.info("ThreatSandbox cleared")


# Factory function for pipeline integration
def create_sandbox(
    profiles: Optional[Dict[str, Dict[str, Any]]] = None
) -> ThreatSandbox:
    """
    Factory function to create a ThreatSandbox instance.
    
    Args:
        profiles: Custom environment profiles.
        
    Returns:
        New ThreatSandbox instance.
    """
    return ThreatSandbox(environment_profiles=profiles)


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    sandbox = create_sandbox()
    
    # Subscribe to alerts
    def on_alert(alert: SandboxAlert):
        print(f"\n>>> SANDBOX ALERT [{alert.risk_score:.2f}]")
        print(f"    Binary: {alert.binary_name}")
        print(f"    Hash: {alert.binary_hash[:32]}...")
        print(f"    Behaviors: {', '.join(alert.behaviors[:5])}")
        if alert.metadata.get("detected_patterns"):
            print(f"    Patterns: {', '.join(alert.metadata['detected_patterns'])}")
        print(f"    Description: {alert.description}\n")
    
    sandbox.subscribe(on_alert)
    
    # Test with various "binaries"
    test_binaries = [
        "/downloads/suspicious_file.exe",
        "/tmp/ransomware_variant.exe",
        "/home/user/mimikatz_clone.exe",
        "/usr/local/bin/normal_app",
        "/downloads/cryptolocker.exe",
    ]
    
    print("=== Running Sandbox Analysis ===\n")
    
    for binary in test_binaries:
        print(f"Analyzing: {binary}")
        print("-" * 50)
        
        alert = sandbox.run(binary)
        
        print(f"  Risk Score: {alert.risk_score:.2f}")
        print(f"  Profiles Tested: {', '.join(alert.profiles_tested)}")
        print(f"  Execution Time: {alert.execution_time:.2f}s")
        print(f"  Behaviors: {len(alert.behaviors)}")
        print()
    
    # Statistics
    print("=== Sandbox Statistics ===")
    stats = sandbox.get_statistics()
    print(f"Total Analyses: {stats['total_analyses']}")
    print(f"Average Risk: {stats.get('avg_risk_score', 0):.2f}")
    print(f"Malicious: {stats.get('malicious_count', 0)}")
    print(f"Suspicious: {stats.get('suspicious_count', 0)}")
    print(f"Clean: {stats.get('clean_count', 0)}")
    print(f"Pattern Distribution: {stats.get('pattern_distribution', {})}")
    
    # Filter high-risk alerts
    print("\n=== High-Risk Alerts ===")
    high_risk = sandbox.get_alert_history(min_risk=0.7)
    for alert in high_risk:
        print(f"  - {alert.binary_name}: {alert.risk_score:.2f}")
