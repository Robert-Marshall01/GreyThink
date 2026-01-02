"""
Adaptive Threat Simulation Engine for GreyAV Antivirus/EDR System.

This module provides a framework for generating synthetic attack scenarios
to validate detection capabilities. It creates realistic but safe events
that mimic attacker behavior, feeds them through the detection pipeline,
and compares expected vs actual outcomes to identify detection gaps.
"""

import random
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Callable, Dict, List, Optional, Set


@dataclass
class Event:
    """
    Represents a system event for simulation purposes.
    
    This is a simplified event structure that can be extended or
    replaced with the actual Event class from the main system.
    
    Attributes:
        event_id: Unique identifier for this event.
        event_type: Type of event (process_spawn, file_write, etc.).
        timestamp: When the event occurred.
        process_name: Name of the process generating the event.
        process_id: Process ID.
        parent_process_id: Parent process ID.
        user: User context.
        details: Additional event-specific details.
        tags: Classification tags for the event.
        is_simulated: Flag indicating this is a simulated event.
    """
    event_id: str
    event_type: str
    timestamp: datetime
    process_name: str = ""
    process_id: int = 0
    parent_process_id: int = 0
    user: str = "SYSTEM"
    details: Dict[str, Any] = field(default_factory=dict)
    tags: List[str] = field(default_factory=list)
    is_simulated: bool = True
    
    @staticmethod
    def generate_id() -> str:
        """Generate a unique event ID."""
        return f"sim-{uuid.uuid4().hex[:12]}"


@dataclass
class SimulationReport:
    """
    Report summarizing the results of a threat simulation.
    
    Attributes:
        simulation_id: Unique identifier for this simulation run.
        scenario_name: Name of the scenario that was executed.
        events_generated: List of synthetic events that were created.
        alerts_triggered: List of alert IDs/descriptions that were triggered.
        success: Whether all expected detections occurred.
        gaps: List of detection gaps identified.
        timestamp: When the simulation was run.
        duration_ms: How long the simulation took in milliseconds.
        expected_alerts: List of alerts that were expected.
        false_positives: List of unexpected alerts triggered.
    """
    simulation_id: str
    scenario_name: str
    events_generated: List[Event]
    alerts_triggered: List[str]
    success: bool
    gaps: List[str]
    timestamp: datetime
    duration_ms: float = 0.0
    expected_alerts: List[str] = field(default_factory=list)
    false_positives: List[str] = field(default_factory=list)
    
    @staticmethod
    def generate_id() -> str:
        """Generate a unique simulation ID."""
        return f"sim-run-{uuid.uuid4().hex[:8]}"
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert report to dictionary for serialization."""
        return {
            "simulation_id": self.simulation_id,
            "scenario_name": self.scenario_name,
            "events_generated": len(self.events_generated),
            "alerts_triggered": self.alerts_triggered,
            "success": self.success,
            "gaps": self.gaps,
            "timestamp": self.timestamp.isoformat(),
            "duration_ms": self.duration_ms,
            "expected_alerts": self.expected_alerts,
            "false_positives": self.false_positives
        }
    
    def summary(self) -> str:
        """Generate a human-readable summary."""
        status = "PASSED" if self.success else "FAILED"
        return (
            f"[{status}] {self.scenario_name}: "
            f"{len(self.alerts_triggered)}/{len(self.expected_alerts)} alerts, "
            f"{len(self.gaps)} gaps, {self.duration_ms:.1f}ms"
        )


@dataclass
class ScenarioDefinition:
    """
    Defines a threat simulation scenario.
    
    Attributes:
        name: Unique name for the scenario.
        description: Human-readable description.
        generator: Callable that generates events for this scenario.
        expected_alerts: List of alert types expected to be triggered.
        tags: Classification tags (e.g., "ransomware", "apt", "persistence").
        severity: Severity level (1-5, where 5 is critical).
    """
    name: str
    description: str
    generator: Callable[[], List[Event]]
    expected_alerts: List[str]
    tags: List[str] = field(default_factory=list)
    severity: int = 3


class EventPipelineInterface:
    """
    Interface for the event pipeline.
    
    This is a mock/interface class that can be replaced with the actual
    EventPipeline from the main system. It collects alerts triggered
    during simulation.
    """
    
    def __init__(self):
        """Initialize the pipeline interface."""
        self._alerts: List[str] = []
        self._processed_events: List[Event] = []
    
    def process_event(self, event: Event) -> List[str]:
        """
        Process an event and return any triggered alerts.
        
        Args:
            event: Event to process.
        
        Returns:
            List of alert identifiers triggered by this event.
        """
        self._processed_events.append(event)
        # Mock: actual implementation would run through detection engines
        return []
    
    def get_alerts(self) -> List[str]:
        """Get all alerts triggered during processing."""
        return self._alerts.copy()
    
    def add_alert(self, alert: str) -> None:
        """Add an alert (used by detection engines)."""
        self._alerts.append(alert)
    
    def clear(self) -> None:
        """Clear all stored alerts and events."""
        self._alerts.clear()
        self._processed_events.clear()


class ThreatSimulator:
    """
    Adaptive Threat Simulation Engine.
    
    This class maintains a library of synthetic attack scenarios and
    provides methods to execute them against the detection pipeline.
    It generates realistic but safe events, feeds them through the
    pipeline, and compares expected vs actual detection outcomes.
    """
    
    def __init__(self, pipeline: Optional[EventPipelineInterface] = None):
        """
        Initialize the ThreatSimulator.
        
        Args:
            pipeline: EventPipeline to inject events into.
                     If None, a mock pipeline is created.
        """
        self._pipeline = pipeline or EventPipelineInterface()
        self._scenarios: Dict[str, ScenarioDefinition] = {}
        self._reports: List[SimulationReport] = []
        self._alert_collector: List[str] = []
        
        # Register built-in scenarios
        self._register_builtin_scenarios()
    
    @property
    def scenarios(self) -> Dict[str, Callable[[], List[Event]]]:
        """Get scenario generators as a simple dict."""
        return {name: s.generator for name, s in self._scenarios.items()}
    
    def add_scenario(
        self,
        name: str,
        generator: Callable[[], List[Event]],
        expected_alerts: Optional[List[str]] = None,
        description: str = "",
        tags: Optional[List[str]] = None,
        severity: int = 3
    ) -> None:
        """
        Add a new simulation scenario.
        
        Args:
            name: Unique name for the scenario.
            generator: Callable that returns a list of Events.
            expected_alerts: List of alert types expected to trigger.
            description: Human-readable description.
            tags: Classification tags.
            severity: Severity level (1-5).
        """
        self._scenarios[name] = ScenarioDefinition(
            name=name,
            description=description or f"Custom scenario: {name}",
            generator=generator,
            expected_alerts=expected_alerts or [],
            tags=tags or [],
            severity=severity
        )
    
    def remove_scenario(self, name: str) -> bool:
        """
        Remove a scenario.
        
        Args:
            name: Scenario name to remove.
        
        Returns:
            True if removed, False if not found.
        """
        if name in self._scenarios:
            del self._scenarios[name]
            return True
        return False
    
    def list_scenarios(self) -> List[str]:
        """Get list of available scenario names."""
        return list(self._scenarios.keys())
    
    def get_scenario_info(self, name: str) -> Optional[Dict[str, Any]]:
        """
        Get information about a scenario.
        
        Args:
            name: Scenario name.
        
        Returns:
            Dictionary with scenario information, or None if not found.
        """
        if name not in self._scenarios:
            return None
        
        scenario = self._scenarios[name]
        return {
            "name": scenario.name,
            "description": scenario.description,
            "expected_alerts": scenario.expected_alerts,
            "tags": scenario.tags,
            "severity": scenario.severity
        }
    
    def run_scenario(self, name: str) -> SimulationReport:
        """
        Execute a simulation scenario.
        
        This method generates synthetic events, feeds them into the
        pipeline, collects triggered alerts, and compares against
        expected outcomes.
        
        Args:
            name: Name of the scenario to run.
        
        Returns:
            SimulationReport with results.
        
        Raises:
            ValueError: If scenario name is not found.
        """
        if name not in self._scenarios:
            raise ValueError(f"Unknown scenario: {name}")
        
        scenario = self._scenarios[name]
        start_time = datetime.now()
        
        # Clear previous state
        self._alert_collector.clear()
        self._pipeline.clear()
        
        # Generate events
        events = scenario.generator()
        
        # Feed events into pipeline
        for event in events:
            alerts = self._pipeline.process_event(event)
            self._alert_collector.extend(alerts)
        
        # Also get any alerts collected by the pipeline
        self._alert_collector.extend(self._pipeline.get_alerts())
        
        # Deduplicate alerts
        triggered_alerts = list(set(self._alert_collector))
        
        # Compare against expected outcomes
        expected_set = set(scenario.expected_alerts)
        triggered_set = set(triggered_alerts)
        
        # Identify gaps (expected but not triggered)
        gaps = list(expected_set - triggered_set)
        
        # Identify false positives (triggered but not expected)
        false_positives = list(triggered_set - expected_set)
        
        # Success if all expected alerts were triggered
        success = len(gaps) == 0
        
        end_time = datetime.now()
        duration_ms = (end_time - start_time).total_seconds() * 1000
        
        report = SimulationReport(
            simulation_id=SimulationReport.generate_id(),
            scenario_name=name,
            events_generated=events,
            alerts_triggered=triggered_alerts,
            success=success,
            gaps=gaps,
            timestamp=start_time,
            duration_ms=duration_ms,
            expected_alerts=scenario.expected_alerts,
            false_positives=false_positives
        )
        
        self._reports.append(report)
        return report
    
    def run_all(self) -> List[SimulationReport]:
        """
        Execute all registered scenarios.
        
        Returns:
            List of SimulationReport objects for all scenarios.
        """
        reports = []
        for name in self._scenarios:
            try:
                report = self.run_scenario(name)
                reports.append(report)
            except Exception as e:
                # Create a failure report
                report = SimulationReport(
                    simulation_id=SimulationReport.generate_id(),
                    scenario_name=name,
                    events_generated=[],
                    alerts_triggered=[],
                    success=False,
                    gaps=[f"Scenario execution failed: {str(e)}"],
                    timestamp=datetime.now()
                )
                reports.append(report)
        
        return reports
    
    def run_by_tag(self, tag: str) -> List[SimulationReport]:
        """
        Execute all scenarios with a specific tag.
        
        Args:
            tag: Tag to filter scenarios by.
        
        Returns:
            List of SimulationReport objects.
        """
        reports = []
        for name, scenario in self._scenarios.items():
            if tag in scenario.tags:
                report = self.run_scenario(name)
                reports.append(report)
        return reports
    
    def get_reports(self) -> List[SimulationReport]:
        """Get all simulation reports from this session."""
        return self._reports.copy()
    
    def clear_reports(self) -> None:
        """Clear stored simulation reports."""
        self._reports.clear()
    
    def generate_summary(self) -> Dict[str, Any]:
        """
        Generate a summary of all simulation results.
        
        Returns:
            Dictionary with summary statistics.
        """
        if not self._reports:
            return {"total": 0, "passed": 0, "failed": 0, "gaps": []}
        
        passed = sum(1 for r in self._reports if r.success)
        failed = len(self._reports) - passed
        all_gaps = []
        for r in self._reports:
            for gap in r.gaps:
                all_gaps.append(f"{r.scenario_name}: {gap}")
        
        return {
            "total": len(self._reports),
            "passed": passed,
            "failed": failed,
            "pass_rate": passed / len(self._reports) * 100,
            "gaps": all_gaps,
            "timestamp": datetime.now().isoformat()
        }
    
    # =========================================================================
    # Built-in Scenario Generators
    # =========================================================================
    
    def _register_builtin_scenarios(self) -> None:
        """Register all built-in threat scenarios."""
        
        self.add_scenario(
            name="ransomware_staging",
            generator=self._scenario_ransomware_staging,
            expected_alerts=[
                "suspicious_process_spawn",
                "high_entropy_file_write",
                "shadow_copy_deletion",
                "ransomware_behavior"
            ],
            description="Simulates ransomware staging: process spawn → entropy write → shadow copy delete",
            tags=["ransomware", "destructive", "critical"],
            severity=5
        )
        
        self.add_scenario(
            name="persistence_attempt",
            generator=self._scenario_persistence_attempt,
            expected_alerts=[
                "registry_autorun_modification",
                "service_installation",
                "persistence_mechanism"
            ],
            description="Simulates persistence: registry write → service install",
            tags=["persistence", "apt", "stealth"],
            severity=4
        )
        
        self.add_scenario(
            name="privilege_escalation",
            generator=self._scenario_privilege_escalation,
            expected_alerts=[
                "lsass_access",
                "token_manipulation",
                "privilege_escalation"
            ],
            description="Simulates privilege escalation: LSASS access → token manipulation",
            tags=["privilege_escalation", "credential_theft", "critical"],
            severity=5
        )
        
        self.add_scenario(
            name="beaconing",
            generator=self._scenario_beaconing,
            expected_alerts=[
                "periodic_outbound_connection",
                "c2_beaconing",
                "suspicious_network_pattern"
            ],
            description="Simulates C2 beaconing: repeated outbound connections",
            tags=["c2", "network", "apt"],
            severity=4
        )
        
        self.add_scenario(
            name="exfiltration",
            generator=self._scenario_exfiltration,
            expected_alerts=[
                "sensitive_file_access",
                "large_outbound_transfer",
                "data_exfiltration"
            ],
            description="Simulates data exfiltration: file read → outbound connection",
            tags=["exfiltration", "data_theft", "critical"],
            severity=5
        )
        
        self.add_scenario(
            name="lateral_movement",
            generator=self._scenario_lateral_movement,
            expected_alerts=[
                "remote_service_access",
                "credential_use",
                "lateral_movement"
            ],
            description="Simulates lateral movement across network",
            tags=["lateral_movement", "apt", "network"],
            severity=4
        )
        
        self.add_scenario(
            name="defense_evasion",
            generator=self._scenario_defense_evasion,
            expected_alerts=[
                "security_tool_tampering",
                "log_deletion",
                "defense_evasion"
            ],
            description="Simulates defense evasion techniques",
            tags=["evasion", "stealth", "critical"],
            severity=5
        )
    
    def _scenario_ransomware_staging(self) -> List[Event]:
        """
        Generate events simulating ransomware staging behavior.
        
        Sequence: process spawn → high entropy file writes → shadow copy deletion
        """
        base_time = datetime.now()
        pid = random.randint(1000, 9999)
        parent_pid = random.randint(100, 999)
        
        events = [
            # Suspicious process spawn
            Event(
                event_id=Event.generate_id(),
                event_type="process_spawn",
                timestamp=base_time,
                process_name="cryptor.exe",
                process_id=pid,
                parent_process_id=parent_pid,
                user="victim_user",
                details={
                    "command_line": "cryptor.exe --encrypt --fast",
                    "parent_process": "explorer.exe",
                    "working_directory": "C:\\Users\\victim_user\\Downloads"
                },
                tags=["suspicious_spawn", "downloaded_executable"]
            ),
            
            # High entropy file write (encrypted file)
            Event(
                event_id=Event.generate_id(),
                event_type="file_write",
                timestamp=base_time + timedelta(seconds=2),
                process_name="cryptor.exe",
                process_id=pid,
                parent_process_id=parent_pid,
                user="victim_user",
                details={
                    "file_path": "C:\\Users\\victim_user\\Documents\\important.docx.encrypted",
                    "entropy": 7.98,
                    "size_bytes": 524288,
                    "original_file": "important.docx"
                },
                tags=["high_entropy", "file_encryption"]
            ),
            
            # More encrypted files
            Event(
                event_id=Event.generate_id(),
                event_type="file_write",
                timestamp=base_time + timedelta(seconds=3),
                process_name="cryptor.exe",
                process_id=pid,
                parent_process_id=parent_pid,
                user="victim_user",
                details={
                    "file_path": "C:\\Users\\victim_user\\Documents\\budget.xlsx.encrypted",
                    "entropy": 7.95,
                    "size_bytes": 102400
                },
                tags=["high_entropy", "file_encryption"]
            ),
            
            # Shadow copy deletion
            Event(
                event_id=Event.generate_id(),
                event_type="process_spawn",
                timestamp=base_time + timedelta(seconds=5),
                process_name="vssadmin.exe",
                process_id=pid + 1,
                parent_process_id=pid,
                user="SYSTEM",
                details={
                    "command_line": "vssadmin.exe delete shadows /all /quiet",
                    "parent_process": "cryptor.exe"
                },
                tags=["shadow_copy_deletion", "anti_recovery"]
            ),
            
            # Ransom note creation
            Event(
                event_id=Event.generate_id(),
                event_type="file_write",
                timestamp=base_time + timedelta(seconds=6),
                process_name="cryptor.exe",
                process_id=pid,
                parent_process_id=parent_pid,
                user="victim_user",
                details={
                    "file_path": "C:\\Users\\victim_user\\Desktop\\README_DECRYPT.txt",
                    "content_preview": "Your files have been encrypted..."
                },
                tags=["ransom_note"]
            )
        ]
        
        return events
    
    def _scenario_persistence_attempt(self) -> List[Event]:
        """
        Generate events simulating persistence mechanism installation.
        
        Sequence: registry autorun modification → service installation
        """
        base_time = datetime.now()
        pid = random.randint(1000, 9999)
        
        events = [
            # Registry autorun modification
            Event(
                event_id=Event.generate_id(),
                event_type="registry_write",
                timestamp=base_time,
                process_name="malware.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "key": "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run",
                    "value_name": "WindowsUpdate",
                    "value_data": "C:\\ProgramData\\update.exe",
                    "value_type": "REG_SZ"
                },
                tags=["autorun", "persistence"]
            ),
            
            # Drop persistence payload
            Event(
                event_id=Event.generate_id(),
                event_type="file_write",
                timestamp=base_time + timedelta(seconds=1),
                process_name="malware.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "file_path": "C:\\ProgramData\\update.exe",
                    "size_bytes": 45056,
                    "is_executable": True
                },
                tags=["dropped_executable", "persistence"]
            ),
            
            # Service creation
            Event(
                event_id=Event.generate_id(),
                event_type="service_install",
                timestamp=base_time + timedelta(seconds=3),
                process_name="sc.exe",
                process_id=pid + 1,
                parent_process_id=pid,
                user="SYSTEM",
                details={
                    "service_name": "WindowsUpdateService",
                    "display_name": "Windows Update Service",
                    "binary_path": "C:\\ProgramData\\svchost.exe",
                    "start_type": "auto"
                },
                tags=["service_installation", "persistence"]
            ),
            
            # Scheduled task creation
            Event(
                event_id=Event.generate_id(),
                event_type="scheduled_task_create",
                timestamp=base_time + timedelta(seconds=5),
                process_name="schtasks.exe",
                process_id=pid + 2,
                parent_process_id=pid,
                user="SYSTEM",
                details={
                    "task_name": "\\Microsoft\\Windows\\UpdateTask",
                    "action": "C:\\ProgramData\\update.exe",
                    "trigger": "ONLOGON"
                },
                tags=["scheduled_task", "persistence"]
            )
        ]
        
        return events
    
    def _scenario_privilege_escalation(self) -> List[Event]:
        """
        Generate events simulating privilege escalation.
        
        Sequence: LSASS memory access → token manipulation
        """
        base_time = datetime.now()
        pid = random.randint(1000, 9999)
        lsass_pid = 672  # Typical LSASS PID
        
        events = [
            # LSASS memory access (credential dumping)
            Event(
                event_id=Event.generate_id(),
                event_type="process_access",
                timestamp=base_time,
                process_name="procdump.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "target_process": "lsass.exe",
                    "target_pid": lsass_pid,
                    "access_mask": "0x1F0FFF",
                    "access_type": "PROCESS_ALL_ACCESS"
                },
                tags=["lsass_access", "credential_access"]
            ),
            
            # Memory dump creation
            Event(
                event_id=Event.generate_id(),
                event_type="file_write",
                timestamp=base_time + timedelta(seconds=2),
                process_name="procdump.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "file_path": "C:\\Temp\\lsass.dmp",
                    "size_bytes": 52428800,
                    "is_memory_dump": True
                },
                tags=["memory_dump", "credential_theft"]
            ),
            
            # Token manipulation
            Event(
                event_id=Event.generate_id(),
                event_type="token_manipulation",
                timestamp=base_time + timedelta(seconds=4),
                process_name="malware.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "action": "impersonate",
                    "target_user": "SYSTEM",
                    "privilege": "SeDebugPrivilege"
                },
                tags=["token_manipulation", "privilege_escalation"]
            ),
            
            # Elevated process spawn
            Event(
                event_id=Event.generate_id(),
                event_type="process_spawn",
                timestamp=base_time + timedelta(seconds=5),
                process_name="cmd.exe",
                process_id=pid + 1,
                parent_process_id=pid,
                user="SYSTEM",
                details={
                    "command_line": "cmd.exe /c whoami",
                    "integrity_level": "System",
                    "elevated": True
                },
                tags=["elevated_process", "privilege_escalation"]
            )
        ]
        
        return events
    
    def _scenario_beaconing(self) -> List[Event]:
        """
        Generate events simulating C2 beaconing behavior.
        
        Sequence: repeated periodic outbound connections
        """
        base_time = datetime.now()
        pid = random.randint(1000, 9999)
        c2_server = "185.192.70.42"
        beacon_interval = 60  # seconds
        
        events = []
        
        # Generate multiple beacon connections
        for i in range(5):
            events.append(Event(
                event_id=Event.generate_id(),
                event_type="network_connection",
                timestamp=base_time + timedelta(seconds=i * beacon_interval),
                process_name="svchost.exe",
                process_id=pid,
                user="SYSTEM",
                details={
                    "direction": "outbound",
                    "remote_ip": c2_server,
                    "remote_port": 443,
                    "local_port": 49152 + i,
                    "protocol": "TCP",
                    "bytes_sent": random.randint(100, 500),
                    "bytes_received": random.randint(50, 200),
                    "beacon_number": i + 1
                },
                tags=["outbound_connection", "periodic", "potential_c2"]
            ))
        
        # DNS query to suspicious domain
        events.append(Event(
            event_id=Event.generate_id(),
            event_type="dns_query",
            timestamp=base_time + timedelta(seconds=10),
            process_name="svchost.exe",
            process_id=pid,
            user="SYSTEM",
            details={
                "query": "update.evil-domain.com",
                "query_type": "A",
                "response": c2_server
            },
            tags=["dns_query", "suspicious_domain"]
        ))
        
        return events
    
    def _scenario_exfiltration(self) -> List[Event]:
        """
        Generate events simulating data exfiltration.
        
        Sequence: sensitive file reads → archive creation → outbound transfer
        """
        base_time = datetime.now()
        pid = random.randint(1000, 9999)
        
        events = [
            # Access sensitive files
            Event(
                event_id=Event.generate_id(),
                event_type="file_read",
                timestamp=base_time,
                process_name="collector.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "file_path": "C:\\Users\\victim_user\\Documents\\passwords.xlsx",
                    "size_bytes": 15360,
                    "file_type": "spreadsheet"
                },
                tags=["sensitive_file_access", "credential_file"]
            ),
            
            Event(
                event_id=Event.generate_id(),
                event_type="file_read",
                timestamp=base_time + timedelta(seconds=1),
                process_name="collector.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "file_path": "C:\\Users\\victim_user\\Documents\\client_data.csv",
                    "size_bytes": 2097152,
                    "file_type": "csv"
                },
                tags=["sensitive_file_access", "pii_data"]
            ),
            
            # Create archive
            Event(
                event_id=Event.generate_id(),
                event_type="file_write",
                timestamp=base_time + timedelta(seconds=3),
                process_name="collector.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "file_path": "C:\\Temp\\data.zip",
                    "size_bytes": 1048576,
                    "is_archive": True,
                    "compressed_files": 15
                },
                tags=["archive_creation", "staging"]
            ),
            
            # Large outbound transfer
            Event(
                event_id=Event.generate_id(),
                event_type="network_connection",
                timestamp=base_time + timedelta(seconds=5),
                process_name="collector.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "direction": "outbound",
                    "remote_ip": "104.21.45.67",
                    "remote_port": 443,
                    "protocol": "TCP",
                    "bytes_sent": 1048576,
                    "bytes_received": 256,
                    "destination_domain": "file-share.io"
                },
                tags=["large_transfer", "exfiltration"]
            ),
            
            # Cleanup
            Event(
                event_id=Event.generate_id(),
                event_type="file_delete",
                timestamp=base_time + timedelta(seconds=7),
                process_name="collector.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "file_path": "C:\\Temp\\data.zip"
                },
                tags=["cleanup", "anti_forensics"]
            )
        ]
        
        return events
    
    def _scenario_lateral_movement(self) -> List[Event]:
        """
        Generate events simulating lateral movement.
        
        Sequence: credential use → remote service access → remote execution
        """
        base_time = datetime.now()
        pid = random.randint(1000, 9999)
        target_host = "WORKSTATION02"
        
        events = [
            # WMI remote connection
            Event(
                event_id=Event.generate_id(),
                event_type="network_connection",
                timestamp=base_time,
                process_name="wmiprvse.exe",
                process_id=pid,
                user="admin_user",
                details={
                    "direction": "outbound",
                    "remote_host": target_host,
                    "remote_port": 135,
                    "protocol": "TCP",
                    "service": "WMI"
                },
                tags=["remote_connection", "wmi"]
            ),
            
            # PsExec-style remote execution
            Event(
                event_id=Event.generate_id(),
                event_type="process_spawn",
                timestamp=base_time + timedelta(seconds=2),
                process_name="psexec.exe",
                process_id=pid + 1,
                parent_process_id=pid,
                user="admin_user",
                details={
                    "command_line": f"psexec.exe \\\\{target_host} -s cmd.exe",
                    "target_host": target_host,
                    "remote_execution": True
                },
                tags=["remote_execution", "psexec"]
            ),
            
            # SMB file copy
            Event(
                event_id=Event.generate_id(),
                event_type="file_write",
                timestamp=base_time + timedelta(seconds=3),
                process_name="cmd.exe",
                process_id=pid + 2,
                user="admin_user",
                details={
                    "file_path": f"\\\\{target_host}\\C$\\Windows\\Temp\\payload.exe",
                    "size_bytes": 45056,
                    "is_remote": True
                },
                tags=["remote_file_copy", "lateral_movement"]
            ),
            
            # Remote service creation
            Event(
                event_id=Event.generate_id(),
                event_type="service_install",
                timestamp=base_time + timedelta(seconds=5),
                process_name="services.exe",
                process_id=pid + 3,
                user="SYSTEM",
                details={
                    "service_name": "RemoteUpdate",
                    "binary_path": "C:\\Windows\\Temp\\payload.exe",
                    "host": target_host,
                    "is_remote": True
                },
                tags=["remote_service", "lateral_movement"]
            )
        ]
        
        return events
    
    def _scenario_defense_evasion(self) -> List[Event]:
        """
        Generate events simulating defense evasion techniques.
        
        Sequence: security tool tampering → log deletion → process hollowing
        """
        base_time = datetime.now()
        pid = random.randint(1000, 9999)
        
        events = [
            # Disable Windows Defender
            Event(
                event_id=Event.generate_id(),
                event_type="registry_write",
                timestamp=base_time,
                process_name="malware.exe",
                process_id=pid,
                user="SYSTEM",
                details={
                    "key": "HKLM\\SOFTWARE\\Policies\\Microsoft\\Windows Defender",
                    "value_name": "DisableAntiSpyware",
                    "value_data": 1,
                    "value_type": "REG_DWORD"
                },
                tags=["security_tampering", "defense_evasion"]
            ),
            
            # Stop security service
            Event(
                event_id=Event.generate_id(),
                event_type="service_stop",
                timestamp=base_time + timedelta(seconds=1),
                process_name="net.exe",
                process_id=pid + 1,
                parent_process_id=pid,
                user="SYSTEM",
                details={
                    "service_name": "WinDefend",
                    "command": "net stop WinDefend"
                },
                tags=["security_tampering", "service_stop"]
            ),
            
            # Clear event logs
            Event(
                event_id=Event.generate_id(),
                event_type="log_clear",
                timestamp=base_time + timedelta(seconds=3),
                process_name="wevtutil.exe",
                process_id=pid + 2,
                parent_process_id=pid,
                user="SYSTEM",
                details={
                    "log_name": "Security",
                    "command": "wevtutil cl Security"
                },
                tags=["log_deletion", "anti_forensics"]
            ),
            
            Event(
                event_id=Event.generate_id(),
                event_type="log_clear",
                timestamp=base_time + timedelta(seconds=4),
                process_name="wevtutil.exe",
                process_id=pid + 3,
                parent_process_id=pid,
                user="SYSTEM",
                details={
                    "log_name": "System",
                    "command": "wevtutil cl System"
                },
                tags=["log_deletion", "anti_forensics"]
            ),
            
            # Process injection/hollowing
            Event(
                event_id=Event.generate_id(),
                event_type="process_injection",
                timestamp=base_time + timedelta(seconds=5),
                process_name="malware.exe",
                process_id=pid,
                user="victim_user",
                details={
                    "target_process": "svchost.exe",
                    "target_pid": 1234,
                    "injection_type": "process_hollowing",
                    "payload_size": 32768
                },
                tags=["process_injection", "defense_evasion"]
            )
        ]
        
        return events


# Factory function
def create_threat_simulator(
    pipeline: Optional[EventPipelineInterface] = None
) -> ThreatSimulator:
    """
    Factory function to create a configured ThreatSimulator.
    
    Args:
        pipeline: Optional EventPipeline to inject events into.
    
    Returns:
        Configured ThreatSimulator instance.
    """
    return ThreatSimulator(pipeline=pipeline)


# Example usage and demonstration
if __name__ == "__main__":
    print("=" * 60)
    print("GreyAV Threat Simulation Engine")
    print("=" * 60)
    
    # Create simulator
    simulator = create_threat_simulator()
    
    # List available scenarios
    print("\nAvailable Scenarios:")
    for name in simulator.list_scenarios():
        info = simulator.get_scenario_info(name)
        print(f"  - {name}: {info['description']}")
    
    # Run all scenarios
    print("\n" + "-" * 60)
    print("Running all scenarios...")
    print("-" * 60)
    
    reports = simulator.run_all()
    
    for report in reports:
        print(f"\n{report.summary()}")
        if report.events_generated:
            print(f"  Events: {len(report.events_generated)}")
            for event in report.events_generated[:3]:  # Show first 3
                print(f"    - {event.event_type}: {event.process_name}")
            if len(report.events_generated) > 3:
                print(f"    ... and {len(report.events_generated) - 3} more")
    
    # Generate summary
    print("\n" + "=" * 60)
    print("Simulation Summary")
    print("=" * 60)
    summary = simulator.generate_summary()
    print(f"Total: {summary['total']}")
    print(f"Passed: {summary['passed']}")
    print(f"Failed: {summary['failed']}")
    print(f"Pass Rate: {summary['pass_rate']:.1f}%")
    
    if summary['gaps']:
        print("\nDetection Gaps:")
        for gap in summary['gaps'][:5]:
            print(f"  - {gap}")
