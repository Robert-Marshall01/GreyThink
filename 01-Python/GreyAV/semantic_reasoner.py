"""
Semantic Threat Reasoning Engine for GreyAV EDR System.

This module translates technical security events into human-readable
narratives, chains related events into coherent attack stories, and
infers attacker goals based on behavioral patterns.
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Optional
import uuid


@dataclass
class Event:
    """
    Represents a system event to be translated and analyzed.
    
    Attributes:
        event_id: Unique identifier for this event.
        event_type: Type of event (e.g., 'file_write', 'process_spawn').
        category: Event category (e.g., 'execution', 'persistence', 'exfiltration').
        process_id: Process ID that generated the event.
        process_name: Name of the process.
        source: Source entity (file, process, user).
        target: Target entity affected by the event.
        severity: Severity score (0.0 to 1.0).
        timestamp: When the event occurred.
        metadata: Additional event-specific data.
    """
    event_id: str
    event_type: str
    category: str
    process_id: int
    process_name: str
    source: str = ""
    target: str = ""
    severity: float = 0.5
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: dict = field(default_factory=dict)

    @staticmethod
    def generate_id() -> str:
        """Generate a unique event ID."""
        return f"EVT-{uuid.uuid4().hex[:12].upper()}"


@dataclass
class CorrelatedAlert:
    """
    Represents a correlated alert from the detection engine.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        process_id: Primary process involved.
        events: List of correlated events.
        correlation_score: Confidence score of the correlation.
        alert_type: Type of alert.
        timestamp: When the alert was generated.
    """
    alert_id: str
    process_id: int
    events: list[Event]
    correlation_score: float
    alert_type: str
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class ThreatNarrative:
    """
    Human-readable narrative summarizing an attack or threat.
    
    ThreatNarratives translate complex technical events into coherent
    stories that explain what happened, how it happened, and what the
    attacker was likely trying to achieve.
    
    Attributes:
        narrative_id: Unique identifier for this narrative.
        process_id: Primary process involved in the attack.
        events: List of events that comprise the narrative.
        inferred_goal: The likely attacker goal.
        description: Full narrative description in plain language.
        timeline: List of human-readable steps describing the attack.
        timestamp: When the narrative was generated.
    """
    narrative_id: str
    process_id: int
    events: list[Event]
    inferred_goal: str
    description: str
    timeline: list[str]
    timestamp: datetime = field(default_factory=datetime.now)

    @staticmethod
    def generate_id() -> str:
        """Generate a unique narrative ID."""
        return f"TN-{uuid.uuid4().hex[:12].upper()}"


class SemanticReasoner:
    """
    Semantic Threat Reasoning Engine.
    
    Translates technical security events into human-readable narratives,
    chains related events into attack stories, and infers attacker goals
    based on observed behavioral patterns.
    """

    # Event type to human-readable phrase mappings
    EVENT_TRANSLATIONS: dict[str, str] = {
        # Process events
        'process_spawn': 'spawned a new process',
        'process_inject': 'injected code into another process',
        'process_terminate': 'terminated a process',
        'process_hollow': 'performed process hollowing',
        
        # File events
        'file_create': 'created a new file',
        'file_write': 'wrote data to a file',
        'file_read': 'read data from a file',
        'file_delete': 'deleted a file',
        'file_modify': 'modified a file',
        'file_rename': 'renamed a file',
        'file_encrypt': 'encrypted a file',
        
        # Registry events (Windows)
        'registry_write': 'modified a registry key',
        'registry_create': 'created a new registry key',
        'registry_delete': 'deleted a registry key',
        
        # Network events
        'network_connection': 'established a network connection',
        'network_listen': 'started listening on a network port',
        'dns_query': 'performed a DNS lookup',
        'http_request': 'made an HTTP request',
        'data_upload': 'uploaded data to a remote server',
        'data_download': 'downloaded data from a remote server',
        
        # Authentication events
        'login_attempt': 'attempted to authenticate',
        'login_success': 'successfully authenticated',
        'login_failure': 'failed to authenticate',
        'privilege_escalation': 'escalated privileges',
        'credential_access': 'accessed stored credentials',
        
        # System events
        'service_create': 'created a new system service',
        'service_modify': 'modified a system service',
        'scheduled_task_create': 'created a scheduled task',
        'driver_load': 'loaded a kernel driver',
        
        # Defense evasion
        'log_clear': 'cleared system logs',
        'security_disable': 'disabled security software',
        'obfuscation': 'used obfuscation techniques',
    }

    # Category to goal inference mappings
    CATEGORY_GOALS: dict[str, str] = {
        'persistence': 'establishing persistence',
        'exfiltration': 'exfiltrating data',
        'privilege_escalation': 'escalating privileges',
        'lateral_movement': 'moving laterally through the network',
        'execution': 'executing malicious code',
        'defense_evasion': 'evading detection',
        'credential_access': 'stealing credentials',
        'discovery': 'reconnaissance and discovery',
        'collection': 'collecting sensitive data',
        'command_and_control': 'establishing command and control',
        'impact': 'causing damage or disruption',
    }

    # Pattern-based goal inference rules
    GOAL_PATTERNS: dict[str, list[str]] = {
        'persistence': [
            'registry_write', 'registry_create', 'service_create',
            'scheduled_task_create', 'startup_modification',
        ],
        'exfiltration': [
            'data_upload', 'network_connection', 'file_read',
            'archive_create', 'dns_exfil',
        ],
        'privilege_escalation': [
            'privilege_escalation', 'token_manipulation', 'driver_load',
            'credential_access', 'sudo_abuse',
        ],
        'ransomware': [
            'file_encrypt', 'file_rename', 'mass_deletion',
            'shadow_copy_delete', 'ransom_note',
        ],
        'credential_theft': [
            'credential_access', 'password_dump', 'keylogger',
            'browser_credential_access', 'lsass_access',
        ],
        'lateral_movement': [
            'remote_execution', 'smb_connection', 'wmi_execution',
            'psexec', 'ssh_connection',
        ],
        'reconnaissance': [
            'network_scan', 'port_scan', 'user_enumeration',
            'file_enumeration', 'system_discovery',
        ],
    }

    def __init__(self) -> None:
        """Initialize the semantic reasoner."""
        self._narrative_subscribers: list[Callable[[ThreatNarrative], None]] = []
        self._custom_translations: dict[str, str] = {}
        self._narrative_history: list[ThreatNarrative] = []

    def translate_event(self, event: Event) -> str:
        """
        Translate a technical event into a human-readable description.
        
        Args:
            event: The event to translate.
            
        Returns:
            Human-readable description of the event.
        """
        # Check custom translations first
        if event.event_type in self._custom_translations:
            base_phrase = self._custom_translations[event.event_type]
        else:
            base_phrase = self.EVENT_TRANSLATIONS.get(
                event.event_type, 
                f"performed {event.event_type}"
            )
        
        # Build the full description
        timestamp_str = event.timestamp.strftime("%H:%M:%S")
        
        description = f"[{timestamp_str}] Process '{event.process_name}' (PID: {event.process_id}) {base_phrase}"
        
        # Add target information if available
        if event.target:
            description += f" targeting '{event.target}'"
        
        # Add source information if relevant
        if event.source and event.source != event.process_name:
            description += f" from '{event.source}'"
        
        # Add severity indicator for high-severity events
        if event.severity >= 0.8:
            description += " [HIGH SEVERITY]"
        elif event.severity >= 0.6:
            description += " [ELEVATED]"
        
        return description

    def add_translation(self, event_type: str, phrase: str) -> None:
        """
        Add a custom event type translation.
        
        Args:
            event_type: Event type to translate.
            phrase: Human-readable phrase for this event type.
        """
        self._custom_translations[event_type] = phrase

    def infer_goal(self, events: list[Event]) -> str:
        """
        Infer the attacker's likely goal based on observed events.
        
        Uses pattern matching against known attack patterns to determine
        the most likely attacker objective.
        
        Args:
            events: List of events to analyze.
            
        Returns:
            String describing the inferred attacker goal.
        """
        if not events:
            return "unknown objective"
        
        # Collect event types and categories
        event_types = [e.event_type for e in events]
        categories = [e.category for e in events if e.category]
        
        # Score each potential goal
        goal_scores: dict[str, float] = {}
        
        # Pattern-based scoring
        for goal, patterns in self.GOAL_PATTERNS.items():
            score = sum(1 for et in event_types if et in patterns)
            if score > 0:
                goal_scores[goal] = score
        
        # Category-based scoring
        for category in categories:
            if category in self.CATEGORY_GOALS:
                goal = self.CATEGORY_GOALS[category]
                goal_scores[goal] = goal_scores.get(goal, 0) + 1.5
        
        # Severity weighting
        avg_severity = sum(e.severity for e in events) / len(events)
        
        # Find the highest-scoring goal
        if goal_scores:
            best_goal = max(goal_scores, key=goal_scores.get)
            confidence = min(goal_scores[best_goal] / len(events), 1.0)
            
            if confidence >= 0.5:
                return best_goal
            elif confidence >= 0.3:
                return f"possibly {best_goal}"
            else:
                return f"unclear, possibly {best_goal}"
        
        # Fallback inference based on severity
        if avg_severity >= 0.8:
            return "malicious activity (high confidence)"
        elif avg_severity >= 0.5:
            return "suspicious activity (investigating)"
        else:
            return "anomalous behavior (low confidence)"

    def build_narrative(self, events: list[Event]) -> ThreatNarrative:
        """
        Build a coherent threat narrative from a list of events.
        
        Chains events into a timeline, infers the attacker's goal,
        and generates a human-readable description of the attack.
        
        Args:
            events: List of events to build narrative from.
            
        Returns:
            ThreatNarrative object summarizing the attack.
        """
        if not events:
            return ThreatNarrative(
                narrative_id=ThreatNarrative.generate_id(),
                process_id=0,
                events=[],
                inferred_goal="no events to analyze",
                description="No events were provided for narrative generation.",
                timeline=[],
                timestamp=datetime.now()
            )
        
        # Sort events by timestamp
        sorted_events = sorted(events, key=lambda e: e.timestamp)
        
        # Get primary process (most common or first)
        process_counts: dict[int, int] = {}
        for event in sorted_events:
            process_counts[event.process_id] = process_counts.get(event.process_id, 0) + 1
        primary_pid = max(process_counts, key=process_counts.get)
        
        # Build timeline
        timeline = [self.translate_event(e) for e in sorted_events]
        
        # Infer goal
        inferred_goal = self.infer_goal(sorted_events)
        
        # Generate narrative description
        description = self._generate_description(sorted_events, inferred_goal)
        
        narrative = ThreatNarrative(
            narrative_id=ThreatNarrative.generate_id(),
            process_id=primary_pid,
            events=sorted_events,
            inferred_goal=inferred_goal,
            description=description,
            timeline=timeline,
            timestamp=datetime.now()
        )
        
        # Store in history and notify subscribers
        self._narrative_history.append(narrative)
        self._notify_subscribers(narrative)
        
        return narrative

    def _generate_description(
        self, 
        events: list[Event], 
        inferred_goal: str
    ) -> str:
        """
        Generate a full narrative description.
        
        Args:
            events: Sorted list of events.
            inferred_goal: The inferred attacker goal.
            
        Returns:
            Full narrative description string.
        """
        if not events:
            return "No activity detected."
        
        first_event = events[0]
        last_event = events[-1]
        
        # Calculate duration
        duration = (last_event.timestamp - first_event.timestamp).total_seconds()
        
        # Count unique processes and targets
        unique_processes = {e.process_name for e in events}
        unique_targets = {e.target for e in events if e.target}
        
        # Get severity assessment
        max_severity = max(e.severity for e in events)
        avg_severity = sum(e.severity for e in events) / len(events)
        
        severity_label = (
            "CRITICAL" if max_severity >= 0.9 else
            "HIGH" if max_severity >= 0.7 else
            "MEDIUM" if max_severity >= 0.4 else
            "LOW"
        )
        
        # Build description
        parts = [
            f"**Threat Narrative Summary**\n",
            f"Severity: {severity_label} (max: {max_severity:.2f}, avg: {avg_severity:.2f})\n",
            f"Timeframe: {first_event.timestamp.strftime('%Y-%m-%d %H:%M:%S')} to "
            f"{last_event.timestamp.strftime('%H:%M:%S')} ({duration:.1f} seconds)\n\n",
            f"**Inferred Goal:** {inferred_goal.title()}\n\n",
            f"**Summary:**\n",
        ]
        
        # Generate summary based on goal
        if 'ransomware' in inferred_goal.lower():
            parts.append(
                f"A potential ransomware attack was detected involving "
                f"{len(unique_processes)} process(es) affecting {len(unique_targets)} target(s). "
                f"The attack sequence shows file encryption and modification patterns "
                f"consistent with ransomware behavior. Immediate containment recommended."
            )
        elif 'exfiltration' in inferred_goal.lower():
            parts.append(
                f"Data exfiltration activity was detected. {len(events)} events show "
                f"a pattern of data collection followed by network transfer. "
                f"The attacker appears to be extracting sensitive information from the system."
            )
        elif 'persistence' in inferred_goal.lower():
            parts.append(
                f"Persistence mechanism establishment detected. The attacker is attempting "
                f"to maintain access to the system through {len(events)} observed actions. "
                f"Registry modifications and service creation patterns were identified."
            )
        elif 'privilege' in inferred_goal.lower():
            parts.append(
                f"Privilege escalation attempt detected. The attacker executed "
                f"{len(events)} actions attempting to gain elevated access. "
                f"Immediate investigation of affected user accounts recommended."
            )
        elif 'lateral' in inferred_goal.lower():
            parts.append(
                f"Lateral movement activity detected across the network. "
                f"The attacker is attempting to spread to additional systems "
                f"from the initially compromised host."
            )
        elif 'credential' in inferred_goal.lower():
            parts.append(
                f"Credential theft activity detected. The attacker is attempting "
                f"to harvest authentication credentials from {len(unique_targets)} location(s). "
                f"Password reset for affected accounts recommended."
            )
        else:
            parts.append(
                f"Suspicious activity detected involving {len(unique_processes)} process(es) "
                f"and {len(unique_targets)} target(s) over {len(events)} events. "
                f"The activity pattern suggests {inferred_goal}. "
                f"Further investigation recommended."
            )
        
        parts.append(f"\n\n**Processes Involved:** {', '.join(unique_processes)}")
        
        if unique_targets:
            targets_display = list(unique_targets)[:5]
            if len(unique_targets) > 5:
                targets_display.append(f"... and {len(unique_targets) - 5} more")
            parts.append(f"\n**Targets Affected:** {', '.join(targets_display)}")
        
        return ''.join(parts)

    def handle_alert(self, alert: CorrelatedAlert) -> ThreatNarrative:
        """
        Handle a correlated alert and generate a threat narrative.
        
        Takes an alert from the correlation engine and enriches it
        with a human-readable narrative explanation.
        
        Args:
            alert: The correlated alert to process.
            
        Returns:
            ThreatNarrative enriching the alert with narrative.
        """
        narrative = self.build_narrative(alert.events)
        
        # Enrich with alert-specific information
        enriched_description = (
            f"**Alert Reference:** {alert.alert_id}\n"
            f"**Alert Type:** {alert.alert_type}\n"
            f"**Correlation Score:** {alert.correlation_score:.2f}\n\n"
            f"{narrative.description}"
        )
        
        return ThreatNarrative(
            narrative_id=narrative.narrative_id,
            process_id=alert.process_id,
            events=narrative.events,
            inferred_goal=narrative.inferred_goal,
            description=enriched_description,
            timeline=narrative.timeline,
            timestamp=datetime.now()
        )

    def get_narrative_summary(self, narrative: ThreatNarrative) -> str:
        """
        Get a brief one-line summary of a narrative.
        
        Args:
            narrative: The narrative to summarize.
            
        Returns:
            Brief summary string.
        """
        return (
            f"[{narrative.narrative_id}] "
            f"{len(narrative.events)} events, "
            f"Goal: {narrative.inferred_goal}, "
            f"Process: {narrative.process_id}"
        )

    def subscribe(self, callback: Callable[[ThreatNarrative], None]) -> None:
        """
        Subscribe to threat narrative generation.
        
        Args:
            callback: Function to call when a narrative is generated.
        """
        self._narrative_subscribers.append(callback)

    def unsubscribe(self, callback: Callable[[ThreatNarrative], None]) -> None:
        """
        Unsubscribe from threat narratives.
        
        Args:
            callback: Previously registered callback function.
        """
        if callback in self._narrative_subscribers:
            self._narrative_subscribers.remove(callback)

    def _notify_subscribers(self, narrative: ThreatNarrative) -> None:
        """Notify all subscribers of a new narrative."""
        for callback in self._narrative_subscribers:
            try:
                callback(narrative)
            except Exception as e:
                print(f"[SemanticReasoner] Subscriber notification failed: {e}")

    def get_narrative_history(
        self, 
        limit: int = 100
    ) -> list[ThreatNarrative]:
        """
        Get recent narrative history.
        
        Args:
            limit: Maximum number of narratives to return.
            
        Returns:
            List of recent narratives.
        """
        return self._narrative_history[-limit:]

    def clear_history(self) -> None:
        """Clear the narrative history."""
        self._narrative_history.clear()


# Pipeline integration handler
def create_semantic_handler(
    reasoner: SemanticReasoner
) -> Callable[[CorrelatedAlert], ThreatNarrative]:
    """
    Create an alert handler for pipeline integration.
    
    This factory creates a handler that processes correlated alerts
    through the semantic reasoner.
    
    Args:
        reasoner: The SemanticReasoner instance to use.
        
    Returns:
        Alert handler function compatible with pipeline subscription.
    """
    def handler(alert: CorrelatedAlert) -> ThreatNarrative:
        return reasoner.handle_alert(alert)
    
    return handler


if __name__ == "__main__":
    # Demo usage
    reasoner = SemanticReasoner()
    
    # Subscribe to narratives
    def narrative_handler(narrative: ThreatNarrative) -> None:
        print(f"\n{'='*60}")
        print(f"[THREAT NARRATIVE] {narrative.narrative_id}")
        print(f"{'='*60}")
        print(f"\n{narrative.description}")
        print(f"\n**Timeline:**")
        for step in narrative.timeline:
            print(f"  • {step}")
    
    reasoner.subscribe(narrative_handler)
    
    # Create sample events simulating a ransomware attack
    base_time = datetime.now()
    
    events = [
        Event(
            Event.generate_id(),
            "process_spawn",
            "execution",
            1234,
            "suspicious.exe",
            source="email_attachment.zip",
            target="C:\\Users\\victim\\AppData\\Local\\Temp\\suspicious.exe",
            severity=0.7,
            timestamp=base_time
        ),
        Event(
            Event.generate_id(),
            "file_read",
            "collection",
            1234,
            "suspicious.exe",
            target="C:\\Users\\victim\\Documents\\important.docx",
            severity=0.5,
            timestamp=base_time
        ),
        Event(
            Event.generate_id(),
            "file_encrypt",
            "impact",
            1234,
            "suspicious.exe",
            target="C:\\Users\\victim\\Documents\\important.docx.encrypted",
            severity=0.9,
            timestamp=base_time
        ),
        Event(
            Event.generate_id(),
            "file_encrypt",
            "impact",
            1234,
            "suspicious.exe",
            target="C:\\Users\\victim\\Documents\\financial.xlsx.encrypted",
            severity=0.9,
            timestamp=base_time
        ),
        Event(
            Event.generate_id(),
            "file_create",
            "impact",
            1234,
            "suspicious.exe",
            target="C:\\Users\\victim\\Desktop\\README_DECRYPT.txt",
            severity=0.95,
            timestamp=base_time
        ),
    ]
    
    print("Building threat narrative from events...")
    narrative = reasoner.build_narrative(events)
    
    # Test with a correlated alert
    print("\n\n" + "="*60)
    print("Processing correlated alert...")
    
    alert = CorrelatedAlert(
        alert_id="CA-123456",
        process_id=1234,
        events=events,
        correlation_score=0.95,
        alert_type="ransomware_attack"
    )
    
    enriched_narrative = reasoner.handle_alert(alert)
    print(f"\nEnriched narrative for alert {alert.alert_id}:")
    print(f"Summary: {reasoner.get_narrative_summary(enriched_narrative)}")
    
    # Demonstrate single event translation
    print("\n\n" + "="*60)
    print("Single event translations:")
    for event in events[:3]:
        print(f"  {reasoner.translate_event(event)}")
