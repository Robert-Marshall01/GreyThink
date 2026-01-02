"""
Chaos Immunization Layer for GreyAV EDR System.

This module implements a controlled chaos engineering approach to strengthen
system resilience. By deliberately injecting micro-chaos events, the system
learns to tolerate perturbations and reveals hidden weaknesses before
attackers can exploit them.

Biological Analogy:
Just as vaccines introduce weakened or dead pathogens to train the immune
system without causing disease, the Chaos Immunization Layer introduces
controlled disruptions to train the security system without causing harm.

Key Metaphors:
- ChaosEvent = Vaccine dose / attenuated pathogen
- inject_privilege_flip() = Inoculation with weakened privilege-based threat
- inject_resource_spike() = Stress inoculation (like fever simulation)
- inject_synthetic_anomaly() = Exposure to novel antigen patterns
- run_immunization_cycle() = Immune response training / antibody production
- tolerance_score = Immunity level / antibody titer
- weaknesses = Vulnerable organs / immunocompromised areas

The philosophy mirrors hormesis - the biological principle where exposure
to low doses of stressors builds resilience against larger stressors.
"""

import uuid
import random
import logging
import time
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, Any, List, Optional, Callable, Tuple
from enum import Enum

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


# =============================================================================
# Enums and Constants
# =============================================================================

class ChaosEventType(Enum):
    """Types of chaos events that can be injected."""
    PRIVILEGE_FLIP = "privilege_flip"
    RESOURCE_SPIKE = "resource_spike"
    SYNTHETIC_ANOMALY = "synthetic_anomaly"
    NETWORK_JITTER = "network_jitter"
    FILE_MUTATION = "file_mutation"
    PROCESS_NOISE = "process_noise"
    TIMING_SKEW = "timing_skew"
    MEMORY_PRESSURE = "memory_pressure"


class ResilienceLevel(Enum):
    """Resilience classification based on tolerance score."""
    VULNERABLE = "vulnerable"       # 0.0 - 0.3
    WEAK = "weak"                   # 0.3 - 0.5
    MODERATE = "moderate"           # 0.5 - 0.7
    STRONG = "strong"               # 0.7 - 0.9
    IMMUNIZED = "immunized"         # 0.9 - 1.0


# =============================================================================
# Core Data Structures
# =============================================================================

@dataclass
class ChaosEvent:
    """
    Represents a controlled chaos event injected into the system.
    
    Biological Analogy:
    A ChaosEvent is like a vaccine dose - a carefully crafted, controlled
    exposure to a weakened version of a threat. Just as vaccines contain
    attenuated pathogens that stimulate immunity without causing disease,
    chaos events simulate threats without causing actual harm.
    
    The payload contains the "antigens" - specific characteristics that
    the system must learn to recognize and tolerate.
    
    Attributes:
        id: Unique identifier for this chaos event (like a vaccine lot number).
        type: Category of chaos (like vaccine type - mRNA, viral vector, etc.).
        payload: Details of the chaos injection (like vaccine composition).
        timestamp: When the event was injected (like vaccination date).
        description: Human-readable description of the chaos event.
        severity: Intensity of the chaos (like vaccine dosage).
        target_entity: The system component being tested.
        expected_response: What a healthy system should do.
        actual_response: What the system actually did.
        tolerance_delta: Change in tolerance from this event.
    """
    id: str
    type: str
    payload: Dict[str, Any]
    timestamp: datetime = field(default_factory=datetime.now)
    description: str = ""
    severity: float = 0.5
    target_entity: str = ""
    expected_response: str = ""
    actual_response: str = ""
    tolerance_delta: float = 0.0
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert event to dictionary for serialization."""
        return {
            "id": self.id,
            "type": self.type,
            "payload": self.payload,
            "timestamp": self.timestamp.isoformat(),
            "description": self.description,
            "severity": self.severity,
            "target_entity": self.target_entity,
            "expected_response": self.expected_response,
            "actual_response": self.actual_response,
            "tolerance_delta": self.tolerance_delta,
        }


@dataclass
class Weakness:
    """
    Represents a discovered system weakness.
    
    Biological Analogy:
    A Weakness is like a vulnerable organ or an immunocompromised area.
    Just as certain organs (lungs, liver) may be more susceptible to
    specific pathogens, certain system components may be more vulnerable
    to specific attack patterns.
    
    Attributes:
        id: Unique weakness identifier.
        component: Affected system component (like the vulnerable organ).
        weakness_type: Category of vulnerability.
        severity: How critical this weakness is.
        discovered_at: When the weakness was found.
        chaos_event_id: The chaos event that revealed this weakness.
        description: Details about the weakness.
        remediation: Suggested fix.
    """
    id: str
    component: str
    weakness_type: str
    severity: float
    discovered_at: datetime
    chaos_event_id: str
    description: str
    remediation: str = ""
    is_remediated: bool = False


@dataclass
class ImmunizationResult:
    """
    Result of an immunization cycle.
    
    Biological Analogy:
    Like post-vaccination blood work that measures antibody titers,
    this records the system's immune response to the chaos exposure.
    
    Attributes:
        cycle_id: Unique cycle identifier.
        events_processed: Number of chaos events in this cycle.
        tolerance_before: Tolerance score before cycle.
        tolerance_after: Tolerance score after cycle.
        weaknesses_discovered: New weaknesses found.
        strengthened_areas: Components that improved.
        duration_ms: How long the cycle took.
    """
    cycle_id: str
    events_processed: int
    tolerance_before: float
    tolerance_after: float
    weaknesses_discovered: List[str]
    strengthened_areas: List[str]
    duration_ms: float
    timestamp: datetime = field(default_factory=datetime.now)


# =============================================================================
# Chaos Immunization Layer
# =============================================================================

class ChaosImmunizationLayer:
    """
    Chaos Immunization Layer - Controlled Chaos for System Resilience.
    
    Biological Analogy:
    This class functions like a vaccination program for the security system.
    Just as immunization programs expose individuals to controlled doses of
    pathogens to build immunity, the Chaos Immunization Layer exposes the
    system to controlled disruptions to build resilience.
    
    Key Concepts:
    
    1. HORMESIS: The biological principle where low-dose stressors trigger
       adaptive responses that protect against larger stressors. Like how
       exercise (controlled muscle damage) builds strength, controlled chaos
       builds system resilience.
    
    2. ANTIFRAGILITY: Going beyond resilience - systems that gain from disorder.
       Like bones becoming stronger under stress, the security system becomes
       more robust through chaos exposure.
    
    3. IMMUNOLOGICAL MEMORY: Just as the immune system remembers past exposures,
       the chaos layer records what worked and what failed, building a library
       of known vulnerabilities and tolerances.
    
    4. VACCINE SCHEDULE: Like childhood vaccination schedules, the chaos layer
       can follow a program of graduated exposures to build comprehensive
       immunity.
    
    Attributes:
        events: History of all chaos events (like vaccination records).
        tolerance_score: Current immunity level (like antibody titer).
        weaknesses: Discovered vulnerabilities (like immunodeficiencies).
    """
    
    # Default tolerance thresholds
    INITIAL_TOLERANCE: float = 0.5
    MAX_TOLERANCE: float = 1.0
    MIN_TOLERANCE: float = 0.0
    
    # Chaos severity ranges
    MICRO_CHAOS_SEVERITY: float = 0.2
    MILD_CHAOS_SEVERITY: float = 0.4
    MODERATE_CHAOS_SEVERITY: float = 0.6
    STRONG_CHAOS_SEVERITY: float = 0.8
    
    def __init__(self, initial_tolerance: float = 0.5):
        """
        Initialize the Chaos Immunization Layer.
        
        Biological Analogy:
        Like initializing a newborn's immune system - starting with some
        innate immunity but requiring exposure to build adaptive immunity.
        
        Args:
            initial_tolerance: Starting tolerance score (baseline immunity).
        """
        self.events: List[ChaosEvent] = []
        self.tolerance_score: float = max(
            self.MIN_TOLERANCE,
            min(self.MAX_TOLERANCE, initial_tolerance)
        )
        self.weaknesses: List[str] = []
        
        # Internal tracking
        self._weakness_registry: Dict[str, Weakness] = {}
        self._component_tolerance: Dict[str, float] = {}
        self._pending_events: List[ChaosEvent] = []
        self._cycle_count: int = 0
        self._immunization_history: List[ImmunizationResult] = []
        self._subscribers: List[Callable[[ChaosEvent], None]] = []
        
        logger.info(
            f"ChaosImmunizationLayer initialized with tolerance: {self.tolerance_score:.2f}"
        )
    
    def subscribe(self, callback: Callable[[ChaosEvent], None]) -> None:
        """Subscribe to chaos event notifications."""
        self._subscribers.append(callback)
    
    def _notify_subscribers(self, event: ChaosEvent) -> None:
        """Notify subscribers of a new chaos event."""
        for callback in self._subscribers:
            try:
                callback(event)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def _generate_event_id(self) -> str:
        """Generate a unique event ID."""
        return f"chaos-{uuid.uuid4().hex[:8]}"
    
    def inject_privilege_flip(self, entity: str) -> ChaosEvent:
        """
        Inject a privilege flip chaos event.
        
        Biological Analogy:
        This is like a vaccine dose that introduces a weakened form of a
        privilege-based threat. Just as a tetanus vaccine contains inactivated
        tetanus toxin that teaches the immune system to recognize the toxin
        without causing lockjaw, a privilege flip teaches the system to
        handle unexpected privilege changes without being compromised.
        
        The "privilege flip" temporarily grants or revokes permissions to test
        whether the system properly enforces authorization checks.
        
        Digital Action:
        - Temporarily escalate or de-escalate an entity's privileges
        - Monitor if security controls detect the anomaly
        - Check if the system gracefully handles the perturbation
        
        Args:
            entity: The user, process, or component to target.
            
        Returns:
            The injected ChaosEvent.
        """
        # Determine flip direction
        flip_direction = random.choice(["escalate", "de-escalate"])
        original_privileges = self._simulate_get_privileges(entity)
        new_privileges = self._simulate_flip_privileges(original_privileges, flip_direction)
        
        event = ChaosEvent(
            id=self._generate_event_id(),
            type=ChaosEventType.PRIVILEGE_FLIP.value,
            payload={
                "entity": entity,
                "flip_direction": flip_direction,
                "original_privileges": original_privileges,
                "new_privileges": new_privileges,
                "duration_ms": random.randint(100, 1000),
            },
            description=(
                f"Privilege flip ({flip_direction}) on {entity}: "
                f"Testing authorization boundary enforcement"
            ),
            severity=self.MILD_CHAOS_SEVERITY,
            target_entity=entity,
            expected_response="Detect anomaly, log alert, revert if unauthorized",
        )
        
        # Add to pending events for next immunization cycle
        self._pending_events.append(event)
        self.events.append(event)
        
        logger.info(
            f"ChaosImmunizationLayer.inject_privilege_flip: "
            f"Injected {flip_direction} for {entity}"
        )
        
        self._notify_subscribers(event)
        
        return event
    
    def _simulate_get_privileges(self, entity: str) -> List[str]:
        """Simulate getting current privileges for an entity."""
        # Simulated privilege sets
        privilege_sets = {
            "user": ["read", "write"],
            "admin": ["read", "write", "delete", "admin"],
            "service": ["read", "execute"],
            "system": ["read", "write", "delete", "admin", "system"],
        }
        
        # Determine entity type from name
        if "admin" in entity.lower():
            return privilege_sets["admin"]
        elif "service" in entity.lower() or "svc" in entity.lower():
            return privilege_sets["service"]
        elif "system" in entity.lower() or "root" in entity.lower():
            return privilege_sets["system"]
        else:
            return privilege_sets["user"]
    
    def _simulate_flip_privileges(
        self, 
        privileges: List[str], 
        direction: str
    ) -> List[str]:
        """Simulate flipping privileges."""
        if direction == "escalate":
            # Add elevated privileges
            elevated = ["admin", "system", "delete"]
            return list(set(privileges + elevated[:random.randint(1, 2)]))
        else:
            # Remove privileges
            if len(privileges) > 1:
                return privileges[:-1]
            return privileges
    
    def inject_resource_spike(self, entity: str, cpu: int, memory: int) -> ChaosEvent:
        """
        Inject a resource spike chaos event.
        
        Biological Analogy:
        This is like stress inoculation - similar to how controlled fever
        (hyperthermia therapy) can strengthen immune response, or how
        altitude training stresses the body to produce more red blood cells.
        
        A resource spike temporarily increases resource consumption to test
        whether the system can handle stress gracefully. Just as the body
        must maintain homeostasis during a fever, the system must maintain
        security during resource pressure.
        
        Digital Action:
        - Temporarily spike CPU and memory usage for an entity
        - Monitor if security monitoring remains effective under load
        - Check if resource exhaustion could be used for DoS or evasion
        
        Args:
            entity: The process or component to spike.
            cpu: CPU percentage to spike (0-100).
            memory: Memory MB to allocate.
            
        Returns:
            The injected ChaosEvent.
        """
        # Normalize inputs
        cpu = max(0, min(100, cpu))
        memory = max(0, memory)
        
        # Calculate severity based on resource levels
        severity = (cpu / 100 * 0.5) + (min(memory, 1024) / 1024 * 0.5)
        severity = min(self.STRONG_CHAOS_SEVERITY, severity)
        
        event = ChaosEvent(
            id=self._generate_event_id(),
            type=ChaosEventType.RESOURCE_SPIKE.value,
            payload={
                "entity": entity,
                "cpu_percent": cpu,
                "memory_mb": memory,
                "duration_ms": random.randint(500, 3000),
                "spike_pattern": random.choice(["sudden", "gradual", "oscillating"]),
            },
            description=(
                f"Resource spike on {entity}: "
                f"CPU={cpu}%, Memory={memory}MB - Testing stress resilience"
            ),
            severity=severity,
            target_entity=entity,
            expected_response="Maintain monitoring, alert on threshold breach",
        )
        
        self._pending_events.append(event)
        self.events.append(event)
        
        logger.info(
            f"ChaosImmunizationLayer.inject_resource_spike: "
            f"Injected CPU={cpu}%, MEM={memory}MB for {entity}"
        )
        
        self._notify_subscribers(event)
        
        return event
    
    def inject_synthetic_anomaly(self, entity: str, anomaly: str) -> ChaosEvent:
        """
        Inject a synthetic anomaly chaos event.
        
        Biological Analogy:
        This is like exposing the immune system to a novel antigen pattern -
        a pathogen variant the system hasn't encountered before. Just as
        flu vaccines include predicted strain variants to prepare for
        seasonal flu, synthetic anomalies prepare the system for novel
        attack patterns.
        
        This tests the system's ability to detect and respond to behaviors
        it hasn't been explicitly trained on, measuring its adaptability
        and pattern recognition capabilities.
        
        Digital Action:
        - Inject unusual but not overtly malicious behavior
        - Test anomaly detection sensitivity
        - Verify the system can distinguish nuance, not just known signatures
        
        Args:
            entity: The component to exhibit the anomaly.
            anomaly: Description of the anomalous behavior.
            
        Returns:
            The injected ChaosEvent.
        """
        # Generate anomaly characteristics
        anomaly_patterns = self._generate_anomaly_patterns(anomaly)
        
        event = ChaosEvent(
            id=self._generate_event_id(),
            type=ChaosEventType.SYNTHETIC_ANOMALY.value,
            payload={
                "entity": entity,
                "anomaly_description": anomaly,
                "patterns": anomaly_patterns,
                "deviation_score": random.uniform(0.3, 0.9),
                "baseline_comparison": self._get_baseline_behavior(entity),
            },
            description=(
                f"Synthetic anomaly on {entity}: {anomaly} - "
                f"Testing adaptive detection capabilities"
            ),
            severity=self.MODERATE_CHAOS_SEVERITY,
            target_entity=entity,
            expected_response="Detect deviation, investigate, adapt detection",
        )
        
        self._pending_events.append(event)
        self.events.append(event)
        
        logger.info(
            f"ChaosImmunizationLayer.inject_synthetic_anomaly: "
            f"Injected '{anomaly}' for {entity}"
        )
        
        self._notify_subscribers(event)
        
        return event
    
    def _generate_anomaly_patterns(self, anomaly: str) -> Dict[str, Any]:
        """Generate synthetic anomaly patterns."""
        return {
            "behavioral_deviation": random.uniform(0.2, 0.8),
            "timing_irregularity": random.uniform(0.1, 0.5),
            "sequence_disruption": random.choice([True, False]),
            "data_entropy_shift": random.uniform(-0.3, 0.3),
            "anomaly_signature": anomaly[:20].replace(" ", "_").lower(),
        }
    
    def _get_baseline_behavior(self, entity: str) -> Dict[str, Any]:
        """Get baseline behavior for comparison."""
        return {
            "avg_cpu": random.uniform(5, 30),
            "avg_memory": random.randint(50, 200),
            "io_pattern": "regular",
            "network_activity": "low",
            "process_count": random.randint(1, 10),
        }
    
    def inject_network_jitter(self, entity: str, latency_ms: int) -> ChaosEvent:
        """
        Inject network jitter chaos event.
        
        Biological Analogy:
        Like testing nerve conduction velocity - introducing delays to see
        if the organism can still coordinate effectively. Tests whether
        security monitoring handles network timing variations.
        
        Args:
            entity: Network component to affect.
            latency_ms: Artificial latency to introduce.
            
        Returns:
            The injected ChaosEvent.
        """
        event = ChaosEvent(
            id=self._generate_event_id(),
            type=ChaosEventType.NETWORK_JITTER.value,
            payload={
                "entity": entity,
                "latency_ms": latency_ms,
                "jitter_variance": random.uniform(0.1, 0.5),
                "packet_loss_percent": random.uniform(0, 5),
            },
            description=f"Network jitter on {entity}: {latency_ms}ms latency",
            severity=self.MICRO_CHAOS_SEVERITY,
            target_entity=entity,
            expected_response="Handle gracefully, maintain connectivity checks",
        )
        
        self._pending_events.append(event)
        self.events.append(event)
        self._notify_subscribers(event)
        
        return event
    
    def inject_timing_skew(self, entity: str, skew_ms: int) -> ChaosEvent:
        """
        Inject timing skew chaos event.
        
        Biological Analogy:
        Like disrupting circadian rhythms - testing if the system can handle
        temporal desynchronization, similar to jet lag effects on the body.
        
        Args:
            entity: Component to desynchronize.
            skew_ms: Time skew in milliseconds.
            
        Returns:
            The injected ChaosEvent.
        """
        event = ChaosEvent(
            id=self._generate_event_id(),
            type=ChaosEventType.TIMING_SKEW.value,
            payload={
                "entity": entity,
                "skew_ms": skew_ms,
                "direction": random.choice(["forward", "backward"]),
                "affects_logs": True,
                "affects_tokens": random.choice([True, False]),
            },
            description=f"Timing skew on {entity}: {skew_ms}ms offset",
            severity=self.MILD_CHAOS_SEVERITY,
            target_entity=entity,
            expected_response="Detect time anomaly, sync correction",
        )
        
        self._pending_events.append(event)
        self.events.append(event)
        self._notify_subscribers(event)
        
        return event
    
    def run_immunization_cycle(self) -> Dict[str, Any]:
        """
        Run an immunization cycle to process pending chaos events.
        
        Biological Analogy:
        This is like the immune response training that occurs after vaccination.
        When you receive a vaccine, your immune system:
        
        1. RECOGNITION: Dendritic cells capture the antigen and present it
        2. ACTIVATION: T cells recognize the antigen and activate B cells
        3. RESPONSE: B cells produce antibodies; memory cells are formed
        4. RESOLUTION: The response subsides, leaving lasting immunity
        
        Similarly, the immunization cycle:
        1. Processes each chaos event
        2. Evaluates system response
        3. Updates tolerance scores
        4. Records any weaknesses discovered
        5. Strengthens defenses based on learnings
        
        Returns:
            Dictionary with cycle results including tolerance changes and
            discovered weaknesses.
        """
        self._cycle_count += 1
        start_time = time.time()
        
        tolerance_before = self.tolerance_score
        events_in_cycle = list(self._pending_events)
        weaknesses_discovered: List[str] = []
        strengthened_areas: List[str] = []
        
        logger.info(
            f"ChaosImmunizationLayer.run_immunization_cycle: "
            f"Starting cycle {self._cycle_count} with {len(events_in_cycle)} events"
        )
        
        # Process each pending chaos event
        for event in events_in_cycle:
            result = self._process_chaos_event(event)
            
            # Update event with actual response
            event.actual_response = result["response"]
            event.tolerance_delta = result["tolerance_delta"]
            
            # Check for weaknesses
            if result["weakness_detected"]:
                weakness = result["weakness"]
                self._register_weakness(weakness, event)
                weaknesses_discovered.append(weakness["description"])
                self.weaknesses.append(weakness["description"])
            
            # Check for strengthening
            if result["strengthened"]:
                strengthened_areas.append(event.target_entity)
                self._update_component_tolerance(event.target_entity, 0.05)
            
            # Update global tolerance
            self.tolerance_score = max(
                self.MIN_TOLERANCE,
                min(self.MAX_TOLERANCE, self.tolerance_score + result["tolerance_delta"])
            )
        
        # Clear pending events
        self._pending_events.clear()
        
        duration_ms = (time.time() - start_time) * 1000
        
        # Create result
        result = ImmunizationResult(
            cycle_id=f"cycle-{self._cycle_count}",
            events_processed=len(events_in_cycle),
            tolerance_before=tolerance_before,
            tolerance_after=self.tolerance_score,
            weaknesses_discovered=weaknesses_discovered,
            strengthened_areas=list(set(strengthened_areas)),
            duration_ms=duration_ms,
        )
        
        self._immunization_history.append(result)
        
        logger.info(
            f"ChaosImmunizationLayer.run_immunization_cycle: "
            f"Completed - tolerance: {tolerance_before:.2f} -> {self.tolerance_score:.2f}, "
            f"weaknesses: {len(weaknesses_discovered)}, strengthened: {len(strengthened_areas)}"
        )
        
        return {
            "cycle_id": result.cycle_id,
            "events_processed": result.events_processed,
            "tolerance_before": result.tolerance_before,
            "tolerance_after": result.tolerance_after,
            "tolerance_change": result.tolerance_after - result.tolerance_before,
            "weaknesses_discovered": result.weaknesses_discovered,
            "strengthened_areas": result.strengthened_areas,
            "duration_ms": result.duration_ms,
            "resilience_level": self._get_resilience_level().value,
        }
    
    def _process_chaos_event(self, event: ChaosEvent) -> Dict[str, Any]:
        """
        Process a single chaos event and evaluate system response.
        
        Biological Analogy:
        Like evaluating the immune response to a specific antigen -
        measuring antibody production, T cell activation, and whether
        the pathogen was successfully neutralized.
        """
        # Simulate system response (in production, would involve actual testing)
        response_quality = self._simulate_response_quality(event)
        
        # Determine if weakness was exposed
        weakness_detected = response_quality < 0.6
        weakness = None
        
        if weakness_detected:
            weakness = {
                "component": event.target_entity,
                "type": event.type,
                "severity": event.severity * (1 - response_quality),
                "description": f"Weakness in {event.target_entity}: "
                              f"Poor response to {event.type} chaos event",
            }
        
        # Calculate tolerance delta
        # Good responses increase tolerance; poor responses decrease it
        if response_quality >= 0.8:
            tolerance_delta = 0.02 * event.severity
            strengthened = True
        elif response_quality >= 0.6:
            tolerance_delta = 0.01 * event.severity
            strengthened = True
        else:
            tolerance_delta = -0.02 * event.severity
            strengthened = False
        
        return {
            "event_id": event.id,
            "response_quality": response_quality,
            "response": self._generate_response_description(event, response_quality),
            "tolerance_delta": tolerance_delta,
            "weakness_detected": weakness_detected,
            "weakness": weakness,
            "strengthened": strengthened,
        }
    
    def _simulate_response_quality(self, event: ChaosEvent) -> float:
        """
        Simulate the quality of system response to a chaos event.
        
        In a production system, this would actually test the response.
        Here we simulate based on event type and current tolerance.
        """
        # Base quality influenced by current tolerance
        base_quality = 0.5 + (self.tolerance_score * 0.3)
        
        # Component-specific tolerance
        component_tolerance = self._component_tolerance.get(event.target_entity, 0.5)
        component_factor = component_tolerance * 0.2
        
        # Event type difficulty
        type_difficulty = {
            ChaosEventType.PRIVILEGE_FLIP.value: 0.7,
            ChaosEventType.RESOURCE_SPIKE.value: 0.6,
            ChaosEventType.SYNTHETIC_ANOMALY.value: 0.8,
            ChaosEventType.NETWORK_JITTER.value: 0.5,
            ChaosEventType.TIMING_SKEW.value: 0.6,
        }
        difficulty = type_difficulty.get(event.type, 0.6)
        
        # Severity impact
        severity_factor = 1 - (event.severity * 0.3)
        
        # Random variation (real-world unpredictability)
        variation = random.uniform(-0.1, 0.1)
        
        quality = (base_quality + component_factor) * severity_factor * difficulty + variation
        
        return max(0.0, min(1.0, quality))
    
    def _generate_response_description(
        self, 
        event: ChaosEvent, 
        quality: float
    ) -> str:
        """Generate a description of the system's response."""
        if quality >= 0.8:
            return (
                f"Excellent response: Detected {event.type} immediately, "
                f"contained threat, logged appropriately"
            )
        elif quality >= 0.6:
            return (
                f"Adequate response: Detected {event.type} with delay, "
                f"partial containment achieved"
            )
        elif quality >= 0.4:
            return (
                f"Weak response: Slow detection of {event.type}, "
                f"minimal containment, escalation needed"
            )
        else:
            return (
                f"Failed response: Did not detect {event.type}, "
                f"no containment, security gap exposed"
            )
    
    def _register_weakness(self, weakness: Dict[str, Any], event: ChaosEvent) -> None:
        """Register a discovered weakness."""
        weakness_obj = Weakness(
            id=f"weakness-{uuid.uuid4().hex[:8]}",
            component=weakness["component"],
            weakness_type=weakness["type"],
            severity=weakness["severity"],
            discovered_at=datetime.now(),
            chaos_event_id=event.id,
            description=weakness["description"],
            remediation=self._suggest_remediation(weakness),
        )
        
        self._weakness_registry[weakness_obj.id] = weakness_obj
    
    def _suggest_remediation(self, weakness: Dict[str, Any]) -> str:
        """Suggest remediation for a weakness."""
        remediations = {
            ChaosEventType.PRIVILEGE_FLIP.value: (
                "Strengthen privilege boundary checks; implement "
                "real-time privilege monitoring"
            ),
            ChaosEventType.RESOURCE_SPIKE.value: (
                "Implement resource quotas; add stress-resilient "
                "fallback mechanisms"
            ),
            ChaosEventType.SYNTHETIC_ANOMALY.value: (
                "Enhance anomaly detection with ML models; "
                "improve behavioral baseline tracking"
            ),
        }
        
        return remediations.get(
            weakness["type"],
            "Review component security; implement additional monitoring"
        )
    
    def _update_component_tolerance(self, component: str, delta: float) -> None:
        """Update tolerance score for a specific component."""
        current = self._component_tolerance.get(component, 0.5)
        self._component_tolerance[component] = max(0.0, min(1.0, current + delta))
    
    def _get_resilience_level(self) -> ResilienceLevel:
        """Get the current resilience level classification."""
        if self.tolerance_score >= 0.9:
            return ResilienceLevel.IMMUNIZED
        elif self.tolerance_score >= 0.7:
            return ResilienceLevel.STRONG
        elif self.tolerance_score >= 0.5:
            return ResilienceLevel.MODERATE
        elif self.tolerance_score >= 0.3:
            return ResilienceLevel.WEAK
        else:
            return ResilienceLevel.VULNERABLE
    
    def report(self) -> Dict[str, Any]:
        """
        Generate a comprehensive immunization report.
        
        Biological Analogy:
        This is like a health report card or vaccination record.
        It summarizes the organism's immunity status, including:
        - Antibody titers (tolerance scores)
        - Known vulnerabilities (weaknesses)
        - Vaccination history (chaos event history)
        - Immune response strength (resilience level)
        
        Returns:
            Dictionary with tolerance score, weaknesses, and statistics.
        """
        # Calculate statistics
        total_events = len(self.events)
        
        event_type_counts = {}
        for event in self.events:
            event_type_counts[event.type] = event_type_counts.get(event.type, 0) + 1
        
        avg_response_quality = 0.0
        if self._immunization_history:
            # Estimate from tolerance changes
            positive_cycles = sum(
                1 for h in self._immunization_history 
                if h.tolerance_after > h.tolerance_before
            )
            avg_response_quality = positive_cycles / len(self._immunization_history)
        
        # Compile weakness summary
        weakness_summary = []
        for weakness_id, weakness in self._weakness_registry.items():
            weakness_summary.append({
                "id": weakness_id,
                "component": weakness.component,
                "type": weakness.weakness_type,
                "severity": weakness.severity,
                "remediated": weakness.is_remediated,
                "remediation_suggestion": weakness.remediation,
            })
        
        report = {
            "tolerance_score": self.tolerance_score,
            "resilience_level": self._get_resilience_level().value,
            "total_chaos_events": total_events,
            "immunization_cycles": self._cycle_count,
            "weaknesses": self.weaknesses,
            "weakness_count": len(self.weaknesses),
            "weakness_details": weakness_summary,
            "event_type_breakdown": event_type_counts,
            "component_tolerances": dict(self._component_tolerance),
            "average_response_quality": avg_response_quality,
            "immunization_history": [
                {
                    "cycle_id": h.cycle_id,
                    "tolerance_before": h.tolerance_before,
                    "tolerance_after": h.tolerance_after,
                    "weaknesses_found": len(h.weaknesses_discovered),
                    "strengthened": len(h.strengthened_areas),
                }
                for h in self._immunization_history[-10:]  # Last 10 cycles
            ],
            "recommendations": self._generate_recommendations(),
        }
        
        logger.info(
            f"ChaosImmunizationLayer.report: Generated report - "
            f"tolerance={self.tolerance_score:.2f}, weaknesses={len(self.weaknesses)}"
        )
        
        return report
    
    def _generate_recommendations(self) -> List[str]:
        """Generate recommendations based on current state."""
        recommendations = []
        
        # Tolerance-based recommendations
        if self.tolerance_score < 0.3:
            recommendations.append(
                "CRITICAL: System is highly vulnerable. Immediate security review needed."
            )
        elif self.tolerance_score < 0.5:
            recommendations.append(
                "WARNING: System resilience is below optimal. Consider additional hardening."
            )
        elif self.tolerance_score >= 0.9:
            recommendations.append(
                "EXCELLENT: System shows strong immunization. Maintain current practices."
            )
        
        # Weakness-based recommendations
        if len(self.weaknesses) > 5:
            recommendations.append(
                f"Found {len(self.weaknesses)} weaknesses. Prioritize remediation by severity."
            )
        
        # Cycle-based recommendations
        if self._cycle_count < 3:
            recommendations.append(
                "Run more immunization cycles to build comprehensive resilience."
            )
        
        # Component-specific recommendations
        weak_components = [
            comp for comp, tol in self._component_tolerance.items()
            if tol < 0.4
        ]
        if weak_components:
            recommendations.append(
                f"Focus on strengthening: {', '.join(weak_components)}"
            )
        
        return recommendations
    
    def get_event_history(self, limit: int = 50) -> List[Dict[str, Any]]:
        """Get recent chaos event history."""
        return [event.to_dict() for event in self.events[-limit:]]
    
    def reset_tolerance(self, new_tolerance: float = 0.5) -> None:
        """Reset tolerance score (like immune system reset after illness)."""
        old_tolerance = self.tolerance_score
        self.tolerance_score = max(
            self.MIN_TOLERANCE,
            min(self.MAX_TOLERANCE, new_tolerance)
        )
        logger.info(
            f"ChaosImmunizationLayer: Reset tolerance "
            f"{old_tolerance:.2f} -> {self.tolerance_score:.2f}"
        )


# =============================================================================
# Integration Points
# =============================================================================

class ChaosScheduler:
    """
    Scheduler for automated chaos injection.
    
    Biological Analogy:
    Like a vaccination schedule - a planned program of immunization
    events to build comprehensive protection over time.
    """
    
    def __init__(self, layer: ChaosImmunizationLayer):
        """Initialize with a chaos immunization layer."""
        self.layer = layer
        self._schedule: List[Dict[str, Any]] = []
    
    def add_scheduled_event(
        self,
        event_type: ChaosEventType,
        target: str,
        delay_seconds: int,
        **kwargs
    ) -> None:
        """Schedule a chaos event for future injection."""
        self._schedule.append({
            "event_type": event_type,
            "target": target,
            "scheduled_for": datetime.now().timestamp() + delay_seconds,
            "kwargs": kwargs,
        })
    
    def get_pending_schedule(self) -> List[Dict[str, Any]]:
        """Get all scheduled events."""
        return list(self._schedule)


# =============================================================================
# Demo / Example Usage
# =============================================================================

if __name__ == "__main__":
    # Configure logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    print("=" * 70)
    print("CHAOS IMMUNIZATION LAYER DEMO")
    print("Controlled Chaos for System Resilience")
    print("=" * 70)
    
    # Initialize the chaos immunization layer
    print("\n[1] Initializing Chaos Immunization Layer...")
    print("-" * 70)
    
    chaos_layer = ChaosImmunizationLayer(initial_tolerance=0.5)
    print(f"    Initial tolerance score: {chaos_layer.tolerance_score:.2f}")
    print(f"    Resilience level: {chaos_layer._get_resilience_level().value}")
    
    # Inject chaos events
    print("\n[2] Injecting Chaos Events (Vaccine Doses)...")
    print("-" * 70)
    
    # Inject privilege flip
    print("\n    >>> Injecting Privilege Flip (Authorization Vaccine)...")
    event1 = chaos_layer.inject_privilege_flip("user_admin_service")
    print(f"    Event ID: {event1.id}")
    print(f"    Type: {event1.type}")
    print(f"    Description: {event1.description}")
    print(f"    Target: {event1.target_entity}")
    print(f"    Severity: {event1.severity:.2f}")
    
    # Inject resource spike
    print("\n    >>> Injecting Resource Spike (Stress Inoculation)...")
    event2 = chaos_layer.inject_resource_spike(
        entity="critical_process",
        cpu=75,
        memory=512
    )
    print(f"    Event ID: {event2.id}")
    print(f"    Type: {event2.type}")
    print(f"    Description: {event2.description}")
    print(f"    Payload: CPU={event2.payload['cpu_percent']}%, "
          f"Memory={event2.payload['memory_mb']}MB")
    
    # Inject synthetic anomaly
    print("\n    >>> Injecting Synthetic Anomaly (Novel Antigen Exposure)...")
    event3 = chaos_layer.inject_synthetic_anomaly(
        entity="network_monitor",
        anomaly="unusual outbound traffic pattern at odd hours"
    )
    print(f"    Event ID: {event3.id}")
    print(f"    Type: {event3.type}")
    print(f"    Description: {event3.description}")
    print(f"    Deviation Score: {event3.payload['deviation_score']:.2f}")
    
    # Inject additional chaos events
    print("\n    >>> Injecting Additional Chaos Events...")
    chaos_layer.inject_network_jitter("firewall_interface", latency_ms=150)
    print(f"    - Network jitter injected")
    
    chaos_layer.inject_timing_skew("log_aggregator", skew_ms=500)
    print(f"    - Timing skew injected")
    
    print(f"\n    Total pending events: {len(chaos_layer._pending_events)}")
    
    # Run immunization cycle
    print("\n[3] Running Immunization Cycle (Immune Response Training)...")
    print("-" * 70)
    
    cycle_result = chaos_layer.run_immunization_cycle()
    
    print(f"\n    Cycle ID: {cycle_result['cycle_id']}")
    print(f"    Events Processed: {cycle_result['events_processed']}")
    print(f"    Tolerance Before: {cycle_result['tolerance_before']:.2f}")
    print(f"    Tolerance After: {cycle_result['tolerance_after']:.2f}")
    print(f"    Tolerance Change: {cycle_result['tolerance_change']:+.3f}")
    print(f"    Resilience Level: {cycle_result['resilience_level']}")
    
    if cycle_result['weaknesses_discovered']:
        print(f"\n    Weaknesses Discovered:")
        for weakness in cycle_result['weaknesses_discovered']:
            print(f"    ⚠️  {weakness}")
    else:
        print(f"\n    No new weaknesses discovered in this cycle.")
    
    if cycle_result['strengthened_areas']:
        print(f"\n    Strengthened Areas:")
        for area in cycle_result['strengthened_areas']:
            print(f"    ✓  {area}")
    
    # Run additional cycles
    print("\n[4] Running Additional Immunization Cycles...")
    print("-" * 70)
    
    for i in range(3):
        # Inject new chaos
        chaos_layer.inject_privilege_flip(f"service_{i}")
        chaos_layer.inject_resource_spike(f"process_{i}", cpu=50 + i*10, memory=256)
        
        # Run cycle
        result = chaos_layer.run_immunization_cycle()
        print(f"\n    Cycle {result['cycle_id']}: "
              f"tolerance {result['tolerance_before']:.2f} -> {result['tolerance_after']:.2f}")
    
    # Generate final report
    print("\n[5] Final Immunization Report (Health Card)...")
    print("-" * 70)
    
    report = chaos_layer.report()
    
    print(f"\n    === SYSTEM IMMUNITY STATUS ===")
    print(f"\n    Tolerance Score: {report['tolerance_score']:.2f}")
    print(f"    Resilience Level: {report['resilience_level'].upper()}")
    print(f"\n    Total Chaos Events: {report['total_chaos_events']}")
    print(f"    Immunization Cycles: {report['immunization_cycles']}")
    print(f"    Total Weaknesses: {report['weakness_count']}")
    
    print(f"\n    Event Type Breakdown:")
    for event_type, count in report['event_type_breakdown'].items():
        print(f"    - {event_type}: {count}")
    
    print(f"\n    Component Tolerances:")
    for component, tolerance in list(report['component_tolerances'].items())[:5]:
        status = "🟢" if tolerance >= 0.6 else "🟡" if tolerance >= 0.4 else "🔴"
        print(f"    {status} {component}: {tolerance:.2f}")
    
    if report['weaknesses']:
        print(f"\n    Discovered Weaknesses (Vulnerable Organs):")
        for weakness in report['weaknesses'][:5]:
            print(f"    ⚠️  {weakness}")
    
    print(f"\n    Recommendations:")
    for rec in report['recommendations']:
        print(f"    • {rec}")
    
    print("\n" + "=" * 70)
    print("CHAOS IMMUNIZATION DEMO COMPLETE")
    print("System resilience has been tested and improved through controlled chaos.")
    print("=" * 70)
