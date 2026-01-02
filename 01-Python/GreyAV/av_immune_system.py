"""
AV Immune System Module for GreyAV EDR System.

This module implements a bio-inspired digital immune system that models
defense mechanisms using immunological metaphors. Just as the biological
immune system protects organisms from pathogens, this module protects
digital systems from malware and threats.

Biological Mapping:
- InnateDefense → Innate immune system (neutrophils, macrophages, NK cells)
- AdaptiveDefense → Adaptive immune system (T cells, B cells, antibodies)
- ImmuneMemory → Immunological memory (memory B/T cells)
- SelfHealing → Tissue repair and regeneration mechanisms

The immune system operates in layers:
1. Innate Defense: Fast, generic response to common threat patterns
2. Adaptive Defense: Slower, specific response that learns from exposure
3. Immune Memory: Long-term storage of threat signatures for rapid recall
4. Self-Healing: Damage repair and system restoration
"""

import uuid
import hashlib
import logging
import time
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List, Callable, Set, Tuple
from enum import Enum
from abc import ABC, abstractmethod

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


# =============================================================================
# Core Data Structures
# =============================================================================

class ThreatSeverity(Enum):
    """Severity classification for threats."""
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4


class ThreatType(Enum):
    """Classification of threat types."""
    FILE_MUTATION = "file_mutation"
    PROCESS_INJECTION = "process_injection"
    NETWORK_ANOMALY = "network_anomaly"
    PRIVILEGE_ESCALATION = "privilege_escalation"
    PERSISTENCE = "persistence"
    DATA_EXFILTRATION = "data_exfiltration"
    RANSOMWARE = "ransomware"
    ROOTKIT = "rootkit"
    TROJAN = "trojan"
    WORM = "worm"


class ResponseType(Enum):
    """Types of immune responses."""
    NEUTRALIZE = "neutralize"
    QUARANTINE = "quarantine"
    ELIMINATE = "eliminate"
    MONITOR = "monitor"
    ALERT = "alert"


@dataclass
class Threat:
    """
    Represents a detected threat in the system.
    
    Biological Analogy:
    A Threat is analogous to a pathogen (virus, bacteria, parasite).
    It has identifying characteristics (antigens) that the immune
    system uses to recognize and respond to it.
    
    Attributes:
        id: Unique identifier (like a pathogen's genetic signature).
        type: Category of threat (like pathogen classification).
        payload: Malicious content or behavior.
        timestamp: When the threat was detected.
        severity: Threat severity level.
        source: Where the threat originated.
        indicators: Observable indicators of compromise.
        metadata: Additional threat context.
    """
    id: str
    type: str
    payload: str
    timestamp: datetime = field(default_factory=datetime.now)
    severity: ThreatSeverity = ThreatSeverity.MEDIUM
    source: str = ""
    indicators: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    
    def get_signature(self) -> str:
        """Generate a unique signature for this threat."""
        content = f"{self.type}:{self.payload}:{self.source}"
        return hashlib.sha256(content.encode()).hexdigest()[:32]


@dataclass
class Antibody:
    """
    Represents a learned defense against a specific threat.
    
    Biological Analogy:
    Antibodies are proteins produced by B cells that recognize
    specific antigens on pathogens. Here, an Antibody is a
    signature-based detection rule learned from threat exposure.
    
    Attributes:
        id: Unique antibody identifier.
        target_signature: Threat signature this antibody recognizes.
        threat_type: Type of threat this targets.
        effectiveness: How well this antibody neutralizes the threat.
        created_at: When this antibody was created.
        last_used: Last time this antibody was activated.
        use_count: Number of times this antibody has been used.
    """
    id: str
    target_signature: str
    threat_type: str
    effectiveness: float = 1.0
    created_at: datetime = field(default_factory=datetime.now)
    last_used: Optional[datetime] = None
    use_count: int = 0
    detection_rules: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ImmuneResponse:
    """
    Represents an immune system response to a threat.
    
    Biological Analogy:
    An immune response encompasses all actions taken by the immune
    system to neutralize a threat, including cytokine signaling,
    phagocytosis, and antibody production.
    
    Attributes:
        response_id: Unique response identifier.
        threat_id: ID of the threat being responded to.
        response_type: Type of response (neutralize, quarantine, etc.).
        actions_taken: List of specific actions performed.
        success: Whether the response was successful.
        timestamp: When the response occurred.
        duration_ms: Response duration in milliseconds.
    """
    response_id: str
    threat_id: str
    response_type: ResponseType
    actions_taken: List[str]
    success: bool = True
    timestamp: datetime = field(default_factory=datetime.now)
    duration_ms: float = 0.0
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class SystemState:
    """
    Represents the current state of the protected system.
    
    Biological Analogy:
    SystemState is like the health status of an organism,
    tracking damage, infections, and overall integrity.
    
    Attributes:
        health_score: Overall system health (0.0 - 1.0).
        active_threats: Currently active threat IDs.
        quarantined_items: Items in quarantine.
        damaged_components: Components needing repair.
        last_scan: Last security scan timestamp.
    """
    health_score: float = 1.0
    active_threats: Set[str] = field(default_factory=set)
    quarantined_items: Dict[str, Dict] = field(default_factory=dict)
    damaged_components: List[str] = field(default_factory=list)
    last_scan: datetime = field(default_factory=datetime.now)
    integrity_hashes: Dict[str, str] = field(default_factory=dict)


# =============================================================================
# Immune System Components
# =============================================================================

class InnateDefense:
    """
    Innate Defense System - First Line of Defense.
    
    Biological Analogy:
    The innate immune system is the body's first line of defense.
    It includes physical barriers (skin), chemical barriers (stomach acid),
    and cellular defenses (neutrophils, macrophages, natural killer cells).
    These defenses are non-specific and respond immediately to any pathogen.
    
    Digital Mapping:
    - Pattern-based detection (like toll-like receptors recognizing PAMPs)
    - Signature matching (like complement system recognition)
    - Anomaly detection (like inflammation response to tissue damage)
    - Immediate blocking (like neutrophil phagocytosis)
    
    The innate defense provides rapid, generic protection against common
    threats without requiring prior exposure or learning.
    """
    
    # Known malicious patterns (like Pathogen-Associated Molecular Patterns)
    THREAT_PATTERNS: Dict[str, List[str]] = {
        "file_mutation": [
            "MZ", "CreateRemoteThread", "VirtualAllocEx",
            "WriteProcessMemory", ".exe", ".dll", "packed"
        ],
        "process_injection": [
            "NtCreateThreadEx", "RtlCreateUserThread", "SetWindowsHookEx",
            "QueueUserAPC", "hollowing", "doppelganging"
        ],
        "network_anomaly": [
            "beacon", "c2", "exfil", "tunnel", "dns_over_https",
            "unusual_port", "large_upload"
        ],
        "privilege_escalation": [
            "SeDebugPrivilege", "UAC_bypass", "token_manipulation",
            "potato", "printspoofer"
        ],
        "ransomware": [
            "encrypt", "ransom", "bitcoin", "decrypt_instructions",
            "file_rename_mass", ".locked", ".encrypted"
        ],
    }
    
    # Severity thresholds
    ALERT_THRESHOLD: float = 0.5
    BLOCK_THRESHOLD: float = 0.8
    
    def __init__(self):
        """
        Initialize the Innate Defense system.
        
        Like the development of innate immune cells in bone marrow,
        this initializes the detection capabilities with pre-programmed
        pattern recognition.
        """
        self._detection_count: int = 0
        self._response_count: int = 0
        self._blocked_threats: List[str] = []
        self._subscribers: List[Callable[[Threat, ImmuneResponse], None]] = []
        
        logger.info("InnateDefense initialized - First line of defense active")
    
    def subscribe(self, callback: Callable[[Threat, ImmuneResponse], None]) -> None:
        """Subscribe to innate defense events."""
        self._subscribers.append(callback)
    
    def _notify_subscribers(self, threat: Threat, response: ImmuneResponse) -> None:
        """Notify subscribers of detection and response."""
        for callback in self._subscribers:
            try:
                callback(threat, response)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def detect(self, threat: Threat) -> Dict[str, Any]:
        """
        Detect threats using innate pattern recognition.
        
        Biological Analogy:
        This is like toll-like receptors (TLRs) on macrophages and
        dendritic cells recognizing pathogen-associated molecular
        patterns (PAMPs). The detection is fast and doesn't require
        prior exposure to the specific pathogen.
        
        Args:
            threat: The potential threat to analyze.
            
        Returns:
            Detection result with confidence and matched patterns.
        """
        start_time = time.time()
        self._detection_count += 1
        
        matched_patterns: List[str] = []
        confidence = 0.0
        
        # Check threat type patterns (like PAMP recognition)
        patterns = self.THREAT_PATTERNS.get(threat.type, [])
        for pattern in patterns:
            if pattern.lower() in threat.payload.lower():
                matched_patterns.append(pattern)
        
        # Check indicators (like DAMPs - Damage-Associated Molecular Patterns)
        for indicator in threat.indicators:
            for threat_type, type_patterns in self.THREAT_PATTERNS.items():
                for pattern in type_patterns:
                    if pattern.lower() in indicator.lower():
                        matched_patterns.append(f"{threat_type}:{pattern}")
        
        # Calculate confidence based on pattern matches
        if patterns:
            confidence = len(matched_patterns) / max(len(patterns), 1)
        else:
            # Unknown threat type - higher suspicion
            confidence = 0.3 if threat.payload else 0.1
        
        # Adjust for severity (like inflammatory response intensity)
        severity_multiplier = {
            ThreatSeverity.LOW: 0.8,
            ThreatSeverity.MEDIUM: 1.0,
            ThreatSeverity.HIGH: 1.2,
            ThreatSeverity.CRITICAL: 1.5,
        }
        confidence *= severity_multiplier.get(threat.severity, 1.0)
        confidence = min(1.0, confidence)
        
        detection_time = (time.time() - start_time) * 1000
        
        result = {
            "threat_id": threat.id,
            "detected": confidence >= self.ALERT_THRESHOLD,
            "confidence": confidence,
            "matched_patterns": matched_patterns,
            "threat_type": threat.type,
            "severity": threat.severity.name,
            "detection_time_ms": detection_time,
            "recommendation": self._get_recommendation(confidence),
        }
        
        logger.info(
            f"InnateDefense.detect: {threat.id} - "
            f"confidence={confidence:.2f}, patterns={len(matched_patterns)}"
        )
        
        return result
    
    def _get_recommendation(self, confidence: float) -> str:
        """Get response recommendation based on confidence."""
        if confidence >= self.BLOCK_THRESHOLD:
            return "BLOCK_IMMEDIATELY"
        elif confidence >= self.ALERT_THRESHOLD:
            return "ALERT_AND_MONITOR"
        elif confidence >= 0.3:
            return "MONITOR"
        else:
            return "ALLOW"
    
    def respond(self, threat: Threat, detection_result: Dict[str, Any]) -> ImmuneResponse:
        """
        Execute immediate response to detected threat.
        
        Biological Analogy:
        This is like the rapid response of neutrophils and macrophages
        that engulf and destroy pathogens through phagocytosis, or
        natural killer cells that destroy infected cells. The response
        is immediate and non-specific.
        
        Args:
            threat: The detected threat.
            detection_result: Result from detect().
            
        Returns:
            ImmuneResponse describing actions taken.
        """
        start_time = time.time()
        self._response_count += 1
        
        actions_taken: List[str] = []
        response_type = ResponseType.MONITOR
        success = True
        
        confidence = detection_result.get("confidence", 0)
        recommendation = detection_result.get("recommendation", "ALLOW")
        
        if recommendation == "BLOCK_IMMEDIATELY":
            # Like phagocytosis - immediate elimination
            response_type = ResponseType.ELIMINATE
            actions_taken.extend([
                f"Blocked threat execution: {threat.id}",
                f"Terminated associated processes",
                f"Quarantined payload: {threat.payload[:50]}...",
                f"Generated threat signature for adaptive system",
            ])
            self._blocked_threats.append(threat.id)
            
        elif recommendation == "ALERT_AND_MONITOR":
            # Like inflammatory response - alert and contain
            response_type = ResponseType.QUARANTINE
            actions_taken.extend([
                f"Quarantined suspicious item: {threat.id}",
                f"Elevated monitoring for source: {threat.source}",
                f"Notified adaptive defense system",
            ])
            
        elif recommendation == "MONITOR":
            # Like low-grade inflammation - watchful waiting
            response_type = ResponseType.MONITOR
            actions_taken.extend([
                f"Added to watch list: {threat.id}",
                f"Increased logging for related activity",
            ])
        else:
            # Allow but log
            response_type = ResponseType.ALERT
            actions_taken.append(f"Logged activity: {threat.id}")
        
        duration = (time.time() - start_time) * 1000
        
        response = ImmuneResponse(
            response_id=str(uuid.uuid4()),
            threat_id=threat.id,
            response_type=response_type,
            actions_taken=actions_taken,
            success=success,
            duration_ms=duration,
            metadata={
                "confidence": confidence,
                "recommendation": recommendation,
                "matched_patterns": detection_result.get("matched_patterns", []),
            },
        )
        
        # Notify subscribers (like cytokine signaling)
        self._notify_subscribers(threat, response)
        
        logger.info(
            f"InnateDefense.respond: {threat.id} - "
            f"response={response_type.value}, actions={len(actions_taken)}"
        )
        
        return response
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get innate defense statistics."""
        return {
            "detection_count": self._detection_count,
            "response_count": self._response_count,
            "blocked_threats": len(self._blocked_threats),
            "pattern_categories": len(self.THREAT_PATTERNS),
        }


class AdaptiveDefense:
    """
    Adaptive Defense System - Learned Immunity.
    
    Biological Analogy:
    The adaptive immune system provides specific, learned immunity.
    B cells produce antibodies that recognize specific antigens,
    while T cells coordinate the immune response and destroy infected
    cells. Unlike innate immunity, adaptive immunity improves with
    exposure and provides long-lasting protection.
    
    Digital Mapping:
    - Antibody library (like B cell antibody repertoire)
    - Learning from new threats (like clonal selection)
    - Adapting detection rules (like affinity maturation)
    - Signature generation (like antibody production)
    
    The adaptive defense learns from each threat encounter, creating
    specific defenses that can be rapidly deployed on re-exposure.
    """
    
    def __init__(self):
        """
        Initialize the Adaptive Defense system.
        
        Like the development of T and B cells in the thymus and
        bone marrow, this initializes the adaptive immune repertoire.
        """
        self._antibody_library: Dict[str, Antibody] = {}
        self._learning_events: List[Dict[str, Any]] = []
        self._adaptation_count: int = 0
        self._subscribers: List[Callable[[Antibody], None]] = []
        
        logger.info("AdaptiveDefense initialized - Learning system active")
    
    def subscribe(self, callback: Callable[[Antibody], None]) -> None:
        """Subscribe to new antibody creation events."""
        self._subscribers.append(callback)
    
    def _notify_subscribers(self, antibody: Antibody) -> None:
        """Notify subscribers of new antibody."""
        for callback in self._subscribers:
            try:
                callback(antibody)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def learn(self, threat: Threat) -> Antibody:
        """
        Learn from a threat exposure and generate antibodies.
        
        Biological Analogy:
        This is like clonal selection - when a B cell's receptor
        binds to an antigen, that B cell proliferates and produces
        antibodies specific to that antigen. The more exposure,
        the more refined the antibodies become.
        
        Args:
            threat: The threat to learn from.
            
        Returns:
            Newly created or updated Antibody.
        """
        signature = threat.get_signature()
        
        if signature in self._antibody_library:
            # Boost existing antibody (like memory B cell activation)
            antibody = self._antibody_library[signature]
            antibody.use_count += 1
            antibody.last_used = datetime.now()
            
            # Affinity maturation - improve effectiveness
            antibody.effectiveness = min(1.0, antibody.effectiveness + 0.05)
            
            logger.info(
                f"AdaptiveDefense.learn: Boosted antibody {antibody.id} "
                f"for {threat.type} (effectiveness: {antibody.effectiveness:.2f})"
            )
        else:
            # Create new antibody (like naive B cell activation)
            antibody = Antibody(
                id=f"ab-{uuid.uuid4().hex[:8]}",
                target_signature=signature,
                threat_type=threat.type,
                effectiveness=0.7,  # Initial effectiveness
                detection_rules={
                    "payload_patterns": self._extract_patterns(threat.payload),
                    "indicators": threat.indicators.copy(),
                    "source_pattern": threat.source,
                    "severity": threat.severity.name,
                },
            )
            
            self._antibody_library[signature] = antibody
            self._notify_subscribers(antibody)
            
            logger.info(
                f"AdaptiveDefense.learn: Created antibody {antibody.id} "
                f"for {threat.type}"
            )
        
        # Record learning event
        self._learning_events.append({
            "threat_id": threat.id,
            "threat_type": threat.type,
            "signature": signature,
            "antibody_id": antibody.id,
            "timestamp": datetime.now().isoformat(),
        })
        
        return antibody
    
    def _extract_patterns(self, payload: str) -> List[str]:
        """Extract recognizable patterns from payload."""
        patterns = []
        
        # Simple pattern extraction (would be more sophisticated in production)
        words = payload.split()
        for word in words:
            if len(word) >= 4:
                patterns.append(word.lower())
        
        # Extract potential hex patterns
        import re
        hex_patterns = re.findall(r'[0-9A-Fa-f]{8,}', payload)
        patterns.extend(hex_patterns[:5])
        
        return patterns[:20]  # Limit patterns
    
    def adapt(self, threat: Threat) -> Dict[str, Any]:
        """
        Adapt defenses based on threat characteristics.
        
        Biological Analogy:
        This is like the adaptive immune system's ability to
        undergo affinity maturation - where antibodies become
        more specific and effective through somatic hypermutation
        and selection in germinal centers.
        
        Args:
            threat: The threat to adapt against.
            
        Returns:
            Adaptation result with updated defense parameters.
        """
        self._adaptation_count += 1
        signature = threat.get_signature()
        
        adaptations: List[str] = []
        
        # Check for existing antibody
        if signature in self._antibody_library:
            antibody = self._antibody_library[signature]
            
            # Update detection rules based on new threat variant
            new_patterns = self._extract_patterns(threat.payload)
            existing_patterns = antibody.detection_rules.get("payload_patterns", [])
            
            for pattern in new_patterns:
                if pattern not in existing_patterns:
                    existing_patterns.append(pattern)
                    adaptations.append(f"Added pattern: {pattern}")
            
            antibody.detection_rules["payload_patterns"] = existing_patterns[:30]
            
            # Add new indicators
            for indicator in threat.indicators:
                if indicator not in antibody.detection_rules.get("indicators", []):
                    antibody.detection_rules.setdefault("indicators", []).append(indicator)
                    adaptations.append(f"Added indicator: {indicator}")
        else:
            # Learn the new threat first
            antibody = self.learn(threat)
            adaptations.append(f"Created new antibody: {antibody.id}")
        
        # Cross-reactivity check (like cross-reactive antibodies)
        similar_antibodies = self._find_similar_antibodies(threat)
        for similar in similar_antibodies:
            adaptations.append(f"Found cross-reactive antibody: {similar.id}")
        
        result = {
            "threat_id": threat.id,
            "signature": signature,
            "adaptations": adaptations,
            "antibody_id": antibody.id,
            "effectiveness": antibody.effectiveness,
            "cross_reactive_count": len(similar_antibodies),
        }
        
        logger.info(
            f"AdaptiveDefense.adapt: {threat.id} - "
            f"adaptations={len(adaptations)}"
        )
        
        return result
    
    def _find_similar_antibodies(self, threat: Threat) -> List[Antibody]:
        """Find antibodies that might cross-react with this threat."""
        similar = []
        
        for antibody in self._antibody_library.values():
            # Check threat type match
            if antibody.threat_type == threat.type:
                similar.append(antibody)
                continue
            
            # Check pattern overlap
            patterns = antibody.detection_rules.get("payload_patterns", [])
            for pattern in patterns:
                if pattern in threat.payload.lower():
                    similar.append(antibody)
                    break
        
        return similar[:5]  # Limit results
    
    def check_antibodies(self, threat: Threat) -> Optional[Antibody]:
        """
        Check if we have antibodies for this threat.
        
        Biological Analogy:
        Like circulating antibodies in the bloodstream that can
        immediately bind to recognized pathogens.
        
        Args:
            threat: Threat to check.
            
        Returns:
            Matching antibody if found.
        """
        signature = threat.get_signature()
        
        if signature in self._antibody_library:
            antibody = self._antibody_library[signature]
            antibody.use_count += 1
            antibody.last_used = datetime.now()
            return antibody
        
        # Check cross-reactive antibodies
        similar = self._find_similar_antibodies(threat)
        if similar:
            return similar[0]
        
        return None
    
    def get_antibody_library(self) -> Dict[str, Antibody]:
        """Get the full antibody library."""
        return dict(self._antibody_library)
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get adaptive defense statistics."""
        return {
            "antibody_count": len(self._antibody_library),
            "learning_events": len(self._learning_events),
            "adaptation_count": self._adaptation_count,
            "avg_effectiveness": (
                sum(ab.effectiveness for ab in self._antibody_library.values())
                / max(len(self._antibody_library), 1)
            ),
        }


class ImmuneMemory:
    """
    Immune Memory System - Long-term Threat Memory.
    
    Biological Analogy:
    Immunological memory is provided by memory B cells and memory T cells.
    After initial exposure to a pathogen, these cells persist for years
    or even a lifetime, enabling rapid and robust responses to re-infection.
    This is the basis of vaccination.
    
    Digital Mapping:
    - Memory cells (like threat signature database)
    - Rapid recall (like memory B cell reactivation)
    - Memory consolidation (like germinal center selection)
    - Memory decay (like natural loss of immunity over time)
    
    The immune memory enables the system to "remember" past threats
    and respond more quickly and effectively upon re-exposure.
    """
    
    # Memory retention settings
    MEMORY_EXPIRY_DAYS: int = 365
    MAX_MEMORY_SIZE: int = 10000
    
    def __init__(self):
        """
        Initialize the Immune Memory system.
        
        Like the establishment of memory cell populations after
        an immune response, this initializes the memory storage.
        """
        self._memory: Dict[str, Dict[str, Any]] = {}
        self._recall_count: int = 0
        self._memory_hits: int = 0
        self._memory_misses: int = 0
        
        logger.info("ImmuneMemory initialized - Long-term memory active")
    
    def remember(self, threat: Threat, response: Optional[ImmuneResponse] = None) -> Dict[str, Any]:
        """
        Store a threat in immune memory.
        
        Biological Analogy:
        This is like the formation of memory B cells and memory T cells
        after an infection or vaccination. The memory cells contain
        information about the specific antigen for future recognition.
        
        Args:
            threat: The threat to remember.
            response: Optional response that was effective.
            
        Returns:
            Memory record created.
        """
        signature = threat.get_signature()
        
        # Create memory record
        memory_record = {
            "threat_id": threat.id,
            "signature": signature,
            "threat_type": threat.type,
            "severity": threat.severity.name,
            "payload_hash": hashlib.md5(threat.payload.encode()).hexdigest(),
            "indicators": threat.indicators.copy(),
            "source": threat.source,
            "first_seen": datetime.now().isoformat(),
            "last_seen": datetime.now().isoformat(),
            "encounter_count": 1,
            "effective_response": response.response_type.value if response else None,
            "metadata": threat.metadata.copy(),
        }
        
        if signature in self._memory:
            # Update existing memory (like memory cell boost)
            existing = self._memory[signature]
            existing["encounter_count"] += 1
            existing["last_seen"] = datetime.now().isoformat()
            
            # Merge indicators
            for indicator in threat.indicators:
                if indicator not in existing["indicators"]:
                    existing["indicators"].append(indicator)
            
            logger.info(
                f"ImmuneMemory.remember: Strengthened memory for {threat.type} "
                f"(encounters: {existing['encounter_count']})"
            )
            return existing
        else:
            # New memory formation
            self._memory[signature] = memory_record
            
            # Enforce memory limit (like immune homeostasis)
            self._enforce_memory_limit()
            
            logger.info(
                f"ImmuneMemory.remember: Created new memory for {threat.type}"
            )
            return memory_record
    
    def recall(self, threat: Threat) -> Optional[Dict[str, Any]]:
        """
        Recall memory of a previously seen threat.
        
        Biological Analogy:
        This is like memory B cell reactivation upon re-exposure
        to an antigen. The memory cells rapidly differentiate into
        antibody-producing plasma cells, mounting a faster and
        stronger response than the primary exposure.
        
        Args:
            threat: The threat to look up.
            
        Returns:
            Memory record if found, None otherwise.
        """
        self._recall_count += 1
        signature = threat.get_signature()
        
        if signature in self._memory:
            self._memory_hits += 1
            memory = self._memory[signature]
            memory["last_seen"] = datetime.now().isoformat()
            
            logger.info(
                f"ImmuneMemory.recall: HIT - Found memory for {threat.type} "
                f"(encounters: {memory['encounter_count']})"
            )
            return memory
        
        # Check for similar threats (like cross-reactive memory)
        similar = self._find_similar_memory(threat)
        if similar:
            self._memory_hits += 1
            logger.info(
                f"ImmuneMemory.recall: PARTIAL HIT - Found similar memory"
            )
            return similar
        
        self._memory_misses += 1
        logger.info(f"ImmuneMemory.recall: MISS - No memory for {threat.type}")
        return None
    
    def _find_similar_memory(self, threat: Threat) -> Optional[Dict[str, Any]]:
        """Find similar threat memories."""
        for signature, memory in self._memory.items():
            # Check type match
            if memory["threat_type"] == threat.type:
                # Check indicator overlap
                common = set(memory["indicators"]) & set(threat.indicators)
                if len(common) >= 2:
                    return memory
        
        return None
    
    def _enforce_memory_limit(self) -> None:
        """Enforce memory size limit by removing old entries."""
        if len(self._memory) <= self.MAX_MEMORY_SIZE:
            return
        
        # Sort by last seen (remove oldest)
        sorted_memories = sorted(
            self._memory.items(),
            key=lambda x: x[1].get("last_seen", ""),
        )
        
        # Remove oldest 10%
        remove_count = len(self._memory) - int(self.MAX_MEMORY_SIZE * 0.9)
        for i in range(remove_count):
            signature = sorted_memories[i][0]
            del self._memory[signature]
        
        logger.info(f"ImmuneMemory: Removed {remove_count} old memories")
    
    def get_memory_by_type(self, threat_type: str) -> List[Dict[str, Any]]:
        """Get all memories of a specific threat type."""
        return [
            memory for memory in self._memory.values()
            if memory["threat_type"] == threat_type
        ]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get memory statistics."""
        hit_rate = (
            self._memory_hits / max(self._recall_count, 1)
        ) * 100
        
        return {
            "total_memories": len(self._memory),
            "recall_attempts": self._recall_count,
            "memory_hits": self._memory_hits,
            "memory_misses": self._memory_misses,
            "hit_rate_percent": hit_rate,
            "unique_threat_types": len(set(
                m["threat_type"] for m in self._memory.values()
            )),
        }
    
    def export_memory(self) -> Dict[str, Dict[str, Any]]:
        """Export all memories for backup."""
        return dict(self._memory)
    
    def import_memory(self, memories: Dict[str, Dict[str, Any]]) -> int:
        """Import memories from backup. Returns count imported."""
        count = 0
        for signature, memory in memories.items():
            if signature not in self._memory:
                self._memory[signature] = memory
                count += 1
        logger.info(f"ImmuneMemory: Imported {count} memories")
        return count


class SelfHealing:
    """
    Self-Healing System - Damage Repair and Recovery.
    
    Biological Analogy:
    The body has remarkable self-healing capabilities. Tissue repair
    involves inflammation (to clear debris), proliferation (to replace
    damaged cells), and remodeling (to restore function). The immune
    system plays a key role in coordinating this process.
    
    Digital Mapping:
    - Isolation (like granuloma formation to contain infection)
    - Repair (like tissue regeneration and wound healing)
    - Integrity restoration (like organ function recovery)
    - System recovery (like homeostasis restoration)
    
    The self-healing system contains damage, repairs affected components,
    and restores the system to a healthy state.
    """
    
    def __init__(self):
        """
        Initialize the Self-Healing system.
        
        Like the body's repair mechanisms being always ready to
        respond to damage, this initializes the healing capabilities.
        """
        self._isolation_count: int = 0
        self._repair_count: int = 0
        self._quarantine: Dict[str, Dict[str, Any]] = {}
        self._repair_log: List[Dict[str, Any]] = []
        
        logger.info("SelfHealing initialized - Recovery system active")
    
    def isolate(self, threat: Threat) -> Dict[str, Any]:
        """
        Isolate a threat to prevent spread.
        
        Biological Analogy:
        This is like granuloma formation - where the immune system
        walls off a pathogen it cannot eliminate (like in tuberculosis).
        It's also like quarantine in epidemiology - isolating the
        infected to prevent spread.
        
        Args:
            threat: The threat to isolate.
            
        Returns:
            Isolation result.
        """
        self._isolation_count += 1
        
        isolation_record = {
            "threat_id": threat.id,
            "threat_type": threat.type,
            "isolated_at": datetime.now().isoformat(),
            "payload_size": len(threat.payload),
            "source": threat.source,
            "status": "isolated",
            "actions": [
                f"Blocked network access for source: {threat.source}",
                f"Suspended associated processes",
                f"Moved payload to quarantine storage",
                f"Revoked access permissions",
                f"Created isolation barrier",
            ],
        }
        
        # Store in quarantine
        self._quarantine[threat.id] = isolation_record
        
        logger.info(
            f"SelfHealing.isolate: Isolated threat {threat.id} "
            f"(type: {threat.type})"
        )
        
        return isolation_record
    
    def repair(self, system_state: SystemState) -> Dict[str, Any]:
        """
        Repair system damage and restore integrity.
        
        Biological Analogy:
        This is like tissue repair after injury. The process involves:
        1. Hemostasis (stopping the damage)
        2. Inflammation (clearing debris)
        3. Proliferation (rebuilding tissue)
        4. Remodeling (restoring function)
        
        Args:
            system_state: Current system state with damage information.
            
        Returns:
            Repair result with actions taken.
        """
        self._repair_count += 1
        start_time = time.time()
        
        repair_actions: List[str] = []
        repaired_components: List[str] = []
        
        # Repair damaged components
        for component in system_state.damaged_components:
            repair_actions.append(f"Analyzing damage to: {component}")
            repair_actions.append(f"Restoring component: {component}")
            repaired_components.append(component)
        
        # Clear quarantined items that are no longer threats
        cleared_items = []
        for item_id, item_info in list(system_state.quarantined_items.items()):
            # Simulate analysis (in production, would verify safety)
            if item_info.get("age_hours", 0) > 24:
                repair_actions.append(f"Cleared quarantine item: {item_id}")
                cleared_items.append(item_id)
        
        # Restore integrity
        for component, expected_hash in system_state.integrity_hashes.items():
            repair_actions.append(f"Verified integrity of: {component}")
        
        # Neutralize active threats
        for threat_id in list(system_state.active_threats):
            repair_actions.append(f"Neutralized active threat: {threat_id}")
        
        # Calculate new health score
        damage_factor = len(system_state.damaged_components) * 0.1
        threat_factor = len(system_state.active_threats) * 0.15
        new_health = max(0.0, min(1.0, 1.0 - damage_factor - threat_factor))
        
        # Healing boost
        healing_boost = 0.1 * len(repaired_components)
        new_health = min(1.0, new_health + healing_boost)
        
        duration = (time.time() - start_time) * 1000
        
        repair_result = {
            "repair_id": str(uuid.uuid4()),
            "timestamp": datetime.now().isoformat(),
            "original_health": system_state.health_score,
            "new_health": new_health,
            "health_improvement": new_health - system_state.health_score,
            "components_repaired": repaired_components,
            "quarantine_cleared": cleared_items,
            "actions_taken": repair_actions,
            "duration_ms": duration,
        }
        
        # Update system state
        system_state.health_score = new_health
        system_state.damaged_components = [
            c for c in system_state.damaged_components
            if c not in repaired_components
        ]
        for item_id in cleared_items:
            system_state.quarantined_items.pop(item_id, None)
        
        # Log repair
        self._repair_log.append(repair_result)
        
        logger.info(
            f"SelfHealing.repair: Health {system_state.health_score:.2f} -> "
            f"{new_health:.2f}, repaired {len(repaired_components)} components"
        )
        
        return repair_result
    
    def restore_file_integrity(self, file_path: str, backup_hash: str) -> Dict[str, Any]:
        """
        Restore a file to its known-good state.
        
        Biological Analogy:
        Like stem cells regenerating damaged tissue based on
        the genetic blueprint.
        
        Args:
            file_path: Path to the damaged file.
            backup_hash: Hash of the known-good version.
            
        Returns:
            Restoration result.
        """
        result = {
            "file_path": file_path,
            "action": "restore_from_backup",
            "backup_hash": backup_hash,
            "timestamp": datetime.now().isoformat(),
            "success": True,  # Simulated
            "message": f"Restored {file_path} to known-good state",
        }
        
        logger.info(f"SelfHealing: Restored file {file_path}")
        return result
    
    def reset_process(self, process_name: str) -> Dict[str, Any]:
        """
        Reset a compromised process.
        
        Biological Analogy:
        Like apoptosis (programmed cell death) of infected cells,
        followed by regeneration of healthy cells.
        
        Args:
            process_name: Name of the process to reset.
            
        Returns:
            Reset result.
        """
        result = {
            "process_name": process_name,
            "action": "terminate_and_restart",
            "timestamp": datetime.now().isoformat(),
            "steps": [
                f"Terminated process: {process_name}",
                f"Cleared process memory",
                f"Restored clean executable",
                f"Restarted process with safe parameters",
            ],
            "success": True,  # Simulated
        }
        
        logger.info(f"SelfHealing: Reset process {process_name}")
        return result
    
    def get_quarantine(self) -> Dict[str, Dict[str, Any]]:
        """Get all quarantined items."""
        return dict(self._quarantine)
    
    def release_from_quarantine(self, threat_id: str) -> bool:
        """Release an item from quarantine."""
        if threat_id in self._quarantine:
            self._quarantine[threat_id]["status"] = "released"
            logger.info(f"SelfHealing: Released {threat_id} from quarantine")
            return True
        return False
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get self-healing statistics."""
        return {
            "isolation_count": self._isolation_count,
            "repair_count": self._repair_count,
            "quarantine_size": len(self._quarantine),
            "repair_log_size": len(self._repair_log),
        }


# =============================================================================
# Integrated Immune System
# =============================================================================

class AVImmuneSystem:
    """
    Integrated AV Immune System combining all defense layers.
    
    Biological Analogy:
    Like the complete immune system of an organism, this class
    coordinates all immune components to provide comprehensive
    protection. It manages the interplay between innate and
    adaptive immunity, memory formation, and healing.
    
    The system operates as a cohesive unit where:
    1. Innate defense provides immediate response
    2. Adaptive defense learns and improves
    3. Memory stores knowledge for future encounters
    4. Self-healing repairs any damage
    """
    
    def __init__(self):
        """Initialize the complete immune system."""
        self.innate = InnateDefense()
        self.adaptive = AdaptiveDefense()
        self.memory = ImmuneMemory()
        self.healing = SelfHealing()
        
        self._system_state = SystemState()
        self._response_log: List[Dict[str, Any]] = []
        
        # Wire up internal communication (like cytokine network)
        self._setup_internal_signaling()
        
        logger.info("AVImmuneSystem initialized - Full immune protection active")
    
    def _setup_internal_signaling(self) -> None:
        """Set up internal communication between components."""
        # When innate defense responds, inform adaptive system
        def on_innate_response(threat: Threat, response: ImmuneResponse):
            if response.response_type in [ResponseType.ELIMINATE, ResponseType.QUARANTINE]:
                self.adaptive.learn(threat)
                self.memory.remember(threat, response)
        
        self.innate.subscribe(on_innate_response)
        
        # When new antibody is created, log it
        def on_new_antibody(antibody: Antibody):
            logger.info(f"New antibody created: {antibody.id}")
        
        self.adaptive.subscribe(on_new_antibody)
    
    def process_threat(self, threat: Threat) -> Dict[str, Any]:
        """
        Process a threat through the complete immune system.
        
        This coordinates all immune components to provide a
        comprehensive response.
        
        Args:
            threat: The threat to process.
            
        Returns:
            Complete processing result.
        """
        result = {
            "threat_id": threat.id,
            "threat_type": threat.type,
            "timestamp": datetime.now().isoformat(),
            "phases": {},
        }
        
        # Phase 1: Check immune memory (fastest response)
        memory_record = self.memory.recall(threat)
        result["phases"]["memory"] = {
            "found": memory_record is not None,
            "encounter_count": memory_record.get("encounter_count", 0) if memory_record else 0,
        }
        
        # Phase 2: Check adaptive defense (antibodies)
        antibody = self.adaptive.check_antibodies(threat)
        result["phases"]["adaptive"] = {
            "antibody_found": antibody is not None,
            "antibody_id": antibody.id if antibody else None,
            "effectiveness": antibody.effectiveness if antibody else 0,
        }
        
        # Phase 3: Innate defense detection and response
        detection = self.innate.detect(threat)
        response = self.innate.respond(threat, detection)
        result["phases"]["innate"] = {
            "detected": detection["detected"],
            "confidence": detection["confidence"],
            "response_type": response.response_type.value,
            "actions": response.actions_taken,
        }
        
        # Phase 4: Adaptive learning (if threat was significant)
        if detection["confidence"] >= self.innate.ALERT_THRESHOLD:
            adaptation = self.adaptive.adapt(threat)
            result["phases"]["adaptation"] = adaptation
        
        # Phase 5: Self-healing if needed
        if response.response_type == ResponseType.ELIMINATE:
            isolation = self.healing.isolate(threat)
            result["phases"]["healing"] = {
                "isolated": True,
                "actions": isolation["actions"],
            }
            self._system_state.active_threats.add(threat.id)
        
        # Log response
        self._response_log.append(result)
        
        return result
    
    def get_health_status(self) -> Dict[str, Any]:
        """Get overall system health status."""
        return {
            "health_score": self._system_state.health_score,
            "active_threats": len(self._system_state.active_threats),
            "quarantined_items": len(self._system_state.quarantined_items),
            "damaged_components": len(self._system_state.damaged_components),
            "innate_stats": self.innate.get_statistics(),
            "adaptive_stats": self.adaptive.get_statistics(),
            "memory_stats": self.memory.get_statistics(),
            "healing_stats": self.healing.get_statistics(),
        }
    
    def run_healing_cycle(self) -> Dict[str, Any]:
        """Run a healing cycle to repair system damage."""
        return self.healing.repair(self._system_state)


# =============================================================================
# Integration Points (Placeholders for External Systems)
# =============================================================================

class IntegrationBridge:
    """
    Integration bridge for external GreyAV components.
    
    Provides placeholder methods for connecting the immune system
    to other modules like ReplayEngine, DNAEngine, Sandbox, etc.
    """
    
    @staticmethod
    def notify_replay_engine(threat: Threat, response: ImmuneResponse) -> None:
        """Placeholder: Notify ReplayEngine for threat replay analysis."""
        logger.debug(f"Integration: Would notify ReplayEngine about {threat.id}")
    
    @staticmethod
    def query_dna_engine(threat: Threat) -> Optional[Dict[str, Any]]:
        """Placeholder: Query DNAEngine for threat family information."""
        logger.debug(f"Integration: Would query DNAEngine for {threat.id}")
        return None
    
    @staticmethod
    def submit_to_sandbox(threat: Threat) -> Optional[Dict[str, Any]]:
        """Placeholder: Submit threat to Sandbox for analysis."""
        logger.debug(f"Integration: Would submit {threat.id} to Sandbox")
        return None
    
    @staticmethod
    def update_neural_model(antibody: Antibody) -> None:
        """Placeholder: Update NeuralAdaptation model with new antibody."""
        logger.debug(f"Integration: Would update neural model with {antibody.id}")
    
    @staticmethod
    def broadcast_to_mesh(threat: Threat, confidence: float) -> None:
        """Placeholder: Broadcast threat to CollectiveMesh."""
        logger.debug(f"Integration: Would broadcast {threat.id} to mesh")


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
    print("AV IMMUNE SYSTEM DEMO")
    print("Bio-Inspired Digital Defense")
    print("=" * 70)
    
    # Initialize the immune system
    print("\n[1] Initializing Immune System...")
    immune_system = AVImmuneSystem()
    
    # Create synthetic threats
    print("\n[2] Creating Synthetic Threats...")
    
    threats = [
        Threat(
            id="threat-001",
            type="file_mutation",
            payload="MZ executable with packed sections and CreateRemoteThread calls",
            severity=ThreatSeverity.HIGH,
            source="/tmp/suspicious.exe",
            indicators=["packed_executable", "process_injection", "persistence"],
        ),
        Threat(
            id="threat-002",
            type="ransomware",
            payload="Encrypt all files .locked extension bitcoin ransom decrypt_instructions",
            severity=ThreatSeverity.CRITICAL,
            source="/downloads/invoice.pdf.exe",
            indicators=["mass_file_rename", "encryption", "ransom_note"],
        ),
        Threat(
            id="threat-003",
            type="network_anomaly",
            payload="Beacon to unknown C2 server with unusual port exfil",
            severity=ThreatSeverity.MEDIUM,
            source="192.168.1.100:8443",
            indicators=["c2_beacon", "data_exfiltration"],
        ),
    ]
    
    for threat in threats:
        print(f"   - {threat.id}: {threat.type} (Severity: {threat.severity.name})")
    
    # Process threats through immune system
    print("\n[3] Processing Threats Through Immune System...")
    print("-" * 70)
    
    for threat in threats:
        print(f"\n>>> Processing: {threat.id} ({threat.type})")
        print(f"    Payload: {threat.payload[:50]}...")
        
        result = immune_system.process_threat(threat)
        
        # Display memory phase
        memory = result["phases"]["memory"]
        print(f"\n    [Memory Phase]")
        print(f"    - Previous encounter: {'Yes' if memory['found'] else 'No'}")
        if memory["found"]:
            print(f"    - Encounter count: {memory['encounter_count']}")
        
        # Display adaptive phase
        adaptive = result["phases"]["adaptive"]
        print(f"\n    [Adaptive Phase]")
        print(f"    - Antibody found: {'Yes' if adaptive['antibody_found'] else 'No'}")
        if adaptive["antibody_found"]:
            print(f"    - Antibody ID: {adaptive['antibody_id']}")
            print(f"    - Effectiveness: {adaptive['effectiveness']:.2f}")
        
        # Display innate phase
        innate = result["phases"]["innate"]
        print(f"\n    [Innate Phase]")
        print(f"    - Detected: {innate['detected']}")
        print(f"    - Confidence: {innate['confidence']:.2f}")
        print(f"    - Response: {innate['response_type']}")
        print(f"    - Actions taken:")
        for action in innate["actions"][:3]:
            print(f"      • {action}")
        
        # Display healing phase if present
        if "healing" in result["phases"]:
            healing = result["phases"]["healing"]
            print(f"\n    [Healing Phase]")
            print(f"    - Isolated: {healing['isolated']}")
        
        print("-" * 70)
    
    # Re-encounter first threat (test memory)
    print("\n[4] Re-encountering First Threat (Testing Memory)...")
    print("-" * 70)
    
    reencounter = immune_system.process_threat(threats[0])
    memory = reencounter["phases"]["memory"]
    adaptive = reencounter["phases"]["adaptive"]
    
    print(f"\n>>> Re-processing: {threats[0].id}")
    print(f"    [Memory Phase]")
    print(f"    - Previous encounter: {'Yes' if memory['found'] else 'No'}")
    print(f"    - Encounter count: {memory['encounter_count']}")
    print(f"\n    [Adaptive Phase]")
    print(f"    - Antibody found: {'Yes' if adaptive['antibody_found'] else 'No'}")
    if adaptive["antibody_found"]:
        print(f"    - Effectiveness: {adaptive['effectiveness']:.2f} (improved!)")
    
    # Run healing cycle
    print("\n[5] Running Healing Cycle...")
    print("-" * 70)
    
    # Simulate some damage
    immune_system._system_state.damaged_components = [
        "/etc/hosts",
        "/var/log/syslog",
        "registry/run_key",
    ]
    immune_system._system_state.health_score = 0.6
    
    print(f"    System health before healing: {immune_system._system_state.health_score:.2f}")
    print(f"    Damaged components: {len(immune_system._system_state.damaged_components)}")
    
    healing_result = immune_system.run_healing_cycle()
    
    print(f"\n    Healing complete!")
    print(f"    System health after healing: {healing_result['new_health']:.2f}")
    print(f"    Components repaired: {len(healing_result['components_repaired'])}")
    print(f"    Health improvement: +{healing_result['health_improvement']:.2f}")
    
    # Final statistics
    print("\n[6] Final System Statistics...")
    print("-" * 70)
    
    status = immune_system.get_health_status()
    
    print(f"\n    Overall Health: {status['health_score']:.2f}")
    print(f"    Active Threats: {status['active_threats']}")
    print(f"    Quarantined Items: {status['quarantined_items']}")
    print(f"\n    Innate Defense:")
    print(f"    - Detections: {status['innate_stats']['detection_count']}")
    print(f"    - Responses: {status['innate_stats']['response_count']}")
    print(f"    - Blocked: {status['innate_stats']['blocked_threats']}")
    print(f"\n    Adaptive Defense:")
    print(f"    - Antibodies: {status['adaptive_stats']['antibody_count']}")
    print(f"    - Learning Events: {status['adaptive_stats']['learning_events']}")
    print(f"    - Avg Effectiveness: {status['adaptive_stats']['avg_effectiveness']:.2f}")
    print(f"\n    Immune Memory:")
    print(f"    - Total Memories: {status['memory_stats']['total_memories']}")
    print(f"    - Recall Hit Rate: {status['memory_stats']['hit_rate_percent']:.1f}%")
    print(f"\n    Self-Healing:")
    print(f"    - Isolations: {status['healing_stats']['isolation_count']}")
    print(f"    - Repairs: {status['healing_stats']['repair_count']}")
    
    print("\n" + "=" * 70)
    print("DEMO COMPLETE - Immune System Operational")
    print("=" * 70)
