"""
Adaptive Threat DNA Engine for GreyAV EDR System.

This module builds behavioral fingerprints from event sequences, clusters
them into attack families, tracks lineage across variants, and detects
new threat variants or family relationships.
"""

import uuid
import hashlib
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, Dict, Any, List, Set, Tuple, Callable

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


@dataclass
class Event:
    """
    Represents a system event for fingerprint generation.
    
    Attributes:
        event_id: Unique identifier for the event.
        event_type: Type of event (process_spawn, file_write, network_connect, etc.).
        category: High-level category (execution, persistence, exfiltration, etc.).
        source: The entity initiating the event.
        target: The entity affected by the event.
        timestamp: When the event occurred.
        metadata: Additional context data.
    """
    event_id: str
    event_type: str
    category: str
    source: str
    target: str
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class DNAAlert:
    """
    Emitted when new variants or family links are detected.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        fingerprint: The behavioral fingerprint that triggered the alert.
        family_id: The attack family this fingerprint belongs to.
        lineage: Chronological list of related fingerprints in this family.
        description: Human-readable description of the detection.
        timestamp: When the alert was generated.
        similarity_score: How similar this fingerprint is to family members.
        is_new_variant: Whether this is a previously unseen variant.
        metadata: Additional context about the detection.
    """
    alert_id: str
    fingerprint: str
    family_id: str
    lineage: List[str]
    description: str
    timestamp: datetime = field(default_factory=datetime.now)
    similarity_score: float = 0.0
    is_new_variant: bool = False
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class FingerprintMetadata:
    """
    Metadata associated with a fingerprint.
    
    Attributes:
        fingerprint: The fingerprint string.
        event_types: Set of event types in this fingerprint.
        categories: Set of categories in this fingerprint.
        event_count: Number of events that generated this fingerprint.
        first_seen: When this fingerprint was first observed.
        last_seen: When this fingerprint was last observed.
        occurrence_count: How many times this fingerprint has been seen.
        family_id: The family this fingerprint belongs to.
        risk_score: Calculated risk score.
    """
    fingerprint: str
    event_types: Set[str]
    categories: Set[str]
    event_count: int
    first_seen: datetime = field(default_factory=datetime.now)
    last_seen: datetime = field(default_factory=datetime.now)
    occurrence_count: int = 1
    family_id: Optional[str] = None
    risk_score: float = 0.0


class ThreatDNAEngine:
    """
    Adaptive Threat DNA Engine for behavioral fingerprinting.
    
    Builds behavioral fingerprints from event sequences, clusters them
    into attack families using similarity metrics, and tracks lineage
    across threat variants.
    
    Attributes:
        fingerprints: Dictionary mapping fingerprint strings to metadata.
        families: Dictionary mapping family IDs to lists of fingerprints.
        subscribers: Callbacks notified when DNA alerts are generated.
    """
    
    # Category risk weights for fingerprint scoring
    CATEGORY_RISK_WEIGHTS: Dict[str, float] = {
        "execution": 0.3,
        "persistence": 0.5,
        "privilege_escalation": 0.7,
        "defense_evasion": 0.6,
        "credential_access": 0.8,
        "discovery": 0.2,
        "lateral_movement": 0.7,
        "collection": 0.4,
        "exfiltration": 0.9,
        "command_and_control": 0.8,
        "impact": 0.9,
    }
    
    # Event type to category mapping
    EVENT_CATEGORY_MAP: Dict[str, str] = {
        "process_spawn": "execution",
        "script_execution": "execution",
        "file_write": "persistence",
        "registry_write": "persistence",
        "scheduled_task": "persistence",
        "service_install": "persistence",
        "privilege_escalation": "privilege_escalation",
        "token_manipulation": "privilege_escalation",
        "process_injection": "defense_evasion",
        "file_delete": "defense_evasion",
        "log_clear": "defense_evasion",
        "credential_dump": "credential_access",
        "keylog": "credential_access",
        "system_info": "discovery",
        "network_scan": "discovery",
        "remote_execution": "lateral_movement",
        "file_copy": "collection",
        "archive_create": "collection",
        "network_connect": "command_and_control",
        "dns_query": "command_and_control",
        "data_upload": "exfiltration",
        "encrypt": "impact",
        "file_modify": "impact",
    }
    
    # Similarity threshold for family assignment
    SIMILARITY_THRESHOLD: float = 0.5
    
    def __init__(self):
        """Initialize the Threat DNA Engine."""
        self.fingerprints: Dict[str, FingerprintMetadata] = {}
        self.families: Dict[str, List[str]] = {}
        self.subscribers: List[Callable[[DNAAlert], None]] = []
        self._alert_history: List[DNAAlert] = []
        logger.info("ThreatDNAEngine initialized")
    
    def subscribe(self, callback: Callable[[DNAAlert], None]) -> None:
        """
        Subscribe to DNA alert notifications.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self.subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[DNAAlert], None]) -> None:
        """
        Unsubscribe from DNA alert notifications.
        
        Args:
            callback: Previously registered callback to remove.
        """
        if callback in self.subscribers:
            self.subscribers.remove(callback)
    
    def _notify_subscribers(self, alert: DNAAlert) -> None:
        """Notify all subscribers of a new alert."""
        for callback in self.subscribers:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def _get_category(self, event_type: str) -> str:
        """
        Get the category for an event type.
        
        Args:
            event_type: The event type string.
            
        Returns:
            Category string.
        """
        return self.EVENT_CATEGORY_MAP.get(event_type, "unknown")
    
    def build_fingerprint(self, events: List[Event]) -> str:
        """
        Build a behavioral fingerprint from a sequence of events.
        
        Creates a compact hash representation of the event sequence
        by combining event types and categories.
        
        Args:
            events: List of events to fingerprint.
            
        Returns:
            Fingerprint string (hex hash).
        """
        if not events:
            return ""
        
        # Sort events by timestamp for consistent fingerprinting
        sorted_events = sorted(events, key=lambda e: e.timestamp)
        
        # Build fingerprint components
        components = []
        for event in sorted_events:
            # Use event type and category
            category = event.category or self._get_category(event.event_type)
            component = f"{event.event_type}:{category}"
            components.append(component)
        
        # Create fingerprint string
        fingerprint_data = "|".join(components)
        
        # Hash to create compact fingerprint
        fingerprint_hash = hashlib.sha256(fingerprint_data.encode()).hexdigest()[:16]
        
        # Store metadata
        event_types = {e.event_type for e in events}
        categories = {e.category or self._get_category(e.event_type) for e in events}
        
        if fingerprint_hash in self.fingerprints:
            # Update existing fingerprint
            meta = self.fingerprints[fingerprint_hash]
            meta.last_seen = datetime.now()
            meta.occurrence_count += 1
        else:
            # Create new fingerprint entry
            self.fingerprints[fingerprint_hash] = FingerprintMetadata(
                fingerprint=fingerprint_hash,
                event_types=event_types,
                categories=categories,
                event_count=len(events),
            )
        
        logger.debug(f"Built fingerprint: {fingerprint_hash} from {len(events)} events")
        return fingerprint_hash
    
    def _calculate_jaccard_similarity(
        self,
        set1: Set[str],
        set2: Set[str]
    ) -> float:
        """
        Calculate Jaccard similarity index between two sets.
        
        Args:
            set1: First set of strings.
            set2: Second set of strings.
            
        Returns:
            Similarity score between 0.0 and 1.0.
        """
        if not set1 and not set2:
            return 1.0
        if not set1 or not set2:
            return 0.0
        
        intersection = len(set1 & set2)
        union = len(set1 | set2)
        
        return intersection / union if union > 0 else 0.0
    
    def _calculate_fingerprint_similarity(
        self,
        fp1: str,
        fp2: str
    ) -> float:
        """
        Calculate similarity between two fingerprints.
        
        Uses Jaccard similarity on event types and categories.
        
        Args:
            fp1: First fingerprint.
            fp2: Second fingerprint.
            
        Returns:
            Similarity score between 0.0 and 1.0.
        """
        if fp1 not in self.fingerprints or fp2 not in self.fingerprints:
            return 0.0
        
        meta1 = self.fingerprints[fp1]
        meta2 = self.fingerprints[fp2]
        
        # Calculate similarity based on event types and categories
        type_similarity = self._calculate_jaccard_similarity(
            meta1.event_types, meta2.event_types
        )
        category_similarity = self._calculate_jaccard_similarity(
            meta1.categories, meta2.categories
        )
        
        # Weight category similarity higher
        return 0.4 * type_similarity + 0.6 * category_similarity
    
    def assign_family(self, fingerprint: str) -> str:
        """
        Assign a fingerprint to an attack family.
        
        Clusters fingerprints by similarity using Jaccard index.
        Creates a new family if no similar family exists.
        
        Args:
            fingerprint: The fingerprint to assign.
            
        Returns:
            Family ID string.
        """
        if fingerprint not in self.fingerprints:
            logger.warning(f"Unknown fingerprint: {fingerprint}")
            return ""
        
        meta = self.fingerprints[fingerprint]
        
        # Check if already assigned
        if meta.family_id:
            return meta.family_id
        
        # Find best matching family
        best_family = None
        best_similarity = 0.0
        
        for family_id, family_fingerprints in self.families.items():
            # Calculate average similarity to family members
            if not family_fingerprints:
                continue
            
            total_similarity = sum(
                self._calculate_fingerprint_similarity(fingerprint, fp)
                for fp in family_fingerprints
            )
            avg_similarity = total_similarity / len(family_fingerprints)
            
            if avg_similarity > best_similarity:
                best_similarity = avg_similarity
                best_family = family_id
        
        # Assign to existing family or create new one
        if best_family and best_similarity >= self.SIMILARITY_THRESHOLD:
            family_id = best_family
            logger.info(
                f"Fingerprint {fingerprint} assigned to existing family "
                f"{family_id} (similarity: {best_similarity:.2f})"
            )
        else:
            # Create new family
            family_id = f"FAM-{uuid.uuid4().hex[:8].upper()}"
            self.families[family_id] = []
            logger.info(f"Created new family {family_id} for fingerprint {fingerprint}")
        
        # Add fingerprint to family
        self.families[family_id].append(fingerprint)
        meta.family_id = family_id
        
        return family_id
    
    def update_lineage(self, family_id: str, fingerprint: str) -> List[str]:
        """
        Update and retrieve the lineage for a fingerprint.
        
        Returns the chronological list of fingerprints in the family
        leading up to and including the given fingerprint.
        
        Args:
            family_id: The family to get lineage from.
            fingerprint: The current fingerprint.
            
        Returns:
            List of fingerprints in chronological order.
        """
        if family_id not in self.families:
            return [fingerprint]
        
        family_fingerprints = self.families[family_id]
        
        # Sort by first_seen timestamp
        sorted_fps = sorted(
            family_fingerprints,
            key=lambda fp: self.fingerprints[fp].first_seen
            if fp in self.fingerprints else datetime.now()
        )
        
        # Return lineage up to and including current fingerprint
        if fingerprint in sorted_fps:
            idx = sorted_fps.index(fingerprint)
            return sorted_fps[:idx + 1]
        
        return sorted_fps + [fingerprint]
    
    def _calculate_risk_score(self, fingerprint: str) -> float:
        """
        Calculate risk score for a fingerprint.
        
        Args:
            fingerprint: The fingerprint to score.
            
        Returns:
            Risk score between 0.0 and 1.0.
        """
        if fingerprint not in self.fingerprints:
            return 0.0
        
        meta = self.fingerprints[fingerprint]
        
        # Calculate based on category risk weights
        category_scores = [
            self.CATEGORY_RISK_WEIGHTS.get(cat, 0.1)
            for cat in meta.categories
        ]
        
        if not category_scores:
            return 0.1
        
        # Use max category risk as base
        base_score = max(category_scores)
        
        # Add bonus for multiple high-risk categories
        high_risk_count = sum(1 for s in category_scores if s >= 0.6)
        bonus = min(high_risk_count * 0.05, 0.2)
        
        return min(base_score + bonus, 1.0)
    
    def analyze_events(self, events: List[Event]) -> DNAAlert:
        """
        Analyze a sequence of events and generate a DNA alert.
        
        Builds a fingerprint, assigns to a family, updates lineage,
        and generates an alert with threat intelligence.
        
        Args:
            events: List of events to analyze.
            
        Returns:
            DNAAlert with fingerprint and family information.
        """
        # Build fingerprint
        fingerprint = self.build_fingerprint(events)
        
        if not fingerprint:
            return DNAAlert(
                alert_id=str(uuid.uuid4()),
                fingerprint="",
                family_id="",
                lineage=[],
                description="No events to analyze",
            )
        
        # Check if this is a new variant
        meta = self.fingerprints[fingerprint]
        is_new = meta.occurrence_count == 1
        
        # Assign to family
        family_id = self.assign_family(fingerprint)
        
        # Update lineage
        lineage = self.update_lineage(family_id, fingerprint)
        
        # Calculate risk score
        risk_score = self._calculate_risk_score(fingerprint)
        meta.risk_score = risk_score
        
        # Find similarity to family
        similarity_score = 0.0
        if len(lineage) > 1:
            parent_fp = lineage[-2]
            similarity_score = self._calculate_fingerprint_similarity(
                fingerprint, parent_fp
            )
        
        # Build description
        if is_new:
            if len(lineage) > 1:
                description = (
                    f"New variant detected in family {family_id}. "
                    f"Fingerprint: {fingerprint}. "
                    f"Lineage depth: {len(lineage)}. "
                    f"Similarity to parent: {similarity_score:.2f}. "
                    f"Categories: {', '.join(meta.categories)}"
                )
            else:
                description = (
                    f"New threat family {family_id} created. "
                    f"Fingerprint: {fingerprint}. "
                    f"Categories: {', '.join(meta.categories)}"
                )
        else:
            description = (
                f"Known variant detected. Family: {family_id}. "
                f"Fingerprint: {fingerprint}. "
                f"Occurrences: {meta.occurrence_count}"
            )
        
        # Create alert
        alert = DNAAlert(
            alert_id=str(uuid.uuid4()),
            fingerprint=fingerprint,
            family_id=family_id,
            lineage=lineage,
            description=description,
            similarity_score=similarity_score,
            is_new_variant=is_new,
            metadata={
                "event_types": list(meta.event_types),
                "categories": list(meta.categories),
                "event_count": meta.event_count,
                "risk_score": risk_score,
            },
        )
        
        # Store and notify
        self._alert_history.append(alert)
        self._notify_subscribers(alert)
        
        logger.info(
            f"DNA analysis complete: {fingerprint} -> {family_id} "
            f"(new={is_new}, risk={risk_score:.2f})"
        )
        
        return alert
    
    def get_family_info(self, family_id: str) -> Dict[str, Any]:
        """
        Get detailed information about a threat family.
        
        Args:
            family_id: The family to get info for.
            
        Returns:
            Dictionary with family information.
        """
        if family_id not in self.families:
            return {}
        
        family_fps = self.families[family_id]
        
        # Aggregate statistics
        all_event_types: Set[str] = set()
        all_categories: Set[str] = set()
        total_occurrences = 0
        risk_scores = []
        
        for fp in family_fps:
            if fp in self.fingerprints:
                meta = self.fingerprints[fp]
                all_event_types.update(meta.event_types)
                all_categories.update(meta.categories)
                total_occurrences += meta.occurrence_count
                risk_scores.append(meta.risk_score)
        
        return {
            "family_id": family_id,
            "variant_count": len(family_fps),
            "fingerprints": family_fps,
            "event_types": list(all_event_types),
            "categories": list(all_categories),
            "total_occurrences": total_occurrences,
            "avg_risk_score": sum(risk_scores) / len(risk_scores) if risk_scores else 0,
            "max_risk_score": max(risk_scores) if risk_scores else 0,
        }
    
    def get_related_families(self, family_id: str) -> List[Tuple[str, float]]:
        """
        Find families related to the given family by similarity.
        
        Args:
            family_id: The family to find relations for.
            
        Returns:
            List of (family_id, similarity_score) tuples.
        """
        if family_id not in self.families:
            return []
        
        source_fps = self.families[family_id]
        if not source_fps:
            return []
        
        related = []
        
        for other_id, other_fps in self.families.items():
            if other_id == family_id or not other_fps:
                continue
            
            # Calculate cross-family similarity
            total_sim = 0.0
            comparisons = 0
            
            for fp1 in source_fps:
                for fp2 in other_fps:
                    total_sim += self._calculate_fingerprint_similarity(fp1, fp2)
                    comparisons += 1
            
            avg_sim = total_sim / comparisons if comparisons > 0 else 0.0
            
            if avg_sim >= 0.3:  # Minimum threshold for relation
                related.append((other_id, avg_sim))
        
        # Sort by similarity
        related.sort(key=lambda x: x[1], reverse=True)
        return related
    
    def search_fingerprints(
        self,
        event_types: Optional[Set[str]] = None,
        categories: Optional[Set[str]] = None,
        min_risk: float = 0.0
    ) -> List[str]:
        """
        Search for fingerprints matching criteria.
        
        Args:
            event_types: Filter by event types (any match).
            categories: Filter by categories (any match).
            min_risk: Minimum risk score.
            
        Returns:
            List of matching fingerprint strings.
        """
        results = []
        
        for fp, meta in self.fingerprints.items():
            # Check risk score
            if meta.risk_score < min_risk:
                continue
            
            # Check event types
            if event_types and not (meta.event_types & event_types):
                continue
            
            # Check categories
            if categories and not (meta.categories & categories):
                continue
            
            results.append(fp)
        
        return results
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get engine statistics.
        
        Returns:
            Dictionary with engine statistics.
        """
        all_categories: Set[str] = set()
        all_event_types: Set[str] = set()
        
        for meta in self.fingerprints.values():
            all_categories.update(meta.categories)
            all_event_types.update(meta.event_types)
        
        return {
            "total_fingerprints": len(self.fingerprints),
            "total_families": len(self.families),
            "unique_categories": list(all_categories),
            "unique_event_types": list(all_event_types),
            "alerts_generated": len(self._alert_history),
        }
    
    def export_families(self) -> Dict[str, Any]:
        """
        Export all family data for persistence or analysis.
        
        Returns:
            Dictionary with all family and fingerprint data.
        """
        return {
            "families": {
                fid: self.get_family_info(fid)
                for fid in self.families
            },
            "fingerprints": {
                fp: {
                    "event_types": list(meta.event_types),
                    "categories": list(meta.categories),
                    "event_count": meta.event_count,
                    "first_seen": meta.first_seen.isoformat(),
                    "last_seen": meta.last_seen.isoformat(),
                    "occurrence_count": meta.occurrence_count,
                    "family_id": meta.family_id,
                    "risk_score": meta.risk_score,
                }
                for fp, meta in self.fingerprints.items()
            },
        }
    
    def clear(self) -> None:
        """Clear all fingerprints and families."""
        self.fingerprints.clear()
        self.families.clear()
        self._alert_history.clear()
        logger.info("ThreatDNAEngine cleared")


# Factory function for pipeline integration
def create_dna_engine() -> ThreatDNAEngine:
    """
    Factory function to create a ThreatDNAEngine instance.
    
    Returns:
        New ThreatDNAEngine instance.
    """
    return ThreatDNAEngine()


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    engine = create_dna_engine()
    
    # Subscribe to alerts
    def on_alert(alert: DNAAlert):
        status = "NEW VARIANT" if alert.is_new_variant else "KNOWN"
        print(f">>> [{status}] Family: {alert.family_id}")
        print(f"    Fingerprint: {alert.fingerprint}")
        print(f"    Lineage: {' -> '.join(alert.lineage)}")
        print(f"    {alert.description}\n")
    
    engine.subscribe(on_alert)
    
    # Simulate attack chain 1: Ransomware
    print("=== Analyzing Attack Chain 1: Ransomware ===\n")
    ransomware_events = [
        Event("e1", "process_spawn", "execution", "explorer.exe", "malware.exe"),
        Event("e2", "file_write", "persistence", "malware.exe", "startup.lnk"),
        Event("e3", "file_modify", "impact", "malware.exe", "document.docx"),
        Event("e4", "encrypt", "impact", "malware.exe", "document.docx.locked"),
        Event("e5", "network_connect", "command_and_control", "malware.exe", "10.0.0.1:443"),
    ]
    alert1 = engine.analyze_events(ransomware_events)
    
    # Simulate attack chain 2: Ransomware variant
    print("=== Analyzing Attack Chain 2: Ransomware Variant ===\n")
    ransomware_variant = [
        Event("e6", "process_spawn", "execution", "cmd.exe", "ransom2.exe"),
        Event("e7", "registry_write", "persistence", "ransom2.exe", "HKCU\\Run\\malware"),
        Event("e8", "file_modify", "impact", "ransom2.exe", "photo.jpg"),
        Event("e9", "encrypt", "impact", "ransom2.exe", "photo.jpg.encrypted"),
        Event("e10", "dns_query", "command_and_control", "ransom2.exe", "evil.com"),
    ]
    alert2 = engine.analyze_events(ransomware_variant)
    
    # Simulate attack chain 3: Data exfiltration
    print("=== Analyzing Attack Chain 3: Data Exfiltration ===\n")
    exfil_events = [
        Event("e11", "process_spawn", "execution", "powershell.exe", "collector.exe"),
        Event("e12", "file_copy", "collection", "collector.exe", "sensitive.zip"),
        Event("e13", "archive_create", "collection", "collector.exe", "data.zip"),
        Event("e14", "network_connect", "command_and_control", "collector.exe", "192.168.1.100:8080"),
        Event("e15", "data_upload", "exfiltration", "collector.exe", "ftp://evil.com/data.zip"),
    ]
    alert3 = engine.analyze_events(exfil_events)
    
    # Simulate repeat of first ransomware
    print("=== Analyzing Attack Chain 4: Same Ransomware Again ===\n")
    alert4 = engine.analyze_events(ransomware_events)
    
    print("=== Engine Statistics ===")
    stats = engine.get_statistics()
    print(f"Total fingerprints: {stats['total_fingerprints']}")
    print(f"Total families: {stats['total_families']}")
    print(f"Unique categories: {stats['unique_categories']}")
    
    print("\n=== Family Information ===")
    for family_id in engine.families:
        info = engine.get_family_info(family_id)
        print(f"\nFamily: {family_id}")
        print(f"  Variants: {info['variant_count']}")
        print(f"  Categories: {info['categories']}")
        print(f"  Avg Risk: {info['avg_risk_score']:.2f}")
    
    print("\n=== Related Families ===")
    for family_id in engine.families:
        related = engine.get_related_families(family_id)
        if related:
            print(f"{family_id} related to:")
            for rel_id, sim in related:
                print(f"  - {rel_id} (similarity: {sim:.2f})")
