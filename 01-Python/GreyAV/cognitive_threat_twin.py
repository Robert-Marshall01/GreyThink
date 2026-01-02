"""
Cognitive Threat Twin Subsystem
===============================

This module implements a biological-inspired digital twin system for threat analysis.
Just as the immune system creates memory cells to recognize and respond to pathogens,
this subsystem creates lightweight digital twins of detected threats to simulate and
predict their behavior in a controlled environment.

Biological Analogy:
- ThreatTwin = A cloned cell specimen for laboratory observation
- CognitiveThreatTwin = The immunology laboratory that studies threat behavior
- Simulation = Observing pathogen behavior in a controlled petri dish
- Confidence Score = Accuracy of immune response prediction
"""

from __future__ import annotations

import uuid
import hashlib
import random
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


@dataclass
class ThreatTwin:
    """
    A digital twin representing a cloned threat specimen.
    
    Biological Analogy:
    -------------------
    Like a cell sample extracted for laboratory analysis, a ThreatTwin is an
    isolated representation of a detected threat. It contains all the genetic
    information (metadata) needed to simulate and predict the threat's behavior
    without exposing the host system to actual danger.
    
    Attributes:
        id: Unique identifier for this twin specimen.
        original_threat_id: Reference to the original threat (the "parent cell").
        simulation_state: Current state of observation ("initial", "running", "completed").
        predicted_actions: List of behaviors predicted during simulation.
        confidence_score: How confident we are in our predictions (0.0 to 1.0).
        timestamp: When this twin was created (specimen collection time).
        notes: Additional observations about the twin specimen.
    """
    id: str
    original_threat_id: str
    simulation_state: str = "initial"
    predicted_actions: list[str] = field(default_factory=list)
    confidence_score: float = 0.0
    timestamp: datetime = field(default_factory=datetime.now)
    notes: str = ""


class CognitiveThreatTwin:
    """
    The Cognitive Threat Twin laboratory for threat behavior simulation.
    
    Biological Analogy:
    -------------------
    This class represents an advanced immunology laboratory where threat specimens
    (digital twins) are cultivated, observed, and analyzed. Like studying pathogens
    in controlled conditions to develop vaccines, we simulate threat behavior to
    predict and prepare defenses against malicious actions.
    
    The laboratory maintains:
    - A collection of twin specimens (twins)
    - Detailed observation logs (simulation_log)
    
    Attributes:
        twins: Collection of all created threat twin specimens.
        simulation_log: Detailed logs of simulation observations per twin.
    """
    
    # Predefined behavior patterns based on threat categories
    # These represent known "pathogen behaviors" from our threat database
    THREAT_BEHAVIOR_PATTERNS: dict[str, list[str]] = {
        "ransomware": [
            "enumerate_files",
            "check_shadow_copies",
            "generate_encryption_key",
            "encrypt_user_documents",
            "delete_backups",
            "drop_ransom_note",
            "establish_c2_connection",
            "exfiltrate_key_material"
        ],
        "trojan": [
            "establish_persistence",
            "disable_security_software",
            "open_backdoor_port",
            "download_secondary_payload",
            "keylog_user_input",
            "capture_screenshots",
            "exfiltrate_credentials"
        ],
        "worm": [
            "scan_network_hosts",
            "enumerate_shares",
            "exploit_vulnerabilities",
            "copy_to_remote_systems",
            "establish_persistence",
            "propagate_laterally"
        ],
        "rootkit": [
            "hook_system_calls",
            "hide_processes",
            "hide_files",
            "intercept_network_traffic",
            "modify_kernel_structures",
            "disable_logging"
        ],
        "spyware": [
            "monitor_user_activity",
            "capture_keystrokes",
            "record_audio",
            "capture_webcam",
            "harvest_browser_data",
            "exfiltrate_to_c2"
        ],
        "generic": [
            "probe_system_info",
            "check_virtualization",
            "establish_persistence",
            "communicate_with_c2",
            "execute_payload"
        ]
    }
    
    def __init__(self) -> None:
        """
        Initialize the Cognitive Threat Twin laboratory.
        
        Sets up an empty specimen collection and observation log system,
        ready to receive and analyze threat twins.
        """
        self.twins: list[ThreatTwin] = []
        self.simulation_log: dict[str, list[str]] = {}
    
    def create_twin(self, threat_id: str, description: str) -> ThreatTwin:
        """
        Create a new digital twin from a detected threat.
        
        Biological Analogy:
        -------------------
        This method is analogous to CLONING A CELL for laboratory study.
        Just as scientists extract and replicate cells to study pathogens
        without risking the host organism, we create a digital twin that
        captures the threat's characteristics for safe simulation.
        
        The cloning process:
        1. Extract threat DNA (generate unique twin ID from threat characteristics)
        2. Prepare culture medium (initialize simulation state)
        3. Place specimen in observation chamber (add to twins collection)
        4. Begin observation log (initialize simulation_log entry)
        
        Args:
            threat_id: The identifier of the original detected threat.
            description: Description of the threat (used for behavior prediction).
        
        Returns:
            ThreatTwin: A newly created twin specimen ready for simulation.
        """
        # Generate unique twin ID (like a specimen barcode)
        twin_id = self._generate_twin_id(threat_id)
        
        # Create the twin specimen
        twin = ThreatTwin(
            id=twin_id,
            original_threat_id=threat_id,
            simulation_state="initial",
            predicted_actions=[],
            confidence_score=0.0,
            timestamp=datetime.now(),
            notes=f"Twin created from threat: {description}"
        )
        
        # Register the specimen in our laboratory
        self.twins.append(twin)
        self.simulation_log[twin_id] = [
            f"[{datetime.now().isoformat()}] Twin specimen created",
            f"[{datetime.now().isoformat()}] Original threat ID: {threat_id}",
            f"[{datetime.now().isoformat()}] Description: {description}"
        ]
        
        return twin
    
    def run_simulation(self, twin: ThreatTwin) -> ThreatTwin:
        """
        Run behavioral simulation on a threat twin.
        
        Biological Analogy:
        -------------------
        This method is analogous to OBSERVING BEHAVIOR IN A PETRI DISH.
        Just as microbiologists watch pathogens grow and interact in
        controlled culture conditions, we simulate the threat's execution
        in an isolated environment to predict its actions.
        
        The observation process:
        1. Prepare the petri dish (set simulation state to "running")
        2. Introduce growth medium (analyze threat characteristics)
        3. Observe and record behaviors (generate predicted actions)
        4. Complete observation cycle (set state to "completed")
        
        Args:
            twin: The ThreatTwin specimen to simulate.
        
        Returns:
            ThreatTwin: The twin with updated predicted_actions and state.
        """
        # Begin observation
        twin.simulation_state = "running"
        self._log(twin.id, "Simulation started - observing specimen behavior")
        
        # Analyze the threat description to determine behavior pattern
        threat_category = self._classify_threat(twin.notes)
        self._log(twin.id, f"Threat classified as: {threat_category}")
        
        # Get base behavior pattern for this threat type
        base_behaviors = self.THREAT_BEHAVIOR_PATTERNS.get(
            threat_category,
            self.THREAT_BEHAVIOR_PATTERNS["generic"]
        )
        
        # Simulate behavioral evolution (some threats may exhibit subset of behaviors)
        # This models the stochastic nature of pathogen behavior
        num_behaviors = random.randint(
            max(2, len(base_behaviors) // 2),
            len(base_behaviors)
        )
        predicted_actions = random.sample(base_behaviors, num_behaviors)
        
        # Add some threat-specific mutations (unique behaviors)
        mutation_actions = self._generate_mutations(twin.original_threat_id)
        predicted_actions.extend(mutation_actions)
        
        # Record observations
        twin.predicted_actions = predicted_actions
        for action in predicted_actions:
            self._log(twin.id, f"Predicted behavior observed: {action}")
        
        # Complete observation cycle
        twin.simulation_state = "completed"
        self._log(twin.id, "Simulation completed - all behaviors recorded")
        
        # Evaluate confidence in our predictions
        twin.confidence_score = self.evaluate_confidence(twin)
        
        return twin
    
    def evaluate_confidence(self, twin: ThreatTwin) -> float:
        """
        Evaluate confidence score for the twin's predicted behaviors.
        
        Biological Analogy:
        -------------------
        This method is analogous to MEASURING IMMUNE RESPONSE ACCURACY.
        Just as immunologists assess how accurately antibodies recognize
        and respond to pathogens, we evaluate how confident we are that
        our behavioral predictions match actual threat capabilities.
        
        Confidence factors:
        1. Sample quality (simulation completeness)
        2. Pattern match strength (how well threat fits known patterns)
        3. Prediction consistency (behavioral coherence)
        4. Historical accuracy (based on threat category)
        
        Args:
            twin: The ThreatTwin to evaluate.
        
        Returns:
            float: Confidence score between 0.0 (no confidence) and 1.0 (certain).
        """
        confidence = 0.0
        
        # Factor 1: Simulation completeness (was simulation run?)
        if twin.simulation_state == "completed":
            confidence += 0.3
            self._log(twin.id, "Confidence +0.30: Simulation completed successfully")
        elif twin.simulation_state == "running":
            confidence += 0.1
            self._log(twin.id, "Confidence +0.10: Simulation in progress")
        
        # Factor 2: Number of predicted actions (richer predictions = higher confidence)
        action_count = len(twin.predicted_actions)
        action_confidence = min(0.3, action_count * 0.05)
        confidence += action_confidence
        self._log(twin.id, f"Confidence +{action_confidence:.2f}: {action_count} behaviors predicted")
        
        # Factor 3: Pattern coherence (do actions form logical attack chain?)
        coherence_score = self._calculate_coherence(twin.predicted_actions)
        confidence += coherence_score * 0.25
        self._log(twin.id, f"Confidence +{coherence_score * 0.25:.2f}: Behavioral coherence factor")
        
        # Factor 4: Threat classification confidence
        threat_category = self._classify_threat(twin.notes)
        if threat_category != "generic":
            confidence += 0.15
            self._log(twin.id, "Confidence +0.15: Known threat category matched")
        
        # Normalize to 0.0 - 1.0 range
        final_confidence = min(1.0, max(0.0, confidence))
        self._log(twin.id, f"Final confidence score: {final_confidence:.2f}")
        
        return round(final_confidence, 2)
    
    def report(self, twin: ThreatTwin) -> dict[str, Any]:
        """
        Generate a comprehensive report for a threat twin.
        
        Biological Analogy:
        -------------------
        This method is analogous to generating a LAB REPORT OF FINDINGS.
        Just as a pathology report summarizes all observations about a
        specimen - its characteristics, behaviors, and risk assessment -
        this report consolidates everything we've learned about the threat.
        
        Report sections:
        1. Specimen identification (twin metadata)
        2. Behavioral observations (predicted actions)
        3. Confidence assessment (prediction reliability)
        4. Risk classification (threat severity)
        5. Recommended countermeasures (defense suggestions)
        
        Args:
            twin: The ThreatTwin to report on.
        
        Returns:
            dict: Comprehensive report containing all findings.
        """
        # Classify threat severity based on predicted actions
        severity = self._assess_severity(twin.predicted_actions)
        
        # Generate defense recommendations
        countermeasures = self._recommend_countermeasures(twin.predicted_actions)
        
        report = {
            "report_id": f"CTT-{twin.id[:8]}",
            "generated_at": datetime.now().isoformat(),
            "specimen": {
                "twin_id": twin.id,
                "original_threat_id": twin.original_threat_id,
                "created_at": twin.timestamp.isoformat(),
                "notes": twin.notes
            },
            "simulation": {
                "state": twin.simulation_state,
                "predicted_actions": twin.predicted_actions,
                "action_count": len(twin.predicted_actions)
            },
            "assessment": {
                "confidence_score": twin.confidence_score,
                "confidence_level": self._confidence_to_level(twin.confidence_score),
                "severity": severity,
                "threat_category": self._classify_threat(twin.notes)
            },
            "countermeasures": countermeasures,
            "observation_log": self.simulation_log.get(twin.id, [])
        }
        
        self._log(twin.id, f"Report generated: {report['report_id']}")
        
        return report
    
    # --- Private Helper Methods ---
    
    def _generate_twin_id(self, threat_id: str) -> str:
        """Generate a unique twin ID based on threat ID and timestamp."""
        unique_string = f"{threat_id}-{datetime.now().isoformat()}-{uuid.uuid4()}"
        return hashlib.sha256(unique_string.encode()).hexdigest()[:16]
    
    def _log(self, twin_id: str, message: str) -> None:
        """Add an entry to the simulation log for a twin."""
        if twin_id not in self.simulation_log:
            self.simulation_log[twin_id] = []
        self.simulation_log[twin_id].append(f"[{datetime.now().isoformat()}] {message}")
    
    def _classify_threat(self, description: str) -> str:
        """Classify threat based on description keywords."""
        description_lower = description.lower()
        
        threat_keywords = {
            "ransomware": ["ransom", "encrypt", "bitcoin", "decrypt", "locked"],
            "trojan": ["trojan", "backdoor", "remote access", "rat"],
            "worm": ["worm", "propagat", "spread", "replicate", "network"],
            "rootkit": ["rootkit", "kernel", "hook", "hidden", "stealth"],
            "spyware": ["spy", "monitor", "keylog", "surveillance", "track"]
        }
        
        for category, keywords in threat_keywords.items():
            if any(kw in description_lower for kw in keywords):
                return category
        
        return "generic"
    
    def _generate_mutations(self, threat_id: str) -> list[str]:
        """Generate unique behavioral mutations based on threat ID."""
        # Use threat ID as seed for reproducible "mutations"
        seed = int(hashlib.md5(threat_id.encode()).hexdigest()[:8], 16)
        random.seed(seed)
        
        mutation_pool = [
            "check_debugger_presence",
            "delay_execution",
            "verify_environment",
            "anti_sandbox_check",
            "enumerate_security_products",
            "modify_registry",
            "create_scheduled_task",
            "inject_into_process"
        ]
        
        num_mutations = random.randint(0, 3)
        mutations = random.sample(mutation_pool, num_mutations) if num_mutations > 0 else []
        
        # Reset random state
        random.seed()
        
        return mutations
    
    def _calculate_coherence(self, actions: list[str]) -> float:
        """Calculate how coherent the predicted action sequence is."""
        if not actions:
            return 0.0
        
        # Higher coherence if actions follow logical attack patterns
        coherence_pairs = [
            ("enumerate", "exploit"),
            ("disable", "execute"),
            ("establish", "exfiltrate"),
            ("scan", "propagate"),
            ("capture", "exfiltrate")
        ]
        
        matches = 0
        for first, second in coherence_pairs:
            has_first = any(first in a for a in actions)
            has_second = any(second in a for a in actions)
            if has_first and has_second:
                matches += 1
        
        return min(1.0, matches / max(1, len(coherence_pairs) // 2))
    
    def _assess_severity(self, actions: list[str]) -> str:
        """Assess threat severity based on predicted actions."""
        critical_actions = {"encrypt", "exfiltrate", "kernel", "rootkit", "delete"}
        high_actions = {"backdoor", "credential", "inject", "disable"}
        medium_actions = {"persist", "enumerate", "scan", "download"}
        
        action_str = " ".join(actions).lower()
        
        if any(ca in action_str for ca in critical_actions):
            return "CRITICAL"
        elif any(ha in action_str for ha in high_actions):
            return "HIGH"
        elif any(ma in action_str for ma in medium_actions):
            return "MEDIUM"
        else:
            return "LOW"
    
    def _confidence_to_level(self, score: float) -> str:
        """Convert numeric confidence to descriptive level."""
        if score >= 0.8:
            return "VERY HIGH"
        elif score >= 0.6:
            return "HIGH"
        elif score >= 0.4:
            return "MODERATE"
        elif score >= 0.2:
            return "LOW"
        else:
            return "VERY LOW"
    
    def _recommend_countermeasures(self, actions: list[str]) -> list[str]:
        """Generate countermeasure recommendations based on predicted actions."""
        countermeasures = []
        action_str = " ".join(actions).lower()
        
        countermeasure_map = {
            "encrypt": "Enable ransomware protection and maintain offline backups",
            "exfiltrate": "Monitor network egress and enable DLP policies",
            "persist": "Audit startup locations and scheduled tasks",
            "backdoor": "Review open ports and network connections",
            "credential": "Enable credential guard and monitor LSASS access",
            "inject": "Enable process injection detection and memory protection",
            "kernel": "Enable kernel integrity checks and secure boot",
            "disable": "Protect security software with tamper protection",
            "propagate": "Segment network and restrict lateral movement",
            "keylog": "Use hardware-backed credential entry where possible"
        }
        
        for trigger, countermeasure in countermeasure_map.items():
            if trigger in action_str:
                countermeasures.append(countermeasure)
        
        if not countermeasures:
            countermeasures.append("Continue standard monitoring and maintain security hygiene")
        
        return countermeasures


# =============================================================================
# EXAMPLE USAGE / DEMONSTRATION
# =============================================================================

if __name__ == "__main__":
    print("=" * 70)
    print("COGNITIVE THREAT TWIN - DEMONSTRATION")
    print("Digital Twin Behavioral Simulation for Threat Analysis")
    print("=" * 70)
    print()
    
    # Initialize the Cognitive Threat Twin laboratory
    lab = CognitiveThreatTwin()
    
    # Simulate detection of a synthetic threat
    synthetic_threat_id = "THREAT-2025-001-RANSOM"
    threat_description = "Suspected ransomware variant with encryption capabilities"
    
    print("[STEP 1] Creating digital twin (cloning the cell)...")
    print("-" * 50)
    twin = lab.create_twin(synthetic_threat_id, threat_description)
    print(f"  Twin ID: {twin.id}")
    print(f"  Original Threat: {twin.original_threat_id}")
    print(f"  State: {twin.simulation_state}")
    print()
    
    print("[STEP 2] Running behavioral simulation (petri dish observation)...")
    print("-" * 50)
    twin = lab.run_simulation(twin)
    print(f"  State: {twin.simulation_state}")
    print(f"  Predicted Actions ({len(twin.predicted_actions)}):")
    for action in twin.predicted_actions:
        print(f"    - {action}")
    print()
    
    print("[STEP 3] Evaluating confidence (immune response accuracy)...")
    print("-" * 50)
    print(f"  Confidence Score: {twin.confidence_score}")
    print()
    
    print("[STEP 4] Generating report (lab report of findings)...")
    print("-" * 50)
    report = lab.report(twin)
    print(f"  Report ID: {report['report_id']}")
    print(f"  Threat Category: {report['assessment']['threat_category']}")
    print(f"  Severity: {report['assessment']['severity']}")
    print(f"  Confidence Level: {report['assessment']['confidence_level']}")
    print()
    print("  Recommended Countermeasures:")
    for cm in report['countermeasures']:
        print(f"    • {cm}")
    print()
    
    print("=" * 70)
    print("DEMONSTRATION COMPLETE")
    print("=" * 70)
