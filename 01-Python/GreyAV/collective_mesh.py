"""
Collective Intelligence Mesh for GreyAV EDR System.

This module implements a distributed threat intelligence network that enables
peer-to-peer sharing of threat fingerprints, consensus-based threat validation,
and collective model updates across mesh nodes.
"""

import uuid
import hashlib
import logging
import threading
import time
import random
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Optional, Dict, Any, List, Callable, Set, Tuple
from enum import Enum

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


class SignalType(Enum):
    """Types of signals that can be shared across the mesh."""
    THREAT_FINGERPRINT = "threat_fingerprint"
    BEHAVIORAL_PATTERN = "behavioral_pattern"
    IOC = "ioc"
    MODEL_UPDATE = "model_update"
    REPUTATION_UPDATE = "reputation_update"
    ALERT = "alert"


class ConsensusState(Enum):
    """State of consensus for a signal."""
    PENDING = "pending"
    CONFIRMED = "confirmed"
    REJECTED = "rejected"
    EXPIRED = "expired"


@dataclass
class MeshAlert:
    """
    Emitted when consensus threshold is reached for a threat.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        fingerprint: Threat fingerprint that reached consensus.
        consensus_score: Final consensus score (0.0 - 1.0).
        peers_involved: List of peer IDs that contributed.
        description: Human-readable description.
        timestamp: When consensus was reached.
        signal_type: Type of signal.
        confidence_distribution: Per-peer confidence values.
        metadata: Additional context.
    """
    alert_id: str
    fingerprint: str
    consensus_score: float
    peers_involved: List[str]
    description: str
    timestamp: datetime = field(default_factory=datetime.now)
    signal_type: str = "threat_fingerprint"
    confidence_distribution: Dict[str, float] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class PeerNode:
    """
    Represents a peer node in the mesh.
    
    Attributes:
        peer_id: Unique identifier for the peer.
        address: Network address (simulated).
        trust_score: Trust level for this peer (0.0 - 1.0).
        last_seen: Last communication timestamp.
        signals_received: Count of signals received from this peer.
        is_active: Whether the peer is currently reachable.
    """
    peer_id: str
    address: str = ""
    trust_score: float = 0.5
    last_seen: datetime = field(default_factory=datetime.now)
    signals_received: int = 0
    is_active: bool = True
    capabilities: Set[str] = field(default_factory=set)


@dataclass
class Signal:
    """
    A threat intelligence signal shared across the mesh.
    
    Attributes:
        fingerprint: Unique identifier for the threat.
        signal_type: Type of signal.
        source_peer: Peer that originated the signal.
        confidence: Reported confidence level.
        timestamp: When the signal was created.
        votes: Per-peer votes/confidences.
        metadata: Signal-specific data.
    """
    fingerprint: str
    signal_type: str
    source_peer: str
    confidence: float
    timestamp: datetime = field(default_factory=datetime.now)
    votes: Dict[str, float] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)
    consensus_state: str = "pending"


class CollectiveMesh:
    """
    Collective Intelligence Mesh for distributed threat intelligence.
    
    Enables peer-to-peer sharing of threat fingerprints, consensus-based
    validation, and collective model updates across the mesh network.
    
    Attributes:
        node_id: This node's unique identifier.
        peers: List of peer node identifiers.
        signals: Dictionary of fingerprint → Signal metadata.
    """
    
    # Consensus thresholds
    CONSENSUS_THRESHOLD: float = 0.7
    MIN_PEERS_FOR_CONSENSUS: int = 2
    SIGNAL_EXPIRY_HOURS: int = 24
    
    def __init__(
        self,
        node_id: Optional[str] = None,
        consensus_threshold: float = 0.7
    ):
        """
        Initialize the Collective Mesh.
        
        Args:
            node_id: Unique identifier for this node. Auto-generated if not provided.
            consensus_threshold: Threshold for consensus (0.0 - 1.0).
        """
        self.node_id = node_id or f"node-{uuid.uuid4().hex[:8]}"
        self.peers: List[str] = []
        self.signals: Dict[str, Signal] = {}
        
        self._peer_nodes: Dict[str, PeerNode] = {}
        self._subscribers: List[Callable[[MeshAlert], None]] = []
        self._alert_history: List[MeshAlert] = []
        self._broadcast_queue: List[Tuple[str, Dict[str, Any]]] = []
        self._consensus_threshold = consensus_threshold
        self._lock = threading.Lock()
        self._running = False
        self._sync_thread: Optional[threading.Thread] = None
        
        # Model update callbacks
        self._model_update_callbacks: List[Callable[[str, Dict], None]] = []
        
        logger.info(f"CollectiveMesh initialized: {self.node_id}")
    
    def subscribe(self, callback: Callable[[MeshAlert], None]) -> None:
        """
        Subscribe to mesh alert notifications.
        
        Args:
            callback: Function to call when a MeshAlert is generated.
        """
        self._subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[MeshAlert], None]) -> None:
        """
        Unsubscribe from mesh alert notifications.
        
        Args:
            callback: Previously registered callback.
        """
        if callback in self._subscribers:
            self._subscribers.remove(callback)
    
    def register_model_update_callback(
        self,
        callback: Callable[[str, Dict], None]
    ) -> None:
        """
        Register callback for model updates from peers.
        
        Args:
            callback: Function(fingerprint, update_data) to call.
        """
        self._model_update_callbacks.append(callback)
    
    def _notify_subscribers(self, alert: MeshAlert) -> None:
        """Notify all subscribers of a new alert."""
        for callback in self._subscribers:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def _notify_model_update(self, fingerprint: str, update_data: Dict) -> None:
        """Notify model update callbacks."""
        for callback in self._model_update_callbacks:
            try:
                callback(fingerprint, update_data)
            except Exception as e:
                logger.error(f"Model update callback failed: {e}")
    
    def add_peer(
        self,
        peer_id: str,
        address: str = "",
        trust_score: float = 0.5
    ) -> PeerNode:
        """
        Add a peer node to the mesh.
        
        Args:
            peer_id: Unique identifier for the peer.
            address: Network address (optional).
            trust_score: Initial trust score (0.0 - 1.0).
            
        Returns:
            The created PeerNode.
        """
        with self._lock:
            if peer_id not in self._peer_nodes:
                peer = PeerNode(
                    peer_id=peer_id,
                    address=address,
                    trust_score=trust_score,
                )
                self._peer_nodes[peer_id] = peer
                self.peers.append(peer_id)
                logger.info(f"Added peer: {peer_id} (trust: {trust_score})")
                return peer
            return self._peer_nodes[peer_id]
    
    def remove_peer(self, peer_id: str) -> bool:
        """
        Remove a peer from the mesh.
        
        Args:
            peer_id: ID of the peer to remove.
            
        Returns:
            True if removed, False if not found.
        """
        with self._lock:
            if peer_id in self._peer_nodes:
                del self._peer_nodes[peer_id]
                self.peers.remove(peer_id)
                logger.info(f"Removed peer: {peer_id}")
                return True
            return False
    
    def get_peer(self, peer_id: str) -> Optional[PeerNode]:
        """Get peer node by ID."""
        return self._peer_nodes.get(peer_id)
    
    def update_peer_trust(self, peer_id: str, delta: float) -> float:
        """
        Update a peer's trust score.
        
        Args:
            peer_id: Peer to update.
            delta: Amount to adjust trust (positive or negative).
            
        Returns:
            New trust score.
        """
        if peer_id in self._peer_nodes:
            peer = self._peer_nodes[peer_id]
            peer.trust_score = max(0.0, min(1.0, peer.trust_score + delta))
            return peer.trust_score
        return 0.0
    
    def _anonymize_fingerprint(self, fingerprint: str) -> str:
        """
        Anonymize a fingerprint for privacy-preserving sharing.
        
        Args:
            fingerprint: Original fingerprint.
            
        Returns:
            Anonymized fingerprint hash.
        """
        # Add salt for privacy
        salted = f"{fingerprint}:{self.node_id}:salt"
        return hashlib.sha256(salted.encode()).hexdigest()[:32]
    
    def broadcast_signal(
        self,
        fingerprint: str,
        metadata: Dict[str, Any],
        signal_type: str = "threat_fingerprint"
    ) -> None:
        """
        Broadcast a threat signal to all peers.
        
        Shares anonymized threat fingerprints with peer nodes
        for collective validation.
        
        Args:
            fingerprint: Threat fingerprint to share.
            metadata: Associated metadata (confidence, indicators, etc.).
        """
        with self._lock:
            # Create or update local signal
            confidence = metadata.get("confidence", 0.5)
            
            if fingerprint not in self.signals:
                signal = Signal(
                    fingerprint=fingerprint,
                    signal_type=signal_type,
                    source_peer=self.node_id,
                    confidence=confidence,
                    metadata=metadata,
                )
                signal.votes[self.node_id] = confidence
                self.signals[fingerprint] = signal
            else:
                signal = self.signals[fingerprint]
                signal.votes[self.node_id] = confidence
                signal.metadata.update(metadata)
            
            # Queue for broadcast to peers
            broadcast_data = {
                "fingerprint": fingerprint,
                "confidence": confidence,
                "signal_type": signal_type,
                "metadata": metadata,
                "source": self.node_id,
                "timestamp": datetime.now().isoformat(),
            }
            
            # Simulate broadcast to all peers
            for peer_id in self.peers:
                self._simulate_send_to_peer(peer_id, broadcast_data)
            
            logger.info(
                f"Broadcast signal: {fingerprint[:16]}... to {len(self.peers)} peers"
            )
    
    def _simulate_send_to_peer(
        self,
        peer_id: str,
        data: Dict[str, Any]
    ) -> bool:
        """
        Simulate sending data to a peer.
        
        In a real implementation, this would use network protocols.
        
        Args:
            peer_id: Target peer.
            data: Data to send.
            
        Returns:
            True if successful.
        """
        peer = self._peer_nodes.get(peer_id)
        if peer and peer.is_active:
            # Simulate network delay
            # In production, this would be async network I/O
            logger.debug(f"Sent signal to peer {peer_id}")
            return True
        return False
    
    def receive_signal(
        self,
        peer: str,
        fingerprint: str,
        metadata: Dict[str, Any]
    ) -> Optional[MeshAlert]:
        """
        Receive and process a signal from a peer.
        
        Updates local knowledge and checks for consensus.
        
        Args:
            peer: ID of the sending peer.
            fingerprint: Threat fingerprint.
            metadata: Signal metadata including confidence.
            
        Returns:
            MeshAlert if consensus threshold reached, None otherwise.
        """
        with self._lock:
            # Validate peer
            if peer not in self._peer_nodes:
                # Auto-register unknown peer with low trust
                self.add_peer(peer, trust_score=0.3)
            
            peer_node = self._peer_nodes[peer]
            peer_node.last_seen = datetime.now()
            peer_node.signals_received += 1
            
            confidence = metadata.get("confidence", 0.5)
            signal_type = metadata.get("signal_type", "threat_fingerprint")
            
            # Apply trust-weighted confidence
            weighted_confidence = confidence * peer_node.trust_score
            
            # Create or update signal
            if fingerprint not in self.signals:
                signal = Signal(
                    fingerprint=fingerprint,
                    signal_type=signal_type,
                    source_peer=peer,
                    confidence=weighted_confidence,
                    metadata=metadata,
                )
                self.signals[fingerprint] = signal
            else:
                signal = self.signals[fingerprint]
            
            # Record vote
            signal.votes[peer] = weighted_confidence
            
            logger.debug(
                f"Received signal from {peer}: {fingerprint[:16]}... "
                f"(confidence: {weighted_confidence:.2f})"
            )
            
            # Check consensus
            consensus_score = self.calculate_consensus(fingerprint)
            
            if self._should_emit_alert(fingerprint, consensus_score):
                alert = self.emit_alert(fingerprint)
                signal.consensus_state = "confirmed"
                return alert
            
            return None
    
    def calculate_consensus(self, fingerprint: str) -> float:
        """
        Calculate consensus score for a fingerprint.
        
        Computes weighted average of peer votes, considering trust scores.
        
        Args:
            fingerprint: Threat fingerprint to evaluate.
            
        Returns:
            Consensus score (0.0 - 1.0).
        """
        if fingerprint not in self.signals:
            return 0.0
        
        signal = self.signals[fingerprint]
        votes = signal.votes
        
        if not votes:
            return 0.0
        
        # Calculate trust-weighted average
        total_weight = 0.0
        weighted_sum = 0.0
        
        for peer_id, confidence in votes.items():
            if peer_id == self.node_id:
                # Self vote has full weight
                weight = 1.0
            else:
                peer = self._peer_nodes.get(peer_id)
                weight = peer.trust_score if peer else 0.3
            
            weighted_sum += confidence * weight
            total_weight += weight
        
        if total_weight == 0:
            return 0.0
        
        consensus = weighted_sum / total_weight
        return min(1.0, max(0.0, consensus))
    
    def _should_emit_alert(self, fingerprint: str, consensus_score: float) -> bool:
        """Check if alert should be emitted."""
        if fingerprint not in self.signals:
            return False
        
        signal = self.signals[fingerprint]
        
        # Already confirmed
        if signal.consensus_state == "confirmed":
            return False
        
        # Check thresholds
        if consensus_score < self._consensus_threshold:
            return False
        
        if len(signal.votes) < self.MIN_PEERS_FOR_CONSENSUS:
            return False
        
        return True
    
    def emit_alert(self, fingerprint: str) -> MeshAlert:
        """
        Emit a MeshAlert for a fingerprint that reached consensus.
        
        Args:
            fingerprint: Threat fingerprint.
            
        Returns:
            Generated MeshAlert.
        """
        signal = self.signals.get(fingerprint)
        
        if signal is None:
            raise ValueError(f"No signal found for fingerprint: {fingerprint}")
        
        consensus_score = self.calculate_consensus(fingerprint)
        peers_involved = list(signal.votes.keys())
        
        # Build description
        description = (
            f"Mesh consensus reached for threat fingerprint {fingerprint[:16]}... "
            f"with score {consensus_score:.2f} from {len(peers_involved)} peers"
        )
        
        alert = MeshAlert(
            alert_id=str(uuid.uuid4()),
            fingerprint=fingerprint,
            consensus_score=consensus_score,
            peers_involved=peers_involved,
            description=description,
            signal_type=signal.signal_type,
            confidence_distribution=dict(signal.votes),
            metadata=signal.metadata,
        )
        
        # Store and notify
        self._alert_history.append(alert)
        self._notify_subscribers(alert)
        
        logger.warning(
            f"MESH ALERT: Consensus reached for {fingerprint[:16]}... "
            f"(score: {consensus_score:.2f}, peers: {len(peers_involved)})"
        )
        
        return alert
    
    def request_peer_votes(self, fingerprint: str) -> Dict[str, float]:
        """
        Request votes from all peers for a fingerprint.
        
        Args:
            fingerprint: Fingerprint to get votes for.
            
        Returns:
            Dictionary of peer_id → confidence.
        """
        votes: Dict[str, float] = {}
        
        for peer_id in self.peers:
            peer = self._peer_nodes.get(peer_id)
            if peer and peer.is_active:
                # Simulate peer response
                # In production, this would be an async network call
                simulated_confidence = random.uniform(0.3, 0.9)
                votes[peer_id] = simulated_confidence
                
                # Record the vote
                if fingerprint in self.signals:
                    weighted = simulated_confidence * peer.trust_score
                    self.signals[fingerprint].votes[peer_id] = weighted
        
        return votes
    
    def update_local_model(
        self,
        fingerprint: str,
        update_data: Dict[str, Any]
    ) -> None:
        """
        Update local model based on peer intelligence.
        
        Args:
            fingerprint: Related fingerprint.
            update_data: Model update data from peers.
        """
        logger.info(f"Applying model update for {fingerprint[:16]}...")
        
        # Notify registered callbacks
        self._notify_model_update(fingerprint, update_data)
        
        # Store update info in signal metadata
        if fingerprint in self.signals:
            self.signals[fingerprint].metadata["model_updates"] = \
                self.signals[fingerprint].metadata.get("model_updates", []) + [
                    {
                        "timestamp": datetime.now().isoformat(),
                        "update_data": update_data,
                    }
                ]
    
    def broadcast_model_update(
        self,
        fingerprint: str,
        update_data: Dict[str, Any]
    ) -> None:
        """
        Broadcast a model update to peers.
        
        Args:
            fingerprint: Related fingerprint.
            update_data: Model update data to share.
        """
        metadata = {
            "signal_type": "model_update",
            "confidence": 1.0,
            "update_data": update_data,
        }
        
        self.broadcast_signal(
            fingerprint=fingerprint,
            metadata=metadata,
            signal_type="model_update"
        )
    
    def get_peer_intelligence(
        self,
        fingerprint: str
    ) -> Dict[str, Any]:
        """
        Get aggregated intelligence from peers for a fingerprint.
        
        Args:
            fingerprint: Fingerprint to query.
            
        Returns:
            Aggregated intelligence data.
        """
        if fingerprint not in self.signals:
            return {}
        
        signal = self.signals[fingerprint]
        
        return {
            "fingerprint": fingerprint,
            "consensus_score": self.calculate_consensus(fingerprint),
            "consensus_state": signal.consensus_state,
            "peer_count": len(signal.votes),
            "votes": dict(signal.votes),
            "source_peer": signal.source_peer,
            "signal_type": signal.signal_type,
            "first_seen": signal.timestamp.isoformat(),
            "metadata": signal.metadata,
        }
    
    def sync_with_peers(self) -> int:
        """
        Synchronize signals with all active peers.
        
        Returns:
            Number of signals synchronized.
        """
        synced = 0
        
        for fingerprint, signal in list(self.signals.items()):
            # Skip expired signals
            age = datetime.now() - signal.timestamp
            if age > timedelta(hours=self.SIGNAL_EXPIRY_HOURS):
                signal.consensus_state = "expired"
                continue
            
            # Request votes from peers that haven't voted
            for peer_id in self.peers:
                if peer_id not in signal.votes:
                    peer = self._peer_nodes.get(peer_id)
                    if peer and peer.is_active:
                        # Simulate vote request/response
                        confidence = random.uniform(0.2, 0.8)
                        weighted = confidence * peer.trust_score
                        signal.votes[peer_id] = weighted
                        synced += 1
        
        logger.info(f"Synchronized {synced} signals with peers")
        return synced
    
    def run(self, sync_interval: float = 60.0) -> None:
        """
        Run continuous mesh synchronization.
        
        Args:
            sync_interval: Interval between sync operations in seconds.
        """
        if self._running:
            logger.warning("CollectiveMesh is already running")
            return
        
        self._running = True
        
        def sync_loop():
            logger.info("CollectiveMesh sync started")
            
            while self._running:
                try:
                    # Sync with peers
                    self.sync_with_peers()
                    
                    # Clean up expired signals
                    self._cleanup_expired_signals()
                    
                    # Check for new consensus
                    self._check_pending_consensus()
                    
                except Exception as e:
                    logger.error(f"Sync loop error: {e}")
                
                # Sleep in increments for quick shutdown
                for _ in range(int(sync_interval)):
                    if not self._running:
                        break
                    time.sleep(1)
            
            logger.info("CollectiveMesh sync stopped")
        
        self._sync_thread = threading.Thread(target=sync_loop, daemon=True)
        self._sync_thread.start()
    
    def stop(self) -> None:
        """Stop the synchronization loop."""
        self._running = False
        if self._sync_thread:
            self._sync_thread.join(timeout=5.0)
            self._sync_thread = None
        logger.info("CollectiveMesh stopped")
    
    def _cleanup_expired_signals(self) -> int:
        """Remove expired signals. Returns count removed."""
        expired = []
        cutoff = datetime.now() - timedelta(hours=self.SIGNAL_EXPIRY_HOURS)
        
        for fingerprint, signal in self.signals.items():
            if signal.timestamp < cutoff:
                expired.append(fingerprint)
        
        for fingerprint in expired:
            del self.signals[fingerprint]
        
        if expired:
            logger.info(f"Cleaned up {len(expired)} expired signals")
        
        return len(expired)
    
    def _check_pending_consensus(self) -> List[MeshAlert]:
        """Check pending signals for consensus."""
        alerts = []
        
        for fingerprint, signal in self.signals.items():
            if signal.consensus_state != "pending":
                continue
            
            consensus_score = self.calculate_consensus(fingerprint)
            
            if self._should_emit_alert(fingerprint, consensus_score):
                alert = self.emit_alert(fingerprint)
                signal.consensus_state = "confirmed"
                alerts.append(alert)
        
        return alerts
    
    def get_alert_history(
        self,
        min_consensus: float = 0.0
    ) -> List[MeshAlert]:
        """
        Get alert history with optional filter.
        
        Args:
            min_consensus: Minimum consensus score filter.
            
        Returns:
            List of matching alerts.
        """
        if min_consensus > 0:
            return [a for a in self._alert_history if a.consensus_score >= min_consensus]
        return list(self._alert_history)
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get mesh statistics.
        
        Returns:
            Dictionary with mesh statistics.
        """
        active_peers = sum(1 for p in self._peer_nodes.values() if p.is_active)
        
        consensus_states = {"pending": 0, "confirmed": 0, "rejected": 0, "expired": 0}
        for signal in self.signals.values():
            state = signal.consensus_state
            consensus_states[state] = consensus_states.get(state, 0) + 1
        
        return {
            "node_id": self.node_id,
            "total_peers": len(self.peers),
            "active_peers": active_peers,
            "total_signals": len(self.signals),
            "consensus_states": consensus_states,
            "total_alerts": len(self._alert_history),
            "consensus_threshold": self._consensus_threshold,
            "is_running": self._running,
            "avg_peer_trust": (
                sum(p.trust_score for p in self._peer_nodes.values()) / len(self._peer_nodes)
                if self._peer_nodes else 0.0
            ),
        }
    
    def clear(self) -> None:
        """Clear all signals and history."""
        with self._lock:
            self.signals.clear()
            self._alert_history.clear()
            logger.info("CollectiveMesh cleared")


# Factory function for pipeline integration
def create_collective_mesh(
    node_id: Optional[str] = None,
    consensus_threshold: float = 0.7
) -> CollectiveMesh:
    """
    Factory function to create a CollectiveMesh instance.
    
    Args:
        node_id: Unique identifier for this node.
        consensus_threshold: Threshold for consensus.
        
    Returns:
        New CollectiveMesh instance.
    """
    return CollectiveMesh(
        node_id=node_id,
        consensus_threshold=consensus_threshold
    )


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    print("=== Collective Intelligence Mesh Demo ===\n")
    
    # Create mesh node
    mesh = create_collective_mesh(node_id="node-alpha")
    
    # Subscribe to alerts
    def on_mesh_alert(alert: MeshAlert):
        print(f"\n>>> MESH ALERT")
        print(f"    Fingerprint: {alert.fingerprint[:24]}...")
        print(f"    Consensus: {alert.consensus_score:.2f}")
        print(f"    Peers: {len(alert.peers_involved)}")
        print(f"    Description: {alert.description}\n")
    
    mesh.subscribe(on_mesh_alert)
    
    # Add peers
    print("=== Adding Peers ===")
    peers = [
        ("node-beta", 0.8),
        ("node-gamma", 0.7),
        ("node-delta", 0.6),
        ("node-epsilon", 0.9),
    ]
    
    for peer_id, trust in peers:
        mesh.add_peer(peer_id, trust_score=trust)
        print(f"  Added: {peer_id} (trust: {trust})")
    
    # Broadcast a threat signal
    print("\n=== Broadcasting Threat Signal ===")
    threat_fingerprint = hashlib.sha256(b"malware_sample_001").hexdigest()
    
    mesh.broadcast_signal(
        fingerprint=threat_fingerprint,
        metadata={
            "confidence": 0.85,
            "threat_type": "ransomware",
            "indicators": ["file_encryption", "ransom_note"],
        }
    )
    
    # Simulate receiving signals from peers
    print("\n=== Receiving Peer Signals ===")
    
    peer_signals = [
        ("node-beta", 0.9, {"threat_type": "ransomware", "family": "lockbit"}),
        ("node-gamma", 0.75, {"threat_type": "ransomware", "confidence": 0.8}),
        ("node-delta", 0.6, {"threat_type": "unknown"}),
        ("node-epsilon", 0.95, {"threat_type": "ransomware", "verified": True}),
    ]
    
    for peer_id, confidence, meta in peer_signals:
        meta["confidence"] = confidence
        alert = mesh.receive_signal(peer_id, threat_fingerprint, meta)
        print(f"  From {peer_id}: confidence={confidence}")
        if alert:
            print(f"    -> Consensus reached!")
    
    # Calculate final consensus
    print("\n=== Consensus Calculation ===")
    final_consensus = mesh.calculate_consensus(threat_fingerprint)
    print(f"Final consensus score: {final_consensus:.2f}")
    
    # Get peer intelligence
    print("\n=== Peer Intelligence ===")
    intel = mesh.get_peer_intelligence(threat_fingerprint)
    print(f"Fingerprint: {intel['fingerprint'][:24]}...")
    print(f"Consensus State: {intel['consensus_state']}")
    print(f"Peer Count: {intel['peer_count']}")
    print("Votes:")
    for peer, vote in intel["votes"].items():
        print(f"  {peer}: {vote:.2f}")
    
    # Broadcast another signal
    print("\n=== Second Threat Signal ===")
    threat_2 = hashlib.sha256(b"suspicious_behavior_002").hexdigest()
    mesh.broadcast_signal(
        fingerprint=threat_2,
        metadata={"confidence": 0.5, "threat_type": "suspicious"},
    )
    
    # Request votes
    votes = mesh.request_peer_votes(threat_2)
    print(f"Collected {len(votes)} peer votes for threat_2")
    
    # Statistics
    print("\n=== Mesh Statistics ===")
    stats = mesh.get_statistics()
    print(f"Node ID: {stats['node_id']}")
    print(f"Total Peers: {stats['total_peers']}")
    print(f"Active Peers: {stats['active_peers']}")
    print(f"Total Signals: {stats['total_signals']}")
    print(f"Total Alerts: {stats['total_alerts']}")
    print(f"Consensus States: {stats['consensus_states']}")
    print(f"Avg Peer Trust: {stats['avg_peer_trust']:.2f}")
    
    print("\n=== Demo Complete ===")
