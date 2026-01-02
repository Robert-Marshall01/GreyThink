"""
Collective Intelligence Mesh Node for GreyAV Antivirus/EDR System.

This module implements a mesh network node that enables distributed threat
intelligence sharing between endpoints. Nodes exchange compact threat signals,
perform consensus scoring to reduce false positives, and emit coordinated
alerts when collective agreement indicates malicious activity.
"""

import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Set


@dataclass
class MeshEvent:
    """
    Represents a threat signal exchanged within the mesh network.
    
    Attributes:
        event_id: Unique identifier for this mesh event.
        source_node_id: ID of the node that originated this signal.
        signal_type: Type of signal ("intent", "alert", "deception", "resilience").
        signal_payload: Dictionary containing signal-specific data.
        consensus_score: Aggregated confidence score from mesh consensus (0.0 to 1.0).
        timestamp: When the signal was created.
        description: Human-readable description of the signal.
    """
    event_id: str
    source_node_id: str
    signal_type: str  # "intent", "alert", "deception", "resilience"
    signal_payload: Dict[str, Any]
    consensus_score: float
    timestamp: datetime
    description: str
    
    @staticmethod
    def generate_id() -> str:
        """Generate a unique event ID."""
        return str(uuid.uuid4())
    
    def to_compact(self) -> Dict[str, Any]:
        """
        Convert to compact dictionary for network transmission.
        
        Returns:
            Compact dictionary representation of the event.
        """
        return {
            "id": self.event_id,
            "src": self.source_node_id,
            "type": self.signal_type,
            "payload": self.signal_payload,
            "score": self.consensus_score,
            "ts": self.timestamp.isoformat(),
            "desc": self.description
        }
    
    @classmethod
    def from_compact(cls, data: Dict[str, Any]) -> "MeshEvent":
        """
        Create MeshEvent from compact dictionary.
        
        Args:
            data: Compact dictionary representation.
        
        Returns:
            MeshEvent instance.
        """
        return cls(
            event_id=data["id"],
            source_node_id=data["src"],
            signal_type=data["type"],
            signal_payload=data["payload"],
            consensus_score=data["score"],
            timestamp=datetime.fromisoformat(data["ts"]),
            description=data["desc"]
        )


@dataclass
class PeerInfo:
    """
    Information about a peer node in the mesh.
    
    Attributes:
        node_id: Unique identifier of the peer.
        address: Network address (can be mock/simulated).
        last_seen: Timestamp of last communication.
        trust_score: Trust level for this peer (0.0 to 1.0).
        is_active: Whether the peer is currently reachable.
    """
    node_id: str
    address: str = ""
    last_seen: datetime = field(default_factory=datetime.now)
    trust_score: float = 1.0
    is_active: bool = True


class SignalStore:
    """
    Local storage for mesh signals with aggregation support.
    
    This class maintains received signals organized by event ID,
    enabling consensus computation across multiple peer observations.
    """
    
    def __init__(self):
        """Initialize the signal store."""
        self._signals: Dict[str, List[MeshEvent]] = {}
        self._consensus_cache: Dict[str, MeshEvent] = {}
    
    def add_signal(self, signal: MeshEvent) -> None:
        """
        Add a signal to the store.
        
        Args:
            signal: MeshEvent to store.
        """
        if signal.event_id not in self._signals:
            self._signals[signal.event_id] = []
        self._signals[signal.event_id].append(signal)
        
        # Invalidate consensus cache for this event
        if signal.event_id in self._consensus_cache:
            del self._consensus_cache[signal.event_id]
    
    def get_signals(self, event_id: str) -> List[MeshEvent]:
        """
        Get all signals for a given event ID.
        
        Args:
            event_id: Event ID to look up.
        
        Returns:
            List of MeshEvent objects for this event.
        """
        return self._signals.get(event_id, [])
    
    def get_all_event_ids(self) -> List[str]:
        """
        Get all event IDs in the store.
        
        Returns:
            List of event IDs.
        """
        return list(self._signals.keys())
    
    def get_signal_count(self, event_id: str) -> int:
        """
        Get the number of signals for an event.
        
        Args:
            event_id: Event ID to count.
        
        Returns:
            Number of signals.
        """
        return len(self._signals.get(event_id, []))
    
    def set_consensus(self, event_id: str, consensus: MeshEvent) -> None:
        """
        Cache a computed consensus result.
        
        Args:
            event_id: Event ID.
            consensus: Computed consensus MeshEvent.
        """
        self._consensus_cache[event_id] = consensus
    
    def get_consensus(self, event_id: str) -> Optional[MeshEvent]:
        """
        Get cached consensus result.
        
        Args:
            event_id: Event ID.
        
        Returns:
            Cached consensus MeshEvent, or None.
        """
        return self._consensus_cache.get(event_id)
    
    def clear_old_signals(self, max_age_seconds: int = 3600) -> int:
        """
        Remove signals older than specified age.
        
        Args:
            max_age_seconds: Maximum age in seconds.
        
        Returns:
            Number of events removed.
        """
        now = datetime.now()
        expired = []
        
        for event_id, signals in self._signals.items():
            if signals:
                oldest = min(s.timestamp for s in signals)
                age = (now - oldest).total_seconds()
                if age > max_age_seconds:
                    expired.append(event_id)
        
        for event_id in expired:
            del self._signals[event_id]
            if event_id in self._consensus_cache:
                del self._consensus_cache[event_id]
        
        return len(expired)


class MeshNode:
    """
    A node in the Collective Intelligence Mesh network.
    
    This class represents a single endpoint participating in the distributed
    threat intelligence mesh. It handles signal broadcast and reception,
    maintains local signal storage, performs consensus scoring, and emits
    MeshEvents when collective agreement indicates malicious activity.
    """
    
    # Consensus threshold - signals with scores above this trigger alerts
    DEFAULT_CONSENSUS_THRESHOLD = 0.7
    
    # Minimum number of peer signals needed for consensus
    MIN_SIGNALS_FOR_CONSENSUS = 2
    
    def __init__(
        self,
        node_id: Optional[str] = None,
        consensus_threshold: float = DEFAULT_CONSENSUS_THRESHOLD
    ):
        """
        Initialize a MeshNode.
        
        Args:
            node_id: Unique identifier for this node. Auto-generated if not provided.
            consensus_threshold: Minimum consensus score to trigger alerts (0.0 to 1.0).
        """
        self.node_id: str = node_id or self._generate_node_id()
        self.peers: Dict[str, PeerInfo] = {}
        self.signal_store: SignalStore = SignalStore()
        self._consensus_threshold: float = consensus_threshold
        self._subscribers: List[Callable[[MeshEvent], None]] = []
        self._outbound_queue: List[MeshEvent] = []
        self._peer_connections: Dict[str, "MeshNode"] = {}  # Mock peer connections
    
    @staticmethod
    def _generate_node_id() -> str:
        """Generate a unique node ID."""
        return f"node-{uuid.uuid4().hex[:12]}"
    
    @property
    def peer_ids(self) -> List[str]:
        """Get list of peer node IDs."""
        return list(self.peers.keys())
    
    def add_peer(self, peer_id: str, address: str = "", trust_score: float = 1.0) -> None:
        """
        Add a peer to the mesh.
        
        Args:
            peer_id: Unique identifier of the peer.
            address: Network address of the peer.
            trust_score: Initial trust level (0.0 to 1.0).
        """
        self.peers[peer_id] = PeerInfo(
            node_id=peer_id,
            address=address,
            trust_score=trust_score
        )
    
    def remove_peer(self, peer_id: str) -> bool:
        """
        Remove a peer from the mesh.
        
        Args:
            peer_id: Peer ID to remove.
        
        Returns:
            True if peer was removed, False if not found.
        """
        if peer_id in self.peers:
            del self.peers[peer_id]
            if peer_id in self._peer_connections:
                del self._peer_connections[peer_id]
            return True
        return False
    
    def connect_peer(self, peer_node: "MeshNode") -> None:
        """
        Establish a mock connection to a peer node (for simulation).
        
        Args:
            peer_node: MeshNode instance to connect to.
        """
        if peer_node.node_id != self.node_id:
            self.add_peer(peer_node.node_id)
            self._peer_connections[peer_node.node_id] = peer_node
    
    def broadcast_signal(self, signal: MeshEvent) -> None:
        """
        Broadcast a signal to all connected peers.
        
        This method sends the signal to all peers in the mesh and stores
        it locally. In a real implementation, this would use network
        communication; here it uses mock peer connections.
        
        Args:
            signal: MeshEvent to broadcast.
        """
        # Store locally
        self.signal_store.add_signal(signal)
        
        # Send to all connected peers
        for peer_id, peer_node in self._peer_connections.items():
            try:
                self._send_to_peer(peer_id, signal)
            except Exception:
                # Mark peer as inactive on failure
                if peer_id in self.peers:
                    self.peers[peer_id].is_active = False
    
    def _send_to_peer(self, peer_id: str, signal: MeshEvent) -> bool:
        """
        Send a signal to a specific peer (mock implementation).
        
        Args:
            peer_id: Target peer ID.
            signal: Signal to send.
        
        Returns:
            True if send was successful.
        """
        if peer_id in self._peer_connections:
            peer_node = self._peer_connections[peer_id]
            # Simulate network delivery
            peer_node.receive_signal(signal)
            
            # Update last seen
            if peer_id in self.peers:
                self.peers[peer_id].last_seen = datetime.now()
            return True
        return False
    
    def receive_signal(self, signal: MeshEvent) -> None:
        """
        Receive and process a signal from a peer.
        
        This method stores the signal locally and triggers consensus
        computation when enough signals have been received.
        
        Args:
            signal: MeshEvent received from a peer.
        """
        # Don't process our own signals (avoid loops)
        if signal.source_node_id == self.node_id:
            return
        
        # Store the signal
        self.signal_store.add_signal(signal)
        
        # Update peer last seen
        if signal.source_node_id in self.peers:
            self.peers[signal.source_node_id].last_seen = datetime.now()
        
        # Check if we have enough signals for consensus
        signal_count = self.signal_store.get_signal_count(signal.event_id)
        if signal_count >= self.MIN_SIGNALS_FOR_CONSENSUS:
            consensus = self.compute_consensus(signal.event_id)
            if consensus and consensus.consensus_score >= self._consensus_threshold:
                self._emit_event(consensus)
    
    def compute_consensus(self, event_id: str) -> Optional[MeshEvent]:
        """
        Compute consensus for an event based on aggregated peer signals.
        
        This method aggregates signals from peers, computes a weighted
        average consensus score based on peer trust levels, and returns
        a MeshEvent if sufficient consensus is reached.
        
        Args:
            event_id: Event ID to compute consensus for.
        
        Returns:
            MeshEvent with computed consensus score, or None if insufficient data.
        """
        # Check cache first
        cached = self.signal_store.get_consensus(event_id)
        if cached:
            return cached
        
        signals = self.signal_store.get_signals(event_id)
        if not signals:
            return None
        
        # Compute weighted consensus score
        total_weight = 0.0
        weighted_score = 0.0
        
        for signal in signals:
            # Get trust weight for the source node
            trust_weight = 1.0
            if signal.source_node_id in self.peers:
                trust_weight = self.peers[signal.source_node_id].trust_score
            
            weighted_score += signal.consensus_score * trust_weight
            total_weight += trust_weight
        
        if total_weight == 0:
            return None
        
        final_score = weighted_score / total_weight
        
        # Use the first signal as a template for the consensus event
        template = signals[0]
        
        # Merge payloads from all signals
        merged_payload = self._merge_payloads([s.signal_payload for s in signals])
        
        consensus_event = MeshEvent(
            event_id=event_id,
            source_node_id=self.node_id,  # Consensus originated from this node
            signal_type=template.signal_type,
            signal_payload=merged_payload,
            consensus_score=final_score,
            timestamp=datetime.now(),
            description=f"Consensus from {len(signals)} signals: {template.description}"
        )
        
        # Cache the result
        self.signal_store.set_consensus(event_id, consensus_event)
        
        return consensus_event
    
    def _merge_payloads(self, payloads: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Merge multiple signal payloads into one.
        
        Args:
            payloads: List of payload dictionaries.
        
        Returns:
            Merged payload dictionary.
        """
        if not payloads:
            return {}
        
        merged = dict(payloads[0])
        merged["_signal_count"] = len(payloads)
        merged["_sources"] = list(set(
            p.get("source", "unknown") for p in payloads if "source" in p
        ))
        
        return merged
    
    def subscribe(self, callback: Callable[[MeshEvent], None]) -> None:
        """
        Subscribe to receive MeshEvent notifications.
        
        Args:
            callback: Callable that accepts a MeshEvent.
        """
        if callback not in self._subscribers:
            self._subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[MeshEvent], None]) -> bool:
        """
        Unsubscribe from MeshEvent notifications.
        
        Args:
            callback: Callback to remove.
        
        Returns:
            True if callback was removed, False if not found.
        """
        try:
            self._subscribers.remove(callback)
            return True
        except ValueError:
            return False
    
    def _emit_event(self, event: MeshEvent) -> None:
        """
        Emit a MeshEvent to all subscribers.
        
        Args:
            event: MeshEvent to emit.
        """
        for subscriber in self._subscribers:
            try:
                subscriber(event)
            except Exception:
                # Log but don't fail on subscriber errors
                pass
    
    def create_signal(
        self,
        signal_type: str,
        payload: Dict[str, Any],
        initial_score: float = 0.5,
        description: str = ""
    ) -> MeshEvent:
        """
        Create a new signal from this node.
        
        Args:
            signal_type: Type of signal ("intent", "alert", "deception", "resilience").
            payload: Signal-specific data.
            initial_score: Initial confidence score (0.0 to 1.0).
            description: Human-readable description.
        
        Returns:
            Created MeshEvent.
        """
        return MeshEvent(
            event_id=MeshEvent.generate_id(),
            source_node_id=self.node_id,
            signal_type=signal_type,
            signal_payload=payload,
            consensus_score=initial_score,
            timestamp=datetime.now(),
            description=description or f"{signal_type} signal from {self.node_id}"
        )
    
    def emit_and_broadcast(
        self,
        signal_type: str,
        payload: Dict[str, Any],
        initial_score: float = 0.5,
        description: str = ""
    ) -> MeshEvent:
        """
        Create a signal, emit it locally, and broadcast to peers.
        
        Args:
            signal_type: Type of signal.
            payload: Signal-specific data.
            initial_score: Initial confidence score.
            description: Human-readable description.
        
        Returns:
            Created MeshEvent.
        """
        signal = self.create_signal(signal_type, payload, initial_score, description)
        self.broadcast_signal(signal)
        return signal
    
    def get_status(self) -> Dict[str, Any]:
        """
        Get current status of this mesh node.
        
        Returns:
            Dictionary with node status information.
        """
        active_peers = sum(1 for p in self.peers.values() if p.is_active)
        return {
            "node_id": self.node_id,
            "peer_count": len(self.peers),
            "active_peers": active_peers,
            "stored_events": len(self.signal_store.get_all_event_ids()),
            "subscriber_count": len(self._subscribers),
            "consensus_threshold": self._consensus_threshold
        }


class MeshNetwork:
    """
    Manages a collection of MeshNodes for simulation and testing.
    
    This class provides utilities for creating and managing a mesh
    network of nodes, useful for testing and demonstration purposes.
    """
    
    def __init__(self):
        """Initialize the mesh network."""
        self._nodes: Dict[str, MeshNode] = {}
    
    def create_node(self, node_id: Optional[str] = None) -> MeshNode:
        """
        Create and register a new mesh node.
        
        Args:
            node_id: Optional node ID. Auto-generated if not provided.
        
        Returns:
            Created MeshNode.
        """
        node = MeshNode(node_id=node_id)
        self._nodes[node.node_id] = node
        return node
    
    def get_node(self, node_id: str) -> Optional[MeshNode]:
        """
        Get a node by ID.
        
        Args:
            node_id: Node ID to look up.
        
        Returns:
            MeshNode or None if not found.
        """
        return self._nodes.get(node_id)
    
    def connect_all(self) -> None:
        """Connect all nodes to each other (full mesh topology)."""
        nodes = list(self._nodes.values())
        for i, node in enumerate(nodes):
            for other in nodes[i + 1:]:
                node.connect_peer(other)
                other.connect_peer(node)
    
    def connect_ring(self) -> None:
        """Connect nodes in a ring topology."""
        nodes = list(self._nodes.values())
        if len(nodes) < 2:
            return
        
        for i in range(len(nodes)):
            next_idx = (i + 1) % len(nodes)
            nodes[i].connect_peer(nodes[next_idx])
            nodes[next_idx].connect_peer(nodes[i])
    
    def broadcast_from(self, node_id: str, signal: MeshEvent) -> bool:
        """
        Broadcast a signal from a specific node.
        
        Args:
            node_id: Source node ID.
            signal: Signal to broadcast.
        
        Returns:
            True if broadcast was successful.
        """
        node = self._nodes.get(node_id)
        if node:
            node.broadcast_signal(signal)
            return True
        return False
    
    def get_network_status(self) -> Dict[str, Any]:
        """
        Get status of the entire mesh network.
        
        Returns:
            Dictionary with network status information.
        """
        return {
            "node_count": len(self._nodes),
            "nodes": {
                node_id: node.get_status()
                for node_id, node in self._nodes.items()
            }
        }


# Factory function for easy integration
def create_mesh_node(
    node_id: Optional[str] = None,
    consensus_threshold: float = MeshNode.DEFAULT_CONSENSUS_THRESHOLD
) -> MeshNode:
    """
    Factory function to create a configured MeshNode.
    
    Args:
        node_id: Optional node ID.
        consensus_threshold: Consensus threshold for alerts.
    
    Returns:
        Configured MeshNode instance.
    """
    return MeshNode(node_id=node_id, consensus_threshold=consensus_threshold)


# Example usage and demonstration
if __name__ == "__main__":
    # Create a mesh network for demonstration
    network = MeshNetwork()
    
    # Create three nodes
    node_a = network.create_node("node-alpha")
    node_b = network.create_node("node-beta")
    node_c = network.create_node("node-gamma")
    
    # Connect all nodes
    network.connect_all()
    
    # Subscribe to events on all nodes
    def log_mesh_event(event: MeshEvent):
        print(f"[MESH] Consensus reached: {event.signal_type} - "
              f"score: {event.consensus_score:.2f} - {event.description}")
    
    for node in [node_a, node_b, node_c]:
        node.subscribe(log_mesh_event)
    
    # Simulate threat detection on node A
    print(f"Node A ({node_a.node_id}) detecting threat...")
    signal = node_a.emit_and_broadcast(
        signal_type="alert",
        payload={
            "threat_type": "ransomware",
            "file_path": "/tmp/suspicious.exe",
            "hash": "abc123..."
        },
        initial_score=0.8,
        description="Potential ransomware behavior detected"
    )
    
    print(f"\nSignal broadcast with ID: {signal.event_id}")
    
    # Node B also sees the same threat
    print(f"\nNode B ({node_b.node_id}) confirming threat...")
    node_b.broadcast_signal(MeshEvent(
        event_id=signal.event_id,  # Same event ID for consensus
        source_node_id=node_b.node_id,
        signal_type="alert",
        signal_payload={
            "threat_type": "ransomware",
            "file_path": "/tmp/suspicious.exe",
            "source": node_b.node_id
        },
        consensus_score=0.9,
        timestamp=datetime.now(),
        description="Confirmed ransomware signature match"
    ))
    
    print("\n--- Network Status ---")
    status = network.get_network_status()
    for node_id, node_status in status["nodes"].items():
        print(f"  {node_id}: {node_status['active_peers']} active peers, "
              f"{node_status['stored_events']} events stored")
