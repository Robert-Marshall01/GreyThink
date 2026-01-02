"""
Adaptive Threat Knowledge Graph for GreyAV EDR System.

This module represents system entities and their relationships as a graph,
continuously updates with new events, and detects suspicious patterns
that may indicate malicious activity.
"""

import uuid
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
    Represents a system event to be ingested into the knowledge graph.
    
    Attributes:
        event_id: Unique identifier for the event.
        event_type: Type of event (process_spawn, file_write, network_connect, etc.).
        source: The entity initiating the event.
        target: The entity affected by the event.
        timestamp: When the event occurred.
        metadata: Additional context data.
    """
    event_id: str
    event_type: str
    source: str
    target: str
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class GraphAlert:
    """
    Emitted when suspicious patterns are detected in the knowledge graph.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        nodes_involved: List of node identifiers involved in the pattern.
        edges_involved: List of edge tuples (src, dst) in the pattern.
        risk_score: Calculated risk score (0.0 - 1.0).
        description: Human-readable description of the detected pattern.
        timestamp: When the pattern was detected.
        pattern_type: Category of the detected pattern.
        metadata: Additional context about the detection.
    """
    alert_id: str
    nodes_involved: List[str]
    edges_involved: List[Tuple[str, str]]
    risk_score: float
    description: str
    timestamp: datetime = field(default_factory=datetime.now)
    pattern_type: str = "unknown"
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class Node:
    """
    Represents an entity in the threat knowledge graph.
    
    Attributes:
        name: Unique identifier for the node.
        node_type: Type of entity (process, file, user, registry, ip, domain).
        metadata: Additional attributes of the entity.
        first_seen: When the entity was first observed.
        last_seen: When the entity was last observed.
        risk_score: Accumulated risk score for this entity.
    """
    name: str
    node_type: str = "unknown"
    metadata: Dict[str, Any] = field(default_factory=dict)
    first_seen: datetime = field(default_factory=datetime.now)
    last_seen: datetime = field(default_factory=datetime.now)
    risk_score: float = 0.0


class ThreatKnowledgeGraph:
    """
    Adaptive Threat Knowledge Graph for tracking entity relationships.
    
    Maintains a graph of system entities (processes, files, users, IPs, etc.)
    and their relationships (spawned, modified, connected, etc.). Continuously
    updates from incoming events and detects suspicious patterns.
    
    Attributes:
        nodes: Dictionary of node names to Node objects.
        edges: Dictionary mapping (src, dst) tuples to edge metadata.
        subscribers: Callbacks notified when alerts are generated.
    """
    
    # Event type to relation mapping
    EVENT_RELATIONS: Dict[str, str] = {
        "process_spawn": "spawned",
        "file_write": "wrote",
        "file_read": "read",
        "file_modify": "modified",
        "file_delete": "deleted",
        "network_connect": "connected",
        "network_listen": "listened",
        "dns_query": "resolved",
        "registry_write": "wrote_registry",
        "registry_read": "read_registry",
        "module_load": "loaded",
        "user_login": "authenticated",
        "privilege_escalation": "escalated",
        "injection": "injected",
    }
    
    # Node type inference patterns
    NODE_TYPE_PATTERNS: Dict[str, List[str]] = {
        "process": [".exe", ".dll", ".so", ".bin", "/usr/bin/", "/usr/sbin/"],
        "file": [".txt", ".doc", ".pdf", ".xlsx", ".csv", ".json", ".xml"],
        "ip": [".", ":"],  # IPv4/IPv6 patterns
        "domain": [".com", ".org", ".net", ".io", ".edu"],
        "registry": ["HKEY_", "HKLM\\", "HKCU\\"],
        "user": ["user:", "uid:", "\\Users\\"],
    }
    
    def __init__(self):
        """Initialize an empty threat knowledge graph."""
        self.nodes: Dict[str, Node] = {}
        self.edges: Dict[Tuple[str, str], Dict[str, Any]] = {}
        self.subscribers: List[Callable[[GraphAlert], None]] = []
        self._alert_history: List[GraphAlert] = []
        logger.info("ThreatKnowledgeGraph initialized")
    
    def subscribe(self, callback: Callable[[GraphAlert], None]) -> None:
        """
        Subscribe to graph alert notifications.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self.subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[GraphAlert], None]) -> None:
        """
        Unsubscribe from graph alert notifications.
        
        Args:
            callback: Previously registered callback to remove.
        """
        if callback in self.subscribers:
            self.subscribers.remove(callback)
    
    def _notify_subscribers(self, alert: GraphAlert) -> None:
        """Notify all subscribers of a new alert."""
        for callback in self.subscribers:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def _infer_node_type(self, name: str) -> str:
        """
        Infer the node type from its name.
        
        Args:
            name: The node name/identifier.
            
        Returns:
            Inferred node type string.
        """
        name_lower = name.lower()
        
        # Check for IP address pattern
        if self._is_ip_address(name):
            return "ip"
        
        # Check other patterns
        for node_type, patterns in self.NODE_TYPE_PATTERNS.items():
            for pattern in patterns:
                if pattern.lower() in name_lower:
                    return node_type
        
        return "unknown"
    
    def _is_ip_address(self, name: str) -> bool:
        """Check if name looks like an IP address."""
        parts = name.split(".")
        if len(parts) == 4:
            try:
                return all(0 <= int(p.split(":")[0]) <= 255 for p in parts)
            except ValueError:
                pass
        return ":" in name and name.count(":") >= 2  # IPv6
    
    def add_node(self, name: str, metadata: Optional[Dict[str, Any]] = None) -> None:
        """
        Add a node to the graph or update if it exists.
        
        Args:
            name: Unique identifier for the node.
            metadata: Optional metadata attributes for the node.
        """
        if name in self.nodes:
            # Update existing node
            node = self.nodes[name]
            node.last_seen = datetime.now()
            if metadata:
                node.metadata.update(metadata)
            logger.debug(f"Updated node: {name}")
        else:
            # Create new node
            node_type = metadata.get("type") if metadata else None
            if not node_type:
                node_type = self._infer_node_type(name)
            
            self.nodes[name] = Node(
                name=name,
                node_type=node_type,
                metadata=metadata or {},
            )
            logger.debug(f"Added node: {name} (type: {node_type})")
    
    def add_edge(
        self,
        src: str,
        dst: str,
        relation: str,
        metadata: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        Add an edge between two nodes.
        
        Creates nodes if they don't exist. Updates edge metadata if edge exists.
        
        Args:
            src: Source node identifier.
            dst: Destination node identifier.
            relation: Type of relationship (spawned, wrote, connected, etc.).
            metadata: Optional edge metadata.
        """
        # Ensure both nodes exist
        if src not in self.nodes:
            self.add_node(src)
        if dst not in self.nodes:
            self.add_node(dst)
        
        edge_key = (src, dst)
        
        if edge_key in self.edges:
            # Update existing edge
            edge_data = self.edges[edge_key]
            edge_data["last_seen"] = datetime.now()
            edge_data["count"] = edge_data.get("count", 1) + 1
            if relation not in edge_data.get("relations", []):
                edge_data.setdefault("relations", []).append(relation)
            if metadata:
                edge_data.setdefault("metadata", {}).update(metadata)
        else:
            # Create new edge
            self.edges[edge_key] = {
                "relation": relation,
                "relations": [relation],
                "first_seen": datetime.now(),
                "last_seen": datetime.now(),
                "count": 1,
                "metadata": metadata or {},
            }
        
        logger.debug(f"Added edge: {src} --[{relation}]--> {dst}")
    
    def update_from_event(self, event: Event) -> None:
        """
        Update the graph from an incoming event.
        
        Maps the event to appropriate nodes and edges.
        
        Args:
            event: The event to process.
        """
        # Determine relation from event type
        relation = self.EVENT_RELATIONS.get(event.event_type, event.event_type)
        
        # Add nodes with event metadata
        source_meta = {
            "type": event.metadata.get("source_type"),
            "event_id": event.event_id,
        }
        target_meta = {
            "type": event.metadata.get("target_type"),
            "event_id": event.event_id,
        }
        
        self.add_node(event.source, source_meta)
        self.add_node(event.target, target_meta)
        
        # Add edge
        edge_meta = {
            "event_id": event.event_id,
            "event_type": event.event_type,
            "timestamp": event.timestamp.isoformat(),
            **event.metadata,
        }
        self.add_edge(event.source, event.target, relation, edge_meta)
        
        logger.info(f"Graph updated from event {event.event_id}: {event.source} -> {event.target}")
    
    def query_subgraph(
        self,
        criteria: Dict[str, Any]
    ) -> List[Tuple[str, str]]:
        """
        Query the graph for edges matching criteria.
        
        Args:
            criteria: Dictionary with optional keys:
                - node_type: Filter by node type
                - relation: Filter by edge relation
                - src_pattern: Source node name pattern
                - dst_pattern: Destination node name pattern
                - min_count: Minimum edge occurrence count
                
        Returns:
            List of (src, dst) tuples matching the criteria.
        """
        results = []
        
        node_type = criteria.get("node_type")
        relation = criteria.get("relation")
        src_pattern = criteria.get("src_pattern", "")
        dst_pattern = criteria.get("dst_pattern", "")
        min_count = criteria.get("min_count", 0)
        
        for (src, dst), edge_data in self.edges.items():
            # Check relation
            if relation and relation not in edge_data.get("relations", []):
                continue
            
            # Check count
            if edge_data.get("count", 1) < min_count:
                continue
            
            # Check source pattern
            if src_pattern and src_pattern.lower() not in src.lower():
                continue
            
            # Check destination pattern
            if dst_pattern and dst_pattern.lower() not in dst.lower():
                continue
            
            # Check node types
            if node_type:
                src_node = self.nodes.get(src)
                dst_node = self.nodes.get(dst)
                if not (
                    (src_node and src_node.node_type == node_type) or
                    (dst_node and dst_node.node_type == node_type)
                ):
                    continue
            
            results.append((src, dst))
        
        return results
    
    def get_neighbors(self, node_name: str, direction: str = "both") -> List[str]:
        """
        Get neighboring nodes.
        
        Args:
            node_name: The node to find neighbors for.
            direction: 'outgoing', 'incoming', or 'both'.
            
        Returns:
            List of neighbor node names.
        """
        neighbors = set()
        
        for (src, dst) in self.edges.keys():
            if direction in ("outgoing", "both") and src == node_name:
                neighbors.add(dst)
            if direction in ("incoming", "both") and dst == node_name:
                neighbors.add(src)
        
        return list(neighbors)
    
    def get_path(
        self,
        start: str,
        end: str,
        max_depth: int = 5
    ) -> Optional[List[str]]:
        """
        Find a path between two nodes using BFS.
        
        Args:
            start: Starting node name.
            end: Ending node name.
            max_depth: Maximum path length to search.
            
        Returns:
            List of node names forming the path, or None if not found.
        """
        if start not in self.nodes or end not in self.nodes:
            return None
        
        if start == end:
            return [start]
        
        visited = {start}
        queue = [(start, [start])]
        
        while queue:
            current, path = queue.pop(0)
            
            if len(path) > max_depth:
                continue
            
            for neighbor in self.get_neighbors(current, "outgoing"):
                if neighbor == end:
                    return path + [neighbor]
                
                if neighbor not in visited:
                    visited.add(neighbor)
                    queue.append((neighbor, path + [neighbor]))
        
        return None
    
    def detect_suspicious_patterns(self) -> List[GraphAlert]:
        """
        Detect suspicious patterns in the graph.
        
        Analyzes the graph for known malicious patterns such as:
        - Process → File → Network (data exfiltration)
        - Rapid file modifications (ransomware)
        - Unusual process spawning chains
        - Known malicious IP connections
        
        Returns:
            List of GraphAlert objects for detected patterns.
        """
        alerts = []
        
        # Pattern 1: Process writing to file then connecting to network
        alerts.extend(self._detect_exfiltration_pattern())
        
        # Pattern 2: Rapid file modifications from single process
        alerts.extend(self._detect_ransomware_pattern())
        
        # Pattern 3: Deep process spawn chains
        alerts.extend(self._detect_spawn_chain_pattern())
        
        # Pattern 4: Process connecting to multiple external IPs
        alerts.extend(self._detect_c2_pattern())
        
        # Pattern 5: Injection patterns
        alerts.extend(self._detect_injection_pattern())
        
        # Store and notify
        for alert in alerts:
            self._alert_history.append(alert)
            self._notify_subscribers(alert)
        
        return alerts
    
    def _detect_exfiltration_pattern(self) -> List[GraphAlert]:
        """Detect potential data exfiltration: process → file → network."""
        alerts = []
        
        # Find processes that wrote files
        for (src, dst), edge_data in self.edges.items():
            if "wrote" not in edge_data.get("relations", []):
                continue
            
            src_node = self.nodes.get(src)
            if not src_node or src_node.node_type != "process":
                continue
            
            # Check if this process also connected to network
            network_edges = [
                (s, d) for (s, d), e in self.edges.items()
                if s == src and "connected" in e.get("relations", [])
            ]
            
            if network_edges:
                # Check if destination is external IP
                for _, net_dst in network_edges:
                    dst_node = self.nodes.get(net_dst)
                    if dst_node and dst_node.node_type == "ip":
                        alert = GraphAlert(
                            alert_id=str(uuid.uuid4()),
                            nodes_involved=[src, dst, net_dst],
                            edges_involved=[(src, dst), (src, net_dst)],
                            risk_score=0.75,
                            description=(
                                f"Potential data exfiltration: Process '{src}' "
                                f"wrote to file '{dst}' and connected to '{net_dst}'"
                            ),
                            pattern_type="data_exfiltration",
                        )
                        alerts.append(alert)
                        logger.warning(f"Exfiltration pattern detected: {alert.description}")
        
        return alerts
    
    def _detect_ransomware_pattern(self) -> List[GraphAlert]:
        """Detect ransomware behavior: rapid file modifications."""
        alerts = []
        
        # Count file operations per process
        process_file_ops: Dict[str, List[str]] = {}
        
        for (src, dst), edge_data in self.edges.items():
            relations = edge_data.get("relations", [])
            if any(r in relations for r in ["wrote", "modified", "deleted"]):
                src_node = self.nodes.get(src)
                if src_node and src_node.node_type == "process":
                    process_file_ops.setdefault(src, []).append(dst)
        
        # Flag processes with many file operations
        for process, files in process_file_ops.items():
            if len(files) >= 10:  # Threshold for suspicious activity
                alert = GraphAlert(
                    alert_id=str(uuid.uuid4()),
                    nodes_involved=[process] + files[:10],
                    edges_involved=[(process, f) for f in files[:10]],
                    risk_score=min(0.5 + len(files) * 0.05, 1.0),
                    description=(
                        f"Potential ransomware: Process '{process}' "
                        f"modified {len(files)} files"
                    ),
                    pattern_type="ransomware_behavior",
                )
                alerts.append(alert)
                logger.warning(f"Ransomware pattern detected: {alert.description}")
        
        return alerts
    
    def _detect_spawn_chain_pattern(self) -> List[GraphAlert]:
        """Detect deep process spawn chains (possible attack chain)."""
        alerts = []
        
        # Build spawn tree
        spawn_children: Dict[str, List[str]] = {}
        for (src, dst), edge_data in self.edges.items():
            if "spawned" in edge_data.get("relations", []):
                spawn_children.setdefault(src, []).append(dst)
        
        # Find deep chains
        def find_chain_depth(node: str, visited: Set[str]) -> List[str]:
            if node in visited or node not in spawn_children:
                return [node]
            visited.add(node)
            
            longest = [node]
            for child in spawn_children.get(node, []):
                chain = [node] + find_chain_depth(child, visited.copy())
                if len(chain) > len(longest):
                    longest = chain
            return longest
        
        for root in spawn_children.keys():
            chain = find_chain_depth(root, set())
            if len(chain) >= 4:  # Suspicious if 4+ levels deep
                edges = [(chain[i], chain[i+1]) for i in range(len(chain)-1)]
                alert = GraphAlert(
                    alert_id=str(uuid.uuid4()),
                    nodes_involved=chain,
                    edges_involved=edges,
                    risk_score=min(0.4 + len(chain) * 0.1, 0.9),
                    description=(
                        f"Suspicious process chain detected: "
                        f"{' -> '.join(chain[:5])}{'...' if len(chain) > 5 else ''}"
                    ),
                    pattern_type="process_chain",
                )
                alerts.append(alert)
                logger.warning(f"Process chain pattern detected: {len(chain)} levels")
        
        return alerts
    
    def _detect_c2_pattern(self) -> List[GraphAlert]:
        """Detect potential C2: process connecting to multiple IPs."""
        alerts = []
        
        # Count IP connections per process
        process_ips: Dict[str, Set[str]] = {}
        
        for (src, dst), edge_data in self.edges.items():
            if "connected" in edge_data.get("relations", []):
                dst_node = self.nodes.get(dst)
                if dst_node and dst_node.node_type == "ip":
                    process_ips.setdefault(src, set()).add(dst)
        
        # Flag processes with many connections
        for process, ips in process_ips.items():
            if len(ips) >= 5:  # Threshold
                alert = GraphAlert(
                    alert_id=str(uuid.uuid4()),
                    nodes_involved=[process] + list(ips),
                    edges_involved=[(process, ip) for ip in ips],
                    risk_score=min(0.5 + len(ips) * 0.05, 0.95),
                    description=(
                        f"Potential C2 activity: Process '{process}' "
                        f"connected to {len(ips)} unique IPs"
                    ),
                    pattern_type="c2_communication",
                )
                alerts.append(alert)
                logger.warning(f"C2 pattern detected: {alert.description}")
        
        return alerts
    
    def _detect_injection_pattern(self) -> List[GraphAlert]:
        """Detect process injection patterns."""
        alerts = []
        
        for (src, dst), edge_data in self.edges.items():
            if "injected" in edge_data.get("relations", []):
                alert = GraphAlert(
                    alert_id=str(uuid.uuid4()),
                    nodes_involved=[src, dst],
                    edges_involved=[(src, dst)],
                    risk_score=0.9,
                    description=(
                        f"Process injection detected: '{src}' injected into '{dst}'"
                    ),
                    pattern_type="process_injection",
                )
                alerts.append(alert)
                logger.warning(f"Injection pattern detected: {alert.description}")
        
        return alerts
    
    def get_node_risk_score(self, node_name: str) -> float:
        """
        Calculate risk score for a node based on its connections.
        
        Args:
            node_name: The node to score.
            
        Returns:
            Risk score between 0.0 and 1.0.
        """
        if node_name not in self.nodes:
            return 0.0
        
        node = self.nodes[node_name]
        base_score = node.risk_score
        
        # Add risk based on suspicious connections
        risky_relations = {"injected", "connected", "escalated"}
        edge_count = 0
        risky_edge_count = 0
        
        for (src, dst), edge_data in self.edges.items():
            if src == node_name or dst == node_name:
                edge_count += 1
                if any(r in risky_relations for r in edge_data.get("relations", [])):
                    risky_edge_count += 1
        
        # Calculate contribution from edges
        edge_contribution = risky_edge_count * 0.1
        
        return min(base_score + edge_contribution, 1.0)
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get graph statistics.
        
        Returns:
            Dictionary with graph statistics.
        """
        node_types: Dict[str, int] = {}
        for node in self.nodes.values():
            node_types[node.node_type] = node_types.get(node.node_type, 0) + 1
        
        relation_types: Dict[str, int] = {}
        for edge_data in self.edges.values():
            for relation in edge_data.get("relations", []):
                relation_types[relation] = relation_types.get(relation, 0) + 1
        
        return {
            "total_nodes": len(self.nodes),
            "total_edges": len(self.edges),
            "node_types": node_types,
            "relation_types": relation_types,
            "alerts_generated": len(self._alert_history),
        }
    
    def export_graph(self) -> Dict[str, Any]:
        """
        Export the graph as a serializable dictionary.
        
        Returns:
            Dictionary representation of the graph.
        """
        return {
            "nodes": {
                name: {
                    "type": node.node_type,
                    "metadata": node.metadata,
                    "first_seen": node.first_seen.isoformat(),
                    "last_seen": node.last_seen.isoformat(),
                    "risk_score": node.risk_score,
                }
                for name, node in self.nodes.items()
            },
            "edges": {
                f"{src}|{dst}": {
                    **data,
                    "first_seen": data["first_seen"].isoformat(),
                    "last_seen": data["last_seen"].isoformat(),
                }
                for (src, dst), data in self.edges.items()
            },
        }
    
    def clear(self) -> None:
        """Clear the entire graph."""
        self.nodes.clear()
        self.edges.clear()
        self._alert_history.clear()
        logger.info("Graph cleared")


# Factory function for pipeline integration
def create_knowledge_graph() -> ThreatKnowledgeGraph:
    """
    Factory function to create a ThreatKnowledgeGraph instance.
    
    Returns:
        New ThreatKnowledgeGraph instance.
    """
    return ThreatKnowledgeGraph()


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    graph = create_knowledge_graph()
    
    # Subscribe to alerts
    def on_alert(alert: GraphAlert):
        print(f">>> ALERT: {alert.pattern_type} - Risk: {alert.risk_score:.2f}")
        print(f"    {alert.description}\n")
    
    graph.subscribe(on_alert)
    
    # Simulate events
    events = [
        Event(
            event_id="evt-001",
            event_type="process_spawn",
            source="/usr/bin/bash",
            target="/tmp/suspicious.exe",
        ),
        Event(
            event_id="evt-002",
            event_type="process_spawn",
            source="/tmp/suspicious.exe",
            target="/tmp/dropper.exe",
        ),
        Event(
            event_id="evt-003",
            event_type="process_spawn",
            source="/tmp/dropper.exe",
            target="/tmp/payload.exe",
        ),
        Event(
            event_id="evt-004",
            event_type="process_spawn",
            source="/tmp/payload.exe",
            target="/tmp/miner.exe",
        ),
        Event(
            event_id="evt-005",
            event_type="file_write",
            source="/tmp/payload.exe",
            target="/home/user/documents/data.txt",
        ),
        Event(
            event_id="evt-006",
            event_type="network_connect",
            source="/tmp/payload.exe",
            target="192.168.1.100:443",
        ),
        Event(
            event_id="evt-007",
            event_type="network_connect",
            source="/tmp/miner.exe",
            target="10.0.0.1:8080",
        ),
        Event(
            event_id="evt-008",
            event_type="network_connect",
            source="/tmp/miner.exe",
            target="10.0.0.2:8080",
        ),
        Event(
            event_id="evt-009",
            event_type="network_connect",
            source="/tmp/miner.exe",
            target="10.0.0.3:8080",
        ),
        Event(
            event_id="evt-010",
            event_type="network_connect",
            source="/tmp/miner.exe",
            target="10.0.0.4:8080",
        ),
        Event(
            event_id="evt-011",
            event_type="network_connect",
            source="/tmp/miner.exe",
            target="10.0.0.5:8080",
        ),
    ]
    
    print("=== Processing Events ===\n")
    for event in events:
        print(f"Event: {event.source} --[{event.event_type}]--> {event.target}")
        graph.update_from_event(event)
    
    print("\n=== Graph Statistics ===")
    stats = graph.get_statistics()
    print(f"Nodes: {stats['total_nodes']}")
    print(f"Edges: {stats['total_edges']}")
    print(f"Node types: {stats['node_types']}")
    print(f"Relations: {stats['relation_types']}")
    
    print("\n=== Detecting Suspicious Patterns ===\n")
    alerts = graph.detect_suspicious_patterns()
    print(f"\nTotal alerts generated: {len(alerts)}")
    
    print("\n=== Query Example: Find all network connections ===")
    connections = graph.query_subgraph({"relation": "connected"})
    for src, dst in connections:
        print(f"  {src} --> {dst}")
    
    print("\n=== Path Finding ===")
    path = graph.get_path("/usr/bin/bash", "10.0.0.5:8080")
    if path:
        print(f"Path found: {' -> '.join(path)}")
    else:
        print("No path found")
