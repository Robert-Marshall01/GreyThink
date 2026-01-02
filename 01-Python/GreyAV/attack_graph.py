"""
Cognitive Attack Graph Engine for GreyAV EDR System.

This module maintains a dynamic graph of system assets and relationships,
tracking potential attacker movements and forecasting attack paths.
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Callable, Optional
import uuid


@dataclass
class Event:
    """
    Represents a system event that may indicate attacker activity.
    
    Attributes:
        event_type: Type of event (e.g., 'file_access', 'process_spawn', 'network_connection').
        source: Source entity (file, process, user, endpoint).
        target: Target entity affected by the event.
        severity: Severity score (0.0 to 1.0).
        timestamp: When the event occurred.
        metadata: Additional event-specific data.
    """
    event_type: str
    source: str
    target: str
    severity: float = 0.5
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: dict = field(default_factory=dict)


@dataclass
class AttackPathAlert:
    """
    Alert object emitted when a malicious attack path is detected or forming.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        source_node: Starting point of the attack path.
        target_node: End point (target asset) of the attack path.
        path: Ordered list of nodes representing the attack trajectory.
        risk_score: Calculated risk score for this path (0.0 to 1.0).
        description: Human-readable description of the threat.
        timestamp: When the alert was generated.
    """
    alert_id: str
    source_node: str
    target_node: str
    path: list[str]
    risk_score: float
    description: str
    timestamp: datetime = field(default_factory=datetime.now)

    @staticmethod
    def generate_id() -> str:
        """Generate a unique alert ID."""
        return f"APA-{uuid.uuid4().hex[:12].upper()}"


class AttackGraph:
    """
    Dynamic graph representing system assets and potential attack relationships.
    
    The graph maintains nodes (assets like files, processes, users, endpoints)
    and weighted edges representing observed or inferred attacker actions.
    Edge weights reflect severity, frequency, and intent predictions.
    
    Attributes:
        nodes: Set of all asset nodes in the graph.
        edges: Dictionary mapping (source, destination) tuples to edge weights.
    """

    # Event type to edge weight multipliers
    EVENT_WEIGHT_MULTIPLIERS: dict[str, float] = {
        'file_access': 0.3,
        'file_modify': 0.5,
        'file_delete': 0.7,
        'process_spawn': 0.4,
        'process_inject': 0.9,
        'privilege_escalation': 0.95,
        'network_connection': 0.3,
        'lateral_movement': 0.8,
        'credential_access': 0.85,
        'data_exfiltration': 0.9,
        'persistence': 0.7,
        'defense_evasion': 0.6,
    }

    # Critical assets that increase risk when targeted
    CRITICAL_ASSET_PATTERNS: list[str] = [
        '/etc/passwd',
        '/etc/shadow',
        'System32',
        'registry',
        'credentials',
        'admin',
        'root',
        'domain_controller',
    ]

    def __init__(self) -> None:
        """Initialize an empty attack graph."""
        self.nodes: set[str] = set()
        self.edges: dict[tuple[str, str], float] = {}
        self._edge_hit_counts: dict[tuple[str, str], int] = {}
        self._alert_subscribers: list[Callable[[AttackPathAlert], None]] = []
        self._high_value_targets: set[str] = set()
        self._entry_points: set[str] = set()

    def add_node(self, name: str, node_type: str = "generic") -> None:
        """
        Add a node representing a system asset to the graph.
        
        Args:
            name: Unique identifier for the node (e.g., file path, process name).
            node_type: Type of asset ('file', 'process', 'user', 'endpoint').
        """
        self.nodes.add(name)
        
        # Identify high-value targets based on patterns
        if any(pattern.lower() in name.lower() for pattern in self.CRITICAL_ASSET_PATTERNS):
            self._high_value_targets.add(name)

    def add_edge(self, src: str, dst: str, weight: float) -> None:
        """
        Add or update a weighted edge between two nodes.
        
        If the edge already exists, the weight is updated using exponential
        moving average to reflect both historical and new observations.
        
        Args:
            src: Source node identifier.
            dst: Destination node identifier.
            weight: Edge weight representing attack likelihood/severity (0.0 to 1.0).
        """
        # Ensure nodes exist
        self.add_node(src)
        self.add_node(dst)
        
        edge_key = (src, dst)
        
        if edge_key in self.edges:
            # Exponential moving average for weight updates
            alpha = 0.3  # Learning rate
            old_weight = self.edges[edge_key]
            self.edges[edge_key] = alpha * weight + (1 - alpha) * old_weight
            self._edge_hit_counts[edge_key] = self._edge_hit_counts.get(edge_key, 1) + 1
        else:
            self.edges[edge_key] = max(0.0, min(1.0, weight))  # Clamp to [0, 1]
            self._edge_hit_counts[edge_key] = 1

    def remove_node(self, name: str) -> None:
        """
        Remove a node and all associated edges from the graph.
        
        Args:
            name: Node identifier to remove.
        """
        self.nodes.discard(name)
        self._high_value_targets.discard(name)
        self._entry_points.discard(name)
        
        # Remove all edges involving this node
        edges_to_remove = [
            edge for edge in self.edges 
            if edge[0] == name or edge[1] == name
        ]
        for edge in edges_to_remove:
            del self.edges[edge]
            self._edge_hit_counts.pop(edge, None)

    def remove_edge(self, src: str, dst: str) -> None:
        """
        Remove an edge from the graph.
        
        Args:
            src: Source node identifier.
            dst: Destination node identifier.
        """
        edge_key = (src, dst)
        self.edges.pop(edge_key, None)
        self._edge_hit_counts.pop(edge_key, None)

    def get_edge_weight(self, src: str, dst: str) -> Optional[float]:
        """
        Get the weight of an edge between two nodes.
        
        Args:
            src: Source node identifier.
            dst: Destination node identifier.
            
        Returns:
            Edge weight if edge exists, None otherwise.
        """
        return self.edges.get((src, dst))

    def get_neighbors(self, node: str) -> list[tuple[str, float]]:
        """
        Get all outgoing neighbors of a node with their edge weights.
        
        Args:
            node: Node identifier.
            
        Returns:
            List of (neighbor_node, edge_weight) tuples.
        """
        return [
            (dst, weight) 
            for (src, dst), weight in self.edges.items() 
            if src == node
        ]

    def mark_entry_point(self, node: str) -> None:
        """
        Mark a node as a potential attack entry point.
        
        Args:
            node: Node identifier to mark.
        """
        self.add_node(node)
        self._entry_points.add(node)

    def mark_high_value_target(self, node: str) -> None:
        """
        Mark a node as a high-value target asset.
        
        Args:
            node: Node identifier to mark.
        """
        self.add_node(node)
        self._high_value_targets.add(node)

    def update_from_event(self, event: Event) -> None:
        """
        Update the graph based on a system event.
        
        This method maps events to graph nodes and edges, adjusting weights
        based on event severity, type, and frequency patterns.
        
        Args:
            event: The system event to process.
        """
        source = event.source
        target = event.target
        
        # Ensure both nodes exist in the graph
        self.add_node(source)
        self.add_node(target)
        
        # Calculate base weight from event severity
        base_weight = event.severity
        
        # Apply event type multiplier
        type_multiplier = self.EVENT_WEIGHT_MULTIPLIERS.get(event.event_type, 0.5)
        
        # Calculate final edge weight
        weight = base_weight * type_multiplier
        
        # Boost weight if targeting critical assets
        if target in self._high_value_targets:
            weight = min(1.0, weight * 1.5)
        
        # Add or update the edge
        self.add_edge(source, target, weight)
        
        # Track entry points (external sources)
        if event.metadata.get('is_external', False):
            self._entry_points.add(source)

    def find_paths(
        self, 
        src: str, 
        dst: str, 
        max_depth: int = 10
    ) -> list[list[str]]:
        """
        Find all paths between two nodes using depth-limited DFS.
        
        Args:
            src: Source node to start from.
            dst: Destination node to reach.
            max_depth: Maximum path length to explore.
            
        Returns:
            List of paths, where each path is a list of node identifiers.
        """
        if src not in self.nodes or dst not in self.nodes:
            return []
        
        paths: list[list[str]] = []
        
        def dfs(current: str, target: str, path: list[str], visited: set[str]) -> None:
            if len(path) > max_depth:
                return
            
            if current == target:
                paths.append(path.copy())
                return
            
            for neighbor, _ in self.get_neighbors(current):
                if neighbor not in visited:
                    visited.add(neighbor)
                    path.append(neighbor)
                    dfs(neighbor, target, path, visited)
                    path.pop()
                    visited.remove(neighbor)
        
        visited = {src}
        dfs(src, dst, [src], visited)
        
        return paths

    def find_highest_risk_paths(
        self, 
        src: str, 
        dst: str, 
        max_depth: int = 10,
        top_k: int = 5
    ) -> list[tuple[list[str], float]]:
        """
        Find paths between nodes sorted by cumulative risk score.
        
        Args:
            src: Source node to start from.
            dst: Destination node to reach.
            max_depth: Maximum path length to explore.
            top_k: Number of highest-risk paths to return.
            
        Returns:
            List of (path, risk_score) tuples sorted by risk descending.
        """
        all_paths = self.find_paths(src, dst, max_depth)
        
        scored_paths: list[tuple[list[str], float]] = []
        for path in all_paths:
            risk = self._calculate_path_risk(path)
            scored_paths.append((path, risk))
        
        # Sort by risk score descending
        scored_paths.sort(key=lambda x: x[1], reverse=True)
        
        return scored_paths[:top_k]

    def _calculate_path_risk(self, path: list[str]) -> float:
        """
        Calculate the risk score for a given path.
        
        Risk is computed as a weighted sum of edge severities, considering:
        - Edge weights (attack likelihood)
        - Edge frequency (hit counts)
        - High-value target presence
        
        Args:
            path: List of node identifiers forming the path.
            
        Returns:
            Risk score between 0.0 and 1.0.
        """
        if len(path) < 2:
            return 0.0
        
        total_weight = 0.0
        max_weight = 0.0
        
        for i in range(len(path) - 1):
            edge_key = (path[i], path[i + 1])
            weight = self.edges.get(edge_key, 0.0)
            
            # Factor in edge frequency
            hit_count = self._edge_hit_counts.get(edge_key, 1)
            frequency_boost = min(1.5, 1.0 + (hit_count - 1) * 0.1)
            
            adjusted_weight = weight * frequency_boost
            total_weight += adjusted_weight
            max_weight = max(max_weight, adjusted_weight)
        
        # Combine average and max for balanced scoring
        avg_weight = total_weight / (len(path) - 1)
        base_risk = 0.6 * avg_weight + 0.4 * max_weight
        
        # Boost if path ends at high-value target
        if path[-1] in self._high_value_targets:
            base_risk = min(1.0, base_risk * 1.3)
        
        # Boost if path starts from entry point
        if path[0] in self._entry_points:
            base_risk = min(1.0, base_risk * 1.2)
        
        return min(1.0, base_risk)

    def forecast_attack_paths(
        self, 
        risk_threshold: float = 0.5,
        max_depth: int = 10
    ) -> list[AttackPathAlert]:
        """
        Analyze the graph to forecast likely attack trajectories.
        
        This method identifies potential attack paths from entry points
        to high-value targets, computing risk scores and generating alerts
        for paths that exceed the risk threshold.
        
        Args:
            risk_threshold: Minimum risk score to generate an alert (0.0 to 1.0).
            max_depth: Maximum path depth to analyze.
            
        Returns:
            List of AttackPathAlert objects for concerning paths.
        """
        alerts: list[AttackPathAlert] = []
        
        # If no entry points defined, use all nodes with outgoing edges
        entry_points = self._entry_points if self._entry_points else {
            src for src, _ in self.edges.keys()
        }
        
        # If no high-value targets defined, use all nodes with incoming edges
        targets = self._high_value_targets if self._high_value_targets else {
            dst for _, dst in self.edges.keys()
        }
        
        # Analyze paths from entry points to targets
        for entry in entry_points:
            for target in targets:
                if entry == target:
                    continue
                
                scored_paths = self.find_highest_risk_paths(
                    entry, target, max_depth, top_k=3
                )
                
                for path, risk_score in scored_paths:
                    if risk_score >= risk_threshold:
                        alert = AttackPathAlert(
                            alert_id=AttackPathAlert.generate_id(),
                            source_node=entry,
                            target_node=target,
                            path=path,
                            risk_score=risk_score,
                            description=self._generate_alert_description(path, risk_score),
                            timestamp=datetime.now()
                        )
                        alerts.append(alert)
        
        # Sort alerts by risk score descending
        alerts.sort(key=lambda a: a.risk_score, reverse=True)
        
        # Notify subscribers
        for alert in alerts:
            self._notify_subscribers(alert)
        
        return alerts

    def _generate_alert_description(self, path: list[str], risk_score: float) -> str:
        """
        Generate a human-readable description for an attack path alert.
        
        Args:
            path: The attack path.
            risk_score: Calculated risk score.
            
        Returns:
            Description string.
        """
        severity = "CRITICAL" if risk_score >= 0.8 else "HIGH" if risk_score >= 0.6 else "MEDIUM"
        path_str = " -> ".join(path[:5])
        if len(path) > 5:
            path_str += f" -> ... ({len(path) - 5} more)"
        
        return (
            f"{severity} risk attack path detected. "
            f"Path: {path_str}. "
            f"Risk score: {risk_score:.2f}. "
            f"Immediate investigation recommended."
        )

    def subscribe(self, callback: Callable[[AttackPathAlert], None]) -> None:
        """
        Subscribe to attack path alerts.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self._alert_subscribers.append(callback)

    def unsubscribe(self, callback: Callable[[AttackPathAlert], None]) -> None:
        """
        Unsubscribe from attack path alerts.
        
        Args:
            callback: Previously registered callback function.
        """
        if callback in self._alert_subscribers:
            self._alert_subscribers.remove(callback)

    def _notify_subscribers(self, alert: AttackPathAlert) -> None:
        """
        Notify all subscribers of a new alert.
        
        Args:
            alert: The alert to broadcast.
        """
        for callback in self._alert_subscribers:
            try:
                callback(alert)
            except Exception as e:
                # Log error but don't stop notification chain
                print(f"[AttackGraph] Subscriber notification failed: {e}")

    def get_graph_stats(self) -> dict[str, Any]:
        """
        Get statistics about the current graph state.
        
        Returns:
            Dictionary containing graph metrics.
        """
        return {
            "node_count": len(self.nodes),
            "edge_count": len(self.edges),
            "entry_points": len(self._entry_points),
            "high_value_targets": len(self._high_value_targets),
            "avg_edge_weight": (
                sum(self.edges.values()) / len(self.edges) 
                if self.edges else 0.0
            ),
            "max_edge_weight": max(self.edges.values()) if self.edges else 0.0,
        }

    def clear(self) -> None:
        """Reset the graph to an empty state."""
        self.nodes.clear()
        self.edges.clear()
        self._edge_hit_counts.clear()
        self._high_value_targets.clear()
        self._entry_points.clear()

    def to_dict(self) -> dict[str, Any]:
        """
        Serialize the graph to a dictionary for persistence.
        
        Returns:
            Dictionary representation of the graph.
        """
        return {
            "nodes": list(self.nodes),
            "edges": {f"{src}|{dst}": weight for (src, dst), weight in self.edges.items()},
            "edge_hit_counts": {f"{src}|{dst}": count for (src, dst), count in self._edge_hit_counts.items()},
            "high_value_targets": list(self._high_value_targets),
            "entry_points": list(self._entry_points),
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "AttackGraph":
        """
        Deserialize a graph from a dictionary.
        
        Args:
            data: Dictionary representation of the graph.
            
        Returns:
            Reconstructed AttackGraph instance.
        """
        graph = cls()
        graph.nodes = set(data.get("nodes", []))
        
        for edge_key, weight in data.get("edges", {}).items():
            src, dst = edge_key.split("|", 1)
            graph.edges[(src, dst)] = weight
        
        for edge_key, count in data.get("edge_hit_counts", {}).items():
            src, dst = edge_key.split("|", 1)
            graph._edge_hit_counts[(src, dst)] = count
        
        graph._high_value_targets = set(data.get("high_value_targets", []))
        graph._entry_points = set(data.get("entry_points", []))
        
        return graph


# Pipeline integration handler
def create_attack_graph_handler(graph: AttackGraph) -> Callable[[Event], None]:
    """
    Create an event handler for pipeline integration.
    
    This factory creates a handler that can be subscribed to an event pipeline
    to automatically update the attack graph when events occur.
    
    Args:
        graph: The AttackGraph instance to update.
        
    Returns:
        Event handler function compatible with pipeline subscription.
    """
    def handler(event: Event) -> None:
        graph.update_from_event(event)
    
    return handler


if __name__ == "__main__":
    # Demo usage
    graph = AttackGraph()
    
    # Add some nodes
    graph.add_node("external_attacker", "endpoint")
    graph.add_node("web_server", "process")
    graph.add_node("database", "process")
    graph.add_node("/etc/shadow", "file")
    
    # Mark entry points and targets
    graph.mark_entry_point("external_attacker")
    graph.mark_high_value_target("/etc/shadow")
    graph.mark_high_value_target("database")
    
    # Simulate events
    events = [
        Event("network_connection", "external_attacker", "web_server", 0.6),
        Event("process_spawn", "web_server", "shell", 0.7),
        Event("file_access", "shell", "/etc/shadow", 0.9),
        Event("lateral_movement", "web_server", "database", 0.8),
    ]
    
    for event in events:
        graph.update_from_event(event)
    
    # Subscribe to alerts
    def alert_handler(alert: AttackPathAlert) -> None:
        print(f"\n[ALERT] {alert.alert_id}")
        print(f"  Path: {' -> '.join(alert.path)}")
        print(f"  Risk: {alert.risk_score:.2f}")
        print(f"  {alert.description}")
    
    graph.subscribe(alert_handler)
    
    # Forecast attack paths
    print("Forecasting attack paths...")
    alerts = graph.forecast_attack_paths(risk_threshold=0.3)
    
    print(f"\nGraph stats: {graph.get_graph_stats()}")
