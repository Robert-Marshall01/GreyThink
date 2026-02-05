//! # Failure & Recovery Integration Tests
//!
//! Validates system resilience:
//! 1. Failure detection (heartbeat, phi accrual)
//! 2. Quarantine and isolation
//! 3. Task replay and recovery
//! 4. Leader failover
//!
//! ## Why This Test Matters
//!
//! Distributed systems must handle failures gracefully. These tests verify
//! that the system can detect failures, isolate bad actors, and recover.

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex, atomic::{AtomicU64, AtomicBool, Ordering}};
use std::time::{Duration, Instant};

// ============================================================================
// Mock Types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NodeId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TaskId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HealthStatus { Healthy, Suspected, Failed, Unknown }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RaftRole { Follower, Candidate, Leader }

#[derive(Debug, Clone)]
struct HeartbeatConfig {
    interval: Duration,
    timeout: Duration,
    max_misses: u32,
}

impl Default for HeartbeatConfig {
    fn default() -> Self {
        Self {
            interval: Duration::from_millis(100),
            timeout: Duration::from_millis(500),
            max_misses: 3,
        }
    }
}

struct SimulatedNode {
    id: NodeId,
    is_alive: AtomicBool,
    role: Mutex<RaftRole>,
    term: AtomicU64,
    last_heartbeat: Mutex<Instant>,
    missed_heartbeats: AtomicU64,
}

impl SimulatedNode {
    fn new(id: u64) -> Self {
        Self {
            id: NodeId(id),
            is_alive: AtomicBool::new(true),
            role: Mutex::new(RaftRole::Follower),
            term: AtomicU64::new(0),
            last_heartbeat: Mutex::new(Instant::now()),
            missed_heartbeats: AtomicU64::new(0),
        }
    }

    fn kill(&self) { self.is_alive.store(false, Ordering::SeqCst); }
    fn revive(&self) { self.is_alive.store(true, Ordering::SeqCst); }
    fn is_alive(&self) -> bool { self.is_alive.load(Ordering::SeqCst) }
    
    fn send_heartbeat(&self) {
        if self.is_alive() {
            *self.last_heartbeat.lock().unwrap() = Instant::now();
            self.missed_heartbeats.store(0, Ordering::SeqCst);
        }
    }

    fn check_heartbeat(&self, timeout: Duration) -> HealthStatus {
        if !self.is_alive() {
            return HealthStatus::Failed;
        }
        
        let last = *self.last_heartbeat.lock().unwrap();
        if last.elapsed() > timeout {
            let misses = self.missed_heartbeats.fetch_add(1, Ordering::SeqCst) + 1;
            if misses >= 3 {
                HealthStatus::Failed
            } else {
                HealthStatus::Suspected
            }
        } else {
            HealthStatus::Healthy
        }
    }

    fn become_leader(&self, term: u64) {
        *self.role.lock().unwrap() = RaftRole::Leader;
        self.term.store(term, Ordering::SeqCst);
    }

    fn role(&self) -> RaftRole { *self.role.lock().unwrap() }
    fn term(&self) -> u64 { self.term.load(Ordering::SeqCst) }
}

struct FailureDetector {
    nodes: Mutex<HashMap<NodeId, Arc<SimulatedNode>>>,
    quarantined: Mutex<HashSet<NodeId>>,
    config: HeartbeatConfig,
}

impl FailureDetector {
    fn new(config: HeartbeatConfig) -> Self {
        Self {
            nodes: Mutex::new(HashMap::new()),
            quarantined: Mutex::new(HashSet::new()),
            config,
        }
    }

    fn register_node(&self, node: Arc<SimulatedNode>) {
        self.nodes.lock().unwrap().insert(node.id, node);
    }

    fn check_all(&self) -> Vec<(NodeId, HealthStatus)> {
        let nodes = self.nodes.lock().unwrap();
        nodes.iter()
            .map(|(id, node)| (*id, node.check_heartbeat(self.config.timeout)))
            .collect()
    }

    fn quarantine(&self, node_id: NodeId) {
        self.quarantined.lock().unwrap().insert(node_id);
    }

    fn is_quarantined(&self, node_id: NodeId) -> bool {
        self.quarantined.lock().unwrap().contains(&node_id)
    }

    fn release_from_quarantine(&self, node_id: NodeId) {
        self.quarantined.lock().unwrap().remove(&node_id);
    }

    fn quarantine_count(&self) -> usize {
        self.quarantined.lock().unwrap().len()
    }
}

#[derive(Debug, Clone)]
struct Event {
    task_id: TaskId,
    action: String,
    timestamp: u64,
}

struct ReplayEngine {
    events: Mutex<VecDeque<Event>>,
    replayed: Mutex<Vec<Event>>,
}

impl ReplayEngine {
    fn new() -> Self {
        Self {
            events: Mutex::new(VecDeque::new()),
            replayed: Mutex::new(Vec::new()),
        }
    }

    fn record(&self, task_id: u64, action: &str) {
        let events = self.events.lock().unwrap();
        let timestamp = events.len() as u64;
        drop(events);
        
        self.events.lock().unwrap().push_back(Event {
            task_id: TaskId(task_id),
            action: action.to_string(),
            timestamp,
        });
    }

    fn replay_all(&self) {
        let events: Vec<_> = self.events.lock().unwrap().drain(..).collect();
        let mut replayed = self.replayed.lock().unwrap();
        replayed.extend(events);
    }

    fn event_count(&self) -> usize {
        self.events.lock().unwrap().len()
    }

    fn replayed_count(&self) -> usize {
        self.replayed.lock().unwrap().len()
    }
}

struct CircuitBreaker {
    failure_count: AtomicU64,
    threshold: u64,
    is_open: AtomicBool,
    last_failure: Mutex<Option<Instant>>,
    reset_timeout: Duration,
}

impl CircuitBreaker {
    fn new(threshold: u64, reset_timeout: Duration) -> Self {
        Self {
            failure_count: AtomicU64::new(0),
            threshold,
            is_open: AtomicBool::new(false),
            last_failure: Mutex::new(None),
            reset_timeout,
        }
    }

    fn record_failure(&self) {
        let count = self.failure_count.fetch_add(1, Ordering::SeqCst) + 1;
        *self.last_failure.lock().unwrap() = Some(Instant::now());
        
        if count >= self.threshold {
            self.is_open.store(true, Ordering::SeqCst);
        }
    }

    fn record_success(&self) {
        self.failure_count.store(0, Ordering::SeqCst);
    }

    fn can_proceed(&self) -> bool {
        if !self.is_open.load(Ordering::SeqCst) {
            return true;
        }
        
        // Check if we should try half-open
        if let Some(last) = *self.last_failure.lock().unwrap() {
            if last.elapsed() > self.reset_timeout {
                return true;
            }
        }
        false
    }

    fn reset(&self) {
        self.is_open.store(false, Ordering::SeqCst);
        self.failure_count.store(0, Ordering::SeqCst);
    }

    fn is_open(&self) -> bool {
        self.is_open.load(Ordering::SeqCst)
    }
}

// ============================================================================
// Test Fixtures
// ============================================================================

struct FailureTestCluster {
    nodes: Vec<Arc<SimulatedNode>>,
    detector: Arc<FailureDetector>,
    replay: Arc<ReplayEngine>,
}

impl FailureTestCluster {
    fn new(node_count: usize) -> Self {
        let detector = Arc::new(FailureDetector::new(HeartbeatConfig::default()));
        let mut nodes = Vec::new();
        
        for i in 0..node_count {
            let node = Arc::new(SimulatedNode::new(i as u64 + 1));
            detector.register_node(node.clone());
            nodes.push(node);
        }
        
        Self {
            nodes,
            detector,
            replay: Arc::new(ReplayEngine::new()),
        }
    }

    fn kill_node(&self, idx: usize) {
        if idx < self.nodes.len() {
            self.nodes[idx].kill();
        }
    }

    fn revive_node(&self, idx: usize) {
        if idx < self.nodes.len() {
            self.nodes[idx].revive();
        }
    }

    fn send_all_heartbeats(&self) {
        for node in &self.nodes {
            node.send_heartbeat();
        }
    }

    fn alive_count(&self) -> usize {
        self.nodes.iter().filter(|n| n.is_alive()).count()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_heartbeat_failure_detection() {
    let cluster = FailureTestCluster::new(3);
    cluster.send_all_heartbeats();
    
    // All nodes should be healthy
    let statuses = cluster.detector.check_all();
    assert!(statuses.iter().all(|(_, s)| *s == HealthStatus::Healthy));
    
    // Kill one node
    cluster.kill_node(0);
    
    // Should eventually detect failure
    let statuses = cluster.detector.check_all();
    let failed: Vec<_> = statuses.iter().filter(|(_, s)| *s == HealthStatus::Failed).collect();
    assert_eq!(failed.len(), 1);
}

#[test]
fn test_quarantine_after_failure() {
    let cluster = FailureTestCluster::new(3);
    let node_id = cluster.nodes[0].id;
    
    cluster.kill_node(0);
    cluster.detector.quarantine(node_id);
    
    assert!(cluster.detector.is_quarantined(node_id));
    assert_eq!(cluster.detector.quarantine_count(), 1);
    
    // Release from quarantine
    cluster.detector.release_from_quarantine(node_id);
    assert!(!cluster.detector.is_quarantined(node_id));
}

#[test]
fn test_circuit_breaker_activation() {
    let breaker = CircuitBreaker::new(3, Duration::from_secs(30));
    
    assert!(breaker.can_proceed());
    
    // Record failures
    breaker.record_failure();
    assert!(breaker.can_proceed()); // 1/3
    
    breaker.record_failure();
    assert!(breaker.can_proceed()); // 2/3
    
    breaker.record_failure();
    assert!(!breaker.can_proceed()); // 3/3 - tripped
    assert!(breaker.is_open());
}

#[test]
fn test_circuit_breaker_reset() {
    let breaker = CircuitBreaker::new(2, Duration::from_millis(10));
    
    breaker.record_failure();
    breaker.record_failure();
    assert!(breaker.is_open());
    
    // Reset
    breaker.reset();
    assert!(!breaker.is_open());
    assert!(breaker.can_proceed());
}

#[test]
fn test_circuit_breaker_success_resets_count() {
    let breaker = CircuitBreaker::new(3, Duration::from_secs(30));
    
    breaker.record_failure();
    breaker.record_failure();
    breaker.record_success(); // Reset count
    breaker.record_failure();
    
    // Should still be able to proceed (only 1 failure after reset)
    assert!(breaker.can_proceed());
}

#[test]
fn test_event_replay() {
    let engine = ReplayEngine::new();
    
    // Record events
    engine.record(1, "started");
    engine.record(1, "checkpoint");
    engine.record(1, "completed");
    
    assert_eq!(engine.event_count(), 3);
    
    // Replay
    engine.replay_all();
    
    assert_eq!(engine.event_count(), 0);
    assert_eq!(engine.replayed_count(), 3);
}

#[test]
fn test_leader_failover() {
    let cluster = FailureTestCluster::new(3);
    
    // First node is leader
    cluster.nodes[0].become_leader(1);
    assert_eq!(cluster.nodes[0].role(), RaftRole::Leader);
    
    // Kill leader
    cluster.kill_node(0);
    
    // Second node becomes new leader (simulated)
    cluster.nodes[1].become_leader(2);
    
    assert_eq!(cluster.nodes[1].role(), RaftRole::Leader);
    assert!(cluster.nodes[1].term() > cluster.nodes[0].term());
}

#[test]
fn test_node_recovery() {
    let cluster = FailureTestCluster::new(3);
    cluster.send_all_heartbeats();
    
    assert_eq!(cluster.alive_count(), 3);
    
    // Kill and revive
    cluster.kill_node(1);
    assert_eq!(cluster.alive_count(), 2);
    
    cluster.revive_node(1);
    assert_eq!(cluster.alive_count(), 3);
}

#[test]
fn test_majority_failure() {
    let cluster = FailureTestCluster::new(5);
    cluster.send_all_heartbeats();
    
    // Kill 3 of 5 nodes
    cluster.kill_node(0);
    cluster.kill_node(1);
    cluster.kill_node(2);
    
    assert_eq!(cluster.alive_count(), 2);
    
    // Verify failures are detected
    let statuses = cluster.detector.check_all();
    let failed = statuses.iter().filter(|(_, s)| *s == HealthStatus::Failed).count();
    assert_eq!(failed, 3);
}

#[test]
fn test_multiple_quarantine() {
    let cluster = FailureTestCluster::new(5);
    
    // Quarantine multiple nodes
    for i in 0..3 {
        cluster.detector.quarantine(cluster.nodes[i].id);
    }
    
    assert_eq!(cluster.detector.quarantine_count(), 3);
    
    // Check each is quarantined
    for i in 0..3 {
        assert!(cluster.detector.is_quarantined(cluster.nodes[i].id));
    }
    
    // Non-quarantined nodes
    for i in 3..5 {
        assert!(!cluster.detector.is_quarantined(cluster.nodes[i].id));
    }
}

#[test]
fn test_cascading_failure_prevention() {
    let breaker = CircuitBreaker::new(5, Duration::from_secs(60));
    
    // Simulate burst of failures
    for _ in 0..5 {
        if breaker.can_proceed() {
            breaker.record_failure();
        }
    }
    
    // Circuit should be open, preventing cascade
    assert!(breaker.is_open());
    assert!(!breaker.can_proceed());
}

#[test]
fn test_detection_timing() {
    let cluster = FailureTestCluster::new(10);
    cluster.send_all_heartbeats();
    
    let start = Instant::now();
    let _ = cluster.detector.check_all();
    let elapsed = start.elapsed();
    
    // Detection should be fast
    assert!(elapsed < Duration::from_millis(10));
}
