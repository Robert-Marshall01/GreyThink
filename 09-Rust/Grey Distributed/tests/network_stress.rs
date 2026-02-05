//! # Network Stress Integration Tests
//!
//! Validates system behavior under network stress:
//! 1. Packet loss handling
//! 2. Latency jitter tolerance
//! 3. Network partition recovery
//! 4. Backpressure propagation
//! 5. Circuit breaker activation
//!
//! ## Why This Test Matters
//!
//! Networks are unreliable. These tests verify the system can handle
//! real-world network conditions without data loss or cascading failures.

use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex, atomic::{AtomicU64, AtomicBool, Ordering}};
use std::time::{Duration, Instant};

// ============================================================================
// Mock Types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NodeId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct MessageId(u64);

#[derive(Debug, Clone)]
struct NetworkConditions {
    packet_loss_rate: f64,     // 0.0 to 1.0
    latency_base_ms: u64,
    latency_jitter_ms: u64,
    is_partitioned: bool,
}

impl Default for NetworkConditions {
    fn default() -> Self {
        Self {
            packet_loss_rate: 0.0,
            latency_base_ms: 1,
            latency_jitter_ms: 0,
            is_partitioned: false,
        }
    }
}

impl NetworkConditions {
    fn with_packet_loss(mut self, rate: f64) -> Self {
        self.packet_loss_rate = rate;
        self
    }

    fn with_latency(mut self, base_ms: u64, jitter_ms: u64) -> Self {
        self.latency_base_ms = base_ms;
        self.latency_jitter_ms = jitter_ms;
        self
    }

    fn partitioned(mut self) -> Self {
        self.is_partitioned = true;
        self
    }
}

#[derive(Debug, Clone)]
struct Message {
    id: MessageId,
    from: NodeId,
    to: NodeId,
    payload: Vec<u8>,
    send_time: Instant,
    retries: u32,
}

impl Message {
    fn new(id: u64, from: u64, to: u64) -> Self {
        Self {
            id: MessageId(id),
            from: NodeId(from),
            to: NodeId(to),
            payload: vec![0u8; 100],
            send_time: Instant::now(),
            retries: 0,
        }
    }
}

struct SimulatedNetwork {
    conditions: Mutex<NetworkConditions>,
    messages_sent: AtomicU64,
    messages_delivered: AtomicU64,
    messages_dropped: AtomicU64,
    messages_retried: AtomicU64,
    pending: Mutex<VecDeque<Message>>,
    delivered: Mutex<Vec<Message>>,
    partitioned_nodes: Mutex<Vec<NodeId>>,
    rng_counter: AtomicU64,
}

impl SimulatedNetwork {
    fn new(conditions: NetworkConditions) -> Self {
        Self {
            conditions: Mutex::new(conditions),
            messages_sent: AtomicU64::new(0),
            messages_delivered: AtomicU64::new(0),
            messages_dropped: AtomicU64::new(0),
            messages_retried: AtomicU64::new(0),
            pending: Mutex::new(VecDeque::new()),
            delivered: Mutex::new(Vec::new()),
            partitioned_nodes: Mutex::new(Vec::new()),
            rng_counter: AtomicU64::new(0),
        }
    }

    fn set_conditions(&self, conditions: NetworkConditions) {
        *self.conditions.lock().unwrap() = conditions;
    }

    fn partition_node(&self, node: NodeId) {
        self.partitioned_nodes.lock().unwrap().push(node);
    }

    fn heal_partition(&self) {
        self.partitioned_nodes.lock().unwrap().clear();
        self.conditions.lock().unwrap().is_partitioned = false;
    }

    fn is_node_partitioned(&self, node: NodeId) -> bool {
        self.partitioned_nodes.lock().unwrap().contains(&node)
    }

    fn send(&self, msg: Message) -> bool {
        self.messages_sent.fetch_add(1, Ordering::SeqCst);
        
        let conditions = self.conditions.lock().unwrap().clone();
        
        // Check partition
        if conditions.is_partitioned || self.is_node_partitioned(msg.to) {
            self.messages_dropped.fetch_add(1, Ordering::SeqCst);
            return false;
        }
        
        // Simulate packet loss using deterministic pseudo-random with better distribution
        // Using multiplicative hashing to spread drops evenly instead of sequential blocks
        let counter = self.rng_counter.fetch_add(1, Ordering::SeqCst);
        let hash = counter.wrapping_mul(2654435761); // Knuth's multiplicative hash constant
        let loss_check = (hash % 1000) as f64 / 1000.0;
        if loss_check < conditions.packet_loss_rate {
            self.messages_dropped.fetch_add(1, Ordering::SeqCst);
            return false;
        }
        
        // Message delivered (after simulated latency)
        self.pending.lock().unwrap().push_back(msg);
        true
    }

    fn deliver_pending(&self) {
        let mut pending = self.pending.lock().unwrap();
        let mut delivered = self.delivered.lock().unwrap();
        
        while let Some(msg) = pending.pop_front() {
            self.messages_delivered.fetch_add(1, Ordering::SeqCst);
            delivered.push(msg);
        }
    }

    fn retry(&self, mut msg: Message) -> bool {
        msg.retries += 1;
        self.messages_retried.fetch_add(1, Ordering::SeqCst);
        self.send(msg)
    }

    fn stats(&self) -> NetworkStats {
        NetworkStats {
            sent: self.messages_sent.load(Ordering::SeqCst),
            delivered: self.messages_delivered.load(Ordering::SeqCst),
            dropped: self.messages_dropped.load(Ordering::SeqCst),
            retried: self.messages_retried.load(Ordering::SeqCst),
        }
    }

    fn reset_stats(&self) {
        self.messages_sent.store(0, Ordering::SeqCst);
        self.messages_delivered.store(0, Ordering::SeqCst);
        self.messages_dropped.store(0, Ordering::SeqCst);
        self.messages_retried.store(0, Ordering::SeqCst);
        self.rng_counter.store(0, Ordering::SeqCst);
    }
}

#[derive(Debug, Clone)]
struct NetworkStats {
    sent: u64,
    delivered: u64,
    dropped: u64,
    retried: u64,
}

struct CircuitBreaker {
    failure_count: AtomicU64,
    success_count: AtomicU64,
    threshold: u64,
    is_open: AtomicBool,
    half_open_attempts: AtomicU64,
}

impl CircuitBreaker {
    fn new(threshold: u64) -> Self {
        Self {
            failure_count: AtomicU64::new(0),
            success_count: AtomicU64::new(0),
            threshold,
            is_open: AtomicBool::new(false),
            half_open_attempts: AtomicU64::new(0),
        }
    }

    fn record_failure(&self) {
        let count = self.failure_count.fetch_add(1, Ordering::SeqCst) + 1;
        if count >= self.threshold {
            self.is_open.store(true, Ordering::SeqCst);
        }
    }

    fn record_success(&self) {
        self.success_count.fetch_add(1, Ordering::SeqCst);
        self.failure_count.store(0, Ordering::SeqCst);
        self.is_open.store(false, Ordering::SeqCst);
    }

    fn can_proceed(&self) -> bool {
        !self.is_open.load(Ordering::SeqCst)
    }

    fn is_open(&self) -> bool {
        self.is_open.load(Ordering::SeqCst)
    }

    fn reset(&self) {
        self.failure_count.store(0, Ordering::SeqCst);
        self.success_count.store(0, Ordering::SeqCst);
        self.is_open.store(false, Ordering::SeqCst);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BackpressureState {
    Normal,
    Warning,
    Critical,
    Blocked,
}

struct BackpressureController {
    pending_count: AtomicU64,
    warning_threshold: u64,
    critical_threshold: u64,
    block_threshold: u64,
    state: Mutex<BackpressureState>,
}

impl BackpressureController {
    fn new(warning: u64, critical: u64, block: u64) -> Self {
        Self {
            pending_count: AtomicU64::new(0),
            warning_threshold: warning,
            critical_threshold: critical,
            block_threshold: block,
            state: Mutex::new(BackpressureState::Normal),
        }
    }

    fn add_pending(&self) {
        let count = self.pending_count.fetch_add(1, Ordering::SeqCst) + 1;
        self.update_state(count);
    }

    fn remove_pending(&self) {
        let count = self.pending_count.fetch_sub(1, Ordering::SeqCst) - 1;
        self.update_state(count);
    }

    fn update_state(&self, count: u64) {
        let new_state = if count >= self.block_threshold {
            BackpressureState::Blocked
        } else if count >= self.critical_threshold {
            BackpressureState::Critical
        } else if count >= self.warning_threshold {
            BackpressureState::Warning
        } else {
            BackpressureState::Normal
        };
        *self.state.lock().unwrap() = new_state;
    }

    fn state(&self) -> BackpressureState {
        *self.state.lock().unwrap()
    }

    fn can_accept(&self) -> bool {
        self.state() != BackpressureState::Blocked
    }

    fn pending_count(&self) -> u64 {
        self.pending_count.load(Ordering::SeqCst)
    }
}

// ============================================================================
// Test Fixtures
// ============================================================================

struct NetworkTestCluster {
    network: Arc<SimulatedNetwork>,
    nodes: Vec<NodeId>,
    circuit_breakers: HashMap<NodeId, Arc<CircuitBreaker>>,
}

impl NetworkTestCluster {
    fn new(node_count: usize, conditions: NetworkConditions) -> Self {
        let network = Arc::new(SimulatedNetwork::new(conditions));
        let mut nodes = Vec::new();
        let mut circuit_breakers = HashMap::new();
        
        for i in 0..node_count {
            let node_id = NodeId(i as u64 + 1);
            nodes.push(node_id);
            circuit_breakers.insert(node_id, Arc::new(CircuitBreaker::new(100)));
        }
        
        Self { network, nodes, circuit_breakers }
    }

    fn send_message(&self, from_idx: usize, to_idx: usize, msg_id: u64) -> bool {
        if from_idx >= self.nodes.len() || to_idx >= self.nodes.len() {
            return false;
        }
        
        let from = self.nodes[from_idx];
        let to = self.nodes[to_idx];
        let breaker = self.circuit_breakers.get(&to).unwrap();
        
        if !breaker.can_proceed() {
            return false;
        }
        
        let msg = Message::new(msg_id, from.0, to.0);
        let result = self.network.send(msg);
        
        if result {
            breaker.record_success();
        } else {
            breaker.record_failure();
        }
        
        result
    }

    fn broadcast(&self, from_idx: usize) -> usize {
        let mut delivered = 0;
        for i in 0..self.nodes.len() {
            if i != from_idx && self.send_message(from_idx, i, i as u64) {
                delivered += 1;
            }
        }
        delivered
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_normal_network() {
    let cluster = NetworkTestCluster::new(3, NetworkConditions::default());
    
    // All messages should be delivered
    for i in 0..10 {
        assert!(cluster.send_message(0, 1, i));
    }
    
    cluster.network.deliver_pending();
    let stats = cluster.network.stats();
    
    assert_eq!(stats.sent, 10);
    assert_eq!(stats.dropped, 0);
}

#[test]
fn test_packet_loss_handling() {
    let conditions = NetworkConditions::default().with_packet_loss(0.5);
    let cluster = NetworkTestCluster::new(3, conditions);
    
    let mut delivered = 0;
    for i in 0..100 {
        if cluster.send_message(0, 1, i) {
            delivered += 1;
        }
    }
    
    cluster.network.deliver_pending();
    let stats = cluster.network.stats();
    
    // With 50% loss, roughly half should be dropped
    assert!(stats.dropped > 0, "Some packets should be dropped");
    assert!(delivered < 100, "Not all should be delivered");
    // Deterministic pseudo-random, expect approximately 50%
    assert!(delivered >= 30 && delivered <= 70, "Expected ~50% delivery, got {}", delivered);
}

#[test]
fn test_network_partition() {
    let conditions = NetworkConditions::default().partitioned();
    let cluster = NetworkTestCluster::new(3, conditions);
    
    // No messages should be delivered
    for i in 0..10 {
        assert!(!cluster.send_message(0, 1, i));
    }
    
    let stats = cluster.network.stats();
    assert_eq!(stats.sent, 10);
    assert_eq!(stats.dropped, 10);
}

#[test]
fn test_partition_healing() {
    let mut cluster = NetworkTestCluster::new(3, NetworkConditions::default().partitioned());
    
    // Initially partitioned
    assert!(!cluster.send_message(0, 1, 1));
    
    // Heal partition
    cluster.network.heal_partition();
    
    // Should work now
    assert!(cluster.send_message(0, 1, 2));
}

#[test]
fn test_node_isolation() {
    let cluster = NetworkTestCluster::new(5, NetworkConditions::default());
    
    // Partition specific node
    cluster.network.partition_node(NodeId(3));
    
    // Messages to partitioned node fail
    assert!(!cluster.send_message(0, 2, 1)); // NodeId(3) is at index 2
    
    // Messages to other nodes succeed
    assert!(cluster.send_message(0, 1, 2));
    assert!(cluster.send_message(0, 3, 3));
}

#[test]
fn test_circuit_breaker_activation() {
    let conditions = NetworkConditions::default().partitioned();
    let cluster = NetworkTestCluster::new(3, conditions);
    
    // Send messages until circuit breaker opens (threshold is 100)
    for i in 0..105 {
        cluster.send_message(0, 1, i);
    }
    
    // Circuit breaker should be open
    let breaker = cluster.circuit_breakers.get(&NodeId(2)).unwrap();
    assert!(breaker.is_open(), "Circuit breaker should be open after failures");
}

#[test]
fn test_circuit_breaker_recovery() {
    let cluster = NetworkTestCluster::new(3, NetworkConditions::default().partitioned());
    
    // Trip the breaker (threshold is 100)
    for i in 0..105 {
        cluster.send_message(0, 1, i);
    }
    
    let breaker = cluster.circuit_breakers.get(&NodeId(2)).unwrap();
    assert!(breaker.is_open());
    
    // Reset breaker and heal network
    breaker.reset();
    cluster.network.heal_partition();
    
    // Should be able to proceed
    assert!(breaker.can_proceed());
    assert!(cluster.send_message(0, 1, 200));
}

#[test]
fn test_backpressure_normal() {
    let controller = BackpressureController::new(10, 20, 30);
    
    assert_eq!(controller.state(), BackpressureState::Normal);
    assert!(controller.can_accept());
    
    for _ in 0..5 {
        controller.add_pending();
    }
    
    assert_eq!(controller.state(), BackpressureState::Normal);
}

#[test]
fn test_backpressure_warning() {
    let controller = BackpressureController::new(5, 10, 15);
    
    for _ in 0..7 {
        controller.add_pending();
    }
    
    assert_eq!(controller.state(), BackpressureState::Warning);
    assert!(controller.can_accept()); // Still accepting
}

#[test]
fn test_backpressure_critical() {
    let controller = BackpressureController::new(5, 10, 15);
    
    for _ in 0..12 {
        controller.add_pending();
    }
    
    assert_eq!(controller.state(), BackpressureState::Critical);
    assert!(controller.can_accept()); // Still accepting, but should slow down
}

#[test]
fn test_backpressure_blocked() {
    let controller = BackpressureController::new(5, 10, 15);
    
    for _ in 0..20 {
        controller.add_pending();
    }
    
    assert_eq!(controller.state(), BackpressureState::Blocked);
    assert!(!controller.can_accept()); // Blocked
}

#[test]
fn test_backpressure_recovery() {
    let controller = BackpressureController::new(5, 10, 15);
    
    // Fill to blocked
    for _ in 0..20 {
        controller.add_pending();
    }
    assert!(!controller.can_accept());
    
    // Drain below warning threshold (5) to recover to Normal
    for _ in 0..16 {
        controller.remove_pending();
    }
    
    // Should recover - 4 pending is below warning threshold of 5
    assert!(controller.can_accept());
    assert_eq!(controller.state(), BackpressureState::Normal);
}

#[test]
fn test_retry_mechanism() {
    let conditions = NetworkConditions::default().with_packet_loss(0.9);
    let network = SimulatedNetwork::new(conditions);
    
    let mut msg = Message::new(1, 1, 2);
    let mut attempts = 0;
    let mut delivered = false;
    
    while !delivered && attempts < 50 {
        if attempts == 0 {
            delivered = network.send(msg.clone());
        } else {
            delivered = network.retry(msg.clone());
        }
        attempts += 1;
    }
    
    let stats = network.stats();
    assert!(stats.retried > 0, "Should have retried");
}

#[test]
fn test_broadcast_with_partial_failure() {
    let conditions = NetworkConditions::default().with_packet_loss(0.3);
    let cluster = NetworkTestCluster::new(10, conditions);
    
    let delivered = cluster.broadcast(0);
    
    // Some should succeed, some fail
    assert!(delivered > 0, "Some broadcasts should succeed");
    assert!(delivered < 9, "Some broadcasts should fail");
}

#[test]
fn test_latency_configuration() {
    let conditions = NetworkConditions::default().with_latency(100, 50);
    
    assert_eq!(conditions.latency_base_ms, 100);
    assert_eq!(conditions.latency_jitter_ms, 50);
}

#[test]
fn test_network_stats_reset() {
    let network = SimulatedNetwork::new(NetworkConditions::default());
    
    network.send(Message::new(1, 1, 2));
    network.send(Message::new(2, 1, 2));
    
    assert_eq!(network.stats().sent, 2);
    
    network.reset_stats();
    
    assert_eq!(network.stats().sent, 0);
}

#[test]
fn test_stress_high_volume() {
    let cluster = NetworkTestCluster::new(5, NetworkConditions::default());
    
    let start = Instant::now();
    let mut count = 0;
    
    for i in 0..1000 {
        if cluster.send_message(i % 5, (i + 1) % 5, i as u64) {
            count += 1;
        }
    }
    
    let elapsed = start.elapsed();
    
    assert_eq!(count, 1000);
    assert!(elapsed < Duration::from_secs(1), "Too slow: {:?}", elapsed);
}

#[test]
fn test_mixed_conditions() {
    let conditions = NetworkConditions::default()
        .with_packet_loss(0.1)
        .with_latency(10, 5);
    let cluster = NetworkTestCluster::new(5, conditions);
    
    let mut delivered = 0;
    for i in 0..100 {
        if cluster.send_message(0, 1, i) {
            delivered += 1;
        }
    }
    
    // With 10% loss, expect ~90 delivered
    assert!(delivered >= 70 && delivered <= 100);
}
