//! Grey Distributed — Minimal Fault Detection Implementation
//!
//! Failure detector with heartbeat-based detection and quarantine logic.
//! Implements Phi Accrual failure detector pattern.

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, Mutex, RwLock};
use tokio::time::interval;

// ============================================================================
// Configuration
// ============================================================================

/// Fault detector configuration.
#[derive(Clone, Debug)]
pub struct FaultConfig {
    /// Heartbeat interval.
    pub heartbeat_interval: Duration,
    /// Base timeout for failure detection.
    pub detection_timeout: Duration,
    /// Number of missed heartbeats before suspicion.
    pub suspicion_threshold: u32,
    /// Number of missed heartbeats before confirmed failure.
    pub failure_threshold: u32,
    /// Quarantine duration for failed nodes.
    pub quarantine_duration: Duration,
    /// History window for failure rate calculation.
    pub history_window: usize,
}

impl Default for FaultConfig {
    fn default() -> Self {
        Self {
            heartbeat_interval: Duration::from_millis(100),
            detection_timeout: Duration::from_millis(500),
            suspicion_threshold: 3,
            failure_threshold: 5,
            quarantine_duration: Duration::from_secs(60),
            history_window: 100,
        }
    }
}

// ============================================================================
// Node State
// ============================================================================

/// State of a monitored node.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeState {
    /// Node is healthy and responding.
    Healthy,
    /// Node is suspected (missed some heartbeats).
    Suspected,
    /// Node is confirmed failed.
    Failed,
    /// Node is quarantined (temporarily excluded).
    Quarantined,
    /// Node is recovering from failure.
    Recovering,
}

/// Heartbeat history for a node.
#[derive(Debug)]
pub struct HeartbeatHistory {
    /// Recent heartbeat intervals.
    intervals: VecDeque<Duration>,
    /// Last heartbeat time.
    last_heartbeat: Instant,
    /// Missed heartbeat count.
    missed_count: u32,
    /// Current state.
    state: NodeState,
    /// Quarantine start time (if quarantined).
    quarantine_start: Option<Instant>,
    /// Failure reason.
    failure_reason: Option<String>,
}

impl HeartbeatHistory {
    fn new() -> Self {
        Self {
            intervals: VecDeque::new(),
            last_heartbeat: Instant::now(),
            missed_count: 0,
            state: NodeState::Healthy,
            quarantine_start: None,
            failure_reason: None,
        }
    }

    /// Record a heartbeat.
    fn record_heartbeat(&mut self, config: &FaultConfig) {
        let now = Instant::now();
        let interval = now.duration_since(self.last_heartbeat);
        
        self.intervals.push_back(interval);
        if self.intervals.len() > config.history_window {
            self.intervals.pop_front();
        }
        
        self.last_heartbeat = now;
        self.missed_count = 0;
        
        if self.state == NodeState::Suspected || self.state == NodeState::Recovering {
            self.state = NodeState::Healthy;
        }
    }

    /// Record a missed heartbeat.
    fn record_miss(&mut self, config: &FaultConfig) {
        self.missed_count += 1;
        
        if self.missed_count >= config.failure_threshold {
            self.state = NodeState::Failed;
            self.failure_reason = Some("Exceeded failure threshold".into());
        } else if self.missed_count >= config.suspicion_threshold {
            self.state = NodeState::Suspected;
        }
    }

    /// Calculate phi (suspicion level) using accrual detector.
    fn calculate_phi(&self, config: &FaultConfig) -> f64 {
        if self.intervals.len() < 2 {
            return 0.0;
        }

        let now = Instant::now();
        let time_since_last = now.duration_since(self.last_heartbeat);
        
        // Calculate mean and variance of intervals.
        let sum: Duration = self.intervals.iter().sum();
        let mean = sum.as_secs_f64() / self.intervals.len() as f64;
        
        let variance: f64 = self.intervals.iter()
            .map(|d| {
                let diff = d.as_secs_f64() - mean;
                diff * diff
            })
            .sum::<f64>() / self.intervals.len() as f64;
        
        let std_dev = variance.sqrt().max(0.001);
        
        // Phi = -log10(P(time_since_last > mean)).
        let z = (time_since_last.as_secs_f64() - mean) / std_dev;
        
        // Approximate using exponential distribution.
        if z <= 0.0 {
            0.0
        } else {
            z.min(16.0) // Cap at 16 for numerical stability.
        }
    }
}

// ============================================================================
// Failure Detector
// ============================================================================

/// Event types from the failure detector.
#[derive(Clone, Debug)]
pub enum FaultEvent {
    NodeSuspected { node_id: u64 },
    NodeFailed { node_id: u64, reason: String },
    NodeRecovered { node_id: u64 },
    NodeQuarantined { node_id: u64, duration: Duration },
    NodeQuarantineLifted { node_id: u64 },
}

/// Phi Accrual Failure Detector.
pub struct FailureDetector {
    config: FaultConfig,
    local_node_id: u64,
    nodes: RwLock<HashMap<u64, HeartbeatHistory>>,
    quarantined: RwLock<HashSet<u64>>,
    event_tx: mpsc::Sender<FaultEvent>,
    event_rx: Mutex<mpsc::Receiver<FaultEvent>>,
}

impl FailureDetector {
    /// Create a new failure detector.
    pub fn new(config: FaultConfig, local_node_id: u64) -> Arc<Self> {
        let (event_tx, event_rx) = mpsc::channel(1000);
        
        Arc::new(Self {
            config,
            local_node_id,
            nodes: RwLock::new(HashMap::new()),
            quarantined: RwLock::new(HashSet::new()),
            event_tx,
            event_rx: Mutex::new(event_rx),
        })
    }

    /// Register a node to monitor.
    pub async fn register_node(&self, node_id: u64) {
        let mut nodes = self.nodes.write().await;
        nodes.insert(node_id, HeartbeatHistory::new());
    }

    /// Unregister a node.
    pub async fn unregister_node(&self, node_id: u64) {
        let mut nodes = self.nodes.write().await;
        nodes.remove(&node_id);
        
        let mut quarantined = self.quarantined.write().await;
        quarantined.remove(&node_id);
    }

    /// Record a heartbeat from a node.
    pub async fn heartbeat(&self, node_id: u64) {
        let mut nodes = self.nodes.write().await;
        
        if let Some(history) = nodes.get_mut(&node_id) {
            let was_failed = history.state == NodeState::Failed 
                || history.state == NodeState::Recovering;
            
            history.record_heartbeat(&self.config);
            
            if was_failed {
                let _ = self.event_tx.send(FaultEvent::NodeRecovered { node_id }).await;
            }
        } else {
            // Auto-register on first heartbeat.
            let mut history = HeartbeatHistory::new();
            history.record_heartbeat(&self.config);
            nodes.insert(node_id, history);
        }
    }

    /// Check all nodes for failures.
    pub async fn check_nodes(&self) {
        let mut nodes = self.nodes.write().await;
        let mut events = Vec::new();
        
        for (&node_id, history) in nodes.iter_mut() {
            if history.state == NodeState::Quarantined {
                // Check if quarantine should be lifted.
                if let Some(start) = history.quarantine_start {
                    if start.elapsed() >= self.config.quarantine_duration {
                        history.state = NodeState::Recovering;
                        history.quarantine_start = None;
                        events.push(FaultEvent::NodeQuarantineLifted { node_id });
                    }
                }
                continue;
            }
            
            let time_since_last = history.last_heartbeat.elapsed();
            
            if time_since_last > self.config.heartbeat_interval {
                let prev_state = history.state;
                history.record_miss(&self.config);
                
                if history.state != prev_state {
                    match history.state {
                        NodeState::Suspected => {
                            events.push(FaultEvent::NodeSuspected { node_id });
                        }
                        NodeState::Failed => {
                            events.push(FaultEvent::NodeFailed {
                                node_id,
                                reason: history.failure_reason.clone().unwrap_or_default(),
                            });
                        }
                        _ => {}
                    }
                }
            }
        }
        
        drop(nodes);
        
        for event in events {
            let _ = self.event_tx.send(event).await;
        }
    }

    /// Quarantine a node.
    pub async fn quarantine(&self, node_id: u64, reason: &str) {
        let mut nodes = self.nodes.write().await;
        
        if let Some(history) = nodes.get_mut(&node_id) {
            history.state = NodeState::Quarantined;
            history.quarantine_start = Some(Instant::now());
            history.failure_reason = Some(reason.to_string());
        }
        
        let mut quarantined = self.quarantined.write().await;
        quarantined.insert(node_id);
        
        let _ = self.event_tx.send(FaultEvent::NodeQuarantined {
            node_id,
            duration: self.config.quarantine_duration,
        }).await;
    }

    /// Lift quarantine for a node.
    pub async fn lift_quarantine(&self, node_id: u64) {
        let mut nodes = self.nodes.write().await;
        
        if let Some(history) = nodes.get_mut(&node_id) {
            if history.state == NodeState::Quarantined {
                history.state = NodeState::Recovering;
                history.quarantine_start = None;
            }
        }
        
        let mut quarantined = self.quarantined.write().await;
        quarantined.remove(&node_id);
        
        let _ = self.event_tx.send(FaultEvent::NodeQuarantineLifted { node_id }).await;
    }

    /// Get the state of a node.
    pub async fn node_state(&self, node_id: u64) -> Option<NodeState> {
        let nodes = self.nodes.read().await;
        nodes.get(&node_id).map(|h| h.state)
    }

    /// Get phi (suspicion level) for a node.
    pub async fn phi(&self, node_id: u64) -> Option<f64> {
        let nodes = self.nodes.read().await;
        nodes.get(&node_id).map(|h| h.calculate_phi(&self.config))
    }

    /// Get all healthy nodes.
    pub async fn healthy_nodes(&self) -> Vec<u64> {
        let nodes = self.nodes.read().await;
        nodes.iter()
            .filter(|(_, h)| h.state == NodeState::Healthy)
            .map(|(&id, _)| id)
            .collect()
    }

    /// Get all failed nodes.
    pub async fn failed_nodes(&self) -> Vec<u64> {
        let nodes = self.nodes.read().await;
        nodes.iter()
            .filter(|(_, h)| h.state == NodeState::Failed || h.state == NodeState::Quarantined)
            .map(|(&id, _)| id)
            .collect()
    }

    /// Get next event.
    pub async fn next_event(&self) -> Option<FaultEvent> {
        let mut rx = self.event_rx.lock().await;
        rx.recv().await
    }

    /// Start the detector background loop.
    pub async fn run(self: Arc<Self>, mut shutdown: mpsc::Receiver<()>) {
        let mut check_interval = interval(self.config.heartbeat_interval);
        
        loop {
            tokio::select! {
                _ = check_interval.tick() => {
                    self.check_nodes().await;
                }
                _ = shutdown.recv() => {
                    break;
                }
            }
        }
    }
}

// ============================================================================
// Circuit Breaker
// ============================================================================

/// Circuit breaker state.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CircuitState {
    Closed,
    Open,
    HalfOpen,
}

/// Circuit breaker for fault isolation.
pub struct CircuitBreaker {
    node_id: u64,
    state: Mutex<CircuitState>,
    failure_count: Mutex<u32>,
    success_count: Mutex<u32>,
    failure_threshold: u32,
    success_threshold: u32,
    reset_timeout: Duration,
    last_failure: Mutex<Option<Instant>>,
}

impl CircuitBreaker {
    pub fn new(node_id: u64, failure_threshold: u32, success_threshold: u32, reset_timeout: Duration) -> Self {
        Self {
            node_id,
            state: Mutex::new(CircuitState::Closed),
            failure_count: Mutex::new(0),
            success_count: Mutex::new(0),
            failure_threshold,
            success_threshold,
            reset_timeout,
            last_failure: Mutex::new(None),
        }
    }

    /// Check if request should be allowed.
    pub async fn allow_request(&self) -> bool {
        let mut state = self.state.lock().await;
        
        match *state {
            CircuitState::Closed => true,
            CircuitState::Open => {
                let last_failure = self.last_failure.lock().await;
                if let Some(t) = *last_failure {
                    if t.elapsed() >= self.reset_timeout {
                        *state = CircuitState::HalfOpen;
                        drop(state);
                        let mut success = self.success_count.lock().await;
                        *success = 0;
                        return true;
                    }
                }
                false
            }
            CircuitState::HalfOpen => true,
        }
    }

    /// Record a success.
    pub async fn record_success(&self) {
        let mut state = self.state.lock().await;
        
        if *state == CircuitState::HalfOpen {
            let mut success = self.success_count.lock().await;
            *success += 1;
            
            if *success >= self.success_threshold {
                *state = CircuitState::Closed;
                let mut failures = self.failure_count.lock().await;
                *failures = 0;
            }
        }
    }

    /// Record a failure.
    pub async fn record_failure(&self) {
        let mut failures = self.failure_count.lock().await;
        *failures += 1;
        
        let mut last = self.last_failure.lock().await;
        *last = Some(Instant::now());
        
        if *failures >= self.failure_threshold {
            let mut state = self.state.lock().await;
            *state = CircuitState::Open;
        }
    }

    /// Get current state.
    pub async fn state(&self) -> CircuitState {
        let state = self.state.lock().await;
        *state
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_heartbeat_recording() {
        let config = FaultConfig::default();
        let detector = FailureDetector::new(config, 0);
        
        detector.register_node(1).await;
        detector.heartbeat(1).await;
        
        let state = detector.node_state(1).await;
        assert_eq!(state, Some(NodeState::Healthy));
    }

    #[tokio::test]
    async fn test_failure_detection() {
        let config = FaultConfig {
            heartbeat_interval: Duration::from_millis(10),
            suspicion_threshold: 2,
            failure_threshold: 3,
            ..Default::default()
        };
        let detector = FailureDetector::new(config, 0);
        
        detector.register_node(1).await;
        detector.heartbeat(1).await;
        
        // Wait and check for failures.
        tokio::time::sleep(Duration::from_millis(50)).await;
        detector.check_nodes().await;
        detector.check_nodes().await;
        detector.check_nodes().await;
        
        let state = detector.node_state(1).await;
        assert!(
            state == Some(NodeState::Suspected) || state == Some(NodeState::Failed),
            "Expected suspected or failed, got {:?}",
            state
        );
    }

    #[tokio::test]
    async fn test_quarantine() {
        let config = FaultConfig::default();
        let detector = FailureDetector::new(config, 0);
        
        detector.register_node(1).await;
        detector.quarantine(1, "test reason").await;
        
        let state = detector.node_state(1).await;
        assert_eq!(state, Some(NodeState::Quarantined));
        
        detector.lift_quarantine(1).await;
        
        let state = detector.node_state(1).await;
        assert_eq!(state, Some(NodeState::Recovering));
    }

    #[tokio::test]
    async fn test_recovery() {
        let config = FaultConfig {
            heartbeat_interval: Duration::from_millis(10),
            suspicion_threshold: 1,
            failure_threshold: 2,
            ..Default::default()
        };
        let detector = FailureDetector::new(config, 0);
        
        detector.register_node(1).await;
        detector.heartbeat(1).await;
        
        // Force failure.
        tokio::time::sleep(Duration::from_millis(30)).await;
        detector.check_nodes().await;
        detector.check_nodes().await;
        
        // Recover.
        detector.heartbeat(1).await;
        
        let state = detector.node_state(1).await;
        assert_eq!(state, Some(NodeState::Healthy));
    }

    #[tokio::test]
    async fn test_circuit_breaker() {
        let cb = CircuitBreaker::new(1, 3, 2, Duration::from_millis(100));
        
        // Initially closed.
        assert!(cb.allow_request().await);
        
        // Record failures.
        cb.record_failure().await;
        cb.record_failure().await;
        cb.record_failure().await;
        
        // Should be open.
        assert_eq!(cb.state().await, CircuitState::Open);
        assert!(!cb.allow_request().await);
        
        // Wait for reset timeout.
        tokio::time::sleep(Duration::from_millis(110)).await;
        
        // Should be half-open.
        assert!(cb.allow_request().await);
        assert_eq!(cb.state().await, CircuitState::HalfOpen);
        
        // Record successes.
        cb.record_success().await;
        cb.record_success().await;
        
        // Should be closed.
        assert_eq!(cb.state().await, CircuitState::Closed);
    }

    #[tokio::test]
    async fn test_healthy_nodes() {
        let config = FaultConfig::default();
        let detector = FailureDetector::new(config, 0);
        
        detector.register_node(1).await;
        detector.register_node(2).await;
        detector.register_node(3).await;
        
        detector.heartbeat(1).await;
        detector.heartbeat(2).await;
        detector.heartbeat(3).await;
        
        detector.quarantine(2, "test").await;
        
        let healthy = detector.healthy_nodes().await;
        assert_eq!(healthy.len(), 2);
        assert!(healthy.contains(&1));
        assert!(healthy.contains(&3));
    }
}
