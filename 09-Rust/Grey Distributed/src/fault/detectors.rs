//! # Failure Detectors
//!
//! Mechanisms for detecting failures in distributed systems.
//!
//! ## Detector Types
//!
//! 1. **Heartbeat-based**: Periodic liveness checks
//! 2. **Phi Accrual**: Probabilistic failure detection
//! 3. **Adaptive**: Self-tuning based on network conditions
//!
//! ## Properties
//!
//! - **Completeness**: Eventually detects all failures
//! - **Accuracy**: Minimizes false positives
//! - **Speed**: Low detection latency
//!
//! ## Trade-offs
//!
//! Faster detection = more false positives
//! More accurate = slower detection
//!
//! The Phi Accrual detector provides a continuous suspicion level
//! rather than binary alive/dead, letting applications choose
//! their tolerance for false positives.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};

use crate::types::NodeId;

// ============================================================================
// Core Types
// ============================================================================

/// Failure detection result
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HealthStatus {
    /// Node is healthy
    Healthy,
    /// Node might be failing (suspicion)
    Suspected { phi: f64 },
    /// Node is considered failed
    Failed,
    /// Node status unknown (no data)
    Unknown,
}

impl HealthStatus {
    pub fn is_healthy(&self) -> bool {
        matches!(self, HealthStatus::Healthy)
    }
    
    pub fn is_failed(&self) -> bool {
        matches!(self, HealthStatus::Failed)
    }
}

// ============================================================================
// Heartbeat Detector
// ============================================================================

/// Simple heartbeat-based failure detector
///
/// ## Algorithm
///
/// 1. Expect heartbeat every `interval`
/// 2. If no heartbeat for `timeout`, mark suspected
/// 3. If no heartbeat for `failure_timeout`, mark failed
///
/// ## Limitations
///
/// - Binary decision (no nuance)
/// - Same timeout for all network conditions
/// - False positives during network partitions
pub struct HeartbeatDetector {
    /// Expected heartbeat interval
    interval: Duration,
    
    /// Mark suspected after this
    suspect_timeout: Duration,
    
    /// Mark failed after this
    failure_timeout: Duration,
    
    /// Last heartbeat times
    heartbeats: RwLock<HashMap<NodeId, Instant>>,
}

impl HeartbeatDetector {
    pub fn new(interval: Duration, suspect_timeout: Duration, failure_timeout: Duration) -> Self {
        Self {
            interval,
            suspect_timeout,
            failure_timeout,
            heartbeats: RwLock::new(HashMap::new()),
        }
    }
    
    /// Record a heartbeat from a node
    pub fn heartbeat(&self, node: NodeId) {
        self.heartbeats.write().insert(node, Instant::now());
    }
    
    /// Check health of a node
    pub fn check(&self, node: NodeId) -> HealthStatus {
        let heartbeats = self.heartbeats.read();
        
        match heartbeats.get(&node) {
            None => HealthStatus::Unknown,
            Some(&last) => {
                let elapsed = last.elapsed();
                
                if elapsed < self.suspect_timeout {
                    HealthStatus::Healthy
                } else if elapsed < self.failure_timeout {
                    let phi = elapsed.as_secs_f64() / self.suspect_timeout.as_secs_f64();
                    HealthStatus::Suspected { phi }
                } else {
                    HealthStatus::Failed
                }
            }
        }
    }
    
    /// Check all nodes and return failed ones
    pub fn check_all(&self) -> Vec<(NodeId, HealthStatus)> {
        let heartbeats = self.heartbeats.read();
        let now = Instant::now();
        
        heartbeats
            .iter()
            .map(|(&node, &last)| {
                let elapsed = now.duration_since(last);
                let status = if elapsed < self.suspect_timeout {
                    HealthStatus::Healthy
                } else if elapsed < self.failure_timeout {
                    let phi = elapsed.as_secs_f64() / self.suspect_timeout.as_secs_f64();
                    HealthStatus::Suspected { phi }
                } else {
                    HealthStatus::Failed
                };
                (node, status)
            })
            .collect()
    }
    
    /// Remove a node from tracking
    pub fn remove(&self, node: NodeId) {
        self.heartbeats.write().remove(&node);
    }
}

// ============================================================================
// Phi Accrual Failure Detector
// ============================================================================

/// Phi Accrual Failure Detector
///
/// Uses heartbeat arrival time distribution to compute
/// a continuous suspicion level (phi).
///
/// ## How it works
///
/// 1. Track arrival times of heartbeats
/// 2. Compute mean and variance of inter-arrival times
/// 3. When checking, compute probability that next heartbeat would have arrived by now
/// 4. Return phi = -log10(probability)
///
/// ## Interpretation
///
/// - phi = 1: 10% chance of mistake
/// - phi = 2: 1% chance of mistake
/// - phi = 4: 0.01% chance of mistake
///
/// Higher phi = more confident node has failed
pub struct PhiAccrualDetector {
    /// Threshold for failure
    threshold: f64,
    
    /// Window size for statistics
    window_size: usize,
    
    /// Per-node state
    nodes: RwLock<HashMap<NodeId, PhiState>>,
    
    /// Default acceptable heartbeat pause
    acceptable_heartbeat_pause: Duration,
    
    /// First heartbeat estimate
    first_heartbeat_estimate: Duration,
}

struct PhiState {
    /// Recent inter-arrival times (circular buffer)
    intervals: Vec<Duration>,
    
    /// Current position in buffer
    position: usize,
    
    /// Last heartbeat time
    last_heartbeat: Instant,
    
    /// Number of samples collected
    sample_count: usize,
}

impl PhiState {
    fn new(window_size: usize) -> Self {
        Self {
            intervals: vec![Duration::ZERO; window_size],
            position: 0,
            last_heartbeat: Instant::now(),
            sample_count: 0,
        }
    }
    
    fn record(&mut self, interval: Duration, window_size: usize) {
        self.intervals[self.position] = interval;
        self.position = (self.position + 1) % window_size;
        self.sample_count = self.sample_count.saturating_add(1).min(window_size);
        self.last_heartbeat = Instant::now();
    }
    
    fn mean(&self) -> Duration {
        if self.sample_count == 0 {
            return Duration::from_millis(1000);
        }
        
        let sum: Duration = self.intervals[..self.sample_count].iter().sum();
        sum / self.sample_count as u32
    }
    
    fn variance(&self) -> f64 {
        if self.sample_count < 2 {
            return 0.0;
        }
        
        let mean = self.mean().as_secs_f64();
        let sq_diff_sum: f64 = self.intervals[..self.sample_count]
            .iter()
            .map(|d| (d.as_secs_f64() - mean).powi(2))
            .sum();
        
        sq_diff_sum / (self.sample_count - 1) as f64
    }
    
    fn std_dev(&self) -> f64 {
        self.variance().sqrt()
    }
}

impl PhiAccrualDetector {
    pub fn new(threshold: f64, window_size: usize) -> Self {
        Self {
            threshold,
            window_size,
            nodes: RwLock::new(HashMap::new()),
            acceptable_heartbeat_pause: Duration::from_secs(0),
            first_heartbeat_estimate: Duration::from_millis(500),
        }
    }
    
    /// Record a heartbeat
    pub fn heartbeat(&self, node: NodeId) {
        let now = Instant::now();
        let mut nodes = self.nodes.write();
        
        let state = nodes.entry(node).or_insert_with(|| PhiState::new(self.window_size));
        let interval = now.duration_since(state.last_heartbeat);
        state.record(interval, self.window_size);
    }
    
    /// Compute phi for a node
    pub fn phi(&self, node: NodeId) -> f64 {
        let nodes = self.nodes.read();
        
        match nodes.get(&node) {
            None => 0.0, // Unknown node, assume healthy
            Some(state) => {
                if state.sample_count < 2 {
                    return 0.0; // Not enough data
                }
                
                let elapsed = state.last_heartbeat.elapsed();
                let adjusted = elapsed + self.acceptable_heartbeat_pause;
                
                let mean = state.mean().as_secs_f64();
                let std_dev = state.std_dev().max(0.0001); // Avoid division by zero
                
                // Normal distribution CDF approximation
                let y = (adjusted.as_secs_f64() - mean) / std_dev;
                let prob = self.normal_cdf(y);
                
                // Phi = -log10(1 - CDF)
                // But we need to handle edge cases
                if prob >= 1.0 {
                    f64::MAX
                } else {
                    -((1.0 - prob).max(1e-100)).log10()
                }
            }
        }
    }
    
    /// Check health using phi threshold
    pub fn check(&self, node: NodeId) -> HealthStatus {
        let phi = self.phi(node);
        
        if phi == 0.0 {
            let nodes = self.nodes.read();
            if nodes.contains_key(&node) {
                HealthStatus::Healthy
            } else {
                HealthStatus::Unknown
            }
        } else if phi < self.threshold * 0.5 {
            HealthStatus::Healthy
        } else if phi < self.threshold {
            HealthStatus::Suspected { phi }
        } else {
            HealthStatus::Failed
        }
    }
    
    /// Get all nodes with phi above threshold
    pub fn failed_nodes(&self) -> Vec<(NodeId, f64)> {
        let nodes = self.nodes.read();
        let now = Instant::now();
        
        nodes
            .iter()
            .filter_map(|(&id, state)| {
                if state.sample_count < 2 {
                    return None;
                }
                
                let elapsed = now.duration_since(state.last_heartbeat);
                let mean = state.mean().as_secs_f64();
                let std_dev = state.std_dev().max(0.0001);
                let y = (elapsed.as_secs_f64() - mean) / std_dev;
                let prob = self.normal_cdf(y);
                let phi = if prob >= 1.0 { f64::MAX } else { -((1.0 - prob).max(1e-100)).log10() };
                
                if phi >= self.threshold {
                    Some((id, phi))
                } else {
                    None
                }
            })
            .collect()
    }
    
    // Approximation of normal CDF using error function
    fn normal_cdf(&self, x: f64) -> f64 {
        0.5 * (1.0 + self.erf(x / std::f64::consts::SQRT_2))
    }
    
    // Error function approximation (Abramowitz and Stegun)
    fn erf(&self, x: f64) -> f64 {
        let a1 = 0.254829592;
        let a2 = -0.284496736;
        let a3 = 1.421413741;
        let a4 = -1.453152027;
        let a5 = 1.061405429;
        let p = 0.3275911;
        
        let sign = if x < 0.0 { -1.0 } else { 1.0 };
        let x = x.abs();
        
        let t = 1.0 / (1.0 + p * x);
        let t2 = t * t;
        let t3 = t2 * t;
        let t4 = t3 * t;
        let t5 = t4 * t;
        
        sign * (1.0 - (a1 * t + a2 * t2 + a3 * t3 + a4 * t4 + a5 * t5) * (-x * x).exp())
    }
}

// ============================================================================
// Circuit Breaker
// ============================================================================

/// Circuit breaker for preventing cascade failures
///
/// ## States
///
/// - Closed: Normal operation, requests pass through
/// - Open: Failing, all requests rejected immediately
/// - Half-Open: Testing if service recovered
///
/// ## State Transitions
///
/// ```text
/// Closed --[failure threshold]--> Open
/// Open --[timeout]--> Half-Open
/// Half-Open --[success]--> Closed
/// Half-Open --[failure]--> Open
/// ```
pub struct CircuitBreaker {
    /// Current state
    state: RwLock<CircuitState>,
    
    /// Failure threshold to open
    failure_threshold: u32,
    
    /// Success threshold to close
    success_threshold: u32,
    
    /// Time to stay open before half-open
    open_timeout: Duration,
    
    /// Failure count in current window
    failures: AtomicU64,
    
    /// Success count in half-open
    half_open_successes: AtomicU64,
    
    /// When circuit opened
    opened_at: Mutex<Option<Instant>>,
    
    /// Circuit name for logging
    name: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CircuitState {
    Closed,
    Open,
    HalfOpen,
}

impl CircuitBreaker {
    pub fn new(name: impl Into<String>, failure_threshold: u32, open_timeout: Duration) -> Self {
        Self {
            state: RwLock::new(CircuitState::Closed),
            failure_threshold,
            success_threshold: 3,
            open_timeout,
            failures: AtomicU64::new(0),
            half_open_successes: AtomicU64::new(0),
            opened_at: Mutex::new(None),
            name: name.into(),
        }
    }
    
    /// Check if request should be allowed
    pub fn allow(&self) -> bool {
        let state = *self.state.read();
        
        match state {
            CircuitState::Closed => true,
            CircuitState::Open => {
                // Check if timeout elapsed
                let opened = self.opened_at.lock();
                if let Some(at) = *opened {
                    if at.elapsed() >= self.open_timeout {
                        drop(opened);
                        self.transition_to_half_open();
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            CircuitState::HalfOpen => true,
        }
    }
    
    /// Record a success
    pub fn record_success(&self) {
        let state = *self.state.read();
        
        match state {
            CircuitState::Closed => {
                self.failures.store(0, Ordering::Relaxed);
            }
            CircuitState::HalfOpen => {
                let successes = self.half_open_successes.fetch_add(1, Ordering::Relaxed) + 1;
                if successes >= self.success_threshold as u64 {
                    self.transition_to_closed();
                }
            }
            CircuitState::Open => {}
        }
    }
    
    /// Record a failure
    pub fn record_failure(&self) {
        let state = *self.state.read();
        
        match state {
            CircuitState::Closed => {
                let failures = self.failures.fetch_add(1, Ordering::Relaxed) + 1;
                if failures >= self.failure_threshold as u64 {
                    self.transition_to_open();
                }
            }
            CircuitState::HalfOpen => {
                self.transition_to_open();
            }
            CircuitState::Open => {}
        }
    }
    
    /// Get current state
    pub fn state(&self) -> CircuitState {
        *self.state.read()
    }
    
    fn transition_to_open(&self) {
        *self.state.write() = CircuitState::Open;
        *self.opened_at.lock() = Some(Instant::now());
        self.half_open_successes.store(0, Ordering::Relaxed);
        
        tracing::warn!(circuit = %self.name, "Circuit breaker opened");
    }
    
    fn transition_to_half_open(&self) {
        *self.state.write() = CircuitState::HalfOpen;
        self.half_open_successes.store(0, Ordering::Relaxed);
        
        tracing::info!(circuit = %self.name, "Circuit breaker half-open");
    }
    
    fn transition_to_closed(&self) {
        *self.state.write() = CircuitState::Closed;
        self.failures.store(0, Ordering::Relaxed);
        *self.opened_at.lock() = None;
        
        tracing::info!(circuit = %self.name, "Circuit breaker closed");
    }
    
    /// Force reset to closed
    pub fn reset(&self) {
        *self.state.write() = CircuitState::Closed;
        self.failures.store(0, Ordering::Relaxed);
        self.half_open_successes.store(0, Ordering::Relaxed);
        *self.opened_at.lock() = None;
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread::sleep;
    
    #[test]
    fn test_heartbeat_detector() {
        let detector = HeartbeatDetector::new(
            Duration::from_millis(100),
            Duration::from_millis(200),
            Duration::from_millis(500),
        );
        
        let node = NodeId(1);
        detector.heartbeat(node);
        
        assert!(detector.check(node).is_healthy());
        
        // Wait for suspect
        sleep(Duration::from_millis(250));
        assert!(matches!(detector.check(node), HealthStatus::Suspected { .. }));
    }
    
    #[test]
    fn test_phi_detector() {
        let detector = PhiAccrualDetector::new(8.0, 10);
        let node = NodeId(1);
        
        // Send some heartbeats
        for _ in 0..5 {
            detector.heartbeat(node);
            sleep(Duration::from_millis(10));
        }
        
        assert!(detector.check(node).is_healthy());
        assert!(detector.phi(node) < 8.0);
    }
    
    #[test]
    fn test_circuit_breaker() {
        let cb = CircuitBreaker::new("test", 3, Duration::from_millis(100));
        
        assert!(cb.allow());
        assert_eq!(cb.state(), CircuitState::Closed);
        
        // Trip the breaker
        cb.record_failure();
        cb.record_failure();
        cb.record_failure();
        
        assert_eq!(cb.state(), CircuitState::Open);
        assert!(!cb.allow());
        
        // Wait for timeout
        sleep(Duration::from_millis(150));
        assert!(cb.allow());
        assert_eq!(cb.state(), CircuitState::HalfOpen);
        
        // Recover
        cb.record_success();
        cb.record_success();
        cb.record_success();
        assert_eq!(cb.state(), CircuitState::Closed);
    }
}
