//! # Chaos Engineering
//!
//! Controlled fault injection for resilience testing.
//!
//! ## Philosophy
//!
//! "The best way to have a major outage is not to practice."
//!
//! Chaos engineering proactively injects failures to:
//! - Find weaknesses before they cause outages
//! - Build confidence in system resilience
//! - Train teams on incident response
//!
//! ## Principles
//!
//! 1. **Hypothesize**: Define steady state
//! 2. **Vary real-world events**: Inject realistic failures
//! 3. **Run in production**: (when safe)
//! 4. **Minimize blast radius**: Limit impact
//! 5. **Automate**: Make it continuous
//!
//! ## Fault Types
//!
//! - Latency injection
//! - Packet loss/corruption
//! - Service failures
//! - Resource exhaustion
//! - Clock skew

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use rand::prelude::*;

// ============================================================================
// Core Types
// ============================================================================

/// Unique experiment identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExperimentId(pub String);

/// Target for chaos injection
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChaosTarget {
    /// Target type (service, node, network, etc.)
    pub target_type: String,
    /// Target identifier
    pub id: String,
}

impl ChaosTarget {
    pub fn service(name: impl Into<String>) -> Self {
        Self {
            target_type: "service".into(),
            id: name.into(),
        }
    }
    
    pub fn node(id: impl Into<String>) -> Self {
        Self {
            target_type: "node".into(),
            id: id.into(),
        }
    }
    
    pub fn network(segment: impl Into<String>) -> Self {
        Self {
            target_type: "network".into(),
            id: segment.into(),
        }
    }
}

// ============================================================================
// Fault Types
// ============================================================================

/// Type of fault to inject
#[derive(Debug, Clone)]
pub enum FaultType {
    /// Add latency to operations
    Latency {
        /// Base delay
        delay: Duration,
        /// Random jitter (+/-)
        jitter: Duration,
    },
    
    /// Fail operations
    Failure {
        /// Error message
        message: String,
        /// Failure probability (0.0 - 1.0)
        probability: f64,
    },
    
    /// Timeout operations
    Timeout {
        /// Timeout duration
        duration: Duration,
    },
    
    /// Simulate network partition
    Partition {
        /// Partitioned targets
        partitioned_from: Vec<ChaosTarget>,
    },
    
    /// Corrupt data
    Corruption {
        /// Corruption probability
        probability: f64,
    },
    
    /// Resource exhaustion
    ResourceExhaustion {
        /// Resource type (cpu, memory, disk, connections)
        resource: String,
        /// Percentage to consume
        percentage: f64,
    },
    
    /// Clock skew
    ClockSkew {
        /// Offset to add to time
        offset: Duration,
        /// Direction (true = forward, false = backward)
        forward: bool,
    },
    
    /// Custom fault
    Custom {
        /// Fault name
        name: String,
        /// Parameters
        params: HashMap<String, String>,
    },
}

// ============================================================================
// Chaos Experiment
// ============================================================================

/// A chaos experiment definition
#[derive(Debug, Clone)]
pub struct ChaosExperiment {
    /// Unique identifier
    pub id: ExperimentId,
    
    /// Human-readable name
    pub name: String,
    
    /// Description
    pub description: String,
    
    /// Targets to affect
    pub targets: Vec<ChaosTarget>,
    
    /// Faults to inject
    pub faults: Vec<FaultType>,
    
    /// Percentage of traffic to affect
    pub traffic_percentage: f64,
    
    /// Duration of experiment (None = until stopped)
    pub duration: Option<Duration>,
    
    /// Steady state hypothesis
    pub hypothesis: SteadyStateHypothesis,
    
    /// Abort conditions
    pub abort_conditions: Vec<AbortCondition>,
    
    /// Tags for organization
    pub tags: Vec<String>,
}

/// Defines what "normal" looks like
#[derive(Debug, Clone)]
pub struct SteadyStateHypothesis {
    /// Metric to check
    pub metric: String,
    
    /// Expected value range
    pub min_value: f64,
    pub max_value: f64,
    
    /// Check interval
    pub check_interval: Duration,
}

/// Conditions that abort an experiment
#[derive(Debug, Clone)]
pub enum AbortCondition {
    /// Error rate exceeds threshold
    ErrorRateExceeds { threshold: f64 },
    
    /// Latency exceeds threshold
    LatencyExceeds { percentile: f64, threshold: Duration },
    
    /// Health check fails
    HealthCheckFails { endpoint: String },
    
    /// Manual abort
    ManualAbort,
    
    /// Custom condition
    Custom { name: String },
}

// ============================================================================
// Chaos Controller
// ============================================================================

/// Controls chaos experiments
pub struct ChaosController {
    /// Active experiments
    experiments: RwLock<HashMap<ExperimentId, RunningExperiment>>,
    
    /// Experiment history
    history: RwLock<Vec<ExperimentResult>>,
    
    /// Global kill switch
    enabled: AtomicBool,
    
    /// Random source
    rng: Mutex<StdRng>,
    
    /// Hooks for injection
    hooks: RwLock<Vec<Box<dyn ChaosHook + Send + Sync>>>,
    
    /// Metrics
    metrics: ChaosMetrics,
}

struct RunningExperiment {
    experiment: ChaosExperiment,
    started_at: Instant,
    affected_requests: AtomicU64,
    injected_faults: AtomicU64,
}

#[derive(Debug, Clone)]
pub struct ExperimentResult {
    pub id: ExperimentId,
    pub name: String,
    pub started_at: Instant,
    pub ended_at: Instant,
    pub outcome: ExperimentOutcome,
    pub affected_requests: u64,
    pub injected_faults: u64,
}

#[derive(Debug, Clone)]
pub enum ExperimentOutcome {
    /// Completed successfully
    Completed,
    /// Aborted due to condition
    Aborted { reason: String },
    /// Manually stopped
    Stopped,
    /// System maintained steady state
    HypothesisHeld,
    /// System violated steady state
    HypothesisViolated { details: String },
}

#[derive(Default)]
struct ChaosMetrics {
    experiments_started: AtomicU64,
    experiments_completed: AtomicU64,
    experiments_aborted: AtomicU64,
    total_faults_injected: AtomicU64,
}

impl ChaosController {
    pub fn new() -> Self {
        Self {
            experiments: RwLock::new(HashMap::new()),
            history: RwLock::new(Vec::new()),
            enabled: AtomicBool::new(false),
            rng: Mutex::new(StdRng::from_entropy()),
            hooks: RwLock::new(Vec::new()),
            metrics: ChaosMetrics::default(),
        }
    }
    
    /// Enable chaos injection
    pub fn enable(&self) {
        self.enabled.store(true, Ordering::SeqCst);
        tracing::warn!("Chaos engineering ENABLED");
    }
    
    /// Disable all chaos injection
    pub fn disable(&self) {
        self.enabled.store(false, Ordering::SeqCst);
        tracing::info!("Chaos engineering disabled");
    }
    
    /// Check if chaos is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled.load(Ordering::Relaxed)
    }
    
    /// Start an experiment
    pub fn start(&self, experiment: ChaosExperiment) -> Result<(), String> {
        if !self.is_enabled() {
            return Err("Chaos engineering is disabled".into());
        }
        
        let id = experiment.id.clone();
        
        let running = RunningExperiment {
            experiment,
            started_at: Instant::now(),
            affected_requests: AtomicU64::new(0),
            injected_faults: AtomicU64::new(0),
        };
        
        self.experiments.write().insert(id.clone(), running);
        self.metrics.experiments_started.fetch_add(1, Ordering::Relaxed);
        
        tracing::warn!(
            experiment_id = %id.0,
            "Chaos experiment started"
        );
        
        Ok(())
    }
    
    /// Stop an experiment
    pub fn stop(&self, id: &ExperimentId) -> Option<ExperimentResult> {
        let running = self.experiments.write().remove(id)?;
        
        let result = ExperimentResult {
            id: id.clone(),
            name: running.experiment.name.clone(),
            started_at: running.started_at,
            ended_at: Instant::now(),
            outcome: ExperimentOutcome::Stopped,
            affected_requests: running.affected_requests.load(Ordering::Relaxed),
            injected_faults: running.injected_faults.load(Ordering::Relaxed),
        };
        
        self.history.write().push(result.clone());
        self.metrics.experiments_completed.fetch_add(1, Ordering::Relaxed);
        
        tracing::info!(
            experiment_id = %id.0,
            "Chaos experiment stopped"
        );
        
        Some(result)
    }
    
    /// Check if a request should be affected by chaos
    pub fn should_inject(&self, target: &ChaosTarget) -> Option<Vec<FaultType>> {
        if !self.is_enabled() {
            return None;
        }
        
        let experiments = self.experiments.read();
        
        for running in experiments.values() {
            // Check if target matches
            let matches = running.experiment.targets.iter().any(|t| t == target);
            if !matches {
                continue;
            }
            
            // Check duration
            if let Some(duration) = running.experiment.duration {
                if running.started_at.elapsed() > duration {
                    continue;
                }
            }
            
            // Check traffic percentage
            let should_affect = {
                let mut rng = self.rng.lock();
                rng.gen::<f64>() < running.experiment.traffic_percentage
            };
            
            if should_affect {
                running.affected_requests.fetch_add(1, Ordering::Relaxed);
                running.injected_faults.fetch_add(running.experiment.faults.len() as u64, Ordering::Relaxed);
                self.metrics.total_faults_injected.fetch_add(running.experiment.faults.len() as u64, Ordering::Relaxed);
                
                return Some(running.experiment.faults.clone());
            }
        }
        
        None
    }
    
    /// Apply a fault (returns modified delay or error)
    pub fn apply_fault(&self, fault: &FaultType) -> FaultEffect {
        match fault {
            FaultType::Latency { delay, jitter } => {
                let jitter_ms = {
                    let mut rng = self.rng.lock();
                    rng.gen_range(0..=jitter.as_millis() as u64 * 2) as i64 - jitter.as_millis() as i64
                };
                let actual_delay = Duration::from_millis(
                    (delay.as_millis() as i64 + jitter_ms).max(0) as u64
                );
                FaultEffect::Delay(actual_delay)
            }
            
            FaultType::Failure { message, probability } => {
                let should_fail = {
                    let mut rng = self.rng.lock();
                    rng.gen::<f64>() < *probability
                };
                
                if should_fail {
                    FaultEffect::Error(message.clone())
                } else {
                    FaultEffect::None
                }
            }
            
            FaultType::Timeout { duration } => {
                FaultEffect::Delay(*duration)
            }
            
            FaultType::Partition { partitioned_from: _ } => {
                FaultEffect::Error("Network partition".into())
            }
            
            FaultType::Corruption { probability } => {
                let should_corrupt = {
                    let mut rng = self.rng.lock();
                    rng.gen::<f64>() < *probability
                };
                
                if should_corrupt {
                    FaultEffect::Corrupt
                } else {
                    FaultEffect::None
                }
            }
            
            FaultType::ResourceExhaustion { resource, percentage: _ } => {
                FaultEffect::Error(format!("{} exhausted", resource))
            }
            
            FaultType::ClockSkew { offset, forward } => {
                if *forward {
                    FaultEffect::ClockOffset(*offset)
                } else {
                    FaultEffect::ClockOffset(Duration::ZERO) // Can't go backward easily
                }
            }
            
            FaultType::Custom { name, params: _ } => {
                FaultEffect::Custom(name.clone())
            }
        }
    }
    
    /// Register a chaos hook
    pub fn register_hook<H: ChaosHook + 'static>(&self, hook: H) {
        self.hooks.write().push(Box::new(hook));
    }
    
    /// List active experiments
    pub fn active_experiments(&self) -> Vec<ExperimentId> {
        self.experiments.read().keys().cloned().collect()
    }
    
    /// Get experiment history
    pub fn history(&self) -> Vec<ExperimentResult> {
        self.history.read().clone()
    }
    
    /// Get metrics
    pub fn stats(&self) -> ChaosStats {
        ChaosStats {
            enabled: self.is_enabled(),
            active_experiments: self.experiments.read().len(),
            experiments_started: self.metrics.experiments_started.load(Ordering::Relaxed),
            experiments_completed: self.metrics.experiments_completed.load(Ordering::Relaxed),
            experiments_aborted: self.metrics.experiments_aborted.load(Ordering::Relaxed),
            total_faults_injected: self.metrics.total_faults_injected.load(Ordering::Relaxed),
        }
    }
}

impl Default for ChaosController {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum FaultEffect {
    None,
    Delay(Duration),
    Error(String),
    Corrupt,
    ClockOffset(Duration),
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct ChaosStats {
    pub enabled: bool,
    pub active_experiments: usize,
    pub experiments_started: u64,
    pub experiments_completed: u64,
    pub experiments_aborted: u64,
    pub total_faults_injected: u64,
}

/// Hook for extending chaos functionality
pub trait ChaosHook: Send + Sync {
    fn on_experiment_start(&self, experiment: &ChaosExperiment);
    fn on_fault_injected(&self, target: &ChaosTarget, fault: &FaultType);
    fn on_experiment_end(&self, result: &ExperimentResult);
}

// ============================================================================
// Latency Injector
// ============================================================================

/// Convenient latency injection
pub struct LatencyInjector {
    /// Base latency
    base: Duration,
    
    /// Jitter
    jitter: Duration,
    
    /// Probability
    probability: f64,
    
    /// Random source
    rng: Mutex<StdRng>,
}

impl LatencyInjector {
    pub fn new(base: Duration, jitter: Duration, probability: f64) -> Self {
        Self {
            base,
            jitter,
            probability,
            rng: Mutex::new(StdRng::from_entropy()),
        }
    }
    
    /// Get latency to add (if any)
    pub fn inject(&self) -> Option<Duration> {
        let mut rng = self.rng.lock();
        
        if rng.gen::<f64>() > self.probability {
            return None;
        }
        
        let jitter_ms = rng.gen_range(0..=self.jitter.as_millis() as u64 * 2) as i64
            - self.jitter.as_millis() as i64;
        
        Some(Duration::from_millis(
            (self.base.as_millis() as i64 + jitter_ms).max(0) as u64
        ))
    }
}

// ============================================================================
// Failure Injector
// ============================================================================

/// Convenient failure injection
pub struct FailureInjector {
    /// Failure probability
    probability: f64,
    
    /// Error message
    message: String,
    
    /// Random source
    rng: Mutex<StdRng>,
}

impl FailureInjector {
    pub fn new(probability: f64, message: impl Into<String>) -> Self {
        Self {
            probability,
            message: message.into(),
            rng: Mutex::new(StdRng::from_entropy()),
        }
    }
    
    /// Check if should fail, returns error message if so
    pub fn check(&self) -> Option<String> {
        let mut rng = self.rng.lock();
        
        if rng.gen::<f64>() < self.probability {
            Some(self.message.clone())
        } else {
            None
        }
    }
    
    /// Same as check but returns Result
    pub fn maybe_fail<T>(&self) -> Result<(), String> {
        match self.check() {
            Some(msg) => Err(msg),
            None => Ok(()),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_chaos_controller() {
        let controller = ChaosController::new();
        controller.enable();
        
        let experiment = ChaosExperiment {
            id: ExperimentId("test-exp".into()),
            name: "Test Experiment".into(),
            description: "Test".into(),
            targets: vec![ChaosTarget::service("test-service")],
            faults: vec![FaultType::Latency {
                delay: Duration::from_millis(100),
                jitter: Duration::from_millis(10),
            }],
            traffic_percentage: 1.0,
            duration: None,
            hypothesis: SteadyStateHypothesis {
                metric: "error_rate".into(),
                min_value: 0.0,
                max_value: 0.01,
                check_interval: Duration::from_secs(1),
            },
            abort_conditions: vec![],
            tags: vec![],
        };
        
        controller.start(experiment).unwrap();
        
        let target = ChaosTarget::service("test-service");
        let faults = controller.should_inject(&target);
        assert!(faults.is_some());
        
        let result = controller.stop(&ExperimentId("test-exp".into()));
        assert!(result.is_some());
    }
    
    #[test]
    fn test_latency_injector() {
        let injector = LatencyInjector::new(
            Duration::from_millis(100),
            Duration::from_millis(10),
            1.0, // Always inject
        );
        
        let latency = injector.inject();
        assert!(latency.is_some());
        assert!(latency.unwrap() >= Duration::from_millis(90));
    }
    
    #[test]
    fn test_failure_injector() {
        let injector = FailureInjector::new(1.0, "Test failure");
        
        let error = injector.check();
        assert!(error.is_some());
        assert_eq!(error.unwrap(), "Test failure");
        
        let never_fail = FailureInjector::new(0.0, "Never");
        assert!(never_fail.check().is_none());
    }
}
