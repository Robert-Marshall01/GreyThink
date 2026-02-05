//! # Fault Injection Stress Test
//!
//! Chaos engineering experiments for Grey Distributed:
//! - Node crashes and restarts
//! - Network partitions and healing
//! - Byzantine behavior simulation
//! - Cascading failure scenarios
//! - Split-brain detection and recovery
//!
//! ## Experiments
//! - Random node termination
//! - Coordinated multi-node failure
//! - Rolling restart (upgrade simulation)
//! - Partition/heal cycles
//! - Resource exhaustion
//!
//! ## Usage
//! ```bash
//! cargo run --release --bin fault_stress -- \
//!     --nodes 100 \
//!     --duration 3600 \
//!     --experiment rolling-chaos
//! ```

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use rand::prelude::*;
use tokio::sync::{broadcast, mpsc};
use tokio::time::{interval, sleep, timeout};

// =============================================================================
// Configuration
// =============================================================================

#[derive(Clone, Debug)]
pub struct StressConfig {
    /// Number of nodes in the cluster
    pub node_count: usize,
    /// Test duration in seconds
    pub duration_secs: u64,
    /// Chaos experiment type
    pub experiment: ExperimentType,
    /// Mean time between failures (seconds)
    pub mtbf_secs: u64,
    /// Mean time to recover (seconds)
    pub mttr_secs: u64,
    /// Maximum simultaneous failures
    pub max_simultaneous_failures: usize,
    /// Coordinator count (for quorum calculations)
    pub coordinator_count: usize,
    /// Enable cascading failure simulation
    pub cascading_failures: bool,
    /// Random seed
    pub seed: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExperimentType {
    /// Random single-node failures
    RandomNodeFailure,
    /// Coordinated multi-node failures
    CoordinatedFailure,
    /// Rolling restart (upgrade simulation)
    RollingRestart,
    /// Network partition cycles
    PartitionCycle,
    /// Resource exhaustion (memory/CPU)
    ResourceExhaustion,
    /// Byzantine behavior
    Byzantine,
    /// Mixed chaos
    RollingChaos,
}

impl Default for StressConfig {
    fn default() -> Self {
        Self {
            node_count: 100,
            duration_secs: 3600,
            experiment: ExperimentType::RollingChaos,
            mtbf_secs: 60,
            mttr_secs: 30,
            max_simultaneous_failures: 20,
            coordinator_count: 5,
            cascading_failures: true,
            seed: 42,
        }
    }
}

// =============================================================================
// Node and Cluster State
// =============================================================================

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeState {
    Healthy,
    Degraded,
    Failing,
    Dead,
    Recovering,
    Partitioned,
    Byzantine,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeRole {
    Coordinator,
    Worker,
}

#[derive(Debug)]
pub struct FaultEvent {
    pub id: u64,
    pub timestamp: Instant,
    pub event_type: FaultType,
    pub affected_nodes: Vec<u64>,
    pub duration: Option<Duration>,
    pub resolved: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FaultType {
    NodeCrash,
    NodeHang,
    NetworkPartition,
    SlowDisk,
    MemoryExhaustion,
    CPUSpike,
    ClockSkew,
    ByzantineLeader,
    CascadingFailure,
}

pub struct Node {
    pub id: u64,
    pub role: NodeRole,
    pub state: NodeState,
    pub last_heartbeat: Instant,
    pub failures: u64,
    pub recoveries: u64,
    pub uptime: Duration,
    pub downtime: Duration,
    pub state_history: VecDeque<(Instant, NodeState)>,
}

impl Node {
    pub fn new(id: u64, role: NodeRole) -> Self {
        let now = Instant::now();
        Self {
            id,
            role,
            state: NodeState::Healthy,
            last_heartbeat: now,
            failures: 0,
            recoveries: 0,
            uptime: Duration::ZERO,
            downtime: Duration::ZERO,
            state_history: VecDeque::from([(now, NodeState::Healthy)]),
        }
    }

    pub fn transition(&mut self, new_state: NodeState) {
        let now = Instant::now();
        let old_state = self.state;

        // Track time in previous state
        if let Some((last_time, _)) = self.state_history.back() {
            let duration = now - *last_time;
            if old_state == NodeState::Healthy {
                self.uptime += duration;
            } else if matches!(old_state, NodeState::Dead | NodeState::Partitioned) {
                self.downtime += duration;
            }
        }

        self.state = new_state;
        self.state_history.push_back((now, new_state));

        // Track failure/recovery counts
        if old_state == NodeState::Healthy && new_state == NodeState::Dead {
            self.failures += 1;
        } else if old_state == NodeState::Dead && new_state == NodeState::Healthy {
            self.recoveries += 1;
        }

        // Limit history size
        while self.state_history.len() > 100 {
            self.state_history.pop_front();
        }
    }

    pub fn availability(&self) -> f64 {
        let total = self.uptime + self.downtime;
        if total.as_secs() == 0 {
            return 1.0;
        }
        self.uptime.as_secs_f64() / total.as_secs_f64()
    }
}

pub struct Cluster {
    pub nodes: HashMap<u64, Arc<RwLock<Node>>>,
    pub partitions: RwLock<Vec<HashSet<u64>>>,
    pub active_faults: Mutex<Vec<FaultEvent>>,
    pub fault_history: Mutex<VecDeque<FaultEvent>>,
    metrics: Arc<FaultMetrics>,
    config: StressConfig,
}

impl Cluster {
    pub fn new(config: StressConfig) -> Self {
        let mut nodes = HashMap::new();

        // Create coordinators
        for i in 0..config.coordinator_count {
            let node = Node::new(i as u64, NodeRole::Coordinator);
            nodes.insert(i as u64, Arc::new(RwLock::new(node)));
        }

        // Create workers
        for i in config.coordinator_count..config.node_count {
            let node = Node::new(i as u64, NodeRole::Worker);
            nodes.insert(i as u64, Arc::new(RwLock::new(node)));
        }

        Self {
            nodes,
            partitions: RwLock::new(Vec::new()),
            active_faults: Mutex::new(Vec::new()),
            fault_history: Mutex::new(VecDeque::with_capacity(10000)),
            metrics: Arc::new(FaultMetrics::new()),
            config,
        }
    }

    /// Count nodes by state
    pub fn state_counts(&self) -> HashMap<NodeState, usize> {
        let mut counts = HashMap::new();
        for node in self.nodes.values() {
            let state = node.read().state;
            *counts.entry(state).or_insert(0) += 1;
        }
        counts
    }

    /// Check if cluster has quorum
    pub fn has_quorum(&self) -> bool {
        let mut healthy_coordinators = 0;
        for (id, node) in &self.nodes {
            if *id < self.config.coordinator_count as u64 {
                if node.read().state == NodeState::Healthy {
                    healthy_coordinators += 1;
                }
            }
        }
        healthy_coordinators > self.config.coordinator_count / 2
    }

    /// Get nodes eligible for failure injection
    pub fn get_failure_candidates(&self) -> Vec<u64> {
        let active = self.active_faults.lock();
        let affected: HashSet<_> = active.iter()
            .flat_map(|f| f.affected_nodes.iter().copied())
            .collect();

        self.nodes.keys()
            .filter(|id| !affected.contains(id))
            .filter(|id| {
                let node = self.nodes.get(id).unwrap().read();
                node.state == NodeState::Healthy
            })
            .copied()
            .collect()
    }

    /// Record a fault event
    pub fn record_fault(&self, event: FaultEvent) {
        self.metrics.faults_injected.fetch_add(1, Ordering::Relaxed);

        match event.event_type {
            FaultType::NodeCrash => self.metrics.node_crashes.fetch_add(1, Ordering::Relaxed),
            FaultType::NetworkPartition => self.metrics.partitions_created.fetch_add(1, Ordering::Relaxed),
            FaultType::CascadingFailure => self.metrics.cascading_failures.fetch_add(1, Ordering::Relaxed),
            _ => {}
        };

        self.active_faults.lock().push(event);
    }

    /// Resolve a fault
    pub fn resolve_fault(&self, fault_id: u64) {
        let mut active = self.active_faults.lock();
        if let Some(idx) = active.iter().position(|f| f.id == fault_id) {
            let mut fault = active.remove(idx);
            fault.resolved = true;
            self.fault_history.lock().push_back(fault);
            self.metrics.faults_resolved.fetch_add(1, Ordering::Relaxed);
        }
    }
}

// =============================================================================
// Metrics Collection
// =============================================================================

pub struct FaultMetrics {
    // Fault injection counts
    pub faults_injected: AtomicU64,
    pub faults_resolved: AtomicU64,
    pub node_crashes: AtomicU64,
    pub partitions_created: AtomicU64,
    pub cascading_failures: AtomicU64,

    // Recovery metrics
    pub recoveries_successful: AtomicU64,
    pub recoveries_failed: AtomicU64,
    pub recovery_times: Mutex<VecDeque<Duration>>,

    // Availability metrics
    pub quorum_loss_events: AtomicU64,
    pub quorum_loss_duration_total: AtomicU64,
    pub split_brain_events: AtomicU64,

    // Resilience metrics
    pub operations_during_failure: AtomicU64,
    pub operations_succeeded: AtomicU64,
    pub operations_failed: AtomicU64,

    // Detection metrics
    pub detection_latencies: Mutex<VecDeque<Duration>>,
    pub false_positives: AtomicU64,
    pub false_negatives: AtomicU64,

    start_time: Instant,
}

impl FaultMetrics {
    pub fn new() -> Self {
        Self {
            faults_injected: AtomicU64::new(0),
            faults_resolved: AtomicU64::new(0),
            node_crashes: AtomicU64::new(0),
            partitions_created: AtomicU64::new(0),
            cascading_failures: AtomicU64::new(0),

            recoveries_successful: AtomicU64::new(0),
            recoveries_failed: AtomicU64::new(0),
            recovery_times: Mutex::new(VecDeque::with_capacity(10000)),

            quorum_loss_events: AtomicU64::new(0),
            quorum_loss_duration_total: AtomicU64::new(0),
            split_brain_events: AtomicU64::new(0),

            operations_during_failure: AtomicU64::new(0),
            operations_succeeded: AtomicU64::new(0),
            operations_failed: AtomicU64::new(0),

            detection_latencies: Mutex::new(VecDeque::with_capacity(10000)),
            false_positives: AtomicU64::new(0),
            false_negatives: AtomicU64::new(0),

            start_time: Instant::now(),
        }
    }

    pub fn record_recovery(&self, duration: Duration, success: bool) {
        if success {
            self.recoveries_successful.fetch_add(1, Ordering::Relaxed);
        } else {
            self.recoveries_failed.fetch_add(1, Ordering::Relaxed);
        }

        let mut times = self.recovery_times.lock();
        times.push_back(duration);
        if times.len() > 10000 {
            times.pop_front();
        }
    }

    pub fn calculate_percentiles(deque: &VecDeque<Duration>) -> RecoveryStats {
        if deque.is_empty() {
            return RecoveryStats::default();
        }

        let mut sorted: Vec<_> = deque.iter().copied().collect();
        sorted.sort();

        let len = sorted.len();
        RecoveryStats {
            p50: sorted[len * 50 / 100],
            p90: sorted[len * 90 / 100],
            p99: sorted[len * 99 / 100],
            max: sorted[len - 1],
            mean: Duration::from_nanos(
                sorted.iter().map(|d| d.as_nanos() as u64).sum::<u64>() / len as u64
            ),
        }
    }

    pub fn report(&self) -> FaultReport {
        let elapsed = self.start_time.elapsed();
        let recovery_times = self.recovery_times.lock();

        FaultReport {
            duration: elapsed,

            faults_injected: self.faults_injected.load(Ordering::Relaxed),
            faults_resolved: self.faults_resolved.load(Ordering::Relaxed),
            node_crashes: self.node_crashes.load(Ordering::Relaxed),
            partitions_created: self.partitions_created.load(Ordering::Relaxed),
            cascading_failures: self.cascading_failures.load(Ordering::Relaxed),

            recoveries_successful: self.recoveries_successful.load(Ordering::Relaxed),
            recoveries_failed: self.recoveries_failed.load(Ordering::Relaxed),
            recovery_success_rate: self.recoveries_successful.load(Ordering::Relaxed) as f64
                / (self.recoveries_successful.load(Ordering::Relaxed)
                    + self.recoveries_failed.load(Ordering::Relaxed)).max(1) as f64,
            recovery_stats: Self::calculate_percentiles(&recovery_times),

            quorum_loss_events: self.quorum_loss_events.load(Ordering::Relaxed),
            quorum_loss_duration_secs: self.quorum_loss_duration_total.load(Ordering::Relaxed) as f64 / 1000.0,
            split_brain_events: self.split_brain_events.load(Ordering::Relaxed),

            operations_during_failure: self.operations_during_failure.load(Ordering::Relaxed),
            operations_succeeded: self.operations_succeeded.load(Ordering::Relaxed),
            operation_success_rate: self.operations_succeeded.load(Ordering::Relaxed) as f64
                / self.operations_during_failure.load(Ordering::Relaxed).max(1) as f64,

            mtbf_actual_secs: elapsed.as_secs_f64()
                / self.faults_injected.load(Ordering::Relaxed).max(1) as f64,
            mttr_actual_secs: recovery_times.iter().map(|d| d.as_secs_f64()).sum::<f64>()
                / recovery_times.len().max(1) as f64,

            availability: 1.0 - (self.quorum_loss_duration_total.load(Ordering::Relaxed) as f64
                / elapsed.as_millis() as f64),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct RecoveryStats {
    pub p50: Duration,
    pub p90: Duration,
    pub p99: Duration,
    pub max: Duration,
    pub mean: Duration,
}

#[derive(Debug)]
pub struct FaultReport {
    pub duration: Duration,

    // Fault counts
    pub faults_injected: u64,
    pub faults_resolved: u64,
    pub node_crashes: u64,
    pub partitions_created: u64,
    pub cascading_failures: u64,

    // Recovery
    pub recoveries_successful: u64,
    pub recoveries_failed: u64,
    pub recovery_success_rate: f64,
    pub recovery_stats: RecoveryStats,

    // Availability
    pub quorum_loss_events: u64,
    pub quorum_loss_duration_secs: f64,
    pub split_brain_events: u64,

    // Operations
    pub operations_during_failure: u64,
    pub operations_succeeded: u64,
    pub operation_success_rate: f64,

    // Calculated metrics
    pub mtbf_actual_secs: f64,
    pub mttr_actual_secs: f64,
    pub availability: f64,
}

impl FaultReport {
    pub fn to_json(&self) -> String {
        serde_json::json!({
            "duration_secs": self.duration.as_secs_f64(),
            "faults": {
                "injected": self.faults_injected,
                "resolved": self.faults_resolved,
                "node_crashes": self.node_crashes,
                "partitions": self.partitions_created,
                "cascading": self.cascading_failures,
            },
            "recovery": {
                "successful": self.recoveries_successful,
                "failed": self.recoveries_failed,
                "success_rate": self.recovery_success_rate,
                "p50_secs": self.recovery_stats.p50.as_secs_f64(),
                "p90_secs": self.recovery_stats.p90.as_secs_f64(),
                "p99_secs": self.recovery_stats.p99.as_secs_f64(),
                "mean_secs": self.recovery_stats.mean.as_secs_f64(),
            },
            "availability": {
                "quorum_loss_events": self.quorum_loss_events,
                "quorum_loss_duration_secs": self.quorum_loss_duration_secs,
                "split_brain_events": self.split_brain_events,
                "overall": self.availability,
            },
            "operations": {
                "during_failure": self.operations_during_failure,
                "succeeded": self.operations_succeeded,
                "success_rate": self.operation_success_rate,
            },
            "metrics": {
                "mtbf_actual_secs": self.mtbf_actual_secs,
                "mttr_actual_secs": self.mttr_actual_secs,
            }
        })
        .to_string()
    }
}

// =============================================================================
// Fault Injectors
// =============================================================================

pub struct FaultInjector {
    cluster: Arc<Cluster>,
    config: StressConfig,
    fault_counter: AtomicU64,
}

impl FaultInjector {
    pub fn new(cluster: Arc<Cluster>, config: StressConfig) -> Self {
        Self {
            cluster,
            config,
            fault_counter: AtomicU64::new(0),
        }
    }

    /// Inject a node crash
    pub async fn inject_node_crash(&self, node_id: u64) -> Option<u64> {
        if let Some(node) = self.cluster.nodes.get(&node_id) {
            let fault_id = self.fault_counter.fetch_add(1, Ordering::Relaxed);

            {
                let mut n = node.write();
                n.transition(NodeState::Dead);
            }

            let event = FaultEvent {
                id: fault_id,
                timestamp: Instant::now(),
                event_type: FaultType::NodeCrash,
                affected_nodes: vec![node_id],
                duration: None,
                resolved: false,
            };

            self.cluster.record_fault(event);
            Some(fault_id)
        } else {
            None
        }
    }

    /// Inject a network partition
    pub async fn inject_partition(&self, partition_a: Vec<u64>, partition_b: Vec<u64>) -> u64 {
        let fault_id = self.fault_counter.fetch_add(1, Ordering::Relaxed);

        // Mark nodes as partitioned
        for &node_id in partition_a.iter().chain(partition_b.iter()) {
            if let Some(node) = self.cluster.nodes.get(&node_id) {
                let mut n = node.write();
                n.transition(NodeState::Partitioned);
            }
        }

        // Record partition
        {
            let mut partitions = self.cluster.partitions.write();
            partitions.push(partition_a.iter().copied().collect());
            partitions.push(partition_b.iter().copied().collect());
        }

        let mut affected = partition_a.clone();
        affected.extend(&partition_b);

        let event = FaultEvent {
            id: fault_id,
            timestamp: Instant::now(),
            event_type: FaultType::NetworkPartition,
            affected_nodes: affected,
            duration: None,
            resolved: false,
        };

        self.cluster.record_fault(event);
        fault_id
    }

    /// Inject cascading failure
    pub async fn inject_cascading_failure(&self, initial_node: u64, cascade_factor: f64) -> Vec<u64> {
        let mut affected = vec![initial_node];
        let mut rng = rand::thread_rng();

        // Start with initial failure
        self.inject_node_crash(initial_node).await;

        // Cascade to dependent nodes
        let mut current_victims = vec![initial_node];
        while !current_victims.is_empty() && affected.len() < self.config.max_simultaneous_failures {
            let mut next_victims = Vec::new();

            for &victim in &current_victims {
                // Each failure can cascade to neighbors with some probability
                for (node_id, _) in &self.cluster.nodes {
                    if !affected.contains(node_id) && rng.gen::<f64>() < cascade_factor {
                        next_victims.push(*node_id);
                        affected.push(*node_id);
                        self.inject_node_crash(*node_id).await;
                    }
                }
            }

            current_victims = next_victims;
            cascade_factor *= 0.5;  // Decay cascade probability
        }

        affected
    }

    /// Recover a node
    pub async fn recover_node(&self, node_id: u64) -> Duration {
        let start = Instant::now();

        if let Some(node) = self.cluster.nodes.get(&node_id) {
            let mut n = node.write();

            // Simulate recovery process
            n.transition(NodeState::Recovering);
            drop(n);

            // Recovery takes time
            let recovery_time = Duration::from_secs(
                rand::thread_rng().gen_range(self.config.mttr_secs / 2..=self.config.mttr_secs)
            );
            sleep(recovery_time).await;

            let mut n = node.write();
            n.transition(NodeState::Healthy);
            n.last_heartbeat = Instant::now();
        }

        start.elapsed()
    }

    /// Heal a partition
    pub async fn heal_partition(&self) {
        // Clear all partitions
        let mut partitions = self.cluster.partitions.write();
        partitions.clear();

        // Mark all partitioned nodes as healthy
        for node in self.cluster.nodes.values() {
            let mut n = node.write();
            if n.state == NodeState::Partitioned {
                n.transition(NodeState::Healthy);
            }
        }
    }
}

// =============================================================================
// Stress Test Runner
// =============================================================================

pub struct StressRunner {
    cluster: Arc<Cluster>,
    injector: Arc<FaultInjector>,
    config: StressConfig,
    shutdown: Arc<AtomicBool>,
}

impl StressRunner {
    pub fn new(config: StressConfig) -> Self {
        let cluster = Arc::new(Cluster::new(config.clone()));
        let injector = Arc::new(FaultInjector::new(Arc::clone(&cluster), config.clone()));

        Self {
            cluster,
            injector,
            config,
            shutdown: Arc::new(AtomicBool::new(false)),
        }
    }

    pub async fn run(&self) -> FaultReport {
        // Start experiment driver
        let experiment_handle = self.spawn_experiment_driver();

        // Start quorum monitor
        let quorum_handle = self.spawn_quorum_monitor();

        // Start operation simulator
        let operation_handle = self.spawn_operation_simulator();

        // Start metrics reporter
        let metrics_handle = self.spawn_metrics_reporter();

        // Run for configured duration
        sleep(Duration::from_secs(self.config.duration_secs)).await;

        // Signal shutdown
        self.shutdown.store(true, Ordering::SeqCst);

        // Wait for all tasks
        let _ = tokio::join!(
            experiment_handle,
            quorum_handle,
            operation_handle,
            metrics_handle
        );

        self.cluster.metrics.report()
    }

    fn spawn_experiment_driver(&self) -> tokio::task::JoinHandle<()> {
        let cluster = Arc::clone(&self.cluster);
        let injector = Arc::clone(&self.injector);
        let config = self.config.clone();
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut rng = StdRng::seed_from_u64(config.seed);

            match config.experiment {
                ExperimentType::RandomNodeFailure => {
                    Self::run_random_failures(&cluster, &injector, &config, &shutdown, &mut rng).await;
                }
                ExperimentType::CoordinatedFailure => {
                    Self::run_coordinated_failures(&cluster, &injector, &config, &shutdown, &mut rng).await;
                }
                ExperimentType::RollingRestart => {
                    Self::run_rolling_restart(&cluster, &injector, &config, &shutdown).await;
                }
                ExperimentType::PartitionCycle => {
                    Self::run_partition_cycles(&cluster, &injector, &config, &shutdown, &mut rng).await;
                }
                ExperimentType::ResourceExhaustion => {
                    Self::run_resource_exhaustion(&cluster, &injector, &config, &shutdown, &mut rng).await;
                }
                ExperimentType::Byzantine => {
                    Self::run_byzantine(&cluster, &injector, &config, &shutdown, &mut rng).await;
                }
                ExperimentType::RollingChaos => {
                    Self::run_rolling_chaos(&cluster, &injector, &config, &shutdown, &mut rng).await;
                }
            }
        })
    }

    async fn run_random_failures(
        cluster: &Arc<Cluster>,
        injector: &Arc<FaultInjector>,
        config: &StressConfig,
        shutdown: &Arc<AtomicBool>,
        rng: &mut StdRng,
    ) {
        while !shutdown.load(Ordering::Relaxed) {
            // Wait for MTBF
            let delay = Duration::from_secs(rng.gen_range(config.mtbf_secs / 2..=config.mtbf_secs * 2));
            sleep(delay).await;

            if shutdown.load(Ordering::Relaxed) {
                break;
            }

            // Select random node
            let candidates = cluster.get_failure_candidates();
            if let Some(&victim) = candidates.choose(rng) {
                println!("💥 Injecting crash on node {}", victim);

                if let Some(fault_id) = injector.inject_node_crash(victim).await {
                    // Schedule recovery
                    let injector = Arc::clone(injector);
                    let cluster = Arc::clone(cluster);
                    let mttr = config.mttr_secs;

                    tokio::spawn(async move {
                        sleep(Duration::from_secs(mttr)).await;
                        let recovery_time = injector.recover_node(victim).await;
                        cluster.metrics.record_recovery(recovery_time, true);
                        cluster.resolve_fault(fault_id);
                        println!("✅ Node {} recovered in {:.1}s", victim, recovery_time.as_secs_f64());
                    });
                }
            }
        }
    }

    async fn run_coordinated_failures(
        cluster: &Arc<Cluster>,
        injector: &Arc<FaultInjector>,
        config: &StressConfig,
        shutdown: &Arc<AtomicBool>,
        rng: &mut StdRng,
    ) {
        while !shutdown.load(Ordering::Relaxed) {
            let delay = Duration::from_secs(config.mtbf_secs * 3);
            sleep(delay).await;

            if shutdown.load(Ordering::Relaxed) {
                break;
            }

            // Fail multiple nodes simultaneously
            let candidates = cluster.get_failure_candidates();
            let fail_count = rng.gen_range(2..=config.max_simultaneous_failures.min(candidates.len()));
            let victims: Vec<_> = candidates.choose_multiple(rng, fail_count).copied().collect();

            println!("💥 Coordinated failure of {} nodes: {:?}", victims.len(), victims);

            for &victim in &victims {
                injector.inject_node_crash(victim).await;
            }

            // Staggered recovery
            for &victim in &victims {
                let injector = Arc::clone(injector);
                let cluster = Arc::clone(cluster);
                let delay = Duration::from_secs(rng.gen_range(config.mttr_secs..config.mttr_secs * 2));

                tokio::spawn(async move {
                    sleep(delay).await;
                    let recovery_time = injector.recover_node(victim).await;
                    cluster.metrics.record_recovery(recovery_time, true);
                });
            }
        }
    }

    async fn run_rolling_restart(
        cluster: &Arc<Cluster>,
        injector: &Arc<FaultInjector>,
        config: &StressConfig,
        shutdown: &Arc<AtomicBool>,
    ) {
        let mut node_ids: Vec<_> = cluster.nodes.keys().copied().collect();
        node_ids.sort();

        while !shutdown.load(Ordering::Relaxed) {
            println!("🔄 Starting rolling restart");

            for &node_id in &node_ids {
                if shutdown.load(Ordering::Relaxed) {
                    break;
                }

                println!("  Restarting node {}", node_id);
                injector.inject_node_crash(node_id).await;

                // Wait for recovery
                sleep(Duration::from_secs(config.mttr_secs / 2)).await;
                let recovery_time = injector.recover_node(node_id).await;
                cluster.metrics.record_recovery(recovery_time, true);

                // Brief pause before next node
                sleep(Duration::from_secs(5)).await;
            }

            println!("✅ Rolling restart complete");
            sleep(Duration::from_secs(300)).await;  // Wait before next cycle
        }
    }

    async fn run_partition_cycles(
        cluster: &Arc<Cluster>,
        injector: &Arc<FaultInjector>,
        config: &StressConfig,
        shutdown: &Arc<AtomicBool>,
        rng: &mut StdRng,
    ) {
        while !shutdown.load(Ordering::Relaxed) {
            sleep(Duration::from_secs(config.mtbf_secs * 2)).await;

            if shutdown.load(Ordering::Relaxed) {
                break;
            }

            // Create random partition
            let all_nodes: Vec<_> = cluster.nodes.keys().copied().collect();
            let split_point = rng.gen_range(all_nodes.len() / 4..all_nodes.len() * 3 / 4);
            let partition_a: Vec<_> = all_nodes[..split_point].to_vec();
            let partition_b: Vec<_> = all_nodes[split_point..].to_vec();

            println!("🔀 Creating partition: {} vs {} nodes", partition_a.len(), partition_b.len());
            let fault_id = injector.inject_partition(partition_a, partition_b).await;

            // Partition duration
            let partition_duration = Duration::from_secs(rng.gen_range(30..120));
            sleep(partition_duration).await;

            println!("🔗 Healing partition after {:.0}s", partition_duration.as_secs_f64());
            injector.heal_partition().await;
            cluster.resolve_fault(fault_id);
        }
    }

    async fn run_resource_exhaustion(
        cluster: &Arc<Cluster>,
        injector: &Arc<FaultInjector>,
        config: &StressConfig,
        shutdown: &Arc<AtomicBool>,
        rng: &mut StdRng,
    ) {
        while !shutdown.load(Ordering::Relaxed) {
            sleep(Duration::from_secs(config.mtbf_secs)).await;

            if shutdown.load(Ordering::Relaxed) {
                break;
            }

            let candidates = cluster.get_failure_candidates();
            if let Some(&victim) = candidates.choose(rng) {
                // Simulate gradual degradation
                if let Some(node) = cluster.nodes.get(&victim) {
                    println!("📈 Simulating resource exhaustion on node {}", victim);

                    let mut n = node.write();
                    n.transition(NodeState::Degraded);
                    drop(n);

                    // Eventually fails
                    let cluster = Arc::clone(cluster);
                    let injector = Arc::clone(injector);
                    tokio::spawn(async move {
                        sleep(Duration::from_secs(30)).await;
                        if let Some(node) = cluster.nodes.get(&victim) {
                            let mut n = node.write();
                            n.transition(NodeState::Dead);
                        }

                        sleep(Duration::from_secs(60)).await;
                        let recovery_time = injector.recover_node(victim).await;
                        cluster.metrics.record_recovery(recovery_time, true);
                    });
                }
            }
        }
    }

    async fn run_byzantine(
        cluster: &Arc<Cluster>,
        _injector: &Arc<FaultInjector>,
        config: &StressConfig,
        shutdown: &Arc<AtomicBool>,
        rng: &mut StdRng,
    ) {
        while !shutdown.load(Ordering::Relaxed) {
            sleep(Duration::from_secs(config.mtbf_secs * 5)).await;

            if shutdown.load(Ordering::Relaxed) {
                break;
            }

            // Byzantine node (only workers, not coordinators)
            let workers: Vec<_> = cluster.nodes.iter()
                .filter(|(id, _)| **id >= config.coordinator_count as u64)
                .map(|(id, _)| *id)
                .collect();

            if let Some(&victim) = workers.choose(rng) {
                if let Some(node) = cluster.nodes.get(&victim) {
                    println!("👿 Node {} becoming Byzantine", victim);

                    let mut n = node.write();
                    n.transition(NodeState::Byzantine);
                    drop(n);

                    // Eventually detected and removed
                    let cluster = Arc::clone(cluster);
                    tokio::spawn(async move {
                        sleep(Duration::from_secs(120)).await;
                        if let Some(node) = cluster.nodes.get(&victim) {
                            let mut n = node.write();
                            n.transition(NodeState::Dead);  // Quarantined
                            println!("🚫 Byzantine node {} quarantined", victim);
                        }
                    });
                }
            }
        }
    }

    async fn run_rolling_chaos(
        cluster: &Arc<Cluster>,
        injector: &Arc<FaultInjector>,
        config: &StressConfig,
        shutdown: &Arc<AtomicBool>,
        rng: &mut StdRng,
    ) {
        let experiments = [
            ExperimentType::RandomNodeFailure,
            ExperimentType::PartitionCycle,
            ExperimentType::ResourceExhaustion,
        ];

        let mut current_idx = 0;

        while !shutdown.load(Ordering::Relaxed) {
            let experiment = experiments[current_idx % experiments.len()];
            println!("🎲 Rolling chaos: {:?}", experiment);

            // Run for a portion of the test
            let phase_duration = config.duration_secs / 10;
            let phase_end = Instant::now() + Duration::from_secs(phase_duration);

            while Instant::now() < phase_end && !shutdown.load(Ordering::Relaxed) {
                match experiment {
                    ExperimentType::RandomNodeFailure => {
                        let candidates = cluster.get_failure_candidates();
                        if let Some(&victim) = candidates.choose(rng) {
                            injector.inject_node_crash(victim).await;

                            let injector = Arc::clone(injector);
                            let cluster = Arc::clone(cluster);
                            tokio::spawn(async move {
                                sleep(Duration::from_secs(30)).await;
                                let recovery_time = injector.recover_node(victim).await;
                                cluster.metrics.record_recovery(recovery_time, true);
                            });
                        }
                        sleep(Duration::from_secs(config.mtbf_secs)).await;
                    }
                    ExperimentType::PartitionCycle => {
                        let all_nodes: Vec<_> = cluster.nodes.keys().copied().collect();
                        let split = all_nodes.len() / 2;
                        let fault_id = injector.inject_partition(
                            all_nodes[..split].to_vec(),
                            all_nodes[split..].to_vec(),
                        ).await;

                        sleep(Duration::from_secs(30)).await;
                        injector.heal_partition().await;
                        cluster.resolve_fault(fault_id);
                        sleep(Duration::from_secs(60)).await;
                    }
                    _ => {
                        sleep(Duration::from_secs(10)).await;
                    }
                }
            }

            current_idx += 1;
        }
    }

    fn spawn_quorum_monitor(&self) -> tokio::task::JoinHandle<()> {
        let cluster = Arc::clone(&self.cluster);
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(1));
            let mut in_quorum_loss = false;
            let mut quorum_loss_start = Instant::now();

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                let has_quorum = cluster.has_quorum();

                if !has_quorum && !in_quorum_loss {
                    in_quorum_loss = true;
                    quorum_loss_start = Instant::now();
                    cluster.metrics.quorum_loss_events.fetch_add(1, Ordering::Relaxed);
                    eprintln!("⚠️  QUORUM LOST");
                } else if has_quorum && in_quorum_loss {
                    let duration = quorum_loss_start.elapsed();
                    cluster.metrics.quorum_loss_duration_total.fetch_add(
                        duration.as_millis() as u64,
                        Ordering::Relaxed,
                    );
                    in_quorum_loss = false;
                    println!("✅ Quorum restored after {:.1}s", duration.as_secs_f64());
                }
            }
        })
    }

    fn spawn_operation_simulator(&self) -> tokio::task::JoinHandle<()> {
        let cluster = Arc::clone(&self.cluster);
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_millis(10));

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                // Simulate an operation
                let has_quorum = cluster.has_quorum();
                cluster.metrics.operations_during_failure.fetch_add(1, Ordering::Relaxed);

                if has_quorum {
                    cluster.metrics.operations_succeeded.fetch_add(1, Ordering::Relaxed);
                }
            }
        })
    }

    fn spawn_metrics_reporter(&self) -> tokio::task::JoinHandle<()> {
        let cluster = Arc::clone(&self.cluster);
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(10));

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                let report = cluster.metrics.report();
                let states = cluster.state_counts();

                println!(
                    "[{:.0}s] Faults: {} | Healthy: {} | Dead: {} | Quorum: {} | Avail: {:.3}%",
                    report.duration.as_secs_f64(),
                    report.faults_injected,
                    states.get(&NodeState::Healthy).unwrap_or(&0),
                    states.get(&NodeState::Dead).unwrap_or(&0),
                    if cluster.has_quorum() { "✓" } else { "✗" },
                    report.availability * 100.0
                );
            }
        })
    }
}

// =============================================================================
// Main Entry Point
// =============================================================================

#[tokio::main]
async fn main() {
    println!("=== Grey Distributed Fault Injection Stress Test ===\n");

    let config = StressConfig {
        node_count: 100,
        duration_secs: 3600,
        experiment: ExperimentType::RollingChaos,
        coordinator_count: 5,
        mtbf_secs: 60,
        mttr_secs: 30,
        ..Default::default()
    };

    println!("Configuration:");
    println!("  Nodes: {} ({} coordinators)", config.node_count, config.coordinator_count);
    println!("  Duration: {}s", config.duration_secs);
    println!("  Experiment: {:?}", config.experiment);
    println!("  MTBF: {}s", config.mtbf_secs);
    println!("  MTTR: {}s", config.mttr_secs);
    println!();

    let runner = StressRunner::new(config);
    let report = runner.run().await;

    println!("\n=== Final Report ===");
    println!("{}", report.to_json());

    // Write report to file
    let report_path = format!(
        "fault_stress_report_{}.json",
        chrono::Utc::now().format("%Y%m%d_%H%M%S")
    );
    std::fs::write(&report_path, report.to_json()).expect("Failed to write report");
    println!("\nReport saved to: {}", report_path);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_short_fault_stress() {
        let config = StressConfig {
            node_count: 10,
            coordinator_count: 3,
            duration_secs: 5,
            mtbf_secs: 1,
            mttr_secs: 2,
            experiment: ExperimentType::RandomNodeFailure,
            ..Default::default()
        };

        let runner = StressRunner::new(config);
        let report = runner.run().await;

        assert!(report.availability > 0.5);
    }

    #[test]
    fn test_quorum_calculation() {
        let config = StressConfig {
            node_count: 10,
            coordinator_count: 5,
            ..Default::default()
        };

        let cluster = Cluster::new(config);

        // All healthy - should have quorum
        assert!(cluster.has_quorum());

        // Fail 2 coordinators - should still have quorum (3 of 5)
        cluster.nodes.get(&0).unwrap().write().transition(NodeState::Dead);
        cluster.nodes.get(&1).unwrap().write().transition(NodeState::Dead);
        assert!(cluster.has_quorum());

        // Fail 1 more - should lose quorum (2 of 5)
        cluster.nodes.get(&2).unwrap().write().transition(NodeState::Dead);
        assert!(!cluster.has_quorum());
    }
}
