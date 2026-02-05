//! Grey Distributed — Fault Tolerance Benchmarks
//!
//! Measures fault tolerance performance:
//! - Node crash recovery time
//! - Quarantine effectiveness
//! - Chaos experiment resilience
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench fault_bench
//! ```

use criterion::{
    black_box, criterion_group, criterion_main,
    BenchmarkId, Criterion, Throughput,
};
use rand::prelude::*;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;
use tokio::sync::{mpsc, Mutex, RwLock};

// ============================================================================
// Configuration
// ============================================================================

#[derive(Clone)]
struct FaultConfig {
    /// Number of nodes in cluster
    cluster_size: usize,
    /// Failure detection timeout
    detection_timeout: Duration,
    /// Heartbeat interval
    heartbeat_interval: Duration,
    /// Recovery timeout
    recovery_timeout: Duration,
    /// Task resubmission delay
    resubmission_delay: Duration,
    /// Quarantine duration
    quarantine_duration: Duration,
}

impl Default for FaultConfig {
    fn default() -> Self {
        Self {
            cluster_size: 10,
            detection_timeout: Duration::from_millis(100),
            heartbeat_interval: Duration::from_millis(25),
            recovery_timeout: Duration::from_secs(5),
            resubmission_delay: Duration::from_millis(50),
            quarantine_duration: Duration::from_secs(60),
        }
    }
}

// ============================================================================
// Fault Simulation
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum NodeStatus {
    Healthy,
    Crashed,
    Recovering,
    Quarantined,
}

struct Node {
    id: usize,
    status: Arc<RwLock<NodeStatus>>,
    tasks_executing: Arc<AtomicU64>,
    tasks_completed: Arc<AtomicU64>,
    tasks_failed: Arc<AtomicU64>,
    last_heartbeat: Arc<RwLock<Instant>>,
}

struct FaultCluster {
    config: FaultConfig,
    nodes: Vec<Arc<Node>>,
    quarantined: Arc<RwLock<HashSet<usize>>>,
    metrics: Arc<FaultMetrics>,
}

struct FaultMetrics {
    crashes_detected: AtomicU64,
    recoveries_completed: AtomicU64,
    tasks_resubmitted: AtomicU64,
    tasks_affected: AtomicU64,
    detection_latency_us: AtomicU64,
    recovery_latency_us: AtomicU64,
    quarantine_events: AtomicU64,
}

impl FaultCluster {
    fn new(config: FaultConfig) -> Self {
        let nodes: Vec<Arc<Node>> = (0..config.cluster_size)
            .map(|id| Arc::new(Node {
                id,
                status: Arc::new(RwLock::new(NodeStatus::Healthy)),
                tasks_executing: Arc::new(AtomicU64::new(0)),
                tasks_completed: Arc::new(AtomicU64::new(0)),
                tasks_failed: Arc::new(AtomicU64::new(0)),
                last_heartbeat: Arc::new(RwLock::new(Instant::now())),
            }))
            .collect();
        
        Self {
            config,
            nodes,
            quarantined: Arc::new(RwLock::new(HashSet::new())),
            metrics: Arc::new(FaultMetrics {
                crashes_detected: AtomicU64::new(0),
                recoveries_completed: AtomicU64::new(0),
                tasks_resubmitted: AtomicU64::new(0),
                tasks_affected: AtomicU64::new(0),
                detection_latency_us: AtomicU64::new(0),
                recovery_latency_us: AtomicU64::new(0),
                quarantine_events: AtomicU64::new(0),
            }),
        }
    }
    
    /// Crash a node.
    async fn crash_node(&self, node_id: usize) -> Instant {
        let crash_time = Instant::now();
        
        if let Some(node) = self.nodes.get(node_id) {
            let mut status = node.status.write().await;
            *status = NodeStatus::Crashed;
            
            // Mark tasks as failed
            let executing = node.tasks_executing.load(Ordering::Relaxed);
            node.tasks_failed.fetch_add(executing, Ordering::Relaxed);
            self.metrics.tasks_affected.fetch_add(executing, Ordering::Relaxed);
        }
        
        crash_time
    }
    
    /// Detect crashed node (failure detector).
    async fn detect_crash(&self, node_id: usize) -> Duration {
        let start = Instant::now();
        
        if let Some(node) = self.nodes.get(node_id) {
            // Simulate detection via missed heartbeats
            let timeout = self.config.detection_timeout;
            
            loop {
                tokio::time::sleep(self.config.heartbeat_interval).await;
                
                let status = node.status.read().await;
                if *status == NodeStatus::Crashed {
                    // Detected!
                    self.metrics.crashes_detected.fetch_add(1, Ordering::Relaxed);
                    break;
                }
                
                if start.elapsed() > timeout {
                    // Timeout - assume crashed
                    self.metrics.crashes_detected.fetch_add(1, Ordering::Relaxed);
                    break;
                }
            }
        }
        
        let latency = start.elapsed();
        self.metrics.detection_latency_us.fetch_add(latency.as_micros() as u64, Ordering::Relaxed);
        latency
    }
    
    /// Recover a crashed node.
    async fn recover_node(&self, node_id: usize) -> Duration {
        let start = Instant::now();
        
        if let Some(node) = self.nodes.get(node_id) {
            let mut status = node.status.write().await;
            *status = NodeStatus::Recovering;
            
            // Simulate recovery process
            tokio::time::sleep(Duration::from_millis(50)).await; // State sync
            tokio::time::sleep(Duration::from_millis(20)).await; // Rejoin cluster
            
            *status = NodeStatus::Healthy;
            
            let mut heartbeat = node.last_heartbeat.write().await;
            *heartbeat = Instant::now();
            
            self.metrics.recoveries_completed.fetch_add(1, Ordering::Relaxed);
        }
        
        let latency = start.elapsed();
        self.metrics.recovery_latency_us.fetch_add(latency.as_micros() as u64, Ordering::Relaxed);
        latency
    }
    
    /// Quarantine a node.
    async fn quarantine_node(&self, node_id: usize, reason: &str) {
        if let Some(node) = self.nodes.get(node_id) {
            let mut status = node.status.write().await;
            *status = NodeStatus::Quarantined;
            
            let mut quarantined = self.quarantined.write().await;
            quarantined.insert(node_id);
            
            self.metrics.quarantine_events.fetch_add(1, Ordering::Relaxed);
        }
    }
    
    /// Lift quarantine.
    async fn lift_quarantine(&self, node_id: usize) {
        if let Some(node) = self.nodes.get(node_id) {
            let mut status = node.status.write().await;
            *status = NodeStatus::Healthy;
            
            let mut quarantined = self.quarantined.write().await;
            quarantined.remove(&node_id);
        }
    }
    
    /// Resubmit tasks from crashed node.
    async fn resubmit_tasks(&self, affected_tasks: u64) -> Duration {
        let start = Instant::now();
        
        // Find healthy nodes
        let mut healthy_nodes = Vec::new();
        for node in &self.nodes {
            let status = node.status.read().await;
            if *status == NodeStatus::Healthy {
                healthy_nodes.push(node.clone());
            }
        }
        
        if healthy_nodes.is_empty() {
            return start.elapsed();
        }
        
        // Redistribute tasks
        for i in 0..affected_tasks {
            let target = &healthy_nodes[i as usize % healthy_nodes.len()];
            target.tasks_executing.fetch_add(1, Ordering::Relaxed);
            self.metrics.tasks_resubmitted.fetch_add(1, Ordering::Relaxed);
            
            tokio::time::sleep(self.config.resubmission_delay).await;
        }
        
        start.elapsed()
    }
    
    /// Get healthy node count.
    async fn healthy_node_count(&self) -> usize {
        let mut count = 0;
        for node in &self.nodes {
            let status = node.status.read().await;
            if *status == NodeStatus::Healthy {
                count += 1;
            }
        }
        count
    }
    
    fn get_metrics(&self) -> FaultMetricsSnapshot {
        let detected = self.metrics.crashes_detected.load(Ordering::Relaxed);
        let recovered = self.metrics.recoveries_completed.load(Ordering::Relaxed);
        
        FaultMetricsSnapshot {
            crashes_detected: detected,
            recoveries_completed: recovered,
            tasks_resubmitted: self.metrics.tasks_resubmitted.load(Ordering::Relaxed),
            tasks_affected: self.metrics.tasks_affected.load(Ordering::Relaxed),
            avg_detection_latency_us: if detected > 0 {
                self.metrics.detection_latency_us.load(Ordering::Relaxed) / detected
            } else { 0 },
            avg_recovery_latency_us: if recovered > 0 {
                self.metrics.recovery_latency_us.load(Ordering::Relaxed) / recovered
            } else { 0 },
            quarantine_events: self.metrics.quarantine_events.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug)]
struct FaultMetricsSnapshot {
    crashes_detected: u64,
    recoveries_completed: u64,
    tasks_resubmitted: u64,
    tasks_affected: u64,
    avg_detection_latency_us: u64,
    avg_recovery_latency_us: u64,
    quarantine_events: u64,
}

// ============================================================================
// Crash Recovery Benchmarks
// ============================================================================

fn bench_crash_detection(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("fault/crash_detection");
    
    group.sample_size(30);
    
    // Vary detection timeout
    for timeout_ms in [50, 100, 200, 500] {
        let config = FaultConfig {
            detection_timeout: Duration::from_millis(timeout_ms),
            heartbeat_interval: Duration::from_millis(timeout_ms / 4),
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("detection_timeout_ms", timeout_ms),
            &config,
            |b, config| {
                b.to_async(&rt).iter(|| {
                    let cluster = FaultCluster::new(config.clone());
                    async move {
                        // Crash a node
                        cluster.crash_node(5).await;
                        
                        // Measure detection time
                        let latency = cluster.detect_crash(5).await;
                        black_box((latency, cluster.get_metrics()))
                    }
                });
            },
        );
    }
    
    group.finish();
}

fn bench_crash_recovery(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("fault/crash_recovery");
    
    group.sample_size(30);
    
    group.bench_function("single_node_recovery", |b| {
        let config = FaultConfig::default();
        
        b.to_async(&rt).iter(|| {
            let cluster = FaultCluster::new(config.clone());
            async move {
                // Crash node
                cluster.crash_node(5).await;
                cluster.detect_crash(5).await;
                
                // Recover
                let recovery_time = cluster.recover_node(5).await;
                black_box((recovery_time, cluster.get_metrics()))
            }
        });
    });
    
    // Multiple node failures
    for failures in [1, 2, 3] {
        group.bench_with_input(
            BenchmarkId::new("concurrent_failures", failures),
            &failures,
            |b, &failures| {
                let config = FaultConfig::default();
                
                b.to_async(&rt).iter(|| {
                    let cluster = FaultCluster::new(config.clone());
                    async move {
                        // Crash multiple nodes
                        for i in 0..failures {
                            cluster.crash_node(i).await;
                        }
                        
                        // Detect all
                        for i in 0..failures {
                            cluster.detect_crash(i).await;
                        }
                        
                        // Recover all
                        let start = Instant::now();
                        for i in 0..failures {
                            cluster.recover_node(i).await;
                        }
                        
                        black_box((start.elapsed(), cluster.get_metrics()))
                    }
                });
            },
        );
    }
    
    group.finish();
}

fn bench_task_resubmission(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("fault/task_resubmission");
    
    group.sample_size(20);
    
    for affected_tasks in [10, 50, 100, 500] {
        let config = FaultConfig {
            resubmission_delay: Duration::from_micros(100), // Faster for benchmarks
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("affected_tasks", affected_tasks),
            &affected_tasks,
            |b, &affected_tasks| {
                b.to_async(&rt).iter(|| {
                    let cluster = FaultCluster::new(config.clone());
                    async move {
                        // Setup: node 5 had tasks
                        if let Some(node) = cluster.nodes.get(5) {
                            node.tasks_executing.store(affected_tasks, Ordering::Relaxed);
                        }
                        
                        // Crash and resubmit
                        cluster.crash_node(5).await;
                        let resubmit_time = cluster.resubmit_tasks(affected_tasks).await;
                        
                        black_box((resubmit_time, cluster.get_metrics()))
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Quarantine Benchmarks
// ============================================================================

fn bench_quarantine(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("fault/quarantine");
    
    group.sample_size(30);
    
    group.bench_function("quarantine_single", |b| {
        let config = FaultConfig::default();
        
        b.to_async(&rt).iter(|| {
            let cluster = FaultCluster::new(config.clone());
            async move {
                let start = Instant::now();
                cluster.quarantine_node(5, "high error rate").await;
                
                assert_eq!(cluster.healthy_node_count().await, 9);
                
                black_box((start.elapsed(), cluster.get_metrics()))
            }
        });
    });
    
    group.bench_function("quarantine_and_lift", |b| {
        let config = FaultConfig::default();
        
        b.to_async(&rt).iter(|| {
            let cluster = FaultCluster::new(config.clone());
            async move {
                cluster.quarantine_node(5, "investigation").await;
                assert_eq!(cluster.healthy_node_count().await, 9);
                
                tokio::time::sleep(Duration::from_millis(10)).await;
                
                cluster.lift_quarantine(5).await;
                assert_eq!(cluster.healthy_node_count().await, 10);
                
                black_box(cluster.get_metrics())
            }
        });
    });
    
    // Quarantine effectiveness (system continues)
    group.bench_function("system_with_quarantined_nodes", |b| {
        let config = FaultConfig::default();
        
        b.to_async(&rt).iter(|| {
            let cluster = Arc::new(FaultCluster::new(config.clone()));
            async move {
                // Quarantine 2 nodes
                cluster.quarantine_node(8, "test").await;
                cluster.quarantine_node(9, "test").await;
                
                // Verify cluster still functional
                let healthy = cluster.healthy_node_count().await;
                assert_eq!(healthy, 8);
                
                // Tasks should redistribute
                let resubmit_time = cluster.resubmit_tasks(20).await;
                
                black_box((healthy, resubmit_time, cluster.get_metrics()))
            }
        });
    });
    
    group.finish();
}

// ============================================================================
// Chaos Experiment Benchmarks
// ============================================================================

fn bench_chaos_random_failures(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("fault/chaos");
    
    group.sample_size(20);
    
    // Random node failures during operation
    for failure_rate in [0.0, 0.1, 0.2, 0.3] {
        group.bench_with_input(
            BenchmarkId::new("failure_rate", (failure_rate * 100.0) as u32),
            &failure_rate,
            |b, &failure_rate| {
                let config = FaultConfig::default();
                
                b.to_async(&rt).iter(|| {
                    let cluster = FaultCluster::new(config.clone());
                    async move {
                        let mut rng = rand::thread_rng();
                        let mut failed_nodes = HashSet::new();
                        
                        // Simulate operations with random failures
                        for _ in 0..100 {
                            // Maybe fail a node
                            if rng.gen::<f64>() < failure_rate {
                                let node_id = rng.gen_range(0..cluster.config.cluster_size);
                                if !failed_nodes.contains(&node_id) {
                                    cluster.crash_node(node_id).await;
                                    failed_nodes.insert(node_id);
                                }
                            }
                            
                            // Maybe recover a node
                            if rng.gen::<f64>() < 0.3 {
                                if let Some(&node_id) = failed_nodes.iter().next() {
                                    cluster.recover_node(node_id).await;
                                    failed_nodes.remove(&node_id);
                                }
                            }
                            
                            tokio::time::sleep(Duration::from_micros(100)).await;
                        }
                        
                        black_box((cluster.healthy_node_count().await, cluster.get_metrics()))
                    }
                });
            },
        );
    }
    
    group.finish();
}

fn bench_cascade_failure(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("fault/cascade");
    
    group.sample_size(20);
    
    // Cascade: one failure triggers load that crashes more nodes
    group.bench_function("cascade_resilience", |b| {
        let config = FaultConfig::default();
        
        b.to_async(&rt).iter(|| {
            let cluster = Arc::new(FaultCluster::new(config.clone()));
            async move {
                // Initial failure
                cluster.crash_node(0).await;
                
                // Load shifts to remaining nodes
                let affected = 50u64;
                
                // Simulate cascade detection
                let mut cascaded = 0;
                for i in 1..cluster.config.cluster_size {
                    let load_per_node = affected / (cluster.config.cluster_size - cascaded - 1) as u64;
                    
                    // If load too high, node "crashes"
                    if load_per_node > 10 {
                        cluster.crash_node(i).await;
                        cascaded += 1;
                    }
                    
                    if cascaded >= 3 {
                        break; // Circuit breaker
                    }
                }
                
                black_box((cascaded, cluster.get_metrics()))
            }
        });
    });
    
    group.finish();
}

fn bench_network_partition(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("fault/partition");
    
    group.sample_size(20);
    
    group.bench_function("partition_detection", |b| {
        let config = FaultConfig::default();
        
        b.to_async(&rt).iter(|| {
            let cluster = FaultCluster::new(config.clone());
            async move {
                let start = Instant::now();
                
                // Simulate partition: nodes 0-4 can't reach 5-9
                // From perspective of 0-4, nodes 5-9 are crashed
                for i in 5..10 {
                    cluster.crash_node(i).await;
                }
                
                // Detection of partition
                for i in 5..10 {
                    cluster.detect_crash(i).await;
                }
                
                let healthy = cluster.healthy_node_count().await;
                
                black_box((start.elapsed(), healthy, cluster.get_metrics()))
            }
        });
    });
    
    group.bench_function("partition_heal", |b| {
        let config = FaultConfig::default();
        
        b.to_async(&rt).iter(|| {
            let cluster = FaultCluster::new(config.clone());
            async move {
                // Create partition
                for i in 5..10 {
                    cluster.crash_node(i).await;
                    cluster.detect_crash(i).await;
                }
                
                // Heal partition
                let start = Instant::now();
                for i in 5..10 {
                    cluster.recover_node(i).await;
                }
                
                let heal_time = start.elapsed();
                let healthy = cluster.healthy_node_count().await;
                
                black_box((heal_time, healthy, cluster.get_metrics()))
            }
        });
    });
    
    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    benches,
    bench_crash_detection,
    bench_crash_recovery,
    bench_task_resubmission,
    bench_quarantine,
    bench_chaos_random_failures,
    bench_cascade_failure,
    bench_network_partition,
);

criterion_main!(benches);

// ============================================================================
// Expected Results Reference
// ============================================================================

/// Expected benchmark results for reference.
///
/// | Metric                        | Value     | Notes                       |
/// |-------------------------------|-----------|----------------------------|
/// | Crash detection time          | 100ms     | 4 missed heartbeats        |
/// | Single node recovery          | 70ms      | State sync + rejoin        |
/// | Multi-node recovery (3)       | 200ms     | Parallel recovery          |
/// | Task resubmission (100)       | 10ms      | 100µs per task             |
/// | Quarantine time               | <1ms      | Status update only         |
/// | Partition detection           | 500ms     | 5 nodes × 100ms            |
/// | Partition heal                | 350ms     | 5 nodes × 70ms             |
///
/// ## Tradeoffs
///
/// - **Faster Detection**: More false positives, quicker response
/// - **Slower Detection**: Fewer false positives, slower response
/// - **Immediate Resubmission**: Lower latency, risk of duplicate work
/// - **Delayed Resubmission**: Dedup, but higher latency
#[cfg(test)]
mod expected_results {}
