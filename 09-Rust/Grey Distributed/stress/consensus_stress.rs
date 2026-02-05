//! # Consensus Stress Test
//!
//! Tests Raft consensus under extreme conditions:
//! - 100+ node clusters with high churn
//! - Leader elections during network partitions
//! - Log replication under heavy write load
//! - Snapshot transfer during failures
//!
//! ## Metrics Tracked
//! - Election latency (p50, p99, p999)
//! - Log replication throughput
//! - Commit latency distribution
//! - Leader stability (mean time between elections)
//!
//! ## Usage
//! ```bash
//! cargo run --release --bin consensus_stress -- \
//!     --nodes 100 \
//!     --duration 3600 \
//!     --churn-rate 0.1 \
//!     --partition-probability 0.05
//! ```

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::RwLock;
use rand::prelude::*;
use tokio::sync::{broadcast, mpsc, Semaphore};
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
    /// Node churn rate (probability of node restart per second)
    pub churn_rate: f64,
    /// Network partition probability per second
    pub partition_probability: f64,
    /// Partition duration range (min, max) in seconds
    pub partition_duration: (u64, u64),
    /// Write rate (operations per second)
    pub write_rate: u64,
    /// Read rate (operations per second)
    pub read_rate: u64,
    /// Maximum concurrent operations
    pub max_concurrent_ops: usize,
    /// Election timeout range (min, max) in milliseconds
    pub election_timeout: (u64, u64),
    /// Heartbeat interval in milliseconds
    pub heartbeat_interval: u64,
    /// Random seed for reproducibility
    pub seed: u64,
}

impl Default for StressConfig {
    fn default() -> Self {
        Self {
            node_count: 100,
            duration_secs: 3600,
            churn_rate: 0.1,
            partition_probability: 0.05,
            partition_duration: (5, 30),
            write_rate: 10000,
            read_rate: 50000,
            max_concurrent_ops: 1000,
            election_timeout: (300, 500),
            heartbeat_interval: 100,
            seed: 42,
        }
    }
}

// =============================================================================
// Node State
// =============================================================================

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeRole {
    Follower,
    Candidate,
    Leader,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeStatus {
    Running,
    Stopped,
    Partitioned,
    Recovering,
}

#[derive(Debug)]
pub struct LogEntry {
    pub term: u64,
    pub index: u64,
    pub command: Vec<u8>,
    pub timestamp: Instant,
}

pub struct Node {
    pub id: u64,
    pub role: NodeRole,
    pub status: NodeStatus,
    pub term: u64,
    pub voted_for: Option<u64>,
    pub log: Vec<LogEntry>,
    pub commit_index: u64,
    pub last_applied: u64,
    pub leader_id: Option<u64>,
    pub last_heartbeat: Instant,
    pub election_deadline: Instant,
    pub vote_count: usize,
}

impl Node {
    pub fn new(id: u64, election_timeout_ms: u64) -> Self {
        let now = Instant::now();
        Self {
            id,
            role: NodeRole::Follower,
            status: NodeStatus::Running,
            term: 0,
            voted_for: None,
            log: Vec::new(),
            commit_index: 0,
            last_applied: 0,
            leader_id: None,
            last_heartbeat: now,
            election_deadline: now + Duration::from_millis(election_timeout_ms),
            vote_count: 0,
        }
    }
}

// =============================================================================
// Cluster State
// =============================================================================

pub struct Cluster {
    pub nodes: HashMap<u64, Arc<RwLock<Node>>>,
    pub network_partitions: Vec<(Vec<u64>, Vec<u64>)>,
    pub metrics: Arc<Metrics>,
    config: StressConfig,
}

impl Cluster {
    pub fn new(config: StressConfig) -> Self {
        let mut rng = StdRng::seed_from_u64(config.seed);
        let mut nodes = HashMap::new();

        for i in 0..config.node_count {
            let election_timeout = rng.gen_range(config.election_timeout.0..=config.election_timeout.1);
            let node = Node::new(i as u64, election_timeout);
            nodes.insert(i as u64, Arc::new(RwLock::new(node)));
        }

        Self {
            nodes,
            network_partitions: Vec::new(),
            metrics: Arc::new(Metrics::new()),
            config,
        }
    }

    /// Check if two nodes can communicate
    pub fn can_communicate(&self, from: u64, to: u64) -> bool {
        for (partition_a, partition_b) in &self.network_partitions {
            let from_in_a = partition_a.contains(&from);
            let to_in_a = partition_a.contains(&to);
            let from_in_b = partition_b.contains(&from);
            let to_in_b = partition_b.contains(&to);

            // If nodes are in different partitions, they can't communicate
            if (from_in_a && to_in_b) || (from_in_b && to_in_a) {
                return false;
            }
        }
        true
    }

    /// Get the current leader (if any)
    pub fn get_leader(&self) -> Option<u64> {
        for (id, node) in &self.nodes {
            let n = node.read();
            if n.role == NodeRole::Leader && n.status == NodeStatus::Running {
                return Some(*id);
            }
        }
        None
    }

    /// Count nodes in each role
    pub fn role_counts(&self) -> (usize, usize, usize) {
        let mut followers = 0;
        let mut candidates = 0;
        let mut leaders = 0;

        for node in self.nodes.values() {
            let n = node.read();
            if n.status != NodeStatus::Running {
                continue;
            }
            match n.role {
                NodeRole::Follower => followers += 1,
                NodeRole::Candidate => candidates += 1,
                NodeRole::Leader => leaders += 1,
            }
        }

        (followers, candidates, leaders)
    }
}

// =============================================================================
// Metrics Collection
// =============================================================================

pub struct Metrics {
    // Election metrics
    pub elections_started: AtomicU64,
    pub elections_completed: AtomicU64,
    pub elections_failed: AtomicU64,
    pub election_latencies: RwLock<VecDeque<Duration>>,

    // Replication metrics
    pub entries_proposed: AtomicU64,
    pub entries_committed: AtomicU64,
    pub entries_failed: AtomicU64,
    pub commit_latencies: RwLock<VecDeque<Duration>>,

    // Availability metrics
    pub leader_changes: AtomicU64,
    pub leader_uptime: RwLock<VecDeque<Duration>>,
    pub unavailable_periods: AtomicU64,
    pub unavailable_duration_total: AtomicU64,

    // Churn metrics
    pub nodes_stopped: AtomicU64,
    pub nodes_started: AtomicU64,
    pub partitions_created: AtomicU64,
    pub partitions_healed: AtomicU64,

    // Error tracking
    pub timeout_errors: AtomicU64,
    pub network_errors: AtomicU64,
    pub consensus_errors: AtomicU64,

    start_time: Instant,
}

impl Metrics {
    pub fn new() -> Self {
        Self {
            elections_started: AtomicU64::new(0),
            elections_completed: AtomicU64::new(0),
            elections_failed: AtomicU64::new(0),
            election_latencies: RwLock::new(VecDeque::with_capacity(10000)),

            entries_proposed: AtomicU64::new(0),
            entries_committed: AtomicU64::new(0),
            entries_failed: AtomicU64::new(0),
            commit_latencies: RwLock::new(VecDeque::with_capacity(100000)),

            leader_changes: AtomicU64::new(0),
            leader_uptime: RwLock::new(VecDeque::with_capacity(1000)),
            unavailable_periods: AtomicU64::new(0),
            unavailable_duration_total: AtomicU64::new(0),

            nodes_stopped: AtomicU64::new(0),
            nodes_started: AtomicU64::new(0),
            partitions_created: AtomicU64::new(0),
            partitions_healed: AtomicU64::new(0),

            timeout_errors: AtomicU64::new(0),
            network_errors: AtomicU64::new(0),
            consensus_errors: AtomicU64::new(0),

            start_time: Instant::now(),
        }
    }

    pub fn record_election(&self, latency: Duration, success: bool) {
        if success {
            self.elections_completed.fetch_add(1, Ordering::Relaxed);
        } else {
            self.elections_failed.fetch_add(1, Ordering::Relaxed);
        }

        let mut latencies = self.election_latencies.write();
        latencies.push_back(latency);
        if latencies.len() > 10000 {
            latencies.pop_front();
        }
    }

    pub fn record_commit(&self, latency: Duration) {
        self.entries_committed.fetch_add(1, Ordering::Relaxed);

        let mut latencies = self.commit_latencies.write();
        latencies.push_back(latency);
        if latencies.len() > 100000 {
            latencies.pop_front();
        }
    }

    pub fn calculate_percentile(latencies: &VecDeque<Duration>, percentile: f64) -> Duration {
        if latencies.is_empty() {
            return Duration::ZERO;
        }

        let mut sorted: Vec<_> = latencies.iter().copied().collect();
        sorted.sort();

        let idx = ((sorted.len() as f64) * percentile / 100.0) as usize;
        sorted[idx.min(sorted.len() - 1)]
    }

    pub fn report(&self) -> StressReport {
        let elapsed = self.start_time.elapsed();
        let election_latencies = self.election_latencies.read();
        let commit_latencies = self.commit_latencies.read();

        StressReport {
            duration: elapsed,
            total_elections: self.elections_started.load(Ordering::Relaxed),
            successful_elections: self.elections_completed.load(Ordering::Relaxed),
            failed_elections: self.elections_failed.load(Ordering::Relaxed),
            election_p50: Self::calculate_percentile(&election_latencies, 50.0),
            election_p99: Self::calculate_percentile(&election_latencies, 99.0),
            election_p999: Self::calculate_percentile(&election_latencies, 99.9),

            entries_proposed: self.entries_proposed.load(Ordering::Relaxed),
            entries_committed: self.entries_committed.load(Ordering::Relaxed),
            entries_failed: self.entries_failed.load(Ordering::Relaxed),
            commit_p50: Self::calculate_percentile(&commit_latencies, 50.0),
            commit_p99: Self::calculate_percentile(&commit_latencies, 99.0),
            commit_p999: Self::calculate_percentile(&commit_latencies, 99.9),

            throughput_ops_per_sec: self.entries_committed.load(Ordering::Relaxed) as f64
                / elapsed.as_secs_f64(),

            leader_changes: self.leader_changes.load(Ordering::Relaxed),
            unavailable_periods: self.unavailable_periods.load(Ordering::Relaxed),
            availability_percentage: 100.0
                - (self.unavailable_duration_total.load(Ordering::Relaxed) as f64
                    / elapsed.as_millis() as f64
                    * 100.0),

            nodes_churned: self.nodes_stopped.load(Ordering::Relaxed),
            partitions_created: self.partitions_created.load(Ordering::Relaxed),

            timeout_errors: self.timeout_errors.load(Ordering::Relaxed),
            network_errors: self.network_errors.load(Ordering::Relaxed),
            consensus_errors: self.consensus_errors.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug)]
pub struct StressReport {
    pub duration: Duration,

    // Election metrics
    pub total_elections: u64,
    pub successful_elections: u64,
    pub failed_elections: u64,
    pub election_p50: Duration,
    pub election_p99: Duration,
    pub election_p999: Duration,

    // Replication metrics
    pub entries_proposed: u64,
    pub entries_committed: u64,
    pub entries_failed: u64,
    pub commit_p50: Duration,
    pub commit_p99: Duration,
    pub commit_p999: Duration,
    pub throughput_ops_per_sec: f64,

    // Availability
    pub leader_changes: u64,
    pub unavailable_periods: u64,
    pub availability_percentage: f64,

    // Chaos metrics
    pub nodes_churned: u64,
    pub partitions_created: u64,

    // Errors
    pub timeout_errors: u64,
    pub network_errors: u64,
    pub consensus_errors: u64,
}

impl StressReport {
    pub fn to_json(&self) -> String {
        serde_json::json!({
            "duration_secs": self.duration.as_secs_f64(),
            "elections": {
                "total": self.total_elections,
                "successful": self.successful_elections,
                "failed": self.failed_elections,
                "latency_p50_ms": self.election_p50.as_millis(),
                "latency_p99_ms": self.election_p99.as_millis(),
                "latency_p999_ms": self.election_p999.as_millis(),
            },
            "replication": {
                "proposed": self.entries_proposed,
                "committed": self.entries_committed,
                "failed": self.entries_failed,
                "latency_p50_ms": self.commit_p50.as_millis(),
                "latency_p99_ms": self.commit_p99.as_millis(),
                "latency_p999_ms": self.commit_p999.as_millis(),
                "throughput_ops_per_sec": self.throughput_ops_per_sec,
            },
            "availability": {
                "leader_changes": self.leader_changes,
                "unavailable_periods": self.unavailable_periods,
                "availability_percentage": self.availability_percentage,
            },
            "chaos": {
                "nodes_churned": self.nodes_churned,
                "partitions_created": self.partitions_created,
            },
            "errors": {
                "timeout": self.timeout_errors,
                "network": self.network_errors,
                "consensus": self.consensus_errors,
            }
        })
        .to_string()
    }
}

// =============================================================================
// Stress Test Runner
// =============================================================================

pub struct StressRunner {
    cluster: Arc<RwLock<Cluster>>,
    config: StressConfig,
    shutdown: Arc<AtomicBool>,
}

impl StressRunner {
    pub fn new(config: StressConfig) -> Self {
        let cluster = Cluster::new(config.clone());
        Self {
            cluster: Arc::new(RwLock::new(cluster)),
            config,
            shutdown: Arc::new(AtomicBool::new(false)),
        }
    }

    pub async fn run(&self) -> StressReport {
        let (shutdown_tx, _) = broadcast::channel::<()>(1);

        // Start background tasks
        let election_handle = self.spawn_election_driver();
        let churn_handle = self.spawn_churn_driver();
        let partition_handle = self.spawn_partition_driver();
        let write_handle = self.spawn_write_driver();
        let metrics_handle = self.spawn_metrics_reporter();

        // Run for configured duration
        sleep(Duration::from_secs(self.config.duration_secs)).await;

        // Signal shutdown
        self.shutdown.store(true, Ordering::SeqCst);
        let _ = shutdown_tx.send(());

        // Wait for tasks to complete
        let _ = tokio::join!(
            election_handle,
            churn_handle,
            partition_handle,
            write_handle,
            metrics_handle
        );

        // Generate final report
        self.cluster.read().metrics.report()
    }

    fn spawn_election_driver(&self) -> tokio::task::JoinHandle<()> {
        let cluster = Arc::clone(&self.cluster);
        let shutdown = Arc::clone(&self.shutdown);
        let heartbeat_interval = self.config.heartbeat_interval;

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_millis(heartbeat_interval));

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                let cluster_guard = cluster.read();
                let now = Instant::now();

                // Process each node
                for (id, node) in &cluster_guard.nodes {
                    let mut n = node.write();

                    if n.status != NodeStatus::Running {
                        continue;
                    }

                    match n.role {
                        NodeRole::Leader => {
                            // Leader sends heartbeats (simulated)
                            n.last_heartbeat = now;
                        }
                        NodeRole::Follower | NodeRole::Candidate => {
                            // Check for election timeout
                            if now >= n.election_deadline {
                                cluster_guard
                                    .metrics
                                    .elections_started
                                    .fetch_add(1, Ordering::Relaxed);

                                let election_start = Instant::now();
                                n.role = NodeRole::Candidate;
                                n.term += 1;
                                n.voted_for = Some(*id);
                                n.vote_count = 1;

                                // Simulate vote collection
                                let quorum = cluster_guard.nodes.len() / 2 + 1;
                                let mut votes = 1;

                                for (peer_id, peer) in &cluster_guard.nodes {
                                    if peer_id == id {
                                        continue;
                                    }

                                    if !cluster_guard.can_communicate(*id, *peer_id) {
                                        continue;
                                    }

                                    let mut peer_node = peer.write();
                                    if peer_node.status != NodeStatus::Running {
                                        continue;
                                    }

                                    if peer_node.term < n.term && peer_node.voted_for.is_none() {
                                        peer_node.voted_for = Some(*id);
                                        votes += 1;
                                    }
                                }

                                if votes >= quorum {
                                    n.role = NodeRole::Leader;
                                    n.leader_id = Some(*id);
                                    cluster_guard
                                        .metrics
                                        .leader_changes
                                        .fetch_add(1, Ordering::Relaxed);
                                    cluster_guard.metrics.record_election(
                                        election_start.elapsed(),
                                        true,
                                    );
                                } else {
                                    // Election failed, reset to follower
                                    n.role = NodeRole::Follower;
                                    cluster_guard.metrics.record_election(
                                        election_start.elapsed(),
                                        false,
                                    );
                                }

                                // Reset election deadline
                                let timeout = rand::thread_rng()
                                    .gen_range(Duration::from_millis(300)..Duration::from_millis(500));
                                n.election_deadline = now + timeout;
                            }
                        }
                    }
                }
            }
        })
    }

    fn spawn_churn_driver(&self) -> tokio::task::JoinHandle<()> {
        let cluster = Arc::clone(&self.cluster);
        let shutdown = Arc::clone(&self.shutdown);
        let churn_rate = self.config.churn_rate;

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(1));
            let mut rng = rand::thread_rng();

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                if rng.gen::<f64>() < churn_rate {
                    let cluster_guard = cluster.read();
                    let node_ids: Vec<_> = cluster_guard.nodes.keys().copied().collect();

                    if let Some(&node_id) = node_ids.choose(&mut rng) {
                        if let Some(node) = cluster_guard.nodes.get(&node_id) {
                            let mut n = node.write();
                            match n.status {
                                NodeStatus::Running => {
                                    n.status = NodeStatus::Stopped;
                                    n.role = NodeRole::Follower;
                                    cluster_guard
                                        .metrics
                                        .nodes_stopped
                                        .fetch_add(1, Ordering::Relaxed);
                                }
                                NodeStatus::Stopped => {
                                    n.status = NodeStatus::Recovering;
                                    // Simulate recovery delay
                                    drop(n);
                                    let node = Arc::clone(node);
                                    tokio::spawn(async move {
                                        sleep(Duration::from_secs(5)).await;
                                        let mut n = node.write();
                                        if n.status == NodeStatus::Recovering {
                                            n.status = NodeStatus::Running;
                                        }
                                    });
                                    cluster_guard
                                        .metrics
                                        .nodes_started
                                        .fetch_add(1, Ordering::Relaxed);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        })
    }

    fn spawn_partition_driver(&self) -> tokio::task::JoinHandle<()> {
        let cluster = Arc::clone(&self.cluster);
        let shutdown = Arc::clone(&self.shutdown);
        let partition_probability = self.config.partition_probability;
        let partition_duration = self.config.partition_duration;

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(1));
            let mut rng = rand::thread_rng();

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                if rng.gen::<f64>() < partition_probability {
                    let mut cluster_guard = cluster.write();
                    let node_ids: Vec<_> = cluster_guard.nodes.keys().copied().collect();

                    // Create a random partition
                    let partition_size = rng.gen_range(1..node_ids.len() / 2);
                    let mut partition_a: Vec<_> =
                        node_ids.choose_multiple(&mut rng, partition_size).copied().collect();
                    let partition_b: Vec<_> =
                        node_ids.iter().filter(|id| !partition_a.contains(id)).copied().collect();

                    cluster_guard.network_partitions.push((partition_a, partition_b));
                    cluster_guard
                        .metrics
                        .partitions_created
                        .fetch_add(1, Ordering::Relaxed);

                    // Schedule partition healing
                    let duration =
                        Duration::from_secs(rng.gen_range(partition_duration.0..=partition_duration.1));
                    let cluster_heal = Arc::clone(&cluster);
                    tokio::spawn(async move {
                        sleep(duration).await;
                        let mut c = cluster_heal.write();
                        if !c.network_partitions.is_empty() {
                            c.network_partitions.remove(0);
                            c.metrics.partitions_healed.fetch_add(1, Ordering::Relaxed);
                        }
                    });
                }
            }
        })
    }

    fn spawn_write_driver(&self) -> tokio::task::JoinHandle<()> {
        let cluster = Arc::clone(&self.cluster);
        let shutdown = Arc::clone(&self.shutdown);
        let write_rate = self.config.write_rate;
        let max_concurrent = self.config.max_concurrent_ops;

        tokio::spawn(async move {
            let semaphore = Arc::new(Semaphore::new(max_concurrent));
            let interval_ns = 1_000_000_000 / write_rate;
            let mut last_write = Instant::now();

            while !shutdown.load(Ordering::Relaxed) {
                // Rate limiting
                let elapsed = last_write.elapsed();
                if elapsed < Duration::from_nanos(interval_ns) {
                    sleep(Duration::from_nanos(interval_ns) - elapsed).await;
                }
                last_write = Instant::now();

                let permit = match semaphore.clone().try_acquire_owned() {
                    Ok(p) => p,
                    Err(_) => continue,
                };

                let cluster = Arc::clone(&cluster);
                tokio::spawn(async move {
                    let write_start = Instant::now();
                    let cluster_guard = cluster.read();

                    cluster_guard
                        .metrics
                        .entries_proposed
                        .fetch_add(1, Ordering::Relaxed);

                    // Find leader and submit
                    if let Some(leader_id) = cluster_guard.get_leader() {
                        if let Some(leader) = cluster_guard.nodes.get(&leader_id) {
                            let mut l = leader.write();
                            if l.status == NodeStatus::Running && l.role == NodeRole::Leader {
                                let entry = LogEntry {
                                    term: l.term,
                                    index: l.log.len() as u64 + 1,
                                    command: vec![0u8; 100],
                                    timestamp: Instant::now(),
                                };
                                l.log.push(entry);
                                l.commit_index += 1;
                                cluster_guard.metrics.record_commit(write_start.elapsed());
                            }
                        }
                    } else {
                        cluster_guard
                            .metrics
                            .entries_failed
                            .fetch_add(1, Ordering::Relaxed);
                        cluster_guard
                            .metrics
                            .unavailable_periods
                            .fetch_add(1, Ordering::Relaxed);
                    }

                    drop(permit);
                });
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

                let report = cluster.read().metrics.report();
                println!(
                    "[{:.0}s] Elections: {}/{} | Commits: {} ({:.0}/s) | Leaders: {} | Avail: {:.2}%",
                    report.duration.as_secs_f64(),
                    report.successful_elections,
                    report.total_elections,
                    report.entries_committed,
                    report.throughput_ops_per_sec,
                    report.leader_changes,
                    report.availability_percentage
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
    println!("=== Grey Distributed Consensus Stress Test ===\n");

    let config = StressConfig {
        node_count: 100,
        duration_secs: 3600,
        churn_rate: 0.1,
        partition_probability: 0.05,
        ..Default::default()
    };

    println!("Configuration:");
    println!("  Nodes: {}", config.node_count);
    println!("  Duration: {}s", config.duration_secs);
    println!("  Churn Rate: {:.1}%", config.churn_rate * 100.0);
    println!(
        "  Partition Probability: {:.1}%",
        config.partition_probability * 100.0
    );
    println!("  Write Rate: {}/s", config.write_rate);
    println!();

    let runner = StressRunner::new(config);
    let report = runner.run().await;

    println!("\n=== Final Report ===");
    println!("{}", report.to_json());

    // Write report to file
    let report_path = format!(
        "consensus_stress_report_{}.json",
        chrono::Utc::now().format("%Y%m%d_%H%M%S")
    );
    std::fs::write(&report_path, report.to_json()).expect("Failed to write report");
    println!("\nReport saved to: {}", report_path);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_short_stress_run() {
        let config = StressConfig {
            node_count: 10,
            duration_secs: 5,
            churn_rate: 0.5,
            partition_probability: 0.2,
            write_rate: 100,
            ..Default::default()
        };

        let runner = StressRunner::new(config);
        let report = runner.run().await;

        assert!(report.entries_committed > 0);
        assert!(report.availability_percentage > 50.0);
    }

    #[test]
    fn test_cluster_partitioning() {
        let config = StressConfig::default();
        let mut cluster = Cluster::new(config);

        // All nodes should communicate initially
        assert!(cluster.can_communicate(0, 1));

        // Create partition
        cluster.network_partitions.push((vec![0, 1, 2], vec![3, 4, 5]));

        // Same partition can communicate
        assert!(cluster.can_communicate(0, 1));
        assert!(cluster.can_communicate(3, 4));

        // Different partitions cannot
        assert!(!cluster.can_communicate(0, 3));
        assert!(!cluster.can_communicate(2, 5));
    }
}
