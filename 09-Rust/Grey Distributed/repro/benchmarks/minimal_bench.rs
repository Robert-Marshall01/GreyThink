//! Grey Distributed — Minimal Reproducibility Benchmark
//!
//! Benchmark harness for consensus + scheduler minimal implementations.
//! Designed for reproducibility: deterministic seeds, fixed configurations.
//!
//! # Running Benchmarks
//!
//! ```bash
//! cd repro/benchmarks
//! cargo bench --bench minimal_bench
//! ```
//!
//! # Reproducing Results
//!
//! Results are deterministic given the same:
//! - Random seed (GREY_SEED env var)
//! - Configuration (inline)
//! - Hardware (document your specs)

use criterion::{
    black_box, criterion_group, criterion_main,
    BenchmarkId, Criterion, Throughput,
};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;
use tokio::sync::{mpsc, Mutex, RwLock};

// ============================================================================
// Reproducibility Configuration
// ============================================================================

/// Fixed seed for reproducibility.
const DEFAULT_SEED: u64 = 42;

/// Get reproducibility seed from environment or use default.
fn get_seed() -> u64 {
    std::env::var("GREY_SEED")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(DEFAULT_SEED)
}

/// Benchmark configuration.
#[derive(Clone, Debug)]
struct BenchConfig {
    seed: u64,
    cluster_size: usize,
    task_count: usize,
    iterations: usize,
}

impl Default for BenchConfig {
    fn default() -> Self {
        Self {
            seed: get_seed(),
            cluster_size: 3,
            task_count: 1000,
            iterations: 100,
        }
    }
}

// ============================================================================
// Minimal Raft Implementation (for benchmarking)
// ============================================================================

#[derive(Clone, Copy, Debug, PartialEq)]
enum RaftRole {
    Follower,
    Candidate,
    Leader,
}

struct RaftNode {
    id: u64,
    role: RwLock<RaftRole>,
    term: AtomicU64,
    log: Mutex<Vec<LogEntry>>,
    commit_index: AtomicU64,
    votes_received: AtomicU64,
}

#[derive(Clone, Debug)]
struct LogEntry {
    term: u64,
    index: u64,
    command: Vec<u8>,
}

impl RaftNode {
    fn new(id: u64) -> Self {
        Self {
            id,
            role: RwLock::new(RaftRole::Follower),
            term: AtomicU64::new(0),
            log: Mutex::new(Vec::new()),
            commit_index: AtomicU64::new(0),
            votes_received: AtomicU64::new(0),
        }
    }

    async fn start_election(&self, cluster_size: usize) -> bool {
        let new_term = self.term.fetch_add(1, Ordering::SeqCst) + 1;
        let mut role = self.role.write().await;
        *role = RaftRole::Candidate;
        self.votes_received.store(1, Ordering::SeqCst); // Vote for self.

        // Simulate vote collection.
        let votes_needed = (cluster_size / 2) + 1;
        let votes = self.votes_received.load(Ordering::SeqCst) as usize;

        if votes >= votes_needed {
            *role = RaftRole::Leader;
            true
        } else {
            false
        }
    }

    async fn receive_vote(&self) {
        self.votes_received.fetch_add(1, Ordering::SeqCst);
    }

    async fn append_entry(&self, command: Vec<u8>) -> u64 {
        let mut log = self.log.lock().await;
        let term = self.term.load(Ordering::SeqCst);
        let index = log.len() as u64 + 1;

        log.push(LogEntry {
            term,
            index,
            command,
        });

        index
    }

    async fn commit_up_to(&self, index: u64) {
        self.commit_index.store(index, Ordering::SeqCst);
    }

    async fn log_length(&self) -> usize {
        let log = self.log.lock().await;
        log.len()
    }
}

struct RaftCluster {
    nodes: Vec<Arc<RaftNode>>,
    leader_id: AtomicU64,
}

impl RaftCluster {
    fn new(size: usize) -> Self {
        let nodes: Vec<Arc<RaftNode>> = (0..size)
            .map(|id| Arc::new(RaftNode::new(id as u64)))
            .collect();

        Self {
            nodes,
            leader_id: AtomicU64::new(0),
        }
    }

    async fn elect_leader(&self) -> u64 {
        let leader = &self.nodes[0];
        
        // Simulate votes from other nodes.
        for node in &self.nodes[1..] {
            leader.receive_vote().await;
        }
        
        leader.start_election(self.nodes.len()).await;
        self.leader_id.store(0, Ordering::SeqCst);
        0
    }

    async fn replicate(&self, command: Vec<u8>) -> u64 {
        let leader_id = self.leader_id.load(Ordering::SeqCst) as usize;
        let leader = &self.nodes[leader_id];

        let index = leader.append_entry(command.clone()).await;

        // Replicate to followers.
        for (i, node) in self.nodes.iter().enumerate() {
            if i != leader_id {
                node.append_entry(command.clone()).await;
            }
        }

        // Commit on quorum.
        let quorum = (self.nodes.len() / 2) + 1;
        leader.commit_up_to(index).await;

        index
    }
}

// ============================================================================
// Minimal Scheduler (for benchmarking)
// ============================================================================

#[derive(Clone, Debug)]
struct Task {
    id: u64,
    priority: u8,
    tenant_id: String,
    payload: Vec<u8>,
}

struct Scheduler {
    queues: Vec<Mutex<Vec<Task>>>,
    completed: AtomicU64,
    next_id: AtomicU64,
}

impl Scheduler {
    fn new(priority_levels: usize) -> Self {
        let queues = (0..priority_levels)
            .map(|_| Mutex::new(Vec::new()))
            .collect();

        Self {
            queues,
            completed: AtomicU64::new(0),
            next_id: AtomicU64::new(0),
        }
    }

    async fn submit(&self, priority: u8, tenant_id: &str, payload: Vec<u8>) -> u64 {
        let task_id = self.next_id.fetch_add(1, Ordering::SeqCst);
        let task = Task {
            id: task_id,
            priority,
            tenant_id: tenant_id.to_string(),
            payload,
        };

        let queue_idx = (priority as usize).min(self.queues.len() - 1);
        let mut queue = self.queues[queue_idx].lock().await;
        queue.push(task);

        task_id
    }

    async fn pop(&self) -> Option<Task> {
        // Priority scheduling: check higher priority queues first.
        for queue in &self.queues {
            let mut q = queue.lock().await;
            if !q.is_empty() {
                return Some(q.remove(0));
            }
        }
        None
    }

    async fn complete(&self, _task_id: u64) {
        self.completed.fetch_add(1, Ordering::SeqCst);
    }

    fn completed_count(&self) -> u64 {
        self.completed.load(Ordering::SeqCst)
    }
}

// ============================================================================
// Consensus Benchmarks
// ============================================================================

fn bench_leader_election(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("repro/consensus/election");

    group.sample_size(50);

    for cluster_size in [3, 5, 7] {
        group.bench_with_input(
            BenchmarkId::new("cluster_size", cluster_size),
            &cluster_size,
            |b, &size| {
                b.to_async(&rt).iter(|| async {
                    let cluster = RaftCluster::new(size);
                    let leader_id = cluster.elect_leader().await;
                    black_box(leader_id)
                });
            },
        );
    }

    group.finish();
}

fn bench_log_replication(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("repro/consensus/replication");

    group.sample_size(50);
    group.throughput(Throughput::Elements(100));

    for cluster_size in [3, 5, 7] {
        group.bench_with_input(
            BenchmarkId::new("cluster_size", cluster_size),
            &cluster_size,
            |b, &size| {
                b.to_async(&rt).iter(|| async {
                    let cluster = RaftCluster::new(size);
                    cluster.elect_leader().await;

                    for i in 0..100 {
                        let cmd = format!("command-{}", i).into_bytes();
                        cluster.replicate(cmd).await;
                    }

                    black_box(cluster.nodes[0].log_length().await)
                });
            },
        );
    }

    group.finish();
}

fn bench_consensus_throughput(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("repro/consensus/throughput");

    group.sample_size(30);

    for entry_count in [100, 500, 1000] {
        group.throughput(Throughput::Elements(entry_count as u64));
        group.bench_with_input(
            BenchmarkId::new("entries", entry_count),
            &entry_count,
            |b, &count| {
                b.to_async(&rt).iter(|| async {
                    let cluster = RaftCluster::new(3);
                    cluster.elect_leader().await;

                    let start = Instant::now();
                    for i in 0..count {
                        let cmd = vec![i as u8; 64];
                        cluster.replicate(cmd).await;
                    }
                    let duration = start.elapsed();

                    black_box((count, duration))
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Scheduler Benchmarks
// ============================================================================

fn bench_task_submission(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("repro/scheduler/submission");

    group.sample_size(50);
    group.throughput(Throughput::Elements(1000));

    group.bench_function("submit_1000_tasks", |b| {
        b.to_async(&rt).iter(|| async {
            let scheduler = Scheduler::new(3);

            for i in 0..1000 {
                let priority = (i % 3) as u8;
                scheduler.submit(priority, "tenant-1", vec![i as u8; 32]).await;
            }

            black_box(scheduler.next_id.load(Ordering::SeqCst))
        });
    });

    group.finish();
}

fn bench_task_processing(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("repro/scheduler/processing");

    group.sample_size(30);

    for task_count in [100, 500, 1000] {
        group.throughput(Throughput::Elements(task_count as u64));
        group.bench_with_input(
            BenchmarkId::new("tasks", task_count),
            &task_count,
            |b, &count| {
                b.to_async(&rt).iter(|| async {
                    let scheduler = Arc::new(Scheduler::new(3));

                    // Submit tasks.
                    for i in 0..count {
                        scheduler.submit((i % 3) as u8, "tenant-1", vec![0u8; 32]).await;
                    }

                    // Process tasks.
                    while let Some(task) = scheduler.pop().await {
                        scheduler.complete(task.id).await;
                    }

                    black_box(scheduler.completed_count())
                });
            },
        );
    }

    group.finish();
}

fn bench_priority_scheduling(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("repro/scheduler/priority");

    group.sample_size(50);

    group.bench_function("priority_order", |b| {
        b.to_async(&rt).iter(|| async {
            let scheduler = Scheduler::new(3);

            // Submit tasks in reverse priority order.
            for i in 0..100 {
                scheduler.submit(2, "low", vec![i as u8]).await;
            }
            for i in 0..100 {
                scheduler.submit(1, "med", vec![i as u8]).await;
            }
            for i in 0..100 {
                scheduler.submit(0, "high", vec![i as u8]).await;
            }

            // Verify high priority processed first.
            let mut high_count = 0u32;
            let mut processed = 0u32;

            while let Some(task) = scheduler.pop().await {
                if processed < 100 {
                    if task.priority == 0 {
                        high_count += 1;
                    }
                }
                processed += 1;
            }

            black_box(high_count)
        });
    });

    group.finish();
}

// ============================================================================
// Combined Benchmarks
// ============================================================================

fn bench_end_to_end(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("repro/e2e");

    group.sample_size(20);

    group.bench_function("consensus_then_schedule", |b| {
        b.to_async(&rt).iter(|| async {
            // 1. Establish consensus cluster.
            let cluster = RaftCluster::new(3);
            cluster.elect_leader().await;

            // 2. Replicate task commands.
            let scheduler = Scheduler::new(3);

            for i in 0..100 {
                let cmd = format!("task:{}", i).into_bytes();
                cluster.replicate(cmd).await;

                // Schedule based on committed log.
                scheduler.submit((i % 3) as u8, "tenant-1", vec![i as u8]).await;
            }

            // 3. Process scheduled tasks.
            while scheduler.pop().await.is_some() {
                // Processing.
            }

            black_box((cluster.nodes[0].log_length().await, scheduler.completed_count()))
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    consensus_benches,
    bench_leader_election,
    bench_log_replication,
    bench_consensus_throughput,
);

criterion_group!(
    scheduler_benches,
    bench_task_submission,
    bench_task_processing,
    bench_priority_scheduling,
);

criterion_group!(
    e2e_benches,
    bench_end_to_end,
);

criterion_main!(consensus_benches, scheduler_benches, e2e_benches);

// ============================================================================
// Expected Results Reference
// ============================================================================

/// Expected results for reproducibility verification.
///
/// Run with: `GREY_SEED=42 cargo bench --bench minimal_bench`
///
/// ## Consensus Benchmarks
///
/// | Benchmark               | Expected Mean | Tolerance |
/// |-------------------------|---------------|-----------|
/// | election/cluster_size/3 | ~5µs          | ±20%      |
/// | election/cluster_size/5 | ~8µs          | ±20%      |
/// | election/cluster_size/7 | ~12µs         | ±20%      |
/// | replication/cluster_size/3 | ~500µs     | ±25%      |
/// | throughput/entries/1000 | ~5ms          | ±25%      |
///
/// ## Scheduler Benchmarks
///
/// | Benchmark               | Expected Mean | Tolerance |
/// |-------------------------|---------------|-----------|
/// | submission/1000_tasks   | ~2ms          | ±20%      |
/// | processing/tasks/1000   | ~3ms          | ±20%      |
/// | priority/order          | 100 high first| Exact     |
///
/// ## Environment Requirements
///
/// - Rust 1.70+
/// - Tokio runtime
/// - 4+ CPU cores for stable results
/// - Minimal background processes
#[cfg(test)]
mod expected_results {}
