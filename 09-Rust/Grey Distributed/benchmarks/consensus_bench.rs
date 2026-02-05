//! Grey Distributed — Consensus Benchmarks
//!
//! Measures Raft consensus performance:
//! - Leader election latency
//! - Log replication throughput
//! - Membership change stability
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench consensus_bench
//! cargo bench --bench consensus_bench -- --save-baseline main
//! cargo bench --bench consensus_bench -- --baseline main
//! ```

use criterion::{
    black_box, criterion_group, criterion_main, 
    BenchmarkId, Criterion, Throughput,
};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;
use tokio::sync::{mpsc, RwLock};

// ============================================================================
// Benchmark Configuration
// ============================================================================

/// Benchmark configuration for consensus tests.
#[derive(Clone)]
struct BenchConfig {
    /// Number of nodes in cluster
    cluster_size: usize,
    /// Simulated network latency
    network_latency: Duration,
    /// Log entry size in bytes
    entry_size: usize,
    /// Number of entries per batch
    batch_size: usize,
    /// Election timeout range
    election_timeout: (Duration, Duration),
}

impl Default for BenchConfig {
    fn default() -> Self {
        Self {
            cluster_size: 5,
            network_latency: Duration::from_micros(100),
            entry_size: 256,
            batch_size: 100,
            election_timeout: (
                Duration::from_millis(150),
                Duration::from_millis(300),
            ),
        }
    }
}

// ============================================================================
// Mock Consensus Implementation (for benchmarking)
// ============================================================================

/// Simplified Raft state for benchmarking.
#[derive(Clone)]
struct RaftState {
    node_id: u64,
    term: u64,
    voted_for: Option<u64>,
    log: Vec<LogEntry>,
    commit_index: u64,
    last_applied: u64,
    role: Role,
}

#[derive(Clone, Copy, PartialEq)]
enum Role {
    Follower,
    Candidate,
    Leader,
}

#[derive(Clone)]
struct LogEntry {
    term: u64,
    index: u64,
    data: Vec<u8>,
}

/// Mock consensus node for benchmarking.
struct ConsensusNode {
    state: Arc<RwLock<RaftState>>,
    peers: Vec<u64>,
    config: BenchConfig,
}

impl ConsensusNode {
    fn new(node_id: u64, peers: Vec<u64>, config: BenchConfig) -> Self {
        Self {
            state: Arc::new(RwLock::new(RaftState {
                node_id,
                term: 0,
                voted_for: None,
                log: Vec::new(),
                commit_index: 0,
                last_applied: 0,
                role: Role::Follower,
            })),
            peers,
            config,
        }
    }

    /// Simulate leader election.
    async fn run_election(&self) -> Duration {
        let start = Instant::now();
        
        let mut state = self.state.write().await;
        state.term += 1;
        state.voted_for = Some(state.node_id);
        state.role = Role::Candidate;
        
        // Simulate RequestVote RPCs to peers
        let quorum = self.peers.len() / 2 + 1;
        let mut votes = 1; // Self-vote
        
        for _peer in &self.peers {
            // Simulate network round-trip
            tokio::time::sleep(self.config.network_latency * 2).await;
            votes += 1;
            
            if votes >= quorum {
                state.role = Role::Leader;
                break;
            }
        }
        
        start.elapsed()
    }

    /// Simulate log replication.
    async fn replicate_entries(&self, entries: Vec<LogEntry>) -> Duration {
        let start = Instant::now();
        
        let mut state = self.state.write().await;
        let base_index = state.log.len() as u64;
        
        // Append entries locally
        for (i, mut entry) in entries.into_iter().enumerate() {
            entry.index = base_index + i as u64;
            entry.term = state.term;
            state.log.push(entry);
        }
        
        // Simulate AppendEntries RPCs to peers
        let quorum = self.peers.len() / 2 + 1;
        let mut acks = 1; // Self
        
        for _peer in &self.peers {
            // Simulate network round-trip
            tokio::time::sleep(self.config.network_latency * 2).await;
            acks += 1;
            
            if acks >= quorum {
                state.commit_index = state.log.len() as u64;
                break;
            }
        }
        
        start.elapsed()
    }

    /// Simulate membership change (joint consensus).
    async fn change_membership(&self, new_peers: Vec<u64>) -> Duration {
        let start = Instant::now();
        
        // Phase 1: Replicate C_old,new configuration
        let joint_entry = LogEntry {
            term: 0,
            index: 0,
            data: format!("joint:{:?}", new_peers).into_bytes(),
        };
        self.replicate_entries(vec![joint_entry]).await;
        
        // Phase 2: Replicate C_new configuration
        let new_entry = LogEntry {
            term: 0,
            index: 0,
            data: format!("new:{:?}", new_peers).into_bytes(),
        };
        self.replicate_entries(vec![new_entry]).await;
        
        start.elapsed()
    }
}

/// Mock cluster for benchmarking.
struct MockCluster {
    nodes: Vec<ConsensusNode>,
    config: BenchConfig,
}

impl MockCluster {
    fn new(config: BenchConfig) -> Self {
        let peer_ids: Vec<u64> = (0..config.cluster_size as u64).collect();
        
        let nodes = peer_ids
            .iter()
            .map(|&id| {
                let peers: Vec<u64> = peer_ids.iter().filter(|&&p| p != id).copied().collect();
                ConsensusNode::new(id, peers, config.clone())
            })
            .collect();
        
        Self { nodes, config }
    }

    fn leader(&self) -> &ConsensusNode {
        &self.nodes[0]
    }
}

// ============================================================================
// Leader Election Benchmarks
// ============================================================================

fn bench_leader_election(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("consensus/leader_election");
    
    // Benchmark different cluster sizes
    for cluster_size in [3, 5, 7, 9] {
        let config = BenchConfig {
            cluster_size,
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("cluster_size", cluster_size),
            &config,
            |b, config| {
                let cluster = MockCluster::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let latency = cluster.nodes[0].run_election().await;
                    black_box(latency)
                });
            },
        );
    }
    
    // Benchmark different network latencies
    for latency_us in [50, 100, 500, 1000, 5000] {
        let config = BenchConfig {
            network_latency: Duration::from_micros(latency_us),
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("network_latency_us", latency_us),
            &config,
            |b, config| {
                let cluster = MockCluster::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let latency = cluster.nodes[0].run_election().await;
                    black_box(latency)
                });
            },
        );
    }
    
    group.finish();
}

/// Measure election stability under repeated elections.
fn bench_election_stability(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("consensus/election_stability");
    
    group.sample_size(50); // Fewer samples, more iterations
    
    let config = BenchConfig::default();
    let cluster = MockCluster::new(config);
    
    // Measure variance in election times
    group.bench_function("repeated_elections", |b| {
        b.to_async(&rt).iter(|| async {
            let mut latencies = Vec::with_capacity(10);
            
            for _ in 0..10 {
                let latency = cluster.nodes[0].run_election().await;
                latencies.push(latency);
            }
            
            // Return mean latency
            let sum: Duration = latencies.iter().sum();
            black_box(sum / latencies.len() as u32)
        });
    });
    
    group.finish();
}

// ============================================================================
// Log Replication Benchmarks
// ============================================================================

fn bench_log_replication(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("consensus/log_replication");
    
    // Benchmark different entry sizes
    for entry_size in [64, 256, 1024, 4096, 16384] {
        let config = BenchConfig {
            entry_size,
            batch_size: 1,
            ..Default::default()
        };
        
        group.throughput(Throughput::Bytes(entry_size as u64));
        
        group.bench_with_input(
            BenchmarkId::new("entry_size_bytes", entry_size),
            &config,
            |b, config| {
                let cluster = MockCluster::new(config.clone());
                let entry = LogEntry {
                    term: 1,
                    index: 0,
                    data: vec![0u8; config.entry_size],
                };
                
                b.to_async(&rt).iter(|| async {
                    let latency = cluster.leader().replicate_entries(vec![entry.clone()]).await;
                    black_box(latency)
                });
            },
        );
    }
    
    group.finish();
}

fn bench_batch_replication(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("consensus/batch_replication");
    
    // Benchmark different batch sizes
    for batch_size in [1, 10, 50, 100, 500, 1000] {
        let config = BenchConfig {
            entry_size: 256,
            batch_size,
            ..Default::default()
        };
        
        group.throughput(Throughput::Elements(batch_size as u64));
        
        group.bench_with_input(
            BenchmarkId::new("batch_size", batch_size),
            &config,
            |b, config| {
                let cluster = MockCluster::new(config.clone());
                let entries: Vec<LogEntry> = (0..config.batch_size)
                    .map(|_| LogEntry {
                        term: 1,
                        index: 0,
                        data: vec![0u8; config.entry_size],
                    })
                    .collect();
                
                b.to_async(&rt).iter(|| async {
                    let latency = cluster.leader().replicate_entries(entries.clone()).await;
                    black_box(latency)
                });
            },
        );
    }
    
    group.finish();
}

/// Throughput under sustained load.
fn bench_replication_throughput(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("consensus/replication_throughput");
    
    group.sample_size(20);
    group.measurement_time(Duration::from_secs(10));
    
    let config = BenchConfig {
        entry_size: 256,
        batch_size: 100,
        ..Default::default()
    };
    
    let cluster = MockCluster::new(config.clone());
    
    group.throughput(Throughput::Elements(1000)); // 10 batches of 100
    
    group.bench_function("sustained_1000_entries", |b| {
        b.to_async(&rt).iter(|| async {
            let entries: Vec<LogEntry> = (0..100)
                .map(|_| LogEntry {
                    term: 1,
                    index: 0,
                    data: vec![0u8; 256],
                })
                .collect();
            
            for _ in 0..10 {
                cluster.leader().replicate_entries(entries.clone()).await;
            }
        });
    });
    
    group.finish();
}

// ============================================================================
// Membership Change Benchmarks
// ============================================================================

fn bench_membership_change(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("consensus/membership_change");
    
    group.sample_size(30);
    
    // Add node
    group.bench_function("add_node", |b| {
        let config = BenchConfig {
            cluster_size: 5,
            ..Default::default()
        };
        let cluster = MockCluster::new(config);
        let new_peers = vec![0, 1, 2, 3, 4, 5]; // Adding node 5
        
        b.to_async(&rt).iter(|| async {
            let latency = cluster.leader().change_membership(new_peers.clone()).await;
            black_box(latency)
        });
    });
    
    // Remove node
    group.bench_function("remove_node", |b| {
        let config = BenchConfig {
            cluster_size: 5,
            ..Default::default()
        };
        let cluster = MockCluster::new(config);
        let new_peers = vec![0, 1, 2, 3]; // Removing node 4
        
        b.to_async(&rt).iter(|| async {
            let latency = cluster.leader().change_membership(new_peers.clone()).await;
            black_box(latency)
        });
    });
    
    // Replace node (remove + add)
    group.bench_function("replace_node", |b| {
        let config = BenchConfig {
            cluster_size: 5,
            ..Default::default()
        };
        let cluster = MockCluster::new(config);
        let new_peers = vec![0, 1, 2, 3, 5]; // Replace node 4 with 5
        
        b.to_async(&rt).iter(|| async {
            let latency = cluster.leader().change_membership(new_peers.clone()).await;
            black_box(latency)
        });
    });
    
    group.finish();
}

// ============================================================================
// Stress Tests
// ============================================================================

fn bench_concurrent_proposals(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("consensus/concurrent_proposals");
    
    group.sample_size(20);
    
    for concurrent in [1, 4, 8, 16, 32] {
        let config = BenchConfig::default();
        
        group.throughput(Throughput::Elements(concurrent as u64 * 10));
        
        group.bench_with_input(
            BenchmarkId::new("concurrent_clients", concurrent),
            &concurrent,
            |b, &concurrent| {
                let cluster = MockCluster::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let mut handles = Vec::new();
                    
                    for _ in 0..concurrent {
                        let leader = cluster.leader();
                        let entries: Vec<LogEntry> = (0..10)
                            .map(|_| LogEntry {
                                term: 1,
                                index: 0,
                                data: vec![0u8; 256],
                            })
                            .collect();
                        
                        // Note: In real code, this would be concurrent
                        // For benchmarking, we simulate sequential execution
                        let latency = leader.replicate_entries(entries).await;
                        handles.push(latency);
                    }
                    
                    black_box(handles)
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Metrics Collection
// ============================================================================

/// Collect and report detailed metrics.
pub struct ConsensusMetrics {
    pub election_latencies: Vec<Duration>,
    pub replication_latencies: Vec<Duration>,
    pub membership_change_latencies: Vec<Duration>,
    pub throughput_samples: Vec<f64>, // entries/sec
}

impl ConsensusMetrics {
    pub fn new() -> Self {
        Self {
            election_latencies: Vec::new(),
            replication_latencies: Vec::new(),
            membership_change_latencies: Vec::new(),
            throughput_samples: Vec::new(),
        }
    }
    
    pub fn report(&self) {
        println!("\n=== Consensus Benchmark Results ===\n");
        
        if !self.election_latencies.is_empty() {
            let (p50, p99, max) = percentiles(&self.election_latencies);
            println!("Leader Election:");
            println!("  P50: {:?}", p50);
            println!("  P99: {:?}", p99);
            println!("  Max: {:?}", max);
        }
        
        if !self.replication_latencies.is_empty() {
            let (p50, p99, max) = percentiles(&self.replication_latencies);
            println!("\nLog Replication:");
            println!("  P50: {:?}", p50);
            println!("  P99: {:?}", p99);
            println!("  Max: {:?}", max);
        }
        
        if !self.throughput_samples.is_empty() {
            let avg: f64 = self.throughput_samples.iter().sum::<f64>() 
                / self.throughput_samples.len() as f64;
            println!("\nThroughput:");
            println!("  Avg: {:.2} entries/sec", avg);
        }
    }
}

fn percentiles(latencies: &[Duration]) -> (Duration, Duration, Duration) {
    let mut sorted = latencies.to_vec();
    sorted.sort();
    
    let p50 = sorted[sorted.len() / 2];
    let p99 = sorted[(sorted.len() as f64 * 0.99) as usize];
    let max = *sorted.last().unwrap();
    
    (p50, p99, max)
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    benches,
    bench_leader_election,
    bench_election_stability,
    bench_log_replication,
    bench_batch_replication,
    bench_replication_throughput,
    bench_membership_change,
    bench_concurrent_proposals,
);

criterion_main!(benches);

// ============================================================================
// Expected Results Reference
// ============================================================================

/// Expected benchmark results for reference.
/// 
/// | Metric                      | P50    | P99    | Notes                     |
/// |-----------------------------|--------|--------|---------------------------|
/// | Leader Election (5 nodes)   | 0.5ms  | 2ms    | Network: 100µs RTT        |
/// | Election (cross-region)     | 50ms   | 200ms  | Network: 25ms RTT         |
/// | Log Replication (256B)      | 0.3ms  | 1ms    | Single entry              |
/// | Batch Replication (100x256B)| 0.5ms  | 2ms    | Amortized overhead        |
/// | Throughput (sustained)      | 50K/s  | -      | 256B entries              |
/// | Membership Change           | 1ms    | 5ms    | Joint consensus           |
///
/// ## Tradeoffs
///
/// - **Cluster Size**: Larger clusters = higher election latency, better fault tolerance
/// - **Batch Size**: Larger batches = better throughput, higher per-batch latency
/// - **Entry Size**: Larger entries = lower throughput, but better for large payloads
/// - **Network Latency**: Dominates consensus performance; co-locate nodes when possible
#[cfg(test)]
mod expected_results {}
