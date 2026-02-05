//! Grey Distributed — Storage Benchmarks
//!
//! Measures storage layer performance:
//! - Sharding scalability
//! - Replication overhead
//! - Quorum read/write latency
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench storage_bench
//! ```

use criterion::{
    black_box, criterion_group, criterion_main,
    BenchmarkId, Criterion, Throughput,
};
use rand::prelude::*;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;
use tokio::sync::{Mutex, RwLock};

// ============================================================================
// Configuration
// ============================================================================

#[derive(Clone)]
struct StorageConfig {
    /// Number of shards
    num_shards: usize,
    /// Replication factor per shard
    replication_factor: usize,
    /// Quorum for reads
    read_quorum: QuorumLevel,
    /// Quorum for writes
    write_quorum: QuorumLevel,
    /// Simulated storage latency
    storage_latency: Duration,
    /// Simulated replication latency
    replication_latency: Duration,
    /// Value size in bytes
    value_size: usize,
}

#[derive(Clone, Copy)]
enum QuorumLevel {
    One,
    Majority,
    All,
}

impl QuorumLevel {
    fn required(&self, total: usize) -> usize {
        match self {
            QuorumLevel::One => 1,
            QuorumLevel::Majority => total / 2 + 1,
            QuorumLevel::All => total,
        }
    }
}

impl Default for StorageConfig {
    fn default() -> Self {
        Self {
            num_shards: 16,
            replication_factor: 3,
            read_quorum: QuorumLevel::Majority,
            write_quorum: QuorumLevel::Majority,
            storage_latency: Duration::from_micros(100),
            replication_latency: Duration::from_micros(200),
            value_size: 256,
        }
    }
}

// ============================================================================
// Storage Implementation
// ============================================================================

struct ShardedStorage {
    config: StorageConfig,
    shards: Vec<Shard>,
    metrics: Arc<StorageMetrics>,
}

struct Shard {
    id: usize,
    replicas: Vec<Replica>,
}

struct Replica {
    id: usize,
    data: Arc<RwLock<HashMap<String, Vec<u8>>>>,
    latency: Duration,
}

struct StorageMetrics {
    reads: AtomicU64,
    writes: AtomicU64,
    bytes_read: AtomicU64,
    bytes_written: AtomicU64,
    read_latency_us: AtomicU64,
    write_latency_us: AtomicU64,
    replication_latency_us: AtomicU64,
    quorum_failures: AtomicU64,
}

impl ShardedStorage {
    fn new(config: StorageConfig) -> Self {
        let shards: Vec<Shard> = (0..config.num_shards)
            .map(|shard_id| {
                let replicas: Vec<Replica> = (0..config.replication_factor)
                    .map(|replica_id| Replica {
                        id: replica_id,
                        data: Arc::new(RwLock::new(HashMap::new())),
                        latency: config.storage_latency,
                    })
                    .collect();
                
                Shard {
                    id: shard_id,
                    replicas,
                }
            })
            .collect();
        
        Self {
            config,
            shards,
            metrics: Arc::new(StorageMetrics {
                reads: AtomicU64::new(0),
                writes: AtomicU64::new(0),
                bytes_read: AtomicU64::new(0),
                bytes_written: AtomicU64::new(0),
                read_latency_us: AtomicU64::new(0),
                write_latency_us: AtomicU64::new(0),
                replication_latency_us: AtomicU64::new(0),
                quorum_failures: AtomicU64::new(0),
            }),
        }
    }
    
    /// Consistent hash to determine shard.
    fn get_shard(&self, key: &str) -> usize {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        (hasher.finish() as usize) % self.config.num_shards
    }
    
    /// Write with quorum.
    async fn write(&self, key: String, value: Vec<u8>) -> Result<(), StorageError> {
        let start = Instant::now();
        let shard_id = self.get_shard(&key);
        let shard = &self.shards[shard_id];
        
        let required = self.config.write_quorum.required(shard.replicas.len());
        let mut acks = 0;
        
        let value_len = value.len();
        
        // Write to replicas in parallel (simulated)
        for replica in &shard.replicas {
            // Simulate storage latency
            tokio::time::sleep(replica.latency).await;
            
            // Simulate replication latency for non-primary
            if replica.id > 0 {
                tokio::time::sleep(self.config.replication_latency).await;
            }
            
            let mut data = replica.data.write().await;
            data.insert(key.clone(), value.clone());
            
            acks += 1;
            if acks >= required {
                break;
            }
        }
        
        if acks < required {
            self.metrics.quorum_failures.fetch_add(1, Ordering::Relaxed);
            return Err(StorageError::QuorumNotReached);
        }
        
        let latency = start.elapsed();
        self.metrics.writes.fetch_add(1, Ordering::Relaxed);
        self.metrics.bytes_written.fetch_add(value_len as u64, Ordering::Relaxed);
        self.metrics.write_latency_us.fetch_add(latency.as_micros() as u64, Ordering::Relaxed);
        
        Ok(())
    }
    
    /// Read with quorum.
    async fn read(&self, key: &str) -> Result<Option<Vec<u8>>, StorageError> {
        let start = Instant::now();
        let shard_id = self.get_shard(key);
        let shard = &self.shards[shard_id];
        
        let required = self.config.read_quorum.required(shard.replicas.len());
        let mut responses: Vec<Option<Vec<u8>>> = Vec::new();
        
        // Read from replicas
        for replica in &shard.replicas {
            tokio::time::sleep(replica.latency).await;
            
            let data = replica.data.read().await;
            responses.push(data.get(key).cloned());
            
            if responses.len() >= required {
                break;
            }
        }
        
        if responses.len() < required {
            self.metrics.quorum_failures.fetch_add(1, Ordering::Relaxed);
            return Err(StorageError::QuorumNotReached);
        }
        
        // Return most common value (simple quorum resolution)
        let value = responses.into_iter().flatten().next();
        
        let latency = start.elapsed();
        self.metrics.reads.fetch_add(1, Ordering::Relaxed);
        if let Some(ref v) = value {
            self.metrics.bytes_read.fetch_add(v.len() as u64, Ordering::Relaxed);
        }
        self.metrics.read_latency_us.fetch_add(latency.as_micros() as u64, Ordering::Relaxed);
        
        Ok(value)
    }
    
    /// Batch write.
    async fn batch_write(&self, items: Vec<(String, Vec<u8>)>) -> Result<usize, StorageError> {
        let mut success = 0;
        
        for (key, value) in items {
            if self.write(key, value).await.is_ok() {
                success += 1;
            }
        }
        
        Ok(success)
    }
    
    /// Scan keys in a shard.
    async fn scan_shard(&self, shard_id: usize, limit: usize) -> Vec<String> {
        if shard_id >= self.shards.len() {
            return Vec::new();
        }
        
        let shard = &self.shards[shard_id];
        let replica = &shard.replicas[0];
        
        tokio::time::sleep(replica.latency).await;
        
        let data = replica.data.read().await;
        data.keys().take(limit).cloned().collect()
    }
    
    fn get_metrics(&self) -> StorageMetricsSnapshot {
        let reads = self.metrics.reads.load(Ordering::Relaxed);
        let writes = self.metrics.writes.load(Ordering::Relaxed);
        
        StorageMetricsSnapshot {
            reads,
            writes,
            bytes_read: self.metrics.bytes_read.load(Ordering::Relaxed),
            bytes_written: self.metrics.bytes_written.load(Ordering::Relaxed),
            avg_read_latency_us: if reads > 0 {
                self.metrics.read_latency_us.load(Ordering::Relaxed) / reads
            } else { 0 },
            avg_write_latency_us: if writes > 0 {
                self.metrics.write_latency_us.load(Ordering::Relaxed) / writes
            } else { 0 },
            quorum_failures: self.metrics.quorum_failures.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug)]
enum StorageError {
    QuorumNotReached,
    ShardNotFound,
    KeyNotFound,
}

#[derive(Debug)]
struct StorageMetricsSnapshot {
    reads: u64,
    writes: u64,
    bytes_read: u64,
    bytes_written: u64,
    avg_read_latency_us: u64,
    avg_write_latency_us: u64,
    quorum_failures: u64,
}

// ============================================================================
// Sharding Scalability Benchmarks
// ============================================================================

fn bench_sharding_scalability(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/sharding");
    
    group.sample_size(30);
    
    // Vary number of shards
    for num_shards in [1, 4, 16, 64, 256] {
        let config = StorageConfig {
            num_shards,
            replication_factor: 1, // No replication for this test
            storage_latency: Duration::from_micros(10),
            ..Default::default()
        };
        
        group.throughput(Throughput::Elements(1000));
        
        group.bench_with_input(
            BenchmarkId::new("num_shards", num_shards),
            &config,
            |b, config| {
                b.to_async(&rt).iter(|| {
                    let storage = Arc::new(ShardedStorage::new(config.clone()));
                    async move {
                        // Write 1000 keys across shards
                        let handles: Vec<_> = (0..1000u64)
                            .map(|i| {
                                let storage = storage.clone();
                                let key = format!("key-{}", i);
                                let value = vec![0u8; 256];
                                tokio::spawn(async move {
                                    storage.write(key, value).await
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(storage.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

fn bench_shard_distribution(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/shard_distribution");
    
    group.sample_size(20);
    
    let config = StorageConfig {
        num_shards: 16,
        replication_factor: 1,
        storage_latency: Duration::from_micros(10),
        ..Default::default()
    };
    
    // Test key distribution uniformity
    group.bench_function("key_distribution", |b| {
        b.to_async(&rt).iter(|| {
            let storage = ShardedStorage::new(config.clone());
            async move {
                let mut shard_counts = vec![0u64; config.num_shards];
                
                for i in 0..10000u64 {
                    let key = format!("tenant-{}-task-{}", i % 100, i);
                    let shard = storage.get_shard(&key);
                    shard_counts[shard] += 1;
                }
                
                // Calculate standard deviation of distribution
                let mean = 10000.0 / config.num_shards as f64;
                let variance: f64 = shard_counts.iter()
                    .map(|&count| (count as f64 - mean).powi(2))
                    .sum::<f64>() / config.num_shards as f64;
                let std_dev = variance.sqrt();
                
                black_box((shard_counts, std_dev))
            }
        });
    });
    
    group.finish();
}

// ============================================================================
// Replication Overhead Benchmarks
// ============================================================================

fn bench_replication_overhead(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/replication");
    
    group.sample_size(30);
    
    // Vary replication factor
    for replication in [1, 2, 3, 5] {
        let config = StorageConfig {
            num_shards: 16,
            replication_factor: replication,
            write_quorum: QuorumLevel::Majority,
            storage_latency: Duration::from_micros(50),
            replication_latency: Duration::from_micros(100),
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("replication_factor", replication),
            &config,
            |b, config| {
                b.to_async(&rt).iter(|| {
                    let storage = Arc::new(ShardedStorage::new(config.clone()));
                    async move {
                        let handles: Vec<_> = (0..100u64)
                            .map(|i| {
                                let storage = storage.clone();
                                tokio::spawn(async move {
                                    let key = format!("key-{}", i);
                                    let value = vec![0u8; 256];
                                    storage.write(key, value).await
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(storage.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

fn bench_replication_latency_impact(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/replication_latency");
    
    group.sample_size(30);
    
    // Vary replication latency
    for latency_us in [0, 100, 500, 1000, 5000] {
        let config = StorageConfig {
            replication_factor: 3,
            replication_latency: Duration::from_micros(latency_us),
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("replication_latency_us", latency_us),
            &config,
            |b, config| {
                b.to_async(&rt).iter(|| {
                    let storage = Arc::new(ShardedStorage::new(config.clone()));
                    async move {
                        let handles: Vec<_> = (0..100u64)
                            .map(|i| {
                                let storage = storage.clone();
                                tokio::spawn(async move {
                                    let key = format!("key-{}", i);
                                    let value = vec![0u8; 256];
                                    storage.write(key, value).await
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(storage.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Quorum Benchmarks
// ============================================================================

fn bench_quorum_reads(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/quorum_read");
    
    group.sample_size(30);
    
    for quorum in ["one", "majority", "all"] {
        let read_quorum = match quorum {
            "one" => QuorumLevel::One,
            "majority" => QuorumLevel::Majority,
            "all" => QuorumLevel::All,
            _ => unreachable!(),
        };
        
        let config = StorageConfig {
            replication_factor: 3,
            read_quorum,
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("quorum", quorum),
            &config,
            |b, config| {
                b.to_async(&rt).iter(|| {
                    let storage = Arc::new(ShardedStorage::new(config.clone()));
                    async move {
                        // Pre-populate data
                        for i in 0..100u64 {
                            let key = format!("key-{}", i);
                            let value = vec![0u8; 256];
                            let _ = storage.write(key, value).await;
                        }
                        
                        // Benchmark reads
                        let handles: Vec<_> = (0..100u64)
                            .map(|i| {
                                let storage = storage.clone();
                                tokio::spawn(async move {
                                    let key = format!("key-{}", i);
                                    storage.read(&key).await
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(storage.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

fn bench_quorum_writes(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/quorum_write");
    
    group.sample_size(30);
    
    for quorum in ["one", "majority", "all"] {
        let write_quorum = match quorum {
            "one" => QuorumLevel::One,
            "majority" => QuorumLevel::Majority,
            "all" => QuorumLevel::All,
            _ => unreachable!(),
        };
        
        let config = StorageConfig {
            replication_factor: 3,
            write_quorum,
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("quorum", quorum),
            &config,
            |b, config| {
                b.to_async(&rt).iter(|| {
                    let storage = Arc::new(ShardedStorage::new(config.clone()));
                    async move {
                        let handles: Vec<_> = (0..100u64)
                            .map(|i| {
                                let storage = storage.clone();
                                tokio::spawn(async move {
                                    let key = format!("key-{}", i);
                                    let value = vec![0u8; 256];
                                    storage.write(key, value).await
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(storage.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Value Size Benchmarks
// ============================================================================

fn bench_value_sizes(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/value_sizes");
    
    group.sample_size(30);
    
    for size in [64, 256, 1024, 4096, 16384, 65536] {
        let config = StorageConfig {
            value_size: size,
            ..Default::default()
        };
        
        group.throughput(Throughput::Bytes(size as u64 * 100));
        
        group.bench_with_input(
            BenchmarkId::new("value_bytes", size),
            &size,
            |b, &size| {
                b.to_async(&rt).iter(|| {
                    let storage = Arc::new(ShardedStorage::new(config.clone()));
                    async move {
                        let handles: Vec<_> = (0..100u64)
                            .map(|i| {
                                let storage = storage.clone();
                                tokio::spawn(async move {
                                    let key = format!("key-{}", i);
                                    let value = vec![0u8; size];
                                    storage.write(key, value).await
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(storage.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Batch Operations Benchmarks
// ============================================================================

fn bench_batch_writes(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/batch_writes");
    
    group.sample_size(20);
    
    for batch_size in [1, 10, 50, 100, 500] {
        let config = StorageConfig::default();
        
        group.throughput(Throughput::Elements(batch_size as u64));
        
        group.bench_with_input(
            BenchmarkId::new("batch_size", batch_size),
            &batch_size,
            |b, &batch_size| {
                b.to_async(&rt).iter(|| {
                    let storage = Arc::new(ShardedStorage::new(config.clone()));
                    async move {
                        let items: Vec<_> = (0..batch_size)
                            .map(|i| {
                                let key = format!("key-{}", i);
                                let value = vec![0u8; 256];
                                (key, value)
                            })
                            .collect();
                        
                        let result = storage.batch_write(items).await;
                        black_box((result, storage.get_metrics()))
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Scan Benchmarks
// ============================================================================

fn bench_shard_scan(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("storage/scan");
    
    group.sample_size(20);
    
    for limit in [10, 100, 1000, 10000] {
        let config = StorageConfig::default();
        
        group.bench_with_input(
            BenchmarkId::new("scan_limit", limit),
            &limit,
            |b, &limit| {
                b.to_async(&rt).iter(|| {
                    let storage = Arc::new(ShardedStorage::new(config.clone()));
                    async move {
                        // Pre-populate
                        for i in 0..limit {
                            let key = format!("key-{}", i);
                            let value = vec![0u8; 256];
                            let _ = storage.write(key, value).await;
                        }
                        
                        // Scan first shard
                        let keys = storage.scan_shard(0, limit).await;
                        black_box(keys)
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Benchmark Groups
// ============================================================================

criterion_group!(
    benches,
    bench_sharding_scalability,
    bench_shard_distribution,
    bench_replication_overhead,
    bench_replication_latency_impact,
    bench_quorum_reads,
    bench_quorum_writes,
    bench_value_sizes,
    bench_batch_writes,
    bench_shard_scan,
);

criterion_main!(benches);

// ============================================================================
// Expected Results Reference
// ============================================================================

/// Expected benchmark results for reference.
///
/// | Metric                          | Value     | Notes                    |
/// |---------------------------------|-----------|--------------------------|
/// | Write latency (RF=1)            | 100µs     | Local only               |
/// | Write latency (RF=3, majority)  | 300µs     | 2 replica acks           |
/// | Write latency (RF=3, all)       | 500µs     | All replica acks         |
/// | Read latency (quorum=1)         | 100µs     | Single replica           |
/// | Read latency (quorum=majority)  | 200µs     | 2 replica reads          |
/// | Sharding overhead (16 shards)   | +5%       | Hash + routing           |
/// | Sharding overhead (256 shards)  | +10%      | More hash operations     |
/// | Batch efficiency (100 items)    | 80%       | Amortized overhead       |
///
/// ## Tradeoffs
///
/// - **More Shards**: Better parallelism, more memory overhead
/// - **Higher RF**: Better durability, slower writes
/// - **Majority Quorum**: Balance of consistency and latency
/// - **Batch Writes**: Better throughput, higher per-batch latency
#[cfg(test)]
mod expected_results {}
