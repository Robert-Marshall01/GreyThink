//! Grey Distributed — Network Benchmarks
//!
//! Measures network performance:
//! - Latency vs. throughput under packet loss
//! - Tail latency mitigation (hedged requests, retries)
//! - Backpressure propagation
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench network_bench
//! ```

use criterion::{
    black_box, criterion_group, criterion_main,
    BenchmarkId, Criterion, Throughput,
};
use rand::prelude::*;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;
use tokio::sync::{mpsc, Semaphore};

// ============================================================================
// Configuration
// ============================================================================

#[derive(Clone)]
struct NetworkConfig {
    /// Base network latency (one-way)
    base_latency: Duration,
    /// Latency jitter (± this value)
    latency_jitter: Duration,
    /// Packet loss rate (0.0 - 1.0)
    packet_loss_rate: f64,
    /// Bandwidth limit (bytes/sec, 0 = unlimited)
    bandwidth_limit: usize,
    /// Enable hedged requests
    hedged_requests: bool,
    /// Hedged request delay (send second request after this)
    hedge_delay: Duration,
    /// Maximum retries
    max_retries: usize,
    /// Retry backoff base
    retry_backoff: Duration,
    /// Backpressure threshold (pending requests)
    backpressure_threshold: usize,
}

impl Default for NetworkConfig {
    fn default() -> Self {
        Self {
            base_latency: Duration::from_micros(500),
            latency_jitter: Duration::from_micros(100),
            packet_loss_rate: 0.0,
            bandwidth_limit: 0,
            hedged_requests: false,
            hedge_delay: Duration::from_millis(10),
            max_retries: 3,
            retry_backoff: Duration::from_millis(10),
            backpressure_threshold: 1000,
        }
    }
}

// ============================================================================
// Network Simulator
// ============================================================================

struct NetworkSimulator {
    config: NetworkConfig,
    metrics: Arc<NetworkMetrics>,
    pending_requests: Arc<AtomicU64>,
    backpressure: Arc<Semaphore>,
}

struct NetworkMetrics {
    requests_sent: AtomicU64,
    requests_succeeded: AtomicU64,
    requests_failed: AtomicU64,
    requests_retried: AtomicU64,
    requests_hedged: AtomicU64,
    bytes_sent: AtomicU64,
    bytes_received: AtomicU64,
    total_latency_us: AtomicU64,
    backpressure_events: AtomicU64,
}

#[derive(Clone)]
struct Request {
    id: u64,
    payload_size: usize,
    sent_at: Instant,
}

#[derive(Clone)]
struct Response {
    request_id: u64,
    payload_size: usize,
    latency: Duration,
}

impl NetworkSimulator {
    fn new(config: NetworkConfig) -> Self {
        Self {
            backpressure: Arc::new(Semaphore::new(config.backpressure_threshold)),
            config,
            metrics: Arc::new(NetworkMetrics {
                requests_sent: AtomicU64::new(0),
                requests_succeeded: AtomicU64::new(0),
                requests_failed: AtomicU64::new(0),
                requests_retried: AtomicU64::new(0),
                requests_hedged: AtomicU64::new(0),
                bytes_sent: AtomicU64::new(0),
                bytes_received: AtomicU64::new(0),
                total_latency_us: AtomicU64::new(0),
                backpressure_events: AtomicU64::new(0),
            }),
            pending_requests: Arc::new(AtomicU64::new(0)),
        }
    }
    
    /// Simulate network latency with jitter.
    fn simulate_latency(&self) -> Duration {
        let mut rng = rand::thread_rng();
        let jitter: i64 = rng.gen_range(
            -(self.config.latency_jitter.as_micros() as i64)
                ..=(self.config.latency_jitter.as_micros() as i64)
        );
        
        let base = self.config.base_latency.as_micros() as i64;
        let total = (base + jitter).max(0) as u64;
        
        Duration::from_micros(total)
    }
    
    /// Check if packet is lost.
    fn is_packet_lost(&self) -> bool {
        if self.config.packet_loss_rate <= 0.0 {
            return false;
        }
        
        let mut rng = rand::thread_rng();
        rng.gen::<f64>() < self.config.packet_loss_rate
    }
    
    /// Send request with optional retries and hedging.
    async fn send_request(&self, request: Request) -> Result<Response, NetworkError> {
        // Apply backpressure
        let permit = self.backpressure.try_acquire();
        if permit.is_err() {
            self.metrics.backpressure_events.fetch_add(1, Ordering::Relaxed);
            // Wait for permit
            let _permit = self.backpressure.acquire().await.unwrap();
        }
        
        self.pending_requests.fetch_add(1, Ordering::Relaxed);
        self.metrics.requests_sent.fetch_add(1, Ordering::Relaxed);
        self.metrics.bytes_sent.fetch_add(request.payload_size as u64, Ordering::Relaxed);
        
        let result = if self.config.hedged_requests {
            self.send_with_hedging(request.clone()).await
        } else {
            self.send_with_retries(request.clone()).await
        };
        
        self.pending_requests.fetch_sub(1, Ordering::Relaxed);
        
        match &result {
            Ok(response) => {
                self.metrics.requests_succeeded.fetch_add(1, Ordering::Relaxed);
                self.metrics.bytes_received.fetch_add(response.payload_size as u64, Ordering::Relaxed);
                self.metrics.total_latency_us.fetch_add(
                    response.latency.as_micros() as u64,
                    Ordering::Relaxed,
                );
            }
            Err(_) => {
                self.metrics.requests_failed.fetch_add(1, Ordering::Relaxed);
            }
        }
        
        result
    }
    
    /// Send with retry logic.
    async fn send_with_retries(&self, request: Request) -> Result<Response, NetworkError> {
        let mut attempts = 0;
        
        loop {
            let latency = self.simulate_latency();
            tokio::time::sleep(latency).await;
            
            if self.is_packet_lost() {
                attempts += 1;
                if attempts > self.config.max_retries {
                    return Err(NetworkError::MaxRetriesExceeded);
                }
                self.metrics.requests_retried.fetch_add(1, Ordering::Relaxed);
                
                // Exponential backoff
                let backoff = self.config.retry_backoff * (1 << attempts.min(5));
                tokio::time::sleep(backoff).await;
                continue;
            }
            
            return Ok(Response {
                request_id: request.id,
                payload_size: request.payload_size,
                latency: request.sent_at.elapsed(),
            });
        }
    }
    
    /// Send with hedged requests (send backup request after delay).
    async fn send_with_hedging(&self, request: Request) -> Result<Response, NetworkError> {
        use tokio::select;
        
        let primary = self.send_single(request.clone());
        
        // Start hedge after delay
        let hedge_delay = self.config.hedge_delay;
        let hedged = async {
            tokio::time::sleep(hedge_delay).await;
            self.metrics.requests_hedged.fetch_add(1, Ordering::Relaxed);
            self.send_single(request.clone()).await
        };
        
        // Return first successful response
        select! {
            result = primary => result,
            result = hedged => result,
        }
    }
    
    /// Send single request without retries.
    async fn send_single(&self, request: Request) -> Result<Response, NetworkError> {
        let latency = self.simulate_latency();
        tokio::time::sleep(latency).await;
        
        if self.is_packet_lost() {
            return Err(NetworkError::PacketLost);
        }
        
        Ok(Response {
            request_id: request.id,
            payload_size: request.payload_size,
            latency: request.sent_at.elapsed(),
        })
    }
    
    fn get_metrics(&self) -> NetworkMetricsSnapshot {
        let succeeded = self.metrics.requests_succeeded.load(Ordering::Relaxed);
        let total_latency = self.metrics.total_latency_us.load(Ordering::Relaxed);
        
        NetworkMetricsSnapshot {
            requests_sent: self.metrics.requests_sent.load(Ordering::Relaxed),
            requests_succeeded: succeeded,
            requests_failed: self.metrics.requests_failed.load(Ordering::Relaxed),
            requests_retried: self.metrics.requests_retried.load(Ordering::Relaxed),
            requests_hedged: self.metrics.requests_hedged.load(Ordering::Relaxed),
            bytes_sent: self.metrics.bytes_sent.load(Ordering::Relaxed),
            bytes_received: self.metrics.bytes_received.load(Ordering::Relaxed),
            avg_latency_us: if succeeded > 0 { total_latency / succeeded } else { 0 },
            backpressure_events: self.metrics.backpressure_events.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug)]
enum NetworkError {
    PacketLost,
    MaxRetriesExceeded,
    Timeout,
    Backpressure,
}

#[derive(Debug)]
struct NetworkMetricsSnapshot {
    requests_sent: u64,
    requests_succeeded: u64,
    requests_failed: u64,
    requests_retried: u64,
    requests_hedged: u64,
    bytes_sent: u64,
    bytes_received: u64,
    avg_latency_us: u64,
    backpressure_events: u64,
}

// ============================================================================
// Latency vs Throughput Benchmarks
// ============================================================================

fn bench_latency_throughput(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("network/latency_throughput");
    
    group.sample_size(30);
    
    // Vary base latency
    for latency_us in [100, 500, 1000, 5000, 10000] {
        let config = NetworkConfig {
            base_latency: Duration::from_micros(latency_us),
            ..Default::default()
        };
        
        group.throughput(Throughput::Elements(100));
        
        group.bench_with_input(
            BenchmarkId::new("base_latency_us", latency_us),
            &config,
            |b, config| {
                let sim = NetworkSimulator::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let handles: Vec<_> = (0..100u64)
                        .map(|i| {
                            let request = Request {
                                id: i,
                                payload_size: 256,
                                sent_at: Instant::now(),
                            };
                            sim.send_request(request)
                        })
                        .collect();
                    
                    let results: Vec<_> = futures::future::join_all(handles).await;
                    black_box(results)
                });
            },
        );
    }
    
    group.finish();
}

fn bench_packet_loss(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("network/packet_loss");
    
    group.sample_size(30);
    
    for loss_pct in [0, 1, 5, 10, 20] {
        let config = NetworkConfig {
            packet_loss_rate: loss_pct as f64 / 100.0,
            max_retries: 5,
            retry_backoff: Duration::from_micros(100),
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("loss_percent", loss_pct),
            &config,
            |b, config| {
                let sim = NetworkSimulator::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let handles: Vec<_> = (0..100u64)
                        .map(|i| {
                            let request = Request {
                                id: i,
                                payload_size: 256,
                                sent_at: Instant::now(),
                            };
                            sim.send_request(request)
                        })
                        .collect();
                    
                    let results: Vec<_> = futures::future::join_all(handles).await;
                    let success_rate = results.iter().filter(|r| r.is_ok()).count() as f64
                        / results.len() as f64;
                    black_box((results, success_rate))
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Tail Latency Mitigation Benchmarks
// ============================================================================

fn bench_hedged_requests(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("network/hedged_requests");
    
    group.sample_size(30);
    
    // Compare with and without hedging under packet loss
    for hedging in [false, true] {
        let config = NetworkConfig {
            packet_loss_rate: 0.1, // 10% loss
            hedged_requests: hedging,
            hedge_delay: Duration::from_millis(5),
            max_retries: 3,
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("hedging_enabled", hedging),
            &config,
            |b, config| {
                let sim = NetworkSimulator::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let handles: Vec<_> = (0..100u64)
                        .map(|i| {
                            let request = Request {
                                id: i,
                                payload_size: 256,
                                sent_at: Instant::now(),
                            };
                            sim.send_request(request)
                        })
                        .collect();
                    
                    let results: Vec<_> = futures::future::join_all(handles).await;
                    black_box(sim.get_metrics())
                });
            },
        );
    }
    
    // Vary hedge delay
    for delay_ms in [1, 5, 10, 20, 50] {
        let config = NetworkConfig {
            packet_loss_rate: 0.05,
            hedged_requests: true,
            hedge_delay: Duration::from_millis(delay_ms),
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("hedge_delay_ms", delay_ms),
            &config,
            |b, config| {
                let sim = NetworkSimulator::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let handles: Vec<_> = (0..100u64)
                        .map(|i| {
                            let request = Request {
                                id: i,
                                payload_size: 256,
                                sent_at: Instant::now(),
                            };
                            sim.send_request(request)
                        })
                        .collect();
                    
                    futures::future::join_all(handles).await;
                    black_box(sim.get_metrics())
                });
            },
        );
    }
    
    group.finish();
}

fn bench_retry_strategies(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("network/retry_strategies");
    
    group.sample_size(30);
    
    for max_retries in [0, 1, 3, 5, 10] {
        let config = NetworkConfig {
            packet_loss_rate: 0.1,
            max_retries,
            retry_backoff: Duration::from_micros(100),
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("max_retries", max_retries),
            &config,
            |b, config| {
                let sim = NetworkSimulator::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let handles: Vec<_> = (0..100u64)
                        .map(|i| {
                            let request = Request {
                                id: i,
                                payload_size: 256,
                                sent_at: Instant::now(),
                            };
                            sim.send_request(request)
                        })
                        .collect();
                    
                    let results: Vec<_> = futures::future::join_all(handles).await;
                    let metrics = sim.get_metrics();
                    black_box((results.len(), metrics.requests_retried))
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Backpressure Benchmarks
// ============================================================================

fn bench_backpressure(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("network/backpressure");
    
    group.sample_size(20);
    
    for threshold in [10, 50, 100, 500, 1000] {
        let config = NetworkConfig {
            backpressure_threshold: threshold,
            base_latency: Duration::from_millis(1), // Slow to test backpressure
            ..Default::default()
        };
        
        group.bench_with_input(
            BenchmarkId::new("threshold", threshold),
            &config,
            |b, config| {
                let sim = Arc::new(NetworkSimulator::new(config.clone()));
                
                b.to_async(&rt).iter(|| {
                    let sim = sim.clone();
                    async move {
                        // Submit more than threshold
                        let handles: Vec<_> = (0..500u64)
                            .map(|i| {
                                let sim = sim.clone();
                                tokio::spawn(async move {
                                    let request = Request {
                                        id: i,
                                        payload_size: 256,
                                        sent_at: Instant::now(),
                                    };
                                    sim.send_request(request).await
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(sim.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

fn bench_burst_handling(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("network/burst_handling");
    
    group.sample_size(20);
    
    for burst_size in [10, 50, 100, 500, 1000] {
        let config = NetworkConfig {
            backpressure_threshold: 100,
            ..Default::default()
        };
        
        group.throughput(Throughput::Elements(burst_size as u64));
        
        group.bench_with_input(
            BenchmarkId::new("burst_size", burst_size),
            &burst_size,
            |b, &burst_size| {
                let sim = Arc::new(NetworkSimulator::new(config.clone()));
                
                b.to_async(&rt).iter(|| {
                    let sim = sim.clone();
                    async move {
                        let start = Instant::now();
                        
                        let handles: Vec<_> = (0..burst_size as u64)
                            .map(|i| {
                                let sim = sim.clone();
                                tokio::spawn(async move {
                                    let request = Request {
                                        id: i,
                                        payload_size: 256,
                                        sent_at: Instant::now(),
                                    };
                                    sim.send_request(request).await
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        let elapsed = start.elapsed();
                        black_box((sim.get_metrics(), elapsed))
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Payload Size Benchmarks
// ============================================================================

fn bench_payload_sizes(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("network/payload_sizes");
    
    group.sample_size(30);
    
    for size in [64, 256, 1024, 4096, 16384, 65536] {
        let config = NetworkConfig::default();
        
        group.throughput(Throughput::Bytes(size as u64 * 100));
        
        group.bench_with_input(
            BenchmarkId::new("payload_bytes", size),
            &size,
            |b, &size| {
                let sim = NetworkSimulator::new(config.clone());
                
                b.to_async(&rt).iter(|| async {
                    let handles: Vec<_> = (0..100u64)
                        .map(|i| {
                            let request = Request {
                                id: i,
                                payload_size: size,
                                sent_at: Instant::now(),
                            };
                            sim.send_request(request)
                        })
                        .collect();
                    
                    futures::future::join_all(handles).await;
                    black_box(sim.get_metrics())
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
    bench_latency_throughput,
    bench_packet_loss,
    bench_hedged_requests,
    bench_retry_strategies,
    bench_backpressure,
    bench_burst_handling,
    bench_payload_sizes,
);

criterion_main!(benches);

// ============================================================================
// Expected Results Reference
// ============================================================================

/// Expected benchmark results for reference.
///
/// | Metric                        | P50      | P99      | Notes                  |
/// |-------------------------------|----------|----------|------------------------|
/// | Base latency (500µs)          | 520µs    | 600µs    | ±100µs jitter          |
/// | 1% packet loss + retries      | 510µs    | 800µs    | Retry overhead         |
/// | 10% loss + hedging            | 515µs    | 650µs    | Hedging reduces P99    |
/// | 10% loss + retries only       | 520µs    | 1.2ms    | Higher tail latency    |
/// | Backpressure @ 100 threshold  | +5ms     | +50ms    | Wait for permits       |
///
/// ## Tradeoffs
///
/// - **Hedged Requests**: Reduces tail latency but increases network load
/// - **Retries**: More reliable but adds latency under failures
/// - **Backpressure**: Prevents overload but increases latency
/// - **Hedge Delay**: Lower = more hedges, higher bandwidth; Higher = less hedging benefit
#[cfg(test)]
mod expected_results {}
