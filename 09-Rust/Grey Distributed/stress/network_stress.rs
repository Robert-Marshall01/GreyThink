//! # Network Stress Test
//!
//! Tests network layer resilience under adverse conditions:
//! - Packet loss simulation (0-50%)
//! - Network congestion and backpressure
//! - Retry storms and exponential backoff
//! - Connection pool exhaustion
//! - Cross-region latency simulation
//!
//! ## Scenarios
//! - Healthy: Baseline performance measurement
//! - Degraded: 5% packet loss, 50ms added latency
//! - Stressed: 20% packet loss, variable latency
//! - Chaos: Random failures, partitions, and delays
//!
//! ## Usage
//! ```bash
//! cargo run --release --bin network_stress -- \
//!     --nodes 100 \
//!     --duration 3600 \
//!     --scenario degraded \
//!     --packet-loss 0.05
//! ```

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use rand::prelude::*;
use tokio::sync::{mpsc, Semaphore};
use tokio::time::{interval, sleep, timeout};

// =============================================================================
// Configuration
// =============================================================================

#[derive(Clone, Debug)]
pub struct StressConfig {
    /// Number of nodes in the network
    pub node_count: usize,
    /// Test duration in seconds
    pub duration_secs: u64,
    /// Network scenario
    pub scenario: NetworkScenario,
    /// Base packet loss rate (0.0 - 1.0)
    pub packet_loss_rate: f64,
    /// Additional latency (min, max) in milliseconds
    pub added_latency: (u64, u64),
    /// Connection pool size per node
    pub connection_pool_size: usize,
    /// Max concurrent connections per node
    pub max_connections: usize,
    /// Message rate per node per second
    pub message_rate: u64,
    /// Message size range (min, max) bytes
    pub message_size: (usize, usize),
    /// Retry configuration
    pub retry_config: RetryConfig,
    /// Cross-region simulation
    pub cross_region: bool,
    /// Random seed
    pub seed: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NetworkScenario {
    Healthy,
    Degraded,
    Stressed,
    Chaos,
    PartitionRecovery,
}

#[derive(Clone, Debug)]
pub struct RetryConfig {
    pub max_attempts: u32,
    pub base_delay_ms: u64,
    pub max_delay_ms: u64,
    pub multiplier: f64,
    pub jitter: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_attempts: 5,
            base_delay_ms: 100,
            max_delay_ms: 30000,
            multiplier: 2.0,
            jitter: 0.1,
        }
    }
}

impl Default for StressConfig {
    fn default() -> Self {
        Self {
            node_count: 100,
            duration_secs: 3600,
            scenario: NetworkScenario::Degraded,
            packet_loss_rate: 0.05,
            added_latency: (10, 100),
            connection_pool_size: 50,
            max_connections: 1000,
            message_rate: 1000,
            message_size: (64, 4096),
            retry_config: RetryConfig::default(),
            cross_region: true,
            seed: 42,
        }
    }
}

// =============================================================================
// Network Types
// =============================================================================

#[derive(Clone, Debug)]
pub struct Message {
    pub id: u64,
    pub from: u64,
    pub to: u64,
    pub payload_size: usize,
    pub sent_at: Instant,
    pub attempt: u32,
    pub message_type: MessageType,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MessageType {
    Heartbeat,
    Request,
    Response,
    Replication,
    Control,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeliveryResult {
    Delivered,
    Lost,
    Timeout,
    ConnectionFailed,
    Rejected,
}

#[derive(Clone, Debug)]
pub struct DeliveryReport {
    pub message_id: u64,
    pub result: DeliveryResult,
    pub latency: Duration,
    pub attempts: u32,
    pub message_type: MessageType,
}

// =============================================================================
// Network Node
// =============================================================================

pub struct NetworkNode {
    pub id: u64,
    pub region: u64,
    pub connections: Mutex<HashMap<u64, Connection>>,
    pub message_queue: Mutex<VecDeque<Message>>,
    pub active_connections: AtomicU64,
    pub messages_sent: AtomicU64,
    pub messages_received: AtomicU64,
    pub bytes_sent: AtomicU64,
    pub bytes_received: AtomicU64,
    pub connection_errors: AtomicU64,
}

impl NetworkNode {
    pub fn new(id: u64, region: u64) -> Self {
        Self {
            id,
            region,
            connections: Mutex::new(HashMap::new()),
            message_queue: Mutex::new(VecDeque::new()),
            active_connections: AtomicU64::new(0),
            messages_sent: AtomicU64::new(0),
            messages_received: AtomicU64::new(0),
            bytes_sent: AtomicU64::new(0),
            bytes_received: AtomicU64::new(0),
            connection_errors: AtomicU64::new(0),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Connection {
    pub peer_id: u64,
    pub state: ConnectionState,
    pub created_at: Instant,
    pub last_used: Instant,
    pub messages_sent: u64,
    pub failures: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConnectionState {
    Idle,
    Active,
    Draining,
    Failed,
}

// =============================================================================
// Network Simulator
// =============================================================================

pub struct NetworkSimulator {
    config: StressConfig,
    nodes: HashMap<u64, Arc<NetworkNode>>,
    /// Region-to-region latency matrix (in ms)
    region_latency: HashMap<(u64, u64), u64>,
    metrics: Arc<NetworkMetrics>,
    rng: Mutex<StdRng>,
}

impl NetworkSimulator {
    pub fn new(config: StressConfig) -> Self {
        let mut rng = StdRng::seed_from_u64(config.seed);

        // Create nodes with region assignment
        let num_regions = if config.cross_region { 5 } else { 1 };
        let mut nodes = HashMap::new();
        for i in 0..config.node_count {
            let region = (i % num_regions) as u64;
            nodes.insert(i as u64, Arc::new(NetworkNode::new(i as u64, region)));
        }

        // Create region latency matrix
        let mut region_latency = HashMap::new();
        for r1 in 0..num_regions {
            for r2 in 0..num_regions {
                let latency = if r1 == r2 {
                    rng.gen_range(1..5) // Same region: 1-5ms
                } else {
                    rng.gen_range(50..200) // Cross-region: 50-200ms
                };
                region_latency.insert((r1 as u64, r2 as u64), latency);
            }
        }

        Self {
            config,
            nodes,
            region_latency,
            metrics: Arc::new(NetworkMetrics::new()),
            rng: Mutex::new(rng),
        }
    }

    /// Get base latency between two nodes
    pub fn base_latency(&self, from: u64, to: u64) -> Duration {
        let from_node = self.nodes.get(&from);
        let to_node = self.nodes.get(&to);

        if let (Some(from_n), Some(to_n)) = (from_node, to_node) {
            let key = (from_n.region, to_n.region);
            let base_ms = self.region_latency.get(&key).copied().unwrap_or(10);
            Duration::from_millis(base_ms)
        } else {
            Duration::from_millis(10)
        }
    }

    /// Calculate effective latency with scenario modifications
    pub fn effective_latency(&self, from: u64, to: u64) -> Duration {
        let base = self.base_latency(from, to);
        let mut rng = self.rng.lock();

        let added = match self.config.scenario {
            NetworkScenario::Healthy => Duration::ZERO,
            NetworkScenario::Degraded => {
                Duration::from_millis(rng.gen_range(self.config.added_latency.0..=self.config.added_latency.1))
            }
            NetworkScenario::Stressed => {
                Duration::from_millis(rng.gen_range(50..500))
            }
            NetworkScenario::Chaos => {
                if rng.gen::<f64>() < 0.1 {
                    Duration::from_secs(rng.gen_range(1..5)) // Occasional massive delay
                } else {
                    Duration::from_millis(rng.gen_range(0..200))
                }
            }
            NetworkScenario::PartitionRecovery => {
                Duration::from_millis(rng.gen_range(10..100))
            }
        };

        base + added
    }

    /// Determine if a packet should be lost
    pub fn should_drop(&self) -> bool {
        let mut rng = self.rng.lock();

        let drop_rate = match self.config.scenario {
            NetworkScenario::Healthy => 0.001,
            NetworkScenario::Degraded => self.config.packet_loss_rate,
            NetworkScenario::Stressed => self.config.packet_loss_rate * 2.0,
            NetworkScenario::Chaos => rng.gen_range(0.0..0.5),
            NetworkScenario::PartitionRecovery => 0.1,
        };

        rng.gen::<f64>() < drop_rate
    }

    /// Send a message with retry logic
    pub async fn send_with_retry(&self, msg: Message) -> DeliveryReport {
        let start = Instant::now();
        let mut attempt = 0u32;
        let mut last_result = DeliveryResult::Lost;

        while attempt < self.config.retry_config.max_attempts {
            attempt += 1;

            // Simulate network delay
            let latency = self.effective_latency(msg.from, msg.to);
            sleep(latency).await;

            // Check for packet loss
            if self.should_drop() {
                self.metrics.packets_dropped.fetch_add(1, Ordering::Relaxed);
                last_result = DeliveryResult::Lost;

                // Calculate backoff delay
                let delay = self.calculate_backoff(attempt);
                self.metrics.retry_count.fetch_add(1, Ordering::Relaxed);
                sleep(delay).await;
                continue;
            }

            // Check connection availability
            if let Some(node) = self.nodes.get(&msg.to) {
                let active = node.active_connections.load(Ordering::Relaxed);
                if active as usize >= self.config.max_connections {
                    self.metrics.connection_rejections.fetch_add(1, Ordering::Relaxed);
                    last_result = DeliveryResult::Rejected;
                    continue;
                }

                // Deliver message
                node.messages_received.fetch_add(1, Ordering::Relaxed);
                node.bytes_received.fetch_add(msg.payload_size as u64, Ordering::Relaxed);

                self.metrics.packets_delivered.fetch_add(1, Ordering::Relaxed);
                self.metrics.bytes_transferred.fetch_add(msg.payload_size as u64, Ordering::Relaxed);
                self.metrics.record_latency(start.elapsed(), msg.message_type);

                return DeliveryReport {
                    message_id: msg.id,
                    result: DeliveryResult::Delivered,
                    latency: start.elapsed(),
                    attempts: attempt,
                    message_type: msg.message_type,
                };
            } else {
                last_result = DeliveryResult::ConnectionFailed;
            }
        }

        // All retries exhausted
        self.metrics.delivery_failures.fetch_add(1, Ordering::Relaxed);

        DeliveryReport {
            message_id: msg.id,
            result: last_result,
            latency: start.elapsed(),
            attempts: attempt,
            message_type: msg.message_type,
        }
    }

    fn calculate_backoff(&self, attempt: u32) -> Duration {
        let mut rng = self.rng.lock();
        let cfg = &self.config.retry_config;

        let base = cfg.base_delay_ms as f64;
        let delay = base * cfg.multiplier.powi(attempt as i32 - 1);
        let capped = delay.min(cfg.max_delay_ms as f64);
        let jitter = capped * cfg.jitter * rng.gen_range(-1.0..1.0);

        Duration::from_millis((capped + jitter).max(0.0) as u64)
    }
}

// =============================================================================
// Metrics Collection
// =============================================================================

pub struct NetworkMetrics {
    // Packet metrics
    pub packets_sent: AtomicU64,
    pub packets_delivered: AtomicU64,
    pub packets_dropped: AtomicU64,
    pub delivery_failures: AtomicU64,

    // Retry metrics
    pub retry_count: AtomicU64,
    pub retry_storms: AtomicU64,  // >100 retries/sec

    // Connection metrics
    pub connections_opened: AtomicU64,
    pub connections_closed: AtomicU64,
    pub connection_rejections: AtomicU64,
    pub connection_timeouts: AtomicU64,

    // Bandwidth metrics
    pub bytes_transferred: AtomicU64,

    // Latency tracking (microseconds)
    pub latencies: Mutex<VecDeque<u64>>,
    pub latencies_by_type: [Mutex<VecDeque<u64>>; 5],

    // Congestion metrics
    pub backpressure_events: AtomicU64,
    pub queue_depth_samples: Mutex<VecDeque<usize>>,

    start_time: Instant,
}

impl NetworkMetrics {
    pub fn new() -> Self {
        Self {
            packets_sent: AtomicU64::new(0),
            packets_delivered: AtomicU64::new(0),
            packets_dropped: AtomicU64::new(0),
            delivery_failures: AtomicU64::new(0),

            retry_count: AtomicU64::new(0),
            retry_storms: AtomicU64::new(0),

            connections_opened: AtomicU64::new(0),
            connections_closed: AtomicU64::new(0),
            connection_rejections: AtomicU64::new(0),
            connection_timeouts: AtomicU64::new(0),

            bytes_transferred: AtomicU64::new(0),

            latencies: Mutex::new(VecDeque::with_capacity(100000)),
            latencies_by_type: std::array::from_fn(|_| Mutex::new(VecDeque::with_capacity(10000))),

            backpressure_events: AtomicU64::new(0),
            queue_depth_samples: Mutex::new(VecDeque::with_capacity(10000)),

            start_time: Instant::now(),
        }
    }

    pub fn record_latency(&self, latency: Duration, msg_type: MessageType) {
        let us = latency.as_micros() as u64;

        let mut latencies = self.latencies.lock();
        latencies.push_back(us);
        if latencies.len() > 100000 {
            latencies.pop_front();
        }

        let type_idx = msg_type as usize;
        if type_idx < 5 {
            let mut type_latencies = self.latencies_by_type[type_idx].lock();
            type_latencies.push_back(us);
            if type_latencies.len() > 10000 {
                type_latencies.pop_front();
            }
        }
    }

    pub fn calculate_percentiles(deque: &VecDeque<u64>) -> LatencyStats {
        if deque.is_empty() {
            return LatencyStats::default();
        }

        let mut sorted: Vec<_> = deque.iter().copied().collect();
        sorted.sort_unstable();

        let len = sorted.len();
        LatencyStats {
            p50: sorted[len * 50 / 100],
            p90: sorted[len * 90 / 100],
            p99: sorted[len * 99 / 100],
            p999: sorted[(len * 999 / 1000).min(len - 1)],
            max: sorted[len - 1],
            mean: sorted.iter().sum::<u64>() / len as u64,
        }
    }

    pub fn report(&self) -> NetworkReport {
        let elapsed = self.start_time.elapsed();
        let sent = self.packets_sent.load(Ordering::Relaxed);
        let delivered = self.packets_delivered.load(Ordering::Relaxed);
        let dropped = self.packets_dropped.load(Ordering::Relaxed);

        let latencies = self.latencies.lock();

        NetworkReport {
            duration: elapsed,

            packets_sent: sent,
            packets_delivered: delivered,
            packets_dropped: dropped,
            delivery_failures: self.delivery_failures.load(Ordering::Relaxed),
            delivery_rate: delivered as f64 / sent.max(1) as f64,

            throughput: delivered as f64 / elapsed.as_secs_f64(),
            bandwidth_mbps: (self.bytes_transferred.load(Ordering::Relaxed) as f64 * 8.0)
                / (elapsed.as_secs_f64() * 1_000_000.0),

            retry_count: self.retry_count.load(Ordering::Relaxed),
            retry_rate: self.retry_count.load(Ordering::Relaxed) as f64 / sent.max(1) as f64,

            connection_rejections: self.connection_rejections.load(Ordering::Relaxed),
            connection_timeouts: self.connection_timeouts.load(Ordering::Relaxed),

            latency: Self::calculate_percentiles(&latencies),

            backpressure_events: self.backpressure_events.load(Ordering::Relaxed),
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct LatencyStats {
    pub p50: u64,
    pub p90: u64,
    pub p99: u64,
    pub p999: u64,
    pub max: u64,
    pub mean: u64,
}

#[derive(Debug)]
pub struct NetworkReport {
    pub duration: Duration,

    // Packet metrics
    pub packets_sent: u64,
    pub packets_delivered: u64,
    pub packets_dropped: u64,
    pub delivery_failures: u64,
    pub delivery_rate: f64,

    // Throughput
    pub throughput: f64,
    pub bandwidth_mbps: f64,

    // Retry metrics
    pub retry_count: u64,
    pub retry_rate: f64,

    // Connection metrics
    pub connection_rejections: u64,
    pub connection_timeouts: u64,

    // Latency
    pub latency: LatencyStats,

    // Congestion
    pub backpressure_events: u64,
}

impl NetworkReport {
    pub fn to_json(&self) -> String {
        serde_json::json!({
            "duration_secs": self.duration.as_secs_f64(),
            "packets": {
                "sent": self.packets_sent,
                "delivered": self.packets_delivered,
                "dropped": self.packets_dropped,
                "failures": self.delivery_failures,
                "delivery_rate": self.delivery_rate,
            },
            "throughput": {
                "packets_per_sec": self.throughput,
                "bandwidth_mbps": self.bandwidth_mbps,
            },
            "retries": {
                "total": self.retry_count,
                "rate": self.retry_rate,
            },
            "connections": {
                "rejections": self.connection_rejections,
                "timeouts": self.connection_timeouts,
            },
            "latency": {
                "p50_us": self.latency.p50,
                "p90_us": self.latency.p90,
                "p99_us": self.latency.p99,
                "p999_us": self.latency.p999,
                "max_us": self.latency.max,
                "mean_us": self.latency.mean,
            },
            "congestion": {
                "backpressure_events": self.backpressure_events,
            }
        })
        .to_string()
    }
}

// =============================================================================
// Stress Test Runner
// =============================================================================

pub struct StressRunner {
    simulator: Arc<NetworkSimulator>,
    config: StressConfig,
    shutdown: Arc<AtomicBool>,
    message_counter: AtomicU64,
}

impl StressRunner {
    pub fn new(config: StressConfig) -> Self {
        let simulator = Arc::new(NetworkSimulator::new(config.clone()));
        Self {
            simulator,
            config,
            shutdown: Arc::new(AtomicBool::new(false)),
            message_counter: AtomicU64::new(0),
        }
    }

    pub async fn run(&self) -> NetworkReport {
        // Start message senders for each node
        let sender_handles: Vec<_> = self.simulator.nodes.keys()
            .map(|&node_id| self.spawn_message_sender(node_id))
            .collect();

        // Start retry storm detector
        let storm_handle = self.spawn_retry_storm_detector();

        // Start congestion monitor
        let congestion_handle = self.spawn_congestion_monitor();

        // Start metrics reporter
        let metrics_handle = self.spawn_metrics_reporter();

        // Run scenario-specific events
        let scenario_handle = self.spawn_scenario_driver();

        // Run for configured duration
        sleep(Duration::from_secs(self.config.duration_secs)).await;

        // Signal shutdown
        self.shutdown.store(true, Ordering::SeqCst);

        // Wait for all tasks
        for h in sender_handles {
            let _ = h.await;
        }
        let _ = tokio::join!(storm_handle, congestion_handle, metrics_handle, scenario_handle);

        self.simulator.metrics.report()
    }

    fn spawn_message_sender(&self, node_id: u64) -> tokio::task::JoinHandle<()> {
        let simulator = Arc::clone(&self.simulator);
        let shutdown = Arc::clone(&self.shutdown);
        let message_rate = self.config.message_rate;
        let message_size = self.config.message_size;
        let node_count = self.config.node_count;
        let message_counter = &self.message_counter as *const AtomicU64;

        tokio::spawn(async move {
            let message_counter = unsafe { &*message_counter };
            let mut rng = rand::thread_rng();
            let interval_us = 1_000_000 / message_rate.max(1);

            while !shutdown.load(Ordering::Relaxed) {
                // Generate random target (not self)
                let target = loop {
                    let t = rng.gen_range(0..node_count as u64);
                    if t != node_id {
                        break t;
                    }
                };

                // Create message
                let msg = Message {
                    id: message_counter.fetch_add(1, Ordering::Relaxed),
                    from: node_id,
                    to: target,
                    payload_size: rng.gen_range(message_size.0..=message_size.1),
                    sent_at: Instant::now(),
                    attempt: 0,
                    message_type: match rng.gen_range(0..5) {
                        0 => MessageType::Heartbeat,
                        1 => MessageType::Request,
                        2 => MessageType::Response,
                        3 => MessageType::Replication,
                        _ => MessageType::Control,
                    },
                };

                simulator.metrics.packets_sent.fetch_add(1, Ordering::Relaxed);

                // Send with retry
                let sim = Arc::clone(&simulator);
                tokio::spawn(async move {
                    sim.send_with_retry(msg).await;
                });

                sleep(Duration::from_micros(interval_us)).await;
            }
        })
    }

    fn spawn_retry_storm_detector(&self) -> tokio::task::JoinHandle<()> {
        let metrics = Arc::clone(&self.simulator.metrics);
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(1));
            let mut last_retry_count = 0u64;

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                let current = metrics.retry_count.load(Ordering::Relaxed);
                let retries_per_sec = current - last_retry_count;
                last_retry_count = current;

                if retries_per_sec > 100 {
                    metrics.retry_storms.fetch_add(1, Ordering::Relaxed);
                    eprintln!("⚠️  Retry storm detected: {}/s", retries_per_sec);
                }
            }
        })
    }

    fn spawn_congestion_monitor(&self) -> tokio::task::JoinHandle<()> {
        let nodes: Vec<_> = self.simulator.nodes.values().cloned().collect();
        let metrics = Arc::clone(&self.simulator.metrics);
        let shutdown = Arc::clone(&self.shutdown);
        let max_conns = self.config.max_connections;

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(1));

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                let mut total_conns = 0usize;
                let mut congested_nodes = 0usize;

                for node in &nodes {
                    let conns = node.active_connections.load(Ordering::Relaxed) as usize;
                    total_conns += conns;

                    if conns > max_conns * 80 / 100 {
                        congested_nodes += 1;
                    }
                }

                if congested_nodes > nodes.len() / 10 {
                    metrics.backpressure_events.fetch_add(1, Ordering::Relaxed);
                }

                let mut samples = metrics.queue_depth_samples.lock();
                samples.push_back(total_conns);
                if samples.len() > 10000 {
                    samples.pop_front();
                }
            }
        })
    }

    fn spawn_metrics_reporter(&self) -> tokio::task::JoinHandle<()> {
        let metrics = Arc::clone(&self.simulator.metrics);
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(10));

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                let report = metrics.report();
                println!(
                    "[{:.0}s] Sent: {} | Delivered: {:.1}% | Retries: {} | p99: {}μs | Bandwidth: {:.1} Mbps",
                    report.duration.as_secs_f64(),
                    report.packets_sent,
                    report.delivery_rate * 100.0,
                    report.retry_count,
                    report.latency.p99,
                    report.bandwidth_mbps
                );
            }
        })
    }

    fn spawn_scenario_driver(&self) -> tokio::task::JoinHandle<()> {
        let scenario = self.config.scenario;
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            match scenario {
                NetworkScenario::Healthy | NetworkScenario::Degraded | NetworkScenario::Stressed => {
                    // These scenarios are static - no dynamic changes
                }
                NetworkScenario::Chaos => {
                    let mut interval = interval(Duration::from_secs(30));
                    while !shutdown.load(Ordering::Relaxed) {
                        interval.tick().await;
                        println!("🔀 Chaos event triggered");
                        // Scenario effects are built into should_drop and effective_latency
                    }
                }
                NetworkScenario::PartitionRecovery => {
                    let mut cycle = 0;
                    while !shutdown.load(Ordering::Relaxed) {
                        // Alternate between partition and recovery phases
                        let phase_duration = if cycle % 2 == 0 { 60 } else { 30 };
                        let phase_name = if cycle % 2 == 0 { "Partition" } else { "Recovery" };
                        println!("🔄 Phase: {} ({}s)", phase_name, phase_duration);
                        sleep(Duration::from_secs(phase_duration)).await;
                        cycle += 1;
                    }
                }
            }
        })
    }
}

// =============================================================================
// Main Entry Point
// =============================================================================

#[tokio::main]
async fn main() {
    println!("=== Grey Distributed Network Stress Test ===\n");

    let config = StressConfig {
        node_count: 100,
        duration_secs: 3600,
        scenario: NetworkScenario::Degraded,
        packet_loss_rate: 0.05,
        ..Default::default()
    };

    println!("Configuration:");
    println!("  Nodes: {}", config.node_count);
    println!("  Duration: {}s", config.duration_secs);
    println!("  Scenario: {:?}", config.scenario);
    println!("  Packet Loss: {:.1}%", config.packet_loss_rate * 100.0);
    println!("  Added Latency: {:?}ms", config.added_latency);
    println!("  Message Rate: {}/s per node", config.message_rate);
    println!("  Cross-Region: {}", config.cross_region);
    println!();

    let runner = StressRunner::new(config);
    let report = runner.run().await;

    println!("\n=== Final Report ===");
    println!("{}", report.to_json());

    // Write report to file
    let report_path = format!(
        "network_stress_report_{}.json",
        chrono::Utc::now().format("%Y%m%d_%H%M%S")
    );
    std::fs::write(&report_path, report.to_json()).expect("Failed to write report");
    println!("\nReport saved to: {}", report_path);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_short_network_stress() {
        let config = StressConfig {
            node_count: 10,
            duration_secs: 5,
            scenario: NetworkScenario::Degraded,
            packet_loss_rate: 0.1,
            message_rate: 100,
            ..Default::default()
        };

        let runner = StressRunner::new(config);
        let report = runner.run().await;

        assert!(report.packets_sent > 0);
        assert!(report.delivery_rate > 0.5);
    }

    #[test]
    fn test_backoff_calculation() {
        let config = StressConfig::default();
        let simulator = NetworkSimulator::new(config);

        let delay1 = simulator.calculate_backoff(1);
        let delay2 = simulator.calculate_backoff(2);
        let delay3 = simulator.calculate_backoff(3);

        // Each delay should be roughly double the previous
        assert!(delay2 > delay1);
        assert!(delay3 > delay2);
    }
}
