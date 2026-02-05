//! # Scheduler Stress Test
//!
//! Tests the scheduler under mixed workloads with focus on:
//! - High-priority vs low-priority task fairness
//! - Tail latency distribution (p99, p99.9, p99.99)
//! - Multi-tenant quota enforcement under load
//! - Work stealing efficiency across workers
//!
//! ## Workload Patterns
//! - Steady-state: Constant rate of mixed-priority tasks
//! - Burst: Sudden spikes of high-priority tasks
//! - Soak: Sustained maximum throughput
//! - Chaos: Random task sizes and priorities
//!
//! ## Usage
//! ```bash
//! cargo run --release --bin scheduler_stress -- \
//!     --workers 100 \
//!     --tenants 50 \
//!     --duration 3600 \
//!     --pattern mixed
//! ```

use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use rand::prelude::*;
use tokio::sync::{mpsc, Semaphore};
use tokio::time::{interval, sleep};

// =============================================================================
// Configuration
// =============================================================================

#[derive(Clone, Debug)]
pub struct StressConfig {
    /// Number of worker threads
    pub worker_count: usize,
    /// Number of tenants
    pub tenant_count: usize,
    /// Test duration in seconds
    pub duration_secs: u64,
    /// Workload pattern
    pub pattern: WorkloadPattern,
    /// Base task submission rate per second
    pub base_rate: u64,
    /// Burst rate multiplier
    pub burst_multiplier: f64,
    /// Burst duration in seconds
    pub burst_duration: u64,
    /// Burst interval in seconds
    pub burst_interval: u64,
    /// Task timeout in milliseconds
    pub task_timeout_ms: u64,
    /// Max queue depth before backpressure
    pub max_queue_depth: usize,
    /// Work stealing enabled
    pub work_stealing: bool,
    /// Random seed
    pub seed: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WorkloadPattern {
    Steady,
    Burst,
    Soak,
    Chaos,
    Mixed,
}

impl Default for StressConfig {
    fn default() -> Self {
        Self {
            worker_count: 100,
            tenant_count: 50,
            duration_secs: 3600,
            pattern: WorkloadPattern::Mixed,
            base_rate: 10000,
            burst_multiplier: 5.0,
            burst_duration: 30,
            burst_interval: 300,
            task_timeout_ms: 30000,
            max_queue_depth: 100000,
            work_stealing: true,
            seed: 42,
        }
    }
}

// =============================================================================
// Task and Queue Types
// =============================================================================

#[derive(Clone, Debug)]
pub struct Task {
    pub id: u64,
    pub tenant_id: u64,
    pub priority: u8,  // 1-10, higher = more important
    pub cpu_units: u32,
    pub duration_ms: u64,
    pub submitted_at: Instant,
    pub started_at: Option<Instant>,
    pub dependencies: Vec<u64>,
}

impl PartialEq for Task {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Task {}

impl PartialOrd for Task {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Task {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Higher priority first, then earlier submission
        self.priority
            .cmp(&other.priority)
            .then_with(|| other.submitted_at.cmp(&self.submitted_at))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TaskStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Timeout,
}

#[derive(Debug)]
pub struct TaskResult {
    pub task_id: u64,
    pub tenant_id: u64,
    pub priority: u8,
    pub status: TaskStatus,
    pub queue_latency: Duration,
    pub execution_latency: Duration,
    pub total_latency: Duration,
}

// =============================================================================
// Tenant Quota Management
// =============================================================================

#[derive(Debug)]
pub struct TenantQuota {
    pub tenant_id: u64,
    pub max_cpu: u32,
    pub max_concurrent: u32,
    pub rate_limit: u64,  // tasks per second
    pub current_cpu: AtomicU64,
    pub current_concurrent: AtomicU64,
    pub submitted_count: AtomicU64,
    pub completed_count: AtomicU64,
    pub rejected_count: AtomicU64,
    pub throttled: AtomicBool,
}

impl TenantQuota {
    pub fn new(tenant_id: u64, max_cpu: u32, max_concurrent: u32, rate_limit: u64) -> Self {
        Self {
            tenant_id,
            max_cpu,
            max_concurrent,
            rate_limit,
            current_cpu: AtomicU64::new(0),
            current_concurrent: AtomicU64::new(0),
            submitted_count: AtomicU64::new(0),
            completed_count: AtomicU64::new(0),
            rejected_count: AtomicU64::new(0),
            throttled: AtomicBool::new(false),
        }
    }

    pub fn can_submit(&self, task: &Task) -> bool {
        let cpu = self.current_cpu.load(Ordering::Relaxed) as u32;
        let concurrent = self.current_concurrent.load(Ordering::Relaxed) as u32;

        cpu + task.cpu_units <= self.max_cpu && concurrent + 1 <= self.max_concurrent
    }

    pub fn acquire(&self, task: &Task) -> bool {
        if !self.can_submit(task) {
            self.rejected_count.fetch_add(1, Ordering::Relaxed);
            return false;
        }

        self.current_cpu
            .fetch_add(task.cpu_units as u64, Ordering::Relaxed);
        self.current_concurrent.fetch_add(1, Ordering::Relaxed);
        true
    }

    pub fn release(&self, task: &Task) {
        self.current_cpu
            .fetch_sub(task.cpu_units as u64, Ordering::Relaxed);
        self.current_concurrent.fetch_sub(1, Ordering::Relaxed);
        self.completed_count.fetch_add(1, Ordering::Relaxed);
    }
}

// =============================================================================
// Worker Pool
// =============================================================================

pub struct Worker {
    pub id: u64,
    pub queue: Mutex<BinaryHeap<Task>>,
    pub current_task: RwLock<Option<Task>>,
    pub tasks_completed: AtomicU64,
    pub tasks_stolen: AtomicU64,
    pub cpu_time_ms: AtomicU64,
}

impl Worker {
    pub fn new(id: u64) -> Self {
        Self {
            id,
            queue: Mutex::new(BinaryHeap::new()),
            current_task: RwLock::new(None),
            tasks_completed: AtomicU64::new(0),
            tasks_stolen: AtomicU64::new(0),
            cpu_time_ms: AtomicU64::new(0),
        }
    }

    pub fn queue_depth(&self) -> usize {
        self.queue.lock().len()
    }

    pub fn steal(&self) -> Option<Task> {
        let mut queue = self.queue.lock();
        // Steal half the tasks (rounded down)
        if queue.len() > 1 {
            // Pop the lowest priority task
            let mut tasks: Vec<_> = std::mem::take(&mut *queue).into_vec();
            let stolen = tasks.pop();
            *queue = BinaryHeap::from(tasks);
            self.tasks_stolen.fetch_add(1, Ordering::Relaxed);
            stolen
        } else {
            None
        }
    }
}

// =============================================================================
// Metrics Collection
// =============================================================================

pub struct Metrics {
    // Submission metrics
    pub tasks_submitted: AtomicU64,
    pub tasks_rejected: AtomicU64,
    pub tasks_throttled: AtomicU64,

    // Completion metrics
    pub tasks_completed: AtomicU64,
    pub tasks_failed: AtomicU64,
    pub tasks_timeout: AtomicU64,

    // Latency tracking (microseconds)
    pub queue_latencies: Mutex<VecDeque<u64>>,
    pub execution_latencies: Mutex<VecDeque<u64>>,
    pub total_latencies: Mutex<VecDeque<u64>>,

    // Per-priority latencies
    pub priority_latencies: [Mutex<VecDeque<u64>>; 10],

    // Work stealing
    pub steal_attempts: AtomicU64,
    pub steal_successes: AtomicU64,

    // Queue metrics
    pub max_queue_depth: AtomicU64,
    pub queue_depth_samples: Mutex<VecDeque<usize>>,

    // Tenant metrics
    pub tenant_completions: RwLock<HashMap<u64, u64>>,

    start_time: Instant,
}

impl Metrics {
    pub fn new() -> Self {
        Self {
            tasks_submitted: AtomicU64::new(0),
            tasks_rejected: AtomicU64::new(0),
            tasks_throttled: AtomicU64::new(0),

            tasks_completed: AtomicU64::new(0),
            tasks_failed: AtomicU64::new(0),
            tasks_timeout: AtomicU64::new(0),

            queue_latencies: Mutex::new(VecDeque::with_capacity(100000)),
            execution_latencies: Mutex::new(VecDeque::with_capacity(100000)),
            total_latencies: Mutex::new(VecDeque::with_capacity(100000)),

            priority_latencies: std::array::from_fn(|_| Mutex::new(VecDeque::with_capacity(10000))),

            steal_attempts: AtomicU64::new(0),
            steal_successes: AtomicU64::new(0),

            max_queue_depth: AtomicU64::new(0),
            queue_depth_samples: Mutex::new(VecDeque::with_capacity(10000)),

            tenant_completions: RwLock::new(HashMap::new()),

            start_time: Instant::now(),
        }
    }

    pub fn record_completion(&self, result: &TaskResult) {
        self.tasks_completed.fetch_add(1, Ordering::Relaxed);

        // Record latencies
        let queue_us = result.queue_latency.as_micros() as u64;
        let exec_us = result.execution_latency.as_micros() as u64;
        let total_us = result.total_latency.as_micros() as u64;

        self.append_latency(&self.queue_latencies, queue_us);
        self.append_latency(&self.execution_latencies, exec_us);
        self.append_latency(&self.total_latencies, total_us);

        // Per-priority tracking
        let priority_idx = (result.priority as usize).saturating_sub(1).min(9);
        self.append_latency(&self.priority_latencies[priority_idx], total_us);

        // Tenant tracking
        let mut completions = self.tenant_completions.write();
        *completions.entry(result.tenant_id).or_insert(0) += 1;
    }

    fn append_latency(&self, deque: &Mutex<VecDeque<u64>>, value: u64) {
        let mut d = deque.lock();
        d.push_back(value);
        if d.len() > 100000 {
            d.pop_front();
        }
    }

    pub fn calculate_percentiles(&self, deque: &Mutex<VecDeque<u64>>) -> LatencyStats {
        let d = deque.lock();
        if d.is_empty() {
            return LatencyStats::default();
        }

        let mut sorted: Vec<_> = d.iter().copied().collect();
        sorted.sort_unstable();

        let len = sorted.len();
        LatencyStats {
            p50: sorted[len * 50 / 100],
            p90: sorted[len * 90 / 100],
            p99: sorted[len * 99 / 100],
            p999: sorted[(len * 999 / 1000).min(len - 1)],
            p9999: sorted[(len * 9999 / 10000).min(len - 1)],
            max: sorted[len - 1],
            mean: sorted.iter().sum::<u64>() / len as u64,
        }
    }

    pub fn report(&self) -> StressReport {
        let elapsed = self.start_time.elapsed();
        let completed = self.tasks_completed.load(Ordering::Relaxed);

        StressReport {
            duration: elapsed,
            tasks_submitted: self.tasks_submitted.load(Ordering::Relaxed),
            tasks_completed: completed,
            tasks_failed: self.tasks_failed.load(Ordering::Relaxed),
            tasks_timeout: self.tasks_timeout.load(Ordering::Relaxed),
            tasks_rejected: self.tasks_rejected.load(Ordering::Relaxed),

            throughput: completed as f64 / elapsed.as_secs_f64(),

            queue_latency: self.calculate_percentiles(&self.queue_latencies),
            execution_latency: self.calculate_percentiles(&self.execution_latencies),
            total_latency: self.calculate_percentiles(&self.total_latencies),

            priority_latencies: std::array::from_fn(|i| {
                self.calculate_percentiles(&self.priority_latencies[i])
            }),

            steal_attempts: self.steal_attempts.load(Ordering::Relaxed),
            steal_successes: self.steal_successes.load(Ordering::Relaxed),
            steal_rate: self.steal_successes.load(Ordering::Relaxed) as f64
                / self.steal_attempts.load(Ordering::Relaxed).max(1) as f64,

            max_queue_depth: self.max_queue_depth.load(Ordering::Relaxed) as usize,

            tenant_fairness: self.calculate_fairness(),
        }
    }

    fn calculate_fairness(&self) -> f64 {
        let completions = self.tenant_completions.read();
        if completions.is_empty() {
            return 1.0;
        }

        let values: Vec<f64> = completions.values().map(|&v| v as f64).collect();
        let mean = values.iter().sum::<f64>() / values.len() as f64;
        let variance = values.iter().map(|v| (v - mean).powi(2)).sum::<f64>() / values.len() as f64;
        let stddev = variance.sqrt();

        // Coefficient of variation (lower = more fair)
        1.0 - (stddev / mean).min(1.0)
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct LatencyStats {
    pub p50: u64,
    pub p90: u64,
    pub p99: u64,
    pub p999: u64,
    pub p9999: u64,
    pub max: u64,
    pub mean: u64,
}

#[derive(Debug)]
pub struct StressReport {
    pub duration: Duration,
    pub tasks_submitted: u64,
    pub tasks_completed: u64,
    pub tasks_failed: u64,
    pub tasks_timeout: u64,
    pub tasks_rejected: u64,

    pub throughput: f64,

    pub queue_latency: LatencyStats,
    pub execution_latency: LatencyStats,
    pub total_latency: LatencyStats,

    pub priority_latencies: [LatencyStats; 10],

    pub steal_attempts: u64,
    pub steal_successes: u64,
    pub steal_rate: f64,

    pub max_queue_depth: usize,

    pub tenant_fairness: f64,
}

impl StressReport {
    pub fn to_json(&self) -> String {
        let priority_arr: Vec<_> = self
            .priority_latencies
            .iter()
            .enumerate()
            .map(|(i, s)| {
                serde_json::json!({
                    "priority": i + 1,
                    "p50_us": s.p50,
                    "p99_us": s.p99,
                    "p999_us": s.p999,
                })
            })
            .collect();

        serde_json::json!({
            "duration_secs": self.duration.as_secs_f64(),
            "tasks": {
                "submitted": self.tasks_submitted,
                "completed": self.tasks_completed,
                "failed": self.tasks_failed,
                "timeout": self.tasks_timeout,
                "rejected": self.tasks_rejected,
            },
            "throughput_per_sec": self.throughput,
            "latency": {
                "queue": {
                    "p50_us": self.queue_latency.p50,
                    "p90_us": self.queue_latency.p90,
                    "p99_us": self.queue_latency.p99,
                    "p999_us": self.queue_latency.p999,
                    "p9999_us": self.queue_latency.p9999,
                    "max_us": self.queue_latency.max,
                },
                "execution": {
                    "p50_us": self.execution_latency.p50,
                    "p99_us": self.execution_latency.p99,
                    "p999_us": self.execution_latency.p999,
                },
                "total": {
                    "p50_us": self.total_latency.p50,
                    "p90_us": self.total_latency.p90,
                    "p99_us": self.total_latency.p99,
                    "p999_us": self.total_latency.p999,
                    "p9999_us": self.total_latency.p9999,
                    "max_us": self.total_latency.max,
                },
            },
            "priority_latencies": priority_arr,
            "work_stealing": {
                "attempts": self.steal_attempts,
                "successes": self.steal_successes,
                "success_rate": self.steal_rate,
            },
            "queue": {
                "max_depth": self.max_queue_depth,
            },
            "fairness": {
                "tenant_fairness_score": self.tenant_fairness,
            }
        })
        .to_string()
    }
}

// =============================================================================
// Stress Test Runner
// =============================================================================

pub struct StressRunner {
    config: StressConfig,
    workers: Vec<Arc<Worker>>,
    tenants: HashMap<u64, Arc<TenantQuota>>,
    metrics: Arc<Metrics>,
    shutdown: Arc<AtomicBool>,
    task_counter: AtomicU64,
}

impl StressRunner {
    pub fn new(config: StressConfig) -> Self {
        let mut rng = StdRng::seed_from_u64(config.seed);

        // Create workers
        let workers: Vec<_> = (0..config.worker_count)
            .map(|i| Arc::new(Worker::new(i as u64)))
            .collect();

        // Create tenants with quotas
        let mut tenants = HashMap::new();
        for i in 0..config.tenant_count {
            let max_cpu = rng.gen_range(50..200);
            let max_concurrent = rng.gen_range(10..100);
            let rate_limit = rng.gen_range(100..500);
            tenants.insert(
                i as u64,
                Arc::new(TenantQuota::new(i as u64, max_cpu, max_concurrent, rate_limit)),
            );
        }

        Self {
            config,
            workers,
            tenants,
            metrics: Arc::new(Metrics::new()),
            shutdown: Arc::new(AtomicBool::new(false)),
            task_counter: AtomicU64::new(0),
        }
    }

    pub async fn run(&self) -> StressReport {
        // Start worker execution threads
        let worker_handles: Vec<_> = self
            .workers
            .iter()
            .map(|w| self.spawn_worker_executor(Arc::clone(w)))
            .collect();

        // Start work stealing (if enabled)
        let steal_handle = if self.config.work_stealing {
            Some(self.spawn_work_stealer())
        } else {
            None
        };

        // Start task submission
        let submit_handle = self.spawn_task_submitter();

        // Start metrics reporter
        let metrics_handle = self.spawn_metrics_reporter();

        // Start queue depth sampler
        let depth_handle = self.spawn_queue_depth_sampler();

        // Run for configured duration
        sleep(Duration::from_secs(self.config.duration_secs)).await;

        // Signal shutdown
        self.shutdown.store(true, Ordering::SeqCst);

        // Wait for all tasks
        let _ = tokio::join!(submit_handle, metrics_handle, depth_handle);
        if let Some(h) = steal_handle {
            let _ = h.await;
        }
        for h in worker_handles {
            let _ = h.await;
        }

        self.metrics.report()
    }

    fn spawn_task_submitter(&self) -> tokio::task::JoinHandle<()> {
        let config = self.config.clone();
        let workers: Vec<_> = self.workers.iter().map(Arc::clone).collect();
        let tenants = self.tenants.clone();
        let metrics = Arc::clone(&self.metrics);
        let shutdown = Arc::clone(&self.shutdown);
        let task_counter = &self.task_counter as *const AtomicU64;

        tokio::spawn(async move {
            let task_counter = unsafe { &*task_counter };
            let mut rng = StdRng::seed_from_u64(config.seed);
            let mut burst_active = false;
            let mut burst_end = Instant::now();
            let mut next_burst = Instant::now() + Duration::from_secs(config.burst_interval);

            while !shutdown.load(Ordering::Relaxed) {
                // Determine current rate based on pattern
                let current_rate = match config.pattern {
                    WorkloadPattern::Steady => config.base_rate,
                    WorkloadPattern::Burst => {
                        let now = Instant::now();
                        if now >= next_burst && !burst_active {
                            burst_active = true;
                            burst_end = now + Duration::from_secs(config.burst_duration);
                            next_burst = burst_end + Duration::from_secs(config.burst_interval);
                        }
                        if burst_active && now >= burst_end {
                            burst_active = false;
                        }
                        if burst_active {
                            (config.base_rate as f64 * config.burst_multiplier) as u64
                        } else {
                            config.base_rate
                        }
                    }
                    WorkloadPattern::Soak => (config.base_rate as f64 * 2.0) as u64,
                    WorkloadPattern::Chaos => rng.gen_range(100..config.base_rate * 3),
                    WorkloadPattern::Mixed => {
                        let phase = (Instant::now().elapsed().as_secs() / 60) % 4;
                        match phase {
                            0 => config.base_rate,
                            1 => (config.base_rate as f64 * config.burst_multiplier) as u64,
                            2 => config.base_rate / 2,
                            _ => rng.gen_range(config.base_rate / 2..config.base_rate * 2),
                        }
                    }
                };

                let interval_us = 1_000_000 / current_rate.max(1);

                // Generate task
                let tenant_id = rng.gen_range(0..config.tenant_count as u64);
                let task = Task {
                    id: task_counter.fetch_add(1, Ordering::Relaxed),
                    tenant_id,
                    priority: rng.gen_range(1..=10),
                    cpu_units: rng.gen_range(1..10),
                    duration_ms: rng.gen_range(1..100),
                    submitted_at: Instant::now(),
                    started_at: None,
                    dependencies: Vec::new(),
                };

                // Check tenant quota
                if let Some(quota) = tenants.get(&tenant_id) {
                    if quota.acquire(&task) {
                        // Find least loaded worker
                        let worker = workers
                            .iter()
                            .min_by_key(|w| w.queue_depth())
                            .cloned()
                            .unwrap();

                        // Check queue depth
                        if worker.queue_depth() < config.max_queue_depth {
                            worker.queue.lock().push(task);
                            metrics.tasks_submitted.fetch_add(1, Ordering::Relaxed);
                            quota.submitted_count.fetch_add(1, Ordering::Relaxed);
                        } else {
                            quota.release(&task);
                            metrics.tasks_rejected.fetch_add(1, Ordering::Relaxed);
                        }
                    } else {
                        metrics.tasks_throttled.fetch_add(1, Ordering::Relaxed);
                    }
                }

                sleep(Duration::from_micros(interval_us)).await;
            }
        })
    }

    fn spawn_worker_executor(&self, worker: Arc<Worker>) -> tokio::task::JoinHandle<()> {
        let metrics = Arc::clone(&self.metrics);
        let tenants = self.tenants.clone();
        let shutdown = Arc::clone(&self.shutdown);
        let timeout_ms = self.config.task_timeout_ms;

        tokio::spawn(async move {
            while !shutdown.load(Ordering::Relaxed) {
                // Get next task
                let task = {
                    let mut queue = worker.queue.lock();
                    queue.pop()
                };

                if let Some(mut task) = task {
                    let now = Instant::now();
                    task.started_at = Some(now);

                    // Check for timeout before even starting
                    let queue_latency = now - task.submitted_at;
                    if queue_latency > Duration::from_millis(timeout_ms) {
                        metrics.tasks_timeout.fetch_add(1, Ordering::Relaxed);
                        if let Some(quota) = tenants.get(&task.tenant_id) {
                            quota.release(&task);
                        }
                        continue;
                    }

                    // Simulate task execution
                    *worker.current_task.write() = Some(task.clone());
                    sleep(Duration::from_millis(task.duration_ms)).await;
                    *worker.current_task.write() = None;

                    // Record completion
                    let execution_latency = now.elapsed();
                    let total_latency = task.submitted_at.elapsed();

                    let result = TaskResult {
                        task_id: task.id,
                        tenant_id: task.tenant_id,
                        priority: task.priority,
                        status: TaskStatus::Completed,
                        queue_latency,
                        execution_latency,
                        total_latency,
                    };

                    metrics.record_completion(&result);
                    worker.tasks_completed.fetch_add(1, Ordering::Relaxed);
                    worker
                        .cpu_time_ms
                        .fetch_add(task.duration_ms, Ordering::Relaxed);

                    if let Some(quota) = tenants.get(&task.tenant_id) {
                        quota.release(&task);
                    }
                } else {
                    // No task, brief sleep
                    sleep(Duration::from_micros(100)).await;
                }
            }
        })
    }

    fn spawn_work_stealer(&self) -> tokio::task::JoinHandle<()> {
        let workers: Vec<_> = self.workers.iter().map(Arc::clone).collect();
        let metrics = Arc::clone(&self.metrics);
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut rng = rand::thread_rng();

            while !shutdown.load(Ordering::Relaxed) {
                sleep(Duration::from_millis(10)).await;

                // Find overloaded and underloaded workers
                let depths: Vec<_> = workers.iter().map(|w| w.queue_depth()).collect();
                let avg_depth = depths.iter().sum::<usize>() / depths.len().max(1);

                for (i, worker) in workers.iter().enumerate() {
                    if depths[i] < avg_depth / 2 {
                        // Try to steal from a random busy worker
                        metrics.steal_attempts.fetch_add(1, Ordering::Relaxed);

                        let busy_workers: Vec<_> = workers
                            .iter()
                            .enumerate()
                            .filter(|(j, _)| depths[*j] > avg_depth * 2 && *j != i)
                            .map(|(_, w)| w)
                            .collect();

                        if let Some(victim) = busy_workers.choose(&mut rng) {
                            if let Some(task) = victim.steal() {
                                worker.queue.lock().push(task);
                                metrics.steal_successes.fetch_add(1, Ordering::Relaxed);
                            }
                        }
                    }
                }
            }
        })
    }

    fn spawn_metrics_reporter(&self) -> tokio::task::JoinHandle<()> {
        let metrics = Arc::clone(&self.metrics);
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(10));

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                let report = metrics.report();
                let total_latency = &report.total_latency;

                println!(
                    "[{:.0}s] Tasks: {} | Throughput: {:.0}/s | p99: {}μs | p999: {}μs | Fairness: {:.2}",
                    report.duration.as_secs_f64(),
                    report.tasks_completed,
                    report.throughput,
                    total_latency.p99,
                    total_latency.p999,
                    report.tenant_fairness
                );
            }
        })
    }

    fn spawn_queue_depth_sampler(&self) -> tokio::task::JoinHandle<()> {
        let workers: Vec<_> = self.workers.iter().map(Arc::clone).collect();
        let metrics = Arc::clone(&self.metrics);
        let shutdown = Arc::clone(&self.shutdown);

        tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs(1));

            while !shutdown.load(Ordering::Relaxed) {
                interval.tick().await;

                let total_depth: usize = workers.iter().map(|w| w.queue_depth()).sum();

                // Update max
                let mut max = metrics.max_queue_depth.load(Ordering::Relaxed);
                while total_depth as u64 > max {
                    match metrics.max_queue_depth.compare_exchange(
                        max,
                        total_depth as u64,
                        Ordering::Relaxed,
                        Ordering::Relaxed,
                    ) {
                        Ok(_) => break,
                        Err(current) => max = current,
                    }
                }

                // Sample
                let mut samples = metrics.queue_depth_samples.lock();
                samples.push_back(total_depth);
                if samples.len() > 10000 {
                    samples.pop_front();
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
    println!("=== Grey Distributed Scheduler Stress Test ===\n");

    let config = StressConfig {
        worker_count: 100,
        tenant_count: 50,
        duration_secs: 3600,
        pattern: WorkloadPattern::Mixed,
        base_rate: 10000,
        ..Default::default()
    };

    println!("Configuration:");
    println!("  Workers: {}", config.worker_count);
    println!("  Tenants: {}", config.tenant_count);
    println!("  Duration: {}s", config.duration_secs);
    println!("  Pattern: {:?}", config.pattern);
    println!("  Base Rate: {}/s", config.base_rate);
    println!("  Work Stealing: {}", config.work_stealing);
    println!();

    let runner = StressRunner::new(config);
    let report = runner.run().await;

    println!("\n=== Final Report ===");
    println!("{}", report.to_json());

    // Write report to file
    let report_path = format!(
        "scheduler_stress_report_{}.json",
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
            worker_count: 10,
            tenant_count: 5,
            duration_secs: 5,
            base_rate: 1000,
            ..Default::default()
        };

        let runner = StressRunner::new(config);
        let report = runner.run().await;

        assert!(report.tasks_completed > 0);
        assert!(report.throughput > 0.0);
    }

    #[test]
    fn test_tenant_quota() {
        let quota = TenantQuota::new(1, 100, 10, 1000);
        let task = Task {
            id: 1,
            tenant_id: 1,
            priority: 5,
            cpu_units: 10,
            duration_ms: 100,
            submitted_at: Instant::now(),
            started_at: None,
            dependencies: vec![],
        };

        assert!(quota.acquire(&task));
        assert_eq!(quota.current_cpu.load(Ordering::Relaxed), 10);
        assert_eq!(quota.current_concurrent.load(Ordering::Relaxed), 1);

        quota.release(&task);
        assert_eq!(quota.current_cpu.load(Ordering::Relaxed), 0);
        assert_eq!(quota.current_concurrent.load(Ordering::Relaxed), 0);
    }
}
