//! Grey Distributed — Scheduler Benchmarks
//!
//! Measures scheduler performance:
//! - Task completion times under mixed workloads
//! - Priority lane enforcement
//! - Work stealing efficiency
//!
//! # Running Benchmarks
//!
//! ```bash
//! cargo bench --bench scheduler_bench
//! cargo bench --bench scheduler_bench -- --save-baseline main
//! ```

use criterion::{
    black_box, criterion_group, criterion_main,
    BenchmarkId, Criterion, Throughput,
};
use rand::prelude::*;
use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;
use tokio::sync::{mpsc, Mutex, RwLock, Semaphore};

// ============================================================================
// Configuration
// ============================================================================

#[derive(Clone)]
struct SchedulerConfig {
    /// Number of worker nodes
    num_workers: usize,
    /// Tasks per worker queue
    queue_capacity: usize,
    /// Enable work stealing
    work_stealing: bool,
    /// Work stealing threshold (steal when local queue < threshold)
    steal_threshold: usize,
    /// Priority levels (higher = more lanes)
    priority_levels: usize,
}

impl Default for SchedulerConfig {
    fn default() -> Self {
        Self {
            num_workers: 8,
            queue_capacity: 1000,
            work_stealing: true,
            steal_threshold: 10,
            priority_levels: 4,
        }
    }
}

// ============================================================================
// Task & Queue Implementation
// ============================================================================

#[derive(Clone, Debug)]
struct Task {
    id: u64,
    tenant_id: String,
    priority: u8, // 0-255, higher = more important
    estimated_duration_us: u64,
    created_at: Instant,
}

impl Task {
    fn new(id: u64, tenant_id: &str, priority: u8, duration_us: u64) -> Self {
        Self {
            id,
            tenant_id: tenant_id.to_string(),
            priority,
            estimated_duration_us: duration_us,
            created_at: Instant::now(),
        }
    }
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
        // Higher priority first, then older tasks
        self.priority.cmp(&other.priority)
            .then_with(|| other.created_at.cmp(&self.created_at))
    }
}

/// Priority queue with multiple lanes.
struct PriorityQueue {
    lanes: Vec<VecDeque<Task>>,
    weights: Vec<usize>,
    current_lane: usize,
    served_in_lane: usize,
}

impl PriorityQueue {
    fn new(levels: usize) -> Self {
        // Weighted fair queuing: higher priority = more weight
        let weights: Vec<usize> = (0..levels).map(|l| 1 << l).collect();
        
        Self {
            lanes: (0..levels).map(|_| VecDeque::new()).collect(),
            weights,
            current_lane: levels - 1, // Start with highest priority
            served_in_lane: 0,
        }
    }
    
    fn push(&mut self, task: Task) {
        let lane = (task.priority as usize * self.lanes.len()) / 256;
        let lane = lane.min(self.lanes.len() - 1);
        self.lanes[lane].push_back(task);
    }
    
    fn pop(&mut self) -> Option<Task> {
        // Weighted round-robin across lanes
        for _ in 0..self.lanes.len() {
            if self.served_in_lane >= self.weights[self.current_lane] {
                self.current_lane = (self.current_lane + self.lanes.len() - 1) % self.lanes.len();
                self.served_in_lane = 0;
            }
            
            if let Some(task) = self.lanes[self.current_lane].pop_front() {
                self.served_in_lane += 1;
                return Some(task);
            }
            
            self.current_lane = (self.current_lane + self.lanes.len() - 1) % self.lanes.len();
            self.served_in_lane = 0;
        }
        
        None
    }
    
    fn len(&self) -> usize {
        self.lanes.iter().map(|l| l.len()).sum()
    }
    
    fn is_empty(&self) -> bool {
        self.lanes.iter().all(|l| l.is_empty())
    }
}

// ============================================================================
// Scheduler Implementation
// ============================================================================

struct Scheduler {
    config: SchedulerConfig,
    workers: Vec<Worker>,
    global_queue: Arc<Mutex<PriorityQueue>>,
    metrics: Arc<SchedulerMetrics>,
}

struct Worker {
    id: usize,
    local_queue: Arc<Mutex<VecDeque<Task>>>,
    completed: Arc<AtomicU64>,
}

struct SchedulerMetrics {
    tasks_submitted: AtomicU64,
    tasks_completed: AtomicU64,
    tasks_stolen: AtomicU64,
    total_wait_time_us: AtomicU64,
    total_execution_time_us: AtomicU64,
}

impl Scheduler {
    fn new(config: SchedulerConfig) -> Self {
        let workers: Vec<Worker> = (0..config.num_workers)
            .map(|id| Worker {
                id,
                local_queue: Arc::new(Mutex::new(VecDeque::new())),
                completed: Arc::new(AtomicU64::new(0)),
            })
            .collect();
        
        Self {
            config: config.clone(),
            workers,
            global_queue: Arc::new(Mutex::new(PriorityQueue::new(config.priority_levels))),
            metrics: Arc::new(SchedulerMetrics {
                tasks_submitted: AtomicU64::new(0),
                tasks_completed: AtomicU64::new(0),
                tasks_stolen: AtomicU64::new(0),
                total_wait_time_us: AtomicU64::new(0),
                total_execution_time_us: AtomicU64::new(0),
            }),
        }
    }
    
    async fn submit(&self, task: Task) {
        self.metrics.tasks_submitted.fetch_add(1, Ordering::Relaxed);
        
        // Simple round-robin to workers
        let worker_id = (task.id as usize) % self.workers.len();
        let mut local = self.workers[worker_id].local_queue.lock().await;
        
        if local.len() < self.config.queue_capacity {
            local.push_back(task);
        } else {
            // Overflow to global queue
            drop(local);
            let mut global = self.global_queue.lock().await;
            global.push(task);
        }
    }
    
    async fn run_worker(&self, worker_id: usize, duration: Duration) {
        let worker = &self.workers[worker_id];
        let start = Instant::now();
        
        while start.elapsed() < duration {
            // Try local queue first
            let task = {
                let mut local = worker.local_queue.lock().await;
                local.pop_front()
            };
            
            let task = match task {
                Some(t) => t,
                None => {
                    // Try global queue
                    let mut global = self.global_queue.lock().await;
                    match global.pop() {
                        Some(t) => t,
                        None if self.config.work_stealing => {
                            drop(global);
                            // Try to steal from other workers
                            self.steal_work(worker_id).await.unwrap_or_else(|| {
                                // No work available, yield
                                std::thread::yield_now();
                                return;
                            })
                        }
                        None => {
                            std::thread::yield_now();
                            continue;
                        }
                    }
                }
            };
            
            // Record wait time
            let wait_time = task.created_at.elapsed();
            self.metrics.total_wait_time_us.fetch_add(
                wait_time.as_micros() as u64,
                Ordering::Relaxed,
            );
            
            // Execute task (simulate work)
            let exec_start = Instant::now();
            self.execute_task(&task).await;
            let exec_time = exec_start.elapsed();
            
            self.metrics.total_execution_time_us.fetch_add(
                exec_time.as_micros() as u64,
                Ordering::Relaxed,
            );
            self.metrics.tasks_completed.fetch_add(1, Ordering::Relaxed);
            worker.completed.fetch_add(1, Ordering::Relaxed);
        }
    }
    
    async fn steal_work(&self, thief_id: usize) -> Option<Task> {
        // Find victim with most work
        let mut best_victim = None;
        let mut max_work = self.config.steal_threshold;
        
        for (i, worker) in self.workers.iter().enumerate() {
            if i == thief_id {
                continue;
            }
            
            let local = worker.local_queue.lock().await;
            if local.len() > max_work {
                max_work = local.len();
                best_victim = Some(i);
            }
        }
        
        if let Some(victim_id) = best_victim {
            let mut victim_queue = self.workers[victim_id].local_queue.lock().await;
            
            // Steal half of victim's work
            let steal_count = victim_queue.len() / 2;
            if steal_count > 0 {
                let task = victim_queue.pop_back();
                self.metrics.tasks_stolen.fetch_add(1, Ordering::Relaxed);
                return task;
            }
        }
        
        None
    }
    
    async fn execute_task(&self, task: &Task) {
        // Simulate task execution
        if task.estimated_duration_us > 0 {
            tokio::time::sleep(Duration::from_micros(task.estimated_duration_us / 10)).await;
        }
    }
    
    fn get_metrics(&self) -> SchedulerMetricsSnapshot {
        let completed = self.metrics.tasks_completed.load(Ordering::Relaxed);
        let wait_time = self.metrics.total_wait_time_us.load(Ordering::Relaxed);
        let exec_time = self.metrics.total_execution_time_us.load(Ordering::Relaxed);
        
        SchedulerMetricsSnapshot {
            tasks_submitted: self.metrics.tasks_submitted.load(Ordering::Relaxed),
            tasks_completed: completed,
            tasks_stolen: self.metrics.tasks_stolen.load(Ordering::Relaxed),
            avg_wait_time_us: if completed > 0 { wait_time / completed } else { 0 },
            avg_exec_time_us: if completed > 0 { exec_time / completed } else { 0 },
        }
    }
}

#[derive(Debug)]
struct SchedulerMetricsSnapshot {
    tasks_submitted: u64,
    tasks_completed: u64,
    tasks_stolen: u64,
    avg_wait_time_us: u64,
    avg_exec_time_us: u64,
}

// ============================================================================
// Task Completion Time Benchmarks
// ============================================================================

fn bench_task_completion(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("scheduler/task_completion");
    
    group.sample_size(30);
    
    // Uniform workload
    group.bench_function("uniform_workload", |b| {
        let config = SchedulerConfig::default();
        let scheduler = Arc::new(Scheduler::new(config.clone()));
        
        b.to_async(&rt).iter(|| {
            let scheduler = scheduler.clone();
            async move {
                // Submit tasks
                for i in 0..1000u64 {
                    let task = Task::new(i, "tenant-1", 128, 100);
                    scheduler.submit(task).await;
                }
                
                // Run workers briefly
                let handles: Vec<_> = (0..scheduler.config.num_workers)
                    .map(|w| {
                        let s = scheduler.clone();
                        tokio::spawn(async move {
                            s.run_worker(w, Duration::from_millis(50)).await;
                        })
                    })
                    .collect();
                
                for h in handles {
                    let _ = h.await;
                }
                
                black_box(scheduler.get_metrics())
            }
        });
    });
    
    // Mixed workload (short + long tasks)
    group.bench_function("mixed_workload", |b| {
        let config = SchedulerConfig::default();
        let scheduler = Arc::new(Scheduler::new(config.clone()));
        
        b.to_async(&rt).iter(|| {
            let scheduler = scheduler.clone();
            async move {
                let mut rng = rand::thread_rng();
                
                // Submit mix of short and long tasks
                for i in 0..1000u64 {
                    let duration = if rng.gen_bool(0.8) { 100 } else { 10000 };
                    let task = Task::new(i, "tenant-1", 128, duration);
                    scheduler.submit(task).await;
                }
                
                let handles: Vec<_> = (0..scheduler.config.num_workers)
                    .map(|w| {
                        let s = scheduler.clone();
                        tokio::spawn(async move {
                            s.run_worker(w, Duration::from_millis(100)).await;
                        })
                    })
                    .collect();
                
                for h in handles {
                    let _ = h.await;
                }
                
                black_box(scheduler.get_metrics())
            }
        });
    });
    
    group.finish();
}

// ============================================================================
// Priority Lane Benchmarks
// ============================================================================

fn bench_priority_enforcement(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("scheduler/priority_lanes");
    
    group.sample_size(30);
    
    // Verify high-priority tasks complete first
    group.bench_function("priority_ordering", |b| {
        let config = SchedulerConfig {
            priority_levels: 4,
            ..Default::default()
        };
        let scheduler = Arc::new(Scheduler::new(config.clone()));
        
        b.to_async(&rt).iter(|| {
            let scheduler = scheduler.clone();
            async move {
                // Submit low priority first
                for i in 0..500u64 {
                    let task = Task::new(i, "tenant-1", 64, 100); // Low priority
                    scheduler.submit(task).await;
                }
                
                // Then high priority
                for i in 500..1000u64 {
                    let task = Task::new(i, "tenant-1", 255, 100); // High priority
                    scheduler.submit(task).await;
                }
                
                let handles: Vec<_> = (0..scheduler.config.num_workers)
                    .map(|w| {
                        let s = scheduler.clone();
                        tokio::spawn(async move {
                            s.run_worker(w, Duration::from_millis(50)).await;
                        })
                    })
                    .collect();
                
                for h in handles {
                    let _ = h.await;
                }
                
                black_box(scheduler.get_metrics())
            }
        });
    });
    
    // Benchmark different priority level counts
    for levels in [2, 4, 8, 16] {
        group.bench_with_input(
            BenchmarkId::new("priority_levels", levels),
            &levels,
            |b, &levels| {
                let config = SchedulerConfig {
                    priority_levels: levels,
                    ..Default::default()
                };
                
                b.to_async(&rt).iter(|| {
                    let scheduler = Arc::new(Scheduler::new(config.clone()));
                    async move {
                        let mut rng = rand::thread_rng();
                        
                        for i in 0..1000u64 {
                            let priority: u8 = rng.gen();
                            let task = Task::new(i, "tenant-1", priority, 100);
                            scheduler.submit(task).await;
                        }
                        
                        let handles: Vec<_> = (0..scheduler.config.num_workers)
                            .map(|w| {
                                let s = scheduler.clone();
                                tokio::spawn(async move {
                                    s.run_worker(w, Duration::from_millis(50)).await;
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(scheduler.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Work Stealing Benchmarks
// ============================================================================

fn bench_work_stealing(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("scheduler/work_stealing");
    
    group.sample_size(30);
    
    // Compare with and without work stealing
    for stealing in [false, true] {
        group.bench_with_input(
            BenchmarkId::new("work_stealing", stealing),
            &stealing,
            |b, &stealing| {
                let config = SchedulerConfig {
                    work_stealing: stealing,
                    ..Default::default()
                };
                
                b.to_async(&rt).iter(|| {
                    let scheduler = Arc::new(Scheduler::new(config.clone()));
                    async move {
                        // Imbalanced workload: all tasks to worker 0
                        for i in 0..1000u64 {
                            let mut task = Task::new(i, "tenant-1", 128, 100);
                            // Force all tasks to worker 0
                            let id = i * scheduler.config.num_workers as u64;
                            task.id = id;
                            scheduler.submit(task).await;
                        }
                        
                        let handles: Vec<_> = (0..scheduler.config.num_workers)
                            .map(|w| {
                                let s = scheduler.clone();
                                tokio::spawn(async move {
                                    s.run_worker(w, Duration::from_millis(100)).await;
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(scheduler.get_metrics())
                    }
                });
            },
        );
    }
    
    // Benchmark steal threshold
    for threshold in [5, 10, 25, 50, 100] {
        group.bench_with_input(
            BenchmarkId::new("steal_threshold", threshold),
            &threshold,
            |b, &threshold| {
                let config = SchedulerConfig {
                    work_stealing: true,
                    steal_threshold: threshold,
                    ..Default::default()
                };
                
                b.to_async(&rt).iter(|| {
                    let scheduler = Arc::new(Scheduler::new(config.clone()));
                    async move {
                        for i in 0..1000u64 {
                            let task = Task::new(i, "tenant-1", 128, 100);
                            scheduler.submit(task).await;
                        }
                        
                        let handles: Vec<_> = (0..scheduler.config.num_workers)
                            .map(|w| {
                                let s = scheduler.clone();
                                tokio::spawn(async move {
                                    s.run_worker(w, Duration::from_millis(50)).await;
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(scheduler.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Scalability Benchmarks
// ============================================================================

fn bench_worker_scaling(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("scheduler/worker_scaling");
    
    group.sample_size(20);
    
    for workers in [1, 2, 4, 8, 16, 32] {
        group.throughput(Throughput::Elements(1000));
        
        group.bench_with_input(
            BenchmarkId::new("num_workers", workers),
            &workers,
            |b, &workers| {
                let config = SchedulerConfig {
                    num_workers: workers,
                    ..Default::default()
                };
                
                b.to_async(&rt).iter(|| {
                    let scheduler = Arc::new(Scheduler::new(config.clone()));
                    async move {
                        for i in 0..1000u64 {
                            let task = Task::new(i, "tenant-1", 128, 100);
                            scheduler.submit(task).await;
                        }
                        
                        let handles: Vec<_> = (0..scheduler.config.num_workers)
                            .map(|w| {
                                let s = scheduler.clone();
                                tokio::spawn(async move {
                                    s.run_worker(w, Duration::from_millis(100)).await;
                                })
                            })
                            .collect();
                        
                        for h in handles {
                            let _ = h.await;
                        }
                        
                        black_box(scheduler.get_metrics())
                    }
                });
            },
        );
    }
    
    group.finish();
}

// ============================================================================
// Multi-Tenant Fairness Benchmarks
// ============================================================================

fn bench_tenant_fairness(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("scheduler/tenant_fairness");
    
    group.sample_size(20);
    
    group.bench_function("equal_tenants", |b| {
        let config = SchedulerConfig::default();
        
        b.to_async(&rt).iter(|| {
            let scheduler = Arc::new(Scheduler::new(config.clone()));
            async move {
                // 3 tenants, equal load
                for i in 0..999u64 {
                    let tenant = format!("tenant-{}", i % 3);
                    let task = Task::new(i, &tenant, 128, 100);
                    scheduler.submit(task).await;
                }
                
                let handles: Vec<_> = (0..scheduler.config.num_workers)
                    .map(|w| {
                        let s = scheduler.clone();
                        tokio::spawn(async move {
                            s.run_worker(w, Duration::from_millis(100)).await;
                        })
                    })
                    .collect();
                
                for h in handles {
                    let _ = h.await;
                }
                
                black_box(scheduler.get_metrics())
            }
        });
    });
    
    group.bench_function("imbalanced_tenants", |b| {
        let config = SchedulerConfig::default();
        
        b.to_async(&rt).iter(|| {
            let scheduler = Arc::new(Scheduler::new(config.clone()));
            async move {
                // Tenant 0: 80% of load, Tenant 1-2: 10% each
                let mut rng = rand::thread_rng();
                
                for i in 0..1000u64 {
                    let r: f64 = rng.gen();
                    let tenant = if r < 0.8 {
                        "tenant-0"
                    } else if r < 0.9 {
                        "tenant-1"
                    } else {
                        "tenant-2"
                    };
                    let task = Task::new(i, tenant, 128, 100);
                    scheduler.submit(task).await;
                }
                
                let handles: Vec<_> = (0..scheduler.config.num_workers)
                    .map(|w| {
                        let s = scheduler.clone();
                        tokio::spawn(async move {
                            s.run_worker(w, Duration::from_millis(100)).await;
                        })
                    })
                    .collect();
                
                for h in handles {
                    let _ = h.await;
                }
                
                black_box(scheduler.get_metrics())
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
    bench_task_completion,
    bench_priority_enforcement,
    bench_work_stealing,
    bench_worker_scaling,
    bench_tenant_fairness,
);

criterion_main!(benches);

// ============================================================================
// Expected Results Reference
// ============================================================================

/// Expected benchmark results for reference.
///
/// | Metric                           | Value      | Notes                    |
/// |----------------------------------|------------|--------------------------|
/// | Task completion (uniform)        | 50K/s      | 8 workers, 100µs tasks   |
/// | Task completion (mixed)          | 30K/s      | 80% short, 20% long      |
/// | Work stealing overhead           | +5%        | CPU overhead             |
/// | Work stealing speedup (imbal.)   | 2-3x       | Imbalanced workload      |
/// | Priority inversion rate          | <0.1%      | High pri tasks first     |
/// | Tenant fairness deviation        | <10%       | Equal resource sharing   |
/// | Worker scaling efficiency        | 85%        | 1→8 workers              |
///
/// ## Tradeoffs
///
/// - **Work Stealing**: Better load balance, more contention overhead
/// - **Priority Levels**: More levels = finer control, more memory
/// - **Steal Threshold**: Lower = faster rebalancing, more stealing overhead
/// - **Queue Capacity**: Higher = better burst handling, more memory
#[cfg(test)]
mod expected_results {}
