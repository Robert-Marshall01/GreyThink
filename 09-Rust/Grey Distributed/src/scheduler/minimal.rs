//! Grey Distributed — Minimal Scheduler Implementation
//!
//! A minimal but runnable deterministic task scheduler demonstrating:
//! - Priority-based fairness lanes
//! - Deterministic task ordering
//! - Work stealing between workers
//!
//! # Design Tradeoffs
//!
//! - **Determinism over throughput**: Single ordering point for reproducibility
//! - **Simple fairness**: Round-robin between priority lanes
//! - **In-memory queues**: No persistence (add journal for production)

use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::cmp::Ordering;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, Mutex, RwLock};

// ============================================================================
// Types
// ============================================================================

pub type TaskId = u64;
pub type TenantId = String;
pub type WorkerId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Priority {
    Critical = 0,
    High = 1,
    Normal = 2,
    Low = 3,
    Background = 4,
}

impl Priority {
    pub fn weight(&self) -> u64 {
        match self {
            Priority::Critical => 16,
            Priority::High => 8,
            Priority::Normal => 4,
            Priority::Low => 2,
            Priority::Background => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Task {
    pub id: TaskId,
    pub tenant_id: TenantId,
    pub priority: Priority,
    pub payload: Vec<u8>,
    pub created_at: Instant,
    pub deadline: Option<Instant>,
    pub retry_count: u32,
    pub max_retries: u32,
}

impl Task {
    pub fn new(id: TaskId, tenant_id: TenantId, priority: Priority, payload: Vec<u8>) -> Self {
        Self {
            id,
            tenant_id,
            priority,
            payload,
            created_at: Instant::now(),
            deadline: None,
            retry_count: 0,
            max_retries: 3,
        }
    }
    
    pub fn with_deadline(mut self, deadline: Instant) -> Self {
        self.deadline = Some(deadline);
        self
    }
}

#[derive(Debug, Clone)]
pub enum TaskResult {
    Success { task_id: TaskId, output: Vec<u8> },
    Failure { task_id: TaskId, error: String, retryable: bool },
}

#[derive(Debug, Clone)]
pub struct WorkerStatus {
    pub id: WorkerId,
    pub active_tasks: u32,
    pub capacity: u32,
    pub last_heartbeat: Instant,
}

// ============================================================================
// Priority Queue Entry
// ============================================================================

struct PriorityEntry {
    task: Task,
    score: u64,
}

impl PartialEq for PriorityEntry {
    fn eq(&self, other: &Self) -> bool {
        self.score == other.score && self.task.id == other.task.id
    }
}

impl Eq for PriorityEntry {}

impl PartialOrd for PriorityEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PriorityEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        // Higher score = higher priority (max-heap)
        self.score.cmp(&other.score)
            .then_with(|| other.task.created_at.cmp(&self.task.created_at)) // FIFO for ties
    }
}

// ============================================================================
// Fairness Lane
// ============================================================================

struct FairnessLane {
    priority: Priority,
    queue: BinaryHeap<PriorityEntry>,
    tokens: u64,
    max_tokens: u64,
}

impl FairnessLane {
    fn new(priority: Priority) -> Self {
        Self {
            priority,
            queue: BinaryHeap::new(),
            tokens: priority.weight(),
            max_tokens: priority.weight() * 2,
        }
    }
    
    fn push(&mut self, task: Task) {
        let score = self.calculate_score(&task);
        self.queue.push(PriorityEntry { task, score });
    }
    
    fn pop(&mut self) -> Option<Task> {
        if self.tokens > 0 {
            self.tokens -= 1;
            self.queue.pop().map(|e| e.task)
        } else {
            None
        }
    }
    
    fn peek(&self) -> Option<&Task> {
        if self.tokens > 0 {
            self.queue.peek().map(|e| &e.task)
        } else {
            None
        }
    }
    
    fn refill_tokens(&mut self) {
        self.tokens = self.max_tokens;
    }
    
    fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
    
    fn len(&self) -> usize {
        self.queue.len()
    }
    
    fn calculate_score(&self, task: &Task) -> u64 {
        let mut score = self.priority.weight() * 1000;
        
        // Boost score for deadlines
        if let Some(deadline) = task.deadline {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining < Duration::from_secs(60) {
                score += 5000; // Urgent deadline boost
            } else if remaining < Duration::from_secs(300) {
                score += 2000; // Approaching deadline
            }
        }
        
        // Age-based boost (prevent starvation)
        let age = task.created_at.elapsed().as_secs();
        score += age.min(100) * 10;
        
        score
    }
}

// ============================================================================
// Scheduler
// ============================================================================

pub struct Scheduler {
    lanes: RwLock<HashMap<Priority, FairnessLane>>,
    workers: RwLock<HashMap<WorkerId, WorkerStatus>>,
    pending_tasks: RwLock<HashMap<TaskId, Task>>,
    task_counter: Mutex<TaskId>,
    
    // Tenant quotas
    tenant_quotas: RwLock<HashMap<TenantId, TenantQuota>>,
}

#[derive(Debug, Clone)]
pub struct TenantQuota {
    pub max_pending: u32,
    pub max_rate_per_sec: u32,
    pub current_pending: u32,
    pub current_rate: u32,
    pub rate_window_start: Instant,
}

impl Default for TenantQuota {
    fn default() -> Self {
        Self {
            max_pending: 1000,
            max_rate_per_sec: 100,
            current_pending: 0,
            current_rate: 0,
            rate_window_start: Instant::now(),
        }
    }
}

impl Scheduler {
    pub fn new() -> Self {
        let mut lanes = HashMap::new();
        lanes.insert(Priority::Critical, FairnessLane::new(Priority::Critical));
        lanes.insert(Priority::High, FairnessLane::new(Priority::High));
        lanes.insert(Priority::Normal, FairnessLane::new(Priority::Normal));
        lanes.insert(Priority::Low, FairnessLane::new(Priority::Low));
        lanes.insert(Priority::Background, FairnessLane::new(Priority::Background));
        
        Self {
            lanes: RwLock::new(lanes),
            workers: RwLock::new(HashMap::new()),
            pending_tasks: RwLock::new(HashMap::new()),
            task_counter: Mutex::new(0),
            tenant_quotas: RwLock::new(HashMap::new()),
        }
    }
    
    /// Submit a task to the scheduler.
    pub async fn submit(&self, mut task: Task) -> Result<TaskId, &'static str> {
        // Assign task ID if not set
        if task.id == 0 {
            let mut counter = self.task_counter.lock().await;
            *counter += 1;
            task.id = *counter;
        }
        
        // Check tenant quota
        {
            let mut quotas = self.tenant_quotas.write().await;
            let quota = quotas.entry(task.tenant_id.clone())
                .or_insert_with(TenantQuota::default);
            
            // Reset rate window if needed
            if quota.rate_window_start.elapsed() > Duration::from_secs(1) {
                quota.current_rate = 0;
                quota.rate_window_start = Instant::now();
            }
            
            if quota.current_pending >= quota.max_pending {
                return Err("pending quota exceeded");
            }
            
            if quota.current_rate >= quota.max_rate_per_sec {
                return Err("rate limit exceeded");
            }
            
            quota.current_pending += 1;
            quota.current_rate += 1;
        }
        
        // Add to lane
        let task_id = task.id;
        let priority = task.priority;
        
        {
            let mut pending = self.pending_tasks.write().await;
            pending.insert(task_id, task.clone());
        }
        
        {
            let mut lanes = self.lanes.write().await;
            if let Some(lane) = lanes.get_mut(&priority) {
                lane.push(task);
            }
        }
        
        Ok(task_id)
    }
    
    /// Dequeue the next task for a worker.
    pub async fn dequeue(&self, worker_id: WorkerId) -> Option<Task> {
        // Update worker heartbeat
        {
            let mut workers = self.workers.write().await;
            if let Some(status) = workers.get_mut(&worker_id) {
                status.last_heartbeat = Instant::now();
            }
        }
        
        let mut lanes = self.lanes.write().await;
        
        // Try lanes in priority order with fairness tokens
        let priorities = [
            Priority::Critical,
            Priority::High,
            Priority::Normal,
            Priority::Low,
            Priority::Background,
        ];
        
        // First pass: use token-based scheduling
        for priority in &priorities {
            if let Some(lane) = lanes.get_mut(priority) {
                if let Some(task) = lane.pop() {
                    return Some(task);
                }
            }
        }
        
        // Second pass: if all lanes exhausted tokens, refill and retry
        let needs_refill = lanes.values().all(|l| l.tokens == 0 || l.is_empty());
        if needs_refill {
            for lane in lanes.values_mut() {
                lane.refill_tokens();
            }
            
            for priority in &priorities {
                if let Some(lane) = lanes.get_mut(priority) {
                    if let Some(task) = lane.pop() {
                        return Some(task);
                    }
                }
            }
        }
        
        None
    }
    
    /// Complete a task.
    pub async fn complete(&self, result: TaskResult) {
        let task_id = match &result {
            TaskResult::Success { task_id, .. } => *task_id,
            TaskResult::Failure { task_id, .. } => *task_id,
        };
        
        let mut pending = self.pending_tasks.write().await;
        
        if let Some(task) = pending.remove(&task_id) {
            // Update tenant quota
            let mut quotas = self.tenant_quotas.write().await;
            if let Some(quota) = quotas.get_mut(&task.tenant_id) {
                quota.current_pending = quota.current_pending.saturating_sub(1);
            }
            
            // Handle retries
            if let TaskResult::Failure { retryable, .. } = result {
                if retryable && task.retry_count < task.max_retries {
                    let mut retry_task = task;
                    retry_task.retry_count += 1;
                    
                    let priority = retry_task.priority;
                    pending.insert(retry_task.id, retry_task.clone());
                    
                    drop(pending);
                    drop(quotas);
                    
                    let mut lanes = self.lanes.write().await;
                    if let Some(lane) = lanes.get_mut(&priority) {
                        lane.push(retry_task);
                    }
                }
            }
        }
    }
    
    /// Register a worker.
    pub async fn register_worker(&self, id: WorkerId, capacity: u32) {
        let mut workers = self.workers.write().await;
        workers.insert(id, WorkerStatus {
            id,
            active_tasks: 0,
            capacity,
            last_heartbeat: Instant::now(),
        });
    }
    
    /// Unregister a worker.
    pub async fn unregister_worker(&self, id: WorkerId) {
        let mut workers = self.workers.write().await;
        workers.remove(&id);
    }
    
    /// Set tenant quota.
    pub async fn set_tenant_quota(&self, tenant_id: TenantId, quota: TenantQuota) {
        let mut quotas = self.tenant_quotas.write().await;
        quotas.insert(tenant_id, quota);
    }
    
    /// Get scheduler stats.
    pub async fn stats(&self) -> SchedulerStats {
        let lanes = self.lanes.read().await;
        let workers = self.workers.read().await;
        let pending = self.pending_tasks.read().await;
        
        let mut queue_depths = HashMap::new();
        for (priority, lane) in lanes.iter() {
            queue_depths.insert(*priority, lane.len());
        }
        
        SchedulerStats {
            total_pending: pending.len(),
            queue_depths,
            worker_count: workers.len(),
            active_workers: workers.values()
                .filter(|w| w.last_heartbeat.elapsed() < Duration::from_secs(30))
                .count(),
        }
    }
    
    /// Work stealing: transfer tasks from one lane to workers with capacity.
    pub async fn steal_work(&self, from_priority: Priority, count: usize) -> Vec<Task> {
        let mut lanes = self.lanes.write().await;
        let mut stolen = Vec::new();
        
        if let Some(lane) = lanes.get_mut(&from_priority) {
            for _ in 0..count {
                if let Some(entry) = lane.queue.pop() {
                    stolen.push(entry.task);
                } else {
                    break;
                }
            }
        }
        
        stolen
    }
}

#[derive(Debug)]
pub struct SchedulerStats {
    pub total_pending: usize,
    pub queue_depths: HashMap<Priority, usize>,
    pub worker_count: usize,
    pub active_workers: usize,
}

// ============================================================================
// Worker
// ============================================================================

pub struct Worker {
    pub id: WorkerId,
    scheduler: Arc<Scheduler>,
    handler: Box<dyn Fn(&Task) -> TaskResult + Send + Sync>,
}

impl Worker {
    pub fn new(
        id: WorkerId,
        scheduler: Arc<Scheduler>,
        handler: impl Fn(&Task) -> TaskResult + Send + Sync + 'static,
    ) -> Self {
        Self {
            id,
            scheduler,
            handler: Box::new(handler),
        }
    }
    
    /// Run the worker loop.
    pub async fn run(self, mut shutdown_rx: mpsc::Receiver<()>) {
        self.scheduler.register_worker(self.id, 10).await;
        
        loop {
            tokio::select! {
                _ = shutdown_rx.recv() => {
                    break;
                }
                _ = tokio::time::sleep(Duration::from_millis(10)) => {
                    if let Some(task) = self.scheduler.dequeue(self.id).await {
                        let result = (self.handler)(&task);
                        self.scheduler.complete(result).await;
                    }
                }
            }
        }
        
        self.scheduler.unregister_worker(self.id).await;
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_basic_scheduling() {
        let scheduler = Scheduler::new();
        
        let task = Task::new(0, "tenant-1".into(), Priority::Normal, vec![1, 2, 3]);
        let task_id = scheduler.submit(task).await.unwrap();
        
        assert!(task_id > 0);
        
        let stats = scheduler.stats().await;
        assert_eq!(stats.total_pending, 1);
        
        scheduler.register_worker(1, 10).await;
        let dequeued = scheduler.dequeue(1).await;
        assert!(dequeued.is_some());
        assert_eq!(dequeued.unwrap().id, task_id);
    }
    
    #[tokio::test]
    async fn test_priority_ordering() {
        let scheduler = Scheduler::new();
        
        // Submit low priority first
        let low_task = Task::new(0, "tenant-1".into(), Priority::Low, vec![]);
        scheduler.submit(low_task).await.unwrap();
        
        // Submit high priority second
        let high_task = Task::new(0, "tenant-1".into(), Priority::High, vec![]);
        scheduler.submit(high_task).await.unwrap();
        
        scheduler.register_worker(1, 10).await;
        
        // High priority should be dequeued first
        let first = scheduler.dequeue(1).await.unwrap();
        assert_eq!(first.priority, Priority::High);
        
        let second = scheduler.dequeue(1).await.unwrap();
        assert_eq!(second.priority, Priority::Low);
    }
    
    #[tokio::test]
    async fn test_tenant_quota() {
        let scheduler = Scheduler::new();
        
        scheduler.set_tenant_quota("limited".into(), TenantQuota {
            max_pending: 2,
            max_rate_per_sec: 100,
            ..Default::default()
        }).await;
        
        // First two should succeed
        let t1 = Task::new(0, "limited".into(), Priority::Normal, vec![]);
        let t2 = Task::new(0, "limited".into(), Priority::Normal, vec![]);
        let t3 = Task::new(0, "limited".into(), Priority::Normal, vec![]);
        
        assert!(scheduler.submit(t1).await.is_ok());
        assert!(scheduler.submit(t2).await.is_ok());
        
        // Third should fail
        assert!(scheduler.submit(t3).await.is_err());
    }
}
