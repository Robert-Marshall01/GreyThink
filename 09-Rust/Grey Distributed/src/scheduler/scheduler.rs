//! # Core Scheduler
//!
//! Multi-level feedback queue scheduler with work stealing.
//!
//! ## Key Design Decisions
//!
//! 1. **Multi-Level Feedback Queue (MLFQ)**: Tasks start at high priority
//!    and get demoted if they consume too much CPU. This balances
//!    interactive responsiveness with batch throughput.
//!
//! 2. **Work Stealing**: Idle workers steal from busy workers' queues.
//!    This provides automatic load balancing without central coordination.
//!
//! 3. **Per-Tenant Queues**: Each tenant has its own queue to prevent
//!    noisy neighbor problems. Scheduling between tenants uses fair-share.
//!
//! ## Invariants
//!
//! 1. A task is in exactly one queue at a time
//! 2. Higher priority queues are always drained first
//! 3. Work stealing only steals from the tail (LIFO theft = FIFO execution)

use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::cmp::Ordering;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering as AtomicOrdering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};

use crate::types::{NodeId, TenantId};

// ============================================================================
// Task Definition
// ============================================================================

/// Unique identifier for a task
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TaskId(pub u64);

/// A schedulable unit of work
#[derive(Debug, Clone)]
pub struct Task {
    /// Unique task identifier
    pub id: TaskId,
    /// Tenant that owns this task
    pub tenant_id: TenantId,
    /// Current priority level
    pub priority: Priority,
    /// Task payload (serialized)
    pub payload: Vec<u8>,
    /// When task was submitted
    pub submitted_at: Instant,
    /// Optional deadline
    pub deadline: Option<Instant>,
    /// Retry state
    pub retries: u32,
    /// Maximum allowed retries
    pub max_retries: u32,
    /// Resource requirements
    pub resources: ResourceRequirements,
    /// Task state
    pub state: TaskState,
}

/// Resource requirements for a task
#[derive(Debug, Clone, Default)]
pub struct ResourceRequirements {
    /// Memory in bytes
    pub memory_bytes: u64,
    /// CPU units (1000 = 1 core)
    pub cpu_units: u32,
    /// I/O bandwidth in bytes/sec
    pub io_bandwidth: u64,
    /// Affinity to specific nodes
    pub node_affinity: Option<Vec<NodeId>>,
}

/// Task state machine
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TaskState {
    /// Waiting to be scheduled
    Pending,
    /// Assigned to a worker
    Assigned { worker_id: NodeId, assigned_at: Instant },
    /// Currently executing
    Running { worker_id: NodeId, started_at: Instant },
    /// Completed successfully
    Completed { result: Vec<u8>, completed_at: Instant },
    /// Failed
    Failed { error: String, failed_at: Instant },
    /// Cancelled by user
    Cancelled { cancelled_at: Instant },
}

/// Priority levels for MLFQ
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Priority {
    /// Highest priority (interactive)
    Level0,
    /// High priority
    Level1,
    /// Medium priority
    Level2,
    /// Low priority (batch)
    Level3,
    /// Lowest priority (background)
    Level4,
}

impl Priority {
    /// Time quantum for this priority level
    ///
    /// Higher priorities get shorter quanta to ensure responsiveness.
    /// Lower priorities get longer quanta for efficiency.
    pub fn time_quantum(&self) -> Duration {
        match self {
            Priority::Level0 => Duration::from_millis(1),
            Priority::Level1 => Duration::from_millis(5),
            Priority::Level2 => Duration::from_millis(20),
            Priority::Level3 => Duration::from_millis(100),
            Priority::Level4 => Duration::from_millis(500),
        }
    }

    /// Demote to lower priority
    pub fn demote(self) -> Self {
        match self {
            Priority::Level0 => Priority::Level1,
            Priority::Level1 => Priority::Level2,
            Priority::Level2 => Priority::Level3,
            Priority::Level3 => Priority::Level4,
            Priority::Level4 => Priority::Level4,
        }
    }

    /// Promote to higher priority
    pub fn promote(self) -> Self {
        match self {
            Priority::Level0 => Priority::Level0,
            Priority::Level1 => Priority::Level0,
            Priority::Level2 => Priority::Level1,
            Priority::Level3 => Priority::Level2,
            Priority::Level4 => Priority::Level3,
        }
    }

    pub fn as_usize(&self) -> usize {
        match self {
            Priority::Level0 => 0,
            Priority::Level1 => 1,
            Priority::Level2 => 2,
            Priority::Level3 => 3,
            Priority::Level4 => 4,
        }
    }
}

// ============================================================================
// Multi-Level Feedback Queue
// ============================================================================

/// Multi-level feedback queue for a single tenant
pub struct TenantQueue {
    /// Tenant identifier
    tenant_id: TenantId,
    /// Queues by priority level
    queues: [RwLock<VecDeque<Task>>; 5],
    /// Total tasks across all queues
    total_tasks: AtomicUsize,
    /// Tasks completed (for fair-share tracking)
    tasks_completed: AtomicU64,
    /// CPU time consumed (for fair-share tracking)
    cpu_time_consumed: AtomicU64,
}

impl TenantQueue {
    pub fn new(tenant_id: TenantId) -> Self {
        Self {
            tenant_id,
            queues: [
                RwLock::new(VecDeque::new()),
                RwLock::new(VecDeque::new()),
                RwLock::new(VecDeque::new()),
                RwLock::new(VecDeque::new()),
                RwLock::new(VecDeque::new()),
            ],
            total_tasks: AtomicUsize::new(0),
            tasks_completed: AtomicU64::new(0),
            cpu_time_consumed: AtomicU64::new(0),
        }
    }

    /// Enqueue a task
    pub fn enqueue(&self, task: Task) {
        let priority = task.priority.as_usize();
        self.queues[priority].write().push_back(task);
        self.total_tasks.fetch_add(1, AtomicOrdering::SeqCst);
    }

    /// Dequeue highest priority task
    pub fn dequeue(&self) -> Option<Task> {
        // Check queues from highest to lowest priority
        for queue in &self.queues {
            if let Some(task) = queue.write().pop_front() {
                self.total_tasks.fetch_sub(1, AtomicOrdering::SeqCst);
                return Some(task);
            }
        }
        None
    }

    /// Steal a task (from tail for work stealing)
    pub fn steal(&self) -> Option<Task> {
        // Steal from lowest priority queues first
        for queue in self.queues.iter().rev() {
            if let Some(task) = queue.write().pop_back() {
                self.total_tasks.fetch_sub(1, AtomicOrdering::SeqCst);
                return Some(task);
            }
        }
        None
    }

    /// Re-enqueue a task (possibly at different priority)
    pub fn requeue(&self, task: Task) {
        self.enqueue(task);
    }

    /// Get total pending tasks
    pub fn pending_count(&self) -> usize {
        self.total_tasks.load(AtomicOrdering::SeqCst)
    }

    /// Record task completion
    pub fn record_completion(&self, cpu_time_us: u64) {
        self.tasks_completed.fetch_add(1, AtomicOrdering::SeqCst);
        self.cpu_time_consumed.fetch_add(cpu_time_us, AtomicOrdering::SeqCst);
    }

    /// Get CPU time consumed
    pub fn cpu_time_consumed(&self) -> u64 {
        self.cpu_time_consumed.load(AtomicOrdering::SeqCst)
    }
}

// ============================================================================
// Global Scheduler
// ============================================================================

/// Configuration for the scheduler
#[derive(Debug, Clone)]
pub struct SchedulerConfig {
    /// Maximum queue depth per tenant
    pub max_queue_depth: usize,
    /// Enable work stealing
    pub work_stealing_enabled: bool,
    /// Number of work stealing attempts before giving up
    pub work_stealing_attempts: usize,
    /// Interval to boost starved tasks
    pub starvation_boost_interval: Duration,
    /// Maximum tasks in flight per worker
    pub max_inflight_per_worker: usize,
}

impl Default for SchedulerConfig {
    fn default() -> Self {
        Self {
            max_queue_depth: 10_000,
            work_stealing_enabled: true,
            work_stealing_attempts: 3,
            starvation_boost_interval: Duration::from_secs(10),
            max_inflight_per_worker: 100,
        }
    }
}

/// Global task scheduler
pub struct Scheduler {
    /// Configuration
    config: SchedulerConfig,
    /// Per-tenant queues
    tenant_queues: RwLock<HashMap<TenantId, Arc<TenantQueue>>>,
    /// All tasks by ID
    all_tasks: RwLock<HashMap<TaskId, Task>>,
    /// Next task ID
    next_task_id: AtomicU64,
    /// Workers
    workers: RwLock<HashMap<NodeId, WorkerState>>,
    /// Last starvation check
    last_starvation_check: Mutex<Instant>,
}

/// State of a worker node
#[derive(Debug, Clone)]
pub struct WorkerState {
    /// Worker node ID
    pub node_id: NodeId,
    /// Available resources
    pub available_resources: ResourceRequirements,
    /// Tasks currently assigned
    pub assigned_tasks: Vec<TaskId>,
    /// Last heartbeat
    pub last_heartbeat: Instant,
    /// Current load (0-100)
    pub load: u32,
}

impl Scheduler {
    pub fn new(config: SchedulerConfig) -> Self {
        Self {
            config,
            tenant_queues: RwLock::new(HashMap::new()),
            all_tasks: RwLock::new(HashMap::new()),
            next_task_id: AtomicU64::new(1),
            workers: RwLock::new(HashMap::new()),
            last_starvation_check: Mutex::new(Instant::now()),
        }
    }

    /// Submit a new task
    pub fn submit(&self, tenant_id: TenantId, payload: Vec<u8>) -> Result<TaskId, super::SchedulerError> {
        let task_id = TaskId(self.next_task_id.fetch_add(1, AtomicOrdering::SeqCst));

        let task = Task {
            id: task_id,
            tenant_id,
            priority: Priority::Level1, // Start at high priority
            payload,
            submitted_at: Instant::now(),
            deadline: None,
            retries: 0,
            max_retries: 3,
            resources: ResourceRequirements::default(),
            state: TaskState::Pending,
        };

        // Get or create tenant queue
        let queue = self.get_or_create_tenant_queue(tenant_id);

        // Check queue depth
        if queue.pending_count() >= self.config.max_queue_depth {
            return Err(super::SchedulerError::QueueFull);
        }

        // Store task
        self.all_tasks.write().insert(task_id, task.clone());

        // Enqueue
        queue.enqueue(task);

        Ok(task_id)
    }

    /// Schedule tasks to workers
    ///
    /// Returns assignments: (worker_id, task_id)
    pub fn schedule(&self) -> Vec<(NodeId, TaskId)> {
        let mut assignments = Vec::new();
        let workers = self.workers.read();
        let tenant_queues = self.tenant_queues.read();

        // For each available worker
        for (worker_id, worker) in workers.iter() {
            if worker.assigned_tasks.len() >= self.config.max_inflight_per_worker {
                continue;
            }

            // Try to get a task for this worker
            // Use fair-share to pick tenant
            if let Some(tenant_id) = self.select_tenant(&tenant_queues) {
                if let Some(queue) = tenant_queues.get(&tenant_id) {
                    if let Some(mut task) = queue.dequeue() {
                        // Check resource requirements
                        if self.can_assign(&task, worker) {
                            task.state = TaskState::Assigned {
                                worker_id: *worker_id,
                                assigned_at: Instant::now(),
                            };
                            assignments.push((*worker_id, task.id));
                            self.all_tasks.write().insert(task.id, task);
                        } else {
                            // Re-queue if resources don't match
                            queue.requeue(task);
                        }
                    }
                }
            }
        }

        assignments
    }

    /// Schedule with work stealing
    pub fn schedule_with_stealing(&self, worker_id: NodeId) -> Option<TaskId> {
        if !self.config.work_stealing_enabled {
            return None;
        }

        let tenant_queues = self.tenant_queues.read();
        let mut candidates: Vec<_> = tenant_queues.values().collect();

        // Sort by queue length (steal from busiest)
        candidates.sort_by_key(|q| std::cmp::Reverse(q.pending_count()));

        for _ in 0..self.config.work_stealing_attempts {
            for queue in &candidates {
                if let Some(task) = queue.steal() {
                    let mut tasks = self.all_tasks.write();
                    let mut stored_task = task.clone();
                    stored_task.state = TaskState::Assigned {
                        worker_id,
                        assigned_at: Instant::now(),
                    };
                    tasks.insert(stored_task.id, stored_task);
                    return Some(task.id);
                }
            }
        }

        None
    }

    /// Handle task completion
    pub fn complete_task(
        &self,
        task_id: TaskId,
        result: Vec<u8>,
        cpu_time_us: u64,
    ) -> Result<(), super::SchedulerError> {
        let mut tasks = self.all_tasks.write();
        let task = tasks
            .get_mut(&task_id)
            .ok_or(super::SchedulerError::TaskNotFound(task_id.0))?;

        task.state = TaskState::Completed {
            result,
            completed_at: Instant::now(),
        };

        // Update tenant stats
        if let Some(queue) = self.tenant_queues.read().get(&task.tenant_id) {
            queue.record_completion(cpu_time_us);
        }

        Ok(())
    }

    /// Handle task failure
    pub fn fail_task(&self, task_id: TaskId, error: String) -> Result<bool, super::SchedulerError> {
        let mut tasks = self.all_tasks.write();
        let task = tasks
            .get_mut(&task_id)
            .ok_or(super::SchedulerError::TaskNotFound(task_id.0))?;

        task.retries += 1;

        if task.retries >= task.max_retries {
            task.state = TaskState::Failed {
                error,
                failed_at: Instant::now(),
            };
            return Ok(false); // No retry
        }

        // Requeue for retry with demoted priority
        task.priority = task.priority.demote();
        task.state = TaskState::Pending;

        let task_clone = task.clone();
        drop(tasks);

        if let Some(queue) = self.tenant_queues.read().get(&task_clone.tenant_id) {
            queue.requeue(task_clone);
        }

        Ok(true) // Will retry
    }

    /// Cancel a task
    pub fn cancel_task(&self, task_id: TaskId) -> Result<(), super::SchedulerError> {
        let mut tasks = self.all_tasks.write();
        let task = tasks
            .get_mut(&task_id)
            .ok_or(super::SchedulerError::TaskNotFound(task_id.0))?;

        task.state = TaskState::Cancelled {
            cancelled_at: Instant::now(),
        };

        Ok(())
    }

    /// Register a worker
    pub fn register_worker(&self, node_id: NodeId, resources: ResourceRequirements) {
        self.workers.write().insert(
            node_id,
            WorkerState {
                node_id,
                available_resources: resources,
                assigned_tasks: Vec::new(),
                last_heartbeat: Instant::now(),
                load: 0,
            },
        );
    }

    /// Update worker heartbeat
    pub fn worker_heartbeat(&self, node_id: NodeId, load: u32) {
        if let Some(worker) = self.workers.write().get_mut(&node_id) {
            worker.last_heartbeat = Instant::now();
            worker.load = load;
        }
    }

    /// Boost priority of starved tasks
    pub fn boost_starved_tasks(&self) {
        let mut last_check = self.last_starvation_check.lock();
        let now = Instant::now();

        if now.duration_since(*last_check) < self.config.starvation_boost_interval {
            return;
        }

        *last_check = now;

        // Find tasks that have been waiting too long and boost them
        let tenant_queues = self.tenant_queues.read();
        for queue in tenant_queues.values() {
            // Temporarily collect tasks, boost, and requeue
            // In production, you'd want a more efficient implementation
            for priority_idx in 1..5 {
                let mut queue_locked = queue.queues[priority_idx].write();
                for task in queue_locked.iter_mut() {
                    if now.duration_since(task.submitted_at) > self.config.starvation_boost_interval {
                        task.priority = task.priority.promote();
                    }
                }
            }
        }
    }

    /// Get task state
    pub fn get_task(&self, task_id: TaskId) -> Option<Task> {
        self.all_tasks.read().get(&task_id).cloned()
    }

    fn get_or_create_tenant_queue(&self, tenant_id: TenantId) -> Arc<TenantQueue> {
        let queues = self.tenant_queues.read();
        if let Some(queue) = queues.get(&tenant_id) {
            return Arc::clone(queue);
        }
        drop(queues);

        let queue = Arc::new(TenantQueue::new(tenant_id));
        self.tenant_queues.write().insert(tenant_id, Arc::clone(&queue));
        queue
    }

    fn select_tenant(&self, tenant_queues: &HashMap<TenantId, Arc<TenantQueue>>) -> Option<TenantId> {
        // Simple round-robin for now
        // In production, use fair-share based on weights and consumption
        tenant_queues
            .iter()
            .filter(|(_, q)| q.pending_count() > 0)
            .min_by_key(|(_, q)| q.cpu_time_consumed())
            .map(|(id, _)| *id)
    }

    fn can_assign(&self, task: &Task, worker: &WorkerState) -> bool {
        // Check node affinity
        if let Some(ref affinity) = task.resources.node_affinity {
            if !affinity.contains(&worker.node_id) {
                return false;
            }
        }

        // Check resources (simplified)
        task.resources.memory_bytes <= worker.available_resources.memory_bytes
            && task.resources.cpu_units <= worker.available_resources.cpu_units
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_priority_demotion() {
        assert_eq!(Priority::Level0.demote(), Priority::Level1);
        assert_eq!(Priority::Level4.demote(), Priority::Level4);
    }

    #[test]
    fn test_tenant_queue() {
        let queue = TenantQueue::new(TenantId(1));

        let task = Task {
            id: TaskId(1),
            tenant_id: TenantId(1),
            priority: Priority::Level0,
            payload: Vec::new(),
            submitted_at: Instant::now(),
            deadline: None,
            retries: 0,
            max_retries: 3,
            resources: ResourceRequirements::default(),
            state: TaskState::Pending,
        };

        queue.enqueue(task);
        assert_eq!(queue.pending_count(), 1);

        let dequeued = queue.dequeue();
        assert!(dequeued.is_some());
        assert_eq!(queue.pending_count(), 0);
    }

    #[test]
    fn test_scheduler_submit() {
        let scheduler = Scheduler::new(SchedulerConfig::default());

        let task_id = scheduler
            .submit(TenantId(1), b"test payload".to_vec())
            .unwrap();

        let task = scheduler.get_task(task_id).unwrap();
        assert!(matches!(task.state, TaskState::Pending));
    }

    #[test]
    fn test_work_stealing() {
        let queue = TenantQueue::new(TenantId(1));

        // Add tasks at different priorities
        for i in 0..5 {
            let task = Task {
                id: TaskId(i),
                tenant_id: TenantId(1),
                priority: Priority::Level2,
                payload: Vec::new(),
                submitted_at: Instant::now(),
                deadline: None,
                retries: 0,
                max_retries: 3,
                resources: ResourceRequirements::default(),
                state: TaskState::Pending,
            };
            queue.enqueue(task);
        }

        // Steal should take from tail
        let stolen = queue.steal().unwrap();
        assert_eq!(stolen.id.0, 4); // Last enqueued
    }
}
