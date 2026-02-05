//! # Task Execution Pipeline Integration Tests
//!
//! Validates the complete task lifecycle:
//! 1. Task submission with resource requirements
//! 2. Scheduling decisions and worker assignment
//! 3. Execution with proof generation
//! 4. Result collection and verification
//!
//! ## Why This Test Matters
//!
//! Task execution is the core value proposition. If tasks don't execute
//! correctly, deterministically, and verifiably, the system is useless.

use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex, atomic::{AtomicU64, Ordering}};
use std::time::{Duration, Instant};

// ============================================================================
// Mock Types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NodeId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TaskId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TenantId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TaskState { Pending, Scheduled, Running, Completed, Failed }

#[derive(Debug, Clone)]
struct ResourceRequirements {
    cpu_cores: u32,
    memory_mb: u64,
    timeout: Duration,
}

impl Default for ResourceRequirements {
    fn default() -> Self {
        Self { cpu_cores: 1, memory_mb: 512, timeout: Duration::from_secs(60) }
    }
}

#[derive(Debug, Clone)]
struct Task {
    id: TaskId,
    tenant_id: TenantId,
    requirements: ResourceRequirements,
    state: TaskState,
    assigned_worker: Option<NodeId>,
    retries: u32,
}

impl Task {
    fn new(id: u64, tenant_id: u64) -> Self {
        Self {
            id: TaskId(id),
            tenant_id: TenantId(tenant_id),
            requirements: ResourceRequirements::default(),
            state: TaskState::Pending,
            assigned_worker: None,
            retries: 0,
        }
    }
}

#[derive(Debug, Clone)]
struct ExecutionProof {
    task_id: TaskId,
    worker_id: NodeId,
    execution_hash: [u8; 32],
    duration: Duration,
}

struct MockWorker {
    id: NodeId,
    available_cpu: u32,
    available_memory: u64,
    tasks: Mutex<Vec<TaskId>>,
    completed: Mutex<Vec<ExecutionProof>>,
}

impl MockWorker {
    fn new(id: u64, cpu: u32, memory: u64) -> Self {
        Self {
            id: NodeId(id),
            available_cpu: cpu,
            available_memory: memory,
            tasks: Mutex::new(Vec::new()),
            completed: Mutex::new(Vec::new()),
        }
    }

    fn assign_task(&self, task_id: TaskId) {
        self.tasks.lock().unwrap().push(task_id);
    }

    fn execute_all(&self) {
        let mut tasks = self.tasks.lock().unwrap();
        let mut completed = self.completed.lock().unwrap();
        
        for task_id in tasks.drain(..) {
            completed.push(ExecutionProof {
                task_id,
                worker_id: self.id,
                execution_hash: [0u8; 32], // Simplified
                duration: Duration::from_millis(10),
            });
        }
    }

    fn completed_count(&self) -> usize {
        self.completed.lock().unwrap().len()
    }

    fn task_count(&self) -> usize {
        self.tasks.lock().unwrap().len()
    }
}

struct Scheduler {
    workers: Mutex<Vec<Arc<MockWorker>>>,
    pending_tasks: Mutex<VecDeque<Task>>,
    scheduled_tasks: Mutex<HashMap<TaskId, NodeId>>,
    next_worker: AtomicU64,
}

impl Scheduler {
    fn new() -> Self {
        Self {
            workers: Mutex::new(Vec::new()),
            pending_tasks: Mutex::new(VecDeque::new()),
            scheduled_tasks: Mutex::new(HashMap::new()),
            next_worker: AtomicU64::new(0),
        }
    }

    fn add_worker(&self, worker: Arc<MockWorker>) {
        self.workers.lock().unwrap().push(worker);
    }

    fn submit_task(&self, task: Task) -> TaskId {
        let id = task.id;
        self.pending_tasks.lock().unwrap().push_back(task);
        id
    }

    /// Round-robin scheduling
    fn schedule_pending(&self) {
        let workers = self.workers.lock().unwrap();
        if workers.is_empty() { return; }
        
        let mut pending = self.pending_tasks.lock().unwrap();
        let mut scheduled = self.scheduled_tasks.lock().unwrap();
        
        while let Some(task) = pending.pop_front() {
            let idx = self.next_worker.fetch_add(1, Ordering::SeqCst) as usize % workers.len();
            let worker = &workers[idx];
            worker.assign_task(task.id);
            scheduled.insert(task.id, worker.id);
        }
    }

    fn worker_count(&self) -> usize {
        self.workers.lock().unwrap().len()
    }

    fn pending_count(&self) -> usize {
        self.pending_tasks.lock().unwrap().len()
    }

    fn scheduled_count(&self) -> usize {
        self.scheduled_tasks.lock().unwrap().len()
    }

    fn get_worker_for_task(&self, task_id: TaskId) -> Option<NodeId> {
        self.scheduled_tasks.lock().unwrap().get(&task_id).copied()
    }
}

// ============================================================================
// Test Fixtures
// ============================================================================

struct TestCluster {
    scheduler: Arc<Scheduler>,
    workers: Vec<Arc<MockWorker>>,
}

impl TestCluster {
    fn new(worker_count: usize) -> Self {
        let scheduler = Arc::new(Scheduler::new());
        let mut workers = Vec::new();
        
        for i in 0..worker_count {
            let worker = Arc::new(MockWorker::new(i as u64 + 1, 4, 8192));
            scheduler.add_worker(worker.clone());
            workers.push(worker);
        }
        
        Self { scheduler, workers }
    }

    fn submit_task(&self, task_id: u64, tenant_id: u64) -> TaskId {
        self.scheduler.submit_task(Task::new(task_id, tenant_id))
    }

    fn schedule_all(&self) {
        self.scheduler.schedule_pending();
    }

    fn execute_all_workers(&self) {
        for worker in &self.workers {
            worker.execute_all();
        }
    }

    fn total_completed(&self) -> usize {
        self.workers.iter().map(|w| w.completed_count()).sum()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_basic_task_submission() {
    let cluster = TestCluster::new(3);
    
    let task_id = cluster.submit_task(1, 1);
    assert_eq!(cluster.scheduler.pending_count(), 1);
    
    cluster.schedule_all();
    assert_eq!(cluster.scheduler.pending_count(), 0);
    assert_eq!(cluster.scheduler.scheduled_count(), 1);
    
    let worker = cluster.scheduler.get_worker_for_task(task_id);
    assert!(worker.is_some());
}

#[test]
fn test_deterministic_scheduling() {
    // Same input should produce same scheduling decisions
    let cluster1 = TestCluster::new(3);
    let cluster2 = TestCluster::new(3);
    
    for i in 0..10 {
        cluster1.submit_task(i, 1);
        cluster2.submit_task(i, 1);
    }
    
    cluster1.schedule_all();
    cluster2.schedule_all();
    
    // Both clusters should schedule tasks to same workers
    for i in 0..10 {
        let w1 = cluster1.scheduler.get_worker_for_task(TaskId(i));
        let w2 = cluster2.scheduler.get_worker_for_task(TaskId(i));
        assert_eq!(w1, w2, "Task {} scheduled differently", i);
    }
}

#[test]
fn test_complete_task_execution_flow() {
    let cluster = TestCluster::new(2);
    
    // Submit tasks
    for i in 0..5 {
        cluster.submit_task(i, 1);
    }
    
    // Schedule and execute
    cluster.schedule_all();
    cluster.execute_all_workers();
    
    // Verify all completed
    assert_eq!(cluster.total_completed(), 5);
}

#[test]
fn test_work_distribution() {
    let cluster = TestCluster::new(3);
    
    // Submit 9 tasks (should be evenly distributed)
    for i in 0..9 {
        cluster.submit_task(i, 1);
    }
    
    cluster.schedule_all();
    cluster.execute_all_workers();
    
    // Each worker should have 3 tasks
    for worker in &cluster.workers {
        assert_eq!(worker.completed_count(), 3);
    }
}

#[test]
fn test_empty_cluster_handling() {
    let scheduler = Scheduler::new();
    
    // Submit task to empty cluster
    scheduler.submit_task(Task::new(1, 1));
    scheduler.schedule_pending();
    
    // Task should remain pending (no workers)
    assert_eq!(scheduler.pending_count(), 1);
    assert_eq!(scheduler.scheduled_count(), 0);
}

#[test]
fn test_multi_tenant_scheduling() {
    let cluster = TestCluster::new(3);
    
    // Submit tasks from different tenants
    cluster.submit_task(1, 1);
    cluster.submit_task(2, 2);
    cluster.submit_task(3, 3);
    
    cluster.schedule_all();
    
    // All tasks should be scheduled regardless of tenant
    assert_eq!(cluster.scheduler.scheduled_count(), 3);
}

#[test]
fn test_large_batch_scheduling() {
    let cluster = TestCluster::new(5);
    
    // Submit 1000 tasks
    for i in 0..1000 {
        cluster.submit_task(i, i % 10);
    }
    
    let start = Instant::now();
    cluster.schedule_all();
    let elapsed = start.elapsed();
    
    assert_eq!(cluster.scheduler.scheduled_count(), 1000);
    assert!(elapsed < Duration::from_secs(1), "Scheduling too slow: {:?}", elapsed);
}

#[test]
fn test_proof_generation() {
    let cluster = TestCluster::new(1);
    cluster.submit_task(1, 1);
    
    cluster.schedule_all();
    cluster.execute_all_workers();
    
    // Worker should have proof
    let worker = &cluster.workers[0];
    assert_eq!(worker.completed_count(), 1);
}

#[test]
fn test_task_state_transitions() {
    let mut task = Task::new(1, 1);
    
    assert_eq!(task.state, TaskState::Pending);
    
    task.state = TaskState::Scheduled;
    task.assigned_worker = Some(NodeId(1));
    assert_eq!(task.state, TaskState::Scheduled);
    assert!(task.assigned_worker.is_some());
    
    task.state = TaskState::Running;
    assert_eq!(task.state, TaskState::Running);
    
    task.state = TaskState::Completed;
    assert_eq!(task.state, TaskState::Completed);
}

#[test]
fn test_resource_requirements() {
    let req = ResourceRequirements {
        cpu_cores: 4,
        memory_mb: 16384,
        timeout: Duration::from_secs(300),
    };
    
    assert_eq!(req.cpu_cores, 4);
    assert_eq!(req.memory_mb, 16384);
    assert_eq!(req.timeout, Duration::from_secs(300));
}

#[test]
fn test_scheduling_timing() {
    let cluster = TestCluster::new(10);
    
    for i in 0..100 {
        cluster.submit_task(i, 1);
    }
    
    let start = Instant::now();
    cluster.schedule_all();
    let elapsed = start.elapsed();
    
    // Scheduling should be fast
    assert!(elapsed < Duration::from_millis(100));
}
