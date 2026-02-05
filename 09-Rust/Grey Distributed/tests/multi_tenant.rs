//! # Multi-Tenant Isolation Integration Tests
//!
//! Validates tenant isolation in a shared cluster:
//! 1. Quota enforcement
//! 2. Fair scheduling across tenants
//! 3. Resource isolation
//! 4. Noisy neighbor prevention
//!
//! ## Why This Test Matters
//!
//! Multi-tenancy is critical for shared infrastructure. Without proper
//! isolation, one tenant can starve others or access their data.

use std::collections::HashMap;
use std::sync::{Arc, Mutex, atomic::{AtomicU64, Ordering}};
use std::time::{Duration, Instant};

// ============================================================================
// Mock Types
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TenantId(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TaskId(u64);

#[derive(Debug, Clone)]
struct TenantQuota {
    tenant_id: TenantId,
    max_cpu_cores: u32,
    max_memory_mb: u64,
    max_concurrent_tasks: u32,
    max_tasks_per_second: u32,
    weight: u32, // For weighted fair scheduling
}

impl TenantQuota {
    fn new(tenant_id: u64) -> Self {
        Self {
            tenant_id: TenantId(tenant_id),
            max_cpu_cores: 100,        // High default to not interfere with task limits
            max_memory_mb: 100000,     // High default to not interfere with task limits
            max_concurrent_tasks: 100,
            max_tasks_per_second: 1000, // High default to not interfere in tests
            weight: 1,
        }
    }

    fn with_weight(mut self, weight: u32) -> Self {
        self.weight = weight;
        self
    }

    fn with_max_tasks(mut self, max: u32) -> Self {
        self.max_concurrent_tasks = max;
        self
    }

    fn with_rate_limit(mut self, rate: u32) -> Self {
        self.max_tasks_per_second = rate;
        self
    }

    fn with_cpu(mut self, cores: u32) -> Self {
        self.max_cpu_cores = cores;
        self
    }

    fn with_memory(mut self, mb: u64) -> Self {
        self.max_memory_mb = mb;
        self
    }
}

#[derive(Debug, Clone)]
struct TenantMetrics {
    tenant_id: TenantId,
    used_cpu_cores: u32,
    used_memory_mb: u64,
    active_tasks: u32,
    tasks_this_second: u32,
    total_tasks: u64,
    rejected_tasks: u64,
}

impl TenantMetrics {
    fn new(tenant_id: TenantId) -> Self {
        Self {
            tenant_id,
            used_cpu_cores: 0,
            used_memory_mb: 0,
            active_tasks: 0,
            tasks_this_second: 0,
            total_tasks: 0,
            rejected_tasks: 0,
        }
    }
}

struct Task {
    id: TaskId,
    tenant_id: TenantId,
    cpu_cores: u32,
    memory_mb: u64,
}

impl Task {
    fn new(id: u64, tenant_id: u64) -> Self {
        Self {
            id: TaskId(id),
            tenant_id: TenantId(tenant_id),
            cpu_cores: 1,
            memory_mb: 512,
        }
    }

    fn with_resources(mut self, cpu: u32, memory: u64) -> Self {
        self.cpu_cores = cpu;
        self.memory_mb = memory;
        self
    }
}

struct QuotaManager {
    quotas: Mutex<HashMap<TenantId, TenantQuota>>,
    metrics: Mutex<HashMap<TenantId, TenantMetrics>>,
}

impl QuotaManager {
    fn new() -> Self {
        Self {
            quotas: Mutex::new(HashMap::new()),
            metrics: Mutex::new(HashMap::new()),
        }
    }

    fn set_quota(&self, quota: TenantQuota) {
        let id = quota.tenant_id;
        self.quotas.lock().unwrap().insert(id, quota);
        self.metrics.lock().unwrap().insert(id, TenantMetrics::new(id));
    }

    fn can_admit(&self, task: &Task) -> bool {
        let quotas = self.quotas.lock().unwrap();
        let metrics = self.metrics.lock().unwrap();
        
        let quota = match quotas.get(&task.tenant_id) {
            Some(q) => q,
            None => return false,
        };
        
        let metric = match metrics.get(&task.tenant_id) {
            Some(m) => m,
            None => return false,
        };
        
        // Check all quota limits
        metric.active_tasks < quota.max_concurrent_tasks
            && metric.used_cpu_cores + task.cpu_cores <= quota.max_cpu_cores
            && metric.used_memory_mb + task.memory_mb <= quota.max_memory_mb
            && metric.tasks_this_second < quota.max_tasks_per_second
    }

    fn admit_task(&self, task: &Task) -> bool {
        if !self.can_admit(task) {
            let mut metrics = self.metrics.lock().unwrap();
            if let Some(m) = metrics.get_mut(&task.tenant_id) {
                m.rejected_tasks += 1;
            }
            return false;
        }
        
        let mut metrics = self.metrics.lock().unwrap();
        if let Some(m) = metrics.get_mut(&task.tenant_id) {
            m.active_tasks += 1;
            m.used_cpu_cores += task.cpu_cores;
            m.used_memory_mb += task.memory_mb;
            m.tasks_this_second += 1;
            m.total_tasks += 1;
        }
        true
    }

    fn complete_task(&self, task: &Task) {
        let mut metrics = self.metrics.lock().unwrap();
        if let Some(m) = metrics.get_mut(&task.tenant_id) {
            m.active_tasks = m.active_tasks.saturating_sub(1);
            m.used_cpu_cores = m.used_cpu_cores.saturating_sub(task.cpu_cores);
            m.used_memory_mb = m.used_memory_mb.saturating_sub(task.memory_mb);
        }
    }

    fn get_metrics(&self, tenant_id: TenantId) -> Option<TenantMetrics> {
        self.metrics.lock().unwrap().get(&tenant_id).cloned()
    }

    fn get_weight(&self, tenant_id: TenantId) -> u32 {
        self.quotas.lock().unwrap()
            .get(&tenant_id)
            .map(|q| q.weight)
            .unwrap_or(1)
    }

    fn reset_rate_limits(&self) {
        let mut metrics = self.metrics.lock().unwrap();
        for m in metrics.values_mut() {
            m.tasks_this_second = 0;
        }
    }
}

struct FairScheduler {
    quota_manager: Arc<QuotaManager>,
    next_task_id: AtomicU64,
    scheduled_per_tenant: Mutex<HashMap<TenantId, u64>>,
}

impl FairScheduler {
    fn new(quota_manager: Arc<QuotaManager>) -> Self {
        Self {
            quota_manager,
            next_task_id: AtomicU64::new(0),
            scheduled_per_tenant: Mutex::new(HashMap::new()),
        }
    }

    fn submit(&self, tenant_id: u64) -> Option<TaskId> {
        let task = Task::new(
            self.next_task_id.fetch_add(1, Ordering::SeqCst),
            tenant_id,
        );
        
        if self.quota_manager.admit_task(&task) {
            let mut scheduled = self.scheduled_per_tenant.lock().unwrap();
            *scheduled.entry(TenantId(tenant_id)).or_insert(0) += 1;
            Some(task.id)
        } else {
            None
        }
    }

    fn scheduled_for_tenant(&self, tenant_id: u64) -> u64 {
        self.scheduled_per_tenant.lock().unwrap()
            .get(&TenantId(tenant_id))
            .copied()
            .unwrap_or(0)
    }

    /// Weighted fair scheduling: schedule proportionally to weights
    fn schedule_weighted(&self, tenant_requests: &[(u64, u64)]) -> HashMap<u64, u64> {
        let mut results = HashMap::new();
        
        // Calculate total weight
        let total_weight: u32 = tenant_requests.iter()
            .map(|(tid, _)| self.quota_manager.get_weight(TenantId(*tid)))
            .sum();
        
        if total_weight == 0 {
            return results;
        }
        
        // Schedule proportionally
        for (tenant_id, requested) in tenant_requests {
            let weight = self.quota_manager.get_weight(TenantId(*tenant_id));
            let share = (*requested as u32 * weight / total_weight.max(1)) as u64;
            
            let mut admitted = 0u64;
            for _ in 0..share {
                if self.submit(*tenant_id).is_some() {
                    admitted += 1;
                }
            }
            results.insert(*tenant_id, admitted);
        }
        
        results
    }
}

// ============================================================================
// Test Fixtures
// ============================================================================

struct MultiTenantCluster {
    quota_manager: Arc<QuotaManager>,
    scheduler: Arc<FairScheduler>,
}

impl MultiTenantCluster {
    fn new() -> Self {
        let quota_manager = Arc::new(QuotaManager::new());
        let scheduler = Arc::new(FairScheduler::new(quota_manager.clone()));
        Self { quota_manager, scheduler }
    }

    fn add_tenant(&self, tenant_id: u64, quota: TenantQuota) {
        self.quota_manager.set_quota(quota);
    }
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn test_quota_enforcement() {
    let cluster = MultiTenantCluster::new();
    cluster.add_tenant(1, TenantQuota::new(1).with_max_tasks(3));
    
    // Submit up to limit
    assert!(cluster.scheduler.submit(1).is_some());
    assert!(cluster.scheduler.submit(1).is_some());
    assert!(cluster.scheduler.submit(1).is_some());
    
    // Fourth should be rejected
    assert!(cluster.scheduler.submit(1).is_none());
    
    let metrics = cluster.quota_manager.get_metrics(TenantId(1)).unwrap();
    assert_eq!(metrics.active_tasks, 3);
    assert_eq!(metrics.rejected_tasks, 1);
}

#[test]
fn test_fair_scheduling() {
    let cluster = MultiTenantCluster::new();
    
    // Two tenants with same weight
    cluster.add_tenant(1, TenantQuota::new(1).with_max_tasks(100));
    cluster.add_tenant(2, TenantQuota::new(2).with_max_tasks(100));
    
    // Submit equal tasks
    for _ in 0..50 {
        cluster.scheduler.submit(1);
        cluster.scheduler.submit(2);
    }
    
    // Both should have similar counts
    let t1 = cluster.scheduler.scheduled_for_tenant(1);
    let t2 = cluster.scheduler.scheduled_for_tenant(2);
    assert_eq!(t1, t2);
}

#[test]
fn test_weighted_fair_scheduling() {
    let cluster = MultiTenantCluster::new();
    
    // Tenant 1: weight 1, Tenant 2: weight 3
    cluster.add_tenant(1, TenantQuota::new(1).with_weight(1).with_max_tasks(100));
    cluster.add_tenant(2, TenantQuota::new(2).with_weight(3).with_max_tasks(100));
    
    // Submit via weighted scheduler
    let requests = vec![(1, 40), (2, 40)];
    let results = cluster.scheduler.schedule_weighted(&requests);
    
    // Tenant 2 should get more (3x weight)
    let t1 = results.get(&1).copied().unwrap_or(0);
    let t2 = results.get(&2).copied().unwrap_or(0);
    assert!(t2 > t1, "Higher weight tenant should get more tasks");
}

#[test]
fn test_resource_isolation() {
    let cluster = MultiTenantCluster::new();
    
    // Tenant 1: limited resources
    cluster.add_tenant(1, TenantQuota::new(1));
    cluster.add_tenant(2, TenantQuota::new(2));
    
    // Tenant 1 exhausts quota
    for _ in 0..100 {
        cluster.scheduler.submit(1);
    }
    
    // Tenant 2 should still be able to submit
    assert!(cluster.scheduler.submit(2).is_some());
}

#[test]
fn test_noisy_neighbor_prevention() {
    let cluster = MultiTenantCluster::new();
    
    // Small limit to simulate constrained resources
    cluster.add_tenant(1, TenantQuota::new(1).with_max_tasks(5));
    cluster.add_tenant(2, TenantQuota::new(2).with_max_tasks(5));
    
    // Tenant 1 tries to flood
    for _ in 0..100 {
        cluster.scheduler.submit(1);
    }
    
    // Tenant 1 should be limited
    let m1 = cluster.quota_manager.get_metrics(TenantId(1)).unwrap();
    assert_eq!(m1.active_tasks, 5);
    assert!(m1.rejected_tasks > 0);
    
    // Tenant 2 should be unaffected
    for _ in 0..5 {
        assert!(cluster.scheduler.submit(2).is_some());
    }
}

#[test]
fn test_rate_limiting() {
    let cluster = MultiTenantCluster::new();
    
    let mut quota = TenantQuota::new(1);
    quota.max_tasks_per_second = 5;
    quota.max_concurrent_tasks = 100;
    cluster.add_tenant(1, quota);
    
    // Submit up to rate limit
    for _ in 0..5 {
        assert!(cluster.scheduler.submit(1).is_some());
    }
    
    // Should be rate limited
    assert!(cluster.scheduler.submit(1).is_none());
    
    // Reset rate limits (simulating new second)
    cluster.quota_manager.reset_rate_limits();
    
    // Should be able to submit again
    assert!(cluster.scheduler.submit(1).is_some());
}

#[test]
fn test_task_completion_frees_quota() {
    let cluster = MultiTenantCluster::new();
    cluster.add_tenant(1, TenantQuota::new(1).with_max_tasks(2));
    
    // Submit to limit
    let t1 = Task::new(1, 1);
    let t2 = Task::new(2, 1);
    assert!(cluster.quota_manager.admit_task(&t1));
    assert!(cluster.quota_manager.admit_task(&t2));
    
    // At limit
    assert!(!cluster.quota_manager.can_admit(&Task::new(3, 1)));
    
    // Complete one
    cluster.quota_manager.complete_task(&t1);
    
    // Can now admit
    assert!(cluster.quota_manager.can_admit(&Task::new(3, 1)));
}

#[test]
fn test_memory_quota() {
    let cluster = MultiTenantCluster::new();
    
    let mut quota = TenantQuota::new(1);
    quota.max_memory_mb = 1024;
    quota.max_concurrent_tasks = 100;
    cluster.add_tenant(1, quota);
    
    // Submit tasks that fit
    let t1 = Task::new(1, 1).with_resources(1, 512);
    let t2 = Task::new(2, 1).with_resources(1, 512);
    assert!(cluster.quota_manager.admit_task(&t1));
    assert!(cluster.quota_manager.admit_task(&t2));
    
    // Next would exceed memory
    let t3 = Task::new(3, 1).with_resources(1, 512);
    assert!(!cluster.quota_manager.can_admit(&t3));
}

#[test]
fn test_cpu_quota() {
    let cluster = MultiTenantCluster::new();
    
    let mut quota = TenantQuota::new(1);
    quota.max_cpu_cores = 4;
    quota.max_concurrent_tasks = 100;
    quota.max_memory_mb = 100000;
    cluster.add_tenant(1, quota);
    
    // Submit tasks
    let t1 = Task::new(1, 1).with_resources(2, 100);
    let t2 = Task::new(2, 1).with_resources(2, 100);
    assert!(cluster.quota_manager.admit_task(&t1));
    assert!(cluster.quota_manager.admit_task(&t2));
    
    // Next would exceed CPU
    let t3 = Task::new(3, 1).with_resources(1, 100);
    assert!(!cluster.quota_manager.can_admit(&t3));
}

#[test]
fn test_many_tenants() {
    let cluster = MultiTenantCluster::new();
    
    // Add 100 tenants
    for i in 1..=100 {
        cluster.add_tenant(i, TenantQuota::new(i).with_max_tasks(10));
    }
    
    // Each tenant submits tasks
    for i in 1..=100 {
        for _ in 0..5 {
            cluster.scheduler.submit(i);
        }
    }
    
    // Verify each tenant has 5 tasks
    for i in 1..=100 {
        let scheduled = cluster.scheduler.scheduled_for_tenant(i);
        assert_eq!(scheduled, 5, "Tenant {} should have 5 tasks", i);
    }
}

#[test]
fn test_scheduling_performance() {
    let cluster = MultiTenantCluster::new();
    
    for i in 1..=10 {
        cluster.add_tenant(i, TenantQuota::new(i).with_max_tasks(1000));
    }
    
    let start = Instant::now();
    for i in 1..=10 {
        for _ in 0..100 {
            cluster.scheduler.submit(i);
        }
    }
    let elapsed = start.elapsed();
    
    assert!(elapsed < Duration::from_millis(100), "Scheduling too slow: {:?}", elapsed);
}
