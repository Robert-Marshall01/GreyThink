//! # Quota Management
//!
//! Multi-tenant resource quota allocation and enforcement.
//!
//! ## Design Principles
//!
//! Quotas provide hard boundaries for resource consumption:
//! - **Fairness**: Each tenant gets guaranteed minimum resources
//! - **Isolation**: One tenant cannot consume another's allocation
//! - **Elasticity**: Unused quota can be borrowed (with limits)
//! - **Observability**: All quota usage is tracked and reported
//!
//! ## Quota Types
//!
//! 1. **Hard Quotas**: Cannot be exceeded under any circumstances
//! 2. **Soft Quotas**: Can be exceeded briefly, generates alerts
//! 3. **Burst Quotas**: Temporary allowance above normal limits
//! 4. **Reserved Quotas**: Guaranteed minimums

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};

use super::GovernanceError;

// ============================================================================
// Quota Types
// ============================================================================

/// Unique tenant identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TenantId(pub u64);

/// Resource types that can be quota'd
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResourceType {
    /// CPU in millicores (1000 = 1 core)
    Cpu,
    /// Memory in bytes
    Memory,
    /// Storage in bytes
    Storage,
    /// Network bandwidth in bytes/second
    NetworkBandwidth,
    /// Request rate (requests per second)
    RequestRate,
    /// Connection count
    Connections,
    /// Task concurrency
    TaskConcurrency,
    /// Custom resource
    Custom(u32),
}

/// Quota specification for a resource
#[derive(Debug, Clone)]
pub struct QuotaSpec {
    /// Resource type
    pub resource: ResourceType,
    
    /// Hard limit (cannot exceed)
    pub hard_limit: u64,
    
    /// Soft limit (triggers warning)
    pub soft_limit: u64,
    
    /// Reserved/guaranteed minimum
    pub reserved: u64,
    
    /// Burst capacity (temporary overage allowed)
    pub burst_limit: Option<u64>,
    
    /// Burst window duration
    pub burst_window: Duration,
    
    /// Enforcement mode
    pub enforcement: EnforcementMode,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EnforcementMode {
    /// Immediately reject requests that exceed quota
    Hard,
    /// Allow but throttle/queue requests
    Throttle,
    /// Allow but generate alerts
    Soft,
    /// Disabled, only track usage
    Monitor,
}

/// Tenant quota configuration
#[derive(Debug, Clone)]
pub struct TenantQuota {
    /// Tenant identifier
    pub tenant_id: TenantId,
    
    /// Resource quotas
    pub quotas: HashMap<ResourceType, QuotaSpec>,
    
    /// Priority tier (affects burst borrowing)
    pub priority_tier: PriorityTier,
    
    /// Whether this tenant can borrow unused capacity
    pub can_burst: bool,
    
    /// Created timestamp
    pub created_at: Instant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PriorityTier {
    /// Lowest priority, first to be throttled
    Free = 0,
    /// Standard priority
    Standard = 1,
    /// Higher priority, more burst capacity
    Premium = 2,
    /// Highest priority, dedicated resources
    Enterprise = 3,
}

// ============================================================================
// Quota Usage Tracking
// ============================================================================

/// Real-time quota usage for a tenant
#[derive(Debug)]
pub struct QuotaUsage {
    /// Current usage per resource
    usage: HashMap<ResourceType, AtomicU64>,
    
    /// Peak usage per resource (for analytics)
    peak_usage: HashMap<ResourceType, AtomicU64>,
    
    /// Burst usage tracking
    burst_tracker: Mutex<BurstTracker>,
    
    /// Last updated timestamp
    last_updated: Mutex<Instant>,
}

#[derive(Debug)]
struct BurstTracker {
    /// Burst start time
    burst_start: Option<Instant>,
    /// Accumulated burst usage
    burst_usage: u64,
}

impl QuotaUsage {
    pub fn new() -> Self {
        Self {
            usage: HashMap::new(),
            peak_usage: HashMap::new(),
            burst_tracker: Mutex::new(BurstTracker {
                burst_start: None,
                burst_usage: 0,
            }),
            last_updated: Mutex::new(Instant::now()),
        }
    }
    
    /// Get current usage for a resource
    pub fn get(&self, resource: &ResourceType) -> u64 {
        self.usage
            .get(resource)
            .map(|v| v.load(Ordering::Relaxed))
            .unwrap_or(0)
    }
    
    /// Add usage for a resource
    pub fn add(&self, resource: ResourceType, amount: u64) -> u64 {
        let usage = self.usage
            .get(&resource)
            .map(|v| v.fetch_add(amount, Ordering::Relaxed) + amount)
            .unwrap_or(amount);
        
        // Update peak
        if let Some(peak) = self.peak_usage.get(&resource) {
            let current_peak = peak.load(Ordering::Relaxed);
            if usage > current_peak {
                peak.store(usage, Ordering::Relaxed);
            }
        }
        
        *self.last_updated.lock() = Instant::now();
        usage
    }
    
    /// Release usage for a resource
    pub fn release(&self, resource: &ResourceType, amount: u64) {
        if let Some(v) = self.usage.get(resource) {
            v.fetch_sub(amount.min(v.load(Ordering::Relaxed)), Ordering::Relaxed);
        }
        *self.last_updated.lock() = Instant::now();
    }
    
    /// Get all current usage
    pub fn all(&self) -> HashMap<ResourceType, u64> {
        self.usage
            .iter()
            .map(|(k, v)| (*k, v.load(Ordering::Relaxed)))
            .collect()
    }
}

// ============================================================================
// Quota Manager
// ============================================================================

/// Central quota management
///
/// Responsibilities:
/// - Register and configure tenant quotas
/// - Track resource usage in real-time
/// - Enforce quota limits
/// - Handle burst capacity
/// - Report quota violations
pub struct QuotaManager {
    /// Tenant quota configurations
    tenants: RwLock<HashMap<TenantId, TenantQuota>>,
    
    /// Real-time usage per tenant
    usage: RwLock<HashMap<TenantId, Arc<QuotaUsage>>>,
    
    /// Global resource pools for burst sharing
    global_pools: RwLock<HashMap<ResourceType, ResourcePool>>,
    
    /// Quota violation callbacks
    violation_handlers: RwLock<Vec<Box<dyn QuotaViolationHandler + Send + Sync>>>,
}

/// Pool of resources available for burst borrowing
#[derive(Debug)]
struct ResourcePool {
    /// Total pool capacity
    total: u64,
    /// Currently used from pool
    used: AtomicU64,
}

impl QuotaManager {
    pub fn new() -> Self {
        Self {
            tenants: RwLock::new(HashMap::new()),
            usage: RwLock::new(HashMap::new()),
            global_pools: RwLock::new(HashMap::new()),
            violation_handlers: RwLock::new(Vec::new()),
        }
    }
    
    /// Register a new tenant with quotas
    pub fn register_tenant(&self, quota: TenantQuota) -> Result<(), GovernanceError> {
        // Validate quotas
        for (_, spec) in &quota.quotas {
            if spec.hard_limit < spec.reserved {
                return Err(GovernanceError::InvalidQuotaConfig(
                    "hard_limit must be >= reserved".to_string(),
                ));
            }
            if spec.soft_limit > spec.hard_limit {
                return Err(GovernanceError::InvalidQuotaConfig(
                    "soft_limit must be <= hard_limit".to_string(),
                ));
            }
        }
        
        let tenant_id = quota.tenant_id;
        
        self.tenants.write().insert(tenant_id, quota);
        self.usage.write().insert(tenant_id, Arc::new(QuotaUsage::new()));
        
        Ok(())
    }
    
    /// Remove a tenant
    pub fn remove_tenant(&self, tenant_id: TenantId) {
        self.tenants.write().remove(&tenant_id);
        self.usage.write().remove(&tenant_id);
    }
    
    /// Request resource allocation
    ///
    /// Returns:
    /// - `Ok(())` if allocation succeeds
    /// - `Err(QuotaExceeded)` if quota exceeded and enforcement is hard
    /// - May queue/throttle based on enforcement mode
    pub fn allocate(
        &self,
        tenant_id: TenantId,
        resource: ResourceType,
        amount: u64,
    ) -> Result<AllocationHandle, GovernanceError> {
        let tenants = self.tenants.read();
        let tenant = tenants
            .get(&tenant_id)
            .ok_or(GovernanceError::TenantNotFound(tenant_id.0))?;
        
        let spec = tenant
            .quotas
            .get(&resource)
            .ok_or(GovernanceError::QuotaExceeded {
                tenant_id: tenant_id.0,
                resource: format!("{:?} not configured", resource),
            })?;
        
        let usage = self.usage.read();
        let tenant_usage = usage
            .get(&tenant_id)
            .ok_or(GovernanceError::TenantNotFound(tenant_id.0))?;
        
        let current = tenant_usage.get(&resource);
        let new_usage = current + amount;
        
        // Check against hard limit
        if new_usage > spec.hard_limit {
            // Check burst capacity
            if let Some(burst_limit) = spec.burst_limit {
                if new_usage <= spec.hard_limit + burst_limit {
                    // Allow burst
                    tenant_usage.add(resource, amount);
                    return Ok(AllocationHandle {
                        tenant_id,
                        resource,
                        amount,
                        is_burst: true,
                    });
                }
            }
            
            match spec.enforcement {
                EnforcementMode::Hard => {
                    self.notify_violation(tenant_id, resource, new_usage, spec.hard_limit);
                    return Err(GovernanceError::QuotaExceeded {
                        tenant_id: tenant_id.0,
                        resource: format!("{:?}", resource),
                    });
                }
                EnforcementMode::Throttle => {
                    // Would implement queuing here
                    return Err(GovernanceError::Throttled {
                        reason: format!("Quota exceeded for {:?}", resource),
                    });
                }
                EnforcementMode::Soft | EnforcementMode::Monitor => {
                    // Allow but notify
                    self.notify_violation(tenant_id, resource, new_usage, spec.hard_limit);
                }
            }
        }
        
        // Check soft limit for warning
        if new_usage > spec.soft_limit {
            self.notify_soft_limit(tenant_id, resource, new_usage, spec.soft_limit);
        }
        
        // Allocate
        tenant_usage.add(resource, amount);
        
        Ok(AllocationHandle {
            tenant_id,
            resource,
            amount,
            is_burst: false,
        })
    }
    
    /// Release previously allocated resources
    pub fn release(&self, handle: AllocationHandle) {
        if let Some(usage) = self.usage.read().get(&handle.tenant_id) {
            usage.release(&handle.resource, handle.amount);
        }
    }
    
    /// Get current usage for a tenant
    pub fn get_usage(&self, tenant_id: TenantId) -> Option<HashMap<ResourceType, u64>> {
        self.usage.read().get(&tenant_id).map(|u| u.all())
    }
    
    /// Get quota utilization percentage
    pub fn get_utilization(&self, tenant_id: TenantId, resource: ResourceType) -> Option<f64> {
        let tenants = self.tenants.read();
        let usage = self.usage.read();
        
        let tenant = tenants.get(&tenant_id)?;
        let spec = tenant.quotas.get(&resource)?;
        let current = usage.get(&tenant_id)?.get(&resource);
        
        Some(current as f64 / spec.hard_limit as f64 * 100.0)
    }
    
    /// Update quota for a tenant
    pub fn update_quota(
        &self,
        tenant_id: TenantId,
        resource: ResourceType,
        spec: QuotaSpec,
    ) -> Result<(), GovernanceError> {
        let mut tenants = self.tenants.write();
        let tenant = tenants
            .get_mut(&tenant_id)
            .ok_or(GovernanceError::TenantNotFound(tenant_id.0))?;
        
        tenant.quotas.insert(resource, spec);
        Ok(())
    }
    
    /// Register a violation handler
    pub fn on_violation<H: QuotaViolationHandler + Send + Sync + 'static>(&self, handler: H) {
        self.violation_handlers.write().push(Box::new(handler));
    }
    
    fn notify_violation(
        &self,
        tenant_id: TenantId,
        resource: ResourceType,
        current: u64,
        limit: u64,
    ) {
        let event = QuotaViolationEvent {
            tenant_id,
            resource,
            current_usage: current,
            limit,
            violation_type: ViolationType::HardLimit,
            timestamp: Instant::now(),
        };
        
        for handler in self.violation_handlers.read().iter() {
            handler.on_violation(&event);
        }
    }
    
    fn notify_soft_limit(
        &self,
        tenant_id: TenantId,
        resource: ResourceType,
        current: u64,
        limit: u64,
    ) {
        let event = QuotaViolationEvent {
            tenant_id,
            resource,
            current_usage: current,
            limit,
            violation_type: ViolationType::SoftLimit,
            timestamp: Instant::now(),
        };
        
        for handler in self.violation_handlers.read().iter() {
            handler.on_violation(&event);
        }
    }
}

// ============================================================================
// Allocation Handle
// ============================================================================

/// Handle representing an active resource allocation
///
/// When dropped, the allocation is NOT automatically released.
/// Use QuotaManager::release() explicitly for RAII-style management.
#[derive(Debug)]
pub struct AllocationHandle {
    pub tenant_id: TenantId,
    pub resource: ResourceType,
    pub amount: u64,
    pub is_burst: bool,
}

/// RAII guard that releases allocation on drop
pub struct AllocationGuard {
    handle: Option<AllocationHandle>,
    manager: Arc<QuotaManager>,
}

impl AllocationGuard {
    pub fn new(handle: AllocationHandle, manager: Arc<QuotaManager>) -> Self {
        Self {
            handle: Some(handle),
            manager,
        }
    }
    
    /// Take ownership of handle without releasing
    pub fn take(mut self) -> AllocationHandle {
        self.handle.take().unwrap()
    }
}

impl Drop for AllocationGuard {
    fn drop(&mut self) {
        if let Some(handle) = self.handle.take() {
            self.manager.release(handle);
        }
    }
}

// ============================================================================
// Quota Violation Handling
// ============================================================================

#[derive(Debug, Clone)]
pub struct QuotaViolationEvent {
    pub tenant_id: TenantId,
    pub resource: ResourceType,
    pub current_usage: u64,
    pub limit: u64,
    pub violation_type: ViolationType,
    pub timestamp: Instant,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ViolationType {
    SoftLimit,
    HardLimit,
    BurstExhausted,
}

/// Handler for quota violations
pub trait QuotaViolationHandler: Send + Sync {
    fn on_violation(&self, event: &QuotaViolationEvent);
}

/// Logging violation handler
pub struct LoggingViolationHandler;

impl QuotaViolationHandler for LoggingViolationHandler {
    fn on_violation(&self, event: &QuotaViolationEvent) {
        match event.violation_type {
            ViolationType::SoftLimit => {
                tracing::warn!(
                    tenant_id = event.tenant_id.0,
                    resource = ?event.resource,
                    current = event.current_usage,
                    limit = event.limit,
                    "Soft quota limit exceeded"
                );
            }
            ViolationType::HardLimit => {
                tracing::error!(
                    tenant_id = event.tenant_id.0,
                    resource = ?event.resource,
                    current = event.current_usage,
                    limit = event.limit,
                    "Hard quota limit exceeded"
                );
            }
            ViolationType::BurstExhausted => {
                tracing::warn!(
                    tenant_id = event.tenant_id.0,
                    resource = ?event.resource,
                    "Burst capacity exhausted"
                );
            }
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_test_quota() -> TenantQuota {
        let mut quotas = HashMap::new();
        quotas.insert(
            ResourceType::Cpu,
            QuotaSpec {
                resource: ResourceType::Cpu,
                hard_limit: 1000,
                soft_limit: 800,
                reserved: 100,
                burst_limit: Some(200),
                burst_window: Duration::from_secs(60),
                enforcement: EnforcementMode::Hard,
            },
        );
        
        TenantQuota {
            tenant_id: TenantId(1),
            quotas,
            priority_tier: PriorityTier::Standard,
            can_burst: true,
            created_at: Instant::now(),
        }
    }
    
    #[test]
    fn test_quota_allocation() {
        let manager = QuotaManager::new();
        manager.register_tenant(create_test_quota()).unwrap();
        
        // Allocate within quota
        let handle = manager
            .allocate(TenantId(1), ResourceType::Cpu, 500)
            .unwrap();
        assert!(!handle.is_burst);
        
        // Check usage
        let usage = manager.get_usage(TenantId(1)).unwrap();
        assert_eq!(usage.get(&ResourceType::Cpu), Some(&500));
    }
    
    #[test]
    fn test_quota_exceeded() {
        let manager = QuotaManager::new();
        manager.register_tenant(create_test_quota()).unwrap();
        
        // Allocate near limit
        let _ = manager.allocate(TenantId(1), ResourceType::Cpu, 950);
        
        // Try to exceed without burst
        let result = manager.allocate(TenantId(1), ResourceType::Cpu, 300);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_burst_allocation() {
        let manager = QuotaManager::new();
        manager.register_tenant(create_test_quota()).unwrap();
        
        // Allocate to hard limit
        let _ = manager.allocate(TenantId(1), ResourceType::Cpu, 1000);
        
        // Allocate burst (within burst limit of 200)
        let result = manager.allocate(TenantId(1), ResourceType::Cpu, 100);
        assert!(result.is_ok());
        assert!(result.unwrap().is_burst);
    }
}
