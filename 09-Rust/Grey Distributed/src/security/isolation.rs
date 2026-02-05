//! # Security Isolation
//!
//! Isolation mechanisms to limit blast radius of failures.
//!
//! ## Philosophy
//!
//! Defense in depth: multiple layers of isolation:
//! - Process isolation (separate processes per tenant)
//! - Resource isolation (cgroups, namespaces)
//! - Network isolation (firewalls, policies)
//! - Memory isolation (sandboxing)
//!
//! ## Sandboxing
//!
//! Untrusted code runs in sandboxed environments:
//! - Limited syscalls (seccomp)
//! - Limited filesystem access
//! - Limited network access
//! - Memory and CPU limits
//!
//! ## Multi-tenancy
//!
//! Complete isolation between tenants:
//! - Separate encryption keys per tenant
//! - Separate quota tracking
//! - No cross-tenant data access

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};

// ============================================================================
// Tenant Isolation
// ============================================================================

/// Unique tenant identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TenantId(String);

impl TenantId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
    
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for TenantId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Tenant isolation boundary
#[derive(Debug, Clone)]
pub struct TenantBoundary {
    /// Tenant ID
    pub tenant_id: TenantId,
    
    /// Allowed operations
    pub permissions: HashSet<Permission>,
    
    /// Resource limits
    pub limits: ResourceLimits,
    
    /// Encryption key (for tenant data)
    pub encryption_key_id: Option<String>,
    
    /// Network policies
    pub network_policies: Vec<NetworkPolicy>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Permission {
    Read,
    Write,
    Delete,
    Admin,
    CreateSubtenant,
    ManageKeys,
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct ResourceLimits {
    /// Maximum CPU (millicores)
    pub cpu_millis: u64,
    
    /// Maximum memory (bytes)
    pub memory_bytes: u64,
    
    /// Maximum storage (bytes)
    pub storage_bytes: u64,
    
    /// Maximum requests per second
    pub requests_per_second: u64,
    
    /// Maximum concurrent connections
    pub max_connections: u64,
}

impl Default for ResourceLimits {
    fn default() -> Self {
        Self {
            cpu_millis: 1000,      // 1 CPU
            memory_bytes: 1 << 30, // 1 GB
            storage_bytes: 10 << 30, // 10 GB
            requests_per_second: 1000,
            max_connections: 100,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NetworkPolicy {
    /// Policy name
    pub name: String,
    
    /// Direction
    pub direction: NetworkDirection,
    
    /// Action
    pub action: NetworkAction,
    
    /// Source/destination CIDR
    pub cidr: String,
    
    /// Ports
    pub ports: Vec<u16>,
}

#[derive(Debug, Clone, Copy)]
pub enum NetworkDirection {
    Ingress,
    Egress,
    Both,
}

#[derive(Debug, Clone, Copy)]
pub enum NetworkAction {
    Allow,
    Deny,
}

// ============================================================================
// Tenant Manager
// ============================================================================

/// Manages tenant isolation boundaries
pub struct TenantManager {
    /// Registered tenants
    tenants: RwLock<HashMap<TenantId, TenantBoundary>>,
    
    /// Default limits for new tenants
    default_limits: ResourceLimits,
    
    /// Current resource usage
    usage: RwLock<HashMap<TenantId, ResourceUsage>>,
}

#[derive(Debug, Clone, Default)]
pub struct ResourceUsage {
    /// Current CPU usage (millicores)
    pub cpu_millis: u64,
    
    /// Current memory usage (bytes)
    pub memory_bytes: u64,
    
    /// Current storage usage (bytes)
    pub storage_bytes: u64,
    
    /// Requests in current second
    pub requests_current: u64,
    
    /// Active connections
    pub connections: u64,
}

impl TenantManager {
    pub fn new(default_limits: ResourceLimits) -> Self {
        Self {
            tenants: RwLock::new(HashMap::new()),
            default_limits,
            usage: RwLock::new(HashMap::new()),
        }
    }
    
    /// Register a new tenant
    pub fn register_tenant(
        &self,
        tenant_id: TenantId,
        permissions: HashSet<Permission>,
        limits: Option<ResourceLimits>,
    ) {
        let boundary = TenantBoundary {
            tenant_id: tenant_id.clone(),
            permissions,
            limits: limits.unwrap_or_else(|| self.default_limits.clone()),
            encryption_key_id: None,
            network_policies: Vec::new(),
        };
        
        self.tenants.write().insert(tenant_id.clone(), boundary);
        self.usage.write().insert(tenant_id, ResourceUsage::default());
    }
    
    /// Check if operation is allowed
    pub fn check_permission(
        &self,
        tenant_id: &TenantId,
        permission: &Permission,
    ) -> bool {
        self.tenants.read()
            .get(tenant_id)
            .map(|b| b.permissions.contains(permission))
            .unwrap_or(false)
    }
    
    /// Check resource limits
    pub fn check_limits(
        &self,
        tenant_id: &TenantId,
        request: &ResourceRequest,
    ) -> LimitCheckResult {
        let tenants = self.tenants.read();
        let usage = self.usage.read();
        
        let Some(boundary) = tenants.get(tenant_id) else {
            return LimitCheckResult::TenantNotFound;
        };
        
        let Some(current) = usage.get(tenant_id) else {
            return LimitCheckResult::TenantNotFound;
        };
        
        // Check CPU
        if let Some(cpu) = request.cpu_millis {
            if current.cpu_millis + cpu > boundary.limits.cpu_millis {
                return LimitCheckResult::Exceeded(LimitType::Cpu);
            }
        }
        
        // Check memory
        if let Some(mem) = request.memory_bytes {
            if current.memory_bytes + mem > boundary.limits.memory_bytes {
                return LimitCheckResult::Exceeded(LimitType::Memory);
            }
        }
        
        // Check request rate
        if request.is_request {
            if current.requests_current >= boundary.limits.requests_per_second {
                return LimitCheckResult::Exceeded(LimitType::RequestRate);
            }
        }
        
        LimitCheckResult::Allowed
    }
    
    /// Update resource usage
    pub fn update_usage(&self, tenant_id: &TenantId, delta: ResourceDelta) {
        if let Some(usage) = self.usage.write().get_mut(tenant_id) {
            match delta {
                ResourceDelta::Cpu(d) => {
                    usage.cpu_millis = (usage.cpu_millis as i64 + d).max(0) as u64;
                }
                ResourceDelta::Memory(d) => {
                    usage.memory_bytes = (usage.memory_bytes as i64 + d).max(0) as u64;
                }
                ResourceDelta::Storage(d) => {
                    usage.storage_bytes = (usage.storage_bytes as i64 + d).max(0) as u64;
                }
                ResourceDelta::Request => {
                    usage.requests_current += 1;
                }
                ResourceDelta::Connection(d) => {
                    usage.connections = (usage.connections as i64 + d).max(0) as u64;
                }
            }
        }
    }
    
    /// Get tenant boundary
    pub fn get_boundary(&self, tenant_id: &TenantId) -> Option<TenantBoundary> {
        self.tenants.read().get(tenant_id).cloned()
    }
    
    /// Get resource usage
    pub fn get_usage(&self, tenant_id: &TenantId) -> Option<ResourceUsage> {
        self.usage.read().get(tenant_id).cloned()
    }
}

#[derive(Debug, Clone, Default)]
pub struct ResourceRequest {
    pub cpu_millis: Option<u64>,
    pub memory_bytes: Option<u64>,
    pub storage_bytes: Option<u64>,
    pub is_request: bool,
}

#[derive(Debug, Clone)]
pub enum ResourceDelta {
    Cpu(i64),
    Memory(i64),
    Storage(i64),
    Request,
    Connection(i64),
}

#[derive(Debug, Clone)]
pub enum LimitCheckResult {
    Allowed,
    Exceeded(LimitType),
    TenantNotFound,
}

#[derive(Debug, Clone, Copy)]
pub enum LimitType {
    Cpu,
    Memory,
    Storage,
    RequestRate,
    Connections,
}

// ============================================================================
// Sandbox
// ============================================================================

/// Sandboxed execution environment
pub struct Sandbox {
    /// Sandbox ID
    id: u64,
    
    /// Owning tenant
    tenant_id: TenantId,
    
    /// State
    state: RwLock<SandboxState>,
    
    /// Resource limits
    limits: ResourceLimits,
    
    /// Allowed syscalls
    allowed_syscalls: HashSet<u32>,
    
    /// Allowed filesystem paths
    allowed_paths: HashSet<String>,
    
    /// Network restrictions
    network_restricted: AtomicBool,
    
    /// Metrics
    metrics: SandboxMetrics,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SandboxState {
    Created,
    Running,
    Paused,
    Terminated,
}

#[derive(Default)]
struct SandboxMetrics {
    syscalls_allowed: AtomicU64,
    syscalls_blocked: AtomicU64,
    violations: AtomicU64,
}

impl Sandbox {
    pub fn new(
        tenant_id: TenantId,
        limits: ResourceLimits,
    ) -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        
        Self {
            id: COUNTER.fetch_add(1, Ordering::SeqCst),
            tenant_id,
            state: RwLock::new(SandboxState::Created),
            limits,
            allowed_syscalls: default_allowed_syscalls(),
            allowed_paths: HashSet::new(),
            network_restricted: AtomicBool::new(true),
            metrics: SandboxMetrics::default(),
        }
    }
    
    /// Get sandbox ID
    pub fn id(&self) -> u64 {
        self.id
    }
    
    /// Start the sandbox
    pub fn start(&self) -> Result<(), SandboxError> {
        let mut state = self.state.write();
        match *state {
            SandboxState::Created | SandboxState::Paused => {
                *state = SandboxState::Running;
                Ok(())
            }
            SandboxState::Running => Ok(()),
            SandboxState::Terminated => Err(SandboxError::AlreadyTerminated),
        }
    }
    
    /// Pause the sandbox
    pub fn pause(&self) -> Result<(), SandboxError> {
        let mut state = self.state.write();
        if *state == SandboxState::Running {
            *state = SandboxState::Paused;
            Ok(())
        } else {
            Err(SandboxError::NotRunning)
        }
    }
    
    /// Terminate the sandbox
    pub fn terminate(&self) {
        *self.state.write() = SandboxState::Terminated;
    }
    
    /// Check if syscall is allowed
    ///
    /// # Syscall Filtering
    /// 
    /// We use a whitelist approach - only explicitly allowed
    /// syscalls can be made. This is more secure than a blacklist.
    pub fn check_syscall(&self, syscall_nr: u32) -> bool {
        if self.allowed_syscalls.contains(&syscall_nr) {
            self.metrics.syscalls_allowed.fetch_add(1, Ordering::Relaxed);
            true
        } else {
            self.metrics.syscalls_blocked.fetch_add(1, Ordering::Relaxed);
            self.metrics.violations.fetch_add(1, Ordering::Relaxed);
            false
        }
    }
    
    /// Check if filesystem path is allowed
    pub fn check_path(&self, path: &str) -> bool {
        // Check if path or any parent is allowed
        for allowed in &self.allowed_paths {
            if path.starts_with(allowed) {
                return true;
            }
        }
        self.metrics.violations.fetch_add(1, Ordering::Relaxed);
        false
    }
    
    /// Allow additional path
    pub fn allow_path(&mut self, path: impl Into<String>) {
        self.allowed_paths.insert(path.into());
    }
    
    /// Allow network access
    pub fn allow_network(&self) {
        self.network_restricted.store(false, Ordering::SeqCst);
    }
    
    /// Check if network is allowed
    pub fn network_allowed(&self) -> bool {
        !self.network_restricted.load(Ordering::Relaxed)
    }
    
    /// Get violation count
    pub fn violation_count(&self) -> u64 {
        self.metrics.violations.load(Ordering::Relaxed)
    }
    
    /// Get current state
    pub fn state(&self) -> SandboxState {
        *self.state.read()
    }
}

/// Default safe syscalls
fn default_allowed_syscalls() -> HashSet<u32> {
    let mut syscalls = HashSet::new();
    // Read/write (to allowed fds)
    syscalls.insert(0);  // read
    syscalls.insert(1);  // write
    // Memory management
    syscalls.insert(9);  // mmap
    syscalls.insert(11); // munmap
    syscalls.insert(12); // brk
    // Exit
    syscalls.insert(60); // exit
    syscalls.insert(231); // exit_group
    
    syscalls
}

#[derive(Debug)]
pub enum SandboxError {
    AlreadyTerminated,
    NotRunning,
    ResourceExhausted,
    ViolationLimit,
}

// ============================================================================
// Process Isolation
// ============================================================================

/// Manages isolated processes
pub struct ProcessIsolator {
    /// Active processes
    processes: RwLock<HashMap<u64, IsolatedProcess>>,
    
    /// Process counter
    counter: AtomicU64,
    
    /// Default cgroup settings
    default_cgroup: CgroupSettings,
}

#[derive(Debug, Clone)]
pub struct IsolatedProcess {
    /// Process ID (internal)
    pub id: u64,
    
    /// Owning tenant
    pub tenant_id: TenantId,
    
    /// OS process ID
    pub pid: Option<u32>,
    
    /// Cgroup path
    pub cgroup_path: String,
    
    /// Network namespace
    pub network_namespace: Option<String>,
    
    /// Creation time
    pub created_at: Instant,
    
    /// Resource limits applied
    pub limits: ResourceLimits,
}

#[derive(Debug, Clone)]
pub struct CgroupSettings {
    /// CPU quota (microseconds per period)
    pub cpu_quota: u64,
    
    /// CPU period (microseconds)
    pub cpu_period: u64,
    
    /// Memory limit (bytes)
    pub memory_limit: u64,
    
    /// Memory swap limit (bytes)
    pub memory_swap_limit: u64,
    
    /// IO weight (1-10000)
    pub io_weight: u32,
}

impl Default for CgroupSettings {
    fn default() -> Self {
        Self {
            cpu_quota: 100_000,    // 100ms
            cpu_period: 100_000,   // 100ms (= 1 CPU)
            memory_limit: 1 << 30, // 1 GB
            memory_swap_limit: 0,  // No swap
            io_weight: 100,
        }
    }
}

impl ProcessIsolator {
    pub fn new(default_cgroup: CgroupSettings) -> Self {
        Self {
            processes: RwLock::new(HashMap::new()),
            counter: AtomicU64::new(0),
            default_cgroup,
        }
    }
    
    /// Create an isolated process
    ///
    /// # What This Does
    ///
    /// 1. Creates cgroup for resource limits
    /// 2. Creates network namespace for isolation
    /// 3. Applies seccomp filters
    /// 4. Starts process in isolated environment
    pub fn create_isolated(
        &self,
        tenant_id: TenantId,
        limits: ResourceLimits,
    ) -> Result<IsolatedProcess, IsolationError> {
        let id = self.counter.fetch_add(1, Ordering::SeqCst);
        
        let process = IsolatedProcess {
            id,
            tenant_id: tenant_id.clone(),
            pid: None, // Would be set when process actually starts
            cgroup_path: format!("/sys/fs/cgroup/grey/{}/{}", tenant_id.as_str(), id),
            network_namespace: Some(format!("grey-{}-{}", tenant_id.as_str(), id)),
            created_at: Instant::now(),
            limits,
        };
        
        // Would actually create cgroup, namespace, etc. here
        
        self.processes.write().insert(id, process.clone());
        
        Ok(process)
    }
    
    /// Terminate an isolated process
    pub fn terminate(&self, id: u64) -> Result<(), IsolationError> {
        let mut processes = self.processes.write();
        
        if let Some(_process) = processes.remove(&id) {
            // Would kill process, remove cgroup, namespace here
            Ok(())
        } else {
            Err(IsolationError::NotFound)
        }
    }
    
    /// Get process info
    pub fn get_process(&self, id: u64) -> Option<IsolatedProcess> {
        self.processes.read().get(&id).cloned()
    }
    
    /// List processes for tenant
    pub fn list_for_tenant(&self, tenant_id: &TenantId) -> Vec<IsolatedProcess> {
        self.processes.read()
            .values()
            .filter(|p| &p.tenant_id == tenant_id)
            .cloned()
            .collect()
    }
}

#[derive(Debug)]
pub enum IsolationError {
    NotFound,
    CgroupCreationFailed,
    NamespaceCreationFailed,
    ResourceExhausted,
}

// ============================================================================
// Encryption Isolation
// ============================================================================

/// Per-tenant encryption key management
pub struct EncryptionIsolator {
    /// Encryption keys per tenant
    keys: RwLock<HashMap<TenantId, EncryptionKeySet>>,
    
    /// Key derivation parameters
    kdf_params: KdfParams,
}

#[derive(Debug, Clone)]
struct EncryptionKeySet {
    /// Data encryption key (encrypted with KEK)
    data_key: Vec<u8>,
    
    /// Key encryption key ID
    kek_id: String,
    
    /// Key version
    version: u64,
    
    /// Creation time
    created_at: Instant,
    
    /// Rotation deadline
    rotate_after: Duration,
}

#[derive(Debug, Clone)]
pub struct KdfParams {
    /// Algorithm (e.g., HKDF-SHA256)
    pub algorithm: String,
    
    /// Salt length
    pub salt_len: usize,
    
    /// Derived key length
    pub key_len: usize,
    
    /// Info context
    pub info: Vec<u8>,
}

impl Default for KdfParams {
    fn default() -> Self {
        Self {
            algorithm: "HKDF-SHA256".to_string(),
            salt_len: 32,
            key_len: 32,
            info: b"grey-distributed".to_vec(),
        }
    }
}

impl EncryptionIsolator {
    pub fn new(kdf_params: KdfParams) -> Self {
        Self {
            keys: RwLock::new(HashMap::new()),
            kdf_params,
        }
    }
    
    /// Create keys for a new tenant
    ///
    /// # Key Hierarchy
    ///
    /// Each tenant gets:
    /// - KEK (Key Encryption Key): Protects DEKs, stored in KMS
    /// - DEK (Data Encryption Key): Actually encrypts data
    ///
    /// This separation allows key rotation without re-encrypting data.
    pub fn create_tenant_keys(&self, tenant_id: TenantId) -> Result<(), String> {
        let key_set = EncryptionKeySet {
            data_key: vec![0u8; self.kdf_params.key_len], // Would derive real key
            kek_id: format!("kek-{}", tenant_id.as_str()),
            version: 1,
            created_at: Instant::now(),
            rotate_after: Duration::from_secs(86400 * 30), // 30 days
        };
        
        self.keys.write().insert(tenant_id, key_set);
        Ok(())
    }
    
    /// Get encryption key for tenant
    pub fn get_key(&self, tenant_id: &TenantId) -> Option<Vec<u8>> {
        self.keys.read()
            .get(tenant_id)
            .map(|ks| ks.data_key.clone())
    }
    
    /// Check if key needs rotation
    pub fn needs_rotation(&self, tenant_id: &TenantId) -> bool {
        self.keys.read()
            .get(tenant_id)
            .map(|ks| ks.created_at.elapsed() > ks.rotate_after)
            .unwrap_or(false)
    }
    
    /// Rotate tenant key
    pub fn rotate_key(&self, tenant_id: &TenantId) -> Result<u64, String> {
        let mut keys = self.keys.write();
        
        if let Some(key_set) = keys.get_mut(tenant_id) {
            key_set.version += 1;
            key_set.data_key = vec![0u8; self.kdf_params.key_len]; // New key
            key_set.created_at = Instant::now();
            Ok(key_set.version)
        } else {
            Err("Tenant not found".to_string())
        }
    }
    
    /// Delete tenant keys
    pub fn delete_tenant_keys(&self, tenant_id: &TenantId) {
        self.keys.write().remove(tenant_id);
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_tenant_manager() {
        let manager = TenantManager::new(ResourceLimits::default());
        
        let tenant_id = TenantId::new("tenant-1");
        let mut permissions = HashSet::new();
        permissions.insert(Permission::Read);
        permissions.insert(Permission::Write);
        
        manager.register_tenant(tenant_id.clone(), permissions, None);
        
        assert!(manager.check_permission(&tenant_id, &Permission::Read));
        assert!(!manager.check_permission(&tenant_id, &Permission::Admin));
    }
    
    #[test]
    fn test_sandbox() {
        let tenant_id = TenantId::new("tenant-1");
        let mut sandbox = Sandbox::new(tenant_id, ResourceLimits::default());
        
        sandbox.start().unwrap();
        assert_eq!(sandbox.state(), SandboxState::Running);
        
        // Check syscall filtering
        assert!(sandbox.check_syscall(0)); // read - allowed
        assert!(!sandbox.check_syscall(999)); // unknown - blocked
        
        assert_eq!(sandbox.violation_count(), 1);
    }
    
    #[test]
    fn test_process_isolator() {
        let isolator = ProcessIsolator::new(CgroupSettings::default());
        let tenant_id = TenantId::new("tenant-1");
        
        let process = isolator.create_isolated(
            tenant_id.clone(),
            ResourceLimits::default(),
        ).unwrap();
        
        assert!(isolator.get_process(process.id).is_some());
        
        let tenant_processes = isolator.list_for_tenant(&tenant_id);
        assert_eq!(tenant_processes.len(), 1);
    }
    
    #[test]
    fn test_encryption_isolator() {
        let isolator = EncryptionIsolator::new(KdfParams::default());
        let tenant_id = TenantId::new("tenant-1");
        
        isolator.create_tenant_keys(tenant_id.clone()).unwrap();
        
        let key = isolator.get_key(&tenant_id);
        assert!(key.is_some());
        
        let version = isolator.rotate_key(&tenant_id).unwrap();
        assert_eq!(version, 2);
    }
}
