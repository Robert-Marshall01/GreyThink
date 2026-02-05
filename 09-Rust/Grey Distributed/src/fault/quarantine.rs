//! # Quarantine System
//!
//! Isolation of faulty components to prevent cascade failures.
//!
//! ## Quarantine Philosophy
//!
//! When a component fails repeatedly:
//! 1. Isolate it from the rest of the system
//! 2. Prevent it from affecting other components
//! 3. Allow time for recovery/investigation
//! 4. Gradually reintroduce when healthy
//!
//! ## Quarantine Levels
//!
//! 1. **Soft**: Reduced traffic, monitoring
//! 2. **Medium**: No new requests, complete in-flight
//! 3. **Hard**: Immediate isolation, reject all traffic
//!
//! ## Blast Radius Containment
//!
//! Limit the impact of failures by:
//! - Isolating failing components quickly
//! - Preventing cascading timeouts
//! - Maintaining system stability

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

use parking_lot::RwLock;

use super::FaultError;

// ============================================================================
// Core Types
// ============================================================================

/// Component identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComponentId(pub String);

impl ComponentId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
}

impl std::fmt::Display for ComponentId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Quarantine entry
#[derive(Debug, Clone)]
pub struct QuarantineEntry {
    /// Component being quarantined
    pub component: ComponentId,
    
    /// Quarantine level
    pub level: QuarantineLevel,
    
    /// Reason for quarantine
    pub reason: String,
    
    /// When quarantine started
    pub started_at: Instant,
    
    /// When quarantine expires (None = indefinite)
    pub expires_at: Option<Instant>,
    
    /// Number of failures that triggered this
    pub failure_count: u32,
    
    /// Tags for categorization
    pub tags: HashSet<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum QuarantineLevel {
    /// Reduced traffic
    Soft,
    /// No new requests
    Medium,
    /// Complete isolation
    Hard,
}

impl QuarantineEntry {
    pub fn new(component: ComponentId, level: QuarantineLevel, reason: impl Into<String>) -> Self {
        Self {
            component,
            level,
            reason: reason.into(),
            started_at: Instant::now(),
            expires_at: None,
            failure_count: 1,
            tags: HashSet::new(),
        }
    }
    
    /// Set expiration time
    pub fn with_expiry(mut self, duration: Duration) -> Self {
        self.expires_at = Some(Instant::now() + duration);
        self
    }
    
    /// Add a tag
    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.insert(tag.into());
        self
    }
    
    /// Check if expired
    pub fn is_expired(&self) -> bool {
        self.expires_at.map_or(false, |exp| Instant::now() > exp)
    }
    
    /// Duration in quarantine
    pub fn duration(&self) -> Duration {
        self.started_at.elapsed()
    }
}

// ============================================================================
// Quarantine Manager
// ============================================================================

/// Manages component quarantine
pub struct QuarantineManager {
    /// Active quarantines
    quarantines: RwLock<HashMap<ComponentId, QuarantineEntry>>,
    
    /// Quarantine history (for analysis)
    history: RwLock<Vec<HistoricalQuarantine>>,
    
    /// Maximum history size
    max_history: usize,
    
    /// Handlers for quarantine events
    handlers: RwLock<Vec<Box<dyn QuarantineHandler + Send + Sync>>>,
    
    /// Configuration
    config: QuarantineConfig,
    
    /// Metrics
    metrics: QuarantineMetrics,
}

#[derive(Debug, Clone)]
pub struct QuarantineConfig {
    /// Default quarantine duration
    pub default_duration: Duration,
    
    /// Escalation thresholds
    pub soft_to_medium_failures: u32,
    pub medium_to_hard_failures: u32,
    
    /// Auto-cleanup expired quarantines
    pub auto_cleanup: bool,
}

impl Default for QuarantineConfig {
    fn default() -> Self {
        Self {
            default_duration: Duration::from_secs(300), // 5 minutes
            soft_to_medium_failures: 5,
            medium_to_hard_failures: 10,
            auto_cleanup: true,
        }
    }
}

#[derive(Default)]
struct QuarantineMetrics {
    total_quarantines: AtomicU64,
    current_quarantines: AtomicU64,
    escalations: AtomicU64,
    releases: AtomicU64,
}

#[derive(Debug, Clone)]
struct HistoricalQuarantine {
    component: ComponentId,
    level: QuarantineLevel,
    reason: String,
    started_at: Instant,
    released_at: Instant,
    release_reason: String,
}

impl QuarantineManager {
    pub fn new(config: QuarantineConfig) -> Self {
        Self {
            quarantines: RwLock::new(HashMap::new()),
            history: RwLock::new(Vec::new()),
            max_history: 1000,
            handlers: RwLock::new(Vec::new()),
            config,
            metrics: QuarantineMetrics::default(),
        }
    }
    
    /// Check if a component is quarantined
    pub fn is_quarantined(&self, component: &ComponentId) -> bool {
        let quarantines = self.quarantines.read();
        
        if let Some(entry) = quarantines.get(component) {
            if entry.is_expired() {
                drop(quarantines);
                self.release(component, "Expired");
                false
            } else {
                true
            }
        } else {
            false
        }
    }
    
    /// Get quarantine status
    pub fn status(&self, component: &ComponentId) -> Option<QuarantineEntry> {
        let quarantines = self.quarantines.read();
        quarantines.get(component).cloned()
    }
    
    /// Quarantine a component
    pub fn quarantine(&self, entry: QuarantineEntry) {
        let component = entry.component.clone();
        let level = entry.level;
        
        {
            let mut quarantines = self.quarantines.write();
            
            // Check for escalation
            if let Some(existing) = quarantines.get(&component) {
                if existing.level < level {
                    self.metrics.escalations.fetch_add(1, Ordering::Relaxed);
                }
            }
            
            quarantines.insert(component.clone(), entry.clone());
        }
        
        self.metrics.total_quarantines.fetch_add(1, Ordering::Relaxed);
        self.metrics.current_quarantines.fetch_add(1, Ordering::Relaxed);
        
        // Notify handlers
        let handlers = self.handlers.read();
        for handler in handlers.iter() {
            handler.on_quarantine(&entry);
        }
        
        tracing::warn!(
            component = %component,
            level = ?level,
            reason = %entry.reason,
            "Component quarantined"
        );
    }
    
    /// Release a component from quarantine
    pub fn release(&self, component: &ComponentId, reason: impl Into<String>) {
        let reason = reason.into();
        
        let entry = {
            let mut quarantines = self.quarantines.write();
            quarantines.remove(component)
        };
        
        if let Some(entry) = entry {
            // Add to history
            {
                let mut history = self.history.write();
                if history.len() >= self.max_history {
                    history.remove(0);
                }
                history.push(HistoricalQuarantine {
                    component: entry.component.clone(),
                    level: entry.level,
                    reason: entry.reason.clone(),
                    started_at: entry.started_at,
                    released_at: Instant::now(),
                    release_reason: reason.clone(),
                });
            }
            
            self.metrics.current_quarantines.fetch_sub(1, Ordering::Relaxed);
            self.metrics.releases.fetch_add(1, Ordering::Relaxed);
            
            // Notify handlers
            let handlers = self.handlers.read();
            for handler in handlers.iter() {
                handler.on_release(&entry, &reason);
            }
            
            tracing::info!(
                component = %entry.component,
                reason = %reason,
                duration = ?entry.duration(),
                "Component released from quarantine"
            );
        }
    }
    
    /// Escalate quarantine level
    pub fn escalate(&self, component: &ComponentId, new_level: QuarantineLevel, reason: impl Into<String>) {
        let mut quarantines = self.quarantines.write();
        
        if let Some(entry) = quarantines.get_mut(component) {
            if new_level > entry.level {
                entry.level = new_level;
                entry.failure_count += 1;
                entry.reason = format!("{} (escalated: {})", entry.reason, reason.into());
                self.metrics.escalations.fetch_add(1, Ordering::Relaxed);
                
                tracing::warn!(
                    component = %component,
                    level = ?new_level,
                    "Quarantine escalated"
                );
            }
        }
    }
    
    /// Record a failure for potential quarantine
    pub fn record_failure(&self, component: &ComponentId, reason: impl Into<String>) {
        let reason = reason.into();
        let mut quarantines = self.quarantines.write();
        
        if let Some(entry) = quarantines.get_mut(component) {
            entry.failure_count += 1;
            
            // Check for escalation
            if entry.level == QuarantineLevel::Soft 
                && entry.failure_count >= self.config.soft_to_medium_failures {
                entry.level = QuarantineLevel::Medium;
                self.metrics.escalations.fetch_add(1, Ordering::Relaxed);
            } else if entry.level == QuarantineLevel::Medium 
                && entry.failure_count >= self.config.medium_to_hard_failures {
                entry.level = QuarantineLevel::Hard;
                self.metrics.escalations.fetch_add(1, Ordering::Relaxed);
            }
        } else {
            // New quarantine
            let entry = QuarantineEntry::new(component.clone(), QuarantineLevel::Soft, reason)
                .with_expiry(self.config.default_duration);
            
            drop(quarantines);
            self.quarantine(entry);
        }
    }
    
    /// Cleanup expired quarantines
    pub fn cleanup(&self) -> Vec<ComponentId> {
        let expired: Vec<_> = {
            let quarantines = self.quarantines.read();
            quarantines
                .iter()
                .filter(|(_, e)| e.is_expired())
                .map(|(id, _)| id.clone())
                .collect()
        };
        
        for id in &expired {
            self.release(id, "Expired");
        }
        
        expired
    }
    
    /// List all quarantined components
    pub fn list(&self) -> Vec<QuarantineEntry> {
        self.quarantines.read().values().cloned().collect()
    }
    
    /// List components at specific level
    pub fn list_by_level(&self, level: QuarantineLevel) -> Vec<ComponentId> {
        self.quarantines
            .read()
            .iter()
            .filter(|(_, e)| e.level == level)
            .map(|(id, _)| id.clone())
            .collect()
    }
    
    /// Register a quarantine handler
    pub fn on_quarantine_event<H: QuarantineHandler + 'static>(&self, handler: H) {
        self.handlers.write().push(Box::new(handler));
    }
    
    /// Get metrics
    pub fn stats(&self) -> QuarantineStats {
        QuarantineStats {
            total: self.metrics.total_quarantines.load(Ordering::Relaxed),
            current: self.metrics.current_quarantines.load(Ordering::Relaxed),
            escalations: self.metrics.escalations.load(Ordering::Relaxed),
            releases: self.metrics.releases.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug, Clone)]
pub struct QuarantineStats {
    pub total: u64,
    pub current: u64,
    pub escalations: u64,
    pub releases: u64,
}

/// Handler for quarantine events
pub trait QuarantineHandler: Send + Sync {
    fn on_quarantine(&self, entry: &QuarantineEntry);
    fn on_release(&self, entry: &QuarantineEntry, reason: &str);
}

// ============================================================================
// Bulkhead Pattern
// ============================================================================

/// Bulkhead for isolating resource pools
///
/// Prevents one failing component from exhausting
/// shared resources (threads, connections, etc.).
///
/// Like compartments in a ship - if one floods,
/// the others stay dry.
pub struct Bulkhead {
    /// Name for identification
    name: String,
    
    /// Maximum concurrent executions
    max_concurrent: usize,
    
    /// Current executions
    current: AtomicU64,
    
    /// Queue limit
    max_queue: usize,
    
    /// Current queue size
    queued: AtomicU64,
    
    /// Rejected count
    rejected: AtomicU64,
}

impl Bulkhead {
    pub fn new(name: impl Into<String>, max_concurrent: usize, max_queue: usize) -> Self {
        Self {
            name: name.into(),
            max_concurrent,
            current: AtomicU64::new(0),
            max_queue,
            queued: AtomicU64::new(0),
            rejected: AtomicU64::new(0),
        }
    }
    
    /// Try to acquire a permit
    pub fn try_acquire(&self) -> Option<BulkheadPermit> {
        let current = self.current.fetch_add(1, Ordering::SeqCst);
        
        if current < self.max_concurrent as u64 {
            Some(BulkheadPermit {
                current: &self.current,
            })
        } else {
            self.current.fetch_sub(1, Ordering::SeqCst);
            self.rejected.fetch_add(1, Ordering::Relaxed);
            None
        }
    }
    
    /// Get current utilization
    pub fn utilization(&self) -> f64 {
        self.current.load(Ordering::Relaxed) as f64 / self.max_concurrent as f64
    }
    
    /// Get stats
    pub fn stats(&self) -> BulkheadStats {
        BulkheadStats {
            name: self.name.clone(),
            max_concurrent: self.max_concurrent,
            current: self.current.load(Ordering::Relaxed),
            rejected: self.rejected.load(Ordering::Relaxed),
        }
    }
}

pub struct BulkheadPermit<'a> {
    current: &'a AtomicU64,
}

impl<'a> Drop for BulkheadPermit<'a> {
    fn drop(&mut self) {
        self.current.fetch_sub(1, Ordering::SeqCst);
    }
}

#[derive(Debug, Clone)]
pub struct BulkheadStats {
    pub name: String,
    pub max_concurrent: usize,
    pub current: u64,
    pub rejected: u64,
}

// ============================================================================
// Blast Radius Limiter
// ============================================================================

/// Limits the blast radius of failures
pub struct BlastRadiusLimiter {
    /// Groups of related components
    groups: RwLock<HashMap<String, HashSet<ComponentId>>>,
    
    /// Maximum failures per group
    max_per_group: usize,
    
    /// Current failures per group
    failures: RwLock<HashMap<String, Vec<ComponentId>>>,
}

impl BlastRadiusLimiter {
    pub fn new(max_per_group: usize) -> Self {
        Self {
            groups: RwLock::new(HashMap::new()),
            max_per_group,
            failures: RwLock::new(HashMap::new()),
        }
    }
    
    /// Define a group of components
    pub fn define_group(&self, name: impl Into<String>, components: Vec<ComponentId>) {
        let mut groups = self.groups.write();
        groups.insert(name.into(), components.into_iter().collect());
    }
    
    /// Check if a failure should be allowed
    pub fn allow_failure(&self, component: &ComponentId) -> bool {
        let groups = self.groups.read();
        let failures = self.failures.read();
        
        // Find which groups this component belongs to
        for (group_name, members) in groups.iter() {
            if members.contains(component) {
                if let Some(group_failures) = failures.get(group_name) {
                    if group_failures.len() >= self.max_per_group {
                        return false;
                    }
                }
            }
        }
        
        true
    }
    
    /// Record a failure
    pub fn record_failure(&self, component: &ComponentId) {
        let groups = self.groups.read();
        let mut failures = self.failures.write();
        
        for (group_name, members) in groups.iter() {
            if members.contains(component) {
                failures
                    .entry(group_name.clone())
                    .or_insert_with(Vec::new)
                    .push(component.clone());
            }
        }
    }
    
    /// Clear failure for a component
    pub fn clear_failure(&self, component: &ComponentId) {
        let mut failures = self.failures.write();
        
        for group_failures in failures.values_mut() {
            group_failures.retain(|c| c != component);
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_quarantine_manager() {
        let manager = QuarantineManager::new(QuarantineConfig::default());
        let component = ComponentId::new("test-service");
        
        assert!(!manager.is_quarantined(&component));
        
        let entry = QuarantineEntry::new(
            component.clone(),
            QuarantineLevel::Soft,
            "Test quarantine",
        );
        manager.quarantine(entry);
        
        assert!(manager.is_quarantined(&component));
        
        manager.release(&component, "Test release");
        assert!(!manager.is_quarantined(&component));
    }
    
    #[test]
    fn test_quarantine_escalation() {
        let config = QuarantineConfig {
            soft_to_medium_failures: 3,
            medium_to_hard_failures: 5,
            ..Default::default()
        };
        let manager = QuarantineManager::new(config);
        let component = ComponentId::new("test-service");
        
        // First failure triggers soft quarantine
        manager.record_failure(&component, "Failure 1");
        assert_eq!(manager.status(&component).unwrap().level, QuarantineLevel::Soft);
        
        // More failures escalate
        manager.record_failure(&component, "Failure 2");
        manager.record_failure(&component, "Failure 3");
        assert_eq!(manager.status(&component).unwrap().level, QuarantineLevel::Medium);
    }
    
    #[test]
    fn test_bulkhead() {
        let bulkhead = Bulkhead::new("test", 2, 0);
        
        let permit1 = bulkhead.try_acquire();
        assert!(permit1.is_some());
        
        let permit2 = bulkhead.try_acquire();
        assert!(permit2.is_some());
        
        // Third should be rejected
        let permit3 = bulkhead.try_acquire();
        assert!(permit3.is_none());
        
        // Drop one, should work again
        drop(permit1);
        let permit4 = bulkhead.try_acquire();
        assert!(permit4.is_some());
    }
    
    #[test]
    fn test_blast_radius() {
        let limiter = BlastRadiusLimiter::new(2);
        
        limiter.define_group("group1", vec![
            ComponentId::new("a"),
            ComponentId::new("b"),
            ComponentId::new("c"),
        ]);
        
        let a = ComponentId::new("a");
        let b = ComponentId::new("b");
        let c = ComponentId::new("c");
        
        assert!(limiter.allow_failure(&a));
        limiter.record_failure(&a);
        
        assert!(limiter.allow_failure(&b));
        limiter.record_failure(&b);
        
        // Third should be blocked
        assert!(!limiter.allow_failure(&c));
    }
}
