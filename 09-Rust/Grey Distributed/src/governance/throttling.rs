//! # Adaptive Throttling
//!
//! Rate limiting and adaptive throttling for protecting system stability.
//!
//! ## Design Principles
//!
//! Throttling provides backpressure to prevent overload:
//! - **Fairness**: Throttle proportionally across tenants
//! - **Responsiveness**: React quickly to load changes
//! - **Graceful Degradation**: Prefer throttling over failure
//! - **Predictability**: Clients can anticipate throttling behavior
//!
//! ## Throttling Strategies
//!
//! 1. **Token Bucket**: Smooth rate limiting with burst capacity
//! 2. **Leaky Bucket**: Strict rate limiting, no bursts
//! 3. **Sliding Window**: Count-based limiting over time window
//! 4. **Adaptive**: Dynamically adjust based on system health

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};

use super::{GovernanceError, TenantId};

// ============================================================================
// Rate Limiter Traits
// ============================================================================

/// Core rate limiter interface
pub trait RateLimiter: Send + Sync {
    /// Try to acquire tokens
    ///
    /// Returns:
    /// - `Ok(())` if tokens acquired
    /// - `Err(wait_time)` if should wait before retrying
    fn try_acquire(&self, tokens: u32) -> Result<(), Duration>;
    
    /// Get current rate (tokens per second)
    fn current_rate(&self) -> f64;
    
    /// Get available tokens
    fn available(&self) -> u32;
}

// ============================================================================
// Token Bucket Rate Limiter
// ============================================================================

/// Token bucket rate limiter
///
/// Classic algorithm that allows bursting up to bucket capacity
/// while maintaining long-term rate limit.
///
/// Properties:
/// - Allows short bursts up to bucket size
/// - Smooth rate limiting over time
/// - Simple and efficient
pub struct TokenBucket {
    /// Maximum tokens in bucket
    capacity: u32,
    
    /// Tokens currently available
    tokens: Mutex<f64>,
    
    /// Token refill rate (per second)
    refill_rate: f64,
    
    /// Last refill time
    last_refill: Mutex<Instant>,
}

impl TokenBucket {
    /// Create new token bucket
    ///
    /// # Arguments
    /// - `capacity`: Maximum tokens (burst size)
    /// - `rate`: Tokens added per second
    pub fn new(capacity: u32, rate: f64) -> Self {
        Self {
            capacity,
            tokens: Mutex::new(capacity as f64),
            refill_rate: rate,
            last_refill: Mutex::new(Instant::now()),
        }
    }
    
    fn refill(&self) {
        let mut tokens = self.tokens.lock();
        let mut last = self.last_refill.lock();
        
        let now = Instant::now();
        let elapsed = now.duration_since(*last).as_secs_f64();
        
        let new_tokens = elapsed * self.refill_rate;
        *tokens = (*tokens + new_tokens).min(self.capacity as f64);
        *last = now;
    }
}

impl RateLimiter for TokenBucket {
    fn try_acquire(&self, tokens: u32) -> Result<(), Duration> {
        self.refill();
        
        let mut available = self.tokens.lock();
        
        if *available >= tokens as f64 {
            *available -= tokens as f64;
            Ok(())
        } else {
            // Calculate wait time
            let deficit = tokens as f64 - *available;
            let wait_secs = deficit / self.refill_rate;
            Err(Duration::from_secs_f64(wait_secs))
        }
    }
    
    fn current_rate(&self) -> f64 {
        self.refill_rate
    }
    
    fn available(&self) -> u32 {
        self.refill();
        *self.tokens.lock() as u32
    }
}

// ============================================================================
// Sliding Window Rate Limiter
// ============================================================================

/// Sliding window counter rate limiter
///
/// Counts requests in a sliding time window for more accurate
/// rate limiting than fixed windows.
///
/// Properties:
/// - More accurate than fixed windows
/// - Memory efficient (only stores two counters)
/// - Prevents boundary burst issues
pub struct SlidingWindow {
    /// Window size
    window: Duration,
    
    /// Maximum requests per window
    limit: u32,
    
    /// Current window start
    current_window_start: Mutex<Instant>,
    
    /// Current window count
    current_count: AtomicU64,
    
    /// Previous window count
    previous_count: AtomicU64,
}

impl SlidingWindow {
    pub fn new(window: Duration, limit: u32) -> Self {
        Self {
            window,
            limit,
            current_window_start: Mutex::new(Instant::now()),
            current_count: AtomicU64::new(0),
            previous_count: AtomicU64::new(0),
        }
    }
    
    fn slide_window(&self) {
        let mut start = self.current_window_start.lock();
        let now = Instant::now();
        let elapsed = now.duration_since(*start);
        
        if elapsed >= self.window {
            // Move to new window
            self.previous_count.store(
                self.current_count.load(Ordering::Relaxed),
                Ordering::Relaxed,
            );
            self.current_count.store(0, Ordering::Relaxed);
            *start = now;
        }
    }
    
    fn weighted_count(&self) -> f64 {
        let start = self.current_window_start.lock();
        let elapsed = Instant::now().duration_since(*start);
        let weight = elapsed.as_secs_f64() / self.window.as_secs_f64();
        
        let current = self.current_count.load(Ordering::Relaxed) as f64;
        let previous = self.previous_count.load(Ordering::Relaxed) as f64;
        
        // Weighted average of current and previous window
        current + previous * (1.0 - weight)
    }
}

impl RateLimiter for SlidingWindow {
    fn try_acquire(&self, tokens: u32) -> Result<(), Duration> {
        self.slide_window();
        
        let count = self.weighted_count();
        
        if count + tokens as f64 <= self.limit as f64 {
            self.current_count.fetch_add(tokens as u64, Ordering::Relaxed);
            Ok(())
        } else {
            // Estimate wait time until slots available
            let excess = count + tokens as f64 - self.limit as f64;
            let rate = self.limit as f64 / self.window.as_secs_f64();
            let wait_secs = excess / rate;
            Err(Duration::from_secs_f64(wait_secs))
        }
    }
    
    fn current_rate(&self) -> f64 {
        self.limit as f64 / self.window.as_secs_f64()
    }
    
    fn available(&self) -> u32 {
        self.slide_window();
        let count = self.weighted_count();
        ((self.limit as f64 - count).max(0.0)) as u32
    }
}

// ============================================================================
// Adaptive Rate Limiter
// ============================================================================

/// Adaptive rate limiter that adjusts based on system health
///
/// Uses AIMD (Additive Increase, Multiplicative Decrease) to
/// dynamically adjust rate limits based on success/failure feedback.
///
/// Properties:
/// - Automatically adjusts to system capacity
/// - Responds to changing conditions
/// - Converges to optimal rate
pub struct AdaptiveRateLimiter {
    /// Minimum rate (floor)
    min_rate: f64,
    
    /// Maximum rate (ceiling)
    max_rate: f64,
    
    /// Current rate
    current_rate: Mutex<f64>,
    
    /// Underlying rate limiter
    limiter: Mutex<TokenBucket>,
    
    /// Additive increase factor
    increase_factor: f64,
    
    /// Multiplicative decrease factor
    decrease_factor: f64,
    
    /// Success window
    success_window: Mutex<SuccessWindow>,
    
    /// Rate adjustment interval
    adjustment_interval: Duration,
    
    /// Last adjustment time
    last_adjustment: Mutex<Instant>,
}

#[derive(Debug)]
struct SuccessWindow {
    successes: u32,
    failures: u32,
    total: u32,
}

impl SuccessWindow {
    fn success_rate(&self) -> f64 {
        if self.total == 0 {
            1.0
        } else {
            self.successes as f64 / self.total as f64
        }
    }
    
    fn reset(&mut self) {
        self.successes = 0;
        self.failures = 0;
        self.total = 0;
    }
}

impl AdaptiveRateLimiter {
    pub fn new(min_rate: f64, max_rate: f64, initial_rate: f64) -> Self {
        Self {
            min_rate,
            max_rate,
            current_rate: Mutex::new(initial_rate),
            limiter: Mutex::new(TokenBucket::new(
                (initial_rate * 2.0) as u32,
                initial_rate,
            )),
            increase_factor: 1.0,
            decrease_factor: 0.5,
            success_window: Mutex::new(SuccessWindow {
                successes: 0,
                failures: 0,
                total: 0,
            }),
            adjustment_interval: Duration::from_secs(1),
            last_adjustment: Mutex::new(Instant::now()),
        }
    }
    
    /// Record a successful operation
    pub fn record_success(&self) {
        let mut window = self.success_window.lock();
        window.successes += 1;
        window.total += 1;
        drop(window);
        
        self.maybe_adjust();
    }
    
    /// Record a failed operation
    pub fn record_failure(&self) {
        let mut window = self.success_window.lock();
        window.failures += 1;
        window.total += 1;
        drop(window);
        
        self.maybe_adjust();
    }
    
    fn maybe_adjust(&self) {
        let mut last = self.last_adjustment.lock();
        let now = Instant::now();
        
        if now.duration_since(*last) < self.adjustment_interval {
            return;
        }
        
        *last = now;
        drop(last);
        
        let mut window = self.success_window.lock();
        let success_rate = window.success_rate();
        window.reset();
        drop(window);
        
        let mut rate = self.current_rate.lock();
        
        // AIMD: Additive Increase, Multiplicative Decrease
        if success_rate >= 0.95 {
            // High success rate: increase additively
            *rate = (*rate + self.increase_factor).min(self.max_rate);
        } else if success_rate < 0.8 {
            // High failure rate: decrease multiplicatively
            *rate = (*rate * self.decrease_factor).max(self.min_rate);
        }
        
        // Update underlying limiter
        let mut limiter = self.limiter.lock();
        *limiter = TokenBucket::new((*rate * 2.0) as u32, *rate);
    }
}

impl RateLimiter for AdaptiveRateLimiter {
    fn try_acquire(&self, tokens: u32) -> Result<(), Duration> {
        self.limiter.lock().try_acquire(tokens)
    }
    
    fn current_rate(&self) -> f64 {
        *self.current_rate.lock()
    }
    
    fn available(&self) -> u32 {
        self.limiter.lock().available()
    }
}

// ============================================================================
// Throttle Manager
// ============================================================================

/// Manages throttling across tenants
pub struct ThrottleManager {
    /// Per-tenant rate limiters
    tenant_limiters: RwLock<HashMap<TenantId, Arc<dyn RateLimiter>>>,
    
    /// Global rate limiter
    global_limiter: Arc<dyn RateLimiter>,
    
    /// Throttle policies
    policies: RwLock<HashMap<TenantId, ThrottlePolicy>>,
    
    /// Metrics
    metrics: ThrottleMetrics,
}

#[derive(Debug, Clone)]
pub struct ThrottlePolicy {
    /// Base rate limit (requests per second)
    pub base_rate: f64,
    
    /// Burst capacity
    pub burst_size: u32,
    
    /// Priority multiplier for rate
    pub priority_multiplier: f64,
    
    /// Strategy for rate limiting
    pub strategy: ThrottleStrategy,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ThrottleStrategy {
    /// Fixed rate limiting
    Fixed,
    /// Adaptive based on success rate
    Adaptive,
    /// Fair share across tenants
    FairShare,
}

#[derive(Debug, Default)]
struct ThrottleMetrics {
    requests_allowed: AtomicU64,
    requests_throttled: AtomicU64,
    total_throttle_time_ms: AtomicU64,
}

impl ThrottleManager {
    pub fn new(global_rate: f64) -> Self {
        Self {
            tenant_limiters: RwLock::new(HashMap::new()),
            global_limiter: Arc::new(TokenBucket::new(
                (global_rate * 2.0) as u32,
                global_rate,
            )),
            policies: RwLock::new(HashMap::new()),
            metrics: ThrottleMetrics::default(),
        }
    }
    
    /// Register a tenant with throttle policy
    pub fn register_tenant(&self, tenant_id: TenantId, policy: ThrottlePolicy) {
        let limiter: Arc<dyn RateLimiter> = match policy.strategy {
            ThrottleStrategy::Fixed => Arc::new(TokenBucket::new(
                policy.burst_size,
                policy.base_rate * policy.priority_multiplier,
            )),
            ThrottleStrategy::Adaptive => Arc::new(AdaptiveRateLimiter::new(
                policy.base_rate * 0.1,
                policy.base_rate * policy.priority_multiplier * 2.0,
                policy.base_rate * policy.priority_multiplier,
            )),
            ThrottleStrategy::FairShare => Arc::new(SlidingWindow::new(
                Duration::from_secs(1),
                (policy.base_rate * policy.priority_multiplier) as u32,
            )),
        };
        
        self.tenant_limiters.write().insert(tenant_id, limiter);
        self.policies.write().insert(tenant_id, policy);
    }
    
    /// Try to allow a request
    pub fn try_acquire(&self, tenant_id: TenantId, tokens: u32) -> Result<(), ThrottleResult> {
        // Check global limit first
        if let Err(wait) = self.global_limiter.try_acquire(tokens) {
            self.metrics.requests_throttled.fetch_add(1, Ordering::Relaxed);
            return Err(ThrottleResult::GlobalLimit { wait_time: wait });
        }
        
        // Check tenant limit
        let limiters = self.tenant_limiters.read();
        if let Some(limiter) = limiters.get(&tenant_id) {
            if let Err(wait) = limiter.try_acquire(tokens) {
                self.metrics.requests_throttled.fetch_add(1, Ordering::Relaxed);
                self.metrics.total_throttle_time_ms.fetch_add(
                    wait.as_millis() as u64,
                    Ordering::Relaxed,
                );
                return Err(ThrottleResult::TenantLimit { wait_time: wait });
            }
        }
        
        self.metrics.requests_allowed.fetch_add(1, Ordering::Relaxed);
        Ok(())
    }
    
    /// Record success for adaptive throttling
    pub fn record_success(&self, tenant_id: TenantId) {
        // Adaptive rate limiting would adjust here
        // In a full implementation, we'd track success rates per tenant
        let _ = tenant_id;
    }
    
    /// Record failure for adaptive throttling
    pub fn record_failure(&self, tenant_id: TenantId) {
        // Adaptive rate limiting would adjust here
        // In a full implementation, we'd track failure rates per tenant
        let _ = tenant_id;
    }
    
    /// Get current rate for a tenant
    pub fn get_rate(&self, tenant_id: TenantId) -> Option<f64> {
        self.tenant_limiters
            .read()
            .get(&tenant_id)
            .map(|l| l.current_rate())
    }
    
    /// Get throttle metrics
    pub fn metrics(&self) -> (u64, u64, u64) {
        (
            self.metrics.requests_allowed.load(Ordering::Relaxed),
            self.metrics.requests_throttled.load(Ordering::Relaxed),
            self.metrics.total_throttle_time_ms.load(Ordering::Relaxed),
        )
    }
}

/// Result when request is throttled
#[derive(Debug)]
pub enum ThrottleResult {
    /// Throttled due to global limit
    GlobalLimit { wait_time: Duration },
    /// Throttled due to tenant limit
    TenantLimit { wait_time: Duration },
}

impl ThrottleResult {
    pub fn wait_time(&self) -> Duration {
        match self {
            Self::GlobalLimit { wait_time } => *wait_time,
            Self::TenantLimit { wait_time } => *wait_time,
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
    fn test_token_bucket() {
        let bucket = TokenBucket::new(10, 10.0);
        
        // Should allow burst up to capacity
        for _ in 0..10 {
            assert!(bucket.try_acquire(1).is_ok());
        }
        
        // Should be exhausted
        assert!(bucket.try_acquire(1).is_err());
    }
    
    #[test]
    fn test_sliding_window() {
        let window = SlidingWindow::new(Duration::from_secs(1), 10);
        
        // Should allow up to limit
        for _ in 0..10 {
            assert!(window.try_acquire(1).is_ok());
        }
        
        // Should be at limit
        assert!(window.try_acquire(1).is_err());
    }
    
    #[test]
    fn test_throttle_manager() {
        let manager = ThrottleManager::new(1000.0);
        
        manager.register_tenant(
            TenantId(1),
            ThrottlePolicy {
                base_rate: 100.0,
                burst_size: 10,
                priority_multiplier: 1.0,
                strategy: ThrottleStrategy::Fixed,
            },
        );
        
        // Should allow requests
        assert!(manager.try_acquire(TenantId(1), 1).is_ok());
        
        // Unknown tenant should pass (no tenant limiter)
        assert!(manager.try_acquire(TenantId(99), 1).is_ok());
    }
}
