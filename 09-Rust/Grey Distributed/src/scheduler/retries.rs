//! # Retry Policies and Backoff Strategies
//!
//! Implements sophisticated retry mechanisms for distributed task execution.
//!
//! ## Design Principles
//!
//! Retries in distributed systems are critical but dangerous:
//! - **Idempotency Required**: Retried operations must be safe to repeat
//! - **Exponential Backoff**: Prevents thundering herd on recovery
//! - **Jitter**: Randomization prevents synchronized retries
//! - **Circuit Breaking**: Stop retrying when system is overloaded
//! - **Deadline Awareness**: Don't retry if deadline has passed
//!
//! ## Retry Strategies
//!
//! 1. **Immediate**: Retry immediately (only for transient failures)
//! 2. **Fixed Delay**: Wait constant time between retries
//! 3. **Exponential Backoff**: Double delay each attempt
//! 4. **Decorrelated Jitter**: AWS-style randomized backoff
//! 5. **Custom**: User-defined retry logic

use std::time::{Duration, Instant};
use std::sync::Arc;

use super::SchedulerError;

// ============================================================================
// Retry Policy Configuration
// ============================================================================

/// Configuration for retry behavior
#[derive(Debug, Clone)]
pub struct RetryPolicy {
    /// Maximum number of retry attempts (0 = no retries)
    pub max_attempts: u32,
    
    /// Base delay between retries
    pub base_delay: Duration,
    
    /// Maximum delay cap
    pub max_delay: Duration,
    
    /// Backoff strategy
    pub strategy: BackoffStrategy,
    
    /// Which errors are retryable
    pub retryable_errors: Vec<RetryableError>,
    
    /// Whether to respect task deadlines
    pub respect_deadline: bool,
    
    /// Jitter configuration
    pub jitter: JitterConfig,
}

impl Default for RetryPolicy {
    fn default() -> Self {
        Self {
            max_attempts: 3,
            base_delay: Duration::from_millis(100),
            max_delay: Duration::from_secs(30),
            strategy: BackoffStrategy::ExponentialWithJitter,
            retryable_errors: vec![
                RetryableError::Transient,
                RetryableError::Timeout,
                RetryableError::Unavailable,
            ],
            respect_deadline: true,
            jitter: JitterConfig::default(),
        }
    }
}

/// Backoff strategies for retry delays
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BackoffStrategy {
    /// No delay between retries
    Immediate,
    
    /// Fixed delay between all retries
    Fixed,
    
    /// delay = base * 2^attempt
    Exponential,
    
    /// delay = base * 2^attempt + random jitter
    ExponentialWithJitter,
    
    /// AWS-style decorrelated jitter
    /// delay = random(base, previous_delay * 3)
    DecorrelatedJitter,
    
    /// Linear increase: delay = base * attempt
    Linear,
}

/// Jitter configuration for randomized delays
#[derive(Debug, Clone)]
pub struct JitterConfig {
    /// Type of jitter to apply
    pub mode: JitterMode,
    
    /// Maximum jitter as fraction of delay (0.0 - 1.0)
    pub factor: f64,
}

impl Default for JitterConfig {
    fn default() -> Self {
        Self {
            mode: JitterMode::Full,
            factor: 0.5,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum JitterMode {
    /// No jitter
    None,
    
    /// Full jitter: random(0, delay)
    Full,
    
    /// Equal jitter: delay/2 + random(0, delay/2)
    Equal,
    
    /// Capped: min(delay, random(0, delay * factor))
    Capped,
}

/// Categories of retryable errors
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RetryableError {
    /// Transient network issues
    Transient,
    
    /// Request timeout
    Timeout,
    
    /// Service temporarily unavailable
    Unavailable,
    
    /// Rate limited (429)
    RateLimited,
    
    /// Server error (5xx)
    ServerError,
    
    /// Resource conflict (optimistic concurrency)
    Conflict,
    
    /// Connection reset
    ConnectionReset,
}

// ============================================================================
// Retry State Machine
// ============================================================================

/// Tracks retry state for a single task
#[derive(Debug, Clone)]
pub struct RetryState {
    /// Task identifier
    pub task_id: u64,
    
    /// Number of attempts made (including initial)
    pub attempts: u32,
    
    /// Time of first attempt
    pub first_attempt: Instant,
    
    /// Time of last attempt
    pub last_attempt: Instant,
    
    /// Last computed delay
    pub last_delay: Duration,
    
    /// Total time spent in retries
    pub total_retry_time: Duration,
    
    /// History of errors encountered
    pub error_history: Vec<RetryableError>,
    
    /// Task deadline (if any)
    pub deadline: Option<Instant>,
}

impl RetryState {
    /// Create new retry state for a task
    pub fn new(task_id: u64, deadline: Option<Instant>) -> Self {
        let now = Instant::now();
        Self {
            task_id,
            attempts: 0,
            first_attempt: now,
            last_attempt: now,
            last_delay: Duration::ZERO,
            total_retry_time: Duration::ZERO,
            error_history: Vec::new(),
            deadline,
        }
    }
    
    /// Record an attempt
    pub fn record_attempt(&mut self, error: Option<RetryableError>) {
        let now = Instant::now();
        self.attempts += 1;
        self.total_retry_time = now.duration_since(self.first_attempt);
        self.last_attempt = now;
        
        if let Some(e) = error {
            self.error_history.push(e);
        }
    }
    
    /// Check if deadline has passed
    pub fn deadline_exceeded(&self) -> bool {
        self.deadline.map(|d| Instant::now() > d).unwrap_or(false)
    }
    
    /// Time remaining until deadline
    pub fn time_until_deadline(&self) -> Option<Duration> {
        self.deadline.map(|d| {
            let now = Instant::now();
            if now > d {
                Duration::ZERO
            } else {
                d.duration_since(now)
            }
        })
    }
}

// ============================================================================
// Retry Calculator
// ============================================================================

/// Calculates retry delays based on policy and state
pub struct RetryCalculator {
    policy: RetryPolicy,
}

impl RetryCalculator {
    pub fn new(policy: RetryPolicy) -> Self {
        Self { policy }
    }
    
    /// Determine if a retry should be attempted
    ///
    /// Returns:
    /// - `Ok(Some(delay))` - Retry after delay
    /// - `Ok(None)` - No retry needed (success)
    /// - `Err(SchedulerError)` - Cannot retry
    pub fn should_retry(
        &self,
        state: &RetryState,
        error: &RetryableError,
    ) -> Result<Option<Duration>, SchedulerError> {
        // Check if error is retryable
        if !self.is_retryable(error) {
            return Err(SchedulerError::MaxRetriesExceeded);
        }
        
        // Check attempt count
        if state.attempts >= self.policy.max_attempts {
            return Err(SchedulerError::MaxRetriesExceeded);
        }
        
        // Calculate delay
        let delay = self.calculate_delay(state);
        
        // Check deadline
        if self.policy.respect_deadline {
            if let Some(remaining) = state.time_until_deadline() {
                if delay > remaining {
                    return Err(SchedulerError::DeadlineMissed);
                }
            }
        }
        
        Ok(Some(delay))
    }
    
    /// Calculate delay for next retry attempt
    ///
    /// Implements various backoff strategies with jitter.
    pub fn calculate_delay(&self, state: &RetryState) -> Duration {
        let base_ms = self.policy.base_delay.as_millis() as f64;
        let attempt = state.attempts as f64;
        
        // Calculate base delay based on strategy
        let delay_ms = match self.policy.strategy {
            BackoffStrategy::Immediate => 0.0,
            
            BackoffStrategy::Fixed => base_ms,
            
            BackoffStrategy::Exponential => {
                base_ms * 2.0_f64.powf(attempt)
            }
            
            BackoffStrategy::ExponentialWithJitter => {
                let exp_delay = base_ms * 2.0_f64.powf(attempt);
                self.apply_jitter(exp_delay)
            }
            
            BackoffStrategy::DecorrelatedJitter => {
                // AWS decorrelated jitter algorithm
                let last_ms = state.last_delay.as_millis() as f64;
                let min = base_ms;
                let max = (last_ms * 3.0).max(base_ms);
                self.random_between(min, max)
            }
            
            BackoffStrategy::Linear => {
                base_ms * (attempt + 1.0)
            }
        };
        
        // Apply cap
        let max_ms = self.policy.max_delay.as_millis() as f64;
        let capped_ms = delay_ms.min(max_ms);
        
        Duration::from_millis(capped_ms as u64)
    }
    
    /// Check if an error type is retryable
    fn is_retryable(&self, error: &RetryableError) -> bool {
        self.policy.retryable_errors.contains(error)
    }
    
    /// Apply jitter to a delay value
    fn apply_jitter(&self, delay_ms: f64) -> f64 {
        match self.policy.jitter.mode {
            JitterMode::None => delay_ms,
            
            JitterMode::Full => {
                // random(0, delay)
                self.random_between(0.0, delay_ms)
            }
            
            JitterMode::Equal => {
                // delay/2 + random(0, delay/2)
                let half = delay_ms / 2.0;
                half + self.random_between(0.0, half)
            }
            
            JitterMode::Capped => {
                // Limit jitter to configured factor
                let jitter_range = delay_ms * self.policy.jitter.factor;
                delay_ms - self.random_between(0.0, jitter_range)
            }
        }
    }
    
    /// Generate random value in range
    fn random_between(&self, min: f64, max: f64) -> f64 {
        // In production, use proper RNG
        // For now, simple pseudo-random based on time
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .subsec_nanos() as f64;
        
        let frac = (nanos % 1000.0) / 1000.0;
        min + (max - min) * frac
    }
}

// ============================================================================
// Retry Executor
// ============================================================================

/// Executes operations with retry logic
pub struct RetryExecutor {
    calculator: RetryCalculator,
}

impl RetryExecutor {
    pub fn new(policy: RetryPolicy) -> Self {
        Self {
            calculator: RetryCalculator::new(policy),
        }
    }
    
    /// Execute an async operation with retries
    ///
    /// The operation must be idempotent as it may be called multiple times.
    ///
    /// # Type Parameters
    /// - `F`: Async function that returns Result<T, RetryableError>
    /// - `T`: Success type
    ///
    /// # Returns
    /// - `Ok(T)` on success
    /// - `Err(SchedulerError)` if all retries exhausted
    pub async fn execute<F, Fut, T>(
        &self,
        task_id: u64,
        deadline: Option<Instant>,
        operation: F,
    ) -> Result<T, SchedulerError>
    where
        F: Fn() -> Fut,
        Fut: std::future::Future<Output = Result<T, RetryableError>>,
    {
        let mut state = RetryState::new(task_id, deadline);
        
        loop {
            state.record_attempt(None);
            
            match operation().await {
                Ok(result) => return Ok(result),
                
                Err(error) => {
                    state.error_history.push(error);
                    
                    match self.calculator.should_retry(&state, &error) {
                        Ok(Some(delay)) => {
                            state.last_delay = delay;
                            tokio::time::sleep(delay).await;
                            // Continue to next iteration
                        }
                        Ok(None) => {
                            // This shouldn't happen for errors
                            return Err(SchedulerError::MaxRetriesExceeded);
                        }
                        Err(e) => return Err(e),
                    }
                }
            }
        }
    }
    
    /// Execute with retry, calling hooks on each attempt
    pub async fn execute_with_hooks<F, Fut, T, H>(
        &self,
        task_id: u64,
        deadline: Option<Instant>,
        operation: F,
        hooks: H,
    ) -> Result<T, SchedulerError>
    where
        F: Fn() -> Fut,
        Fut: std::future::Future<Output = Result<T, RetryableError>>,
        H: RetryHooks,
    {
        let mut state = RetryState::new(task_id, deadline);
        
        hooks.on_start(task_id);
        
        loop {
            state.record_attempt(None);
            hooks.on_attempt(task_id, state.attempts);
            
            match operation().await {
                Ok(result) => {
                    hooks.on_success(task_id, state.attempts);
                    return Ok(result);
                }
                
                Err(error) => {
                    state.error_history.push(error);
                    hooks.on_error(task_id, state.attempts, &error);
                    
                    match self.calculator.should_retry(&state, &error) {
                        Ok(Some(delay)) => {
                            state.last_delay = delay;
                            hooks.on_retry(task_id, state.attempts + 1, delay);
                            tokio::time::sleep(delay).await;
                        }
                        Ok(None) => {
                            return Err(SchedulerError::MaxRetriesExceeded);
                        }
                        Err(e) => {
                            hooks.on_exhausted(task_id, state.attempts, &state.error_history);
                            return Err(e);
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Retry Hooks
// ============================================================================

/// Hooks for observing retry behavior
pub trait RetryHooks: Send + Sync {
    /// Called when retry sequence starts
    fn on_start(&self, task_id: u64);
    
    /// Called before each attempt
    fn on_attempt(&self, task_id: u64, attempt: u32);
    
    /// Called on successful completion
    fn on_success(&self, task_id: u64, attempts: u32);
    
    /// Called on each error
    fn on_error(&self, task_id: u64, attempt: u32, error: &RetryableError);
    
    /// Called before retry delay
    fn on_retry(&self, task_id: u64, next_attempt: u32, delay: Duration);
    
    /// Called when retries exhausted
    fn on_exhausted(&self, task_id: u64, attempts: u32, errors: &[RetryableError]);
}

/// No-op implementation of retry hooks
pub struct NoOpRetryHooks;

impl RetryHooks for NoOpRetryHooks {
    fn on_start(&self, _task_id: u64) {}
    fn on_attempt(&self, _task_id: u64, _attempt: u32) {}
    fn on_success(&self, _task_id: u64, _attempts: u32) {}
    fn on_error(&self, _task_id: u64, _attempt: u32, _error: &RetryableError) {}
    fn on_retry(&self, _task_id: u64, _next_attempt: u32, _delay: Duration) {}
    fn on_exhausted(&self, _task_id: u64, _attempts: u32, _errors: &[RetryableError]) {}
}

/// Logging implementation of retry hooks
pub struct LoggingRetryHooks;

impl RetryHooks for LoggingRetryHooks {
    fn on_start(&self, task_id: u64) {
        tracing::debug!(task_id, "Starting retry sequence");
    }
    
    fn on_attempt(&self, task_id: u64, attempt: u32) {
        tracing::trace!(task_id, attempt, "Executing attempt");
    }
    
    fn on_success(&self, task_id: u64, attempts: u32) {
        tracing::debug!(task_id, attempts, "Task succeeded");
    }
    
    fn on_error(&self, task_id: u64, attempt: u32, error: &RetryableError) {
        tracing::warn!(task_id, attempt, ?error, "Attempt failed");
    }
    
    fn on_retry(&self, task_id: u64, next_attempt: u32, delay: Duration) {
        tracing::info!(
            task_id,
            next_attempt,
            delay_ms = delay.as_millis(),
            "Scheduling retry"
        );
    }
    
    fn on_exhausted(&self, task_id: u64, attempts: u32, errors: &[RetryableError]) {
        tracing::error!(
            task_id,
            attempts,
            error_count = errors.len(),
            "Retry attempts exhausted"
        );
    }
}

// ============================================================================
// Retry Budget
// ============================================================================

/// Tracks retry budget across the system
///
/// Prevents retry storms by limiting total retry rate.
/// Uses token bucket algorithm for rate limiting.
pub struct RetryBudget {
    /// Maximum tokens in bucket
    max_tokens: u32,
    
    /// Current tokens available
    tokens: std::sync::atomic::AtomicU32,
    
    /// Token refill rate (tokens per second)
    refill_rate: f64,
    
    /// Last refill time
    last_refill: std::sync::Mutex<Instant>,
    
    /// Minimum retry ratio (retries / requests)
    min_retry_ratio: f64,
    
    /// Request counter
    request_count: std::sync::atomic::AtomicU64,
    
    /// Retry counter
    retry_count: std::sync::atomic::AtomicU64,
}

impl RetryBudget {
    pub fn new(max_tokens: u32, refill_rate: f64) -> Self {
        Self {
            max_tokens,
            tokens: std::sync::atomic::AtomicU32::new(max_tokens),
            refill_rate,
            last_refill: std::sync::Mutex::new(Instant::now()),
            min_retry_ratio: 0.1, // Allow at least 10% retry rate
            request_count: std::sync::atomic::AtomicU64::new(0),
            retry_count: std::sync::atomic::AtomicU64::new(0),
        }
    }
    
    /// Try to acquire a retry token
    ///
    /// Returns true if retry is allowed, false if budget exhausted.
    pub fn try_acquire(&self) -> bool {
        self.refill();
        
        // Atomic decrement if tokens available
        let mut current = self.tokens.load(std::sync::atomic::Ordering::Relaxed);
        loop {
            if current == 0 {
                return false;
            }
            
            match self.tokens.compare_exchange_weak(
                current,
                current - 1,
                std::sync::atomic::Ordering::Relaxed,
                std::sync::atomic::Ordering::Relaxed,
            ) {
                Ok(_) => {
                    self.retry_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    return true;
                }
                Err(x) => current = x,
            }
        }
    }
    
    /// Record a request (for ratio tracking)
    pub fn record_request(&self) {
        self.request_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
    
    /// Refill tokens based on elapsed time
    fn refill(&self) {
        let mut last = self.last_refill.lock().unwrap();
        let now = Instant::now();
        let elapsed = now.duration_since(*last).as_secs_f64();
        
        if elapsed < 0.001 {
            return; // Too soon to refill
        }
        
        let new_tokens = (elapsed * self.refill_rate) as u32;
        if new_tokens > 0 {
            let current = self.tokens.load(std::sync::atomic::Ordering::Relaxed);
            let refilled = (current + new_tokens).min(self.max_tokens);
            self.tokens.store(refilled, std::sync::atomic::Ordering::Relaxed);
            *last = now;
        }
    }
    
    /// Get current retry ratio
    pub fn retry_ratio(&self) -> f64 {
        let requests = self.request_count.load(std::sync::atomic::Ordering::Relaxed);
        let retries = self.retry_count.load(std::sync::atomic::Ordering::Relaxed);
        
        if requests == 0 {
            return 0.0;
        }
        
        retries as f64 / requests as f64
    }
    
    /// Get available tokens
    pub fn available_tokens(&self) -> u32 {
        self.tokens.load(std::sync::atomic::Ordering::Relaxed)
    }
}

// ============================================================================
// Predefined Policies
// ============================================================================

/// Common retry policies for different scenarios
pub struct RetryPolicies;

impl RetryPolicies {
    /// Aggressive retries for idempotent operations
    pub fn aggressive() -> RetryPolicy {
        RetryPolicy {
            max_attempts: 5,
            base_delay: Duration::from_millis(50),
            max_delay: Duration::from_secs(5),
            strategy: BackoffStrategy::ExponentialWithJitter,
            retryable_errors: vec![
                RetryableError::Transient,
                RetryableError::Timeout,
                RetryableError::Unavailable,
                RetryableError::RateLimited,
                RetryableError::ServerError,
                RetryableError::ConnectionReset,
            ],
            respect_deadline: true,
            jitter: JitterConfig::default(),
        }
    }
    
    /// Conservative retries for non-idempotent operations
    pub fn conservative() -> RetryPolicy {
        RetryPolicy {
            max_attempts: 2,
            base_delay: Duration::from_millis(500),
            max_delay: Duration::from_secs(2),
            strategy: BackoffStrategy::Fixed,
            retryable_errors: vec![
                RetryableError::Transient,
                RetryableError::ConnectionReset,
            ],
            respect_deadline: true,
            jitter: JitterConfig { mode: JitterMode::None, factor: 0.0 },
        }
    }
    
    /// No retries
    pub fn no_retry() -> RetryPolicy {
        RetryPolicy {
            max_attempts: 1,
            base_delay: Duration::ZERO,
            max_delay: Duration::ZERO,
            strategy: BackoffStrategy::Immediate,
            retryable_errors: vec![],
            respect_deadline: false,
            jitter: JitterConfig { mode: JitterMode::None, factor: 0.0 },
        }
    }
    
    /// Retries optimized for rate-limited APIs
    pub fn rate_limited() -> RetryPolicy {
        RetryPolicy {
            max_attempts: 10,
            base_delay: Duration::from_secs(1),
            max_delay: Duration::from_secs(60),
            strategy: BackoffStrategy::DecorrelatedJitter,
            retryable_errors: vec![
                RetryableError::RateLimited,
                RetryableError::Unavailable,
            ],
            respect_deadline: false,
            jitter: JitterConfig::default(),
        }
    }
    
    /// Retries for distributed consensus operations
    pub fn consensus() -> RetryPolicy {
        RetryPolicy {
            max_attempts: 3,
            base_delay: Duration::from_millis(100),
            max_delay: Duration::from_secs(1),
            strategy: BackoffStrategy::ExponentialWithJitter,
            retryable_errors: vec![
                RetryableError::Timeout,
                RetryableError::Unavailable,
                RetryableError::Conflict,
            ],
            respect_deadline: true,
            jitter: JitterConfig {
                mode: JitterMode::Full,
                factor: 0.5,
            },
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
    fn test_exponential_backoff() {
        let policy = RetryPolicy {
            max_attempts: 5,
            base_delay: Duration::from_millis(100),
            max_delay: Duration::from_secs(10),
            strategy: BackoffStrategy::Exponential,
            jitter: JitterConfig { mode: JitterMode::None, factor: 0.0 },
            ..Default::default()
        };
        
        let calc = RetryCalculator::new(policy);
        
        // Attempt 0: 100 * 2^0 = 100ms
        let state0 = RetryState::new(1, None);
        assert_eq!(calc.calculate_delay(&state0), Duration::from_millis(100));
        
        // Attempt 1: 100 * 2^1 = 200ms
        let mut state1 = RetryState::new(1, None);
        state1.attempts = 1;
        assert_eq!(calc.calculate_delay(&state1), Duration::from_millis(200));
        
        // Attempt 2: 100 * 2^2 = 400ms
        let mut state2 = RetryState::new(1, None);
        state2.attempts = 2;
        assert_eq!(calc.calculate_delay(&state2), Duration::from_millis(400));
    }
    
    #[test]
    fn test_retry_budget() {
        let budget = RetryBudget::new(10, 1.0);
        
        // Should allow initial retries
        for _ in 0..10 {
            assert!(budget.try_acquire());
        }
        
        // Should be exhausted
        assert!(!budget.try_acquire());
    }
    
    #[test]
    fn test_should_retry_max_attempts() {
        let policy = RetryPolicy {
            max_attempts: 3,
            ..Default::default()
        };
        
        let calc = RetryCalculator::new(policy);
        
        let mut state = RetryState::new(1, None);
        state.attempts = 3;
        
        let result = calc.should_retry(&state, &RetryableError::Transient);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_non_retryable_error() {
        let policy = RetryPolicy {
            retryable_errors: vec![RetryableError::Transient],
            ..Default::default()
        };
        
        let calc = RetryCalculator::new(policy);
        let state = RetryState::new(1, None);
        
        // Timeout is not in retryable list
        let result = calc.should_retry(&state, &RetryableError::Timeout);
        assert!(result.is_err());
    }
}
