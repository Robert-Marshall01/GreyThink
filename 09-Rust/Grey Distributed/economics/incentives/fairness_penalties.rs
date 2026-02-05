//! # Grey Distributed — Fairness Penalties
//!
//! This module implements penalty mechanisms for quota violations and
//! unfair resource usage, ensuring all tenants receive their fair share.
//!
//! ## Penalty Model
//!
//! The penalty system uses:
//! - **Progressive penalties**: Increasing severity for repeat offenses
//! - **Grace periods**: Allowance for brief spikes before penalties
//! - **Proportional fines**: Penalties scale with violation magnitude
//! - **Cooling-off periods**: Reputation recovery after good behavior

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type TenantId = String;
pub type TokenAmount = u64;

/// Grace period before penalties apply (seconds)
const DEFAULT_GRACE_PERIOD: Duration = Duration::from_secs(60);

/// Minimum penalty amount (micro-tokens)
const MIN_PENALTY: TokenAmount = 10;

/// Maximum penalty multiplier for repeat offenses
const MAX_REPEAT_MULTIPLIER: f64 = 10.0;

/// Lookback window for repeat offense detection
const REPEAT_OFFENSE_WINDOW: Duration = Duration::from_secs(86400); // 24 hours

// =============================================================================
// Penalty Configuration
// =============================================================================

/// Configuration for the penalty system
#[derive(Debug, Clone)]
pub struct PenaltyConfig {
    /// Grace period before penalties apply
    pub grace_period: Duration,
    
    /// CPU quota violation penalties
    pub cpu_penalties: ResourcePenaltyConfig,
    
    /// Memory quota violation penalties
    pub memory_penalties: ResourcePenaltyConfig,
    
    /// Network quota violation penalties
    pub network_penalties: ResourcePenaltyConfig,
    
    /// Storage quota violation penalties
    pub storage_penalties: ResourcePenaltyConfig,
    
    /// Rate limit violation penalties
    pub rate_limit_penalties: RateLimitPenaltyConfig,
    
    /// Repeat offense settings
    pub repeat_offense: RepeatOffenseConfig,
    
    /// Cooling-off period settings
    pub cooling_off: CoolingOffConfig,
}

impl Default for PenaltyConfig {
    fn default() -> Self {
        Self {
            grace_period: DEFAULT_GRACE_PERIOD,
            cpu_penalties: ResourcePenaltyConfig::default(),
            memory_penalties: ResourcePenaltyConfig::default(),
            network_penalties: ResourcePenaltyConfig {
                base_rate: 5,   // Lower base for network
                escalation_factor: 1.3,
                max_rate: 100,
            },
            storage_penalties: ResourcePenaltyConfig {
                base_rate: 2,   // Lower base for storage
                escalation_factor: 1.2,
                max_rate: 50,
            },
            rate_limit_penalties: RateLimitPenaltyConfig::default(),
            repeat_offense: RepeatOffenseConfig::default(),
            cooling_off: CoolingOffConfig::default(),
        }
    }
}

/// Resource-specific penalty configuration
#[derive(Debug, Clone)]
pub struct ResourcePenaltyConfig {
    /// Base penalty rate per unit over quota
    pub base_rate: TokenAmount,
    
    /// Escalation factor for continued violation
    pub escalation_factor: f64,
    
    /// Maximum penalty rate per unit
    pub max_rate: TokenAmount,
}

impl Default for ResourcePenaltyConfig {
    fn default() -> Self {
        Self {
            base_rate: 10,
            escalation_factor: 1.5,
            max_rate: 500,
        }
    }
}

/// Rate limit penalty configuration
#[derive(Debug, Clone)]
pub struct RateLimitPenaltyConfig {
    /// Penalty per request over limit
    pub per_request_penalty: TokenAmount,
    
    /// Cooldown after rate limit hit
    pub cooldown_duration: Duration,
    
    /// Whether to block requests during cooldown
    pub block_during_cooldown: bool,
}

impl Default for RateLimitPenaltyConfig {
    fn default() -> Self {
        Self {
            per_request_penalty: 1,
            cooldown_duration: Duration::from_secs(60),
            block_during_cooldown: false,
        }
    }
}

/// Repeat offense configuration
#[derive(Debug, Clone)]
pub struct RepeatOffenseConfig {
    /// Lookback window for counting offenses
    pub lookback_window: Duration,
    
    /// Multiplier increase per repeat offense
    pub multiplier_per_offense: f64,
    
    /// Maximum total multiplier
    pub max_multiplier: f64,
    
    /// Threshold for suspension consideration
    pub suspension_threshold: u32,
}

impl Default for RepeatOffenseConfig {
    fn default() -> Self {
        Self {
            lookback_window: REPEAT_OFFENSE_WINDOW,
            multiplier_per_offense: 1.5,
            max_multiplier: MAX_REPEAT_MULTIPLIER,
            suspension_threshold: 10,
        }
    }
}

/// Cooling-off period configuration
#[derive(Debug, Clone)]
pub struct CoolingOffConfig {
    /// Duration required for offense count decay
    pub decay_period: Duration,
    
    /// Number of offenses removed per decay period
    pub decay_count: u32,
    
    /// Good behavior bonus (multiplier reduction)
    pub good_behavior_bonus: f64,
}

impl Default for CoolingOffConfig {
    fn default() -> Self {
        Self {
            decay_period: Duration::from_secs(86400 * 7), // 1 week
            decay_count: 1,
            good_behavior_bonus: 0.1,
        }
    }
}

// =============================================================================
// Violation Types
// =============================================================================

/// A quota violation event
#[derive(Debug, Clone)]
pub struct QuotaViolation {
    /// Tenant committing the violation
    pub tenant_id: TenantId,
    
    /// Type of resource violated
    pub resource_type: ResourceType,
    
    /// Quota limit
    pub quota_limit: f64,
    
    /// Actual usage
    pub actual_usage: f64,
    
    /// Duration of violation
    pub violation_duration: Duration,
    
    /// When violation started
    pub started_at: SystemTime,
    
    /// When violation ended (None if ongoing)
    pub ended_at: Option<SystemTime>,
    
    /// Whether this was intentional (burst request)
    pub intentional_burst: bool,
}

/// Resource types that can have quota violations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ResourceType {
    Cpu,
    Memory,
    NetworkBandwidth,
    NetworkRequests,
    StorageCapacity,
    StorageIops,
    ApiRateLimit,
}

impl ResourceType {
    pub fn unit_name(&self) -> &'static str {
        match self {
            ResourceType::Cpu => "cores",
            ResourceType::Memory => "GB",
            ResourceType::NetworkBandwidth => "Mbps",
            ResourceType::NetworkRequests => "req/s",
            ResourceType::StorageCapacity => "GB",
            ResourceType::StorageIops => "IOPS",
            ResourceType::ApiRateLimit => "req/s",
        }
    }
}

// =============================================================================
// Penalty Engine
// =============================================================================

/// Calculates and tracks fairness penalties
pub struct PenaltyEngine {
    config: PenaltyConfig,
    tenant_history: HashMap<TenantId, TenantViolationHistory>,
    active_violations: HashMap<(TenantId, ResourceType), ActiveViolation>,
}

#[derive(Debug, Clone, Default)]
pub struct TenantViolationHistory {
    /// Total violations in lookback window
    pub recent_violations: Vec<ViolationRecord>,
    
    /// Total penalties assessed
    pub total_penalties: TokenAmount,
    
    /// Current repeat offense multiplier
    pub current_multiplier: f64,
    
    /// Last violation time
    pub last_violation: Option<SystemTime>,
    
    /// Last good behavior check
    pub last_decay_check: SystemTime,
    
    /// Consecutive clean periods
    pub clean_periods: u32,
}

#[derive(Debug, Clone)]
pub struct ViolationRecord {
    pub resource_type: ResourceType,
    pub timestamp: SystemTime,
    pub penalty_amount: TokenAmount,
    pub excess_usage: f64,
}

#[derive(Debug, Clone)]
struct ActiveViolation {
    started_at: SystemTime,
    last_check: SystemTime,
    total_excess: f64,
    accumulated_penalty: TokenAmount,
}

impl PenaltyEngine {
    pub fn new(config: PenaltyConfig) -> Self {
        Self {
            config,
            tenant_history: HashMap::new(),
            active_violations: HashMap::new(),
        }
    }
    
    // =========================================================================
    // Penalty Calculation
    // =========================================================================
    
    /// Calculate penalty for a quota violation
    ///
    /// ## Penalty Formula
    ///
    /// ```text
    /// penalty = base_penalty × escalation_factor × repeat_multiplier × duration_factor
    ///
    /// where:
    ///   base_penalty = excess_usage × base_rate
    ///   escalation_factor = min(1.5^active_minutes, max_rate/base_rate)
    ///   repeat_multiplier = 1.5^recent_offense_count (capped at 10x)
    ///   duration_factor = max(1, log2(duration_minutes))
    /// ```
    pub fn calculate_penalty(&self, violation: &QuotaViolation) -> PenaltyCalculation {
        // Step 1: Check grace period
        if violation.violation_duration < self.config.grace_period && !violation.intentional_burst {
            return PenaltyCalculation {
                tenant_id: violation.tenant_id.clone(),
                resource_type: violation.resource_type,
                base_penalty: 0,
                escalation_factor: 1.0,
                repeat_multiplier: 1.0,
                duration_factor: 1.0,
                final_penalty: 0,
                in_grace_period: true,
                warning_issued: true,
            };
        }
        
        // Step 2: Calculate base penalty
        let excess = violation.actual_usage - violation.quota_limit;
        let base_penalty = self.calculate_base_penalty(violation.resource_type, excess);
        
        // Step 3: Calculate escalation factor (for ongoing violations)
        let escalation = self.calculate_escalation_factor(
            violation.resource_type,
            violation.violation_duration,
        );
        
        // Step 4: Get repeat offense multiplier
        let repeat_mult = self.get_repeat_multiplier(&violation.tenant_id);
        
        // Step 5: Calculate duration factor
        let duration_factor = self.calculate_duration_factor(violation.violation_duration);
        
        // Step 6: Combine factors
        let raw_penalty = base_penalty as f64 
            * escalation 
            * repeat_mult 
            * duration_factor;
        
        let final_penalty = (raw_penalty as TokenAmount).max(MIN_PENALTY);
        
        PenaltyCalculation {
            tenant_id: violation.tenant_id.clone(),
            resource_type: violation.resource_type,
            base_penalty,
            escalation_factor: escalation,
            repeat_multiplier: repeat_mult,
            duration_factor,
            final_penalty,
            in_grace_period: false,
            warning_issued: false,
        }
    }
    
    /// Calculate base penalty for resource type
    fn calculate_base_penalty(&self, resource_type: ResourceType, excess: f64) -> TokenAmount {
        let config = match resource_type {
            ResourceType::Cpu => &self.config.cpu_penalties,
            ResourceType::Memory => &self.config.memory_penalties,
            ResourceType::NetworkBandwidth | ResourceType::NetworkRequests => {
                &self.config.network_penalties
            }
            ResourceType::StorageCapacity | ResourceType::StorageIops => {
                &self.config.storage_penalties
            }
            ResourceType::ApiRateLimit => {
                // Special handling for rate limits
                return (excess as TokenAmount) * self.config.rate_limit_penalties.per_request_penalty;
            }
        };
        
        (excess * config.base_rate as f64).max(0.0) as TokenAmount
    }
    
    /// Calculate escalation factor based on violation duration
    ///
    /// ```text
    /// escalation = min(escalation_factor^(minutes), max_rate/base_rate)
    /// ```
    fn calculate_escalation_factor(
        &self,
        resource_type: ResourceType,
        duration: Duration,
    ) -> f64 {
        let config = match resource_type {
            ResourceType::Cpu => &self.config.cpu_penalties,
            ResourceType::Memory => &self.config.memory_penalties,
            ResourceType::NetworkBandwidth | ResourceType::NetworkRequests => {
                &self.config.network_penalties
            }
            ResourceType::StorageCapacity | ResourceType::StorageIops => {
                &self.config.storage_penalties
            }
            ResourceType::ApiRateLimit => {
                return 1.0; // No escalation for rate limits
            }
        };
        
        let minutes = duration.as_secs_f64() / 60.0;
        let max_factor = config.max_rate as f64 / config.base_rate.max(1) as f64;
        
        config.escalation_factor.powf(minutes).min(max_factor)
    }
    
    /// Get repeat offense multiplier for tenant
    fn get_repeat_multiplier(&self, tenant_id: &TenantId) -> f64 {
        self.tenant_history
            .get(tenant_id)
            .map(|h| h.current_multiplier)
            .unwrap_or(1.0)
    }
    
    /// Calculate duration factor
    ///
    /// ```text
    /// duration_factor = max(1, log2(duration_minutes + 1))
    /// ```
    fn calculate_duration_factor(&self, duration: Duration) -> f64 {
        let minutes = duration.as_secs_f64() / 60.0;
        (minutes + 1.0).log2().max(1.0)
    }
    
    // =========================================================================
    // Violation Tracking
    // =========================================================================
    
    /// Record a violation and update tenant history
    pub fn record_violation(
        &mut self,
        violation: &QuotaViolation,
        penalty: &PenaltyCalculation,
    ) {
        let history = self.tenant_history
            .entry(violation.tenant_id.clone())
            .or_insert_with(|| TenantViolationHistory {
                last_decay_check: SystemTime::now(),
                current_multiplier: 1.0,
                ..Default::default()
            });
        
        // Add to recent violations
        history.recent_violations.push(ViolationRecord {
            resource_type: violation.resource_type,
            timestamp: violation.started_at,
            penalty_amount: penalty.final_penalty,
            excess_usage: violation.actual_usage - violation.quota_limit,
        });
        
        // Update totals
        history.total_penalties += penalty.final_penalty;
        history.last_violation = Some(SystemTime::now());
        history.clean_periods = 0;
        
        // Update multiplier for repeat offenses
        self.update_repeat_multiplier(&violation.tenant_id);
        
        // Prune old violations
        self.prune_old_violations(&violation.tenant_id);
    }
    
    /// Update repeat offense multiplier
    fn update_repeat_multiplier(&mut self, tenant_id: &TenantId) {
        if let Some(history) = self.tenant_history.get_mut(tenant_id) {
            let now = SystemTime::now();
            let cutoff = now - self.config.repeat_offense.lookback_window;
            
            let recent_count = history.recent_violations.iter()
                .filter(|v| v.timestamp > cutoff)
                .count();
            
            history.current_multiplier = self.config.repeat_offense
                .multiplier_per_offense
                .powi(recent_count as i32)
                .min(self.config.repeat_offense.max_multiplier);
        }
    }
    
    /// Prune violations outside lookback window
    fn prune_old_violations(&mut self, tenant_id: &TenantId) {
        if let Some(history) = self.tenant_history.get_mut(tenant_id) {
            let cutoff = SystemTime::now() - self.config.repeat_offense.lookback_window;
            history.recent_violations.retain(|v| v.timestamp > cutoff);
        }
    }
    
    // =========================================================================
    // Active Violation Tracking
    // =========================================================================
    
    /// Start tracking an active violation
    pub fn start_active_violation(
        &mut self,
        tenant_id: TenantId,
        resource_type: ResourceType,
    ) {
        let key = (tenant_id, resource_type);
        self.active_violations.insert(key, ActiveViolation {
            started_at: SystemTime::now(),
            last_check: SystemTime::now(),
            total_excess: 0.0,
            accumulated_penalty: 0,
        });
    }
    
    /// Update an active violation with current excess
    pub fn update_active_violation(
        &mut self,
        tenant_id: &TenantId,
        resource_type: ResourceType,
        current_excess: f64,
    ) -> Option<TokenAmount> {
        let key = (tenant_id.clone(), resource_type);
        
        self.active_violations.get_mut(&key).map(|violation| {
            let now = SystemTime::now();
            let interval = now.duration_since(violation.last_check)
                .unwrap_or(Duration::ZERO);
            
            // Calculate incremental penalty
            let incremental_penalty = self.calculate_base_penalty(resource_type, current_excess);
            
            violation.total_excess += current_excess * interval.as_secs_f64();
            violation.accumulated_penalty += incremental_penalty;
            violation.last_check = now;
            
            incremental_penalty
        })
    }
    
    /// End an active violation and return total penalty
    pub fn end_active_violation(
        &mut self,
        tenant_id: &TenantId,
        resource_type: ResourceType,
    ) -> Option<ActiveViolationSummary> {
        let key = (tenant_id.clone(), resource_type);
        
        self.active_violations.remove(&key).map(|violation| {
            let duration = SystemTime::now()
                .duration_since(violation.started_at)
                .unwrap_or(Duration::ZERO);
            
            ActiveViolationSummary {
                tenant_id: tenant_id.clone(),
                resource_type,
                started_at: violation.started_at,
                duration,
                total_excess: violation.total_excess,
                total_penalty: violation.accumulated_penalty,
            }
        })
    }
    
    // =========================================================================
    // Good Behavior & Cooling Off
    // =========================================================================
    
    /// Apply cooling-off decay to tenant history
    pub fn apply_cooling_off(&mut self, tenant_id: &TenantId) {
        if let Some(history) = self.tenant_history.get_mut(tenant_id) {
            let now = SystemTime::now();
            
            // Check if enough time has passed for decay
            let elapsed = now.duration_since(history.last_decay_check)
                .unwrap_or(Duration::ZERO);
            
            if elapsed >= self.config.cooling_off.decay_period {
                let periods = (elapsed.as_secs() / self.config.cooling_off.decay_period.as_secs()) as u32;
                let decay_count = periods * self.config.cooling_off.decay_count;
                
                // Remove old violations
                for _ in 0..decay_count {
                    if !history.recent_violations.is_empty() {
                        history.recent_violations.remove(0);
                    }
                }
                
                // Update multiplier
                history.current_multiplier = (history.current_multiplier 
                    * (1.0 - self.config.cooling_off.good_behavior_bonus * periods as f64))
                    .max(1.0);
                
                history.clean_periods += periods;
                history.last_decay_check = now;
            }
        }
    }
    
    /// Check if tenant should be suspended
    pub fn should_suspend(&self, tenant_id: &TenantId) -> SuspensionCheck {
        self.tenant_history.get(tenant_id).map_or(
            SuspensionCheck {
                should_suspend: false,
                violation_count: 0,
                threshold: self.config.repeat_offense.suspension_threshold,
                recommendation: SuspensionRecommendation::None,
            },
            |history| {
                let count = history.recent_violations.len() as u32;
                let threshold = self.config.repeat_offense.suspension_threshold;
                
                let (should_suspend, recommendation) = if count >= threshold * 2 {
                    (true, SuspensionRecommendation::Terminate)
                } else if count >= threshold {
                    (true, SuspensionRecommendation::Suspend)
                } else if count >= threshold / 2 {
                    (false, SuspensionRecommendation::Warning)
                } else {
                    (false, SuspensionRecommendation::None)
                };
                
                SuspensionCheck {
                    should_suspend,
                    violation_count: count,
                    threshold,
                    recommendation,
                }
            }
        )
    }
    
    // =========================================================================
    // Reporting
    // =========================================================================
    
    /// Get violation report for tenant
    pub fn get_tenant_report(&self, tenant_id: &TenantId) -> Option<TenantViolationReport> {
        self.tenant_history.get(tenant_id).map(|history| {
            let by_type: HashMap<ResourceType, u32> = history.recent_violations.iter()
                .fold(HashMap::new(), |mut acc, v| {
                    *acc.entry(v.resource_type).or_default() += 1;
                    acc
                });
            
            TenantViolationReport {
                tenant_id: tenant_id.clone(),
                total_violations: history.recent_violations.len() as u32,
                total_penalties: history.total_penalties,
                current_multiplier: history.current_multiplier,
                violations_by_type: by_type,
                last_violation: history.last_violation,
                clean_periods: history.clean_periods,
                suspension_check: self.should_suspend(tenant_id),
            }
        })
    }
}

// =============================================================================
// Result Types
// =============================================================================

#[derive(Debug, Clone)]
pub struct PenaltyCalculation {
    pub tenant_id: TenantId,
    pub resource_type: ResourceType,
    pub base_penalty: TokenAmount,
    pub escalation_factor: f64,
    pub repeat_multiplier: f64,
    pub duration_factor: f64,
    pub final_penalty: TokenAmount,
    pub in_grace_period: bool,
    pub warning_issued: bool,
}

#[derive(Debug, Clone)]
pub struct ActiveViolationSummary {
    pub tenant_id: TenantId,
    pub resource_type: ResourceType,
    pub started_at: SystemTime,
    pub duration: Duration,
    pub total_excess: f64,
    pub total_penalty: TokenAmount,
}

#[derive(Debug, Clone)]
pub struct SuspensionCheck {
    pub should_suspend: bool,
    pub violation_count: u32,
    pub threshold: u32,
    pub recommendation: SuspensionRecommendation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SuspensionRecommendation {
    None,
    Warning,
    Suspend,
    Terminate,
}

#[derive(Debug, Clone)]
pub struct TenantViolationReport {
    pub tenant_id: TenantId,
    pub total_violations: u32,
    pub total_penalties: TokenAmount,
    pub current_multiplier: f64,
    pub violations_by_type: HashMap<ResourceType, u32>,
    pub last_violation: Option<SystemTime>,
    pub clean_periods: u32,
    pub suspension_check: SuspensionCheck,
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_violation(
        tenant_id: &str,
        resource_type: ResourceType,
        quota: f64,
        usage: f64,
        duration_secs: u64,
    ) -> QuotaViolation {
        QuotaViolation {
            tenant_id: tenant_id.to_string(),
            resource_type,
            quota_limit: quota,
            actual_usage: usage,
            violation_duration: Duration::from_secs(duration_secs),
            started_at: SystemTime::now() - Duration::from_secs(duration_secs),
            ended_at: Some(SystemTime::now()),
            intentional_burst: false,
        }
    }
    
    #[test]
    fn test_grace_period() {
        let engine = PenaltyEngine::new(PenaltyConfig::default());
        
        // Violation within grace period
        let violation = create_violation("tenant-1", ResourceType::Cpu, 10.0, 12.0, 30);
        let penalty = engine.calculate_penalty(&violation);
        
        assert!(penalty.in_grace_period);
        assert_eq!(penalty.final_penalty, 0);
    }
    
    #[test]
    fn test_base_penalty() {
        let engine = PenaltyEngine::new(PenaltyConfig::default());
        
        // Violation after grace period
        let violation = create_violation("tenant-1", ResourceType::Cpu, 10.0, 12.0, 120);
        let penalty = engine.calculate_penalty(&violation);
        
        assert!(!penalty.in_grace_period);
        assert!(penalty.base_penalty > 0);
        // 2 units over × 10 per unit = 20
        assert_eq!(penalty.base_penalty, 20);
    }
    
    #[test]
    fn test_repeat_multiplier() {
        let mut engine = PenaltyEngine::new(PenaltyConfig::default());
        
        // First violation
        let violation1 = create_violation("tenant-1", ResourceType::Cpu, 10.0, 12.0, 120);
        let penalty1 = engine.calculate_penalty(&violation1);
        engine.record_violation(&violation1, &penalty1);
        
        // Second violation - should have higher multiplier
        let violation2 = create_violation("tenant-1", ResourceType::Memory, 100.0, 120.0, 120);
        let penalty2 = engine.calculate_penalty(&violation2);
        
        assert!(penalty2.repeat_multiplier > 1.0);
    }
    
    #[test]
    fn test_suspension_threshold() {
        let mut engine = PenaltyEngine::new(PenaltyConfig::default());
        let tenant_id = "tenant-1".to_string();
        
        // Record many violations
        for i in 0..15 {
            let violation = create_violation(
                &tenant_id,
                ResourceType::Cpu,
                10.0,
                12.0 + i as f64,
                120,
            );
            let penalty = engine.calculate_penalty(&violation);
            engine.record_violation(&violation, &penalty);
        }
        
        let check = engine.should_suspend(&tenant_id);
        assert!(check.should_suspend);
    }
}
