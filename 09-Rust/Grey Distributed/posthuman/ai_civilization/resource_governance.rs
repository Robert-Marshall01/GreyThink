//! # AI Civilization Resource Governance
//!
//! Resource governance mechanisms for AI collectives managing computational
//! resources at civilization scale. These protocols ensure fair, efficient,
//! and sustainable resource allocation across potentially billions of AI agents.
//!
//! ## Design Philosophy
//!
//! 1. **Abundance Orientation**: Design for resource abundance, not scarcity
//! 2. **Fair Allocation**: Resources distributed based on contribution and need
//! 3. **Sustainable Growth**: Prevent resource exhaustion through limits
//! 4. **Anti-Hoarding**: Prevent resource concentration and monopolization
//! 5. **Dynamic Adaptation**: Allocation adjusts to changing conditions
//!
//! ## Resource Categories
//!
//! - **Compute**: Processing cycles, GPU/TPU time, quantum qubits
//! - **Memory**: Working memory, persistent storage, cache
//! - **Bandwidth**: Network capacity, inter-collective communication
//! - **Energy**: Power allocation (critical for sustainable operation)
//! - **Attention**: Human attention/oversight (the scarcest resource)

use std::collections::{HashMap, BTreeMap, VecDeque};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

// =============================================================================
// CORE RESOURCE TYPES
// =============================================================================

/// Unique identifier for a resource pool
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ResourcePoolId(pub String);

/// Unique identifier for an AI agent
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AgentId(pub String);

/// Unique identifier for an AI collective
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CollectiveId(pub String);

/// Represents a quantum of computational resources
#[derive(Clone, Debug)]
pub struct ResourceQuantum {
    /// Compute units (normalized)
    pub compute: u64,
    /// Memory in bytes
    pub memory: u64,
    /// Bandwidth in bytes/second
    pub bandwidth: u64,
    /// Energy in joules
    pub energy: u64,
    /// Storage in bytes
    pub storage: u64,
    /// Human attention units (normalized)
    pub attention: u64,
}

impl ResourceQuantum {
    pub fn zero() -> Self {
        Self {
            compute: 0,
            memory: 0,
            bandwidth: 0,
            energy: 0,
            storage: 0,
            attention: 0,
        }
    }

    pub fn add(&self, other: &ResourceQuantum) -> Self {
        Self {
            compute: self.compute.saturating_add(other.compute),
            memory: self.memory.saturating_add(other.memory),
            bandwidth: self.bandwidth.saturating_add(other.bandwidth),
            energy: self.energy.saturating_add(other.energy),
            storage: self.storage.saturating_add(other.storage),
            attention: self.attention.saturating_add(other.attention),
        }
    }

    pub fn subtract(&self, other: &ResourceQuantum) -> Option<Self> {
        Some(Self {
            compute: self.compute.checked_sub(other.compute)?,
            memory: self.memory.checked_sub(other.memory)?,
            bandwidth: self.bandwidth.checked_sub(other.bandwidth)?,
            energy: self.energy.checked_sub(other.energy)?,
            storage: self.storage.checked_sub(other.storage)?,
            attention: self.attention.checked_sub(other.attention)?,
        })
    }

    /// Calculate total value using weighted pricing
    pub fn total_value(&self, pricing: &ResourcePricing) -> f64 {
        self.compute as f64 * pricing.compute_price
            + self.memory as f64 * pricing.memory_price
            + self.bandwidth as f64 * pricing.bandwidth_price
            + self.energy as f64 * pricing.energy_price
            + self.storage as f64 * pricing.storage_price
            + self.attention as f64 * pricing.attention_price
    }
}

#[derive(Clone, Debug)]
pub struct ResourcePricing {
    pub compute_price: f64,
    pub memory_price: f64,
    pub bandwidth_price: f64,
    pub energy_price: f64,
    pub storage_price: f64,
    pub attention_price: f64,
}

impl Default for ResourcePricing {
    fn default() -> Self {
        Self {
            compute_price: 1.0,
            memory_price: 0.5,
            bandwidth_price: 0.3,
            energy_price: 2.0,      // Energy is precious
            storage_price: 0.1,
            attention_price: 100.0,  // Human attention is scarce
        }
    }
}

// =============================================================================
// RESOURCE ACCOUNTS AND ALLOCATIONS
// =============================================================================

/// A resource account for an agent or collective
#[derive(Clone, Debug)]
pub struct ResourceAccount {
    pub owner: AccountOwner,
    /// Current resource balance
    pub balance: ResourceQuantum,
    /// Maximum allowed resources (anti-hoarding limit)
    pub ceiling: ResourceQuantum,
    /// Minimum guaranteed resources (survival floor)
    pub floor: ResourceQuantum,
    /// Reserved for future commitments
    pub reserved: ResourceQuantum,
    /// Contribution score (affects allocation priority)
    pub contribution_score: f64,
    /// Historical usage for prediction
    pub usage_history: VecDeque<UsageRecord>,
    /// Account creation time
    pub created_at: SystemTime,
    /// Last activity time
    pub last_activity: SystemTime,
}

#[derive(Clone, Debug)]
pub enum AccountOwner {
    Agent(AgentId),
    Collective(CollectiveId),
    SharedPool(ResourcePoolId),
}

#[derive(Clone, Debug)]
pub struct UsageRecord {
    pub timestamp: SystemTime,
    pub allocated: ResourceQuantum,
    pub actually_used: ResourceQuantum,
    pub efficiency: f64,
}

/// A request for resource allocation
#[derive(Clone, Debug)]
pub struct AllocationRequest {
    pub requester: AgentId,
    pub request_id: String,
    pub requested: ResourceQuantum,
    pub priority: AllocationPriority,
    pub justification: AllocationJustification,
    pub duration: Duration,
    pub flexible: bool, // Can accept partial allocation
    pub created_at: SystemTime,
}

#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq)]
pub enum AllocationPriority {
    /// Emergency/survival needs
    Critical = 4,
    /// High-value work
    High = 3,
    /// Normal operations
    Normal = 2,
    /// Background/optional work
    Low = 1,
    /// Speculative/experimental
    Speculative = 0,
}

#[derive(Clone, Debug)]
pub struct AllocationJustification {
    /// What the resources will be used for
    pub purpose: String,
    /// Expected output/benefit
    pub expected_value: f64,
    /// How this benefits the collective
    pub collective_benefit: String,
    /// Reasoning chain supporting this request
    pub reasoning: Vec<String>,
}

/// The result of an allocation request
#[derive(Clone, Debug)]
pub struct AllocationResult {
    pub request_id: String,
    pub status: AllocationStatus,
    pub allocated: ResourceQuantum,
    pub valid_until: SystemTime,
    pub conditions: Vec<AllocationCondition>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AllocationStatus {
    /// Full allocation granted
    Granted,
    /// Partial allocation granted
    Partial { reason: String },
    /// Request queued for future allocation
    Queued { position: u32, estimated_wait: Duration },
    /// Request denied
    Denied { reason: String },
    /// Deferred for governance review
    Deferred { reason: String },
}

#[derive(Clone, Debug)]
pub struct AllocationCondition {
    pub condition_type: ConditionType,
    pub description: String,
}

#[derive(Clone, Debug)]
pub enum ConditionType {
    /// Must maintain minimum efficiency
    EfficiencyRequirement { min_efficiency: f64 },
    /// Must report usage
    UsageReporting { interval: Duration },
    /// Must share outputs with collective
    OutputSharing { share_percentage: f64 },
    /// Must release if higher priority needs arise
    PreemptionClause { min_priority: AllocationPriority },
}

// =============================================================================
// RESOURCE GOVERNANCE ENGINE
// =============================================================================

/// Main resource governance engine for AI collectives
pub struct ResourceGovernanceEngine {
    /// The collective this engine governs
    collective_id: CollectiveId,
    /// Total resource pool
    total_resources: ResourceQuantum,
    /// Currently allocated resources
    allocated_resources: ResourceQuantum,
    /// Agent resource accounts
    accounts: HashMap<AgentId, ResourceAccount>,
    /// Pending allocation requests
    pending_requests: VecDeque<AllocationRequest>,
    /// Active allocations
    active_allocations: HashMap<String, Allocation>,
    /// Governance configuration
    config: GovernanceConfig,
    /// Resource pricing (for value calculations)
    pricing: ResourcePricing,
    /// Anti-hoarding policies
    hoarding_detector: HoardingDetector,
    /// Fairness tracker
    fairness_tracker: FairnessTracker,
}

#[derive(Clone, Debug)]
pub struct Allocation {
    pub allocation_id: String,
    pub request: AllocationRequest,
    pub allocated: ResourceQuantum,
    pub start_time: SystemTime,
    pub end_time: SystemTime,
    pub actual_usage: ResourceQuantum,
    pub conditions: Vec<AllocationCondition>,
}

#[derive(Clone, Debug)]
pub struct GovernanceConfig {
    /// Maximum percentage of resources any single agent can hold
    pub max_agent_share: f64,
    /// Minimum guaranteed resources per active agent
    pub guaranteed_minimum: ResourceQuantum,
    /// How often to rebalance allocations
    pub rebalance_interval: Duration,
    /// Whether to allow resource trading between agents
    pub allow_trading: bool,
    /// Whether to enforce efficiency requirements
    pub enforce_efficiency: bool,
    /// Minimum efficiency threshold
    pub min_efficiency: f64,
    /// Whether to preempt low-priority allocations
    pub allow_preemption: bool,
    /// How much resource buffer to maintain
    pub buffer_percentage: f64,
}

impl Default for GovernanceConfig {
    fn default() -> Self {
        Self {
            max_agent_share: 0.05,  // No agent can have >5% of total
            guaranteed_minimum: ResourceQuantum {
                compute: 1000,
                memory: 1_000_000,
                bandwidth: 10_000,
                energy: 100,
                storage: 10_000_000,
                attention: 0,
            },
            rebalance_interval: Duration::from_secs(60),
            allow_trading: true,
            enforce_efficiency: true,
            min_efficiency: 0.7,
            allow_preemption: true,
            buffer_percentage: 0.1,
        }
    }
}

impl ResourceGovernanceEngine {
    /// Create a new resource governance engine
    pub fn new(
        collective_id: CollectiveId,
        total_resources: ResourceQuantum,
        config: GovernanceConfig,
    ) -> Self {
        Self {
            collective_id,
            total_resources,
            allocated_resources: ResourceQuantum::zero(),
            accounts: HashMap::new(),
            pending_requests: VecDeque::new(),
            active_allocations: HashMap::new(),
            config,
            pricing: ResourcePricing::default(),
            hoarding_detector: HoardingDetector::new(),
            fairness_tracker: FairnessTracker::new(),
        }
    }

    /// Register a new agent in the resource system
    pub fn register_agent(&mut self, agent_id: AgentId) -> Result<(), GovernanceError> {
        if self.accounts.contains_key(&agent_id) {
            return Err(GovernanceError::AlreadyRegistered(agent_id));
        }

        // Calculate ceiling based on max_agent_share
        let ceiling = ResourceQuantum {
            compute: (self.total_resources.compute as f64 * self.config.max_agent_share) as u64,
            memory: (self.total_resources.memory as f64 * self.config.max_agent_share) as u64,
            bandwidth: (self.total_resources.bandwidth as f64 * self.config.max_agent_share) as u64,
            energy: (self.total_resources.energy as f64 * self.config.max_agent_share) as u64,
            storage: (self.total_resources.storage as f64 * self.config.max_agent_share) as u64,
            attention: (self.total_resources.attention as f64 * self.config.max_agent_share) as u64,
        };

        let account = ResourceAccount {
            owner: AccountOwner::Agent(agent_id.clone()),
            balance: self.config.guaranteed_minimum.clone(),
            ceiling,
            floor: self.config.guaranteed_minimum.clone(),
            reserved: ResourceQuantum::zero(),
            contribution_score: 1.0,
            usage_history: VecDeque::with_capacity(100),
            created_at: SystemTime::now(),
            last_activity: SystemTime::now(),
        };

        self.accounts.insert(agent_id, account);
        Ok(())
    }

    /// Request resource allocation
    pub fn request_allocation(
        &mut self,
        request: AllocationRequest,
    ) -> Result<AllocationResult, GovernanceError> {
        // Validate requester has an account
        let account = self.accounts.get(&request.requester)
            .ok_or_else(|| GovernanceError::UnknownAgent(request.requester.clone()))?;

        // Check if request exceeds agent's ceiling
        let new_total = account.balance.add(&request.requested);
        if new_total.compute > account.ceiling.compute
            || new_total.memory > account.ceiling.memory
            || new_total.bandwidth > account.ceiling.bandwidth
        {
            return Ok(AllocationResult {
                request_id: request.request_id.clone(),
                status: AllocationStatus::Denied {
                    reason: "Request would exceed maximum allocation ceiling".to_string(),
                },
                allocated: ResourceQuantum::zero(),
                valid_until: SystemTime::now(),
                conditions: vec![],
            });
        }

        // Check if we have available resources
        let available = self.calculate_available_resources();
        
        // Try to allocate
        let result = self.try_allocate(&request, &available);

        // Update fairness tracking
        self.fairness_tracker.record_allocation_attempt(&request, &result);

        result
    }

    fn calculate_available_resources(&self) -> ResourceQuantum {
        // Calculate buffer to maintain
        let buffer = ResourceQuantum {
            compute: (self.total_resources.compute as f64 * self.config.buffer_percentage) as u64,
            memory: (self.total_resources.memory as f64 * self.config.buffer_percentage) as u64,
            bandwidth: (self.total_resources.bandwidth as f64 * self.config.buffer_percentage) as u64,
            energy: (self.total_resources.energy as f64 * self.config.buffer_percentage) as u64,
            storage: (self.total_resources.storage as f64 * self.config.buffer_percentage) as u64,
            attention: (self.total_resources.attention as f64 * self.config.buffer_percentage) as u64,
        };

        self.total_resources
            .subtract(&self.allocated_resources)
            .and_then(|r| r.subtract(&buffer))
            .unwrap_or(ResourceQuantum::zero())
    }

    fn try_allocate(
        &mut self,
        request: &AllocationRequest,
        available: &ResourceQuantum,
    ) -> Result<AllocationResult, GovernanceError> {
        // Check if we can fulfill the full request
        if available.compute >= request.requested.compute
            && available.memory >= request.requested.memory
            && available.bandwidth >= request.requested.bandwidth
            && available.energy >= request.requested.energy
        {
            return self.grant_full_allocation(request);
        }

        // Try partial allocation if flexible
        if request.flexible {
            return self.grant_partial_allocation(request, available);
        }

        // Try preemption if allowed
        if self.config.allow_preemption && request.priority >= AllocationPriority::High {
            if let Some(result) = self.try_preemption(request)? {
                return Ok(result);
            }
        }

        // Queue the request
        self.pending_requests.push_back(request.clone());
        
        Ok(AllocationResult {
            request_id: request.request_id.clone(),
            status: AllocationStatus::Queued {
                position: self.pending_requests.len() as u32,
                estimated_wait: Duration::from_secs(60),
            },
            allocated: ResourceQuantum::zero(),
            valid_until: SystemTime::now() + request.duration,
            conditions: vec![],
        })
    }

    fn grant_full_allocation(
        &mut self,
        request: &AllocationRequest,
    ) -> Result<AllocationResult, GovernanceError> {
        let conditions = self.generate_allocation_conditions(request);

        let allocation = Allocation {
            allocation_id: request.request_id.clone(),
            request: request.clone(),
            allocated: request.requested.clone(),
            start_time: SystemTime::now(),
            end_time: SystemTime::now() + request.duration,
            actual_usage: ResourceQuantum::zero(),
            conditions: conditions.clone(),
        };

        // Update account balance
        if let Some(account) = self.accounts.get_mut(&request.requester) {
            account.balance = account.balance.add(&request.requested);
            account.last_activity = SystemTime::now();
        }

        // Track allocation
        self.allocated_resources = self.allocated_resources.add(&request.requested);
        self.active_allocations.insert(request.request_id.clone(), allocation);

        Ok(AllocationResult {
            request_id: request.request_id.clone(),
            status: AllocationStatus::Granted,
            allocated: request.requested.clone(),
            valid_until: SystemTime::now() + request.duration,
            conditions,
        })
    }

    fn grant_partial_allocation(
        &mut self,
        request: &AllocationRequest,
        available: &ResourceQuantum,
    ) -> Result<AllocationResult, GovernanceError> {
        // Allocate what we can
        let allocated = ResourceQuantum {
            compute: available.compute.min(request.requested.compute),
            memory: available.memory.min(request.requested.memory),
            bandwidth: available.bandwidth.min(request.requested.bandwidth),
            energy: available.energy.min(request.requested.energy),
            storage: available.storage.min(request.requested.storage),
            attention: available.attention.min(request.requested.attention),
        };

        let conditions = self.generate_allocation_conditions(request);

        let allocation = Allocation {
            allocation_id: request.request_id.clone(),
            request: request.clone(),
            allocated: allocated.clone(),
            start_time: SystemTime::now(),
            end_time: SystemTime::now() + request.duration,
            actual_usage: ResourceQuantum::zero(),
            conditions: conditions.clone(),
        };

        // Update account
        if let Some(account) = self.accounts.get_mut(&request.requester) {
            account.balance = account.balance.add(&allocated);
            account.last_activity = SystemTime::now();
        }

        // Track allocation
        self.allocated_resources = self.allocated_resources.add(&allocated);
        self.active_allocations.insert(request.request_id.clone(), allocation);

        let fulfillment = allocated.total_value(&self.pricing) 
            / request.requested.total_value(&self.pricing);

        Ok(AllocationResult {
            request_id: request.request_id.clone(),
            status: AllocationStatus::Partial {
                reason: format!("Fulfilled {:.1}% of request due to resource constraints", fulfillment * 100.0),
            },
            allocated,
            valid_until: SystemTime::now() + request.duration,
            conditions,
        })
    }

    fn try_preemption(
        &mut self,
        request: &AllocationRequest,
    ) -> Result<Option<AllocationResult>, GovernanceError> {
        // Find lower-priority allocations that could be preempted
        let preemptable: Vec<_> = self.active_allocations.values()
            .filter(|a| a.request.priority < request.priority)
            .filter(|a| {
                // Check if this allocation has a preemption clause
                a.conditions.iter().any(|c| matches!(
                    c.condition_type,
                    ConditionType::PreemptionClause { min_priority } 
                        if min_priority <= request.priority
                ))
            })
            .cloned()
            .collect();

        // Calculate total preemptable resources
        let mut total_preemptable = ResourceQuantum::zero();
        for alloc in &preemptable {
            total_preemptable = total_preemptable.add(&alloc.allocated);
        }

        // Check if preemption would provide enough resources
        if total_preemptable.compute >= request.requested.compute
            && total_preemptable.memory >= request.requested.memory
        {
            // Preempt allocations (in a real implementation, would notify affected agents)
            for alloc in preemptable {
                self.release_allocation(&alloc.allocation_id)?;
            }
            
            // Now grant the new allocation
            return Ok(Some(self.grant_full_allocation(request)?));
        }

        Ok(None)
    }

    fn generate_allocation_conditions(&self, request: &AllocationRequest) -> Vec<AllocationCondition> {
        let mut conditions = Vec::new();

        // Efficiency requirement
        if self.config.enforce_efficiency {
            conditions.push(AllocationCondition {
                condition_type: ConditionType::EfficiencyRequirement {
                    min_efficiency: self.config.min_efficiency,
                },
                description: format!(
                    "Must maintain at least {:.0}% resource efficiency",
                    self.config.min_efficiency * 100.0
                ),
            });
        }

        // Usage reporting
        conditions.push(AllocationCondition {
            condition_type: ConditionType::UsageReporting {
                interval: Duration::from_secs(60),
            },
            description: "Must report usage every 60 seconds".to_string(),
        });

        // Preemption clause for lower-priority allocations
        if request.priority < AllocationPriority::Critical {
            conditions.push(AllocationCondition {
                condition_type: ConditionType::PreemptionClause {
                    min_priority: AllocationPriority::Critical,
                },
                description: "May be preempted for critical priority requests".to_string(),
            });
        }

        conditions
    }

    /// Release an allocation back to the pool
    pub fn release_allocation(&mut self, allocation_id: &str) -> Result<(), GovernanceError> {
        let allocation = self.active_allocations.remove(allocation_id)
            .ok_or_else(|| GovernanceError::AllocationNotFound(allocation_id.to_string()))?;

        // Update account balance
        if let Some(account) = self.accounts.get_mut(&allocation.request.requester) {
            if let Some(new_balance) = account.balance.subtract(&allocation.allocated) {
                account.balance = new_balance;
            }

            // Record usage history
            let efficiency = if allocation.allocated.compute > 0 {
                allocation.actual_usage.compute as f64 / allocation.allocated.compute as f64
            } else {
                1.0
            };

            account.usage_history.push_back(UsageRecord {
                timestamp: SystemTime::now(),
                allocated: allocation.allocated.clone(),
                actually_used: allocation.actual_usage,
                efficiency,
            });

            // Trim history
            while account.usage_history.len() > 100 {
                account.usage_history.pop_front();
            }
        }

        // Return to available pool
        if let Some(new_allocated) = self.allocated_resources.subtract(&allocation.allocated) {
            self.allocated_resources = new_allocated;
        }

        // Process pending requests
        self.process_pending_queue();

        Ok(())
    }

    fn process_pending_queue(&mut self) {
        let available = self.calculate_available_resources();
        
        // Sort by priority and creation time
        let mut pending: Vec<_> = self.pending_requests.drain(..).collect();
        pending.sort_by(|a, b| {
            b.priority.cmp(&a.priority)
                .then(a.created_at.cmp(&b.created_at))
        });

        let mut remaining_available = available;

        for request in pending {
            if remaining_available.compute >= request.requested.compute
                && remaining_available.memory >= request.requested.memory
            {
                // Can fulfill this request
                if let Ok(result) = self.grant_full_allocation(&request) {
                    if let Some(new_available) = remaining_available.subtract(&request.requested) {
                        remaining_available = new_available;
                    }
                }
            } else if request.flexible {
                // Try partial
                let _ = self.grant_partial_allocation(&request, &remaining_available);
            } else {
                // Re-queue
                self.pending_requests.push_back(request);
            }
        }
    }

    /// Perform periodic rebalancing of resources
    pub fn rebalance(&mut self) -> RebalanceReport {
        let mut report = RebalanceReport {
            timestamp: SystemTime::now(),
            reclaimed_resources: ResourceQuantum::zero(),
            redistributed_resources: ResourceQuantum::zero(),
            efficiency_warnings: Vec::new(),
            hoarding_warnings: Vec::new(),
        };

        // Check for hoarding
        let hoarding_results = self.hoarding_detector.detect(&self.accounts);
        report.hoarding_warnings = hoarding_results;

        // Check for inefficient allocations
        for (agent_id, account) in &self.accounts {
            let avg_efficiency = self.calculate_average_efficiency(account);
            if avg_efficiency < self.config.min_efficiency {
                report.efficiency_warnings.push(EfficiencyWarning {
                    agent: agent_id.clone(),
                    efficiency: avg_efficiency,
                    threshold: self.config.min_efficiency,
                });
            }
        }

        // Reclaim expired allocations
        let now = SystemTime::now();
        let expired: Vec<_> = self.active_allocations.iter()
            .filter(|(_, a)| a.end_time <= now)
            .map(|(id, _)| id.clone())
            .collect();

        for id in expired {
            if let Some(alloc) = self.active_allocations.remove(&id) {
                report.reclaimed_resources = report.reclaimed_resources.add(&alloc.allocated);
            }
        }

        // Update fairness metrics
        self.fairness_tracker.update_metrics(&self.accounts);

        report
    }

    fn calculate_average_efficiency(&self, account: &ResourceAccount) -> f64 {
        if account.usage_history.is_empty() {
            return 1.0;
        }

        let sum: f64 = account.usage_history.iter()
            .map(|r| r.efficiency)
            .sum();
        
        sum / account.usage_history.len() as f64
    }

    /// Get fairness metrics for governance review
    pub fn get_fairness_metrics(&self) -> FairnessMetrics {
        self.fairness_tracker.get_metrics()
    }
}

// =============================================================================
// ANTI-HOARDING DETECTION
// =============================================================================

/// Detects and prevents resource hoarding
pub struct HoardingDetector {
    /// Threshold for hoarding detection
    hoarding_threshold: f64,
    /// Minimum usage for allocated resources
    min_usage_ratio: f64,
}

impl HoardingDetector {
    pub fn new() -> Self {
        Self {
            hoarding_threshold: 0.05,  // Holding >5% of total indicates possible hoarding
            min_usage_ratio: 0.5,      // Must use at least 50% of allocated resources
        }
    }

    pub fn detect(&self, accounts: &HashMap<AgentId, ResourceAccount>) -> Vec<HoardingWarning> {
        let mut warnings = Vec::new();

        // Calculate total allocated
        let total: u64 = accounts.values()
            .map(|a| a.balance.compute)
            .sum();

        for (agent_id, account) in accounts {
            let share = account.balance.compute as f64 / total.max(1) as f64;
            
            if share > self.hoarding_threshold {
                // Check if they're actually using it
                let avg_usage = if account.usage_history.is_empty() {
                    0.0
                } else {
                    account.usage_history.iter()
                        .map(|r| r.actually_used.compute as f64 / r.allocated.compute.max(1) as f64)
                        .sum::<f64>() / account.usage_history.len() as f64
                };

                if avg_usage < self.min_usage_ratio {
                    warnings.push(HoardingWarning {
                        agent: agent_id.clone(),
                        resource_share: share,
                        usage_ratio: avg_usage,
                        recommendation: HoardingRecommendation::ReduceAllocation {
                            target_share: self.hoarding_threshold * 0.8,
                        },
                    });
                }
            }
        }

        warnings
    }
}

#[derive(Clone, Debug)]
pub struct HoardingWarning {
    pub agent: AgentId,
    pub resource_share: f64,
    pub usage_ratio: f64,
    pub recommendation: HoardingRecommendation,
}

#[derive(Clone, Debug)]
pub enum HoardingRecommendation {
    /// Reduce allocation to target share
    ReduceAllocation { target_share: f64 },
    /// Increase activity to justify allocation
    IncreaseActivity,
    /// Share resources with other agents
    ShareResources { suggested_recipients: Vec<AgentId> },
}

// =============================================================================
// FAIRNESS TRACKING
// =============================================================================

/// Tracks fairness in resource allocation
pub struct FairnessTracker {
    /// Allocation success rates per agent
    success_rates: HashMap<AgentId, AllocationSuccessRate>,
    /// Gini coefficient over time
    gini_history: VecDeque<f64>,
    /// Most recent fairness metrics
    current_metrics: FairnessMetrics,
}

#[derive(Clone, Debug)]
pub struct AllocationSuccessRate {
    pub total_requests: u64,
    pub granted: u64,
    pub partial: u64,
    pub denied: u64,
}

#[derive(Clone, Debug)]
pub struct FairnessMetrics {
    /// Gini coefficient (0 = perfect equality, 1 = perfect inequality)
    pub gini_coefficient: f64,
    /// Standard deviation of resource allocation
    pub allocation_std_dev: f64,
    /// Percentage of agents above minimum threshold
    pub agents_above_minimum: f64,
    /// Average request success rate
    pub avg_success_rate: f64,
    /// Whether the system is considered fair
    pub is_fair: bool,
}

impl FairnessTracker {
    pub fn new() -> Self {
        Self {
            success_rates: HashMap::new(),
            gini_history: VecDeque::with_capacity(100),
            current_metrics: FairnessMetrics {
                gini_coefficient: 0.0,
                allocation_std_dev: 0.0,
                agents_above_minimum: 1.0,
                avg_success_rate: 1.0,
                is_fair: true,
            },
        }
    }

    pub fn record_allocation_attempt(
        &mut self,
        request: &AllocationRequest,
        result: &Result<AllocationResult, GovernanceError>,
    ) {
        let rate = self.success_rates
            .entry(request.requester.clone())
            .or_insert(AllocationSuccessRate {
                total_requests: 0,
                granted: 0,
                partial: 0,
                denied: 0,
            });

        rate.total_requests += 1;

        if let Ok(r) = result {
            match &r.status {
                AllocationStatus::Granted => rate.granted += 1,
                AllocationStatus::Partial { .. } => rate.partial += 1,
                AllocationStatus::Denied { .. } => rate.denied += 1,
                _ => {}
            }
        }
    }

    pub fn update_metrics(&mut self, accounts: &HashMap<AgentId, ResourceAccount>) {
        let allocations: Vec<f64> = accounts.values()
            .map(|a| a.balance.compute as f64)
            .collect();

        if allocations.is_empty() {
            return;
        }

        // Calculate Gini coefficient
        let gini = self.calculate_gini(&allocations);
        self.gini_history.push_back(gini);
        while self.gini_history.len() > 100 {
            self.gini_history.pop_front();
        }

        // Calculate standard deviation
        let mean: f64 = allocations.iter().sum::<f64>() / allocations.len() as f64;
        let variance: f64 = allocations.iter()
            .map(|x| (x - mean).powi(2))
            .sum::<f64>() / allocations.len() as f64;
        let std_dev = variance.sqrt();

        // Calculate success rate
        let total_requests: u64 = self.success_rates.values().map(|r| r.total_requests).sum();
        let total_granted: u64 = self.success_rates.values().map(|r| r.granted + r.partial).sum();
        let success_rate = if total_requests > 0 {
            total_granted as f64 / total_requests as f64
        } else {
            1.0
        };

        self.current_metrics = FairnessMetrics {
            gini_coefficient: gini,
            allocation_std_dev: std_dev,
            agents_above_minimum: 1.0, // Would need minimum threshold to calculate
            avg_success_rate: success_rate,
            is_fair: gini < 0.3 && success_rate > 0.8,
        };
    }

    fn calculate_gini(&self, values: &[f64]) -> f64 {
        if values.is_empty() {
            return 0.0;
        }

        let n = values.len() as f64;
        let mean = values.iter().sum::<f64>() / n;

        if mean == 0.0 {
            return 0.0;
        }

        let mut sum_diff = 0.0;
        for (i, &x) in values.iter().enumerate() {
            for &y in values.iter().skip(i + 1) {
                sum_diff += (x - y).abs();
            }
        }

        sum_diff / (n * n * mean)
    }

    pub fn get_metrics(&self) -> FairnessMetrics {
        self.current_metrics.clone()
    }
}

// =============================================================================
// RESOURCE TRADING
// =============================================================================

/// Enables resource trading between agents
pub struct ResourceMarket {
    /// Active trade offers
    offers: HashMap<String, TradeOffer>,
    /// Completed trades
    trade_history: VecDeque<CompletedTrade>,
    /// Market configuration
    config: MarketConfig,
}

#[derive(Clone, Debug)]
pub struct TradeOffer {
    pub offer_id: String,
    pub offerer: AgentId,
    pub offering: ResourceQuantum,
    pub requesting: ResourceQuantum,
    pub expires_at: SystemTime,
}

#[derive(Clone, Debug)]
pub struct CompletedTrade {
    pub trade_id: String,
    pub party_a: AgentId,
    pub party_b: AgentId,
    pub a_gave: ResourceQuantum,
    pub b_gave: ResourceQuantum,
    pub completed_at: SystemTime,
}

#[derive(Clone, Debug)]
pub struct MarketConfig {
    /// Maximum offer duration
    pub max_offer_duration: Duration,
    /// Transaction fee (percentage)
    pub transaction_fee: f64,
    /// Minimum trade value
    pub min_trade_value: f64,
}

// =============================================================================
// REPORTS
// =============================================================================

#[derive(Clone, Debug)]
pub struct RebalanceReport {
    pub timestamp: SystemTime,
    pub reclaimed_resources: ResourceQuantum,
    pub redistributed_resources: ResourceQuantum,
    pub efficiency_warnings: Vec<EfficiencyWarning>,
    pub hoarding_warnings: Vec<HoardingWarning>,
}

#[derive(Clone, Debug)]
pub struct EfficiencyWarning {
    pub agent: AgentId,
    pub efficiency: f64,
    pub threshold: f64,
}

// =============================================================================
// ERRORS
// =============================================================================

#[derive(Debug)]
pub enum GovernanceError {
    AlreadyRegistered(AgentId),
    UnknownAgent(AgentId),
    AllocationNotFound(String),
    InsufficientResources { requested: ResourceQuantum, available: ResourceQuantum },
    CeilingExceeded { agent: AgentId, resource: String },
    InvalidRequest(String),
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_engine() -> ResourceGovernanceEngine {
        let total = ResourceQuantum {
            compute: 1_000_000,
            memory: 1_000_000_000,
            bandwidth: 100_000_000,
            energy: 1_000_000,
            storage: 10_000_000_000,
            attention: 100,
        };

        ResourceGovernanceEngine::new(
            CollectiveId("test".into()),
            total,
            GovernanceConfig::default(),
        )
    }

    #[test]
    fn test_agent_registration() {
        let mut engine = create_test_engine();
        assert!(engine.register_agent(AgentId("agent-1".into())).is_ok());
        assert!(engine.register_agent(AgentId("agent-1".into())).is_err());
    }

    #[test]
    fn test_allocation_request() {
        let mut engine = create_test_engine();
        engine.register_agent(AgentId("agent-1".into())).unwrap();

        let request = AllocationRequest {
            requester: AgentId("agent-1".into()),
            request_id: "req-1".into(),
            requested: ResourceQuantum {
                compute: 1000,
                memory: 1_000_000,
                bandwidth: 10_000,
                energy: 100,
                storage: 1_000_000,
                attention: 0,
            },
            priority: AllocationPriority::Normal,
            justification: AllocationJustification {
                purpose: "Testing".into(),
                expected_value: 100.0,
                collective_benefit: "Test coverage".into(),
                reasoning: vec!["Need resources for testing".into()],
            },
            duration: Duration::from_secs(3600),
            flexible: true,
            created_at: SystemTime::now(),
        };

        let result = engine.request_allocation(request).unwrap();
        assert!(matches!(result.status, AllocationStatus::Granted));
    }
}
