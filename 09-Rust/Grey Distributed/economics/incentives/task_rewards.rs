//! # Grey Distributed — Task Reward System
//!
//! Incentive mechanisms to reward task completion with fair distribution
//! and quality-based bonuses.
//!
//! ## Reward Formula
//!
//! ```text
//! TaskReward = BaseReward × CompletionMultiplier × QualityBonus × UrgencyBonus
//!
//! Where:
//!   BaseReward           = f(resource_cost, task_complexity)
//!   CompletionMultiplier = f(success_rate, time_performance)
//!   QualityBonus         = f(accuracy, reliability)
//!   UrgencyBonus         = f(deadline_met, priority_level)
//! ```
//!
//! ## Fairness Guarantees
//!
//! - **Proportional Rewards**: Rewards scale with actual contribution
//! - **Quality Incentives**: Higher quality → better rewards
//! - **Anti-Gaming**: Mechanisms to prevent reward exploitation
//! - **Minimum Guarantees**: Floor on rewards to prevent starvation

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type NodeId = String;
pub type TaskId = String;
pub type TokenAmount = f64;

/// Base reward per compute-unit-second
const BASE_REWARD_PER_CUS: TokenAmount = 0.001;

/// Maximum quality bonus multiplier
const MAX_QUALITY_BONUS: f64 = 2.0;

/// Maximum urgency bonus multiplier
const MAX_URGENCY_BONUS: f64 = 1.5;

/// Minimum reward floor (anti-starvation)
const MIN_REWARD_FLOOR: TokenAmount = 0.0001;

/// Grace period for deadline (percentage)
const DEADLINE_GRACE_PERCENT: f64 = 0.10;

// =============================================================================
// Task Types
// =============================================================================

/// Task complexity classification
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskComplexity {
    /// Simple CPU-bound tasks
    Simple,
    /// Standard mixed workloads  
    Standard,
    /// Complex multi-resource tasks
    Complex,
    /// Critical system tasks
    Critical,
}

impl TaskComplexity {
    /// Base multiplier for complexity
    pub fn multiplier(&self) -> f64 {
        match self {
            TaskComplexity::Simple => 0.5,
            TaskComplexity::Standard => 1.0,
            TaskComplexity::Complex => 2.0,
            TaskComplexity::Critical => 3.0,
        }
    }
}

/// Task priority level
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TaskPriority {
    Background = 0,
    Low = 1,
    Normal = 2,
    High = 3,
    Urgent = 4,
    Critical = 5,
}

impl TaskPriority {
    pub fn urgency_multiplier(&self) -> f64 {
        match self {
            TaskPriority::Background => 0.5,
            TaskPriority::Low => 0.75,
            TaskPriority::Normal => 1.0,
            TaskPriority::High => 1.25,
            TaskPriority::Urgent => 1.5,
            TaskPriority::Critical => 2.0,
        }
    }
}

// =============================================================================
// Task Execution Record
// =============================================================================

/// Record of a completed task
#[derive(Debug, Clone)]
pub struct TaskExecutionRecord {
    pub task_id: TaskId,
    pub executor_node: NodeId,
    
    /// Resources consumed
    pub cpu_seconds: f64,
    pub memory_gb_seconds: f64,
    pub gpu_seconds: f64,
    
    /// Task metadata
    pub complexity: TaskComplexity,
    pub priority: TaskPriority,
    
    /// Time tracking
    pub scheduled_at: SystemTime,
    pub started_at: SystemTime,
    pub completed_at: SystemTime,
    pub deadline: Option<SystemTime>,
    
    /// Quality metrics
    pub result_quality: ResultQuality,
    pub retries: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum ResultQuality {
    /// Task failed
    Failed,
    /// Completed with issues
    Degraded,
    /// Completed successfully
    Success,
    /// Exceeded expectations
    Excellent,
}

impl ResultQuality {
    pub fn quality_score(&self) -> f64 {
        match self {
            ResultQuality::Failed => 0.0,
            ResultQuality::Degraded => 0.5,
            ResultQuality::Success => 1.0,
            ResultQuality::Excellent => 1.5,
        }
    }
}

// =============================================================================
// Node Reputation
// =============================================================================

/// Track node reliability for reward adjustment
#[derive(Debug, Clone)]
pub struct NodeReputation {
    pub node_id: NodeId,
    
    /// Historical success rate (0.0 - 1.0)
    pub success_rate: f64,
    
    /// Average task quality (0.0 - 1.5)
    pub avg_quality: f64,
    
    /// Tasks completed
    pub tasks_completed: u64,
    
    /// Uptime percentage
    pub uptime_percent: f64,
    
    /// Reputation score (composite)
    pub reputation_score: f64,
}

impl NodeReputation {
    pub fn new(node_id: NodeId) -> Self {
        Self {
            node_id,
            success_rate: 1.0,       // Start optimistic
            avg_quality: 1.0,
            tasks_completed: 0,
            uptime_percent: 100.0,
            reputation_score: 1.0,
        }
    }
    
    /// Update reputation after task completion
    pub fn update(&mut self, record: &TaskExecutionRecord) {
        let quality = record.result_quality.quality_score();
        let n = self.tasks_completed as f64;
        
        // Exponential moving average for quality
        self.avg_quality = (self.avg_quality * n + quality) / (n + 1.0);
        
        // Update success rate
        let success = if matches!(record.result_quality, ResultQuality::Failed) { 0.0 } else { 1.0 };
        self.success_rate = (self.success_rate * n + success) / (n + 1.0);
        
        self.tasks_completed += 1;
        
        // Recalculate composite score
        self.reputation_score = self.calculate_composite_score();
    }
    
    fn calculate_composite_score(&self) -> f64 {
        let weights = ReputationWeights::default();
        
        (self.success_rate * weights.success_weight)
            + (self.avg_quality * weights.quality_weight)
            + (self.uptime_percent / 100.0 * weights.uptime_weight)
    }
}

struct ReputationWeights {
    success_weight: f64,
    quality_weight: f64,
    uptime_weight: f64,
}

impl Default for ReputationWeights {
    fn default() -> Self {
        Self {
            success_weight: 0.4,
            quality_weight: 0.4,
            uptime_weight: 0.2,
        }
    }
}

// =============================================================================
// Task Reward Engine
// =============================================================================

/// Primary reward calculation engine
pub struct TaskRewardEngine {
    /// Node reputations
    node_reputations: HashMap<NodeId, NodeReputation>,
    
    /// Reward pool (for distribution)
    reward_pool: TokenAmount,
    
    /// Enable anti-gaming measures
    anti_gaming_enabled: bool,
    
    /// Recent rewards for rate limiting
    recent_rewards: Vec<RewardRecord>,
}

#[derive(Debug, Clone)]
struct RewardRecord {
    node_id: NodeId,
    task_id: TaskId,
    amount: TokenAmount,
    timestamp: SystemTime,
}

impl TaskRewardEngine {
    pub fn new(initial_pool: TokenAmount) -> Self {
        Self {
            node_reputations: HashMap::new(),
            reward_pool: initial_pool,
            anti_gaming_enabled: true,
            recent_rewards: Vec::new(),
        }
    }
    
    /// Register new node
    pub fn register_node(&mut self, node_id: NodeId) {
        self.node_reputations
            .entry(node_id.clone())
            .or_insert_with(|| NodeReputation::new(node_id));
    }
    
    /// Calculate reward for completed task
    ///
    /// ## Reward Calculation
    ///
    /// ```text
    /// 1. BaseReward = ResourceCost × Complexity
    /// 2. TimeBonus = f(deadline performance)
    /// 3. QualityBonus = f(result quality, retries)
    /// 4. ReputationMultiplier = f(node reputation)
    /// 5. FinalReward = max(Base × Bonuses × Reputation, MinFloor)
    /// ```
    pub fn calculate_reward(&self, record: &TaskExecutionRecord) -> TaskRewardQuote {
        // Step 1: Calculate base reward from resources
        let resource_cost = self.calculate_resource_cost(record);
        let base_reward = resource_cost * record.complexity.multiplier();
        
        // Step 2: Time performance bonus
        let time_bonus = self.calculate_time_bonus(record);
        
        // Step 3: Quality bonus
        let quality_bonus = self.calculate_quality_bonus(record);
        
        // Step 4: Urgency bonus
        let urgency_bonus = record.priority.urgency_multiplier();
        
        // Step 5: Reputation multiplier
        let reputation = self.get_reputation(&record.executor_node);
        let reputation_multiplier = reputation.map(|r| r.reputation_score).unwrap_or(1.0);
        
        // Step 6: Calculate gross reward
        let gross_reward = base_reward 
            * time_bonus 
            * quality_bonus 
            * urgency_bonus 
            * reputation_multiplier;
        
        // Step 7: Apply anti-gaming adjustments
        let adjusted_reward = if self.anti_gaming_enabled {
            self.apply_anti_gaming(record, gross_reward)
        } else {
            gross_reward
        };
        
        // Step 8: Apply floor
        let final_reward = adjusted_reward.max(MIN_REWARD_FLOOR);
        
        TaskRewardQuote {
            task_id: record.task_id.clone(),
            executor_node: record.executor_node.clone(),
            final_reward,
            breakdown: RewardBreakdown {
                resource_cost,
                base_reward,
                time_bonus,
                quality_bonus,
                urgency_bonus,
                reputation_multiplier,
                gross_reward,
                anti_gaming_adjustment: gross_reward - adjusted_reward,
                floor_applied: adjusted_reward < MIN_REWARD_FLOOR,
            },
        }
    }
    
    /// Distribute reward to node
    pub fn distribute_reward(&mut self, quote: &TaskRewardQuote) -> Result<TokenAmount, RewardError> {
        if quote.final_reward > self.reward_pool {
            return Err(RewardError::InsufficientPool {
                requested: quote.final_reward,
                available: self.reward_pool,
            });
        }
        
        // Deduct from pool
        self.reward_pool -= quote.final_reward;
        
        // Record for anti-gaming
        self.recent_rewards.push(RewardRecord {
            node_id: quote.executor_node.clone(),
            task_id: quote.task_id.clone(),
            amount: quote.final_reward,
            timestamp: SystemTime::now(),
        });
        
        Ok(quote.final_reward)
    }
    
    /// Update node reputation after task
    pub fn record_task_completion(&mut self, record: &TaskExecutionRecord) {
        if let Some(rep) = self.node_reputations.get_mut(&record.executor_node) {
            rep.update(record);
        }
    }
    
    // =========================================================================
    // Calculation Helpers
    // =========================================================================
    
    fn calculate_resource_cost(&self, record: &TaskExecutionRecord) -> TokenAmount {
        // Convert to Compute Unit Seconds (CUS)
        // 1 CUS = 1 CPU-second = 1 GB-second = 0.1 GPU-second
        let cpu_cus = record.cpu_seconds;
        let mem_cus = record.memory_gb_seconds;
        let gpu_cus = record.gpu_seconds * 10.0; // GPUs worth 10x
        
        let total_cus = cpu_cus + mem_cus + gpu_cus;
        total_cus * BASE_REWARD_PER_CUS
    }
    
    fn calculate_time_bonus(&self, record: &TaskExecutionRecord) -> f64 {
        let deadline = match record.deadline {
            Some(d) => d,
            None => return 1.0, // No deadline = no bonus
        };
        
        let deadline_duration = deadline
            .duration_since(record.scheduled_at)
            .unwrap_or(Duration::from_secs(1));
        
        let actual_duration = record.completed_at
            .duration_since(record.scheduled_at)
            .unwrap_or(Duration::from_secs(0));
        
        let deadline_secs = deadline_duration.as_secs_f64();
        let actual_secs = actual_duration.as_secs_f64();
        
        // Grace period
        let grace = deadline_secs * DEADLINE_GRACE_PERCENT;
        let effective_deadline = deadline_secs + grace;
        
        if actual_secs > effective_deadline {
            // Missed deadline - penalty
            0.5
        } else if actual_secs < deadline_secs * 0.5 {
            // Very early - max bonus
            MAX_URGENCY_BONUS
        } else if actual_secs < deadline_secs {
            // On time - proportional bonus
            1.0 + (1.0 - actual_secs / deadline_secs) * (MAX_URGENCY_BONUS - 1.0)
        } else {
            // Within grace period
            0.9
        }
    }
    
    fn calculate_quality_bonus(&self, record: &TaskExecutionRecord) -> f64 {
        let base_quality = record.result_quality.quality_score();
        
        // Penalty for retries
        let retry_penalty = match record.retries {
            0 => 1.0,
            1 => 0.9,
            2 => 0.75,
            _ => 0.5,
        };
        
        let quality = base_quality * retry_penalty;
        
        quality.min(MAX_QUALITY_BONUS)
    }
    
    fn get_reputation(&self, node_id: &NodeId) -> Option<&NodeReputation> {
        self.node_reputations.get(node_id)
    }
    
    fn apply_anti_gaming(&self, record: &TaskExecutionRecord, reward: TokenAmount) -> TokenAmount {
        // Check for suspicious patterns
        let recent_count = self.count_recent_rewards(&record.executor_node, Duration::from_secs(60));
        
        // Rate limiting: diminishing returns for rapid task completion
        if recent_count > 100 {
            let penalty = 1.0 - ((recent_count - 100) as f64 * 0.01).min(0.5);
            reward * penalty
        } else {
            reward
        }
    }
    
    fn count_recent_rewards(&self, node_id: &NodeId, window: Duration) -> usize {
        let cutoff = SystemTime::now()
            .checked_sub(window)
            .unwrap_or(SystemTime::UNIX_EPOCH);
        
        self.recent_rewards
            .iter()
            .filter(|r| r.node_id == *node_id && r.timestamp > cutoff)
            .count()
    }
    
    // =========================================================================
    // Pool Management
    // =========================================================================
    
    /// Add tokens to reward pool
    pub fn add_to_pool(&mut self, amount: TokenAmount) {
        self.reward_pool += amount;
    }
    
    /// Get current pool balance
    pub fn pool_balance(&self) -> TokenAmount {
        self.reward_pool
    }
    
    /// Get node stats
    pub fn get_node_stats(&self, node_id: &NodeId) -> Option<&NodeReputation> {
        self.node_reputations.get(node_id)
    }
}

// =============================================================================
// Output Types
// =============================================================================

#[derive(Debug, Clone)]
pub struct TaskRewardQuote {
    pub task_id: TaskId,
    pub executor_node: NodeId,
    pub final_reward: TokenAmount,
    pub breakdown: RewardBreakdown,
}

#[derive(Debug, Clone)]
pub struct RewardBreakdown {
    pub resource_cost: TokenAmount,
    pub base_reward: TokenAmount,
    pub time_bonus: f64,
    pub quality_bonus: f64,
    pub urgency_bonus: f64,
    pub reputation_multiplier: f64,
    pub gross_reward: TokenAmount,
    pub anti_gaming_adjustment: TokenAmount,
    pub floor_applied: bool,
}

#[derive(Debug, Clone)]
pub enum RewardError {
    InsufficientPool { requested: TokenAmount, available: TokenAmount },
    NodeNotRegistered(NodeId),
    TaskAlreadyRewarded(TaskId),
}

impl Default for TaskRewardEngine {
    fn default() -> Self {
        Self::new(1_000_000.0) // 1M token initial pool
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_test_record(quality: ResultQuality, retries: u32) -> TaskExecutionRecord {
        let now = SystemTime::now();
        TaskExecutionRecord {
            task_id: "task-1".to_string(),
            executor_node: "node-1".to_string(),
            cpu_seconds: 100.0,
            memory_gb_seconds: 50.0,
            gpu_seconds: 0.0,
            complexity: TaskComplexity::Standard,
            priority: TaskPriority::Normal,
            scheduled_at: now - Duration::from_secs(120),
            started_at: now - Duration::from_secs(100),
            completed_at: now,
            deadline: Some(now + Duration::from_secs(60)),
            result_quality: quality,
            retries,
        }
    }
    
    #[test]
    fn test_quality_affects_reward() {
        let engine = TaskRewardEngine::default();
        
        let good = create_test_record(ResultQuality::Excellent, 0);
        let bad = create_test_record(ResultQuality::Degraded, 2);
        
        let good_reward = engine.calculate_reward(&good);
        let bad_reward = engine.calculate_reward(&bad);
        
        assert!(good_reward.final_reward > bad_reward.final_reward);
    }
    
    #[test]
    fn test_reputation_grows() {
        let mut rep = NodeReputation::new("node-1".to_string());
        
        for _ in 0..10 {
            let record = create_test_record(ResultQuality::Excellent, 0);
            rep.update(&record);
        }
        
        assert!(rep.avg_quality > 1.0);
        assert_eq!(rep.tasks_completed, 10);
    }
    
    #[test]
    fn test_minimum_reward_floor() {
        let engine = TaskRewardEngine::default();
        
        let failed = TaskExecutionRecord {
            task_id: "fail".to_string(),
            executor_node: "node".to_string(),
            cpu_seconds: 0.001,
            memory_gb_seconds: 0.001,
            gpu_seconds: 0.0,
            complexity: TaskComplexity::Simple,
            priority: TaskPriority::Background,
            scheduled_at: SystemTime::now(),
            started_at: SystemTime::now(),
            completed_at: SystemTime::now(),
            deadline: None,
            result_quality: ResultQuality::Failed,
            retries: 5,
        };
        
        let reward = engine.calculate_reward(&failed);
        assert!(reward.final_reward >= MIN_REWARD_FLOOR);
    }
}
