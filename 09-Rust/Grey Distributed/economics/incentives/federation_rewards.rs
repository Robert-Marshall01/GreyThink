//! # Grey Distributed — Federation Rewards
//!
//! This module implements incentive mechanisms for cross-cluster cooperation,
//! encouraging federated clusters to share resources and collaborate effectively.
//!
//! ## Reward Model
//!
//! Federation rewards incentivize:
//! - **Resource sharing**: Clusters that share capacity with partners
//! - **Attestation participation**: Nodes that verify other clusters
//! - **Network contribution**: High-quality cross-cluster connectivity
//! - **Uptime bonuses**: Reliable federation endpoint availability

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type ClusterId = String;
pub type NodeId = String;
pub type TokenAmount = u64;

/// Base reward per resource unit shared per hour
const BASE_SHARING_REWARD_PER_HOUR: TokenAmount = 100;

/// Attestation verification reward
const ATTESTATION_REWARD: TokenAmount = 10;

/// Minimum uptime for bonus eligibility
const MIN_UPTIME_FOR_BONUS: f64 = 0.99;

/// Maximum federation reward multiplier
const MAX_REWARD_MULTIPLIER: f64 = 3.0;

// =============================================================================
// Federation Reward Configuration
// =============================================================================

/// Configuration for federation rewards
#[derive(Debug, Clone)]
pub struct FederationRewardConfig {
    /// Resource sharing rewards
    pub resource_sharing: ResourceSharingConfig,
    
    /// Attestation rewards
    pub attestation: AttestationRewardConfig,
    
    /// Network contribution rewards
    pub network: NetworkRewardConfig,
    
    /// Uptime bonuses
    pub uptime: UptimeRewardConfig,
    
    /// Cooperation multipliers
    pub cooperation: CooperationMultiplierConfig,
}

impl Default for FederationRewardConfig {
    fn default() -> Self {
        Self {
            resource_sharing: ResourceSharingConfig::default(),
            attestation: AttestationRewardConfig::default(),
            network: NetworkRewardConfig::default(),
            uptime: UptimeRewardConfig::default(),
            cooperation: CooperationMultiplierConfig::default(),
        }
    }
}

/// Resource sharing reward configuration
#[derive(Debug, Clone)]
pub struct ResourceSharingConfig {
    /// Reward per CPU-hour shared
    pub cpu_reward_per_hour: TokenAmount,
    
    /// Reward per GB-hour of memory shared
    pub memory_reward_per_gb_hour: TokenAmount,
    
    /// Reward per GB of storage shared per day
    pub storage_reward_per_gb_day: TokenAmount,
    
    /// Reward per GB of bandwidth provided
    pub bandwidth_reward_per_gb: TokenAmount,
    
    /// Urgency multiplier for on-demand sharing
    pub urgency_multiplier: f64,
}

impl Default for ResourceSharingConfig {
    fn default() -> Self {
        Self {
            cpu_reward_per_hour: 50,
            memory_reward_per_gb_hour: 20,
            storage_reward_per_gb_day: 5,
            bandwidth_reward_per_gb: 10,
            urgency_multiplier: 1.5,
        }
    }
}

/// Attestation reward configuration
#[derive(Debug, Clone)]
pub struct AttestationRewardConfig {
    /// Reward for participating in attestation verification
    pub verification_reward: TokenAmount,
    
    /// Bonus for being a verification leader
    pub leader_bonus: TokenAmount,
    
    /// Reward for successful challenge response
    pub challenge_response_reward: TokenAmount,
    
    /// Penalty for failed attestation
    pub attestation_failure_penalty: TokenAmount,
}

impl Default for AttestationRewardConfig {
    fn default() -> Self {
        Self {
            verification_reward: 10,
            leader_bonus: 25,
            challenge_response_reward: 15,
            attestation_failure_penalty: 100,
        }
    }
}

/// Network contribution reward configuration
#[derive(Debug, Clone)]
pub struct NetworkRewardConfig {
    /// Reward for maintaining low-latency connection
    pub low_latency_bonus: TokenAmount,
    
    /// Latency threshold for bonus (ms)
    pub latency_threshold_ms: u64,
    
    /// Reward for high throughput capacity
    pub throughput_bonus_per_gbps: TokenAmount,
    
    /// Penalty for connection instability
    pub instability_penalty: TokenAmount,
}

impl Default for NetworkRewardConfig {
    fn default() -> Self {
        Self {
            low_latency_bonus: 30,
            latency_threshold_ms: 50,
            throughput_bonus_per_gbps: 20,
            instability_penalty: 50,
        }
    }
}

/// Uptime reward configuration
#[derive(Debug, Clone)]
pub struct UptimeRewardConfig {
    /// Base uptime bonus percentage
    pub base_bonus_percent: f64,
    
    /// Threshold for bonus (0.99 = 99%)
    pub bonus_threshold: f64,
    
    /// Additional bonus per 0.1% above threshold
    pub per_tenth_percent_bonus: TokenAmount,
    
    /// Maximum uptime bonus
    pub max_bonus: TokenAmount,
}

impl Default for UptimeRewardConfig {
    fn default() -> Self {
        Self {
            base_bonus_percent: 0.1,
            bonus_threshold: 0.99,
            per_tenth_percent_bonus: 10,
            max_bonus: 500,
        }
    }
}

/// Cooperation multiplier configuration
#[derive(Debug, Clone)]
pub struct CooperationMultiplierConfig {
    /// Multiplier increase per successful cooperation
    pub success_increment: f64,
    
    /// Multiplier decrease per failed cooperation
    pub failure_decrement: f64,
    
    /// Maximum multiplier
    pub max_multiplier: f64,
    
    /// Minimum multiplier
    pub min_multiplier: f64,
    
    /// Decay rate per day of inactivity
    pub inactivity_decay: f64,
}

impl Default for CooperationMultiplierConfig {
    fn default() -> Self {
        Self {
            success_increment: 0.05,
            failure_decrement: 0.1,
            max_multiplier: MAX_REWARD_MULTIPLIER,
            min_multiplier: 0.5,
            inactivity_decay: 0.01,
        }
    }
}

// =============================================================================
// Federation Activity Records
// =============================================================================

/// Record of resource sharing activity
#[derive(Debug, Clone)]
pub struct ResourceSharingRecord {
    /// Provider cluster
    pub provider_cluster: ClusterId,
    
    /// Consumer cluster
    pub consumer_cluster: ClusterId,
    
    /// Resources shared
    pub resources: SharedResources,
    
    /// Sharing period
    pub started_at: SystemTime,
    pub ended_at: Option<SystemTime>,
    
    /// Was this urgent (on-demand) sharing
    pub urgent: bool,
    
    /// Quality of service provided
    pub qos_score: f64,
}

#[derive(Debug, Clone)]
pub struct SharedResources {
    pub cpu_hours: f64,
    pub memory_gb_hours: f64,
    pub storage_gb_days: f64,
    pub bandwidth_gb: f64,
}

/// Record of attestation participation
#[derive(Debug, Clone)]
pub struct AttestationRecord {
    /// Cluster being attested
    pub attested_cluster: ClusterId,
    
    /// Clusters participating in verification
    pub verifier_clusters: Vec<ClusterId>,
    
    /// Leader cluster
    pub leader_cluster: ClusterId,
    
    /// Attestation result
    pub result: AttestationResult,
    
    /// Timestamp
    pub timestamp: SystemTime,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttestationResult {
    Verified,
    Failed,
    Inconclusive,
}

/// Record of network contribution
#[derive(Debug, Clone)]
pub struct NetworkContributionRecord {
    /// Cluster providing network service
    pub provider_cluster: ClusterId,
    
    /// Target clusters
    pub target_clusters: Vec<ClusterId>,
    
    /// Average latency (ms)
    pub avg_latency_ms: f64,
    
    /// Measured throughput (Gbps)
    pub throughput_gbps: f64,
    
    /// Uptime percentage
    pub uptime_percent: f64,
    
    /// Connection stability score (0-1)
    pub stability_score: f64,
    
    /// Measurement period
    pub period_start: SystemTime,
    pub period_end: SystemTime,
}

// =============================================================================
// Federation Reward Calculator
// =============================================================================

/// Calculates rewards for federation activities
pub struct FederationRewardCalculator {
    config: FederationRewardConfig,
    cluster_scores: HashMap<ClusterId, ClusterCooperationScore>,
}

#[derive(Debug, Clone)]
pub struct ClusterCooperationScore {
    /// Current cooperation multiplier
    pub multiplier: f64,
    
    /// Total successful cooperations
    pub successful_cooperations: u64,
    
    /// Total failed cooperations
    pub failed_cooperations: u64,
    
    /// Total rewards earned
    pub total_rewards: TokenAmount,
    
    /// Last activity timestamp
    pub last_activity: SystemTime,
}

impl Default for ClusterCooperationScore {
    fn default() -> Self {
        Self {
            multiplier: 1.0,
            successful_cooperations: 0,
            failed_cooperations: 0,
            total_rewards: 0,
            last_activity: SystemTime::now(),
        }
    }
}

impl FederationRewardCalculator {
    pub fn new(config: FederationRewardConfig) -> Self {
        Self {
            config,
            cluster_scores: HashMap::new(),
        }
    }
    
    // =========================================================================
    // Resource Sharing Rewards
    // =========================================================================
    
    /// Calculate reward for resource sharing
    ///
    /// ## Formula
    ///
    /// ```text
    /// reward = (cpu_reward + memory_reward + storage_reward + bandwidth_reward)
    ///        × urgency_factor
    ///        × qos_factor
    ///        × cooperation_multiplier
    ///
    /// where:
    ///   cpu_reward = cpu_hours × cpu_reward_per_hour
    ///   memory_reward = memory_gb_hours × memory_reward_per_gb_hour
    ///   storage_reward = storage_gb_days × storage_reward_per_gb_day
    ///   bandwidth_reward = bandwidth_gb × bandwidth_reward_per_gb
    ///   urgency_factor = 1.5 if urgent else 1.0
    ///   qos_factor = qos_score (0-1 normalized)
    /// ```
    pub fn calculate_sharing_reward(
        &self,
        record: &ResourceSharingRecord,
    ) -> SharingRewardCalculation {
        let cfg = &self.config.resource_sharing;
        
        // Calculate component rewards
        let cpu_reward = (record.resources.cpu_hours * cfg.cpu_reward_per_hour as f64) as TokenAmount;
        let memory_reward = (record.resources.memory_gb_hours * cfg.memory_reward_per_gb_hour as f64) as TokenAmount;
        let storage_reward = (record.resources.storage_gb_days * cfg.storage_reward_per_gb_day as f64) as TokenAmount;
        let bandwidth_reward = (record.resources.bandwidth_gb * cfg.bandwidth_reward_per_gb as f64) as TokenAmount;
        
        let base_reward = cpu_reward + memory_reward + storage_reward + bandwidth_reward;
        
        // Apply factors
        let urgency_factor = if record.urgent { cfg.urgency_multiplier } else { 1.0 };
        let qos_factor = record.qos_score.clamp(0.5, 1.5); // QoS affects reward ±50%
        let multiplier = self.get_cooperation_multiplier(&record.provider_cluster);
        
        let final_reward = (base_reward as f64 * urgency_factor * qos_factor * multiplier) as TokenAmount;
        
        SharingRewardCalculation {
            provider_cluster: record.provider_cluster.clone(),
            consumer_cluster: record.consumer_cluster.clone(),
            cpu_reward,
            memory_reward,
            storage_reward,
            bandwidth_reward,
            base_reward,
            urgency_factor,
            qos_factor,
            cooperation_multiplier: multiplier,
            final_reward,
        }
    }
    
    // =========================================================================
    // Attestation Rewards
    // =========================================================================
    
    /// Calculate rewards for attestation participation
    ///
    /// ## Reward Distribution
    ///
    /// - Leader: verification_reward + leader_bonus
    /// - Verifiers: verification_reward each
    /// - If successful: challenge_response_reward to attested cluster
    /// - If failed: penalty to attested cluster
    pub fn calculate_attestation_rewards(
        &self,
        record: &AttestationRecord,
    ) -> AttestationRewardCalculation {
        let cfg = &self.config.attestation;
        
        let mut cluster_rewards: HashMap<ClusterId, TokenAmount> = HashMap::new();
        
        // Leader reward
        let leader_reward = cfg.verification_reward + cfg.leader_bonus;
        cluster_rewards.insert(record.leader_cluster.clone(), leader_reward);
        
        // Verifier rewards
        for verifier in &record.verifier_clusters {
            if verifier != &record.leader_cluster {
                *cluster_rewards.entry(verifier.clone()).or_default() += cfg.verification_reward;
            }
        }
        
        // Attested cluster reward/penalty
        let attested_reward = match record.result {
            AttestationResult::Verified => cfg.challenge_response_reward as i64,
            AttestationResult::Failed => -(cfg.attestation_failure_penalty as i64),
            AttestationResult::Inconclusive => 0,
        };
        
        AttestationRewardCalculation {
            attested_cluster: record.attested_cluster.clone(),
            leader_cluster: record.leader_cluster.clone(),
            verifier_count: record.verifier_clusters.len(),
            result: record.result.clone(),
            cluster_rewards,
            attested_cluster_adjustment: attested_reward,
            total_rewards_distributed: leader_reward 
                + (cfg.verification_reward * (record.verifier_clusters.len() - 1) as u64),
        }
    }
    
    // =========================================================================
    // Network Contribution Rewards
    // =========================================================================
    
    /// Calculate rewards for network contribution
    ///
    /// ## Formula
    ///
    /// ```text
    /// reward = latency_bonus + throughput_bonus - stability_penalty
    ///
    /// where:
    ///   latency_bonus = low_latency_bonus if avg_latency < threshold
    ///   throughput_bonus = throughput_gbps × throughput_bonus_per_gbps
    ///   stability_penalty = instability_penalty × (1 - stability_score)
    /// ```
    pub fn calculate_network_reward(
        &self,
        record: &NetworkContributionRecord,
    ) -> NetworkRewardCalculation {
        let cfg = &self.config.network;
        
        // Latency bonus
        let latency_bonus = if record.avg_latency_ms <= cfg.latency_threshold_ms as f64 {
            cfg.low_latency_bonus
        } else {
            0
        };
        
        // Throughput bonus
        let throughput_bonus = (record.throughput_gbps * cfg.throughput_bonus_per_gbps as f64) as TokenAmount;
        
        // Stability penalty
        let instability = 1.0 - record.stability_score;
        let stability_penalty = (instability * cfg.instability_penalty as f64) as TokenAmount;
        
        // Uptime bonus
        let uptime_bonus = self.calculate_uptime_bonus(record.uptime_percent);
        
        let gross_reward = latency_bonus + throughput_bonus + uptime_bonus;
        let final_reward = gross_reward.saturating_sub(stability_penalty);
        
        let multiplier = self.get_cooperation_multiplier(&record.provider_cluster);
        let adjusted_reward = (final_reward as f64 * multiplier) as TokenAmount;
        
        NetworkRewardCalculation {
            provider_cluster: record.provider_cluster.clone(),
            target_clusters: record.target_clusters.clone(),
            latency_bonus,
            throughput_bonus,
            uptime_bonus,
            stability_penalty,
            gross_reward,
            cooperation_multiplier: multiplier,
            final_reward: adjusted_reward,
        }
    }
    
    /// Calculate uptime bonus
    fn calculate_uptime_bonus(&self, uptime_percent: f64) -> TokenAmount {
        let cfg = &self.config.uptime;
        
        if uptime_percent < cfg.bonus_threshold {
            return 0;
        }
        
        let above_threshold = uptime_percent - cfg.bonus_threshold;
        let tenths_above = (above_threshold * 1000.0) as u64; // Convert to 0.1% units
        
        let bonus = tenths_above * cfg.per_tenth_percent_bonus;
        bonus.min(cfg.max_bonus)
    }
    
    // =========================================================================
    // Cooperation Score Management
    // =========================================================================
    
    /// Get cooperation multiplier for a cluster
    pub fn get_cooperation_multiplier(&self, cluster_id: &ClusterId) -> f64 {
        self.cluster_scores
            .get(cluster_id)
            .map(|s| s.multiplier)
            .unwrap_or(1.0)
    }
    
    /// Record successful cooperation
    pub fn record_successful_cooperation(
        &mut self,
        cluster_id: &ClusterId,
        reward: TokenAmount,
    ) {
        let entry = self.cluster_scores
            .entry(cluster_id.clone())
            .or_insert_with(ClusterCooperationScore::default);
        
        entry.successful_cooperations += 1;
        entry.total_rewards += reward;
        entry.last_activity = SystemTime::now();
        
        entry.multiplier = (entry.multiplier + self.config.cooperation.success_increment)
            .min(self.config.cooperation.max_multiplier);
    }
    
    /// Record failed cooperation
    pub fn record_failed_cooperation(&mut self, cluster_id: &ClusterId) {
        let entry = self.cluster_scores
            .entry(cluster_id.clone())
            .or_insert_with(ClusterCooperationScore::default);
        
        entry.failed_cooperations += 1;
        entry.last_activity = SystemTime::now();
        
        entry.multiplier = (entry.multiplier - self.config.cooperation.failure_decrement)
            .max(self.config.cooperation.min_multiplier);
    }
    
    /// Apply inactivity decay to all clusters
    pub fn apply_inactivity_decay(&mut self) {
        let now = SystemTime::now();
        let decay = self.config.cooperation.inactivity_decay;
        
        for score in self.cluster_scores.values_mut() {
            let days_inactive = score.last_activity
                .elapsed()
                .unwrap_or(Duration::ZERO)
                .as_secs_f64() / 86400.0;
            
            if days_inactive > 1.0 {
                score.multiplier = (score.multiplier - decay * days_inactive)
                    .max(self.config.cooperation.min_multiplier);
            }
        }
    }
    
    /// Get cluster score
    pub fn get_cluster_score(&self, cluster_id: &ClusterId) -> Option<&ClusterCooperationScore> {
        self.cluster_scores.get(cluster_id)
    }
    
    // =========================================================================
    // Aggregate Calculations
    // =========================================================================
    
    /// Calculate total periodic rewards for a cluster
    pub fn calculate_periodic_rewards(
        &self,
        cluster_id: &ClusterId,
        sharing_records: &[ResourceSharingRecord],
        attestation_records: &[AttestationRecord],
        network_record: Option<&NetworkContributionRecord>,
    ) -> PeriodicRewardSummary {
        // Calculate sharing rewards
        let sharing_rewards: Vec<_> = sharing_records.iter()
            .filter(|r| &r.provider_cluster == cluster_id)
            .map(|r| self.calculate_sharing_reward(r))
            .collect();
        let total_sharing = sharing_rewards.iter().map(|r| r.final_reward).sum();
        
        // Calculate attestation rewards
        let attestation_rewards: Vec<_> = attestation_records.iter()
            .map(|r| self.calculate_attestation_rewards(r))
            .collect();
        let total_attestation = attestation_rewards.iter()
            .map(|r| r.cluster_rewards.get(cluster_id).copied().unwrap_or(0))
            .sum();
        
        // Calculate network rewards
        let network_reward = network_record
            .map(|r| self.calculate_network_reward(r).final_reward)
            .unwrap_or(0);
        
        let total = total_sharing + total_attestation + network_reward;
        
        PeriodicRewardSummary {
            cluster_id: cluster_id.clone(),
            period_start: SystemTime::now() - Duration::from_secs(86400), // Mock
            period_end: SystemTime::now(),
            sharing_rewards: total_sharing,
            attestation_rewards: total_attestation,
            network_rewards: network_reward,
            total_rewards: total,
            cooperation_multiplier: self.get_cooperation_multiplier(cluster_id),
        }
    }
}

// =============================================================================
// Result Types
// =============================================================================

#[derive(Debug, Clone)]
pub struct SharingRewardCalculation {
    pub provider_cluster: ClusterId,
    pub consumer_cluster: ClusterId,
    pub cpu_reward: TokenAmount,
    pub memory_reward: TokenAmount,
    pub storage_reward: TokenAmount,
    pub bandwidth_reward: TokenAmount,
    pub base_reward: TokenAmount,
    pub urgency_factor: f64,
    pub qos_factor: f64,
    pub cooperation_multiplier: f64,
    pub final_reward: TokenAmount,
}

#[derive(Debug, Clone)]
pub struct AttestationRewardCalculation {
    pub attested_cluster: ClusterId,
    pub leader_cluster: ClusterId,
    pub verifier_count: usize,
    pub result: AttestationResult,
    pub cluster_rewards: HashMap<ClusterId, TokenAmount>,
    pub attested_cluster_adjustment: i64,
    pub total_rewards_distributed: TokenAmount,
}

#[derive(Debug, Clone)]
pub struct NetworkRewardCalculation {
    pub provider_cluster: ClusterId,
    pub target_clusters: Vec<ClusterId>,
    pub latency_bonus: TokenAmount,
    pub throughput_bonus: TokenAmount,
    pub uptime_bonus: TokenAmount,
    pub stability_penalty: TokenAmount,
    pub gross_reward: TokenAmount,
    pub cooperation_multiplier: f64,
    pub final_reward: TokenAmount,
}

#[derive(Debug, Clone)]
pub struct PeriodicRewardSummary {
    pub cluster_id: ClusterId,
    pub period_start: SystemTime,
    pub period_end: SystemTime,
    pub sharing_rewards: TokenAmount,
    pub attestation_rewards: TokenAmount,
    pub network_rewards: TokenAmount,
    pub total_rewards: TokenAmount,
    pub cooperation_multiplier: f64,
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_sharing_reward() {
        let calc = FederationRewardCalculator::new(FederationRewardConfig::default());
        
        let record = ResourceSharingRecord {
            provider_cluster: "cluster-a".to_string(),
            consumer_cluster: "cluster-b".to_string(),
            resources: SharedResources {
                cpu_hours: 10.0,
                memory_gb_hours: 100.0,
                storage_gb_days: 50.0,
                bandwidth_gb: 10.0,
            },
            started_at: SystemTime::now() - Duration::from_secs(3600),
            ended_at: Some(SystemTime::now()),
            urgent: false,
            qos_score: 1.0,
        };
        
        let reward = calc.calculate_sharing_reward(&record);
        
        // 10 CPU-h × 50 + 100 GB-h × 20 + 50 GB-d × 5 + 10 GB × 10
        // = 500 + 2000 + 250 + 100 = 2850
        assert_eq!(reward.base_reward, 2850);
    }
    
    #[test]
    fn test_urgency_multiplier() {
        let calc = FederationRewardCalculator::new(FederationRewardConfig::default());
        
        let normal = ResourceSharingRecord {
            provider_cluster: "cluster-a".to_string(),
            consumer_cluster: "cluster-b".to_string(),
            resources: SharedResources {
                cpu_hours: 10.0,
                memory_gb_hours: 0.0,
                storage_gb_days: 0.0,
                bandwidth_gb: 0.0,
            },
            started_at: SystemTime::now(),
            ended_at: None,
            urgent: false,
            qos_score: 1.0,
        };
        
        let urgent = ResourceSharingRecord {
            urgent: true,
            ..normal.clone()
        };
        
        let normal_reward = calc.calculate_sharing_reward(&normal);
        let urgent_reward = calc.calculate_sharing_reward(&urgent);
        
        assert!(urgent_reward.final_reward > normal_reward.final_reward);
    }
    
    #[test]
    fn test_attestation_rewards() {
        let calc = FederationRewardCalculator::new(FederationRewardConfig::default());
        
        let record = AttestationRecord {
            attested_cluster: "cluster-a".to_string(),
            verifier_clusters: vec![
                "cluster-b".to_string(),
                "cluster-c".to_string(),
                "cluster-d".to_string(),
            ],
            leader_cluster: "cluster-b".to_string(),
            result: AttestationResult::Verified,
            timestamp: SystemTime::now(),
        };
        
        let rewards = calc.calculate_attestation_rewards(&record);
        
        // Leader gets verification + bonus
        assert!(rewards.cluster_rewards.get("cluster-b").unwrap() > &10);
        // Verifiers get verification reward
        assert_eq!(rewards.cluster_rewards.get("cluster-c"), Some(&10));
    }
    
    #[test]
    fn test_cooperation_multiplier() {
        let mut calc = FederationRewardCalculator::new(FederationRewardConfig::default());
        let cluster = "cluster-a".to_string();
        
        // Initial multiplier is 1.0
        assert_eq!(calc.get_cooperation_multiplier(&cluster), 1.0);
        
        // After success, multiplier increases
        calc.record_successful_cooperation(&cluster, 100);
        assert!(calc.get_cooperation_multiplier(&cluster) > 1.0);
        
        // After failure, multiplier decreases
        calc.record_failed_cooperation(&cluster);
        calc.record_failed_cooperation(&cluster);
        assert!(calc.get_cooperation_multiplier(&cluster) < 1.1);
    }
}
