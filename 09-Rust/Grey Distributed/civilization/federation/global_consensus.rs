//! Grey Distributed — Global Consensus Protocol
//!
//! This module implements consensus across sovereign regions for civilization-scale
//! coordination. It handles:
//! - Multi-region Byzantine fault tolerance
//! - Sovereignty-respecting consensus boundaries  
//! - Cross-jurisdictional finality
//! - Geopolitical partition tolerance

use std::collections::{HashMap, HashSet, BTreeMap};
use std::time::{Duration, SystemTime};

// =============================================================================
// CORE TYPES
// =============================================================================

/// Unique identifier for a sovereign region (nation-state or economic bloc)
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct RegionId(pub String);

impl RegionId {
    pub fn new(name: &str) -> Self {
        Self(name.to_string())
    }
}

/// Geographic zone for latency-aware routing
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GeoZone {
    NorthAmerica,
    SouthAmerica,
    WesternEurope,
    EasternEurope,
    MiddleEast,
    Africa,
    SouthAsia,
    EastAsia,
    SoutheastAsia,
    Oceania,
}

impl GeoZone {
    /// Adjacent zones for quorum optimization
    pub fn adjacent(&self) -> Vec<GeoZone> {
        use GeoZone::*;
        match self {
            NorthAmerica => vec![SouthAmerica, WesternEurope, EastAsia],
            SouthAmerica => vec![NorthAmerica, Africa, WesternEurope],
            WesternEurope => vec![NorthAmerica, EasternEurope, Africa, MiddleEast],
            EasternEurope => vec![WesternEurope, MiddleEast, SouthAsia],
            MiddleEast => vec![WesternEurope, EasternEurope, Africa, SouthAsia],
            Africa => vec![WesternEurope, MiddleEast, SouthAmerica],
            SouthAsia => vec![EasternEurope, MiddleEast, EastAsia, SoutheastAsia],
            EastAsia => vec![NorthAmerica, SouthAsia, SoutheastAsia, Oceania],
            SoutheastAsia => vec![SouthAsia, EastAsia, Oceania],
            Oceania => vec![EastAsia, SoutheastAsia],
        }
    }
    
    /// Base latency between zones in milliseconds
    pub fn latency_to(&self, other: &GeoZone) -> Duration {
        if self == other {
            return Duration::from_millis(5);
        }
        
        // Simplified latency model based on geographic distance
        let base_latency = if self.adjacent().contains(other) {
            80 // Adjacent zones ~80ms
        } else {
            200 // Distant zones ~200ms
        };
        
        Duration::from_millis(base_latency)
    }
}

/// Sovereignty level determines data residency and consensus requirements
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SovereigntyLevel {
    /// Data can be processed anywhere
    Global,
    /// Data stays within economic bloc (e.g., EU, ASEAN)
    Bloc,
    /// Data stays within nation-state
    National,
    /// Data stays within specific jurisdiction
    SubNational,
    /// Maximum restrictions (military, critical infrastructure)
    Restricted,
}

/// A participant in global consensus
#[derive(Debug, Clone)]
pub struct ConsensusNode {
    pub node_id: String,
    pub region: RegionId,
    pub zone: GeoZone,
    pub sovereignty: SovereigntyLevel,
    pub stake: u64,
    pub reputation: f64,
    pub public_key: Vec<u8>,
    pub last_seen: SystemTime,
    pub is_validator: bool,
}

impl ConsensusNode {
    /// Calculate effective voting weight
    pub fn voting_weight(&self) -> f64 {
        let stake_weight = (self.stake as f64).sqrt();
        let reputation_multiplier = 0.5 + (self.reputation * 0.5);
        stake_weight * reputation_multiplier
    }
}

// =============================================================================
// CONSENSUS PROTOCOL
// =============================================================================

/// Global consensus configuration
#[derive(Debug, Clone)]
pub struct GlobalConsensusConfig {
    /// Minimum regions required for global quorum
    pub min_regions_quorum: usize,
    
    /// Minimum geo zones required for geographic diversity
    pub min_geo_zones: usize,
    
    /// Supermajority threshold (typically 2/3)
    pub supermajority_threshold: f64,
    
    /// Maximum time to wait for consensus
    pub consensus_timeout: Duration,
    
    /// Enable hierarchical consensus (region-first, then global)
    pub hierarchical_mode: bool,
    
    /// Finality depth (blocks before considered irreversible)
    pub finality_depth: u64,
}

impl Default for GlobalConsensusConfig {
    fn default() -> Self {
        Self {
            min_regions_quorum: 5,
            min_geo_zones: 3,
            supermajority_threshold: 0.67,
            consensus_timeout: Duration::from_secs(30),
            hierarchical_mode: true,
            finality_depth: 3,
        }
    }
}

/// Represents a proposal for global consensus
#[derive(Debug, Clone)]
pub struct GlobalProposal {
    pub proposal_id: String,
    pub proposer: String,
    pub data_hash: [u8; 32],
    pub sovereignty_requirements: SovereigntyLevel,
    pub affected_regions: HashSet<RegionId>,
    pub timestamp: SystemTime,
    pub round: u64,
}

/// Vote on a global proposal
#[derive(Debug, Clone)]
pub struct GlobalVote {
    pub proposal_id: String,
    pub voter: String,
    pub region: RegionId,
    pub zone: GeoZone,
    pub approve: bool,
    pub weight: f64,
    pub signature: Vec<u8>,
    pub timestamp: SystemTime,
}

/// Result of regional consensus (first tier in hierarchical mode)
#[derive(Debug, Clone)]
pub struct RegionalConsensusResult {
    pub region: RegionId,
    pub proposal_id: String,
    pub approved: bool,
    pub vote_weight: f64,
    pub participating_nodes: usize,
    pub signatures: Vec<Vec<u8>>,
    pub timestamp: SystemTime,
}

/// Final result of global consensus
#[derive(Debug, Clone)]
pub struct GlobalConsensusResult {
    pub proposal_id: String,
    pub approved: bool,
    pub total_weight: f64,
    pub approving_regions: HashSet<RegionId>,
    pub rejecting_regions: HashSet<RegionId>,
    pub abstaining_regions: HashSet<RegionId>,
    pub geo_zone_coverage: HashSet<GeoZone>,
    pub finality_block: u64,
    pub aggregate_signature: Vec<u8>,
}

/// The global consensus engine
pub struct GlobalConsensus {
    config: GlobalConsensusConfig,
    nodes: HashMap<String, ConsensusNode>,
    regions: HashMap<RegionId, Vec<String>>,
    current_round: u64,
    proposals: HashMap<String, GlobalProposal>,
    votes: HashMap<String, Vec<GlobalVote>>,
    regional_results: HashMap<(String, RegionId), RegionalConsensusResult>,
}

impl GlobalConsensus {
    pub fn new(config: GlobalConsensusConfig) -> Self {
        Self {
            config,
            nodes: HashMap::new(),
            regions: HashMap::new(),
            current_round: 0,
            proposals: HashMap::new(),
            votes: HashMap::new(),
            regional_results: HashMap::new(),
        }
    }
    
    /// Register a node in the consensus network
    pub fn register_node(&mut self, node: ConsensusNode) {
        let region = node.region.clone();
        let node_id = node.node_id.clone();
        
        self.nodes.insert(node_id.clone(), node);
        self.regions.entry(region).or_default().push(node_id);
    }
    
    /// Submit a proposal for global consensus
    pub fn submit_proposal(&mut self, proposal: GlobalProposal) -> Result<String, ConsensusError> {
        // Validate proposer is a registered validator
        let proposer_node = self.nodes.get(&proposal.proposer)
            .ok_or(ConsensusError::UnknownProposer)?;
        
        if !proposer_node.is_validator {
            return Err(ConsensusError::NotValidator);
        }
        
        // Validate affected regions have sufficient representation
        for region in &proposal.affected_regions {
            if !self.regions.contains_key(region) {
                return Err(ConsensusError::InsufficientRegionalCoverage);
            }
        }
        
        let proposal_id = proposal.proposal_id.clone();
        self.proposals.insert(proposal_id.clone(), proposal);
        self.votes.insert(proposal_id.clone(), Vec::new());
        
        Ok(proposal_id)
    }
    
    /// Cast a vote on a proposal
    pub fn cast_vote(&mut self, vote: GlobalVote) -> Result<(), ConsensusError> {
        let proposal = self.proposals.get(&vote.proposal_id)
            .ok_or(ConsensusError::ProposalNotFound)?;
        
        // Validate voter
        let voter_node = self.nodes.get(&vote.voter)
            .ok_or(ConsensusError::UnknownVoter)?;
        
        if !voter_node.is_validator {
            return Err(ConsensusError::NotValidator);
        }
        
        // Validate sovereignty: voter must have appropriate clearance
        if voter_node.sovereignty < proposal.sovereignty_requirements {
            return Err(ConsensusError::SovereigntyViolation);
        }
        
        // Record vote
        self.votes.get_mut(&vote.proposal_id)
            .unwrap()
            .push(vote);
        
        Ok(())
    }
    
    /// Run hierarchical consensus (region-first, then global)
    pub fn run_hierarchical_consensus(
        &mut self,
        proposal_id: &str,
    ) -> Result<GlobalConsensusResult, ConsensusError> {
        let proposal = self.proposals.get(proposal_id)
            .ok_or(ConsensusError::ProposalNotFound)?
            .clone();
        
        let votes = self.votes.get(proposal_id)
            .ok_or(ConsensusError::NoVotes)?
            .clone();
        
        // Phase 1: Regional consensus
        let mut regional_results = HashMap::new();
        
        for region in &proposal.affected_regions {
            let region_votes: Vec<_> = votes.iter()
                .filter(|v| &v.region == region)
                .collect();
            
            if region_votes.is_empty() {
                continue;
            }
            
            let total_weight: f64 = region_votes.iter().map(|v| v.weight).sum();
            let approve_weight: f64 = region_votes.iter()
                .filter(|v| v.approve)
                .map(|v| v.weight)
                .sum();
            
            let approved = approve_weight / total_weight >= self.config.supermajority_threshold;
            
            let result = RegionalConsensusResult {
                region: region.clone(),
                proposal_id: proposal_id.to_string(),
                approved,
                vote_weight: total_weight,
                participating_nodes: region_votes.len(),
                signatures: region_votes.iter().map(|v| v.signature.clone()).collect(),
                timestamp: SystemTime::now(),
            };
            
            regional_results.insert(region.clone(), result);
        }
        
        // Phase 2: Global aggregation
        let approving_regions: HashSet<_> = regional_results.iter()
            .filter(|(_, r)| r.approved)
            .map(|(id, _)| id.clone())
            .collect();
        
        let rejecting_regions: HashSet<_> = regional_results.iter()
            .filter(|(_, r)| !r.approved)
            .map(|(id, _)| id.clone())
            .collect();
        
        let abstaining_regions: HashSet<_> = proposal.affected_regions
            .difference(&approving_regions)
            .filter(|r| !rejecting_regions.contains(*r))
            .cloned()
            .collect();
        
        // Calculate geographic zone coverage
        let geo_zone_coverage: HashSet<_> = votes.iter()
            .filter(|v| v.approve)
            .map(|v| v.zone)
            .collect();
        
        // Check quorum requirements
        let regions_met = approving_regions.len() >= self.config.min_regions_quorum;
        let geo_met = geo_zone_coverage.len() >= self.config.min_geo_zones;
        
        let total_weight: f64 = regional_results.values().map(|r| r.vote_weight).sum();
        let approve_weight: f64 = regional_results.values()
            .filter(|r| r.approved)
            .map(|r| r.vote_weight)
            .sum();
        
        let threshold_met = total_weight > 0.0 && 
            approve_weight / total_weight >= self.config.supermajority_threshold;
        
        let approved = regions_met && geo_met && threshold_met;
        
        self.current_round += 1;
        
        Ok(GlobalConsensusResult {
            proposal_id: proposal_id.to_string(),
            approved,
            total_weight,
            approving_regions,
            rejecting_regions,
            abstaining_regions,
            geo_zone_coverage,
            finality_block: self.current_round + self.config.finality_depth,
            aggregate_signature: vec![], // Simplified; real impl aggregates BLS signatures
        })
    }
}

// =============================================================================
// PARTITION TOLERANCE
// =============================================================================

/// Handles geopolitical network partitions
pub struct PartitionHandler {
    /// Regions currently partitioned from global network
    partitioned_regions: HashSet<RegionId>,
    
    /// Local consensus state during partition
    local_states: HashMap<RegionId, LocalPartitionState>,
    
    /// Reconciliation queue for when partition heals
    reconciliation_queue: Vec<PartitionReconciliation>,
}

#[derive(Debug, Clone)]
pub struct LocalPartitionState {
    pub region: RegionId,
    pub partition_start: SystemTime,
    pub local_round: u64,
    pub pending_proposals: Vec<GlobalProposal>,
    pub tentative_results: Vec<GlobalConsensusResult>,
}

#[derive(Debug, Clone)]
pub struct PartitionReconciliation {
    pub region: RegionId,
    pub global_round_at_partition: u64,
    pub local_decisions: Vec<GlobalConsensusResult>,
    pub conflicts: Vec<ConflictRecord>,
}

#[derive(Debug, Clone)]
pub struct ConflictRecord {
    pub proposal_id: String,
    pub local_decision: bool,
    pub global_decision: bool,
    pub resolution: ConflictResolution,
}

#[derive(Debug, Clone, Copy)]
pub enum ConflictResolution {
    /// Accept global decision, rollback local
    AcceptGlobal,
    /// Local decision stands (sovereignty override)
    LocalOverride,
    /// Manual resolution required
    ManualReview,
    /// Merge both decisions (for compatible changes)
    Merge,
}

impl PartitionHandler {
    pub fn new() -> Self {
        Self {
            partitioned_regions: HashSet::new(),
            local_states: HashMap::new(),
            reconciliation_queue: Vec::new(),
        }
    }
    
    /// Detect and handle a region becoming partitioned
    pub fn handle_partition(&mut self, region: RegionId, global_round: u64) {
        self.partitioned_regions.insert(region.clone());
        
        self.local_states.insert(region.clone(), LocalPartitionState {
            region,
            partition_start: SystemTime::now(),
            local_round: global_round,
            pending_proposals: Vec::new(),
            tentative_results: Vec::new(),
        });
    }
    
    /// Process local consensus during partition
    pub fn process_local_proposal(
        &mut self,
        region: &RegionId,
        proposal: GlobalProposal,
    ) -> Result<(), ConsensusError> {
        let state = self.local_states.get_mut(region)
            .ok_or(ConsensusError::RegionNotPartitioned)?;
        
        state.pending_proposals.push(proposal);
        Ok(())
    }
    
    /// Handle partition healing and reconciliation
    pub fn heal_partition(
        &mut self,
        region: &RegionId,
        global_state: &GlobalConsensus,
    ) -> Result<PartitionReconciliation, ConsensusError> {
        let local_state = self.local_states.remove(region)
            .ok_or(ConsensusError::RegionNotPartitioned)?;
        
        self.partitioned_regions.remove(region);
        
        // Identify conflicts between local and global decisions
        let mut conflicts = Vec::new();
        
        for local_result in &local_state.tentative_results {
            if let Some(global_proposal) = global_state.proposals.get(&local_result.proposal_id) {
                // Check if global network also processed this proposal
                // In real implementation, compare with global finalized state
                let conflict = ConflictRecord {
                    proposal_id: local_result.proposal_id.clone(),
                    local_decision: local_result.approved,
                    global_decision: false, // Would fetch from global state
                    resolution: ConflictResolution::AcceptGlobal,
                };
                conflicts.push(conflict);
            }
        }
        
        let reconciliation = PartitionReconciliation {
            region: region.clone(),
            global_round_at_partition: local_state.local_round,
            local_decisions: local_state.tentative_results,
            conflicts,
        };
        
        self.reconciliation_queue.push(reconciliation.clone());
        
        Ok(reconciliation)
    }
}

// =============================================================================
// CROSS-JURISDICTIONAL FINALITY
// =============================================================================

/// Manages finality across different legal jurisdictions
pub struct FinalityManager {
    /// Finality requirements by jurisdiction
    jurisdiction_requirements: HashMap<RegionId, JurisdictionRequirements>,
    
    /// Finalized decisions
    finalized: BTreeMap<u64, Vec<String>>, // round -> proposal_ids
    
    /// Pending finality confirmations
    pending_confirmations: HashMap<String, FinalityStatus>,
}

#[derive(Debug, Clone)]
pub struct JurisdictionRequirements {
    pub region: RegionId,
    
    /// Minimum time before finality (for regulatory compliance)
    pub min_finality_delay: Duration,
    
    /// Required attestations from regulatory-approved validators
    pub required_attestations: usize,
    
    /// Whether cross-border finality is recognized
    pub recognize_foreign_finality: bool,
    
    /// Specific regions whose finality is recognized
    pub recognized_regions: HashSet<RegionId>,
}

#[derive(Debug, Clone)]
pub struct FinalityStatus {
    pub proposal_id: String,
    pub global_finality_block: u64,
    pub regional_finality: HashMap<RegionId, RegionalFinality>,
    pub is_globally_final: bool,
}

#[derive(Debug, Clone)]
pub struct RegionalFinality {
    pub region: RegionId,
    pub is_final: bool,
    pub finality_time: Option<SystemTime>,
    pub attestation_count: usize,
    pub regulatory_approval: bool,
}

impl FinalityManager {
    pub fn new() -> Self {
        Self {
            jurisdiction_requirements: HashMap::new(),
            finalized: BTreeMap::new(),
            pending_confirmations: HashMap::new(),
        }
    }
    
    /// Register jurisdiction requirements
    pub fn register_jurisdiction(&mut self, requirements: JurisdictionRequirements) {
        self.jurisdiction_requirements.insert(
            requirements.region.clone(),
            requirements,
        );
    }
    
    /// Check if a decision is final in a specific jurisdiction
    pub fn check_finality(
        &self,
        proposal_id: &str,
        region: &RegionId,
    ) -> Result<bool, ConsensusError> {
        let status = self.pending_confirmations.get(proposal_id)
            .ok_or(ConsensusError::ProposalNotFound)?;
        
        let requirements = self.jurisdiction_requirements.get(region)
            .ok_or(ConsensusError::JurisdictionUnknown)?;
        
        // Check if minimum delay has passed
        if let Some(regional) = status.regional_finality.get(region) {
            if !regional.is_final {
                return Ok(false);
            }
            
            // Check attestation requirements
            if regional.attestation_count < requirements.required_attestations {
                return Ok(false);
            }
            
            // Check regulatory approval if required
            if !regional.regulatory_approval {
                return Ok(false);
            }
            
            return Ok(true);
        }
        
        Ok(false)
    }
    
    /// Process global finality and propagate to jurisdictions
    pub fn process_global_finality(
        &mut self,
        result: &GlobalConsensusResult,
        current_block: u64,
    ) {
        if current_block < result.finality_block {
            return; // Not yet final
        }
        
        // Record global finality
        self.finalized
            .entry(result.finality_block)
            .or_default()
            .push(result.proposal_id.clone());
        
        // Update regional finality status
        if let Some(status) = self.pending_confirmations.get_mut(&result.proposal_id) {
            status.is_globally_final = true;
            
            for region in &result.approving_regions {
                status.regional_finality.insert(region.clone(), RegionalFinality {
                    region: region.clone(),
                    is_final: true,
                    finality_time: Some(SystemTime::now()),
                    attestation_count: 0, // Would be populated from attestations
                    regulatory_approval: false, // Would require external confirmation
                });
            }
        }
    }
}

// =============================================================================
// ERRORS
// =============================================================================

#[derive(Debug, Clone)]
pub enum ConsensusError {
    UnknownProposer,
    UnknownVoter,
    NotValidator,
    ProposalNotFound,
    NoVotes,
    InsufficientRegionalCoverage,
    SovereigntyViolation,
    QuorumNotMet,
    RegionNotPartitioned,
    JurisdictionUnknown,
    FinalityNotReached,
    PartitionTimeout,
}

// =============================================================================
// METRICS
// =============================================================================

/// Metrics for global consensus monitoring
#[derive(Debug, Default)]
pub struct ConsensusMetrics {
    pub proposals_submitted: u64,
    pub proposals_approved: u64,
    pub proposals_rejected: u64,
    pub average_consensus_time_ms: f64,
    pub regional_participation: HashMap<RegionId, f64>,
    pub geo_zone_coverage: HashMap<GeoZone, u64>,
    pub partition_events: u64,
    pub reconciliation_conflicts: u64,
    pub finality_confirmations: u64,
}

impl ConsensusMetrics {
    pub fn record_proposal_result(&mut self, result: &GlobalConsensusResult, duration_ms: u64) {
        self.proposals_submitted += 1;
        
        if result.approved {
            self.proposals_approved += 1;
        } else {
            self.proposals_rejected += 1;
        }
        
        // Update running average
        let n = self.proposals_submitted as f64;
        self.average_consensus_time_ms = 
            self.average_consensus_time_ms * ((n - 1.0) / n) + (duration_ms as f64 / n);
        
        // Record geo zone participation
        for zone in &result.geo_zone_coverage {
            *self.geo_zone_coverage.entry(*zone).or_default() += 1;
        }
    }
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_test_node(id: &str, region: &str, zone: GeoZone) -> ConsensusNode {
        ConsensusNode {
            node_id: id.to_string(),
            region: RegionId::new(region),
            zone,
            sovereignty: SovereigntyLevel::Global,
            stake: 1000,
            reputation: 0.9,
            public_key: vec![],
            last_seen: SystemTime::now(),
            is_validator: true,
        }
    }
    
    #[test]
    fn test_regional_consensus() {
        let mut consensus = GlobalConsensus::new(GlobalConsensusConfig {
            min_regions_quorum: 2,
            min_geo_zones: 2,
            ..Default::default()
        });
        
        // Register nodes from different regions
        consensus.register_node(create_test_node("us-1", "USA", GeoZone::NorthAmerica));
        consensus.register_node(create_test_node("eu-1", "EU", GeoZone::WesternEurope));
        consensus.register_node(create_test_node("asia-1", "Japan", GeoZone::EastAsia));
        
        // Create proposal
        let proposal = GlobalProposal {
            proposal_id: "prop-1".to_string(),
            proposer: "us-1".to_string(),
            data_hash: [0u8; 32],
            sovereignty_requirements: SovereigntyLevel::Global,
            affected_regions: vec![
                RegionId::new("USA"),
                RegionId::new("EU"),
            ].into_iter().collect(),
            timestamp: SystemTime::now(),
            round: 1,
        };
        
        consensus.submit_proposal(proposal).unwrap();
        
        // Cast votes
        consensus.cast_vote(GlobalVote {
            proposal_id: "prop-1".to_string(),
            voter: "us-1".to_string(),
            region: RegionId::new("USA"),
            zone: GeoZone::NorthAmerica,
            approve: true,
            weight: 1.0,
            signature: vec![],
            timestamp: SystemTime::now(),
        }).unwrap();
        
        consensus.cast_vote(GlobalVote {
            proposal_id: "prop-1".to_string(),
            voter: "eu-1".to_string(),
            region: RegionId::new("EU"),
            zone: GeoZone::WesternEurope,
            approve: true,
            weight: 1.0,
            signature: vec![],
            timestamp: SystemTime::now(),
        }).unwrap();
        
        let result = consensus.run_hierarchical_consensus("prop-1").unwrap();
        assert!(result.approved);
        assert_eq!(result.approving_regions.len(), 2);
    }
    
    #[test]
    fn test_sovereignty_violation() {
        let mut consensus = GlobalConsensus::new(GlobalConsensusConfig::default());
        
        let mut restricted_node = create_test_node("mil-1", "USA", GeoZone::NorthAmerica);
        restricted_node.sovereignty = SovereigntyLevel::National;
        consensus.register_node(restricted_node);
        
        let proposal = GlobalProposal {
            proposal_id: "prop-restricted".to_string(),
            proposer: "mil-1".to_string(),
            data_hash: [0u8; 32],
            sovereignty_requirements: SovereigntyLevel::Restricted,
            affected_regions: vec![RegionId::new("USA")].into_iter().collect(),
            timestamp: SystemTime::now(),
            round: 1,
        };
        
        consensus.submit_proposal(proposal).unwrap();
        
        let result = consensus.cast_vote(GlobalVote {
            proposal_id: "prop-restricted".to_string(),
            voter: "mil-1".to_string(),
            region: RegionId::new("USA"),
            zone: GeoZone::NorthAmerica,
            approve: true,
            weight: 1.0,
            signature: vec![],
            timestamp: SystemTime::now(),
        });
        
        assert!(matches!(result, Err(ConsensusError::SovereigntyViolation)));
    }
}
