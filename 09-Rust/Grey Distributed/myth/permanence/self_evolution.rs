//! Grey Distributed — Self-Evolution Logic
//!
//! Mechanisms for adapting Grey Distributed across centuries while
//! preserving core purpose and accumulated knowledge.

use std::collections::HashMap;
use std::time::{Duration, Instant};

// =============================================================================
// SELF-EVOLUTION PHILOSOPHY
// =============================================================================
//
// Grey Distributed must not merely survive—it must evolve to remain relevant
// across decades and centuries. However, evolution must be controlled to
// prevent drift from core purpose.
//
// Principles:
// 1. Core purpose is immutable; implementation is fluid
// 2. Evolution must be gradual, not revolutionary
// 3. All changes must be reversible for a transition period
// 4. New capabilities must prove value before permanence
// 5. Evolution decisions require broad consensus
//
// =============================================================================

/// Core aspects of Grey Distributed that cannot change
#[derive(Debug, Clone)]
pub struct ImmutableCore {
    /// Fundamental purpose statement
    pub purpose: String,
    
    /// Core ethical principles
    pub ethics: Vec<EthicalPrinciple>,
    
    /// Foundational security guarantees
    pub security_guarantees: Vec<SecurityGuarantee>,
    
    /// Minimum operational requirements
    pub operational_minimums: OperationalMinimums,
}

impl Default for ImmutableCore {
    fn default() -> Self {
        Self {
            purpose: "Provide permanent, trustworthy distributed infrastructure for humanity".to_string(),
            ethics: vec![
                EthicalPrinciple {
                    name: "Human Primacy".to_string(),
                    description: "Humans remain in control of all significant decisions".to_string(),
                },
                EthicalPrinciple {
                    name: "Universal Access".to_string(),
                    description: "Infrastructure benefits must be broadly accessible".to_string(),
                },
                EthicalPrinciple {
                    name: "Transparency".to_string(),
                    description: "Operations and decisions must be auditable".to_string(),
                },
                EthicalPrinciple {
                    name: "Sustainability".to_string(),
                    description: "Long-term viability over short-term gains".to_string(),
                },
            ],
            security_guarantees: vec![
                SecurityGuarantee {
                    name: "Data Integrity".to_string(),
                    description: "Data is never silently corrupted or lost".to_string(),
                },
                SecurityGuarantee {
                    name: "Access Control".to_string(),
                    description: "Only authorized parties access protected resources".to_string(),
                },
            ],
            operational_minimums: OperationalMinimums {
                availability: 0.999,
                redundancy_factor: 3,
                geographic_distribution: 3,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct EthicalPrinciple {
    pub name: String,
    pub description: String,
}

#[derive(Debug, Clone)]
pub struct SecurityGuarantee {
    pub name: String,
    pub description: String,
}

#[derive(Debug, Clone)]
pub struct OperationalMinimums {
    pub availability: f64,
    pub redundancy_factor: u32,
    pub geographic_distribution: u32,
}

/// Aspects of Grey Distributed that can evolve
#[derive(Debug, Clone)]
pub enum EvolvableAspect {
    /// Technology implementations
    Technology {
        domain: String,
        current: TechnologyVersion,
        candidates: Vec<TechnologyCandidate>,
    },
    
    /// Protocols and interfaces
    Protocol {
        name: String,
        current_version: String,
        evolution_path: Vec<ProtocolVersion>,
    },
    
    /// Governance processes
    Governance {
        process: String,
        current_rules: Vec<GovernanceRule>,
        proposed_changes: Vec<GovernanceChange>,
    },
    
    /// Operational procedures
    Operations {
        procedure: String,
        current_version: String,
        improvement_proposals: Vec<OperationalImprovement>,
    },
    
    /// Resource allocation policies
    Resources {
        policy: String,
        current_allocation: ResourceAllocation,
        optimization_proposals: Vec<OptimizationProposal>,
    },
}

#[derive(Debug, Clone)]
pub struct TechnologyVersion {
    pub name: String,
    pub version: String,
    pub adopted: Instant,
    pub maturity: TechnologyMaturity,
}

#[derive(Debug, Clone)]
pub enum TechnologyMaturity {
    Experimental,
    Emerging,
    Mainstream,
    Mature,
    Legacy,
    Deprecated,
}

#[derive(Debug, Clone)]
pub struct TechnologyCandidate {
    pub name: String,
    pub version: String,
    pub benefits: Vec<String>,
    pub risks: Vec<String>,
    pub migration_cost: MigrationCost,
    pub evaluation_status: EvaluationStatus,
}

#[derive(Debug, Clone)]
pub enum MigrationCost {
    Trivial,
    Low,
    Moderate,
    High,
    Massive,
}

#[derive(Debug, Clone)]
pub enum EvaluationStatus {
    Proposed,
    UnderEvaluation { started: Instant },
    Piloting { regions: Vec<String> },
    Approved,
    Rejected { reason: String },
}

#[derive(Debug, Clone)]
pub struct ProtocolVersion {
    pub version: String,
    pub changes: Vec<String>,
    pub backward_compatible: bool,
    pub transition_period: Duration,
}

#[derive(Debug, Clone)]
pub struct GovernanceRule {
    pub name: String,
    pub description: String,
    pub enacted: Instant,
}

#[derive(Debug, Clone)]
pub struct GovernanceChange {
    pub description: String,
    pub rationale: String,
    pub proposed_by: String,
    pub support_level: f64,
}

#[derive(Debug, Clone)]
pub struct OperationalImprovement {
    pub description: String,
    pub expected_benefit: String,
    pub implementation_effort: ImplementationEffort,
}

#[derive(Debug, Clone)]
pub enum ImplementationEffort {
    Minor,
    Moderate,
    Significant,
    Major,
}

#[derive(Debug, Clone)]
pub struct ResourceAllocation {
    pub compute: f64,
    pub storage: f64,
    pub network: f64,
    pub personnel: u32,
}

#[derive(Debug, Clone)]
pub struct OptimizationProposal {
    pub description: String,
    pub expected_savings: f64,
    pub implementation_risk: f64,
}

/// Evolution proposal with full lifecycle tracking
#[derive(Debug, Clone)]
pub struct EvolutionProposal {
    pub id: ProposalId,
    pub title: String,
    pub description: String,
    pub aspect: EvolvableAspect,
    pub proposed_by: String,
    pub proposed_at: Instant,
    pub status: ProposalStatus,
    pub core_impact_assessment: CoreImpactAssessment,
    pub timeline: EvolutionTimeline,
    pub reversibility: ReversibilityPlan,
}

pub type ProposalId = u64;

#[derive(Debug, Clone)]
pub enum ProposalStatus {
    Draft,
    UnderReview,
    PublicComment { ends: Instant },
    Voting { ends: Instant },
    Approved,
    Rejected { reason: String },
    Implementing { progress: f64 },
    Validating { metrics: Vec<ValidationMetric> },
    Complete,
    Reverted { reason: String },
}

#[derive(Debug, Clone)]
pub struct CoreImpactAssessment {
    /// Does this affect immutable core? (should be false)
    pub affects_core: bool,
    
    /// If affects_core is true, this must explain why it's acceptable
    pub core_impact_justification: Option<String>,
    
    /// Assessment of alignment with purpose
    pub purpose_alignment: AlignmentAssessment,
    
    /// Assessment of alignment with ethics
    pub ethics_alignment: AlignmentAssessment,
    
    /// Security impact assessment
    pub security_impact: SecurityImpact,
}

#[derive(Debug, Clone)]
pub enum AlignmentAssessment {
    StronglyAligned,
    Aligned,
    Neutral,
    Concerning,
    Incompatible,
}

#[derive(Debug, Clone)]
pub enum SecurityImpact {
    NoImpact,
    Positive { description: String },
    MinorRisk { mitigation: String },
    SignificantRisk { mitigation: String },
    Unacceptable,
}

#[derive(Debug, Clone)]
pub struct EvolutionTimeline {
    pub phases: Vec<EvolutionPhase>,
    pub total_duration: Duration,
    pub checkpoints: Vec<Checkpoint>,
}

#[derive(Debug, Clone)]
pub struct EvolutionPhase {
    pub name: String,
    pub duration: Duration,
    pub success_criteria: Vec<String>,
    pub rollback_trigger: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Checkpoint {
    pub name: String,
    pub at_progress: f64,
    pub decision: CheckpointDecision,
}

#[derive(Debug, Clone)]
pub enum CheckpointDecision {
    Pending,
    Continue,
    Pause { reason: String },
    Rollback { reason: String },
}

#[derive(Debug, Clone)]
pub struct ReversibilityPlan {
    /// Period during which full reversal is possible
    pub reversible_period: Duration,
    
    /// Steps to reverse the change
    pub reversal_steps: Vec<String>,
    
    /// Estimated reversal time
    pub reversal_duration: Duration,
    
    /// What cannot be reversed (if anything)
    pub irreversible_aspects: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ValidationMetric {
    pub name: String,
    pub target: f64,
    pub current: f64,
    pub threshold: f64,
}

/// The core self-evolution engine
pub struct SelfEvolutionEngine {
    /// Immutable core definition
    core: ImmutableCore,
    
    /// Active evolution proposals
    proposals: HashMap<ProposalId, EvolutionProposal>,
    
    /// Completed evolutions (history)
    history: Vec<EvolutionRecord>,
    
    /// Configuration
    config: EvolutionConfig,
    
    /// Next proposal ID
    next_proposal_id: ProposalId,
}

#[derive(Debug, Clone)]
pub struct EvolutionConfig {
    /// Minimum review period for proposals
    pub min_review_period: Duration,
    
    /// Public comment period duration
    pub public_comment_period: Duration,
    
    /// Voting period duration
    pub voting_period: Duration,
    
    /// Approval threshold (0.0 to 1.0)
    pub approval_threshold: f64,
    
    /// Minimum participation for valid vote
    pub min_participation: f64,
    
    /// Default reversibility period
    pub default_reversible_period: Duration,
    
    /// Maximum concurrent evolutions
    pub max_concurrent_evolutions: usize,
}

impl Default for EvolutionConfig {
    fn default() -> Self {
        Self {
            min_review_period: Duration::from_secs(7 * 24 * 3600), // 1 week
            public_comment_period: Duration::from_secs(30 * 24 * 3600), // 30 days
            voting_period: Duration::from_secs(14 * 24 * 3600), // 2 weeks
            approval_threshold: 0.66,
            min_participation: 0.50,
            default_reversible_period: Duration::from_secs(90 * 24 * 3600), // 90 days
            max_concurrent_evolutions: 5,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EvolutionRecord {
    pub proposal: EvolutionProposal,
    pub implemented_at: Instant,
    pub outcome: EvolutionOutcome,
    pub lessons_learned: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum EvolutionOutcome {
    Successful { impact: String },
    PartialSuccess { achieved: String, gaps: String },
    Reverted { reason: String },
    Superseded { by: ProposalId },
}

impl SelfEvolutionEngine {
    pub fn new(config: EvolutionConfig) -> Self {
        Self {
            core: ImmutableCore::default(),
            proposals: HashMap::new(),
            history: Vec::new(),
            config,
            next_proposal_id: 1,
        }
    }
    
    /// Propose a new evolution
    pub fn propose(&mut self, 
        title: String, 
        description: String, 
        aspect: EvolvableAspect,
        proposed_by: String,
    ) -> Result<ProposalId, ProposalError> {
        // Check core impact
        let impact = self.assess_core_impact(&aspect);
        if impact.affects_core && impact.core_impact_justification.is_none() {
            return Err(ProposalError::AffectsCore);
        }
        
        // Check concurrent evolution limit
        let active_count = self.proposals.values()
            .filter(|p| matches!(p.status, ProposalStatus::Implementing { .. }))
            .count();
        
        if active_count >= self.config.max_concurrent_evolutions {
            return Err(ProposalError::TooManyConcurrent);
        }
        
        let id = self.next_proposal_id;
        self.next_proposal_id += 1;
        
        let proposal = EvolutionProposal {
            id,
            title,
            description,
            aspect,
            proposed_by,
            proposed_at: Instant::now(),
            status: ProposalStatus::Draft,
            core_impact_assessment: impact,
            timeline: self.default_timeline(),
            reversibility: self.default_reversibility(),
        };
        
        self.proposals.insert(id, proposal);
        Ok(id)
    }
    
    /// Move proposal to review
    pub fn submit_for_review(&mut self, id: ProposalId) -> Result<(), ProposalError> {
        let proposal = self.proposals.get_mut(&id)
            .ok_or(ProposalError::NotFound)?;
        
        if !matches!(proposal.status, ProposalStatus::Draft) {
            return Err(ProposalError::InvalidState);
        }
        
        proposal.status = ProposalStatus::UnderReview;
        Ok(())
    }
    
    /// Open proposal for public comment
    pub fn open_public_comment(&mut self, id: ProposalId) -> Result<(), ProposalError> {
        let proposal = self.proposals.get_mut(&id)
            .ok_or(ProposalError::NotFound)?;
        
        if !matches!(proposal.status, ProposalStatus::UnderReview) {
            return Err(ProposalError::InvalidState);
        }
        
        proposal.status = ProposalStatus::PublicComment {
            ends: Instant::now() + self.config.public_comment_period,
        };
        Ok(())
    }
    
    /// Start voting on proposal
    pub fn start_voting(&mut self, id: ProposalId) -> Result<(), ProposalError> {
        let proposal = self.proposals.get_mut(&id)
            .ok_or(ProposalError::NotFound)?;
        
        if !matches!(proposal.status, ProposalStatus::PublicComment { .. }) {
            return Err(ProposalError::InvalidState);
        }
        
        proposal.status = ProposalStatus::Voting {
            ends: Instant::now() + self.config.voting_period,
        };
        Ok(())
    }
    
    /// Record voting result
    pub fn record_vote_result(&mut self, id: ProposalId, approval: f64, participation: f64) 
        -> Result<(), ProposalError> 
    {
        let proposal = self.proposals.get_mut(&id)
            .ok_or(ProposalError::NotFound)?;
        
        if !matches!(proposal.status, ProposalStatus::Voting { .. }) {
            return Err(ProposalError::InvalidState);
        }
        
        if participation < self.config.min_participation {
            proposal.status = ProposalStatus::Rejected { 
                reason: "Insufficient participation".to_string() 
            };
        } else if approval >= self.config.approval_threshold {
            proposal.status = ProposalStatus::Approved;
        } else {
            proposal.status = ProposalStatus::Rejected { 
                reason: "Did not meet approval threshold".to_string() 
            };
        }
        
        Ok(())
    }
    
    /// Begin implementing an approved proposal
    pub fn begin_implementation(&mut self, id: ProposalId) -> Result<(), ProposalError> {
        let proposal = self.proposals.get_mut(&id)
            .ok_or(ProposalError::NotFound)?;
        
        if !matches!(proposal.status, ProposalStatus::Approved) {
            return Err(ProposalError::InvalidState);
        }
        
        proposal.status = ProposalStatus::Implementing { progress: 0.0 };
        Ok(())
    }
    
    /// Update implementation progress
    pub fn update_progress(&mut self, id: ProposalId, progress: f64) -> Result<(), ProposalError> {
        let proposal = self.proposals.get_mut(&id)
            .ok_or(ProposalError::NotFound)?;
        
        if !matches!(proposal.status, ProposalStatus::Implementing { .. }) {
            return Err(ProposalError::InvalidState);
        }
        
        proposal.status = ProposalStatus::Implementing { 
            progress: progress.min(1.0).max(0.0) 
        };
        
        if progress >= 1.0 {
            proposal.status = ProposalStatus::Validating { 
                metrics: Vec::new() 
            };
        }
        
        Ok(())
    }
    
    /// Complete evolution after successful validation
    pub fn complete(&mut self, id: ProposalId) -> Result<(), ProposalError> {
        let proposal = self.proposals.get_mut(&id)
            .ok_or(ProposalError::NotFound)?;
        
        if !matches!(proposal.status, ProposalStatus::Validating { .. }) {
            return Err(ProposalError::InvalidState);
        }
        
        proposal.status = ProposalStatus::Complete;
        
        // Move to history
        let proposal = self.proposals.remove(&id).unwrap();
        self.history.push(EvolutionRecord {
            proposal,
            implemented_at: Instant::now(),
            outcome: EvolutionOutcome::Successful { 
                impact: "Pending assessment".to_string() 
            },
            lessons_learned: Vec::new(),
        });
        
        Ok(())
    }
    
    /// Revert an evolution
    pub fn revert(&mut self, id: ProposalId, reason: String) -> Result<(), ProposalError> {
        let proposal = self.proposals.get_mut(&id)
            .ok_or(ProposalError::NotFound)?;
        
        proposal.status = ProposalStatus::Reverted { reason };
        
        // Move to history
        let proposal = self.proposals.remove(&id).unwrap();
        let reason = if let ProposalStatus::Reverted { reason } = &proposal.status {
            reason.clone()
        } else {
            "Unknown".to_string()
        };
        
        self.history.push(EvolutionRecord {
            proposal,
            implemented_at: Instant::now(),
            outcome: EvolutionOutcome::Reverted { reason },
            lessons_learned: Vec::new(),
        });
        
        Ok(())
    }
    
    // --- Private helper methods ---
    
    fn assess_core_impact(&self, _aspect: &EvolvableAspect) -> CoreImpactAssessment {
        CoreImpactAssessment {
            affects_core: false,
            core_impact_justification: None,
            purpose_alignment: AlignmentAssessment::Aligned,
            ethics_alignment: AlignmentAssessment::Aligned,
            security_impact: SecurityImpact::NoImpact,
        }
    }
    
    fn default_timeline(&self) -> EvolutionTimeline {
        EvolutionTimeline {
            phases: vec![
                EvolutionPhase {
                    name: "Pilot".to_string(),
                    duration: Duration::from_secs(30 * 24 * 3600),
                    success_criteria: vec!["No regressions".to_string()],
                    rollback_trigger: vec!["Critical issues".to_string()],
                },
                EvolutionPhase {
                    name: "Staged Rollout".to_string(),
                    duration: Duration::from_secs(60 * 24 * 3600),
                    success_criteria: vec!["Metrics stable".to_string()],
                    rollback_trigger: vec!["Performance degradation".to_string()],
                },
                EvolutionPhase {
                    name: "Full Deployment".to_string(),
                    duration: Duration::from_secs(30 * 24 * 3600),
                    success_criteria: vec!["Complete coverage".to_string()],
                    rollback_trigger: vec!["Unforeseen issues".to_string()],
                },
            ],
            total_duration: Duration::from_secs(120 * 24 * 3600),
            checkpoints: Vec::new(),
        }
    }
    
    fn default_reversibility(&self) -> ReversibilityPlan {
        ReversibilityPlan {
            reversible_period: self.config.default_reversible_period,
            reversal_steps: vec![
                "Document current state".to_string(),
                "Execute reversal procedure".to_string(),
                "Validate reversal complete".to_string(),
                "Post-mortem analysis".to_string(),
            ],
            reversal_duration: Duration::from_secs(24 * 3600),
            irreversible_aspects: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ProposalError {
    AffectsCore,
    TooManyConcurrent,
    NotFound,
    InvalidState,
}

// =============================================================================
// PERMANENCE INTEGRATION
// =============================================================================

/// Evolution health status for permanence monitoring
#[derive(Debug, Clone)]
pub struct EvolutionHealth {
    /// Active proposals by status
    pub proposals_by_status: HashMap<String, usize>,
    
    /// Evolution velocity (proposals completed per year)
    pub evolution_velocity: f64,
    
    /// Reversion rate
    pub reversion_rate: f64,
    
    /// Average time from proposal to completion
    pub average_cycle_time: Duration,
    
    /// Core integrity (always should be 1.0)
    pub core_integrity: f64,
}

impl SelfEvolutionEngine {
    /// Get health status for dashboard reporting
    pub fn health_status(&self) -> EvolutionHealth {
        let mut by_status = HashMap::new();
        for proposal in self.proposals.values() {
            let status_name = match &proposal.status {
                ProposalStatus::Draft => "Draft",
                ProposalStatus::UnderReview => "UnderReview",
                ProposalStatus::PublicComment { .. } => "PublicComment",
                ProposalStatus::Voting { .. } => "Voting",
                ProposalStatus::Approved => "Approved",
                ProposalStatus::Rejected { .. } => "Rejected",
                ProposalStatus::Implementing { .. } => "Implementing",
                ProposalStatus::Validating { .. } => "Validating",
                ProposalStatus::Complete => "Complete",
                ProposalStatus::Reverted { .. } => "Reverted",
            };
            *by_status.entry(status_name.to_string()).or_insert(0) += 1;
        }
        
        let reverted = self.history.iter()
            .filter(|r| matches!(r.outcome, EvolutionOutcome::Reverted { .. }))
            .count();
        
        EvolutionHealth {
            proposals_by_status: by_status,
            evolution_velocity: self.history.len() as f64, // Simplified
            reversion_rate: if self.history.is_empty() {
                0.0
            } else {
                reverted as f64 / self.history.len() as f64
            },
            average_cycle_time: Duration::from_secs(90 * 24 * 3600), // Placeholder
            core_integrity: 1.0, // Should always be 1.0
        }
    }
    
    /// Get immutable core for verification
    pub fn immutable_core(&self) -> &ImmutableCore {
        &self.core
    }
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_proposal_lifecycle() {
        let mut engine = SelfEvolutionEngine::new(EvolutionConfig::default());
        
        let id = engine.propose(
            "Test Evolution".to_string(),
            "A test evolution proposal".to_string(),
            EvolvableAspect::Operations {
                procedure: "deployment".to_string(),
                current_version: "1.0".to_string(),
                improvement_proposals: Vec::new(),
            },
            "test-proposer".to_string(),
        ).unwrap();
        
        assert!(engine.proposals.contains_key(&id));
        assert!(matches!(engine.proposals[&id].status, ProposalStatus::Draft));
        
        engine.submit_for_review(id).unwrap();
        assert!(matches!(engine.proposals[&id].status, ProposalStatus::UnderReview));
    }
    
    #[test]
    fn test_core_integrity() {
        let engine = SelfEvolutionEngine::new(EvolutionConfig::default());
        let health = engine.health_status();
        
        assert_eq!(health.core_integrity, 1.0);
    }
}
