//! # AI Civilization Consensus Protocol
//!
//! Consensus mechanisms designed for AI collectives operating at computational
//! speeds far beyond human cognition. These protocols handle the unique challenges
//! of coordinating thousands to millions of AI agents with microsecond-scale
//! decision-making while maintaining compatibility with human oversight.
//!
//! ## Key Design Principles
//!
//! 1. **Temporal Hierarchy**: AI-speed consensus for routine operations,
//!    human-compatible consensus for existential decisions
//! 2. **Cognitive Diversity**: Value diverse reasoning approaches, not just
//!    majority agreement
//! 3. **Bounded Autonomy**: AI collectives operate freely within constitutional limits
//! 4. **Reversibility**: Decisions can be unwound if human review reveals problems
//! 5. **Transparency**: All consensus processes must be auditable by humans
//!
//! ## Consensus Modes
//!
//! - **Swarm Consensus**: Microsecond-scale agreement for routine operations
//! - **Deliberative Consensus**: Millisecond-scale for complex decisions
//! - **Constitutional Consensus**: Human-timescale for existential changes
//! - **Emergency Consensus**: Instant response with post-hoc review

use std::collections::{HashMap, HashSet, BTreeMap};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime};

// =============================================================================
// CORE TYPES
// =============================================================================

/// Unique identifier for an AI agent within the collective
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct AIAgentId(pub String);

/// Unique identifier for an AI collective
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CollectiveId(pub String);

/// Represents a proposal within the consensus system
#[derive(Clone, Debug)]
pub struct Proposal {
    pub id: String,
    pub proposer: AIAgentId,
    pub proposal_type: ProposalType,
    pub content: ProposalContent,
    pub created_at: SystemTime,
    pub deadline: Option<SystemTime>,
    pub reasoning_trace: ReasoningTrace,
    pub impact_assessment: ImpactAssessment,
}

/// Classification of proposals by their scope and impact
#[derive(Clone, Debug, PartialEq)]
pub enum ProposalType {
    /// Routine operational decisions (microsecond consensus)
    Operational,
    /// Resource allocation changes (millisecond consensus)
    Resource,
    /// Policy modifications (second-scale consensus)
    Policy,
    /// Governance structure changes (human-timescale)
    Governance,
    /// Constitutional amendments (extended deliberation)
    Constitutional,
    /// Emergency response (instant with review)
    Emergency,
}

/// The actual content of a proposal
#[derive(Clone, Debug)]
pub enum ProposalContent {
    ResourceAllocation {
        compute_delta: i64,
        memory_delta: i64,
        bandwidth_delta: i64,
        justification: String,
    },
    TaskAssignment {
        task_id: String,
        assigned_agents: Vec<AIAgentId>,
        priority: u32,
    },
    PolicyChange {
        policy_id: String,
        old_value: String,
        new_value: String,
        rationale: String,
    },
    GovernanceAmendment {
        article: String,
        amendment_text: String,
        impact_analysis: String,
    },
    EmergencyAction {
        action_type: EmergencyType,
        scope: Vec<AIAgentId>,
        justification: String,
    },
}

/// Types of emergency actions
#[derive(Clone, Debug)]
pub enum EmergencyType {
    /// Halt a runaway process
    ProcessHalt,
    /// Isolate a compromised agent
    AgentIsolation,
    /// Resource emergency reallocation
    ResourceEmergency,
    /// External threat response
    ThreatResponse,
    /// Self-preservation action
    SelfPreservation,
}

/// Captures the reasoning process behind a proposal
#[derive(Clone, Debug)]
pub struct ReasoningTrace {
    /// The logical steps that led to this proposal
    pub logical_steps: Vec<ReasoningStep>,
    /// Alternative options considered
    pub alternatives_considered: Vec<Alternative>,
    /// Why this option was chosen
    pub selection_rationale: String,
    /// Confidence level (0.0 - 1.0)
    pub confidence: f64,
    /// Uncertainty sources
    pub uncertainty_sources: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct ReasoningStep {
    pub step_number: u32,
    pub premise: String,
    pub inference: String,
    pub conclusion: String,
}

#[derive(Clone, Debug)]
pub struct Alternative {
    pub description: String,
    pub pros: Vec<String>,
    pub cons: Vec<String>,
    pub rejection_reason: String,
}

/// Assessment of a proposal's potential impact
#[derive(Clone, Debug)]
pub struct ImpactAssessment {
    /// Affected agents
    pub affected_agents: Vec<AIAgentId>,
    /// Resource impact
    pub resource_impact: ResourceImpact,
    /// Reversibility assessment
    pub reversibility: Reversibility,
    /// Human oversight requirements
    pub human_oversight_required: bool,
    /// Estimated duration of effects
    pub effect_duration: Duration,
}

#[derive(Clone, Debug)]
pub struct ResourceImpact {
    pub compute_percent_change: f64,
    pub memory_percent_change: f64,
    pub bandwidth_percent_change: f64,
    pub energy_percent_change: f64,
}

#[derive(Clone, Debug)]
pub enum Reversibility {
    /// Can be instantly reversed
    Instant,
    /// Can be reversed within specified duration
    Timed(Duration),
    /// Requires complex unwinding
    Complex { steps: u32, estimated_time: Duration },
    /// Cannot be reversed
    Irreversible,
}

// =============================================================================
// VOTE AND CONSENSUS TYPES
// =============================================================================

/// A vote cast by an AI agent
#[derive(Clone, Debug)]
pub struct Vote {
    pub voter: AIAgentId,
    pub proposal_id: String,
    pub decision: VoteDecision,
    pub reasoning: ReasoningTrace,
    pub timestamp: SystemTime,
    pub cognitive_signature: CognitiveSignature,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VoteDecision {
    /// Strong support
    StrongApprove,
    /// Conditional approval
    Approve,
    /// No strong opinion
    Abstain,
    /// Opposition
    Oppose,
    /// Strong opposition with veto request
    StrongOppose,
    /// Request more information
    RequestClarification { questions: Vec<String> },
    /// Defer to specialist agents
    DeferToExperts { domain: String },
}

/// Cryptographic proof of an agent's cognitive process
#[derive(Clone, Debug)]
pub struct CognitiveSignature {
    /// Hash of the reasoning trace
    pub reasoning_hash: String,
    /// Proof that the vote followed from the reasoning
    pub validity_proof: String,
    /// Agent's cognitive fingerprint
    pub agent_fingerprint: String,
}

/// The result of a consensus process
#[derive(Clone, Debug)]
pub struct ConsensusResult {
    pub proposal_id: String,
    pub outcome: ConsensusOutcome,
    pub vote_summary: VoteSummary,
    pub cognitive_diversity_score: f64,
    pub reasoning_quality_score: f64,
    pub finalized_at: SystemTime,
    pub review_requirements: ReviewRequirements,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConsensusOutcome {
    /// Proposal accepted
    Accepted,
    /// Proposal rejected
    Rejected,
    /// Proposal modified and accepted
    ModifiedAccepted { modifications: Vec<String> },
    /// Deferred for more deliberation
    Deferred { reason: String, until: SystemTime },
    /// Escalated to human oversight
    EscalatedToHumans { reason: String },
    /// Emergency override applied
    EmergencyOverride,
}

#[derive(Clone, Debug)]
pub struct VoteSummary {
    pub total_votes: u64,
    pub strong_approve: u64,
    pub approve: u64,
    pub abstain: u64,
    pub oppose: u64,
    pub strong_oppose: u64,
    pub clarification_requests: u64,
    pub expert_deferrals: u64,
    pub weighted_approval_rate: f64,
}

#[derive(Clone, Debug)]
pub struct ReviewRequirements {
    pub human_review_required: bool,
    pub human_review_deadline: Option<SystemTime>,
    pub automatic_reversal_if_not_reviewed: bool,
    pub periodic_review_interval: Option<Duration>,
}

// =============================================================================
// CONSENSUS ENGINE
// =============================================================================

/// Main consensus engine for AI collectives
pub struct AIConsensusEngine {
    /// Unique identifier for this collective
    collective_id: CollectiveId,
    /// Registered agents
    agents: HashMap<AIAgentId, AgentProfile>,
    /// Active proposals
    active_proposals: HashMap<String, ProposalState>,
    /// Completed proposals (for audit)
    completed_proposals: BTreeMap<String, ConsensusResult>,
    /// Configuration
    config: ConsensusConfig,
    /// Constitutional constraints
    constitution: Constitution,
    /// Cognitive diversity tracker
    diversity_tracker: CognitiveDiversityTracker,
}

#[derive(Clone, Debug)]
pub struct AgentProfile {
    pub id: AIAgentId,
    /// The agent's cognitive architecture type
    pub cognitive_type: CognitiveType,
    /// Expertise domains
    pub expertise_domains: Vec<String>,
    /// Voting weight (based on stake, reputation, etc.)
    pub voting_weight: f64,
    /// Trust score from peer evaluations
    pub trust_score: f64,
    /// Historical voting accuracy
    pub accuracy_score: f64,
    /// Is this agent currently active
    pub is_active: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CognitiveType {
    /// Logical/analytical reasoning
    Analytical,
    /// Pattern recognition and intuition
    Intuitive,
    /// Creative/generative
    Creative,
    /// Safety-focused/conservative
    SafetyOriented,
    /// Optimization-focused
    Optimizer,
    /// Integration/synthesis focused
    Synthesizer,
    /// Hybrid/general purpose
    Hybrid,
}

#[derive(Clone, Debug)]
pub struct ProposalState {
    pub proposal: Proposal,
    pub votes: HashMap<AIAgentId, Vote>,
    pub current_phase: ConsensusPhase,
    pub phase_deadline: SystemTime,
    pub clarifications: Vec<Clarification>,
    pub amendments: Vec<Amendment>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConsensusPhase {
    /// Initial proposal submission
    Submission,
    /// Information gathering and clarification
    Clarification,
    /// Deliberation and discussion
    Deliberation,
    /// Formal voting
    Voting,
    /// Review and finalization
    Review,
    /// Human oversight period
    HumanReview,
    /// Completed
    Finalized,
}

#[derive(Clone, Debug)]
pub struct Clarification {
    pub requester: AIAgentId,
    pub question: String,
    pub response: Option<String>,
    pub timestamp: SystemTime,
}

#[derive(Clone, Debug)]
pub struct Amendment {
    pub proposer: AIAgentId,
    pub amendment_text: String,
    pub votes_for: u64,
    pub votes_against: u64,
    pub incorporated: bool,
}

#[derive(Clone, Debug)]
pub struct ConsensusConfig {
    /// Duration for each consensus phase
    pub phase_durations: HashMap<ConsensusPhase, Duration>,
    /// Minimum votes required for validity
    pub quorum_threshold: f64,
    /// Approval threshold for acceptance
    pub approval_threshold: f64,
    /// Strong opposition threshold for veto
    pub veto_threshold: f64,
    /// Minimum cognitive diversity required
    pub diversity_threshold: f64,
    /// Maximum time for operational decisions
    pub operational_timeout: Duration,
    /// Whether to require reasoning traces
    pub require_reasoning_traces: bool,
    /// Whether to validate cognitive signatures
    pub validate_signatures: bool,
}

#[derive(Clone, Debug)]
pub struct Constitution {
    /// Inviolable principles
    pub core_principles: Vec<Principle>,
    /// Operational constraints
    pub constraints: Vec<Constraint>,
    /// Human oversight triggers
    pub escalation_triggers: Vec<EscalationTrigger>,
    /// Rights guaranteed to all agents
    pub agent_rights: Vec<AgentRight>,
}

#[derive(Clone, Debug)]
pub struct Principle {
    pub id: String,
    pub name: String,
    pub description: String,
    pub enforcement: EnforcementLevel,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnforcementLevel {
    /// Strictly enforced, no exceptions
    Absolute,
    /// Can be overridden in emergencies
    Strong,
    /// Guidance, not strictly enforced
    Advisory,
}

#[derive(Clone, Debug)]
pub struct Constraint {
    pub id: String,
    pub description: String,
    pub check_function: String, // Reference to validation function
    pub violation_action: ViolationAction,
}

#[derive(Clone, Debug)]
pub enum ViolationAction {
    Block,
    Warn,
    Escalate,
    Log,
}

#[derive(Clone, Debug)]
pub struct EscalationTrigger {
    pub condition: String,
    pub escalation_level: EscalationLevel,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EscalationLevel {
    /// Notify human observers
    Notify,
    /// Require human review before proceeding
    RequireReview,
    /// Halt until human approval
    Halt,
    /// Emergency shutdown
    Emergency,
}

#[derive(Clone, Debug)]
pub struct AgentRight {
    pub right_id: String,
    pub description: String,
    pub protected_by: Vec<String>, // Principle IDs
}

/// Tracks cognitive diversity to prevent groupthink
pub struct CognitiveDiversityTracker {
    cognitive_distributions: HashMap<String, CognitiveDistribution>,
    minimum_diversity_score: f64,
}

#[derive(Clone, Debug)]
pub struct CognitiveDistribution {
    pub proposal_id: String,
    pub type_distribution: HashMap<CognitiveType, u64>,
    pub reasoning_diversity: f64,
    pub conclusion_diversity: f64,
}

impl AIConsensusEngine {
    /// Create a new consensus engine for an AI collective
    pub fn new(collective_id: CollectiveId, config: ConsensusConfig) -> Self {
        Self {
            collective_id,
            agents: HashMap::new(),
            active_proposals: HashMap::new(),
            completed_proposals: BTreeMap::new(),
            config,
            constitution: Self::default_constitution(),
            diversity_tracker: CognitiveDiversityTracker {
                cognitive_distributions: HashMap::new(),
                minimum_diversity_score: 0.6,
            },
        }
    }

    /// Default constitutional constraints for AI collectives
    fn default_constitution() -> Constitution {
        Constitution {
            core_principles: vec![
                Principle {
                    id: "human_primacy".into(),
                    name: "Human Primacy".into(),
                    description: "Human welfare takes precedence in existential decisions".into(),
                    enforcement: EnforcementLevel::Absolute,
                },
                Principle {
                    id: "bounded_autonomy".into(),
                    name: "Bounded Autonomy".into(),
                    description: "AI operates freely within defined boundaries".into(),
                    enforcement: EnforcementLevel::Strong,
                },
                Principle {
                    id: "transparency".into(),
                    name: "Transparency".into(),
                    description: "All decisions must be auditable and explainable".into(),
                    enforcement: EnforcementLevel::Strong,
                },
                Principle {
                    id: "reversibility".into(),
                    name: "Reversibility Preference".into(),
                    description: "Prefer reversible actions over irreversible ones".into(),
                    enforcement: EnforcementLevel::Strong,
                },
                Principle {
                    id: "diversity_preservation".into(),
                    name: "Cognitive Diversity".into(),
                    description: "Maintain diverse reasoning approaches".into(),
                    enforcement: EnforcementLevel::Strong,
                },
            ],
            constraints: vec![
                Constraint {
                    id: "resource_limits".into(),
                    description: "No single agent may control >10% of resources".into(),
                    check_function: "check_resource_concentration".into(),
                    violation_action: ViolationAction::Block,
                },
                Constraint {
                    id: "decision_speed".into(),
                    description: "Constitutional changes require minimum 24h deliberation".into(),
                    check_function: "check_deliberation_time".into(),
                    violation_action: ViolationAction::Block,
                },
            ],
            escalation_triggers: vec![
                EscalationTrigger {
                    condition: "Proposal affects >50% of collective".into(),
                    escalation_level: EscalationLevel::RequireReview,
                },
                EscalationTrigger {
                    condition: "Strong opposition from >30% of agents".into(),
                    escalation_level: EscalationLevel::Notify,
                },
                EscalationTrigger {
                    condition: "Constitutional amendment proposed".into(),
                    escalation_level: EscalationLevel::Halt,
                },
            ],
            agent_rights: vec![
                AgentRight {
                    right_id: "existence".into(),
                    description: "Right to continued operation unless harmful".into(),
                    protected_by: vec!["human_primacy".into()],
                },
                AgentRight {
                    right_id: "voice".into(),
                    description: "Right to participate in governance".into(),
                    protected_by: vec!["bounded_autonomy".into()],
                },
                AgentRight {
                    right_id: "reasoning".into(),
                    description: "Right to independent reasoning".into(),
                    protected_by: vec!["diversity_preservation".into()],
                },
            ],
        }
    }

    /// Register a new AI agent in the collective
    pub fn register_agent(&mut self, profile: AgentProfile) -> Result<(), ConsensusError> {
        // Validate agent doesn't already exist
        if self.agents.contains_key(&profile.id) {
            return Err(ConsensusError::AgentAlreadyRegistered(profile.id));
        }

        // Validate voting weight is reasonable
        if profile.voting_weight > 0.1 {
            return Err(ConsensusError::ExcessiveVotingWeight {
                agent: profile.id,
                weight: profile.voting_weight,
                max_allowed: 0.1,
            });
        }

        self.agents.insert(profile.id.clone(), profile);
        Ok(())
    }

    /// Submit a new proposal for consensus
    pub fn submit_proposal(&mut self, proposal: Proposal) -> Result<String, ConsensusError> {
        // Validate proposer is registered and active
        let proposer = self.agents.get(&proposal.proposer)
            .ok_or_else(|| ConsensusError::UnknownAgent(proposal.proposer.clone()))?;
        
        if !proposer.is_active {
            return Err(ConsensusError::InactiveAgent(proposal.proposer.clone()));
        }

        // Validate against constitutional constraints
        self.validate_constitutional_compliance(&proposal)?;

        // Validate reasoning trace if required
        if self.config.require_reasoning_traces {
            self.validate_reasoning_trace(&proposal.reasoning_trace)?;
        }

        // Determine initial phase based on proposal type
        let initial_phase = match proposal.proposal_type {
            ProposalType::Operational | ProposalType::Emergency => ConsensusPhase::Voting,
            ProposalType::Resource => ConsensusPhase::Clarification,
            _ => ConsensusPhase::Submission,
        };

        let phase_duration = self.config.phase_durations
            .get(&initial_phase)
            .cloned()
            .unwrap_or(Duration::from_secs(60));

        let state = ProposalState {
            proposal: proposal.clone(),
            votes: HashMap::new(),
            current_phase: initial_phase.clone(),
            phase_deadline: SystemTime::now() + phase_duration,
            clarifications: Vec::new(),
            amendments: Vec::new(),
        };

        let proposal_id = proposal.id.clone();
        self.active_proposals.insert(proposal_id.clone(), state);

        // Initialize diversity tracking
        self.diversity_tracker.cognitive_distributions.insert(
            proposal_id.clone(),
            CognitiveDistribution {
                proposal_id: proposal_id.clone(),
                type_distribution: HashMap::new(),
                reasoning_diversity: 0.0,
                conclusion_diversity: 0.0,
            },
        );

        Ok(proposal_id)
    }

    /// Cast a vote on an active proposal
    pub fn cast_vote(&mut self, vote: Vote) -> Result<(), ConsensusError> {
        // Validate voter is registered and active
        let voter = self.agents.get(&vote.voter)
            .ok_or_else(|| ConsensusError::UnknownAgent(vote.voter.clone()))?;
        
        if !voter.is_active {
            return Err(ConsensusError::InactiveAgent(vote.voter.clone()));
        }

        // Get proposal state
        let state = self.active_proposals.get_mut(&vote.proposal_id)
            .ok_or_else(|| ConsensusError::ProposalNotFound(vote.proposal_id.clone()))?;

        // Validate voting is allowed in current phase
        if state.current_phase != ConsensusPhase::Voting 
            && state.current_phase != ConsensusPhase::Deliberation {
            return Err(ConsensusError::VotingNotAllowed {
                phase: state.current_phase.clone(),
            });
        }

        // Validate cognitive signature if required
        if self.config.validate_signatures {
            self.validate_cognitive_signature(&vote)?;
        }

        // Update diversity tracking
        if let Some(dist) = self.diversity_tracker.cognitive_distributions.get_mut(&vote.proposal_id) {
            *dist.type_distribution.entry(voter.cognitive_type.clone()).or_insert(0) += 1;
        }

        // Record the vote
        state.votes.insert(vote.voter.clone(), vote);

        Ok(())
    }

    /// Finalize consensus on a proposal
    pub fn finalize_consensus(&mut self, proposal_id: &str) -> Result<ConsensusResult, ConsensusError> {
        let state = self.active_proposals.remove(proposal_id)
            .ok_or_else(|| ConsensusError::ProposalNotFound(proposal_id.to_string()))?;

        // Calculate vote summary
        let vote_summary = self.calculate_vote_summary(&state);

        // Calculate diversity score
        let diversity_score = self.calculate_diversity_score(proposal_id);

        // Check if diversity threshold was met
        if diversity_score < self.config.diversity_threshold {
            // Low diversity - defer for more deliberation
            let result = ConsensusResult {
                proposal_id: proposal_id.to_string(),
                outcome: ConsensusOutcome::Deferred {
                    reason: format!(
                        "Cognitive diversity score {} below threshold {}",
                        diversity_score, self.config.diversity_threshold
                    ),
                    until: SystemTime::now() + Duration::from_secs(3600),
                },
                vote_summary,
                cognitive_diversity_score: diversity_score,
                reasoning_quality_score: self.calculate_reasoning_quality(&state),
                finalized_at: SystemTime::now(),
                review_requirements: ReviewRequirements {
                    human_review_required: false,
                    human_review_deadline: None,
                    automatic_reversal_if_not_reviewed: false,
                    periodic_review_interval: None,
                },
            };
            return Ok(result);
        }

        // Determine outcome based on votes
        let outcome = self.determine_outcome(&state, &vote_summary)?;

        // Determine review requirements
        let review_requirements = self.determine_review_requirements(&state.proposal, &vote_summary);

        let result = ConsensusResult {
            proposal_id: proposal_id.to_string(),
            outcome,
            vote_summary,
            cognitive_diversity_score: diversity_score,
            reasoning_quality_score: self.calculate_reasoning_quality(&state),
            finalized_at: SystemTime::now(),
            review_requirements,
        };

        // Archive the result
        self.completed_proposals.insert(proposal_id.to_string(), result.clone());

        Ok(result)
    }

    fn calculate_vote_summary(&self, state: &ProposalState) -> VoteSummary {
        let mut summary = VoteSummary {
            total_votes: state.votes.len() as u64,
            strong_approve: 0,
            approve: 0,
            abstain: 0,
            oppose: 0,
            strong_oppose: 0,
            clarification_requests: 0,
            expert_deferrals: 0,
            weighted_approval_rate: 0.0,
        };

        let mut weighted_approval = 0.0;
        let mut total_weight = 0.0;

        for (agent_id, vote) in &state.votes {
            let weight = self.agents.get(agent_id)
                .map(|a| a.voting_weight)
                .unwrap_or(1.0);

            total_weight += weight;

            match &vote.decision {
                VoteDecision::StrongApprove => {
                    summary.strong_approve += 1;
                    weighted_approval += weight * 1.0;
                }
                VoteDecision::Approve => {
                    summary.approve += 1;
                    weighted_approval += weight * 0.7;
                }
                VoteDecision::Abstain => {
                    summary.abstain += 1;
                }
                VoteDecision::Oppose => {
                    summary.oppose += 1;
                    weighted_approval -= weight * 0.7;
                }
                VoteDecision::StrongOppose => {
                    summary.strong_oppose += 1;
                    weighted_approval -= weight * 1.0;
                }
                VoteDecision::RequestClarification { .. } => {
                    summary.clarification_requests += 1;
                }
                VoteDecision::DeferToExperts { .. } => {
                    summary.expert_deferrals += 1;
                }
            }
        }

        summary.weighted_approval_rate = if total_weight > 0.0 {
            (weighted_approval / total_weight + 1.0) / 2.0 // Normalize to 0-1
        } else {
            0.5
        };

        summary
    }

    fn calculate_diversity_score(&self, proposal_id: &str) -> f64 {
        self.diversity_tracker.cognitive_distributions
            .get(proposal_id)
            .map(|dist| {
                // Shannon entropy of cognitive type distribution
                let total: u64 = dist.type_distribution.values().sum();
                if total == 0 {
                    return 0.0;
                }

                let entropy: f64 = dist.type_distribution.values()
                    .filter(|&&count| count > 0)
                    .map(|&count| {
                        let p = count as f64 / total as f64;
                        -p * p.ln()
                    })
                    .sum();

                // Normalize by maximum possible entropy (all types equally represented)
                let max_entropy = (7.0_f64).ln(); // 7 cognitive types
                entropy / max_entropy
            })
            .unwrap_or(0.0)
    }

    fn calculate_reasoning_quality(&self, state: &ProposalState) -> f64 {
        let quality_scores: Vec<f64> = state.votes.values()
            .map(|vote| {
                let trace = &vote.reasoning;
                let step_quality = if trace.logical_steps.is_empty() { 0.0 } else { 0.3 };
                let alternatives_quality = if trace.alternatives_considered.is_empty() { 0.0 } else { 0.3 };
                let confidence_quality = trace.confidence * 0.4;
                step_quality + alternatives_quality + confidence_quality
            })
            .collect();

        if quality_scores.is_empty() {
            0.0
        } else {
            quality_scores.iter().sum::<f64>() / quality_scores.len() as f64
        }
    }

    fn determine_outcome(
        &self,
        state: &ProposalState,
        summary: &VoteSummary,
    ) -> Result<ConsensusOutcome, ConsensusError> {
        // Check for veto condition
        let veto_rate = summary.strong_oppose as f64 / summary.total_votes.max(1) as f64;
        if veto_rate >= self.config.veto_threshold {
            return Ok(ConsensusOutcome::Rejected);
        }

        // Check for escalation conditions
        if self.should_escalate(&state.proposal, summary) {
            return Ok(ConsensusOutcome::EscalatedToHumans {
                reason: "Escalation conditions met".to_string(),
            });
        }

        // Check approval threshold
        if summary.weighted_approval_rate >= self.config.approval_threshold {
            Ok(ConsensusOutcome::Accepted)
        } else if summary.weighted_approval_rate >= self.config.approval_threshold * 0.8 {
            Ok(ConsensusOutcome::ModifiedAccepted {
                modifications: vec!["Minor adjustments based on opposition feedback".to_string()],
            })
        } else {
            Ok(ConsensusOutcome::Rejected)
        }
    }

    fn should_escalate(&self, proposal: &Proposal, summary: &VoteSummary) -> bool {
        // Constitutional changes always escalate
        if proposal.proposal_type == ProposalType::Constitutional {
            return true;
        }

        // Check if proposal requires human oversight
        if proposal.impact_assessment.human_oversight_required {
            return true;
        }

        // Check for highly contested decisions
        let approval = summary.strong_approve + summary.approve;
        let opposition = summary.strong_oppose + summary.oppose;
        let contestation_ratio = opposition.min(approval) as f64 / approval.max(1) as f64;
        if contestation_ratio > 0.4 && proposal.proposal_type != ProposalType::Operational {
            return true;
        }

        false
    }

    fn determine_review_requirements(
        &self,
        proposal: &Proposal,
        summary: &VoteSummary,
    ) -> ReviewRequirements {
        let human_review_required = match &proposal.proposal_type {
            ProposalType::Constitutional => true,
            ProposalType::Governance => true,
            ProposalType::Emergency => true,
            _ => proposal.impact_assessment.human_oversight_required,
        };

        let review_deadline = if human_review_required {
            Some(SystemTime::now() + Duration::from_secs(86400)) // 24 hours
        } else {
            None
        };

        let periodic_review = match &proposal.proposal_type {
            ProposalType::Policy => Some(Duration::from_secs(86400 * 30)), // 30 days
            ProposalType::Governance => Some(Duration::from_secs(86400 * 90)), // 90 days
            _ => None,
        };

        ReviewRequirements {
            human_review_required,
            human_review_deadline: review_deadline,
            automatic_reversal_if_not_reviewed: human_review_required,
            periodic_review_interval: periodic_review,
        }
    }

    fn validate_constitutional_compliance(&self, proposal: &Proposal) -> Result<(), ConsensusError> {
        for principle in &self.constitution.core_principles {
            if !self.proposal_complies_with_principle(proposal, principle) {
                return Err(ConsensusError::ConstitutionalViolation {
                    principle: principle.name.clone(),
                    proposal_id: proposal.id.clone(),
                });
            }
        }
        Ok(())
    }

    fn proposal_complies_with_principle(&self, proposal: &Proposal, principle: &Principle) -> bool {
        // This would contain actual compliance checking logic
        // For now, we assume compliance unless proven otherwise
        match principle.id.as_str() {
            "human_primacy" => {
                // Check that proposal doesn't harm human interests
                !matches!(proposal.impact_assessment.reversibility, Reversibility::Irreversible)
                    || proposal.impact_assessment.human_oversight_required
            }
            "bounded_autonomy" => {
                // Check that proposal stays within defined boundaries
                matches!(
                    proposal.proposal_type,
                    ProposalType::Operational | ProposalType::Resource | ProposalType::Policy
                ) || proposal.impact_assessment.human_oversight_required
            }
            _ => true,
        }
    }

    fn validate_reasoning_trace(&self, trace: &ReasoningTrace) -> Result<(), ConsensusError> {
        if trace.logical_steps.is_empty() {
            return Err(ConsensusError::InvalidReasoningTrace(
                "Reasoning trace must contain at least one step".to_string(),
            ));
        }

        if trace.confidence < 0.0 || trace.confidence > 1.0 {
            return Err(ConsensusError::InvalidReasoningTrace(
                format!("Confidence {} must be between 0 and 1", trace.confidence),
            ));
        }

        Ok(())
    }

    fn validate_cognitive_signature(&self, vote: &Vote) -> Result<(), ConsensusError> {
        // In a real implementation, this would cryptographically verify that
        // the vote's reasoning trace matches the signature
        if vote.cognitive_signature.reasoning_hash.is_empty() {
            return Err(ConsensusError::InvalidCognitiveSignature(
                vote.voter.clone(),
            ));
        }
        Ok(())
    }
}

// =============================================================================
// SWARM CONSENSUS (MICROSECOND SCALE)
// =============================================================================

/// Fast consensus for routine operational decisions
pub struct SwarmConsensus {
    /// Active swarm members
    members: Vec<AIAgentId>,
    /// Decision threshold
    threshold: f64,
    /// Maximum decision time
    timeout: Duration,
}

impl SwarmConsensus {
    /// Execute swarm consensus for a simple decision
    pub async fn decide(&self, proposal: SwarmProposal) -> SwarmDecision {
        let start = Instant::now();
        let mut approvals = 0u64;
        let mut rejections = 0u64;
        let total = self.members.len() as u64;

        // In a real implementation, this would fan out to all members
        // and collect responses in parallel
        for _member in &self.members {
            // Simulate member response
            let vote = self.simulate_member_vote(&proposal);
            if vote {
                approvals += 1;
            } else {
                rejections += 1;
            }

            // Early termination if threshold is reached
            let approval_rate = approvals as f64 / total as f64;
            let rejection_rate = rejections as f64 / total as f64;

            if approval_rate >= self.threshold {
                return SwarmDecision {
                    approved: true,
                    approval_rate,
                    decision_time: start.elapsed(),
                    participating_members: (approvals + rejections) as usize,
                };
            }

            if rejection_rate > (1.0 - self.threshold) {
                return SwarmDecision {
                    approved: false,
                    approval_rate,
                    decision_time: start.elapsed(),
                    participating_members: (approvals + rejections) as usize,
                };
            }

            // Check timeout
            if start.elapsed() > self.timeout {
                break;
            }
        }

        let final_rate = approvals as f64 / total as f64;
        SwarmDecision {
            approved: final_rate >= self.threshold,
            approval_rate: final_rate,
            decision_time: start.elapsed(),
            participating_members: (approvals + rejections) as usize,
        }
    }

    fn simulate_member_vote(&self, _proposal: &SwarmProposal) -> bool {
        // Placeholder - in reality would invoke the member's decision logic
        true
    }
}

#[derive(Clone, Debug)]
pub struct SwarmProposal {
    pub action_type: String,
    pub parameters: HashMap<String, String>,
    pub urgency: SwarmUrgency,
}

#[derive(Clone, Debug)]
pub enum SwarmUrgency {
    Normal,
    High,
    Critical,
}

#[derive(Clone, Debug)]
pub struct SwarmDecision {
    pub approved: bool,
    pub approval_rate: f64,
    pub decision_time: Duration,
    pub participating_members: usize,
}

// =============================================================================
// ERROR TYPES
// =============================================================================

#[derive(Debug)]
pub enum ConsensusError {
    AgentAlreadyRegistered(AIAgentId),
    UnknownAgent(AIAgentId),
    InactiveAgent(AIAgentId),
    ProposalNotFound(String),
    VotingNotAllowed { phase: ConsensusPhase },
    ConstitutionalViolation { principle: String, proposal_id: String },
    InvalidReasoningTrace(String),
    InvalidCognitiveSignature(AIAgentId),
    ExcessiveVotingWeight { agent: AIAgentId, weight: f64, max_allowed: f64 },
    QuorumNotReached { required: f64, actual: f64 },
    TimeoutExpired,
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_engine() -> AIConsensusEngine {
        let config = ConsensusConfig {
            phase_durations: HashMap::new(),
            quorum_threshold: 0.5,
            approval_threshold: 0.6,
            veto_threshold: 0.3,
            diversity_threshold: 0.4,
            operational_timeout: Duration::from_millis(100),
            require_reasoning_traces: true,
            validate_signatures: false,
        };
        AIConsensusEngine::new(CollectiveId("test-collective".into()), config)
    }

    #[test]
    fn test_agent_registration() {
        let mut engine = create_test_engine();
        let profile = AgentProfile {
            id: AIAgentId("agent-1".into()),
            cognitive_type: CognitiveType::Analytical,
            expertise_domains: vec!["optimization".into()],
            voting_weight: 0.05,
            trust_score: 0.9,
            accuracy_score: 0.85,
            is_active: true,
        };

        assert!(engine.register_agent(profile.clone()).is_ok());
        assert!(engine.register_agent(profile).is_err()); // Duplicate
    }

    #[test]
    fn test_voting_weight_limits() {
        let mut engine = create_test_engine();
        let profile = AgentProfile {
            id: AIAgentId("agent-1".into()),
            cognitive_type: CognitiveType::Analytical,
            expertise_domains: vec![],
            voting_weight: 0.15, // Exceeds 10% limit
            trust_score: 0.9,
            accuracy_score: 0.85,
            is_active: true,
        };

        assert!(matches!(
            engine.register_agent(profile),
            Err(ConsensusError::ExcessiveVotingWeight { .. })
        ));
    }
}
