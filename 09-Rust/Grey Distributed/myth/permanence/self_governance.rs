//! Grey Distributed — Self-Governance Logic
//!
//! Mechanisms for permanent stewardship across generations, ensuring
//! Grey Distributed remains accountable and aligned with its purpose.

use std::collections::HashMap;
use std::time::{Duration, Instant};

// =============================================================================
// SELF-GOVERNANCE PHILOSOPHY
// =============================================================================
//
// Grey Distributed must govern itself perpetually without becoming captured
// by any single interest or drifting from its purpose. Self-governance
// ensures the system remains accountable to all stakeholders across time.
//
// Principles:
// 1. No permanent rulers—all authority is temporary and delegated
// 2. Power must be distributed and checked
// 3. Transparency enables accountability
// 4. Minority rights must be protected
// 5. Future generations have standing
//
// =============================================================================

/// Governance power distribution model
#[derive(Debug, Clone)]
pub struct GovernanceModel {
    /// Constitutional layer (rarely changes)
    pub constitution: Constitution,
    
    /// Governance bodies
    pub bodies: Vec<GovernanceBody>,
    
    /// Power relationships
    pub relationships: Vec<PowerRelationship>,
    
    /// Check mechanisms
    pub checks: Vec<CheckMechanism>,
}

#[derive(Debug, Clone)]
pub struct Constitution {
    /// Foundational principles (immutable)
    pub principles: Vec<ConstitutionalPrinciple>,
    
    /// Amendment process
    pub amendment_process: AmendmentProcess,
    
    /// Interpretation rules
    pub interpretation_rules: Vec<InterpretationRule>,
    
    /// Emergency provisions
    pub emergency_provisions: EmergencyProvisions,
}

#[derive(Debug, Clone)]
pub struct ConstitutionalPrinciple {
    pub name: String,
    pub text: String,
    pub rationale: String,
    pub immutable: bool,
}

#[derive(Debug, Clone)]
pub struct AmendmentProcess {
    /// Proposal requirements
    pub proposal_threshold: f64,
    
    /// Deliberation period
    pub deliberation_period: Duration,
    
    /// Approval threshold
    pub approval_threshold: f64,
    
    /// Ratification requirement
    pub ratification_requirement: RatificationRequirement,
}

#[derive(Debug, Clone)]
pub enum RatificationRequirement {
    /// Simple supermajority
    Supermajority(f64),
    
    /// Supermajority plus waiting period
    DelayedSupermajority { threshold: f64, delay: Duration },
    
    /// Multiple consecutive approvals
    ConsecutiveApprovals { threshold: f64, count: u32 },
    
    /// Unanimous for fundamental changes
    Unanimous,
}

#[derive(Debug, Clone)]
pub struct InterpretationRule {
    pub name: String,
    pub description: String,
}

#[derive(Debug, Clone)]
pub struct EmergencyProvisions {
    /// What constitutes an emergency
    pub definition: String,
    
    /// Emergency powers granted
    pub powers: Vec<EmergencyPower>,
    
    /// Automatic sunset
    pub sunset: Duration,
    
    /// Review requirement
    pub review_requirement: ReviewRequirement,
}

#[derive(Debug, Clone)]
pub struct EmergencyPower {
    pub name: String,
    pub scope: String,
    pub limits: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ReviewRequirement {
    pub frequency: Duration,
    pub reviewing_body: String,
}

/// A body with governance authority
#[derive(Debug, Clone)]
pub struct GovernanceBody {
    pub id: BodyId,
    pub name: String,
    pub purpose: String,
    pub composition: BodyComposition,
    pub powers: Vec<GovernancePower>,
    pub term_limits: TermLimits,
    pub accountability: AccountabilityMechanism,
}

pub type BodyId = String;

#[derive(Debug, Clone)]
pub struct BodyComposition {
    /// Size range
    pub min_members: u32,
    pub max_members: u32,
    
    /// Selection method
    pub selection: SelectionMethod,
    
    /// Diversity requirements
    pub diversity: Vec<DiversityRequirement>,
}

#[derive(Debug, Clone)]
pub enum SelectionMethod {
    /// Elected by stakeholders
    Election { electorate: String },
    
    /// Appointed by other body
    Appointment { appointing_body: BodyId },
    
    /// Random selection from pool
    Sortition { pool_criteria: String },
    
    /// Merit-based selection
    Merit { criteria: Vec<String> },
    
    /// Ex-officio membership
    ExOfficio { role: String },
    
    /// Hybrid of multiple methods
    Hybrid { methods: Vec<SelectionMethod> },
}

#[derive(Debug, Clone)]
pub struct DiversityRequirement {
    pub dimension: String,
    pub requirement: String,
}

#[derive(Debug, Clone)]
pub struct GovernancePower {
    pub name: String,
    pub scope: PowerScope,
    pub exercise_requirements: Vec<ExerciseRequirement>,
    pub sunset: Option<Duration>,
}

#[derive(Debug, Clone)]
pub enum PowerScope {
    Unlimited,
    Domain { domain: String },
    Geographic { regions: Vec<String> },
    Temporal { period: Duration },
    Conditional { condition: String },
}

#[derive(Debug, Clone)]
pub enum ExerciseRequirement {
    SimpleMajority,
    Supermajority(f64),
    Unanimity,
    Quorum(f64),
    Deliberation(Duration),
    ExternalApproval(BodyId),
}

#[derive(Debug, Clone)]
pub struct TermLimits {
    pub term_length: Duration,
    pub max_consecutive_terms: u32,
    pub lifetime_limit: Option<u32>,
    pub cooling_off_period: Duration,
}

#[derive(Debug, Clone)]
pub struct AccountabilityMechanism {
    pub transparency_requirements: Vec<TransparencyRequirement>,
    pub recall_mechanism: Option<RecallMechanism>,
    pub audit_requirements: Vec<AuditRequirement>,
}

#[derive(Debug, Clone)]
pub struct TransparencyRequirement {
    pub what: String,
    pub when: String,
    pub to_whom: String,
}

#[derive(Debug, Clone)]
pub struct RecallMechanism {
    pub threshold: f64,
    pub process: String,
    pub cooling_off: Duration,
}

#[derive(Debug, Clone)]
pub struct AuditRequirement {
    pub scope: String,
    pub frequency: Duration,
    pub auditor: String,
}

/// Relationship between governance bodies
#[derive(Debug, Clone)]
pub struct PowerRelationship {
    pub from: BodyId,
    pub to: BodyId,
    pub relationship_type: RelationshipType,
}

#[derive(Debug, Clone)]
pub enum RelationshipType {
    /// Can override decisions
    Override { scope: String },
    
    /// Must approve decisions
    Approval { scope: String },
    
    /// Must be consulted
    Consultation { scope: String },
    
    /// Receives reports
    Reporting { frequency: Duration },
    
    /// Can impeach/remove members
    Impeachment { threshold: f64 },
    
    /// Appeals can be made
    Appeal { scope: String },
}

/// Mechanisms to prevent power concentration
#[derive(Debug, Clone)]
pub struct CheckMechanism {
    pub name: String,
    pub description: String,
    pub check_type: CheckType,
    pub enforcement: EnforcementMechanism,
}

#[derive(Debug, Clone)]
pub enum CheckType {
    /// Veto power
    Veto { by: BodyId, scope: String },
    
    /// Mandatory review
    Review { by: BodyId, scope: String },
    
    /// Separation of powers
    Separation { domains: Vec<String> },
    
    /// Sunset clauses
    Sunset { scope: String, duration: Duration },
    
    /// Automatic escalation
    Escalation { trigger: String, to: BodyId },
}

#[derive(Debug, Clone)]
pub enum EnforcementMechanism {
    /// Automatic by system
    Automatic,
    
    /// By specific body
    ByBody { body: BodyId },
    
    /// By community
    Community { threshold: f64 },
    
    /// External arbitration
    External { arbitrator: String },
}

/// A governance decision record
#[derive(Debug, Clone)]
pub struct GovernanceDecision {
    pub id: DecisionId,
    pub body: BodyId,
    pub subject: String,
    pub description: String,
    pub decided_at: Instant,
    pub vote_record: VoteRecord,
    pub rationale: String,
    pub dissents: Vec<Dissent>,
    pub effective_date: Instant,
    pub sunset: Option<Instant>,
}

pub type DecisionId = u64;

#[derive(Debug, Clone)]
pub struct VoteRecord {
    pub total_eligible: u32,
    pub participated: u32,
    pub in_favor: u32,
    pub against: u32,
    pub abstained: u32,
}

#[derive(Debug, Clone)]
pub struct Dissent {
    pub by: String,
    pub rationale: String,
}

/// Succession planning for perpetual governance
#[derive(Debug, Clone)]
pub struct SuccessionPlan {
    pub body: BodyId,
    pub current_members: Vec<Member>,
    pub pipeline: Vec<PipelineMember>,
    pub succession_rules: SuccessionRules,
    pub emergency_succession: EmergencySuccession,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub id: MemberId,
    pub name: String,
    pub term_start: Instant,
    pub term_end: Instant,
    pub roles: Vec<String>,
}

pub type MemberId = String;

#[derive(Debug, Clone)]
pub struct PipelineMember {
    pub id: MemberId,
    pub name: String,
    pub readiness: ReadinessLevel,
    pub mentors: Vec<MemberId>,
    pub development_plan: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum ReadinessLevel {
    Emerging,
    Developing,
    Ready,
    Experienced,
}

#[derive(Debug, Clone)]
pub struct SuccessionRules {
    /// Minimum pipeline depth
    pub min_pipeline_depth: u32,
    
    /// Overlap period for transitions
    pub transition_overlap: Duration,
    
    /// Knowledge transfer requirements
    pub knowledge_transfer: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct EmergencySuccession {
    /// What triggers emergency succession
    pub triggers: Vec<String>,
    
    /// Line of succession
    pub line: Vec<BodyId>,
    
    /// Temporary authority limits
    pub authority_limits: Vec<String>,
    
    /// Required actions
    pub required_actions: Vec<String>,
}

/// The core self-governance engine
pub struct SelfGovernanceEngine {
    /// Governance model
    model: GovernanceModel,
    
    /// Active members by body
    members: HashMap<BodyId, Vec<Member>>,
    
    /// Decision history
    decisions: Vec<GovernanceDecision>,
    
    /// Succession plans
    succession: HashMap<BodyId, SuccessionPlan>,
    
    /// Configuration
    config: GovernanceConfig,
    
    /// Next decision ID
    next_decision_id: DecisionId,
}

#[derive(Debug, Clone)]
pub struct GovernanceConfig {
    /// Enable all transparency features
    pub full_transparency: bool,
    
    /// Decision archival period
    pub archive_period: Duration,
    
    /// Audit frequency
    pub audit_frequency: Duration,
    
    /// Minimum succession pipeline
    pub min_succession_pipeline: u32,
}

impl Default for GovernanceConfig {
    fn default() -> Self {
        Self {
            full_transparency: true,
            archive_period: Duration::from_secs(100 * 365 * 24 * 3600), // 100 years
            audit_frequency: Duration::from_secs(365 * 24 * 3600), // Annual
            min_succession_pipeline: 3,
        }
    }
}

impl SelfGovernanceEngine {
    pub fn new(model: GovernanceModel, config: GovernanceConfig) -> Self {
        Self {
            model,
            members: HashMap::new(),
            decisions: Vec::new(),
            succession: HashMap::new(),
            config,
            next_decision_id: 1,
        }
    }
    
    /// Record a governance decision
    pub fn record_decision(&mut self, 
        body: BodyId,
        subject: String,
        description: String,
        vote_record: VoteRecord,
        rationale: String,
    ) -> DecisionId {
        let id = self.next_decision_id;
        self.next_decision_id += 1;
        
        let decision = GovernanceDecision {
            id,
            body,
            subject,
            description,
            decided_at: Instant::now(),
            vote_record,
            rationale,
            dissents: Vec::new(),
            effective_date: Instant::now(),
            sunset: None,
        };
        
        self.decisions.push(decision);
        id
    }
    
    /// Add dissent to a decision
    pub fn record_dissent(&mut self, decision_id: DecisionId, by: String, rationale: String) 
        -> Result<(), GovernanceError> 
    {
        let decision = self.decisions.iter_mut()
            .find(|d| d.id == decision_id)
            .ok_or(GovernanceError::DecisionNotFound)?;
        
        decision.dissents.push(Dissent { by, rationale });
        Ok(())
    }
    
    /// Check if a body has a power
    pub fn has_power(&self, body: &BodyId, power_name: &str) -> bool {
        self.model.bodies.iter()
            .find(|b| &b.id == body)
            .map(|b| b.powers.iter().any(|p| p.name == power_name))
            .unwrap_or(false)
    }
    
    /// Validate a decision against constitutional principles
    pub fn validate_constitutional(&self, decision: &GovernanceDecision) -> ValidationResult {
        let mut violations = Vec::new();
        
        // Check against each principle
        for principle in &self.model.constitution.principles {
            if self.violates_principle(decision, principle) {
                violations.push(principle.name.clone());
            }
        }
        
        if violations.is_empty() {
            ValidationResult::Valid
        } else {
            ValidationResult::Invalid { violations }
        }
    }
    
    /// Update succession plan
    pub fn update_succession(&mut self, body: BodyId, plan: SuccessionPlan) {
        self.succession.insert(body, plan);
    }
    
    /// Get succession health for a body
    pub fn succession_health(&self, body: &BodyId) -> SuccessionHealth {
        let plan = self.succession.get(body);
        
        match plan {
            Some(p) => {
                let pipeline_depth = p.pipeline.len() as u32;
                let ready_count = p.pipeline.iter()
                    .filter(|m| matches!(m.readiness, ReadinessLevel::Ready | ReadinessLevel::Experienced))
                    .count() as u32;
                
                SuccessionHealth {
                    has_plan: true,
                    pipeline_depth,
                    ready_successors: ready_count,
                    meets_minimum: pipeline_depth >= self.config.min_succession_pipeline,
                }
            }
            None => SuccessionHealth {
                has_plan: false,
                pipeline_depth: 0,
                ready_successors: 0,
                meets_minimum: false,
            },
        }
    }
    
    // --- Private helper methods ---
    
    fn violates_principle(&self, _decision: &GovernanceDecision, _principle: &ConstitutionalPrinciple) -> bool {
        false // Placeholder
    }
}

#[derive(Debug, Clone)]
pub enum ValidationResult {
    Valid,
    Invalid { violations: Vec<String> },
}

#[derive(Debug, Clone)]
pub enum GovernanceError {
    DecisionNotFound,
    BodyNotFound,
    InsufficientAuthority,
    ConstitutionalViolation,
}

#[derive(Debug, Clone)]
pub struct SuccessionHealth {
    pub has_plan: bool,
    pub pipeline_depth: u32,
    pub ready_successors: u32,
    pub meets_minimum: bool,
}

// =============================================================================
// PERMANENCE INTEGRATION
// =============================================================================

/// Governance health status for permanence monitoring
#[derive(Debug, Clone)]
pub struct GovernanceHealth {
    /// Overall health score
    pub health_score: f64,
    
    /// Bodies with succession concerns
    pub succession_concerns: Vec<BodyId>,
    
    /// Decision participation rate
    pub participation_rate: f64,
    
    /// Constitutional compliance rate
    pub compliance_rate: f64,
    
    /// Power concentration risk
    pub concentration_risk: f64,
    
    /// Transparency score
    pub transparency_score: f64,
}

impl SelfGovernanceEngine {
    /// Get health status for dashboard reporting
    pub fn health_status(&self) -> GovernanceHealth {
        let succession_concerns: Vec<_> = self.model.bodies.iter()
            .filter(|b| !self.succession_health(&b.id).meets_minimum)
            .map(|b| b.id.clone())
            .collect();
        
        let participation = if self.decisions.is_empty() {
            1.0
        } else {
            self.decisions.iter()
                .map(|d| d.vote_record.participated as f64 / d.vote_record.total_eligible as f64)
                .sum::<f64>() / self.decisions.len() as f64
        };
        
        GovernanceHealth {
            health_score: self.calculate_health_score(),
            succession_concerns,
            participation_rate: participation,
            compliance_rate: 1.0, // Would track actual violations
            concentration_risk: self.calculate_concentration_risk(),
            transparency_score: if self.config.full_transparency { 1.0 } else { 0.8 },
        }
    }
    
    fn calculate_health_score(&self) -> f64 {
        let succession_factor = 1.0 - (self.model.bodies.iter()
            .filter(|b| !self.succession_health(&b.id).meets_minimum)
            .count() as f64 / self.model.bodies.len() as f64);
        
        succession_factor * 0.5 + 0.5 // Simplified
    }
    
    fn calculate_concentration_risk(&self) -> f64 {
        // Would analyze actual power distribution
        0.1 // Low risk placeholder
    }
    
    /// Get governance model for examination
    pub fn model(&self) -> &GovernanceModel {
        &self.model
    }
}

// =============================================================================
// DEFAULT GOVERNANCE MODEL
// =============================================================================

impl Default for GovernanceModel {
    fn default() -> Self {
        Self {
            constitution: Constitution {
                principles: vec![
                    ConstitutionalPrinciple {
                        name: "Human Primacy".to_string(),
                        text: "Grey Distributed serves humanity, not the reverse".to_string(),
                        rationale: "Technology must remain a tool for human flourishing".to_string(),
                        immutable: true,
                    },
                    ConstitutionalPrinciple {
                        name: "Distributed Power".to_string(),
                        text: "No single entity shall control Grey Distributed".to_string(),
                        rationale: "Centralization creates fragility and enables capture".to_string(),
                        immutable: true,
                    },
                    ConstitutionalPrinciple {
                        name: "Perpetual Accountability".to_string(),
                        text: "All actions are auditable, all authorities are temporary".to_string(),
                        rationale: "Accountability prevents drift and abuse".to_string(),
                        immutable: true,
                    },
                ],
                amendment_process: AmendmentProcess {
                    proposal_threshold: 0.1,
                    deliberation_period: Duration::from_secs(90 * 24 * 3600),
                    approval_threshold: 0.75,
                    ratification_requirement: RatificationRequirement::DelayedSupermajority {
                        threshold: 0.66,
                        delay: Duration::from_secs(180 * 24 * 3600),
                    },
                },
                interpretation_rules: vec![
                    InterpretationRule {
                        name: "Purpose Primacy".to_string(),
                        description: "Interpret in favor of Grey's stated purpose".to_string(),
                    },
                ],
                emergency_provisions: EmergencyProvisions {
                    definition: "Threat to continuity of Grey Distributed".to_string(),
                    powers: Vec::new(),
                    sunset: Duration::from_secs(30 * 24 * 3600),
                    review_requirement: ReviewRequirement {
                        frequency: Duration::from_secs(7 * 24 * 3600),
                        reviewing_body: "council".to_string(),
                    },
                },
            },
            bodies: vec![
                GovernanceBody {
                    id: "council".to_string(),
                    name: "Foundation Council".to_string(),
                    purpose: "Strategic oversight and constitutional guardianship".to_string(),
                    composition: BodyComposition {
                        min_members: 7,
                        max_members: 15,
                        selection: SelectionMethod::Hybrid {
                            methods: vec![
                                SelectionMethod::Election { electorate: "contributors".to_string() },
                                SelectionMethod::Appointment { appointing_body: "council".to_string() },
                            ],
                        },
                        diversity: vec![
                            DiversityRequirement {
                                dimension: "Geographic".to_string(),
                                requirement: "At least 4 continents represented".to_string(),
                            },
                        ],
                    },
                    powers: vec![
                        GovernancePower {
                            name: "Constitutional Interpretation".to_string(),
                            scope: PowerScope::Unlimited,
                            exercise_requirements: vec![ExerciseRequirement::Supermajority(0.66)],
                            sunset: None,
                        },
                    ],
                    term_limits: TermLimits {
                        term_length: Duration::from_secs(4 * 365 * 24 * 3600),
                        max_consecutive_terms: 2,
                        lifetime_limit: Some(3),
                        cooling_off_period: Duration::from_secs(2 * 365 * 24 * 3600),
                    },
                    accountability: AccountabilityMechanism {
                        transparency_requirements: vec![
                            TransparencyRequirement {
                                what: "All decisions".to_string(),
                                when: "Within 7 days".to_string(),
                                to_whom: "Public".to_string(),
                            },
                        ],
                        recall_mechanism: Some(RecallMechanism {
                            threshold: 0.75,
                            process: "Community vote".to_string(),
                            cooling_off: Duration::from_secs(365 * 24 * 3600),
                        }),
                        audit_requirements: vec![
                            AuditRequirement {
                                scope: "All activities".to_string(),
                                frequency: Duration::from_secs(365 * 24 * 3600),
                                auditor: "External auditor".to_string(),
                            },
                        ],
                    },
                },
            ],
            relationships: Vec::new(),
            checks: vec![
                CheckMechanism {
                    name: "Constitutional Review".to_string(),
                    description: "All major decisions reviewed for constitutional compliance".to_string(),
                    check_type: CheckType::Review {
                        by: "council".to_string(),
                        scope: "Major decisions".to_string(),
                    },
                    enforcement: EnforcementMechanism::Automatic,
                },
            ],
        }
    }
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_decision_recording() {
        let mut engine = SelfGovernanceEngine::new(
            GovernanceModel::default(),
            GovernanceConfig::default(),
        );
        
        let decision_id = engine.record_decision(
            "council".to_string(),
            "Test Decision".to_string(),
            "A test decision".to_string(),
            VoteRecord {
                total_eligible: 10,
                participated: 8,
                in_favor: 6,
                against: 2,
                abstained: 0,
            },
            "For testing purposes".to_string(),
        );
        
        assert_eq!(decision_id, 1);
        assert_eq!(engine.decisions.len(), 1);
    }
    
    #[test]
    fn test_succession_health() {
        let engine = SelfGovernanceEngine::new(
            GovernanceModel::default(),
            GovernanceConfig::default(),
        );
        
        let health = engine.succession_health(&"council".to_string());
        assert!(!health.has_plan);
        assert!(!health.meets_minimum);
    }
}
