//! Governance Collapse Fallback
//!
//! Handles distributed system operation during failures of organizational
//! governance, legal frameworks, and coordination mechanisms.
//!
//! Key challenges addressed:
//! - Organizational collapse
//! - Legal jurisdiction failures
//! - Regulatory uncertainty
//! - Stakeholder defection
//! - Emergency decision-making

use std::collections::{HashMap, HashSet};
use std::time::Duration;
use serde::{Deserialize, Serialize};

// =============================================================================
// GOVERNANCE FAILURE MODEL
// =============================================================================

/// Types of governance failures
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GovernanceFailure {
    /// Organizational leadership failure
    LeadershipVacuum {
        affected_roles: Vec<Role>,
        cause: VacuumCause,
        succession_available: bool,
    },
    
    /// Legal/regulatory framework collapse
    LegalFrameworkCollapse {
        jurisdictions: Vec<Jurisdiction>,
        affected_regulations: Vec<String>,
        compliance_status: ComplianceStatus,
    },
    
    /// Stakeholder coordination failure
    StakeholderDefection {
        defecting_parties: Vec<StakeholderId>,
        remaining_parties: Vec<StakeholderId>,
        quorum_implications: QuorumStatus,
    },
    
    /// Financial governance failure
    FinancialGovernanceFailure {
        failure_type: FinancialFailureType,
        affected_funds: f64,
        recovery_options: Vec<String>,
    },
    
    /// Technical governance breakdown
    TechnicalGovernanceBreakdown {
        affected_areas: Vec<TechnicalArea>,
        decision_backlog: usize,
        critical_decisions_pending: Vec<String>,
    },
    
    /// Complete organizational collapse
    OrganizationalCollapse {
        cause: String,
        remaining_personnel: usize,
        automated_systems_operational: bool,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Role {
    pub title: String,
    pub authority_level: AuthorityLevel,
    pub critical: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AuthorityLevel {
    Board,
    Executive,
    Director,
    Manager,
    Operator,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VacuumCause {
    Resignation,
    Incapacitation,
    Conflict,
    External,
    Unknown,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Jurisdiction {
    pub id: String,
    pub name: String,
    pub legal_system: String,
    pub operational: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ComplianceStatus {
    Compliant,
    Uncertain,
    NonCompliant,
    Inapplicable,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum QuorumStatus {
    HasQuorum,
    AtRisk,
    NoQuorum,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FinancialFailureType {
    TreasuryExhausted,
    FundingFrozen,
    AuditFailure,
    FraudDetected,
    RegulatorySeizure,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum TechnicalArea {
    Architecture,
    Security,
    Operations,
    Development,
    Compliance,
}

// =============================================================================
// SUCCESSION MANAGEMENT
// =============================================================================

/// Manages succession during leadership failures
#[derive(Debug, Clone)]
pub struct SuccessionManager {
    /// Succession chains for each role
    succession_chains: HashMap<String, Vec<Successor>>,
    
    /// Currently active personnel
    active_personnel: HashMap<String, PersonnelStatus>,
    
    /// Emergency contacts
    emergency_contacts: Vec<EmergencyContact>,
    
    /// Automated authority delegation
    automated_delegation: HashMap<String, DelegationRule>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Successor {
    pub person_id: String,
    pub name: String,
    pub priority: u32,
    pub conditions: Vec<SuccessionCondition>,
    pub authority_scope: AuthorityScope,
    pub contact_info: ContactInfo,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SuccessionCondition {
    Immediate,
    AfterHours(u64),
    RequiresConfirmation,
    RequiresQuorum,
    EmergencyOnly,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthorityScope {
    pub can_approve_changes: bool,
    pub can_authorize_spending: Option<f64>,
    pub can_modify_access: bool,
    pub can_shutdown_systems: bool,
    pub can_represent_externally: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContactInfo {
    pub primary_phone: Option<String>,
    pub secondary_phone: Option<String>,
    pub email: Option<String>,
    pub signal: Option<String>,
    pub physical_location: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PersonnelStatus {
    pub person_id: String,
    pub available: bool,
    pub current_role: Option<String>,
    pub last_check_in: Timestamp,
    pub location: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmergencyContact {
    pub name: String,
    pub role: String,
    pub contact_info: ContactInfo,
    pub authority_in_emergency: AuthorityScope,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DelegationRule {
    pub role: String,
    pub trigger: DelegationTrigger,
    pub delegate_to: String,
    pub scope: AuthorityScope,
    pub duration: Option<Duration>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DelegationTrigger {
    Unavailable(Duration),
    HealthCheck(String),
    ManualActivation,
    EmergencyDeclared,
}

impl SuccessionManager {
    /// Activate succession for a role
    pub async fn activate_succession(&mut self, role: &str) -> SuccessionResult {
        let chain = self.succession_chains.get(role);
        
        match chain {
            Some(chain) if !chain.is_empty() => {
                // Find first available successor
                for successor in chain {
                    let status = self.active_personnel.get(&successor.person_id);
                    
                    if status.map(|s| s.available).unwrap_or(false) {
                        // Check conditions
                        if self.check_conditions(&successor.conditions) {
                            return SuccessionResult::Activated {
                                role: role.to_string(),
                                successor: successor.clone(),
                            };
                        }
                    }
                }
                
                SuccessionResult::NoAvailableSuccessor {
                    role: role.to_string(),
                    chain_length: chain.len(),
                }
            }
            Some(_) | None => SuccessionResult::NoSuccessionChain {
                role: role.to_string(),
            },
        }
    }
    
    /// Check all succession chains for gaps
    pub fn audit_succession_chains(&self) -> SuccessionAudit {
        let mut audit = SuccessionAudit::new();
        
        for (role, chain) in &self.succession_chains {
            if chain.is_empty() {
                audit.add_critical_gap(role.clone());
            } else if chain.len() < 2 {
                audit.add_warning(role.clone(), "Insufficient succession depth".to_string());
            } else {
                // Check if any successors are available
                let available = chain.iter()
                    .filter(|s| self.active_personnel.get(&s.person_id)
                        .map(|p| p.available).unwrap_or(false))
                    .count();
                
                if available == 0 {
                    audit.add_warning(role.clone(), "No successors currently available".to_string());
                }
            }
        }
        
        audit
    }
    
    fn check_conditions(&self, conditions: &[SuccessionCondition]) -> bool {
        conditions.iter().all(|c| match c {
            SuccessionCondition::Immediate => true,
            SuccessionCondition::EmergencyOnly => true, // Assume we're in emergency
            _ => true, // Simplified
        })
    }
}

// =============================================================================
// AUTONOMOUS OPERATION
// =============================================================================

/// Enables autonomous system operation without human governance
#[derive(Debug, Clone)]
pub struct AutonomousGovernance {
    /// Current autonomy level
    autonomy_level: AutonomyLevel,
    
    /// Automated decision rules
    decision_rules: Vec<AutomatedDecisionRule>,
    
    /// Human override requirements
    override_requirements: HashMap<DecisionType, OverrideRequirement>,
    
    /// Audit log of automated decisions
    decision_log: Vec<AutomatedDecision>,
    
    /// Hard limits on autonomous actions
    hard_limits: HardLimits,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AutonomyLevel {
    /// Full human oversight required
    Normal,
    
    /// Routine decisions automated
    Elevated,
    
    /// Most decisions automated
    High,
    
    /// Emergency autonomous operation
    Emergency,
    
    /// Full autonomy (last resort)
    Full,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutomatedDecisionRule {
    pub id: String,
    pub decision_type: DecisionType,
    pub conditions: Vec<DecisionCondition>,
    pub action: AutomatedAction,
    pub requires_confirmation: bool,
    pub reversible: bool,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum DecisionType {
    ResourceAllocation,
    SecurityResponse,
    ServiceDegradation,
    CostManagement,
    CapacityPlanning,
    IncidentResponse,
    AccessControl,
    DataRetention,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DecisionCondition {
    ThresholdExceeded { metric: String, value: f64 },
    TimeElapsed { duration: Duration },
    NoHumanResponse { timeout: Duration },
    EmergencyDeclared,
    PredefinedSchedule,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AutomatedAction {
    ScaleResources { direction: String, percentage: f64 },
    ActivateDefenses { level: String },
    DegradeService { tier: String },
    ReduceSpending { percentage: f64 },
    RotateCredentials,
    IsolateComponent { component: String },
    NotifyStakeholders { message: String },
    ArchiveData { policy: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OverrideRequirement {
    pub decision_type: DecisionType,
    pub required_authority: AuthorityLevel,
    pub confirmation_count: usize,
    pub timeout: Duration,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AutomatedDecision {
    pub id: String,
    pub timestamp: Timestamp,
    pub decision_type: DecisionType,
    pub rule_id: String,
    pub action_taken: AutomatedAction,
    pub human_notified: bool,
    pub reversible: bool,
    pub reversed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HardLimits {
    /// Maximum spending without human approval
    pub max_autonomous_spend: f64,
    
    /// Maximum users that can be affected
    pub max_users_affected: u64,
    
    /// Actions never taken autonomously
    pub prohibited_actions: Vec<String>,
    
    /// Maximum duration of autonomous operation
    pub max_autonomous_duration: Duration,
}

impl AutonomousGovernance {
    /// Escalate autonomy level
    pub async fn escalate_autonomy(&mut self, reason: &str) -> EscalationResult {
        let new_level = match self.autonomy_level {
            AutonomyLevel::Normal => AutonomyLevel::Elevated,
            AutonomyLevel::Elevated => AutonomyLevel::High,
            AutonomyLevel::High => AutonomyLevel::Emergency,
            AutonomyLevel::Emergency => AutonomyLevel::Full,
            AutonomyLevel::Full => return EscalationResult::AlreadyMaximum,
        };
        
        let previous = self.autonomy_level;
        self.autonomy_level = new_level;
        
        EscalationResult::Escalated {
            from: previous,
            to: new_level,
            reason: reason.to_string(),
            timestamp: Timestamp::now(),
        }
    }
    
    /// Make automated decision
    pub async fn make_decision(&mut self, decision_type: DecisionType) -> DecisionResult {
        // Check if decision type is allowed at current autonomy level
        if !self.is_decision_allowed(decision_type) {
            return DecisionResult::NotAllowed {
                reason: "Decision type not allowed at current autonomy level".to_string(),
            };
        }
        
        // Find applicable rule
        let rule = self.decision_rules.iter()
            .find(|r| r.decision_type == decision_type);
        
        match rule {
            Some(rule) if self.check_decision_conditions(&rule.conditions) => {
                // Check hard limits
                if self.would_violate_limits(&rule.action) {
                    return DecisionResult::LimitViolation {
                        action: rule.action.clone(),
                        limit: "Hard limit would be exceeded".to_string(),
                    };
                }
                
                // Execute action
                let decision = AutomatedDecision {
                    id: format!("decision-{}", Timestamp::now()),
                    timestamp: Timestamp::now(),
                    decision_type,
                    rule_id: rule.id.clone(),
                    action_taken: rule.action.clone(),
                    human_notified: true,
                    reversible: rule.reversible,
                    reversed: false,
                };
                
                self.decision_log.push(decision.clone());
                
                DecisionResult::Executed {
                    decision,
                }
            }
            Some(_) => DecisionResult::ConditionsNotMet,
            None => DecisionResult::NoApplicableRule,
        }
    }
    
    fn is_decision_allowed(&self, decision_type: DecisionType) -> bool {
        match self.autonomy_level {
            AutonomyLevel::Normal => false,
            AutonomyLevel::Elevated => matches!(decision_type, 
                DecisionType::ResourceAllocation | DecisionType::IncidentResponse),
            AutonomyLevel::High => !matches!(decision_type,
                DecisionType::AccessControl | DecisionType::DataRetention),
            AutonomyLevel::Emergency | AutonomyLevel::Full => true,
        }
    }
    
    fn check_decision_conditions(&self, conditions: &[DecisionCondition]) -> bool {
        conditions.iter().all(|c| match c {
            DecisionCondition::EmergencyDeclared => {
                matches!(self.autonomy_level, AutonomyLevel::Emergency | AutonomyLevel::Full)
            }
            _ => true, // Simplified
        })
    }
    
    fn would_violate_limits(&self, action: &AutomatedAction) -> bool {
        match action {
            AutomatedAction::ReduceSpending { percentage } if *percentage > 50.0 => true,
            _ => false,
        }
    }
}

// =============================================================================
// LEGAL FALLBACK
// =============================================================================

/// Manages operations when legal frameworks fail
#[derive(Debug, Clone)]
pub struct LegalFallbackManager {
    /// Jurisdiction status
    jurisdiction_status: HashMap<String, JurisdictionStatus>,
    
    /// Fallback jurisdictions
    fallback_jurisdictions: Vec<FallbackJurisdiction>,
    
    /// Compliance requirements
    compliance_requirements: Vec<ComplianceRequirement>,
    
    /// Legal agreements status
    agreement_status: HashMap<String, AgreementStatus>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JurisdictionStatus {
    pub id: String,
    pub name: String,
    pub operational: bool,
    pub court_availability: CourtAvailability,
    pub enforcement_capability: EnforcementCapability,
    pub regulatory_status: RegulatoryStatus,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum CourtAvailability {
    Normal,
    Delayed,
    EmergencyOnly,
    Unavailable,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum EnforcementCapability {
    Full,
    Limited,
    Suspended,
    None,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum RegulatoryStatus {
    Active,
    Suspended,
    Unclear,
    Collapsed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FallbackJurisdiction {
    pub jurisdiction_id: String,
    pub priority: u32,
    pub conditions: Vec<FallbackCondition>,
    pub migration_requirements: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FallbackCondition {
    PrimaryUnavailable,
    SpecificRegulation(String),
    EmergencyDeclared,
    Always,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceRequirement {
    pub id: String,
    pub regulation: String,
    pub jurisdiction: String,
    pub status: ComplianceStatus,
    pub fallback_approach: FallbackApproach,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FallbackApproach {
    /// Continue as compliant under force majeure
    ForceMajeure,
    
    /// Migrate to alternative jurisdiction
    Relocate { target: String },
    
    /// Suspend affected operations
    Suspend,
    
    /// Continue with documented risk
    AcceptRisk { justification: String },
    
    /// Seek emergency authorization
    EmergencyAuthorization,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgreementStatus {
    pub agreement_id: String,
    pub counterparty: String,
    pub enforceable: bool,
    pub force_majeure_applicable: bool,
    pub alternative_dispute: Option<String>,
}

impl LegalFallbackManager {
    /// Assess legal situation
    pub fn assess_legal_situation(&self) -> LegalAssessment {
        let failed_jurisdictions: Vec<_> = self.jurisdiction_status.values()
            .filter(|j| !j.operational)
            .map(|j| j.id.clone())
            .collect();
        
        let affected_compliance: Vec<_> = self.compliance_requirements.iter()
            .filter(|c| failed_jurisdictions.contains(&c.jurisdiction))
            .cloned()
            .collect();
        
        let unenforceable_agreements: Vec<_> = self.agreement_status.values()
            .filter(|a| !a.enforceable)
            .map(|a| a.agreement_id.clone())
            .collect();
        
        LegalAssessment {
            failed_jurisdictions,
            affected_compliance,
            unenforceable_agreements,
            recommended_actions: self.generate_legal_recommendations(),
        }
    }
    
    /// Activate fallback for jurisdiction
    pub async fn activate_fallback(&mut self, jurisdiction_id: &str) -> FallbackActivationResult {
        // Find fallback
        let fallback = self.fallback_jurisdictions.iter()
            .find(|f| self.check_fallback_conditions(jurisdiction_id, &f.conditions));
        
        match fallback {
            Some(fb) => {
                FallbackActivationResult::Activated {
                    from: jurisdiction_id.to_string(),
                    to: fb.jurisdiction_id.clone(),
                    requirements: fb.migration_requirements.clone(),
                }
            }
            None => FallbackActivationResult::NoFallbackAvailable,
        }
    }
    
    fn check_fallback_conditions(&self, _primary: &str, conditions: &[FallbackCondition]) -> bool {
        conditions.iter().any(|c| matches!(c, 
            FallbackCondition::PrimaryUnavailable | FallbackCondition::Always))
    }
    
    fn generate_legal_recommendations(&self) -> Vec<String> {
        vec![
            "Document all decisions under force majeure".to_string(),
            "Maintain evidence of good faith compliance attempts".to_string(),
            "Consider alternative dispute resolution mechanisms".to_string(),
        ]
    }
}

// =============================================================================
// GOVERNANCE FAILURE COORDINATOR
// =============================================================================

/// Main coordinator for governance failure response
pub struct GovernanceFailureCoordinator {
    /// Current governance status
    governance_status: GovernanceStatus,
    
    /// Succession management
    succession: SuccessionManager,
    
    /// Autonomous operation
    autonomous: AutonomousGovernance,
    
    /// Legal fallback
    legal: LegalFallbackManager,
    
    /// Emergency measures
    emergency_measures: EmergencyMeasures,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GovernanceStatus {
    pub overall_health: GovernanceHealth,
    pub active_failures: Vec<GovernanceFailure>,
    pub succession_gaps: Vec<String>,
    pub autonomous_decisions_pending: usize,
    pub legal_uncertainties: Vec<String>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum GovernanceHealth {
    Normal,
    Degraded,
    Impaired,
    Critical,
    Failed,
}

#[derive(Debug, Clone)]
pub struct EmergencyMeasures {
    pub declared: bool,
    pub declared_at: Option<Timestamp>,
    pub expected_duration: Option<Duration>,
    pub active_measures: Vec<EmergencyMeasure>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmergencyMeasure {
    pub id: String,
    pub description: String,
    pub authority: String,
    pub expires: Option<Timestamp>,
}

impl GovernanceFailureCoordinator {
    /// Handle governance failure
    pub async fn handle_failure(&mut self, failure: GovernanceFailure) -> GovernanceFailureResponse {
        self.governance_status.active_failures.push(failure.clone());
        
        let mut response = GovernanceFailureResponse::new();
        
        match &failure {
            GovernanceFailure::LeadershipVacuum { affected_roles, succession_available, .. } => {
                // Attempt succession for each role
                for role in affected_roles {
                    let result = self.succession.activate_succession(&role.title).await;
                    response.succession_results.push((role.title.clone(), result));
                }
                
                // Escalate autonomy if succession insufficient
                if !*succession_available {
                    let escalation = self.autonomous.escalate_autonomy("Leadership vacuum").await;
                    response.autonomy_escalation = Some(escalation);
                }
            }
            
            GovernanceFailure::LegalFrameworkCollapse { jurisdictions, .. } => {
                // Activate fallbacks for each affected jurisdiction
                for jurisdiction in jurisdictions {
                    let result = self.legal.activate_fallback(&jurisdiction.id).await;
                    response.legal_fallbacks.push((jurisdiction.id.clone(), result));
                }
            }
            
            GovernanceFailure::StakeholderDefection { quorum_implications, .. } => {
                // If no quorum, escalate to emergency governance
                if matches!(quorum_implications, QuorumStatus::NoQuorum) {
                    self.declare_emergency("Loss of stakeholder quorum").await;
                    response.emergency_declared = true;
                }
            }
            
            GovernanceFailure::OrganizationalCollapse { automated_systems_operational, .. } => {
                // Full autonomous operation
                if *automated_systems_operational {
                    self.autonomous.escalate_autonomy("Organizational collapse").await;
                    self.autonomous.escalate_autonomy("Organizational collapse - full").await;
                }
                response.autonomous_operation = true;
            }
            
            _ => {}
        }
        
        response
    }
    
    /// Declare emergency governance
    async fn declare_emergency(&mut self, reason: &str) {
        self.emergency_measures.declared = true;
        self.emergency_measures.declared_at = Some(Timestamp::now());
        self.emergency_measures.active_measures.push(EmergencyMeasure {
            id: format!("em-{}", Timestamp::now()),
            description: reason.to_string(),
            authority: "System".to_string(),
            expires: Some(Timestamp::now() + 86400 * 30), // 30 days
        });
    }
    
    /// Get governance health assessment
    pub fn health_assessment(&self) -> GovernanceHealthAssessment {
        let succession_audit = self.succession.audit_succession_chains();
        let legal_assessment = self.legal.assess_legal_situation();
        
        let health = if self.governance_status.active_failures.iter().any(|f| 
            matches!(f, GovernanceFailure::OrganizationalCollapse { .. })) {
            GovernanceHealth::Failed
        } else if self.governance_status.active_failures.len() > 3 {
            GovernanceHealth::Critical
        } else if !succession_audit.critical_gaps.is_empty() {
            GovernanceHealth::Impaired
        } else if self.governance_status.active_failures.is_empty() {
            GovernanceHealth::Normal
        } else {
            GovernanceHealth::Degraded
        };
        
        GovernanceHealthAssessment {
            overall_health: health,
            succession_audit,
            legal_assessment,
            autonomy_level: self.autonomous.autonomy_level,
            emergency_active: self.emergency_measures.declared,
            recommendations: self.generate_recommendations(health),
        }
    }
    
    fn generate_recommendations(&self, health: GovernanceHealth) -> Vec<String> {
        match health {
            GovernanceHealth::Failed => vec![
                "Initiate emergency stakeholder contact".to_string(),
                "Activate all automated safeguards".to_string(),
                "Prepare for extended autonomous operation".to_string(),
            ],
            GovernanceHealth::Critical => vec![
                "Review and fill succession gaps immediately".to_string(),
                "Escalate to emergency governance protocols".to_string(),
            ],
            _ => vec![],
        }
    }
}

// =============================================================================
// TYPE DEFINITIONS
// =============================================================================

pub type StakeholderId = String;
pub type Timestamp = u64;

impl Timestamp {
    pub fn now() -> Self {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }
}

#[derive(Debug, Clone)]
pub enum SuccessionResult {
    Activated { role: String, successor: Successor },
    NoAvailableSuccessor { role: String, chain_length: usize },
    NoSuccessionChain { role: String },
}

#[derive(Debug, Clone)]
pub struct SuccessionAudit {
    pub critical_gaps: Vec<String>,
    pub warnings: Vec<(String, String)>,
    pub coverage_score: f64,
}

impl SuccessionAudit {
    pub fn new() -> Self {
        Self {
            critical_gaps: vec![],
            warnings: vec![],
            coverage_score: 1.0,
        }
    }
    
    pub fn add_critical_gap(&mut self, role: String) {
        self.critical_gaps.push(role);
        self.coverage_score -= 0.2;
    }
    
    pub fn add_warning(&mut self, role: String, warning: String) {
        self.warnings.push((role, warning));
        self.coverage_score -= 0.05;
    }
}

#[derive(Debug, Clone)]
pub enum EscalationResult {
    Escalated { from: AutonomyLevel, to: AutonomyLevel, reason: String, timestamp: Timestamp },
    AlreadyMaximum,
}

#[derive(Debug, Clone)]
pub enum DecisionResult {
    Executed { decision: AutomatedDecision },
    NotAllowed { reason: String },
    LimitViolation { action: AutomatedAction, limit: String },
    ConditionsNotMet,
    NoApplicableRule,
}

#[derive(Debug, Clone)]
pub struct LegalAssessment {
    pub failed_jurisdictions: Vec<String>,
    pub affected_compliance: Vec<ComplianceRequirement>,
    pub unenforceable_agreements: Vec<String>,
    pub recommended_actions: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum FallbackActivationResult {
    Activated { from: String, to: String, requirements: Vec<String> },
    NoFallbackAvailable,
}

#[derive(Debug, Clone)]
pub struct GovernanceFailureResponse {
    pub succession_results: Vec<(String, SuccessionResult)>,
    pub autonomy_escalation: Option<EscalationResult>,
    pub legal_fallbacks: Vec<(String, FallbackActivationResult)>,
    pub emergency_declared: bool,
    pub autonomous_operation: bool,
}

impl GovernanceFailureResponse {
    pub fn new() -> Self {
        Self {
            succession_results: vec![],
            autonomy_escalation: None,
            legal_fallbacks: vec![],
            emergency_declared: false,
            autonomous_operation: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GovernanceHealthAssessment {
    pub overall_health: GovernanceHealth,
    pub succession_audit: SuccessionAudit,
    pub legal_assessment: LegalAssessment,
    pub autonomy_level: AutonomyLevel,
    pub emergency_active: bool,
    pub recommendations: Vec<String>,
}
