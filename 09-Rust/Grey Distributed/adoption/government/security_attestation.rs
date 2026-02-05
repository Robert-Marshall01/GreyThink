//! Grey Distributed — Government Security Attestation Module
//!
//! Implements cryptographic attestation for government cluster security posture.
//! Supports FISMA continuous monitoring, FedRAMP POA&M, and cross-agency trust.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                   Attestation Authority                     │
//! │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
//! │  │ Control     │  │ Posture     │  │ Evidence            │ │
//! │  │ Assessor    │  │ Evaluator   │  │ Collector           │ │
//! │  └──────┬──────┘  └──────┬──────┘  └──────────┬──────────┘ │
//! │         │                │                     │            │
//! │         └────────────────┼─────────────────────┘            │
//! │                          ▼                                  │
//! │              ┌──────────────────────┐                       │
//! │              │  Attestation Engine  │                       │
//! │              └──────────┬───────────┘                       │
//! │                         │                                   │
//! │         ┌───────────────┼───────────────┐                   │
//! │         ▼               ▼               ▼                   │
//! │   ┌──────────┐   ┌──────────┐   ┌──────────────┐           │
//! │   │ FISMA    │   │ FedRAMP  │   │ Agency-      │           │
//! │   │ Reporter │   │ POA&M    │   │ Specific     │           │
//! │   └──────────┘   └──────────┘   └──────────────┘           │
//! └─────────────────────────────────────────────────────────────┘
//! ```

use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::RwLock;

// =============================================================================
// Security Classification
// =============================================================================

/// Security classification levels per government standards.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ClassificationLevel {
    /// Unclassified information
    Unclassified,
    /// Controlled Unclassified Information
    CUI,
    /// Confidential
    Confidential,
    /// Secret
    Secret,
    /// Top Secret
    TopSecret,
}

impl ClassificationLevel {
    pub fn marking(&self) -> &'static str {
        match self {
            Self::Unclassified => "UNCLASSIFIED",
            Self::CUI => "CUI",
            Self::Confidential => "CONFIDENTIAL",
            Self::Secret => "SECRET",
            Self::TopSecret => "TOP SECRET",
        }
    }
    
    pub fn color(&self) -> &'static str {
        match self {
            Self::Unclassified => "green",
            Self::CUI => "yellow",
            Self::Confidential => "blue",
            Self::Secret => "red",
            Self::TopSecret => "orange",
        }
    }
    
    pub fn federation_allowed(&self) -> bool {
        matches!(self, Self::Unclassified | Self::CUI)
    }
}

/// Handling caveats for CUI.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HandlingCaveat {
    /// For Official Use Only
    FOUO,
    /// Law Enforcement Sensitive
    LES,
    /// Not Releasable to Foreign Nationals
    NOFORN,
    /// Releasable to specific allies
    REL(Vec<String>),
    /// Originator Controlled
    ORCON,
    /// Custom caveat
    Custom(String),
}

// =============================================================================
// NIST 800-53 Control Families
// =============================================================================

/// NIST 800-53 control families.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ControlFamily {
    /// Access Control (AC)
    AccessControl,
    /// Awareness and Training (AT)
    AwarenessTraining,
    /// Audit and Accountability (AU)
    AuditAccountability,
    /// Assessment, Authorization, and Monitoring (CA)
    AssessmentAuthorization,
    /// Configuration Management (CM)
    ConfigurationManagement,
    /// Contingency Planning (CP)
    ContingencyPlanning,
    /// Identification and Authentication (IA)
    IdentificationAuthentication,
    /// Incident Response (IR)
    IncidentResponse,
    /// Maintenance (MA)
    Maintenance,
    /// Media Protection (MP)
    MediaProtection,
    /// Physical and Environmental Protection (PE)
    PhysicalEnvironmental,
    /// Planning (PL)
    Planning,
    /// Program Management (PM)
    ProgramManagement,
    /// Personnel Security (PS)
    PersonnelSecurity,
    /// PII Processing and Transparency (PT)
    PIIProcessing,
    /// Risk Assessment (RA)
    RiskAssessment,
    /// System and Services Acquisition (SA)
    SystemAcquisition,
    /// System and Communications Protection (SC)
    SystemCommunications,
    /// System and Information Integrity (SI)
    SystemIntegrity,
    /// Supply Chain Risk Management (SR)
    SupplyChain,
}

impl ControlFamily {
    pub fn code(&self) -> &'static str {
        match self {
            Self::AccessControl => "AC",
            Self::AwarenessTraining => "AT",
            Self::AuditAccountability => "AU",
            Self::AssessmentAuthorization => "CA",
            Self::ConfigurationManagement => "CM",
            Self::ContingencyPlanning => "CP",
            Self::IdentificationAuthentication => "IA",
            Self::IncidentResponse => "IR",
            Self::Maintenance => "MA",
            Self::MediaProtection => "MP",
            Self::PhysicalEnvironmental => "PE",
            Self::Planning => "PL",
            Self::ProgramManagement => "PM",
            Self::PersonnelSecurity => "PS",
            Self::PIIProcessing => "PT",
            Self::RiskAssessment => "RA",
            Self::SystemAcquisition => "SA",
            Self::SystemCommunications => "SC",
            Self::SystemIntegrity => "SI",
            Self::SupplyChain => "SR",
        }
    }
}

/// A specific security control with assessment status.
#[derive(Debug, Clone)]
pub struct SecurityControl {
    /// Control identifier (e.g., "AC-2")
    pub id: String,
    /// Control family
    pub family: ControlFamily,
    /// Control title
    pub title: String,
    /// Control description
    pub description: String,
    /// Implementation status
    pub status: ControlStatus,
    /// FedRAMP baseline (Low, Moderate, High)
    pub baseline: FedRAMPBaseline,
    /// Assessment evidence
    pub evidence: Vec<ControlEvidence>,
    /// Last assessment timestamp
    pub last_assessed: SystemTime,
    /// Responsible party
    pub responsible: String,
}

/// Control implementation status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlStatus {
    /// Fully implemented
    Implemented,
    /// Partially implemented
    PartiallyImplemented,
    /// Planned for implementation
    Planned,
    /// Alternative implementation
    Alternative,
    /// Not applicable
    NotApplicable,
    /// Not implemented (finding)
    NotImplemented,
}

/// FedRAMP baseline levels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FedRAMPBaseline {
    /// Low impact baseline (~156 controls)
    Low,
    /// Moderate impact baseline (~325 controls)
    Moderate,
    /// High impact baseline (~421 controls)
    High,
}

/// Evidence supporting control implementation.
#[derive(Debug, Clone)]
pub struct ControlEvidence {
    /// Evidence type
    pub evidence_type: EvidenceType,
    /// Evidence description
    pub description: String,
    /// Artifact reference (document ID, screenshot, etc.)
    pub artifact_ref: String,
    /// Collection timestamp
    pub collected_at: SystemTime,
    /// Automated vs manual
    pub automated: bool,
}

/// Types of control evidence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EvidenceType {
    /// Configuration screenshot
    Screenshot,
    /// System log excerpt
    LogExcerpt,
    /// Policy document
    PolicyDocument,
    /// Automated scan result
    ScanResult,
    /// Interview notes
    InterviewNotes,
    /// Test result
    TestResult,
    /// Code review
    CodeReview,
}

// =============================================================================
// Attestation Claims
// =============================================================================

/// A cryptographically signed attestation claim.
#[derive(Debug, Clone)]
pub struct AttestationClaim {
    /// Unique claim identifier
    pub claim_id: String,
    /// Claim type
    pub claim_type: ClaimType,
    /// Subject of the claim (cluster, workload, etc.)
    pub subject: AttestationSubject,
    /// Claim assertions
    pub assertions: Vec<Assertion>,
    /// Validity period
    pub valid_from: SystemTime,
    pub valid_until: SystemTime,
    /// Issuer information
    pub issuer: Issuer,
    /// Cryptographic signature
    pub signature: Signature,
}

/// Types of attestation claims.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClaimType {
    /// Security posture attestation
    SecurityPosture,
    /// Compliance status attestation
    ComplianceStatus,
    /// Authorization to operate
    AuthorizationToOperate,
    /// Continuous monitoring
    ContinuousMonitoring,
    /// Supply chain attestation
    SupplyChain,
    /// Federation eligibility
    FederationEligibility,
}

/// Subject of an attestation.
#[derive(Debug, Clone)]
pub struct AttestationSubject {
    /// Subject type
    pub subject_type: SubjectType,
    /// Subject identifier
    pub id: String,
    /// Subject metadata
    pub metadata: HashMap<String, String>,
}

/// Types of attestation subjects.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SubjectType {
    /// Entire cluster
    Cluster,
    /// Individual node
    Node,
    /// Workload/application
    Workload,
    /// Data classification
    Data,
    /// Network segment
    Network,
}

/// Individual assertion within a claim.
#[derive(Debug, Clone)]
pub struct Assertion {
    /// Assertion type
    pub assertion_type: String,
    /// Assertion value
    pub value: AssertionValue,
    /// Confidence level (0.0 - 1.0)
    pub confidence: f64,
    /// Supporting evidence references
    pub evidence_refs: Vec<String>,
}

/// Assertion value types.
#[derive(Debug, Clone)]
pub enum AssertionValue {
    Boolean(bool),
    String(String),
    Integer(i64),
    Float(f64),
    Enum(String),
    List(Vec<String>),
}

/// Attestation issuer information.
#[derive(Debug, Clone)]
pub struct Issuer {
    /// Issuer identifier
    pub id: String,
    /// Issuer name
    pub name: String,
    /// Issuer type
    pub issuer_type: IssuerType,
    /// PKI certificate reference
    pub certificate_ref: String,
}

/// Types of issuers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IssuerType {
    /// System automated attestation
    System,
    /// Third party assessor (3PAO)
    ThirdPartyAssessor,
    /// Authorizing official
    AuthorizingOfficial,
    /// Continuous monitoring system
    ContinuousMonitoring,
}

/// Cryptographic signature on attestation.
#[derive(Debug, Clone)]
pub struct Signature {
    /// Algorithm used
    pub algorithm: SignatureAlgorithm,
    /// Signature bytes (base64 encoded)
    pub value: String,
    /// Key identifier
    pub key_id: String,
    /// Signature timestamp
    pub timestamp: SystemTime,
}

/// Supported signature algorithms.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignatureAlgorithm {
    /// ECDSA with P-384 curve
    EcdsaP384,
    /// RSA with SHA-384
    RsaSha384,
    /// Ed25519
    Ed25519,
}

// =============================================================================
// Plan of Action and Milestones (POA&M)
// =============================================================================

/// FedRAMP Plan of Action and Milestones entry.
#[derive(Debug, Clone)]
pub struct POAMEntry {
    /// Unique POA&M ID
    pub id: String,
    /// Associated control
    pub control_id: String,
    /// Weakness description
    pub weakness: String,
    /// Risk level
    pub risk_level: RiskLevel,
    /// Remediation plan
    pub remediation_plan: String,
    /// Scheduled completion date
    pub scheduled_completion: SystemTime,
    /// Milestones
    pub milestones: Vec<Milestone>,
    /// Current status
    pub status: POAMStatus,
    /// Responsible party
    pub responsible: String,
    /// Deviation justification (if overdue)
    pub deviation: Option<String>,
}

/// Risk levels for findings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RiskLevel {
    Low,
    Moderate,
    High,
    Critical,
}

/// Milestone within a POA&M.
#[derive(Debug, Clone)]
pub struct Milestone {
    /// Milestone ID
    pub id: String,
    /// Description
    pub description: String,
    /// Target date
    pub target_date: SystemTime,
    /// Completion status
    pub completed: bool,
    /// Actual completion date (if completed)
    pub completed_date: Option<SystemTime>,
}

/// POA&M status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum POAMStatus {
    /// Actively being remediated
    Open,
    /// Remediation complete, awaiting verification
    PendingVerification,
    /// Verified closed
    Closed,
    /// Overdue
    Overdue,
    /// Risk accepted
    RiskAccepted,
}

// =============================================================================
// Continuous Monitoring
// =============================================================================

/// Configuration for continuous monitoring.
#[derive(Debug, Clone)]
pub struct ContinuousMonitoringConfig {
    /// Automated scan frequencies
    pub scan_frequencies: ScanFrequencies,
    /// Control assessment schedule
    pub assessment_schedule: AssessmentSchedule,
    /// Alert thresholds
    pub alert_thresholds: AlertThresholds,
    /// Reporting requirements
    pub reporting: ReportingConfig,
}

/// Scan frequency configuration.
#[derive(Debug, Clone)]
pub struct ScanFrequencies {
    /// Vulnerability scanning
    pub vulnerability: Duration,
    /// Configuration compliance
    pub configuration: Duration,
    /// Access review
    pub access_review: Duration,
    /// Network scanning
    pub network: Duration,
    /// Log analysis
    pub log_analysis: Duration,
}

/// Assessment schedule per control family.
#[derive(Debug, Clone)]
pub struct AssessmentSchedule {
    /// Annual controls
    pub annual: Vec<String>,
    /// Quarterly controls
    pub quarterly: Vec<String>,
    /// Monthly controls
    pub monthly: Vec<String>,
    /// Continuous controls (automated)
    pub continuous: Vec<String>,
}

/// Alert thresholds for security events.
#[derive(Debug, Clone)]
pub struct AlertThresholds {
    /// Critical vulnerabilities before alert
    pub critical_vulns: u32,
    /// High vulnerabilities before alert
    pub high_vulns: u32,
    /// Failed logins before alert
    pub failed_logins: u32,
    /// Configuration drift percentage
    pub config_drift_percent: f64,
    /// Unauthorized access attempts
    pub unauthorized_access: u32,
}

/// Reporting configuration.
#[derive(Debug, Clone)]
pub struct ReportingConfig {
    /// Monthly ConMon report
    pub monthly_conmon: bool,
    /// Quarterly SAR update
    pub quarterly_sar: bool,
    /// Annual POA&M review
    pub annual_poam_review: bool,
    /// Real-time SIEM integration
    pub siem_integration: bool,
    /// US-CERT incident reporting
    pub us_cert_reporting: bool,
}

// =============================================================================
// Attestation Authority
// =============================================================================

/// The attestation authority manages security attestations.
pub struct AttestationAuthority {
    /// Cluster identity
    cluster_id: String,
    /// Security classification limit
    classification_limit: ClassificationLevel,
    /// Current security posture
    posture: Arc<RwLock<SecurityPosture>>,
    /// Active controls
    controls: Arc<RwLock<HashMap<String, SecurityControl>>>,
    /// Active POA&M entries
    poam: Arc<RwLock<Vec<POAMEntry>>>,
    /// Continuous monitoring config
    conmon_config: ContinuousMonitoringConfig,
    /// Private key for signing attestations
    signing_key: SigningKey,
}

/// Signing key abstraction.
pub struct SigningKey {
    /// Key identifier
    pub key_id: String,
    /// Algorithm
    pub algorithm: SignatureAlgorithm,
    // In practice, this would hold the actual key material
    // Private key bytes omitted for security
}

/// Overall security posture assessment.
#[derive(Debug, Clone)]
pub struct SecurityPosture {
    /// Overall score (0-100)
    pub score: u8,
    /// Assessment timestamp
    pub assessed_at: SystemTime,
    /// Control implementation percentage
    pub control_coverage: f64,
    /// Open findings by severity
    pub open_findings: HashMap<RiskLevel, u32>,
    /// Recent scan results
    pub scan_results: Vec<ScanResult>,
    /// Authorization status
    pub authorization: AuthorizationStatus,
}

/// Scan result from automated tooling.
#[derive(Debug, Clone)]
pub struct ScanResult {
    /// Scan type
    pub scan_type: ScanType,
    /// Scan timestamp
    pub timestamp: SystemTime,
    /// Findings count
    pub findings: u32,
    /// Critical findings
    pub critical: u32,
    /// High findings
    pub high: u32,
    /// Tool used
    pub tool: String,
}

/// Types of security scans.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScanType {
    Vulnerability,
    Configuration,
    SAST,
    DAST,
    SCA,
    Container,
    Infrastructure,
}

/// Authorization to Operate status.
#[derive(Debug, Clone)]
pub struct AuthorizationStatus {
    /// Current status
    pub status: ATOStatus,
    /// Authorization date
    pub authorized_date: Option<SystemTime>,
    /// Expiration date
    pub expiration_date: Option<SystemTime>,
    /// Authorizing official
    pub authorizing_official: Option<String>,
    /// Authorization boundary
    pub boundary: String,
}

/// ATO status values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ATOStatus {
    /// Authorized
    Authorized,
    /// Provisional authorization
    Provisional,
    /// Authorization pending
    Pending,
    /// Authorization denied
    Denied,
    /// Authorization revoked
    Revoked,
    /// Authorization expired
    Expired,
}

impl AttestationAuthority {
    /// Create a new attestation authority.
    pub fn new(
        cluster_id: String,
        classification_limit: ClassificationLevel,
        signing_key: SigningKey,
        conmon_config: ContinuousMonitoringConfig,
    ) -> Self {
        Self {
            cluster_id,
            classification_limit,
            posture: Arc::new(RwLock::new(SecurityPosture {
                score: 0,
                assessed_at: SystemTime::now(),
                control_coverage: 0.0,
                open_findings: HashMap::new(),
                scan_results: Vec::new(),
                authorization: AuthorizationStatus {
                    status: ATOStatus::Pending,
                    authorized_date: None,
                    expiration_date: None,
                    authorizing_official: None,
                    boundary: String::new(),
                },
            })),
            controls: Arc::new(RwLock::new(HashMap::new())),
            poam: Arc::new(RwLock::new(Vec::new())),
            conmon_config,
            signing_key,
        }
    }
    
    /// Generate a security posture attestation.
    pub async fn generate_posture_attestation(&self) -> Result<AttestationClaim, AttestationError> {
        let posture = self.posture.read().await;
        let controls = self.controls.read().await;
        
        // Build assertions
        let mut assertions = Vec::new();
        
        // Overall score
        assertions.push(Assertion {
            assertion_type: "security_score".to_string(),
            value: AssertionValue::Integer(posture.score as i64),
            confidence: 0.95,
            evidence_refs: vec!["automated_assessment".to_string()],
        });
        
        // Control coverage
        assertions.push(Assertion {
            assertion_type: "control_coverage".to_string(),
            value: AssertionValue::Float(posture.control_coverage),
            confidence: 0.99,
            evidence_refs: vec!["control_inventory".to_string()],
        });
        
        // Authorization status
        assertions.push(Assertion {
            assertion_type: "authorization_status".to_string(),
            value: AssertionValue::Enum(format!("{:?}", posture.authorization.status)),
            confidence: 1.0,
            evidence_refs: vec!["ato_letter".to_string()],
        });
        
        // Classification limit
        assertions.push(Assertion {
            assertion_type: "classification_limit".to_string(),
            value: AssertionValue::String(self.classification_limit.marking().to_string()),
            confidence: 1.0,
            evidence_refs: vec!["system_categorization".to_string()],
        });
        
        // Implemented controls count
        let implemented = controls.values()
            .filter(|c| c.status == ControlStatus::Implemented)
            .count();
        assertions.push(Assertion {
            assertion_type: "implemented_controls".to_string(),
            value: AssertionValue::Integer(implemented as i64),
            confidence: 0.95,
            evidence_refs: vec!["control_assessment".to_string()],
        });
        
        // Create claim
        let now = SystemTime::now();
        let validity = Duration::from_secs(3600); // 1 hour validity
        
        let claim = AttestationClaim {
            claim_id: format!("claim-{}-{}", self.cluster_id, now.duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs()),
            claim_type: ClaimType::SecurityPosture,
            subject: AttestationSubject {
                subject_type: SubjectType::Cluster,
                id: self.cluster_id.clone(),
                metadata: HashMap::new(),
            },
            assertions,
            valid_from: now,
            valid_until: now + validity,
            issuer: Issuer {
                id: format!("issuer-{}", self.cluster_id),
                name: "Grey Attestation Authority".to_string(),
                issuer_type: IssuerType::System,
                certificate_ref: format!("cert:{}", self.signing_key.key_id),
            },
            signature: self.sign_claim()?,
        };
        
        Ok(claim)
    }
    
    /// Generate a compliance attestation.
    pub async fn generate_compliance_attestation(
        &self,
        framework: ComplianceFramework,
    ) -> Result<AttestationClaim, AttestationError> {
        let controls = self.controls.read().await;
        let poam = self.poam.read().await;
        
        let mut assertions = Vec::new();
        
        // Framework compliance
        assertions.push(Assertion {
            assertion_type: "compliance_framework".to_string(),
            value: AssertionValue::Enum(format!("{:?}", framework)),
            confidence: 1.0,
            evidence_refs: vec!["ssp".to_string()],
        });
        
        // Calculate compliance percentage
        let total_controls = controls.len();
        let compliant = controls.values()
            .filter(|c| matches!(c.status, ControlStatus::Implemented | ControlStatus::Alternative | ControlStatus::NotApplicable))
            .count();
        let compliance_pct = if total_controls > 0 {
            (compliant as f64 / total_controls as f64) * 100.0
        } else {
            0.0
        };
        
        assertions.push(Assertion {
            assertion_type: "compliance_percentage".to_string(),
            value: AssertionValue::Float(compliance_pct),
            confidence: 0.95,
            evidence_refs: vec!["control_assessment".to_string()],
        });
        
        // Open POA&M count
        let open_poam = poam.iter()
            .filter(|p| matches!(p.status, POAMStatus::Open | POAMStatus::Overdue))
            .count();
        assertions.push(Assertion {
            assertion_type: "open_poam_items".to_string(),
            value: AssertionValue::Integer(open_poam as i64),
            confidence: 1.0,
            evidence_refs: vec!["poam".to_string()],
        });
        
        // Critical/high findings
        let critical_findings: u32 = poam.iter()
            .filter(|p| p.risk_level == RiskLevel::Critical && p.status != POAMStatus::Closed)
            .count() as u32;
        assertions.push(Assertion {
            assertion_type: "critical_findings".to_string(),
            value: AssertionValue::Integer(critical_findings as i64),
            confidence: 1.0,
            evidence_refs: vec!["poam".to_string()],
        });
        
        let now = SystemTime::now();
        let validity = Duration::from_secs(86400); // 24 hour validity
        
        let claim = AttestationClaim {
            claim_id: format!("compliance-{}-{}", self.cluster_id, now.duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs()),
            claim_type: ClaimType::ComplianceStatus,
            subject: AttestationSubject {
                subject_type: SubjectType::Cluster,
                id: self.cluster_id.clone(),
                metadata: {
                    let mut m = HashMap::new();
                    m.insert("framework".to_string(), format!("{:?}", framework));
                    m
                },
            },
            assertions,
            valid_from: now,
            valid_until: now + validity,
            issuer: Issuer {
                id: format!("issuer-{}", self.cluster_id),
                name: "Grey Attestation Authority".to_string(),
                issuer_type: IssuerType::ContinuousMonitoring,
                certificate_ref: format!("cert:{}", self.signing_key.key_id),
            },
            signature: self.sign_claim()?,
        };
        
        Ok(claim)
    }
    
    /// Generate federation eligibility attestation.
    pub async fn generate_federation_attestation(&self) -> Result<AttestationClaim, AttestationError> {
        let posture = self.posture.read().await;
        
        // Check federation eligibility
        let is_eligible = posture.authorization.status == ATOStatus::Authorized
            && posture.score >= 70
            && self.classification_limit.federation_allowed();
        
        let mut assertions = Vec::new();
        
        assertions.push(Assertion {
            assertion_type: "federation_eligible".to_string(),
            value: AssertionValue::Boolean(is_eligible),
            confidence: if is_eligible { 1.0 } else { 0.0 },
            evidence_refs: vec!["ato_letter".to_string(), "posture_assessment".to_string()],
        });
        
        assertions.push(Assertion {
            assertion_type: "max_classification".to_string(),
            value: AssertionValue::String(self.classification_limit.marking().to_string()),
            confidence: 1.0,
            evidence_refs: vec!["system_categorization".to_string()],
        });
        
        assertions.push(Assertion {
            assertion_type: "ato_status".to_string(),
            value: AssertionValue::Enum(format!("{:?}", posture.authorization.status)),
            confidence: 1.0,
            evidence_refs: vec!["ato_letter".to_string()],
        });
        
        let now = SystemTime::now();
        let validity = Duration::from_secs(3600); // 1 hour validity for federation
        
        let claim = AttestationClaim {
            claim_id: format!("federation-{}-{}", self.cluster_id, now.duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs()),
            claim_type: ClaimType::FederationEligibility,
            subject: AttestationSubject {
                subject_type: SubjectType::Cluster,
                id: self.cluster_id.clone(),
                metadata: HashMap::new(),
            },
            assertions,
            valid_from: now,
            valid_until: now + validity,
            issuer: Issuer {
                id: format!("issuer-{}", self.cluster_id),
                name: "Grey Attestation Authority".to_string(),
                issuer_type: IssuerType::System,
                certificate_ref: format!("cert:{}", self.signing_key.key_id),
            },
            signature: self.sign_claim()?,
        };
        
        Ok(claim)
    }
    
    /// Verify an incoming attestation from a partner.
    pub fn verify_attestation(&self, claim: &AttestationClaim) -> Result<bool, AttestationError> {
        // Check validity period
        let now = SystemTime::now();
        if now < claim.valid_from || now > claim.valid_until {
            return Err(AttestationError::Expired);
        }
        
        // Verify signature (implementation would use cryptographic verification)
        // In production, this would verify against the issuer's public key
        if claim.signature.value.is_empty() {
            return Err(AttestationError::InvalidSignature);
        }
        
        // Check required assertions
        let has_ato = claim.assertions.iter()
            .any(|a| a.assertion_type == "ato_status");
        if !has_ato {
            return Err(AttestationError::MissingAssertion("ato_status".to_string()));
        }
        
        Ok(true)
    }
    
    /// Register a security control.
    pub async fn register_control(&self, control: SecurityControl) {
        let mut controls = self.controls.write().await;
        controls.insert(control.id.clone(), control);
    }
    
    /// Add a POA&M entry.
    pub async fn add_poam_entry(&self, entry: POAMEntry) {
        let mut poam = self.poam.write().await;
        poam.push(entry);
    }
    
    /// Update security posture.
    pub async fn update_posture(&self, posture: SecurityPosture) {
        let mut p = self.posture.write().await;
        *p = posture;
    }
    
    /// Sign a claim (placeholder implementation).
    fn sign_claim(&self) -> Result<Signature, AttestationError> {
        // In production, this would perform actual cryptographic signing
        Ok(Signature {
            algorithm: self.signing_key.algorithm,
            value: "base64_signature_placeholder".to_string(),
            key_id: self.signing_key.key_id.clone(),
            timestamp: SystemTime::now(),
        })
    }
}

/// Compliance frameworks.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComplianceFramework {
    FISMA,
    FedRAMPLow,
    FedRAMPModerate,
    FedRAMPHigh,
    NIST80053,
    NIST800171,
    CJIS,
    ITAR,
}

/// Attestation errors.
#[derive(Debug)]
pub enum AttestationError {
    Expired,
    InvalidSignature,
    MissingAssertion(String),
    SigningError(String),
}

impl std::fmt::Display for AttestationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expired => write!(f, "Attestation has expired"),
            Self::InvalidSignature => write!(f, "Invalid cryptographic signature"),
            Self::MissingAssertion(a) => write!(f, "Missing required assertion: {}", a),
            Self::SigningError(e) => write!(f, "Signing error: {}", e),
        }
    }
}

impl std::error::Error for AttestationError {}

// =============================================================================
// Example Usage
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_posture_attestation() {
        let signing_key = SigningKey {
            key_id: "key-001".to_string(),
            algorithm: SignatureAlgorithm::EcdsaP384,
        };
        
        let conmon_config = ContinuousMonitoringConfig {
            scan_frequencies: ScanFrequencies {
                vulnerability: Duration::from_secs(86400),
                configuration: Duration::from_secs(3600),
                access_review: Duration::from_secs(86400 * 7),
                network: Duration::from_secs(86400),
                log_analysis: Duration::from_secs(300),
            },
            assessment_schedule: AssessmentSchedule {
                annual: vec!["PE-1".to_string(), "PL-1".to_string()],
                quarterly: vec!["AC-2".to_string(), "AU-6".to_string()],
                monthly: vec!["RA-5".to_string(), "SI-2".to_string()],
                continuous: vec!["AU-2".to_string(), "SC-7".to_string()],
            },
            alert_thresholds: AlertThresholds {
                critical_vulns: 0,
                high_vulns: 5,
                failed_logins: 10,
                config_drift_percent: 5.0,
                unauthorized_access: 1,
            },
            reporting: ReportingConfig {
                monthly_conmon: true,
                quarterly_sar: true,
                annual_poam_review: true,
                siem_integration: true,
                us_cert_reporting: true,
            },
        };
        
        let authority = AttestationAuthority::new(
            "gov-cluster-001".to_string(),
            ClassificationLevel::CUI,
            signing_key,
            conmon_config,
        );
        
        // Register a control
        authority.register_control(SecurityControl {
            id: "AC-2".to_string(),
            family: ControlFamily::AccessControl,
            title: "Account Management".to_string(),
            description: "Manage system accounts".to_string(),
            status: ControlStatus::Implemented,
            baseline: FedRAMPBaseline::Moderate,
            evidence: vec![],
            last_assessed: SystemTime::now(),
            responsible: "ISSO".to_string(),
        }).await;
        
        // Update posture
        let mut findings = HashMap::new();
        findings.insert(RiskLevel::Low, 5);
        findings.insert(RiskLevel::Moderate, 2);
        
        authority.update_posture(SecurityPosture {
            score: 85,
            assessed_at: SystemTime::now(),
            control_coverage: 0.92,
            open_findings: findings,
            scan_results: vec![],
            authorization: AuthorizationStatus {
                status: ATOStatus::Authorized,
                authorized_date: Some(SystemTime::now()),
                expiration_date: Some(SystemTime::now() + Duration::from_secs(86400 * 365)),
                authorizing_official: Some("AO Name".to_string()),
                boundary: "Grey Government Cluster".to_string(),
            },
        }).await;
        
        // Generate attestation
        let claim = authority.generate_posture_attestation().await.unwrap();
        
        assert_eq!(claim.claim_type, ClaimType::SecurityPosture);
        assert!(!claim.assertions.is_empty());
    }
}
