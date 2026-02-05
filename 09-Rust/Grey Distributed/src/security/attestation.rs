//! # Remote Attestation
//!
//! Verify remote node integrity and trustworthiness.
//!
//! ## Philosophy
//!
//! In distributed systems, you can't trust other nodes implicitly.
//! Remote attestation lets you verify:
//! - Software version/build
//! - Configuration state
//! - Hardware integrity (with TEE support)
//!
//! ## Attestation Types
//!
//! 1. **Software Attestation**: Verify code hash
//! 2. **Configuration Attestation**: Verify config state
//! 3. **Hardware Attestation**: TEE-backed verification (SGX/TDX)
//!
//! ## Challenge-Response
//!
//! 1. Verifier sends random challenge
//! 2. Prover generates attestation including challenge
//! 3. Verifier checks attestation and challenge match

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant, SystemTime};

use parking_lot::{Mutex, RwLock};

use super::identity::CertSubjectId;

// ============================================================================
// Attestation Types
// ============================================================================

/// Attestation result
#[derive(Debug, Clone)]
pub struct Attestation {
    /// Node being attested
    pub node_id: CertSubjectId,
    
    /// Attestation type
    pub attestation_type: AttestationType,
    
    /// When attestation was created
    pub timestamp: SystemTime,
    
    /// Challenge that was answered
    pub challenge: Vec<u8>,
    
    /// Attestation evidence
    pub evidence: AttestationEvidence,
    
    /// Signature over attestation
    pub signature: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttestationType {
    /// Software-only attestation
    Software,
    /// Configuration attestation
    Configuration,
    /// Hardware-backed (TPM)
    Tpm,
    /// Intel SGX attestation
    Sgx,
    /// Intel TDX attestation  
    Tdx,
    /// AMD SEV attestation
    Sev,
}

#[derive(Debug, Clone)]
pub enum AttestationEvidence {
    /// Software measurements
    Software(SoftwareEvidence),
    /// Hardware quote
    Hardware(HardwareEvidence),
    /// Combined evidence
    Combined {
        software: SoftwareEvidence,
        hardware: HardwareEvidence,
    },
}

#[derive(Debug, Clone)]
pub struct SoftwareEvidence {
    /// Binary hash
    pub binary_hash: Vec<u8>,
    
    /// Configuration hash
    pub config_hash: Vec<u8>,
    
    /// Version string
    pub version: String,
    
    /// Build timestamp
    pub build_time: String,
    
    /// Additional measurements
    pub measurements: HashMap<String, Vec<u8>>,
}

#[derive(Debug, Clone)]
pub struct HardwareEvidence {
    /// Platform type
    pub platform: String,
    
    /// Quote/report
    pub quote: Vec<u8>,
    
    /// PCR values (for TPM)
    pub pcr_values: HashMap<u32, Vec<u8>>,
    
    /// Enclave measurements (for SGX/TDX)
    pub enclave_measurements: HashMap<String, Vec<u8>>,
}

// ============================================================================
// Attestation Challenge
// ============================================================================

/// Challenge for attestation protocol
#[derive(Debug, Clone)]
pub struct AttestationChallenge {
    /// Challenge ID
    pub id: u64,
    
    /// Random nonce
    pub nonce: Vec<u8>,
    
    /// Requested attestation type
    pub requested_type: AttestationType,
    
    /// Creation time
    pub created_at: Instant,
    
    /// Expiry duration
    pub expires_in: Duration,
}

impl AttestationChallenge {
    /// Create new challenge with random nonce
    pub fn new(requested_type: AttestationType, expires_in: Duration) -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        
        // Generate random nonce
        let mut nonce = vec![0u8; 32];
        for byte in &mut nonce {
            *byte = rand::random();
        }
        
        Self {
            id: COUNTER.fetch_add(1, Ordering::SeqCst),
            nonce,
            requested_type,
            created_at: Instant::now(),
            expires_in,
        }
    }
    
    /// Check if challenge has expired
    pub fn is_expired(&self) -> bool {
        self.created_at.elapsed() > self.expires_in
    }
}

// ============================================================================
// Attestation Verifier
// ============================================================================

/// Verifies attestation evidence
pub struct AttestationVerifier {
    /// Expected measurements
    expected: RwLock<ExpectedMeasurements>,
    
    /// Pending challenges
    pending_challenges: Mutex<HashMap<u64, AttestationChallenge>>,
    
    /// Verified attestations cache
    verified: RwLock<HashMap<CertSubjectId, VerifiedAttestation>>,
    
    /// Challenge timeout
    challenge_timeout: Duration,
    
    /// Attestation validity period
    attestation_validity: Duration,
}

#[derive(Debug, Clone)]
pub struct ExpectedMeasurements {
    /// Expected binary hashes (any match is valid)
    pub binary_hashes: Vec<Vec<u8>>,
    
    /// Expected version patterns
    pub allowed_versions: Vec<String>,
    
    /// Expected PCR values (for TPM)
    pub expected_pcrs: HashMap<u32, Vec<u8>>,
    
    /// Enclave signers (for SGX)
    pub trusted_signers: Vec<Vec<u8>>,
}

impl Default for ExpectedMeasurements {
    fn default() -> Self {
        Self {
            binary_hashes: Vec::new(),
            allowed_versions: Vec::new(),
            expected_pcrs: HashMap::new(),
            trusted_signers: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VerifiedAttestation {
    /// Attestation
    pub attestation: Attestation,
    
    /// Verification time
    pub verified_at: Instant,
    
    /// Verification result
    pub result: VerificationResult,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerificationResult {
    /// Full verification passed
    Verified,
    /// Partial verification (some checks skipped)
    PartiallyVerified,
    /// Verification failed
    Failed,
}

impl AttestationVerifier {
    pub fn new(
        challenge_timeout: Duration,
        attestation_validity: Duration,
    ) -> Self {
        Self {
            expected: RwLock::new(ExpectedMeasurements::default()),
            pending_challenges: Mutex::new(HashMap::new()),
            verified: RwLock::new(HashMap::new()),
            challenge_timeout,
            attestation_validity,
        }
    }
    
    /// Set expected measurements
    pub fn set_expected(&self, expected: ExpectedMeasurements) {
        *self.expected.write() = expected;
    }
    
    /// Create a new challenge for a node
    ///
    /// # Timeout Importance
    /// 
    /// Challenges must timeout to prevent replay attacks
    /// and to avoid unbounded memory growth.
    pub fn create_challenge(
        &self,
        attestation_type: AttestationType,
    ) -> AttestationChallenge {
        let challenge = AttestationChallenge::new(
            attestation_type,
            self.challenge_timeout,
        );
        
        self.pending_challenges.lock().insert(challenge.id, challenge.clone());
        challenge
    }
    
    /// Verify attestation response
    pub fn verify(&self, attestation: &Attestation) -> Result<VerificationResult, AttestError> {
        // Find the challenge
        let challenge = self.pending_challenges.lock().remove(&{
            // Extract challenge ID from nonce (simplified)
            let mut id_bytes = [0u8; 8];
            id_bytes.copy_from_slice(&attestation.challenge[..8]);
            u64::from_le_bytes(id_bytes)
        }).ok_or(AttestError::UnknownChallenge)?;
        
        // Check challenge expiry
        if challenge.is_expired() {
            return Err(AttestError::ChallengeExpired);
        }
        
        // Verify nonce matches
        if attestation.challenge != challenge.nonce {
            return Err(AttestError::NonceMismatch);
        }
        
        // Verify evidence based on type
        let result = match &attestation.evidence {
            AttestationEvidence::Software(sw) => {
                self.verify_software(sw)?
            }
            AttestationEvidence::Hardware(hw) => {
                self.verify_hardware(hw)?
            }
            AttestationEvidence::Combined { software, hardware } => {
                let sw_result = self.verify_software(software)?;
                let hw_result = self.verify_hardware(hardware)?;
                
                if sw_result == VerificationResult::Verified &&
                   hw_result == VerificationResult::Verified {
                    VerificationResult::Verified
                } else {
                    VerificationResult::PartiallyVerified
                }
            }
        };
        
        // Cache successful verification
        if result != VerificationResult::Failed {
            self.verified.write().insert(
                attestation.node_id.clone(),
                VerifiedAttestation {
                    attestation: attestation.clone(),
                    verified_at: Instant::now(),
                    result,
                }
            );
        }
        
        Ok(result)
    }
    
    /// Verify software evidence
    fn verify_software(&self, evidence: &SoftwareEvidence) -> Result<VerificationResult, AttestError> {
        let expected = self.expected.read();
        
        // Check binary hash
        if !expected.binary_hashes.is_empty() {
            let hash_matches = expected.binary_hashes.iter()
                .any(|h| h == &evidence.binary_hash);
            if !hash_matches {
                return Err(AttestError::BinaryMismatch);
            }
        }
        
        // Check version
        if !expected.allowed_versions.is_empty() {
            let version_matches = expected.allowed_versions.iter()
                .any(|v| v == &evidence.version);
            if !version_matches {
                return Err(AttestError::VersionMismatch);
            }
        }
        
        Ok(VerificationResult::Verified)
    }
    
    /// Verify hardware evidence
    fn verify_hardware(&self, evidence: &HardwareEvidence) -> Result<VerificationResult, AttestError> {
        let expected = self.expected.read();
        
        // Check PCR values
        for (pcr, expected_value) in &expected.expected_pcrs {
            if let Some(actual) = evidence.pcr_values.get(pcr) {
                if actual != expected_value {
                    return Err(AttestError::PcrMismatch(*pcr));
                }
            }
        }
        
        // Would verify quote signature here
        
        Ok(VerificationResult::Verified)
    }
    
    /// Check if node has valid attestation
    pub fn is_attested(&self, node_id: &CertSubjectId) -> bool {
        if let Some(verified) = self.verified.read().get(node_id) {
            if verified.verified_at.elapsed() < self.attestation_validity {
                return verified.result != VerificationResult::Failed;
            }
        }
        false
    }
    
    /// Get cached attestation
    pub fn get_attestation(&self, node_id: &CertSubjectId) -> Option<VerifiedAttestation> {
        self.verified.read().get(node_id).cloned()
    }
    
    /// Clean up expired challenges and attestations
    pub fn cleanup(&self) {
        // Clean expired challenges
        self.pending_challenges.lock().retain(|_, c| !c.is_expired());
        
        // Clean expired attestations  
        let validity = self.attestation_validity;
        self.verified.write().retain(|_, v| {
            v.verified_at.elapsed() < validity
        });
    }
}

#[derive(Debug)]
pub enum AttestError {
    UnknownChallenge,
    ChallengeExpired,
    NonceMismatch,
    BinaryMismatch,
    VersionMismatch,
    PcrMismatch(u32),
    QuoteVerificationFailed,
    InvalidSignature,
}

// ============================================================================
// Attestation Producer
// ============================================================================

/// Produces attestations for this node
pub struct AttestationProducer {
    /// Our node ID
    node_id: CertSubjectId,
    
    /// Current software evidence
    software_evidence: RwLock<SoftwareEvidence>,
    
    /// Hardware attestation support
    hardware_support: Option<Box<dyn HardwareAttester>>,
}

/// Interface for hardware attestation
pub trait HardwareAttester: Send + Sync {
    fn platform(&self) -> &str;
    fn generate_quote(&self, nonce: &[u8]) -> Result<Vec<u8>, String>;
    fn get_pcrs(&self) -> HashMap<u32, Vec<u8>>;
}

impl AttestationProducer {
    pub fn new(node_id: CertSubjectId) -> Self {
        Self {
            node_id,
            software_evidence: RwLock::new(SoftwareEvidence {
                binary_hash: Vec::new(),
                config_hash: Vec::new(),
                version: String::new(),
                build_time: String::new(),
                measurements: HashMap::new(),
            }),
            hardware_support: None,
        }
    }
    
    /// Set software evidence
    pub fn set_software_evidence(&self, evidence: SoftwareEvidence) {
        *self.software_evidence.write() = evidence;
    }
    
    /// Set hardware attester
    pub fn set_hardware_attester(&mut self, attester: Box<dyn HardwareAttester>) {
        self.hardware_support = Some(attester);
    }
    
    /// Generate attestation response
    pub fn generate(&self, challenge: &AttestationChallenge) -> Result<Attestation, String> {
        let evidence = match challenge.requested_type {
            AttestationType::Software | AttestationType::Configuration => {
                AttestationEvidence::Software(self.software_evidence.read().clone())
            }
            AttestationType::Tpm | AttestationType::Sgx | 
            AttestationType::Tdx | AttestationType::Sev => {
                if let Some(ref hw) = self.hardware_support {
                    let quote = hw.generate_quote(&challenge.nonce)?;
                    AttestationEvidence::Hardware(HardwareEvidence {
                        platform: hw.platform().to_string(),
                        quote,
                        pcr_values: hw.get_pcrs(),
                        enclave_measurements: HashMap::new(),
                    })
                } else {
                    return Err("Hardware attestation not supported".to_string());
                }
            }
        };
        
        Ok(Attestation {
            node_id: self.node_id.clone(),
            attestation_type: challenge.requested_type,
            timestamp: SystemTime::now(),
            challenge: challenge.nonce.clone(),
            evidence,
            signature: Vec::new(), // Would sign here
        })
    }
}

// ============================================================================
// Trust Score
// ============================================================================

/// Calculates trust score based on attestation history
pub struct TrustScorer {
    /// Trust scores by node
    scores: RwLock<HashMap<CertSubjectId, TrustScore>>,
    
    /// Scoring weights
    weights: TrustWeights,
}

#[derive(Debug, Clone)]
pub struct TrustScore {
    /// Overall score (0.0 to 1.0)
    pub score: f64,
    
    /// Successful attestations
    pub successful: u64,
    
    /// Failed attestations
    pub failed: u64,
    
    /// Last attestation time
    pub last_attestation: Option<Instant>,
}

#[derive(Debug, Clone)]
pub struct TrustWeights {
    /// Weight for attestation success
    pub attestation_success: f64,
    /// Weight for attestation recency
    pub recency: f64,
    /// Weight for hardware attestation
    pub hardware_bonus: f64,
}

impl Default for TrustWeights {
    fn default() -> Self {
        Self {
            attestation_success: 0.5,
            recency: 0.3,
            hardware_bonus: 0.2,
        }
    }
}

impl TrustScorer {
    pub fn new(weights: TrustWeights) -> Self {
        Self {
            scores: RwLock::new(HashMap::new()),
            weights,
        }
    }
    
    /// Record attestation result
    pub fn record_attestation(
        &self,
        node_id: &CertSubjectId,
        success: bool,
        hardware_backed: bool,
    ) {
        let mut scores = self.scores.write();
        let score = scores.entry(node_id.clone()).or_insert(TrustScore {
            score: 0.5, // Start neutral
            successful: 0,
            failed: 0,
            last_attestation: None,
        });
        
        if success {
            score.successful += 1;
        } else {
            score.failed += 1;
        }
        
        score.last_attestation = Some(Instant::now());
        
        // Recalculate score
        let total = score.successful + score.failed;
        let success_rate = if total > 0 {
            score.successful as f64 / total as f64
        } else {
            0.5
        };
        
        let mut new_score = success_rate * self.weights.attestation_success;
        
        // Add hardware bonus
        if hardware_backed && success {
            new_score += self.weights.hardware_bonus;
        }
        
        // Add recency bonus
        if score.last_attestation.map(|t| t.elapsed() < Duration::from_secs(300)).unwrap_or(false) {
            new_score += self.weights.recency;
        }
        
        score.score = new_score.min(1.0);
    }
    
    /// Get trust score
    pub fn get_score(&self, node_id: &CertSubjectId) -> Option<TrustScore> {
        self.scores.read().get(node_id).cloned()
    }
    
    /// Check if node meets trust threshold
    pub fn meets_threshold(&self, node_id: &CertSubjectId, threshold: f64) -> bool {
        self.scores.read()
            .get(node_id)
            .map(|s| s.score >= threshold)
            .unwrap_or(false)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_challenge_creation() {
        let challenge = AttestationChallenge::new(
            AttestationType::Software,
            Duration::from_secs(60),
        );
        
        assert_eq!(challenge.nonce.len(), 32);
        assert!(!challenge.is_expired());
    }
    
    #[test]
    fn test_verifier() {
        let verifier = AttestationVerifier::new(
            Duration::from_secs(60),
            Duration::from_secs(3600),
        );
        
        // Set expected
        let mut expected = ExpectedMeasurements::default();
        expected.binary_hashes.push(vec![1, 2, 3]);
        expected.allowed_versions.push("1.0.0".to_string());
        verifier.set_expected(expected);
        
        // Create challenge
        let _challenge = verifier.create_challenge(AttestationType::Software);
        
        // Would test full flow here
    }
    
    #[test]
    fn test_trust_scorer() {
        let scorer = TrustScorer::new(TrustWeights::default());
        let node_id = CertSubjectId::new("node-1");
        
        // Record some attestations
        scorer.record_attestation(&node_id, true, false);
        scorer.record_attestation(&node_id, true, false);
        scorer.record_attestation(&node_id, false, false);
        
        let score = scorer.get_score(&node_id).unwrap();
        assert_eq!(score.successful, 2);
        assert_eq!(score.failed, 1);
        assert!(score.score > 0.0);
    }
}
