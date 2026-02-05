//! # Grey Distributed — Security Attestation Protocol
//!
//! This module implements cross-cluster trust verification and security
//! attestation for federated Grey deployments.
//!
//! ## Overview
//!
//! Security attestation ensures that:
//! 1. **Identity**: Clusters are who they claim to be
//! 2. **Integrity**: Cluster software hasn't been tampered with
//! 3. **Compliance**: Clusters meet security policy requirements
//! 4. **Authorization**: Clusters are permitted to participate
//!
//! ## Trust Model
//!
//! The federation uses a hierarchical trust model:
//! - **Root of Trust**: Federation CA (Certificate Authority)
//! - **Cluster Certificates**: Issued by Federation CA
//! - **Attestation Tokens**: Short-lived, scoped credentials
//! - **Policy Verification**: Runtime compliance checks

use std::collections::{HashMap, HashSet};
use std::time::{Duration, Instant, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type ClusterId = String;
pub type TenantId = String;
pub type CertificateFingerprint = String;

/// Maximum attestation token lifetime
const MAX_ATTESTATION_LIFETIME: Duration = Duration::from_secs(3600); // 1 hour

/// Minimum key size for RSA keys (bits)
const MIN_RSA_KEY_SIZE: u32 = 2048;

/// Attestation refresh interval
const ATTESTATION_REFRESH_INTERVAL: Duration = Duration::from_secs(900); // 15 min

// =============================================================================
// Certificate Types
// =============================================================================

/// X.509-style certificate for cluster identity
#[derive(Debug, Clone)]
pub struct ClusterCertificate {
    pub subject: ClusterId,
    pub issuer: String,
    pub serial_number: String,
    pub not_before: SystemTime,
    pub not_after: SystemTime,
    pub public_key: PublicKey,
    pub fingerprint: CertificateFingerprint,
    pub extensions: CertificateExtensions,
    pub signature: Vec<u8>,
}

/// Public key types supported
#[derive(Debug, Clone)]
pub enum PublicKey {
    Rsa { modulus: Vec<u8>, exponent: Vec<u8>, bits: u32 },
    Ecdsa { curve: String, point: Vec<u8> },
    Ed25519 { key: [u8; 32] },
}

/// Certificate extensions for Grey-specific metadata
#[derive(Debug, Clone)]
pub struct CertificateExtensions {
    pub organization_id: String,
    pub region: String,
    pub capabilities: Vec<ClusterCapability>,
    pub security_level: SecurityLevel,
    pub compliance_certs: Vec<String>,
}

/// Cluster capabilities advertised in certificate
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ClusterCapability {
    Compute,
    Storage,
    Gpu,
    HighMemory,
    LowLatency,
    DataSovereignty(String), // Region/jurisdiction
}

/// Security level of the cluster
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SecurityLevel {
    Basic,
    Standard,
    Enhanced,
    Sovereign,
}

// =============================================================================
// Attestation Types
// =============================================================================

/// Attestation token for cross-cluster communication
#[derive(Debug, Clone)]
pub struct AttestationToken {
    pub token_id: String,
    pub subject: ClusterId,
    pub issuer: ClusterId,
    pub audience: Vec<ClusterId>,
    pub issued_at: SystemTime,
    pub expires_at: SystemTime,
    pub claims: AttestationClaims,
    pub signature: Vec<u8>,
}

/// Claims included in attestation
#[derive(Debug, Clone)]
pub struct AttestationClaims {
    /// Software version
    pub version: String,
    
    /// Configuration hash (for integrity)
    pub config_hash: String,
    
    /// Runtime measurements (e.g., from TPM)
    pub measurements: Vec<Measurement>,
    
    /// Security policies in effect
    pub active_policies: Vec<String>,
    
    /// Tenants this cluster serves
    pub tenant_attestations: Vec<TenantAttestation>,
    
    /// Network configuration
    pub network_config: NetworkConfig,
    
    /// Compliance status
    pub compliance: ComplianceStatus,
}

/// Hardware/software measurement for integrity
#[derive(Debug, Clone)]
pub struct Measurement {
    pub pcr_index: u8,
    pub algorithm: String,
    pub digest: Vec<u8>,
    pub description: String,
}

/// Per-tenant attestation within a cluster
#[derive(Debug, Clone)]
pub struct TenantAttestation {
    pub tenant_id: TenantId,
    pub isolation_level: IsolationLevel,
    pub encryption_type: EncryptionType,
    pub data_residency: Option<String>,
}

/// Tenant isolation levels
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IsolationLevel {
    Shared,           // Multi-tenant, shared resources
    Dedicated,        // Dedicated workers, shared control plane
    Isolated,         // Dedicated workers and control plane
    Sovereign,        // Fully isolated, auditable
}

/// Encryption types for data protection
#[derive(Debug, Clone)]
pub enum EncryptionType {
    None,
    AtRest,
    InTransit,
    EndToEnd,
    CustomerManaged { key_id: String },
}

/// Network configuration for security
#[derive(Debug, Clone)]
pub struct NetworkConfig {
    pub mtls_enabled: bool,
    pub allowed_cidrs: Vec<String>,
    pub egress_controlled: bool,
    pub dns_private: bool,
}

/// Compliance certification status
#[derive(Debug, Clone)]
pub struct ComplianceStatus {
    pub soc2_type2: bool,
    pub iso27001: bool,
    pub hipaa: bool,
    pub gdpr: bool,
    pub fedramp: Option<String>,
    pub custom_certs: Vec<String>,
}

// =============================================================================
// Attestation Verifier
// =============================================================================

/// Verifies attestations from other clusters
pub struct AttestationVerifier {
    /// This cluster's ID
    cluster_id: ClusterId,
    
    /// Trusted CA certificates
    trusted_cas: HashMap<CertificateFingerprint, ClusterCertificate>,
    
    /// Known cluster certificates
    cluster_certs: HashMap<ClusterId, ClusterCertificate>,
    
    /// Cached attestation tokens
    attestation_cache: HashMap<ClusterId, AttestationToken>,
    
    /// Required security policies
    required_policies: HashSet<String>,
    
    /// Minimum security level for communication
    min_security_level: SecurityLevel,
    
    /// Revoked certificates
    revoked_certs: HashSet<CertificateFingerprint>,
}

impl AttestationVerifier {
    pub fn new(cluster_id: ClusterId, min_security_level: SecurityLevel) -> Self {
        Self {
            cluster_id,
            trusted_cas: HashMap::new(),
            cluster_certs: HashMap::new(),
            attestation_cache: HashMap::new(),
            required_policies: HashSet::new(),
            min_security_level,
            revoked_certs: HashSet::new(),
        }
    }
    
    // =========================================================================
    // Step 1: Add Trust Anchors
    // =========================================================================
    
    /// Add a trusted CA certificate
    /// 
    /// This establishes the root of trust for the federation.
    /// Only certificates issued by trusted CAs will be accepted.
    pub fn add_trusted_ca(&mut self, cert: ClusterCertificate) -> Result<(), AttestationError> {
        // Step 1a: Validate CA certificate
        self.validate_ca_certificate(&cert)?;
        
        // Step 1b: Check for revocation
        if self.revoked_certs.contains(&cert.fingerprint) {
            return Err(AttestationError::CertificateRevoked {
                fingerprint: cert.fingerprint,
            });
        }
        
        // Step 1c: Store as trusted CA
        self.trusted_cas.insert(cert.fingerprint.clone(), cert);
        
        Ok(())
    }
    
    fn validate_ca_certificate(&self, cert: &ClusterCertificate) -> Result<(), AttestationError> {
        let now = SystemTime::now();
        
        // Check validity period
        if cert.not_before > now {
            return Err(AttestationError::CertificateNotYetValid {
                not_before: cert.not_before,
            });
        }
        
        if cert.not_after < now {
            return Err(AttestationError::CertificateExpired {
                not_after: cert.not_after,
            });
        }
        
        // Check key strength
        if let PublicKey::Rsa { bits, .. } = &cert.public_key {
            if *bits < MIN_RSA_KEY_SIZE {
                return Err(AttestationError::WeakKey {
                    algorithm: "RSA".to_string(),
                    size: *bits,
                    minimum: MIN_RSA_KEY_SIZE,
                });
            }
        }
        
        Ok(())
    }
    
    // =========================================================================
    // Step 2: Register Cluster Certificate
    // =========================================================================
    
    /// Register a cluster's certificate
    /// 
    /// The certificate must be signed by a trusted CA.
    pub fn register_cluster_cert(
        &mut self,
        cert: ClusterCertificate,
    ) -> Result<(), AttestationError> {
        // Step 2a: Verify certificate chain
        let issuer_cert = self.trusted_cas.get(&cert.issuer)
            .or_else(|| self.trusted_cas.values().find(|c| c.subject == cert.issuer))
            .ok_or_else(|| AttestationError::UntrustedIssuer {
                issuer: cert.issuer.clone(),
            })?;
        
        // Step 2b: Validate certificate
        self.validate_cluster_certificate(&cert)?;
        
        // Step 2c: Verify signature (placeholder - real impl would use crypto)
        self.verify_signature(&cert, issuer_cert)?;
        
        // Step 2d: Check revocation
        if self.revoked_certs.contains(&cert.fingerprint) {
            return Err(AttestationError::CertificateRevoked {
                fingerprint: cert.fingerprint,
            });
        }
        
        // Step 2e: Check security level
        if cert.extensions.security_level < self.min_security_level {
            return Err(AttestationError::InsufficientSecurityLevel {
                required: self.min_security_level.clone(),
                actual: cert.extensions.security_level.clone(),
            });
        }
        
        // Step 2f: Store certificate
        self.cluster_certs.insert(cert.subject.clone(), cert);
        
        Ok(())
    }
    
    fn validate_cluster_certificate(&self, cert: &ClusterCertificate) -> Result<(), AttestationError> {
        let now = SystemTime::now();
        
        if cert.not_before > now {
            return Err(AttestationError::CertificateNotYetValid {
                not_before: cert.not_before,
            });
        }
        
        if cert.not_after < now {
            return Err(AttestationError::CertificateExpired {
                not_after: cert.not_after,
            });
        }
        
        Ok(())
    }
    
    fn verify_signature(
        &self,
        _cert: &ClusterCertificate,
        _issuer: &ClusterCertificate,
    ) -> Result<(), AttestationError> {
        // Placeholder - real implementation would perform cryptographic verification
        // using the issuer's public key to verify the certificate's signature
        Ok(())
    }
    
    // =========================================================================
    // Step 3: Create Attestation Token
    // =========================================================================
    
    /// Create an attestation token for this cluster
    /// 
    /// This token can be sent to other clusters to prove identity and state.
    pub fn create_attestation(
        &self,
        audience: Vec<ClusterId>,
        claims: AttestationClaims,
        private_key: &[u8],
    ) -> Result<AttestationToken, AttestationError> {
        let now = SystemTime::now();
        let expires_at = now + MAX_ATTESTATION_LIFETIME;
        
        // Step 3a: Validate claims
        self.validate_claims(&claims)?;
        
        // Step 3b: Create token
        let token = AttestationToken {
            token_id: generate_token_id(),
            subject: self.cluster_id.clone(),
            issuer: self.cluster_id.clone(),
            audience,
            issued_at: now,
            expires_at,
            claims,
            signature: Vec::new(), // Will be set below
        };
        
        // Step 3c: Sign token (placeholder)
        let signed_token = self.sign_attestation(token, private_key)?;
        
        Ok(signed_token)
    }
    
    fn validate_claims(&self, claims: &AttestationClaims) -> Result<(), AttestationError> {
        // Check required policies are active
        for policy in &self.required_policies {
            if !claims.active_policies.contains(policy) {
                return Err(AttestationError::MissingPolicy {
                    policy: policy.clone(),
                });
            }
        }
        
        Ok(())
    }
    
    fn sign_attestation(
        &self,
        mut token: AttestationToken,
        _private_key: &[u8],
    ) -> Result<AttestationToken, AttestationError> {
        // Placeholder - real implementation would:
        // 1. Serialize token claims
        // 2. Compute signature using private key
        // 3. Attach signature to token
        token.signature = vec![0u8; 64]; // Placeholder signature
        Ok(token)
    }
    
    // =========================================================================
    // Step 4: Verify Attestation Token
    // =========================================================================
    
    /// Verify an attestation token from another cluster
    /// 
    /// This validates identity, integrity, and policy compliance.
    pub fn verify_attestation(
        &mut self,
        token: &AttestationToken,
    ) -> Result<VerificationResult, AttestationError> {
        let now = SystemTime::now();
        
        // Step 4a: Check token expiration
        if token.expires_at < now {
            return Err(AttestationError::TokenExpired {
                expired_at: token.expires_at,
            });
        }
        
        // Step 4b: Check we are in audience
        if !token.audience.contains(&self.cluster_id) && !token.audience.is_empty() {
            return Err(AttestationError::NotInAudience {
                expected: self.cluster_id.clone(),
                audience: token.audience.clone(),
            });
        }
        
        // Step 4c: Get cluster certificate
        let cert = self.cluster_certs.get(&token.subject)
            .ok_or_else(|| AttestationError::UnknownCluster {
                cluster_id: token.subject.clone(),
            })?;
        
        // Step 4d: Verify signature
        self.verify_token_signature(token, cert)?;
        
        // Step 4e: Verify claims
        let claim_result = self.verify_claims(&token.claims)?;
        
        // Step 4f: Check policy compliance
        let policy_violations = self.check_policy_compliance(&token.claims);
        
        // Step 4g: Cache valid attestation
        if policy_violations.is_empty() {
            self.attestation_cache.insert(token.subject.clone(), token.clone());
        }
        
        Ok(VerificationResult {
            cluster_id: token.subject.clone(),
            security_level: cert.extensions.security_level.clone(),
            capabilities: cert.extensions.capabilities.clone(),
            claim_verification: claim_result,
            policy_violations,
            verified_at: now,
        })
    }
    
    fn verify_token_signature(
        &self,
        _token: &AttestationToken,
        _cert: &ClusterCertificate,
    ) -> Result<(), AttestationError> {
        // Placeholder - real implementation would verify cryptographic signature
        Ok(())
    }
    
    fn verify_claims(&self, claims: &AttestationClaims) -> Result<ClaimVerification, AttestationError> {
        let mut warnings = Vec::new();
        
        // Check version
        // In production, would verify against known good versions
        
        // Check measurements
        let measurements_valid = self.verify_measurements(&claims.measurements);
        if !measurements_valid {
            warnings.push("Some measurements could not be verified".to_string());
        }
        
        // Check network config
        if !claims.network_config.mtls_enabled {
            warnings.push("mTLS not enabled".to_string());
        }
        
        Ok(ClaimVerification {
            version_verified: true,
            config_verified: true,
            measurements_verified: measurements_valid,
            warnings,
        })
    }
    
    fn verify_measurements(&self, measurements: &[Measurement]) -> bool {
        // Placeholder - real implementation would:
        // 1. Compare against known good values
        // 2. Verify TPM quotes if available
        // 3. Check for unauthorized modifications
        !measurements.is_empty()
    }
    
    fn check_policy_compliance(&self, claims: &AttestationClaims) -> Vec<PolicyViolation> {
        let mut violations = Vec::new();
        
        for policy in &self.required_policies {
            if !claims.active_policies.contains(policy) {
                violations.push(PolicyViolation {
                    policy: policy.clone(),
                    severity: ViolationSeverity::Error,
                    description: format!("Required policy '{}' not active", policy),
                });
            }
        }
        
        // Check compliance requirements
        if !claims.compliance.soc2_type2 && self.required_policies.contains("soc2") {
            violations.push(PolicyViolation {
                policy: "soc2".to_string(),
                severity: ViolationSeverity::Error,
                description: "SOC2 Type 2 certification required".to_string(),
            });
        }
        
        violations
    }
    
    // =========================================================================
    // Step 5: Tenant Attestation Verification
    // =========================================================================
    
    /// Verify that a cluster can serve a specific tenant
    /// 
    /// This checks isolation levels, encryption, and data residency.
    pub fn verify_tenant_attestation(
        &self,
        cluster_id: &ClusterId,
        tenant_id: &TenantId,
        required_isolation: IsolationLevel,
        required_residency: Option<&str>,
    ) -> Result<TenantAttestationResult, AttestationError> {
        // Step 5a: Get cached attestation
        let attestation = self.attestation_cache.get(cluster_id)
            .ok_or_else(|| AttestationError::NoAttestation {
                cluster_id: cluster_id.clone(),
            })?;
        
        // Step 5b: Find tenant attestation
        let tenant_att = attestation.claims.tenant_attestations
            .iter()
            .find(|t| &t.tenant_id == tenant_id)
            .ok_or_else(|| AttestationError::TenantNotAttested {
                tenant_id: tenant_id.clone(),
                cluster_id: cluster_id.clone(),
            })?;
        
        // Step 5c: Check isolation level
        let isolation_ok = self.check_isolation_level(
            &tenant_att.isolation_level,
            &required_isolation,
        );
        
        // Step 5d: Check data residency
        let residency_ok = match (required_residency, &tenant_att.data_residency) {
            (None, _) => true,
            (Some(req), Some(actual)) => req == actual,
            (Some(_), None) => false,
        };
        
        // Step 5e: Check encryption
        let encryption_ok = !matches!(tenant_att.encryption_type, EncryptionType::None);
        
        Ok(TenantAttestationResult {
            tenant_id: tenant_id.clone(),
            cluster_id: cluster_id.clone(),
            isolation_satisfied: isolation_ok,
            residency_satisfied: residency_ok,
            encryption_adequate: encryption_ok,
            isolation_level: tenant_att.isolation_level.clone(),
            encryption_type: tenant_att.encryption_type.clone(),
        })
    }
    
    fn check_isolation_level(
        &self,
        actual: &IsolationLevel,
        required: &IsolationLevel,
    ) -> bool {
        // Higher isolation levels satisfy lower requirements
        match (required, actual) {
            (IsolationLevel::Shared, _) => true,
            (IsolationLevel::Dedicated, IsolationLevel::Dedicated)
            | (IsolationLevel::Dedicated, IsolationLevel::Isolated)
            | (IsolationLevel::Dedicated, IsolationLevel::Sovereign) => true,
            (IsolationLevel::Isolated, IsolationLevel::Isolated)
            | (IsolationLevel::Isolated, IsolationLevel::Sovereign) => true,
            (IsolationLevel::Sovereign, IsolationLevel::Sovereign) => true,
            _ => false,
        }
    }
    
    // =========================================================================
    // Step 6: Revocation Management
    // =========================================================================
    
    /// Revoke a certificate
    /// 
    /// Called when a cluster is compromised or decommissioned.
    pub fn revoke_certificate(&mut self, fingerprint: &CertificateFingerprint) {
        self.revoked_certs.insert(fingerprint.clone());
        
        // Remove from trusted CAs if present
        self.trusted_cas.remove(fingerprint);
        
        // Remove from cluster certs
        self.cluster_certs.retain(|_, cert| &cert.fingerprint != fingerprint);
        
        // Invalidate any cached attestations from revoked cluster
        self.attestation_cache.retain(|cluster_id, _| {
            if let Some(cert) = self.cluster_certs.get(cluster_id) {
                &cert.fingerprint != fingerprint
            } else {
                false
            }
        });
    }
    
    /// Check if a certificate is revoked
    pub fn is_revoked(&self, fingerprint: &CertificateFingerprint) -> bool {
        self.revoked_certs.contains(fingerprint)
    }
    
    // =========================================================================
    // Step 7: Attestation Refresh
    // =========================================================================
    
    /// Check if attestation needs refresh
    pub fn needs_refresh(&self, cluster_id: &ClusterId) -> bool {
        match self.attestation_cache.get(cluster_id) {
            None => true,
            Some(att) => {
                let now = SystemTime::now();
                let age = now.duration_since(att.issued_at)
                    .unwrap_or(Duration::from_secs(u64::MAX));
                age > ATTESTATION_REFRESH_INTERVAL
            }
        }
    }
    
    /// Get all clusters needing attestation refresh
    pub fn clusters_needing_refresh(&self) -> Vec<ClusterId> {
        self.cluster_certs.keys()
            .filter(|id| self.needs_refresh(id))
            .cloned()
            .collect()
    }
    
    // =========================================================================
    // Step 8: Audit and Reporting
    // =========================================================================
    
    /// Get attestation status for all known clusters
    pub fn attestation_status(&self) -> Vec<ClusterAttestationStatus> {
        self.cluster_certs.keys()
            .map(|id| {
                let attestation = self.attestation_cache.get(id);
                let cert = self.cluster_certs.get(id);
                
                ClusterAttestationStatus {
                    cluster_id: id.clone(),
                    has_valid_cert: cert.is_some(),
                    has_valid_attestation: attestation.map(|a| a.expires_at > SystemTime::now())
                        .unwrap_or(false),
                    security_level: cert.map(|c| c.extensions.security_level.clone()),
                    last_attestation: attestation.map(|a| a.issued_at),
                    attestation_expires: attestation.map(|a| a.expires_at),
                }
            })
            .collect()
    }
}

// =============================================================================
// Result Types
// =============================================================================

/// Result of attestation verification
#[derive(Debug, Clone)]
pub struct VerificationResult {
    pub cluster_id: ClusterId,
    pub security_level: SecurityLevel,
    pub capabilities: Vec<ClusterCapability>,
    pub claim_verification: ClaimVerification,
    pub policy_violations: Vec<PolicyViolation>,
    pub verified_at: SystemTime,
}

impl VerificationResult {
    pub fn is_valid(&self) -> bool {
        self.policy_violations.is_empty()
    }
}

/// Claim verification details
#[derive(Debug, Clone)]
pub struct ClaimVerification {
    pub version_verified: bool,
    pub config_verified: bool,
    pub measurements_verified: bool,
    pub warnings: Vec<String>,
}

/// Policy violation details
#[derive(Debug, Clone)]
pub struct PolicyViolation {
    pub policy: String,
    pub severity: ViolationSeverity,
    pub description: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ViolationSeverity {
    Warning,
    Error,
    Critical,
}

/// Tenant attestation verification result
#[derive(Debug, Clone)]
pub struct TenantAttestationResult {
    pub tenant_id: TenantId,
    pub cluster_id: ClusterId,
    pub isolation_satisfied: bool,
    pub residency_satisfied: bool,
    pub encryption_adequate: bool,
    pub isolation_level: IsolationLevel,
    pub encryption_type: EncryptionType,
}

impl TenantAttestationResult {
    pub fn is_valid(&self) -> bool {
        self.isolation_satisfied && self.residency_satisfied && self.encryption_adequate
    }
}

/// Cluster attestation status for reporting
#[derive(Debug, Clone)]
pub struct ClusterAttestationStatus {
    pub cluster_id: ClusterId,
    pub has_valid_cert: bool,
    pub has_valid_attestation: bool,
    pub security_level: Option<SecurityLevel>,
    pub last_attestation: Option<SystemTime>,
    pub attestation_expires: Option<SystemTime>,
}

// =============================================================================
// Error Types
// =============================================================================

/// Attestation errors
#[derive(Debug, Clone)]
pub enum AttestationError {
    CertificateExpired { not_after: SystemTime },
    CertificateNotYetValid { not_before: SystemTime },
    CertificateRevoked { fingerprint: CertificateFingerprint },
    UntrustedIssuer { issuer: String },
    WeakKey { algorithm: String, size: u32, minimum: u32 },
    InsufficientSecurityLevel { required: SecurityLevel, actual: SecurityLevel },
    TokenExpired { expired_at: SystemTime },
    NotInAudience { expected: ClusterId, audience: Vec<ClusterId> },
    UnknownCluster { cluster_id: ClusterId },
    NoAttestation { cluster_id: ClusterId },
    TenantNotAttested { tenant_id: TenantId, cluster_id: ClusterId },
    MissingPolicy { policy: String },
    SignatureInvalid,
    InvalidClaims { reason: String },
}

// =============================================================================
// Helpers
// =============================================================================

fn generate_token_id() -> String {
    use std::time::UNIX_EPOCH;
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("att-{:x}", timestamp)
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_test_ca() -> ClusterCertificate {
        ClusterCertificate {
            subject: "federation-ca".to_string(),
            issuer: "federation-ca".to_string(),
            serial_number: "1".to_string(),
            not_before: SystemTime::now() - Duration::from_secs(86400),
            not_after: SystemTime::now() + Duration::from_secs(86400 * 365),
            public_key: PublicKey::Ed25519 { key: [0u8; 32] },
            fingerprint: "ca-fingerprint".to_string(),
            extensions: CertificateExtensions {
                organization_id: "federation".to_string(),
                region: "global".to_string(),
                capabilities: vec![],
                security_level: SecurityLevel::Standard,
                compliance_certs: vec![],
            },
            signature: vec![],
        }
    }
    
    fn create_test_cluster_cert(cluster_id: &str) -> ClusterCertificate {
        ClusterCertificate {
            subject: cluster_id.to_string(),
            issuer: "federation-ca".to_string(),
            serial_number: "2".to_string(),
            not_before: SystemTime::now() - Duration::from_secs(86400),
            not_after: SystemTime::now() + Duration::from_secs(86400 * 365),
            public_key: PublicKey::Ed25519 { key: [0u8; 32] },
            fingerprint: format!("{}-fingerprint", cluster_id),
            extensions: CertificateExtensions {
                organization_id: "org-1".to_string(),
                region: "us-east".to_string(),
                capabilities: vec![ClusterCapability::Compute, ClusterCapability::Storage],
                security_level: SecurityLevel::Standard,
                compliance_certs: vec!["SOC2".to_string()],
            },
            signature: vec![],
        }
    }
    
    #[test]
    fn test_add_trusted_ca() {
        let mut verifier = AttestationVerifier::new(
            "cluster-1".to_string(),
            SecurityLevel::Basic,
        );
        
        let ca = create_test_ca();
        let result = verifier.add_trusted_ca(ca);
        assert!(result.is_ok());
    }
    
    #[test]
    fn test_register_cluster_cert() {
        let mut verifier = AttestationVerifier::new(
            "cluster-1".to_string(),
            SecurityLevel::Basic,
        );
        
        // Add CA first
        verifier.add_trusted_ca(create_test_ca()).unwrap();
        
        // Register cluster cert
        let cert = create_test_cluster_cert("cluster-2");
        let result = verifier.register_cluster_cert(cert);
        assert!(result.is_ok());
    }
    
    #[test]
    fn test_isolation_level_hierarchy() {
        let verifier = AttestationVerifier::new(
            "cluster-1".to_string(),
            SecurityLevel::Basic,
        );
        
        // Higher levels satisfy lower requirements
        assert!(verifier.check_isolation_level(
            &IsolationLevel::Sovereign,
            &IsolationLevel::Shared,
        ));
        
        // Lower levels don't satisfy higher requirements
        assert!(!verifier.check_isolation_level(
            &IsolationLevel::Shared,
            &IsolationLevel::Sovereign,
        ));
    }
}
