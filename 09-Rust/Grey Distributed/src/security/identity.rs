//! # Node Identity and Authentication
//!
//! Cryptographic identity for nodes in the distributed system.
//!
//! ## Philosophy
//!
//! Every node needs a verifiable identity that:
//! - Cannot be forged
//! - Can be verified offline
//! - Can be revoked centrally
//! - Supports key rotation
//!
//! ## Implementation
//!
//! We use X.509 certificates with:
//! - Ed25519 or ECDSA keys
//! - Short-lived certificates (hours, not years)
//! - Automatic renewal via CA
//!
//! ## mTLS
//!
//! All node-to-node communication uses mutual TLS (mTLS):
//! - Both sides present certificates
//! - Both sides verify the other
//! - Channel encryption + authentication in one

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use parking_lot::{Mutex, RwLock};

// ============================================================================
// Node Identity
// ============================================================================

/// Unique node identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CertSubjectId(String);

impl CertSubjectId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
    
    pub fn as_str(&self) -> &str {
        &self.0
    }
    
    /// Generate from public key
    pub fn from_public_key(key: &[u8]) -> Self {
        // Hash the public key to get a stable ID
        let hash = simple_hash(key);
        Self(format!("node-{:016x}", hash))
    }
}

impl std::fmt::Display for CertSubjectId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Node's cryptographic identity
#[derive(Debug, Clone)]
pub struct NodeIdentity {
    /// Node ID
    pub node_id: CertSubjectId,
    
    /// Public key (DER encoded)
    pub public_key: Vec<u8>,
    
    /// Certificate chain (PEM encoded)
    pub certificate_chain: Vec<u8>,
    
    /// Certificate expiry
    pub expires_at: SystemTime,
    
    /// Node roles
    pub roles: Vec<String>,
    
    /// Node attributes
    pub attributes: HashMap<String, String>,
}

impl NodeIdentity {
    /// Check if identity is still valid
    pub fn is_valid(&self) -> bool {
        SystemTime::now() < self.expires_at
    }
    
    /// Time until expiry
    pub fn time_to_expiry(&self) -> Duration {
        self.expires_at
            .duration_since(SystemTime::now())
            .unwrap_or(Duration::ZERO)
    }
    
    /// Check if identity has a specific role
    pub fn has_role(&self, role: &str) -> bool {
        self.roles.iter().any(|r| r == role)
    }
}

// ============================================================================
// Key Pair
// ============================================================================

/// Cryptographic key pair
pub struct KeyPair {
    /// Key algorithm
    pub algorithm: KeyAlgorithm,
    
    /// Private key (kept secret!)
    private_key: Vec<u8>,
    
    /// Public key
    public_key: Vec<u8>,
    
    /// Creation time
    created_at: SystemTime,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyAlgorithm {
    /// Ed25519 (recommended)
    Ed25519,
    /// ECDSA with P-256
    EcdsaP256,
    /// ECDSA with P-384
    EcdsaP384,
    /// RSA 2048 (legacy)
    Rsa2048,
}

impl KeyPair {
    /// Generate a new key pair
    ///
    /// # Real Implementation
    ///
    /// Would use ring, openssl, or rustls for actual key generation.
    /// Here we just simulate the interface.
    pub fn generate(algorithm: KeyAlgorithm) -> Self {
        let key_size = match algorithm {
            KeyAlgorithm::Ed25519 => 32,
            KeyAlgorithm::EcdsaP256 => 32,
            KeyAlgorithm::EcdsaP384 => 48,
            KeyAlgorithm::Rsa2048 => 256,
        };
        
        Self {
            algorithm,
            private_key: vec![0u8; key_size],
            public_key: vec![0u8; key_size],
            created_at: SystemTime::now(),
        }
    }
    
    /// Get public key bytes
    pub fn public_key(&self) -> &[u8] {
        &self.public_key
    }
    
    /// Sign data
    ///
    /// Returns signature bytes.
    pub fn sign(&self, data: &[u8]) -> Vec<u8> {
        // Would use actual crypto here
        let hash = simple_hash(data);
        hash.to_le_bytes().to_vec()
    }
    
    /// Verify signature
    pub fn verify(&self, data: &[u8], signature: &[u8]) -> bool {
        // Would use actual crypto here
        let expected = self.sign(data);
        expected == signature
    }
    
    /// Age of the key
    pub fn age(&self) -> Duration {
        self.created_at.elapsed().unwrap_or(Duration::ZERO)
    }
}

// ============================================================================
// Certificate Manager
// ============================================================================

/// Manages node certificates
pub struct CertificateManager {
    /// Current certificate
    current: RwLock<Option<Certificate>>,
    
    /// CA certificate (for verification)
    ca_certificate: RwLock<Option<Certificate>>,
    
    /// Revoked certificates
    revoked: RwLock<Vec<String>>,
    
    /// Renewal threshold
    renewal_threshold: Duration,
    
    /// Auto-renewal enabled
    auto_renew: AtomicBool,
}

/// X.509 certificate (simplified)
#[derive(Debug, Clone)]
pub struct Certificate {
    /// Serial number
    pub serial: String,
    
    /// Subject (node ID)
    pub subject: CertSubjectId,
    
    /// Issuer
    pub issuer: String,
    
    /// Not before
    pub not_before: SystemTime,
    
    /// Not after
    pub not_after: SystemTime,
    
    /// Public key
    pub public_key: Vec<u8>,
    
    /// PEM encoded certificate
    pub pem: Vec<u8>,
    
    /// Extensions
    pub extensions: HashMap<String, String>,
}

impl Certificate {
    /// Check if certificate is currently valid
    pub fn is_valid(&self) -> bool {
        let now = SystemTime::now();
        now >= self.not_before && now < self.not_after
    }
    
    /// Check if certificate needs renewal
    pub fn needs_renewal(&self, threshold: Duration) -> bool {
        if let Ok(remaining) = self.not_after.duration_since(SystemTime::now()) {
            remaining <= threshold
        } else {
            true // Already expired
        }
    }
}

impl CertificateManager {
    pub fn new() -> Self {
        Self {
            current: RwLock::new(None),
            ca_certificate: RwLock::new(None),
            revoked: RwLock::new(Vec::new()),
            renewal_threshold: Duration::from_secs(3600), // 1 hour
            auto_renew: AtomicBool::new(true),
        }
    }
    
    /// Set the CA certificate
    pub fn set_ca_certificate(&self, cert: Certificate) {
        *self.ca_certificate.write() = Some(cert);
    }
    
    /// Set the current certificate
    pub fn set_certificate(&self, cert: Certificate) {
        *self.current.write() = Some(cert);
    }
    
    /// Get current certificate
    pub fn certificate(&self) -> Option<Certificate> {
        self.current.read().clone()
    }
    
    /// Check if current certificate needs renewal
    pub fn needs_renewal(&self) -> bool {
        if let Some(cert) = self.current.read().as_ref() {
            cert.needs_renewal(self.renewal_threshold)
        } else {
            true // No certificate
        }
    }
    
    /// Verify a certificate
    ///
    /// Checks:
    /// - Signature chain (up to CA)
    /// - Time validity
    /// - Not revoked
    pub fn verify(&self, cert: &Certificate) -> Result<(), CertError> {
        // Check time validity
        if !cert.is_valid() {
            return Err(CertError::Expired);
        }
        
        // Check revocation
        if self.revoked.read().contains(&cert.serial) {
            return Err(CertError::Revoked);
        }
        
        // Would verify signature chain here
        Ok(())
    }
    
    /// Add to revocation list
    pub fn revoke(&self, serial: String) {
        self.revoked.write().push(serial);
    }
    
    /// Check if serial is revoked
    pub fn is_revoked(&self, serial: &str) -> bool {
        self.revoked.read().iter().any(|s| s == serial)
    }
}

impl Default for CertificateManager {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum CertError {
    Expired,
    NotYetValid,
    Revoked,
    InvalidSignature,
    UnknownIssuer,
    BadFormat,
}

// ============================================================================
// Token-based Authentication
// ============================================================================

/// JWT-like authentication tokens
pub struct TokenManager {
    /// Signing key
    signing_key: RwLock<Vec<u8>>,
    
    /// Token lifetime
    token_lifetime: Duration,
    
    /// Valid tokens (for revocation checking)
    valid_tokens: RwLock<HashMap<String, TokenInfo>>,
    
    /// Token counter
    counter: AtomicU64,
}

#[derive(Debug, Clone)]
struct TokenInfo {
    subject: String,
    issued_at: SystemTime,
    expires_at: SystemTime,
    revoked: bool,
}

/// Authentication token
#[derive(Debug, Clone)]
pub struct AuthToken {
    /// Token ID
    pub token_id: String,
    
    /// Subject (node ID)
    pub subject: String,
    
    /// Issued at
    pub issued_at: SystemTime,
    
    /// Expires at
    pub expires_at: SystemTime,
    
    /// Claims
    pub claims: HashMap<String, String>,
    
    /// Signature
    pub signature: Vec<u8>,
}

impl TokenManager {
    pub fn new(token_lifetime: Duration) -> Self {
        Self {
            signing_key: RwLock::new(vec![0u8; 32]),
            token_lifetime,
            valid_tokens: RwLock::new(HashMap::new()),
            counter: AtomicU64::new(0),
        }
    }
    
    /// Set signing key
    pub fn set_signing_key(&self, key: Vec<u8>) {
        *self.signing_key.write() = key;
    }
    
    /// Issue a new token
    pub fn issue(&self, subject: &str, claims: HashMap<String, String>) -> AuthToken {
        let now = SystemTime::now();
        let expires_at = now + self.token_lifetime;
        let token_id = format!(
            "tok_{}",
            self.counter.fetch_add(1, Ordering::SeqCst)
        );
        
        let token = AuthToken {
            token_id: token_id.clone(),
            subject: subject.to_string(),
            issued_at: now,
            expires_at,
            claims,
            signature: Vec::new(), // Would sign here
        };
        
        // Track issued token
        self.valid_tokens.write().insert(token_id, TokenInfo {
            subject: subject.to_string(),
            issued_at: now,
            expires_at,
            revoked: false,
        });
        
        token
    }
    
    /// Validate a token
    pub fn validate(&self, token: &AuthToken) -> Result<(), TokenError> {
        // Check expiry
        if SystemTime::now() >= token.expires_at {
            return Err(TokenError::Expired);
        }
        
        // Check revocation
        if let Some(info) = self.valid_tokens.read().get(&token.token_id) {
            if info.revoked {
                return Err(TokenError::Revoked);
            }
        } else {
            return Err(TokenError::Unknown);
        }
        
        // Would verify signature here
        
        Ok(())
    }
    
    /// Revoke a token
    pub fn revoke(&self, token_id: &str) {
        if let Some(info) = self.valid_tokens.write().get_mut(token_id) {
            info.revoked = true;
        }
    }
    
    /// Clean up expired tokens
    pub fn cleanup_expired(&self) {
        let now = SystemTime::now();
        self.valid_tokens.write().retain(|_, info| {
            info.expires_at > now
        });
    }
}

#[derive(Debug)]
pub enum TokenError {
    Expired,
    Revoked,
    Unknown,
    InvalidSignature,
    MissingClaim(String),
}

// ============================================================================
// Identity Verifier
// ============================================================================

/// Verifies node identities
pub struct IdentityVerifier {
    /// Trusted CA certificates
    trusted_cas: RwLock<Vec<Certificate>>,
    
    /// Known nodes (cached identities)
    known_nodes: RwLock<HashMap<CertSubjectId, NodeIdentity>>,
    
    /// Verification policy
    policy: VerificationPolicy,
}

#[derive(Debug, Clone)]
pub struct VerificationPolicy {
    /// Require valid certificate
    pub require_certificate: bool,
    
    /// Require specific roles
    pub required_roles: Vec<String>,
    
    /// Allow self-signed certificates
    pub allow_self_signed: bool,
    
    /// Maximum certificate age
    pub max_cert_age: Duration,
}

impl Default for VerificationPolicy {
    fn default() -> Self {
        Self {
            require_certificate: true,
            required_roles: Vec::new(),
            allow_self_signed: false,
            max_cert_age: Duration::from_secs(86400), // 24 hours
        }
    }
}

impl IdentityVerifier {
    pub fn new(policy: VerificationPolicy) -> Self {
        Self {
            trusted_cas: RwLock::new(Vec::new()),
            known_nodes: RwLock::new(HashMap::new()),
            policy,
        }
    }
    
    /// Add a trusted CA
    pub fn add_trusted_ca(&self, ca: Certificate) {
        self.trusted_cas.write().push(ca);
    }
    
    /// Verify a node's identity
    pub fn verify(&self, identity: &NodeIdentity) -> Result<(), VerifyError> {
        // Check expiry
        if !identity.is_valid() {
            return Err(VerifyError::Expired);
        }
        
        // Check required roles
        for role in &self.policy.required_roles {
            if !identity.has_role(role) {
                return Err(VerifyError::MissingRole(role.clone()));
            }
        }
        
        // Would verify certificate chain here
        
        Ok(())
    }
    
    /// Cache a verified identity
    pub fn cache_identity(&self, identity: NodeIdentity) {
        self.known_nodes.write().insert(identity.node_id.clone(), identity);
    }
    
    /// Get cached identity
    pub fn get_identity(&self, node_id: &CertSubjectId) -> Option<NodeIdentity> {
        self.known_nodes.read().get(node_id).cloned()
    }
}

#[derive(Debug)]
pub enum VerifyError {
    Expired,
    InvalidCertificate,
    UnknownCA,
    MissingRole(String),
    Revoked,
}

// ============================================================================
// Helpers
// ============================================================================

/// Simple non-cryptographic hash for demonstration
fn simple_hash(data: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for byte in data {
        h ^= *byte as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_node_id() {
        let id = CertSubjectId::new("node-1");
        assert_eq!(id.as_str(), "node-1");
        
        let id_from_key = CertSubjectId::from_public_key(b"test-key");
        assert!(id_from_key.as_str().starts_with("node-"));
    }
    
    #[test]
    fn test_key_pair() {
        let kp = KeyPair::generate(KeyAlgorithm::Ed25519);
        
        let data = b"test message";
        let sig = kp.sign(data);
        assert!(kp.verify(data, &sig));
        assert!(!kp.verify(b"other", &sig));
    }
    
    #[test]
    fn test_certificate_validity() {
        let now = SystemTime::now();
        
        let cert = Certificate {
            serial: "123".into(),
            subject: CertSubjectId::new("node-1"),
            issuer: "ca".into(),
            not_before: now - Duration::from_secs(3600),
            not_after: now + Duration::from_secs(3600),
            public_key: Vec::new(),
            pem: Vec::new(),
            extensions: HashMap::new(),
        };
        
        assert!(cert.is_valid());
        assert!(!cert.needs_renewal(Duration::from_secs(7200)));
    }
    
    #[test]
    fn test_token_manager() {
        let manager = TokenManager::new(Duration::from_secs(3600));
        
        let token = manager.issue("node-1", HashMap::new());
        assert!(manager.validate(&token).is_ok());
        
        manager.revoke(&token.token_id);
        assert!(manager.validate(&token).is_err());
    }
}
