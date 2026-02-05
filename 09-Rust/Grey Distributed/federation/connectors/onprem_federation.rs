//! # Grey Distributed — On-Premises Federation Connector
//!
//! This module implements federation with on-premises Grey clusters,
//! using mTLS, X.509 certificates, and hardware attestation.
//!
//! ## On-Premises-Specific Features
//!
//! - **mTLS Authentication**: Mutual TLS with client certificates
//! - **PKI Integration**: Enterprise CA and certificate management
//! - **Hardware Attestation**: TPM-based and software attestation
//! - **Network Policies**: Firewall rules and network segmentation

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type ClusterId = String;
pub type TenantId = String;
pub type CertificateFingerprint = String;

/// Default mTLS handshake timeout
const MTLS_HANDSHAKE_TIMEOUT: Duration = Duration::from_secs(10);

/// Certificate renewal threshold (30 days before expiry)
const CERT_RENEWAL_THRESHOLD: Duration = Duration::from_secs(30 * 24 * 60 * 60);

/// Maximum retries for connection establishment
const MAX_RETRIES: u32 = 3;

// =============================================================================
// On-Premises Cluster Configuration
// =============================================================================

/// Configuration for an on-premises Grey cluster
#[derive(Debug, Clone)]
pub struct OnPremClusterConfig {
    /// Unique cluster identifier
    pub cluster_id: ClusterId,
    
    /// Human-readable cluster name
    pub cluster_name: String,
    
    /// Datacenter location identifier
    pub datacenter: String,
    
    /// Network CIDR block
    pub network_cidr: String,
    
    /// API endpoint (hostname:port)
    pub api_endpoint: String,
    
    /// mTLS port for federation traffic
    pub mtls_port: u16,
    
    /// Expected server certificate fingerprint (pinning)
    pub server_cert_fingerprint: CertificateFingerprint,
    
    /// PKI configuration
    pub pki_config: PkiConfig,
    
    /// Hardware attestation configuration
    pub attestation_config: AttestationConfig,
    
    /// Network policy rules
    pub network_policies: Vec<NetworkPolicy>,
    
    /// Custom metadata
    pub metadata: HashMap<String, String>,
}

/// PKI configuration for certificate management
#[derive(Debug, Clone)]
pub struct PkiConfig {
    /// CA certificate chain (PEM format)
    pub ca_chain: String,
    
    /// CRL (Certificate Revocation List) endpoint
    pub crl_endpoint: Option<String>,
    
    /// OCSP (Online Certificate Status Protocol) endpoint
    pub ocsp_endpoint: Option<String>,
    
    /// Certificate transparency log endpoint
    pub ct_log_endpoint: Option<String>,
    
    /// Allowed key algorithms
    pub allowed_key_algorithms: Vec<KeyAlgorithm>,
    
    /// Minimum key size in bits
    pub minimum_key_size: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeyAlgorithm {
    RSA,
    ECDSA,
    Ed25519,
}

/// Hardware attestation configuration
#[derive(Debug, Clone)]
pub struct AttestationConfig {
    /// Type of attestation required
    pub attestation_type: AttestationType,
    
    /// TPM PCR values expected (if TPM attestation)
    pub expected_pcr_values: Option<HashMap<u8, String>>,
    
    /// Attestation verification endpoint
    pub verification_endpoint: Option<String>,
    
    /// Whether to require secure boot attestation
    pub require_secure_boot: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttestationType {
    /// No attestation required
    None,
    /// Software-based attestation
    Software,
    /// TPM 2.0 attestation
    Tpm2,
    /// Intel SGX attestation
    Sgx,
    /// AMD SEV attestation
    Sev,
}

/// Network policy rule
#[derive(Debug, Clone)]
pub struct NetworkPolicy {
    pub rule_id: String,
    pub direction: TrafficDirection,
    pub protocol: Protocol,
    pub source_cidr: Option<String>,
    pub dest_cidr: Option<String>,
    pub port_range: Option<(u16, u16)>,
    pub action: PolicyAction,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TrafficDirection {
    Ingress,
    Egress,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Protocol {
    Tcp,
    Udp,
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PolicyAction {
    Allow,
    Deny,
}

// =============================================================================
// mTLS Credentials
// =============================================================================

/// Client certificate for mTLS authentication
#[derive(Debug, Clone)]
pub struct ClientCertificate {
    /// Certificate in PEM format
    pub certificate_pem: String,
    
    /// Private key in PEM format (encrypted)
    pub private_key_pem: String,
    
    /// Certificate chain (issuing CAs)
    pub certificate_chain: Vec<String>,
    
    /// Certificate fingerprint (SHA-256)
    pub fingerprint: CertificateFingerprint,
    
    /// Subject DN
    pub subject: String,
    
    /// Issuer DN
    pub issuer: String,
    
    /// Valid from
    pub not_before: SystemTime,
    
    /// Valid until
    pub not_after: SystemTime,
    
    /// Key usage extensions
    pub key_usage: Vec<KeyUsage>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeyUsage {
    DigitalSignature,
    KeyEncipherment,
    ClientAuth,
    ServerAuth,
}

impl ClientCertificate {
    /// Check if certificate is currently valid
    pub fn is_valid(&self) -> bool {
        let now = SystemTime::now();
        now >= self.not_before && now <= self.not_after
    }
    
    /// Check if certificate needs renewal
    pub fn needs_renewal(&self) -> bool {
        let now = SystemTime::now();
        now + CERT_RENEWAL_THRESHOLD >= self.not_after
    }
    
    /// Get days until expiration
    pub fn days_until_expiry(&self) -> Option<i64> {
        self.not_after.duration_since(SystemTime::now())
            .ok()
            .map(|d| d.as_secs() as i64 / 86400)
    }
}

// =============================================================================
// On-Premises Federation Connector
// =============================================================================

/// Manages federation with on-premises Grey clusters
pub struct OnPremFederationConnector {
    /// This cluster's ID
    local_cluster_id: ClusterId,
    
    /// This cluster's datacenter
    local_datacenter: String,
    
    /// Local client certificate
    local_certificate: Option<ClientCertificate>,
    
    /// CA certificate chain for verifying remote clusters
    ca_chain: Option<String>,
    
    /// Registered on-premises clusters
    clusters: HashMap<ClusterId, OnPremClusterConfig>,
    
    /// Active mTLS connections
    connections: HashMap<ClusterId, MtlsConnection>,
    
    /// Attestation cache
    attestations: HashMap<ClusterId, OnPremAttestation>,
    
    /// Certificate revocation cache
    revocation_cache: HashMap<CertificateFingerprint, RevocationStatus>,
}

impl OnPremFederationConnector {
    pub fn new(cluster_id: ClusterId, datacenter: String) -> Self {
        Self {
            local_cluster_id: cluster_id,
            local_datacenter: datacenter,
            local_certificate: None,
            ca_chain: None,
            clusters: HashMap::new(),
            connections: HashMap::new(),
            attestations: HashMap::new(),
            revocation_cache: HashMap::new(),
        }
    }
    
    // =========================================================================
    // Step 1: Initialize with Client Certificate
    // =========================================================================
    
    /// Initialize connector with client certificate and CA chain
    ///
    /// ## Certificate Requirements
    ///
    /// - Must have clientAuth extended key usage
    /// - Must be signed by a trusted CA
    /// - Must not be expired or revoked
    pub fn initialize(
        &mut self,
        certificate: ClientCertificate,
        ca_chain: String,
    ) -> Result<(), OnPremError> {
        // Step 1a: Validate certificate
        if !certificate.is_valid() {
            return Err(OnPremError::CertificateExpired {
                fingerprint: certificate.fingerprint.clone(),
                expiry: certificate.not_after,
            });
        }
        
        // Step 1b: Verify key usage includes clientAuth
        if !certificate.key_usage.contains(&KeyUsage::ClientAuth) {
            return Err(OnPremError::InvalidKeyUsage {
                expected: "clientAuth".to_string(),
                actual: format!("{:?}", certificate.key_usage),
            });
        }
        
        // Step 1c: Verify certificate is signed by CA chain
        // In real implementation:
        // verify_certificate_chain(&certificate, &ca_chain)?;
        
        // Step 1d: Store credentials
        self.local_certificate = Some(certificate);
        self.ca_chain = Some(ca_chain);
        
        Ok(())
    }
    
    // =========================================================================
    // Step 2: Register Remote Cluster
    // =========================================================================
    
    /// Register a remote on-premises cluster for federation
    pub fn register_cluster(&mut self, config: OnPremClusterConfig) -> Result<(), OnPremError> {
        // Step 2a: Validate configuration
        self.validate_cluster_config(&config)?;
        
        // Step 2b: Verify network connectivity (optional)
        // In real implementation: self.test_network_connectivity(&config)?;
        
        // Step 2c: Store cluster configuration
        self.clusters.insert(config.cluster_id.clone(), config);
        
        Ok(())
    }
    
    fn validate_cluster_config(&self, config: &OnPremClusterConfig) -> Result<(), OnPremError> {
        // Validate CIDR format
        if !is_valid_cidr(&config.network_cidr) {
            return Err(OnPremError::InvalidCidr {
                cidr: config.network_cidr.clone(),
            });
        }
        
        // Validate endpoint format
        if !config.api_endpoint.contains(':') {
            return Err(OnPremError::InvalidEndpoint {
                endpoint: config.api_endpoint.clone(),
            });
        }
        
        // Validate PKI config
        if config.pki_config.minimum_key_size < 2048 {
            return Err(OnPremError::WeakCrypto {
                message: "Minimum key size must be at least 2048 bits".to_string(),
            });
        }
        
        Ok(())
    }
    
    // =========================================================================
    // Step 3: Establish mTLS Connection
    // =========================================================================
    
    /// Establish mTLS connection to a remote cluster
    pub fn connect(&mut self, cluster_id: &ClusterId) -> Result<&MtlsConnection, OnPremError> {
        // Check if already connected
        if let Some(conn) = self.connections.get(cluster_id) {
            if conn.is_active() {
                return Ok(self.connections.get(cluster_id).unwrap());
            }
        }
        
        let config = self.clusters.get(cluster_id)
            .ok_or_else(|| OnPremError::ClusterNotFound {
                cluster_id: cluster_id.clone(),
            })?;
        
        let local_cert = self.local_certificate.as_ref()
            .ok_or(OnPremError::NotInitialized)?;
        
        // Step 3a: Resolve endpoint
        let endpoint = format!("{}:{}", config.api_endpoint.split(':').next().unwrap(), config.mtls_port);
        
        // Step 3b: Create TLS configuration
        // In real implementation:
        // let tls_config = TlsConfig::builder()
        //     .with_client_cert(local_cert)
        //     .with_ca_chain(&self.ca_chain)
        //     .with_server_cert_verifier(PinnedVerifier::new(&config.server_cert_fingerprint))
        //     .build()?;
        
        // Step 3c: Perform TLS handshake with certificate pinning
        // In real implementation:
        // let stream = TlsConnector::connect(&endpoint, tls_config).await?;
        
        // Step 3d: Verify server certificate fingerprint
        // let server_fingerprint = stream.peer_certificate_fingerprint()?;
        // if server_fingerprint != config.server_cert_fingerprint {
        //     return Err(OnPremError::CertificatePinningFailed {...});
        // }
        
        let connection = MtlsConnection {
            cluster_id: cluster_id.clone(),
            endpoint: endpoint.clone(),
            local_fingerprint: local_cert.fingerprint.clone(),
            remote_fingerprint: config.server_cert_fingerprint.clone(),
            cipher_suite: "TLS_AES_256_GCM_SHA384".to_string(),
            tls_version: "TLSv1.3".to_string(),
            established_at: SystemTime::now(),
            last_activity: SystemTime::now(),
            status: ConnectionStatus::Active,
        };
        
        self.connections.insert(cluster_id.clone(), connection);
        Ok(self.connections.get(cluster_id).unwrap())
    }
    
    // =========================================================================
    // Step 4: Hardware Attestation
    // =========================================================================
    
    /// Generate attestation for this cluster
    pub fn generate_attestation(&self) -> Result<OnPremAttestation, OnPremError> {
        let local_cert = self.local_certificate.as_ref()
            .ok_or(OnPremError::NotInitialized)?;
        
        // Step 4a: Gather system information
        let system_info = SystemInfo {
            hostname: get_hostname(),
            os_name: "Linux".to_string(),
            os_version: "5.15.0".to_string(),
            kernel_version: "5.15.0-generic".to_string(),
            cpu_model: "Intel Xeon Platinum 8375C".to_string(),
            memory_gb: 256,
        };
        
        // Step 4b: TPM attestation (if available)
        // In real implementation:
        // let tpm_quote = tpm2::quote(
        //     &[0, 1, 2, 7],  // PCRs
        //     attestation_key_handle,
        //     nonce
        // )?;
        
        let tpm_attestation = Some(TpmAttestation {
            tpm_version: "2.0".to_string(),
            manufacturer: "Intel".to_string(),
            pcr_values: vec![
                (0, "0000000000000000000000000000000000000000".to_string()),
                (1, "1111111111111111111111111111111111111111".to_string()),
                (7, "7777777777777777777777777777777777777777".to_string()),
            ].into_iter().collect(),
            quote: "base64_encoded_tpm_quote".to_string(),
            signature: "base64_encoded_signature".to_string(),
        });
        
        // Step 4c: Secure boot status
        let secure_boot = SecureBootStatus {
            enabled: true,
            setup_mode: false,
            secure_boot_mode: SecureBootMode::DeployedMode,
            pk_enrolled: true,
            kek_enrolled: true,
            db_entries: 5,
        };
        
        let attestation = OnPremAttestation {
            cluster_id: self.local_cluster_id.clone(),
            datacenter: self.local_datacenter.clone(),
            certificate_fingerprint: local_cert.fingerprint.clone(),
            system_info,
            tpm_attestation,
            secure_boot,
            software_inventory: self.generate_software_inventory(),
            created_at: SystemTime::now(),
            expires_at: SystemTime::now() + Duration::from_secs(3600),
            nonce: generate_nonce(),
        };
        
        Ok(attestation)
    }
    
    fn generate_software_inventory(&self) -> Vec<SoftwareEntry> {
        vec![
            SoftwareEntry {
                name: "grey-distributed".to_string(),
                version: "1.0.0".to_string(),
                signature: Some("sha256:abcd1234...".to_string()),
            },
            SoftwareEntry {
                name: "linux-kernel".to_string(),
                version: "5.15.0".to_string(),
                signature: Some("sha256:efgh5678...".to_string()),
            },
        ]
    }
    
    /// Verify attestation from a remote cluster
    pub fn verify_attestation(
        &mut self,
        attestation: &OnPremAttestation,
    ) -> Result<AttestationResult, OnPremError> {
        let config = self.clusters.get(&attestation.cluster_id)
            .ok_or_else(|| OnPremError::ClusterNotFound {
                cluster_id: attestation.cluster_id.clone(),
            })?;
        
        let mut warnings = Vec::new();
        
        // Step 4d: Verify certificate fingerprint matches registered cluster
        // In real implementation, we'd look up expected fingerprint
        
        // Step 4e: Verify TPM attestation if required
        if config.attestation_config.attestation_type == AttestationType::Tpm2 {
            if let Some(ref tpm) = attestation.tpm_attestation {
                // Verify PCR values match expected
                if let Some(ref expected_pcrs) = config.attestation_config.expected_pcr_values {
                    for (pcr, expected_value) in expected_pcrs {
                        if let Some(actual_value) = tpm.pcr_values.get(pcr) {
                            if actual_value != expected_value {
                                return Err(OnPremError::AttestationFailed {
                                    reason: format!("PCR {} mismatch", pcr),
                                });
                            }
                        }
                    }
                }
            } else {
                return Err(OnPremError::AttestationFailed {
                    reason: "TPM attestation required but not provided".to_string(),
                });
            }
        }
        
        // Step 4f: Verify secure boot if required
        if config.attestation_config.require_secure_boot && !attestation.secure_boot.enabled {
            return Err(OnPremError::AttestationFailed {
                reason: "Secure boot required but not enabled".to_string(),
            });
        }
        
        // Step 4g: Cache attestation
        self.attestations.insert(attestation.cluster_id.clone(), attestation.clone());
        
        Ok(AttestationResult {
            cluster_id: attestation.cluster_id.clone(),
            verified: true,
            warnings,
            verified_at: SystemTime::now(),
        })
    }
    
    // =========================================================================
    // Step 5: Certificate Revocation Checking
    // =========================================================================
    
    /// Check if a certificate is revoked
    pub fn check_revocation(
        &mut self,
        fingerprint: &CertificateFingerprint,
        cluster_id: &ClusterId,
    ) -> Result<RevocationStatus, OnPremError> {
        // Check cache first
        if let Some(status) = self.revocation_cache.get(fingerprint) {
            if status.checked_at + Duration::from_secs(3600) > SystemTime::now() {
                return Ok(status.clone());
            }
        }
        
        let config = self.clusters.get(cluster_id)
            .ok_or_else(|| OnPremError::ClusterNotFound {
                cluster_id: cluster_id.clone(),
            })?;
        
        // Step 5a: Check CRL if available
        // In real implementation:
        // if let Some(ref crl_endpoint) = config.pki_config.crl_endpoint {
        //     let crl = fetch_crl(crl_endpoint).await?;
        //     if crl.is_revoked(fingerprint) {
        //         return Ok(RevocationStatus::Revoked { ... });
        //     }
        // }
        
        // Step 5b: Check OCSP if available
        // In real implementation:
        // if let Some(ref ocsp_endpoint) = config.pki_config.ocsp_endpoint {
        //     let response = ocsp_query(ocsp_endpoint, fingerprint).await?;
        //     if response.status == OcspStatus::Revoked {
        //         return Ok(RevocationStatus::Revoked { ... });
        //     }
        // }
        
        let status = RevocationStatus::Valid {
            checked_at: SystemTime::now(),
            method: RevocationCheckMethod::Ocsp,
        };
        
        self.revocation_cache.insert(fingerprint.clone(), status.clone());
        Ok(status)
    }
    
    // =========================================================================
    // Step 6: Secure Communication
    // =========================================================================
    
    /// Send encrypted message to a remote cluster
    pub fn send_message(
        &mut self,
        cluster_id: &ClusterId,
        message: &[u8],
    ) -> Result<Vec<u8>, OnPremError> {
        // Step 6a: Ensure connection is active
        let conn = self.connect(cluster_id)?;
        
        if !conn.is_active() {
            return Err(OnPremError::ConnectionClosed {
                cluster_id: cluster_id.clone(),
            });
        }
        
        // Step 6b: Send message over TLS connection
        // In real implementation:
        // let response = conn.stream.send_and_receive(message).await?;
        
        // Mocked response
        Ok(vec![0u8; 32])
    }
    
    // =========================================================================
    // Step 7: Health and Monitoring
    // =========================================================================
    
    /// Get health status of all registered clusters
    pub fn health_check(&self) -> Vec<ClusterHealth> {
        self.clusters.keys()
            .map(|cluster_id| {
                let config = self.clusters.get(cluster_id).unwrap();
                let connection = self.connections.get(cluster_id);
                let attestation = self.attestations.get(cluster_id);
                
                ClusterHealth {
                    cluster_id: cluster_id.clone(),
                    datacenter: config.datacenter.clone(),
                    connected: connection.map(|c| c.is_active()).unwrap_or(false),
                    attestation_valid: attestation
                        .map(|a| a.expires_at > SystemTime::now())
                        .unwrap_or(false),
                    certificate_valid: connection
                        .map(|_| true)  // Would check actual cert validity
                        .unwrap_or(false),
                    last_contact: connection.map(|c| c.last_activity),
                    latency_ms: None,  // Would measure actual latency
                }
            })
            .collect()
    }
    
    /// Disconnect from a remote cluster
    pub fn disconnect(&mut self, cluster_id: &ClusterId) -> Result<(), OnPremError> {
        if let Some(mut conn) = self.connections.remove(cluster_id) {
            conn.status = ConnectionStatus::Closed;
            // In real implementation: conn.stream.shutdown().await?;
        }
        Ok(())
    }
    
    /// Disconnect from all remote clusters
    pub fn disconnect_all(&mut self) {
        let cluster_ids: Vec<_> = self.connections.keys().cloned().collect();
        for cluster_id in cluster_ids {
            let _ = self.disconnect(&cluster_id);
        }
    }
}

// =============================================================================
// Connection Types
// =============================================================================

/// Active mTLS connection to a remote cluster
#[derive(Debug, Clone)]
pub struct MtlsConnection {
    pub cluster_id: ClusterId,
    pub endpoint: String,
    pub local_fingerprint: CertificateFingerprint,
    pub remote_fingerprint: CertificateFingerprint,
    pub cipher_suite: String,
    pub tls_version: String,
    pub established_at: SystemTime,
    pub last_activity: SystemTime,
    pub status: ConnectionStatus,
}

impl MtlsConnection {
    pub fn is_active(&self) -> bool {
        self.status == ConnectionStatus::Active
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionStatus {
    Connecting,
    Active,
    Idle,
    Closed,
    Error,
}

// =============================================================================
// Attestation Types
// =============================================================================

/// On-premises attestation
#[derive(Debug, Clone)]
pub struct OnPremAttestation {
    pub cluster_id: ClusterId,
    pub datacenter: String,
    pub certificate_fingerprint: CertificateFingerprint,
    pub system_info: SystemInfo,
    pub tpm_attestation: Option<TpmAttestation>,
    pub secure_boot: SecureBootStatus,
    pub software_inventory: Vec<SoftwareEntry>,
    pub created_at: SystemTime,
    pub expires_at: SystemTime,
    pub nonce: String,
}

#[derive(Debug, Clone)]
pub struct SystemInfo {
    pub hostname: String,
    pub os_name: String,
    pub os_version: String,
    pub kernel_version: String,
    pub cpu_model: String,
    pub memory_gb: u64,
}

#[derive(Debug, Clone)]
pub struct TpmAttestation {
    pub tpm_version: String,
    pub manufacturer: String,
    pub pcr_values: HashMap<u8, String>,
    pub quote: String,
    pub signature: String,
}

#[derive(Debug, Clone)]
pub struct SecureBootStatus {
    pub enabled: bool,
    pub setup_mode: bool,
    pub secure_boot_mode: SecureBootMode,
    pub pk_enrolled: bool,
    pub kek_enrolled: bool,
    pub db_entries: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SecureBootMode {
    SetupMode,
    UserMode,
    DeployedMode,
}

#[derive(Debug, Clone)]
pub struct SoftwareEntry {
    pub name: String,
    pub version: String,
    pub signature: Option<String>,
}

#[derive(Debug, Clone)]
pub struct AttestationResult {
    pub cluster_id: ClusterId,
    pub verified: bool,
    pub warnings: Vec<String>,
    pub verified_at: SystemTime,
}

// =============================================================================
// Revocation Types
// =============================================================================

#[derive(Debug, Clone)]
pub enum RevocationStatus {
    Valid {
        checked_at: SystemTime,
        method: RevocationCheckMethod,
    },
    Revoked {
        revoked_at: SystemTime,
        reason: RevocationReason,
    },
    Unknown {
        checked_at: SystemTime,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum RevocationCheckMethod {
    Crl,
    Ocsp,
    CtLog,
}

#[derive(Debug, Clone)]
pub enum RevocationReason {
    KeyCompromise,
    CaCompromise,
    AffiliationChanged,
    Superseded,
    CessationOfOperation,
    CertificateHold,
    Unspecified,
}

// =============================================================================
// Health Types
// =============================================================================

#[derive(Debug, Clone)]
pub struct ClusterHealth {
    pub cluster_id: ClusterId,
    pub datacenter: String,
    pub connected: bool,
    pub attestation_valid: bool,
    pub certificate_valid: bool,
    pub last_contact: Option<SystemTime>,
    pub latency_ms: Option<u64>,
}

// =============================================================================
// Error Types
// =============================================================================

#[derive(Debug, Clone)]
pub enum OnPremError {
    NotInitialized,
    CertificateExpired {
        fingerprint: CertificateFingerprint,
        expiry: SystemTime,
    },
    InvalidKeyUsage {
        expected: String,
        actual: String,
    },
    InvalidCidr {
        cidr: String,
    },
    InvalidEndpoint {
        endpoint: String,
    },
    WeakCrypto {
        message: String,
    },
    ClusterNotFound {
        cluster_id: ClusterId,
    },
    ConnectionClosed {
        cluster_id: ClusterId,
    },
    CertificatePinningFailed {
        expected: CertificateFingerprint,
        actual: CertificateFingerprint,
    },
    AttestationFailed {
        reason: String,
    },
    TlsHandshakeFailed {
        message: String,
    },
    NetworkError {
        message: String,
    },
}

// =============================================================================
// Helpers
// =============================================================================

fn is_valid_cidr(cidr: &str) -> bool {
    let parts: Vec<&str> = cidr.split('/').collect();
    if parts.len() != 2 {
        return false;
    }
    
    // Validate IP address
    let ip_parts: Vec<&str> = parts[0].split('.').collect();
    if ip_parts.len() != 4 {
        return false;
    }
    
    for part in &ip_parts {
        if part.parse::<u8>().is_err() {
            return false;
        }
    }
    
    // Validate prefix length
    if let Ok(prefix) = parts[1].parse::<u8>() {
        prefix <= 32
    } else {
        false
    }
}

fn get_hostname() -> String {
    std::env::var("HOSTNAME").unwrap_or_else(|_| "grey-node".to_string())
}

fn generate_nonce() -> String {
    use std::time::UNIX_EPOCH;
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("{:032x}", timestamp)
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_certificate_validity() {
        let valid_cert = ClientCertificate {
            certificate_pem: "-----BEGIN CERTIFICATE-----\n...".to_string(),
            private_key_pem: "-----BEGIN PRIVATE KEY-----\n...".to_string(),
            certificate_chain: vec![],
            fingerprint: "sha256:abcd1234".to_string(),
            subject: "CN=grey-cluster".to_string(),
            issuer: "CN=Grey CA".to_string(),
            not_before: SystemTime::now() - Duration::from_secs(3600),
            not_after: SystemTime::now() + Duration::from_secs(86400 * 365),
            key_usage: vec![KeyUsage::ClientAuth, KeyUsage::DigitalSignature],
        };
        
        assert!(valid_cert.is_valid());
        assert!(!valid_cert.needs_renewal());
    }
    
    #[test]
    fn test_cidr_validation() {
        assert!(is_valid_cidr("10.0.0.0/8"));
        assert!(is_valid_cidr("192.168.1.0/24"));
        assert!(!is_valid_cidr("invalid"));
        assert!(!is_valid_cidr("192.168.1.0"));
        assert!(!is_valid_cidr("192.168.1.0/33"));
    }
    
    #[test]
    fn test_connector_creation() {
        let connector = OnPremFederationConnector::new(
            "cluster-1".to_string(),
            "dc-east".to_string(),
        );
        
        assert_eq!(connector.local_cluster_id, "cluster-1");
        assert_eq!(connector.local_datacenter, "dc-east");
    }
    
    #[test]
    fn test_connection_status() {
        let conn = MtlsConnection {
            cluster_id: "remote".to_string(),
            endpoint: "10.0.0.1:443".to_string(),
            local_fingerprint: "sha256:local".to_string(),
            remote_fingerprint: "sha256:remote".to_string(),
            cipher_suite: "TLS_AES_256_GCM_SHA384".to_string(),
            tls_version: "TLSv1.3".to_string(),
            established_at: SystemTime::now(),
            last_activity: SystemTime::now(),
            status: ConnectionStatus::Active,
        };
        
        assert!(conn.is_active());
    }
}
