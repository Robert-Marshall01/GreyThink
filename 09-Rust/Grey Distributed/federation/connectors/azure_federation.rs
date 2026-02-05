//! # Grey Distributed — Azure Federation Connector
//!
//! This module implements federation with Azure-hosted Grey clusters,
//! handling authentication, attestation, and cross-cloud communication.
//!
//! ## Azure-Specific Features
//!
//! - **Azure AD / Entra ID**: Uses managed identities and service principals
//! - **VNet Peering**: Secure cross-subscription networking
//! - **Azure Key Vault**: Customer-managed encryption keys
//! - **Azure Monitor**: Metrics and logging integration

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type ClusterId = String;
pub type TenantId = String;
pub type SubscriptionId = String;
pub type AzureRegion = String;

/// Default timeout for Azure API calls
const AZURE_API_TIMEOUT: Duration = Duration::from_secs(30);

/// Azure AD token default lifetime
const TOKEN_LIFETIME: Duration = Duration::from_secs(3600);

/// Maximum retries for transient failures
const MAX_RETRIES: u32 = 3;

/// Azure Resource Manager API version
const ARM_API_VERSION: &str = "2023-01-01";

// =============================================================================
// Azure Cluster Configuration
// =============================================================================

/// Configuration for an Azure-hosted Grey cluster
#[derive(Debug, Clone)]
pub struct AzureClusterConfig {
    /// Unique cluster identifier
    pub cluster_id: ClusterId,
    
    /// Azure AD Tenant ID
    pub tenant_id: String,
    
    /// Azure Subscription ID
    pub subscription_id: SubscriptionId,
    
    /// Azure Region (e.g., eastus)
    pub region: AzureRegion,
    
    /// Resource Group name
    pub resource_group: String,
    
    /// Virtual Network name
    pub vnet_name: String,
    
    /// Managed Identity or Service Principal for federation
    pub identity: AzureIdentity,
    
    /// Key Vault URL for encryption keys
    pub key_vault_url: Option<String>,
    
    /// AKS cluster name (if on AKS)
    pub aks_cluster: Option<String>,
    
    /// Endpoint for Grey API
    pub api_endpoint: String,
    
    /// Whether using Private Endpoints
    pub private_endpoint: bool,
    
    /// Tags for resource tracking
    pub tags: HashMap<String, String>,
}

/// Azure identity for authentication
#[derive(Debug, Clone)]
pub enum AzureIdentity {
    /// System-assigned managed identity
    SystemAssigned,
    
    /// User-assigned managed identity
    UserAssigned { client_id: String },
    
    /// Service principal with secret
    ServicePrincipal {
        client_id: String,
        client_secret: String,
    },
    
    /// Service principal with certificate
    ServicePrincipalCert {
        client_id: String,
        certificate_path: String,
        certificate_password: Option<String>,
    },
}

/// VNet peering configuration
#[derive(Debug, Clone)]
pub struct VNetPeeringConfig {
    pub name: String,
    pub local_vnet_id: String,
    pub remote_vnet_id: String,
    pub state: PeeringState,
    pub allow_virtual_network_access: bool,
    pub allow_forwarded_traffic: bool,
    pub allow_gateway_transit: bool,
    pub use_remote_gateways: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PeeringState {
    Initiated,
    Connected,
    Disconnected,
}

// =============================================================================
// Azure Authentication
// =============================================================================

/// Azure AD access token
#[derive(Debug, Clone)]
pub struct AzureAccessToken {
    pub access_token: String,
    pub token_type: String,
    pub expires_at: SystemTime,
    pub resource: String,
}

impl AzureAccessToken {
    /// Check if token is still valid
    pub fn is_valid(&self) -> bool {
        SystemTime::now() < self.expires_at
    }
    
    /// Check if token needs refresh (within 5 minutes of expiry)
    pub fn needs_refresh(&self) -> bool {
        let refresh_threshold = Duration::from_secs(300);
        SystemTime::now() + refresh_threshold >= self.expires_at
    }
}

/// Managed Identity metadata response
#[derive(Debug, Clone)]
pub struct ManagedIdentityToken {
    pub access_token: String,
    pub expires_on: String,
    pub resource: String,
    pub token_type: String,
}

// =============================================================================
// Azure Federation Connector
// =============================================================================

/// Manages federation with Azure-hosted Grey clusters
pub struct AzureFederationConnector {
    /// This cluster's ID
    local_cluster_id: ClusterId,
    
    /// This cluster's Azure tenant
    local_tenant_id: String,
    
    /// This cluster's Azure subscription
    local_subscription: SubscriptionId,
    
    /// This cluster's Azure region
    local_region: AzureRegion,
    
    /// Local identity configuration
    local_identity: Option<AzureIdentity>,
    
    /// Local access token
    local_token: Option<AzureAccessToken>,
    
    /// Registered Azure clusters
    clusters: HashMap<ClusterId, AzureClusterConfig>,
    
    /// Cached access tokens
    token_cache: HashMap<String, AzureAccessToken>,
    
    /// VNet peering connections
    vnet_peerings: HashMap<ClusterId, VNetPeeringConfig>,
    
    /// Attestation cache
    attestations: HashMap<ClusterId, AzureAttestation>,
}

impl AzureFederationConnector {
    pub fn new(
        cluster_id: ClusterId,
        tenant_id: String,
        subscription: SubscriptionId,
        region: AzureRegion,
    ) -> Self {
        Self {
            local_cluster_id: cluster_id,
            local_tenant_id: tenant_id,
            local_subscription: subscription,
            local_region: region,
            local_identity: None,
            local_token: None,
            clusters: HashMap::new(),
            token_cache: HashMap::new(),
            vnet_peerings: HashMap::new(),
            attestations: HashMap::new(),
        }
    }
    
    // =========================================================================
    // Step 1: Initialize Authentication
    // =========================================================================
    
    /// Initialize with Managed Identity (preferred for Azure VMs/AKS)
    /// 
    /// Managed Identity allows Azure resources to authenticate to Azure AD
    /// without managing credentials.
    pub fn initialize_managed_identity(
        &mut self,
        user_assigned_client_id: Option<String>,
    ) -> Result<(), AzureError> {
        // Step 1a: Determine identity type
        let identity = match user_assigned_client_id {
            Some(client_id) => AzureIdentity::UserAssigned { client_id },
            None => AzureIdentity::SystemAssigned,
        };
        
        // Step 1b: Get token from IMDS (Instance Metadata Service)
        // In real implementation:
        // let token = imds_client.get_token("https://management.azure.com/").await?;
        
        let token = AzureAccessToken {
            access_token: format!("eyJ0eXAi{}", generate_random_id()),
            token_type: "Bearer".to_string(),
            expires_at: SystemTime::now() + TOKEN_LIFETIME,
            resource: "https://management.azure.com/".to_string(),
        };
        
        self.local_identity = Some(identity);
        self.local_token = Some(token);
        
        Ok(())
    }
    
    /// Initialize with service principal
    pub fn initialize_service_principal(
        &mut self,
        client_id: String,
        client_secret: String,
    ) -> Result<(), AzureError> {
        // Step 1c: Get token using client credentials flow
        // In real implementation:
        // let token = azure_ad_client.get_token_client_credentials(
        //     &self.local_tenant_id,
        //     &client_id,
        //     &client_secret,
        //     "https://management.azure.com/.default",
        // ).await?;
        
        let token = AzureAccessToken {
            access_token: format!("eyJ0eXAi{}", generate_random_id()),
            token_type: "Bearer".to_string(),
            expires_at: SystemTime::now() + TOKEN_LIFETIME,
            resource: "https://management.azure.com/".to_string(),
        };
        
        self.local_identity = Some(AzureIdentity::ServicePrincipal {
            client_id,
            client_secret,
        });
        self.local_token = Some(token);
        
        Ok(())
    }
    
    // =========================================================================
    // Step 2: Register Remote Cluster
    // =========================================================================
    
    /// Register a remote Azure cluster for federation
    pub fn register_cluster(&mut self, config: AzureClusterConfig) -> Result<(), AzureError> {
        // Step 2a: Validate configuration
        self.validate_cluster_config(&config)?;
        
        // Step 2b: Test cross-tenant authentication
        let test_token = self.get_cross_tenant_token(
            &config.tenant_id,
            "https://management.azure.com/",
        )?;
        
        // Step 2c: Verify we can reach the cluster
        // In real implementation: self.test_connectivity(&config, &test_token)?;
        
        // Step 2d: Store configuration
        self.clusters.insert(config.cluster_id.clone(), config);
        
        Ok(())
    }
    
    fn validate_cluster_config(&self, config: &AzureClusterConfig) -> Result<(), AzureError> {
        // Validate subscription ID format (GUID)
        if !is_valid_guid(&config.subscription_id) {
            return Err(AzureError::InvalidSubscriptionId {
                subscription_id: config.subscription_id.clone(),
            });
        }
        
        // Validate tenant ID format (GUID)
        if !is_valid_guid(&config.tenant_id) {
            return Err(AzureError::InvalidTenantId {
                tenant_id: config.tenant_id.clone(),
            });
        }
        
        // Validate region
        if !is_valid_region(&config.region) {
            return Err(AzureError::InvalidRegion {
                region: config.region.clone(),
            });
        }
        
        Ok(())
    }
    
    // =========================================================================
    // Step 3: Cross-Tenant Authentication
    // =========================================================================
    
    /// Get token for accessing resources in another tenant
    /// 
    /// This uses Azure AD multi-tenant app registration.
    pub fn get_cross_tenant_token(
        &self,
        target_tenant: &str,
        resource: &str,
    ) -> Result<AzureAccessToken, AzureError> {
        let _local_token = self.local_token.as_ref()
            .ok_or(AzureError::NotInitialized)?;
        
        // Step 3a: Get token for target tenant
        // In real implementation, this requires:
        // 1. Multi-tenant app registration
        // 2. Admin consent in target tenant
        // 3. OAuth2 client credentials flow to target tenant
        
        // let token = azure_ad_client.get_token_client_credentials(
        //     target_tenant,
        //     &client_id,
        //     &client_secret,
        //     &format!("{}/.default", resource),
        // ).await?;
        
        Ok(AzureAccessToken {
            access_token: format!("eyJ0eXAi.cross.{}", generate_random_id()),
            token_type: "Bearer".to_string(),
            expires_at: SystemTime::now() + TOKEN_LIFETIME,
            resource: resource.to_string(),
        })
    }
    
    /// Get cached token for a cluster, refreshing if needed
    pub fn get_token(&mut self, cluster_id: &ClusterId) -> Result<&AzureAccessToken, AzureError> {
        let config = self.clusters.get(cluster_id)
            .ok_or_else(|| AzureError::ClusterNotFound {
                cluster_id: cluster_id.clone(),
            })?;
        
        let cache_key = format!("{}:{}", config.tenant_id, config.subscription_id);
        
        // Check cache
        if let Some(token) = self.token_cache.get(&cache_key) {
            if token.is_valid() && !token.needs_refresh() {
                return Ok(self.token_cache.get(&cache_key).unwrap());
            }
        }
        
        // Refresh token
        let tenant_id = config.tenant_id.clone();
        let new_token = self.get_cross_tenant_token(
            &tenant_id,
            "https://management.azure.com/",
        )?;
        
        self.token_cache.insert(cache_key.clone(), new_token);
        Ok(self.token_cache.get(&cache_key).unwrap())
    }
    
    // =========================================================================
    // Step 4: VNet Peering
    // =========================================================================
    
    /// Create VNet peering with a remote cluster
    pub fn create_vnet_peering(
        &mut self,
        remote_cluster_id: &ClusterId,
    ) -> Result<VNetPeeringConfig, AzureError> {
        let remote_config = self.clusters.get(remote_cluster_id)
            .ok_or_else(|| AzureError::ClusterNotFound {
                cluster_id: remote_cluster_id.clone(),
            })?;
        
        // Step 4a: Build VNet resource IDs
        let local_vnet_id = format!(
            "/subscriptions/{}/resourceGroups/grey-rg/providers/Microsoft.Network/virtualNetworks/grey-vnet",
            self.local_subscription
        );
        
        let remote_vnet_id = format!(
            "/subscriptions/{}/resourceGroups/{}/providers/Microsoft.Network/virtualNetworks/{}",
            remote_config.subscription_id,
            remote_config.resource_group,
            remote_config.vnet_name
        );
        
        // Step 4b: Create peering
        // In real implementation:
        // let arm = ArmClient::new(&token);
        // arm.create_vnet_peering(&local_vnet_id, &remote_vnet_id, &peering_config).await?;
        
        let peering = VNetPeeringConfig {
            name: format!("grey-peering-{}", remote_cluster_id),
            local_vnet_id,
            remote_vnet_id,
            state: PeeringState::Initiated,
            allow_virtual_network_access: true,
            allow_forwarded_traffic: true,
            allow_gateway_transit: false,
            use_remote_gateways: false,
        };
        
        self.vnet_peerings.insert(remote_cluster_id.clone(), peering.clone());
        
        Ok(peering)
    }
    
    /// Accept VNet peering from a remote cluster
    pub fn accept_vnet_peering(
        &mut self,
        remote_cluster_id: &ClusterId,
    ) -> Result<(), AzureError> {
        // In Azure, both sides must create the peering
        // Once both exist, they become Connected
        
        if let Some(peering) = self.vnet_peerings.get_mut(remote_cluster_id) {
            // Step 4c: Create reciprocal peering on remote side
            // arm.create_vnet_peering(&remote_vnet_id, &local_vnet_id, &peering_config).await?;
            
            peering.state = PeeringState::Connected;
            Ok(())
        } else {
            Err(AzureError::PeeringNotFound {
                cluster_id: remote_cluster_id.clone(),
            })
        }
    }
    
    // =========================================================================
    // Step 5: Attestation Exchange
    // =========================================================================
    
    /// Generate attestation for this cluster
    pub fn generate_attestation(&self) -> Result<AzureAttestation, AzureError> {
        let _local_token = self.local_token.as_ref()
            .ok_or(AzureError::NotInitialized)?;
        
        // Step 5a: Get VM/VMSS instance metadata
        // In real implementation:
        // let imds = ImdsClient::new();
        // let metadata = imds.get_instance_metadata().await?;
        
        // Step 5b: Get AKS cluster info (if on AKS)
        // let aks = AksClient::new(&token);
        // let cluster_info = aks.get_cluster_info().await?;
        
        let attestation = AzureAttestation {
            cluster_id: self.local_cluster_id.clone(),
            tenant_id: self.local_tenant_id.clone(),
            subscription_id: self.local_subscription.clone(),
            region: self.local_region.clone(),
            vm_info: VmInfo {
                vm_id: format!("vm-{}", generate_random_id()),
                vm_size: "Standard_D8s_v3".to_string(),
                zone: Some("1".to_string()),
                platform_fault_domain: 0,
            },
            aks_info: Some(AksInfo {
                cluster_name: format!("grey-{}", self.local_cluster_id),
                resource_id: format!(
                    "/subscriptions/{}/resourceGroups/grey-rg/providers/Microsoft.ContainerService/managedClusters/grey-aks",
                    self.local_subscription
                ),
                kubernetes_version: "1.28.0".to_string(),
                node_resource_group: format!("MC_grey-rg_grey-aks_{}", self.local_region),
            }),
            network_profile: NetworkProfile {
                vnet_id: format!(
                    "/subscriptions/{}/resourceGroups/grey-rg/providers/Microsoft.Network/virtualNetworks/grey-vnet",
                    self.local_subscription
                ),
                subnet_id: format!(
                    "/subscriptions/{}/resourceGroups/grey-rg/providers/Microsoft.Network/virtualNetworks/grey-vnet/subnets/default",
                    self.local_subscription
                ),
                private_ip: "10.0.0.100".to_string(),
            },
            key_vault_url: None,
            created_at: SystemTime::now(),
            expires_at: SystemTime::now() + Duration::from_secs(3600),
            signature: vec![0u8; 64],
        };
        
        Ok(attestation)
    }
    
    /// Verify attestation from a remote Azure cluster
    pub fn verify_attestation(
        &mut self,
        attestation: &AzureAttestation,
    ) -> Result<AttestationVerification, AzureError> {
        // Step 5c: Validate cluster is registered
        let config = self.clusters.get(&attestation.cluster_id)
            .ok_or_else(|| AzureError::ClusterNotFound {
                cluster_id: attestation.cluster_id.clone(),
            })?;
        
        // Step 5d: Verify tenant ID matches
        if config.tenant_id != attestation.tenant_id {
            return Err(AzureError::AttestationMismatch {
                field: "tenant_id".to_string(),
                expected: config.tenant_id.clone(),
                actual: attestation.tenant_id.clone(),
            });
        }
        
        // Step 5e: Verify subscription ID matches
        if config.subscription_id != attestation.subscription_id {
            return Err(AzureError::AttestationMismatch {
                field: "subscription_id".to_string(),
                expected: config.subscription_id.clone(),
                actual: attestation.subscription_id.clone(),
            });
        }
        
        // Step 5f: Verify signature
        // In real implementation, verify with Azure Attestation service
        
        // Step 5g: Cache attestation
        self.attestations.insert(attestation.cluster_id.clone(), attestation.clone());
        
        Ok(AttestationVerification {
            cluster_id: attestation.cluster_id.clone(),
            verified: true,
            warnings: vec![],
            verified_at: SystemTime::now(),
        })
    }
    
    // =========================================================================
    // Step 6: Encryption with Key Vault
    // =========================================================================
    
    /// Encrypt data for a remote cluster using their Key Vault
    pub fn encrypt_for_cluster(
        &mut self,
        cluster_id: &ClusterId,
        plaintext: &[u8],
    ) -> Result<EncryptedPayload, AzureError> {
        let config = self.clusters.get(cluster_id)
            .ok_or_else(|| AzureError::ClusterNotFound {
                cluster_id: cluster_id.clone(),
            })?;
        
        let key_vault_url = config.key_vault_url.as_ref()
            .ok_or(AzureError::NoKeyVault {
                cluster_id: cluster_id.clone(),
            })?;
        
        // Step 6a: Get token with Key Vault scope
        // let token = self.get_cross_tenant_token(
        //     &config.tenant_id,
        //     "https://vault.azure.net",
        // )?;
        
        // Step 6b: Encrypt using Key Vault
        // In real implementation:
        // let kv = KeyVaultClient::new(key_vault_url, &token);
        // let ciphertext = kv.encrypt("grey-federation-key", plaintext).await?;
        
        Ok(EncryptedPayload {
            key_vault_url: key_vault_url.clone(),
            key_name: "grey-federation-key".to_string(),
            key_version: "latest".to_string(),
            ciphertext: vec![0u8; plaintext.len() + 32],
            algorithm: "RSA-OAEP-256".to_string(),
        })
    }
    
    // =========================================================================
    // Step 7: API Communication
    // =========================================================================
    
    /// Send request to a remote cluster's API
    pub async fn send_request(
        &mut self,
        cluster_id: &ClusterId,
        request: FederationRequest,
    ) -> Result<FederationResponse, AzureError> {
        let config = self.clusters.get(cluster_id)
            .ok_or_else(|| AzureError::ClusterNotFound {
                cluster_id: cluster_id.clone(),
            })?;
        
        // Step 7a: Get fresh token
        let _token = self.get_token(cluster_id)?;
        
        // Step 7b: Send request with Bearer token
        // In real implementation:
        // let response = http_client
        //     .post(&config.api_endpoint)
        //     .bearer_auth(&token.access_token)
        //     .json(&request)
        //     .send()
        //     .await?;
        
        Ok(FederationResponse {
            request_id: request.request_id,
            status: ResponseStatus::Success,
            payload: vec![],
        })
    }
    
    // =========================================================================
    // Step 8: Health Monitoring
    // =========================================================================
    
    /// Check health of all registered clusters
    pub fn health_check(&self) -> Vec<ClusterHealth> {
        self.clusters.keys()
            .map(|cluster_id| {
                let config = self.clusters.get(cluster_id).unwrap();
                let attestation = self.attestations.get(cluster_id);
                let peering = self.vnet_peerings.get(cluster_id);
                
                let cache_key = format!("{}:{}", config.tenant_id, config.subscription_id);
                
                ClusterHealth {
                    cluster_id: cluster_id.clone(),
                    tenant_id: config.tenant_id.clone(),
                    subscription_id: config.subscription_id.clone(),
                    region: config.region.clone(),
                    connectivity: peering.map(|p| p.state == PeeringState::Connected).unwrap_or(false),
                    attestation_valid: attestation
                        .map(|a| a.expires_at > SystemTime::now())
                        .unwrap_or(false),
                    token_valid: self.token_cache.get(&cache_key)
                        .map(|t| t.is_valid())
                        .unwrap_or(false),
                    last_contact: attestation.map(|a| a.created_at),
                }
            })
            .collect()
    }
}

// =============================================================================
// Azure-Specific Types
// =============================================================================

/// Azure-specific attestation
#[derive(Debug, Clone)]
pub struct AzureAttestation {
    pub cluster_id: ClusterId,
    pub tenant_id: String,
    pub subscription_id: SubscriptionId,
    pub region: AzureRegion,
    pub vm_info: VmInfo,
    pub aks_info: Option<AksInfo>,
    pub network_profile: NetworkProfile,
    pub key_vault_url: Option<String>,
    pub created_at: SystemTime,
    pub expires_at: SystemTime,
    pub signature: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct VmInfo {
    pub vm_id: String,
    pub vm_size: String,
    pub zone: Option<String>,
    pub platform_fault_domain: u32,
}

#[derive(Debug, Clone)]
pub struct AksInfo {
    pub cluster_name: String,
    pub resource_id: String,
    pub kubernetes_version: String,
    pub node_resource_group: String,
}

#[derive(Debug, Clone)]
pub struct NetworkProfile {
    pub vnet_id: String,
    pub subnet_id: String,
    pub private_ip: String,
}

#[derive(Debug, Clone)]
pub struct AttestationVerification {
    pub cluster_id: ClusterId,
    pub verified: bool,
    pub warnings: Vec<String>,
    pub verified_at: SystemTime,
}

#[derive(Debug, Clone)]
pub struct EncryptedPayload {
    pub key_vault_url: String,
    pub key_name: String,
    pub key_version: String,
    pub ciphertext: Vec<u8>,
    pub algorithm: String,
}

#[derive(Debug, Clone)]
pub struct FederationRequest {
    pub request_id: String,
    pub request_type: RequestType,
    pub payload: Vec<u8>,
}

#[derive(Debug, Clone)]
pub enum RequestType {
    Heartbeat,
    ResourceQuery,
    TaskSubmission,
    AttestationExchange,
}

#[derive(Debug, Clone)]
pub struct FederationResponse {
    pub request_id: String,
    pub status: ResponseStatus,
    pub payload: Vec<u8>,
}

#[derive(Debug, Clone)]
pub enum ResponseStatus {
    Success,
    Error(String),
    Retry,
}

#[derive(Debug, Clone)]
pub struct ClusterHealth {
    pub cluster_id: ClusterId,
    pub tenant_id: String,
    pub subscription_id: SubscriptionId,
    pub region: AzureRegion,
    pub connectivity: bool,
    pub attestation_valid: bool,
    pub token_valid: bool,
    pub last_contact: Option<SystemTime>,
}

// =============================================================================
// Error Types
// =============================================================================

#[derive(Debug, Clone)]
pub enum AzureError {
    NotInitialized,
    TokenExpired,
    InvalidSubscriptionId { subscription_id: String },
    InvalidTenantId { tenant_id: String },
    InvalidRegion { region: String },
    ClusterNotFound { cluster_id: ClusterId },
    PeeringNotFound { cluster_id: ClusterId },
    NoKeyVault { cluster_id: ClusterId },
    AttestationMismatch { field: String, expected: String, actual: String },
    ApiError { code: String, message: String },
    NetworkError { message: String },
}

// =============================================================================
// Helpers
// =============================================================================

fn is_valid_guid(guid: &str) -> bool {
    // GUID format: 8-4-4-4-12 hex digits
    if guid.len() != 36 {
        return false;
    }
    
    let parts: Vec<&str> = guid.split('-').collect();
    if parts.len() != 5 {
        return false;
    }
    
    parts[0].len() == 8
        && parts[1].len() == 4
        && parts[2].len() == 4
        && parts[3].len() == 4
        && parts[4].len() == 12
        && guid.chars().filter(|c| *c != '-').all(|c| c.is_ascii_hexdigit())
}

fn is_valid_region(region: &str) -> bool {
    let valid_regions = [
        "eastus", "eastus2", "westus", "westus2", "westus3",
        "centralus", "northcentralus", "southcentralus", "westcentralus",
        "canadacentral", "canadaeast",
        "brazilsouth", "brazilsoutheast",
        "northeurope", "westeurope", "uksouth", "ukwest",
        "francecentral", "francesouth",
        "germanywestcentral", "germanynorth",
        "switzerlandnorth", "switzerlandwest",
        "norwayeast", "norwaywest",
        "swedencentral",
        "australiaeast", "australiasoutheast", "australiacentral",
        "eastasia", "southeastasia",
        "japaneast", "japanwest",
        "koreacentral", "koreasouth",
        "centralindia", "southindia", "westindia",
        "uaenorth", "uaecentral",
        "southafricanorth", "southafricawest",
    ];
    valid_regions.contains(&region)
}

fn generate_random_id() -> String {
    use std::time::UNIX_EPOCH;
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    format!("{:016x}", timestamp)
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_token_valid() {
        let valid = AzureAccessToken {
            access_token: "eyJ0eXAi...".to_string(),
            token_type: "Bearer".to_string(),
            expires_at: SystemTime::now() + Duration::from_secs(3600),
            resource: "https://management.azure.com/".to_string(),
        };
        assert!(valid.is_valid());
        assert!(!valid.needs_refresh());
    }
    
    #[test]
    fn test_guid_validation() {
        assert!(is_valid_guid("12345678-1234-1234-1234-123456789abc"));
        assert!(!is_valid_guid("not-a-guid"));
        assert!(!is_valid_guid("12345678123412341234123456789abc")); // No dashes
    }
    
    #[test]
    fn test_region_validation() {
        assert!(is_valid_region("eastus"));
        assert!(is_valid_region("westeurope"));
        assert!(!is_valid_region("invalid-region"));
    }
}
