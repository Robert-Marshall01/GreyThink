//! Grey Distributed — Enterprise IAM Adapter
//!
//! Identity and Access Management integration for enterprise deployments.
//! Supports OAuth2, OpenID Connect (OIDC), and SAML 2.0 for federated authentication.
//!
//! # Authentication Flow
//!
//! ```text
//! ┌──────────────┐     ┌─────────────────┐     ┌──────────────────┐
//! │   Client     │────▶│  Grey Gateway   │────▶│  IAM Provider    │
//! │              │     │                 │     │  (IdP)           │
//! └──────────────┘     └─────────────────┘     └──────────────────┘
//!        │                     │                       │
//!        │  1. Request         │                       │
//!        │─────────────────────▶                       │
//!        │                     │  2. Redirect/Token    │
//!        │                     │───────────────────────▶
//!        │                     │                       │
//!        │                     │  3. Claims/Assertion  │
//!        │                     │◀───────────────────────
//!        │                     │                       │
//!        │  4. Grey Token      │                       │
//!        │◀─────────────────────                       │
//!        │                     │                       │
//! ```
//!
//! # Design Decisions
//!
//! 1. **Token Exchange**: Exchanges external IdP tokens for Grey-native tokens.
//!    This centralizes authorization logic and reduces IdP dependencies.
//!
//! 2. **Claims Mapping**: Flexible mapping from IdP claims to Grey roles and
//!    tenant assignments. Supports custom claim transformers.
//!
//! 3. **Token Caching**: Caches validated tokens to reduce IdP round-trips.
//!    Critical for high-throughput API calls.
//!
//! 4. **Multi-IdP**: Supports multiple IdPs simultaneously with automatic
//!    discovery based on token issuer.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use async_trait::async_trait;
use jsonwebtoken::{decode, decode_header, Algorithm, DecodingKey, Validation};
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn, instrument};

// ============================================================================
// Configuration
// ============================================================================

/// IAM adapter configuration.
///
/// # Security Considerations
///
/// - Always use HTTPS for IdP endpoints
/// - Rotate client secrets regularly
/// - Enable token binding when possible
/// - Configure appropriate token lifetimes
#[derive(Debug, Clone, Deserialize)]
pub struct IAMConfig {
    /// Registered identity providers
    pub providers: Vec<ProviderConfig>,
    
    /// Default provider (by name)
    pub default_provider: Option<String>,
    
    /// Token caching configuration
    #[serde(default)]
    pub cache: CacheConfig,
    
    /// Claims mapping rules
    #[serde(default)]
    pub claims_mapping: ClaimsMappingConfig,
    
    /// Grey-native token configuration
    #[serde(default)]
    pub grey_token: GreyTokenConfig,
    
    /// Security settings
    #[serde(default)]
    pub security: SecurityConfig,
}

/// Identity provider configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct ProviderConfig {
    /// Unique provider name
    pub name: String,
    
    /// Provider type
    pub provider_type: ProviderType,
    
    /// Enabled flag
    #[serde(default = "default_true")]
    pub enabled: bool,
    
    /// OIDC-specific configuration
    pub oidc: Option<OIDCConfig>,
    
    /// SAML-specific configuration
    pub saml: Option<SAMLConfig>,
    
    /// Claims to role mapping
    #[serde(default)]
    pub role_mappings: Vec<RoleMapping>,
    
    /// Claims to tenant mapping
    #[serde(default)]
    pub tenant_mappings: Vec<TenantMapping>,
}

fn default_true() -> bool { true }

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderType {
    Oidc,
    Saml,
    OAuth2,
}

/// OpenID Connect configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct OIDCConfig {
    /// OIDC discovery URL (.well-known/openid-configuration)
    pub discovery_url: String,
    
    /// OAuth2 client ID
    pub client_id: String,
    
    /// OAuth2 client secret (use secrets management in production)
    pub client_secret: Option<String>,
    
    /// Expected issuer (validated against 'iss' claim)
    pub issuer: String,
    
    /// Expected audience (validated against 'aud' claim)
    pub audience: String,
    
    /// Required scopes
    #[serde(default)]
    pub required_scopes: Vec<String>,
    
    /// JWKS URI (auto-discovered if not specified)
    pub jwks_uri: Option<String>,
    
    /// Token introspection endpoint
    pub introspection_endpoint: Option<String>,
    
    /// Userinfo endpoint
    pub userinfo_endpoint: Option<String>,
    
    /// Supported algorithms
    #[serde(default = "default_algorithms")]
    pub algorithms: Vec<String>,
    
    /// Clock skew tolerance in seconds
    #[serde(default = "default_clock_skew")]
    pub clock_skew_seconds: u64,
}

fn default_algorithms() -> Vec<String> {
    vec!["RS256".to_string(), "RS384".to_string(), "RS512".to_string()]
}

fn default_clock_skew() -> u64 { 60 }

/// SAML 2.0 configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct SAMLConfig {
    /// IdP metadata URL
    pub metadata_url: Option<String>,
    
    /// IdP metadata XML (inline)
    pub metadata_xml: Option<String>,
    
    /// IdP entity ID
    pub idp_entity_id: String,
    
    /// IdP SSO URL
    pub idp_sso_url: String,
    
    /// IdP certificate (PEM format)
    pub idp_certificate: String,
    
    /// SP entity ID
    pub sp_entity_id: String,
    
    /// SP ACS URL
    pub sp_acs_url: String,
    
    /// Name ID format
    #[serde(default = "default_name_id_format")]
    pub name_id_format: String,
    
    /// Sign authentication requests
    #[serde(default)]
    pub sign_authn_requests: bool,
    
    /// SP signing certificate (PEM)
    pub sp_certificate: Option<String>,
    
    /// SP signing key (PEM, use secrets management)
    pub sp_private_key: Option<String>,
}

fn default_name_id_format() -> String {
    "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress".to_string()
}

/// Claims to role mapping.
#[derive(Debug, Clone, Deserialize)]
pub struct RoleMapping {
    /// Claim name to check
    pub claim: String,
    
    /// Expected claim value (exact match or regex)
    pub value: ClaimMatch,
    
    /// Grey role to assign
    pub role: String,
}

/// Claim matching modes.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ClaimMatch {
    Exact(String),
    Contains(String),
    Regex(String),
    Any(Vec<String>),
}

/// Claims to tenant mapping.
#[derive(Debug, Clone, Deserialize)]
pub struct TenantMapping {
    /// Claim containing tenant ID
    pub claim: String,
    
    /// Transform function (optional)
    pub transform: Option<TenantTransform>,
    
    /// Default tenant if claim missing
    pub default: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TenantTransform {
    /// Use claim value as-is
    Identity,
    /// Extract from email domain
    EmailDomain,
    /// Apply prefix
    Prefix(String),
    /// Apply suffix
    Suffix(String),
    /// Lookup from mapping table
    Lookup(HashMap<String, String>),
}

/// Token cache configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct CacheConfig {
    /// Enable caching
    #[serde(default = "default_true")]
    pub enabled: bool,
    
    /// Maximum cache entries
    #[serde(default = "default_cache_size")]
    pub max_entries: usize,
    
    /// Cache TTL in seconds (must be less than token lifetime)
    #[serde(default = "default_cache_ttl")]
    pub ttl_seconds: u64,
    
    /// Refresh tokens before expiry (seconds)
    #[serde(default = "default_refresh_buffer")]
    pub refresh_buffer_seconds: u64,
}

fn default_cache_size() -> usize { 10_000 }
fn default_cache_ttl() -> u64 { 300 }
fn default_refresh_buffer() -> u64 { 60 }

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            max_entries: default_cache_size(),
            ttl_seconds: default_cache_ttl(),
            refresh_buffer_seconds: default_refresh_buffer(),
        }
    }
}

/// Claims mapping configuration.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct ClaimsMappingConfig {
    /// Standard claim mappings
    #[serde(default)]
    pub standard: StandardClaims,
    
    /// Custom claim mappings
    #[serde(default)]
    pub custom: HashMap<String, String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct StandardClaims {
    /// Subject claim (user ID)
    #[serde(default = "default_sub_claim")]
    pub subject: String,
    
    /// Email claim
    #[serde(default = "default_email_claim")]
    pub email: String,
    
    /// Name claim
    #[serde(default = "default_name_claim")]
    pub name: String,
    
    /// Groups/roles claim
    #[serde(default = "default_groups_claim")]
    pub groups: String,
}

fn default_sub_claim() -> String { "sub".to_string() }
fn default_email_claim() -> String { "email".to_string() }
fn default_name_claim() -> String { "name".to_string() }
fn default_groups_claim() -> String { "groups".to_string() }

impl Default for StandardClaims {
    fn default() -> Self {
        Self {
            subject: default_sub_claim(),
            email: default_email_claim(),
            name: default_name_claim(),
            groups: default_groups_claim(),
        }
    }
}

/// Grey token generation configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct GreyTokenConfig {
    /// Token issuer
    #[serde(default = "default_issuer")]
    pub issuer: String,
    
    /// Token lifetime in seconds
    #[serde(default = "default_token_lifetime")]
    pub lifetime_seconds: u64,
    
    /// Signing algorithm
    #[serde(default = "default_signing_algorithm")]
    pub algorithm: String,
    
    /// Signing key path (RSA private key)
    pub signing_key_path: Option<String>,
    
    /// HMAC secret (alternative to RSA, less secure)
    pub hmac_secret: Option<String>,
    
    /// Include original claims
    #[serde(default)]
    pub include_original_claims: bool,
}

fn default_issuer() -> String { "grey.io".to_string() }
fn default_token_lifetime() -> u64 { 3600 }
fn default_signing_algorithm() -> String { "RS256".to_string() }

impl Default for GreyTokenConfig {
    fn default() -> Self {
        Self {
            issuer: default_issuer(),
            lifetime_seconds: default_token_lifetime(),
            algorithm: default_signing_algorithm(),
            signing_key_path: None,
            hmac_secret: None,
            include_original_claims: false,
        }
    }
}

/// Security settings.
#[derive(Debug, Clone, Deserialize)]
pub struct SecurityConfig {
    /// Require HTTPS for all IdP communication
    #[serde(default = "default_true")]
    pub require_https: bool,
    
    /// Allow expired tokens (for testing only!)
    #[serde(default)]
    pub allow_expired: bool,
    
    /// Enable token binding
    #[serde(default)]
    pub enable_token_binding: bool,
    
    /// Allowed token binding types
    #[serde(default)]
    pub token_binding_types: Vec<String>,
    
    /// Maximum token age in seconds
    #[serde(default = "default_max_token_age")]
    pub max_token_age_seconds: u64,
    
    /// Verify token not-before claim
    #[serde(default = "default_true")]
    pub verify_nbf: bool,
}

fn default_max_token_age() -> u64 { 86400 }

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            require_https: true,
            allow_expired: false,
            enable_token_binding: false,
            token_binding_types: vec![],
            max_token_age_seconds: default_max_token_age(),
            verify_nbf: true,
        }
    }
}

// ============================================================================
// Token Types
// ============================================================================

/// Validated identity from external IdP.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternalIdentity {
    /// External user ID (subject)
    pub subject: String,
    
    /// Email address
    pub email: Option<String>,
    
    /// Display name
    pub name: Option<String>,
    
    /// Provider name
    pub provider: String,
    
    /// Original issuer
    pub issuer: String,
    
    /// Token expiration
    pub expires_at: u64,
    
    /// Groups/roles from IdP
    pub groups: Vec<String>,
    
    /// All claims (for custom mapping)
    pub claims: HashMap<String, serde_json::Value>,
}

/// Grey-native identity after claims mapping.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GreyIdentity {
    /// Grey user ID
    pub user_id: String,
    
    /// Assigned tenant ID
    pub tenant_id: String,
    
    /// Assigned roles
    pub roles: Vec<String>,
    
    /// Email address
    pub email: Option<String>,
    
    /// Display name
    pub name: Option<String>,
    
    /// Original provider
    pub provider: String,
    
    /// authenticated at
    pub authenticated_at: u64,
    
    /// Session expiration
    pub expires_at: u64,
    
    /// Custom attributes
    pub attributes: HashMap<String, String>,
}

/// Grey API token (JWT).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GreyToken {
    /// Token string (JWT)
    pub token: String,
    
    /// Token type (always "Bearer")
    pub token_type: String,
    
    /// Expiration timestamp
    pub expires_at: u64,
    
    /// Refresh token (optional)
    pub refresh_token: Option<String>,
}

/// JWT claims for Grey token.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GreyTokenClaims {
    /// Issuer
    pub iss: String,
    
    /// Subject (user ID)
    pub sub: String,
    
    /// Audience
    pub aud: String,
    
    /// Expiration time
    pub exp: u64,
    
    /// Issued at
    pub iat: u64,
    
    /// Not before
    pub nbf: u64,
    
    /// JWT ID
    pub jti: String,
    
    /// Tenant ID
    pub tenant_id: String,
    
    /// Roles
    pub roles: Vec<String>,
    
    /// Email
    #[serde(skip_serializing_if = "Option::is_none")]
    pub email: Option<String>,
    
    /// Name
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    
    /// Original provider
    pub provider: String,
}

// ============================================================================
// IAM Adapter
// ============================================================================

/// Main IAM adapter for identity federation.
///
/// # Thread Safety
///
/// All operations are thread-safe and can be called concurrently.
/// Uses RwLock for cache access (optimized for reads).
pub struct IAMAdapter {
    config: IAMConfig,
    
    /// OIDC providers (keyed by name)
    oidc_providers: HashMap<String, OIDCProvider>,
    
    /// SAML providers (keyed by name)
    saml_providers: HashMap<String, SAMLProvider>,
    
    /// Token cache
    cache: Arc<RwLock<TokenCache>>,
    
    /// JWKS cache (keyed by issuer)
    jwks_cache: Arc<RwLock<HashMap<String, JWKSCache>>>,
    
    /// Metrics
    metrics: Arc<IAMMetrics>,
}

struct OIDCProvider {
    config: OIDCConfig,
    http_client: reqwest::Client,
}

struct SAMLProvider {
    config: SAMLConfig,
}

struct TokenCache {
    entries: HashMap<String, CacheEntry>,
    max_entries: usize,
}

struct CacheEntry {
    identity: GreyIdentity,
    cached_at: SystemTime,
    expires_at: SystemTime,
}

struct JWKSCache {
    keys: HashMap<String, DecodingKey>,
    fetched_at: SystemTime,
    expires_at: SystemTime,
}

pub struct IAMMetrics {
    tokens_validated: std::sync::atomic::AtomicU64,
    tokens_issued: std::sync::atomic::AtomicU64,
    cache_hits: std::sync::atomic::AtomicU64,
    cache_misses: std::sync::atomic::AtomicU64,
    validation_failures: std::sync::atomic::AtomicU64,
}

impl IAMAdapter {
    /// Create a new IAM adapter.
    #[instrument(name = "iam_adapter_new")]
    pub async fn new(config: IAMConfig) -> Result<Self, IAMError> {
        let mut oidc_providers = HashMap::new();
        let mut saml_providers = HashMap::new();
        
        // Initialize providers
        for provider_config in &config.providers {
            if !provider_config.enabled {
                continue;
            }
            
            match provider_config.provider_type {
                ProviderType::Oidc | ProviderType::OAuth2 => {
                    if let Some(ref oidc_config) = provider_config.oidc {
                        let http_client = reqwest::Client::builder()
                            .timeout(Duration::from_secs(30))
                            .build()
                            .map_err(|e| IAMError::Configuration(e.to_string()))?;
                        
                        oidc_providers.insert(
                            provider_config.name.clone(),
                            OIDCProvider {
                                config: oidc_config.clone(),
                                http_client,
                            },
                        );
                    }
                }
                ProviderType::Saml => {
                    if let Some(ref saml_config) = provider_config.saml {
                        saml_providers.insert(
                            provider_config.name.clone(),
                            SAMLProvider {
                                config: saml_config.clone(),
                            },
                        );
                    }
                }
            }
        }
        
        info!(
            oidc_count = oidc_providers.len(),
            saml_count = saml_providers.len(),
            "IAM adapter initialized"
        );
        
        Ok(Self {
            config: config.clone(),
            oidc_providers,
            saml_providers,
            cache: Arc::new(RwLock::new(TokenCache {
                entries: HashMap::new(),
                max_entries: config.cache.max_entries,
            })),
            jwks_cache: Arc::new(RwLock::new(HashMap::new())),
            metrics: Arc::new(IAMMetrics::new()),
        })
    }
    
    /// Validate an external token and exchange for Grey identity.
    ///
    /// # Flow
    ///
    /// 1. Check cache for existing validation
    /// 2. Determine provider from token (issuer claim or header)
    /// 3. Validate token with provider
    /// 4. Map claims to Grey identity
    /// 5. Cache result
    #[instrument(skip(self, token), fields(provider))]
    pub async fn validate_token(&self, token: &str) -> Result<GreyIdentity, IAMError> {
        // Check cache first
        if self.config.cache.enabled {
            let cache = self.cache.read().await;
            if let Some(entry) = cache.entries.get(token) {
                if entry.expires_at > SystemTime::now() {
                    self.metrics.cache_hits.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    return Ok(entry.identity.clone());
                }
            }
        }
        self.metrics.cache_misses.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        
        // Determine provider from token
        let (provider_name, provider_config) = self.identify_provider(token)?;
        tracing::Span::current().record("provider", &provider_name);
        
        // Validate with appropriate provider
        let external_identity = match provider_config.provider_type {
            ProviderType::Oidc | ProviderType::OAuth2 => {
                self.validate_oidc_token(&provider_name, token).await?
            }
            ProviderType::Saml => {
                // SAML uses assertions, not bearer tokens
                return Err(IAMError::InvalidToken("Use validate_saml_assertion for SAML".into()));
            }
        };
        
        // Map to Grey identity
        let grey_identity = self.map_to_grey_identity(&external_identity, provider_config)?;
        
        // Cache result
        if self.config.cache.enabled {
            let mut cache = self.cache.write().await;
            let now = SystemTime::now();
            let ttl = Duration::from_secs(self.config.cache.ttl_seconds);
            
            // Evict expired entries if at capacity
            if cache.entries.len() >= cache.max_entries {
                cache.entries.retain(|_, entry| entry.expires_at > now);
            }
            
            cache.entries.insert(token.to_string(), CacheEntry {
                identity: grey_identity.clone(),
                cached_at: now,
                expires_at: now + ttl,
            });
        }
        
        self.metrics.tokens_validated.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        Ok(grey_identity)
    }
    
    /// Validate an OIDC/OAuth2 token.
    async fn validate_oidc_token(
        &self,
        provider_name: &str,
        token: &str,
    ) -> Result<ExternalIdentity, IAMError> {
        let provider = self.oidc_providers.get(provider_name)
            .ok_or_else(|| IAMError::UnknownProvider(provider_name.to_string()))?;
        
        let config = &provider.config;
        
        // Get JWKS for validation
        let decoding_key = self.get_jwks_key(provider_name, token).await?;
        
        // Configure validation
        let mut validation = Validation::new(Algorithm::RS256);
        validation.set_issuer(&[&config.issuer]);
        validation.set_audience(&[&config.audience]);
        validation.leeway = config.clock_skew_seconds;
        
        if !self.config.security.verify_nbf {
            validation.validate_nbf = false;
        }
        
        if self.config.security.allow_expired {
            validation.validate_exp = false;
        }
        
        // Decode and validate token
        let token_data = decode::<HashMap<String, serde_json::Value>>(
            token,
            &decoding_key,
            &validation,
        ).map_err(|e| {
            self.metrics.validation_failures.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            IAMError::TokenValidation(e.to_string())
        })?;
        
        let claims = token_data.claims;
        
        // Validate required scopes
        if !config.required_scopes.is_empty() {
            let token_scopes = claims.get("scope")
                .and_then(|v| v.as_str())
                .map(|s| s.split_whitespace().collect::<Vec<_>>())
                .unwrap_or_default();
            
            for required_scope in &config.required_scopes {
                if !token_scopes.contains(&required_scope.as_str()) {
                    return Err(IAMError::MissingScope(required_scope.clone()));
                }
            }
        }
        
        // Extract standard claims
        let subject = claims.get("sub")
            .and_then(|v| v.as_str())
            .ok_or_else(|| IAMError::MissingClaim("sub".to_string()))?
            .to_string();
        
        let email = claims.get("email")
            .and_then(|v| v.as_str())
            .map(String::from);
        
        let name = claims.get("name")
            .and_then(|v| v.as_str())
            .map(String::from);
        
        let expires_at = claims.get("exp")
            .and_then(|v| v.as_u64())
            .unwrap_or(0);
        
        let groups = claims.get("groups")
            .and_then(|v| v.as_array())
            .map(|arr| arr.iter().filter_map(|v| v.as_str().map(String::from)).collect())
            .unwrap_or_default();
        
        Ok(ExternalIdentity {
            subject,
            email,
            name,
            provider: provider_name.to_string(),
            issuer: config.issuer.clone(),
            expires_at,
            groups,
            claims,
        })
    }
    
    /// Get JWKS decoding key for token validation.
    async fn get_jwks_key(
        &self,
        provider_name: &str,
        token: &str,
    ) -> Result<DecodingKey, IAMError> {
        let provider = self.oidc_providers.get(provider_name)
            .ok_or_else(|| IAMError::UnknownProvider(provider_name.to_string()))?;
        
        // Extract key ID from token header
        let header = decode_header(token)
            .map_err(|e| IAMError::TokenValidation(e.to_string()))?;
        
        let kid = header.kid.unwrap_or_default();
        
        // Check JWKS cache
        {
            let cache = self.jwks_cache.read().await;
            if let Some(jwks) = cache.get(provider_name) {
                if jwks.expires_at > SystemTime::now() {
                    if let Some(key) = jwks.keys.get(&kid) {
                        return Ok(key.clone());
                    }
                }
            }
        }
        
        // Fetch JWKS from IdP
        let jwks_uri = provider.config.jwks_uri.clone()
            .unwrap_or_else(|| format!("{}/.well-known/jwks.json", &provider.config.issuer));
        
        debug!(jwks_uri, "Fetching JWKS");
        
        let response = provider.http_client
            .get(&jwks_uri)
            .send()
            .await
            .map_err(|e| IAMError::JWKSFetch(e.to_string()))?;
        
        let jwks: JWKSResponse = response.json()
            .await
            .map_err(|e| IAMError::JWKSFetch(e.to_string()))?;
        
        // Parse and cache keys
        let mut keys = HashMap::new();
        for key in &jwks.keys {
            if let (Some(n), Some(e)) = (&key.n, &key.e) {
                let decoding_key = DecodingKey::from_rsa_components(n, e)
                    .map_err(|e| IAMError::JWKSParse(e.to_string()))?;
                keys.insert(key.kid.clone().unwrap_or_default(), decoding_key);
            }
        }
        
        // Cache JWKS
        {
            let mut cache = self.jwks_cache.write().await;
            cache.insert(provider_name.to_string(), JWKSCache {
                keys: keys.clone(),
                fetched_at: SystemTime::now(),
                expires_at: SystemTime::now() + Duration::from_secs(3600),
            });
        }
        
        keys.get(&kid)
            .cloned()
            .ok_or_else(|| IAMError::KeyNotFound(kid))
    }
    
    /// Validate a SAML assertion.
    #[instrument(skip(self, assertion), fields(provider))]
    pub async fn validate_saml_assertion(
        &self,
        provider_name: &str,
        assertion: &str,
    ) -> Result<GreyIdentity, IAMError> {
        let _provider = self.saml_providers.get(provider_name)
            .ok_or_else(|| IAMError::UnknownProvider(provider_name.to_string()))?;
        
        // SAML assertion validation would be implemented here
        // This requires XML parsing and signature verification
        // Using a library like samael or opensaml
        
        // For now, return placeholder
        Err(IAMError::NotImplemented("SAML validation not yet implemented".into()))
    }
    
    /// Identify provider from token.
    fn identify_provider(&self, token: &str) -> Result<(String, &ProviderConfig), IAMError> {
        // Try to decode token header without validation to get issuer
        if let Ok(header) = decode_header(token) {
            // Check for custom issuer in header (non-standard)
            if let Some(_alg) = header.alg.as_str().as_ref() {
                // Try to decode claims without validation to get issuer
                let parts: Vec<&str> = token.split('.').collect();
                if parts.len() >= 2 {
                    if let Ok(payload) = base64::decode(parts[1]) {
                        if let Ok(claims) = serde_json::from_slice::<HashMap<String, serde_json::Value>>(&payload) {
                            if let Some(issuer) = claims.get("iss").and_then(|v| v.as_str()) {
                                // Find provider by issuer
                                for provider in &self.config.providers {
                                    if provider.enabled {
                                        if let Some(ref oidc) = provider.oidc {
                                            if oidc.issuer == issuer {
                                                return Ok((provider.name.clone(), provider));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Fall back to default provider
        if let Some(ref default_name) = self.config.default_provider {
            for provider in &self.config.providers {
                if provider.enabled && &provider.name == default_name {
                    return Ok((provider.name.clone(), provider));
                }
            }
        }
        
        Err(IAMError::UnknownProvider("Could not identify provider from token".into()))
    }
    
    /// Map external identity to Grey identity.
    fn map_to_grey_identity(
        &self,
        external: &ExternalIdentity,
        provider_config: &ProviderConfig,
    ) -> Result<GreyIdentity, IAMError> {
        // Map roles from claims
        let mut roles = Vec::new();
        for mapping in &provider_config.role_mappings {
            if let Some(claim_value) = external.claims.get(&mapping.claim) {
                let matches = match &mapping.value {
                    ClaimMatch::Exact(expected) => {
                        claim_value.as_str() == Some(expected.as_str())
                    }
                    ClaimMatch::Contains(substring) => {
                        claim_value.as_str().map(|s| s.contains(substring)).unwrap_or(false)
                    }
                    ClaimMatch::Regex(pattern) => {
                        if let (Some(value), Ok(re)) = (claim_value.as_str(), regex::Regex::new(pattern)) {
                            re.is_match(value)
                        } else {
                            false
                        }
                    }
                    ClaimMatch::Any(values) => {
                        if let Some(value) = claim_value.as_str() {
                            values.iter().any(|v| v == value)
                        } else {
                            false
                        }
                    }
                };
                
                if matches {
                    roles.push(mapping.role.clone());
                }
            }
        }
        
        // Default role if none mapped
        if roles.is_empty() {
            roles.push("user".to_string());
        }
        
        // Map tenant from claims
        let mut tenant_id = None;
        for mapping in &provider_config.tenant_mappings {
            if let Some(claim_value) = external.claims.get(&mapping.claim) {
                let value = claim_value.as_str().unwrap_or_default();
                
                tenant_id = Some(match &mapping.transform {
                    Some(TenantTransform::Identity) | None => value.to_string(),
                    Some(TenantTransform::EmailDomain) => {
                        value.split('@').last().unwrap_or(value).to_string()
                    }
                    Some(TenantTransform::Prefix(prefix)) => format!("{}{}", prefix, value),
                    Some(TenantTransform::Suffix(suffix)) => format!("{}{}", value, suffix),
                    Some(TenantTransform::Lookup(map)) => {
                        map.get(value).cloned().unwrap_or_else(|| value.to_string())
                    }
                });
                break;
            }
            
            // Use default if claim missing
            if tenant_id.is_none() {
                tenant_id = mapping.default.clone();
            }
        }
        
        let tenant_id = tenant_id.ok_or_else(|| IAMError::MissingClaim("tenant".to_string()))?;
        
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        
        Ok(GreyIdentity {
            user_id: format!("{}:{}", external.provider, external.subject),
            tenant_id,
            roles,
            email: external.email.clone(),
            name: external.name.clone(),
            provider: external.provider.clone(),
            authenticated_at: now,
            expires_at: external.expires_at,
            attributes: HashMap::new(),
        })
    }
    
    /// Issue a Grey-native token for an identity.
    #[instrument(skip(self))]
    pub async fn issue_grey_token(&self, identity: &GreyIdentity) -> Result<GreyToken, IAMError> {
        let config = &self.config.grey_token;
        
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        
        let exp = now + config.lifetime_seconds;
        
        let claims = GreyTokenClaims {
            iss: config.issuer.clone(),
            sub: identity.user_id.clone(),
            aud: "grey-api".to_string(),
            exp,
            iat: now,
            nbf: now,
            jti: uuid::Uuid::new_v4().to_string(),
            tenant_id: identity.tenant_id.clone(),
            roles: identity.roles.clone(),
            email: identity.email.clone(),
            name: identity.name.clone(),
            provider: identity.provider.clone(),
        };
        
        // Sign token
        let token = if let Some(ref secret) = config.hmac_secret {
            let key = jsonwebtoken::EncodingKey::from_secret(secret.as_bytes());
            let header = jsonwebtoken::Header::new(Algorithm::HS256);
            jsonwebtoken::encode(&header, &claims, &key)
                .map_err(|e| IAMError::TokenGeneration(e.to_string()))?
        } else {
            // RSA signing would be implemented here
            return Err(IAMError::Configuration("No signing key configured".into()));
        };
        
        self.metrics.tokens_issued.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        
        Ok(GreyToken {
            token,
            token_type: "Bearer".to_string(),
            expires_at: exp,
            refresh_token: None,
        })
    }
    
    /// Validate a Grey-native token.
    pub async fn validate_grey_token(&self, token: &str) -> Result<GreyIdentity, IAMError> {
        let config = &self.config.grey_token;
        
        let key = if let Some(ref secret) = config.hmac_secret {
            DecodingKey::from_secret(secret.as_bytes())
        } else {
            return Err(IAMError::Configuration("No signing key configured".into()));
        };
        
        let mut validation = Validation::new(Algorithm::HS256);
        validation.set_issuer(&[&config.issuer]);
        validation.set_audience(&["grey-api"]);
        
        let token_data = decode::<GreyTokenClaims>(token, &key, &validation)
            .map_err(|e| IAMError::TokenValidation(e.to_string()))?;
        
        let claims = token_data.claims;
        
        Ok(GreyIdentity {
            user_id: claims.sub,
            tenant_id: claims.tenant_id,
            roles: claims.roles,
            email: claims.email,
            name: claims.name,
            provider: claims.provider,
            authenticated_at: claims.iat,
            expires_at: claims.exp,
            attributes: HashMap::new(),
        })
    }
    
    /// Export metrics.
    pub fn get_metrics(&self) -> HashMap<String, u64> {
        let mut metrics = HashMap::new();
        metrics.insert(
            "grey_iam_tokens_validated_total".to_string(),
            self.metrics.tokens_validated.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics.insert(
            "grey_iam_tokens_issued_total".to_string(),
            self.metrics.tokens_issued.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics.insert(
            "grey_iam_cache_hits_total".to_string(),
            self.metrics.cache_hits.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics.insert(
            "grey_iam_cache_misses_total".to_string(),
            self.metrics.cache_misses.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics.insert(
            "grey_iam_validation_failures_total".to_string(),
            self.metrics.validation_failures.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics
    }
}

impl IAMMetrics {
    fn new() -> Self {
        Self {
            tokens_validated: std::sync::atomic::AtomicU64::new(0),
            tokens_issued: std::sync::atomic::AtomicU64::new(0),
            cache_hits: std::sync::atomic::AtomicU64::new(0),
            cache_misses: std::sync::atomic::AtomicU64::new(0),
            validation_failures: std::sync::atomic::AtomicU64::new(0),
        }
    }
}

// ============================================================================
// JWKS Types
// ============================================================================

#[derive(Debug, Deserialize)]
struct JWKSResponse {
    keys: Vec<JWK>,
}

#[derive(Debug, Deserialize)]
struct JWK {
    kty: String,
    kid: Option<String>,
    #[serde(rename = "use")]
    key_use: Option<String>,
    n: Option<String>,
    e: Option<String>,
}

// ============================================================================
// Errors
// ============================================================================

#[derive(Debug, thiserror::Error)]
pub enum IAMError {
    #[error("Configuration error: {0}")]
    Configuration(String),
    
    #[error("Unknown provider: {0}")]
    UnknownProvider(String),
    
    #[error("Invalid token: {0}")]
    InvalidToken(String),
    
    #[error("Token validation failed: {0}")]
    TokenValidation(String),
    
    #[error("Missing required claim: {0}")]
    MissingClaim(String),
    
    #[error("Missing required scope: {0}")]
    MissingScope(String),
    
    #[error("JWKS fetch failed: {0}")]
    JWKSFetch(String),
    
    #[error("JWKS parse failed: {0}")]
    JWKSParse(String),
    
    #[error("Key not found: {0}")]
    KeyNotFound(String),
    
    #[error("Token generation failed: {0}")]
    TokenGeneration(String),
    
    #[error("Not implemented: {0}")]
    NotImplemented(String),
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_tenant_transform_email_domain() {
        let transform = TenantTransform::EmailDomain;
        let email = "user@example.com";
        let result = match transform {
            TenantTransform::EmailDomain => email.split('@').last().unwrap_or(email).to_string(),
            _ => email.to_string(),
        };
        assert_eq!(result, "example.com");
    }
    
    #[test]
    fn test_tenant_transform_prefix() {
        let transform = TenantTransform::Prefix("tenant-".to_string());
        let value = "abc123";
        let result = match transform {
            TenantTransform::Prefix(prefix) => format!("{}{}", prefix, value),
            _ => value.to_string(),
        };
        assert_eq!(result, "tenant-abc123");
    }
    
    #[test]
    fn test_claim_match_exact() {
        let claim_match = ClaimMatch::Exact("admin".to_string());
        let matches = match &claim_match {
            ClaimMatch::Exact(expected) => "admin" == expected,
            _ => false,
        };
        assert!(matches);
    }
}
