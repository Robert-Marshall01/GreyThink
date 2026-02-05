//! # Grey Distributed — Enterprise Integration Examples
//!
//! This module provides integration patterns for connecting Grey Distributed
//! with common enterprise systems: ERP, CRM, identity providers, and data platforms.
//!
//! ## Integration Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────┐
//! │                        Enterprise Layer                            │
//! │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  │
//! │  │   ERP   │  │   CRM   │  │   IAM   │  │  Data   │  │ Monitor │  │
//! │  │(SAP/ORC)│  │(SFDC)   │  │(Okta)   │  │(Snowflk)│  │(Datadog)│  │
//! │  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘  │
//! └───────┼────────────┼────────────┼────────────┼────────────┼───────┘
//!         │            │            │            │            │
//!         ▼            ▼            ▼            ▼            ▼
//! ┌─────────────────────────────────────────────────────────────────────┐
//! │                    Integration Adapters                             │
//! │  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐   │
//! │  │ ERP Adapter │ │ CRM Adapter │ │ SSO Adapter │ │Data Adapter │   │
//! │  └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘   │
//! └─────────────────────────────────────────────────────────────────────┘
//!                              │
//!                              ▼
//! ┌─────────────────────────────────────────────────────────────────────┐
//! │                      Grey Distributed Core                          │
//! └─────────────────────────────────────────────────────────────────────┘
//! ```

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// =============================================================================
// Common Types
// =============================================================================

pub type TenantId = String;
pub type UserId = String;
pub type ResourceId = String;

/// Integration result type
pub type IntegrationResult<T> = Result<T, IntegrationError>;

#[derive(Debug, Clone)]
pub enum IntegrationError {
    ConnectionFailed(String),
    AuthenticationFailed(String),
    RateLimited { retry_after: Duration },
    InvalidData(String),
    NotFound(String),
    InternalError(String),
}

impl std::fmt::Display for IntegrationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegrationError::ConnectionFailed(msg) => write!(f, "Connection failed: {}", msg),
            IntegrationError::AuthenticationFailed(msg) => write!(f, "Auth failed: {}", msg),
            IntegrationError::RateLimited { retry_after } => {
                write!(f, "Rate limited, retry after {:?}", retry_after)
            }
            IntegrationError::InvalidData(msg) => write!(f, "Invalid data: {}", msg),
            IntegrationError::NotFound(msg) => write!(f, "Not found: {}", msg),
            IntegrationError::InternalError(msg) => write!(f, "Internal error: {}", msg),
        }
    }
}

impl std::error::Error for IntegrationError {}

// =============================================================================
// ERP Integration (SAP, Oracle, NetSuite)
// =============================================================================

/// ERP integration for cost allocation and resource tracking
pub mod erp {
    use super::*;

    /// ERP connector configuration
    #[derive(Debug, Clone)]
    pub struct ErpConfig {
        pub provider: ErpProvider,
        pub endpoint: String,
        pub credentials: ErpCredentials,
        pub cost_center_mapping: HashMap<TenantId, String>,
        pub sync_interval: Duration,
    }

    #[derive(Debug, Clone)]
    pub enum ErpProvider {
        SapS4Hana,
        OracleCloud,
        NetSuite,
        MicrosoftDynamics,
    }

    #[derive(Debug, Clone)]
    pub struct ErpCredentials {
        pub client_id: String,
        pub client_secret: String,
        pub tenant_id: Option<String>,
    }

    /// Cost allocation record for ERP sync
    #[derive(Debug, Clone)]
    pub struct CostAllocation {
        pub allocation_id: String,
        pub cost_center: String,
        pub project_code: Option<String>,
        pub amount: f64,
        pub currency: String,
        pub period_start: SystemTime,
        pub period_end: SystemTime,
        pub resource_breakdown: Vec<ResourceCost>,
    }

    #[derive(Debug, Clone)]
    pub struct ResourceCost {
        pub resource_type: String,
        pub usage_quantity: f64,
        pub usage_unit: String,
        pub unit_cost: f64,
        pub total_cost: f64,
    }

    /// ERP integration client
    pub struct ErpClient {
        config: ErpConfig,
    }

    impl ErpClient {
        pub fn new(config: ErpConfig) -> Self {
            Self { config }
        }

        /// Sync cost allocations to ERP
        ///
        /// ## Integration Flow
        /// 1. Aggregate Grey usage costs by cost center
        /// 2. Map to ERP chart of accounts
        /// 3. Create journal entries or cost allocations
        /// 4. Post to ERP via API
        pub async fn sync_cost_allocations(
            &self,
            allocations: Vec<CostAllocation>,
        ) -> IntegrationResult<SyncResult> {
            // Validate allocations
            for alloc in &allocations {
                if !self.config.cost_center_mapping.values().any(|cc| cc == &alloc.cost_center) {
                    return Err(IntegrationError::InvalidData(
                        format!("Unknown cost center: {}", alloc.cost_center)
                    ));
                }
            }

            // In production: POST to ERP API
            // Example for SAP S/4HANA:
            // POST /sap/opu/odata/sap/API_COSTCENTERACTIVITYALLOCATION_SRV/CostCenterAllocation
            
            Ok(SyncResult {
                records_synced: allocations.len(),
                failed_records: 0,
                sync_timestamp: SystemTime::now(),
            })
        }

        /// Import budget data from ERP
        pub async fn import_budgets(&self) -> IntegrationResult<Vec<Budget>> {
            // In production: GET from ERP API
            // Maps ERP budget line items to Grey quotas
            
            Ok(vec![
                Budget {
                    cost_center: "CC-ENGINEERING".into(),
                    annual_budget: 500_000.0,
                    currency: "USD".into(),
                    fiscal_year: 2026,
                },
            ])
        }

        /// Get cost center for a tenant
        pub fn get_cost_center(&self, tenant_id: &TenantId) -> Option<String> {
            self.config.cost_center_mapping.get(tenant_id).cloned()
        }
    }

    #[derive(Debug, Clone)]
    pub struct SyncResult {
        pub records_synced: usize,
        pub failed_records: usize,
        pub sync_timestamp: SystemTime,
    }

    #[derive(Debug, Clone)]
    pub struct Budget {
        pub cost_center: String,
        pub annual_budget: f64,
        pub currency: String,
        pub fiscal_year: u32,
    }

    /// Example: SAP integration setup
    /// 
    /// ```rust,ignore
    /// let erp_config = ErpConfig {
    ///     provider: ErpProvider::SapS4Hana,
    ///     endpoint: "https://my-company.s4hana.cloud.sap".into(),
    ///     credentials: ErpCredentials {
    ///         client_id: env::var("SAP_CLIENT_ID").unwrap(),
    ///         client_secret: env::var("SAP_CLIENT_SECRET").unwrap(),
    ///         tenant_id: Some("my-tenant".into()),
    ///     },
    ///     cost_center_mapping: [
    ///         ("tenant-engineering".into(), "CC-1001".into()),
    ///         ("tenant-marketing".into(), "CC-1002".into()),
    ///     ].into(),
    ///     sync_interval: Duration::from_secs(3600),
    /// };
    /// 
    /// let erp = ErpClient::new(erp_config);
    /// erp.sync_cost_allocations(allocations).await?;
    /// ```
}

// =============================================================================
// CRM Integration (Salesforce, HubSpot)
// =============================================================================

/// CRM integration for customer usage tracking and billing
pub mod crm {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct CrmConfig {
        pub provider: CrmProvider,
        pub instance_url: String,
        pub credentials: CrmCredentials,
        pub account_field_mapping: AccountFieldMapping,
    }

    #[derive(Debug, Clone)]
    pub enum CrmProvider {
        Salesforce,
        HubSpot,
        MicrosoftDynamics365,
        Zoho,
    }

    #[derive(Debug, Clone)]
    pub struct CrmCredentials {
        pub client_id: String,
        pub client_secret: String,
        pub refresh_token: Option<String>,
    }

    #[derive(Debug, Clone)]
    pub struct AccountFieldMapping {
        pub tenant_id_field: String,
        pub usage_field: String,
        pub tier_field: String,
        pub contract_end_field: String,
    }

    /// Usage data for CRM sync
    #[derive(Debug, Clone)]
    pub struct CustomerUsage {
        pub account_id: String,
        pub tenant_id: TenantId,
        pub period: String,
        pub total_usage_tokens: u64,
        pub usage_by_resource: HashMap<String, f64>,
        pub tier: String,
        pub contract_utilization: f64,
    }

    /// CRM integration client
    pub struct CrmClient {
        config: CrmConfig,
    }

    impl CrmClient {
        pub fn new(config: CrmConfig) -> Self {
            Self { config }
        }

        /// Sync usage data to CRM
        ///
        /// Updates custom fields on Account/Company objects with Grey usage metrics.
        /// Enables sales/CS teams to:
        /// - Track customer engagement
        /// - Identify upsell opportunities
        /// - Monitor contract utilization
        pub async fn sync_usage_to_crm(
            &self,
            usage_records: Vec<CustomerUsage>,
        ) -> IntegrationResult<SyncResult> {
            // In production: PATCH to CRM API
            // Salesforce example:
            // PATCH /services/data/v57.0/sobjects/Account/{account_id}
            
            Ok(SyncResult {
                records_synced: usage_records.len(),
                failed_records: 0,
                sync_timestamp: SystemTime::now(),
            })
        }

        /// Get accounts approaching quota limits
        pub async fn get_upsell_candidates(&self) -> IntegrationResult<Vec<UpsellCandidate>> {
            // Query CRM for accounts with high utilization
            Ok(vec![])
        }

        /// Create renewal opportunity
        pub async fn create_renewal_opportunity(
            &self,
            account_id: &str,
            opportunity: RenewalOpportunity,
        ) -> IntegrationResult<String> {
            // In production: POST to CRM API
            Ok(format!("opp-{}", account_id))
        }
    }

    #[derive(Debug, Clone)]
    pub struct SyncResult {
        pub records_synced: usize,
        pub failed_records: usize,
        pub sync_timestamp: SystemTime,
    }

    #[derive(Debug, Clone)]
    pub struct UpsellCandidate {
        pub account_id: String,
        pub account_name: String,
        pub current_tier: String,
        pub utilization: f64,
        pub recommended_tier: String,
        pub estimated_revenue_increase: f64,
    }

    #[derive(Debug, Clone)]
    pub struct RenewalOpportunity {
        pub name: String,
        pub amount: f64,
        pub close_date: SystemTime,
        pub stage: String,
    }

    /// Example: Salesforce integration
    ///
    /// ```rust,ignore
    /// let crm = CrmClient::new(CrmConfig {
    ///     provider: CrmProvider::Salesforce,
    ///     instance_url: "https://mycompany.my.salesforce.com".into(),
    ///     credentials: CrmCredentials {
    ///         client_id: env::var("SFDC_CLIENT_ID").unwrap(),
    ///         client_secret: env::var("SFDC_CLIENT_SECRET").unwrap(),
    ///         refresh_token: Some(env::var("SFDC_REFRESH_TOKEN").unwrap()),
    ///     },
    ///     account_field_mapping: AccountFieldMapping {
    ///         tenant_id_field: "Grey_Tenant_ID__c".into(),
    ///         usage_field: "Grey_Monthly_Usage__c".into(),
    ///         tier_field: "Grey_Tier__c".into(),
    ///         contract_end_field: "Grey_Contract_End__c".into(),
    ///     },
    /// });
    /// ```
}

// =============================================================================
// Identity Provider Integration (Okta, Azure AD, Auth0)
// =============================================================================

/// SSO and identity management integration
pub mod identity {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct IdpConfig {
        pub provider: IdpProvider,
        pub issuer_url: String,
        pub client_id: String,
        pub client_secret: String,
        pub scopes: Vec<String>,
        pub user_attribute_mapping: UserAttributeMapping,
    }

    #[derive(Debug, Clone)]
    pub enum IdpProvider {
        Okta,
        AzureAd,
        Auth0,
        PingIdentity,
        OneLogin,
    }

    #[derive(Debug, Clone)]
    pub struct UserAttributeMapping {
        /// IdP attribute → Grey attribute
        pub tenant_id: String,
        pub user_id: String,
        pub email: String,
        pub groups: String,
        pub roles: String,
    }

    #[derive(Debug, Clone)]
    pub struct AuthenticatedUser {
        pub user_id: UserId,
        pub tenant_id: TenantId,
        pub email: String,
        pub groups: Vec<String>,
        pub roles: Vec<String>,
        pub session_expires: SystemTime,
    }

    /// SCIM provisioning for user lifecycle
    #[derive(Debug, Clone)]
    pub struct ScimUser {
        pub external_id: String,
        pub user_name: String,
        pub email: String,
        pub active: bool,
        pub groups: Vec<String>,
    }

    /// Identity provider client
    pub struct IdpClient {
        config: IdpConfig,
    }

    impl IdpClient {
        pub fn new(config: IdpConfig) -> Self {
            Self { config }
        }

        /// Validate JWT token from IdP
        pub async fn validate_token(&self, token: &str) -> IntegrationResult<AuthenticatedUser> {
            // In production:
            // 1. Fetch JWKS from IdP
            // 2. Validate JWT signature
            // 3. Verify claims (iss, aud, exp)
            // 4. Map attributes to Grey user
            
            Ok(AuthenticatedUser {
                user_id: "user-123".into(),
                tenant_id: "tenant-abc".into(),
                email: "user@example.com".into(),
                groups: vec!["developers".into()],
                roles: vec!["admin".into()],
                session_expires: SystemTime::now() + Duration::from_secs(3600),
            })
        }

        /// SCIM: Create user
        pub async fn provision_user(&self, user: ScimUser) -> IntegrationResult<String> {
            // Map SCIM user to Grey tenant user
            Ok(format!("grey-user-{}", user.external_id))
        }

        /// SCIM: Update user
        pub async fn update_user(&self, user_id: &str, user: ScimUser) -> IntegrationResult<()> {
            // Sync user attributes
            Ok(())
        }

        /// SCIM: Deactivate user
        pub async fn deactivate_user(&self, user_id: &str) -> IntegrationResult<()> {
            // Revoke access, keep audit trail
            Ok(())
        }

        /// Get groups for RBAC sync
        pub async fn sync_groups(&self) -> IntegrationResult<Vec<IdpGroup>> {
            Ok(vec![])
        }
    }

    #[derive(Debug, Clone)]
    pub struct IdpGroup {
        pub group_id: String,
        pub name: String,
        pub members: Vec<String>,
    }

    /// Example: Okta SSO integration
    ///
    /// ```rust,ignore
    /// let idp = IdpClient::new(IdpConfig {
    ///     provider: IdpProvider::Okta,
    ///     issuer_url: "https://mycompany.okta.com".into(),
    ///     client_id: env::var("OKTA_CLIENT_ID").unwrap(),
    ///     client_secret: env::var("OKTA_CLIENT_SECRET").unwrap(),
    ///     scopes: vec!["openid".into(), "profile".into(), "email".into()],
    ///     user_attribute_mapping: UserAttributeMapping {
    ///         tenant_id: "grey_tenant".into(),
    ///         user_id: "sub".into(),
    ///         email: "email".into(),
    ///         groups: "groups".into(),
    ///         roles: "grey_roles".into(),
    ///     },
    /// });
    /// 
    /// // Validate incoming token
    /// let user = idp.validate_token(bearer_token).await?;
    /// ```
}

// =============================================================================
// Data Platform Integration (Snowflake, Databricks, BigQuery)
// =============================================================================

/// Data platform integration for analytics and reporting
pub mod data_platform {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct DataPlatformConfig {
        pub provider: DataPlatformProvider,
        pub connection_string: String,
        pub credentials: DataPlatformCredentials,
        pub target_schema: String,
        pub sync_tables: Vec<TableSync>,
    }

    #[derive(Debug, Clone)]
    pub enum DataPlatformProvider {
        Snowflake,
        Databricks,
        BigQuery,
        Redshift,
    }

    #[derive(Debug, Clone)]
    pub struct DataPlatformCredentials {
        pub account: String,
        pub user: String,
        pub private_key: Option<String>,
        pub password: Option<String>,
    }

    #[derive(Debug, Clone)]
    pub struct TableSync {
        pub source_metric: String,
        pub target_table: String,
        pub sync_mode: SyncMode,
        pub partition_key: Option<String>,
    }

    #[derive(Debug, Clone)]
    pub enum SyncMode {
        Incremental,
        FullRefresh,
        Append,
    }

    /// Metrics data for export
    #[derive(Debug, Clone)]
    pub struct MetricsExport {
        pub timestamp: SystemTime,
        pub tenant_id: TenantId,
        pub metric_name: String,
        pub metric_value: f64,
        pub dimensions: HashMap<String, String>,
    }

    /// Data platform client
    pub struct DataPlatformClient {
        config: DataPlatformConfig,
    }

    impl DataPlatformClient {
        pub fn new(config: DataPlatformConfig) -> Self {
            Self { config }
        }

        /// Export usage metrics to data warehouse
        ///
        /// Enables:
        /// - Custom BI dashboards
        /// - ML-based forecasting
        /// - Cost optimization analysis
        pub async fn export_metrics(
            &self,
            metrics: Vec<MetricsExport>,
        ) -> IntegrationResult<ExportResult> {
            // In production:
            // 1. Transform to target schema
            // 2. Stage data (S3/GCS/Azure Blob)
            // 3. COPY INTO target table
            
            Ok(ExportResult {
                rows_exported: metrics.len(),
                bytes_written: metrics.len() * 100, // Estimate
                export_timestamp: SystemTime::now(),
            })
        }

        /// Export token ledger for financial analysis
        pub async fn export_ledger(
            &self,
            start_time: SystemTime,
            end_time: SystemTime,
        ) -> IntegrationResult<ExportResult> {
            Ok(ExportResult {
                rows_exported: 0,
                bytes_written: 0,
                export_timestamp: SystemTime::now(),
            })
        }

        /// Create materialized view for real-time analytics
        pub async fn create_live_view(&self, view_name: &str, query: &str) -> IntegrationResult<()> {
            // In production: CREATE MATERIALIZED VIEW
            Ok(())
        }
    }

    #[derive(Debug, Clone)]
    pub struct ExportResult {
        pub rows_exported: usize,
        pub bytes_written: usize,
        pub export_timestamp: SystemTime,
    }

    /// Example: Snowflake integration
    ///
    /// ```rust,ignore
    /// let data_platform = DataPlatformClient::new(DataPlatformConfig {
    ///     provider: DataPlatformProvider::Snowflake,
    ///     connection_string: "myaccount.snowflakecomputing.com".into(),
    ///     credentials: DataPlatformCredentials {
    ///         account: "myaccount".into(),
    ///         user: "grey_service".into(),
    ///         private_key: Some(env::var("SNOWFLAKE_PRIVATE_KEY").unwrap()),
    ///         password: None,
    ///     },
    ///     target_schema: "GREY_ANALYTICS".into(),
    ///     sync_tables: vec![
    ///         TableSync {
    ///             source_metric: "resource_usage".into(),
    ///             target_table: "RESOURCE_USAGE".into(),
    ///             sync_mode: SyncMode::Incremental,
    ///             partition_key: Some("date".into()),
    ///         },
    ///     ],
    /// });
    /// ```
}

// =============================================================================
// Monitoring Integration (Datadog, New Relic, Splunk)
// =============================================================================

/// Observability platform integration
pub mod monitoring {
    use super::*;

    #[derive(Debug, Clone)]
    pub struct MonitoringConfig {
        pub provider: MonitoringProvider,
        pub api_key: String,
        pub api_endpoint: Option<String>,
        pub service_name: String,
        pub environment: String,
        pub custom_tags: HashMap<String, String>,
    }

    #[derive(Debug, Clone)]
    pub enum MonitoringProvider {
        Datadog,
        NewRelic,
        Splunk,
        Dynatrace,
        Prometheus,
    }

    /// Metric for external monitoring
    #[derive(Debug, Clone)]
    pub struct ExternalMetric {
        pub name: String,
        pub value: f64,
        pub metric_type: MetricType,
        pub tags: HashMap<String, String>,
        pub timestamp: SystemTime,
    }

    #[derive(Debug, Clone)]
    pub enum MetricType {
        Gauge,
        Counter,
        Histogram,
    }

    /// Monitoring client
    pub struct MonitoringClient {
        config: MonitoringConfig,
    }

    impl MonitoringClient {
        pub fn new(config: MonitoringConfig) -> Self {
            Self { config }
        }

        /// Send custom metrics to monitoring platform
        pub async fn send_metrics(&self, metrics: Vec<ExternalMetric>) -> IntegrationResult<()> {
            // In production: POST to monitoring API
            // Datadog: POST https://api.datadoghq.com/api/v1/series
            Ok(())
        }

        /// Send custom event
        pub async fn send_event(&self, event: MonitoringEvent) -> IntegrationResult<()> {
            Ok(())
        }

        /// Create/update dashboard
        pub async fn sync_dashboard(&self, dashboard: DashboardDefinition) -> IntegrationResult<String> {
            Ok("dashboard-123".into())
        }

        /// Configure alerts
        pub async fn sync_alerts(&self, alerts: Vec<AlertDefinition>) -> IntegrationResult<()> {
            Ok(())
        }
    }

    #[derive(Debug, Clone)]
    pub struct MonitoringEvent {
        pub title: String,
        pub text: String,
        pub alert_type: String,
        pub tags: Vec<String>,
    }

    #[derive(Debug, Clone)]
    pub struct DashboardDefinition {
        pub title: String,
        pub widgets: Vec<String>, // JSON widget definitions
    }

    #[derive(Debug, Clone)]
    pub struct AlertDefinition {
        pub name: String,
        pub query: String,
        pub threshold: f64,
        pub notify: Vec<String>,
    }
}

// =============================================================================
// Integration Orchestrator
// =============================================================================

/// Orchestrates all enterprise integrations
pub struct IntegrationOrchestrator {
    erp: Option<erp::ErpClient>,
    crm: Option<crm::CrmClient>,
    idp: Option<identity::IdpClient>,
    data_platform: Option<data_platform::DataPlatformClient>,
    monitoring: Option<monitoring::MonitoringClient>,
}

impl IntegrationOrchestrator {
    pub fn new() -> Self {
        Self {
            erp: None,
            crm: None,
            idp: None,
            data_platform: None,
            monitoring: None,
        }
    }

    pub fn with_erp(mut self, client: erp::ErpClient) -> Self {
        self.erp = Some(client);
        self
    }

    pub fn with_crm(mut self, client: crm::CrmClient) -> Self {
        self.crm = Some(client);
        self
    }

    pub fn with_idp(mut self, client: identity::IdpClient) -> Self {
        self.idp = Some(client);
        self
    }

    pub fn with_data_platform(mut self, client: data_platform::DataPlatformClient) -> Self {
        self.data_platform = Some(client);
        self
    }

    pub fn with_monitoring(mut self, client: monitoring::MonitoringClient) -> Self {
        self.monitoring = Some(client);
        self
    }

    /// Run periodic sync for all configured integrations
    pub async fn run_sync(&self) -> IntegrationResult<SyncReport> {
        let mut report = SyncReport::default();

        if let Some(erp) = &self.erp {
            // Sync cost allocations
            report.erp_synced = true;
        }

        if let Some(crm) = &self.crm {
            // Sync usage data
            report.crm_synced = true;
        }

        if let Some(data_platform) = &self.data_platform {
            // Export metrics
            report.data_platform_synced = true;
        }

        report.sync_timestamp = SystemTime::now();
        Ok(report)
    }
}

#[derive(Debug, Clone, Default)]
pub struct SyncReport {
    pub erp_synced: bool,
    pub crm_synced: bool,
    pub idp_synced: bool,
    pub data_platform_synced: bool,
    pub monitoring_synced: bool,
    pub sync_timestamp: SystemTime,
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_orchestrator_builder() {
        let orchestrator = IntegrationOrchestrator::new();
        assert!(orchestrator.erp.is_none());
        assert!(orchestrator.crm.is_none());
    }

    #[test]
    fn test_cost_center_mapping() {
        let config = erp::ErpConfig {
            provider: erp::ErpProvider::SapS4Hana,
            endpoint: "https://example.com".into(),
            credentials: erp::ErpCredentials {
                client_id: "test".into(),
                client_secret: "test".into(),
                tenant_id: None,
            },
            cost_center_mapping: [
                ("tenant-1".to_string(), "CC-1001".to_string()),
            ].into(),
            sync_interval: Duration::from_secs(3600),
        };

        let client = erp::ErpClient::new(config);
        assert_eq!(client.get_cost_center(&"tenant-1".into()), Some("CC-1001".into()));
        assert_eq!(client.get_cost_center(&"tenant-unknown".into()), None);
    }
}
