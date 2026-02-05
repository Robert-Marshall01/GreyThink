//! Grey Distributed — AWS Cloud Adapter
//!
//! Integration with Amazon Web Services for deployment, storage, and monitoring.
//! Supports multi-region replication, cost monitoring, and native AWS service integration.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────┐
//! │                        Grey Distributed                         │
//! └─────────────────────────────────────────────────────────────────┘
//!          │              │              │              │
//!          ▼              ▼              ▼              ▼
//! ┌─────────────┐ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
//! │     S3      │ │    DynamoDB │ │     EKS     │ │  CloudWatch │
//! │  (Storage)  │ │   (State)   │ │  (Compute)  │ │ (Monitoring)│
//! └─────────────┘ └─────────────┘ └─────────────┘ └─────────────┘
//!          │              │              │              │
//!          └──────────────┴──────────────┴──────────────┘
//!                                  │
//!                                  ▼
//!                    ┌─────────────────────────┐
//!                    │     Cost Explorer       │
//!                    │     (Cost Tracking)     │
//!                    └─────────────────────────┘
//! ```
//!
//! # Design Decisions
//!
//! 1. **Multi-Region**: Uses S3 Cross-Region Replication (CRR) for data durability.
//!    DynamoDB Global Tables for consistent state across regions.
//!
//! 2. **Cost Awareness**: All operations include cost tracking tags.
//!    Hooks into Cost Explorer API for real-time cost monitoring.
//!
//! 3. **IAM Integration**: Uses IAM roles with least-privilege principles.
//!    Supports instance profiles for EKS/EC2 deployments.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use async_trait::async_trait;
use aws_config::SdkConfig;
use aws_sdk_s3::Client as S3Client;
use aws_sdk_dynamodb::Client as DynamoDBClient;
use aws_sdk_cloudwatch::Client as CloudWatchClient;
use aws_sdk_costexplorer::Client as CostExplorerClient;
use aws_sdk_eks::Client as EKSClient;
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn, instrument};

// ============================================================================
// Configuration
// ============================================================================

/// AWS adapter configuration.
///
/// # Tradeoffs
///
/// - `enable_multi_region`: Increases durability but adds latency and cost.
/// - `storage_class`: STANDARD vs INTELLIGENT_TIERING affects cost/access patterns.
/// - `dynamodb_on_demand`: Simplifies scaling but may be costlier for predictable loads.
#[derive(Debug, Clone, Deserialize)]
pub struct AWSConfig {
    /// AWS region for primary deployment
    pub primary_region: String,
    
    /// Additional regions for replication
    #[serde(default)]
    pub replica_regions: Vec<String>,
    
    /// Enable multi-region replication
    #[serde(default)]
    pub enable_multi_region: bool,
    
    /// S3 configuration
    pub s3: S3Config,
    
    /// DynamoDB configuration
    pub dynamodb: DynamoDBConfig,
    
    /// EKS configuration
    pub eks: Option<EKSConfig>,
    
    /// CloudWatch configuration
    #[serde(default)]
    pub cloudwatch: CloudWatchConfig,
    
    /// Cost monitoring configuration
    #[serde(default)]
    pub cost_monitoring: CostMonitoringConfig,
    
    /// IAM configuration
    #[serde(default)]
    pub iam: IAMConfig,
    
    /// Resource tags applied to all AWS resources
    #[serde(default)]
    pub tags: HashMap<String, String>,
}

/// S3 configuration for Grey storage.
#[derive(Debug, Clone, Deserialize)]
pub struct S3Config {
    /// Bucket name for task artifacts
    pub artifacts_bucket: String,
    
    /// Bucket name for state snapshots
    pub state_bucket: String,
    
    /// Bucket name for proof artifacts
    pub proofs_bucket: String,
    
    /// Storage class for artifacts
    #[serde(default = "default_storage_class")]
    pub storage_class: String,
    
    /// Enable server-side encryption
    #[serde(default = "default_true")]
    pub enable_encryption: bool,
    
    /// KMS key ID for encryption (uses aws/s3 if not specified)
    pub kms_key_id: Option<String>,
    
    /// Enable versioning
    #[serde(default = "default_true")]
    pub enable_versioning: bool,
    
    /// Lifecycle rules for cost optimization
    #[serde(default)]
    pub lifecycle_rules: Vec<LifecycleRule>,
    
    /// Enable intelligent tiering
    #[serde(default)]
    pub intelligent_tiering: bool,
}

fn default_storage_class() -> String { "STANDARD".to_string() }
fn default_true() -> bool { true }

#[derive(Debug, Clone, Deserialize)]
pub struct LifecycleRule {
    /// Rule ID
    pub id: String,
    /// Object prefix filter
    pub prefix: String,
    /// Transition to GLACIER after days
    pub transition_glacier_days: Option<u32>,
    /// Transition to DEEP_ARCHIVE after days
    pub transition_deep_archive_days: Option<u32>,
    /// Delete after days
    pub expiration_days: Option<u32>,
}

/// DynamoDB configuration for Grey state.
#[derive(Debug, Clone, Deserialize)]
pub struct DynamoDBConfig {
    /// Table name for task state
    pub task_table: String,
    
    /// Table name for cluster state
    pub cluster_table: String,
    
    /// Table name for tenant metadata
    pub tenant_table: String,
    
    /// Use on-demand capacity
    #[serde(default = "default_true")]
    pub on_demand: bool,
    
    /// Read capacity units (if not on-demand)
    pub read_capacity: Option<u64>,
    
    /// Write capacity units (if not on-demand)
    pub write_capacity: Option<u64>,
    
    /// Enable point-in-time recovery
    #[serde(default = "default_true")]
    pub enable_pitr: bool,
    
    /// Enable global tables for multi-region
    #[serde(default)]
    pub global_table: bool,
    
    /// Time-to-live attribute name
    pub ttl_attribute: Option<String>,
}

/// EKS configuration for Grey compute.
#[derive(Debug, Clone, Deserialize)]
pub struct EKSConfig {
    /// EKS cluster name
    pub cluster_name: String,
    
    /// Node group configurations
    #[serde(default)]
    pub node_groups: Vec<NodeGroupConfig>,
    
    /// Fargate profile for serverless
    pub fargate_profile: Option<String>,
    
    /// Enable cluster autoscaler
    #[serde(default = "default_true")]
    pub enable_autoscaler: bool,
    
    /// Enable Nitro Enclaves for TEE
    #[serde(default)]
    pub enable_nitro_enclaves: bool,
}

#[derive(Debug, Clone, Deserialize)]
pub struct NodeGroupConfig {
    /// Node group name
    pub name: String,
    /// Instance types
    pub instance_types: Vec<String>,
    /// Minimum nodes
    pub min_size: u32,
    /// Maximum nodes
    pub max_size: u32,
    /// Desired nodes
    pub desired_size: u32,
    /// Use spot instances
    #[serde(default)]
    pub use_spot: bool,
    /// Labels for node selection
    #[serde(default)]
    pub labels: HashMap<String, String>,
}

/// CloudWatch configuration.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct CloudWatchConfig {
    /// Log group for Grey logs
    #[serde(default = "default_log_group")]
    pub log_group: String,
    
    /// Metric namespace
    #[serde(default = "default_metric_namespace")]
    pub metric_namespace: String,
    
    /// Enable Container Insights
    #[serde(default)]
    pub enable_container_insights: bool,
    
    /// Enable X-Ray tracing
    #[serde(default)]
    pub enable_xray: bool,
    
    /// Custom dashboard name
    pub dashboard_name: Option<String>,
    
    /// Alarm SNS topic ARN
    pub alarm_topic_arn: Option<String>,
}

fn default_log_group() -> String { "/grey/distributed".to_string() }
fn default_metric_namespace() -> String { "Grey/Distributed".to_string() }

/// Cost monitoring configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct CostMonitoringConfig {
    /// Enable cost tracking
    #[serde(default = "default_true")]
    pub enabled: bool,
    
    /// Cost allocation tag key
    #[serde(default = "default_cost_tag")]
    pub cost_allocation_tag: String,
    
    /// Budget alert threshold (USD)
    pub budget_alert_threshold: Option<f64>,
    
    /// Budget alert SNS topic ARN
    pub budget_alert_topic_arn: Option<String>,
    
    /// Cost report S3 bucket
    pub cost_report_bucket: Option<String>,
    
    /// Report granularity: DAILY, MONTHLY
    #[serde(default = "default_granularity")]
    pub report_granularity: String,
}

fn default_cost_tag() -> String { "grey:tenant-id".to_string() }
fn default_granularity() -> String { "DAILY".to_string() }

impl Default for CostMonitoringConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            cost_allocation_tag: default_cost_tag(),
            budget_alert_threshold: None,
            budget_alert_topic_arn: None,
            cost_report_bucket: None,
            report_granularity: default_granularity(),
        }
    }
}

/// IAM configuration.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct IAMConfig {
    /// Use instance profile/IRSA for authentication
    #[serde(default = "default_true")]
    pub use_instance_profile: bool,
    
    /// Explicit role ARN (overrides instance profile)
    pub role_arn: Option<String>,
    
    /// External ID for cross-account access
    pub external_id: Option<String>,
    
    /// Session duration in seconds
    #[serde(default = "default_session_duration")]
    pub session_duration_seconds: u64,
}

fn default_session_duration() -> u64 { 3600 }

// ============================================================================
// AWS Adapter
// ============================================================================

/// Main AWS cloud adapter.
///
/// # Thread Safety
///
/// All AWS SDK clients are internally thread-safe.
/// The adapter can be shared across async tasks.
pub struct AWSAdapter {
    config: AWSConfig,
    
    /// AWS SDK configuration
    sdk_config: SdkConfig,
    
    /// S3 client
    s3: S3Client,
    
    /// DynamoDB client
    dynamodb: DynamoDBClient,
    
    /// CloudWatch client
    cloudwatch: CloudWatchClient,
    
    /// Cost Explorer client
    cost_explorer: CostExplorerClient,
    
    /// EKS client (optional)
    eks: Option<EKSClient>,
    
    /// Regional clients for multi-region
    regional_s3_clients: Arc<RwLock<HashMap<String, S3Client>>>,
    
    /// Cost tracking state
    cost_tracker: Arc<RwLock<CostTracker>>,
    
    /// Metrics
    metrics: Arc<AWSMetrics>,
}

/// Tracks costs for Grey operations.
struct CostTracker {
    /// Cost by tenant
    tenant_costs: HashMap<String, TenantCost>,
    
    /// Last update time
    last_updated: SystemTime,
    
    /// Monthly budget remaining
    budget_remaining: Option<f64>,
}

#[derive(Debug, Clone, Default)]
struct TenantCost {
    /// Storage cost (USD)
    storage: f64,
    /// Compute cost (USD)
    compute: f64,
    /// Data transfer cost (USD)
    data_transfer: f64,
    /// Total cost (USD)
    total: f64,
    /// Cost period start
    period_start: Option<SystemTime>,
}

pub struct AWSMetrics {
    s3_operations: std::sync::atomic::AtomicU64,
    dynamodb_operations: std::sync::atomic::AtomicU64,
    bytes_stored: std::sync::atomic::AtomicU64,
    bytes_transferred: std::sync::atomic::AtomicU64,
    api_errors: std::sync::atomic::AtomicU64,
}

impl AWSAdapter {
    /// Create a new AWS adapter.
    #[instrument(skip_all, name = "aws_adapter_new")]
    pub async fn new(config: AWSConfig) -> Result<Self, AWSError> {
        // Load AWS SDK configuration
        let sdk_config = aws_config::defaults(aws_config::BehaviorVersion::latest())
            .region(aws_config::Region::new(config.primary_region.clone()))
            .load()
            .await;
        
        // Create clients
        let s3 = S3Client::new(&sdk_config);
        let dynamodb = DynamoDBClient::new(&sdk_config);
        let cloudwatch = CloudWatchClient::new(&sdk_config);
        let cost_explorer = CostExplorerClient::new(&sdk_config);
        
        let eks = if config.eks.is_some() {
            Some(EKSClient::new(&sdk_config))
        } else {
            None
        };
        
        info!(
            region = %config.primary_region,
            replica_regions = ?config.replica_regions,
            "AWS adapter initialized"
        );
        
        Ok(Self {
            config,
            sdk_config,
            s3,
            dynamodb,
            cloudwatch,
            cost_explorer,
            eks,
            regional_s3_clients: Arc::new(RwLock::new(HashMap::new())),
            cost_tracker: Arc::new(RwLock::new(CostTracker {
                tenant_costs: HashMap::new(),
                last_updated: SystemTime::now(),
                budget_remaining: None,
            })),
            metrics: Arc::new(AWSMetrics::new()),
        })
    }
    
    // ========================================================================
    // S3 Storage Operations
    // ========================================================================
    
    /// Store a task artifact in S3.
    #[instrument(skip(self, data), fields(bucket, key))]
    pub async fn store_artifact(
        &self,
        tenant_id: &str,
        task_id: &str,
        artifact_type: &str,
        data: Vec<u8>,
    ) -> Result<String, AWSError> {
        let bucket = &self.config.s3.artifacts_bucket;
        let key = format!("{}/{}/{}", tenant_id, task_id, artifact_type);
        
        tracing::Span::current().record("bucket", bucket);
        tracing::Span::current().record("key", &key);
        
        let mut request = self.s3.put_object()
            .bucket(bucket)
            .key(&key)
            .body(data.clone().into())
            .storage_class(self.config.s3.storage_class.clone().into())
            .tagging(format!(
                "grey:tenant-id={}&grey:task-id={}",
                tenant_id, task_id
            ));
        
        // Add encryption
        if self.config.s3.enable_encryption {
            request = request.server_side_encryption(aws_sdk_s3::types::ServerSideEncryption::AwsKms);
            if let Some(ref kms_key) = self.config.s3.kms_key_id {
                request = request.ssekms_key_id(kms_key);
            }
        }
        
        request.send().await
            .map_err(|e| AWSError::S3Operation(e.to_string()))?;
        
        self.metrics.s3_operations.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.metrics.bytes_stored.fetch_add(data.len() as u64, std::sync::atomic::Ordering::Relaxed);
        
        debug!(bucket, key, size = data.len(), "Stored artifact");
        
        Ok(format!("s3://{}/{}", bucket, key))
    }
    
    /// Retrieve a task artifact from S3.
    #[instrument(skip(self), fields(bucket, key))]
    pub async fn get_artifact(
        &self,
        tenant_id: &str,
        task_id: &str,
        artifact_type: &str,
    ) -> Result<Vec<u8>, AWSError> {
        let bucket = &self.config.s3.artifacts_bucket;
        let key = format!("{}/{}/{}", tenant_id, task_id, artifact_type);
        
        let response = self.s3.get_object()
            .bucket(bucket)
            .key(&key)
            .send()
            .await
            .map_err(|e| AWSError::S3Operation(e.to_string()))?;
        
        let data = response.body.collect().await
            .map_err(|e| AWSError::S3Operation(e.to_string()))?
            .into_bytes()
            .to_vec();
        
        self.metrics.s3_operations.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.metrics.bytes_transferred.fetch_add(data.len() as u64, std::sync::atomic::Ordering::Relaxed);
        
        Ok(data)
    }
    
    /// Store proof artifact with multi-region replication.
    #[instrument(skip(self, proof_data))]
    pub async fn store_proof(
        &self,
        tenant_id: &str,
        task_id: &str,
        proof_data: &ProofArtifact,
    ) -> Result<String, AWSError> {
        let bucket = &self.config.s3.proofs_bucket;
        let key = format!("{}/{}/proof.json", tenant_id, task_id);
        
        let data = serde_json::to_vec(proof_data)
            .map_err(|e| AWSError::Serialization(e.to_string()))?;
        
        // Store in primary region
        self.s3.put_object()
            .bucket(bucket)
            .key(&key)
            .body(data.clone().into())
            .content_type("application/json")
            .server_side_encryption(aws_sdk_s3::types::ServerSideEncryption::AwsKms)
            .tagging(format!(
                "grey:tenant-id={}&grey:task-id={}&grey:artifact-type=proof",
                tenant_id, task_id
            ))
            .send()
            .await
            .map_err(|e| AWSError::S3Operation(e.to_string()))?;
        
        // Multi-region replication is handled by S3 CRR configuration
        // Alternatively, we could explicitly copy to replica regions here
        
        info!(bucket, key, "Stored proof artifact");
        
        Ok(format!("s3://{}/{}", bucket, key))
    }
    
    // ========================================================================
    // DynamoDB State Operations
    // ========================================================================
    
    /// Store task state in DynamoDB.
    #[instrument(skip(self, state))]
    pub async fn store_task_state(
        &self,
        tenant_id: &str,
        task_id: &str,
        state: &TaskState,
    ) -> Result<(), AWSError> {
        let table = &self.config.dynamodb.task_table;
        
        let mut item = HashMap::new();
        item.insert("pk".to_string(),
            aws_sdk_dynamodb::types::AttributeValue::S(format!("TENANT#{}", tenant_id)));
        item.insert("sk".to_string(),
            aws_sdk_dynamodb::types::AttributeValue::S(format!("TASK#{}", task_id)));
        item.insert("task_id".to_string(),
            aws_sdk_dynamodb::types::AttributeValue::S(task_id.to_string()));
        item.insert("tenant_id".to_string(),
            aws_sdk_dynamodb::types::AttributeValue::S(tenant_id.to_string()));
        item.insert("status".to_string(),
            aws_sdk_dynamodb::types::AttributeValue::S(state.status.clone()));
        item.insert("created_at".to_string(),
            aws_sdk_dynamodb::types::AttributeValue::N(state.created_at.to_string()));
        item.insert("updated_at".to_string(),
            aws_sdk_dynamodb::types::AttributeValue::N(state.updated_at.to_string()));
        
        if let Some(ref result) = state.result {
            item.insert("result".to_string(),
                aws_sdk_dynamodb::types::AttributeValue::S(result.to_string()));
        }
        
        // Add TTL if configured
        if let Some(ref ttl_attr) = self.config.dynamodb.ttl_attribute {
            let ttl = state.updated_at + 86400 * 30; // 30 days
            item.insert(ttl_attr.clone(),
                aws_sdk_dynamodb::types::AttributeValue::N(ttl.to_string()));
        }
        
        self.dynamodb.put_item()
            .table_name(table)
            .set_item(Some(item))
            .send()
            .await
            .map_err(|e| AWSError::DynamoDBOperation(e.to_string()))?;
        
        self.metrics.dynamodb_operations.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        
        debug!(table, task_id, "Stored task state");
        Ok(())
    }
    
    /// Get task state from DynamoDB.
    #[instrument(skip(self))]
    pub async fn get_task_state(
        &self,
        tenant_id: &str,
        task_id: &str,
    ) -> Result<Option<TaskState>, AWSError> {
        let table = &self.config.dynamodb.task_table;
        
        let pk = format!("TENANT#{}", tenant_id);
        let sk = format!("TASK#{}", task_id);
        
        let response = self.dynamodb.get_item()
            .table_name(table)
            .key("pk", aws_sdk_dynamodb::types::AttributeValue::S(pk))
            .key("sk", aws_sdk_dynamodb::types::AttributeValue::S(sk))
            .send()
            .await
            .map_err(|e| AWSError::DynamoDBOperation(e.to_string()))?;
        
        self.metrics.dynamodb_operations.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        
        if let Some(item) = response.item {
            let state = TaskState {
                task_id: item.get("task_id")
                    .and_then(|v| v.as_s().ok())
                    .map(String::from)
                    .unwrap_or_default(),
                tenant_id: item.get("tenant_id")
                    .and_then(|v| v.as_s().ok())
                    .map(String::from)
                    .unwrap_or_default(),
                status: item.get("status")
                    .and_then(|v| v.as_s().ok())
                    .map(String::from)
                    .unwrap_or_default(),
                created_at: item.get("created_at")
                    .and_then(|v| v.as_n().ok())
                    .and_then(|n| n.parse().ok())
                    .unwrap_or(0),
                updated_at: item.get("updated_at")
                    .and_then(|v| v.as_n().ok())
                    .and_then(|n| n.parse().ok())
                    .unwrap_or(0),
                result: item.get("result")
                    .and_then(|v| v.as_s().ok())
                    .and_then(|s| serde_json::from_str(s).ok()),
            };
            Ok(Some(state))
        } else {
            Ok(None)
        }
    }
    
    /// Query tasks by tenant.
    #[instrument(skip(self))]
    pub async fn query_tenant_tasks(
        &self,
        tenant_id: &str,
        status_filter: Option<&str>,
        limit: Option<i32>,
    ) -> Result<Vec<TaskState>, AWSError> {
        let table = &self.config.dynamodb.task_table;
        let pk = format!("TENANT#{}", tenant_id);
        
        let mut query = self.dynamodb.query()
            .table_name(table)
            .key_condition_expression("pk = :pk AND begins_with(sk, :sk_prefix)")
            .expression_attribute_values(":pk", 
                aws_sdk_dynamodb::types::AttributeValue::S(pk))
            .expression_attribute_values(":sk_prefix",
                aws_sdk_dynamodb::types::AttributeValue::S("TASK#".to_string()));
        
        if let Some(status) = status_filter {
            query = query
                .filter_expression("#status = :status")
                .expression_attribute_names("#status", "status")
                .expression_attribute_values(":status",
                    aws_sdk_dynamodb::types::AttributeValue::S(status.to_string()));
        }
        
        if let Some(limit) = limit {
            query = query.limit(limit);
        }
        
        let response = query.send().await
            .map_err(|e| AWSError::DynamoDBOperation(e.to_string()))?;
        
        self.metrics.dynamodb_operations.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        
        let tasks = response.items.unwrap_or_default()
            .into_iter()
            .filter_map(|item| {
                Some(TaskState {
                    task_id: item.get("task_id")?.as_s().ok()?.to_string(),
                    tenant_id: item.get("tenant_id")?.as_s().ok()?.to_string(),
                    status: item.get("status")?.as_s().ok()?.to_string(),
                    created_at: item.get("created_at")?.as_n().ok()?.parse().ok()?,
                    updated_at: item.get("updated_at")?.as_n().ok()?.parse().ok()?,
                    result: item.get("result")
                        .and_then(|v| v.as_s().ok())
                        .and_then(|s| serde_json::from_str(s).ok()),
                })
            })
            .collect();
        
        Ok(tasks)
    }
    
    // ========================================================================
    // CloudWatch Metrics
    // ========================================================================
    
    /// Publish Grey metrics to CloudWatch.
    #[instrument(skip(self, metrics))]
    pub async fn publish_metrics(&self, metrics: &GreyMetrics) -> Result<(), AWSError> {
        let namespace = &self.config.cloudwatch.metric_namespace;
        
        let mut metric_data = Vec::new();
        
        // Task metrics
        metric_data.push(aws_sdk_cloudwatch::types::MetricDatum::builder()
            .metric_name("TasksSubmitted")
            .value(metrics.tasks_submitted as f64)
            .unit(aws_sdk_cloudwatch::types::StandardUnit::Count)
            .dimensions(aws_sdk_cloudwatch::types::Dimension::builder()
                .name("TenantId")
                .value(&metrics.tenant_id)
                .build())
            .build());
        
        metric_data.push(aws_sdk_cloudwatch::types::MetricDatum::builder()
            .metric_name("TasksCompleted")
            .value(metrics.tasks_completed as f64)
            .unit(aws_sdk_cloudwatch::types::StandardUnit::Count)
            .dimensions(aws_sdk_cloudwatch::types::Dimension::builder()
                .name("TenantId")
                .value(&metrics.tenant_id)
                .build())
            .build());
        
        metric_data.push(aws_sdk_cloudwatch::types::MetricDatum::builder()
            .metric_name("TasksFailed")
            .value(metrics.tasks_failed as f64)
            .unit(aws_sdk_cloudwatch::types::StandardUnit::Count)
            .dimensions(aws_sdk_cloudwatch::types::Dimension::builder()
                .name("TenantId")
                .value(&metrics.tenant_id)
                .build())
            .build());
        
        metric_data.push(aws_sdk_cloudwatch::types::MetricDatum::builder()
            .metric_name("AverageLatencyMs")
            .value(metrics.avg_latency_ms)
            .unit(aws_sdk_cloudwatch::types::StandardUnit::Milliseconds)
            .dimensions(aws_sdk_cloudwatch::types::Dimension::builder()
                .name("TenantId")
                .value(&metrics.tenant_id)
                .build())
            .build());
        
        self.cloudwatch.put_metric_data()
            .namespace(namespace)
            .set_metric_data(Some(metric_data))
            .send()
            .await
            .map_err(|e| AWSError::CloudWatchOperation(e.to_string()))?;
        
        debug!(namespace, "Published metrics to CloudWatch");
        Ok(())
    }
    
    // ========================================================================
    // Cost Monitoring
    // ========================================================================
    
    /// Get cost breakdown by tenant.
    ///
    /// # Cost Awareness
    ///
    /// This method queries Cost Explorer to get accurate cost attribution.
    /// Uses cost allocation tags to break down costs by tenant.
    #[instrument(skip(self))]
    pub async fn get_tenant_costs(
        &self,
        tenant_id: &str,
        start_date: &str,
        end_date: &str,
    ) -> Result<TenantCostReport, AWSError> {
        if !self.config.cost_monitoring.enabled {
            return Err(AWSError::CostMonitoringDisabled);
        }
        
        let response = self.cost_explorer.get_cost_and_usage()
            .time_period(aws_sdk_costexplorer::types::DateInterval::builder()
                .start(start_date)
                .end(end_date)
                .build()
                .map_err(|e| AWSError::CostExplorerOperation(e.to_string()))?)
            .granularity(aws_sdk_costexplorer::types::Granularity::Daily)
            .filter(aws_sdk_costexplorer::types::Expression::builder()
                .tags(aws_sdk_costexplorer::types::TagValues::builder()
                    .key(&self.config.cost_monitoring.cost_allocation_tag)
                    .values(tenant_id)
                    .build())
                .build())
            .metrics("UnblendedCost")
            .group_by(aws_sdk_costexplorer::types::GroupDefinition::builder()
                .r#type(aws_sdk_costexplorer::types::GroupDefinitionType::Dimension)
                .key("SERVICE")
                .build())
            .send()
            .await
            .map_err(|e| AWSError::CostExplorerOperation(e.to_string()))?;
        
        let mut report = TenantCostReport {
            tenant_id: tenant_id.to_string(),
            start_date: start_date.to_string(),
            end_date: end_date.to_string(),
            total_cost: 0.0,
            by_service: HashMap::new(),
            currency: "USD".to_string(),
        };
        
        for result in response.results_by_time.unwrap_or_default() {
            for group in result.groups.unwrap_or_default() {
                let service = group.keys.as_ref()
                    .and_then(|k| k.first())
                    .cloned()
                    .unwrap_or_default();
                
                let amount = group.metrics.as_ref()
                    .and_then(|m| m.get("UnblendedCost"))
                    .and_then(|v| v.amount.as_ref())
                    .and_then(|a| a.parse::<f64>().ok())
                    .unwrap_or(0.0);
                
                *report.by_service.entry(service).or_insert(0.0) += amount;
                report.total_cost += amount;
            }
        }
        
        info!(tenant_id, total_cost = report.total_cost, "Retrieved tenant costs");
        Ok(report)
    }
    
    /// Check budget status and trigger alerts if needed.
    #[instrument(skip(self))]
    pub async fn check_budget_status(&self) -> Result<BudgetStatus, AWSError> {
        if !self.config.cost_monitoring.enabled {
            return Err(AWSError::CostMonitoringDisabled);
        }
        
        let threshold = self.config.cost_monitoring.budget_alert_threshold
            .ok_or(AWSError::BudgetNotConfigured)?;
        
        // Get current month's costs
        let now = chrono::Utc::now();
        let start_date = format!("{}-{:02}-01", now.year(), now.month());
        let end_date = now.format("%Y-%m-%d").to_string();
        
        let response = self.cost_explorer.get_cost_and_usage()
            .time_period(aws_sdk_costexplorer::types::DateInterval::builder()
                .start(&start_date)
                .end(&end_date)
                .build()
                .map_err(|e| AWSError::CostExplorerOperation(e.to_string()))?)
            .granularity(aws_sdk_costexplorer::types::Granularity::Monthly)
            .metrics("UnblendedCost")
            .send()
            .await
            .map_err(|e| AWSError::CostExplorerOperation(e.to_string()))?;
        
        let current_cost = response.results_by_time.unwrap_or_default()
            .first()
            .and_then(|r| r.total.as_ref())
            .and_then(|t| t.get("UnblendedCost"))
            .and_then(|v| v.amount.as_ref())
            .and_then(|a| a.parse::<f64>().ok())
            .unwrap_or(0.0);
        
        let percentage = (current_cost / threshold) * 100.0;
        let exceeded = current_cost > threshold;
        
        let status = BudgetStatus {
            current_cost,
            budget_limit: threshold,
            percentage_used: percentage,
            exceeded,
            forecast_end_of_month: current_cost * 30.0 / now.day() as f64,
        };
        
        if exceeded {
            warn!(
                current_cost,
                threshold,
                "Budget threshold exceeded!"
            );
        }
        
        Ok(status)
    }
    
    // ========================================================================
    // EKS Operations
    // ========================================================================
    
    /// Get EKS cluster status.
    #[instrument(skip(self))]
    pub async fn get_cluster_status(&self) -> Result<ClusterStatus, AWSError> {
        let eks = self.eks.as_ref()
            .ok_or(AWSError::EKSNotConfigured)?;
        
        let cluster_name = self.config.eks.as_ref()
            .map(|c| c.cluster_name.clone())
            .ok_or(AWSError::EKSNotConfigured)?;
        
        let response = eks.describe_cluster()
            .name(&cluster_name)
            .send()
            .await
            .map_err(|e| AWSError::EKSOperation(e.to_string()))?;
        
        let cluster = response.cluster
            .ok_or(AWSError::EKSOperation("Cluster not found".to_string()))?;
        
        let status = ClusterStatus {
            name: cluster.name.unwrap_or_default(),
            status: cluster.status.map(|s| s.as_str().to_string()).unwrap_or_default(),
            version: cluster.version.unwrap_or_default(),
            endpoint: cluster.endpoint.unwrap_or_default(),
            platform_version: cluster.platform_version.unwrap_or_default(),
        };
        
        Ok(status)
    }
    
    /// Export adapter metrics.
    pub fn get_metrics(&self) -> HashMap<String, u64> {
        let mut metrics = HashMap::new();
        metrics.insert(
            "aws_s3_operations_total".to_string(),
            self.metrics.s3_operations.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics.insert(
            "aws_dynamodb_operations_total".to_string(),
            self.metrics.dynamodb_operations.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics.insert(
            "aws_bytes_stored_total".to_string(),
            self.metrics.bytes_stored.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics.insert(
            "aws_bytes_transferred_total".to_string(),
            self.metrics.bytes_transferred.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics.insert(
            "aws_api_errors_total".to_string(),
            self.metrics.api_errors.load(std::sync::atomic::Ordering::Relaxed),
        );
        metrics
    }
}

impl AWSMetrics {
    fn new() -> Self {
        Self {
            s3_operations: std::sync::atomic::AtomicU64::new(0),
            dynamodb_operations: std::sync::atomic::AtomicU64::new(0),
            bytes_stored: std::sync::atomic::AtomicU64::new(0),
            bytes_transferred: std::sync::atomic::AtomicU64::new(0),
            api_errors: std::sync::atomic::AtomicU64::new(0),
        }
    }
}

// ============================================================================
// Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskState {
    pub task_id: String,
    pub tenant_id: String,
    pub status: String,
    pub created_at: u64,
    pub updated_at: u64,
    pub result: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofArtifact {
    pub platform: String,
    pub mrenclave: String,
    pub mrsigner: String,
    pub signature: String,
    pub timestamp: u64,
}

#[derive(Debug, Clone)]
pub struct GreyMetrics {
    pub tenant_id: String,
    pub tasks_submitted: u64,
    pub tasks_completed: u64,
    pub tasks_failed: u64,
    pub avg_latency_ms: f64,
}

#[derive(Debug, Clone, Serialize)]
pub struct TenantCostReport {
    pub tenant_id: String,
    pub start_date: String,
    pub end_date: String,
    pub total_cost: f64,
    pub by_service: HashMap<String, f64>,
    pub currency: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct BudgetStatus {
    pub current_cost: f64,
    pub budget_limit: f64,
    pub percentage_used: f64,
    pub exceeded: bool,
    pub forecast_end_of_month: f64,
}

#[derive(Debug, Clone, Serialize)]
pub struct ClusterStatus {
    pub name: String,
    pub status: String,
    pub version: String,
    pub endpoint: String,
    pub platform_version: String,
}

// ============================================================================
// Errors
// ============================================================================

#[derive(Debug, thiserror::Error)]
pub enum AWSError {
    #[error("S3 operation failed: {0}")]
    S3Operation(String),
    
    #[error("DynamoDB operation failed: {0}")]
    DynamoDBOperation(String),
    
    #[error("CloudWatch operation failed: {0}")]
    CloudWatchOperation(String),
    
    #[error("Cost Explorer operation failed: {0}")]
    CostExplorerOperation(String),
    
    #[error("EKS operation failed: {0}")]
    EKSOperation(String),
    
    #[error("Serialization error: {0}")]
    Serialization(String),
    
    #[error("Cost monitoring is disabled")]
    CostMonitoringDisabled,
    
    #[error("Budget not configured")]
    BudgetNotConfigured,
    
    #[error("EKS not configured")]
    EKSNotConfigured,
}

// Use chrono for date handling
use chrono::{Datelike, Utc};

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_task_state_key_generation() {
        let tenant_id = "tenant-123";
        let task_id = "task-456";
        
        let pk = format!("TENANT#{}", tenant_id);
        let sk = format!("TASK#{}", task_id);
        
        assert_eq!(pk, "TENANT#tenant-123");
        assert_eq!(sk, "TASK#task-456");
    }
    
    #[test]
    fn test_s3_key_generation() {
        let tenant_id = "tenant-123";
        let task_id = "task-456";
        let artifact_type = "result";
        
        let key = format!("{}/{}/{}", tenant_id, task_id, artifact_type);
        assert_eq!(key, "tenant-123/task-456/result");
    }
}
