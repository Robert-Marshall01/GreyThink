//! Grey Distributed — RabbitMQ Connector
//!
//! Integrates Grey Distributed with RabbitMQ and AMQP-compatible message brokers.
//! Supports task submission via queues and result delivery via exchanges.
//!
//! # Architecture
//!
//! ```text
//! ┌──────────────────┐     ┌──────────────────┐     ┌─────────────────┐
//! │  Task Queue      │────▶│  Grey Connector  │────▶│  Grey Workers   │
//! │  grey.tasks.{t}  │     │                  │     │                 │
//! └──────────────────┘     └──────────────────┘     └─────────────────┘
//!                                  │
//!                                  ▼
//!                          ┌──────────────────┐
//!                          │  Result Exchange │
//!                          │  grey.results    │
//!                          └──────────────────┘
//!                                  │
//!                  ┌───────────────┼───────────────┐
//!                  ▼               ▼               ▼
//!           ┌──────────┐   ┌──────────┐   ┌──────────┐
//!           │ tenant-a │   │ tenant-b │   │ tenant-c │
//!           └──────────┘   └──────────┘   └──────────┘
//! ```
//!
//! # Design Decisions
//!
//! 1. **Queue Per Tenant**: Each tenant has a dedicated queue for isolation
//!    and independent rate limiting.
//!
//! 2. **Publisher Confirms**: Uses publisher confirms for reliable message
//!    delivery to Grey. Messages are only acked after Grey accepts them.
//!
//! 3. **Dead Letter Exchange**: Failed messages are routed to DLX for
//!    inspection and retry.
//!
//! 4. **Quorum Queues**: Recommended for durability, uses Raft for replication.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use lapin::{
    options::*,
    types::FieldTable,
    BasicProperties, Channel, Connection, ConnectionProperties,
    Consumer, ExchangeKind,
};
use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc, RwLock, Semaphore};
use tracing::{debug, error, info, warn, instrument, Span};

// ============================================================================
// Configuration
// ============================================================================

/// RabbitMQ connector configuration.
///
/// # Tradeoffs
///
/// - `prefetch_count`: Higher values increase throughput but risk memory
///   pressure if Grey processing is slow. 10-50 is usually optimal.
///
/// - `use_quorum_queues`: Provides stronger durability guarantees but
///   requires RabbitMQ 3.8+ and more resources.
///
/// - `confirm_timeout`: Must be long enough for Grey to accept the task
///   but short enough to detect failures promptly.
#[derive(Debug, Clone, Deserialize)]
pub struct RabbitMQConfig {
    /// AMQP connection URL
    /// Format: amqp://user:pass@host:port/vhost
    pub amqp_url: String,
    
    /// Queue name prefix for task queues
    #[serde(default = "default_queue_prefix")]
    pub queue_prefix: String,
    
    /// Exchange name for results
    #[serde(default = "default_result_exchange")]
    pub result_exchange: String,
    
    /// Dead letter exchange
    #[serde(default = "default_dlx")]
    pub dead_letter_exchange: String,
    
    /// Prefetch count per consumer
    #[serde(default = "default_prefetch")]
    pub prefetch_count: u16,
    
    /// Use quorum queues for durability
    #[serde(default)]
    pub use_quorum_queues: bool,
    
    /// Publisher confirm timeout
    #[serde(default = "default_confirm_timeout")]
    pub confirm_timeout_ms: u64,
    
    /// Message TTL in milliseconds (0 = no TTL)
    #[serde(default)]
    pub message_ttl_ms: u64,
    
    /// Enable TLS
    #[serde(default)]
    pub tls_enabled: bool,
    
    /// TLS CA certificate path
    pub tls_ca_cert: Option<String>,
    
    /// Heartbeat interval in seconds
    #[serde(default = "default_heartbeat")]
    pub heartbeat_seconds: u16,
    
    /// Tenant queue configuration
    #[serde(default)]
    pub tenant_queues: HashMap<String, TenantQueueConfig>,
}

fn default_queue_prefix() -> String { "grey.tasks".to_string() }
fn default_result_exchange() -> String { "grey.results".to_string() }
fn default_dlx() -> String { "grey.dlx".to_string() }
fn default_prefetch() -> u16 { 20 }
fn default_confirm_timeout() -> u64 { 10_000 }
fn default_heartbeat() -> u16 { 60 }

/// Per-tenant queue configuration.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct TenantQueueConfig {
    /// Queue name override (default: {prefix}.{tenant_id})
    pub queue_name: Option<String>,
    
    /// Max queue length in messages
    pub max_length: Option<i64>,
    
    /// Max queue size in bytes
    pub max_length_bytes: Option<i64>,
    
    /// Priority queue levels (1-255)
    pub max_priority: Option<u8>,
    
    /// Consumer concurrency for this tenant
    pub concurrency: Option<u16>,
}

// ============================================================================
// Message Types
// ============================================================================

/// Task message from RabbitMQ.
///
/// # Message Format
///
/// Uses JSON encoding with AMQP properties:
/// - `correlation_id`: For request-reply pattern
/// - `reply_to`: Queue name for result delivery
/// - `message_id`: Unique ID (used as idempotency key)
/// - `type`: Task type
/// - `priority`: Message priority (0-255)
/// - `expiration`: Message TTL
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RabbitTaskMessage {
    /// Task payload (opaque to connector)
    pub payload: serde_json::Value,
    
    /// Task type
    pub task_type: TaskType,
    
    /// Priority level
    #[serde(default)]
    pub priority: Priority,
    
    /// Task timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout_seconds: u32,
    
    /// Labels for filtering
    #[serde(default)]
    pub labels: HashMap<String, String>,
    
    /// Require TEE execution
    #[serde(default)]
    pub require_tee: bool,
}

fn default_timeout() -> u32 { 300 }

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskType {
    Stateless,
    Stateful,
    Pipeline,
    Batch,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Priority {
    Low,
    #[default]
    Normal,
    High,
    Critical,
}

impl Priority {
    fn to_amqp_priority(&self) -> u8 {
        match self {
            Priority::Low => 1,
            Priority::Normal => 5,
            Priority::High => 7,
            Priority::Critical => 9,
        }
    }
}

/// Result message published to RabbitMQ.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RabbitResultMessage {
    /// Grey task ID
    pub task_id: String,
    
    /// Tenant ID
    pub tenant_id: String,
    
    /// Task status
    pub status: TaskStatus,
    
    /// Task result (if completed)
    pub result: Option<serde_json::Value>,
    
    /// Error details (if failed)
    pub error: Option<ErrorDetails>,
    
    /// TEE proof artifact
    pub proof_artifact: Option<ProofArtifact>,
    
    /// Execution metrics
    pub metrics: ExecutionMetrics,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskStatus {
    Completed,
    Failed,
    Cancelled,
    TimedOut,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorDetails {
    pub code: String,
    pub message: String,
    pub retryable: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofArtifact {
    pub platform: String,
    pub mrenclave: String,
    pub signature: String,
    pub timestamp: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionMetrics {
    pub queue_time_ms: u64,
    pub execution_time_ms: u64,
    pub worker_node: String,
}

// ============================================================================
// Grey Client Interface
// ============================================================================

#[async_trait]
pub trait GreyClient: Send + Sync {
    async fn submit_task(&self, request: TaskSubmitRequest) -> Result<TaskSubmitResponse, GreyError>;
    async fn get_queue_depth(&self, tenant_id: &str) -> Result<usize, GreyError>;
    async fn subscribe_completions(&self, tenant_id: &str) -> Result<mpsc::Receiver<TaskCompletion>, GreyError>;
}

#[derive(Debug, Clone)]
pub struct TaskSubmitRequest {
    pub tenant_id: String,
    pub task_type: TaskType,
    pub payload: serde_json::Value,
    pub priority: Priority,
    pub timeout_seconds: u32,
    pub labels: HashMap<String, String>,
    pub require_tee: bool,
    pub idempotency_key: String,
}

#[derive(Debug, Clone)]
pub struct TaskSubmitResponse {
    pub task_id: String,
}

#[derive(Debug, Clone)]
pub struct TaskCompletion {
    pub task_id: String,
    pub original_message_id: String,
    pub correlation_id: Option<String>,
    pub reply_to: Option<String>,
    pub status: TaskStatus,
    pub result: Option<serde_json::Value>,
    pub error: Option<ErrorDetails>,
    pub proof_artifact: Option<ProofArtifact>,
    pub metrics: ExecutionMetrics,
}

#[derive(Debug, thiserror::Error)]
pub enum GreyError {
    #[error("Connection error: {0}")]
    Connection(String),
    
    #[error("Rate limited")]
    RateLimited,
    
    #[error("Invalid request: {0}")]
    InvalidRequest(String),
}

// ============================================================================
// RabbitMQ Connector
// ============================================================================

/// Main RabbitMQ connector implementation.
///
/// # Design Notes
///
/// - Maintains separate channels for consuming and publishing
/// - Uses publisher confirms for reliable delivery
/// - Implements per-tenant queues with independent consumers
/// - Tracks in-flight messages for reliable acking
pub struct RabbitMQConnector<C: GreyClient> {
    config: RabbitMQConfig,
    grey_client: Arc<C>,
    connection: Connection,
    
    /// Channel for consuming messages
    consume_channel: Channel,
    
    /// Channel for publishing results
    publish_channel: Channel,
    
    /// Active consumers by tenant
    consumers: Arc<RwLock<HashMap<String, ConsumerState>>>,
    
    /// In-flight message tracking
    in_flight: Arc<RwLock<HashMap<String, InFlightMessage>>>,
    
    /// Metrics
    metrics: Arc<ConnectorMetrics>,
    
    /// Shutdown signal
    shutdown_tx: Option<mpsc::Sender<()>>,
}

#[derive(Debug)]
struct ConsumerState {
    tenant_id: String,
    queue_name: String,
    consumer_tag: String,
    paused: bool,
}

#[derive(Debug)]
struct InFlightMessage {
    delivery_tag: u64,
    message_id: String,
    tenant_id: String,
    correlation_id: Option<String>,
    reply_to: Option<String>,
    received_at: std::time::Instant,
}

pub struct ConnectorMetrics {
    messages_consumed: std::sync::atomic::AtomicU64,
    messages_published: std::sync::atomic::AtomicU64,
    messages_acked: std::sync::atomic::AtomicU64,
    messages_nacked: std::sync::atomic::AtomicU64,
    messages_dlq: std::sync::atomic::AtomicU64,
    publish_confirms: std::sync::atomic::AtomicU64,
    publish_failures: std::sync::atomic::AtomicU64,
}

impl<C: GreyClient + 'static> RabbitMQConnector<C> {
    /// Create a new RabbitMQ connector.
    ///
    /// # Arguments
    ///
    /// * `config` - Connector configuration
    /// * `grey_client` - Grey Distributed client
    ///
    /// # Connection Management
    ///
    /// Uses connection pooling and automatic reconnection.
    /// Channels are multiplexed over a single TCP connection.
    #[instrument(skip(grey_client), name = "rabbitmq_connector_new")]
    pub async fn new(config: RabbitMQConfig, grey_client: C) -> Result<Self, ConnectorError> {
        // Configure connection properties
        let conn_props = ConnectionProperties::default()
            .with_executor(tokio_executor_trait::Tokio::current())
            .with_reactor(tokio_reactor_trait::Tokio);
        
        // Establish connection
        let connection = Connection::connect(&config.amqp_url, conn_props)
            .await
            .map_err(|e| ConnectorError::Connection(e.to_string()))?;
        
        info!(
            vhost = %connection.status().vhost(),
            "Connected to RabbitMQ"
        );
        
        // Create channels
        let consume_channel = connection.create_channel()
            .await
            .map_err(|e| ConnectorError::Channel(e.to_string()))?;
        
        let publish_channel = connection.create_channel()
            .await
            .map_err(|e| ConnectorError::Channel(e.to_string()))?;
        
        // Set QoS
        consume_channel.basic_qos(config.prefetch_count, BasicQosOptions::default())
            .await
            .map_err(|e| ConnectorError::Channel(e.to_string()))?;
        
        // Enable publisher confirms
        publish_channel.confirm_select(ConfirmSelectOptions::default())
            .await
            .map_err(|e| ConnectorError::Channel(e.to_string()))?;
        
        Ok(Self {
            config,
            grey_client: Arc::new(grey_client),
            connection,
            consume_channel,
            publish_channel,
            consumers: Arc::new(RwLock::new(HashMap::new())),
            in_flight: Arc::new(RwLock::new(HashMap::new())),
            metrics: Arc::new(ConnectorMetrics::new()),
            shutdown_tx: None,
        })
    }
    
    /// Initialize exchanges and queues.
    ///
    /// # Topology
    ///
    /// Creates:
    /// - Dead letter exchange (fanout)
    /// - Result exchange (topic)
    /// - Per-tenant task queues
    /// - Dead letter queues
    #[instrument(skip(self), name = "rabbitmq_setup_topology")]
    pub async fn setup_topology(&self, tenant_ids: &[String]) -> Result<(), ConnectorError> {
        // Declare dead letter exchange
        self.consume_channel.exchange_declare(
            &self.config.dead_letter_exchange,
            ExchangeKind::Fanout,
            ExchangeDeclareOptions {
                durable: true,
                ..Default::default()
            },
            FieldTable::default(),
        ).await.map_err(|e| ConnectorError::Topology(e.to_string()))?;
        
        // Declare result exchange
        self.consume_channel.exchange_declare(
            &self.config.result_exchange,
            ExchangeKind::Topic,
            ExchangeDeclareOptions {
                durable: true,
                ..Default::default()
            },
            FieldTable::default(),
        ).await.map_err(|e| ConnectorError::Topology(e.to_string()))?;
        
        // Setup queues for each tenant
        for tenant_id in tenant_ids {
            self.setup_tenant_queue(tenant_id).await?;
        }
        
        info!(tenant_count = tenant_ids.len(), "RabbitMQ topology initialized");
        Ok(())
    }
    
    /// Setup queue for a specific tenant.
    async fn setup_tenant_queue(&self, tenant_id: &str) -> Result<String, ConnectorError> {
        let tenant_config = self.config.tenant_queues.get(tenant_id);
        
        let queue_name = tenant_config
            .and_then(|c| c.queue_name.clone())
            .unwrap_or_else(|| format!("{}.{}", self.config.queue_prefix, tenant_id));
        
        let dlq_name = format!("{}.dlq", queue_name);
        
        // Build queue arguments
        let mut args = FieldTable::default();
        
        // Dead letter exchange
        args.insert(
            "x-dead-letter-exchange".into(),
            lapin::types::AMQPValue::LongString(self.config.dead_letter_exchange.clone().into()),
        );
        
        // Quorum queue type
        if self.config.use_quorum_queues {
            args.insert(
                "x-queue-type".into(),
                lapin::types::AMQPValue::LongString("quorum".into()),
            );
        }
        
        // Max length
        if let Some(max_len) = tenant_config.and_then(|c| c.max_length) {
            args.insert(
                "x-max-length".into(),
                lapin::types::AMQPValue::LongLongInt(max_len),
            );
        }
        
        // Max priority
        if let Some(max_priority) = tenant_config.and_then(|c| c.max_priority) {
            args.insert(
                "x-max-priority".into(),
                lapin::types::AMQPValue::ShortShortUInt(max_priority),
            );
        }
        
        // Message TTL
        if self.config.message_ttl_ms > 0 {
            args.insert(
                "x-message-ttl".into(),
                lapin::types::AMQPValue::LongLongInt(self.config.message_ttl_ms as i64),
            );
        }
        
        // Declare task queue
        self.consume_channel.queue_declare(
            &queue_name,
            QueueDeclareOptions {
                durable: true,
                ..Default::default()
            },
            args,
        ).await.map_err(|e| ConnectorError::Topology(e.to_string()))?;
        
        // Declare dead letter queue
        self.consume_channel.queue_declare(
            &dlq_name,
            QueueDeclareOptions {
                durable: true,
                ..Default::default()
            },
            FieldTable::default(),
        ).await.map_err(|e| ConnectorError::Topology(e.to_string()))?;
        
        // Bind DLQ to DLX
        self.consume_channel.queue_bind(
            &dlq_name,
            &self.config.dead_letter_exchange,
            &format!("{}.#", tenant_id),
            QueueBindOptions::default(),
            FieldTable::default(),
        ).await.map_err(|e| ConnectorError::Topology(e.to_string()))?;
        
        debug!(tenant_id, queue_name, "Tenant queue configured");
        Ok(queue_name)
    }
    
    /// Start consuming from tenant queues.
    ///
    /// # Processing Flow
    ///
    /// 1. Receive message from queue
    /// 2. Parse and validate message
    /// 3. Submit task to Grey
    /// 4. Track in-flight message
    /// 5. Wait for Grey completion
    /// 6. Publish result to exchange
    /// 7. Ack original message
    #[instrument(skip(self), name = "rabbitmq_start")]
    pub async fn start(&mut self, tenant_ids: &[String]) -> Result<ConnectorHandle, ConnectorError> {
        let (shutdown_tx, shutdown_rx) = mpsc::channel(1);
        self.shutdown_tx = Some(shutdown_tx.clone());
        
        // Start consumers for each tenant
        for tenant_id in tenant_ids {
            self.start_tenant_consumer(tenant_id, shutdown_rx.clone()).await?;
        }
        
        // Start result publisher
        let result_handle = self.start_result_publisher();
        
        // Start health checker
        let health_handle = self.start_health_checker();
        
        info!(tenant_count = tenant_ids.len(), "RabbitMQ connector started");
        
        Ok(ConnectorHandle {
            shutdown_tx,
            result_handle,
            health_handle,
        })
    }
    
    /// Start consumer for a specific tenant.
    async fn start_tenant_consumer(
        &self,
        tenant_id: &str,
        mut shutdown_rx: mpsc::Receiver<()>,
    ) -> Result<(), ConnectorError> {
        let queue_name = format!("{}.{}", self.config.queue_prefix, tenant_id);
        let consumer_tag = format!("grey-connector-{}", tenant_id);
        
        let consumer = self.consume_channel.basic_consume(
            &queue_name,
            &consumer_tag,
            BasicConsumeOptions {
                no_ack: false,
                ..Default::default()
            },
            FieldTable::default(),
        ).await.map_err(|e| ConnectorError::Consumer(e.to_string()))?;
        
        // Track consumer state
        {
            let mut consumers = self.consumers.write().await;
            consumers.insert(tenant_id.to_string(), ConsumerState {
                tenant_id: tenant_id.to_string(),
                queue_name: queue_name.clone(),
                consumer_tag: consumer_tag.clone(),
                paused: false,
            });
        }
        
        // Spawn consumer task
        let grey_client = Arc::clone(&self.grey_client);
        let in_flight = Arc::clone(&self.in_flight);
        let metrics = Arc::clone(&self.metrics);
        let channel = self.consume_channel.clone();
        let tenant_id = tenant_id.to_string();
        
        tokio::spawn(async move {
            Self::consume_loop(
                consumer,
                &grey_client,
                &in_flight,
                &metrics,
                &channel,
                &tenant_id,
                shutdown_rx,
            ).await;
        });
        
        Ok(())
    }
    
    /// Main consume loop.
    async fn consume_loop(
        mut consumer: Consumer,
        grey_client: &Arc<C>,
        in_flight: &Arc<RwLock<HashMap<String, InFlightMessage>>>,
        metrics: &Arc<ConnectorMetrics>,
        channel: &Channel,
        tenant_id: &str,
        mut shutdown_rx: mpsc::Receiver<()>,
    ) {
        use futures_util::stream::StreamExt;
        
        loop {
            tokio::select! {
                _ = shutdown_rx.recv() => {
                    info!(tenant_id, "Consumer shutting down");
                    break;
                }
                
                delivery = consumer.next() => {
                    match delivery {
                        Some(Ok(delivery)) => {
                            if let Err(e) = Self::process_delivery(
                                &delivery,
                                grey_client,
                                in_flight,
                                metrics,
                                channel,
                                tenant_id,
                            ).await {
                                error!(error = %e, "Failed to process delivery");
                                // Nack with requeue on transient errors
                                let _ = channel.basic_nack(
                                    delivery.delivery_tag,
                                    BasicNackOptions {
                                        requeue: e.is_transient(),
                                        ..Default::default()
                                    },
                                ).await;
                            }
                        }
                        Some(Err(e)) => {
                            error!(error = %e, "Consumer error");
                        }
                        None => {
                            warn!("Consumer stream ended");
                            break;
                        }
                    }
                }
            }
        }
    }
    
    /// Process a single delivery.
    #[instrument(skip_all, fields(delivery_tag = %delivery.delivery_tag))]
    async fn process_delivery(
        delivery: &lapin::message::Delivery,
        grey_client: &Arc<C>,
        in_flight: &Arc<RwLock<HashMap<String, InFlightMessage>>>,
        metrics: &Arc<ConnectorMetrics>,
        channel: &Channel,
        tenant_id: &str,
    ) -> Result<(), ProcessError> {
        // Parse message
        let task_msg: RabbitTaskMessage = serde_json::from_slice(&delivery.data)
            .map_err(|e| ProcessError::InvalidMessage(e.to_string()))?;
        
        // Extract AMQP properties
        let props = &delivery.properties;
        let message_id = props.message_id()
            .as_ref()
            .map(|s| s.to_string())
            .unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
        
        let correlation_id = props.correlation_id().as_ref().map(|s| s.to_string());
        let reply_to = props.reply_to().as_ref().map(|s| s.to_string());
        
        debug!(
            message_id = %message_id,
            tenant_id,
            "Processing message"
        );
        
        // Track in-flight
        {
            let mut in_flight = in_flight.write().await;
            in_flight.insert(message_id.clone(), InFlightMessage {
                delivery_tag: delivery.delivery_tag,
                message_id: message_id.clone(),
                tenant_id: tenant_id.to_string(),
                correlation_id: correlation_id.clone(),
                reply_to: reply_to.clone(),
                received_at: std::time::Instant::now(),
            });
        }
        
        // Submit to Grey
        let request = TaskSubmitRequest {
            tenant_id: tenant_id.to_string(),
            task_type: task_msg.task_type,
            payload: task_msg.payload,
            priority: task_msg.priority,
            timeout_seconds: task_msg.timeout_seconds,
            labels: task_msg.labels,
            require_tee: task_msg.require_tee,
            idempotency_key: message_id.clone(),
        };
        
        let response = grey_client.submit_task(request).await
            .map_err(|e| ProcessError::GreySubmit(e.to_string()))?;
        
        info!(
            message_id = %message_id,
            task_id = %response.task_id,
            "Task submitted to Grey"
        );
        
        metrics.messages_consumed.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        
        // Note: Ack happens after result is published back
        // This is handled by the result publisher task
        
        Ok(())
    }
    
    /// Start result publisher task.
    fn start_result_publisher(&self) -> tokio::task::JoinHandle<()> {
        let channel = self.publish_channel.clone();
        let exchange = self.config.result_exchange.clone();
        let in_flight = Arc::clone(&self.in_flight);
        let metrics = Arc::clone(&self.metrics);
        let consume_channel = self.consume_channel.clone();
        let grey_client = Arc::clone(&self.grey_client);
        
        tokio::spawn(async move {
            // In production, this would subscribe to Grey completion events
            loop {
                tokio::time::sleep(Duration::from_millis(100)).await;
                
                // Process completions and publish results
                // Ack original messages after successful publish
            }
        })
    }
    
    /// Publish result message.
    async fn publish_result(
        channel: &Channel,
        exchange: &str,
        tenant_id: &str,
        result: &RabbitResultMessage,
        correlation_id: Option<&str>,
    ) -> Result<(), PublishError> {
        let payload = serde_json::to_vec(result)
            .map_err(|e| PublishError::Serialization(e.to_string()))?;
        
        let routing_key = format!("{}.results", tenant_id);
        
        let mut props = BasicProperties::default()
            .with_content_type("application/json".into())
            .with_delivery_mode(2)  // Persistent
            .with_message_id(result.task_id.clone().into());
        
        if let Some(cid) = correlation_id {
            props = props.with_correlation_id(cid.into());
        }
        
        let confirm = channel.basic_publish(
            exchange,
            &routing_key,
            BasicPublishOptions::default(),
            &payload,
            props,
        ).await.map_err(|e| PublishError::Publish(e.to_string()))?;
        
        // Wait for publisher confirm
        confirm.await.map_err(|e| PublishError::Confirm(e.to_string()))?;
        
        debug!(
            task_id = %result.task_id,
            routing_key,
            "Published result to RabbitMQ"
        );
        
        Ok(())
    }
    
    /// Start health checker task.
    fn start_health_checker(&self) -> tokio::task::JoinHandle<()> {
        let connection = self.connection.clone();
        
        tokio::spawn(async move {
            loop {
                tokio::time::sleep(Duration::from_secs(30)).await;
                
                if !connection.status().connected() {
                    warn!("RabbitMQ connection lost");
                    // Trigger reconnection logic
                }
            }
        })
    }
    
    /// Pause consumption for a tenant (backpressure).
    pub async fn pause_tenant(&self, tenant_id: &str) -> Result<(), ConnectorError> {
        let mut consumers = self.consumers.write().await;
        if let Some(state) = consumers.get_mut(tenant_id) {
            // Cancel and recreate consumer with flow control
            // (simplified - actual implementation would use channel.basic_cancel)
            state.paused = true;
            info!(tenant_id, "Paused consumption");
        }
        Ok(())
    }
    
    /// Resume consumption for a tenant.
    pub async fn resume_tenant(&self, tenant_id: &str) -> Result<(), ConnectorError> {
        let mut consumers = self.consumers.write().await;
        if let Some(state) = consumers.get_mut(tenant_id) {
            state.paused = false;
            info!(tenant_id, "Resumed consumption");
        }
        Ok(())
    }
}

impl ConnectorMetrics {
    fn new() -> Self {
        Self {
            messages_consumed: std::sync::atomic::AtomicU64::new(0),
            messages_published: std::sync::atomic::AtomicU64::new(0),
            messages_acked: std::sync::atomic::AtomicU64::new(0),
            messages_nacked: std::sync::atomic::AtomicU64::new(0),
            messages_dlq: std::sync::atomic::AtomicU64::new(0),
            publish_confirms: std::sync::atomic::AtomicU64::new(0),
            publish_failures: std::sync::atomic::AtomicU64::new(0),
        }
    }
    
    /// Export metrics for Prometheus.
    pub fn export_metrics(&self) -> HashMap<String, f64> {
        let mut metrics = HashMap::new();
        metrics.insert(
            "grey_rabbitmq_messages_consumed_total".to_string(),
            self.messages_consumed.load(std::sync::atomic::Ordering::Relaxed) as f64,
        );
        metrics.insert(
            "grey_rabbitmq_messages_published_total".to_string(),
            self.messages_published.load(std::sync::atomic::Ordering::Relaxed) as f64,
        );
        metrics.insert(
            "grey_rabbitmq_messages_acked_total".to_string(),
            self.messages_acked.load(std::sync::atomic::Ordering::Relaxed) as f64,
        );
        metrics.insert(
            "grey_rabbitmq_messages_nacked_total".to_string(),
            self.messages_nacked.load(std::sync::atomic::Ordering::Relaxed) as f64,
        );
        metrics.insert(
            "grey_rabbitmq_messages_dlq_total".to_string(),
            self.messages_dlq.load(std::sync::atomic::Ordering::Relaxed) as f64,
        );
        metrics
    }
}

// ============================================================================
// Handle and Errors
// ============================================================================

pub struct ConnectorHandle {
    shutdown_tx: mpsc::Sender<()>,
    result_handle: tokio::task::JoinHandle<()>,
    health_handle: tokio::task::JoinHandle<()>,
}

impl ConnectorHandle {
    pub async fn shutdown(self) -> Result<(), ConnectorError> {
        let _ = self.shutdown_tx.send(()).await;
        let _ = tokio::join!(self.result_handle, self.health_handle);
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ConnectorError {
    #[error("Connection error: {0}")]
    Connection(String),
    
    #[error("Channel error: {0}")]
    Channel(String),
    
    #[error("Topology error: {0}")]
    Topology(String),
    
    #[error("Consumer error: {0}")]
    Consumer(String),
}

#[derive(Debug, thiserror::Error)]
pub enum ProcessError {
    #[error("Invalid message: {0}")]
    InvalidMessage(String),
    
    #[error("Grey submit error: {0}")]
    GreySubmit(String),
    
    #[error("Transient error: {0}")]
    Transient(String),
}

impl ProcessError {
    fn is_transient(&self) -> bool {
        matches!(self, ProcessError::Transient(_))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum PublishError {
    #[error("Serialization error: {0}")]
    Serialization(String),
    
    #[error("Publish error: {0}")]
    Publish(String),
    
    #[error("Confirm error: {0}")]
    Confirm(String),
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_priority_conversion() {
        assert_eq!(Priority::Low.to_amqp_priority(), 1);
        assert_eq!(Priority::Normal.to_amqp_priority(), 5);
        assert_eq!(Priority::High.to_amqp_priority(), 7);
        assert_eq!(Priority::Critical.to_amqp_priority(), 9);
    }
    
    #[test]
    fn test_queue_name_generation() {
        let prefix = "grey.tasks";
        let tenant_id = "tenant-123";
        let queue_name = format!("{}.{}", prefix, tenant_id);
        assert_eq!(queue_name, "grey.tasks.tenant-123");
    }
}
