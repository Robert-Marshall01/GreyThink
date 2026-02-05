# Grey Distributed — Enterprise Integration

Integration patterns for Kafka, RabbitMQ, and IAM systems.

---

## Overview

Grey Distributed integrates with enterprise systems for:

- **Event streaming** — Kafka for task events and results
- **Message queues** — RabbitMQ/NATS for async communication
- **Identity federation** — OAuth2, OIDC, SAML for authentication

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      External Systems                            │
├────────────────┬────────────────┬───────────────────────────────┤
│     Kafka      │   RabbitMQ     │        Identity Provider      │
│  (Events)      │   (Commands)   │      (Keycloak/Okta/AD)       │
└───────┬────────┴───────┬────────┴───────────────┬───────────────┘
        │                │                        │
        ▼                ▼                        ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Grey Distributed                              │
├─────────────────┬───────────────────┬───────────────────────────┤
│ Kafka Connector │ RabbitMQ Connector│     IAM Adapter           │
│ (consume/produce)│ (consume/publish)│ (token validation)        │
└─────────────────┴───────────────────┴───────────────────────────┘
```

---

## Kafka Connector

**File:** [integrations/enterprise/kafka_connector.rs](../../integrations/enterprise/kafka_connector.rs)

### Features

- Consume tasks from Kafka topics
- Produce results and events to Kafka
- Exactly-once semantics with transactions
- Schema registry integration (Avro/Protobuf)
- Tenant-isolated topics

### Configuration

```yaml
kafka:
  brokers:
    - kafka1.grey.io:9092
    - kafka2.grey.io:9092
    - kafka3.grey.io:9092
  
  security:
    protocol: SASL_SSL
    mechanism: SCRAM-SHA-512
    username: grey-service
    password: ${KAFKA_PASSWORD}
    ssl_ca_location: /certs/ca.pem
  
  consumer:
    group_id: grey-cluster
    topics:
      - pattern: "grey.tasks.*"
        tenant_extraction: topic_suffix  # grey.tasks.tenant-123
    auto_offset_reset: earliest
    enable_auto_commit: false  # Manual commit after processing
    max_poll_records: 100
    session_timeout_ms: 30000
  
  producer:
    acks: all
    enable_idempotence: true
    transactional_id: grey-producer
    compression_type: lz4
    linger_ms: 5
    batch_size: 65536
  
  schema_registry:
    url: https://schema-registry.grey.io:8081
    basic_auth: ${SCHEMA_REGISTRY_AUTH}
```

### Topic Structure

```
# Task submission (per tenant)
grey.tasks.{tenant_id}
  Key: task_id
  Value: TaskPayload (Avro)

# Task results (per tenant)
grey.results.{tenant_id}
  Key: task_id
  Value: TaskResult (Avro)

# Events (global, partitioned by tenant)
grey.events
  Key: tenant_id
  Value: TaskEvent (Avro)
  Headers:
    - event_type: submitted|started|completed|failed
    - tenant_id: ...
    - task_id: ...
```

### Message Schemas

```avro
// TaskPayload.avsc
{
  "type": "record",
  "name": "TaskPayload",
  "namespace": "io.grey.tasks",
  "fields": [
    {"name": "task_id", "type": "string"},
    {"name": "tenant_id", "type": "string"},
    {"name": "task_type", "type": {"type": "enum", "name": "TaskType", "symbols": ["STATELESS", "STATEFUL", "PIPELINE"]}},
    {"name": "payload", "type": "bytes"},
    {"name": "priority", "type": "int", "default": 5},
    {"name": "require_tee", "type": "boolean", "default": false},
    {"name": "idempotency_key", "type": ["null", "string"], "default": null},
    {"name": "headers", "type": {"type": "map", "values": "string"}}
  ]
}

// TaskResult.avsc
{
  "type": "record",
  "name": "TaskResult",
  "namespace": "io.grey.tasks",
  "fields": [
    {"name": "task_id", "type": "string"},
    {"name": "tenant_id", "type": "string"},
    {"name": "status", "type": {"type": "enum", "name": "Status", "symbols": ["COMPLETED", "FAILED"]}},
    {"name": "result", "type": ["null", "bytes"], "default": null},
    {"name": "error", "type": ["null", "string"], "default": null},
    {"name": "proof", "type": ["null", "bytes"], "default": null},
    {"name": "duration_ms", "type": "long"},
    {"name": "completed_at", "type": "long"}
  ]
}
```

### Usage

```rust
use integrations::enterprise::kafka_connector::KafkaConnector;

// Initialize connector
let connector = KafkaConnector::new(config).await?;

// Start consuming tasks
connector.start_consumer(|message| async {
    let task: TaskPayload = message.deserialize()?;
    
    // Process through Grey
    let result = grey_client.submit_and_wait(task).await?;
    
    // Produce result
    connector.produce_result(result).await?;
    
    Ok(())
}).await?;

// Produce task programmatically
connector.produce_task(TaskPayload {
    task_id: "task-123".into(),
    tenant_id: "tenant-abc".into(),
    task_type: TaskType::Stateless,
    payload: data,
    ..Default::default()
}).await?;
```

### Exactly-Once Semantics

```rust
// Transactional consume-process-produce
connector.transactional(|txn| async move {
    // 1. Consume (within transaction)
    let messages = txn.poll().await?;
    
    for msg in messages {
        // 2. Process
        let result = process(msg).await?;
        
        // 3. Produce (within transaction)
        txn.produce("grey.results", result).await?;
    }
    
    // 4. Commit offsets + produced messages atomically
    txn.commit().await
}).await?;
```

**Tradeoffs:**
- ✅ Exactly-once delivery guarantees
- ⚠️ Higher latency (~10-20ms per transaction)
- ⚠️ Requires Kafka 2.5+ with transactions enabled

---

## RabbitMQ Connector

**File:** [integrations/enterprise/rabbitmq_connector.rs](../../integrations/enterprise/rabbitmq_connector.rs)

### Features

- Work queue pattern for task distribution
- Dead-letter queues for failed tasks
- Priority queues
- Message TTL and expiration
- Tenant-specific virtual hosts

### Configuration

```yaml
rabbitmq:
  hosts:
    - host: rabbit1.grey.io
      port: 5672
    - host: rabbit2.grey.io
      port: 5672
  
  credentials:
    username: grey-service
    password: ${RABBITMQ_PASSWORD}
  
  tls:
    enabled: true
    ca_cert: /certs/ca.pem
    verify_peer: true
  
  connection:
    heartbeat: 60
    connection_timeout: 30
    channel_max: 2047
  
  exchanges:
    - name: grey.tasks
      type: topic
      durable: true
    - name: grey.results
      type: topic
      durable: true
    - name: grey.dlx
      type: fanout
      durable: true
  
  queues:
    - name: grey.tasks.{tenant}
      durable: true
      max_priority: 10
      dead_letter_exchange: grey.dlx
      message_ttl: 3600000  # 1 hour
  
  prefetch: 10
```

### Queue Topology

```
                    ┌──────────────────┐
                    │  grey.tasks      │
                    │  (topic exchange)│
                    └────────┬─────────┘
                             │
         ┌───────────────────┼───────────────────┐
         │                   │                   │
         ▼                   ▼                   ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│ grey.tasks.t1   │ │ grey.tasks.t2   │ │ grey.tasks.t3   │
│ (priority queue)│ │ (priority queue)│ │ (priority queue)│
└────────┬────────┘ └────────┬────────┘ └────────┬────────┘
         │                   │                   │
         └───────────────────┼───────────────────┘
                             │ (on failure)
                             ▼
                    ┌──────────────────┐
                    │  grey.dlx        │
                    │  (dead letter)   │
                    └────────┬─────────┘
                             │
                             ▼
                    ┌──────────────────┐
                    │  grey.dlq        │
                    │  (dead letter Q) │
                    └──────────────────┘
```

### Usage

```rust
use integrations::enterprise::rabbitmq_connector::RabbitMQConnector;

// Initialize connector
let connector = RabbitMQConnector::new(config).await?;

// Declare tenant queue
connector.declare_tenant_queue("tenant-123").await?;

// Publish task
connector.publish(
    "grey.tasks",           // exchange
    "tasks.tenant-123",     // routing key
    TaskPayload { ... },
    PublishOptions {
        priority: Some(8),
        expiration: Some(Duration::from_secs(3600)),
        persistent: true,
    }
).await?;

// Consume tasks
connector.consume("grey.tasks.tenant-123", |delivery| async {
    let task: TaskPayload = delivery.deserialize()?;
    
    match process(task).await {
        Ok(result) => {
            // Publish result
            connector.publish("grey.results", ..., result).await?;
            delivery.ack().await
        }
        Err(e) if e.is_retryable() => {
            // Requeue
            delivery.nack(true).await
        }
        Err(e) => {
            // Send to DLQ
            delivery.nack(false).await
        }
    }
}).await?;
```

### Priority Queue

```rust
// High priority task
connector.publish(
    "grey.tasks",
    "tasks.tenant-123",
    urgent_task,
    PublishOptions {
        priority: Some(10),  // 0-10, higher = more priority
        ..Default::default()
    }
).await?;

// Normal priority
connector.publish(
    "grey.tasks",
    "tasks.tenant-123",
    normal_task,
    PublishOptions {
        priority: Some(5),
        ..Default::default()
    }
).await?;
```

**Tradeoffs:**
- ✅ Simple priority ordering
- ⚠️ Priority queues have memory overhead
- ⚠️ Head-of-line blocking with many priorities

---

## IAM Adapter

**File:** [integrations/enterprise/iam_adapter.rs](../../integrations/enterprise/iam_adapter.rs)

### Features

- OAuth2/OIDC token validation
- SAML assertion verification
- Role-based access control (RBAC)
- Tenant claim extraction
- Token caching and refresh

### Configuration

```yaml
iam:
  provider: oidc  # oidc, saml, custom
  
  oidc:
    issuer: https://auth.grey.io/realms/grey
    audience: grey-api
    client_id: grey-service
    client_secret: ${OIDC_CLIENT_SECRET}
    
    # Discovery (prefered)
    discovery_url: https://auth.grey.io/realms/grey/.well-known/openid-configuration
    
    # Or manual configuration
    # jwks_url: https://auth.grey.io/realms/grey/protocol/openid-connect/certs
    # token_url: https://auth.grey.io/realms/grey/protocol/openid-connect/token
    
    # Claim mapping
    claims:
      tenant_id: grey_tenant_id
      roles: grey_roles
      permissions: grey_permissions
    
    # Token validation
    validate:
      issuer: true
      audience: true
      expiration: true
      not_before: true
    
    # Caching
    cache_jwks: true
    jwks_refresh_interval: 3600
  
  saml:
    enabled: false
    idp_metadata_url: https://idp.example.com/metadata
    sp_entity_id: grey-api
    assertion_consumer_service_url: https://grey.io/saml/acs
    certificate: /certs/saml.pem
  
  rbac:
    enabled: true
    roles:
      admin:
        - "*"
      operator:
        - "tasks:read"
        - "tasks:write"
        - "cluster:read"
      viewer:
        - "tasks:read"
        - "cluster:read"
```

### Token Structure

```json
// JWT Claims (OIDC)
{
  "iss": "https://auth.grey.io/realms/grey",
  "sub": "user-123",
  "aud": "grey-api",
  "exp": 1706918400,
  "iat": 1706914800,
  "grey_tenant_id": "tenant-456",
  "grey_roles": ["operator"],
  "grey_permissions": ["tasks:read", "tasks:write"]
}
```

### Usage

```rust
use integrations::enterprise::iam_adapter::IAMAdapter;

// Initialize adapter
let iam = IAMAdapter::new(config).await?;

// Validate token
let claims = iam.validate_token(bearer_token).await?;

// Extract tenant
let tenant_id = claims.tenant_id()?;

// Check permissions
if !iam.has_permission(&claims, "tasks:write") {
    return Err(AuthError::Forbidden);
}

// RBAC check
if !iam.has_role(&claims, "operator") {
    return Err(AuthError::InsufficientRole);
}
```

### Middleware Integration

```rust
// Axum middleware
async fn auth_middleware(
    State(iam): State<Arc<IAMAdapter>>,
    TypedHeader(auth): TypedHeader<Authorization<Bearer>>,
    mut request: Request,
    next: Next,
) -> Result<Response, AuthError> {
    // Validate token
    let claims = iam.validate_token(auth.token()).await?;
    
    // Extract tenant and inject into request
    let tenant_id = claims.tenant_id()?;
    request.extensions_mut().insert(TenantContext { tenant_id });
    
    Ok(next.run(request).await)
}

// Use in router
let app = Router::new()
    .route("/tasks", post(submit_task))
    .layer(middleware::from_fn_with_state(iam.clone(), auth_middleware));
```

### Service-to-Service Auth

```rust
// Client credentials flow
let token = iam.get_service_token(&[
    "grey.tasks.read",
    "grey.tasks.write",
]).await?;

// Use token for internal calls
client
    .header("Authorization", format!("Bearer {}", token))
    .post("/internal/sync")
    .await?;
```

### SAML Integration

```rust
// SAML assertion validation
let assertion = iam.validate_saml_response(saml_response).await?;

// Extract attributes
let tenant_id = assertion.attribute("grey_tenant_id")?;
let roles = assertion.attribute_values("grey_roles")?;

// Create session
let session = Session {
    subject: assertion.subject()?,
    tenant_id,
    roles,
    expires_at: assertion.conditions()?.not_on_or_after,
};
```

---

## Security Considerations

### 1. Tenant Isolation

```rust
// Always validate tenant access
async fn submit_task(
    tenant: TenantContext,  // Extracted from token
    task: TaskPayload,
) -> Result<TaskId> {
    // Ensure task targets same tenant
    if task.tenant_id != tenant.tenant_id {
        return Err(AuthError::TenantMismatch);
    }
    
    // Process...
}
```

### 2. Secrets Management

```yaml
# Use external secrets, never hardcode
kafka:
  password: ${KAFKA_PASSWORD}  # From env/vault

rabbitmq:
  password: ${RABBITMQ_PASSWORD}

iam:
  client_secret: ${OIDC_CLIENT_SECRET}
```

### 3. mTLS for Internal Communication

```yaml
# All connectors support mTLS
tls:
  enabled: true
  cert: /certs/client.pem
  key: /certs/client-key.pem
  ca: /certs/ca.pem
  verify_peer: true
```

### 4. Audit Logging

```rust
// Log all authentication events
info!(
    event = "auth_success",
    subject = %claims.subject,
    tenant_id = %claims.tenant_id,
    roles = ?claims.roles,
    "User authenticated"
);

warn!(
    event = "auth_failure",
    reason = %error,
    token_hash = %hash_token(token),
    "Authentication failed"
);
```

---

## Monitoring

### Metrics

```prometheus
# Kafka
grey_kafka_messages_consumed_total{topic="grey.tasks.tenant-123"}
grey_kafka_messages_produced_total{topic="grey.results.tenant-123"}
grey_kafka_consumer_lag{group="grey-cluster",topic="grey.tasks.tenant-123"}
grey_kafka_producer_latency_seconds{quantile="0.99"}

# RabbitMQ
grey_rabbitmq_messages_consumed_total{queue="grey.tasks.tenant-123"}
grey_rabbitmq_messages_published_total{exchange="grey.results"}
grey_rabbitmq_queue_depth{queue="grey.tasks.tenant-123"}
grey_rabbitmq_dlq_depth{queue="grey.dlq"}

# IAM
grey_iam_token_validations_total{result="success"}
grey_iam_token_validations_total{result="failure",reason="expired"}
grey_iam_token_validation_duration_seconds{quantile="0.99"}
grey_iam_jwks_refresh_total
```

### Alerts

```yaml
groups:
  - name: grey-enterprise
    rules:
      - alert: KafkaConsumerLag
        expr: grey_kafka_consumer_lag > 10000
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Kafka consumer lag > 10000"
          
      - alert: RabbitMQDLQGrowing
        expr: increase(grey_rabbitmq_dlq_depth[1h]) > 100
        labels:
          severity: warning
        annotations:
          summary: "Dead letter queue growing"
          
      - alert: HighAuthFailureRate
        expr: rate(grey_iam_token_validations_total{result="failure"}[5m]) > 0.1
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High authentication failure rate"
```

---

*Enterprise integrations are in [integrations/enterprise/](../../integrations/enterprise/)*
