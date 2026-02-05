# Grey Distributed — Integration Guide

Complete guide for integrating Grey Distributed with external systems.

---

## Overview

Grey Distributed provides modular adapters for:

| Category | Adapters |
|----------|----------|
| **Databases** | PostgreSQL, Cassandra, MongoDB |
| **AI Pipelines** | PyTorch, TensorFlow |
| **Monitoring** | Prometheus, Grafana, OpenTelemetry |
| **Enterprise** | Kafka, RabbitMQ, IAM/OIDC |
| **Cloud** | AWS, GCP, Azure |

---

## Quick Links

| Document | Description |
|----------|-------------|
| [Database Integration](database.md) | Sharding, replication, quorum reads/writes |
| [AI Pipeline Integration](ai_pipelines.md) | Model serving, proof artifacts, distributed inference |
| [Monitoring Integration](monitoring.md) | Metrics, dashboards, tracing |
| [Enterprise Integration](enterprise.md) | Kafka, RabbitMQ, IAM federation |
| [Cloud Integration](cloud.md) | AWS, GCP, Azure deployment |

---

## Architecture

```
┌────────────────────────────────────────────────────────────────┐
│                     Grey Distributed Core                       │
├───────────┬───────────┬───────────┬─────────────┬──────────────┤
│ Database  │    AI     │ Monitoring│  Enterprise │    Cloud     │
│ Adapters  │  Adapters │ Exporters │ Connectors  │   Adapters   │
├───────────┼───────────┼───────────┼─────────────┼──────────────┤
│ PostgreSQL│ PyTorch   │ Prometheus│ Kafka       │ AWS S3/EKS   │
│ Cassandra │ TensorFlow│ OTel      │ RabbitMQ    │ GCP GCS/GKE  │
│ MongoDB   │           │ Grafana   │ IAM/OIDC    │ Azure Blob   │
└───────────┴───────────┴───────────┴─────────────┴──────────────┘
```

---

## Common Patterns

### 1. Tenant Isolation

All adapters enforce tenant isolation:

```rust
// Every operation requires tenant_id
adapter.execute(tenant_id, operation, params);

// Resources are partitioned by tenant
let key = format!("{}/{}", tenant_id, resource_id);
```

### 2. Retry with Backoff

All network operations use exponential backoff:

```rust
let policy = RetryPolicy::exponential()
    .max_retries(5)
    .base_delay(Duration::from_millis(100))
    .max_delay(Duration::from_secs(30));
```

### 3. Cost Attribution

All cloud resources are tagged for cost tracking:

```yaml
tags:
  grey-tenant-id: tenant-123
  grey-task-id: task-456
  grey-environment: production
```

### 4. Proof Artifacts

AI and storage adapters generate proof artifacts:

```json
{
  "task_id": "task-456",
  "platform": "SGX",
  "measurement": "abc123...",
  "signature": "def456...",
  "timestamp": 1706918400
}
```

---

## Configuration

### Environment Variables

```bash
# Database
GREY_PG_URL=postgres://user:pass@host:5432/db
GREY_CASSANDRA_HOSTS=host1,host2,host3
GREY_MONGO_URL=mongodb://host:27017/db

# AI
GREY_MODEL_REGISTRY=s3://models
GREY_INFERENCE_TIMEOUT=30s

# Monitoring
GREY_PROMETHEUS_PORT=9090
GREY_OTEL_ENDPOINT=http://collector:4317

# Enterprise
GREY_KAFKA_BROKERS=kafka1:9092,kafka2:9092
GREY_IAM_PROVIDER=keycloak

# Cloud
AWS_REGION=us-east-1
GOOGLE_PROJECT=grey-prod
AZURE_SUBSCRIPTION_ID=abc-123
```

### Config File

```yaml
# grey-integrations.yaml
database:
  postgres:
    url: ${GREY_PG_URL}
    pool_size: 20
    quorum: majority
    
monitoring:
  prometheus:
    port: 9090
    path: /metrics
  opentelemetry:
    endpoint: ${GREY_OTEL_ENDPOINT}
    sample_rate: 0.1
    
cloud:
  provider: aws
  region: us-east-1
  cost_tags:
    project: grey
    team: platform
```

---

## Next Steps

1. **Database Integration** → [database.md](database.md)
2. **AI Pipelines** → [ai_pipelines.md](ai_pipelines.md)
3. **Monitoring** → [monitoring.md](monitoring.md)
4. **Enterprise** → [enterprise.md](enterprise.md)
5. **Cloud** → [cloud.md](cloud.md)
