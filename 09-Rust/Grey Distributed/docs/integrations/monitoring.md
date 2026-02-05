# Grey Distributed — Monitoring Integration

Integration patterns for Prometheus, Grafana, and OpenTelemetry.

---

## Overview

Grey Distributed exposes:

- **Prometheus metrics** — Cluster health, task execution, resource usage
- **OpenTelemetry traces** — Distributed tracing across components
- **Grafana dashboards** — Pre-built visualizations

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Grey Distributed Cluster                      │
├─────────────┬───────────────┬───────────────┬──────────────────┤
│ Coordinator │    Worker     │    Worker     │     Gateway      │
│  (metrics)  │   (metrics)   │   (metrics)   │    (metrics)     │
└──────┬──────┴───────┬───────┴───────┬───────┴────────┬─────────┘
       │              │               │                │
       ▼              ▼               ▼                ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Prometheus Scrape                             │
└─────────────────────────┬───────────────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
   ┌───────────┐   ┌───────────┐   ┌───────────┐
   │ Prometheus│   │  Grafana  │   │  Alertmgr │
   └───────────┘   └───────────┘   └───────────┘

┌─────────────────────────────────────────────────────────────────┐
│                    OpenTelemetry Collector                       │
│                    (traces + spans)                              │
└─────────────────────────┬───────────────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
   ┌───────────┐   ┌───────────┐   ┌───────────┐
   │   Jaeger  │   │   Tempo   │   │   Zipkin  │
   └───────────┘   └───────────┘   └───────────┘
```

---

## Prometheus Exporter

**File:** [integrations/monitoring/prometheus_exporter.rs](../../integrations/monitoring/prometheus_exporter.rs)

### Configuration

```yaml
prometheus:
  enabled: true
  port: 9090
  path: /metrics
  
  # Include tenant labels (careful: cardinality)
  include_tenant_labels: true
  
  # Histogram bucket configuration
  latency_buckets:
    - 0.001  # 1ms
    - 0.005  # 5ms
    - 0.01   # 10ms
    - 0.05   # 50ms
    - 0.1    # 100ms
    - 0.5    # 500ms
    - 1.0    # 1s
    - 5.0    # 5s
```

### Exposed Metrics

#### Cluster Metrics

```prometheus
# Node status
grey_cluster_nodes_total{role="worker",status="active"} 14
grey_cluster_nodes_total{role="coordinator",status="active"} 3

# Consensus
grey_consensus_term 42
grey_consensus_leader{node_id="grey-coordinator-0"} 1
grey_consensus_quorum_size 3
grey_consensus_commit_index 12345

# Resource utilization
grey_cluster_cpu_utilization_ratio 0.72
grey_cluster_memory_utilization_ratio 0.65
```

#### Task Metrics

```prometheus
# Task counters
grey_tasks_submitted_total{tenant="tenant-123",type="stateless"} 50000
grey_tasks_completed_total{tenant="tenant-123",type="stateless"} 49500
grey_tasks_failed_total{tenant="tenant-123",type="stateless"} 500

# Task latency (histogram)
grey_task_duration_seconds_bucket{tenant="tenant-123",le="0.1"} 40000
grey_task_duration_seconds_bucket{tenant="tenant-123",le="1.0"} 49000
grey_task_duration_seconds_bucket{tenant="tenant-123",le="+Inf"} 49500
grey_task_duration_seconds_sum{tenant="tenant-123"} 12345.67
grey_task_duration_seconds_count{tenant="tenant-123"} 49500

# Queue depth
grey_queue_depth{tenant="tenant-123",priority="normal"} 1234
grey_queue_depth{tenant="tenant-123",priority="high"} 45
```

#### TEE Metrics

```prometheus
# Attestation
grey_tee_attestations_total{platform="sgx",result="success"} 1000
grey_tee_attestations_total{platform="sgx",result="failure"} 5
grey_tee_attestation_duration_seconds{quantile="0.99"} 0.05

# Enclave status
grey_tee_enclaves_active{platform="sgx"} 10
grey_tee_enclave_memory_bytes{enclave_id="enc-1"} 268435456
```

#### Database Metrics

```prometheus
# Connection pools
grey_db_connections_active{backend="postgres",shard="0"} 15
grey_db_connections_idle{backend="postgres",shard="0"} 5

# Query latency
grey_db_query_duration_seconds{backend="postgres",operation="read"} 0.002
grey_db_query_duration_seconds{backend="postgres",operation="write"} 0.005

# Replication lag
grey_db_replication_lag_seconds{backend="postgres",replica="replica-1"} 0.1
```

### Scrape Configuration

```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'grey-cluster'
    kubernetes_sd_configs:
      - role: pod
        selectors:
          - role: pod
            label: "app=grey"
    relabel_configs:
      - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_scrape]
        action: keep
        regex: true
      - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_path]
        action: replace
        target_label: __metrics_path__
        regex: (.+)
      - source_labels: [__address__, __meta_kubernetes_pod_annotation_prometheus_io_port]
        action: replace
        regex: ([^:]+)(?::\d+)?;(\d+)
        replacement: $1:$2
        target_label: __address__
```

---

## Grafana Dashboards

**File:** [integrations/monitoring/grafana_dashboard.json](../../integrations/monitoring/grafana_dashboard.json)

### Dashboard: Cluster Overview

**Panels:**

1. **Cluster Health** — Status indicator (healthy/degraded/critical)
2. **Node Map** — Geographic distribution of nodes
3. **Resource Utilization** — CPU/Memory gauges
4. **Task Throughput** — Tasks/second over time
5. **Queue Depth** — Current queue depth by priority
6. **Consensus Status** — Leader, term, commit index

### Dashboard: Task Execution

**Panels:**

1. **Task Status Distribution** — Pie chart (completed/running/failed)
2. **Task Latency Heatmap** — P50/P95/P99 over time
3. **Task Throughput by Tenant** — Stacked area chart
4. **Failure Rate** — Percentage of failed tasks
5. **Retry Distribution** — Histogram of retry counts
6. **Top Errors** — Table of error codes and counts

### Dashboard: Tenant Isolation

**Panels:**

1. **Resource Usage by Tenant** — Stacked bar chart
2. **Quota Utilization** — Gauge per tenant
3. **Rate Limit Status** — Current vs limit
4. **Task Count by Tenant** — Time series
5. **Cost per Tenant** — Estimated cost

### Dashboard: TEE Operations

**Panels:**

1. **Attestation Success Rate** — Gauge
2. **Attestation Latency** — Histogram
3. **Active Enclaves** — Count by platform
4. **Proof Generation Rate** — Tasks with proofs
5. **Platform Distribution** — SGX vs TDX vs SEV

### Importing Dashboards

```bash
# Using Grafana API
curl -X POST -H "Content-Type: application/json" \
  -d @grafana_dashboard.json \
  http://admin:admin@grafana:3000/api/dashboards/db

# Or via Kubernetes ConfigMap
kubectl create configmap grey-dashboards \
  --from-file=integrations/monitoring/grafana_dashboard.json
```

---

## OpenTelemetry Integration

**File:** [integrations/monitoring/opentelemetry.rs](../../integrations/monitoring/opentelemetry.rs)

### Configuration

```yaml
opentelemetry:
  enabled: true
  
  exporter:
    protocol: grpc  # or http/protobuf
    endpoint: http://otel-collector:4317
    headers:
      Authorization: "Bearer ${OTEL_TOKEN}"
  
  sampling:
    strategy: parent_based_traceid_ratio
    ratio: 0.1  # Sample 10% of traces
    always_sample:
      - error=true
      - priority=critical
  
  resource:
    service.name: grey-distributed
    service.version: 1.5.0
    deployment.environment: production
  
  propagation:
    - tracecontext  # W3C Trace Context
    - baggage       # W3C Baggage
```

### Span Hierarchy

```
grey.request
├── grey.auth.validate_token
├── grey.scheduler.submit_task
│   ├── grey.consensus.propose
│   │   └── grey.consensus.replicate
│   └── grey.storage.persist
├── grey.worker.execute
│   ├── grey.tee.enter_enclave
│   ├── grey.task.run
│   └── grey.tee.exit_enclave
└── grey.response.build
```

### Custom Attributes

```rust
// Add span attributes
span.set_attribute(KeyValue::new("grey.tenant_id", tenant_id));
span.set_attribute(KeyValue::new("grey.task_id", task_id));
span.set_attribute(KeyValue::new("grey.task_type", task_type));
span.set_attribute(KeyValue::new("grey.tee.platform", "sgx"));

// Add events
span.add_event("task_queued", vec![
    KeyValue::new("queue_depth", queue_depth),
    KeyValue::new("priority", priority),
]);

// Record errors
span.record_error(&error);
span.set_status(Status::error(error.to_string()));
```

### Context Propagation

```rust
// Extract from incoming request
let parent_context = global::get_text_map_propagator(|propagator| {
    propagator.extract(&HeaderExtractor(&request.headers))
});

// Create span with parent
let span = tracer
    .span_builder("grey.request")
    .with_parent_context(parent_context)
    .start(&tracer);

// Inject into outgoing request
global::get_text_map_propagator(|propagator| {
    propagator.inject_context(&context, &mut HeaderInjector(&mut headers))
});
```

### Baggage for Tenant Isolation

```rust
// Set tenant in baggage (propagates to all downstream services)
let mut baggage = Baggage::new();
baggage.insert("grey.tenant_id", tenant_id.to_string());

// Read tenant from baggage
let tenant_id = Baggage::current()
    .get("grey.tenant_id")
    .map(|v| v.as_str());
```

---

## Alert Rules

### Recording Rules

```yaml
# prometheus-rules.yaml
groups:
  - name: grey-recording
    rules:
      # Task success rate (5m window)
      - record: grey:task_success_rate:5m
        expr: |
          sum(rate(grey_tasks_completed_total[5m])) by (tenant)
          /
          sum(rate(grey_tasks_submitted_total[5m])) by (tenant)
      
      # P99 latency (5m window)
      - record: grey:task_latency_p99:5m
        expr: |
          histogram_quantile(0.99,
            sum(rate(grey_task_duration_seconds_bucket[5m])) by (tenant, le)
          )
```

### Alerting Rules

```yaml
groups:
  - name: grey-alerts
    rules:
      # Cluster health
      - alert: GreyClusterDegraded
        expr: grey_cluster_nodes_total{status="active"} < grey_cluster_nodes_total * 0.8
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Grey cluster has <80% healthy nodes"
          runbook: "https://grey.io/runbooks/cluster-degraded"
      
      - alert: GreyClusterCritical
        expr: grey_cluster_nodes_total{status="active"} < grey_cluster_nodes_total * 0.5
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "Grey cluster has <50% healthy nodes"
          runbook: "https://grey.io/runbooks/cluster-critical"
      
      # Consensus
      - alert: GreyConsensusNoLeader
        expr: sum(grey_consensus_leader) == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "No Raft leader elected"
      
      # Task processing
      - alert: GreyHighTaskFailureRate
        expr: grey:task_success_rate:5m < 0.95
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Task success rate < 95% for tenant {{ $labels.tenant }}"
      
      - alert: GreyHighTaskLatency
        expr: grey:task_latency_p99:5m > 5
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "P99 task latency > 5s for tenant {{ $labels.tenant }}"
      
      # Queue
      - alert: GreyQueueBacklog
        expr: grey_queue_depth > 10000
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Queue depth > 10000 for tenant {{ $labels.tenant }}"
      
      # TEE
      - alert: GreyTEEAttestationFailure
        expr: rate(grey_tee_attestations_total{result="failure"}[5m]) > 0.1
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "TEE attestation failures detected"
```

---

## Logging Integration

### Structured Logging

```rust
// tracing crate integration
use tracing::{info, warn, error, instrument};

#[instrument(skip(payload), fields(task_id, tenant_id))]
async fn submit_task(
    tenant_id: &str,
    payload: Payload,
) -> Result<TaskId> {
    tracing::Span::current().record("tenant_id", tenant_id);
    
    info!("Task submitted");
    
    match process(payload).await {
        Ok(result) => {
            info!(result_size = result.len(), "Task completed");
            Ok(result)
        }
        Err(e) => {
            error!(error = %e, "Task failed");
            Err(e)
        }
    }
}
```

### Log Export to Loki

```yaml
# Vector configuration for log shipping
sources:
  grey_logs:
    type: kubernetes_logs
    namespace_annotation_fields:
      - name: grey.io/tenant-id
        label: tenant_id

transforms:
  grey_parse:
    type: remap
    inputs: ["grey_logs"]
    source: |
      . = parse_json!(.message)
      .timestamp = now()

sinks:
  loki:
    type: loki
    inputs: ["grey_parse"]
    endpoint: http://loki:3100
    labels:
      app: grey
      tenant: "{{ tenant_id }}"
    encoding:
      codec: json
```

---

## Custom Metrics

### Adding Custom Metrics

```rust
use prometheus::{register_counter, register_histogram, Counter, Histogram};

lazy_static! {
    static ref CUSTOM_OPS: Counter = register_counter!(
        "grey_custom_operations_total",
        "Total custom operations"
    ).unwrap();
    
    static ref CUSTOM_LATENCY: Histogram = register_histogram!(
        "grey_custom_operation_duration_seconds",
        "Custom operation latency",
        vec![0.001, 0.01, 0.1, 1.0, 10.0]
    ).unwrap();
}

fn my_operation() {
    let timer = CUSTOM_LATENCY.start_timer();
    // ... do work ...
    timer.observe_duration();
    CUSTOM_OPS.inc();
}
```

### Tenant-Aware Metrics

```rust
// Use labels carefully (cardinality risk)
let task_counter = register_counter_vec!(
    "grey_tenant_tasks_total",
    "Tasks by tenant",
    &["tenant_id", "task_type"]
).unwrap();

// High-cardinality mitigation: use consistent hashing
fn tenant_bucket(tenant_id: &str) -> String {
    let hash = consistent_hash(tenant_id);
    format!("bucket-{:02}", hash % 100)
}
```

---

## Best Practices

### 1. Label Cardinality

```yaml
# BAD: unbounded labels
grey_tasks_total{task_id="abc123"}  # Millions of unique values

# GOOD: bounded labels
grey_tasks_total{tenant="t123",type="stateless",status="completed"}
```

### 2. Histogram Buckets

```rust
// Match expected latency distribution
let buckets = vec![
    0.001, 0.005, 0.01,    // Fast path (1-10ms)
    0.05, 0.1, 0.25,       // Normal (50-250ms)
    0.5, 1.0, 2.5, 5.0,    // Slow (500ms-5s)
    10.0, 30.0,            // Timeout territory
];
```

### 3. Trace Sampling

```yaml
sampling:
  # Sample 10% normally
  ratio: 0.1
  
  # Always sample errors and critical paths
  always_sample:
    - error=true
    - priority=critical
    - tee.platform=*
```

---

*Monitoring integrations are in [integrations/monitoring/](../../integrations/monitoring/)*
