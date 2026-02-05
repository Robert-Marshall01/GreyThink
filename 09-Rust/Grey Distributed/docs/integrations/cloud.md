# Grey Distributed — Cloud Integration

Integration patterns for AWS, GCP, and Azure.

---

## Overview

Grey Distributed provides cloud adapters for:

- **Compute** — EKS, GKE, AKS for worker node deployment
- **Storage** — S3, GCS, Azure Blob for artifacts and state
- **Multi-region** — Cross-region replication for global deployments
- **Cost monitoring** — Per-tenant cost attribution

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Grey Distributed Core                         │
├─────────────────────────────────────────────────────────────────┤
│                    Cloud Abstraction Layer                       │
├───────────────────┬───────────────────┬─────────────────────────┤
│    AWS Adapter    │    GCP Adapter    │    Azure Adapter        │
├───────────────────┼───────────────────┼─────────────────────────┤
│ S3 + DynamoDB     │ GCS + Firestore   │ Blob + Cosmos DB        │
│ EKS + Nitro       │ GKE + Confidential│ AKS + Confidential      │
│ CloudWatch        │ Cloud Monitoring  │ Azure Monitor           │
│ Cost Explorer     │ Billing API       │ Cost Management         │
└───────────────────┴───────────────────┴─────────────────────────┘
```

---

## AWS Adapter

**File:** [integrations/cloud/aws_adapter.rs](../../integrations/cloud/aws_adapter.rs)

### Features

- S3 for artifact storage with versioning
- DynamoDB for task state (optional)
- EKS integration for compute
- Nitro Enclaves for TEE
- Cost Explorer for attribution

### Configuration

```yaml
aws:
  region: us-east-1
  
  credentials:
    # Use IAM role (recommended)
    use_instance_profile: true
    # Or explicit credentials
    # access_key_id: ${AWS_ACCESS_KEY_ID}
    # secret_access_key: ${AWS_SECRET_ACCESS_KEY}
  
  s3:
    bucket: grey-artifacts-prod
    prefix: artifacts/
    region: us-east-1
    
    # Cross-region replication
    replication:
      enabled: true
      destination_bucket: grey-artifacts-prod-eu
      destination_region: eu-west-1
    
    # Lifecycle
    lifecycle:
      - prefix: artifacts/
        transition_to_ia_days: 30
        transition_to_glacier_days: 90
        expiration_days: 365
    
    # Encryption
    encryption:
      type: aws:kms
      kms_key_id: alias/grey-key
    
    # Access logging
    access_logging:
      enabled: true
      bucket: grey-logs
      prefix: s3-access/
  
  dynamodb:
    enabled: false  # Optional, use for DynamoDB-backed state
    table_name: grey-tasks
    region: us-east-1
    billing_mode: PAY_PER_REQUEST
    # billing_mode: PROVISIONED
    # read_capacity: 100
    # write_capacity: 100
  
  eks:
    cluster_name: grey-prod
    region: us-east-1
    
    node_groups:
      - name: workers
        instance_types: [m5.2xlarge, m5.4xlarge]
        min_size: 3
        max_size: 50
        capacity_type: ON_DEMAND
        
      - name: tee-workers
        instance_types: [c5.4xlarge]  # Nitro Enclave support
        min_size: 2
        max_size: 20
        capacity_type: ON_DEMAND
        labels:
          grey.io/tee: "nitro"
  
  nitro:
    enabled: true
    enclave_cpu_count: 4
    enclave_memory_mib: 4096
  
  cost:
    enabled: true
    cost_allocation_tag: grey-tenant-id
    budget:
      name: grey-monthly
      amount: 10000
      currency: USD
      alerts: [50, 80, 100]
```

### Storage Operations

```rust
use integrations::cloud::aws_adapter::AwsAdapter;

let adapter = AwsAdapter::new(config).await?;

// Store artifact
let url = adapter.store_artifact(
    "tenant-123",
    "task-456",
    "result",
    data,
).await?;
// -> s3://grey-artifacts-prod/artifacts/tenant-123/task-456/result

// Retrieve artifact
let data = adapter.get_artifact(
    "tenant-123",
    "task-456",
    "result",
).await?;

// Store proof (replicated to DR region)
let url = adapter.store_proof(
    "tenant-123",
    "task-456",
    &proof_artifact,
).await?;

// Generate presigned URL (for client download)
let presigned = adapter.presign_artifact(
    "tenant-123",
    "task-456",
    "result",
    Duration::from_secs(3600),
).await?;
```

### Cost Tracking

```rust
// Get tenant costs
let report = adapter.get_tenant_costs(
    "tenant-123",
    "2024-01-01",
    "2024-01-31",
).await?;

println!("Total: ${}", report.total_cost);
println!("S3: ${}", report.by_service.get("Amazon S3"));
println!("EC2: ${}", report.by_service.get("Amazon EC2"));

// Check budget status
let budget = adapter.check_budget_status().await?;
if budget.percentage_used > 80.0 {
    warn!("Budget 80% consumed");
}

// Get cost forecast
let forecast = adapter.get_cost_forecast(30).await?;
println!("30-day forecast: ${}", forecast.forecasted_cost);
```

### Nitro Enclaves

```rust
// Request attestation
let attestation = adapter.request_nitro_attestation("node-123").await?;

// Verify attestation document
let verified = adapter.verify_attestation_document(
    &attestation.document,
    &expected_pcrs,
).await?;
```

---

## GCP Adapter

**File:** [integrations/cloud/gcp_adapter.rs](../../integrations/cloud/gcp_adapter.rs)

### Features

- GCS for artifact storage
- Firestore for task state (optional)
- GKE integration for compute
- Confidential VMs/GKE for TEE
- Billing API for cost attribution

### Configuration

```yaml
gcp:
  project_id: grey-prod
  region: us-central1
  
  credentials:
    # Use workload identity (recommended)
    use_workload_identity: true
    # Or service account key
    # service_account_key: /secrets/sa-key.json
  
  storage:
    bucket: grey-artifacts-prod
    location: US
    storage_class: STANDARD
    
    # Dual-region for HA
    # location: NAM4
    
    lifecycle:
      - action: SetStorageClass
        storage_class: NEARLINE
        age_days: 30
      - action: SetStorageClass
        storage_class: COLDLINE
        age_days: 90
      - action: Delete
        age_days: 365
    
    versioning: true
    
    encryption:
      type: CMEK
      kms_key: projects/grey-prod/locations/global/keyRings/grey/cryptoKeys/storage
  
  firestore:
    enabled: false
    database: grey-tasks
    collection_prefix: tasks/
  
  gke:
    cluster_name: grey-prod
    location: us-central1
    
    node_pools:
      - name: workers
        machine_type: n2-standard-8
        min_count: 3
        max_count: 50
        
      - name: confidential-workers
        machine_type: n2d-standard-8
        min_count: 2
        max_count: 20
        confidential_nodes: true
        labels:
          grey.io/tee: "sev"
  
  confidential:
    enabled: true
    platform: AMD_SEV
    # platform: AMD_SEV_SNP (preview)
  
  billing:
    enabled: true
    billing_account_id: 012345-ABCDEF-GHIJKL
    cost_tag: grey-tenant-id
    budget:
      name: grey-monthly
      amount: 10000
      currency: USD
      thresholds: [0.5, 0.8, 1.0]
```

### Storage Operations

```rust
use integrations::cloud::gcp_adapter::GcpAdapter;

let adapter = GcpAdapter::new(config).await?;

// Store artifact
let url = adapter.store_artifact(
    "tenant-123",
    "task-456",
    "result",
    data,
).await?;
// -> gs://grey-artifacts-prod/artifacts/tenant-123/task-456/result

// Conditional write (for CAS)
adapter.store_artifact_if_generation(
    "tenant-123",
    "task-456", 
    "state",
    data,
    expected_generation,  // 0 for "must not exist"
).await?;

// Signed URL
let signed = adapter.create_signed_url(
    "tenant-123",
    "task-456",
    "result",
    Duration::from_secs(3600),
    SignedUrlMethod::GET,
).await?;
```

### Cost Tracking

```rust
// Query BigQuery billing export
let costs = adapter.query_tenant_costs(
    "tenant-123",
    "2024-01-01",
    "2024-01-31",
).await?;

// Budget alerts via Pub/Sub
adapter.subscribe_budget_alerts(|alert| async {
    if alert.threshold_exceeded >= 0.8 {
        notify_ops("Budget 80% exceeded for {}", alert.budget_name);
    }
}).await?;
```

### Confidential VMs

```rust
// Request attestation report
let report = adapter.request_sev_attestation("node-123").await?;

// Verify with Google's attestation service
let verified = adapter.verify_confidential_attestation(
    &report,
    &expected_measurement,
).await?;
```

---

## Azure Adapter

**File:** [integrations/cloud/azure_adapter.rs](../../integrations/cloud/azure_adapter.rs)

### Features

- Azure Blob for artifact storage
- Cosmos DB for task state (optional)
- AKS integration for compute
- Confidential VMs (SEV-SNP, TDX) for TEE
- Cost Management for attribution

### Configuration

```yaml
azure:
  subscription_id: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
  resource_group: grey-prod
  primary_region: eastus
  secondary_region: westus2
  
  blob_storage:
    account_name: greystorageprod
    # account_key: ${AZURE_STORAGE_KEY}  # Or use managed identity
    
    artifacts_container: artifacts
    state_container: state
    proofs_container: proofs
    
    access_tier: Hot
    replication_type: GZRS  # Geo-zone-redundant
    
    enable_soft_delete: true
    soft_delete_retention_days: 30
    enable_versioning: true
    
    lifecycle_rules:
      - name: archive-old
        prefix_match: ["artifacts/"]
        tier_to_cool_after_days: 30
        tier_to_archive_after_days: 90
        delete_after_days: 365
  
  cosmos_db:
    enabled: false
    account_name: grey-cosmos-prod
    database_name: grey
    consistency_level: Session
    enable_multi_region_writes: true
    write_regions: [eastus, westeurope]
  
  aks:
    cluster_name: grey-aks-prod
    
    node_pools:
      - name: workers
        vm_size: Standard_D8s_v4
        min_count: 3
        max_count: 50
        
      - name: confidential
        vm_size: Standard_DC8s_v3  # Confidential VM
        min_count: 2
        max_count: 20
        confidential_vm: true
        labels:
          grey.io/tee: "sev-snp"
  
  cost_management:
    enabled: true
    cost_tag: grey-tenant-id
    budget_name: grey-monthly
    budget_amount: 10000
    alert_thresholds: [50, 75, 90, 100]
```

### Storage Operations

```rust
use integrations::cloud::azure_adapter::AzureAdapter;

let adapter = AzureAdapter::new(config).await?;

// Store artifact
let url = adapter.store_artifact(
    "tenant-123",
    "task-456",
    "result",
    data,
).await?;
// -> https://greystorageprod.blob.core.windows.net/artifacts/tenant-123/task-456/result

// Store with tags (for cost allocation)
adapter.store_artifact_with_tags(
    "tenant-123",
    "task-456",
    "result",
    data,
    &[
        ("grey-tenant-id", "tenant-123"),
        ("grey-task-id", "task-456"),
    ],
).await?;

// Generate SAS URL
let sas = adapter.create_sas_url(
    "tenant-123",
    "task-456",
    "result",
    Duration::from_secs(3600),
    SasPermissions::READ,
).await?;
```

### Cost Tracking

```rust
// Query Cost Management API
let report = adapter.get_tenant_costs(
    "tenant-123",
    "2024-01-01",
    "2024-01-31",
).await?;

// Check budget
let budget = adapter.check_budget_status().await?;

// Get forecast
let forecast = adapter.get_cost_forecast(30).await?;
```

### Confidential Computing

```rust
// Request MAA attestation
let attestation = adapter.request_confidential_attestation("node-123").await?;

// Verify MAA token
let verified = adapter.verify_maa_token(&attestation.maa_token).await?;
```

---

## Multi-Cloud Strategy

### Active-Active Deployment

```yaml
# grey-multicloud.yaml
multi_cloud:
  primary: aws
  secondary: gcp
  tertiary: azure
  
  routing:
    strategy: latency_based
    fallback: round_robin
    health_check_interval: 30s
  
  replication:
    # Proofs replicated to all clouds
    proofs:
      strategy: sync
      targets: [aws, gcp, azure]
    
    # Artifacts replicated async
    artifacts:
      strategy: async
      targets: [aws, gcp]
      lag_tolerance: 5m
    
    # State synced to primary only
    state:
      strategy: primary_only
```

### Failover

```rust
// Automatic failover on health check failure
let adapter = MultiCloudAdapter::new(config).await?;

// Transparent routing
let result = adapter.store_artifact(
    "tenant-123",
    "task-456",
    "result",
    data,
).await?;
// Internally routes to healthy cloud

// Explicit cloud selection
let result = adapter.store_artifact_to(
    Cloud::AWS,
    "tenant-123",
    "task-456",
    "result",
    data,
).await?;
```

---

## Cost Optimization

### Spot/Preemptible Instances

```yaml
# AWS
node_groups:
  - name: spot-workers
    capacity_type: SPOT
    instance_types: [m5.2xlarge, m5.4xlarge, m5a.2xlarge]
    min_size: 0
    max_size: 100

# GCP
node_pools:
  - name: preemptible-workers
    preemptible: true
    machine_type: n2-standard-8

# Azure
node_pools:
  - name: spot-workers
    enable_spot: true
    vm_size: Standard_D8s_v4
    spot_max_price: -1  # Pay up to on-demand price
```

### Storage Tiering

```yaml
# Automatic lifecycle management
lifecycle:
  - prefix: artifacts/
    transitions:
      - days: 30
        storage_class: INFREQUENT_ACCESS  # ~40% cheaper
      - days: 90
        storage_class: ARCHIVE           # ~75% cheaper
    expiration_days: 365
```

### Reserved Capacity

```yaml
# For predictable workloads
reserved:
  aws:
    savings_plans:
      - type: Compute
        commitment_amount: 100  # $/hour
        term: 1_YEAR
  
  gcp:
    committed_use:
      - machine_family: n2
        vcpus: 100
        memory_gb: 400
        term: 1_YEAR
  
  azure:
    reservations:
      - vm_size: Standard_D8s_v4
        quantity: 10
        term: 1_YEAR
```

---

## Security

### Encryption at Rest

```yaml
# All clouds use customer-managed keys
aws:
  s3:
    encryption:
      type: aws:kms
      kms_key_id: alias/grey-key

gcp:
  storage:
    encryption:
      type: CMEK
      kms_key: projects/.../cryptoKeys/storage

azure:
  blob_storage:
    key_vault_url: https://grey-vault.vault.azure.net/
```

### Network Security

```yaml
# Private endpoints only
aws:
  vpc_endpoints:
    - s3
    - dynamodb
    - kms

gcp:
  private_google_access: true
  vpc_service_controls: true

azure:
  private_endpoints:
    - storage
    - cosmos
    - keyvault
```

### IAM Best Practices

```yaml
# Workload identity (no long-lived credentials)
aws:
  credentials:
    use_instance_profile: true
    # IRSA for EKS pods

gcp:
  credentials:
    use_workload_identity: true

azure:
  azure_ad:
    use_managed_identity: true
```

---

## Monitoring

### Cloud-Native Metrics

```yaml
# AWS CloudWatch
aws:
  cloudwatch:
    namespace: Grey/Distributed
    dimensions:
      - Environment
      - TenantId

# GCP Cloud Monitoring
gcp:
  monitoring:
    metric_prefix: grey.googleapis.com

# Azure Monitor
azure:
  monitor:
    metrics_namespace: Grey/Distributed
```

### Cross-Cloud Aggregation

```prometheus
# Unified metrics across clouds
grey_cloud_storage_bytes{cloud="aws",tenant="t123"} 1234567890
grey_cloud_storage_bytes{cloud="gcp",tenant="t123"} 9876543210

grey_cloud_api_duration_seconds{cloud="azure",operation="upload"}
grey_cloud_api_errors_total{cloud="aws",error="throttled"}

grey_cloud_cost_usd{cloud="aws",tenant="t123",service="s3"} 123.45
```

---

*Cloud adapters are in [integrations/cloud/](../../integrations/cloud/)*
