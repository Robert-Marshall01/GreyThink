# Grey Distributed — Enterprise Deployment Playbook

This playbook provides a step-by-step guide for deploying Grey Distributed in enterprise environments with full integration into existing infrastructure.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Phase 1: Planning & Assessment](#phase-1-planning--assessment)
3. [Phase 2: Infrastructure Preparation](#phase-2-infrastructure-preparation)
4. [Phase 3: Core Deployment](#phase-3-core-deployment)
5. [Phase 4: Integration](#phase-4-integration)
6. [Phase 5: Security Hardening](#phase-5-security-hardening)
7. [Phase 6: Production Cutover](#phase-6-production-cutover)
8. [Post-Deployment Operations](#post-deployment-operations)

---

## Prerequisites

### Technical Requirements

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| Kubernetes | v1.28+ | v1.30+ |
| CPU cores (control plane) | 16 | 32 |
| RAM (control plane) | 64 GB | 128 GB |
| Storage | 500 GB NVMe | 2 TB NVMe |
| Network | 10 Gbps | 25 Gbps |
| Availability zones | 2 | 3+ |

### Organizational Requirements

- [ ] Executive sponsor identified
- [ ] Budget approved ($100K-$500K typical Year 1)
- [ ] Security team sign-off
- [ ] Network team engaged
- [ ] DBA/storage team consulted
- [ ] Change management process established

### Team Composition

| Role | FTE | Skills |
|------|-----|--------|
| Platform Engineer | 2-3 | Kubernetes, Rust, distributed systems |
| Security Engineer | 1 | IAM, PKI, compliance |
| Network Engineer | 0.5 | SDN, load balancing |
| DBA | 0.5 | Distributed storage, backup |
| Project Manager | 1 | Agile, stakeholder management |

---

## Phase 1: Planning & Assessment

**Duration:** 2-4 weeks

### Step 1.1: Workload Assessment

```bash
# Run workload assessment tool
greyctl assess workloads \
  --source kubernetes \
  --namespace production \
  --output report.json

# Generate sizing recommendations
greyctl assess sizing \
  --input report.json \
  --target-utilization 0.7 \
  --growth-factor 1.5
```

**Output:** Workload inventory with resource requirements

### Step 1.2: Network Topology Design

```yaml
# network_design.yaml
topology:
  regions:
    - name: us-east-1
      zones: [a, b, c]
      primary: true
      ingress:
        type: nlb
        ports: [443, 6443]
    - name: us-west-2
      zones: [a, b]
      primary: false
      federation: true

  interconnect:
    type: dedicated  # dedicated | vpn | internet
    bandwidth: 10gbps
    encryption: wireguard
    
  dns:
    provider: route53
    zone: grey.internal.company.com
```

### Step 1.3: Security Architecture Review

Checklist:
- [ ] Network segmentation plan
- [ ] PKI/certificate strategy
- [ ] Identity provider integration (SAML/OIDC)
- [ ] Secrets management (Vault/AWS SM/Azure KV)
- [ ] Audit logging destination
- [ ] DLP/data classification alignment

### Step 1.4: Integration Inventory

| System | Integration Type | Priority | Complexity |
|--------|------------------|----------|------------|
| Active Directory | Identity | P0 | Medium |
| SAP ERP | Data sync | P1 | High |
| Salesforce | API | P1 | Medium |
| ServiceNow | ITSM | P2 | Low |
| Splunk/Datadog | Observability | P0 | Low |

---

## Phase 2: Infrastructure Preparation

**Duration:** 2-3 weeks

### Step 2.1: Kubernetes Cluster Provisioning

```bash
# Using Terraform (example: AWS EKS)
cd deploy/terraform/aws

terraform init
terraform plan \
  -var="cluster_name=grey-prod" \
  -var="kubernetes_version=1.30" \
  -var="node_count=10" \
  -var="node_instance_type=m6i.4xlarge" \
  -var="enable_federation=true" \
  -out=plan.tfplan

terraform apply plan.tfplan
```

### Step 2.2: Storage Configuration

```yaml
# storage-class.yaml
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: grey-fast
provisioner: ebs.csi.aws.com
parameters:
  type: gp3
  iops: "16000"
  throughput: "1000"
  encrypted: "true"
  kmsKeyId: arn:aws:kms:us-east-1:xxx:key/xxx
volumeBindingMode: WaitForFirstConsumer
allowVolumeExpansion: true
---
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: grey-archive
provisioner: s3.csi.aws.com
parameters:
  bucket: grey-archive-prod
  region: us-east-1
```

### Step 2.3: Network Policies

```yaml
# network-policy-baseline.yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: grey-baseline
  namespace: grey-system
spec:
  podSelector: {}
  policyTypes:
    - Ingress
    - Egress
  ingress:
    - from:
        - namespaceSelector:
            matchLabels:
              grey.io/allowed: "true"
        - podSelector:
            matchLabels:
              grey.io/component: "true"
  egress:
    - to:
        - namespaceSelector:
            matchLabels:
              grey.io/allowed: "true"
    - to:
        - ipBlock:
            cidr: 10.0.0.0/8  # Internal network
    - ports:
        - port: 53
          protocol: UDP  # DNS
```

### Step 2.4: Certificate Infrastructure

```bash
# Generate root CA (or integrate with existing PKI)
greyctl pki init \
  --ca-common-name "Grey Distributed Enterprise CA" \
  --ca-validity 10y \
  --output /secure/pki/

# Generate cluster certificates
greyctl pki generate \
  --type cluster \
  --cluster-name grey-prod \
  --validity 1y \
  --ca-cert /secure/pki/ca.crt \
  --ca-key /secure/pki/ca.key
```

---

## Phase 3: Core Deployment

**Duration:** 1-2 weeks

### Step 3.1: Install Grey Distributed Operator

```bash
# Add Helm repository
helm repo add grey https://charts.grey.io
helm repo update

# Install operator
helm install grey-operator grey/grey-operator \
  --namespace grey-system \
  --create-namespace \
  --values values-enterprise.yaml \
  --version 2.5.0
```

**values-enterprise.yaml:**
```yaml
operator:
  replicas: 3
  resources:
    requests:
      cpu: 2
      memory: 4Gi
    limits:
      cpu: 4
      memory: 8Gi

security:
  podSecurityPolicy: restricted
  serviceAccount:
    annotations:
      iam.amazonaws.com/role: arn:aws:iam::xxx:role/grey-operator

monitoring:
  enabled: true
  serviceMonitor: true
  
logging:
  format: json
  level: info
  destination: stdout

ha:
  enabled: true
  leaderElection: true
```

### Step 3.2: Deploy Control Plane

```bash
# Deploy control plane cluster
kubectl apply -f - <<EOF
apiVersion: grey.io/v1
kind: GreyCluster
metadata:
  name: grey-prod
  namespace: grey-system
spec:
  version: "2.5.0"
  
  controlPlane:
    replicas: 3
    resources:
      requests: {cpu: 8, memory: 32Gi}
      limits: {cpu: 16, memory: 64Gi}
    storage:
      class: grey-fast
      size: 200Gi
  
  consensus:
    algorithm: raft
    electionTimeout: 1000ms
    heartbeatInterval: 100ms
  
  scheduler:
    algorithm: drf
    preemption: true
    binPacking: bestFit
  
  federation:
    enabled: true
    mode: active
    peers: []  # Added in Phase 4
  
  security:
    tls:
      enabled: true
      certSecret: grey-tls-certs
    authentication:
      type: oidc
      oidc:
        issuer: https://login.company.com
        clientId: grey-distributed
    authorization:
      type: rbac
      defaultPolicy: deny
EOF
```

### Step 3.3: Verify Core Deployment

```bash
# Check cluster health
greyctl cluster status --name grey-prod

# Expected output:
# Cluster: grey-prod
# Status: Healthy
# Control Plane: 3/3 nodes ready
# Consensus: Raft leader elected (node-0)
# Federation: Enabled, 0 peers
# Scheduler: Running (DRF mode)
# Last checkpoint: 2026-02-04T10:30:00Z

# Run smoke tests
greyctl test smoke --cluster grey-prod
```

---

## Phase 4: Integration

**Duration:** 2-4 weeks

### Step 4.1: Identity Provider Integration

```yaml
# oidc-config.yaml
apiVersion: grey.io/v1
kind: AuthConfig
metadata:
  name: enterprise-auth
  namespace: grey-system
spec:
  oidc:
    issuer: https://login.company.com
    clientId: grey-distributed
    clientSecretRef:
      name: oidc-secret
      key: client-secret
    scopes: [openid, profile, email, groups]
    groupsClaim: groups
    usernameClaim: email
  
  groupMappings:
    - group: CN=Grey-Admins,OU=Groups,DC=company,DC=com
      role: cluster-admin
    - group: CN=Grey-Operators,OU=Groups,DC=company,DC=com
      role: operator
    - group: CN=Grey-Developers,OU=Groups,DC=company,DC=com
      role: developer
```

### Step 4.2: Observability Integration

```yaml
# observability-config.yaml
apiVersion: grey.io/v1
kind: ObservabilityConfig
metadata:
  name: enterprise-observability
spec:
  metrics:
    enabled: true
    destination: prometheus
    remoteWrite:
      url: https://prometheus.company.com/api/v1/write
      tlsConfig:
        certSecret: prometheus-tls
  
  logging:
    enabled: true
    destination: splunk
    splunk:
      endpoint: https://splunk-hec.company.com:8088
      tokenSecretRef:
        name: splunk-token
      index: grey-distributed
      sourcetype: grey:logs
  
  tracing:
    enabled: true
    destination: jaeger
    jaeger:
      endpoint: https://jaeger.company.com:14268/api/traces
```

### Step 4.3: ERP Integration

See [integration_examples.rs](integration_examples.rs) for detailed code.

```bash
# Deploy SAP connector
kubectl apply -f - <<EOF
apiVersion: grey.io/v1
kind: Integration
metadata:
  name: sap-erp
spec:
  type: erp
  provider: sap
  connection:
    host: sap.company.com
    port: 443
    clientId: 100
    credentialsRef:
      name: sap-credentials
  sync:
    interval: 5m
    resources: [cost-centers, projects, users]
  billing:
    enabled: true
    costAllocation: true
EOF
```

### Step 4.4: ITSM Integration (ServiceNow)

```yaml
# servicenow-integration.yaml
apiVersion: grey.io/v1
kind: Integration
metadata:
  name: servicenow
spec:
  type: itsm
  provider: servicenow
  connection:
    instance: company.service-now.com
    credentialsRef:
      name: servicenow-credentials
  
  incidents:
    enabled: true
    autoCreate:
      onAlert: [critical, high]
      assignmentGroup: "Cloud Platform Team"
    syncStatus: bidirectional
  
  changes:
    enabled: true
    requireApproval:
      actions: [scale, upgrade, federation-add]
    approvalGroup: "CAB-Cloud"
```

---

## Phase 5: Security Hardening

**Duration:** 1-2 weeks

### Step 5.1: Enable Audit Logging

```yaml
apiVersion: grey.io/v1
kind: AuditPolicy
metadata:
  name: enterprise-audit
spec:
  level: RequestResponse
  
  rules:
    - level: Metadata
      resources:
        - group: ""
          resources: ["pods", "services"]
    - level: Request
      resources:
        - group: "grey.io"
          resources: ["*"]
    - level: RequestResponse
      users: ["system:admin"]
  
  destination:
    type: s3
    s3:
      bucket: grey-audit-logs
      region: us-east-1
      encryption: aws:kms
      kmsKeyId: arn:aws:kms:us-east-1:xxx:key/xxx
  
  retention:
    days: 2555  # 7 years for compliance
```

### Step 5.2: Enable Encryption at Rest

```bash
# Enable transparent data encryption
greyctl security encryption enable \
  --cluster grey-prod \
  --kms-provider aws \
  --kms-key-id arn:aws:kms:us-east-1:xxx:key/xxx \
  --rotation-period 90d
```

### Step 5.3: Network Segmentation

```yaml
# Implement microsegmentation
apiVersion: grey.io/v1
kind: SecurityZone
metadata:
  name: production
spec:
  namespaces:
    - grey-system
    - grey-workloads
  
  ingress:
    allowed:
      - zone: dmz
        ports: [443]
      - zone: monitoring
        ports: [9090, 9091]
  
  egress:
    allowed:
      - zone: database
        ports: [5432, 3306]
      - destination: internet
        ports: [443]
        domains:
          - "*.company.com"
          - "api.grey.io"
```

### Step 5.4: Vulnerability Scanning

```bash
# Enable continuous scanning
greyctl security scan enable \
  --cluster grey-prod \
  --scan-interval 24h \
  --severity-threshold high \
  --alert-destination slack:#security-alerts
```

---

## Phase 6: Production Cutover

**Duration:** 1 week

### Step 6.1: Pre-Cutover Checklist

```markdown
## Pre-Production Checklist

### Infrastructure
- [ ] All nodes healthy (3/3 control plane, N/N workers)
- [ ] Storage provisioned and tested
- [ ] Network policies applied and verified
- [ ] DNS configured and propagated
- [ ] Load balancers healthy

### Security
- [ ] TLS certificates valid (>30 days)
- [ ] OIDC authentication tested
- [ ] RBAC policies reviewed
- [ ] Audit logging confirmed
- [ ] Vulnerability scan passed

### Integration
- [ ] Identity provider connected
- [ ] Monitoring dashboards live
- [ ] Alerting configured
- [ ] ITSM integration tested
- [ ] Backup/restore tested

### Operations
- [ ] Runbooks documented
- [ ] On-call rotation set
- [ ] Escalation paths defined
- [ ] DR procedure tested
- [ ] Performance baseline established
```

### Step 6.2: Staged Rollout

```bash
# Phase 1: Canary (5% traffic)
greyctl migration start \
  --source kubernetes-legacy \
  --target grey-prod \
  --strategy canary \
  --percentage 5 \
  --workloads "app=non-critical"

# Monitor for 24 hours, then proceed

# Phase 2: Expand (25% traffic)
greyctl migration expand --percentage 25

# Phase 3: Majority (75% traffic)
greyctl migration expand --percentage 75

# Phase 4: Complete (100% traffic)
greyctl migration complete
```

### Step 6.3: Cutover Validation

```bash
# Run full validation suite
greyctl validate production \
  --cluster grey-prod \
  --checks all \
  --timeout 30m

# Expected output:
# ✓ Control plane health
# ✓ Worker node health  
# ✓ Storage performance
# ✓ Network connectivity
# ✓ Authentication flow
# ✓ Authorization policies
# ✓ Monitoring pipeline
# ✓ Alerting delivery
# ✓ Backup integrity
# 
# All 9 checks passed. Production ready.
```

---

## Post-Deployment Operations

### Day 2 Operations

| Task | Frequency | Owner |
|------|-----------|-------|
| Health check review | Daily | Platform Team |
| Security scan review | Weekly | Security Team |
| Capacity planning | Monthly | Platform Team |
| Cost optimization | Monthly | FinOps |
| Upgrade assessment | Quarterly | Platform Team |
| DR test | Quarterly | Platform + DR Team |
| Compliance audit | Annually | Compliance + Security |

### Upgrade Path

```bash
# Check available upgrades
greyctl upgrade check --cluster grey-prod

# Plan upgrade
greyctl upgrade plan \
  --cluster grey-prod \
  --target-version 2.6.0 \
  --output upgrade-plan.yaml

# Execute upgrade (rolling)
greyctl upgrade execute \
  --plan upgrade-plan.yaml \
  --strategy rolling \
  --max-unavailable 1
```

### Support Escalation

| Severity | Response Time | Escalation Path |
|----------|---------------|-----------------|
| P1 (Critical) | 15 minutes | Platform Lead → Director → VP |
| P2 (High) | 1 hour | Platform Team → Platform Lead |
| P3 (Medium) | 4 hours | Platform Team |
| P4 (Low) | 1 business day | Platform Team queue |

---

*Last updated: February 2026*
