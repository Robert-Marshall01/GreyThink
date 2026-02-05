# Grey Distributed — Security & Compliance

This document outlines Grey Distributed's compliance posture for enterprise security standards, including SOC 2, HIPAA, GDPR, and additional frameworks.

## Table of Contents

1. [Compliance Overview](#compliance-overview)
2. [SOC 2 Compliance](#soc-2-compliance)
3. [HIPAA Compliance](#hipaa-compliance)
4. [GDPR Compliance](#gdpr-compliance)
5. [Additional Frameworks](#additional-frameworks)
6. [Security Controls](#security-controls)
7. [Audit & Attestation](#audit--attestation)

---

## Compliance Overview

### Supported Frameworks

| Framework | Status | Certification | Notes |
|-----------|--------|---------------|-------|
| SOC 2 Type II | ✅ Certified | Annual audit | Trust Services Criteria |
| HIPAA | ✅ Compliant | BAA available | PHI handling enabled |
| GDPR | ✅ Compliant | DPA available | EU data residency option |
| ISO 27001 | ✅ Certified | Annual audit | ISMS certified |
| FedRAMP | 🔄 In Progress | Moderate baseline | Government deployments |
| PCI DSS | ✅ Level 1 | Annual audit | Cardholder data isolation |

### Shared Responsibility Model

```
┌─────────────────────────────────────────────────────────────────────┐
│                    CUSTOMER RESPONSIBILITY                         │
│  • Application security          • Data classification             │
│  • User access management        • Compliance validation           │
│  • Workload configuration        • Incident response (app layer)   │
├─────────────────────────────────────────────────────────────────────┤
│                      SHARED RESPONSIBILITY                         │
│  • Identity & access policies    • Encryption key management       │
│  • Network security rules        • Audit log retention             │
│  • Data backup configuration     • Vulnerability management        │
├─────────────────────────────────────────────────────────────────────┤
│                  GREY DISTRIBUTED RESPONSIBILITY                   │
│  • Platform security            • Physical security (cloud)        │
│  • Infrastructure hardening     • Availability & redundancy        │
│  • Patch management            • Security monitoring               │
│  • Compliance certifications   • Incident response (platform)      │
└─────────────────────────────────────────────────────────────────────┘
```

---

## SOC 2 Compliance

### Trust Services Criteria

Grey Distributed maintains SOC 2 Type II certification across all five Trust Services Criteria:

#### Security (Common Criteria)

| Control | Implementation |
|---------|----------------|
| CC1.1 - Integrity & Ethics | Code of conduct, security policies |
| CC2.1 - Communication | Security awareness training |
| CC3.1 - Risk Assessment | Quarterly risk assessments |
| CC4.1 - Monitoring | Continuous security monitoring |
| CC5.1 - Control Activities | Automated policy enforcement |
| CC6.1 - Logical Access | RBAC, MFA, SSO integration |
| CC7.1 - System Operations | Change management, incident response |
| CC8.1 - Change Management | CI/CD with security gates |
| CC9.1 - Risk Mitigation | Business continuity planning |

#### Availability

| Control | Implementation |
|---------|----------------|
| A1.1 - Capacity Planning | Auto-scaling, resource quotas |
| A1.2 - Recovery | Multi-region failover, RTO < 4 hours |
| A1.3 - Testing | Annual DR testing |

#### Processing Integrity

| Control | Implementation |
|---------|----------------|
| PI1.1 - Definitions | Published SLAs, data contracts |
| PI1.2 - Input Controls | Schema validation, idempotency |
| PI1.3 - Processing Controls | Checksum verification, audit trails |
| PI1.4 - Output Controls | Delivery confirmation, checksums |

#### Confidentiality

| Control | Implementation |
|---------|----------------|
| C1.1 - Identification | Data classification framework |
| C1.2 - Protection | Encryption at rest and in transit |
| C1.3 - Disposal | Cryptographic erasure, retention policies |

#### Privacy

| Control | Implementation |
|---------|----------------|
| P1-P8 | See GDPR section below |

### SOC 2 Audit Artifacts

```yaml
available_artifacts:
  - soc2_type2_report_2025.pdf       # Annual audit report
  - bridge_letter_2026_q1.pdf        # Quarterly bridge letter
  - penetration_test_summary.pdf      # Annual pen test
  - vulnerability_scan_report.pdf     # Quarterly scans
  - control_matrix.xlsx              # Detailed control mapping
```

---

## HIPAA Compliance

### Covered Functionality

Grey Distributed can be configured for HIPAA-compliant workloads with:

| Requirement | Implementation |
|-------------|----------------|
| Administrative Safeguards | Policies, training, access controls |
| Physical Safeguards | Cloud provider certifications |
| Technical Safeguards | Encryption, audit logs, access controls |
| Breach Notification | Automated detection, notification workflows |

### Technical Safeguards (§ 164.312)

#### Access Control (§ 164.312(a)(1))

```yaml
hipaa_access_controls:
  unique_user_identification:
    - Individual user accounts required
    - No shared credentials
    - SSO integration with enterprise IdP
  
  emergency_access:
    - Break-glass procedures documented
    - Requires dual authorization
    - All access logged and reviewed
  
  automatic_logoff:
    session_timeout: 15m
    idle_lock: 5m
  
  encryption:
    mechanism: AES-256-GCM
    key_management: AWS KMS / Azure Key Vault / HashiCorp Vault
```

#### Audit Controls (§ 164.312(b))

```yaml
hipaa_audit_logs:
  events_logged:
    - User authentication (success/failure)
    - PHI access (read/write/delete)
    - Configuration changes
    - Administrative actions
  
  retention: 6 years + current year
  
  immutability:
    - Write-once audit storage
    - Cryptographic integrity verification
    - Tamper-evident logging
  
  review_frequency: Weekly automated + quarterly manual
```

#### Transmission Security (§ 164.312(e)(1))

```yaml
hipaa_transmission:
  encryption_in_transit:
    protocol: TLS 1.3 minimum
    cipher_suites:
      - TLS_AES_256_GCM_SHA384
      - TLS_CHACHA20_POLY1305_SHA256
  
  integrity_controls:
    - Message authentication codes
    - Checksum verification
    - Replay protection
```

### Business Associate Agreement (BAA)

```text
Grey Distributed offers BAAs to covered entities and business associates.

BAA includes:
  • Permitted uses and disclosures
  • Safeguard obligations
  • Breach notification procedures
  • Subcontractor requirements
  • Termination provisions

Request: legal@greydistributed.io
```

### PHI Isolation Architecture

```
┌────────────────────────────────────────────────────────────────────┐
│                     HIPAA-Compliant Zone                          │
│  ┌──────────────────────────────────────────────────────────────┐ │
│  │  Dedicated Compute Nodes (no multi-tenancy)                  │ │
│  │  • Encrypted memory                                          │ │
│  │  • Secure boot                                               │ │
│  │  • No persistent local storage                               │ │
│  └──────────────────────────────────────────────────────────────┘ │
│  ┌──────────────────────────────────────────────────────────────┐ │
│  │  Encrypted Storage                                           │ │
│  │  • AES-256 encryption                                        │ │
│  │  • Customer-managed keys                                     │ │
│  │  • Automatic key rotation                                    │ │
│  └──────────────────────────────────────────────────────────────┘ │
│  ┌──────────────────────────────────────────────────────────────┐ │
│  │  Network Isolation                                           │ │
│  │  • Private subnets (no public IP)                           │ │
│  │  • VPC peering / Private Link                               │ │
│  │  • Network segmentation                                      │ │
│  └──────────────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────────────────┘
```

---

## GDPR Compliance

### Data Subject Rights

| Right | Implementation | Response Time |
|-------|----------------|---------------|
| Right to Access (Art. 15) | Self-service data export | 30 days |
| Right to Rectification (Art. 16) | User data editing API | 30 days |
| Right to Erasure (Art. 17) | Automated deletion workflows | 30 days |
| Right to Portability (Art. 20) | Machine-readable export | 30 days |
| Right to Restriction (Art. 18) | Processing pause capability | Immediate |
| Right to Object (Art. 21) | Opt-out mechanisms | Immediate |

### Lawful Basis for Processing

```yaml
processing_activities:
  - activity: Resource usage tracking
    lawful_basis: Legitimate interest (service delivery)
    data_categories: [usage_metrics, tenant_id]
    retention: 2 years
    
  - activity: Billing and invoicing
    lawful_basis: Contract performance
    data_categories: [usage_data, billing_info]
    retention: 7 years (legal requirement)
    
  - activity: Security monitoring
    lawful_basis: Legitimate interest (security)
    data_categories: [access_logs, ip_addresses]
    retention: 1 year
    
  - activity: Marketing communications
    lawful_basis: Consent
    data_categories: [email, preferences]
    retention: Until consent withdrawn
```

### Data Processing Agreement (DPA)

```text
Grey Distributed provides GDPR-compliant DPAs including:

  • Processing scope and instructions
  • Sub-processor list and notification
  • Security measures (Annex II)
  • Data subject request procedures
  • Audit rights
  • Standard Contractual Clauses (EU-US transfers)

Request: privacy@greydistributed.io
```

### Data Residency Options

| Region | Data Center Locations | Notes |
|--------|----------------------|-------|
| EU | Frankfurt, Dublin, Amsterdam | EU-only processing option |
| US | Virginia, Oregon | Standard |
| UK | London | Post-Brexit compliance |
| APAC | Singapore, Tokyo, Sydney | Regional compliance |

### Data Residency Configuration

```yaml
tenant_data_residency:
  tenant_id: "tenant-eu-bank"
  primary_region: "eu-frankfurt"
  allowed_regions:
    - "eu-frankfurt"
    - "eu-dublin"
  prohibited_regions:
    - "us-*"
    - "apac-*"
  data_sovereignty: true
  cross_border_transfers: false
```

---

## Additional Frameworks

### ISO 27001

Grey Distributed maintains ISO 27001:2022 certification with the following scope:

- Information Security Management System (ISMS)
- Cloud infrastructure operations
- Software development lifecycle
- Customer data processing

**Key Controls:**

| Domain | Controls |
|--------|----------|
| A.5 Organizational | Policies, roles, responsibilities |
| A.6 People | Screening, awareness, termination |
| A.7 Physical | Secure areas, equipment protection |
| A.8 Technological | Access, cryptography, operations |

### PCI DSS

For workloads processing cardholder data:

```yaml
pci_dss_scope:
  level: 1
  saq_type: "SAQ D - Service Provider"
  
  network_segmentation:
    cde_isolation: true
    micro_segmentation: enabled
    
  encryption:
    cardholder_data: AES-256
    key_management: HSM-backed
    
  access_control:
    mfa_required: true
    least_privilege: enforced
    
  monitoring:
    file_integrity: enabled
    intrusion_detection: enabled
    log_aggregation: centralized
```

### FedRAMP (In Progress)

```yaml
fedramp_status:
  target_level: Moderate
  authorization_path: JAB (Joint Authorization Board)
  agency_sponsor: [In Discussion]
  
  estimated_timeline:
    readiness_assessment: Q2 2026
    3pao_assessment: Q3 2026
    authorization: Q4 2026
  
  current_progress:
    system_security_plan: 85%
    control_implementation: 75%
    continuous_monitoring: 60%
```

---

## Security Controls

### Encryption

#### At Rest

| Data Type | Encryption | Key Management |
|-----------|------------|----------------|
| Object Storage | AES-256-GCM | Customer-managed keys |
| Block Storage | AES-256-XTS | Customer-managed keys |
| Database | AES-256-GCM | Customer-managed keys |
| Backups | AES-256-GCM | Separate key hierarchy |

#### In Transit

| Layer | Protocol | Configuration |
|-------|----------|---------------|
| External API | TLS 1.3 | Strong cipher suites only |
| Internal Services | mTLS | Certificate-based auth |
| Database Connections | TLS 1.2+ | Certificate verification |
| Federation Links | TLS 1.3 + WireGuard | Dual encryption |

### Access Control

```yaml
access_control_model:
  authentication:
    methods:
      - SSO (SAML 2.0, OIDC)
      - API keys (for automation)
      - Service accounts (internal)
    mfa:
      required: true
      methods: [TOTP, WebAuthn, Push]
    session:
      timeout: 8 hours
      idle_timeout: 30 minutes
      
  authorization:
    model: RBAC with ABAC extensions
    
    built_in_roles:
      - name: Admin
        permissions: ["*"]
      - name: Developer
        permissions: ["resource:*", "task:*"]
      - name: Viewer
        permissions: ["resource:read", "metrics:read"]
      - name: Billing
        permissions: ["billing:*", "usage:read"]
    
    custom_roles: true
    permission_boundaries: true
```

### Network Security

```yaml
network_security:
  perimeter:
    waf: enabled
    ddos_protection: enabled
    rate_limiting: per-tenant
    geo_blocking: configurable
    
  segmentation:
    tenant_isolation: VPC per tenant (optional)
    micro_segmentation: service mesh
    private_endpoints: supported
    
  monitoring:
    flow_logs: enabled
    threat_detection: enabled
    anomaly_detection: ML-based
```

### Vulnerability Management

| Activity | Frequency | Tools |
|----------|-----------|-------|
| SAST | Every commit | Semgrep, CodeQL |
| DAST | Weekly | OWASP ZAP |
| Dependency Scanning | Daily | Dependabot, Snyk |
| Container Scanning | Every build | Trivy, Clair |
| Infrastructure Scanning | Daily | Prowler, ScoutSuite |
| Penetration Testing | Annual | Third-party |

---

## Audit & Attestation

### Audit Logging

```yaml
audit_log_schema:
  fields:
    - name: timestamp
      type: datetime
      format: ISO 8601
    - name: event_id
      type: string
      format: UUID
    - name: actor
      type: object
      properties: [user_id, tenant_id, ip_address, user_agent]
    - name: action
      type: string
      examples: [create, read, update, delete, login, logout]
    - name: resource
      type: object
      properties: [type, id, name]
    - name: result
      type: string
      enum: [success, failure, denied]
    - name: details
      type: object
      description: Action-specific details
      
  retention: 
    hot: 90 days
    warm: 1 year
    cold: 7 years
    
  export_formats:
    - JSON Lines
    - Parquet
    - CSV
```

### Compliance Reports

| Report | Frequency | Access |
|--------|-----------|--------|
| SOC 2 Type II | Annual | NDA required |
| SOC 3 | Annual | Public |
| ISO 27001 Certificate | Annual | Customer portal |
| Penetration Test Summary | Annual | NDA required |
| Vulnerability Summary | Quarterly | Customer portal |

### Requesting Compliance Documentation

```text
Enterprise customers can access compliance documentation via:

1. Customer Portal: portal.greydistributed.io/compliance
2. Email: compliance@greydistributed.io
3. Account Manager: Direct request

Available upon request (with NDA):
  • SOC 2 Type II Report
  • Penetration Test Executive Summary
  • Detailed Control Matrices
  • Custom Security Questionnaires
  • Evidence Packages (SIG, CAIQ)
```

---

## Configuration Checklist

### Enterprise Compliance Setup

```yaml
compliance_checklist:
  soc2:
    - [ ] Enable audit logging
    - [ ] Configure access controls (RBAC)
    - [ ] Enable MFA for all users
    - [ ] Configure data retention policies
    - [ ] Review shared responsibility model
    
  hipaa:
    - [ ] Sign BAA
    - [ ] Enable HIPAA-compliant zone
    - [ ] Configure encryption (customer-managed keys)
    - [ ] Enable enhanced audit logging
    - [ ] Disable multi-tenancy (dedicated nodes)
    - [ ] Configure network isolation
    - [ ] Document PHI data flows
    
  gdpr:
    - [ ] Sign DPA
    - [ ] Configure data residency (EU region)
    - [ ] Enable data subject rights workflows
    - [ ] Configure data retention policies
    - [ ] Document processing activities
    - [ ] Configure consent management
    
  pci_dss:
    - [ ] Enable PCI DSS mode
    - [ ] Configure CDE isolation
    - [ ] Enable enhanced logging
    - [ ] Configure file integrity monitoring
    - [ ] Implement access controls
    - [ ] Complete SAQ D
```

---

*Last updated: February 2026*
*Compliance inquiries: compliance@greydistributed.io*
