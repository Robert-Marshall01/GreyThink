# 🧬 Digital Organism: Compliance-as-Code

> **A Living Digital Organism** - PostgreSQL Compliance Auditing Database using biological metaphors to protect your infrastructure through an immune system-inspired architecture.

[![Terraform](https://img.shields.io/badge/Terraform->=1.5.0-623CE4?logo=terraform)](https://www.terraform.io/)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-16-336791?logo=postgresql)](https://www.postgresql.org/)
[![Docker](https://img.shields.io/badge/Docker-Compose-2496ED?logo=docker)](https://www.docker.com/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

---

## 🌱 The Digital Organism Metaphor

This project treats compliance as a **biological system**:

| Biological System | Digital Equivalent | Module | Table |
|-------------------|-------------------|--------|-------|
| 🧬 **DNA/Genetic Code** | Database roles & extensions | `database` | - |
| 🛡️ **Antibody Library** | Threat detection patterns | `compliance` | `immune_rules` |
| 🧠 **Adaptive Memory** | Compliance rule definitions | `compliance` | `compliance_rules` |
| 🔬 **Sensory Memory** | Audit & response logging | `audit` | `resource_audit` |

---

## 🚀 Quick Start - Birth Your Digital Organism

### Step 1: Start Postgres with Docker

```bash
# Navigate to project directory
cd compliance-as-code

# Start PostgreSQL container (the organism's host body)
docker-compose up -d

# Verify it's running (organism is alive!)
docker-compose ps
pg_isready -h localhost -p 5432
```

### Step 2: Initialize and Apply Terraform

```bash
# Set credentials (the organism's access keys) - REQUIRED!
export TF_VAR_postgres_username="admin"
export TF_VAR_postgres_password="<YOUR_ADMIN_PASSWORD>"
export TF_VAR_compliance_user_password="<YOUR_COMPLIANCE_PASSWORD>"

# Initialize Terraform (download organism DNA)
terraform init

# Review the organism's structure
terraform plan

# Birth the digital organism!
terraform apply -auto-approve
```

### Step 3: Verify Tables with psql

```bash
# Connect to the organism's brain
PGPASSWORD='<YOUR_ADMIN_PASSWORD>' psql -h localhost -p 5432 -U admin -d compliance_db

# List all organism organs (tables)
\dt compliance.*

# View the antibody library (immune_rules)
SELECT antibody_name, threat_signature, severity_level FROM compliance.immune_rules;

# View adaptive memory (compliance_rules)
SELECT rule_name, severity FROM compliance.compliance_rules;

# Exit psql
\q
```

**Expected output:**
```
             List of relations
   Schema   |       Name       | Type  | Owner 
------------+------------------+-------+-------
 compliance | audit_summary    | view  | admin
 compliance | compliance_rules | table | admin
 compliance | immune_rules     | table | admin
 compliance | resource_audit   | table | admin
```

---

## ✨ Features

| Feature | Description |
|---------|-------------|
| 🐳 **Local PostgreSQL** | Docker-based development environment (no cloud required) |
| 🧬 **Digital Organism** | Biological metaphors for intuitive understanding |
| 🛡️ **Immune System** | `immune_rules` table with antibody patterns |
| 🧠 **Adaptive Memory** | `compliance_rules` table for policy enforcement |
| 🔬 **Sensory Logging** | `resource_audit` table for response tracking |
| 📦 **Modular Architecture** | Separate `database`, `compliance`, and `audit` modules |
| 🔐 **Secure by Default** | No hardcoded passwords; environment variables required |
| 🔄 **CI/CD Ready** | GitHub Actions workflow included |
| ☸️ **Kubernetes Ready** | Full K8s manifests with Kustomize |
| 🐋 **Containerized** | Dockerfile for Terraform runner |

---

## 🐳 Docker Containerization

### Option A: Docker Compose (Recommended for Development)

```bash
# Set credentials
export TF_VAR_postgres_username="admin"
export TF_VAR_postgres_password="<YOUR_ADMIN_PASSWORD>"
export TF_VAR_compliance_user_password="<YOUR_COMPLIANCE_PASSWORD>"

# Start PostgreSQL only
docker compose up -d postgres

# Run Terraform via container
docker compose run --rm terraform init
docker compose run --rm terraform apply -auto-approve

# Run compliance scan
docker compose --profile scan up scanner

# View all running services
docker compose ps

# Stop everything
docker compose down

# Remove all data
docker compose down -v
```

### Option B: Build and Run Terraform Image Manually

```bash
# Build the Terraform runner image
docker build -t compliance-terraform .

# Run Terraform commands
docker run --rm \
  -e TF_VAR_postgres_host=host.docker.internal \
  -e TF_VAR_postgres_password="<YOUR_PASSWORD>" \
  -e TF_VAR_compliance_user_password="<YOUR_COMPLIANCE_PASSWORD>" \
  compliance-terraform plan

# Interactive shell
docker run --rm -it \
  -e TF_VAR_postgres_password="<YOUR_PASSWORD>" \
  compliance-terraform /bin/bash
```

---

## ☸️ Kubernetes Deployment

Deploy the Digital Organism to any Kubernetes cluster.

### Prerequisites

- Kubernetes cluster (minikube, kind, EKS, AKS, GKE, etc.)
- `kubectl` configured
- Docker (for building images)

### Quick Deploy

```bash
# Set credentials
export TF_VAR_postgres_username="admin"
export TF_VAR_postgres_password="<YOUR_ADMIN_PASSWORD>"
export TF_VAR_compliance_user_password="<YOUR_COMPLIANCE_PASSWORD>"

# Use the deploy script
cd k8s
./deploy.sh deploy    # Build image + deploy all resources
./deploy.sh apply     # Run Terraform job
./deploy.sh scan      # Run compliance scan
./deploy.sh status    # View pod status
./deploy.sh logs      # View logs
./deploy.sh destroy   # Remove everything
```

### Manual Deployment

```bash
# 1. Build the Docker image
docker build -t compliance-terraform:latest .

# 2. Create namespace and secrets
kubectl create namespace compliance-system

kubectl create secret generic postgres-credentials \
  --from-literal=username=admin \
  --from-literal=password='<YOUR_ADMIN_PASSWORD>' \
  --from-literal=compliance-username=compliance_user \
  --from-literal=compliance-password='<YOUR_COMPLIANCE_PASSWORD>' \
  -n compliance-system

# 3. Deploy all resources with Kustomize
kubectl apply -k k8s/

# 4. Wait for PostgreSQL to be ready
kubectl wait --for=condition=ready pod -l app=postgres -n compliance-system --timeout=120s

# 5. Run Terraform to initialize the organism
kubectl apply -f k8s/terraform-job.yaml

# 6. Watch the job logs
kubectl logs -f job/terraform-apply -n compliance-system
```

### Kubernetes Resources

| Resource | File | Purpose |
|----------|------|---------|
| Namespace | `namespace.yaml` | Isolated environment |
| ConfigMap | `configmap.yaml` | Non-sensitive config |
| Secret | `secrets.yaml` | Credentials (template) |
| Deployment | `postgres-deployment.yaml` | PostgreSQL + PVC |
| Job | `terraform-job.yaml` | One-time Terraform apply |
| CronJob | `cronjob.yaml` | Hourly compliance scans |
| Kustomization | `kustomization.yaml` | Deploy all resources |

### Access PostgreSQL in Kubernetes

```bash
# Port forward to localhost
kubectl port-forward svc/postgres-service 5432:5432 -n compliance-system

# Connect with psql
PGPASSWORD='<YOUR_PASSWORD>' psql -h localhost -p 5432 -U admin -d compliance_db
```

---

## 🏗️ Digital Organism Architecture

```text
┌─────────────────────────────────────────────────────────────────────────┐
│                    🧬 Digital Organism Architecture                      │
│                         compliance-as-code                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐              │
│  │   DATABASE   │    │  COMPLIANCE  │    │    AUDIT     │              │
│  │  (DNA/Genes) │───▶│(Immune Sys.) │───▶│(Nervous Sys.)│              │
│  └──────────────┘    └──────────────┘    └──────────────┘              │
│         │                   │                   │                       │
│         ▼                   ▼                   ▼                       │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐              │
│  │ • uuid-ossp  │    │ • Schema     │    │ • Audit tbl  │              │
│  │ • User role  │    │ • immune_    │    │   (sensory   │              │
│  │ • Metadata   │    │   rules 🛡️   │    │    memory)   │              │
│  │              │    │ • compliance_│    │ • Summary    │              │
│  │              │    │   rules 🧠   │    │   views      │              │
│  └──────────────┘    └──────────────┘    └──────────────┘              │
│                                                                          │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                 PostgreSQL Container (Docker)                      │  │
│  │     compliance_db → compliance schema → 3 tables + 1 view         │  │
│  └───────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
```

### Module Responsibilities (Organ Systems)

| Module | Biological Role | Resources Created |
| ------ | --------------- | ----------------- |
| `database` | 🧬 DNA/Genetic Code | `uuid-ossp` extension, `compliance_user` role |
| `compliance` | 🛡️ Immune System | `immune_rules`, `compliance_rules` tables, seed data |
| `audit` | 🔬 Nervous System | `resource_audit` table, `audit_summary` view |

```text
compliance-as-code/
├── main.tf                 # Root config - orchestrates modules
├── variables.tf            # Input variables (no default passwords!)
├── outputs.tf              # Validation outputs + connection info
├── docker-compose.yml      # PostgreSQL 16 container
├── .gitignore              # Ignores .env, tfstate, secrets
├── README.md               # This file
│
├── modules/
│   ├── database/           # Database foundation module
│   │   ├── main.tf         # Extension + role creation
│   │   ├── variables.tf    # Connection params
│   │   └── outputs.tf      # Role name, extension status
│   │
│   ├── compliance/         # Compliance rules module
│   │   ├── main.tf         # Schema + rules table + seeding
│   │   ├── variables.tf    # Schema config
│   │   └── outputs.tf      # Schema name, table status
│   │
│   └── audit/              # Audit trail module
│       ├── main.tf         # Audit table + summary view
│       ├── variables.tf    # Audit config
│       └── outputs.tf      # Table status, module status
│
└── .github/
    └── workflows/
        └── terraform.yml   # CI/CD: validate, plan, security scan
```

---

## 📋 Prerequisites

| Requirement | Version | Check Command |
| ----------- | ------- | ------------- |
| Terraform | >= 1.5.0 | `terraform version` |
| Docker | Any recent | `docker --version` |
| Docker Compose | v2+ | `docker compose version` |
| PostgreSQL Client | Any (optional) | `psql --version` |

---

## 🔧 Quick Start

> ⚠️ **Important:** You must set environment variables for credentials before running Terraform!

### Step 1: Set Environment Variables (Required)

```bash
# Required: Set credentials via environment variables
export TF_VAR_postgres_username="admin"
export TF_VAR_postgres_password="YourSecurePassword123!"
export TF_VAR_compliance_user_password="AnotherSecurePass456!"
```

> 🔒 **Security Note:** Never commit credentials to version control. Use a `.env` file or secrets manager for production.

### Step 2: Start PostgreSQL with Docker

```bash
cd compliance-as-code

# Start PostgreSQL container
docker compose up -d

# Verify it's running
docker compose ps
pg_isready -h localhost -p 5432
```

### Step 3: Initialize Terraform

```bash
# Initialize Terraform (downloads postgresql provider)
terraform init

# Validate configuration
terraform validate
```

### Step 4: Apply Infrastructure

```bash
# Review the plan
terraform plan

# Apply (creates role, schema, tables, and seeds data)
terraform apply -auto-approve
```

### Step 5: Verify Installation

```bash
# Connect as admin (uses the password from your env var)
PGPASSWORD="$TF_VAR_postgres_password" psql -h localhost -p 5432 -U "$TF_VAR_postgres_username" -d compliance_db
```

Once connected, run these SQL commands to verify:

```sql
-- List all tables in the compliance schema
\dt compliance.*

-- View seeded compliance rules
SELECT rule_name, severity FROM compliance.compliance_rules;

-- Exit psql
\q
```

✅ **Success!** You should see 7 compliance rules with severities like CRITICAL, HIGH, and MEDIUM.

---

## 📊 Query Examples

### Connect to the Database

```bash
# Using environment variable (recommended)
PGPASSWORD="$TF_VAR_postgres_password" psql -h localhost -p 5432 -U admin -d compliance_db

# Or use Terraform output
eval $(terraform output -raw psql_command_admin)
```

### View All Compliance Rules

```sql
-- List all rules with their severity
SELECT rule_id, rule_name, severity 
FROM compliance.compliance_rules 
ORDER BY severity, rule_name;
```

### Filter Rules by Severity

```sql
-- Show only CRITICAL and HIGH severity rules
SELECT rule_name, severity, sql_check
FROM compliance.compliance_rules
WHERE severity IN ('CRITICAL', 'HIGH')
ORDER BY severity;
```

### Insert an Audit Record

```sql
-- Record a compliance check result
INSERT INTO compliance.resource_audit 
    (resource_id, resource_type, compliance_status, rule_id, evidence, created_by)
VALUES 
    ('arn:aws:s3:::my-bucket', 
     'aws_s3_bucket', 
     'COMPLIANT', 
     1,  -- rule_id from compliance_rules
     '{"encryption": "AES256", "versioning": true}'::jsonb,
     'security-scanner');
```

### Query Audit Results

```sql
-- View all audit results with rule names
SELECT 
    a.resource_id,
    a.resource_type,
    a.compliance_status,
    r.rule_name,
    r.severity,
    a.checked_at
FROM compliance.resource_audit a
JOIN compliance.compliance_rules r ON a.rule_id = r.rule_id
ORDER BY a.checked_at DESC;
```

### Find Non-Compliant Resources

```sql
-- Show all resources that failed compliance checks
SELECT 
    resource_id,
    resource_type,
    compliance_status,
    evidence,
    checked_at
FROM compliance.resource_audit
WHERE compliance_status = 'NON_COMPLIANT'
ORDER BY checked_at DESC;
```

### Compliance Summary Report

```sql
-- Count resources by compliance status
SELECT 
    compliance_status,
    COUNT(*) as count
FROM compliance.resource_audit
GROUP BY compliance_status
ORDER BY count DESC;
```

### Export Results to CSV

```bash
# Run from terminal - exports audit results to CSV
PGPASSWORD="$TF_VAR_postgres_password" psql -h localhost -p 5432 -U admin -d compliance_db \
  -c "COPY (SELECT * FROM compliance.resource_audit) TO STDOUT WITH CSV HEADER" > audit_report.csv
```

---

## Input Variables

| Name | Description | Type | Default |
|------|-------------|------|---------|
| `postgres_host` | PostgreSQL server hostname | `string` | `"localhost"` |
| `postgres_port` | PostgreSQL server port | `number` | `5432` |
| `postgres_database` | PostgreSQL database name | `string` | `"compliance_db"` |
| `postgres_username` | Admin username | `string` | **Required** |
| `postgres_password` | Admin password | `string` | **Required** |
| `postgres_sslmode` | SSL mode (disable for Docker) | `string` | `"disable"` |
| `compliance_user` | Compliance role name | `string` | `"compliance_user"` |
| `compliance_user_password` | Compliance role password | `string` | **Required** |
| `schema_name` | Schema name | `string` | `"compliance"` |

## Outputs

| Name | Description | Sensitive |
|------|-------------|-----------|
| `postgres_host` | PostgreSQL hostname | no |
| `postgres_port` | PostgreSQL port | no |
| `postgres_database` | Database name | no |
| `connection_string_admin` | Admin connection string | **yes** |
| `connection_string_compliance_user` | Compliance user connection string | **yes** |
| `psql_command_admin` | psql CLI command (admin) | **yes** |
| `psql_command_compliance_user` | psql CLI command (compliance_user) | **yes** |
| `compliance_user_name` | Compliance role name | no |
| `schema_name` | Schema name | no |

### Accessing Sensitive Outputs

```bash
# View all outputs (sensitive values hidden)
terraform output

# View specific sensitive output
terraform output -raw connection_string_admin

# Connect using output
eval $(terraform output -raw psql_command_admin)
```

## Database Schema

### 🛡️ immune_rules Table (Antibody Library)

Stores immune response patterns - antibodies that recognize specific threat signatures:

| Column | Type | Description |
|--------|------|-------------|
| `rule_id` | SERIAL | Primary key (auto-increment) |
| `antibody_name` | TEXT | Name of the immune response pattern |
| `threat_signature` | TEXT | Pattern that triggers immune response |
| `response_action` | TEXT | Action taken when threat detected |
| `severity_level` | TEXT | CRITICAL, HIGH, MEDIUM, LOW |
| `active` | BOOLEAN | Whether antibody is active |
| `created_at` | TIMESTAMPTZ | Creation timestamp |

### 🧠 compliance_rules Table (Adaptive Memory)

Stores compliance rule definitions:

| Column | Type | Description |
|--------|------|-------------|
| `rule_id` | SERIAL | Primary key (auto-increment) |
| `rule_name` | TEXT | Human-readable rule name |
| `severity` | TEXT | CRITICAL, HIGH, MEDIUM, LOW |
| `sql_check` | TEXT | SQL query for compliance verification |
| `created_at` | TIMESTAMPTZ | Creation timestamp |
| `updated_at` | TIMESTAMPTZ | Last update timestamp |

### 🔬 resource_audit Table (Sensory Memory)

Logs compliance check results:

| Column | Type | Description |
|--------|------|-------------|
| `audit_id` | SERIAL | Primary key (auto-increment) |
| `resource_id` | TEXT | Cloud resource identifier |
| `resource_type` | TEXT | Resource type |
| `compliance_status` | TEXT | COMPLIANT, NON_COMPLIANT, ERROR, PENDING |
| `checked_at` | TIMESTAMPTZ | Check timestamp (default now()) |
| `rule_id` | INTEGER | FK to compliance_rules |
| `evidence` | JSONB | Check evidence data |
| `created_by` | TEXT | Who ran the check |

## Seeded Immune Rules (Antibodies)

The module seeds **8 baseline immune rules** (antibodies) for threat detection:

| Antibody Name | Threat Signature | Response | Severity |
| ------------- | ---------------- | -------- | -------- |
| SQL Injection Antibody | `SELECT.*FROM.*WHERE.*=.*OR.*1=1` | block | **CRITICAL** |
| XSS Attack Antibody | `<script>.*</script>` | sanitize | HIGH |
| Path Traversal Antibody | `\\.\\./\\.\\./\\.\\./etc/` | block | **CRITICAL** |
| Brute Force Antibody | `failed_login_count > 10` | lockout | HIGH |
| Data Exfiltration Antibody | `bulk_download > 1000_records` | alert | MEDIUM |
| Privilege Escalation Antibody | `role_change_attempt` | audit | HIGH |
| Malware Signature Antibody | `base64_encoded_executable` | quarantine | **CRITICAL** |
| DDoS Pattern Antibody | `requests_per_second > 1000` | throttle | HIGH |

## Seeded Compliance Rules (Adaptive Memory)

The module seeds **10 baseline compliance rules** covering:

| Rule | Severity | Category |
| ---- | -------- | -------- |
| CloudTrail must be enabled in all regions | HIGH | logging |
| All S3 buckets must be encrypted | HIGH | encryption |
| Security groups must not allow unrestricted SSH | **CRITICAL** | network |
| RDS instances must have encryption enabled | HIGH | encryption |
| EC2 instances must use encrypted EBS volumes | MEDIUM | encryption |
| IAM policies must follow least privilege | **CRITICAL** | iam |
| VPC flow logs must be enabled | MEDIUM | logging |
| Public S3 buckets must be blocked | **CRITICAL** | storage |
| Multi-factor authentication must be enabled for root | **CRITICAL** | iam |
| Secrets must not be hardcoded in code | **CRITICAL** | secrets |

---

## ✅ Verification Commands

After running `terraform apply`, verify the setup:

```bash
# Check validation outputs
terraform output setup_validated
terraform output tables_exist
terraform output schema_exists

# Quick verification (shows tables and rule count)
PGPASSWORD="$TF_VAR_postgres_password" psql -h localhost -p 5432 -U admin -d compliance_db -c "
  SELECT 'Schema: ' || schema_name FROM information_schema.schemata WHERE schema_name = 'compliance';
  SELECT 'Tables: ' || string_agg(table_name, ', ') FROM information_schema.tables WHERE table_schema = 'compliance';
  SELECT 'Rules: ' || COUNT(*)::text FROM compliance.compliance_rules;
"
```

**Expected output:**
```text
✅ Schema 'compliance' exists
✅ compliance_rules created  
✅ resource_audit created
✅ 10 compliance rules seeded
```

---

## Docker Commands

```bash
# Start PostgreSQL
docker compose up -d

# Stop PostgreSQL
docker compose down

# View logs
docker compose logs -f postgres

# Remove data volume (clean slate)
docker compose down -v
```

---

## 🔒 Security Best Practices

| Practice | Implementation |
| -------- | -------------- |
| No hardcoded passwords | All credentials via `TF_VAR_*` environment variables |
| Sensitive outputs | Connection strings marked `sensitive = true` |
| .gitignore | Covers `.env`, `*.tfvars`, `*.tfstate`, keys, credentials |
| Least privilege | `compliance_user` has only schema-level permissions |
| CI/CD secrets | GitHub Actions uses repository secrets |

### Setting Up GitHub Secrets (for CI/CD)

```bash
# In your GitHub repository settings → Secrets → Actions
# Add these secrets:
TF_VAR_POSTGRES_PASSWORD=YourSecurePassword123!
TF_VAR_COMPLIANCE_USER_PASSWORD=AnotherSecurePass456!
```

---

## 🔄 CI/CD Pipeline

The included GitHub Actions workflow (`.github/workflows/terraform.yml`) provides:

| Job | Trigger | Description |
| --- | ------- | ----------- |
| **validate** | PR, Push | Format check, `terraform validate`, module validation |
| **sql-validation** | PR, Push | Tests SQL syntax against real PostgreSQL |
| **security** | PR, Push | Runs `tfsec` and `checkov` security scans |
| **plan** | PR only | Creates and posts Terraform plan to PR |
| **docs** | Push to main | Auto-generates Terraform documentation |

### Manual Workflow Trigger

```bash
# Trigger workflow manually via GitHub CLI
gh workflow run terraform.yml
```

---

## Cleanup

```bash
# Destroy Terraform resources (keeps Docker running)
terraform destroy -auto-approve

# Stop and remove Docker container
docker compose down

# Remove data volume too
docker compose down -v
```

## Troubleshooting

### Connection Refused

PostgreSQL container isn't running:

```bash
docker compose up -d
pg_isready -h localhost -p 5432
```

### Authentication Failed

Wrong credentials. Check `docker-compose.yml` matches your environment variables:

```bash
# Required credentials (set via environment variables)
# Username: Set TF_VAR_postgres_username
# Password: Set TF_VAR_postgres_password (min 8 chars)
# Database: compliance_db
```

### Permission Denied on USB Drive (FAT32)

Terraform provider binaries can't execute on FAT32. Copy project to native filesystem:

```bash
cp -r /media/*/compliance-as-code ~/compliance-as-code
cd ~/compliance-as-code
rm -rf .terraform .terraform.lock.hcl
terraform init && terraform apply
```

## License

MIT License - See LICENSE file for details.
