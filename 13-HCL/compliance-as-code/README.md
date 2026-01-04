# PostgreSQL Compliance Auditing Database - Terraform Module

This Terraform module provisions a local PostgreSQL database (via Docker) configured for infrastructure compliance auditing.

## Disclaimer

This module is intended for local development and testing purposes only. It is not recommended for production use without proper security hardening and configuration.

the program has not been fully tested, and therefore, there are rough edges, and it may not cover all security best practices.

Credentials need to be setup via environment variables before running Terraform:

```bash
export TF_VAR_postgres_username="<YOUR_ADMIN_USERNAME>"
export TF_VAR_postgres_password="<YOUR_ADMIN_PASSWORD>"
export TF_VAR_compliance_user_password="<YOUR_COMPLIANCE_PASSWORD>"
```

## Features

- **Local PostgreSQL** running in Docker for development/testing
- **Automated schema creation** using the `cyrilgdn/postgresql` Terraform provider
- **Compliance user and schema** with proper access control
- **Baseline compliance rules** seeded automatically
- **Metadata annotations** via PostgreSQL COMMENT statements

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Local Docker Environment                      │
│                                                                  │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │            PostgreSQL Container (postgres:16-alpine)       │  │
│  │            Port: localhost:5432                            │  │
│  │                                                            │  │
│  │  ┌─────────────────────────────────────────────────────┐  │  │
│  │  │              compliance_db                           │  │  │
│  │  │                                                      │  │  │
│  │  │  ┌─────────────────┐  ┌─────────────────────────┐   │  │  │
│  │  │  │ compliance_rules│  │    resource_audit       │   │  │  │
│  │  │  │                 │  │                         │   │  │  │
│  │  │  │ - rule_id       │  │ - audit_id              │   │  │  │
│  │  │  │ - rule_name     │  │ - resource_id           │   │  │  │
│  │  │  │ - severity      │  │ - resource_type         │   │  │  │
│  │  │  │ - sql_check     │  │ - compliance_status     │   │  │  │
│  │  │  └─────────────────┘  │ - checked_at            │   │  │  │
│  │  │                       └─────────────────────────┘   │  │  │
│  │  └─────────────────────────────────────────────────────┘  │  │
│  │                                                            │  │
│  │  Owner: compliance_user | Schema: compliance               │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                  │
│  Metadata: compliance=true, owner=devsecops, managed_by=terraform│
└─────────────────────────────────────────────────────────────────┘
```

## Prerequisites

- Terraform >= 1.5.0
- Docker installed and running
- PostgreSQL client (`psql`) installed locally

## File Structure

```
compliance-as-code/
├── main.tf              # Main Terraform configuration (postgresql provider)
├── variables.tf         # Input variables with validation
├── outputs.tf           # Output definitions (connection strings, etc.)
├── docker-compose.yml   # Docker configuration for PostgreSQL
└── README.md            # This file
```

## Quick Start

### Step 1: Start PostgreSQL with Docker

```bash
cd compliance-as-code

# Start PostgreSQL container
docker compose up -d

# Verify it's running
docker compose ps
pg_isready -h localhost -p 5432
```

### Step 2: Initialize Terraform

```bash
# Initialize Terraform (downloads postgresql provider)
terraform init

# Validate configuration
terraform validate
```

### Step 3: Apply Infrastructure

```bash
# Review the plan
terraform plan

# Apply (creates role, schema, tables, and seeds data)
terraform apply -auto-approve
```

### Step 4: Verify

```bash
# Connect as admin (use your password from TF_VAR_postgres_password)
PGPASSWORD='<YOUR_ADMIN_PASSWORD>' psql -h localhost -p 5432 -U admin -d compliance_db

# List tables
\dt compliance.*

# View seeded rules
SELECT rule_name, severity FROM compliance.compliance_rules;
```

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

### compliance_rules Table

Stores compliance rule definitions:

| Column | Type | Description |
|--------|------|-------------|
| `rule_id` | SERIAL | Primary key (auto-increment) |
| `rule_name` | TEXT | Human-readable rule name |
| `severity` | TEXT | CRITICAL, HIGH, MEDIUM, LOW |
| `sql_check` | TEXT | SQL query for compliance verification |
| `created_at` | TIMESTAMPTZ | Creation timestamp |
| `updated_at` | TIMESTAMPTZ | Last update timestamp |

### resource_audit Table

Logs compliance check results:

| Column | Type | Description |
|--------|------|-------------|
| `audit_id` | SERIAL | Primary key (auto-increment) |
| `resource_id` | TEXT | Cloud resource identifier |
| `resource_type` | TEXT | Resource type |
| `compliance_status` | TEXT | COMPLIANT, NON_COMPLIANT, ERROR, PENDING |
| `checked_at` | TIMESTAMPTZ | Check timestamp |
| `rule_id` | INTEGER | FK to compliance_rules |
| `evidence` | JSONB | Check evidence data |
| `created_by` | TEXT | Who ran the check |

## Seeded Compliance Rules

The module seeds baseline compliance rules covering:

- **S3 Bucket Security**: Encryption, public access blocking
- **RDS Database Security**: Encryption at rest
- **EC2 Instance Security**: EBS encryption
- **Security Groups**: Unrestricted SSH access
- **IAM Security**: Least privilege policies
- **CloudTrail**: Multi-region logging
- **VPC**: Flow logs enabled

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

Wrong credentials. Check `docker-compose.yml` matches `variables.tf`:

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
