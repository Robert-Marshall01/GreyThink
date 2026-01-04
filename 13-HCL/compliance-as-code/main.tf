#--------------------------------------------------------------
# Terraform Configuration for PostgreSQL Compliance Auditing Database
# 
# This module manages a local Dockerized PostgreSQL instance:
# - Creates a compliance_user role with login privileges
# - Creates a compliance schema
# - Creates compliance_rules and resource_audit tables
# - Adds metadata comments for compliance context
#
# Prerequisites:
#   1. Docker running with PostgreSQL container
#   2. Run: docker-compose up -d
#--------------------------------------------------------------

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    postgresql = {
      source  = "cyrilgdn/postgresql"
      version = "~> 1.22"
    }
    null = {
      source  = "hashicorp/null"
      version = "~> 3.2"
    }
  }
}

#--------------------------------------------------------------
# PostgreSQL Provider Configuration
# 
# Connects to local Dockerized PostgreSQL instance
#--------------------------------------------------------------

provider "postgresql" {
  host            = var.postgres_host
  port            = var.postgres_port
  database        = var.postgres_database
  username        = var.postgres_username
  password        = var.postgres_password
  sslmode         = var.postgres_sslmode
  connect_timeout = 15
  superuser       = false
}

#--------------------------------------------------------------
# Local Variables
#--------------------------------------------------------------

locals {
  # Format metadata as a JSON string for PostgreSQL COMMENT
  metadata_json = jsonencode(var.metadata)
  
  # Timestamp for tracking
  created_at = timestamp()
}

#--------------------------------------------------------------
# PostgreSQL Role: compliance_user
# 
# Application role with login privileges for compliance operations.
# This role will own the compliance schema and its objects.
#--------------------------------------------------------------

resource "postgresql_role" "compliance_user" {
  name     = var.compliance_user
  login    = true
  password = var.compliance_user_password
  
  # Role attributes
  create_database = false
  create_role     = false
  inherit         = true
  superuser       = false
  replication     = false
  
  # Connection limit (-1 = unlimited)
  connection_limit = -1
}

#--------------------------------------------------------------
# PostgreSQL Schema: compliance
# 
# Dedicated schema for all compliance-related objects.
# Owned by compliance_user for proper access control.
#--------------------------------------------------------------

resource "postgresql_schema" "compliance" {
  name     = var.schema_name
  owner    = postgresql_role.compliance_user.name
  database = var.postgres_database

  # Grant usage to compliance_user
  policy {
    usage = true
    role  = postgresql_role.compliance_user.name
  }

  depends_on = [postgresql_role.compliance_user]
}

#--------------------------------------------------------------
# Grant Schema Privileges
# 
# Ensure compliance_user has full access to the schema
#--------------------------------------------------------------

resource "postgresql_grant" "compliance_user_schema" {
  database    = var.postgres_database
  role        = postgresql_role.compliance_user.name
  schema      = postgresql_schema.compliance.name
  object_type = "schema"
  privileges  = ["CREATE", "USAGE"]

  depends_on = [postgresql_schema.compliance]
}

#--------------------------------------------------------------
# Table: compliance_rules
# 
# Stores compliance rule definitions that are checked against
# infrastructure resources.
#
# Columns:
#   - rule_id: Auto-incrementing primary key
#   - rule_name: Human-readable name of the rule
#   - severity: Risk level (CRITICAL, HIGH, MEDIUM, LOW)
#   - sql_check: SQL query for compliance verification
#--------------------------------------------------------------

resource "postgresql_extension" "uuid_ossp" {
  name     = "uuid-ossp"
  database = var.postgres_database
}

# Use null_resource with local-exec to create tables
# The postgresql provider doesn't have native table management
resource "null_resource" "compliance_rules_table" {
  triggers = {
    schema   = var.schema_name
    host     = var.postgres_host
    port     = var.postgres_port
    username = var.postgres_username
    password = var.postgres_password
    database = var.postgres_database
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.postgres_password}' psql \
        -h ${var.postgres_host} \
        -p ${var.postgres_port} \
        -U ${var.postgres_username} \
        -d ${var.postgres_database} \
        -c "
        -- Create compliance_rules table
        CREATE TABLE IF NOT EXISTS ${var.schema_name}.compliance_rules (
            rule_id      SERIAL PRIMARY KEY,
            rule_name    TEXT NOT NULL,
            severity     TEXT NOT NULL CHECK (severity IN ('CRITICAL', 'HIGH', 'MEDIUM', 'LOW')),
            sql_check    TEXT NOT NULL,
            created_at   TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
            updated_at   TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
        );

        -- Add metadata comment for compliance context
        COMMENT ON TABLE ${var.schema_name}.compliance_rules IS '${local.metadata_json}';

        -- Create index for faster lookups
        CREATE INDEX IF NOT EXISTS idx_compliance_rules_severity 
            ON ${var.schema_name}.compliance_rules(severity);

        -- Grant privileges to compliance_user
        GRANT ALL PRIVILEGES ON TABLE ${var.schema_name}.compliance_rules TO ${var.compliance_user};
        GRANT USAGE, SELECT ON SEQUENCE ${var.schema_name}.compliance_rules_rule_id_seq TO ${var.compliance_user};
        "
    EOT
  }

  provisioner "local-exec" {
    when    = destroy
    command = <<-EOT
      PGPASSWORD='${self.triggers.password}' psql \
        -h ${self.triggers.host} \
        -p ${self.triggers.port} \
        -U ${self.triggers.username} \
        -d ${self.triggers.database} \
        -c "DROP TABLE IF EXISTS ${self.triggers.schema}.compliance_rules CASCADE;" || true
    EOT
  }

  depends_on = [
    postgresql_schema.compliance,
    postgresql_grant.compliance_user_schema
  ]
}

#--------------------------------------------------------------
# Table: resource_audit
# 
# Logs the results of compliance checks performed against
# infrastructure resources.
#--------------------------------------------------------------

resource "null_resource" "resource_audit_table" {
  triggers = {
    schema   = var.schema_name
    host     = var.postgres_host
    port     = var.postgres_port
    username = var.postgres_username
    password = var.postgres_password
    database = var.postgres_database
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.postgres_password}' psql \
        -h ${var.postgres_host} \
        -p ${var.postgres_port} \
        -U ${var.postgres_username} \
        -d ${var.postgres_database} \
        -c "
        -- Create resource_audit table
        CREATE TABLE IF NOT EXISTS ${var.schema_name}.resource_audit (
            audit_id          SERIAL PRIMARY KEY,
            resource_id       TEXT NOT NULL,
            resource_type     TEXT NOT NULL,
            compliance_status TEXT NOT NULL CHECK (
                compliance_status IN ('COMPLIANT', 'NON_COMPLIANT', 'NOT_APPLICABLE', 'ERROR', 'PENDING')
            ),
            checked_at        TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
            rule_id           INTEGER REFERENCES ${var.schema_name}.compliance_rules(rule_id),
            evidence          JSONB DEFAULT '{}'::jsonb,
            created_by        TEXT DEFAULT 'system'
        );

        -- Add metadata comment for compliance context
        COMMENT ON TABLE ${var.schema_name}.resource_audit IS '${local.metadata_json}';

        -- Create indexes for common queries
        CREATE INDEX IF NOT EXISTS idx_resource_audit_resource_id 
            ON ${var.schema_name}.resource_audit(resource_id);
        CREATE INDEX IF NOT EXISTS idx_resource_audit_status 
            ON ${var.schema_name}.resource_audit(compliance_status);
        CREATE INDEX IF NOT EXISTS idx_resource_audit_checked_at 
            ON ${var.schema_name}.resource_audit(checked_at DESC);

        -- Grant privileges to compliance_user
        GRANT ALL PRIVILEGES ON TABLE ${var.schema_name}.resource_audit TO ${var.compliance_user};
        GRANT USAGE, SELECT ON SEQUENCE ${var.schema_name}.resource_audit_audit_id_seq TO ${var.compliance_user};
        "
    EOT
  }

  provisioner "local-exec" {
    when    = destroy
    command = <<-EOT
      PGPASSWORD='${self.triggers.password}' psql \
        -h ${self.triggers.host} \
        -p ${self.triggers.port} \
        -U ${self.triggers.username} \
        -d ${self.triggers.database} \
        -c "DROP TABLE IF EXISTS ${self.triggers.schema}.resource_audit CASCADE;" || true
    EOT
  }

  depends_on = [null_resource.compliance_rules_table]
}

#--------------------------------------------------------------
# Seed Data: Baseline Compliance Rules
#--------------------------------------------------------------

resource "null_resource" "seed_compliance_rules" {
  triggers = {
    schema   = var.schema_name
    host     = var.postgres_host
    port     = var.postgres_port
    username = var.postgres_username
    password = var.postgres_password
    database = var.postgres_database
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.postgres_password}' psql \
        -h ${var.postgres_host} \
        -p ${var.postgres_port} \
        -U ${var.postgres_username} \
        -d ${var.postgres_database} \
        -c "
        -- Seed baseline compliance rules
        INSERT INTO ${var.schema_name}.compliance_rules (rule_name, severity, sql_check)
        SELECT * FROM (VALUES 
            ('All S3 buckets must be encrypted', 'HIGH', 
             'SELECT bucket_name FROM aws_s3_bucket WHERE encryption IS NULL'),
            
            ('S3 buckets must block public access', 'CRITICAL', 
             'SELECT bucket_name FROM aws_s3_bucket WHERE block_public_acls = false'),
            
            ('RDS instances must be encrypted at rest', 'CRITICAL', 
             'SELECT db_instance_identifier FROM aws_rds_db_instance WHERE storage_encrypted = false'),
            
            ('EC2 instances must use encrypted EBS volumes', 'HIGH', 
             'SELECT instance_id FROM aws_ec2_instance WHERE encrypted = false'),
            
            ('Security groups must not allow unrestricted SSH', 'CRITICAL', 
             'SELECT group_id FROM aws_security_group_rule WHERE from_port = 22 AND cidr = ''0.0.0.0/0'''),
            
            ('IAM policies must follow least privilege', 'HIGH', 
             'SELECT policy_name FROM aws_iam_policy WHERE policy_document LIKE ''%Action: *%'''),
            
            ('CloudTrail must be enabled in all regions', 'HIGH', 
             'SELECT trail_name FROM aws_cloudtrail WHERE is_multi_region_trail = false'),
            
            ('VPC flow logs must be enabled', 'MEDIUM', 
             'SELECT vpc_id FROM aws_vpc WHERE flow_logs_enabled = false')
        ) AS v(rule_name, severity, sql_check)
        WHERE NOT EXISTS (
            SELECT 1 FROM ${var.schema_name}.compliance_rules cr 
            WHERE cr.rule_name = v.rule_name
        );
        "
    EOT
  }

  depends_on = [null_resource.compliance_rules_table]
}

#--------------------------------------------------------------
# Database Comment: Metadata Annotation
#--------------------------------------------------------------

resource "null_resource" "database_metadata" {
  triggers = {
    schema = var.schema_name
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.postgres_password}' psql \
        -h ${var.postgres_host} \
        -p ${var.postgres_port} \
        -U ${var.postgres_username} \
        -d ${var.postgres_database} \
        -c "COMMENT ON SCHEMA ${var.schema_name} IS 'Schema for compliance rules and resource auditing. Metadata: ${local.metadata_json}';"
    EOT
  }

  depends_on = [postgresql_schema.compliance]
}
