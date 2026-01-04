#--------------------------------------------------------------
# Compliance Module - compliance-as-code
# digital organism metaphor
# 
# Purpose: Creates the compliance schema and tables for the immune system
# This module represents the "immune system" of the digital organism -
# containing the antibodies (immune_rules) and adaptive responses
# (compliance_rules) that protect against threats.
#
# Managed by: Terraform | Owner: DevSecOps
#--------------------------------------------------------------

terraform {
  required_providers {
    postgresql = {
      source  = "cyrilgdn/postgresql"
      version = "~> 1.21"
    }
  }
}

#--------------------------------------------------------------
# Compliance Schema
# digital organism metaphor: Immune system organ boundary
# Isolated namespace for all compliance-related objects
#--------------------------------------------------------------
resource "postgresql_schema" "compliance" {
  name     = var.schema_name
  database = var.database_name
  owner    = var.compliance_user

  # Grant usage to compliance user
  policy {
    usage = true
    role  = var.compliance_user
  }

  lifecycle {
    prevent_destroy = false
  }
}

#--------------------------------------------------------------
# Schema Permissions Grant
# digital organism metaphor: Immune cell access credentials
# Ensures compliance_user has full access to the schema
#--------------------------------------------------------------
resource "postgresql_grant" "compliance_user_schema" {
  database    = var.database_name
  role        = var.compliance_user
  schema      = postgresql_schema.compliance.name
  object_type = "schema"
  privileges  = ["USAGE", "CREATE"]
}

#--------------------------------------------------------------
# Immune Rules Table
# digital organism metaphor: Antibody library
# 
# Stores immune response patterns - antibodies that recognize
# specific threat signatures and trigger protective responses.
# compliance-as-code: Biological defense patterns
#--------------------------------------------------------------
resource "null_resource" "immune_rules_table" {
  triggers = {
    schema   = postgresql_schema.compliance.name
    database = var.database_name
    host     = var.host
    port     = var.port
    username = var.admin_username
    password = var.admin_password
    # Table definition hash for change detection
    table_hash = sha256("immune_rules_v1")
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "
          -- compliance-as-code: Create immune_rules table
          -- digital organism metaphor: Antibody storage
          CREATE TABLE IF NOT EXISTS ${var.schema_name}.immune_rules (
            rule_id           SERIAL PRIMARY KEY,
            antibody_name     TEXT NOT NULL UNIQUE,
            threat_signature  TEXT NOT NULL,
            response_action   TEXT DEFAULT 'alert',
            severity_level    TEXT DEFAULT 'MEDIUM' CHECK (severity_level IN ('CRITICAL', 'HIGH', 'MEDIUM', 'LOW')),
            active            BOOLEAN DEFAULT true,
            created_at        TIMESTAMPTZ DEFAULT NOW(),
            updated_at        TIMESTAMPTZ DEFAULT NOW()
          );

          -- Add table comment for documentation
          COMMENT ON TABLE ${var.schema_name}.immune_rules IS 
            'Immune system antibodies | digital organism metaphor | compliance-as-code | managed_by=terraform';
          
          -- Add column comments with biological metaphors
          COMMENT ON COLUMN ${var.schema_name}.immune_rules.rule_id IS 'Unique antibody identifier';
          COMMENT ON COLUMN ${var.schema_name}.immune_rules.antibody_name IS 'Name of the immune response pattern';
          COMMENT ON COLUMN ${var.schema_name}.immune_rules.threat_signature IS 'Pattern that triggers immune response';
          COMMENT ON COLUMN ${var.schema_name}.immune_rules.response_action IS 'Action taken when threat detected';

          -- Create index for threat detection
          CREATE INDEX IF NOT EXISTS idx_immune_rules_threat 
            ON ${var.schema_name}.immune_rules(threat_signature);
          CREATE INDEX IF NOT EXISTS idx_immune_rules_active 
            ON ${var.schema_name}.immune_rules(active) WHERE active = true;

          -- Grant permissions to compliance user
          GRANT ALL ON ${var.schema_name}.immune_rules TO ${var.compliance_user};
          GRANT USAGE, SELECT ON SEQUENCE ${var.schema_name}.immune_rules_rule_id_seq TO ${var.compliance_user};
        "
    EOT
  }

  # Destroy provisioner to clean up
  provisioner "local-exec" {
    when    = destroy
    command = <<-EOT
      PGPASSWORD='${self.triggers.password}' psql \
        -h ${self.triggers.host} \
        -p ${self.triggers.port} \
        -U ${self.triggers.username} \
        -d ${self.triggers.database} \
        -c "DROP TABLE IF EXISTS ${self.triggers.schema}.immune_rules CASCADE;" \
        2>/dev/null || true
    EOT
  }

  depends_on = [postgresql_grant.compliance_user_schema]
}

#--------------------------------------------------------------
# Compliance Rules Table
# digital organism metaphor: Adaptive immune memory
# Stores compliance rule definitions with severity levels
# compliance-as-code: Core rule storage
#--------------------------------------------------------------
resource "null_resource" "compliance_rules_table" {
  triggers = {
    schema   = postgresql_schema.compliance.name
    database = var.database_name
    host     = var.host
    port     = var.port
    username = var.admin_username
    password = var.admin_password
    # Table definition hash for change detection
    table_hash = sha256("compliance_rules_v2")
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "
          -- Create compliance_rules table
          CREATE TABLE IF NOT EXISTS ${var.schema_name}.compliance_rules (
            rule_id      SERIAL PRIMARY KEY,
            rule_name    TEXT NOT NULL UNIQUE,
            severity     TEXT NOT NULL CHECK (severity IN ('CRITICAL', 'HIGH', 'MEDIUM', 'LOW')),
            sql_check    TEXT,
            description  TEXT,
            category     TEXT DEFAULT 'general',
            enabled      BOOLEAN DEFAULT true,
            created_at   TIMESTAMPTZ DEFAULT NOW(),
            updated_at   TIMESTAMPTZ DEFAULT NOW()
          );

          -- Add table comment for documentation
          COMMENT ON TABLE ${var.schema_name}.compliance_rules IS 
            'Compliance rule definitions | compliance-as-code | managed_by=terraform';
          
          -- Add column comments
          COMMENT ON COLUMN ${var.schema_name}.compliance_rules.rule_id IS 'Unique rule identifier';
          COMMENT ON COLUMN ${var.schema_name}.compliance_rules.severity IS 'Rule severity: CRITICAL, HIGH, MEDIUM, LOW';
          COMMENT ON COLUMN ${var.schema_name}.compliance_rules.sql_check IS 'SQL query to verify compliance';

          -- Create index for common queries
          CREATE INDEX IF NOT EXISTS idx_compliance_rules_severity 
            ON ${var.schema_name}.compliance_rules(severity);
          CREATE INDEX IF NOT EXISTS idx_compliance_rules_enabled 
            ON ${var.schema_name}.compliance_rules(enabled) WHERE enabled = true;

          -- Grant permissions to compliance user
          GRANT ALL ON ${var.schema_name}.compliance_rules TO ${var.compliance_user};
          GRANT USAGE, SELECT ON SEQUENCE ${var.schema_name}.compliance_rules_rule_id_seq TO ${var.compliance_user};
        "
    EOT
  }

  # Destroy provisioner to clean up
  provisioner "local-exec" {
    when    = destroy
    command = <<-EOT
      PGPASSWORD='${self.triggers.password}' psql \
        -h ${self.triggers.host} \
        -p ${self.triggers.port} \
        -U ${self.triggers.username} \
        -d ${self.triggers.database} \
        -c "DROP TABLE IF EXISTS ${self.triggers.schema}.compliance_rules CASCADE;" \
        2>/dev/null || true
    EOT
  }

  depends_on = [null_resource.immune_rules_table]
}

#--------------------------------------------------------------
# Seed Immune Rules
# digital organism metaphor: Pre-programmed antibody library
# Baseline immune patterns for threat detection
# compliance-as-code: Initial antibody set
#--------------------------------------------------------------
resource "null_resource" "seed_immune_rules" {
  triggers = {
    table_created = null_resource.immune_rules_table.id
    seed_version  = "v1.0"
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "
          -- compliance-as-code: Seed immune rules (antibodies)
          -- digital organism metaphor: Pre-programmed immune responses
          INSERT INTO ${var.schema_name}.immune_rules 
            (antibody_name, threat_signature, response_action, severity_level)
          VALUES
            ('SQL Injection Antibody', 'SELECT.*FROM.*WHERE.*=.*OR.*1=1', 'block', 'CRITICAL'),
            ('XSS Attack Antibody', '<script>.*</script>', 'sanitize', 'HIGH'),
            ('Path Traversal Antibody', '\\.\\./\\.\\./\\.\\./etc/', 'block', 'CRITICAL'),
            ('Brute Force Antibody', 'failed_login_count > 10', 'lockout', 'HIGH'),
            ('Data Exfiltration Antibody', 'bulk_download > 1000_records', 'alert', 'MEDIUM'),
            ('Privilege Escalation Antibody', 'role_change_attempt', 'audit', 'HIGH'),
            ('Malware Signature Antibody', 'base64_encoded_executable', 'quarantine', 'CRITICAL'),
            ('DDoS Pattern Antibody', 'requests_per_second > 1000', 'throttle', 'HIGH')
          ON CONFLICT (antibody_name) DO UPDATE SET
            threat_signature = EXCLUDED.threat_signature,
            response_action = EXCLUDED.response_action,
            severity_level = EXCLUDED.severity_level,
            updated_at = NOW();
        "
    EOT
  }

  depends_on = [null_resource.immune_rules_table]
}

#--------------------------------------------------------------
# Seed Compliance Rules
# digital organism metaphor: Adaptive immune memory cells
# Baseline security rules for infrastructure compliance
# compliance-as-code: Initial rule set
#--------------------------------------------------------------
resource "null_resource" "seed_compliance_rules" {
  triggers = {
    table_created = null_resource.compliance_rules_table.id
    immune_seeded = null_resource.seed_immune_rules.id
    seed_version  = "v2.0"
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "
          -- Seed baseline compliance rules
          INSERT INTO ${var.schema_name}.compliance_rules 
            (rule_name, severity, sql_check, description, category)
          VALUES
            ('CloudTrail must be enabled in all regions', 'HIGH', 
             'SELECT * FROM aws_cloudtrail WHERE is_multi_region = false', 
             'Ensures CloudTrail logging covers all AWS regions',
             'logging'),
            ('All S3 buckets must be encrypted', 'HIGH', 
             'SELECT * FROM aws_s3_bucket WHERE encryption_status = ''disabled''', 
             'Ensures all S3 buckets have encryption enabled',
             'encryption'),
            ('Security groups must not allow unrestricted SSH', 'CRITICAL', 
             'SELECT * FROM aws_security_group_rule WHERE port = 22 AND cidr = ''0.0.0.0/0''', 
             'Prevents unrestricted SSH access from the internet',
             'network'),
            ('RDS instances must have encryption enabled', 'HIGH', 
             'SELECT * FROM aws_rds_instance WHERE storage_encrypted = false', 
             'Ensures RDS database storage is encrypted',
             'encryption'),
            ('EC2 instances must use encrypted EBS volumes', 'MEDIUM', 
             'SELECT * FROM aws_ebs_volume WHERE encrypted = false', 
             'Ensures EBS volumes are encrypted at rest',
             'encryption'),
            ('IAM policies must follow least privilege', 'CRITICAL', 
             'SELECT * FROM aws_iam_policy WHERE policy_document LIKE ''%\"Action\": \"*\"%''', 
             'Prevents overly permissive IAM policies',
             'iam'),
            ('VPC flow logs must be enabled', 'MEDIUM', 
             'SELECT * FROM aws_vpc WHERE flow_logs_enabled = false', 
             'Ensures network traffic is logged for security analysis',
             'logging'),
            ('Public S3 buckets must be blocked', 'CRITICAL',
             'SELECT * FROM aws_s3_bucket WHERE public_access_block = false',
             'Prevents accidental public exposure of S3 data',
             'storage'),
            ('Multi-factor authentication must be enabled for root', 'CRITICAL',
             'SELECT * FROM aws_iam_account WHERE root_mfa_enabled = false',
             'Ensures root account has MFA protection',
             'iam'),
            ('Secrets must not be hardcoded in code', 'CRITICAL',
             'SELECT * FROM code_scan WHERE secret_detected = true',
             'Prevents credentials from being committed to repositories',
             'secrets')
          ON CONFLICT (rule_name) DO UPDATE SET
            severity = EXCLUDED.severity,
            sql_check = EXCLUDED.sql_check,
            description = EXCLUDED.description,
            category = EXCLUDED.category,
            updated_at = NOW();
        "
    EOT
  }

  depends_on = [null_resource.compliance_rules_table]
}

#--------------------------------------------------------------
# Validate Table Creation
# digital organism metaphor: Immune system health check
# Outputs the table structure for verification
#--------------------------------------------------------------
resource "null_resource" "validate_compliance_table" {
  triggers = {
    table_id  = null_resource.compliance_rules_table.id
    immune_id = null_resource.immune_rules_table.id
    seed_id   = null_resource.seed_compliance_rules.id
  }

  provisioner "local-exec" {
    command = <<-EOT
      echo "=== Validating immune_rules table (Antibody Library) ==="
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "SELECT antibody_name, threat_signature, severity_level FROM ${var.schema_name}.immune_rules ORDER BY severity_level, antibody_name;"
      
      echo ""
      echo "=== Validating compliance_rules table (Adaptive Memory) ==="
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "SELECT rule_name, severity, category FROM ${var.schema_name}.compliance_rules ORDER BY severity, rule_name;"
    EOT
  }

  depends_on = [null_resource.seed_compliance_rules]
}
