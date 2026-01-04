#--------------------------------------------------------------
# Audit Module - compliance-as-code
# digital organism metaphor
# 
# Purpose: Creates the resource_audit table for tracking compliance checks
# This module represents the "nervous system" of the digital organism -
# the sensory organs that detect changes in the environment and record
# the organism's responses to threats and stimuli.
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
# Resource Audit Table
# digital organism metaphor: Sensory memory and response log
# Logs all compliance check results with evidence
# compliance-as-code: Audit trail storage
#--------------------------------------------------------------
resource "null_resource" "resource_audit_table" {
  triggers = {
    schema   = var.schema_name
    database = var.database_name
    host     = var.host
    port     = var.port
    username = var.admin_username
    password = var.admin_password
    # Table definition hash for change detection
    table_hash = sha256("resource_audit_v2")
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "
          -- Create resource_audit table
          CREATE TABLE IF NOT EXISTS ${var.schema_name}.resource_audit (
            audit_id          SERIAL PRIMARY KEY,
            resource_id       TEXT NOT NULL,
            resource_type     TEXT NOT NULL,
            resource_name     TEXT,
            compliance_status TEXT NOT NULL CHECK (compliance_status IN ('COMPLIANT', 'NON_COMPLIANT', 'ERROR', 'PENDING', 'SKIPPED')),
            rule_id           INTEGER REFERENCES ${var.schema_name}.compliance_rules(rule_id),
            checked_at        TIMESTAMPTZ DEFAULT NOW(),
            evidence          JSONB DEFAULT '{}'::jsonb,
            remediation       TEXT,
            created_by        TEXT DEFAULT CURRENT_USER,
            scan_id           UUID DEFAULT uuid_generate_v4(),
            environment       TEXT DEFAULT 'production'
          );

          -- Add table comment for documentation
          COMMENT ON TABLE ${var.schema_name}.resource_audit IS 
            'Compliance audit trail | compliance-as-code | managed_by=terraform';

          -- Add column comments
          COMMENT ON COLUMN ${var.schema_name}.resource_audit.audit_id IS 'Unique audit record identifier';
          COMMENT ON COLUMN ${var.schema_name}.resource_audit.resource_id IS 'Cloud resource ARN or identifier';
          COMMENT ON COLUMN ${var.schema_name}.resource_audit.compliance_status IS 'Check result: COMPLIANT, NON_COMPLIANT, ERROR, PENDING, SKIPPED';
          COMMENT ON COLUMN ${var.schema_name}.resource_audit.evidence IS 'JSON evidence data from the compliance check';
          COMMENT ON COLUMN ${var.schema_name}.resource_audit.scan_id IS 'UUID grouping related audit records from same scan';

          -- Create indexes for common queries
          CREATE INDEX IF NOT EXISTS idx_resource_audit_status 
            ON ${var.schema_name}.resource_audit(compliance_status);
          CREATE INDEX IF NOT EXISTS idx_resource_audit_checked_at 
            ON ${var.schema_name}.resource_audit(checked_at DESC);
          CREATE INDEX IF NOT EXISTS idx_resource_audit_resource_type 
            ON ${var.schema_name}.resource_audit(resource_type);
          CREATE INDEX IF NOT EXISTS idx_resource_audit_rule_id 
            ON ${var.schema_name}.resource_audit(rule_id);
          CREATE INDEX IF NOT EXISTS idx_resource_audit_scan_id 
            ON ${var.schema_name}.resource_audit(scan_id);
          CREATE INDEX IF NOT EXISTS idx_resource_audit_environment 
            ON ${var.schema_name}.resource_audit(environment);

          -- Grant permissions to compliance user
          GRANT ALL ON ${var.schema_name}.resource_audit TO ${var.compliance_user};
          GRANT USAGE, SELECT ON SEQUENCE ${var.schema_name}.resource_audit_audit_id_seq TO ${var.compliance_user};
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
        -c "DROP TABLE IF EXISTS ${self.triggers.schema}.resource_audit CASCADE;" \
        2>/dev/null || true
    EOT
  }
}

#--------------------------------------------------------------
# Audit Summary View
# digital organism metaphor: Neural dashboard - aggregate health metrics
# Provides quick compliance statistics
# compliance-as-code: Reporting helper
#--------------------------------------------------------------
resource "null_resource" "audit_summary_view" {
  triggers = {
    table_id = null_resource.resource_audit_table.id
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "
          -- Create or replace the audit summary view
          CREATE OR REPLACE VIEW ${var.schema_name}.audit_summary AS
          SELECT 
            r.rule_name,
            r.severity,
            r.category,
            a.compliance_status,
            COUNT(*) as resource_count,
            MAX(a.checked_at) as last_checked
          FROM ${var.schema_name}.resource_audit a
          JOIN ${var.schema_name}.compliance_rules r ON a.rule_id = r.rule_id
          GROUP BY r.rule_name, r.severity, r.category, a.compliance_status
          ORDER BY 
            CASE r.severity 
              WHEN 'CRITICAL' THEN 1 
              WHEN 'HIGH' THEN 2 
              WHEN 'MEDIUM' THEN 3 
              WHEN 'LOW' THEN 4 
            END,
            a.compliance_status;

          -- Grant read access
          GRANT SELECT ON ${var.schema_name}.audit_summary TO ${var.compliance_user};

          COMMENT ON VIEW ${var.schema_name}.audit_summary IS 
            'Aggregated compliance status by rule | compliance-as-code';
        "
    EOT
  }

  depends_on = [null_resource.resource_audit_table]
}

#--------------------------------------------------------------
# Sample Audit Data (Optional)
# digital organism metaphor: Training data for immune response
# Seeds example audit records for testing
#--------------------------------------------------------------
resource "null_resource" "seed_audit_data" {
  count = var.seed_sample_data ? 1 : 0

  triggers = {
    table_id = null_resource.resource_audit_table.id
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "
          -- Seed sample audit data for testing
          INSERT INTO ${var.schema_name}.resource_audit 
            (resource_id, resource_type, resource_name, compliance_status, rule_id, evidence, environment)
          SELECT 
            'arn:aws:s3:::example-bucket-' || i,
            'aws_s3_bucket',
            'example-bucket-' || i,
            CASE WHEN i % 3 = 0 THEN 'NON_COMPLIANT' ELSE 'COMPLIANT' END,
            (SELECT rule_id FROM ${var.schema_name}.compliance_rules WHERE rule_name LIKE '%S3%' LIMIT 1),
            jsonb_build_object('encryption', CASE WHEN i % 3 = 0 THEN 'disabled' ELSE 'AES256' END),
            'development'
          FROM generate_series(1, 5) as i
          ON CONFLICT DO NOTHING;

          -- Add some security group checks
          INSERT INTO ${var.schema_name}.resource_audit 
            (resource_id, resource_type, resource_name, compliance_status, rule_id, evidence, environment)
          VALUES
            ('sg-abc123', 'aws_security_group', 'web-sg', 'COMPLIANT', 
             (SELECT rule_id FROM ${var.schema_name}.compliance_rules WHERE rule_name LIKE '%SSH%' LIMIT 1),
             '{\"ssh_open\": false}'::jsonb, 'production'),
            ('sg-def456', 'aws_security_group', 'bastion-sg', 'NON_COMPLIANT', 
             (SELECT rule_id FROM ${var.schema_name}.compliance_rules WHERE rule_name LIKE '%SSH%' LIMIT 1),
             '{\"ssh_open\": true, \"cidr\": \"0.0.0.0/0\"}'::jsonb, 'production')
          ON CONFLICT DO NOTHING;
        "
    EOT
  }

  depends_on = [null_resource.resource_audit_table]
}

#--------------------------------------------------------------
# Validate Table Creation
# digital organism metaphor: System health diagnostic
#--------------------------------------------------------------
resource "null_resource" "validate_audit_table" {
  triggers = {
    table_id = null_resource.resource_audit_table.id
    view_id  = null_resource.audit_summary_view.id
  }

  provisioner "local-exec" {
    command = <<-EOT
      echo "=== Validating resource_audit table ==="
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "\\d ${var.schema_name}.resource_audit"
    EOT
  }

  depends_on = [null_resource.audit_summary_view]
}
