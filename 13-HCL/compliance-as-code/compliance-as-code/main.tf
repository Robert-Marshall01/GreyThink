#--------------------------------------------------------------
# Terraform Configuration for PostgreSQL Compliance Auditing Database
# compliance-as-code
# digital organism metaphor
# 
# This configuration creates a living digital organism with:
#   - database module: DNA/genetic code - foundational identity
#   - compliance module: Immune system - antibodies and adaptive responses
#   - audit module: Nervous system - sensory organs and response logging
#
# The organism protects your infrastructure through biological defense
# patterns, detecting threats and logging responses like a living system.
#
# Prerequisites:
#   1. Docker running with PostgreSQL container
#   2. Set environment variables:
#      export TF_VAR_postgres_password="YourSecurePassword123!"
#      export TF_VAR_compliance_user_password="AnotherSecurePass456!"
#   3. Run: docker compose up -d
#--------------------------------------------------------------

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    postgresql = {
      source  = "cyrilgdn/postgresql"
      version = "~> 1.21"
    }
    null = {
      source  = "hashicorp/null"
      version = "~> 3.2"
    }
  }
}

#--------------------------------------------------------------
# PostgreSQL Provider Configuration
# digital organism metaphor: Connection to the host body
# Connects to local Dockerized PostgreSQL instance
# compliance-as-code: Provider setup
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
# digital organism metaphor: Shared organism parameters
#--------------------------------------------------------------

locals {
  # Common connection parameters passed to all modules
  connection_params = {
    host           = var.postgres_host
    port           = var.postgres_port
    database_name  = var.postgres_database
    admin_username = var.postgres_username
    admin_password = var.postgres_password
  }
}

#--------------------------------------------------------------
# Module: Database
# digital organism metaphor: DNA/Genetic Code Layer
# Creates compliance user role and enables extensions
# compliance-as-code: Foundation layer
#--------------------------------------------------------------

module "database" {
  source = "./modules/database"

  host                     = local.connection_params.host
  port                     = local.connection_params.port
  database_name            = local.connection_params.database_name
  admin_username           = local.connection_params.admin_username
  admin_password           = local.connection_params.admin_password
  compliance_user          = var.compliance_user
  compliance_user_password = var.compliance_user_password
  metadata_owner           = var.metadata.owner
}

#--------------------------------------------------------------
# Module: Compliance
# digital organism metaphor: Immune System Layer
# Creates schema, immune_rules (antibodies), and compliance_rules
# compliance-as-code: Rule definition layer
#--------------------------------------------------------------

module "compliance" {
  source = "./modules/compliance"

  host            = local.connection_params.host
  port            = local.connection_params.port
  database_name   = local.connection_params.database_name
  admin_username  = local.connection_params.admin_username
  admin_password  = local.connection_params.admin_password
  schema_name     = var.schema_name
  compliance_user = var.compliance_user

  depends_on = [module.database]
}

#--------------------------------------------------------------
# Module: Audit
# digital organism metaphor: Nervous System Layer
# Creates resource_audit table and reporting views
# compliance-as-code: Audit trail layer
#--------------------------------------------------------------

module "audit" {
  source = "./modules/audit"

  host             = local.connection_params.host
  port             = local.connection_params.port
  database_name    = local.connection_params.database_name
  admin_username   = local.connection_params.admin_username
  admin_password   = local.connection_params.admin_password
  schema_name      = var.schema_name
  compliance_user  = var.compliance_user
  seed_sample_data = var.seed_sample_data

  depends_on = [module.compliance]
}

#--------------------------------------------------------------
# Validation: Confirm Setup
# digital organism metaphor: Organism vitality check
# Runs after all modules to verify the installation
# compliance-as-code: Validation layer
#--------------------------------------------------------------

resource "null_resource" "validate_setup" {
  triggers = {
    database_done   = module.database.database_configured
    compliance_done = module.compliance.compliance_rules_seeded
    immune_done     = module.compliance.immune_rules_seeded
    audit_done      = module.audit.audit_module_configured
  }

  provisioner "local-exec" {
    command = <<-EOT
      echo ""
      echo "=========================================="
      echo "  🧬 Digital Organism: Setup Complete"
      echo "  compliance-as-code"
      echo "=========================================="
      echo ""
      PGPASSWORD='${var.postgres_password}' psql \
        -h ${var.postgres_host} \
        -p ${var.postgres_port} \
        -U ${var.postgres_username} \
        -d ${var.postgres_database} \
        -c "
          SELECT '✅ Schema: ' || schema_name AS status 
          FROM information_schema.schemata 
          WHERE schema_name = '${var.schema_name}';
          
          SELECT '✅ Tables: ' || string_agg(table_name, ', ') AS status
          FROM information_schema.tables 
          WHERE table_schema = '${var.schema_name}';
          
          SELECT '🛡️ Immune Rules (Antibodies): ' || COUNT(*)::text AS status
          FROM ${var.schema_name}.immune_rules;
          
          SELECT '📋 Compliance Rules: ' || COUNT(*)::text AS status
          FROM ${var.schema_name}.compliance_rules;
        "
      echo ""
      echo "🔗 Connect with: PGPASSWORD='\$TF_VAR_postgres_password' psql -h localhost -p 5432 -U admin -d compliance_db"
      echo "📊 Verify tables: \\dt compliance.*"
      echo ""
    EOT
  }

  depends_on = [module.audit]
}
