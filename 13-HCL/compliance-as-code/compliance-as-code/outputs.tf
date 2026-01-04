#--------------------------------------------------------------
# Outputs for PostgreSQL Compliance Auditing Database
# compliance-as-code
# digital organism metaphor
# 
# Provides connection information, validation status, and
# resource identifiers for the Terraform-managed PostgreSQL objects.
#--------------------------------------------------------------

#--------------------------------------------------------------
# Validation Outputs - Confirm Setup Success
# digital organism metaphor: Organism vitality report
# compliance-as-code: Verification layer
#--------------------------------------------------------------

output "setup_validated" {
  description = "Confirms all modules (organism systems) completed successfully"
  value = {
    database_module   = module.database.database_configured
    compliance_module = module.compliance.compliance_rules_seeded
    immune_module     = module.compliance.immune_rules_seeded
    audit_module      = module.audit.audit_module_configured
  }
}

output "schema_exists" {
  description = "Confirms the compliance schema (organism boundary) was created"
  value       = module.compliance.schema_name != "" ? "✅ Schema '${module.compliance.schema_name}' exists" : "❌ Schema not found"
}

output "tables_exist" {
  description = "Confirms the required tables (organism components) were created"
  value = {
    immune_rules     = module.compliance.immune_rules_table_created ? "✅ immune_rules (antibody library) created" : "❌ Not created"
    compliance_rules = module.compliance.compliance_rules_table_created ? "✅ compliance_rules (adaptive memory) created" : "❌ Not created"
    resource_audit   = module.audit.resource_audit_table_created ? "✅ resource_audit (sensory log) created" : "❌ Not created"
  }
}

#--------------------------------------------------------------
# Connection Information
#--------------------------------------------------------------

output "postgres_host" {
  description = "PostgreSQL server hostname"
  value       = var.postgres_host
}

output "postgres_port" {
  description = "PostgreSQL server port"
  value       = var.postgres_port
}

output "postgres_database" {
  description = "PostgreSQL database name"
  value       = var.postgres_database
}

#--------------------------------------------------------------
# Connection Strings
#--------------------------------------------------------------

output "connection_string_admin" {
  description = "PostgreSQL connection string for admin user"
  sensitive   = true
  value       = "postgresql://${var.postgres_username}:${var.postgres_password}@${var.postgres_host}:${var.postgres_port}/${var.postgres_database}"
}

output "connection_string_compliance_user" {
  description = "PostgreSQL connection string for compliance_user"
  sensitive   = true
  value       = "postgresql://${var.compliance_user}:${var.compliance_user_password}@${var.postgres_host}:${var.postgres_port}/${var.postgres_database}"
}

output "psql_command_admin" {
  description = "psql CLI command to connect as admin"
  sensitive   = true
  value       = "PGPASSWORD='${var.postgres_password}' psql -h ${var.postgres_host} -p ${var.postgres_port} -U ${var.postgres_username} -d ${var.postgres_database}"
}

output "psql_command_compliance_user" {
  description = "psql CLI command to connect as compliance_user"
  sensitive   = true
  value       = "PGPASSWORD='${var.compliance_user_password}' psql -h ${var.postgres_host} -p ${var.postgres_port} -U ${var.compliance_user} -d ${var.postgres_database}"
}

#--------------------------------------------------------------
# Role Information
#--------------------------------------------------------------

output "compliance_user_name" {
  description = "Name of the compliance application role"
  value       = module.database.compliance_user_name
}

output "compliance_user_password" {
  description = "Password for the compliance application role"
  sensitive   = true
  value       = var.compliance_user_password
}

#--------------------------------------------------------------
# Schema Information
#--------------------------------------------------------------

output "schema_name" {
  description = "Name of the compliance schema"
  value       = module.compliance.schema_name
}

output "schema_owner" {
  description = "Owner of the compliance schema"
  value       = module.compliance.schema_owner
}

#--------------------------------------------------------------
# Table Names
# digital organism metaphor: Organ identifiers
#--------------------------------------------------------------

output "immune_rules_table" {
  description = "Fully qualified name of the immune_rules table (antibody library)"
  value       = "${var.schema_name}.immune_rules"
}

output "compliance_rules_table" {
  description = "Fully qualified name of the compliance_rules table (adaptive memory)"
  value       = "${var.schema_name}.compliance_rules"
}

output "resource_audit_table" {
  description = "Fully qualified name of the resource_audit table (sensory log)"
  value       = "${var.schema_name}.resource_audit"
}

#--------------------------------------------------------------
# Module Information
#--------------------------------------------------------------

output "modules_loaded" {
  description = "List of loaded modules"
  value       = ["database", "compliance", "audit"]
}

output "uuid_extension_enabled" {
  description = "Confirms uuid-ossp extension is enabled"
  value       = module.database.uuid_extension_enabled
}

#--------------------------------------------------------------
# Metadata
#--------------------------------------------------------------

output "metadata" {
  description = "Compliance metadata applied to database objects"
  value       = var.metadata
}

#--------------------------------------------------------------
# Docker Commands (for convenience)
#--------------------------------------------------------------

output "docker_start_command" {
  description = "Command to start the PostgreSQL container"
  value       = "docker compose up -d"
}

output "docker_stop_command" {
  description = "Command to stop the PostgreSQL container"
  value       = "docker compose down"
}

output "docker_logs_command" {
  description = "Command to view PostgreSQL container logs"
  value       = "docker compose logs -f postgres"
}

#--------------------------------------------------------------
# Quick Verification Commands
# digital organism metaphor: Diagnostic tools
# compliance-as-code: Helper outputs
#--------------------------------------------------------------

output "verify_tables_command" {
  description = "Command to verify all organism tables were created"
  value       = "PGPASSWORD='$TF_VAR_postgres_password' psql -h localhost -p 5432 -U admin -d compliance_db -c '\\dt compliance.*'"
}

output "verify_immune_rules_command" {
  description = "Command to view antibody library (immune_rules)"
  value       = "PGPASSWORD='$TF_VAR_postgres_password' psql -h localhost -p 5432 -U admin -d compliance_db -c 'SELECT antibody_name, threat_signature, severity_level FROM compliance.immune_rules;'"
}

output "verify_compliance_rules_command" {
  description = "Command to view seeded compliance rules"
  value       = "PGPASSWORD='$TF_VAR_postgres_password' psql -h localhost -p 5432 -U admin -d compliance_db -c 'SELECT rule_name, severity FROM compliance.compliance_rules;'"
}
