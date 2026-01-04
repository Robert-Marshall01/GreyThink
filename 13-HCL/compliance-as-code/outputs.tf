#--------------------------------------------------------------
# Outputs for PostgreSQL Compliance Auditing Database
# 
# Provides connection information and resource identifiers
# for the Terraform-managed PostgreSQL objects.
#--------------------------------------------------------------

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
# 
# Various formats for different applications/tools
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
  value       = postgresql_role.compliance_user.name
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
  value       = postgresql_schema.compliance.name
}

output "schema_owner" {
  description = "Owner of the compliance schema"
  value       = postgresql_schema.compliance.owner
}

#--------------------------------------------------------------
# Table Names
#--------------------------------------------------------------

output "compliance_rules_table" {
  description = "Fully qualified name of the compliance_rules table"
  value       = "${var.schema_name}.compliance_rules"
}

output "resource_audit_table" {
  description = "Fully qualified name of the resource_audit table"
  value       = "${var.schema_name}.resource_audit"
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
  value       = "docker-compose up -d"
}

output "docker_stop_command" {
  description = "Command to stop the PostgreSQL container"
  value       = "docker-compose down"
}

output "docker_logs_command" {
  description = "Command to view PostgreSQL container logs"
  value       = "docker-compose logs -f postgres"
}
