#--------------------------------------------------------------
# Database Module Outputs - compliance-as-code
#--------------------------------------------------------------

output "compliance_user_name" {
  description = "Name of the created compliance user role"
  value       = postgresql_role.compliance_user.name
}

output "compliance_user_id" {
  description = "ID of the created compliance user role"
  value       = postgresql_role.compliance_user.id
}

output "uuid_extension_enabled" {
  description = "Confirms uuid-ossp extension is enabled"
  value       = postgresql_extension.uuid_ossp.name != "" ? true : false
}

output "database_configured" {
  description = "Confirmation that database module completed successfully"
  value       = true
}
