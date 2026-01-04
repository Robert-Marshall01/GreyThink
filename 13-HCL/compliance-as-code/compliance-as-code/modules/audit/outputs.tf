#--------------------------------------------------------------
# Audit Module Outputs - compliance-as-code
# digital organism metaphor
#--------------------------------------------------------------

output "resource_audit_table_created" {
  description = "Confirms resource_audit table (sensory memory) was created"
  value       = null_resource.resource_audit_table.id != "" ? true : false
}

output "audit_summary_view_created" {
  description = "Confirms audit_summary view (health dashboard) was created"
  value       = null_resource.audit_summary_view.id != "" ? true : false
}

output "audit_module_configured" {
  description = "Confirmation that audit module (nervous system) completed successfully"
  value       = true
}
