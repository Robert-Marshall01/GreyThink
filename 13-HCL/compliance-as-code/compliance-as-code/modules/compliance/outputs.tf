#--------------------------------------------------------------
# Compliance Module Outputs - compliance-as-code
# digital organism metaphor
#--------------------------------------------------------------

output "schema_name" {
  description = "Name of the created compliance schema"
  value       = postgresql_schema.compliance.name
}

output "schema_id" {
  description = "ID of the created compliance schema"
  value       = postgresql_schema.compliance.id
}

output "immune_rules_table_created" {
  description = "Confirms immune_rules table (antibody library) was created"
  value       = null_resource.immune_rules_table.id != "" ? true : false
}

output "immune_rules_seeded" {
  description = "Confirms immune rules (antibodies) were seeded"
  value       = null_resource.seed_immune_rules.id != "" ? true : false
}

output "compliance_rules_table_created" {
  description = "Confirms compliance_rules table was created"
  value       = null_resource.compliance_rules_table.id != "" ? true : false
}

output "compliance_rules_seeded" {
  description = "Confirms compliance rules were seeded"
  value       = null_resource.seed_compliance_rules.id != "" ? true : false
}

output "schema_owner" {
  description = "Owner of the compliance schema"
  value       = postgresql_schema.compliance.owner
}
