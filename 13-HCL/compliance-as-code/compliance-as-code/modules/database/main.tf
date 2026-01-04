#--------------------------------------------------------------
# Database Module - compliance-as-code
# digital organism metaphor
# 
# Purpose: Creates the compliance user role and enables extensions
# This module represents the "DNA" of the digital organism - the
# foundational genetic code that defines how the organism operates.
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
# PostgreSQL Extension: UUID Generation
# digital organism metaphor: Cellular identification system
# Required for generating unique identifiers in audit records
#--------------------------------------------------------------
resource "postgresql_extension" "uuid_ossp" {
  name     = "uuid-ossp"
  database = var.database_name

  lifecycle {
    prevent_destroy = false
  }
}

#--------------------------------------------------------------
# Compliance User Role
# digital organism metaphor: Primary neural pathway for organism control
# Application-level user for compliance checking operations
#--------------------------------------------------------------
resource "postgresql_role" "compliance_user" {
  name     = var.compliance_user
  login    = true
  password = var.compliance_user_password

  # Role attributes - organism permissions
  create_database = false
  create_role     = false
  superuser       = false
  inherit         = true

  lifecycle {
    prevent_destroy = false
  }
}

#--------------------------------------------------------------
# Database Metadata Comments
# digital organism metaphor: Organism identity and lineage markers
# Annotations for compliance tracking and ownership
#--------------------------------------------------------------
resource "null_resource" "database_metadata" {
  triggers = {
    database = var.database_name
    owner    = var.metadata_owner
  }

  provisioner "local-exec" {
    command = <<-EOT
      PGPASSWORD='${var.admin_password}' psql \
        -h ${var.host} \
        -p ${var.port} \
        -U ${var.admin_username} \
        -d ${var.database_name} \
        -c "COMMENT ON DATABASE ${var.database_name} IS 'Compliance auditing database | compliance=true | owner=${var.metadata_owner} | managed_by=terraform';"
    EOT
  }
}
