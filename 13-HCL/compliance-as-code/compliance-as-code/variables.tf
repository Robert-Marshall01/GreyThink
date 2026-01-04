#--------------------------------------------------------------
# Variables for PostgreSQL Compliance Auditing Database
# 
# This configuration uses the PostgreSQL provider to manage
# a local Dockerized PostgreSQL instance for development/testing.
#--------------------------------------------------------------

#--------------------------------------------------------------
# PostgreSQL Connection Settings
#--------------------------------------------------------------

variable "postgres_host" {
  description = "PostgreSQL server hostname"
  type        = string
  default     = "localhost"
}

variable "postgres_port" {
  description = "PostgreSQL server port"
  type        = number
  default     = 5432
}

variable "postgres_database" {
  description = "PostgreSQL database name"
  type        = string
  default     = "compliance_db"
}

variable "postgres_username" {
  description = "PostgreSQL admin username for provider authentication. Set via TF_VAR_postgres_username"
  type        = string
  sensitive   = true
}

variable "postgres_password" {
  description = "PostgreSQL admin password for provider authentication. Set via TF_VAR_postgres_password"
  type        = string
  sensitive   = true

  validation {
    condition     = length(var.postgres_password) >= 8
    error_message = "Password must be at least 8 characters."
  }
}

variable "postgres_sslmode" {
  description = "SSL mode for PostgreSQL connection (disable for local Docker)"
  type        = string
  default     = "disable"

  validation {
    condition     = contains(["disable", "require", "verify-ca", "verify-full"], var.postgres_sslmode)
    error_message = "SSL mode must be one of: disable, require, verify-ca, verify-full."
  }
}

#--------------------------------------------------------------
# Compliance User Configuration
#--------------------------------------------------------------

variable "compliance_user" {
  description = "Username for the compliance application role"
  type        = string
  default     = "compliance_user"
}

variable "compliance_user_password" {
  description = "Password for the compliance application role. Set via TF_VAR_compliance_user_password"
  type        = string
  sensitive   = true

  validation {
    condition     = length(var.compliance_user_password) >= 8
    error_message = "Password must be at least 8 characters."
  }
}

#--------------------------------------------------------------
# Schema Configuration
#--------------------------------------------------------------

variable "schema_name" {
  description = "Name of the schema for compliance objects"
  type        = string
  default     = "compliance"
}

#--------------------------------------------------------------
# Metadata / Tagging
# 
# PostgreSQL uses COMMENT statements for metadata annotations
# since it doesn't have a native tagging system like cloud providers.
#--------------------------------------------------------------

variable "metadata" {
  description = "Metadata annotations for compliance context"
  type        = map(string)
  default = {
    compliance  = "true"
    owner       = "devsecops"
    managed_by  = "terraform"
    environment = "development"
  }
}

#--------------------------------------------------------------
# Optional Features
#--------------------------------------------------------------

variable "seed_sample_data" {
  description = "Whether to seed sample audit data for testing/demo purposes"
  type        = bool
  default     = false
}
