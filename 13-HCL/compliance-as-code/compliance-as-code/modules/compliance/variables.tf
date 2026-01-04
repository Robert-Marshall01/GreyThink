#--------------------------------------------------------------
# Compliance Module Variables - compliance-as-code
#--------------------------------------------------------------

variable "host" {
  description = "PostgreSQL server hostname"
  type        = string
}

variable "port" {
  description = "PostgreSQL server port"
  type        = number
}

variable "database_name" {
  description = "PostgreSQL database name"
  type        = string
}

variable "admin_username" {
  description = "PostgreSQL admin username"
  type        = string
}

variable "admin_password" {
  description = "PostgreSQL admin password"
  type        = string
  sensitive   = true
}

variable "schema_name" {
  description = "Name of the compliance schema"
  type        = string
  default     = "compliance"
}

variable "compliance_user" {
  description = "Name of the compliance role"
  type        = string
}
