#--------------------------------------------------------------
# Database Module Variables - compliance-as-code
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

variable "compliance_user" {
  description = "Name of the compliance role to create"
  type        = string
}

variable "compliance_user_password" {
  description = "Password for the compliance user role"
  type        = string
  sensitive   = true
}

variable "metadata_owner" {
  description = "Owner metadata tag for the database"
  type        = string
  default     = "devsecops"
}
