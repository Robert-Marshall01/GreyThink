# =============================================================================
# GENERAL VARIABLES
# =============================================================================
# Core configuration for naming, location, and tagging
# MAINTAINABILITY: Use consistent naming conventions and descriptive defaults

variable "resource_group_name" {
  description = "Name of the resource group - follows Azure naming convention (rg-<workload>-<env>)"
  type        = string
  default     = "rg-cloud-example"

  validation {
    condition     = can(regex("^rg-", var.resource_group_name))
    error_message = "Resource group name should start with 'rg-' prefix."
  }
}

variable "location" {
  description = "Azure region for resources - choose based on latency and compliance requirements"
  type        = string
  default     = "eastus"
}

variable "tags" {
  description = "Tags to apply to all resources - used for cost allocation and organization"
  type        = map(string)
  default = {
    Environment = "Development"
    Project     = "CloudExample"
    ManagedBy   = "Terraform"
  }
}

# =============================================================================
# SECURITY VARIABLES
# =============================================================================
# Configuration for security features across all resources

variable "allowed_ip_ranges" {
  description = "List of allowed IP ranges for Key Vault access (CIDR notation)"
  type        = list(string)
  default     = []  # Empty = no IP whitelist (rely on Azure RBAC)
}

variable "log_retention_days" {
  description = "Number of days to retain logs in Log Analytics - set based on compliance requirements"
  type        = number
  default     = 30

  validation {
    condition     = var.log_retention_days >= 30 && var.log_retention_days <= 730
    error_message = "Log retention must be between 30 and 730 days."
  }
}

# =============================================================================
# AKS VARIABLES - CORE
# =============================================================================
# Basic AKS cluster configuration

variable "aks_cluster_name" {
  description = "Name of the AKS cluster - follows Azure naming convention (aks-<workload>-<env>)"
  type        = string
  default     = "aks-cloud-example"
}

variable "aks_dns_prefix" {
  description = "DNS prefix for the AKS cluster - must be unique within Azure"
  type        = string
  default     = "akscloudexample"
}

variable "kubernetes_version" {
  description = "Kubernetes version for the AKS cluster - check supported versions with 'az aks get-versions'"
  type        = string
  default     = "1.28"
}

# =============================================================================
# AKS VARIABLES - SECURITY
# =============================================================================
# SECURITY: Azure RBAC, AAD integration, and access control

variable "aks_enable_azure_rbac" {
  description = "Enable Azure RBAC for Kubernetes authorization - recommended for enterprise security"
  type        = bool
  default     = true
}

variable "aks_admin_group_ids" {
  description = "List of Azure AD group object IDs that will have admin access to the cluster"
  type        = list(string)
  default     = []  # SECURITY: Add your AAD group IDs here
}

variable "aks_disable_local_accounts" {
  description = "Disable local Kubernetes accounts (kubeconfig) - forces Azure AD authentication"
  type        = bool
  default     = false  # Set to true in production for better security
}

variable "aks_enable_host_encryption" {
  description = "Enable encryption at host for AKS nodes - encrypts temp disk and OS disk cache"
  type        = bool
  default     = false  # Requires subscription feature registration
}

# =============================================================================
# AKS VARIABLES - SCALABILITY (SYSTEM NODE POOL)
# =============================================================================
# Configuration for the system node pool (runs critical system pods)

variable "aks_node_count" {
  description = "Initial number of nodes in the system node pool"
  type        = number
  default     = 2

  validation {
    condition     = var.aks_node_count >= 1
    error_message = "Node count must be at least 1."
  }
}

variable "aks_vm_size" {
  description = "VM size for AKS system nodes - choose based on workload requirements"
  type        = string
  default     = "Standard_DS2_v2"
}

variable "aks_enable_auto_scaling" {
  description = "Enable auto-scaling for the system node pool"
  type        = bool
  default     = true
}

variable "aks_min_node_count" {
  description = "Minimum number of nodes when auto-scaling is enabled (system pool)"
  type        = number
  default     = 1
}

variable "aks_max_node_count" {
  description = "Maximum number of nodes when auto-scaling is enabled (system pool)"
  type        = number
  default     = 5
}

variable "aks_availability_zones" {
  description = "Availability zones for AKS nodes - use multiple zones for HA"
  type        = list(string)
  default     = ["1", "2", "3"]  # SCALABILITY: Deploy across all zones
}

# =============================================================================
# AKS VARIABLES - SCALABILITY (USER NODE POOL)
# =============================================================================
# Configuration for the user node pool (runs application workloads)

variable "aks_user_vm_size" {
  description = "VM size for AKS user nodes - typically larger for application workloads"
  type        = string
  default     = "Standard_DS3_v2"
}

variable "aks_user_min_count" {
  description = "Minimum number of user nodes when auto-scaling is enabled"
  type        = number
  default     = 1
}

variable "aks_user_max_count" {
  description = "Maximum number of user nodes when auto-scaling is enabled"
  type        = number
  default     = 10
}

# =============================================================================
# AKS VARIABLES - NETWORKING
# =============================================================================
# Network configuration for the AKS cluster

variable "aks_service_cidr" {
  description = "CIDR range for Kubernetes services - must not overlap with VNet"
  type        = string
  default     = "10.0.0.0/16"
}

variable "aks_dns_service_ip" {
  description = "IP address for Kubernetes DNS service - must be within service_cidr"
  type        = string
  default     = "10.0.0.10"
}

# =============================================================================
# AKS VARIABLES - MAINTAINABILITY
# =============================================================================
# Upgrade and maintenance configuration

variable "aks_upgrade_channel" {
  description = "Automatic upgrade channel: none, patch, rapid, stable, node-image"
  type        = string
  default     = "patch"  # MAINTAINABILITY: Auto-apply security patches

  validation {
    condition     = contains(["none", "patch", "rapid", "stable", "node-image"], var.aks_upgrade_channel)
    error_message = "Upgrade channel must be one of: none, patch, rapid, stable, node-image."
  }
}

# =============================================================================
# POSTGRESQL VARIABLES - CORE
# =============================================================================
# Basic PostgreSQL configuration

variable "postgres_server_name" {
  description = "Name of the PostgreSQL Flexible Server - must be globally unique"
  type        = string
  default     = "psql-cloud-example"
}

variable "postgres_version" {
  description = "PostgreSQL version - check supported versions in Azure documentation"
  type        = string
  default     = "15"
}

variable "postgres_admin_login" {
  description = "Administrator login for PostgreSQL - avoid common names like 'admin' or 'postgres'"
  type        = string
  default     = "psqladmin"

  validation {
    condition     = !contains(["admin", "administrator", "root", "postgres"], lower(var.postgres_admin_login))
    error_message = "Admin login cannot be a reserved name."
  }
}

variable "postgres_database_name" {
  description = "Name of the PostgreSQL database to create"
  type        = string
  default     = "appdb"
}

# =============================================================================
# POSTGRESQL VARIABLES - SCALABILITY
# =============================================================================
# Storage, compute, and high availability configuration

variable "postgres_storage_mb" {
  description = "Storage size in MB for PostgreSQL (min 32GB, max 16TB)"
  type        = number
  default     = 32768  # 32 GB

  validation {
    condition     = var.postgres_storage_mb >= 32768
    error_message = "Storage must be at least 32768 MB (32 GB)."
  }
}

variable "postgres_sku_name" {
  description = "SKU name for PostgreSQL - format: <tier>_<compute>_<vCores> (e.g., GP_Standard_D2s_v3)"
  type        = string
  default     = "GP_Standard_D2s_v3"
}

variable "postgres_ha_mode" {
  description = "High availability mode: Disabled, SameZone, ZoneRedundant"
  type        = string
  default     = "ZoneRedundant"  # SCALABILITY: Use zone-redundant for production

  validation {
    condition     = contains(["Disabled", "SameZone", "ZoneRedundant"], var.postgres_ha_mode)
    error_message = "HA mode must be one of: Disabled, SameZone, ZoneRedundant."
  }
}

# =============================================================================
# POSTGRESQL VARIABLES - BACKUP & RECOVERY
# =============================================================================
# Backup configuration for disaster recovery

variable "postgres_backup_retention_days" {
  description = "Backup retention days for PostgreSQL (7-35 days)"
  type        = number
  default     = 7

  validation {
    condition     = var.postgres_backup_retention_days >= 7 && var.postgres_backup_retention_days <= 35
    error_message = "Backup retention must be between 7 and 35 days."
  }
}

variable "postgres_geo_redundant_backup" {
  description = "Enable geo-redundant backup for PostgreSQL - recommended for production"
  type        = bool
  default     = false
}
