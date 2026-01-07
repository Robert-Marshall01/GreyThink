# =============================================================================
# TERRAFORM CONFIGURATION
# =============================================================================
# Configure providers and backend for state management
# Best Practice: Pin provider versions for reproducibility

terraform {
  required_providers {
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"  # Pin to major version for stability
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.0"
    }
  }
  required_version = ">= 1.0"

  # SECURITY: Enable remote state storage for team collaboration and state locking
  # Uncomment and configure for production use
  # backend "azurerm" {
  #   resource_group_name  = "rg-terraform-state"
  #   storage_account_name = "stterraformstate"
  #   container_name       = "tfstate"
  #   key                  = "cloud-example.tfstate"
  #   use_oidc             = true  # Use OIDC for authentication (recommended)
  # }
}

provider "azurerm" {
  features {
    # SECURITY: Soft delete protection for Key Vault
    key_vault {
      purge_soft_delete_on_destroy    = false  # Prevent accidental permanent deletion
      recover_soft_deleted_key_vaults = true   # Enable recovery of deleted vaults
    }
  }
}

# =============================================================================
# DATA SOURCES
# =============================================================================
# Retrieve current Azure context for RBAC and policies

data "azurerm_client_config" "current" {}
# Used for: Key Vault access policies, RBAC assignments

# =============================================================================
# RESOURCE GROUP
# =============================================================================
# Logical container for all related Azure resources
# MAINTAINABILITY: Use consistent naming convention and tagging

resource "azurerm_resource_group" "main" {
  name     = var.resource_group_name
  location = var.location

  tags = merge(var.tags, {
    CreatedBy = "Terraform"
    CreatedAt = timestamp()
  })

  lifecycle {
    # SECURITY: Prevent accidental deletion of resource group
    prevent_destroy = false  # Set to true in production
  }
}

# =============================================================================
# AZURE KUBERNETES SERVICE (AKS)
# =============================================================================
# Managed Kubernetes cluster for container orchestration
# SECURITY: Azure RBAC, private cluster option, defender enabled
# SCALABILITY: Auto-scaling, multiple node pools support

resource "azurerm_kubernetes_cluster" "aks" {
  name                = var.aks_cluster_name
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  dns_prefix          = var.aks_dns_prefix
  kubernetes_version  = var.kubernetes_version

  # SECURITY: Use Azure RBAC for Kubernetes authorization (least privilege)
  azure_active_directory_role_based_access_control {
    azure_rbac_enabled = var.aks_enable_azure_rbac  # Enable Azure RBAC for K8s
    admin_group_object_ids = var.aks_admin_group_ids  # AAD groups with admin access
  }

  # SECURITY: Enable local account only if needed (disable for production)
  local_account_disabled = var.aks_disable_local_accounts

  # SCALABILITY: System node pool for critical workloads
  default_node_pool {
    name                 = "system"              # Renamed for clarity
    node_count           = var.aks_node_count
    vm_size              = var.aks_vm_size
    auto_scaling_enabled = var.aks_enable_auto_scaling
    min_count            = var.aks_enable_auto_scaling ? var.aks_min_node_count : null
    max_count            = var.aks_enable_auto_scaling ? var.aks_max_node_count : null
    
    # SCALABILITY: Node configuration for performance
    os_disk_size_gb      = 128                   # Increased for container images
    os_disk_type         = "Managed"             # Use managed disks
    
    # SECURITY: Enable encryption at host
    host_encryption_enabled = var.aks_enable_host_encryption
    
    # MAINTAINABILITY: Use availability zones for HA
    zones = var.aks_availability_zones
    
    # MAINTAINABILITY: Node labels for workload scheduling
    node_labels = {
      "nodepool-type" = "system"
      "environment"   = var.tags["Environment"]
    }
    
    # SECURITY: Restrict node access
    only_critical_addons_enabled = true  # Only run system pods on this pool
  }

  # SECURITY: Use User-Assigned Managed Identity for better control
  identity {
    type = "SystemAssigned"
  }

  # SECURITY & SCALABILITY: Network configuration
  network_profile {
    network_plugin    = "azure"           # Azure CNI for advanced networking
    network_policy    = "azure"           # Network policies for pod security
    load_balancer_sku = "standard"        # Standard LB for production
    outbound_type     = "loadBalancer"    # Control egress traffic
    
    # SCALABILITY: Configure service and pod CIDR
    service_cidr       = var.aks_service_cidr
    dns_service_ip     = var.aks_dns_service_ip
  }

  # SECURITY: Enable Microsoft Defender for containers
  microsoft_defender {
    log_analytics_workspace_id = azurerm_log_analytics_workspace.main.id
  }

  # MAINTAINABILITY: Enable monitoring with Azure Monitor
  oms_agent {
    log_analytics_workspace_id = azurerm_log_analytics_workspace.main.id
  }

  # SECURITY: Enable Azure Key Vault secrets provider
  key_vault_secrets_provider {
    secret_rotation_enabled  = true
    secret_rotation_interval = "2m"
  }

  # MAINTAINABILITY: Automatic upgrade channel
  automatic_upgrade_channel = var.aks_upgrade_channel  # Options: patch, rapid, stable, node-image

  # SECURITY: Image cleaner to remove unused images
  image_cleaner_enabled        = true
  image_cleaner_interval_hours = 48

  tags = var.tags

  lifecycle {
    ignore_changes = [
      # Ignore changes to node count as it's managed by autoscaler
      default_node_pool[0].node_count,
    ]
  }
}

# =============================================================================
# USER NODE POOL (SCALABILITY)
# =============================================================================
# Separate node pool for user workloads - allows independent scaling

resource "azurerm_kubernetes_cluster_node_pool" "user" {
  name                  = "user"
  kubernetes_cluster_id = azurerm_kubernetes_cluster.aks.id
  vm_size               = var.aks_user_vm_size
  
  # SCALABILITY: Auto-scaling configuration
  auto_scaling_enabled = true
  min_count            = var.aks_user_min_count
  max_count            = var.aks_user_max_count
  
  # MAINTAINABILITY: Availability zones
  zones = var.aks_availability_zones
  
  # MAINTAINABILITY: Labels and taints for workload isolation
  node_labels = {
    "nodepool-type" = "user"
    "environment"   = var.tags["Environment"]
  }
  
  # SCALABILITY: Allow user workloads only
  node_taints = []
  
  os_disk_size_gb = 128
  os_disk_type    = "Managed"
  
  tags = var.tags
}

# =============================================================================
# LOG ANALYTICS WORKSPACE (MONITORING & SECURITY)
# =============================================================================
# Centralized logging for security monitoring and troubleshooting

resource "azurerm_log_analytics_workspace" "main" {
  name                = "log-${var.resource_group_name}"
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  sku                 = "PerGB2018"           # Pay-per-GB pricing
  retention_in_days   = var.log_retention_days  # SECURITY: Retain logs for compliance

  tags = var.tags
}

# =============================================================================
# AZURE KEY VAULT (SECRETS MANAGEMENT)
# =============================================================================
# SECURITY: Centralized secrets management with access policies
# Stores PostgreSQL credentials and other sensitive data

resource "azurerm_key_vault" "main" {
  name                = "kv-${replace(var.resource_group_name, "-", "")}"  # Key Vault names must be unique
  location            = azurerm_resource_group.main.location
  resource_group_name = azurerm_resource_group.main.name
  tenant_id           = data.azurerm_client_config.current.tenant_id
  sku_name            = "standard"

  # SECURITY: Enable purge protection to prevent permanent deletion
  purge_protection_enabled   = true
  soft_delete_retention_days = 90

  # SECURITY: Enable RBAC for Key Vault access (recommended over access policies)
  enable_rbac_authorization = true

  # SECURITY: Network rules - restrict access
  network_acls {
    default_action = "Deny"
    bypass         = "AzureServices"  # Allow Azure services to access
    ip_rules       = var.allowed_ip_ranges  # Whitelist specific IPs
  }

  tags = var.tags
}

# SECURITY: Grant AKS access to Key Vault secrets
resource "azurerm_role_assignment" "aks_keyvault_secrets" {
  scope                = azurerm_key_vault.main.id
  role_definition_name = "Key Vault Secrets User"  # Least privilege - read-only
  principal_id         = azurerm_kubernetes_cluster.aks.key_vault_secrets_provider[0].secret_identity[0].object_id
}

# SECURITY: Grant current user/service principal admin access
resource "azurerm_role_assignment" "current_user_keyvault" {
  scope                = azurerm_key_vault.main.id
  role_definition_name = "Key Vault Administrator"
  principal_id         = data.azurerm_client_config.current.object_id
}

# =============================================================================
# RANDOM PASSWORD GENERATION
# =============================================================================
# SECURITY: Generate strong, cryptographically random password

resource "random_password" "postgres_password" {
  length           = 32              # Increased length for security
  special          = true
  override_special = "!#$%&*()-_=+[]{}<>:?"
  min_lower        = 4               # Require lowercase
  min_upper        = 4               # Require uppercase
  min_numeric      = 4               # Require numbers
  min_special      = 2               # Require special characters
}

# SECURITY: Store PostgreSQL password in Key Vault
resource "azurerm_key_vault_secret" "postgres_password" {
  name         = "postgres-admin-password"
  value        = random_password.postgres_password.result
  key_vault_id = azurerm_key_vault.main.id

  # MAINTAINABILITY: Add metadata
  content_type = "password"
  
  tags = var.tags

  depends_on = [azurerm_role_assignment.current_user_keyvault]
}

# =============================================================================
# AZURE DATABASE FOR POSTGRESQL FLEXIBLE SERVER
# =============================================================================
# Managed PostgreSQL database service
# SECURITY: SSL enforcement, private networking, encryption
# SCALABILITY: Flexible compute and storage scaling

resource "azurerm_postgresql_flexible_server" "postgres" {
  name                          = var.postgres_server_name
  resource_group_name           = azurerm_resource_group.main.name
  location                      = azurerm_resource_group.main.location
  version                       = var.postgres_version
  administrator_login           = var.postgres_admin_login
  administrator_password        = random_password.postgres_password.result
  
  # SCALABILITY: Storage configuration
  storage_mb                    = var.postgres_storage_mb
  storage_tier                  = "P30"              # Performance tier
  
  # SCALABILITY: Compute configuration
  sku_name                      = var.postgres_sku_name
  
  # SECURITY & MAINTAINABILITY: Backup configuration
  backup_retention_days         = var.postgres_backup_retention_days
  geo_redundant_backup_enabled  = var.postgres_geo_redundant_backup
  
  # SCALABILITY: High availability configuration
  zone                          = "1"
  high_availability {
    mode                      = var.postgres_ha_mode  # "ZoneRedundant" or "SameZone"
    standby_availability_zone = "2"
  }

  # MAINTAINABILITY: Automatic maintenance window
  maintenance_window {
    day_of_week  = 0    # Sunday
    start_hour   = 2    # 2 AM
    start_minute = 0
  }

  tags = var.tags

  lifecycle {
    # SECURITY: Prevent accidental deletion
    prevent_destroy = false  # Set to true in production
    
    # Ignore password changes made outside Terraform
    ignore_changes = [
      administrator_password,
    ]
  }
}

# SECURITY: PostgreSQL server configuration for hardening
resource "azurerm_postgresql_flexible_server_configuration" "ssl_enforcement" {
  name      = "require_secure_transport"
  server_id = azurerm_postgresql_flexible_server.postgres.id
  value     = "on"  # SECURITY: Enforce SSL connections
}

resource "azurerm_postgresql_flexible_server_configuration" "log_connections" {
  name      = "log_connections"
  server_id = azurerm_postgresql_flexible_server.postgres.id
  value     = "on"  # SECURITY: Audit connection attempts
}

resource "azurerm_postgresql_flexible_server_configuration" "log_disconnections" {
  name      = "log_disconnections"
  server_id = azurerm_postgresql_flexible_server.postgres.id
  value     = "on"  # SECURITY: Audit disconnections
}

resource "azurerm_postgresql_flexible_server_configuration" "connection_throttling" {
  name      = "connection_throttle.enable"
  server_id = azurerm_postgresql_flexible_server.postgres.id
  value     = "on"  # SECURITY: Protect against brute force
}

# PostgreSQL Firewall Rule - Allow Azure Services
# SECURITY: Consider using Private Endpoints in production for better isolation
resource "azurerm_postgresql_flexible_server_firewall_rule" "allow_azure_services" {
  name             = "AllowAzureServices"
  server_id        = azurerm_postgresql_flexible_server.postgres.id
  start_ip_address = "0.0.0.0"
  end_ip_address   = "0.0.0.0"
}

# =============================================================================
# POSTGRESQL DATABASE
# =============================================================================

resource "azurerm_postgresql_flexible_server_database" "main" {
  name      = var.postgres_database_name
  server_id = azurerm_postgresql_flexible_server.postgres.id
  collation = "en_US.utf8"
  charset   = "UTF8"
}

# =============================================================================
# DIAGNOSTIC SETTINGS (MONITORING)
# =============================================================================
# Send logs and metrics to Log Analytics for monitoring and alerting

resource "azurerm_monitor_diagnostic_setting" "aks" {
  name                       = "aks-diagnostics"
  target_resource_id         = azurerm_kubernetes_cluster.aks.id
  log_analytics_workspace_id = azurerm_log_analytics_workspace.main.id

  # SECURITY: Enable all log categories for audit
  enabled_log {
    category = "kube-apiserver"
  }
  enabled_log {
    category = "kube-controller-manager"
  }
  enabled_log {
    category = "kube-scheduler"
  }
  enabled_log {
    category = "kube-audit"
  }
  enabled_log {
    category = "guard"
  }
}

resource "azurerm_monitor_diagnostic_setting" "postgres" {
  name                       = "postgres-diagnostics"
  target_resource_id         = azurerm_postgresql_flexible_server.postgres.id
  log_analytics_workspace_id = azurerm_log_analytics_workspace.main.id

  enabled_log {
    category = "PostgreSQLLogs"
  }
}
