# =============================================================================
# TERRAFORM OUTPUTS
# =============================================================================
# Outputs for use in CI/CD pipelines, other Terraform modules, and operations
# SECURITY: Sensitive values are marked to prevent accidental exposure

# =============================================================================
# RESOURCE GROUP OUTPUTS
# =============================================================================

output "resource_group_name" {
  description = "Name of the resource group"
  value       = azurerm_resource_group.main.name
}

output "resource_group_location" {
  description = "Location of the resource group"
  value       = azurerm_resource_group.main.location
}

output "resource_group_id" {
  description = "ID of the resource group"
  value       = azurerm_resource_group.main.id
}

# =============================================================================
# AKS OUTPUTS
# =============================================================================

output "aks_cluster_name" {
  description = "Name of the AKS cluster"
  value       = azurerm_kubernetes_cluster.aks.name
}

output "aks_cluster_id" {
  description = "ID of the AKS cluster"
  value       = azurerm_kubernetes_cluster.aks.id
}

output "aks_kube_config" {
  description = "Kubernetes configuration for the AKS cluster (use: terraform output -raw aks_kube_config > ~/.kube/config)"
  value       = azurerm_kubernetes_cluster.aks.kube_config_raw
  sensitive   = true
}

output "aks_host" {
  description = "Kubernetes API server endpoint"
  value       = azurerm_kubernetes_cluster.aks.kube_config[0].host
  sensitive   = true
}

output "aks_client_certificate" {
  description = "Client certificate for AKS authentication"
  value       = azurerm_kubernetes_cluster.aks.kube_config[0].client_certificate
  sensitive   = true
}

output "aks_client_key" {
  description = "Client key for AKS authentication"
  value       = azurerm_kubernetes_cluster.aks.kube_config[0].client_key
  sensitive   = true
}

output "aks_cluster_ca_certificate" {
  description = "Cluster CA certificate for AKS"
  value       = azurerm_kubernetes_cluster.aks.kube_config[0].cluster_ca_certificate
  sensitive   = true
}

output "aks_node_resource_group" {
  description = "Resource group containing AKS node resources (managed by Azure)"
  value       = azurerm_kubernetes_cluster.aks.node_resource_group
}

output "aks_identity_principal_id" {
  description = "Principal ID of the AKS cluster managed identity"
  value       = azurerm_kubernetes_cluster.aks.identity[0].principal_id
}

output "aks_kubelet_identity_object_id" {
  description = "Object ID of the AKS kubelet managed identity"
  value       = azurerm_kubernetes_cluster.aks.kubelet_identity[0].object_id
}

output "aks_oidc_issuer_url" {
  description = "OIDC issuer URL for workload identity federation"
  value       = azurerm_kubernetes_cluster.aks.oidc_issuer_url
}

# =============================================================================
# KEY VAULT OUTPUTS
# =============================================================================

output "key_vault_name" {
  description = "Name of the Azure Key Vault"
  value       = azurerm_key_vault.main.name
}

output "key_vault_id" {
  description = "ID of the Azure Key Vault"
  value       = azurerm_key_vault.main.id
}

output "key_vault_uri" {
  description = "URI of the Azure Key Vault"
  value       = azurerm_key_vault.main.vault_uri
}

# =============================================================================
# LOG ANALYTICS OUTPUTS
# =============================================================================

output "log_analytics_workspace_id" {
  description = "ID of the Log Analytics workspace"
  value       = azurerm_log_analytics_workspace.main.id
}

output "log_analytics_workspace_name" {
  description = "Name of the Log Analytics workspace"
  value       = azurerm_log_analytics_workspace.main.name
}

# =============================================================================
# POSTGRESQL OUTPUTS
# =============================================================================

output "postgres_server_name" {
  description = "Name of the PostgreSQL Flexible Server"
  value       = azurerm_postgresql_flexible_server.postgres.name
}

output "postgres_server_fqdn" {
  description = "FQDN of the PostgreSQL Flexible Server"
  value       = azurerm_postgresql_flexible_server.postgres.fqdn
}

output "postgres_server_id" {
  description = "ID of the PostgreSQL Flexible Server"
  value       = azurerm_postgresql_flexible_server.postgres.id
}

output "postgres_database_name" {
  description = "Name of the PostgreSQL database"
  value       = azurerm_postgresql_flexible_server_database.main.name
}

# SECURITY: Sensitive outputs - retrieve from Key Vault in production
output "postgres_admin_login" {
  description = "Administrator login for PostgreSQL"
  value       = azurerm_postgresql_flexible_server.postgres.administrator_login
  sensitive   = true
}

# SECURITY: Password stored in Key Vault - use Key Vault reference instead
output "postgres_password_keyvault_secret_id" {
  description = "Key Vault secret ID for PostgreSQL admin password (use this instead of raw password)"
  value       = azurerm_key_vault_secret.postgres_password.id
}

output "postgres_connection_string" {
  description = "PostgreSQL connection string (SECURITY: Use Key Vault in production)"
  value       = "postgresql://${azurerm_postgresql_flexible_server.postgres.administrator_login}@${azurerm_postgresql_flexible_server.postgres.fqdn}:5432/${azurerm_postgresql_flexible_server_database.main.name}?sslmode=require"
  sensitive   = true
}

# =============================================================================
# DEPLOYMENT INSTRUCTIONS
# =============================================================================

output "deployment_instructions" {
  description = "Instructions for deploying to the AKS cluster"
  value       = <<-EOT
    # Get AKS credentials
    az aks get-credentials --resource-group ${azurerm_resource_group.main.name} --name ${azurerm_kubernetes_cluster.aks.name}
    
    # Deploy Kubernetes manifests
    kubectl apply -f kubernetes/
    
    # Get PostgreSQL password from Key Vault
    az keyvault secret show --name postgres-admin-password --vault-name ${azurerm_key_vault.main.name} --query value -o tsv
    
    # View Azure portal
    https://portal.azure.com/#@/resource${azurerm_resource_group.main.id}
  EOT
}
