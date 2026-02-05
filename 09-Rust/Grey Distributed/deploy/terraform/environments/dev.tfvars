# =============================================================================
# Grey Distributed — Development Environment
# =============================================================================
#
# Cost-optimized configuration for local development and testing.
#
# Estimated Monthly Cost: ~$150-200
#   - EKS control plane: $73
#   - 3 t3.medium nodes: ~$75
#   - NAT Gateway (single): ~$32
#   - Storage (minimal): ~$20
#
# =============================================================================

environment = "dev"
region      = "us-east-1"

# Kubernetes
kubernetes_version = "1.29"

# Coordinators: Minimal for dev
coordinator_min_nodes     = 1
coordinator_max_nodes     = 3
coordinator_desired_nodes = 1
coordinator_instance_type = "t3.medium"

# Workers: Small pool
worker_min_nodes     = 1
worker_max_nodes     = 5
worker_desired_nodes = 2
worker_instance_type = "t3.medium"

# Network
vpc_cidr = "10.0.0.0/16"

# Storage: Baseline performance
storage_iops          = 3000
storage_throughput    = 125
backup_retention_days = 7

# Observability
enable_monitoring         = true
enable_tracing            = true
prometheus_retention_days = 7

# Federation: Disabled for dev
enable_federation = false

# Grey Distributed
grey_chart_version = "1.0.0"
