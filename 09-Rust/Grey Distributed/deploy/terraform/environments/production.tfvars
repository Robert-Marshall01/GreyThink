# =============================================================================
# Grey Distributed — Production Environment
# =============================================================================
#
# Production-grade configuration with high availability and federation.
#
# Estimated Monthly Cost: ~$2,500-4,000
#   - EKS control plane: $73
#   - 5 m6i.xlarge coordinators: ~$700
#   - 5-20 m6i.large workers: ~$350-1400
#   - Multi-AZ NAT Gateways: ~$100
#   - Storage (high IOPS): ~$200
#   - Federation (secondary region): ~$800
#   - Monitoring/Observability: ~$150
#
# Reliability Targets:
#   - Availability: 99.99%
#   - RPO: 1 minute
#   - RTO: 5 minutes
#
# =============================================================================

environment      = "production"
region           = "us-east-1"
secondary_region = "us-west-2"

# Kubernetes
kubernetes_version = "1.29"

# Coordinators: 5 nodes for quorum (tolerates 2 failures)
coordinator_min_nodes     = 5
coordinator_max_nodes     = 7
coordinator_desired_nodes = 5
coordinator_instance_type = "m6i.xlarge"  # 4 vCPU, 16 GB RAM

# Workers: Auto-scaling pool
worker_min_nodes     = 5
worker_max_nodes     = 50
worker_desired_nodes = 10
worker_instance_type = "m6i.large"  # 2 vCPU, 8 GB RAM

# Network: Multi-AZ high availability
vpc_cidr           = "10.0.0.0/16"
secondary_vpc_cidr = "10.1.0.0/16"

# Storage: High performance
storage_iops          = 10000
storage_throughput    = 500
backup_retention_days = 90

# Observability: Full stack with long retention
enable_monitoring         = true
enable_tracing            = true
prometheus_retention_days = 30

# Federation: Multi-region active-passive
enable_federation = true

# Grey Distributed
grey_chart_version = "1.0.0"

# Edge nodes (optional)
enable_edge_nodes = true
edge_regions = [
  "eu-west-1",
  "ap-southeast-1",
  "sa-east-1"
]
