# =============================================================================
# Grey Distributed — Terraform Variables
# =============================================================================
#
# This file defines all input variables for the Grey Distributed infrastructure.
# Override these values using .tfvars files for different environments.
#
# =============================================================================

# =============================================================================
# General Configuration
# =============================================================================

variable "environment" {
  description = "Deployment environment (dev, staging, production)"
  type        = string
  default     = "dev"

  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be dev, staging, or production."
  }
}

variable "region" {
  description = "Primary AWS region for deployment"
  type        = string
  default     = "us-east-1"
}

variable "secondary_region" {
  description = "Secondary AWS region for federation"
  type        = string
  default     = "us-west-2"
}

# =============================================================================
# Networking Configuration
# =============================================================================

variable "vpc_cidr" {
  description = "CIDR block for the VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "secondary_vpc_cidr" {
  description = "CIDR block for secondary region VPC"
  type        = string
  default     = "10.1.0.0/16"
}

# =============================================================================
# Kubernetes Configuration
# =============================================================================

variable "kubernetes_version" {
  description = "Kubernetes version for EKS cluster"
  type        = string
  default     = "1.29"
}

# =============================================================================
# Coordinator Node Configuration
# =============================================================================
# Coordinators run the Raft consensus and should be on dedicated, stable nodes.
# Use odd numbers (3, 5, 7) for fault tolerance.

variable "coordinator_min_nodes" {
  description = "Minimum number of coordinator nodes"
  type        = number
  default     = 3
}

variable "coordinator_max_nodes" {
  description = "Maximum number of coordinator nodes"
  type        = number
  default     = 5
}

variable "coordinator_desired_nodes" {
  description = "Desired number of coordinator nodes"
  type        = number
  default     = 3
}

variable "coordinator_instance_type" {
  description = "EC2 instance type for coordinator nodes"
  type        = string
  default     = "m6i.xlarge"
  
  # Recommendation:
  # - Dev: t3.medium (2 CPU, 4GB RAM)
  # - Staging: m6i.large (2 CPU, 8GB RAM)
  # - Production: m6i.xlarge (4 CPU, 16GB RAM)
}

# =============================================================================
# Worker Node Configuration
# =============================================================================
# Workers execute scheduled tasks and scale based on workload.

variable "worker_min_nodes" {
  description = "Minimum number of worker nodes"
  type        = number
  default     = 2
}

variable "worker_max_nodes" {
  description = "Maximum number of worker nodes"
  type        = number
  default     = 20
}

variable "worker_desired_nodes" {
  description = "Desired number of worker nodes"
  type        = number
  default     = 3
}

variable "worker_instance_type" {
  description = "EC2 instance type for worker nodes"
  type        = string
  default     = "m6i.large"
  
  # Recommendation:
  # - Dev: t3.medium (2 CPU, 4GB RAM)
  # - Staging: m6i.large (2 CPU, 8GB RAM)
  # - Production: m6i.xlarge or c6i.xlarge for compute-intensive
}

# =============================================================================
# Storage Configuration
# =============================================================================

variable "storage_iops" {
  description = "IOPS for EBS volumes (gp3)"
  type        = number
  default     = 3000
  
  # Recommendation:
  # - Dev: 3000 (baseline)
  # - Staging: 5000
  # - Production: 10000+ for high-throughput consensus
}

variable "storage_throughput" {
  description = "Throughput (MB/s) for EBS volumes (gp3)"
  type        = number
  default     = 125
  
  # Recommendation:
  # - Dev: 125 (baseline)
  # - Staging: 250
  # - Production: 500+ for high-throughput
}

variable "backup_retention_days" {
  description = "Number of days to retain backups in S3"
  type        = number
  default     = 30
}

# =============================================================================
# Grey Distributed Configuration
# =============================================================================

variable "helm_repository" {
  description = "Helm repository URL for Grey chart"
  type        = string
  default     = "oci://ghcr.io/grey-systems/charts"
}

variable "grey_chart_version" {
  description = "Version of the Grey Helm chart"
  type        = string
  default     = "1.0.0"
}

variable "attestation_key" {
  description = "Attestation key for node identity (sensitive)"
  type        = string
  sensitive   = true
  default     = ""
}

# =============================================================================
# Observability Configuration
# =============================================================================

variable "enable_monitoring" {
  description = "Enable Prometheus + Grafana monitoring stack"
  type        = bool
  default     = true
}

variable "enable_tracing" {
  description = "Enable distributed tracing (Jaeger/OTLP)"
  type        = bool
  default     = true
}

variable "prometheus_retention_days" {
  description = "Days to retain Prometheus metrics"
  type        = number
  default     = 15
}

variable "slack_webhook_url" {
  description = "Slack webhook URL for alerts"
  type        = string
  sensitive   = true
  default     = ""
}

# =============================================================================
# Federation Configuration
# =============================================================================

variable "enable_federation" {
  description = "Enable multi-region federation"
  type        = bool
  default     = false
}

# =============================================================================
# Edge Configuration
# =============================================================================

variable "enable_edge_nodes" {
  description = "Enable edge node deployment"
  type        = bool
  default     = false
}

variable "edge_regions" {
  description = "List of edge regions to deploy to"
  type        = list(string)
  default     = []
}
