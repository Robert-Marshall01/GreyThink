# =============================================================================
# Grey Distributed — Terraform Infrastructure
# =============================================================================
#
# This is the root module for provisioning Grey Distributed infrastructure.
# Supports AWS, GCP, and Azure with multi-region federation.
#
# Architecture:
#   ┌─────────────────────────────────────────────────────────────────────────┐
#   │                         Multi-Region Federation                          │
#   ├─────────────────────────────────────────────────────────────────────────┤
#   │  Region A (Primary)    │  Region B (Secondary)   │  Region C (Edge)     │
#   │  ┌─────────────────┐   │  ┌─────────────────┐    │  ┌─────────────────┐ │
#   │  │ Coordinator (3) │◄──┼──│ Coordinator (3) │◄───┼──│ Edge Nodes      │ │
#   │  │ Workers (auto)  │   │  │ Workers (auto)  │    │  │ (DaemonSet)     │ │
#   │  │ Storage         │   │  │ Storage         │    │  │ Local Cache     │ │
#   │  └─────────────────┘   │  └─────────────────┘    │  └─────────────────┘ │
#   └─────────────────────────────────────────────────────────────────────────┘
#
# Usage:
#   terraform init
#   terraform plan -var-file=environments/production.tfvars
#   terraform apply -var-file=environments/production.tfvars
#
# =============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 3.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.25"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.12"
    }
  }

  # Remote state backend (uncomment for production)
  # backend "s3" {
  #   bucket         = "grey-terraform-state"
  #   key            = "grey-distributed/terraform.tfstate"
  #   region         = "us-east-1"
  #   encrypt        = true
  #   dynamodb_table = "grey-terraform-locks"
  # }
}

# =============================================================================
# Local Values
# =============================================================================

locals {
  # Common tags applied to all resources
  common_tags = {
    Project     = "grey-distributed"
    Environment = var.environment
    ManagedBy   = "terraform"
    Repository  = "github.com/grey-systems/grey-distributed"
  }

  # Cluster naming convention
  cluster_name = "grey-${var.environment}-${var.region}"

  # Node pool configurations
  coordinator_node_config = {
    min_size     = var.coordinator_min_nodes
    max_size     = var.coordinator_max_nodes
    desired_size = var.coordinator_desired_nodes
    instance_type = var.coordinator_instance_type
  }

  worker_node_config = {
    min_size     = var.worker_min_nodes
    max_size     = var.worker_max_nodes
    desired_size = var.worker_desired_nodes
    instance_type = var.worker_instance_type
  }
}

# =============================================================================
# Provider Configuration
# =============================================================================

# AWS Provider (primary)
provider "aws" {
  region = var.region

  default_tags {
    tags = local.common_tags
  }
}

# AWS Provider for secondary region (federation)
provider "aws" {
  alias  = "secondary"
  region = var.secondary_region

  default_tags {
    tags = local.common_tags
  }
}

# Kubernetes provider (configured after cluster creation)
provider "kubernetes" {
  host                   = module.eks.cluster_endpoint
  cluster_ca_certificate = base64decode(module.eks.cluster_ca_certificate)

  exec {
    api_version = "client.authentication.k8s.io/v1beta1"
    command     = "aws"
    args        = ["eks", "get-token", "--cluster-name", local.cluster_name]
  }
}

# Helm provider
provider "helm" {
  kubernetes {
    host                   = module.eks.cluster_endpoint
    cluster_ca_certificate = base64decode(module.eks.cluster_ca_certificate)

    exec {
      api_version = "client.authentication.k8s.io/v1beta1"
      command     = "aws"
      args        = ["eks", "get-token", "--cluster-name", local.cluster_name]
    }
  }
}

# =============================================================================
# Data Sources
# =============================================================================

# Get available AZs in the region
data "aws_availability_zones" "available" {
  state = "available"
}

# Get AWS account ID
data "aws_caller_identity" "current" {}

# Get current AWS region
data "aws_region" "current" {}

# =============================================================================
# Networking Module
# =============================================================================

module "vpc" {
  source = "./modules/networking"

  environment    = var.environment
  region         = var.region
  vpc_cidr       = var.vpc_cidr
  cluster_name   = local.cluster_name
  
  # Use 3 AZs for high availability
  azs = slice(data.aws_availability_zones.available.names, 0, 3)

  # Enable VPC endpoints for private access
  enable_nat_gateway   = true
  single_nat_gateway   = var.environment != "production"
  enable_vpn_gateway   = false
  enable_vpc_endpoints = true

  tags = local.common_tags
}

# =============================================================================
# EKS Cluster Module
# =============================================================================

module "eks" {
  source = "./modules/eks"

  cluster_name    = local.cluster_name
  cluster_version = var.kubernetes_version
  
  vpc_id          = module.vpc.vpc_id
  subnet_ids      = module.vpc.private_subnet_ids

  # Coordinator node group (dedicated for consensus)
  coordinator_node_group = {
    name           = "coordinators"
    instance_types = [local.coordinator_node_config.instance_type]
    min_size       = local.coordinator_node_config.min_size
    max_size       = local.coordinator_node_config.max_size
    desired_size   = local.coordinator_node_config.desired_size

    labels = {
      "grey.io/role" = "coordinator"
      "grey.io/consensus" = "true"
    }

    taints = [{
      key    = "grey.io/coordinator"
      value  = "true"
      effect = "NO_SCHEDULE"
    }]
  }

  # Worker node group (auto-scaling for tasks)
  worker_node_group = {
    name           = "workers"
    instance_types = [local.worker_node_config.instance_type]
    min_size       = local.worker_node_config.min_size
    max_size       = local.worker_node_config.max_size
    desired_size   = local.worker_node_config.desired_size

    labels = {
      "grey.io/role" = "worker"
    }
  }

  # Enable IRSA for pod-level IAM
  enable_irsa = true

  # Cluster add-ons
  cluster_addons = {
    coredns = {
      most_recent = true
    }
    kube-proxy = {
      most_recent = true
    }
    vpc-cni = {
      most_recent = true
    }
    aws-ebs-csi-driver = {
      most_recent = true
    }
  }

  tags = local.common_tags
}

# =============================================================================
# Storage Module
# =============================================================================

module "storage" {
  source = "./modules/storage"

  environment  = var.environment
  cluster_name = local.cluster_name
  
  # EBS storage class for persistent volumes
  storage_class_name = "grey-ssd"
  volume_type        = "gp3"
  iops               = var.storage_iops
  throughput         = var.storage_throughput

  # S3 bucket for snapshots and backups
  enable_s3_backup   = true
  backup_bucket_name = "grey-backups-${data.aws_caller_identity.current.account_id}-${var.region}"
  backup_retention_days = var.backup_retention_days

  tags = local.common_tags
}

# =============================================================================
# Security Module
# =============================================================================

module "security" {
  source = "./modules/security"

  environment  = var.environment
  cluster_name = local.cluster_name
  
  # KMS key for encryption
  enable_kms_encryption = true
  kms_key_alias        = "grey-${var.environment}"

  # IAM roles for Grey Distributed
  eks_cluster_role_arn     = module.eks.cluster_role_arn
  eks_node_role_arn        = module.eks.node_role_arn
  
  # Pod identity for attestation
  enable_pod_identity = true
  oidc_provider_arn   = module.eks.oidc_provider_arn
  oidc_provider_url   = module.eks.oidc_provider_url

  # Secrets Manager for node identity
  enable_secrets_manager = true

  tags = local.common_tags
}

# =============================================================================
# Grey Distributed Helm Deployment
# =============================================================================

resource "helm_release" "grey_distributed" {
  name       = "grey"
  namespace  = "grey-system"
  repository = var.helm_repository
  chart      = "grey"
  version    = var.grey_chart_version

  create_namespace = true

  # Wait for deployment to be ready
  wait    = true
  timeout = 600

  # Values file based on environment
  values = [
    templatefile("${path.module}/helm-values/${var.environment}.yaml", {
      cluster_name           = local.cluster_name
      region                 = var.region
      storage_class          = module.storage.storage_class_name
      kms_key_arn            = module.security.kms_key_arn
      backup_bucket          = module.storage.backup_bucket_name
      coordinator_replicas   = var.coordinator_desired_nodes
      worker_min_replicas    = var.worker_min_nodes
      worker_max_replicas    = var.worker_max_nodes
      enable_monitoring      = var.enable_monitoring
      enable_tracing         = var.enable_tracing
    })
  ]

  # Set sensitive values from secrets
  set_sensitive {
    name  = "security.attestationKey"
    value = var.attestation_key
  }

  depends_on = [
    module.eks,
    module.storage,
    module.security
  ]
}

# =============================================================================
# Monitoring Stack (Optional)
# =============================================================================

module "monitoring" {
  source = "./modules/monitoring"
  count  = var.enable_monitoring ? 1 : 0

  environment  = var.environment
  cluster_name = local.cluster_name
  namespace    = "monitoring"

  # Prometheus + Grafana
  enable_prometheus = true
  enable_grafana    = true
  
  # Alertmanager
  enable_alertmanager = true
  slack_webhook_url   = var.slack_webhook_url

  # Retention
  prometheus_retention_days = var.prometheus_retention_days

  depends_on = [module.eks]
}

# =============================================================================
# Federation (Multi-Region)
# =============================================================================

module "federation" {
  source = "./modules/federation"
  count  = var.enable_federation ? 1 : 0

  providers = {
    aws.primary   = aws
    aws.secondary = aws.secondary
  }

  primary_cluster_name   = local.cluster_name
  secondary_cluster_name = "grey-${var.environment}-${var.secondary_region}"
  
  primary_vpc_id     = module.vpc.vpc_id
  primary_vpc_cidr   = var.vpc_cidr
  secondary_vpc_cidr = var.secondary_vpc_cidr

  # VPC Peering for cross-region communication
  enable_vpc_peering = true
  
  # Transit Gateway for complex topologies
  enable_transit_gateway = var.environment == "production"

  tags = local.common_tags
}

# =============================================================================
# Outputs
# =============================================================================

output "cluster_name" {
  description = "Name of the EKS cluster"
  value       = local.cluster_name
}

output "cluster_endpoint" {
  description = "EKS cluster API endpoint"
  value       = module.eks.cluster_endpoint
}

output "cluster_ca_certificate" {
  description = "EKS cluster CA certificate"
  value       = module.eks.cluster_ca_certificate
  sensitive   = true
}

output "vpc_id" {
  description = "VPC ID"
  value       = module.vpc.vpc_id
}

output "backup_bucket" {
  description = "S3 bucket for backups"
  value       = module.storage.backup_bucket_name
}

output "kms_key_arn" {
  description = "KMS key ARN for encryption"
  value       = module.security.kms_key_arn
}

output "kubeconfig_command" {
  description = "Command to configure kubectl"
  value       = "aws eks update-kubeconfig --name ${local.cluster_name} --region ${var.region}"
}
