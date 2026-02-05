# =============================================================================
# Grey Distributed — EKS Module
# =============================================================================
#
# Provisions an EKS cluster with dedicated node groups for coordinators and workers.
#
# Cluster Architecture:
#   ┌─────────────────────────────────────────────────────────────────────────┐
#   │                           EKS Control Plane                             │
#   │                        (Managed by AWS, Multi-AZ)                        │
#   └─────────────────────────────────────────────────────────────────────────┘
#                                      │
#          ┌───────────────────────────┼───────────────────────────┐
#          ▼                           ▼                           ▼
#   ┌─────────────────┐    ┌─────────────────────────┐    ┌─────────────────┐
#   │  Coordinator    │    │    Worker Node Group     │    │  System Pods    │
#   │  Node Group     │    │    (Auto-Scaling)        │    │  (CoreDNS, etc) │
#   │  ───────────────│    │    ─────────────────────│    │  ─────────────  │
#   │  • Dedicated    │    │    • Scales 2-20 nodes   │    │  • Runs on      │
#   │  • No preemption│    │    • Spot instances OK   │    │    workers      │
#   │  • Raft consensus│   │    • Task execution      │    │                 │
#   └─────────────────┘    └─────────────────────────┘    └─────────────────┘
#
# Why separate node groups:
#   - Coordinators need stable, dedicated nodes for consensus
#   - Workers can use cheaper spot instances and auto-scale
#   - Taints prevent workloads from landing on wrong node type
#
# Tradeoffs:
#   - Dedicated coordinators increase minimum cost
#   - IRSA adds complexity but improves security
#   - Managed node groups limit customization vs self-managed
#
# =============================================================================

variable "cluster_name" {
  type = string
}

variable "cluster_version" {
  type = string
}

variable "vpc_id" {
  type = string
}

variable "subnet_ids" {
  type = list(string)
}

variable "coordinator_node_group" {
  type = object({
    name           = string
    instance_types = list(string)
    min_size       = number
    max_size       = number
    desired_size   = number
    labels         = map(string)
    taints         = list(object({
      key    = string
      value  = string
      effect = string
    }))
  })
}

variable "worker_node_group" {
  type = object({
    name           = string
    instance_types = list(string)
    min_size       = number
    max_size       = number
    desired_size   = number
    labels         = map(string)
  })
}

variable "enable_irsa" {
  type    = bool
  default = true
}

variable "cluster_addons" {
  type    = map(any)
  default = {}
}

variable "tags" {
  type    = map(string)
  default = {}
}

# =============================================================================
# IAM Roles
# =============================================================================

# EKS Cluster Role
resource "aws_iam_role" "cluster" {
  name = "${var.cluster_name}-cluster-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "eks.amazonaws.com"
      }
    }]
  })

  tags = var.tags
}

resource "aws_iam_role_policy_attachment" "cluster_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSClusterPolicy"
  role       = aws_iam_role.cluster.name
}

resource "aws_iam_role_policy_attachment" "cluster_vpc_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSVPCResourceController"
  role       = aws_iam_role.cluster.name
}

# Node Role
resource "aws_iam_role" "node" {
  name = "${var.cluster_name}-node-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "ec2.amazonaws.com"
      }
    }]
  })

  tags = var.tags
}

resource "aws_iam_role_policy_attachment" "node_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy"
  role       = aws_iam_role.node.name
}

resource "aws_iam_role_policy_attachment" "node_cni_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy"
  role       = aws_iam_role.node.name
}

resource "aws_iam_role_policy_attachment" "node_ecr_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly"
  role       = aws_iam_role.node.name
}

resource "aws_iam_role_policy_attachment" "node_ssm_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
  role       = aws_iam_role.node.name
}

# =============================================================================
# Security Groups
# =============================================================================

# Cluster Security Group
resource "aws_security_group" "cluster" {
  name_prefix = "${var.cluster_name}-cluster-"
  vpc_id      = var.vpc_id

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-cluster-sg"
  })

  lifecycle {
    create_before_destroy = true
  }
}

# Allow all egress
resource "aws_security_group_rule" "cluster_egress" {
  type              = "egress"
  from_port         = 0
  to_port           = 0
  protocol          = "-1"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.cluster.id
}

# Node Security Group
resource "aws_security_group" "node" {
  name_prefix = "${var.cluster_name}-node-"
  vpc_id      = var.vpc_id

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-node-sg"
    "kubernetes.io/cluster/${var.cluster_name}" = "owned"
  })

  lifecycle {
    create_before_destroy = true
  }
}

# Node-to-node communication
resource "aws_security_group_rule" "node_internal" {
  type                     = "ingress"
  from_port                = 0
  to_port                  = 65535
  protocol                 = "-1"
  source_security_group_id = aws_security_group.node.id
  security_group_id        = aws_security_group.node.id
}

# Cluster to nodes
resource "aws_security_group_rule" "cluster_to_node" {
  type                     = "ingress"
  from_port                = 1025
  to_port                  = 65535
  protocol                 = "tcp"
  source_security_group_id = aws_security_group.cluster.id
  security_group_id        = aws_security_group.node.id
}

# Node to cluster API
resource "aws_security_group_rule" "node_to_cluster" {
  type                     = "ingress"
  from_port                = 443
  to_port                  = 443
  protocol                 = "tcp"
  source_security_group_id = aws_security_group.node.id
  security_group_id        = aws_security_group.cluster.id
}

# Node egress
resource "aws_security_group_rule" "node_egress" {
  type              = "egress"
  from_port         = 0
  to_port           = 0
  protocol          = "-1"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = aws_security_group.node.id
}

# =============================================================================
# EKS Cluster
# =============================================================================

resource "aws_eks_cluster" "main" {
  name     = var.cluster_name
  version  = var.cluster_version
  role_arn = aws_iam_role.cluster.arn

  vpc_config {
    subnet_ids              = var.subnet_ids
    security_group_ids      = [aws_security_group.cluster.id]
    endpoint_private_access = true
    endpoint_public_access  = true  # Set to false for private clusters
  }

  # Enable secrets encryption
  encryption_config {
    provider {
      key_arn = aws_kms_key.cluster.arn
    }
    resources = ["secrets"]
  }

  # Enable control plane logging
  enabled_cluster_log_types = [
    "api",
    "audit",
    "authenticator",
    "controllerManager",
    "scheduler"
  ]

  tags = var.tags

  depends_on = [
    aws_iam_role_policy_attachment.cluster_policy,
    aws_iam_role_policy_attachment.cluster_vpc_policy
  ]
}

# KMS key for secrets encryption
resource "aws_kms_key" "cluster" {
  description             = "EKS cluster secrets encryption key for ${var.cluster_name}"
  deletion_window_in_days = 7
  enable_key_rotation     = true

  tags = var.tags
}

# =============================================================================
# OIDC Provider (for IRSA)
# =============================================================================

data "tls_certificate" "cluster" {
  count = var.enable_irsa ? 1 : 0
  url   = aws_eks_cluster.main.identity[0].oidc[0].issuer
}

resource "aws_iam_openid_connect_provider" "cluster" {
  count = var.enable_irsa ? 1 : 0

  client_id_list  = ["sts.amazonaws.com"]
  thumbprint_list = [data.tls_certificate.cluster[0].certificates[0].sha1_fingerprint]
  url             = aws_eks_cluster.main.identity[0].oidc[0].issuer

  tags = var.tags
}

# =============================================================================
# Node Groups
# =============================================================================

# Coordinator Node Group
# Dedicated nodes for Raft consensus - should not be preempted
resource "aws_eks_node_group" "coordinators" {
  cluster_name    = aws_eks_cluster.main.name
  node_group_name = var.coordinator_node_group.name
  node_role_arn   = aws_iam_role.node.arn
  subnet_ids      = var.subnet_ids

  capacity_type   = "ON_DEMAND"  # Never use spot for coordinators
  instance_types  = var.coordinator_node_group.instance_types

  scaling_config {
    min_size     = var.coordinator_node_group.min_size
    max_size     = var.coordinator_node_group.max_size
    desired_size = var.coordinator_node_group.desired_size
  }

  update_config {
    max_unavailable = 1  # Rolling update, one at a time for consensus safety
  }

  labels = var.coordinator_node_group.labels

  dynamic "taint" {
    for_each = var.coordinator_node_group.taints
    content {
      key    = taint.value.key
      value  = taint.value.value
      effect = taint.value.effect
    }
  }

  tags = merge(var.tags, {
    "grey.io/node-group" = "coordinators"
  })

  depends_on = [
    aws_iam_role_policy_attachment.node_policy,
    aws_iam_role_policy_attachment.node_cni_policy,
    aws_iam_role_policy_attachment.node_ecr_policy
  ]
}

# Worker Node Group
# Auto-scaling nodes for task execution
resource "aws_eks_node_group" "workers" {
  cluster_name    = aws_eks_cluster.main.name
  node_group_name = var.worker_node_group.name
  node_role_arn   = aws_iam_role.node.arn
  subnet_ids      = var.subnet_ids

  capacity_type   = "ON_DEMAND"  # Can switch to SPOT for cost savings
  instance_types  = var.worker_node_group.instance_types

  scaling_config {
    min_size     = var.worker_node_group.min_size
    max_size     = var.worker_node_group.max_size
    desired_size = var.worker_node_group.desired_size
  }

  update_config {
    max_unavailable_percentage = 25  # Faster rollout for workers
  }

  labels = var.worker_node_group.labels

  tags = merge(var.tags, {
    "grey.io/node-group" = "workers"
  })

  depends_on = [
    aws_iam_role_policy_attachment.node_policy,
    aws_iam_role_policy_attachment.node_cni_policy,
    aws_iam_role_policy_attachment.node_ecr_policy
  ]
}

# =============================================================================
# Cluster Add-ons
# =============================================================================

resource "aws_eks_addon" "addons" {
  for_each = var.cluster_addons

  cluster_name = aws_eks_cluster.main.name
  addon_name   = each.key

  resolve_conflicts_on_create = "OVERWRITE"
  resolve_conflicts_on_update = "OVERWRITE"

  tags = var.tags

  depends_on = [
    aws_eks_node_group.coordinators,
    aws_eks_node_group.workers
  ]
}

# =============================================================================
# Outputs
# =============================================================================

output "cluster_endpoint" {
  description = "EKS cluster API endpoint"
  value       = aws_eks_cluster.main.endpoint
}

output "cluster_ca_certificate" {
  description = "EKS cluster CA certificate"
  value       = aws_eks_cluster.main.certificate_authority[0].data
}

output "cluster_name" {
  description = "EKS cluster name"
  value       = aws_eks_cluster.main.name
}

output "cluster_role_arn" {
  description = "EKS cluster IAM role ARN"
  value       = aws_iam_role.cluster.arn
}

output "node_role_arn" {
  description = "EKS node IAM role ARN"
  value       = aws_iam_role.node.arn
}

output "oidc_provider_arn" {
  description = "OIDC provider ARN for IRSA"
  value       = var.enable_irsa ? aws_iam_openid_connect_provider.cluster[0].arn : null
}

output "oidc_provider_url" {
  description = "OIDC provider URL"
  value       = aws_eks_cluster.main.identity[0].oidc[0].issuer
}
