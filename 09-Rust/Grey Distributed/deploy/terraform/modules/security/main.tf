# =============================================================================
# Grey Distributed — Security Module
# =============================================================================
#
# Provisions security resources for Grey Distributed:
# - KMS keys for encryption at rest
# - IAM roles for pod identity (IRSA)
# - Secrets Manager for node attestation keys
# - Security policies for Grey components
#
# Security Model:
#   ┌─────────────────────────────────────────────────────────────────────────┐
#   │                         Security Layers                                  │
#   ├─────────────────────────────────────────────────────────────────────────┤
#   │  Layer 1: Network Isolation                                             │
#   │  • VPC private subnets                                                  │
#   │  • Security groups with least-privilege                                 │
#   │  • Network policies in Kubernetes                                       │
#   ├─────────────────────────────────────────────────────────────────────────┤
#   │  Layer 2: Identity & Access                                             │
#   │  • IRSA for pod-level IAM roles                                         │
#   │  • Node attestation via Secrets Manager                                 │
#   │  • RBAC for Kubernetes resources                                        │
#   ├─────────────────────────────────────────────────────────────────────────┤
#   │  Layer 3: Encryption                                                    │
#   │  • KMS for EBS volume encryption                                        │
#   │  • KMS for Kubernetes secrets                                           │
#   │  • TLS for all service communication                                    │
#   └─────────────────────────────────────────────────────────────────────────┘
#
# Integration Points:
#   - Grey Optimizer: IAM role for resource metering
#   - GreyAV: IAM role for anomaly detection logs
#   - Grey Multi-Tenant: Namespace isolation policies
#
# =============================================================================

variable "environment" {
  type = string
}

variable "cluster_name" {
  type = string
}

variable "enable_kms_encryption" {
  type    = bool
  default = true
}

variable "kms_key_alias" {
  type = string
}

variable "eks_cluster_role_arn" {
  type = string
}

variable "eks_node_role_arn" {
  type = string
}

variable "enable_pod_identity" {
  type    = bool
  default = true
}

variable "oidc_provider_arn" {
  type    = string
  default = null
}

variable "oidc_provider_url" {
  type    = string
  default = null
}

variable "enable_secrets_manager" {
  type    = bool
  default = true
}

variable "tags" {
  type    = map(string)
  default = {}
}

# =============================================================================
# KMS Key for Encryption
# =============================================================================

resource "aws_kms_key" "grey" {
  count = var.enable_kms_encryption ? 1 : 0

  description             = "Grey Distributed encryption key for ${var.environment}"
  deletion_window_in_days = 30
  enable_key_rotation     = true
  multi_region            = var.environment == "production"

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "Enable IAM User Permissions"
        Effect = "Allow"
        Principal = {
          AWS = "arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"
        }
        Action   = "kms:*"
        Resource = "*"
      },
      {
        Sid    = "Allow EKS to use the key"
        Effect = "Allow"
        Principal = {
          AWS = var.eks_cluster_role_arn
        }
        Action = [
          "kms:Encrypt",
          "kms:Decrypt",
          "kms:ReEncrypt*",
          "kms:GenerateDataKey*",
          "kms:DescribeKey"
        ]
        Resource = "*"
      },
      {
        Sid    = "Allow Nodes to use the key"
        Effect = "Allow"
        Principal = {
          AWS = var.eks_node_role_arn
        }
        Action = [
          "kms:Decrypt",
          "kms:DescribeKey"
        ]
        Resource = "*"
      }
    ]
  })

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-kms"
  })
}

resource "aws_kms_alias" "grey" {
  count = var.enable_kms_encryption ? 1 : 0

  name          = "alias/${var.kms_key_alias}"
  target_key_id = aws_kms_key.grey[0].key_id
}

data "aws_caller_identity" "current" {}

# =============================================================================
# IAM Role for Grey Coordinators (IRSA)
# =============================================================================

resource "aws_iam_role" "grey_coordinator" {
  count = var.enable_pod_identity && var.oidc_provider_arn != null ? 1 : 0

  name = "${var.cluster_name}-coordinator-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Principal = {
        Federated = var.oidc_provider_arn
      }
      Action = "sts:AssumeRoleWithWebIdentity"
      Condition = {
        StringEquals = {
          "${replace(var.oidc_provider_url, "https://", "")}:sub" = "system:serviceaccount:grey-system:grey-coordinator"
          "${replace(var.oidc_provider_url, "https://", "")}:aud" = "sts.amazonaws.com"
        }
      }
    }]
  })

  tags = var.tags
}

# Coordinator policy: Access to KMS, S3 backups, Secrets Manager
resource "aws_iam_role_policy" "grey_coordinator" {
  count = var.enable_pod_identity && var.oidc_provider_arn != null ? 1 : 0

  name = "grey-coordinator-policy"
  role = aws_iam_role.grey_coordinator[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "KMSAccess"
        Effect = "Allow"
        Action = [
          "kms:Decrypt",
          "kms:Encrypt",
          "kms:GenerateDataKey"
        ]
        Resource = var.enable_kms_encryption ? aws_kms_key.grey[0].arn : "*"
      },
      {
        Sid    = "S3BackupAccess"
        Effect = "Allow"
        Action = [
          "s3:PutObject",
          "s3:GetObject",
          "s3:ListBucket"
        ]
        Resource = [
          "arn:aws:s3:::grey-backups-*",
          "arn:aws:s3:::grey-backups-*/*"
        ]
      },
      {
        Sid    = "SecretsManagerAccess"
        Effect = "Allow"
        Action = [
          "secretsmanager:GetSecretValue"
        ]
        Resource = "arn:aws:secretsmanager:*:*:secret:grey/${var.environment}/*"
      }
    ]
  })
}

# =============================================================================
# IAM Role for Grey Workers (IRSA)
# =============================================================================

resource "aws_iam_role" "grey_worker" {
  count = var.enable_pod_identity && var.oidc_provider_arn != null ? 1 : 0

  name = "${var.cluster_name}-worker-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Principal = {
        Federated = var.oidc_provider_arn
      }
      Action = "sts:AssumeRoleWithWebIdentity"
      Condition = {
        StringEquals = {
          "${replace(var.oidc_provider_url, "https://", "")}:sub" = "system:serviceaccount:grey-system:grey-worker"
          "${replace(var.oidc_provider_url, "https://", "")}:aud" = "sts.amazonaws.com"
        }
      }
    }]
  })

  tags = var.tags
}

# Worker policy: Minimal access, task-specific
resource "aws_iam_role_policy" "grey_worker" {
  count = var.enable_pod_identity && var.oidc_provider_arn != null ? 1 : 0

  name = "grey-worker-policy"
  role = aws_iam_role.grey_worker[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "KMSDecrypt"
        Effect = "Allow"
        Action = [
          "kms:Decrypt"
        ]
        Resource = var.enable_kms_encryption ? aws_kms_key.grey[0].arn : "*"
      },
      {
        Sid    = "CloudWatchLogs"
        Effect = "Allow"
        Action = [
          "logs:CreateLogStream",
          "logs:PutLogEvents"
        ]
        Resource = "arn:aws:logs:*:*:log-group:/grey/*"
      }
    ]
  })
}

# =============================================================================
# Secrets Manager for Node Attestation
# =============================================================================

resource "aws_secretsmanager_secret" "attestation_key" {
  count = var.enable_secrets_manager ? 1 : 0

  name                    = "grey/${var.environment}/attestation-key"
  description             = "Attestation key for Grey node identity verification"
  recovery_window_in_days = var.environment == "production" ? 30 : 7

  tags = merge(var.tags, {
    Purpose = "grey-node-attestation"
  })
}

resource "aws_secretsmanager_secret" "cluster_ca" {
  count = var.enable_secrets_manager ? 1 : 0

  name                    = "grey/${var.environment}/cluster-ca"
  description             = "Grey cluster internal CA certificate"
  recovery_window_in_days = var.environment == "production" ? 30 : 7

  tags = merge(var.tags, {
    Purpose = "grey-internal-tls"
  })
}

# =============================================================================
# CloudWatch Log Group for Audit Logs
# =============================================================================

resource "aws_cloudwatch_log_group" "grey_audit" {
  name              = "/grey/${var.environment}/audit"
  retention_in_days = var.environment == "production" ? 365 : 30

  tags = merge(var.tags, {
    Purpose = "grey-audit-logs"
  })
}

resource "aws_cloudwatch_log_group" "grey_security" {
  name              = "/grey/${var.environment}/security"
  retention_in_days = var.environment == "production" ? 365 : 30

  tags = merge(var.tags, {
    Purpose = "grey-security-events"
  })
}

# =============================================================================
# Outputs
# =============================================================================

output "kms_key_arn" {
  description = "KMS key ARN for encryption"
  value       = var.enable_kms_encryption ? aws_kms_key.grey[0].arn : null
}

output "kms_key_id" {
  description = "KMS key ID"
  value       = var.enable_kms_encryption ? aws_kms_key.grey[0].key_id : null
}

output "coordinator_role_arn" {
  description = "IAM role ARN for Grey coordinators"
  value       = var.enable_pod_identity && var.oidc_provider_arn != null ? aws_iam_role.grey_coordinator[0].arn : null
}

output "worker_role_arn" {
  description = "IAM role ARN for Grey workers"
  value       = var.enable_pod_identity && var.oidc_provider_arn != null ? aws_iam_role.grey_worker[0].arn : null
}

output "attestation_secret_arn" {
  description = "Secrets Manager ARN for attestation key"
  value       = var.enable_secrets_manager ? aws_secretsmanager_secret.attestation_key[0].arn : null
}

output "audit_log_group" {
  description = "CloudWatch log group for audit logs"
  value       = aws_cloudwatch_log_group.grey_audit.name
}
