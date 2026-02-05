# =============================================================================
# Grey Distributed — Storage Module
# =============================================================================
#
# Provisions storage resources for Grey Distributed:
# - EBS-backed StorageClass for persistent volumes
# - S3 bucket for snapshots and backups
# - Lifecycle policies for cost optimization
#
# Storage Tiers:
#   ┌─────────────────────────────────────────────────────────────────────────┐
#   │  Hot Storage (EBS gp3)      │  Warm Storage (S3 Standard)               │
#   │  ─────────────────────      │  ─────────────────────────                │
#   │  • Raft log                 │  • Daily snapshots                        │
#   │  • Active state             │  • Checkpoint files                       │
#   │  • Low latency (<1ms)       │  • 30-day retention                       │
#   ├─────────────────────────────┼───────────────────────────────────────────┤
#   │  Cold Storage (S3 Glacier)                                              │
#   │  ────────────────────────                                               │
#   │  • Long-term backups (>90 days)                                         │
#   │  • Compliance archives                                                  │
#   └─────────────────────────────────────────────────────────────────────────┘
#
# Why gp3 over gp2:
#   - gp3: Fixed IOPS/throughput, cheaper baseline
#   - gp2: Burst-based, unpredictable performance
#   - For consensus workload, consistent IOPS is critical
#
# =============================================================================

variable "environment" {
  type = string
}

variable "cluster_name" {
  type = string
}

variable "storage_class_name" {
  type    = string
  default = "grey-ssd"
}

variable "volume_type" {
  type    = string
  default = "gp3"
}

variable "iops" {
  type    = number
  default = 3000
}

variable "throughput" {
  type    = number
  default = 125
}

variable "enable_s3_backup" {
  type    = bool
  default = true
}

variable "backup_bucket_name" {
  type = string
}

variable "backup_retention_days" {
  type    = number
  default = 30
}

variable "tags" {
  type    = map(string)
  default = {}
}

# =============================================================================
# Kubernetes StorageClass
# =============================================================================

resource "kubernetes_storage_class" "grey_ssd" {
  metadata {
    name = var.storage_class_name
    annotations = {
      "storageclass.kubernetes.io/is-default-class" = "false"
    }
  }

  storage_provisioner    = "ebs.csi.aws.com"
  reclaim_policy         = "Retain"  # Don't delete data on PVC deletion
  volume_binding_mode    = "WaitForFirstConsumer"  # Schedule-aware provisioning
  allow_volume_expansion = true

  parameters = {
    type       = var.volume_type
    iops       = tostring(var.iops)
    throughput = tostring(var.throughput)
    encrypted  = "true"
    fsType     = "ext4"
  }
}

# StorageClass for high-throughput workloads (WAL, hot data)
resource "kubernetes_storage_class" "grey_high_iops" {
  metadata {
    name = "${var.storage_class_name}-high-iops"
  }

  storage_provisioner    = "ebs.csi.aws.com"
  reclaim_policy         = "Retain"
  volume_binding_mode    = "WaitForFirstConsumer"
  allow_volume_expansion = true

  parameters = {
    type       = "io2"
    iops       = "10000"
    encrypted  = "true"
    fsType     = "ext4"
  }
}

# =============================================================================
# S3 Backup Bucket
# =============================================================================

resource "aws_s3_bucket" "backup" {
  count  = var.enable_s3_backup ? 1 : 0
  bucket = var.backup_bucket_name

  tags = merge(var.tags, {
    Name    = var.backup_bucket_name
    Purpose = "grey-distributed-backups"
  })
}

# Block public access
resource "aws_s3_bucket_public_access_block" "backup" {
  count  = var.enable_s3_backup ? 1 : 0
  bucket = aws_s3_bucket.backup[0].id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# Server-side encryption
resource "aws_s3_bucket_server_side_encryption_configuration" "backup" {
  count  = var.enable_s3_backup ? 1 : 0
  bucket = aws_s3_bucket.backup[0].id

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm = "aws:kms"
    }
    bucket_key_enabled = true
  }
}

# Versioning for backup integrity
resource "aws_s3_bucket_versioning" "backup" {
  count  = var.enable_s3_backup ? 1 : 0
  bucket = aws_s3_bucket.backup[0].id

  versioning_configuration {
    status = "Enabled"
  }
}

# Lifecycle policy for cost optimization
resource "aws_s3_bucket_lifecycle_configuration" "backup" {
  count  = var.enable_s3_backup ? 1 : 0
  bucket = aws_s3_bucket.backup[0].id

  # Snapshots: Move to Glacier after 30 days
  rule {
    id     = "snapshots-lifecycle"
    status = "Enabled"

    filter {
      prefix = "snapshots/"
    }

    transition {
      days          = 30
      storage_class = "STANDARD_IA"
    }

    transition {
      days          = 90
      storage_class = "GLACIER"
    }

    expiration {
      days = 365  # Delete after 1 year
    }

    noncurrent_version_expiration {
      noncurrent_days = 30
    }
  }

  # Checkpoints: Keep for retention period only
  rule {
    id     = "checkpoints-lifecycle"
    status = "Enabled"

    filter {
      prefix = "checkpoints/"
    }

    expiration {
      days = var.backup_retention_days
    }

    noncurrent_version_expiration {
      noncurrent_days = 7
    }
  }

  # Logs: Shorter retention
  rule {
    id     = "logs-lifecycle"
    status = "Enabled"

    filter {
      prefix = "logs/"
    }

    expiration {
      days = 7
    }
  }
}

# =============================================================================
# Outputs
# =============================================================================

output "storage_class_name" {
  description = "Name of the primary StorageClass"
  value       = kubernetes_storage_class.grey_ssd.metadata[0].name
}

output "high_iops_storage_class_name" {
  description = "Name of the high-IOPS StorageClass"
  value       = kubernetes_storage_class.grey_high_iops.metadata[0].name
}

output "backup_bucket_name" {
  description = "S3 bucket name for backups"
  value       = var.enable_s3_backup ? aws_s3_bucket.backup[0].id : null
}

output "backup_bucket_arn" {
  description = "S3 bucket ARN for backups"
  value       = var.enable_s3_backup ? aws_s3_bucket.backup[0].arn : null
}
