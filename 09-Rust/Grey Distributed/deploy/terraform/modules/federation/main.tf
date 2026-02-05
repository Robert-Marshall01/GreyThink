# =============================================================================
# Grey Distributed — Federation Module
# =============================================================================
#
# Provisions multi-region federation infrastructure for Grey Distributed:
# - VPC Peering between regions
# - Transit Gateway for complex topologies
# - Cross-region DNS for service discovery
#
# Federation Topology:
#   ┌─────────────────────────────────────────────────────────────────────────┐
#   │                     Multi-Region Federation                              │
#   ├─────────────────────────────────────────────────────────────────────────┤
#   │                                                                          │
#   │  Region A (Primary)              Region B (Secondary)                    │
#   │  ┌───────────────────┐           ┌───────────────────┐                  │
#   │  │ Grey Cluster      │◄─────────▶│ Grey Cluster      │                  │
#   │  │ • Leader Election │  VPC      │ • Follower        │                  │
#   │  │ • Active Writes   │  Peering  │ • Read Replicas   │                  │
#   │  │ • Full Replication│           │ • Async Sync      │                  │
#   │  └───────────────────┘           └───────────────────┘                  │
#   │          │                               │                              │
#   │          │ Transit Gateway               │                              │
#   │          ▼                               ▼                              │
#   │  ┌───────────────────────────────────────────────────────────┐         │
#   │  │                    Edge Regions                            │         │
#   │  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐       │         │
#   │  │  │ Edge A  │  │ Edge B  │  │ Edge C  │  │ Edge D  │       │         │
#   │  │  └─────────┘  └─────────┘  └─────────┘  └─────────┘       │         │
#   │  └───────────────────────────────────────────────────────────┘         │
#   │                                                                          │
#   └─────────────────────────────────────────────────────────────────────────┘
#
# Why VPC Peering + Transit Gateway:
#   - VPC Peering: Low latency (sub-ms penalty), no additional hops
#   - Transit Gateway: Scales to many VPCs, centralized routing
#   - Use peering for production regions, TGW for edge
#
# Tradeoffs:
#   - VPC Peering is non-transitive (A↔B, B↔C doesn't give A↔C)
#   - Transit Gateway adds ~$0.05/GB data processing cost
#   - Cross-region latency is unavoidable (physics)
#
# =============================================================================

terraform {
  required_providers {
    aws = {
      source                = "hashicorp/aws"
      version               = "~> 5.0"
      configuration_aliases = [aws.primary, aws.secondary]
    }
  }
}

variable "primary_cluster_name" {
  type = string
}

variable "secondary_cluster_name" {
  type = string
}

variable "primary_vpc_id" {
  type = string
}

variable "primary_vpc_cidr" {
  type = string
}

variable "secondary_vpc_cidr" {
  type = string
}

variable "enable_vpc_peering" {
  type    = bool
  default = true
}

variable "enable_transit_gateway" {
  type    = bool
  default = false
}

variable "tags" {
  type    = map(string)
  default = {}
}

# =============================================================================
# Data Sources
# =============================================================================

data "aws_region" "primary" {
  provider = aws.primary
}

data "aws_region" "secondary" {
  provider = aws.secondary
}

data "aws_caller_identity" "primary" {
  provider = aws.primary
}

# =============================================================================
# Secondary VPC (if not already created)
# =============================================================================

resource "aws_vpc" "secondary" {
  provider = aws.secondary

  cidr_block           = var.secondary_vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = merge(var.tags, {
    Name = "${var.secondary_cluster_name}-vpc"
  })
}

# =============================================================================
# VPC Peering Connection
# =============================================================================

resource "aws_vpc_peering_connection" "primary_secondary" {
  count    = var.enable_vpc_peering ? 1 : 0
  provider = aws.primary

  vpc_id        = var.primary_vpc_id
  peer_vpc_id   = aws_vpc.secondary.id
  peer_region   = data.aws_region.secondary.name
  auto_accept   = false

  tags = merge(var.tags, {
    Name = "grey-${data.aws_region.primary.name}-to-${data.aws_region.secondary.name}"
  })
}

# Accept the peering connection in the secondary region
resource "aws_vpc_peering_connection_accepter" "secondary" {
  count    = var.enable_vpc_peering ? 1 : 0
  provider = aws.secondary

  vpc_peering_connection_id = aws_vpc_peering_connection.primary_secondary[0].id
  auto_accept               = true

  tags = merge(var.tags, {
    Name = "grey-${data.aws_region.secondary.name}-from-${data.aws_region.primary.name}"
  })
}

# =============================================================================
# Route Table Updates for Peering
# =============================================================================

# Get route tables from primary VPC
data "aws_route_tables" "primary" {
  provider = aws.primary
  vpc_id   = var.primary_vpc_id
}

# Get route tables from secondary VPC
data "aws_route_tables" "secondary" {
  provider = aws.secondary
  vpc_id   = aws_vpc.secondary.id
}

# Add routes to secondary VPC in primary route tables
resource "aws_route" "primary_to_secondary" {
  count    = var.enable_vpc_peering ? length(data.aws_route_tables.primary.ids) : 0
  provider = aws.primary

  route_table_id            = data.aws_route_tables.primary.ids[count.index]
  destination_cidr_block    = var.secondary_vpc_cidr
  vpc_peering_connection_id = aws_vpc_peering_connection.primary_secondary[0].id
}

# Add routes to primary VPC in secondary route tables
resource "aws_route" "secondary_to_primary" {
  count    = var.enable_vpc_peering ? length(data.aws_route_tables.secondary.ids) : 0
  provider = aws.secondary

  route_table_id            = data.aws_route_tables.secondary.ids[count.index]
  destination_cidr_block    = var.primary_vpc_cidr
  vpc_peering_connection_id = aws_vpc_peering_connection.primary_secondary[0].id
}

# =============================================================================
# Transit Gateway (for complex topologies)
# =============================================================================

resource "aws_ec2_transit_gateway" "grey" {
  count    = var.enable_transit_gateway ? 1 : 0
  provider = aws.primary

  description                     = "Grey Distributed federation transit gateway"
  default_route_table_association = "enable"
  default_route_table_propagation = "enable"
  dns_support                     = "enable"

  tags = merge(var.tags, {
    Name = "grey-federation-tgw"
  })
}

# Share Transit Gateway with secondary region
resource "aws_ram_resource_share" "tgw" {
  count    = var.enable_transit_gateway ? 1 : 0
  provider = aws.primary

  name                      = "grey-tgw-share"
  allow_external_principals = false

  tags = var.tags
}

resource "aws_ram_resource_association" "tgw" {
  count    = var.enable_transit_gateway ? 1 : 0
  provider = aws.primary

  resource_arn       = aws_ec2_transit_gateway.grey[0].arn
  resource_share_arn = aws_ram_resource_share.tgw[0].arn
}

# =============================================================================
# Cross-Region DNS (Route53)
# =============================================================================

resource "aws_route53_zone" "grey_internal" {
  provider = aws.primary

  name    = "grey.internal"
  comment = "Grey Distributed internal service discovery"

  vpc {
    vpc_id     = var.primary_vpc_id
    vpc_region = data.aws_region.primary.name
  }

  tags = merge(var.tags, {
    Purpose = "grey-service-discovery"
  })

  lifecycle {
    ignore_changes = [vpc]
  }
}

# Associate secondary VPC with the hosted zone
resource "aws_route53_zone_association" "secondary" {
  provider = aws.secondary

  zone_id = aws_route53_zone.grey_internal.zone_id
  vpc_id  = aws_vpc.secondary.id
}

# Service records
resource "aws_route53_record" "primary_cluster" {
  provider = aws.primary

  zone_id = aws_route53_zone.grey_internal.zone_id
  name    = "primary.grey.internal"
  type    = "A"
  ttl     = 60
  records = ["10.0.100.1"]  # Placeholder, should be service LoadBalancer IP
}

resource "aws_route53_record" "secondary_cluster" {
  provider = aws.primary

  zone_id = aws_route53_zone.grey_internal.zone_id
  name    = "secondary.grey.internal"
  type    = "A"
  ttl     = 60
  records = ["10.1.100.1"]  # Placeholder, should be service LoadBalancer IP
}

# =============================================================================
# Outputs
# =============================================================================

output "vpc_peering_connection_id" {
  description = "VPC Peering connection ID"
  value       = var.enable_vpc_peering ? aws_vpc_peering_connection.primary_secondary[0].id : null
}

output "transit_gateway_id" {
  description = "Transit Gateway ID"
  value       = var.enable_transit_gateway ? aws_ec2_transit_gateway.grey[0].id : null
}

output "internal_dns_zone_id" {
  description = "Route53 private hosted zone ID"
  value       = aws_route53_zone.grey_internal.zone_id
}

output "secondary_vpc_id" {
  description = "Secondary VPC ID"
  value       = aws_vpc.secondary.id
}
