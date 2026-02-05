# =============================================================================
# Grey Distributed — Networking Module
# =============================================================================
#
# Provisions VPC, subnets, NAT gateways, and VPC endpoints for Grey Distributed.
#
# Network Architecture:
#   ┌─────────────────────────────────────────────────────────────────────────┐
#   │                              VPC (10.0.0.0/16)                          │
#   ├─────────────────────────────────────────────────────────────────────────┤
#   │  AZ-a                    │  AZ-b                    │  AZ-c             │
#   │  ┌──────────────────┐    │  ┌──────────────────┐    │  ┌────────────┐   │
#   │  │ Public Subnet    │    │  │ Public Subnet    │    │  │ Public     │   │
#   │  │ 10.0.0.0/24      │    │  │ 10.0.1.0/24      │    │  │ 10.0.2.0/24│   │
#   │  │ (NAT, LB, Ingress)│   │  │ (NAT, LB)        │    │  │            │   │
#   │  └──────────────────┘    │  └──────────────────┘    │  └────────────┘   │
#   │  ┌──────────────────┐    │  ┌──────────────────┐    │  ┌────────────┐   │
#   │  │ Private Subnet   │    │  │ Private Subnet   │    │  │ Private    │   │
#   │  │ 10.0.10.0/24     │    │  │ 10.0.11.0/24     │    │  │ 10.0.12.0/24│  │
#   │  │ (EKS Nodes)      │    │  │ (EKS Nodes)      │    │  │            │   │
#   │  └──────────────────┘    │  └──────────────────┘    │  └────────────┘   │
#   └─────────────────────────────────────────────────────────────────────────┘
#
# Why this matters:
#   - Public subnets for load balancers and NAT gateways
#   - Private subnets for EKS nodes (no direct internet access)
#   - VPC endpoints reduce NAT costs and improve latency
#   - Multi-AZ for high availability
#
# Tradeoffs:
#   - NAT gateway costs $0.045/hour + data processing
#   - Single NAT gateway in non-prod saves ~$100/month but reduces HA
#   - VPC endpoints have hourly cost but save NAT data costs
#
# =============================================================================

variable "environment" {
  type = string
}

variable "region" {
  type = string
}

variable "vpc_cidr" {
  type = string
}

variable "cluster_name" {
  type = string
}

variable "azs" {
  type = list(string)
}

variable "enable_nat_gateway" {
  type    = bool
  default = true
}

variable "single_nat_gateway" {
  type    = bool
  default = false
}

variable "enable_vpn_gateway" {
  type    = bool
  default = false
}

variable "enable_vpc_endpoints" {
  type    = bool
  default = true
}

variable "tags" {
  type    = map(string)
  default = {}
}

# =============================================================================
# VPC
# =============================================================================

resource "aws_vpc" "main" {
  cidr_block           = var.vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-vpc"
    # Required for EKS
    "kubernetes.io/cluster/${var.cluster_name}" = "shared"
  })
}

# =============================================================================
# Internet Gateway
# =============================================================================

resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-igw"
  })
}

# =============================================================================
# Subnets
# =============================================================================

# Public subnets (for NAT gateways, load balancers)
resource "aws_subnet" "public" {
  count = length(var.azs)

  vpc_id                  = aws_vpc.main.id
  cidr_block              = cidrsubnet(var.vpc_cidr, 8, count.index)
  availability_zone       = var.azs[count.index]
  map_public_ip_on_launch = true

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-public-${var.azs[count.index]}"
    "kubernetes.io/cluster/${var.cluster_name}" = "shared"
    "kubernetes.io/role/elb" = "1"  # For public load balancers
  })
}

# Private subnets (for EKS nodes)
resource "aws_subnet" "private" {
  count = length(var.azs)

  vpc_id            = aws_vpc.main.id
  cidr_block        = cidrsubnet(var.vpc_cidr, 8, count.index + 10)
  availability_zone = var.azs[count.index]

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-private-${var.azs[count.index]}"
    "kubernetes.io/cluster/${var.cluster_name}" = "shared"
    "kubernetes.io/role/internal-elb" = "1"  # For internal load balancers
  })
}

# =============================================================================
# NAT Gateways
# =============================================================================

# Elastic IPs for NAT gateways
resource "aws_eip" "nat" {
  count  = var.enable_nat_gateway ? (var.single_nat_gateway ? 1 : length(var.azs)) : 0
  domain = "vpc"

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-nat-eip-${count.index}"
  })

  depends_on = [aws_internet_gateway.main]
}

# NAT gateways
resource "aws_nat_gateway" "main" {
  count = var.enable_nat_gateway ? (var.single_nat_gateway ? 1 : length(var.azs)) : 0

  allocation_id = aws_eip.nat[count.index].id
  subnet_id     = aws_subnet.public[count.index].id

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-nat-${count.index}"
  })

  depends_on = [aws_internet_gateway.main]
}

# =============================================================================
# Route Tables
# =============================================================================

# Public route table
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-public-rt"
  })
}

# Associate public subnets with public route table
resource "aws_route_table_association" "public" {
  count = length(var.azs)

  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

# Private route tables (one per AZ for HA, or single for cost savings)
resource "aws_route_table" "private" {
  count  = var.single_nat_gateway ? 1 : length(var.azs)
  vpc_id = aws_vpc.main.id

  dynamic "route" {
    for_each = var.enable_nat_gateway ? [1] : []
    content {
      cidr_block     = "0.0.0.0/0"
      nat_gateway_id = var.single_nat_gateway ? aws_nat_gateway.main[0].id : aws_nat_gateway.main[count.index].id
    }
  }

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-private-rt-${count.index}"
  })
}

# Associate private subnets with private route tables
resource "aws_route_table_association" "private" {
  count = length(var.azs)

  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = var.single_nat_gateway ? aws_route_table.private[0].id : aws_route_table.private[count.index].id
}

# =============================================================================
# VPC Endpoints
# =============================================================================
# VPC endpoints allow private access to AWS services without going through NAT,
# reducing costs and improving latency for high-volume traffic.

# Security group for VPC endpoints
resource "aws_security_group" "vpc_endpoints" {
  count = var.enable_vpc_endpoints ? 1 : 0

  name_prefix = "${var.cluster_name}-vpce-"
  vpc_id      = aws_vpc.main.id

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = [var.vpc_cidr]
  }

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-vpce-sg"
  })

  lifecycle {
    create_before_destroy = true
  }
}

# S3 Gateway Endpoint (free, recommended)
resource "aws_vpc_endpoint" "s3" {
  count = var.enable_vpc_endpoints ? 1 : 0

  vpc_id            = aws_vpc.main.id
  service_name      = "com.amazonaws.${var.region}.s3"
  vpc_endpoint_type = "Gateway"
  route_table_ids   = aws_route_table.private[*].id

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-s3-endpoint"
  })
}

# ECR API Endpoint (for pulling container images)
resource "aws_vpc_endpoint" "ecr_api" {
  count = var.enable_vpc_endpoints ? 1 : 0

  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${var.region}.ecr.api"
  vpc_endpoint_type   = "Interface"
  subnet_ids          = aws_subnet.private[*].id
  security_group_ids  = [aws_security_group.vpc_endpoints[0].id]
  private_dns_enabled = true

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-ecr-api-endpoint"
  })
}

# ECR DKR Endpoint (for Docker registry)
resource "aws_vpc_endpoint" "ecr_dkr" {
  count = var.enable_vpc_endpoints ? 1 : 0

  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${var.region}.ecr.dkr"
  vpc_endpoint_type   = "Interface"
  subnet_ids          = aws_subnet.private[*].id
  security_group_ids  = [aws_security_group.vpc_endpoints[0].id]
  private_dns_enabled = true

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-ecr-dkr-endpoint"
  })
}

# STS Endpoint (for IAM role credentials)
resource "aws_vpc_endpoint" "sts" {
  count = var.enable_vpc_endpoints ? 1 : 0

  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${var.region}.sts"
  vpc_endpoint_type   = "Interface"
  subnet_ids          = aws_subnet.private[*].id
  security_group_ids  = [aws_security_group.vpc_endpoints[0].id]
  private_dns_enabled = true

  tags = merge(var.tags, {
    Name = "${var.cluster_name}-sts-endpoint"
  })
}

# =============================================================================
# Outputs
# =============================================================================

output "vpc_id" {
  description = "VPC ID"
  value       = aws_vpc.main.id
}

output "vpc_cidr" {
  description = "VPC CIDR block"
  value       = aws_vpc.main.cidr_block
}

output "public_subnet_ids" {
  description = "Public subnet IDs"
  value       = aws_subnet.public[*].id
}

output "private_subnet_ids" {
  description = "Private subnet IDs"
  value       = aws_subnet.private[*].id
}

output "nat_gateway_ips" {
  description = "NAT gateway public IPs"
  value       = aws_eip.nat[*].public_ip
}
