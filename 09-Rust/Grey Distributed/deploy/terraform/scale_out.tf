# =============================================================================
# Grey Distributed — Terraform Scale-Out Configuration
# =============================================================================
#
# This Terraform module provisions auto-scaling infrastructure for Grey.
#
# Architecture:
#   - Coordinator Node Pool: Fixed 5 nodes across 3 AZs (Raft quorum)
#   - Worker Node Pool: 95-200 nodes with auto-scaling (task execution)
#   - Observability Node Pool: 3 nodes (Prometheus, Grafana, Jaeger)
#
# Cost Optimization:
#   - Spot instances for workers (70% cost savings, acceptable interruption)
#   - Reserved instances for coordinators (predictable, critical)
#   - Scheduled scaling for known workload patterns
#
# =============================================================================

terraform {
  required_version = ">= 1.5.0"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.25"
    }
  }
  
  backend "s3" {
    bucket         = "grey-terraform-state"
    key            = "production/scale-out/terraform.tfstate"
    region         = "us-east-1"
    encrypt        = true
    dynamodb_table = "grey-terraform-locks"
  }
}

# =============================================================================
# Variables
# =============================================================================

variable "environment" {
  description = "Deployment environment"
  type        = string
  default     = "production"
}

variable "region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"
}

variable "cluster_name" {
  description = "EKS cluster name"
  type        = string
  default     = "grey-production"
}

variable "coordinator_instance_type" {
  description = "EC2 instance type for coordinators"
  type        = string
  default     = "m6i.xlarge"  # 4 vCPU, 16 GB RAM, EBS-optimized
}

variable "worker_instance_types" {
  description = "EC2 instance types for workers (spot fleet diversity)"
  type        = list(string)
  default     = [
    "m6i.large",    # 2 vCPU, 8 GB RAM
    "m6a.large",    # AMD alternative
    "m5.large",     # Previous gen fallback
    "c6i.large",    # Compute optimized
  ]
}

variable "observability_instance_type" {
  description = "EC2 instance type for observability stack"
  type        = string
  default     = "m6i.large"
}

variable "coordinator_count" {
  description = "Number of coordinator nodes (should be odd: 3, 5, or 7)"
  type        = number
  default     = 5
  
  validation {
    condition     = var.coordinator_count % 2 == 1 && var.coordinator_count >= 3 && var.coordinator_count <= 7
    error_message = "Coordinator count must be odd (3, 5, or 7) for Raft quorum."
  }
}

variable "worker_min_count" {
  description = "Minimum number of worker nodes"
  type        = number
  default     = 95
}

variable "worker_max_count" {
  description = "Maximum number of worker nodes"
  type        = number
  default     = 200
}

variable "worker_desired_count" {
  description = "Desired number of worker nodes"
  type        = number
  default     = 95
}

variable "spot_allocation_strategy" {
  description = "Spot instance allocation strategy"
  type        = string
  default     = "capacity-optimized"  # Reduces interruptions
}

variable "on_demand_base_capacity" {
  description = "Minimum on-demand instances (always available)"
  type        = number
  default     = 20  # ~20% on-demand for baseline reliability
}

variable "on_demand_percentage" {
  description = "Percentage of on-demand vs spot for scaling"
  type        = number
  default     = 30  # 30% on-demand, 70% spot above base
}

# =============================================================================
# Data Sources
# =============================================================================

data "aws_availability_zones" "available" {
  state = "available"
  filter {
    name   = "opt-in-status"
    values = ["opt-in-not-required"]
  }
}

data "aws_eks_cluster" "grey" {
  name = var.cluster_name
}

data "aws_eks_cluster_auth" "grey" {
  name = var.cluster_name
}

# =============================================================================
# Provider Configuration
# =============================================================================

provider "aws" {
  region = var.region
  
  default_tags {
    tags = {
      Project     = "grey-distributed"
      Environment = var.environment
      ManagedBy   = "terraform"
      CostCenter  = "platform-infrastructure"
    }
  }
}

provider "kubernetes" {
  host                   = data.aws_eks_cluster.grey.endpoint
  cluster_ca_certificate = base64decode(data.aws_eks_cluster.grey.certificate_authority[0].data)
  token                  = data.aws_eks_cluster_auth.grey.token
}

# =============================================================================
# Coordinator Node Group (Fixed Size, On-Demand)
# =============================================================================
# Critical nodes that run Raft consensus. Never use spot for these.
# Spread across AZs for fault tolerance.
# =============================================================================

resource "aws_eks_node_group" "coordinators" {
  cluster_name    = var.cluster_name
  node_group_name = "grey-coordinators"
  node_role_arn   = aws_iam_role.node_role.arn
  
  # Spread across 3 AZs (at least 2 in each zone for 5 nodes)
  subnet_ids = slice(data.aws_availability_zones.available.zone_ids, 0, 3)
  
  # Fixed size - coordinators don't auto-scale
  scaling_config {
    desired_size = var.coordinator_count
    min_size     = var.coordinator_count
    max_size     = var.coordinator_count
  }
  
  # On-demand only - coordinators are critical
  capacity_type = "ON_DEMAND"
  
  instance_types = [var.coordinator_instance_type]
  disk_size      = 100  # GB for Raft WAL
  
  labels = {
    "grey.io/role"    = "coordinator"
    "grey.io/critical" = "true"
  }
  
  taint {
    key    = "grey.io/role"
    value  = "coordinator"
    effect = "NO_SCHEDULE"
  }
  
  update_config {
    max_unavailable = 1  # Update one at a time to maintain quorum
  }
  
  lifecycle {
    create_before_destroy = true
    ignore_changes        = [scaling_config[0].desired_size]
  }
  
  tags = {
    Name = "grey-coordinator"
    Role = "coordinator"
  }
}

# =============================================================================
# Worker Node Group (Auto-Scaling, Mixed Spot/On-Demand)
# =============================================================================
# Worker nodes execute tasks. Use spot instances for cost savings.
# Mixed strategy ensures baseline capacity even during spot interruptions.
# =============================================================================

resource "aws_launch_template" "workers" {
  name_prefix   = "grey-worker-"
  description   = "Launch template for Grey worker nodes"
  
  # Use EKS-optimized AMI
  image_id = data.aws_ssm_parameter.eks_ami.value
  
  vpc_security_group_ids = [aws_security_group.workers.id]
  
  iam_instance_profile {
    arn = aws_iam_instance_profile.node_profile.arn
  }
  
  # Enable detailed monitoring for better autoscaling decisions
  monitoring {
    enabled = true
  }
  
  # EBS optimization for storage performance
  ebs_optimized = true
  
  block_device_mappings {
    device_name = "/dev/xvda"
    ebs {
      volume_size           = 50
      volume_type           = "gp3"
      iops                  = 3000
      throughput            = 125
      delete_on_termination = true
      encrypted             = true
    }
  }
  
  # User data for node bootstrap
  user_data = base64encode(templatefile("${path.module}/templates/worker_userdata.sh", {
    cluster_name = var.cluster_name
    node_labels  = "grey.io/role=worker"
    node_taints  = ""
  }))
  
  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"  # IMDSv2 only
    http_put_response_hop_limit = 1
  }
  
  tag_specifications {
    resource_type = "instance"
    tags = {
      Name = "grey-worker"
      Role = "worker"
    }
  }
  
  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_autoscaling_group" "workers" {
  name                = "grey-workers"
  vpc_zone_identifier = slice(data.aws_availability_zones.available.zone_ids, 0, 3)
  
  min_size         = var.worker_min_count
  max_size         = var.worker_max_count
  desired_capacity = var.worker_desired_count
  
  # Health check configuration
  health_check_type         = "ELB"
  health_check_grace_period = 300
  
  # Termination policy: oldest first for rolling updates
  termination_policies = ["OldestInstance", "Default"]
  
  # Enable instance refresh for zero-downtime updates
  instance_refresh {
    strategy = "Rolling"
    preferences {
      min_healthy_percentage = 90
      instance_warmup        = 120
    }
  }
  
  # Mixed instances policy for spot/on-demand
  mixed_instances_policy {
    instances_distribution {
      on_demand_base_capacity                  = var.on_demand_base_capacity
      on_demand_percentage_above_base_capacity = var.on_demand_percentage
      spot_allocation_strategy                 = var.spot_allocation_strategy
      spot_instance_pools                      = 0  # Use all pools with capacity-optimized
    }
    
    launch_template {
      launch_template_specification {
        launch_template_id = aws_launch_template.workers.id
        version            = "$Latest"
      }
      
      # Instance type diversity for spot availability
      dynamic "override" {
        for_each = var.worker_instance_types
        content {
          instance_type     = override.value
          weighted_capacity = "1"
        }
      }
    }
  }
  
  # Warm pool for faster scale-out
  warm_pool {
    pool_state                  = "Stopped"
    min_size                    = 10
    max_group_prepared_capacity = 30
    
    instance_reuse_policy {
      reuse_on_scale_in = true
    }
  }
  
  # Tags for identification
  dynamic "tag" {
    for_each = {
      "Name"                                      = "grey-worker"
      "Role"                                      = "worker"
      "kubernetes.io/cluster/${var.cluster_name}" = "owned"
      "k8s.io/cluster-autoscaler/enabled"         = "true"
      "k8s.io/cluster-autoscaler/${var.cluster_name}" = "owned"
    }
    content {
      key                 = tag.key
      value               = tag.value
      propagate_at_launch = true
    }
  }
  
  lifecycle {
    create_before_destroy = true
    ignore_changes        = [desired_capacity]  # Managed by autoscaler
  }
}

# =============================================================================
# Auto-Scaling Policies
# =============================================================================

# Scale out on high CPU
resource "aws_autoscaling_policy" "workers_scale_out_cpu" {
  name                   = "grey-workers-scale-out-cpu"
  autoscaling_group_name = aws_autoscaling_group.workers.name
  policy_type            = "TargetTrackingScaling"
  
  target_tracking_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ASGAverageCPUUtilization"
    }
    target_value     = 70.0
    disable_scale_in = false
  }
}

# Scale based on custom queue depth metric
resource "aws_autoscaling_policy" "workers_scale_queue" {
  name                   = "grey-workers-scale-queue"
  autoscaling_group_name = aws_autoscaling_group.workers.name
  policy_type            = "StepScaling"
  adjustment_type        = "ChangeInCapacity"
  
  # Step adjustments based on queue depth
  step_adjustment {
    metric_interval_lower_bound = 0
    metric_interval_upper_bound = 5000
    scaling_adjustment          = 0
  }
  step_adjustment {
    metric_interval_lower_bound = 5000
    metric_interval_upper_bound = 10000
    scaling_adjustment          = 10
  }
  step_adjustment {
    metric_interval_lower_bound = 10000
    metric_interval_upper_bound = 25000
    scaling_adjustment          = 25
  }
  step_adjustment {
    metric_interval_lower_bound = 25000
    scaling_adjustment          = 50
  }
}

resource "aws_cloudwatch_metric_alarm" "queue_depth_high" {
  alarm_name          = "grey-queue-depth-high"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "grey_scheduler_queue_depth"
  namespace           = "Grey/Production"
  period              = 60
  statistic           = "Average"
  threshold           = 5000
  alarm_actions       = [aws_autoscaling_policy.workers_scale_queue.arn]
}

# Predictive scaling based on historical patterns
resource "aws_autoscaling_policy" "workers_predictive" {
  name                   = "grey-workers-predictive"
  autoscaling_group_name = aws_autoscaling_group.workers.name
  policy_type            = "PredictiveScaling"
  
  predictive_scaling_configuration {
    mode                         = "ForecastAndScale"
    scheduling_buffer_time       = 300  # 5 min buffer before predicted load
    max_capacity_breach_behavior = "IncreaseMaxCapacity"
    max_capacity_buffer          = 10
    
    metric_specification {
      target_value = 70
      
      predefined_load_metric_specification {
        predefined_metric_type = "ASGTotalCPUUtilization"
        resource_label         = "grey-workers"
      }
      
      predefined_scaling_metric_specification {
        predefined_metric_type = "ASGAverageCPUUtilization"
        resource_label         = "grey-workers"
      }
    }
  }
}

# Scheduled scaling for known patterns
resource "aws_autoscaling_schedule" "workers_scale_up_business_hours" {
  scheduled_action_name  = "scale-up-business-hours"
  autoscaling_group_name = aws_autoscaling_group.workers.name
  
  min_size         = 120
  max_size         = 200
  desired_capacity = 120
  
  recurrence = "0 8 * * 1-5"  # 8 AM UTC, Mon-Fri
  time_zone  = "UTC"
}

resource "aws_autoscaling_schedule" "workers_scale_down_night" {
  scheduled_action_name  = "scale-down-night"
  autoscaling_group_name = aws_autoscaling_group.workers.name
  
  min_size         = 50
  max_size         = 100
  desired_capacity = 50
  
  recurrence = "0 22 * * *"  # 10 PM UTC daily
  time_zone  = "UTC"
}

# =============================================================================
# Observability Node Group
# =============================================================================

resource "aws_eks_node_group" "observability" {
  cluster_name    = var.cluster_name
  node_group_name = "grey-observability"
  node_role_arn   = aws_iam_role.node_role.arn
  
  subnet_ids = slice(data.aws_availability_zones.available.zone_ids, 0, 3)
  
  scaling_config {
    desired_size = 3
    min_size     = 3
    max_size     = 5
  }
  
  capacity_type  = "ON_DEMAND"
  instance_types = [var.observability_instance_type]
  disk_size      = 200  # Large disk for metrics/traces
  
  labels = {
    "grey.io/role" = "observability"
  }
  
  taint {
    key    = "grey.io/role"
    value  = "observability"
    effect = "NO_SCHEDULE"
  }
  
  tags = {
    Name = "grey-observability"
    Role = "observability"
  }
}

# =============================================================================
# Spot Interruption Handler
# =============================================================================
# Handle spot interruptions gracefully by draining nodes before termination

resource "kubernetes_daemon_set" "spot_handler" {
  metadata {
    name      = "grey-spot-handler"
    namespace = "kube-system"
  }
  
  spec {
    selector {
      match_labels = {
        app = "grey-spot-handler"
      }
    }
    
    template {
      metadata {
        labels = {
          app = "grey-spot-handler"
        }
      }
      
      spec {
        service_account_name = "grey-spot-handler"
        
        container {
          name  = "handler"
          image = "amazon/aws-node-termination-handler:v1.21.0"
          
          env {
            name  = "ENABLE_SPOT_INTERRUPTION_DRAINING"
            value = "true"
          }
          env {
            name  = "ENABLE_SCHEDULED_EVENT_DRAINING"
            value = "true"
          }
          env {
            name  = "DELETE_LOCAL_DATA"
            value = "true"
          }
          env {
            name  = "GRACE_PERIOD"
            value = "60"
          }
          env {
            name  = "POD_TERMINATION_GRACE_PERIOD"
            value = "60"
          }
        }
        
        node_selector = {
          "grey.io/role" = "worker"
        }
      }
    }
  }
}

# =============================================================================
# Supporting Resources
# =============================================================================

data "aws_ssm_parameter" "eks_ami" {
  name = "/aws/service/eks/optimized-ami/1.28/amazon-linux-2/recommended/image_id"
}

resource "aws_iam_role" "node_role" {
  name = "grey-node-role"
  
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
}

resource "aws_iam_role_policy_attachment" "node_policies" {
  for_each = toset([
    "arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy",
    "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly",
    "arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy",
  ])
  
  role       = aws_iam_role.node_role.name
  policy_arn = each.value
}

resource "aws_iam_instance_profile" "node_profile" {
  name = "grey-node-profile"
  role = aws_iam_role.node_role.name
}

resource "aws_security_group" "workers" {
  name_prefix = "grey-workers-"
  vpc_id      = data.aws_eks_cluster.grey.vpc_config[0].vpc_id
  
  ingress {
    from_port = 0
    to_port   = 0
    protocol  = "-1"
    self      = true
  }
  
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  lifecycle {
    create_before_destroy = true
  }
}

# =============================================================================
# Outputs
# =============================================================================

output "coordinator_node_group_arn" {
  description = "ARN of coordinator node group"
  value       = aws_eks_node_group.coordinators.arn
}

output "worker_asg_arn" {
  description = "ARN of worker auto-scaling group"
  value       = aws_autoscaling_group.workers.arn
}

output "worker_asg_name" {
  description = "Name of worker auto-scaling group"
  value       = aws_autoscaling_group.workers.name
}

output "spot_allocation_strategy" {
  description = "Spot allocation strategy in use"
  value       = var.spot_allocation_strategy
}

output "cost_optimization_summary" {
  description = "Cost optimization configuration"
  value = {
    coordinator_capacity_type = "ON_DEMAND"
    worker_on_demand_base     = var.on_demand_base_capacity
    worker_on_demand_percent  = var.on_demand_percentage
    estimated_spot_savings    = "60-70%"
  }
}
