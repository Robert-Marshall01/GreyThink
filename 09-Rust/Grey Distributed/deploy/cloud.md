# Cloud Deployment Guide

Deploy Grey Distributed on major cloud platforms.

## AWS Deployment

### Architecture

```
                    ┌─────────────────┐
                    │   Route 53      │
                    │   (DNS)         │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │   ALB/NLB       │
                    │   (Load Balancer)│
                    └────────┬────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
   ┌────▼────┐         ┌────▼────┐         ┌────▼────┐
   │  AZ-a   │         │  AZ-b   │         │  AZ-c   │
   │ Node 1  │◄───────►│ Node 2  │◄───────►│ Node 3  │
   │ m5.2xl  │         │ m5.2xl  │         │ m5.2xl  │
   └────┬────┘         └────┬────┘         └────┬────┘
        │                    │                    │
   ┌────▼────┐         ┌────▼────┐         ┌────▼────┐
   │ EBS gp3 │         │ EBS gp3 │         │ EBS gp3 │
   │ 500 GB  │         │ 500 GB  │         │ 500 GB  │
   └─────────┘         └─────────┘         └─────────┘
```

### Terraform Configuration

```hcl
# main.tf
terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

variable "cluster_size" {
  default = 3
}

variable "instance_type" {
  default = "m5.2xlarge"
}

# VPC
module "vpc" {
  source = "terraform-aws-modules/vpc/aws"
  
  name = "grey-vpc"
  cidr = "10.0.0.0/16"
  
  azs             = ["us-east-1a", "us-east-1b", "us-east-1c"]
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]
  
  enable_nat_gateway = true
}

# Security Group
resource "aws_security_group" "grey" {
  name        = "grey-cluster"
  vpc_id      = module.vpc.vpc_id
  
  # API
  ingress {
    from_port   = 8080
    to_port     = 8080
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  # Raft (internal only)
  ingress {
    from_port   = 9080
    to_port     = 9080
    protocol    = "tcp"
    self        = true
  }
  
  # Gossip
  ingress {
    from_port   = 7946
    to_port     = 7946
    protocol    = "tcp"
    self        = true
  }
  
  ingress {
    from_port   = 7946
    to_port     = 7946
    protocol    = "udp"
    self        = true
  }
  
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# Launch Template
resource "aws_launch_template" "grey" {
  name_prefix   = "grey-"
  image_id      = data.aws_ami.amazon_linux.id
  instance_type = var.instance_type
  
  block_device_mappings {
    device_name = "/dev/xvda"
    ebs {
      volume_size = 500
      volume_type = "gp3"
      iops        = 16000
      throughput  = 1000
    }
  }
  
  user_data = base64encode(templatefile("${path.module}/user-data.sh", {
    cluster_size = var.cluster_size
  }))
}

# Auto Scaling Group
resource "aws_autoscaling_group" "grey" {
  name                = "grey-cluster"
  desired_capacity    = var.cluster_size
  max_size            = var.cluster_size
  min_size            = var.cluster_size
  vpc_zone_identifier = module.vpc.private_subnets
  
  launch_template {
    id      = aws_launch_template.grey.id
    version = "$Latest"
  }
  
  tag {
    key                 = "Name"
    value               = "grey-node"
    propagate_at_launch = true
  }
}

# Network Load Balancer
resource "aws_lb" "grey" {
  name               = "grey-nlb"
  internal           = false
  load_balancer_type = "network"
  subnets            = module.vpc.public_subnets
}

resource "aws_lb_target_group" "grey" {
  name     = "grey-api"
  port     = 8080
  protocol = "TCP"
  vpc_id   = module.vpc.vpc_id
  
  health_check {
    enabled  = true
    protocol = "TCP"
    port     = 8080
  }
}

resource "aws_lb_listener" "grey" {
  load_balancer_arn = aws_lb.grey.arn
  port              = 8080
  protocol          = "TCP"
  
  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.grey.arn
  }
}
```

### EKS Deployment (Kubernetes)

```yaml
# grey-statefulset.yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: grey
spec:
  serviceName: grey
  replicas: 3
  selector:
    matchLabels:
      app: grey
  template:
    metadata:
      labels:
        app: grey
    spec:
      affinity:
        podAntiAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
            - labelSelector:
                matchLabels:
                  app: grey
              topologyKey: topology.kubernetes.io/zone
      containers:
        - name: grey
          image: grey-distributed:latest
          ports:
            - containerPort: 8080
              name: api
            - containerPort: 9080
              name: raft
            - containerPort: 7946
              name: gossip
          env:
            - name: GREY_NODE_ID
              valueFrom:
                fieldRef:
                  fieldPath: metadata.name
            - name: GREY_BOOTSTRAP_PEERS
              value: "grey-0.grey:9080,grey-1.grey:9080,grey-2.grey:9080"
          volumeMounts:
            - name: data
              mountPath: /var/lib/grey
          resources:
            requests:
              cpu: "4"
              memory: "16Gi"
            limits:
              cpu: "8"
              memory: "32Gi"
  volumeClaimTemplates:
    - metadata:
        name: data
      spec:
        accessModes: ["ReadWriteOnce"]
        storageClassName: gp3
        resources:
          requests:
            storage: 500Gi
---
apiVersion: v1
kind: Service
metadata:
  name: grey
spec:
  clusterIP: None
  selector:
    app: grey
  ports:
    - port: 8080
      name: api
    - port: 9080
      name: raft
    - port: 7946
      name: gossip
```

## GCP Deployment

### Architecture

```
                    ┌─────────────────┐
                    │  Cloud DNS      │
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │  Cloud Load     │
                    │  Balancer       │
                    └────────┬────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
   ┌────▼────┐         ┌────▼────┐         ┌────▼────┐
   │ Zone-a  │         │ Zone-b  │         │ Zone-c  │
   │ n2-std-8│◄───────►│ n2-std-8│◄───────►│ n2-std-8│
   └────┬────┘         └────┬────┘         └────┬────┘
        │                    │                    │
   ┌────▼────┐         ┌────▼────┐         ┌────▼────┐
   │ PD-SSD  │         │ PD-SSD  │         │ PD-SSD  │
   │ 500 GB  │         │ 500 GB  │         │ 500 GB  │
   └─────────┘         └─────────┘         └─────────┘
```

### GKE Deployment

```yaml
# grey-config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: grey-config
data:
  grey.yaml: |
    network:
      api_addr: 0.0.0.0:8080
      raft_addr: 0.0.0.0:9080
    
    consensus:
      election_timeout: 1000ms
      heartbeat_interval: 150ms
    
    storage:
      engine: rocksdb
      shards: 32
```

## Azure Deployment

### Architecture

Uses Azure Virtual Machine Scale Sets with managed disks.

```hcl
# azure-main.tf
resource "azurerm_virtual_machine_scale_set" "grey" {
  name                = "grey-vmss"
  location            = azurerm_resource_group.grey.location
  resource_group_name = azurerm_resource_group.grey.name
  
  sku {
    name     = "Standard_D8s_v3"
    capacity = 3
  }
  
  storage_profile_os_disk {
    caching           = "ReadWrite"
    create_option     = "FromImage"
    managed_disk_type = "Premium_LRS"
  }
  
  storage_profile_data_disk {
    lun               = 0
    caching           = "None"
    create_option     = "Empty"
    disk_size_gb      = 500
    managed_disk_type = "Premium_LRS"
  }
  
  zones = ["1", "2", "3"]
}
```

## Multi-Region Considerations

### Latency Impact

| Topology | Latency | Throughput | Recommendation |
|----------|---------|------------|----------------|
| Single AZ | < 1ms | Highest | Dev/Test |
| Multi-AZ | 1-2ms | High | Production |
| Multi-Region | 50-100ms | Lower | Use federation |

### Cross-Region Replication

For multi-region, use Grey Federation instead of a single cluster:

```yaml
# federation.yaml
federation:
  enabled: true
  regions:
    - name: us-east
      endpoint: grey-east.example.com:8080
      primary: true
    - name: us-west
      endpoint: grey-west.example.com:8080
      replica: true
  
  replication:
    mode: async
    lag_threshold: 1000ms
```

## Cost Optimization

### Instance Sizing

| Workload | Instance Type | Monthly Cost (approx) |
|----------|---------------|----------------------|
| Small | m5.xlarge | $150 |
| Medium | m5.2xlarge | $300 |
| Large | m5.4xlarge | $600 |

### Reserved Instances

For production workloads, use 1-year or 3-year reserved instances for 30-60% savings.

### Spot Instances

**Not recommended** for Grey nodes due to:
- Data durability requirements
- Raft consensus stability
- Potential split-brain on termination

## Next Steps

- [Hybrid Deployment](hybrid.md) - Multi-cloud setup
- [Cluster Operations](cluster.md) - Day-2 operations
- [Security Guide](../docs/security.md)
