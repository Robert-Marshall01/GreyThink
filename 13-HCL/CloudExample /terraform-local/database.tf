# =============================================================================
# TERRAFORM LOCAL DATABASE CONFIGURATION
# =============================================================================
# Infrastructure as Code for local PostgreSQL database using Docker provider
# This creates a containerized PostgreSQL instance for local development
#
# Usage:
#   terraform init      - Initialize providers
#   terraform plan      - Preview changes
#   terraform apply     - Create resources
#   terraform destroy   - Remove resources
#
# Prerequisites:
#   - Docker installed and running
#   - Terraform >= 1.0

terraform {
  # Terraform version constraint
  required_version = ">= 1.0"
  
  # Provider requirements
  required_providers {
    # Docker provider for container management
    docker = {
      source  = "kreuzwerker/docker"      # Docker provider source
      version = "~> 3.0"                  # Version constraint
    }
    
    # Random provider for generating credentials
    random = {
      source  = "hashicorp/random"
      version = "~> 3.0"
    }
    
    # Local provider for file operations
    local = {
      source  = "hashicorp/local"
      version = "~> 2.0"
    }
  }
  
  # Local backend for state storage (no cloud required)
  # State file stored locally in terraform.tfstate
}

# =============================================================================
# DOCKER PROVIDER CONFIGURATION
# =============================================================================
# Configure the Docker provider to communicate with the local Docker daemon

provider "docker" {
  # Uses default Docker socket
  # Linux: unix:///var/run/docker.sock
  # Windows: npipe:////.//pipe//docker_engine
  # macOS: unix:///var/run/docker.sock (via Docker Desktop)
}

# =============================================================================
# VARIABLES
# =============================================================================
# Input variables for flexible configuration

variable "postgres_version" {
  description = "PostgreSQL version to deploy"
  type        = string
  default     = "15-alpine"               # PostgreSQL 15 on Alpine Linux
}

variable "postgres_port" {
  description = "Host port to expose PostgreSQL"
  type        = number
  default     = 5432                      # Standard PostgreSQL port
}

variable "postgres_database" {
  description = "Name of the database to create"
  type        = string
  default     = "appdb"                   # Default database name
}

variable "postgres_user" {
  description = "PostgreSQL admin username"
  type        = string
  default     = "appuser"                 # Default admin user
}

variable "container_name" {
  description = "Name for the PostgreSQL container"
  type        = string
  default     = "postgres-local"          # Container name
}

variable "volume_name" {
  description = "Name for the persistent data volume"
  type        = string
  default     = "postgres-data-local"     # Volume name
}

variable "network_name" {
  description = "Name for the Docker network"
  type        = string
  default     = "local-app-network"       # Network name
}

# =============================================================================
# RANDOM PASSWORD GENERATION
# =============================================================================
# Generate a secure random password for PostgreSQL

resource "random_password" "postgres_password" {
  length           = 24                   # Password length
  special          = true                 # Include special characters
  override_special = "!#$%&*()-_=+[]{}?"  # Allowed special characters
  min_lower        = 4                    # Minimum lowercase letters
  min_upper        = 4                    # Minimum uppercase letters
  min_numeric      = 4                    # Minimum numbers
  min_special      = 2                    # Minimum special characters
}

# =============================================================================
# DOCKER NETWORK
# =============================================================================
# Create a Docker network for container communication

resource "docker_network" "app_network" {
  name   = var.network_name               # Network name
  driver = "bridge"                       # Bridge network for local connectivity
  
  # IP address management configuration
  ipam_config {
    subnet  = "172.28.0.0/16"             # Subnet for the network
    gateway = "172.28.0.1"                # Gateway IP
  }
}

# =============================================================================
# DOCKER VOLUME FOR PERSISTENT DATA
# =============================================================================
# Create a named volume for PostgreSQL data persistence

resource "docker_volume" "postgres_data" {
  name = var.volume_name                  # Volume name
  
  # Volume persists data across container restarts
  # Data stored in Docker's default volume location
  # Linux: /var/lib/docker/volumes/<volume_name>/_data
}

# =============================================================================
# POSTGRESQL DOCKER IMAGE
# =============================================================================
# Pull the PostgreSQL Docker image

resource "docker_image" "postgres" {
  name         = "postgres:${var.postgres_version}"  # Image name with version
  keep_locally = true                     # Keep image after terraform destroy
}

# =============================================================================
# POSTGRESQL CONTAINER
# =============================================================================
# Create and run the PostgreSQL container

resource "docker_container" "postgres" {
  name  = var.container_name              # Container name
  image = docker_image.postgres.image_id  # Reference to pulled image
  
  # Restart policy
  restart = "unless-stopped"              # Auto-restart unless manually stopped
  
  # Port mapping: HOST:CONTAINER
  # SECURITY: Bind to localhost only to prevent external network access
  ports {
    internal = 5432                       # Container port (PostgreSQL default)
    external = var.postgres_port          # Host port (configurable)
    ip       = "127.0.0.1"                # SECURITY: Bind to localhost only
    protocol = "tcp"                      # Protocol
  }
  
  # Environment variables for PostgreSQL configuration
  env = [
    # Database name to create on first run
    "POSTGRES_DB=${var.postgres_database}",
    
    # Admin username
    "POSTGRES_USER=${var.postgres_user}",
    
    # Admin password (generated randomly)
    "POSTGRES_PASSWORD=${random_password.postgres_password.result}",
    
    # Data directory inside container
    "PGDATA=/var/lib/postgresql/data/pgdata"
  ]
  
  # Volume mount for persistent data
  volumes {
    volume_name    = docker_volume.postgres_data.name  # Reference to volume
    container_path = "/var/lib/postgresql/data"       # Mount path in container
    read_only      = false                            # Writable
  }
  
  # Connect to the application network
  networks_advanced {
    name         = docker_network.app_network.name    # Network name
    ipv4_address = "172.28.0.10"                      # Static IP address
  }
  
  # Health check configuration
  healthcheck {
    # Command to check if PostgreSQL is ready
    test         = ["CMD-SHELL", "pg_isready -U ${var.postgres_user} -d ${var.postgres_database}"]
    interval     = "10s"                  # Check every 10 seconds
    timeout      = "5s"                   # Timeout for each check
    retries      = 5                      # Number of retries before unhealthy
    start_period = "30s"                  # Initial delay before health checks
  }
  
  # Resource limits (optional - uncomment to limit resources)
  # memory = 512                          # Memory limit in MB
  # cpu_shares = 512                      # CPU shares (relative weight)
  
  # Wait for container to be ready
  must_run = true
}

# =============================================================================
# LOCAL FILE FOR CREDENTIALS
# =============================================================================
# Save database credentials to a local file for easy reference
# WARNING: This file contains sensitive data - do not commit to version control

resource "local_sensitive_file" "db_credentials" {
  filename = "${path.module}/.db-credentials"  # Output file path
  content  = <<-EOT
    # =============================================================================
    # PostgreSQL Local Development Credentials
    # =============================================================================
    # WARNING: This file contains sensitive information. Do not commit to git!
    #
    # Generated by Terraform on ${timestamp()}
    
    # Connection Details
    POSTGRES_HOST=localhost
    POSTGRES_PORT=${var.postgres_port}
    POSTGRES_DATABASE=${var.postgres_database}
    POSTGRES_USER=${var.postgres_user}
    POSTGRES_PASSWORD=${random_password.postgres_password.result}
    
    # Connection String
    DATABASE_URL=postgresql://${var.postgres_user}:${random_password.postgres_password.result}@localhost:${var.postgres_port}/${var.postgres_database}
    
    # psql command
    # psql -h localhost -p ${var.postgres_port} -U ${var.postgres_user} -d ${var.postgres_database}
  EOT
  
  file_permission = "0600"                # Restrict file permissions (owner only)
}

# =============================================================================
# OUTPUTS
# =============================================================================
# Output values for reference and integration

output "postgres_host" {
  description = "PostgreSQL host address"
  value       = "localhost"
}

output "postgres_port" {
  description = "PostgreSQL port"
  value       = var.postgres_port
}

output "postgres_database" {
  description = "PostgreSQL database name"
  value       = var.postgres_database
}

output "postgres_user" {
  description = "PostgreSQL username"
  value       = var.postgres_user
}

output "postgres_password" {
  description = "PostgreSQL password (sensitive)"
  value       = random_password.postgres_password.result
  sensitive   = true                      # Hide in console output
}

output "connection_string" {
  description = "PostgreSQL connection string (sensitive)"
  value       = "postgresql://${var.postgres_user}:${random_password.postgres_password.result}@localhost:${var.postgres_port}/${var.postgres_database}"
  sensitive   = true                      # Hide in console output
}

output "container_id" {
  description = "Docker container ID"
  value       = docker_container.postgres.id
}

output "container_name" {
  description = "Docker container name"
  value       = docker_container.postgres.name
}

output "network_name" {
  description = "Docker network name"
  value       = docker_network.app_network.name
}

output "volume_name" {
  description = "Docker volume name for persistent data"
  value       = docker_volume.postgres_data.name
}

output "psql_command" {
  description = "Command to connect via psql"
  value       = "psql -h localhost -p ${var.postgres_port} -U ${var.postgres_user} -d ${var.postgres_database}"
}

output "credentials_file" {
  description = "Path to credentials file"
  value       = local_sensitive_file.db_credentials.filename
}
