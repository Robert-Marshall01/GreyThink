<#
.SYNOPSIS
    Grey Multi-Tenant - Auto-Start Services at Boot

.DESCRIPTION
    This script is run by Windows Task Scheduler at system boot to ensure
    all Grey Multi-Tenant services are running:
    - Docker Desktop (for PostgreSQL container)
    - PostgreSQL container (via docker compose)
    - Grey Core API backend

.NOTES
    Installed by: install.ps1
    Scheduled Task: GreyMultiTenantAutoStart
#>

$ErrorActionPreference = "SilentlyContinue"
$LogFile = "$env:ProgramData\GreyMultiTenant\logs\autostart.log"

function Write-Log {
    param([string]$Message)
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "[$timestamp] $Message"
    Add-Content -Path $LogFile -Value $logMessage -ErrorAction SilentlyContinue
    Write-Host $logMessage
}

# Ensure log directory exists
$logDir = Split-Path -Parent $LogFile
if (-not (Test-Path $logDir)) {
    New-Item -ItemType Directory -Path $logDir -Force | Out-Null
}

Write-Log "=== Grey Multi-Tenant Auto-Start Beginning ==="

# Find project directory (for docker-compose.yml)
$possibleDirs = @(
    "$env:ProgramFiles\GreyMultiTenant",
    "$env:USERPROFILE\OneDrive\Desktop\Grey Multi-Tenant",
    "$env:USERPROFILE\Desktop\Grey Multi-Tenant",
    "C:\Grey Multi-Tenant"
)

$ProjectDir = $null
foreach ($dir in $possibleDirs) {
    $composePath = Join-Path $dir "docker-compose.yml"
    if (Test-Path $composePath) {
        $ProjectDir = $dir
        break
    }
}

if (-not $ProjectDir) {
    Write-Log "ERROR: Could not find docker-compose.yml in known locations"
    exit 1
}

Write-Log "Project directory: $ProjectDir"

# Step 1: Start Docker Desktop if not running
Write-Log "Checking Docker Desktop..."
$dockerReady = $false
$dockerInfo = docker info 2>&1
if ($LASTEXITCODE -eq 0) {
    Write-Log "Docker Desktop is already running"
    $dockerReady = $true
} else {
    Write-Log "Starting Docker Desktop..."
    $dockerPaths = @(
        "$env:ProgramFiles\Docker\Docker\Docker Desktop.exe",
        "${env:LOCALAPPDATA}\Docker\Docker Desktop Installer.exe"
    )
    foreach ($dPath in $dockerPaths) {
        if (Test-Path $dPath) {
            Start-Process -FilePath $dPath -ErrorAction SilentlyContinue
            Write-Log "Launched Docker Desktop from: $dPath"
            break
        }
    }
}

# Step 2: Wait for Docker Desktop to be ready
if (-not $dockerReady) {
    Write-Log "Waiting for Docker Desktop to initialize..."
    $maxWait = 180  # 3 minutes max for cold start
    $waited = 0
    while ($waited -lt $maxWait) {
        Start-Sleep -Seconds 5
        $waited += 5
        $dockerInfo = docker info 2>&1
        if ($LASTEXITCODE -eq 0) {
            Write-Log "Docker is ready after $waited seconds"
            $dockerReady = $true
            break
        }
        if ($waited % 30 -eq 0) {
            Write-Log "Still waiting for Docker... ($waited seconds)"
        }
    }
    
    if (-not $dockerReady) {
        Write-Log "ERROR: Docker did not become ready within $maxWait seconds"
        exit 1
    }
}

# Step 2: Start PostgreSQL container
Write-Log "Starting PostgreSQL container..."
Push-Location $ProjectDir
try {
    docker compose up -d postgres 2>&1 | ForEach-Object { Write-Log "  $_" }
    Write-Log "PostgreSQL container started"
} catch {
    Write-Log "ERROR starting PostgreSQL: $_"
} finally {
    Pop-Location
}

# Wait for PostgreSQL to be healthy
Write-Log "Waiting for PostgreSQL to be healthy..."
$maxHealthWait = 60
$healthWait = 0
while ($healthWait -lt $maxHealthWait) {
    $health = docker inspect --format='{{.State.Health.Status}}' grey-postgres 2>&1
    if ($health -eq "healthy") {
        Write-Log "PostgreSQL is healthy"
        break
    }
    Start-Sleep -Seconds 3
    $healthWait += 3
}

if ($healthWait -ge $maxHealthWait) {
    Write-Log "WARNING: PostgreSQL health check timed out, continuing anyway"
}

# Step 3: Start Grey Core API
Write-Log "Starting Grey Core API..."

# Look for the API binary
$apiLocations = @(
    "$env:ProgramFiles\GreyMultiTenant\bin\grey-core-api.exe",
    (Join-Path $ProjectDir "services\grey-core-api\bin\grey-core-api.exe")
)

$ApiPath = $null
foreach ($loc in $apiLocations) {
    if (Test-Path $loc) {
        $ApiPath = $loc
        break
    }
}

if ($ApiPath) {
    # Check if already running
    $existing = Get-Process -Name "grey-core-api" -ErrorAction SilentlyContinue
    if ($existing) {
        Write-Log "Grey Core API already running (PID: $($existing.Id))"
    } else {
        $workDir = Split-Path -Parent (Split-Path -Parent $ApiPath)
        Write-Log "Starting API from: $ApiPath"
        Write-Log "Working directory: $workDir"
        
        # Start detached process
        Start-Process -FilePath $ApiPath -WorkingDirectory $workDir -WindowStyle Hidden
        Start-Sleep -Seconds 3
        
        # Verify it started
        $apiProc = Get-Process -Name "grey-core-api" -ErrorAction SilentlyContinue
        if ($apiProc) {
            Write-Log "Grey Core API started (PID: $($apiProc.Id))"
        } else {
            Write-Log "WARNING: Grey Core API may not have started"
        }
    }
} else {
    Write-Log "WARNING: grey-core-api.exe not found in expected locations"
}

# Step 4: Verify services
Write-Log "Verifying services..."
Start-Sleep -Seconds 5

# Check containers
$containers = docker ps --format "{{.Names}}: {{.Status}}" 2>&1
Write-Log "Docker containers:"
$containers | ForEach-Object { Write-Log "  $_" }

# Check API health
try {
    $health = Invoke-RestMethod -Uri "http://localhost:8080/health" -TimeoutSec 5 -ErrorAction Stop
    Write-Log "API health check: OK"
} catch {
    Write-Log "WARNING: API health check failed"
}

Write-Log "=== Grey Multi-Tenant Auto-Start Complete ==="
