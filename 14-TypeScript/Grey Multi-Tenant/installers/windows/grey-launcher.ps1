<#
.SYNOPSIS
    Grey Multi-Tenant Platform Launcher
.DESCRIPTION
    Checks prerequisites and launches the Grey Multi-Tenant API with visible logging.
    Self-healing launcher that automatically detects install path and config location.
#>

$ErrorActionPreference = "Continue"

# Determine install directory from script location
$ScriptDir = if ($PSScriptRoot) { $PSScriptRoot } else { Split-Path -Parent $MyInvocation.MyCommand.Path }

# Support both installed path (grey-launcher.ps1 in InstallDir root) and dev structure
if (Test-Path "$ScriptDir\bin\grey-core-api.exe") {
    $InstallDir = $ScriptDir
} elseif (Test-Path "$ScriptDir\..\bin\grey-core-api.exe") {
    $InstallDir = Split-Path -Parent $ScriptDir
} else {
    $InstallDir = "$env:ProgramFiles\GreyMultiTenant"
}

# Find config file - search multiple locations
$ConfigSearchPaths = @(
    "$InstallDir\config\.env",
    "$InstallDir\.env",
    "$env:ProgramData\GreyMultiTenant\.env",
    "$ScriptDir\config\.env",
    "$ScriptDir\.env"
)
$ConfigPath = $null
foreach ($path in $ConfigSearchPaths) {
    if (Test-Path $path) {
        $ConfigPath = $path
        break
    }
}

# Set GREY_CONFIG_PATH environment variable for the process
if ($ConfigPath) {
    $env:GREY_CONFIG_PATH = $ConfigPath
}

$DataDir = "$env:ProgramData\GreyMultiTenant"
$LogFile = "$DataDir\logs\startup-$(Get-Date -Format 'yyyy-MM-dd-HHmmss').log"

function Write-Log {
    param([string]$Message, [string]$Level = "INFO")
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "[$timestamp] [$Level] $Message"
    Write-Host $logMessage
    Add-Content -Path $LogFile -Value $logMessage -ErrorAction SilentlyContinue
}

# Ensure log directory exists
New-Item -ItemType Directory -Force -Path "$DataDir\logs" -ErrorAction SilentlyContinue | Out-Null

Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Grey Multi-Tenant Platform" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

Write-Log "Install directory: $InstallDir"
if ($ConfigPath) {
    Write-Log "Configuration: $ConfigPath"
} else {
    Write-Log "WARNING: No .env config file found, using environment variables" "WARN"
}

Write-Log "Checking prerequisites..."

# Check PostgreSQL availability with retry
$pgReady = $false
$maxRetries = 3
$retryDelay = 2

for ($attempt = 1; $attempt -le $maxRetries; $attempt++) {
    Write-Log "Checking PostgreSQL on localhost:5432... (attempt $attempt/$maxRetries)"

# Try Docker first
if (Get-Command docker -ErrorAction SilentlyContinue) {
    $container = docker ps --filter "name=grey-postgres" --format "{{.Names}}" 2>$null
    if ($container -eq "grey-postgres") {
        Write-Log "PostgreSQL running in Docker container"
        $pgReady = $true
    }
}

# Check native PostgreSQL service
if (-not $pgReady) {
    $pgService = Get-Service -Name "postgresql*" -ErrorAction SilentlyContinue | Where-Object { $_.Status -eq "Running" }
    if ($pgService) {
        Write-Log "PostgreSQL service running: $($pgService.Name)"
        $pgReady = $true
    }
}

# Test connection
$tcpTest = Test-NetConnection -ComputerName localhost -Port 5432 -WarningAction SilentlyContinue -ErrorAction SilentlyContinue
if ($tcpTest.TcpTestSucceeded) {
    Write-Log "PostgreSQL is accepting connections on port 5432"
    $pgReady = $true
} elseif (-not $pgReady) {
    Write-Log "PostgreSQL is NOT available on port 5432" "WARN"
}

    # If ready, break out of retry loop
    if ($pgReady) {
        break
    }
    
    # Wait before retry (except on last attempt)
    if ($attempt -lt $maxRetries -and -not $pgReady) {
        Write-Log "Waiting $retryDelay seconds before retry..."
        Start-Sleep -Seconds $retryDelay
    }
}

if (-not $pgReady) {
    Write-Host ""
    Write-Host "PostgreSQL not running. Attempting to start via Docker..." -ForegroundColor Yellow
    
    # Try to start PostgreSQL via Docker
    if (Get-Command docker -ErrorAction SilentlyContinue) {
        # Check if grey-postgres container exists
        $containerExists = docker ps -a --filter "name=grey-postgres" --format "{{.Names}}" 2>$null
        if ($containerExists -eq "grey-postgres") {
            Write-Log "Starting existing grey-postgres container..."
            docker start grey-postgres 2>$null
            Start-Sleep -Seconds 3
            $container = docker ps --filter "name=grey-postgres" --format "{{.Names}}" 2>$null
            if ($container -eq "grey-postgres") {
                Write-Log "PostgreSQL container started successfully"
                $pgReady = $true
            }
        } else {
            # Try to find docker-compose.yml and start postgres service
            $composeLocations = @(
                "$InstallDir\docker-compose.yml",
                "$env:USERPROFILE\Desktop\Grey Multi-Tenant\docker-compose.yml",
                "C:\Grey Multi-Tenant\docker-compose.yml"
            )
            foreach ($composePath in $composeLocations) {
                if (Test-Path $composePath) {
                    $composeDir = Split-Path -Parent $composePath
                    Write-Log "Found docker-compose.yml at $composeDir"
                    Push-Location $composeDir
                    docker compose up -d postgres 2>$null
                    Pop-Location
                    Start-Sleep -Seconds 5
                    $container = docker ps --filter "name=grey-postgres" --format "{{.Names}}" 2>$null
                    if ($container -eq "grey-postgres") {
                        Write-Log "PostgreSQL container started via docker compose"
                        $pgReady = $true
                        break
                    }
                }
            }
        }
    }
}

if (-not $pgReady) {
    Write-Host ""
    Write-Host "ERROR: PostgreSQL is not running!" -ForegroundColor Red
    Write-Host ""
    Write-Host "To fix, choose one of these options:" -ForegroundColor Yellow
    Write-Host "  1. Install Docker Desktop and run: docker compose up -d postgres"
    Write-Host "  2. Install PostgreSQL from https://www.postgresql.org/download/windows/"
    Write-Host ""
    Write-Log "Startup aborted: PostgreSQL not available" "FATAL"
    Write-Host "Press any key to exit..." -ForegroundColor Gray
    $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    exit 1
}

# Launch the application
Write-Log "Starting Grey Core API..."
Write-Log "GREY_CONFIG_PATH = $env:GREY_CONFIG_PATH"
Write-Log "Working directory = $InstallDir"
Write-Host ""
Write-Host "Server starting... Press Ctrl+C to stop." -ForegroundColor Yellow
Write-Host ""

# Set working directory to install dir so relative paths work
Push-Location $InstallDir

try {
    & "$InstallDir\bin\grey-core-api.exe" 2>&1 | ForEach-Object {
        Write-Host $_
        Add-Content -Path $LogFile -Value $_ -ErrorAction SilentlyContinue
    }
    $exitCode = $LASTEXITCODE
} catch {
    Write-Log "Exception: $_" "ERROR"
    $exitCode = 1
} finally {
    Pop-Location
}

if ($exitCode -ne 0) {
    Write-Host ""
    Write-Host "Application exited with error code: $exitCode" -ForegroundColor Red
    Write-Log "Application exited with code $exitCode" "ERROR"
    Write-Host "Log file: $LogFile" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Press any key to exit..." -ForegroundColor Gray
    $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
}

exit $exitCode
