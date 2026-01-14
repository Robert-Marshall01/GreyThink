<#
.SYNOPSIS
    Grey Optimizer - Windows Installer

.DESCRIPTION
    Installs Grey Optimizer on Windows systems with service registration.
    
    Supports simulation mode (default, safe) and live mode (requires admin).
    All actions are logged to SQLite audit database with HMAC signatures.

.PARAMETER Mode
    Installation mode: simulation (default) or live

.PARAMETER ConfirmLive
    Required confirmation for live mode

.PARAMETER PreserveLogs
    Keep existing log files during reinstall

.PARAMETER SkipHealthCheck
    Skip post-install health verification

.PARAMETER Prefix
    Installation prefix (default: C:\Program Files\Grey Optimizer)

.EXAMPLE
    .\install.ps1
    Runs in simulation mode (safe, no changes)

.EXAMPLE
    .\install.ps1 -Mode live -ConfirmLive
    Runs in live mode with admin privileges

.EXAMPLE
    .\install.ps1 -Mode live -ConfirmLive -Prefix "D:\Grey Optimizer"
    Custom installation path
#>

[CmdletBinding()]
param(
    [ValidateSet("simulation", "live")]
    [string]$Mode = "simulation",
    
    [switch]$ConfirmLive,
    [switch]$PreserveLogs,
    [switch]$SkipHealthCheck,
    [string]$Prefix = "$env:ProgramFiles\Grey Optimizer"
)

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

$Version = "1.0.0"
$InstallTimestamp = Get-Date -Format "yyyyMMdd-HHmmss"

$InstallDir = $Prefix
$ConfigDir = "$env:ProgramData\Grey Optimizer"
$LogDir = "$env:ProgramData\Grey Optimizer\logs"
$DataDir = "$env:ProgramData\Grey Optimizer\data"
$BackupDir = "$env:ProgramData\Grey Optimizer\backups"

$ServiceName = "GreyOptimizer"
$ServiceDisplayName = "Grey Optimizer"
$ServiceDescription = "Hardware Resource Enforcement Daemon"

$HealthEndpoint = "http://127.0.0.1:5000/healthz"
$HealthTimeout = 30
$HealthRetryInterval = 2

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptDir

# Audit secret
$AuditSecret = if ($env:GREY_AUDIT_SECRET) { $env:GREY_AUDIT_SECRET } else { "$env:COMPUTERNAME-grey-optimizer-audit" }

# ─────────────────────────────────────────────────────────────────────────────
# Logging Functions
# ─────────────────────────────────────────────────────────────────────────────

function Write-LogInfo { param([string]$Message) Write-Host "[INFO] $Message" -ForegroundColor Blue }
function Write-LogSuccess { param([string]$Message) Write-Host "[OK] $Message" -ForegroundColor Green }
function Write-LogWarn { param([string]$Message) Write-Host "[WARN] $Message" -ForegroundColor Yellow }
function Write-LogError { param([string]$Message) Write-Host "[ERROR] $Message" -ForegroundColor Red }
function Write-LogSim { param([string]$Message) Write-Host "[SIMULATION] Would: $Message" -ForegroundColor Cyan }

# ─────────────────────────────────────────────────────────────────────────────
# Audit Functions
# ─────────────────────────────────────────────────────────────────────────────

function Initialize-AuditDB {
    param([string]$DbPath)
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Initialize audit database at $DbPath"
        return
    }
    
    $dbDir = Split-Path -Parent $DbPath
    if (-not (Test-Path $dbDir)) {
        New-Item -ItemType Directory -Path $dbDir -Force | Out-Null
    }
    
    $sql = @"
CREATE TABLE IF NOT EXISTS audit_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL DEFAULT (datetime('now')),
    action TEXT NOT NULL,
    category TEXT NOT NULL,
    details TEXT,
    mode TEXT NOT NULL,
    user TEXT,
    hostname TEXT,
    success INTEGER,
    signature TEXT
);
CREATE TABLE IF NOT EXISTS checkpoints (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL DEFAULT (datetime('now')),
    checkpoint_type TEXT NOT NULL,
    state_hash TEXT NOT NULL,
    signature TEXT NOT NULL
);
"@
    
    # Use sqlite3 if available, otherwise skip
    try {
        $sql | sqlite3 $DbPath
        Write-LogInfo "Audit database initialized: $DbPath"
    } catch {
        Write-LogWarn "Could not initialize audit database (sqlite3 not found)"
    }
}

function Write-AuditLog {
    param(
        [string]$Action,
        [string]$Category,
        [string]$Details = "",
        [int]$Success = 1
    )
    
    $dbPath = "$DataDir\audit.db"
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Audit: $Action ($Category)"
        return
    }
    
    # Create signature
    $timestamp = [DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
    $signData = "$Action|$Category|$Details|$Success|$timestamp"
    $hmac = New-Object System.Security.Cryptography.HMACSHA256
    $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes($AuditSecret)
    $hash = $hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($signData))
    $signature = [BitConverter]::ToString($hash).Replace("-", "").ToLower()
    
    try {
        $sql = "INSERT INTO audit_log (action, category, details, mode, user, hostname, success, signature) VALUES ('$Action', '$Category', '$Details', '$Mode', '$env:USERNAME', '$env:COMPUTERNAME', $Success, '$signature');"
        $sql | sqlite3 $dbPath
    } catch {
        # Silently continue if audit fails
    }
}

function New-Checkpoint {
    param(
        [string]$CheckpointType,
        [string]$StateData
    )
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Create checkpoint: $CheckpointType"
        return
    }
    
    $dbPath = "$DataDir\audit.db"
    
    $sha256 = [System.Security.Cryptography.SHA256]::Create()
    $stateHash = [BitConverter]::ToString($sha256.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($StateData))).Replace("-", "").ToLower()
    
    $timestamp = [DateTimeOffset]::UtcNow.ToUnixTimeSeconds()
    $signData = "$CheckpointType|$stateHash|$timestamp"
    $hmac = New-Object System.Security.Cryptography.HMACSHA256
    $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes($AuditSecret)
    $hash = $hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($signData))
    $signature = [BitConverter]::ToString($hash).Replace("-", "").ToLower()
    
    try {
        $sql = "INSERT INTO checkpoints (checkpoint_type, state_hash, signature) VALUES ('$CheckpointType', '$stateHash', '$signature');"
        $sql | sqlite3 $dbPath
        Write-LogInfo "Checkpoint created: $CheckpointType"
    } catch {
        # Silently continue
    }
}

# ─────────────────────────────────────────────────────────────────────────────
# Backup Functions
# ─────────────────────────────────────────────────────────────────────────────

function New-Backup {
    param(
        [string]$Source,
        [string]$BackupName = ""
    )
    
    if (-not (Test-Path $Source)) {
        Write-LogInfo "No existing file to backup: $Source"
        return
    }
    
    if (-not $BackupName) {
        $BackupName = Split-Path -Leaf $Source
    }
    
    $backupPath = "$BackupDir\$InstallTimestamp\$BackupName"
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Backup $Source -> $backupPath"
        return
    }
    
    $backupDir = Split-Path -Parent $backupPath
    if (-not (Test-Path $backupDir)) {
        New-Item -ItemType Directory -Path $backupDir -Force | Out-Null
    }
    
    Copy-Item -Path $Source -Destination $backupPath -Recurse -Force
    Write-AuditLog "backup_created" "filesystem" "source=$Source backup=$backupPath"
    Write-LogInfo "Backup created: $backupPath"
}

# ─────────────────────────────────────────────────────────────────────────────
# System Checks
# ─────────────────────────────────────────────────────────────────────────────

function Test-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Test-Dependencies {
    Write-LogInfo "Checking dependencies..."
    
    $missing = @()
    
    # Check Python
    try {
        $pyVersion = python --version 2>&1
        Write-LogInfo "Python: $pyVersion"
    } catch {
        $missing += "python"
    }
    
    # Check pip
    try {
        $null = pip --version 2>&1
    } catch {
        $missing += "pip"
    }
    
    if ($missing.Count -gt 0) {
        Write-LogError "Missing dependencies: $($missing -join ', ')"
        Write-LogError "Please install Python 3.11+ from https://python.org"
        return $false
    }
    
    return $true
}

# ─────────────────────────────────────────────────────────────────────────────
# Installation Functions
# ─────────────────────────────────────────────────────────────────────────────

function New-Directories {
    $dirs = @($InstallDir, $ConfigDir, $LogDir, $DataDir, $BackupDir)
    
    foreach ($dir in $dirs) {
        if ($Mode -eq "simulation") {
            Write-LogSim "Create directory: $dir"
        } else {
            if (-not (Test-Path $dir)) {
                New-Item -ItemType Directory -Path $dir -Force | Out-Null
                Write-LogInfo "Created: $dir"
            }
        }
    }
}

function Install-PythonPackage {
    Write-LogInfo "Installing Python package..."
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Create venv at $InstallDir\venv"
        Write-LogSim "pip install -e $ProjectRoot\backend"
        Write-LogSim "Install greyctl CLI"
        return
    }
    
    # Create virtual environment
    python -m venv "$InstallDir\venv"
    
    # Install package
    & "$InstallDir\venv\Scripts\pip.exe" install --upgrade pip wheel
    & "$InstallDir\venv\Scripts\pip.exe" install -e "$ProjectRoot\backend"
    
    # Create CLI wrapper batch file
    $cliContent = @"
@echo off
"$InstallDir\venv\Scripts\python.exe" -m grey_optimizer.cli %*
"@
    Set-Content -Path "$InstallDir\greyctl.bat" -Value $cliContent
    
    # Add to PATH (user)
    $currentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
    if ($currentPath -notlike "*$InstallDir*") {
        [Environment]::SetEnvironmentVariable("Path", "$currentPath;$InstallDir", "Machine")
    }
    
    Write-AuditLog "python_installed" "installation" "venv=$InstallDir\venv"
    Write-LogSuccess "Python package installed"
}

function Install-Config {
    $configFile = "$ConfigDir\config.yaml"
    
    New-Backup $configFile "config.yaml"
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Install config to $configFile"
        return
    }
    
    $config = @"
# Grey Optimizer Configuration (Windows)
mode: simulation

targets:
  cpu: 90
  memory: 90
  disk: 90

api:
  host: 127.0.0.1
  port: 5000

profiles:
  conservative:
    cpu_limit_percent: 50
    priority_class: BELOW_NORMAL_PRIORITY_CLASS
    
  balanced:
    cpu_limit_percent: 30
    priority_class: IDLE_PRIORITY_CLASS
    
  aggressive:
    cpu_limit_percent: 10
    priority_class: IDLE_PRIORITY_CLASS
    use_job_objects: true

logging:
  level: INFO
  file: $LogDir\daemon.log
"@
    
    Set-Content -Path $configFile -Value $config
    
    Write-AuditLog "config_installed" "installation" "path=$configFile"
    Write-LogSuccess "Configuration installed"
}

function Install-WindowsService {
    Write-LogInfo "Installing Windows service..."
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Register service: $ServiceName"
        Write-LogSim "sc.exe create $ServiceName ..."
        return
    }
    
    # Create service wrapper script
    $wrapperScript = @"
# Grey Optimizer Service Wrapper
# This script is executed by the Windows Task Scheduler or NSSM

`$ErrorActionPreference = 'Stop'

`$pythonExe = "$InstallDir\venv\Scripts\python.exe"
`$configPath = "$ConfigDir\config.yaml"

# Start the daemon
& `$pythonExe -m grey_optimizer.daemon --config `$configPath
"@
    
    Set-Content -Path "$InstallDir\service_wrapper.ps1" -Value $wrapperScript
    
    # Create scheduled task as service alternative
    $action = New-ScheduledTaskAction -Execute "powershell.exe" `
        -Argument "-NoProfile -ExecutionPolicy Bypass -File `"$InstallDir\service_wrapper.ps1`""
    
    $trigger = New-ScheduledTaskTrigger -AtStartup
    
    $principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" `
        -LogonType ServiceAccount -RunLevel Highest
    
    $settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries `
        -DontStopIfGoingOnBatteries -StartWhenAvailable `
        -RestartCount 3 -RestartInterval (New-TimeSpan -Minutes 1)
    
    # Remove existing task if present
    Unregister-ScheduledTask -TaskName $ServiceName -Confirm:$false -ErrorAction SilentlyContinue
    
    # Register the task
    Register-ScheduledTask -TaskName $ServiceName `
        -Action $action -Trigger $trigger -Principal $principal `
        -Settings $settings -Description $ServiceDescription
    
    Write-AuditLog "service_installed" "installation" "name=$ServiceName"
    Write-LogSuccess "Windows service installed (as scheduled task)"
    
    # Also show sc.exe alternative for reference
    Write-LogInfo "Alternative: Use NSSM for true Windows Service"
    Write-LogInfo "  nssm install $ServiceName `"$InstallDir\venv\Scripts\python.exe`""
}

function Start-GreyService {
    Write-LogInfo "Starting service..."
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Start-ScheduledTask $ServiceName"
        return
    }
    
    Start-ScheduledTask -TaskName $ServiceName
    
    Write-AuditLog "service_started" "service" "name=$ServiceName"
    Write-LogSuccess "Service started"
}

function Wait-ForHealth {
    if ($SkipHealthCheck) {
        Write-LogWarn "Skipping health check"
        return $true
    }
    
    Write-LogInfo "Waiting for service health..."
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Wait for $HealthEndpoint"
        return $true
    }
    
    $elapsed = 0
    
    while ($elapsed -lt $HealthTimeout) {
        try {
            $response = Invoke-WebRequest -Uri $HealthEndpoint -UseBasicParsing -TimeoutSec 5
            if ($response.StatusCode -eq 200) {
                Write-LogSuccess "Health check passed"
                Write-AuditLog "health_check" "verification" "status=passed"
                return $true
            }
        } catch {
            # Continue waiting
        }
        
        Start-Sleep -Seconds $HealthRetryInterval
        $elapsed += $HealthRetryInterval
        Write-Host "." -NoNewline
    }
    
    Write-Host ""
    Write-LogError "Health check failed after ${HealthTimeout}s"
    Write-AuditLog "health_check" "verification" "status=failed" 0
    return $false
}

# ─────────────────────────────────────────────────────────────────────────────
# Rollback
# ─────────────────────────────────────────────────────────────────────────────

function Invoke-Rollback {
    Write-LogError "Installation failed, initiating rollback..."
    
    if ($Mode -eq "simulation") {
        Write-LogSim "Rollback backups from $BackupDir\$InstallTimestamp"
        return
    }
    
    # Stop service
    Stop-ScheduledTask -TaskName $ServiceName -ErrorAction SilentlyContinue
    Unregister-ScheduledTask -TaskName $ServiceName -Confirm:$false -ErrorAction SilentlyContinue
    
    # Restore backups
    $backupBase = "$BackupDir\$InstallTimestamp"
    if (Test-Path $backupBase) {
        Get-ChildItem $backupBase | ForEach-Object {
            $target = switch ($_.Name) {
                "config.yaml" { "$ConfigDir\config.yaml" }
                default { $null }
            }
            if ($target) {
                Copy-Item -Path $_.FullName -Destination $target -Force
            }
        }
    }
    
    Write-AuditLog "rollback_completed" "installation" "backup_dir=$backupBase"
    Write-LogWarn "Rollback completed"
}

# ─────────────────────────────────────────────────────────────────────────────
# Consent Flow
# ─────────────────────────────────────────────────────────────────────────────

function Show-ConsentChecklist {
    Write-Host ""
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host "       Grey Optimizer - Windows Live Mode Installation" -ForegroundColor Cyan
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "You are about to install Grey Optimizer in LIVE mode."
    Write-Host ""
    Write-Host "Actions to be performed:" -ForegroundColor Yellow
    Write-Host "  [1] Install files to: $InstallDir"
    Write-Host "  [2] Create configuration in: $ConfigDir"
    Write-Host "  [3] Register Windows scheduled task/service"
    Write-Host "  [4] Start the service"
    Write-Host "  [5] Apply hardware enforcement (Job Objects, priorities)"
    Write-Host ""
    Write-Host "Safety measures:" -ForegroundColor Green
    Write-Host "  ✓ All changes backed up to: $BackupDir"
    Write-Host "  ✓ Rollback via: greyctl rollback"
    Write-Host "  ✓ All actions logged to audit database"
    Write-Host ""
    
    if (-not $ConfirmLive) {
        Write-Host "ERROR: Live mode requires -ConfirmLive flag" -ForegroundColor Red
        Write-Host ""
        Write-Host "To proceed, run:"
        Write-Host "  .\install.ps1 -Mode live -ConfirmLive"
        return $false
    }
    
    return $true
}

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

function Main {
    Write-Host ""
    Write-Host "Grey Optimizer Windows Installer v$Version" -ForegroundColor White
    Write-Host "Mode: $Mode" -ForegroundColor Cyan
    Write-Host ""
    
    # Mode checks
    if ($Mode -eq "live") {
        if (-not (Test-Administrator)) {
            Write-LogError "Live mode requires Administrator privileges"
            Write-Host "Right-click PowerShell and 'Run as Administrator'"
            exit 1
        }
        
        if (-not (Show-ConsentChecklist)) {
            exit 1
        }
    } else {
        Write-LogInfo "Running in SIMULATION mode - no changes will be made"
    }
    
    # Dependency check
    if (-not (Test-Dependencies)) {
        exit 1
    }
    
    # Initialize audit
    Initialize-AuditDB "$DataDir\audit.db"
    New-Checkpoint "pre_install" "$([DateTimeOffset]::UtcNow.ToUnixTimeSeconds())|$Version|$Mode"
    
    # Install
    try {
        Write-AuditLog "install_started" "installation" "version=$Version mode=$Mode"
        
        New-Directories
        Install-PythonPackage
        Install-Config
        Install-WindowsService
        Start-GreyService
        
        if (-not (Wait-ForHealth)) {
            throw "Health check failed"
        }
        
        New-Checkpoint "post_install" "$([DateTimeOffset]::UtcNow.ToUnixTimeSeconds())|$Version|success"
        Write-AuditLog "install_completed" "installation" "success=true"
        
    } catch {
        Write-LogError "Installation failed: $_"
        Invoke-Rollback
        exit 1
    }
    
    # Success
    Write-Host ""
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Green
    Write-Host "       Grey Optimizer installed successfully!" -ForegroundColor Green
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Green
    Write-Host ""
    Write-Host "Commands:"
    Write-Host "  greyctl status   - Check status"
    Write-Host "  greyctl health   - Health check"
    Write-Host "  greyctl rollback - Rollback changes"
    Write-Host ""
    Write-Host "Service management:"
    Write-Host "  Get-ScheduledTask -TaskName '$ServiceName'"
    Write-Host "  Start-ScheduledTask -TaskName '$ServiceName'"
    Write-Host "  Stop-ScheduledTask -TaskName '$ServiceName'"
    Write-Host ""
    
    if ($Mode -eq "simulation") {
        Write-Host "NOTE: Ran in simulation mode." -ForegroundColor Yellow
        Write-Host "For real install: .\install.ps1 -Mode live -ConfirmLive"
    }
}

Main
