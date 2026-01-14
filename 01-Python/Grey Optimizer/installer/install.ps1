# Grey Optimizer - Windows Installer (PowerShell)
# This script installs Grey Optimizer as a Windows Service

#Requires -RunAsAdministrator

param(
    [switch]$Simulation,
    [switch]$ConfirmLive,
    [switch]$SkipConsent,
    [switch]$Uninstall,
    [switch]$Help
)

# ═══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════
$ErrorActionPreference = "Stop"
$Version = "1.0.0"
$ServiceName = "GreyOptimizer"
$ServiceDisplayName = "Grey Optimizer"
$ServiceDescription = "Resource Optimization Daemon - Reduces CPU, RAM, and Disk usage"

$InstallDir = "C:\Program Files\Grey Optimizer"
$ConfigDir = "C:\ProgramData\Grey Optimizer\config"
$LogDir = "C:\ProgramData\Grey Optimizer\logs"
$DataDir = "C:\ProgramData\Grey Optimizer\data"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectDir = Split-Path -Parent $ScriptDir

# Mode
$SimulationMode = $true
if ($ConfirmLive) { $SimulationMode = $false }
if ($Simulation) { $SimulationMode = $true }

# ═══════════════════════════════════════════════════════════════════════════════
# FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════

function Write-Header {
    Write-Host ""
    Write-Host "╔══════════════════════════════════════════════════════════════════╗" -ForegroundColor Cyan
    Write-Host "║         Grey Optimizer - Windows Installer v$Version                ║" -ForegroundColor Cyan
    Write-Host "╚══════════════════════════════════════════════════════════════════╝" -ForegroundColor Cyan
    Write-Host ""
}

function Show-Help {
    Write-Host @"
Grey Optimizer Windows Installer v$Version

Usage:
    .\install.ps1 [OPTIONS]

Options:
    -Simulation     Install in simulation mode (default, safe)
    -ConfirmLive    Enable live enforcement mode (modifies system)
    -SkipConsent    Skip interactive consent prompts
    -Uninstall      Remove Grey Optimizer
    -Help           Show this help message

Examples:
    # Install in simulation mode
    .\install.ps1 -Simulation

    # Install in live mode with consent
    .\install.ps1 -ConfirmLive

    # Uninstall
    .\install.ps1 -Uninstall

"@
    exit 0
}

function Test-Administrator {
    $currentUser = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($currentUser)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Show-ConsentChecklist {
    if ($SkipConsent) {
        Write-Host "Consent prompts skipped (-SkipConsent)" -ForegroundColor Yellow
        return $true
    }
    
    if ($SimulationMode) {
        Write-Host "Simulation mode - no consent required" -ForegroundColor Green
        return $true
    }
    
    Write-Host ""
    Write-Host "╔══════════════════════════════════════════════════════════════════╗" -ForegroundColor Yellow
    Write-Host "║              LIVE MODE CONSENT CHECKLIST                         ║" -ForegroundColor Yellow
    Write-Host "╚══════════════════════════════════════════════════════════════════╝" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Grey Optimizer Live Mode will perform the following actions:" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "  [1] Create Job Objects to limit process CPU and memory"
    Write-Host "  [2] Modify process priority classes"
    Write-Host "  [3] Set process affinity masks"
    Write-Host "  [4] Create NTFS hardlinks for duplicate files"
    Write-Host "  [5] Convert eligible files to sparse format"
    Write-Host "  [6] Install a Windows Service that runs on boot"
    Write-Host ""
    Write-Host "Safety guarantees:" -ForegroundColor Cyan
    Write-Host "  ✓ All actions are logged to an audit trail"
    Write-Host "  ✓ All actions can be rolled back with 'greyctl rollback'"
    Write-Host "  ✓ File operations create backups before modification"
    Write-Host ""
    
    $consent = Read-Host "Do you understand and consent to these actions? (yes/no)"
    
    if ($consent -ne "yes") {
        Write-Host "Consent not given. Aborting installation." -ForegroundColor Red
        exit 1
    }
    
    Write-Host "✓ Consent confirmed" -ForegroundColor Green
    return $true
}

function Test-Dependencies {
    Write-Host "Checking dependencies..." -ForegroundColor Blue
    
    # Python
    try {
        $pythonVersion = python --version 2>&1
        if ($pythonVersion -match "Python 3\.(\d+)") {
            $minor = [int]$Matches[1]
            if ($minor -ge 11) {
                Write-Host "  ✓ $pythonVersion" -ForegroundColor Green
            } else {
                throw "Python 3.11+ required"
            }
        }
    } catch {
        Write-Host "  ✗ Python 3.11+ not found" -ForegroundColor Red
        Write-Host "    Install from: https://www.python.org/downloads/" -ForegroundColor Yellow
        exit 1
    }
    
    # Visual Studio Build Tools (for C modules)
    $vsWhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
    if (Test-Path $vsWhere) {
        Write-Host "  ✓ Visual Studio Build Tools available" -ForegroundColor Green
    } else {
        Write-Host "  ⚠ Visual Studio Build Tools not detected (C modules may not compile)" -ForegroundColor Yellow
    }
    
    Write-Host "✓ Dependencies checked" -ForegroundColor Green
}

function New-Directories {
    Write-Host "Creating directories..." -ForegroundColor Blue
    
    if ($SimulationMode) {
        $script:InstallDir = $ProjectDir
        $script:ConfigDir = Join-Path $ProjectDir "config"
        $script:LogDir = Join-Path $ProjectDir "logs"
        $script:DataDir = Join-Path $ProjectDir "data"
    }
    
    @($ConfigDir, $LogDir, "$DataDir\proofs", "$DataDir\backups") | ForEach-Object {
        if (-not (Test-Path $_)) {
            New-Item -ItemType Directory -Path $_ -Force | Out-Null
        }
    }
    
    if (-not $SimulationMode) {
        if (-not (Test-Path $InstallDir)) {
            New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
        }
    }
    
    Write-Host "✓ Directories created" -ForegroundColor Green
}

function Copy-InstallFiles {
    if ($SimulationMode) {
        Write-Host "Simulation mode: using project directory in place" -ForegroundColor Cyan
        return
    }
    
    Write-Host "Copying files to $InstallDir..." -ForegroundColor Blue
    
    # Copy backend
    Copy-Item -Path "$ProjectDir\backend\*" -Destination $InstallDir -Recurse -Force
    
    # Copy enforcement
    Copy-Item -Path "$ProjectDir\enforcement" -Destination $InstallDir -Recurse -Force
    
    Write-Host "✓ Files copied" -ForegroundColor Green
}

function Initialize-PythonEnvironment {
    Write-Host "Setting up Python environment..." -ForegroundColor Blue
    
    $venvDir = if ($SimulationMode) { "$ProjectDir\backend\venv" } else { "$InstallDir\venv" }
    
    if (-not (Test-Path $venvDir)) {
        python -m venv $venvDir
    }
    
    $activateScript = Join-Path $venvDir "Scripts\Activate.ps1"
    & $activateScript
    
    pip install --upgrade pip -q
    
    $backendPath = if ($SimulationMode) { "$ProjectDir\backend" } else { $InstallDir }
    pip install -e "$backendPath[dev]" -q
    
    deactivate
    
    Write-Host "✓ Python environment ready" -ForegroundColor Green
}

function New-Configuration {
    Write-Host "Creating configuration..." -ForegroundColor Blue
    
    $configFile = Join-Path $ConfigDir "config.yaml"
    
    if (Test-Path $configFile) {
        Write-Host "Configuration already exists, preserving" -ForegroundColor Yellow
        return
    }
    
    $configContent = @"
# Grey Optimizer Configuration
# Generated by installer on $(Get-Date -Format "yyyy-MM-ddTHH:mm:ss")

telemetry:
  interval_seconds: 1.0
  history_size: 300
  warmup_seconds: 10

cpu_enforcement:
  enabled: true
  target_percent: 10
  min_percent: 1

ram_enforcement:
  enabled: true
  target_mb: 64
  min_mb: 64

disk_enforcement:
  enabled: true
  target_iops: 100
  enable_dedupe: true
  enable_sparse: true

safety:
  simulation_mode: $($SimulationMode.ToString().ToLower())
  protected_patterns:
    - "System*"
    - "csrss*"
    - "wininit*"
    - "services*"
    - "lsass*"
    - "svchost*"
    - "grey-optimizer*"
  rollback_on_failure: true
  max_enforcement_percent: 90
  backup_before_modify: true

api:
  host: "127.0.0.1"
  port: 8080
  enable_websocket: true

persistence:
  database_path: "$($DataDir -replace '\\', '/')/audit.db"
  proof_dir: "$($DataDir -replace '\\', '/')/proofs"
  backup_dir: "$($DataDir -replace '\\', '/')/backups"
  retention_days: 30

logging:
  level: INFO
  file_path: "$($LogDir -replace '\\', '/')/daemon.log"
"@
    
    $configContent | Out-File -FilePath $configFile -Encoding UTF8
    
    Write-Host "✓ Configuration created" -ForegroundColor Green
}

function Install-WindowsService {
    if ($SimulationMode) {
        Write-Host "Simulation mode: skipping service installation" -ForegroundColor Cyan
        return
    }
    
    Write-Host "Installing Windows Service..." -ForegroundColor Blue
    
    # Check if service exists
    $existingService = Get-Service -Name $ServiceName -ErrorAction SilentlyContinue
    if ($existingService) {
        Write-Host "Stopping existing service..." -ForegroundColor Yellow
        Stop-Service -Name $ServiceName -Force
        sc.exe delete $ServiceName | Out-Null
        Start-Sleep -Seconds 2
    }
    
    # Create service wrapper script
    $serviceScript = @"
# Grey Optimizer Windows Service Wrapper
`$env:GREY_OPTIMIZER_CONFIG = "$ConfigDir\config.yaml"
& "$InstallDir\venv\Scripts\python.exe" -m grey_optimizer.daemon --immediate
"@
    
    $serviceScriptPath = Join-Path $InstallDir "service.ps1"
    $serviceScript | Out-File -FilePath $serviceScriptPath -Encoding UTF8
    
    # Use NSSM or sc.exe to create service
    # For simplicity, we'll use a scheduled task that runs at startup
    $action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "-NoProfile -ExecutionPolicy Bypass -File `"$serviceScriptPath`""
    $trigger = New-ScheduledTaskTrigger -AtStartup
    $principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" -LogonType ServiceAccount -RunLevel Highest
    $settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable -RestartCount 3 -RestartInterval (New-TimeSpan -Minutes 1)
    
    Register-ScheduledTask -TaskName $ServiceName -Action $action -Trigger $trigger -Principal $principal -Settings $settings -Description $ServiceDescription -Force | Out-Null
    
    Write-Host "✓ Windows Service installed" -ForegroundColor Green
}

function Start-GreyService {
    if ($SimulationMode) {
        Write-Host "Simulation mode: starting daemon in console..." -ForegroundColor Cyan
        
        $venvDir = "$ProjectDir\backend\venv"
        $activateScript = Join-Path $venvDir "Scripts\Activate.ps1"
        
        $env:GREY_OPTIMIZER_CONFIG = "$ConfigDir\config.yaml"
        
        Write-Host "Starting daemon..." -ForegroundColor Blue
        Start-Process -FilePath "$venvDir\Scripts\python.exe" -ArgumentList "-m grey_optimizer.daemon --immediate" -NoNewWindow
        
        Start-Sleep -Seconds 3
        Write-Host "✓ Daemon started in simulation mode" -ForegroundColor Green
        return
    }
    
    Write-Host "Starting service..." -ForegroundColor Blue
    
    Start-ScheduledTask -TaskName $ServiceName
    
    Start-Sleep -Seconds 3
    
    $taskInfo = Get-ScheduledTaskInfo -TaskName $ServiceName
    if ($taskInfo.LastTaskResult -eq 0 -or $taskInfo.LastTaskResult -eq 267009) {
        Write-Host "✓ Service started successfully" -ForegroundColor Green
    } else {
        Write-Host "⚠ Service may have issues (Result: $($taskInfo.LastTaskResult))" -ForegroundColor Yellow
    }
}

function Uninstall-Grey {
    Write-Host "Uninstalling Grey Optimizer..." -ForegroundColor Blue
    
    # Stop and remove scheduled task
    $task = Get-ScheduledTask -TaskName $ServiceName -ErrorAction SilentlyContinue
    if ($task) {
        Stop-ScheduledTask -TaskName $ServiceName -ErrorAction SilentlyContinue
        Unregister-ScheduledTask -TaskName $ServiceName -Confirm:$false
        Write-Host "✓ Service removed" -ForegroundColor Green
    }
    
    # Rollback enforcements
    if (Test-Path "$InstallDir\venv\Scripts\python.exe") {
        Write-Host "Rolling back enforcement actions..." -ForegroundColor Blue
        $env:GREY_OPTIMIZER_CONFIG = "$ConfigDir\config.yaml"
        & "$InstallDir\venv\Scripts\python.exe" -m grey_optimizer.cli rollback --force 2>$null
    }
    
    # Remove installation
    $removeData = Read-Host "Remove configuration and data? (yes/no)"
    
    if (Test-Path $InstallDir) {
        Remove-Item -Path $InstallDir -Recurse -Force
        Write-Host "✓ Installation directory removed" -ForegroundColor Green
    }
    
    if ($removeData -eq "yes") {
        if (Test-Path $ConfigDir) { Remove-Item -Path $ConfigDir -Recurse -Force }
        if (Test-Path $DataDir) { Remove-Item -Path $DataDir -Recurse -Force }
        if (Test-Path $LogDir) { Remove-Item -Path $LogDir -Recurse -Force }
        Write-Host "✓ Data and configuration removed" -ForegroundColor Green
    }
    
    Write-Host "✓ Grey Optimizer uninstalled" -ForegroundColor Green
    exit 0
}

# ═══════════════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════════════

if ($Help) { Show-Help }

Write-Header

if (-not (Test-Administrator)) {
    Write-Host "This script requires Administrator privileges." -ForegroundColor Red
    Write-Host "Right-click PowerShell and select 'Run as Administrator'" -ForegroundColor Yellow
    exit 1
}

Write-Host "Detected: Windows $([System.Environment]::OSVersion.Version)" -ForegroundColor Cyan

if ($Uninstall) {
    Uninstall-Grey
}

Show-ConsentChecklist
Test-Dependencies
New-Directories
Copy-InstallFiles
Initialize-PythonEnvironment
New-Configuration
Install-WindowsService
Start-GreyService

Write-Host ""
Write-Host "╔══════════════════════════════════════════════════════════════════╗" -ForegroundColor Green
Write-Host "║         Installation Complete!                                   ║" -ForegroundColor Green
Write-Host "╚══════════════════════════════════════════════════════════════════╝" -ForegroundColor Green
Write-Host ""

if ($SimulationMode) {
    Write-Host "Running in SIMULATION mode - no system changes applied" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "To enable live enforcement:" -ForegroundColor Cyan
    Write-Host "  .\install.ps1 -ConfirmLive"
} else {
    Write-Host "Running in LIVE mode - enforcement active" -ForegroundColor Green
}

Write-Host ""
Write-Host "Commands:"
Write-Host "  greyctl status    - View current status"
Write-Host "  greyctl rollback  - Rollback all changes"
Write-Host "  greyctl stop      - Stop the daemon"
Write-Host ""
