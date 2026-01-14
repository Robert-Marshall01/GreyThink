# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Windows Installer
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script installs Grey Optimizer as a Windows Service.
#
# Usage (Run as Administrator):
#   .\install_windows.ps1                     # Simulation mode (default)
#   .\install_windows.ps1 -Live -Confirm      # Live mode
#
# Parameters:
#   -Live         Install in live mode (creates service)
#   -Confirm      Required confirmation for live mode
#   -Simulation   Run in simulation mode (default)
#   -Interval     Enforcement interval in seconds (default: 60)
#   -Uninstall    Remove existing installation
#   -Verbose      Show detailed output
#
# ═══════════════════════════════════════════════════════════════════════════════

[CmdletBinding()]
param(
    [switch]$Live,
    [switch]$Confirm,
    [switch]$Simulation,
    [int]$Interval = 60,
    [switch]$Uninstall,
    [string]$InstallPath = "C:\GreyOptimizer"
)

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

$ErrorActionPreference = "Stop"
$VERSION = "2.0.0"
$SERVICE_NAME = "GreyOptimizer"
$SERVICE_DISPLAY = "Grey Optimizer Daemon"
$SERVICE_DESCRIPTION = "Adaptive RAM Reclamation and System Optimization Service"
$HEALTH_ENDPOINT = "http://127.0.0.1:8090/health"
$HEALTH_TIMEOUT = 30
$NSSM_URL = "https://nssm.cc/release/nssm-2.24.zip"

# Paths
$SCRIPT_DIR = Split-Path -Parent $MyInvocation.MyCommand.Path
$PROJECT_ROOT = Split-Path -Parent $SCRIPT_DIR
$LOG_DIR = "$InstallPath\logs"
$DATA_DIR = "$InstallPath\data"
$VENV_DIR = "$InstallPath\.venv"
$DAEMON_DIR = "$InstallPath\daemon"

# ─────────────────────────────────────────────────────────────────────────────
# Helper Functions
# ─────────────────────────────────────────────────────────────────────────────

function Write-ColorOutput {
    param(
        [string]$Message,
        [string]$Level = "INFO"
    )
    
    $color = switch ($Level) {
        "INFO"    { "Cyan" }
        "SUCCESS" { "Green" }
        "WARN"    { "Yellow" }
        "ERROR"   { "Red" }
        "SIM"     { "Magenta" }
        default   { "White" }
    }
    
    $prefix = switch ($Level) {
        "INFO"    { "[INFO]" }
        "SUCCESS" { "[OK]" }
        "WARN"    { "[WARN]" }
        "ERROR"   { "[ERROR]" }
        "SIM"     { "[SIMULATION]" }
        default   { "" }
    }
    
    Write-Host "$prefix $Message" -ForegroundColor $color
}

function Test-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = [Security.Principal.WindowsPrincipal]$identity
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Test-PythonAvailable {
    try {
        $pythonVersion = & python --version 2>&1
        if ($pythonVersion -match "Python (\d+)\.(\d+)") {
            $major = [int]$Matches[1]
            $minor = [int]$Matches[2]
            if ($major -ge 3 -and $minor -ge 8) {
                Write-ColorOutput "Python $major.$minor detected" "SUCCESS"
                return $true
            }
        }
        Write-ColorOutput "Python 3.8+ required (found: $pythonVersion)" "ERROR"
        return $false
    }
    catch {
        Write-ColorOutput "Python not found. Please install Python 3.8+ from python.org" "ERROR"
        return $false
    }
}

function Install-NSSM {
    param([bool]$IsSimulation)
    
    $nssmPath = "$InstallPath\nssm.exe"
    
    if (Test-Path $nssmPath) {
        Write-ColorOutput "NSSM already installed" "INFO"
        return $nssmPath
    }
    
    if ($IsSimulation) {
        Write-ColorOutput "Would download and install NSSM" "SIM"
        return "nssm.exe"
    }
    
    Write-ColorOutput "Downloading NSSM (Non-Sucking Service Manager)..." "INFO"
    
    $zipPath = "$env:TEMP\nssm.zip"
    $extractPath = "$env:TEMP\nssm"
    
    try {
        [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
        Invoke-WebRequest -Uri $NSSM_URL -OutFile $zipPath -UseBasicParsing
        
        Expand-Archive -Path $zipPath -DestinationPath $extractPath -Force
        
        # Find the correct architecture
        $arch = if ([Environment]::Is64BitOperatingSystem) { "win64" } else { "win32" }
        $nssmSource = Get-ChildItem -Path $extractPath -Recurse -Filter "nssm.exe" | 
                      Where-Object { $_.DirectoryName -like "*$arch*" } | 
                      Select-Object -First 1
        
        if (-not $nssmSource) {
            $nssmSource = Get-ChildItem -Path $extractPath -Recurse -Filter "nssm.exe" | Select-Object -First 1
        }
        
        Copy-Item -Path $nssmSource.FullName -Destination $nssmPath -Force
        
        # Cleanup
        Remove-Item $zipPath -Force -ErrorAction SilentlyContinue
        Remove-Item $extractPath -Recurse -Force -ErrorAction SilentlyContinue
        
        Write-ColorOutput "NSSM installed to $nssmPath" "SUCCESS"
        return $nssmPath
    }
    catch {
        Write-ColorOutput "Failed to install NSSM: $_" "ERROR"
        throw
    }
}

function New-VirtualEnvironment {
    param([bool]$IsSimulation)
    
    if ($IsSimulation) {
        Write-ColorOutput "Would create virtual environment at $VENV_DIR" "SIM"
        return
    }
    
    Write-ColorOutput "Creating Python virtual environment..." "INFO"
    
    if (Test-Path $VENV_DIR) {
        Write-ColorOutput "Virtual environment already exists" "INFO"
    } else {
        & python -m venv $VENV_DIR
    }
    
    # Install dependencies
    $pipPath = "$VENV_DIR\Scripts\pip.exe"
    & $pipPath install --upgrade pip wheel
    & $pipPath install aiosqlite aiohttp
    
    Write-ColorOutput "Python dependencies installed" "SUCCESS"
}

function Copy-DaemonFiles {
    param([bool]$IsSimulation)
    
    if ($IsSimulation) {
        Write-ColorOutput "Would copy daemon files to $DAEMON_DIR" "SIM"
        return
    }
    
    Write-ColorOutput "Copying daemon files..." "INFO"
    
    $sourceDaemon = Join-Path $PROJECT_ROOT "daemon"
    if (Test-Path $sourceDaemon) {
        Copy-Item -Path $sourceDaemon -Destination $InstallPath -Recurse -Force
        Write-ColorOutput "Daemon files copied" "SUCCESS"
    } else {
        Write-ColorOutput "Source daemon directory not found: $sourceDaemon" "ERROR"
        throw "Daemon source not found"
    }
}

function New-WrapperScript {
    param([bool]$IsSimulation)
    
    $wrapperPath = "$InstallPath\run-daemon.bat"
    
    if ($IsSimulation) {
        Write-ColorOutput "Would create wrapper script at $wrapperPath" "SIM"
        return $wrapperPath
    }
    
    $mode = if ($Live) { "live" } else { "simulation" }
    
    $content = @"
@echo off
cd /d "$InstallPath"
"$VENV_DIR\Scripts\python.exe" -u "$DAEMON_DIR\grey_daemon.py" --mode=$mode --enforcement-interval=$Interval
"@
    
    Set-Content -Path $wrapperPath -Value $content -Encoding ASCII
    Write-ColorOutput "Wrapper script created" "SUCCESS"
    
    return $wrapperPath
}

function Install-WindowsService {
    param(
        [string]$NssmPath,
        [string]$WrapperPath,
        [bool]$IsSimulation
    )
    
    if ($IsSimulation) {
        Write-ColorOutput "Would create Windows service: $SERVICE_NAME" "SIM"
        Write-ColorOutput "  Display Name: $SERVICE_DISPLAY" "SIM"
        Write-ColorOutput "  Start Type: Automatic" "SIM"
        return
    }
    
    Write-ColorOutput "Installing Windows service..." "INFO"
    
    # Remove existing service if present
    $existingService = Get-Service -Name $SERVICE_NAME -ErrorAction SilentlyContinue
    if ($existingService) {
        Write-ColorOutput "Stopping existing service..." "WARN"
        Stop-Service -Name $SERVICE_NAME -Force -ErrorAction SilentlyContinue
        & $NssmPath remove $SERVICE_NAME confirm 2>$null
        Start-Sleep -Seconds 2
    }
    
    # Install with NSSM
    & $NssmPath install $SERVICE_NAME $WrapperPath
    & $NssmPath set $SERVICE_NAME DisplayName $SERVICE_DISPLAY
    & $NssmPath set $SERVICE_NAME Description $SERVICE_DESCRIPTION
    & $NssmPath set $SERVICE_NAME Start SERVICE_AUTO_START
    & $NssmPath set $SERVICE_NAME AppDirectory $InstallPath
    & $NssmPath set $SERVICE_NAME AppStdout "$LOG_DIR\daemon.log"
    & $NssmPath set $SERVICE_NAME AppStderr "$LOG_DIR\daemon.error.log"
    & $NssmPath set $SERVICE_NAME AppRotateFiles 1
    & $NssmPath set $SERVICE_NAME AppRotateBytes 10485760
    & $NssmPath set $SERVICE_NAME AppRestartDelay 5000
    
    Write-ColorOutput "Windows service installed" "SUCCESS"
}

function Start-GreyService {
    param([bool]$IsSimulation)
    
    if ($IsSimulation) {
        Write-ColorOutput "Would start service: $SERVICE_NAME" "SIM"
        return
    }
    
    Write-ColorOutput "Starting service..." "INFO"
    Start-Service -Name $SERVICE_NAME
    
    # Wait for service to be running
    $timeout = 10
    $elapsed = 0
    while ($elapsed -lt $timeout) {
        $service = Get-Service -Name $SERVICE_NAME -ErrorAction SilentlyContinue
        if ($service.Status -eq "Running") {
            Write-ColorOutput "Service started successfully" "SUCCESS"
            return
        }
        Start-Sleep -Seconds 1
        $elapsed++
    }
    
    Write-ColorOutput "Service did not start within ${timeout}s" "WARN"
}

function Test-HealthEndpoint {
    param([bool]$IsSimulation)
    
    if ($IsSimulation) {
        Write-ColorOutput "Would check health endpoint: $HEALTH_ENDPOINT" "SIM"
        return $true
    }
    
    Write-ColorOutput "Checking health endpoint..." "INFO"
    
    $elapsed = 0
    while ($elapsed -lt $HEALTH_TIMEOUT) {
        try {
            $response = Invoke-RestMethod -Uri $HEALTH_ENDPOINT -TimeoutSec 5
            if ($response.status -eq "healthy") {
                Write-ColorOutput "Health check passed!" "SUCCESS"
                Write-ColorOutput "  Status: $($response.status)" "INFO"
                Write-ColorOutput "  Version: $($response.version)" "INFO"
                Write-ColorOutput "  Mode: $($response.mode)" "INFO"
                return $true
            }
        }
        catch {
            # Retry
        }
        
        Write-Host "." -NoNewline
        Start-Sleep -Seconds 2
        $elapsed += 2
    }
    
    Write-Host ""
    Write-ColorOutput "Health check failed after ${HEALTH_TIMEOUT}s" "ERROR"
    return $false
}

function Uninstall-GreyOptimizer {
    param([bool]$IsSimulation)
    
    Write-ColorOutput "Uninstalling Grey Optimizer..." "INFO"
    
    if ($IsSimulation) {
        Write-ColorOutput "Would stop and remove service: $SERVICE_NAME" "SIM"
        Write-ColorOutput "Would remove installation directory: $InstallPath" "SIM"
        return
    }
    
    # Stop and remove service
    $service = Get-Service -Name $SERVICE_NAME -ErrorAction SilentlyContinue
    if ($service) {
        Stop-Service -Name $SERVICE_NAME -Force -ErrorAction SilentlyContinue
        
        $nssmPath = "$InstallPath\nssm.exe"
        if (Test-Path $nssmPath) {
            & $nssmPath remove $SERVICE_NAME confirm
        } else {
            sc.exe delete $SERVICE_NAME
        }
        
        Write-ColorOutput "Service removed" "SUCCESS"
    }
    
    # Remove installation directory (preserve data by default)
    if (Test-Path $InstallPath) {
        # Keep data and logs
        Get-ChildItem -Path $InstallPath -Exclude "data", "logs" | Remove-Item -Recurse -Force
        Write-ColorOutput "Installation files removed (data preserved)" "SUCCESS"
        Write-ColorOutput "To remove all data: Remove-Item -Recurse -Force '$InstallPath'" "INFO"
    }
}

function Show-Summary {
    param([bool]$IsSimulation)
    
    Write-Host ""
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host "  Grey Optimizer - Installation Complete" -ForegroundColor Cyan
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host ""
    
    Write-Host "Installation Summary:" -ForegroundColor White
    Write-Host "  Install Path:    $InstallPath" -ForegroundColor Gray
    Write-Host "  Log Directory:   $LOG_DIR" -ForegroundColor Gray
    Write-Host "  Data Directory:  $DATA_DIR" -ForegroundColor Gray
    Write-Host "  Service Name:    $SERVICE_NAME" -ForegroundColor Gray
    Write-Host "  Mode:            $(if ($Live) { 'LIVE' } else { 'SIMULATION' })" -ForegroundColor $(if ($Live) { "Yellow" } else { "Cyan" })
    Write-Host ""
    
    Write-Host "Service Commands:" -ForegroundColor White
    Write-Host "  Get-Service $SERVICE_NAME           # Check status" -ForegroundColor Gray
    Write-Host "  Start-Service $SERVICE_NAME         # Start service" -ForegroundColor Gray
    Write-Host "  Stop-Service $SERVICE_NAME          # Stop service" -ForegroundColor Gray
    Write-Host "  Restart-Service $SERVICE_NAME       # Restart service" -ForegroundColor Gray
    Write-Host ""
    
    Write-Host "Health Check:" -ForegroundColor White
    Write-Host "  Invoke-RestMethod $HEALTH_ENDPOINT" -ForegroundColor Gray
    Write-Host ""
    
    Write-Host "Logs:" -ForegroundColor White
    Write-Host "  Get-Content '$LOG_DIR\daemon.log' -Tail 50" -ForegroundColor Gray
    Write-Host ""
    
    if ($IsSimulation) {
        Write-Host "NOTE: This was a SIMULATION. No changes were made." -ForegroundColor Magenta
        Write-Host "Run with -Live -Confirm for actual installation." -ForegroundColor Magenta
    }
}

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

function Main {
    Write-Host ""
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host "  Grey Optimizer Windows Installer v$VERSION" -ForegroundColor Cyan
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host ""
    
    # Determine mode
    $IsSimulation = -not $Live
    $modeText = if ($IsSimulation) { "SIMULATION" } else { "LIVE" }
    Write-ColorOutput "Mode: $modeText" "INFO"
    
    # Handle uninstall
    if ($Uninstall) {
        if (-not (Test-Administrator)) {
            Write-ColorOutput "Administrator privileges required for uninstall" "ERROR"
            exit 1
        }
        Uninstall-GreyOptimizer -IsSimulation $IsSimulation
        exit 0
    }
    
    # Validate live mode
    if ($Live -and -not $Confirm) {
        Write-ColorOutput "Live mode requires -Confirm flag" "ERROR"
        Write-Host ""
        Write-Host "Usage: .\install_windows.ps1 -Live -Confirm" -ForegroundColor Yellow
        exit 1
    }
    
    # Check admin for live mode
    if ($Live -and -not (Test-Administrator)) {
        Write-ColorOutput "Administrator privileges required for live installation" "ERROR"
        Write-Host ""
        Write-Host "Right-click PowerShell and select 'Run as Administrator'" -ForegroundColor Yellow
        exit 1
    }
    
    # Check Python
    if (-not (Test-PythonAvailable)) {
        exit 1
    }
    
    # Create directories
    if (-not $IsSimulation) {
        New-Item -ItemType Directory -Force -Path $InstallPath | Out-Null
        New-Item -ItemType Directory -Force -Path $LOG_DIR | Out-Null
        New-Item -ItemType Directory -Force -Path $DATA_DIR | Out-Null
        Write-ColorOutput "Directories created" "SUCCESS"
    } else {
        Write-ColorOutput "Would create directories: $InstallPath, $LOG_DIR, $DATA_DIR" "SIM"
    }
    
    # Install NSSM
    $nssmPath = Install-NSSM -IsSimulation $IsSimulation
    
    # Create virtual environment
    New-VirtualEnvironment -IsSimulation $IsSimulation
    
    # Copy daemon files
    Copy-DaemonFiles -IsSimulation $IsSimulation
    
    # Create wrapper script
    $wrapperPath = New-WrapperScript -IsSimulation $IsSimulation
    
    # Install Windows service
    Install-WindowsService -NssmPath $nssmPath -WrapperPath $wrapperPath -IsSimulation $IsSimulation
    
    # Start service
    Start-GreyService -IsSimulation $IsSimulation
    
    # Health check
    $healthy = Test-HealthEndpoint -IsSimulation $IsSimulation
    
    # Show summary
    Show-Summary -IsSimulation $IsSimulation
    
    if (-not $IsSimulation -and -not $healthy) {
        Write-ColorOutput "Installation completed but health check failed" "WARN"
        Write-ColorOutput "Check logs at: $LOG_DIR\daemon.error.log" "INFO"
        exit 1
    }
}

# Run main
Main
