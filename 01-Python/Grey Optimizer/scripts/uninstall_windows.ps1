# ═══════════════════════════════════════════════════════════════════════════════
# Grey Optimizer - Windows Uninstaller
# ═══════════════════════════════════════════════════════════════════════════════
#
# This script removes Grey Optimizer from Windows.
#
# Usage (Run as Administrator):
#   .\uninstall_windows.ps1           # Remove service, keep data
#   .\uninstall_windows.ps1 -Purge    # Remove everything including data
#
# ═══════════════════════════════════════════════════════════════════════════════

[CmdletBinding()]
param(
    [switch]$Purge,
    [switch]$Force,
    [string]$InstallPath = "C:\GreyOptimizer"
)

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

$ErrorActionPreference = "Stop"
$VERSION = "2.0.0"
$SERVICE_NAME = "GreyOptimizer"

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
        default   { "White" }
    }
    
    $prefix = switch ($Level) {
        "INFO"    { "[INFO]" }
        "SUCCESS" { "[OK]" }
        "WARN"    { "[WARN]" }
        "ERROR"   { "[ERROR]" }
        default   { "" }
    }
    
    Write-Host "$prefix $Message" -ForegroundColor $color
}

function Test-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = [Security.Principal.WindowsPrincipal]$identity
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Stop-GreyService {
    $service = Get-Service -Name $SERVICE_NAME -ErrorAction SilentlyContinue
    
    if (-not $service) {
        Write-ColorOutput "Service not found: $SERVICE_NAME" "INFO"
        return
    }
    
    if ($service.Status -eq "Running") {
        Write-ColorOutput "Stopping service..." "INFO"
        Stop-Service -Name $SERVICE_NAME -Force
        
        # Wait for stop
        $timeout = 10
        $elapsed = 0
        while ($elapsed -lt $timeout) {
            $service = Get-Service -Name $SERVICE_NAME -ErrorAction SilentlyContinue
            if ($service.Status -eq "Stopped") {
                Write-ColorOutput "Service stopped" "SUCCESS"
                return
            }
            Start-Sleep -Seconds 1
            $elapsed++
        }
        
        Write-ColorOutput "Service did not stop gracefully, forcing..." "WARN"
        Stop-Process -Name "python" -Force -ErrorAction SilentlyContinue
    }
    else {
        Write-ColorOutput "Service is not running" "INFO"
    }
}

function Remove-GreyService {
    $service = Get-Service -Name $SERVICE_NAME -ErrorAction SilentlyContinue
    
    if (-not $service) {
        Write-ColorOutput "Service not found" "INFO"
        return
    }
    
    Write-ColorOutput "Removing service..." "INFO"
    
    # Try NSSM first
    $nssmPath = "$InstallPath\nssm.exe"
    if (Test-Path $nssmPath) {
        & $nssmPath remove $SERVICE_NAME confirm 2>$null
    }
    else {
        # Fallback to sc.exe
        sc.exe delete $SERVICE_NAME
    }
    
    # Wait for removal
    Start-Sleep -Seconds 2
    
    $service = Get-Service -Name $SERVICE_NAME -ErrorAction SilentlyContinue
    if (-not $service) {
        Write-ColorOutput "Service removed successfully" "SUCCESS"
    }
    else {
        Write-ColorOutput "Service may not be fully removed until reboot" "WARN"
    }
}

function Remove-InstallationFiles {
    param([bool]$IncludeData)
    
    if (-not (Test-Path $InstallPath)) {
        Write-ColorOutput "Installation directory not found: $InstallPath" "INFO"
        return
    }
    
    if ($IncludeData) {
        Write-ColorOutput "Removing entire installation directory..." "INFO"
        Remove-Item -Path $InstallPath -Recurse -Force
        Write-ColorOutput "All files removed" "SUCCESS"
    }
    else {
        Write-ColorOutput "Removing installation files (preserving data)..." "INFO"
        
        # Remove everything except data and logs
        $preserve = @("data", "logs")
        Get-ChildItem -Path $InstallPath | 
            Where-Object { $_.Name -notin $preserve } | 
            ForEach-Object {
                Remove-Item -Path $_.FullName -Recurse -Force
            }
        
        Write-ColorOutput "Installation files removed" "SUCCESS"
        Write-ColorOutput "Data preserved at: $InstallPath\data" "INFO"
        Write-ColorOutput "Logs preserved at: $InstallPath\logs" "INFO"
    }
}

function Show-UninstallSummary {
    param([bool]$DataRemoved)
    
    Write-Host ""
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host "  Grey Optimizer - Uninstall Complete" -ForegroundColor Cyan
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host ""
    
    Write-Host "Removed:" -ForegroundColor White
    Write-Host "  ✓ Windows service: $SERVICE_NAME" -ForegroundColor Gray
    Write-Host "  ✓ Installation files" -ForegroundColor Gray
    
    if ($DataRemoved) {
        Write-Host "  ✓ Data and logs" -ForegroundColor Gray
    }
    else {
        Write-Host ""
        Write-Host "Preserved:" -ForegroundColor White
        Write-Host "  • Data: $InstallPath\data" -ForegroundColor Gray
        Write-Host "  • Logs: $InstallPath\logs" -ForegroundColor Gray
        Write-Host ""
        Write-Host "To remove all data, run:" -ForegroundColor Yellow
        Write-Host "  .\uninstall_windows.ps1 -Purge" -ForegroundColor Yellow
        Write-Host "  # or" -ForegroundColor Gray
        Write-Host "  Remove-Item -Recurse -Force '$InstallPath'" -ForegroundColor Yellow
    }
    
    Write-Host ""
    Write-ColorOutput "Grey Optimizer has been uninstalled" "SUCCESS"
}

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

function Main {
    Write-Host ""
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host "  Grey Optimizer Windows Uninstaller v$VERSION" -ForegroundColor Cyan
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host ""
    
    # Check admin
    if (-not (Test-Administrator)) {
        Write-ColorOutput "Administrator privileges required" "ERROR"
        Write-Host ""
        Write-Host "Right-click PowerShell and select 'Run as Administrator'" -ForegroundColor Yellow
        exit 1
    }
    
    # Confirmation
    if (-not $Force) {
        Write-Host "This will remove the Grey Optimizer service and installation files." -ForegroundColor Yellow
        if ($Purge) {
            Write-Host "WARNING: -Purge flag set - ALL data will be deleted!" -ForegroundColor Red
        }
        Write-Host ""
        
        $confirm = Read-Host "Continue? (y/N)"
        if ($confirm -ne "y" -and $confirm -ne "Y") {
            Write-ColorOutput "Uninstall cancelled" "INFO"
            exit 0
        }
    }
    
    Write-Host ""
    
    # Stop service
    Stop-GreyService
    
    # Remove service
    Remove-GreyService
    
    # Remove files
    Remove-InstallationFiles -IncludeData $Purge
    
    # Summary
    Show-UninstallSummary -DataRemoved $Purge
}

# Run main
Main
