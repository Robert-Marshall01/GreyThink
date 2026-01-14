<#
.SYNOPSIS
    Grey Optimizer - Windows Uninstaller

.DESCRIPTION
    Safely removes Grey Optimizer from Windows systems.
    Performs dry run by default; requires -ConfirmUninstall for actual removal.

.PARAMETER ConfirmUninstall
    Required to actually remove (default: dry run)

.PARAMETER PreserveLogs
    Keep log files and audit database

.EXAMPLE
    .\uninstall.ps1
    Dry run - shows what would be removed

.EXAMPLE
    .\uninstall.ps1 -ConfirmUninstall
    Actually uninstall
#>

[CmdletBinding()]
param(
    [switch]$ConfirmUninstall,
    [switch]$PreserveLogs,
    [switch]$Help
)

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

$Version = "1.0.0"

$InstallDir = "$env:ProgramFiles\Grey Optimizer"
$ConfigDir = "$env:ProgramData\Grey Optimizer"
$LogDir = "$env:ProgramData\Grey Optimizer\logs"
$DataDir = "$env:ProgramData\Grey Optimizer\data"

$ServiceName = "GreyOptimizer"

$DryRun = -not $ConfirmUninstall

# ─────────────────────────────────────────────────────────────────────────────
# Logging
# ─────────────────────────────────────────────────────────────────────────────

function Write-LogInfo { param([string]$Message) Write-Host "[INFO] $Message" -ForegroundColor Blue }
function Write-LogSuccess { param([string]$Message) Write-Host "[OK] $Message" -ForegroundColor Green }
function Write-LogWarn { param([string]$Message) Write-Host "[WARN] $Message" -ForegroundColor Yellow }
function Write-LogError { param([string]$Message) Write-Host "[ERROR] $Message" -ForegroundColor Red }
function Write-LogDry { param([string]$Message) Write-Host "[DRY RUN] Would: $Message" -ForegroundColor Cyan }

# ─────────────────────────────────────────────────────────────────────────────
# Help
# ─────────────────────────────────────────────────────────────────────────────

if ($Help) {
    Write-Host @"

Grey Optimizer Windows Uninstaller v$Version

Usage: .\uninstall.ps1 [OPTIONS]

Options:
  -ConfirmUninstall    Actually remove (default: dry run)
  -PreserveLogs        Keep logs and audit data
  -Help                Show this help

Examples:
  .\uninstall.ps1                      # Dry run
  .\uninstall.ps1 -ConfirmUninstall    # Actually remove

"@
    exit 0
}

# ─────────────────────────────────────────────────────────────────────────────
# Admin Check
# ─────────────────────────────────────────────────────────────────────────────

function Test-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

# ─────────────────────────────────────────────────────────────────────────────
# Uninstall Functions
# ─────────────────────────────────────────────────────────────────────────────

function Stop-GreyService {
    Write-LogInfo "Stopping service..."
    
    $task = Get-ScheduledTask -TaskName $ServiceName -ErrorAction SilentlyContinue
    
    if (-not $task) {
        Write-LogInfo "Service not found (already removed?)"
        return
    }
    
    if ($DryRun) {
        Write-LogDry "Stop-ScheduledTask $ServiceName"
        Write-LogDry "Unregister-ScheduledTask $ServiceName"
        return
    }
    
    if ($task.State -eq 'Running') {
        Stop-ScheduledTask -TaskName $ServiceName -ErrorAction SilentlyContinue
        Write-LogSuccess "Service stopped"
    }
    
    Unregister-ScheduledTask -TaskName $ServiceName -Confirm:$false -ErrorAction SilentlyContinue
    Write-LogSuccess "Service removed"
}

function Remove-Installation {
    Write-LogInfo "Removing installation..."
    
    if (Test-Path $InstallDir) {
        # Kill any running processes from install dir
        Get-Process | Where-Object { $_.Path -like "$InstallDir\*" } | Stop-Process -Force -ErrorAction SilentlyContinue
        Start-Sleep -Seconds 1
        
        if ($DryRun) {
            Write-LogDry "Remove $InstallDir"
        } else {
            try {
                Remove-Item -Path $InstallDir -Recurse -Force
                Write-LogSuccess "Removed: $InstallDir"
            } catch {
                Write-LogWarn "Could not fully remove: $_"
            }
        }
    } else {
        Write-LogInfo "Installation directory not found"
    }
    
    # Remove from PATH
    if (-not $DryRun) {
        $currentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
        if ($currentPath -like "*$InstallDir*") {
            $newPath = ($currentPath -split ';' | Where-Object { $_ -ne $InstallDir }) -join ';'
            [Environment]::SetEnvironmentVariable("Path", $newPath, "Machine")
            Write-LogSuccess "Removed from PATH"
        }
    }
}

function Remove-Config {
    Write-LogInfo "Removing configuration..."
    
    if (-not $PreserveLogs) {
        if (Test-Path $ConfigDir) {
            if ($DryRun) {
                Write-LogDry "Remove $ConfigDir"
            } else {
                Remove-Item -Path $ConfigDir -Recurse -Force
                Write-LogSuccess "Removed: $ConfigDir"
            }
        }
    } else {
        Write-LogInfo "Preserving logs (-PreserveLogs)"
    }
}

function Remove-Registry {
    Write-LogInfo "Removing registry entries..."
    
    $regPath = "HKLM:\SOFTWARE\Grey Optimizer"
    
    if (Test-Path $regPath) {
        if ($DryRun) {
            Write-LogDry "Remove registry: $regPath"
        } else {
            Remove-Item -Path $regPath -Recurse -Force
            Write-LogSuccess "Registry entries removed"
        }
    }
}

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────

function Main {
    Write-Host ""
    Write-Host "Grey Optimizer Windows Uninstaller v$Version" -ForegroundColor White
    Write-Host ""
    
    if ($DryRun) {
        Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
        Write-Host "   DRY RUN MODE - No changes will be made" -ForegroundColor Cyan
        Write-Host "   Use -ConfirmUninstall to actually remove" -ForegroundColor Cyan
        Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
        Write-Host ""
    } else {
        if (-not (Test-Administrator)) {
            Write-LogError "Uninstall requires Administrator privileges"
            exit 1
        }
        
        Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Red
        Write-Host "   LIVE UNINSTALL - Grey Optimizer will be removed" -ForegroundColor Red
        Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Red
        Write-Host ""
    }
    
    Stop-GreyService
    Remove-Installation
    Remove-Config
    Remove-Registry
    
    Write-Host ""
    if ($DryRun) {
        Write-Host "Dry run complete. To uninstall: .\uninstall.ps1 -ConfirmUninstall" -ForegroundColor Cyan
    } else {
        Write-Host "Grey Optimizer uninstalled successfully!" -ForegroundColor Green
        
        if ($PreserveLogs) {
            Write-Host ""
            Write-Host "Logs preserved at: $ConfigDir" -ForegroundColor Yellow
        }
    }
    Write-Host ""
}

Main
