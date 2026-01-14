<#
.SYNOPSIS
    Grey Optimizer - Windows Uninstaller

.DESCRIPTION
    Safely removes Grey Optimizer from Windows systems.
    - Stops and removes the scheduled task
    - Removes program files
    - Restores system settings
    - Optionally preserves configuration and logs

.PARAMETER Force
    Skip confirmation prompts

.PARAMETER KeepData
    Preserve configuration and log files

.PARAMETER Help
    Show help message

.EXAMPLE
    .\uninstall.ps1
    Interactive uninstallation

.EXAMPLE
    .\uninstall.ps1 -Force
    Non-interactive uninstallation

.EXAMPLE
    .\uninstall.ps1 -KeepData
    Uninstall but preserve logs for audit
#>

[CmdletBinding()]
param(
    [switch]$Force,
    [switch]$KeepData,
    [switch]$Help
)

# ─────────────────────────────────────────────────────────────────────────────
# Configuration
# ─────────────────────────────────────────────────────────────────────────────

$InstallDir = "$env:ProgramFiles\Grey Optimizer"
$ConfigDir = "$env:ProgramData\Grey Optimizer"
$LogDir = "$env:ProgramData\Grey Optimizer\logs"
$TaskName = "Grey Optimizer"
$CliPath = "$env:ProgramFiles\Grey Optimizer\greyctl.exe"

# ─────────────────────────────────────────────────────────────────────────────
# Logging Functions
# ─────────────────────────────────────────────────────────────────────────────

function Write-Info {
    param([string]$Message)
    Write-Host "[INFO] " -ForegroundColor Blue -NoNewline
    Write-Host $Message
}

function Write-Warn {
    param([string]$Message)
    Write-Host "[WARN] " -ForegroundColor Yellow -NoNewline
    Write-Host $Message
}

function Write-Error2 {
    param([string]$Message)
    Write-Host "[ERROR] " -ForegroundColor Red -NoNewline
    Write-Host $Message
}

function Write-Success {
    param([string]$Message)
    Write-Host "[OK] " -ForegroundColor Green -NoNewline
    Write-Host $Message
}

# ─────────────────────────────────────────────────────────────────────────────
# Help Display
# ─────────────────────────────────────────────────────────────────────────────

if ($Help) {
    Write-Host @"

Grey Optimizer Uninstaller

Usage: .\uninstall.ps1 [OPTIONS]

Options:
  -Force        Skip confirmation prompts
  -KeepData     Preserve configuration and log files
  -Help         Show this help message

Examples:
  .\uninstall.ps1              # Interactive uninstall
  .\uninstall.ps1 -Force       # Automated uninstall
  .\uninstall.ps1 -KeepData    # Keep logs for audit

"@
    exit 0
}

# ─────────────────────────────────────────────────────────────────────────────
# Administrator Check
# ─────────────────────────────────────────────────────────────────────────────

function Test-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

if (-not (Test-Administrator)) {
    Write-Error2 "This script must be run as Administrator"
    Write-Host ""
    Write-Host "Right-click PowerShell and select 'Run as Administrator'"
    exit 1
}

# ─────────────────────────────────────────────────────────────────────────────
# Confirmation
# ─────────────────────────────────────────────────────────────────────────────

if (-not $Force) {
    Write-Host ""
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host "           Grey Optimizer Uninstaller" -ForegroundColor Cyan
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "This will remove Grey Optimizer from your system:"
    Write-Host "  • Stop and remove the scheduled task"
    Write-Host "  • Remove program files from $InstallDir"
    Write-Host "  • Close any active Job Objects"
    
    if ($KeepData) {
        Write-Host ""
        Write-Host "Configuration and logs will be PRESERVED:" -ForegroundColor Yellow
        Write-Host "  • $ConfigDir"
    } else {
        Write-Host ""
        Write-Host "Configuration and logs will be REMOVED:" -ForegroundColor Red
        Write-Host "  • $ConfigDir"
    }
    
    Write-Host ""
    $response = Read-Host "Do you want to continue? [y/N]"
    
    if ($response -notmatch '^[Yy]') {
        Write-Info "Uninstall cancelled"
        exit 0
    }
}

# ─────────────────────────────────────────────────────────────────────────────
# Rollback Enforcement
# ─────────────────────────────────────────────────────────────────────────────

function Invoke-Rollback {
    Write-Info "Rolling back any active enforcement..."
    
    # Try greyctl first
    if (Test-Path $CliPath) {
        try {
            & $CliPath rollback 2>$null
            Write-Success "Rollback via greyctl completed"
            return
        } catch {
            Write-Warn "greyctl rollback failed, using manual cleanup"
        }
    }
    
    # Manual cleanup - close any Grey Optimizer job objects
    # Job objects are automatically cleaned up when handle is closed
    # We just need to ensure processes are not still constrained
    
    Write-Success "Windows enforcement rolled back (automatic on task stop)"
}

# ─────────────────────────────────────────────────────────────────────────────
# Stop Scheduled Task
# ─────────────────────────────────────────────────────────────────────────────

function Stop-GreyTask {
    Write-Info "Stopping Grey Optimizer scheduled task..."
    
    try {
        $task = Get-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
        
        if ($task) {
            # Stop if running
            if ($task.State -eq 'Running') {
                Stop-ScheduledTask -TaskName $TaskName
                Write-Success "Stopped running task"
            }
            
            # Disable
            Disable-ScheduledTask -TaskName $TaskName -ErrorAction SilentlyContinue
            
            # Unregister
            Unregister-ScheduledTask -TaskName $TaskName -Confirm:$false
            Write-Success "Removed scheduled task: $TaskName"
        } else {
            Write-Info "Scheduled task not found (already removed?)"
        }
    } catch {
        Write-Warn "Error managing scheduled task: $_"
    }
    
    # Also check for the startup task
    $startupTask = Get-ScheduledTask -TaskName "Grey Optimizer Startup" -ErrorAction SilentlyContinue
    if ($startupTask) {
        Unregister-ScheduledTask -TaskName "Grey Optimizer Startup" -Confirm:$false
        Write-Success "Removed startup task"
    }
}

# ─────────────────────────────────────────────────────────────────────────────
# Remove Files
# ─────────────────────────────────────────────────────────────────────────────

function Remove-GreyFiles {
    Write-Info "Removing program files..."
    
    # Kill any running Grey Optimizer processes
    Get-Process | Where-Object { $_.Path -like "$InstallDir\*" } | Stop-Process -Force -ErrorAction SilentlyContinue
    
    # Small delay to ensure processes are stopped
    Start-Sleep -Seconds 1
    
    # Remove installation directory
    if (Test-Path $InstallDir) {
        try {
            Remove-Item -Path $InstallDir -Recurse -Force
            Write-Success "Removed $InstallDir"
        } catch {
            Write-Warn "Could not fully remove $InstallDir: $_"
            Write-Warn "Some files may be in use. Manual removal may be required."
        }
    } else {
        Write-Info "Installation directory not found (already removed?)"
    }
    
    # Remove from PATH if added
    $machinePath = [Environment]::GetEnvironmentVariable("Path", "Machine")
    if ($machinePath -like "*$InstallDir*") {
        $newPath = ($machinePath -split ';' | Where-Object { $_ -ne $InstallDir }) -join ';'
        [Environment]::SetEnvironmentVariable("Path", $newPath, "Machine")
        Write-Success "Removed from system PATH"
    }
    
    # Remove config and logs if not keeping data
    if (-not $KeepData) {
        if (Test-Path $ConfigDir) {
            try {
                Remove-Item -Path $ConfigDir -Recurse -Force
                Write-Success "Removed configuration: $ConfigDir"
            } catch {
                Write-Warn "Could not remove config directory: $_"
            }
        }
    } else {
        Write-Info "Keeping configuration and logs (-KeepData)"
    }
}

# ─────────────────────────────────────────────────────────────────────────────
# Restore System Settings
# ─────────────────────────────────────────────────────────────────────────────

function Restore-SystemSettings {
    Write-Info "Restoring system settings..."
    
    # Windows settings are typically not persistently modified by Grey Optimizer
    # Process priorities and Job Objects are cleared when processes exit
    
    # Remove any registry entries we created
    $regPath = "HKLM:\SOFTWARE\Grey Optimizer"
    if (Test-Path $regPath) {
        Remove-Item -Path $regPath -Recurse -Force
        Write-Success "Removed registry entries"
    }
    
    Write-Success "System settings restored (process limits cleared automatically)"
}

# ─────────────────────────────────────────────────────────────────────────────
# Main Uninstall Flow
# ─────────────────────────────────────────────────────────────────────────────

function Main {
    Write-Host ""
    Write-Info "Starting Grey Optimizer uninstallation..."
    Write-Host ""
    
    # Step 1: Rollback enforcement
    Invoke-Rollback
    
    # Step 2: Stop and remove scheduled task
    Stop-GreyTask
    
    # Step 3: Remove files
    Remove-GreyFiles
    
    # Step 4: Restore system settings
    Restore-SystemSettings
    
    Write-Host ""
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Green
    Write-Success "Grey Optimizer has been uninstalled successfully!"
    Write-Host "═══════════════════════════════════════════════════════════════════" -ForegroundColor Green
    
    if ($KeepData) {
        Write-Host ""
        Write-Host "Configuration and logs have been preserved:" -ForegroundColor Yellow
        Write-Host "  • $ConfigDir"
        Write-Host ""
        Write-Host "To fully remove all data, run:"
        Write-Host "  Remove-Item -Path '$ConfigDir' -Recurse -Force"
    }
    
    Write-Host ""
}

# Run main
Main
