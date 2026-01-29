<#
.SYNOPSIS
    Grey Multi-Tenant Platform - Windows Unified Uninstaller

.DESCRIPTION
    Removes both the Grey Multi-Tenant backend service and GUI desktop application.
    Automatically prompts for elevation if not run as Administrator.

.PARAMETER KeepData
    Preserve configuration and log files

.PARAMETER BackendOnly
    Only uninstall the backend, keep GUI

.PARAMETER GUIOnly
    Only uninstall the GUI, keep backend

.NOTES
    Can be run from any terminal - will auto-elevate with UAC prompt.
#>

param(
    [string]$InstallDir = "$env:ProgramFiles\GreyMultiTenant",
    [string]$DataDir = "$env:ProgramData\GreyMultiTenant",
    [switch]$KeepData = $false,
    [switch]$BackendOnly = $false,
    [switch]$GUIOnly = $false,
    [switch]$Elevated = $false
)

$ErrorActionPreference = "Stop"

# Self-elevation
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "Requesting administrator privileges..." -ForegroundColor Yellow
    
    $scriptFullPath = $MyInvocation.MyCommand.Path
    if (-not $scriptFullPath) { $scriptFullPath = $PSCommandPath }
    if (-not $scriptFullPath) {
        $scriptFullPath = Join-Path (Get-Location) "uninstall.ps1"
    }
    
    # Build the command - escape single quotes in paths
    $escapedScript = $scriptFullPath -replace "'", "''"
    $escapedInstallDir = $InstallDir -replace "'", "''"
    $escapedDataDir = $DataDir -replace "'", "''"
    
    $cmd = "& '$escapedScript' -InstallDir '$escapedInstallDir' -DataDir '$escapedDataDir' -Elevated"
    if ($KeepData) { $cmd += " -KeepData" }
    if ($BackendOnly) { $cmd += " -BackendOnly" }
    if ($GUIOnly) { $cmd += " -GUIOnly" }
    $cmd += "; Write-Host ''; Write-Host 'Press any key to close...' -ForegroundColor Gray; `$null = `$Host.UI.RawUI.ReadKey('NoEcho,IncludeKeyDown')"
    
    # Encode as base64 to avoid quoting issues
    $bytes = [System.Text.Encoding]::Unicode.GetBytes($cmd)
    $encodedCmd = [Convert]::ToBase64String($bytes)
    
    try {
        $proc = Start-Process powershell.exe -Verb RunAs -ArgumentList "-NoProfile -ExecutionPolicy Bypass -EncodedCommand $encodedCmd" -Wait -PassThru -ErrorAction Stop
        exit $proc.ExitCode
    } catch {
        Write-Host "ERROR: Administrator privileges required." -ForegroundColor Red
        exit 1
    }
}

# Configuration
$AppName = "Grey Multi-Tenant"
$ServiceName = "GreyMultiTenant"
$GUIInstallDir = "$env:ProgramFiles\GreyMultiTenantGUI"
$GUIDataDir = "$env:ProgramData\GreyMultiTenantGUI"

Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Grey Multi-Tenant Platform Uninstaller" -ForegroundColor Cyan
Write-Host "  Windows Unified Edition" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

# Determine what to uninstall
$uninstallBackend = -not $GUIOnly
$uninstallGUI = -not $BackendOnly

# Show what will be removed
Write-Host "This will remove:" -ForegroundColor Yellow
if ($uninstallBackend) { Write-Host "  - Backend service and files" -ForegroundColor White }
if ($uninstallGUI) { Write-Host "  - GUI application" -ForegroundColor White }
if (-not $KeepData) {
    Write-Host "  - All configuration and log data" -ForegroundColor Red
}
Write-Host ""
$confirmation = Read-Host "Are you sure? (yes/no)"
if ($confirmation -ne "yes") {
    Write-Host "Uninstallation cancelled." -ForegroundColor Yellow
    exit 0
}

Write-Host ""
$step = 0
$totalSteps = 0
if ($uninstallBackend) { $totalSteps += 5 }  # Service + scheduled task + PATH + files + data
if ($uninstallGUI) { $totalSteps += 2 }
$totalSteps += 1  # Shortcuts

# ============================================
# BACKEND UNINSTALL
# ============================================
if ($uninstallBackend) {
    Write-Host "--- Removing Backend ---" -ForegroundColor Magenta
    
    $step++
    Write-Host "[$step/$totalSteps] Stopping Windows service..." -ForegroundColor Yellow
    $existingService = Get-Service -Name $ServiceName -ErrorAction SilentlyContinue
    if ($existingService) {
        if ($existingService.Status -eq "Running") {
            Stop-Service -Name $ServiceName -Force -ErrorAction SilentlyContinue
            Start-Sleep -Seconds 3
        }
        sc.exe delete $ServiceName | Out-Null
        Start-Sleep -Seconds 1
        Write-Host "  Service removed." -ForegroundColor Green
    } else {
        Write-Host "  Service not found." -ForegroundColor Gray
    }

    $step++
    Write-Host "[$step/$totalSteps] Removing auto-start scheduled task..." -ForegroundColor Yellow
    $TaskName = "GreyMultiTenantAutoStart"
    $TaskPath = "\Grey Multi-Tenant\"
    $existingTask = Get-ScheduledTask -TaskName $TaskName -TaskPath $TaskPath -ErrorAction SilentlyContinue
    if ($existingTask) {
        Unregister-ScheduledTask -TaskName $TaskName -TaskPath $TaskPath -Confirm:$false -ErrorAction SilentlyContinue
        Write-Host "  Scheduled task removed." -ForegroundColor Green
    } else {
        Write-Host "  Scheduled task not found." -ForegroundColor Gray
    }

    $step++
    Write-Host "[$step/$totalSteps] Removing from PATH..." -ForegroundColor Yellow
    $CurrentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
    $NewPath = ($CurrentPath -split ";" | Where-Object { $_ -notlike "*GreyMultiTenant*" }) -join ";"
    if ($CurrentPath -ne $NewPath) {
        [Environment]::SetEnvironmentVariable("Path", $NewPath, "Machine")
        Write-Host "  Removed from PATH." -ForegroundColor Green
    } else {
        Write-Host "  Not in PATH." -ForegroundColor Gray
    }

    $step++
    Write-Host "[$step/$totalSteps] Removing backend files..." -ForegroundColor Yellow
    if (Test-Path $InstallDir) {
        Remove-Item -Path $InstallDir -Recurse -Force
        Write-Host "  Installation directory removed." -ForegroundColor Green
    } else {
        Write-Host "  Directory not found." -ForegroundColor Gray
    }

    if (-not $KeepData) {
        $step++
        Write-Host "[$step/$totalSteps] Removing backend data..." -ForegroundColor Yellow
        if (Test-Path $DataDir) {
            Remove-Item -Path $DataDir -Recurse -Force
            Write-Host "  Data directory removed." -ForegroundColor Green
        }
    } else {
        $step++
        Write-Host "[$step/$totalSteps] Preserving backend data at $DataDir" -ForegroundColor Yellow
    }
}

# ============================================
# GUI UNINSTALL
# ============================================
if ($uninstallGUI) {
    Write-Host ""
    Write-Host "--- Removing GUI ---" -ForegroundColor Magenta
    
    $step++
    Write-Host "[$step/$totalSteps] Removing GUI files..." -ForegroundColor Yellow
    if (Test-Path $GUIInstallDir) {
        Remove-Item -Path $GUIInstallDir -Recurse -Force
        Write-Host "  GUI directory removed." -ForegroundColor Green
    } else {
        Write-Host "  GUI not found." -ForegroundColor Gray
    }

    if (-not $KeepData) {
        $step++
        Write-Host "[$step/$totalSteps] Removing GUI data..." -ForegroundColor Yellow
        if (Test-Path $GUIDataDir) {
            Remove-Item -Path $GUIDataDir -Recurse -Force
            Write-Host "  GUI data removed." -ForegroundColor Green
        }
    } else {
        $step++
        Write-Host "[$step/$totalSteps] Preserving GUI data at $GUIDataDir" -ForegroundColor Yellow
    }
}

# ============================================
# SHORTCUTS
# ============================================
Write-Host ""
Write-Host "--- Removing Shortcuts ---" -ForegroundColor Magenta

$step++
Write-Host "[$step/$totalSteps] Removing shortcuts..." -ForegroundColor Yellow

# Desktop shortcuts
if ($uninstallBackend) {
    $BackendShortcut = [System.IO.Path]::Combine([Environment]::GetFolderPath("CommonDesktopDirectory"), "$AppName.lnk")
    if (Test-Path $BackendShortcut) { Remove-Item -Path $BackendShortcut -Force }
}
if ($uninstallGUI) {
    $GUIShortcut = [System.IO.Path]::Combine([Environment]::GetFolderPath("CommonDesktopDirectory"), "Grey Multi-Tenant GUI.lnk")
    if (Test-Path $GUIShortcut) { Remove-Item -Path $GUIShortcut -Force }
}

# Start Menu
$StartMenuPath = [System.IO.Path]::Combine([Environment]::GetFolderPath("CommonPrograms"), "Grey Multi-Tenant")
if ($uninstallBackend -and $uninstallGUI) {
    # Remove entire folder
    if (Test-Path $StartMenuPath) {
        Remove-Item -Path $StartMenuPath -Recurse -Force
    }
} else {
    # Remove specific shortcuts
    if ($uninstallBackend -and (Test-Path "$StartMenuPath\$AppName.lnk")) {
        Remove-Item "$StartMenuPath\$AppName.lnk" -Force
    }
    if ($uninstallGUI -and (Test-Path "$StartMenuPath\Grey Multi-Tenant GUI.lnk")) {
        Remove-Item "$StartMenuPath\Grey Multi-Tenant GUI.lnk" -Force
    }
}

Write-Host "  Shortcuts removed." -ForegroundColor Green

# Summary
Write-Host ""
Write-Host "============================================" -ForegroundColor Green
Write-Host "  Uninstallation Complete!" -ForegroundColor Green
Write-Host "============================================" -ForegroundColor Green
Write-Host ""
Write-Host "Removed:" -ForegroundColor Cyan
if ($uninstallBackend) { Write-Host "  - Backend service and files" -ForegroundColor White }
if ($uninstallGUI) { Write-Host "  - GUI application" -ForegroundColor White }
if ($KeepData) {
    Write-Host ""
    Write-Host "Data preserved at:" -ForegroundColor Yellow
    if ($uninstallBackend) { Write-Host "  $DataDir" -ForegroundColor White }
    if ($uninstallGUI) { Write-Host "  $GUIDataDir" -ForegroundColor White }
}
Write-Host ""
