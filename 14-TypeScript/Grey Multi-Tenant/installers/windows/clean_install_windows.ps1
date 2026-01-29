<#
.SYNOPSIS
    Grey Multi-Tenant Platform - Windows Clean Install

.DESCRIPTION
    Performs a complete uninstall followed by a fresh install.
    This ensures no leftover files or configurations interfere with the new installation.
    Automatically prompts for elevation if not run as Administrator.

.EXAMPLE
    .\clean_install_windows.ps1
    .\clean_install_windows.ps1 -KeepData

.NOTES
    Can be run from any terminal - will auto-elevate with UAC prompt.
#>

param(
    [string]$InstallDir = "$env:ProgramFiles\GreyMultiTenant",
    [string]$DataDir = "$env:ProgramData\GreyMultiTenant",
    [switch]$KeepData = $false,
    [switch]$SkipService = $false,
    [switch]$Force = $false,
    [switch]$Elevated = $false
)

$ErrorActionPreference = "Stop"

# Self-elevation: Request UAC prompt if not running as Administrator
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "Requesting administrator privileges..." -ForegroundColor Yellow
    
    # Get the current script path
    $scriptFullPath = $MyInvocation.MyCommand.Path
    if (-not $scriptFullPath) {
        $scriptFullPath = $PSCommandPath
    }
    
    # Build command to run elevated
    $command = "Set-Location -Path '$(Split-Path -Parent $scriptFullPath)'; "
    $command += "& '$scriptFullPath' -InstallDir '$InstallDir' -DataDir '$DataDir' -Elevated"
    if ($KeepData) { $command += " -KeepData" }
    if ($SkipService) { $command += " -SkipService" }
    if ($Force) { $command += " -Force" }
    $command += "; Write-Host ''; Write-Host 'Press any key to close...' -ForegroundColor Gray; `$null = `$Host.UI.RawUI.ReadKey('NoEcho,IncludeKeyDown')"
    
    try {
        Start-Process powershell.exe -Verb RunAs -ArgumentList "-NoProfile -ExecutionPolicy Bypass -Command `"$command`"" -Wait
        exit 0
    } catch {
        if ($_.Exception.Message -match "canceled by the user") {
            Write-Host "UAC prompt was declined." -ForegroundColor Yellow
        } else {
            Write-Host "ERROR: Administrator privileges required." -ForegroundColor Red
            Write-Host $_.Exception.Message -ForegroundColor Gray
        }
        Write-Host "Please right-click the script and select 'Run with PowerShell' as Administrator." -ForegroundColor Yellow
        exit 1
    }
}

# Get script directory for finding other scripts
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if (-not $ScriptDir) { $ScriptDir = Split-Path -Parent $PSCommandPath }

Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Grey Multi-Tenant Platform" -ForegroundColor Cyan
Write-Host "  Clean Install - Windows" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

# Confirm clean install
if (-not $Force) {
    Write-Host "This will UNINSTALL and REINSTALL Grey Multi-Tenant Platform." -ForegroundColor Yellow
    if (-not $KeepData) {
        Write-Host "WARNING: All configuration and log data will be deleted!" -ForegroundColor Red
    } else {
        Write-Host "Configuration and logs will be preserved." -ForegroundColor Green
    }
    Write-Host ""
    $confirmation = Read-Host "Are you sure you want to continue? (yes/no)"
    if ($confirmation -ne "yes") {
        Write-Host "Clean install cancelled." -ForegroundColor Yellow
        exit 0
    }
}

# Phase 1: Uninstall
Write-Host ""
Write-Host "========== PHASE 1: UNINSTALL ==========" -ForegroundColor Magenta
Write-Host ""

# Stop and remove service
$ServiceName = "GreyMultiTenant"
$existingService = Get-Service -Name $ServiceName -ErrorAction SilentlyContinue
if ($existingService) {
    Write-Host "Stopping service..." -ForegroundColor Yellow
    if ($existingService.Status -eq "Running") {
        Stop-Service -Name $ServiceName -Force -ErrorAction SilentlyContinue
        Start-Sleep -Seconds 3
    }
    Write-Host "Removing service..." -ForegroundColor Yellow
    sc.exe delete $ServiceName | Out-Null
    Start-Sleep -Seconds 2
    Write-Host "  Service removed." -ForegroundColor Green
}

# Kill any running processes
Get-Process -Name "grey-core-api" -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue
Start-Sleep -Seconds 1

# Remove shortcuts
$AppName = "Grey Multi-Tenant"
$DesktopShortcut = [System.IO.Path]::Combine([Environment]::GetFolderPath("CommonDesktopDirectory"), "$AppName.lnk")
if (Test-Path $DesktopShortcut) { Remove-Item -Path $DesktopShortcut -Force }

$StartMenuPath = [System.IO.Path]::Combine([Environment]::GetFolderPath("CommonPrograms"), "Grey Multi-Tenant")
if (Test-Path $StartMenuPath) { Remove-Item -Path $StartMenuPath -Recurse -Force }

# Remove from PATH
$CurrentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
$NewPath = ($CurrentPath -split ";" | Where-Object { $_ -notlike "*GreyMultiTenant*" }) -join ";"
if ($CurrentPath -ne $NewPath) {
    [Environment]::SetEnvironmentVariable("Path", $NewPath, "Machine")
}

# Remove installation files
if (Test-Path $InstallDir) {
    Write-Host "Removing installation directory..." -ForegroundColor Yellow
    Remove-Item -Path $InstallDir -Recurse -Force
    Write-Host "  Installation directory removed." -ForegroundColor Green
}

# Remove data if not preserving
if (-not $KeepData) {
    if (Test-Path $DataDir) {
        Write-Host "Removing data directory..." -ForegroundColor Yellow
        Remove-Item -Path $DataDir -Recurse -Force
        Write-Host "  Data directory removed." -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "Uninstall complete." -ForegroundColor Green
Write-Host ""

# Phase 2: Fresh Install
Write-Host "========== PHASE 2: INSTALL ==========" -ForegroundColor Magenta
Write-Host ""

# Find install script (use $ScriptDir set at top of script)
$InstallScript = Join-Path $ScriptDir "install.ps1"

if (-not (Test-Path $InstallScript)) {
    Write-Host "ERROR: Install script not found: $InstallScript" -ForegroundColor Red
    exit 1
}

# Run install
$installArgs = @{
    InstallDir = $InstallDir
    DataDir = $DataDir
}
if ($SkipService) {
    $installArgs["SkipService"] = $true
}

& $InstallScript @installArgs

# Verification
Write-Host ""
Write-Host "========== VERIFICATION ==========" -ForegroundColor Magenta
Write-Host ""

# Check service
$service = Get-Service -Name $ServiceName -ErrorAction SilentlyContinue
if ($service) {
    Write-Host "Service installed: OK" -ForegroundColor Green
    if (-not $SkipService) {
        Write-Host "Starting service..." -ForegroundColor Yellow
        Start-Service -Name $ServiceName -ErrorAction SilentlyContinue
        Start-Sleep -Seconds 3
        $service = Get-Service -Name $ServiceName
        if ($service.Status -eq "Running") {
            Write-Host "Service running: OK" -ForegroundColor Green
        } else {
            Write-Host "Service status: $($service.Status)" -ForegroundColor Yellow
        }
    }
} else {
    Write-Host "Service not installed (expected if --SkipService)" -ForegroundColor Yellow
}

# Check binary
if (Test-Path "$InstallDir\bin\grey-core-api.exe") {
    Write-Host "Binary installed: OK" -ForegroundColor Green
} else {
    Write-Host "Binary not found!" -ForegroundColor Red
}

# Check config
if (Test-Path "$InstallDir\config\.env") {
    Write-Host "Configuration: OK" -ForegroundColor Green
} else {
    Write-Host "Configuration not found!" -ForegroundColor Red
}

Write-Host ""
Write-Host "============================================" -ForegroundColor Green
Write-Host "  Clean Install Complete!" -ForegroundColor Green
Write-Host "============================================" -ForegroundColor Green
Write-Host ""

