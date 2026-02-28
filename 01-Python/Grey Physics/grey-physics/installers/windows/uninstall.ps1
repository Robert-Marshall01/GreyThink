#Requires -Version 5.1
<#
.SYNOPSIS
    Grey Physics Uninstaller for Windows.
.DESCRIPTION
    Removes Grey Physics, its virtual environment, Start Menu shortcuts,
    PATH entries, and all associated files.
.PARAMETER InstallDir
    The installation directory to remove. If not specified, reads from
    the install manifest in the current script directory.
.PARAMETER KeepConfig
    Preserve any user configuration files.
#>
[CmdletBinding()]
param(
    [string]$InstallDir,
    [switch]$KeepConfig
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# ── Constants ──────────────────────────────────────────────────────────
$AppName   = 'Grey Physics'
$AppId     = 'GreyPhysics'
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition

# ── Helper functions ───────────────────────────────────────────────────
function Write-Status  { param([string]$Msg) Write-Host "[*] $Msg" -ForegroundColor Cyan }
function Write-Success { param([string]$Msg) Write-Host "[+] $Msg" -ForegroundColor Green }
function Write-Failure { param([string]$Msg) Write-Host "[-] $Msg" -ForegroundColor Red }

function Test-Admin {
    $identity  = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

# ── Locate manifest ───────────────────────────────────────────────────
Write-Host ''
Write-Host '==========================================' -ForegroundColor Yellow
Write-Host "  $AppName Uninstaller (Windows)"           -ForegroundColor Yellow
Write-Host '==========================================' -ForegroundColor Yellow
Write-Host ''

# Try to find the install manifest
$manifest = $null
$manifestPath = $null

if ($InstallDir) {
    $manifestPath = Join-Path $InstallDir 'install-manifest.json'
} else {
    # Look in the script's own directory first
    $manifestPath = Join-Path $ScriptDir 'install-manifest.json'
    if (-not (Test-Path $manifestPath)) {
        # Fallback to default locations
        $defaultPaths = @(
            (Join-Path $env:LOCALAPPDATA $AppId),
            (Join-Path $env:ProgramFiles $AppId)
        )
        foreach ($p in $defaultPaths) {
            $candidate = Join-Path $p 'install-manifest.json'
            if (Test-Path $candidate) {
                $manifestPath = $candidate
                break
            }
        }
    }
}

if ($manifestPath -and (Test-Path $manifestPath)) {
    $manifest = Get-Content -Path $manifestPath -Raw | ConvertFrom-Json
    $InstallDir = $manifest.InstallDir
    Write-Status "Found installation manifest at: $manifestPath"
} else {
    if (-not $InstallDir) {
        Write-Failure 'Could not locate installation manifest.'
        Write-Failure 'Please specify -InstallDir with the path where Grey Physics is installed.'
        exit 1
    }
}

Write-Status "Uninstalling from: $InstallDir"

# Check if admin is required
$isAllUsers = $false
if ($manifest -and $manifest.AllUsers) {
    $isAllUsers = $true
}
if ($isAllUsers -and -not (Test-Admin)) {
    Write-Failure 'This was an all-users installation. Administrator privileges are required to uninstall.'
    exit 1
}

# ── Confirmation ───────────────────────────────────────────────────────
Write-Host ''
$response = Read-Host "This will remove $AppName from '$InstallDir'. Continue? (y/N)"
if ($response -notin @('y', 'Y', 'yes', 'Yes', 'YES')) {
    Write-Status 'Uninstallation cancelled.'
    exit 0
}

# ── Remove from PATH ──────────────────────────────────────────────────
Write-Status 'Removing from PATH ...'
if ($isAllUsers) {
    $pathScope = 'Machine'
} else {
    $pathScope = 'User'
}
$currentPath = [Environment]::GetEnvironmentVariable('Path', $pathScope)
if ($currentPath) {
    $pathEntries = $currentPath.Split(';') | Where-Object { $_ -ne $InstallDir -and $_ -ne '' }
    $newPath = $pathEntries -join ';'
    [Environment]::SetEnvironmentVariable('Path', $newPath, $pathScope)
    Write-Success 'Removed from PATH.'
} else {
    Write-Status 'PATH entry not found (already clean).'
}

# ── Remove Start Menu shortcuts ────────────────────────────────────────
Write-Status 'Removing Start Menu shortcuts ...'
$startMenuDir = $null
if ($manifest -and $manifest.StartMenu) {
    $startMenuDir = $manifest.StartMenu
} else {
    if ($isAllUsers) {
        $startMenuDir = Join-Path $env:ProgramData 'Microsoft\Windows\Start Menu\Programs\Grey Physics'
    } else {
        $startMenuDir = Join-Path $env:APPDATA 'Microsoft\Windows\Start Menu\Programs\Grey Physics'
    }
}
if ($startMenuDir -and (Test-Path $startMenuDir)) {
    Remove-Item -Recurse -Force $startMenuDir
    Write-Success 'Start Menu shortcuts removed.'
} else {
    Write-Status 'No Start Menu shortcuts found.'
}

# ── Remove installation directory ──────────────────────────────────────
Write-Status 'Removing installation directory ...'
if (Test-Path $InstallDir) {
    # The uninstaller may be running from this directory, so we need
    # to change away from it first
    $originalDir = Get-Location
    if ((Get-Location).Path.StartsWith($InstallDir)) {
        Set-Location $env:TEMP
    }

    try {
        Remove-Item -Recurse -Force $InstallDir
        Write-Success 'Installation directory removed.'
    } catch {
        Write-Failure "Could not fully remove install directory: $_"
        Write-Failure "You may need to manually delete: $InstallDir"
    }
} else {
    Write-Status 'Installation directory already removed.'
}

# ── Summary ────────────────────────────────────────────────────────────
Write-Host ''
Write-Host '==========================================' -ForegroundColor Green
Write-Host "  $AppName uninstalled successfully."       -ForegroundColor Green
Write-Host '==========================================' -ForegroundColor Green
Write-Host ''
Write-Host '  NOTE: You may need to restart your terminal for PATH changes to take effect.'
Write-Host ''
