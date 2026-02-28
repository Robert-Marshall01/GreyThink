#Requires -Version 5.1
<#
.SYNOPSIS
    Grey Physics Installer for Windows.
.DESCRIPTION
    Installs Grey Physics into an isolated virtual environment, creates
    Start Menu shortcuts, PATH entries, and registers an uninstaller.
.PARAMETER InstallDir
    Target installation directory. Defaults to %LOCALAPPDATA%\GreyPhysics.
.PARAMETER AllUsers
    Install for all users (requires Administrator privileges).
.PARAMETER Extras
    Comma-separated optional extras: gpu, viz, ide, all, dev.
#>
[CmdletBinding()]
param(
    [string]$InstallDir,
    [switch]$AllUsers,
    [string]$Extras
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# ── Constants ──────────────────────────────────────────────────────────
$AppName        = 'Grey Physics'
$AppId          = 'GreyPhysics'
$PackageName    = 'grey-physics'
$MinPythonMajor = 3
$MinPythonMinor = 10
$ScriptDir      = Split-Path -Parent $MyInvocation.MyCommand.Definition
$ProjectRoot    = (Resolve-Path (Join-Path $ScriptDir '..\..')).Path

# ── Helper functions ───────────────────────────────────────────────────
function Write-Status  { param([string]$Msg) Write-Host "[*] $Msg" -ForegroundColor Cyan }
function Write-Success { param([string]$Msg) Write-Host "[+] $Msg" -ForegroundColor Green }
function Write-Failure { param([string]$Msg) Write-Host "[-] $Msg" -ForegroundColor Red }

function Test-Admin {
    $identity  = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    return $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
}

function Find-Python {
    # Search for python3, python, py launcher
    foreach ($cmd in @('python3', 'python', 'py')) {
        $found = Get-Command $cmd -ErrorAction SilentlyContinue
        if ($found) {
            $versionOutput = & $found.Source --version 2>&1
            if ($versionOutput -match 'Python (\d+)\.(\d+)') {
                $major = [int]$Matches[1]
                $minor = [int]$Matches[2]
                if ($major -gt $MinPythonMajor -or ($major -eq $MinPythonMajor -and $minor -ge $MinPythonMinor)) {
                    return $found.Source
                }
            }
        }
    }
    return $null
}

# ── Pre-flight checks ─────────────────────────────────────────────────
Write-Host ''
Write-Host '==========================================' -ForegroundColor Yellow
Write-Host "  $AppName Installer (Windows)"             -ForegroundColor Yellow
Write-Host '==========================================' -ForegroundColor Yellow
Write-Host ''

if ($AllUsers -and -not (Test-Admin)) {
    Write-Failure 'All-users installation requires Administrator privileges.'
    Write-Failure 'Please re-run this script from an elevated PowerShell prompt.'
    exit 1
}

# Resolve install directory
if (-not $InstallDir) {
    if ($AllUsers) {
        $InstallDir = Join-Path $env:ProgramFiles $AppId
    } else {
        $InstallDir = Join-Path $env:LOCALAPPDATA $AppId
    }
}

Write-Status "Install directory: $InstallDir"

# Find Python
Write-Status 'Locating Python >= 3.10 ...'
$PythonExe = Find-Python
if (-not $PythonExe) {
    Write-Failure "Python >= $MinPythonMajor.$MinPythonMinor not found on PATH."
    Write-Failure 'Please install Python from https://www.python.org/downloads/ and try again.'
    exit 1
}
$pyVersion = & $PythonExe --version 2>&1
Write-Success "Found: $pyVersion at $PythonExe"

# ── Installation ───────────────────────────────────────────────────────

# 1. Create install directory
Write-Status "Creating install directory ..."
if (-not (Test-Path $InstallDir)) {
    New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
}

# 2. Create virtual environment
$VenvDir = Join-Path $InstallDir 'venv'
if (Test-Path $VenvDir) {
    Write-Status 'Removing existing virtual environment ...'
    Remove-Item -Recurse -Force $VenvDir
}
Write-Status 'Creating virtual environment ...'
& $PythonExe -m venv $VenvDir
if ($LASTEXITCODE -ne 0) {
    Write-Failure 'Failed to create virtual environment.'
    exit 1
}
Write-Success 'Virtual environment created.'

$VenvPython = Join-Path $VenvDir 'Scripts\python.exe'
$VenvPip    = Join-Path $VenvDir 'Scripts\pip.exe'

# 3. Upgrade pip
Write-Status 'Upgrading pip ...'
& $VenvPython -m pip install --upgrade pip --quiet
if ($LASTEXITCODE -ne 0) {
    Write-Failure 'Failed to upgrade pip.'
    exit 1
}

# 4. Install the package
$pipArgs = @('install', $ProjectRoot)
if ($Extras) {
    $pipArgs = @('install', "$ProjectRoot[$Extras]")
}
$pipArgs += '--quiet'

Write-Status "Installing $PackageName ..."
& $VenvPip install @pipArgs
if ($LASTEXITCODE -ne 0) {
    Write-Failure "Failed to install $PackageName."
    exit 1
}

# Verify the installation
& $VenvPython -c "import grey_physics; print(f'Version {grey_physics.__version__}')" 2>&1 | Out-Null
if ($LASTEXITCODE -ne 0) {
    Write-Failure 'Package installed but import verification failed.'
    exit 1
}
Write-Success "$PackageName installed successfully."

# 5. Create launcher script
$LauncherPath = Join-Path $InstallDir 'grey-physics.cmd'
$launcherContent = @"
@echo off
REM Grey Physics GUI launcher
"%~dp0venv\Scripts\python.exe" -m grey_physics %*
"@
Set-Content -Path $LauncherPath -Value $launcherContent -Encoding ASCII
Write-Success 'Launcher script created.'

# 7. Add to PATH
Write-Status 'Updating PATH ...'
if ($AllUsers) {
    $pathScope = 'Machine'
} else {
    $pathScope = 'User'
}
$currentPath = [Environment]::GetEnvironmentVariable('Path', $pathScope)
if ($currentPath -and $currentPath.Split(';') -contains $InstallDir) {
    Write-Status 'Install directory already on PATH.'
} else {
    [Environment]::SetEnvironmentVariable('Path', "$currentPath;$InstallDir", $pathScope)
    Write-Success 'Added install directory to PATH.'
}

# 8. Create Start Menu shortcut
Write-Status 'Creating Start Menu shortcut ...'
if ($AllUsers) {
    $startMenuDir = Join-Path $env:ProgramData 'Microsoft\Windows\Start Menu\Programs\Grey Physics'
} else {
    $startMenuDir = Join-Path $env:APPDATA 'Microsoft\Windows\Start Menu\Programs\Grey Physics'
}
if (-not (Test-Path $startMenuDir)) {
    New-Item -ItemType Directory -Path $startMenuDir -Force | Out-Null
}
$shell = New-Object -ComObject WScript.Shell
$shortcut = $shell.CreateShortcut((Join-Path $startMenuDir 'Grey Physics.lnk'))
$shortcut.TargetPath = $LauncherPath
$shortcut.WorkingDirectory = [Environment]::GetFolderPath('Desktop')
$shortcut.Description = 'Grey Physics — Physics & Mathematics IDE'
$shortcut.Save()
Write-Success 'Start Menu shortcut created.'

# 9. Write installation manifest (used by uninstaller)
$manifest = @{
    AppName     = $AppName
    PackageName = $PackageName
    InstallDir  = $InstallDir
    VenvDir     = $VenvDir
    AllUsers    = $AllUsers.IsPresent
    StartMenu   = $startMenuDir
    InstalledAt = (Get-Date -Format 'o')
    PythonExe   = $PythonExe
}
$manifestPath = Join-Path $InstallDir 'install-manifest.json'
$manifest | ConvertTo-Json -Depth 4 | Set-Content -Path $manifestPath -Encoding UTF8
Write-Success 'Installation manifest written.'

# 10. Copy uninstaller to install directory
$uninstallerSrc = Join-Path $ScriptDir 'uninstall.ps1'
if (Test-Path $uninstallerSrc) {
    Copy-Item -Path $uninstallerSrc -Destination (Join-Path $InstallDir 'uninstall.ps1') -Force
    # Also copy the batch wrapper
    $uninstBatSrc = Join-Path $ScriptDir 'uninstall.bat'
    if (Test-Path $uninstBatSrc) {
        Copy-Item -Path $uninstBatSrc -Destination (Join-Path $InstallDir 'uninstall.bat') -Force
    }
    Write-Success 'Uninstaller copied to install directory.'
}

# ── Summary ────────────────────────────────────────────────────────────
Write-Host ''
Write-Host '==========================================' -ForegroundColor Green
Write-Host "  $AppName installed successfully!"         -ForegroundColor Green
Write-Host '==========================================' -ForegroundColor Green
Write-Host ''
Write-Host "  Install directory : $InstallDir"
Write-Host "  Python            : $VenvPython"
Write-Host "  Launcher          : $LauncherPath"
Write-Host ''
Write-Host '  Usage:'
Write-Host '    grey-physics --version'
Write-Host ''
Write-Host '  To uninstall, run:'
Write-Host "    $InstallDir\uninstall.bat"
Write-Host ''
Write-Host '  NOTE: You may need to restart your terminal for PATH changes to take effect.'
Write-Host ''
