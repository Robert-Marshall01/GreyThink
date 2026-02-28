#Requires -RunAsAdministrator
# ─────────────────────────────────────────────────────────────
# Grey DB — Windows Installer (PowerShell)
# Run as Administrator: Right-click → "Run with PowerShell"
# ─────────────────────────────────────────────────────────────

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# ── Configuration ───────────────────────────────────────────
$AppName        = "GreyDB"
$AppDisplayName = "Grey DB"
$AppVersion     = "1.0.0"
$InstallDir     = "$env:ProgramFiles\GreyDB"
$DataDir        = "$env:ProgramData\GreyDB"
$LogFile        = "$DataDir\install.log"

# ── Helpers ─────────────────────────────────────────────────
function Write-Log {
    param([string]$Message, [string]$Level = "INFO")
    $ts = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $entry = "[$ts] [$Level] $Message"
    if (Test-Path (Split-Path $LogFile -Parent)) {
        Add-Content -Path $LogFile -Value $entry
    }
    switch ($Level) {
        "ERROR"   { Write-Host "  [ERROR] $Message" -ForegroundColor Red }
        "WARN"    { Write-Host "  [WARN]  $Message" -ForegroundColor Yellow }
        "OK"      { Write-Host "  [OK]    $Message" -ForegroundColor Green }
        default   { Write-Host "  [INFO]  $Message" -ForegroundColor Cyan }
    }
}

function Test-Command {
    param([string]$Cmd)
    $null = Get-Command $Cmd -ErrorAction SilentlyContinue
    return $?
}

function Assert-MinVersion {
    param([string]$Actual, [string]$Minimum, [string]$Name)
    # Extract only numeric version parts
    $cleanActual = ($Actual -replace '[^0-9.]', '').Trim('.')
    try {
        $act = [Version]$cleanActual
        $min = [Version]$Minimum
        if ($act -lt $min) {
            Write-Log "$Name version $cleanActual is below minimum $Minimum" "ERROR"
            return $false
        }
    } catch {
        Write-Log "Could not parse $Name version: $Actual" "WARN"
        return $true  # Assume OK if we can't parse
    }
    return $true
}

# ── Banner ──────────────────────────────────────────────────
Write-Host ""
Write-Host "  ╔══════════════════════════════════════════════╗" -ForegroundColor Magenta
Write-Host "  ║         Grey DB — Windows Installer          ║" -ForegroundColor Magenta
Write-Host "  ║              Version $AppVersion                  ║" -ForegroundColor Magenta
Write-Host "  ╚══════════════════════════════════════════════╝" -ForegroundColor Magenta
Write-Host ""

# ── Check Administrator ────────────────────────────────────
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal(
    [Security.Principal.WindowsIdentity]::GetCurrent()
)
if (-not $currentPrincipal.IsInRole(
    [Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "  This installer must be run as Administrator." -ForegroundColor Red
    Write-Host "  Right-click the script and select 'Run with PowerShell (Admin)'." -ForegroundColor Yellow
    exit 1
}

# ── Create directories ─────────────────────────────────────
Write-Host "  [1/8] Creating directories..." -ForegroundColor White
New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
New-Item -ItemType Directory -Path $DataDir -Force | Out-Null
New-Item -ItemType Directory -Path "$DataDir\logs" -Force | Out-Null
New-Item -ItemType Directory -Path "$DataDir\config" -Force | Out-Null
Write-Log "Directories created" "OK"

# ── Check prerequisites ────────────────────────────────────
Write-Host "  [2/8] Checking prerequisites..." -ForegroundColor White

# Node.js
if (-not (Test-Command "node")) {
    Write-Log "Node.js is not installed. Please install Node.js 20+ from https://nodejs.org" "ERROR"
    exit 1
}
$nodeVer = (node --version).TrimStart('v')
if (-not (Assert-MinVersion $nodeVer "20.0.0" "Node.js")) { exit 1 }
Write-Log "Node.js $nodeVer detected" "OK"

# npm
if (-not (Test-Command "npm")) {
    Write-Log "npm is not installed. It should come with Node.js." "ERROR"
    exit 1
}
$npmVer = npm --version
if (-not (Assert-MinVersion $npmVer "9.0.0" "npm")) { exit 1 }
Write-Log "npm $npmVer detected" "OK"

# Docker
if (-not (Test-Command "docker")) {
    Write-Log "Docker is not installed. Please install Docker Desktop from https://docker.com" "ERROR"
    exit 1
}
Write-Log "Docker detected" "OK"

# Git (optional, warn only)
if (-not (Test-Command "git")) {
    Write-Log "Git is not installed — optional but recommended" "WARN"
} else {
    Write-Log "Git detected" "OK"
}

# ── Locate source ──────────────────────────────────────────
Write-Host "  [3/8] Locating source files..." -ForegroundColor White

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$SourceDir = Split-Path -Parent $ScriptDir

if (-not (Test-Path "$SourceDir\package.json")) {
    Write-Log "Cannot find Grey DB source at $SourceDir" "ERROR"
    Write-Log "Ensure this script is in the 'installers' folder inside the Grey DB project." "ERROR"
    exit 1
}
Write-Log "Source found at $SourceDir" "OK"

# ── Copy files to install directory ─────────────────────────
Write-Host "  [4/8] Copying files to $InstallDir..." -ForegroundColor White

# Use robocopy for reliable copy (exit codes 0-7 are success)
$robocopyArgs = @(
    $SourceDir, $InstallDir,
    "/MIR",       # Mirror directory tree
    "/XD", "node_modules", ".git",  # Exclude these dirs
    "/XF", "*.log",
    "/NFL", "/NDL", "/NJH", "/NJS", "/NP"   # Quiet output
)
$robocopyResult = & robocopy @robocopyArgs
if ($LASTEXITCODE -gt 7) {
    Write-Log "File copy failed (robocopy exit code $LASTEXITCODE)" "ERROR"
    exit 1
}

# Clean stale build artifacts so tsc does a full rebuild
Get-ChildItem -Path $InstallDir -Filter "*.tsbuildinfo" -Recurse -ErrorAction SilentlyContinue | Remove-Item -Force
Get-ChildItem -Path "$InstallDir\packages\*\dist" -Directory -ErrorAction SilentlyContinue | Remove-Item -Recurse -Force
Get-ChildItem -Path "$InstallDir\examples\*\dist" -Directory -ErrorAction SilentlyContinue | Remove-Item -Recurse -Force

Write-Log "Files copied successfully" "OK"

# ── Install npm dependencies ───────────────────────────────
Write-Host "  [5/8] Installing npm dependencies (this may take a few minutes)..." -ForegroundColor White

Push-Location $InstallDir
try {
    # Remove stale node_modules from any prior install attempt
    if (Test-Path "node_modules") { Remove-Item -Recurse -Force "node_modules" }
    $env:NODE_ENV = "development"
    & npm install --include=dev --no-audit --no-fund 2>&1 | Out-Null
    if ($LASTEXITCODE -ne 0) {
        Write-Log "npm install failed" "ERROR"
        exit 1
    }
    Write-Log "npm dependencies installed" "OK"
} finally {
    Pop-Location
}

# ── Build the project ──────────────────────────────────────
Write-Host "  [6/8] Building Grey DB..." -ForegroundColor White

Push-Location $InstallDir
try {
    # Build in dependency order: core must compile before packages that depend on it
    $workspaces = @("packages/core", "packages/server", "packages/cli", "packages/ui", "examples/crm")
    foreach ($ws in $workspaces) {
        if (Test-Path "$ws\package.json") {
            $pkg = Get-Content "$ws\package.json" -Raw
            if ($pkg -match '"build"') {
                Write-Log "Building $ws..." "INFO"
                & npm run build --workspace="$ws" 2>&1 | Out-Null
                if ($LASTEXITCODE -ne 0) {
                    Write-Log "Build failed for $ws" "ERROR"
                    exit 1
                }
            }
        }
    }
        Write-Log "Build failed" "ERROR"
        exit 1
    }
    Write-Log "Build completed" "OK"
} finally {
    Pop-Location
}

# ── Create environment config ──────────────────────────────
Write-Host "  [7/8] Configuring environment..." -ForegroundColor White

$envFile = "$DataDir\config\.env"
if (-not (Test-Path $envFile)) {
    $envContent = @"
# Grey DB Configuration
GREY_DB_HOST=localhost
GREY_DB_PORT=5432
GREY_DB_DATABASE=greydb
GREY_DB_USER=greydb
GREY_DB_PASSWORD=greydb_secret
PORT=4000
VITE_API_URL=http://localhost:4000
NODE_ENV=production
"@
    Set-Content -Path $envFile -Value $envContent
    Write-Log "Environment config created at $envFile" "OK"
} else {
    Write-Log "Environment config already exists — skipping" "OK"
}

# Copy .env into install dir for runtime
Copy-Item -Path $envFile -Destination "$InstallDir\.env" -Force

# ── Register CLI and PATH ──────────────────────────────────
Write-Host "  [8/8] Registering CLI and services..." -ForegroundColor White

# Create a launcher batch file for the CLI
$cliBat = "$InstallDir\greydb.cmd"
$cliContent = @"
@echo off
node "%~dp0packages\cli\dist\index.js" %*
"@
Set-Content -Path $cliBat -Value $cliContent

# Add to system PATH if not already there
$currentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
if ($currentPath -notlike "*$InstallDir*") {
    [Environment]::SetEnvironmentVariable(
        "Path",
        "$currentPath;$InstallDir",
        "Machine"
    )
    Write-Log "Added $InstallDir to system PATH" "OK"
} else {
    Write-Log "$InstallDir already in PATH" "OK"
}

# ── Write uninstall info to registry ────────────────────────
$uninstallKey = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$AppName"
New-Item -Path $uninstallKey -Force | Out-Null
Set-ItemProperty -Path $uninstallKey -Name "DisplayName" -Value $AppDisplayName
Set-ItemProperty -Path $uninstallKey -Name "DisplayVersion" -Value $AppVersion
Set-ItemProperty -Path $uninstallKey -Name "Publisher" -Value "GreyThink"
Set-ItemProperty -Path $uninstallKey -Name "InstallLocation" -Value $InstallDir
Set-ItemProperty -Path $uninstallKey -Name "UninstallString" -Value "powershell.exe -ExecutionPolicy Bypass -File `"$InstallDir\installers\uninstall.ps1`""
Set-ItemProperty -Path $uninstallKey -Name "NoModify" -Value 1 -Type DWord
Set-ItemProperty -Path $uninstallKey -Name "NoRepair" -Value 1 -Type DWord
Write-Log "Registry entries created" "OK"

# ── Done ────────────────────────────────────────────────────
Write-Host ""
Write-Host "  ╔══════════════════════════════════════════════╗" -ForegroundColor Green
Write-Host "  ║       Grey DB installed successfully!        ║" -ForegroundColor Green
Write-Host "  ╚══════════════════════════════════════════════╝" -ForegroundColor Green
Write-Host ""
Write-Host "  Install directory:  $InstallDir" -ForegroundColor White
Write-Host "  Data directory:     $DataDir" -ForegroundColor White
Write-Host "  Config file:        $envFile" -ForegroundColor White
Write-Host ""
Write-Host "  Next steps:" -ForegroundColor Yellow
Write-Host "    1. Open a NEW terminal (so PATH updates take effect)" -ForegroundColor White
Write-Host "    2. Start PostgreSQL:  docker compose -f `"$InstallDir\docker-compose.yml`" up -d postgres" -ForegroundColor White
Write-Host "    3. Start the server:  cd `"$InstallDir`" && npm run dev:server" -ForegroundColor White
Write-Host "    4. Start the UI:      cd `"$InstallDir`" && npm run dev:ui" -ForegroundColor White
Write-Host "    5. Use the CLI:       greydb --help" -ForegroundColor White
Write-Host ""
Write-Host "  To uninstall, run:" -ForegroundColor Yellow
Write-Host "    powershell -ExecutionPolicy Bypass -File `"$InstallDir\installers\uninstall.ps1`"" -ForegroundColor White
Write-Host ""

Write-Log "Installation completed successfully" "OK"
