#Requires -RunAsAdministrator
# ─────────────────────────────────────────────────────────────
# Grey DB — Windows Uninstaller (PowerShell)
# Run as Administrator: Right-click → "Run with PowerShell"
# ─────────────────────────────────────────────────────────────

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# ── Configuration ───────────────────────────────────────────
$AppName    = "GreyDB"
$InstallDir = "$env:ProgramFiles\GreyDB"
$DataDir    = "$env:ProgramData\GreyDB"

# ── Banner ──────────────────────────────────────────────────
Write-Host ""
Write-Host "  ╔══════════════════════════════════════════════╗" -ForegroundColor Magenta
Write-Host "  ║        Grey DB — Windows Uninstaller         ║" -ForegroundColor Magenta
Write-Host "  ╚══════════════════════════════════════════════╝" -ForegroundColor Magenta
Write-Host ""

# ── Check Administrator ────────────────────────────────────
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal(
    [Security.Principal.WindowsIdentity]::GetCurrent()
)
if (-not $currentPrincipal.IsInRole(
    [Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "  This uninstaller must be run as Administrator." -ForegroundColor Red
    exit 1
}

# ── Confirm ─────────────────────────────────────────────────
Write-Host "  This will remove Grey DB from your system." -ForegroundColor Yellow
Write-Host ""
Write-Host "  Install directory: $InstallDir" -ForegroundColor White
Write-Host "  Data directory:    $DataDir" -ForegroundColor White
Write-Host ""

$response = Read-Host "  Are you sure you want to uninstall Grey DB? (y/N)"
if ($response -notin @("y", "Y", "yes", "Yes", "YES")) {
    Write-Host "  Uninstall cancelled." -ForegroundColor Yellow
    exit 0
}

# ── Stop Docker containers ─────────────────────────────────
Write-Host ""
Write-Host "  [1/5] Stopping Docker containers..." -ForegroundColor White

if (Get-Command "docker" -ErrorAction SilentlyContinue) {
    $containers = @("greydb-postgres", "greydb-server", "greydb-ui")
    foreach ($container in $containers) {
        $running = docker ps -q --filter "name=$container" 2>$null
        if ($running) {
            docker stop $container 2>$null | Out-Null
            docker rm $container 2>$null | Out-Null
            Write-Host "    Stopped and removed container: $container" -ForegroundColor Green
        }
    }

    # Ask about Docker volumes
    $removeVolumes = Read-Host "  Remove Docker volumes (database data will be lost)? (y/N)"
    if ($removeVolumes -in @("y", "Y", "yes", "Yes", "YES")) {
        docker volume rm grey-db_pgdata 2>$null | Out-Null
        docker volume rm greydb_pgdata 2>$null | Out-Null
        Write-Host "    Docker volumes removed" -ForegroundColor Green
    }
} else {
    Write-Host "    Docker not found — skipping container cleanup" -ForegroundColor Yellow
}

# ── Remove from PATH ───────────────────────────────────────
Write-Host "  [2/5] Removing from system PATH..." -ForegroundColor White

$currentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
if ($currentPath -like "*$InstallDir*") {
    $newPath = ($currentPath -split ";" | Where-Object { $_ -ne $InstallDir }) -join ";"
    [Environment]::SetEnvironmentVariable("Path", $newPath, "Machine")
    Write-Host "    Removed $InstallDir from PATH" -ForegroundColor Green
} else {
    Write-Host "    Not found in PATH — skipping" -ForegroundColor Yellow
}

# ── Remove registry entries ────────────────────────────────
Write-Host "  [3/5] Removing registry entries..." -ForegroundColor White

$uninstallKey = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\$AppName"
if (Test-Path $uninstallKey) {
    Remove-Item -Path $uninstallKey -Recurse -Force
    Write-Host "    Registry entries removed" -ForegroundColor Green
} else {
    Write-Host "    No registry entries found — skipping" -ForegroundColor Yellow
}

# ── Remove install directory ───────────────────────────────
Write-Host "  [4/5] Removing installation files..." -ForegroundColor White

if (Test-Path $InstallDir) {
    Remove-Item -Path $InstallDir -Recurse -Force
    Write-Host "    Removed $InstallDir" -ForegroundColor Green
} else {
    Write-Host "    Install directory not found — skipping" -ForegroundColor Yellow
}

# ── Remove data directory ──────────────────────────────────
Write-Host "  [5/5] Cleaning up data directory..." -ForegroundColor White

if (Test-Path $DataDir) {
    $removeData = Read-Host "  Remove data directory ($DataDir)? Config and logs will be lost. (y/N)"
    if ($removeData -in @("y", "Y", "yes", "Yes", "YES")) {
        Remove-Item -Path $DataDir -Recurse -Force
        Write-Host "    Removed $DataDir" -ForegroundColor Green
    } else {
        Write-Host "    Data directory preserved at $DataDir" -ForegroundColor Yellow
    }
} else {
    Write-Host "    Data directory not found — skipping" -ForegroundColor Yellow
}

# ── Done ────────────────────────────────────────────────────
Write-Host ""
Write-Host "  ╔══════════════════════════════════════════════╗" -ForegroundColor Green
Write-Host "  ║     Grey DB uninstalled successfully!        ║" -ForegroundColor Green
Write-Host "  ╚══════════════════════════════════════════════╝" -ForegroundColor Green
Write-Host ""
Write-Host "  Please open a new terminal for PATH changes to take effect." -ForegroundColor Yellow
Write-Host ""
