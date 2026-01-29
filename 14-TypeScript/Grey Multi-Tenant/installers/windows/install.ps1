<#
.SYNOPSIS
    Grey Multi-Tenant Platform - Windows Unified Installer

.DESCRIPTION
    Installs both the Grey Multi-Tenant backend (Windows service) and GUI desktop application.
    Automatically prompts for elevation if not run as Administrator.

.PARAMETER InstallDir
    Installation directory for the backend service (default: C:\Program Files\GreyMultiTenant)

.PARAMETER SkipBackend
    Skip backend/service installation, install GUI only

.PARAMETER SkipGUI
    Skip GUI installation, install backend only

.PARAMETER SkipService
    Install backend but skip Windows service registration

.NOTES
    Can be run from any terminal - will auto-elevate with UAC prompt.
#>

param(
    [string]$InstallDir = "$env:ProgramFiles\GreyMultiTenant",
    [string]$DataDir = "$env:ProgramData\GreyMultiTenant",
    [switch]$SkipBackend = $false,
    [switch]$SkipGUI = $false,
    [switch]$SkipService = $false,
    [switch]$Elevated = $false
)

$ErrorActionPreference = "Stop"

# Self-elevation: Request UAC prompt if not running as Administrator
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "Requesting administrator privileges..." -ForegroundColor Yellow
    
    $scriptFullPath = $MyInvocation.MyCommand.Path
    if (-not $scriptFullPath) { $scriptFullPath = $PSCommandPath }
    if (-not $scriptFullPath) {
        $scriptFullPath = Join-Path (Get-Location) "install.ps1"
    }
    
    # Build the command - escape single quotes in paths
    $escapedScript = $scriptFullPath -replace "'", "''"
    $escapedInstallDir = $InstallDir -replace "'", "''"
    $escapedDataDir = $DataDir -replace "'", "''"
    
    $cmd = "& '$escapedScript' -InstallDir '$escapedInstallDir' -DataDir '$escapedDataDir' -Elevated"
    if ($SkipBackend) { $cmd += " -SkipBackend" }
    if ($SkipGUI) { $cmd += " -SkipGUI" }
    if ($SkipService) { $cmd += " -SkipService" }
    $cmd += "; Write-Host ''; Write-Host 'Press any key to close...' -ForegroundColor Gray; `$null = `$Host.UI.RawUI.ReadKey('NoEcho,IncludeKeyDown')"
    
    # Encode as base64 to avoid quoting issues
    $bytes = [System.Text.Encoding]::Unicode.GetBytes($cmd)
    $encodedCmd = [Convert]::ToBase64String($bytes)
    
    try {
        $proc = Start-Process powershell.exe -Verb RunAs -ArgumentList "-NoProfile -ExecutionPolicy Bypass -EncodedCommand $encodedCmd" -Wait -PassThru -ErrorAction Stop
        exit $proc.ExitCode
    } catch {
        Write-Host ""
        if ($_.Exception.Message -match "canceled by the user") {
            Write-Host "UAC prompt was declined." -ForegroundColor Yellow
        } else {
            Write-Host "ERROR: Administrator privileges required." -ForegroundColor Red
            Write-Host $_.Exception.Message -ForegroundColor Gray
        }
        exit 1
    }
}

# Get script directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if (-not $ScriptDir) { $ScriptDir = Split-Path -Parent $PSCommandPath }
$SourceDir = Split-Path -Parent (Split-Path -Parent $ScriptDir)

# Debug: Show resolved paths
Write-Host "DEBUG: ScriptDir  = $ScriptDir" -ForegroundColor Gray
Write-Host "DEBUG: SourceDir  = $SourceDir" -ForegroundColor Gray
Write-Host ""

# Configuration
$AppName = "Grey Multi-Tenant"
$ServiceName = "GreyMultiTenant"
$ServiceDisplayName = "Grey Multi-Tenant Platform"
$ServiceDescription = "Grey Multi-Tenant operational platform backend service"
$ExeName = "grey-core-api.exe"
$IconName = "grey-icon.ico"
$GUIInstallDir = "$env:ProgramFiles\GreyMultiTenantGUI"
$GUIDataDir = "$env:ProgramData\GreyMultiTenantGUI"

Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Grey Multi-Tenant Platform Installer" -ForegroundColor Cyan
Write-Host "  Windows Unified Edition" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

$totalSteps = 0
if (-not $SkipBackend) { $totalSteps += 6 }
if (-not $SkipGUI) { $totalSteps += 4 }
$totalSteps += 2  # Shortcuts and finish
$currentStep = 0

# ============================================
# BACKEND INSTALLATION
# ============================================
if (-not $SkipBackend) {
    Write-Host ""
    Write-Host "--- Backend Installation ---" -ForegroundColor Magenta
    Write-Host ""

    # Pre-step: Stop existing service and kill any running processes
    $existingService = Get-Service -Name $ServiceName -ErrorAction SilentlyContinue
    if ($existingService) {
        Write-Host "Stopping existing service..." -ForegroundColor Yellow
        Stop-Service -Name $ServiceName -Force -ErrorAction SilentlyContinue
        Start-Sleep -Seconds 2
    }
    
    # Kill any running grey-core-api process
    $runningProcs = Get-Process -Name "grey-core-api" -ErrorAction SilentlyContinue
    if ($runningProcs) {
        Write-Host "Terminating running processes..." -ForegroundColor Yellow
        $runningProcs | Stop-Process -Force -ErrorAction SilentlyContinue
        Start-Sleep -Seconds 2
    }
    
    # Delete the old executable if it exists (to release any lingering handles)
    $targetExe = "$InstallDir\bin\$ExeName"
    if (Test-Path $targetExe) {
        Write-Host "Removing old executable..." -ForegroundColor Yellow
        for ($i = 1; $i -le 5; $i++) {
            try {
                Remove-Item -Path $targetExe -Force -ErrorAction Stop
                break
            } catch {
                if ($i -lt 5) {
                    Write-Host "  File locked, waiting... (attempt $i/5)" -ForegroundColor Yellow
                    Start-Sleep -Seconds 2
                } else {
                    Write-Host "  WARNING: Could not remove old executable. Continuing anyway..." -ForegroundColor Yellow
                }
            }
        }
    }

    # Step: Build the Go binary
    $currentStep++
    Write-Host "[$currentStep/$totalSteps] Building backend application..." -ForegroundColor Yellow
    $GoBuildDir = Join-Path $SourceDir "services\grey-core-api"
    Write-Host "  DEBUG: GoBuildDir = $GoBuildDir" -ForegroundColor Gray
    if (-not (Test-Path $GoBuildDir)) {
        Write-Host "ERROR: Source directory not found: $GoBuildDir" -ForegroundColor Red
        Write-Host "  Check that SourceDir is correct: $SourceDir" -ForegroundColor Red
        exit 1
    }

    Push-Location $GoBuildDir
    try {
        $env:CGO_ENABLED = "0"
        $env:GOOS = "windows"
        $env:GOARCH = "amd64"
        Write-Host "  Running: go build -o bin\$ExeName ./cmd/api" -ForegroundColor Gray
        go build -o "bin\$ExeName" ./cmd/api 2>&1 | ForEach-Object { Write-Host "    $_" -ForegroundColor Gray }
        if ($LASTEXITCODE -ne 0) { throw "Go build failed with exit code $LASTEXITCODE" }
    } catch {
        Write-Host "ERROR during build: $($_.Exception.Message)" -ForegroundColor Red
        Pop-Location
        exit 1
    } finally {
        Pop-Location
    }
    Write-Host "  Build complete." -ForegroundColor Green

    # Step: Create installation directories
    $currentStep++
    Write-Host "[$currentStep/$totalSteps] Creating backend directories..." -ForegroundColor Yellow
    try {
        New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null
        New-Item -ItemType Directory -Force -Path "$InstallDir\bin" | Out-Null
        New-Item -ItemType Directory -Force -Path "$InstallDir\config" | Out-Null
        New-Item -ItemType Directory -Force -Path "$InstallDir\icons" | Out-Null
        New-Item -ItemType Directory -Force -Path $DataDir | Out-Null
        New-Item -ItemType Directory -Force -Path "$DataDir\logs" | Out-Null
        Write-Host "  Directories created." -ForegroundColor Green
    } catch {
        Write-Host "ERROR creating directories: $($_.Exception.Message)" -ForegroundColor Red
        exit 1
    }

    # Step: Copy application files
    $currentStep++
    Write-Host "[$currentStep/$totalSteps] Copying backend files..." -ForegroundColor Yellow
    try {
        $BinarySource = Join-Path $GoBuildDir "bin\$ExeName"
        Write-Host "  DEBUG: BinarySource = $BinarySource" -ForegroundColor Gray
        if (-not (Test-Path $BinarySource)) {
            throw "Binary not found at $BinarySource - build may have failed"
        }
        
        # Retry copy in case service is still releasing file handle
        $copySuccess = $false
        for ($i = 1; $i -le 5; $i++) {
            try {
                Copy-Item -Path $BinarySource -Destination "$InstallDir\bin\$ExeName" -Force -ErrorAction Stop
                $copySuccess = $true
                break
            } catch {
                if ($i -lt 5) {
                    Write-Host "  File in use, waiting... (attempt $i/5)" -ForegroundColor Yellow
                    Start-Sleep -Seconds 2
                } else {
                    throw $_
                }
            }
        }
        
        $EnvSource = Join-Path $GoBuildDir ".env.example"
        if (Test-Path $EnvSource) {
            Copy-Item -Path $EnvSource -Destination "$InstallDir\config\.env.example" -Force
        }
        $IconSource = Join-Path $SourceDir "installers\icons\$IconName"
        if (Test-Path $IconSource) {
            Copy-Item -Path $IconSource -Destination "$InstallDir\icons\$IconName" -Force
        }
        $LauncherSource = Join-Path $ScriptDir "grey-launcher.ps1"
        if (Test-Path $LauncherSource) {
            Copy-Item -Path $LauncherSource -Destination "$InstallDir\grey-launcher.ps1" -Force
        }
        Write-Host "  Files copied." -ForegroundColor Green
    } catch {
        Write-Host "ERROR copying files: $($_.Exception.Message)" -ForegroundColor Red
        exit 1
    }

    # Step: Create environment configuration
    $currentStep++
    Write-Host "[$currentStep/$totalSteps] Creating backend configuration..." -ForegroundColor Yellow
    try {
        $ConfigPath = "$InstallDir\config\.env"
    if (-not (Test-Path $ConfigPath)) {
        $configContent = @"
# Grey Multi-Tenant Configuration
DATABASE_URL=postgres://grey:grey_password@localhost:5432/grey_multitenant?sslmode=disable
SERVER_HOST=0.0.0.0
SERVER_PORT=8080
GRPC_PORT=50051
JWT_SECRET=CHANGE_THIS_TO_A_SECURE_SECRET
JWT_ACCESS_TOKEN_EXPIRY=15m
JWT_REFRESH_TOKEN_EXPIRY=168h
CORS_ALLOWED_ORIGINS=http://localhost:3000
LOG_LEVEL=info
ENVIRONMENT=production
"@
        $utf8NoBom = New-Object System.Text.UTF8Encoding $false
        [System.IO.File]::WriteAllText($ConfigPath, $configContent, $utf8NoBom)
        Write-Host "  Configuration created." -ForegroundColor Green
    } else {
        Write-Host "  Configuration exists, skipping." -ForegroundColor Green
    }
    } catch {
        Write-Host "ERROR creating configuration: $($_.Exception.Message)" -ForegroundColor Red
        exit 1
    }

    # Step: Register Windows Service
    if (-not $SkipService) {
        $currentStep++
        Write-Host "[$currentStep/$totalSteps] Registering Windows service..." -ForegroundColor Yellow
        $existingService = Get-Service -Name $ServiceName -ErrorAction SilentlyContinue
        if ($existingService) {
            if ($existingService.Status -eq "Running") {
                Stop-Service -Name $ServiceName -Force
                Start-Sleep -Seconds 2
            }
            sc.exe delete $ServiceName | Out-Null
            Start-Sleep -Seconds 1
        }
        $BinaryPath = "`"$InstallDir\bin\$ExeName`""
        New-Service -Name $ServiceName -BinaryPathName $BinaryPath -DisplayName $ServiceDisplayName -Description $ServiceDescription -StartupType Automatic -ErrorAction Stop | Out-Null
        sc.exe failure $ServiceName reset= 86400 actions= restart/5000/restart/10000/restart/30000 | Out-Null
        Write-Host "  Service registered." -ForegroundColor Green
    } else {
        $currentStep++
        Write-Host "[$currentStep/$totalSteps] Skipping service registration." -ForegroundColor Yellow
    }

    # Step: Add to PATH
    $currentStep++
    Write-Host "[$currentStep/$totalSteps] Updating system PATH..." -ForegroundColor Yellow
    $CurrentPath = [Environment]::GetEnvironmentVariable("Path", "Machine")
    if ($CurrentPath -notlike "*$InstallDir\bin*") {
        [Environment]::SetEnvironmentVariable("Path", "$CurrentPath;$InstallDir\bin", "Machine")
        Write-Host "  Added to PATH." -ForegroundColor Green
    } else {
        Write-Host "  Already in PATH." -ForegroundColor Green
    }

    # Step: Setup auto-start at boot
    $currentStep++
    Write-Host "[$currentStep/$totalSteps] Setting up auto-start at boot..." -ForegroundColor Yellow
    try {
        # Copy auto-start script
        $AutostartSource = Join-Path $ScriptDir "autostart-services.ps1"
        if (Test-Path $AutostartSource) {
            Copy-Item -Path $AutostartSource -Destination "$InstallDir\autostart-services.ps1" -Force
        }
        
        # Copy docker-compose.yml for container management
        $ComposeSource = Join-Path $SourceDir "docker-compose.yml"
        if (Test-Path $ComposeSource) {
            Copy-Item -Path $ComposeSource -Destination "$InstallDir\docker-compose.yml" -Force
        }
        
        # Register scheduled task
        $TaskName = "GreyMultiTenantAutoStart"
        $TaskPath = "\Grey Multi-Tenant\"
        
        # Remove existing task if present
        Unregister-ScheduledTask -TaskName $TaskName -TaskPath $TaskPath -Confirm:$false -ErrorAction SilentlyContinue
        
        # Create the task action
        $action = New-ScheduledTaskAction `
            -Execute "powershell.exe" `
            -Argument "-ExecutionPolicy Bypass -WindowStyle Hidden -File `"$InstallDir\autostart-services.ps1`""
        
        # Create the trigger (at startup, with 60 second delay)
        $trigger = New-ScheduledTaskTrigger -AtStartup
        $trigger.Delay = "PT60S"
        
        # Create the principal (run as SYSTEM with highest privileges)
        $principal = New-ScheduledTaskPrincipal `
            -UserId "SYSTEM" `
            -LogonType ServiceAccount `
            -RunLevel Highest
        
        # Create settings
        $settings = New-ScheduledTaskSettingsSet `
            -AllowStartIfOnBatteries `
            -DontStopIfGoingOnBatteries `
            -StartWhenAvailable `
            -ExecutionTimeLimit (New-TimeSpan -Minutes 10) `
            -RestartCount 3 `
            -RestartInterval (New-TimeSpan -Minutes 1)
        
        # Register the task
        Register-ScheduledTask `
            -TaskName $TaskName `
            -TaskPath $TaskPath `
            -Action $action `
            -Trigger $trigger `
            -Principal $principal `
            -Settings $settings `
            -Description "Starts Grey Multi-Tenant services (Docker, PostgreSQL, API) at system boot" | Out-Null
        
        Write-Host "  Auto-start scheduled task registered." -ForegroundColor Green
    } catch {
        Write-Host "  WARNING: Failed to setup auto-start: $($_.Exception.Message)" -ForegroundColor Yellow
    }
}

# ============================================
# GUI INSTALLATION
# ============================================
if (-not $SkipGUI) {
    Write-Host ""
    Write-Host "--- GUI Installation ---" -ForegroundColor Magenta
    Write-Host ""

    $currentStep++
    Write-Host "[$currentStep/$totalSteps] Checking for GUI build..." -ForegroundColor Yellow
    $GuiBuildDir = Join-Path $SourceDir "gui\release"
    $GuiExePath = ""
    $GUIExeName = "Grey Multi-Tenant GUI.exe"

    $possibleLocations = @(
        "C:\Temp\grey-gui-release\win-unpacked\$GUIExeName",
        (Join-Path $GuiBuildDir "win-unpacked\$GUIExeName"),
        (Join-Path $GuiBuildDir "$GUIExeName"),
        (Join-Path $SourceDir "gui\out\$GUIExeName")
    )

    foreach ($loc in $possibleLocations) {
        if (Test-Path $loc) {
            $GuiExePath = $loc
            break
        }
    }

    if (-not $GuiExePath) {
        Write-Host "  GUI not pre-built. Building now..." -ForegroundColor Yellow
        Push-Location (Join-Path $SourceDir "gui")
        try {
            Write-Host "    Installing dependencies..." -ForegroundColor Gray
            pnpm install --ignore-workspace 2>&1 | Out-Null
            Write-Host "    Building GUI..." -ForegroundColor Gray
            pnpm build 2>&1 | Out-Null
            Write-Host "    Packaging for Windows..." -ForegroundColor Gray
            pnpm package:win 2>&1 | Out-Null
            foreach ($loc in $possibleLocations) {
                if (Test-Path $loc) {
                    $GuiExePath = $loc
                    break
                }
            }
        } catch {
            Write-Host "  WARNING: GUI build failed. Skipping GUI installation." -ForegroundColor Yellow
            $SkipGUI = $true
        } finally {
            Pop-Location
        }
    }

    if (-not $SkipGUI -and $GuiExePath -and (Test-Path $GuiExePath)) {
        $GuiSourceDir = Split-Path -Parent $GuiExePath
        Write-Host "  Found GUI at: $GuiExePath" -ForegroundColor Green

        $currentStep++
        Write-Host "[$currentStep/$totalSteps] Creating GUI directories..." -ForegroundColor Yellow
        New-Item -ItemType Directory -Force -Path $GUIInstallDir | Out-Null
        New-Item -ItemType Directory -Force -Path "$GUIInstallDir\icons" | Out-Null
        New-Item -ItemType Directory -Force -Path $GUIDataDir | Out-Null
        Write-Host "  Directories created." -ForegroundColor Green

        $currentStep++
        Write-Host "[$currentStep/$totalSteps] Copying GUI files..." -ForegroundColor Yellow
        Copy-Item -Path "$GuiSourceDir\*" -Destination $GUIInstallDir -Recurse -Force
        $GUIIconSource = Join-Path $SourceDir "gui\resources\icon.ico"
        if (Test-Path $GUIIconSource) {
            Copy-Item -Path $GUIIconSource -Destination "$GUIInstallDir\icons\grey-gui.ico" -Force
        }
        Write-Host "  GUI files copied." -ForegroundColor Green
    } else {
        Write-Host "  GUI not available. Skipping." -ForegroundColor Yellow
    }
}

# ============================================
# SHORTCUTS & FINISH
# ============================================
Write-Host ""
Write-Host "--- Creating Shortcuts ---" -ForegroundColor Magenta
Write-Host ""

$currentStep++
Write-Host "[$currentStep/$totalSteps] Creating shortcuts..." -ForegroundColor Yellow
$WScriptShell = New-Object -ComObject WScript.Shell
$StartMenuPath = [System.IO.Path]::Combine([Environment]::GetFolderPath("CommonPrograms"), "Grey Multi-Tenant")
New-Item -ItemType Directory -Force -Path $StartMenuPath | Out-Null

# Backend shortcuts
if (-not $SkipBackend) {
    $IconPath = "$InstallDir\icons\$IconName"
    
    # Desktop shortcut - Backend
    $ShortcutPath = [System.IO.Path]::Combine([Environment]::GetFolderPath("CommonDesktopDirectory"), "$AppName.lnk")
    $Shortcut = $WScriptShell.CreateShortcut($ShortcutPath)
    $Shortcut.TargetPath = "powershell.exe"
    $Shortcut.Arguments = "-ExecutionPolicy Bypass -WindowStyle Normal -File `"$InstallDir\grey-launcher.ps1`""
    $Shortcut.WorkingDirectory = $InstallDir
    $Shortcut.Description = $ServiceDescription
    if (Test-Path $IconPath) { $Shortcut.IconLocation = $IconPath }
    $Shortcut.Save()

    # Start Menu - Backend
    $StartShortcut = $WScriptShell.CreateShortcut("$StartMenuPath\$AppName.lnk")
    $StartShortcut.TargetPath = "powershell.exe"
    $StartShortcut.Arguments = "-ExecutionPolicy Bypass -WindowStyle Normal -File `"$InstallDir\grey-launcher.ps1`""
    $StartShortcut.WorkingDirectory = $InstallDir
    $StartShortcut.Description = $ServiceDescription
    if (Test-Path $IconPath) { $StartShortcut.IconLocation = $IconPath }
    $StartShortcut.Save()
    Write-Host "  Backend shortcuts created." -ForegroundColor Green
}

# GUI shortcuts
if (-not $SkipGUI -and (Test-Path "$GUIInstallDir\$GUIExeName")) {
    $GUIIconPath = "$GUIInstallDir\icons\grey-gui.ico"
    
    # Desktop shortcut - GUI
    $GUIShortcutPath = [System.IO.Path]::Combine([Environment]::GetFolderPath("CommonDesktopDirectory"), "Grey Multi-Tenant GUI.lnk")
    $GUIShortcut = $WScriptShell.CreateShortcut($GUIShortcutPath)
    $GUIShortcut.TargetPath = "$GUIInstallDir\$GUIExeName"
    $GUIShortcut.WorkingDirectory = $GUIInstallDir
    $GUIShortcut.Description = "Grey Multi-Tenant Platform Desktop Application"
    if (Test-Path $GUIIconPath) { $GUIShortcut.IconLocation = $GUIIconPath }
    $GUIShortcut.Save()

    # Start Menu - GUI
    $GUIStartShortcut = $WScriptShell.CreateShortcut("$StartMenuPath\Grey Multi-Tenant GUI.lnk")
    $GUIStartShortcut.TargetPath = "$GUIInstallDir\$GUIExeName"
    $GUIStartShortcut.WorkingDirectory = $GUIInstallDir
    $GUIStartShortcut.Description = "Grey Multi-Tenant Platform Desktop Application"
    if (Test-Path $GUIIconPath) { $GUIStartShortcut.IconLocation = $GUIIconPath }
    $GUIStartShortcut.Save()
    Write-Host "  GUI shortcuts created." -ForegroundColor Green
}

# Uninstaller shortcut
$currentStep++
Write-Host "[$currentStep/$totalSteps] Installing uninstaller..." -ForegroundColor Yellow
$UninstallerSource = Join-Path $ScriptDir "uninstall.ps1"
if (Test-Path $UninstallerSource) {
    Copy-Item -Path $UninstallerSource -Destination "$InstallDir\uninstall.ps1" -Force
}
$UninstallShortcut = $WScriptShell.CreateShortcut("$StartMenuPath\Uninstall.lnk")
$UninstallShortcut.TargetPath = "powershell.exe"
$UninstallShortcut.Arguments = "-ExecutionPolicy Bypass -File `"$InstallDir\uninstall.ps1`""
$UninstallShortcut.Description = "Uninstall Grey Multi-Tenant Platform"
$UninstallShortcut.Save()
Write-Host "  Uninstaller ready." -ForegroundColor Green

# Summary
Write-Host ""
Write-Host "============================================" -ForegroundColor Green
Write-Host "  Installation Complete!" -ForegroundColor Green
Write-Host "============================================" -ForegroundColor Green
Write-Host ""
if (-not $SkipBackend) {
    Write-Host "Backend:" -ForegroundColor Cyan
    Write-Host "  Install: $InstallDir" -ForegroundColor White
    Write-Host "  Config:  $InstallDir\config\.env" -ForegroundColor White
    Write-Host "  Service: $ServiceName" -ForegroundColor White
    Write-Host ""
}
if (-not $SkipGUI) {
    Write-Host "GUI:" -ForegroundColor Cyan
    Write-Host "  Install: $GUIInstallDir" -ForegroundColor White
    Write-Host ""
}
Write-Host "Shortcuts created:" -ForegroundColor Yellow
if (-not $SkipBackend) { Write-Host "  - Desktop: Grey Multi-Tenant" -ForegroundColor White }
if (-not $SkipGUI) { Write-Host "  - Desktop: Grey Multi-Tenant GUI" -ForegroundColor White }
Write-Host "  - Start Menu: Grey Multi-Tenant folder" -ForegroundColor White
Write-Host ""
Write-Host "To start the backend service:" -ForegroundColor Cyan
Write-Host "  Start-Service $ServiceName" -ForegroundColor White
Write-Host ""
