<#
.SYNOPSIS
    Register Grey Multi-Tenant Auto-Start Scheduled Task

.DESCRIPTION
    Creates a Windows scheduled task that runs at system startup to ensure
    all Grey Multi-Tenant services are running (Docker, PostgreSQL, API).

.PARAMETER Unregister
    Remove the scheduled task instead of creating it

.NOTES
    Requires Administrator privileges
#>

param(
    [switch]$Unregister = $false
)

$TaskName = "GreyMultiTenantAutoStart"
$TaskPath = "\Grey Multi-Tenant\"

# Check for admin privileges
$currentPrincipal = New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())
if (-not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Host "ERROR: This script requires Administrator privileges." -ForegroundColor Red
    Write-Host "Please run PowerShell as Administrator and try again." -ForegroundColor Yellow
    exit 1
}

if ($Unregister) {
    Write-Host "Removing scheduled task: $TaskName" -ForegroundColor Yellow
    Unregister-ScheduledTask -TaskName $TaskName -TaskPath $TaskPath -Confirm:$false -ErrorAction SilentlyContinue
    Write-Host "Task removed." -ForegroundColor Green
    exit 0
}

# Find the autostart script
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$AutostartScript = Join-Path $ScriptDir "autostart-services.ps1"

# Also check installed location
if (-not (Test-Path $AutostartScript)) {
    $AutostartScript = "$env:ProgramFiles\GreyMultiTenant\autostart-services.ps1"
}

if (-not (Test-Path $AutostartScript)) {
    Write-Host "ERROR: autostart-services.ps1 not found" -ForegroundColor Red
    exit 1
}

Write-Host "Registering scheduled task: $TaskName" -ForegroundColor Cyan
Write-Host "Script: $AutostartScript" -ForegroundColor Gray

# Remove existing task if present
$existingTask = Get-ScheduledTask -TaskName $TaskName -TaskPath $TaskPath -ErrorAction SilentlyContinue
if ($existingTask) {
    Write-Host "Removing existing task..." -ForegroundColor Yellow
    Unregister-ScheduledTask -TaskName $TaskName -TaskPath $TaskPath -Confirm:$false
}

# Create the task action
$action = New-ScheduledTaskAction `
    -Execute "powershell.exe" `
    -Argument "-ExecutionPolicy Bypass -WindowStyle Hidden -File `"$AutostartScript`""

# Create the trigger (at startup, with delay to allow Docker to start)
$trigger = New-ScheduledTaskTrigger -AtStartup
$trigger.Delay = "PT60S"  # 60 second delay after boot

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
try {
    Register-ScheduledTask `
        -TaskName $TaskName `
        -TaskPath $TaskPath `
        -Action $action `
        -Trigger $trigger `
        -Principal $principal `
        -Settings $settings `
        -Description "Starts Grey Multi-Tenant services (Docker, PostgreSQL, API) at system boot"
    
    Write-Host ""
    Write-Host "Scheduled task registered successfully!" -ForegroundColor Green
    Write-Host ""
    Write-Host "Task Details:" -ForegroundColor Cyan
    Write-Host "  Name: $TaskName" -ForegroundColor White
    Write-Host "  Trigger: At system startup (60 second delay)" -ForegroundColor White
    Write-Host "  Run as: SYSTEM" -ForegroundColor White
    Write-Host ""
    Write-Host "To test the task manually:" -ForegroundColor Yellow
    Write-Host "  Start-ScheduledTask -TaskPath '$TaskPath' -TaskName '$TaskName'" -ForegroundColor White
    Write-Host ""
    Write-Host "To view task status:" -ForegroundColor Yellow
    Write-Host "  Get-ScheduledTask -TaskName '$TaskName' -TaskPath '$TaskPath'" -ForegroundColor White
} catch {
    Write-Host "ERROR: Failed to register scheduled task" -ForegroundColor Red
    Write-Host $_.Exception.Message -ForegroundColor Gray
    exit 1
}
