<#
userdel_safe.ps1 - safe user deletion helper for Windows
Usage: .\userdel_safe.ps1 -UserName joe [-RemoveProfile] [-Force]

Behavior:
- Protects 'Administrator' and the currently logged-on user from accidental deletion.
- If running elevated, calls `net user <user> /delete` to remove the local account.
- If not elevated, prompts to elevate or falls back to removing an emulated entry from .msh_users.
- Requires typing the username to confirm (unless -Force is provided).
#>

param(
    [Parameter(Mandatory=$true)]
    [string]$UserName,

    [switch]$RemoveProfile,

    [switch]$Force
)

function Test-Admin {
    $current = [System.Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object System.Security.Principal.WindowsPrincipal($current)
    return $principal.IsInRole([System.Security.Principal.WindowsBuiltInRole]::Administrator)
}

# Protect current user and built-in Administrator
$currentUser = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name.Split('\\')[-1]
if ($UserName -ieq 'Administrator' -or $UserName -ieq $currentUser) {
    Write-Error "Refusing to delete protected account: $UserName"
    exit 1
}

# Confirmation unless forced
if (-not $Force) {
    if ($Host.UI.RawUI.KeyAvailable) {
        # interactive console: require typing username
        Write-Host "Type the username to confirm deletion of '$UserName' (or Ctrl-C to abort): " -NoNewline
        $confirm = Read-Host
        if ($confirm -ne $UserName) {
            Write-Error "Confirmation mismatch; aborting."
            exit 2
        }
    } else {
        Write-Error "Non-interactive session and -Force not specified. Exiting."
        exit 3
    }
}

$emulatedFile = Join-Path (Get-Location) '.msh_users'

if (Test-Admin) {
    # Run deletion
    $cmd = "net user `"$UserName`" /delete"
    Write-Host "Running: $cmd"
    $proc = Start-Process -FilePath cmd.exe -ArgumentList '/C', $cmd -Wait -NoNewWindow -PassThru
    if ($proc.ExitCode -eq 0) {
        Write-Host "User $UserName deleted (system)."
        if ($RemoveProfile) {
            $profilePath = Join-Path 'C:\Users' $UserName
            if (Test-Path $profilePath) {
                Write-Host "Removing profile directory: $profilePath"
                Remove-Item -LiteralPath $profilePath -Recurse -Force -ErrorAction SilentlyContinue
            }
        }
        exit 0
    } else {
        Write-Error "net user returned exit code $($proc.ExitCode)."
        exit $proc.ExitCode
    }
} else {
    Write-Host "Not running as Administrator. Will attempt to delete emulated entry in $emulatedFile if present. To delete a real account, re-run as Administrator."
    if (-not (Test-Path $emulatedFile)) {
        Write-Error "No $emulatedFile found; nothing to do."
        exit 1
    }
    $lines = Get-Content $emulatedFile -ErrorAction Stop
    if ($lines -contains $UserName) {
        $new = $lines | Where-Object { $_ -ne $UserName }
        $tmp = [System.IO.Path]::GetTempFileName()
        $new | Out-File -FilePath $tmp -Encoding utf8
        Move-Item -Force -Path $tmp -Destination $emulatedFile
        Write-Host "Emulated user $UserName removed from $emulatedFile"
        exit 0
    } else {
        Write-Error "No emulated entry for $UserName in $emulatedFile"
        exit 1
    }
}
