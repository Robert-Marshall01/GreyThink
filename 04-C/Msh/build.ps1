Get-ChildItem -Path . -Filter "run_builtin_command.*","Msh.*"# PowerShell build script for Msh (TDM-GCC)
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$exe = Join-Path $scriptDir "Msh.exe"
$srcs = @("Msh.c","run_builtin_command.c") | ForEach-Object { Join-Path $scriptDir $_ }
$cmd = "gcc " + ($srcs -join " ") + " -o `"$exe`" -static"
Write-Host "Running: $cmd"
$proc = Start-Process -FilePath gcc -ArgumentList $srcs + @("-o", $exe, "-static") -NoNewWindow -Wait -PassThru
if ($proc.ExitCode -ne 0) { Write-Error "Build failed with exit code $($proc.ExitCode)"; exit $proc.ExitCode }
Write-Host "Build succeeded."; exit 0
