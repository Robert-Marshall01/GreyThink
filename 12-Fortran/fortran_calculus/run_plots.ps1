# Run plotting helper for the Fortran demos
# Usage: .\run_plots.ps1 [-All] [-Sigma] [-Stability]
param(
    [switch]$All,
    [switch]$Sigma,
    [switch]$Stability
)

$script = Join-Path $PSScriptRoot 'plot_demo.py'
if (-not (Test-Path $script)) {
    Write-Error "plot_demo.py not found in $PSScriptRoot"
    exit 1
}

$cmd = 'python' 
if ($All -or (-not $Sigma -and -not $Stability)) { $args = '--all' }
elseif ($Sigma) { $args = '--sigma' }
elseif ($Stability) { $args = '--stability' }
else { $args = '--all' }

Write-Host "Running: python $script $args"
python $script $args
if ($LASTEXITCODE -ne 0) { Write-Error "plot_demo.py failed with exit code $LASTEXITCODE"; exit $LASTEXITCODE }
Write-Host "Plots created."
