param(
  [string]$Compiler = 'gfortran'
)

Write-Host "Building with $Compiler"
& $Compiler -std=f2008 -O2 -fdefault-real-8 -o fortran_calculus.exe fortran_calculus.f90
if ($LASTEXITCODE -ne 0) {
  Write-Error "Build failed with exit code $LASTEXITCODE"
  exit $LASTEXITCODE
}
Write-Host "Build succeeded: .\fortran_calculus.exe"
