Write-Host "Compiling..."
gfortran -std=f2008 -O2 -fdefault-real-8 -o fortran_calculus.exe ..\..\fortran_calculus.f90
if ($LASTEXITCODE -ne 0) { throw "Compilation failed: $LASTEXITCODE" }
Write-Host "Running executable..."
.\fortran_calculus.exe
Write-Host "Smoke test completed."
