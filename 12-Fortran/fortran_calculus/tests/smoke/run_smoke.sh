#!/usr/bin/env bash
set -euo pipefail

echo "Compiling..."
gfortran -std=f2008 -O2 -fdefault-real-8 -o fortran_calculus.exe ../../fortran_calculus.f90
echo "Running executable..."
./fortran_calculus.exe
echo "Smoke test completed."
