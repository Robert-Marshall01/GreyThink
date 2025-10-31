Clear-Host
Write-Host "Building Medit..."
gcc "Medit.c" -o "Medit.exe"
if ($LASTEXITCODE -ne 0) { Write-Error "Medit build failed (gcc Medit.c)"; exit $LASTEXITCODE }

Write-Host "Compiling C sources..."
gcc -c "Msh.c" -o "Msh.o"
if ($LASTEXITCODE -ne 0) { Write-Error "Failed to compile Msh.c"; exit $LASTEXITCODE }
gcc -c "run_builtin_command.c" -o "run_builtin_command.o"
if ($LASTEXITCODE -ne 0) { Write-Error "Failed to compile run_builtin_command.c"; exit $LASTEXITCODE }
gcc -c "jobs.c" -o "jobs.o"
if ($LASTEXITCODE -ne 0) { Write-Error "Failed to compile jobs.c"; exit $LASTEXITCODE }

Write-Host "Compiling C++ regex wrapper..."
g++ -std=c++11 -c "regex_wrapper.cpp" -o "regex_wrapper.o"
if ($LASTEXITCODE -ne 0) { Write-Error "Failed to compile regex_wrapper.cpp (g++)"; exit $LASTEXITCODE }

Write-Host "Linking objects with g++ (links libstdc++)..."
g++ "Msh.o" "run_builtin_command.o" "jobs.o" "regex_wrapper.o" -o "Msh.exe" -static -lstdc++
if ($LASTEXITCODE -ne 0) { Write-Error "Link failed"; exit $LASTEXITCODE }

Write-Host "Build succeeded. Running .\Msh.exe"
.\Msh.exe