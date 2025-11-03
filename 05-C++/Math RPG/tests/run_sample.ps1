# PowerShell sample runner
Set-StrictMode -Version Latest

# Compile
g++ -std=c++17 -O2 -o math_rpg_test.exe -Wall -Wextra math_rpg.cpp

# Run sample
.\math_rpg_test.exe --sample 12345 5
