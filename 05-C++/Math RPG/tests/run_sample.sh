#!/usr/bin/env bash
set -euo pipefail

# Compile
g++ -std=c++17 -O2 -o math_rpg_test -Wall -Wextra math_rpg.cpp

# Run sample mode with seed 12345 and 5 questions
./math_rpg_test --sample 12345 5
