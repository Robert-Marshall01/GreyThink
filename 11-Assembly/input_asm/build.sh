#!/usr/bin/env bash
set -euo pipefail

# Simple build script for Linux / WSL
nasm -f elf32 input_asm.asm -o input_asm.o
ld -m elf_i386 input_asm.o -o input_asm
echo "Built ./input_asm"
