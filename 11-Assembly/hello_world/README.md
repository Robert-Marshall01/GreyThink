# Hello World (NASM)

This repository contains a minimal NASM "Hello, world!" example for Linux x86_64.

## Files

- `hello_linux.asm` — Linux (ELF) using syscalls (primary example in this README)
- `hello_win.asm`   — Windows (Win64) using WinAPI (present in the repo but not documented here)

## Building and running (Linux / WSL)

Run these commands in a Linux shell or inside WSL (Ubuntu/Debian):

```bash
# Install tools if needed (Debian/Ubuntu):
sudo apt update
sudo apt install -y nasm build-essential

# Assemble, link, and run the Linux example:
nasm -f elf64 hello_linux.asm -o hello_linux.o
ld hello_linux.o -o hello_linux
./hello_linux
```

## Notes

- This README documents the Linux workflow only. The Windows example `hello_win.asm` remains in
  the repository for reference; Windows build instructions were intentionally omitted here to avoid
  cross-platform confusion.

## Why split files?

Mixing both targets in a single `.asm` produced linker errors when assembling for one platform and
linking on another (for example, assembling for ELF64 left Windows externals like `GetStdHandle`
unresolved). Keeping target-specific source files avoids those cross-target symbol conflicts.

## Author note

Note: I have very weak Assembly skills. If something in these examples is unclear or incorrect,
please let me know — corrections or improvements are welcome.

## Prerequisites

- `nasm` (assembler)
- `ld` or `gcc` (linker)
- If using WSL, an installed Linux distro such as Ubuntu

## Alternative link command

You can also link with `gcc` which will pull in standard linker defaults on Linux:

```bash
# Link with gcc instead of ld:
gcc hello_linux.o -o hello_linux
./hello_linux
```

## Expected output

When the program runs it should print exactly:

```
Hello, world!
```

## Running from WSL (notes for Windows users)

If you are working in Windows and using WSL, your project files on the Windows filesystem are
accessible under `/mnt`. For example, to run from the same folder shown in Explorer you can:

```bash
cd /mnt/c/Users/Rober/OneDrive/Desktop/Coding\ Projects/11-Assembly/hello_world
# then run the commands above
```

## Author note

I'm still learning Assembly — suggestions and PRs are welcome.


