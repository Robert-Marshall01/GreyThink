# input_asm

[![CI](https://github.com/<your-username>/input_asm/actions/workflows/ci.yml/badge.svg)](https://github.com/<your-username>/input_asm/actions/workflows/ci.yml)

Version: 0.2.0

Simple NASM (32-bit) example that reads a single ASCII digit from stdin and prints whether the number is positive or negative. This project uses Linux 32-bit syscalls (`int 0x80`) and is intended to be assembled with NASM.

**Prerequisites:**
- `nasm` (Netwide Assembler)
- `ld` (GNU linker) or `gcc` with 32-bit support (on 64-bit hosts you may need `gcc-multilib` / `libc6-dev-i386`)
- A Linux environment (native or via WSL / VM) to run the resulting binary, since the program uses Linux `int 0x80` syscalls.

**Build (recommended)**
Assemble to 32-bit ELF object and link:

```bash
nasm -f elf32 input_asm.asm -o input_asm.o
ld -m elf_i386 input_asm.o -o input_asm
```

Or assemble and link with `gcc` (convenient on systems with multilib):

```bash
nasm -f elf32 input_asm.asm -o input_asm.o
gcc -m32 input_asm.o -o input_asm
```

**Run**

```bash
./input_asm
```

Type a single digit and press Enter. Entering `0` will exit the program.

**Notes**
- The source file uses NASM-style directives (`section .data`, `%macro`) and Linux `int 0x80` syscalls; it targets 32-bit x86 Linux.
- If you're on Windows, use WSL (Windows Subsystem for Linux) or a Linux VM to assemble and run.

**CI / Runner**
- The repository's workflow uses `runs-on: ubuntu-latest` in GitHub Actions. `ubuntu-latest` is an alias that maps to the current Ubuntu runner image provided by GitHub Actions (for reproducible builds you can pin the workflow to a specific runner such as `ubuntu-22.04`).
- To verify the exact image used by `ubuntu-latest`, add a diagnostic step that prints `/etc/os-release` in your workflow or pin the runner explicitly.

**License**
This repository is provided under the MIT License. See the `LICENSE` file.

If you want a different license or want me to include author/maintainer info, tell me and I'll update these files.
# input_asm

Simple NASM (32-bit) example that reads a single ASCII digit from stdin and prints whether the number is positive or negative. This project uses Linux 32-bit syscalls (`int 0x80`) and is intended to be assembled with NASM.

**Prerequisites:**
- `nasm` (Netwide Assembler)
- `ld` (GNU linker) or `gcc` with 32-bit support (on 64-bit hosts you may need `gcc-multilib` / `libc6-dev-i386`)
- A Linux environment (native or via WSL / VM) to run the resulting binary, since the program uses Linux `int 0x80` syscalls.

**Build (recommended)**
Assemble to 32-bit ELF object and link:

```bash
nasm -f elf32 input_asm.asm -o input_asm.o
ld -m elf_i386 input_asm.o -o input_asm
```

Or assemble and link with `gcc` (convenient on systems with multilib):

```bash
nasm -f elf32 input_asm.asm -o input_asm.o
gcc -m32 input_asm.o -o input_asm
```

**Run**

```bash
./input_asm
```

Type a single digit and press Enter. Entering `0` will exit the program.

**Notes**
- The source file uses NASM-style directives (`section .data`, `%macro`) and Linux `int 0x80` syscalls; it targets 32-bit x86 Linux.
- If you're on Windows, use WSL (Windows Subsystem for Linux) or a Linux VM to assemble and run.

**License**
This repository is provided under the MIT License. See the `LICENSE` file.

If you want a different license or want me to include author/maintainer info, tell me and I'll update these files.
