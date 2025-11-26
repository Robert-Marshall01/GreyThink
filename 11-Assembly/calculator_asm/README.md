# calculator_asm

A small assembly-language calculator project. The main source file is `calculator_asm.asm`.
Negatives may be buggy.
Decimal inputs are currently not supported.

## Contents

- `calculator_asm.asm` — the assembly source for the calculator.

## Description

This repository contains a single assembly program that implements a basic calculator. It is intended for educational purposes and may rely on a specific assembler and runtime environment.

## Requirements

- An x86 assembler and tools (e.g., MASM, NASM, TASM or a toolchain appropriate for the target platform).
- A Windows environment is assumed for running/debugging (PowerShell, debuggers, or DOS emulators as needed).

## Project Version & Supported Targets

- **Version:** `0.2.0` (see the `VERSION` file in the repository root). The project follows [Semantic Versioning](https://semver.org/) — update `VERSION` and `CHANGELOG.md` when making releases.
- **Primary target:** Linux x86 (i386) — the source uses `int 0x80` syscalls and defines the `_start` entry symbol. Assemble with `nasm -f elf32` and link with `ld -m elf_i386` as shown below.
- **Other supported formats (examples included):**
	- x86_64 / ELF64 (WSL / Linux): assemble with `nasm -f elf64` and link with `gcc` or `ld`.
	- Windows PE (MinGW/MSYS2): assemble with `nasm -f win64` / `-f win32` and link with MinGW `gcc` to produce native `.exe` files.
- **Minimum tooling (recommended):**
	- `nasm` >= 2.14 (newer is fine) — older NASM releases may still work but testing was done with recent versions.
	- `ld` (GNU binutils) or `gcc` (MinGW-w64 / WSL GCC) for linking.

Notes:
- Because the program currently uses direct syscalls and `_start`, linking with `ld` produces a minimal native binary without the C runtime. If you change the entrypoint to `main` or call C library functions, prefer linking via `gcc` to supply the CRT startup code.
- If you plan to add variants for other architectures (ARM, RISC-V, etc.), include separate build examples and automated CI tests that build each target.

## Build / Run (NASM examples)

These examples assume you are on Windows and using NASM. You will need `nasm` installed and a linker (for example, `gcc` from MinGW-w64) available on your PATH.

Recommended (WSL / Linux, 64-bit):

```bash
nasm -f elf64 calculator_asm.asm -o calculator_asm.o
gcc calculator_asm.o -o calculator_asm
# Run
./calculator_asm
```

Alternative (WSL / Linux, 32-bit):

```bash
# Using `ld` directly (WSL / Linux, 32-bit)
nasm -f elf32 calculator_asm.asm -o calculator_asm.o
ld -m elf_i386 calculator_asm.o -o calculator_asm
# Run
./calculator_asm
```

Notes:
- If your assembly uses the C runtime (`main`), linking with `gcc` supplies the startup code and CRT automatically.
- If you use a custom entry symbol (e.g., `_start`) or bypass the C runtime, tell me which symbol you use and I will give the exact linker flags.
- If you plan to run under WSL, prefer the `elf64`/`elf32` examples above; for native Windows executables from Bash, use the `win64`/`win32` examples and MinGW's `gcc`.

## Contributing

Feel free to open issues or create pull requests. If you add platform-specific build instructions, update this README accordingly.

## AI Assistance

Some parts of this repository (documentation and boilerplate) were generated or edited with assistance from Microsoft Copilot (AI). While the code and instructions aim to be correct, they may contain bugs or inaccuracies—please review, test, and validate before using in production. Report any issues or unexpected behavior via issues or pull requests.

## License

This project is licensed under the MIT License — see the `LICENSE` file.

