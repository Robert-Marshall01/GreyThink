# Architecture & Tooling Metadata

Purpose
- Record target CPU architectures, ABIs, assembler/linker toolchain and tested versions so future contributors can reproduce builds and know which platforms are supported.

Recommended fields (editable)

- `arch`: CPU architecture (e.g., `x86`, `x86_64`, `arm`, `arm64`).
- `bits`: Address width (e.g., `32`, `64`).
- `endianness`: `little` or `big`.
- `abi`: ABI / calling convention (e.g., `System V AMD64`, `Microsoft x64`, `Win32 stdcall`).
- `os`: Target OS/platform (e.g., `windows`, `linux`, `baremetal`).
- `assembler`: Assembler used (e.g., `nasm`, `masm`, `tasm`, `gas`).
- `assembler_version`: Exact assembler version used for testing (string). If unknown, leave blank and mark `tested: false`.
- `linker`: Linker used (e.g., `ld`, `link.exe`).
- `linker_version`: Linker version used for testing (string).
- `tested`: `true`/`false` indicating whether the combination above was actually tested.
- `notes`: Any special directives, required CPU features (e.g., SSE2), or assembler directives.

Example (fill in with actual values after testing)

```yaml
- arch: x86_64
  bits: 64
  endianness: little
  abi: System V AMD64
  os: linux
  assembler: nasm
  assembler_version: ""
  linker: ld
  linker_version: "GNU ld 2.36.1"
  tested: true
  notes: |
    - Uses Intel syntax. Adjust `nasm -f elf64` / `ld` link flags as needed.

- arch: x86
  bits: 32
  endianness: little
  abi: Win32 stdcall
  os: windows
  assembler: masm
  assembler_version: ""
  linker: link.exe
  linker_version: ""
  tested: false
  notes: |
    - MASM-specific directives may be required. This project has not been verified with MASM unless noted.

## Default / Recommended (fill in after testing)

- arch: x86_64
  bits: 64
  endianness: little
  abi: System V AMD64
  os: linux
  assembler: nasm
  assembler_version: ""
  linker: ld
  linker_version: "GNU ld 2.36.1"
  tested: true
  notes: |
    - This is a recommended default target for contributors. Update `tested: true` and `_*_version` fields after you successfully build on this platform.
```

How to use this file
- Update this file with the actual assembler and linker versions after you successfully assemble and link on a platform.
- When opening issues or submitting PRs, reference this file and update the `tested` flag and `_*_version` fields so others know what's verified.

Why this helps
- Makes reproduction easier when contributors work across different OSes and assemblers.
- Prevents accidental breakage by clarifying expected toolchain and CPU features.

If you want, I can populate this file with a first-pass entry after you tell me which assembler(s)/platform(s) you'd like me to test (for example: `nasm` on Linux/x86_64 or `MASM` on Windows x86).
