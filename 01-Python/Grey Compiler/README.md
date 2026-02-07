# Grey Compiler

A complete, modern programming language compiler built entirely from scratch in Python — no external dependencies. Grey compiles to VM bytecode for immediate execution, or emits native x86-64 assembly and portable C for ahead-of-time compilation.

Warning: The program may contain instabilities. Always verify stability before deploying in a high stakes environment.

```
Source (.grey) → Lexer → Parser → AST → Semantic Analysis → IR → Optimization → Codegen → Executable
```

## Features

- **Full compiler pipeline** — lexer, parser, semantic analyzer, IR generator, optimizer, and code generator
- **Three compilation targets** — VM bytecode, x86-64 NASM assembly, C99 source
- **Register-based VM** — 256 registers, 60+ opcodes, garbage-collected runtime
- **6 optimization passes** — constant folding, dead code elimination, CSE, constant propagation, strength reduction, block merging
- **Developer tooling** — REPL, code formatter, static analysis linter (12 rules)
- **Zero dependencies** — pure Python 3, nothing to install

## Quick Start

```bash
# Clone the repository
git clone https://github.com/Robert-Marshall01/GreyThink
cd Grey-Compiler

# Run a Grey program
python -m grey run examples/hello.grey

# See all available commands
python -m grey --help
```

### Hello World

```grey
fn main() {
    println("Hello, World!");
}

main();
```

## Commands

| Command | Description |
|---------|-------------|
| `python -m grey run <file>` | Compile and execute a `.grey` source file |
| `python -m grey compile <file>` | Compile to bytecode (`.greyc`) |
| `python -m grey build <file>` | Compile to native executable via C backend |
| `python -m grey emit-asm <file>` | Emit x86-64 NASM assembly (`.asm`) |
| `python -m grey emit-c <file>` | Emit portable C99 source (`.c`) |
| `python -m grey disasm <file>` | Disassemble a compiled `.greyc` file |
| `python -m grey repl` | Start the interactive REPL |
| `python -m grey fmt <file>` | Auto-format a Grey source file |
| `python -m grey lint <file>` | Run static analysis on a Grey source file |
| `python -m grey version` | Show compiler version |

### Options

| Option | Description |
|--------|-------------|
| `-O0` to `-O3` | Optimization level (default: `-O1`) |
| `-o <path>` | Output file path |
| `--trace` | Enable VM execution tracing |
| `--ast` | Print the abstract syntax tree |
| `--ir` | Print the intermediate representation |
| `--dis` | Print disassembly alongside compilation |
| `--json` | Emit JSON metadata instead of binary |

## Language Overview

Grey is a statically-typed, imperative language with C/Rust-inspired syntax. See [DOCS.md](DOCS.md) for the full language reference.

```grey
// Variables
let x = 42;                // immutable binding
let mut y = 10;            // mutable binding
const PI = 3.14159;        // compile-time constant

// Functions
fn factorial(n: int) -> int {
    if n <= 1 { return 1; }
    return n * factorial(n - 1);
}

// Structs
struct Point { x: float, y: float }
let p = Point { x: 1.0, y: 2.0 };

// Control flow uses `and`, `or`, `not` (not `&&`, `||`, `!`)
if x > 0 and y > 0 {
    println("both positive");
}

// Arrays
let nums = [64, 34, 25, 12];
println(len(nums));

// While loops
let mut i = 0;
while i < 10 {
    println(i);
    i = i + 1;
}

// Match expressions
match value {
    1 => println("one"),
    2 => println("two"),
    _ => println("other"),
}
```

## Architecture

```
Grey Compiler/
├── grey/
│   ├── __init__.py              # Package root (v1.0.0)
│   ├── __main__.py              # Module entry point
│   ├── main.py                  # CLI & compilation orchestration
│   ├── frontend/                # ── Frontend ──────────────────
│   │   ├── tokens.py            #   Token definitions (80+ types)
│   │   ├── lexer.py             #   Scanner / tokenizer
│   │   ├── ast_nodes.py         #   AST node hierarchy (~30 nodes)
│   │   ├── parser.py            #   Recursive descent parser
│   │   ├── symbols.py           #   Symbol table & scoped lookup
│   │   ├── semantic.py          #   Type checker & semantic analysis
│   │   └── normalizer.py        #   AST desugaring & normalization
│   ├── middle/                  # ── Middle End ────────────────
│   │   ├── ir.py                #   IR types (3-address SSA form)
│   │   ├── ir_generator.py      #   AST → IR translation
│   │   └── optimizer.py         #   6-pass optimization pipeline
│   ├── backend/                 # ── Backend ───────────────────
│   │   ├── instruction.py       #   VM opcode definitions (~60 ops)
│   │   ├── codegen.py           #   IR → VM bytecode
│   │   ├── register_alloc.py    #   Linear scan register allocator
│   │   ├── emitter.py           #   Binary / disassembly / JSON emitter
│   │   ├── x86_64_codegen.py    #   IR → x86-64 NASM assembly
│   │   └── c_codegen.py         #   IR → portable C99 source
│   ├── runtime/                 # ── Runtime System ────────────
│   │   ├── vm.py                #   Register-based virtual machine
│   │   ├── memory.py            #   Tagged values & heap management
│   │   ├── gc.py                #   Mark-and-sweep garbage collector
│   │   └── stdlib.py            #   Standard library (40+ builtins)
│   └── tools/                   # ── Tooling ──────────────────
│       ├── repl.py              #   Interactive REPL
│       ├── formatter.py         #   Code formatter
│       └── linter.py            #   Static analysis linter (12 rules)
├── examples/                    # Example Grey programs
│   ├── hello.grey               #   Hello World
│   ├── fibonacci.grey           #   Recursive & iterative Fibonacci
│   ├── factorial.grey           #   Factorial with structs
│   ├── algorithms.grey          #   Bubble sort & FizzBuzz
│   └── native_test.grey         #   Native codegen test suite
├── tests/                       # Test suite (46 tests)
│   ├── test_lexer.py            #   7 lexer tests
│   ├── test_parser.py           #   10 parser tests
│   ├── test_compiler.py         #   11 full-pipeline tests
│   └── test_native_codegen.py   #   18 native backend tests
├── DOCS.md                      # Language & compiler documentation
├── LICENSE                      # MIT License
└── README.md
```

## Running Tests

```bash
# Run individual test suites
python tests/test_lexer.py
python tests/test_parser.py
python tests/test_compiler.py
python tests/test_native_codegen.py

# Run all tests at once
python tests/test_lexer.py && python tests/test_parser.py && python tests/test_compiler.py && python tests/test_native_codegen.py
```

All **46 tests** should pass (1 skip if no C compiler is available for native compilation).

## Native Compilation

Grey supports ahead-of-time compilation to native code via two backends:

### C Backend (portable)
```bash
python -m grey emit-c examples/native_test.grey -o output.c
gcc -O2 output.c -o program -lm
./program
```

### x86-64 Assembly Backend (Windows x64 & System V AMD64)
```bash
python -m grey emit-asm examples/native_test.grey -o output.asm
nasm -f win64 output.asm -o output.o    # or -f elf64 on Linux
gcc output.o -o program -lm
./program
```

### One-step native build
```bash
python -m grey build examples/native_test.grey -o program
```
Requires `gcc` on your PATH.

## Binary Format (.greyc)

Compiled bytecode uses a custom binary format:

| Section | Content |
|---------|---------|
| Magic | `GREY` (4 bytes) |
| Version | major.minor (2 bytes) |
| String table | Interned string literals |
| Global names | Global variable registry |
| Chunks | Compiled functions — constants, instructions, debug info |
| Instructions | 32-bit encoded: `[opcode:8][A:8][B:8][C:8]` |

## License

[MIT](LICENSE) — Copyright (c) 2026 [Robert-Marshall01](https://github.com/Robert-Marshall01)
