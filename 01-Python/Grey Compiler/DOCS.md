# Grey Language Documentation

Complete reference for the Grey programming language and compiler internals.

---

## Table of Contents

- [1. Language Reference](#1-language-reference)
  - [1.1 Comments](#11-comments)
  - [1.2 Variables & Constants](#12-variables--constants)
  - [1.3 Data Types](#13-data-types)
  - [1.4 Operators](#14-operators)
  - [1.5 Functions](#15-functions)
  - [1.6 Control Flow](#16-control-flow)
  - [1.7 Data Structures](#17-data-structures)
  - [1.8 Traits & Implementations](#18-traits--implementations)
  - [1.9 Lambdas](#19-lambdas)
  - [1.10 Modules & Imports](#110-modules--imports)
  - [1.11 Semicolons](#111-semicolons)
- [2. Standard Library](#2-standard-library)
  - [2.1 I/O](#21-io)
  - [2.2 Math](#22-math)
  - [2.3 Strings](#23-strings)
  - [2.4 Arrays](#24-arrays)
  - [2.5 Type Operations](#25-type-operations)
  - [2.6 System](#26-system)
- [3. CLI Reference](#3-cli-reference)
- [4. Compiler Architecture](#4-compiler-architecture)
  - [4.1 Frontend](#41-frontend)
  - [4.2 Middle End](#42-middle-end)
  - [4.3 Backend](#43-backend)
  - [4.4 Runtime System](#44-runtime-system)
  - [4.5 Tooling](#45-tooling)
- [5. VM Architecture](#5-vm-architecture)
  - [5.1 Registers](#51-registers)
  - [5.2 Instruction Encoding](#52-instruction-encoding)
  - [5.3 Opcode Reference](#53-opcode-reference)
- [6. Binary Format (.greyc)](#6-binary-format-greyc)
- [7. Optimization Passes](#7-optimization-passes)
- [8. Linter Rules](#8-linter-rules)
- [9. Native Backends](#9-native-backends)

---

## 1. Language Reference

### 1.1 Comments

```grey
// Single-line comment

/* Multi-line
   comment */

/* Nested /* comments */ are supported */
```

### 1.2 Variables & Constants

Grey has three kinds of bindings:

```grey
let x = 42;              // immutable — cannot be reassigned
let mut y = 10;           // mutable — can be reassigned
const PI = 3.14159;       // constant — evaluated at declaration
```

Type annotations are optional (types are inferred) but can be explicit:

```grey
let name: string = "Grey";
let mut count: int = 0;
```

**Mutability rule:** Variables declared with `let` are immutable by default. To reassign a variable, it must be declared with `let mut`. This is enforced by the semantic analyzer.

```grey
let x = 5;
x = 10;        // ERROR: Cannot reassign immutable variable 'x'

let mut y = 5;
y = 10;        // OK
```

### 1.3 Data Types

| Type | Description | Literals |
|------|-------------|----------|
| `int` | 64-bit integer | `42`, `0xFF`, `0b1010`, `0o77` |
| `float` | 64-bit floating point | `3.14`, `1e10`, `2.5e-3` |
| `string` | UTF-8 string | `"hello"`, `'world'` |
| `bool` | Boolean | `true`, `false` |
| `nil` | Null value | `nil` |
| `[T]` | Array of type T | `[1, 2, 3]` |
| `struct` | Named product type | `Point { x: 1.0, y: 2.0 }` |
| `enum` | Sum type | `Color.Red` |
| `fn` | Function type | `fn(int, int) -> int` |

**Numeric literals** support multiple bases:
```grey
let decimal = 255;
let hex     = 0xFF;
let binary  = 0b11111111;
let octal   = 0o377;
let float   = 3.14e2;      // 314.0
```

**String escape sequences:** `\"`, `\\`, `\n`, `\t`, `\r`, `\0`

### 1.4 Operators

#### Arithmetic
| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition / string concatenation | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `%` | Modulo | `a % b` |
| `**` | Exponentiation | `a ** b` |

#### Comparison
| Operator | Description |
|----------|-------------|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less or equal |
| `>=` | Greater or equal |

#### Logical

Grey uses **keyword-based** logical operators (not `&&`, `||`, `!`):

| Operator | Description | Example |
|----------|-------------|---------|
| `and` | Logical AND | `a and b` |
| `or` | Logical OR | `a or b` |
| `not` | Logical NOT | `not a` |

#### Bitwise
| Operator | Description |
|----------|-------------|
| `&` | Bitwise AND |
| `\|` | Bitwise OR |
| `^` | Bitwise XOR |
| `~` | Bitwise NOT |
| `<<` | Left shift |
| `>>` | Right shift |

#### Assignment
| Operator | Description |
|----------|-------------|
| `=` | Assign |
| `+=` | Add and assign |
| `-=` | Subtract and assign |
| `*=` | Multiply and assign |
| `/=` | Divide and assign |
| `%=` | Modulo and assign |

Compound assignments (`+=`, etc.) are desugared by the AST normalizer into `x = x + value`.

### 1.5 Functions

Functions are declared with `fn`. Parameters require type annotations; the return type follows `->`:

```grey
fn add(a: int, b: int) -> int {
    return a + b;
}

fn greet(name: string) {
    println("Hello, " + name + "!");
}
```

Functions can be recursive:

```grey
fn fibonacci(n: int) -> int {
    if n <= 1 { return n; }
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

Functions are first-class values and can be passed to other functions or stored in variables:

```grey
let f = add;
println(f(2, 3));  // 5
```

### 1.6 Control Flow

#### If / Elif / Else

```grey
if condition {
    // ...
} elif other_condition {
    // ...
} else {
    // ...
}
```

Braces are **required**. There is no ternary operator.

#### While Loop

```grey
let mut i = 0;
while i < 10 {
    println(i);
    i = i + 1;
}
```

#### For-In Loop

```grey
for item in collection {
    println(item);
}

for i in 0..10 {
    println(i);       // 0 through 9
}

for i in 0..=10 {
    println(i);       // 0 through 10 (inclusive)
}
```

`for` loops are desugared to `while` loops by the AST normalizer.

#### Break & Continue

```grey
let mut i = 0;
while i < 100 {
    if i == 50 { break; }
    if i % 2 == 0 {
        i = i + 1;
        continue;
    }
    println(i);
    i = i + 1;
}
```

#### Match

```grey
match value {
    1 => println("one"),
    2 => println("two"),
    3 => {
        println("three");
        println("is the magic number");
    },
    _ => println("something else"),
}
```

### 1.7 Data Structures

#### Arrays

```grey
let numbers = [1, 2, 3, 4, 5];
let first = numbers[0];          // indexing
numbers[0] = 10;                 // mutation (if mutable)
println(len(numbers));           // 5
push(numbers, 6);                // append
```

#### Structs

```grey
struct Point {
    x: float,
    y: float,
}

let p = Point { x: 1.0, y: 2.0 };
println(p.x);    // field access
```

#### Enums

```grey
enum Direction {
    North,
    South,
    East,
    West,
}
```

### 1.8 Traits & Implementations

```grey
trait Printable {
    fn to_string() -> string;
}

impl Printable for Point {
    fn to_string() -> string {
        return "(" + to_string(self.x) + ", " + to_string(self.y) + ")";
    }
}
```

`impl` blocks can also add methods without a trait:

```grey
impl Point {
    fn distance(other: Point) -> float {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        return sqrt(dx * dx + dy * dy);
    }
}
```

### 1.9 Lambdas

```grey
let double = |x: int| -> int { return x * 2; };
println(double(5));  // 10
```

### 1.10 Modules & Imports

```grey
import math;
from utils import helper;
import io as file_io;
```

### 1.11 Semicolons

- **Required** after `let`, `let mut`, and `const` declarations
- **Optional** after expression statements and `return`
- **Never used** after block-based constructs (`if`, `while`, `for`, `fn`, `struct`, `impl`, etc.)

```grey
let x = 42;               // semicolon required
println("hello");          // semicolon optional
return x;                  // semicolon optional

fn foo() {                 // no semicolon
    if true {              // no semicolon
        println("yes")     // semicolon optional
    }
}
```

---

## 2. Standard Library

Grey ships with 40+ built-in functions available without imports.

### 2.1 I/O

| Function | Signature | Description |
|----------|-----------|-------------|
| `print(value)` | `any -> void` | Print without newline |
| `println(value)` | `any -> void` | Print with newline |
| `input(prompt)` | `string -> string` | Read line from stdin |

### 2.2 Math

| Function | Signature | Description |
|----------|-----------|-------------|
| `abs(x)` | `number -> number` | Absolute value |
| `min(a, b)` | `number, number -> number` | Minimum of two values |
| `max(a, b)` | `number, number -> number` | Maximum of two values |
| `pow(base, exp)` | `number, number -> number` | Exponentiation |
| `sqrt(x)` | `number -> float` | Square root |
| `floor(x)` | `float -> int` | Floor |
| `ceil(x)` | `float -> int` | Ceiling |
| `round(x)` | `float -> int` | Round to nearest integer |

### 2.3 Strings

| Function | Signature | Description |
|----------|-----------|-------------|
| `len(s)` | `string -> int` | String length |
| `substr(s, start, len)` | `string, int, int -> string` | Substring |
| `concat(a, b)` | `string, string -> string` | Concatenate |
| `split(s, sep)` | `string, string -> [string]` | Split by separator |
| `trim(s)` | `string -> string` | Trim whitespace |
| `upper(s)` | `string -> string` | To uppercase |
| `lower(s)` | `string -> string` | To lowercase |
| `starts_with(s, prefix)` | `string, string -> bool` | Prefix check |
| `ends_with(s, suffix)` | `string, string -> bool` | Suffix check |
| `contains(s, sub)` | `string, string -> bool` | Substring check |
| `replace(s, old, new)` | `string, string, string -> string` | Replace occurrences |
| `char_at(s, i)` | `string, int -> string` | Character at index |

### 2.4 Arrays

| Function | Signature | Description |
|----------|-----------|-------------|
| `len(arr)` | `[T] -> int` | Array length |
| `push(arr, val)` | `[T], T -> void` | Append element |
| `pop(arr)` | `[T] -> T` | Remove and return last element |
| `sort(arr)` | `[T] -> [T]` | Sort array |
| `reverse(arr)` | `[T] -> [T]` | Reverse array |
| `range(start, end)` | `int, int -> [int]` | Generate integer range |

### 2.5 Type Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `typeof(value)` | `any -> string` | Runtime type name |
| `to_int(value)` | `any -> int` | Convert to integer |
| `to_float(value)` | `any -> float` | Convert to float |
| `to_string(value)` | `any -> string` | Convert to string |
| `is_nil(value)` | `any -> bool` | Check if nil |

### 2.6 System

| Function | Signature | Description |
|----------|-----------|-------------|
| `assert(cond)` | `bool -> void` | Assert condition is true |
| `panic(msg)` | `string -> void` | Halt with error message |
| `exit(code)` | `int -> void` | Exit with status code |
| `clock()` | `void -> float` | Current time in seconds |

---

## 3. CLI Reference

### `run` — Execute a Grey program

```bash
python -m grey run <file.grey> [options]
```

Compiles the source to VM bytecode in memory and executes it immediately.

Options:
- `--trace` — Print each VM instruction as it executes
- `--ast` — Print the parsed AST
- `--ir` — Print the generated IR
- `--dis` — Print bytecode disassembly
- `-O0` to `-O3` — Optimization level

### `compile` — Compile to bytecode

```bash
python -m grey compile <file.grey> [-o output.greyc] [--dis] [--json]
```

Produces a `.greyc` binary bytecode file. Use `--dis` to also write a `.greyd` disassembly file. Use `--json` to emit JSON metadata instead.

### `build` — Compile to native executable

```bash
python -m grey build <file.grey> [-o output]
```

Emits C via the C backend and invokes `gcc` to produce a native executable. Requires `gcc` on your PATH.

### `emit-asm` — Emit x86-64 assembly

```bash
python -m grey emit-asm <file.grey> [-o output.asm]
```

Generates NASM-syntax x86-64 assembly. Supports Windows x64 (default on Windows) and System V AMD64 calling conventions.

### `emit-c` — Emit C source

```bash
python -m grey emit-c <file.grey> [-o output.c]
```

Generates self-contained, portable C99 code with an embedded runtime. The output can be compiled with any C99-compliant compiler.

### `disasm` — Disassemble bytecode

```bash
python -m grey disasm <file.greyc>
```

Pretty-prints the contents of a compiled `.greyc` bytecode file: chunks, constants, instructions, and metadata.

### `repl` — Interactive REPL

```bash
python -m grey repl
```

Special REPL commands:

| Command | Description |
|---------|-------------|
| `:ast` | Print AST for the last input |
| `:ir` | Print IR for the last input |
| `:dis` | Print disassembly for the last input |
| `:clear` | Clear REPL state |
| `:help` | Show help |
| `:quit` / `:exit` | Exit the REPL |

### `fmt` — Format source code

```bash
python -m grey fmt <file.grey>
```

Prints the formatted source to stdout. Normalizes indentation (4 spaces), brace placement, and spacing.

### `lint` — Static analysis

```bash
python -m grey lint <file.grey>
```

Runs 12 lint rules and reports warnings. See [Linter Rules](#8-linter-rules) for details.

### `version` — Show version

```bash
python -m grey version
```

---

## 4. Compiler Architecture

The Grey compiler follows a classical multi-pass architecture:

```
Source Code
    │
    ▼
┌──────────┐
│  Lexer   │  Tokenization: source text → token stream
└────┬─────┘
     ▼
┌──────────┐
│  Parser  │  Recursive descent: tokens → AST
└────┬─────┘
     ▼
┌──────────┐
│ Semantic │  Type checking, scope resolution, mutability checks
│ Analyzer │
└────┬─────┘
     ▼
┌──────────┐
│ Normal-  │  Desugaring: compound assignments, for→while, elif chains
│ izer     │
└────┬─────┘
     ▼
┌──────────┐
│ IR Gen   │  AST → three-address code (SSA-style IR)
└────┬─────┘
     ▼
┌──────────┐
│ Optimizer│  6 configurable passes (levels O0–O3)
└────┬─────┘
     ▼
┌────────────────┬────────────────┬──────────────┐
│  VM Codegen    │  x86-64 Codegen│  C Codegen   │
│  (bytecode)    │  (NASM asm)    │  (C99 source)│
└───────┬────────┴───────┬────────┴──────┬───────┘
        ▼                ▼               ▼
   .greyc file      .asm file        .c file
        │
        ▼
   ┌─────────┐
   │ Grey VM │  Register-based execution
   └─────────┘
```

### 4.1 Frontend

#### Lexer (`grey/frontend/lexer.py`)

Full scanner supporting 80+ token types:
- Integer literals in decimal, hex (`0x`), binary (`0b`), and octal (`0o`)
- Float literals with scientific notation (`1.5e-3`)
- String literals with escape sequences (`\n`, `\t`, `\\`, `\"`, `\0`)
- Single-line (`//`) and nested block (`/* */`) comments
- All Grey keywords, operators, and punctuation

#### Parser (`grey/frontend/parser.py`)

Recursive descent parser producing a typed AST with ~30 node types:
- Operator precedence climbing for binary expressions
- Error recovery via synchronization on statement boundaries
- Full support for all Grey syntax constructs

#### Semantic Analyzer (`grey/frontend/semantic.py`)

Two-pass analysis:
1. **Declaration pass** — collect all top-level declarations (functions, structs, enums, traits)
2. **Analysis pass** — type inference, scope resolution, mutability enforcement, function signature validation

Errors are reported with file, line, and column information.

#### AST Normalizer (`grey/frontend/normalizer.py`)

Simplifies the AST before IR generation:
- Compound assignments (`x += 1` → `x = x + 1`)
- For-in loops → while loops
- Elif chains → nested if/else
- Constant folding of simple expressions

### 4.2 Middle End

#### IR (`grey/middle/ir.py`)

Three-address code with SSA-style temporaries:
- **Module** → list of functions + globals
- **Function** → list of basic blocks
- **BasicBlock** → sequence of instructions with a single terminator
- **Instruction** → `result = opcode operand1, operand2, ...`

Types: `i32`, `i64`, `f64`, `bool`, `str`, `ptr`, `void`, `array`, `struct`, `func`, `any`

#### IR Generator (`grey/middle/ir_generator.py`)

Translates the normalized AST into IR. Generates a `__main__` function for top-level statements and separate `IRFunction` entries for each function declaration.

#### Optimizer (`grey/middle/optimizer.py`)

6 optimization passes controlled by `-O` level:

| Level | Passes |
|-------|--------|
| `-O0` | None (debug mode) |
| `-O1` | Constant folding, dead code elimination |
| `-O2` | + constant propagation, strength reduction |
| `-O3` | + CSE, block merging |

### 4.3 Backend

#### VM Code Generator (`grey/backend/codegen.py`)

Translates IR into register-based VM bytecode:
- Simple linear register allocation (one register per SSA temp)
- Jump patching for control flow
- Constant pool management per chunk
- Special-case handling for builtins (`println`, `print`, `assert`)

#### Register Allocator (`grey/backend/register_alloc.py`)

Linear scan register allocator with:
- Liveness analysis
- Register spilling when pressure exceeds 256 registers
- Coalescing for MOVE elimination

#### Bytecode Emitter (`grey/backend/emitter.py`)

Serializes compiled programs in three formats:
- **Binary** (`.greyc`) — compact bytecode for the VM
- **Disassembly** (`.greyd`) — human-readable instruction listing
- **JSON** — structured metadata for tooling

#### x86-64 Code Generator (`grey/backend/x86_64_codegen.py`)

Generates NASM-syntax x86-64 assembly:
- Windows x64 calling convention (shadow space, RCX/RDX/R8/R9)
- System V AMD64 calling convention (RDI/RSI/RDX/RCX/R8/R9)
- Proper stack alignment, prologue/epilogue, extern linkage
- Calls `printf` and `pow` from libc

#### C Code Generator (`grey/backend/c_codegen.py`)

Generates portable, self-contained C99 source:
- Embedded runtime with `GreyArray` and `GreyStruct` types
- Proper function prototypes and forward declarations
- Compiles with `gcc -O2 output.c -o program -lm`

### 4.4 Runtime System

#### Virtual Machine (`grey/runtime/vm.py`)

Register-based VM (inspired by Lua and Dalvik):
- 256 general-purpose registers per call frame
- Fetch-decode-execute loop for 60+ opcodes
- Call stack with frame isolation
- Built-in function dispatch via the standard library
- Runtime type checking and error propagation with stack traces

#### Memory Manager (`grey/runtime/memory.py`)

Tagged value representation:
- `GreyValue` — tagged union with type, data, GC mark bit, and reference count
- Value types: nil, bool, int, float, string, array, struct, function, closure, native_fn
- Heap allocation for arrays and structs
- Global variable table

#### Garbage Collector (`grey/runtime/gc.py`)

Mark-and-sweep collector:
- Traces roots from the call stack, registers, and global table
- Adaptive threshold (doubles after each collection)
- Manual `gc.collect()` support
- Reference counting for simple/eager cleanup

#### Standard Library (`grey/runtime/stdlib.py`)

40+ built-in functions registered as native callables. See [Standard Library](#2-standard-library) for the full list.

### 4.5 Tooling

#### REPL (`grey/tools/repl.py`)

Interactive read-eval-print loop:
- Multi-line input (opens a block on `{`)
- Introspection commands: `:ast`, `:ir`, `:dis`
- State persists across inputs within a session

#### Formatter (`grey/tools/formatter.py`)

Automatic code formatting:
- 4-space indentation
- Consistent brace placement
- Normalized spacing around operators and keywords
- Token-based formatting (works even on syntactically incomplete files)

#### Linter (`grey/tools/linter.py`)

12 static analysis rules. See [Linter Rules](#8-linter-rules).

---

## 5. VM Architecture

### 5.1 Registers

Each call frame has 256 general-purpose registers (`R0`–`R255`). Function parameters are passed in `R0`, `R1`, etc. Return values are written to the caller's destination register.

### 5.2 Instruction Encoding

Instructions are 32 bits wide with two formats:

```
ABC format:  [opcode:8][A:8][B:8][C:8]
ABx format:  [opcode:8][A:8][Bx:16]        (Bx = B<<8 | C)
AsBx format: [opcode:8][A:8][sBx:16]       (signed 16-bit offset)
```

- **A** — destination register or first operand
- **B, C** — source registers or operands
- **Bx** — 16-bit unsigned immediate (constant index, global index)
- **sBx** — 16-bit signed offset (for jumps)

### 5.3 Opcode Reference

#### Load & Store
| Opcode | Format | Description |
|--------|--------|-------------|
| `LOAD_CONST` | `A, Bx` | `R[A] = constants[Bx]` |
| `LOAD_NIL` | `A` | `R[A] = nil` |
| `LOAD_TRUE` | `A` | `R[A] = true` |
| `LOAD_FALSE` | `A` | `R[A] = false` |
| `LOAD_LOCAL` | `A, B` | `R[A] = R[B]` |
| `STORE_LOCAL` | `A, B` | `R[B] = R[A]` |
| `LOAD_GLOBAL` | `A, Bx` | `R[A] = globals[name(Bx)]` |
| `STORE_GLOBAL` | `A, Bx` | `globals[name(Bx)] = R[A]` |
| `MOVE` | `A, B` | `R[A] = R[B]` |

#### Arithmetic
| Opcode | Format | Description |
|--------|--------|-------------|
| `ADD` | `A, B, C` | `R[A] = R[B] + R[C]` |
| `SUB` | `A, B, C` | `R[A] = R[B] - R[C]` |
| `MUL` | `A, B, C` | `R[A] = R[B] * R[C]` |
| `DIV` | `A, B, C` | `R[A] = R[B] / R[C]` |
| `MOD` | `A, B, C` | `R[A] = R[B] % R[C]` |
| `POW` | `A, B, C` | `R[A] = R[B] ** R[C]` |
| `NEG` | `A, B` | `R[A] = -R[B]` |

#### Comparison
| Opcode | Format | Description |
|--------|--------|-------------|
| `EQ` | `A, B, C` | `R[A] = R[B] == R[C]` |
| `NEQ` | `A, B, C` | `R[A] = R[B] != R[C]` |
| `LT` | `A, B, C` | `R[A] = R[B] < R[C]` |
| `GT` | `A, B, C` | `R[A] = R[B] > R[C]` |
| `LTE` | `A, B, C` | `R[A] = R[B] <= R[C]` |
| `GTE` | `A, B, C` | `R[A] = R[B] >= R[C]` |

#### Logical & Bitwise
| Opcode | Format | Description |
|--------|--------|-------------|
| `NOT` | `A, B` | `R[A] = not R[B]` |
| `AND` | `A, B, C` | `R[A] = R[B] and R[C]` |
| `OR` | `A, B, C` | `R[A] = R[B] or R[C]` |
| `BIT_AND` | `A, B, C` | `R[A] = R[B] & R[C]` |
| `BIT_OR` | `A, B, C` | `R[A] = R[B] \| R[C]` |
| `BIT_XOR` | `A, B, C` | `R[A] = R[B] ^ R[C]` |
| `BIT_NOT` | `A, B` | `R[A] = ~R[B]` |
| `SHL` | `A, B, C` | `R[A] = R[B] << R[C]` |
| `SHR` | `A, B, C` | `R[A] = R[B] >> R[C]` |

#### Control Flow
| Opcode | Format | Description |
|--------|--------|-------------|
| `JUMP` | `sBx` | `PC += sBx` |
| `JUMP_IF_TRUE` | `A, sBx` | `if R[A]: PC += sBx` |
| `JUMP_IF_FALSE` | `A, sBx` | `if not R[A]: PC += sBx` |
| `CALL` | `A, B, C` | `R[A] = call R[B] with C args` |
| `RETURN` | `A` | Return `R[A]` to caller |
| `RETURN_NONE` | — | Return nil to caller |

#### Array & Struct
| Opcode | Format | Description |
|--------|--------|-------------|
| `ARRAY_NEW` | `A, B` | `R[A] = new array(size=B)` |
| `ARRAY_GET` | `A, B, C` | `R[A] = R[B][R[C]]` |
| `ARRAY_SET` | `A, B, C` | `R[A][R[B]] = R[C]` |
| `ARRAY_LEN` | `A, B` | `R[A] = len(R[B])` |
| `ARRAY_PUSH` | `A, B` | `R[A].push(R[B])` |
| `ARRAY_POP` | `A, B` | `R[A] = R[B].pop()` |
| `STRUCT_NEW` | `A` | `R[A] = new struct` |
| `FIELD_GET` | `A, B, C` | `R[A] = R[B].field(C)` |
| `FIELD_SET` | `A, B, C` | `R[A].field(B) = R[C]` |

#### I/O & System
| Opcode | Format | Description |
|--------|--------|-------------|
| `PRINT` | `A` | Print `R[A]` |
| `PRINTLN` | `A` | Print `R[A]` with newline |
| `INPUT` | `A, B` | `R[A] = input(R[B])` |
| `ASSERT` | `A, B` | Assert `R[A]`, message in `R[B]` |
| `BUILTIN_CALL` | `A, B, C` | Call builtin `constants[B]` with C args |
| `HALT` | — | Stop execution |
| `NOP` | — | No operation |

---

## 6. Binary Format (.greyc)

The `.greyc` file format is a compact binary representation of a compiled Grey program.

### File Structure

```
┌────────────────┐
│ Magic: "GREY"  │  4 bytes
├────────────────┤
│ Version        │  2 bytes (major, minor)
├────────────────┤
│ String Table   │  count (4 bytes) + length-prefixed UTF-8 strings
├────────────────┤
│ Global Names   │  count (4 bytes) + length-prefixed UTF-8 strings
├────────────────┤
│ Main Chunk Idx │  4 bytes
├────────────────┤
│ Chunk Count    │  4 bytes
├────────────────┤
│ Chunks[]       │  repeated chunk entries
│   ├── Name     │    length-prefixed string
│   ├── Params   │    4 bytes
│   ├── Locals   │    4 bytes
│   ├── Constants│    count + typed constant entries
│   └── Code     │    count + 32-bit instruction words
└────────────────┘
```

Load and execute a `.greyc` file:

```bash
python -m grey compile examples/hello.grey -o hello.greyc
python -m grey disasm hello.greyc
```

---

## 7. Optimization Passes

### Constant Folding

Evaluates constant expressions at compile time:

```
// Before                    // After (IR)
let x = 2 + 3;      →      STORE x, 5
let y = 10 * 2;     →      STORE y, 20
```

### Dead Code Elimination

Removes instructions whose results are never used and blocks that are never reached.

### Constant Propagation

Replaces variable references with their known constant values:

```
let x = 5;
let y = x + 1;   →   let y = 5 + 1;   →   let y = 6;
```

### Strength Reduction

Replaces expensive operations with cheaper equivalents:

| Original | Optimized |
|----------|-----------|
| `x * 2` | `x + x` |
| `x * 4` | `x << 2` |
| `x / 2` | `x >> 1` |
| `x ** 2` | `x * x` |

### Common Subexpression Elimination (CSE)

Identifies repeated computations and reuses the first result:

```
let a = x + y;
let b = x + y;   →   let b = a;
```

### Block Merging

Merges basic blocks connected by unconditional jumps into a single block, reducing branch overhead.

---

## 8. Linter Rules

| Rule | Severity | Description |
|------|----------|-------------|
| `W001` | Warning | Unused variable |
| `W002` | Warning | Variable shadows outer scope |
| `W003` | Warning | Function too long (> 50 lines) |
| `W004` | Warning | Too many parameters (> 6) |
| `W005` | Warning | Deeply nested code (> 4 levels) |
| `W006` | Warning | Empty block |
| `W007` | Warning | Unreachable code after return/break/continue |
| `W008` | Warning | Comparison to nil (use `is_nil()`) |
| `W009` | Warning | Redundant boolean expression (`x == true`) |
| `W010` | Warning | Missing return in non-void function |
| `W011` | Warning | Mutable variable never mutated |
| `W012` | Warning | Use of deprecated function |

Run the linter:

```bash
python -m grey lint examples/hello.grey
```

---

## 9. Native Backends

### C Backend

The C code generator produces self-contained C99 source files with an embedded runtime. The generated code includes:

- Forward declarations for all functions
- A `GreyArray` type with push/pop/get/set operations
- A `GreyStruct` type with field-based access
- A `grey_pow()` helper for the `**` operator
- A `main()` entry point that calls the Grey `__main__` function

```bash
python -m grey emit-c program.grey -o program.c
gcc -O2 program.c -o program -lm
./program
```

### x86-64 Assembly Backend

The assembly code generator produces NASM-syntax x86-64 assembly with:

- Automatic calling convention selection (Windows x64 vs System V AMD64)
- Proper stack frame management with 16-byte alignment
- Shadow space (32 bytes) on Windows x64
- External calls to `printf` and `pow` from libc
- BSS section for global variables

```bash
python -m grey emit-asm program.grey -o program.asm

# Windows
nasm -f win64 program.asm -o program.o
gcc program.o -o program.exe -lm

# Linux
nasm -f elf64 program.asm -o program.o
gcc program.o -o program -lm -no-pie
```

### One-Step Build

```bash
python -m grey build program.grey -o program
```

This emits C code to a temporary file, invokes `gcc`, and produces a native executable. Requires `gcc` on your PATH.
