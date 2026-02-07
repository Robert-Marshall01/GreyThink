# Grey Runtime Engine

A complete bytecode runtime interpreter for the Grey++ language, similar in architecture to CPython. Features a NaN-boxed value system, mark-sweep garbage collector, closure-based function model, class/inheritance hierarchy, module serialization (`.greyc` files), and a full standard library.

Warning: may contain instability. Verify stability before implementing in a high-stakes environment.
## Architecture

```
Grey++ Source → [Compiler] → Bytecode (.greyc) → [Grey Runtime VM] → Execution
```

### Core Subsystems

| Subsystem | Header | Description |
|-----------|--------|-------------|
| **Value System** | `include/grey/common.h` | NaN-boxed 64-bit values: nil, bool, int, float, object |
| **Bytecode** | `include/grey/bytecode.h` | ~80 opcodes with `Chunk` bytecode container |
| **Objects** | `include/grey/objects.h` | String, Array, Map, Set, Function, Class, Instance, etc. |
| **Memory** | `include/grey/memory.h` | Bump/Arena allocators, ValueStack, GarbageCollector |
| **VM** | `include/grey/vm.h` | Stack-based virtual machine with dispatch loop |
| **ABI** | `include/grey/abi.h` | Binary module serialization (`ModuleWriter` / `ModuleReader`) |
| **Modules** | `include/grey/module.h` | Module loading, resolution, dependency tracking |
| **Concurrency** | `include/grey/concurrency.h` | Fibers, Futures, Channels, AsyncRuntime |
| **Sandbox** | `include/grey/sandbox.h` | Security policies, resource limits, deterministic mode |
| **Debug** | `include/grey/debug.h` | Debugger, Profiler, Logger |
| **JIT** | `include/grey/jit.h` | JIT compiler stub (IR → native) |
| **StdLib** | `include/grey/stdlib/` | IO, System, Collections, Networking, Crypto |

## Requirements

- **C++20** compiler (MSVC 2022+, GCC 11+, Clang 14+)
- **CMake 3.20+**
- Windows/Linux/macOS

## Building

### Windows (Visual Studio)

```powershell
# Open VS Developer Command Prompt, then:
mkdir build
cd build
cmake .. -G "Visual Studio 17 2022"
cmake --build . --config Release
```

### Windows (MinGW / MSYS2)

```bash
mkdir build && cd build
cmake .. -G "MinGW Makefiles"
cmake --build .
```

### Linux / macOS

```bash
mkdir build && cd build
cmake ..
make -j$(nproc)
```

### Build Outputs

| Target | Description |
|--------|-------------|
| `grey` | CLI executable (REPL, file runner, disassembler) |
| `grey_tests` | Test suite (~35 tests) |
| `example_arithmetic` | Arithmetic operations demo |
| `example_variables` | Variables & control flow demo |
| `example_strings_collections` | Strings, arrays, maps demo |
| `example_functions` | Functions, natives, closures demo |
| `example_classes` | Classes, instances, inheritance demo |
| `example_exceptions` | Exception handling demo |
| `example_bytecode_serial` | Module serialization (.greyc) demo |
| `example_full_program` | FizzBuzz, Fibonacci, type system demo |

## Usage

### CLI

```bash
# Run a compiled module
grey program.greyc

# Interactive REPL
grey --repl

# Disassemble a module
grey --disasm module.greyc

# Run with profiler
grey --profile program.greyc

# Run in sandbox mode
grey --sandbox --deterministic contract.greyc

# Run with debugger
grey --debug program.greyc
```

### Example Programs

Each example is self-contained and demonstrates a different runtime feature:

```bash
# Run examples after building
./build/example_arithmetic           # Basic math: (10+20)*3 - 15/5 = 87
./build/example_variables            # Variables, if-else, bool logic
./build/example_strings_collections  # String concat, arrays, maps, GC
./build/example_functions            # Native functions, print, factorial
./build/example_classes              # Classes, instances, inheritance
./build/example_exceptions           # Error handling, type validation
./build/example_bytecode_serial      # Serialize/deserialize .greyc files
./build/example_full_program         # FizzBuzz, Fibonacci, typeof
```

### Embedding the Runtime

```cpp
#include "grey/runtime.h"
#include "grey/vm.h"
#include "grey/bytecode.h"
#include "grey/objects.h"

using namespace grey;

int main() {
    VMConfig config;
    VM vm(config);

    // Register a native function
    vm.define_native("greet", [&vm](int argc, Value* args) -> Value {
        std::cout << "Hello from Grey Runtime!" << std::endl;
        return Value::nil();
    }, 0);

    // Build bytecode
    Chunk chunk;
    chunk.emit_constant(Value::integer(2), 1);
    chunk.emit_constant(Value::integer(3), 1);
    chunk.emit(OpCode::ADD, 1);
    chunk.emit(OpCode::RETURN, 2);

    auto* fn = new ObjFunction();
    fn->chunk = chunk;
    vm.gc().track(fn);

    Value result = vm.execute(fn);  // => 5
}
```

## Bytecode Format

Instructions are variable-width. The opcode is always 1 byte, followed by optional operands:

| Opcode | Operands | Description |
|--------|----------|-------------|
| `PUSH_CONST` | u16 index | Push constant from pool |
| `PUSH_NIL` | — | Push nil |
| `PUSH_TRUE` | — | Push true |
| `PUSH_FALSE` | — | Push false |
| `ADD` | — | Pop two, push sum |
| `SUB` | — | Pop two, push difference |
| `MUL` | — | Pop two, push product |
| `DIV` | — | Pop two, push quotient |
| `EQ` / `LT` / `GT` | — | Comparison |
| `JUMP` | i16 offset | Unconditional jump |
| `JUMP_IF_FALSE` | i16 offset | Conditional jump |
| `CALL` | u8 arg_count | Call function |
| `RETURN` | — | Return from function |
| `LOAD_LOCAL` / `STORE_LOCAL` | u16 slot | Local variable access |
| `NEW_ARRAY` | u16 count | Create array from stack |
| `NEW_MAP` | u16 count | Create map from stack pairs |

See [include/grey/bytecode.h](include/grey/bytecode.h) for the full ~80 opcode list.

## Module Format (.greyc)

Binary serialization uses the `ModuleWriter`/`ModuleReader` API:

```
Header:    magic(4) | version(2) | abi(16) | counts(16)
Strings:   [length(u32) | chars(utf8)]...
Functions: [entry(FunctionEntry) | code(bytes) | constants(Values)]...
Types:     [TypeDescriptor]...
Debug:     [DebugMapping]...
```

## Testing

```bash
cd build
ctest --output-on-failure
# Or run directly:
./grey_tests
```

## License

Copyright (c) 2026 Grey Runtime Project. All rights reserved.
