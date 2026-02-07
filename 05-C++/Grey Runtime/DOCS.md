# Grey Runtime Documentation

Comprehensive technical documentation for the Grey Runtime bytecode interpreter engine.

## Table of Contents

- [Overview](#overview)
- [Value System](#value-system)
- [Bytecode Instruction Set](#bytecode-instruction-set)
- [Virtual Machine](#virtual-machine)
- [Object Model](#object-model)
- [Memory Management](#memory-management)
- [Module System](#module-system)
- [Standard Library](#standard-library)
- [Concurrency](#concurrency)
- [Sandbox & Security](#sandbox--security)
- [Debugging & Profiling](#debugging--profiling)
- [ABI & Binary Format](#abi--binary-format)
- [Embedding Guide](#embedding-guide)
- [API Reference](#api-reference)

---

## Overview

Grey Runtime is a stack-based bytecode virtual machine for the Grey++ language. It follows a similar architecture to CPython: source code is compiled to bytecode, serialized into `.greyc` module files, and executed by the VM.

```
Grey++ Source Code
       │
       ▼
  [Compiler]        (external — not part of this runtime)
       │
       ▼
  Bytecode (.greyc)
       │
       ▼
  ┌─────────────────────────────────────────────┐
  │              Grey Runtime                    │
  │  ┌─────────┐  ┌──────┐  ┌───────────────┐  │
  │  │ Module  │→ │  VM  │→ │ GC / Memory   │  │
  │  │ Loader  │  │      │  │ Management    │  │
  │  └─────────┘  └──────┘  └───────────────┘  │
  │  ┌─────────┐  ┌──────────┐  ┌───────────┐  │
  │  │ StdLib  │  │ Debugger │  │  Sandbox  │  │
  │  └─────────┘  └──────────┘  └───────────┘  │
  └─────────────────────────────────────────────┘
       │
       ▼
    Result
```

### Key Design Decisions

- **NaN-boxed values**: All values are 64 bits, using IEEE 754 NaN space to encode type tags
- **Mark-sweep GC**: Simple, predictable garbage collection with generational hints
- **Closure-based functions**: All functions are closures with captured upvalues
- **Single-pass dispatch**: Bytecode is executed via a `switch` dispatch loop (no computed goto)
- **Header-heavy design**: Most types are defined in headers for inlining

---

## Value System

All values in Grey Runtime are represented as 64-bit NaN-boxed values. This means a single `Value` type can hold nil, booleans, integers, floating-point numbers, and object pointers without heap allocation for primitives.

### Bit Layout

```
IEEE 754 double NaN:  0 11111111111 1xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
                      S Exponent    Quiet NaN bit + payload (51 bits)

Grey Value encoding:
  QNAN  = 0x7FF8_0000_0000_0000   (quiet NaN prefix — 13 bits)
  Tags occupy bits 48-51:
    TAG_NIL   = 0x0001_0000_0000_0000
    TAG_FALSE = 0x0002_0000_0000_0000
    TAG_TRUE  = 0x0003_0000_0000_0000
    TAG_INT   = 0x0004_0000_0000_0000
    TAG_OBJ   = 0x8000_0000_0000_0000  (sign bit — 64-bit pointer in lower 48 bits)
```

### Type Predicates

| Method | Returns true when |
|--------|-------------------|
| `is_nil()` | Value is nil |
| `is_bool()` / `is_boolean()` | Value is true or false |
| `is_int()` | Value is a 48-bit signed integer |
| `is_number()` | Value is a double-precision float |
| `is_object()` | Value is a heap-allocated object pointer |
| `is_truthy()` | Value is not nil, not false, not zero |

### Value Creation

```cpp
Value::nil()            // nil
Value::boolean(true)    // true
Value::boolean(false)   // false
Value::integer(42)      // 48-bit signed integer
Value::number(3.14)     // IEEE 754 double
Value::object(ptr)      // heap object pointer
```

### Integer Range

Integers are stored in 48 bits with sign extension, giving a range of:
- Min: -140,737,488,355,328 (-2^47)
- Max:  140,737,488,355,327 (2^47 - 1)

Values outside this range should use `Value::number()` instead.

---

## Bytecode Instruction Set

The VM uses variable-width instructions. Each instruction starts with a 1-byte opcode, optionally followed by operands.

### Stack Operations (0x00–0x0F)

| Opcode | Hex | Operands | Stack Effect | Description |
|--------|-----|----------|--------------|-------------|
| `NOP` | 0x00 | — | — | No operation |
| `PUSH_NIL` | 0x01 | — | → nil | Push nil |
| `PUSH_TRUE` | 0x02 | — | → true | Push true |
| `PUSH_FALSE` | 0x03 | — | → false | Push false |
| `PUSH_CONST` | 0x04 | u16 index | → value | Push constant from pool |
| `PUSH_CONST_LONG` | 0x05 | u32 index | → value | Push constant (extended) |
| `PUSH_INT` | 0x06 | i32 value | → int | Push immediate integer |
| `PUSH_ZERO` | 0x07 | — | → 0 | Push integer 0 |
| `PUSH_ONE` | 0x08 | — | → 1 | Push integer 1 |
| `POP` | 0x09 | — | value → | Discard top of stack |
| `DUP` | 0x0A | — | a → a a | Duplicate top |
| `DUP2` | 0x0B | — | a b → a b a b | Duplicate top two |
| `SWAP` | 0x0C | — | a b → b a | Swap top two |
| `ROT3` | 0x0D | — | a b c → c a b | Rotate top three |

### Arithmetic (0x20–0x2D)

| Opcode | Hex | Stack Effect | Description |
|--------|-----|--------------|-------------|
| `ADD` | 0x20 | a b → a+b | Addition (also string concat) |
| `SUB` | 0x21 | a b → a-b | Subtraction |
| `MUL` | 0x22 | a b → a*b | Multiplication |
| `DIV` | 0x23 | a b → a/b | Division |
| `MOD` | 0x24 | a b → a%b | Modulo |
| `NEG` | 0x25 | a → -a | Negate |
| `POW` | 0x26 | a b → a^b | Power |
| `FLOOR_DIV` | 0x27 | a b → a//b | Floor division |
| `BIT_AND` | 0x28 | a b → a&b | Bitwise AND |
| `BIT_OR` | 0x29 | a b → a\|b | Bitwise OR |
| `BIT_XOR` | 0x2A | a b → a^b | Bitwise XOR |
| `BIT_NOT` | 0x2B | a → ~a | Bitwise NOT |
| `SHL` | 0x2C | a b → a<<b | Shift left |
| `SHR` | 0x2D | a b → a>>b | Shift right |

### Comparison (0x30–0x35)

| Opcode | Stack Effect | Description |
|--------|--------------|-------------|
| `EQ` | a b → bool | Equal |
| `NEQ` | a b → bool | Not equal |
| `LT` | a b → bool | Less than |
| `LTE` | a b → bool | Less than or equal |
| `GT` | a b → bool | Greater than |
| `GTE` | a b → bool | Greater than or equal |

### Logical (0x38–0x3A)

| Opcode | Stack Effect | Description |
|--------|--------------|-------------|
| `NOT` | a → !a | Logical NOT |
| `AND` | a b → a&&b | Logical AND |
| `OR` | a b → a\|\|b | Logical OR |

### Variables (0x40–0x4A)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `LOAD_LOCAL` | u16 slot | Load local variable |
| `STORE_LOCAL` | u16 slot | Store local variable |
| `LOAD_GLOBAL` | u16 name_idx | Load global by name |
| `STORE_GLOBAL` | u16 name_idx | Store global by name |
| `LOAD_UPVALUE` | u16 index | Load closure upvalue |
| `STORE_UPVALUE` | u16 index | Store closure upvalue |
| `CLOSE_UPVALUE` | — | Close upvalue on stack top |
| `LOAD_FIELD` | u16 name_idx | Load object field |
| `STORE_FIELD` | u16 name_idx | Store object field |
| `LOAD_INDEX` | — | Load obj[index] |
| `STORE_INDEX` | — | Store obj[index] = val |

### Control Flow (0x60–0x65)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `JUMP` | i16 offset | Unconditional jump |
| `JUMP_LONG` | i32 offset | Long unconditional jump |
| `JUMP_IF_TRUE` | i16 offset | Jump if top is truthy |
| `JUMP_IF_FALSE` | i16 offset | Jump if top is falsy |
| `JUMP_IF_NIL` | i16 offset | Jump if top is nil |
| `LOOP` | u16 offset | Loop back (subtracts offset) |

### Functions (0x70–0x75)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `CALL` | u8 arg_count | Call function |
| `CALL_METHOD` | u16 name, u8 argc | Call method on object |
| `RETURN` | — | Return from function |
| `RETURN_NIL` | — | Return nil |
| `CLOSURE` | u16 func_idx | Create closure |
| `BIND_METHOD` | — | Bind method to class |

### Objects & Classes (0x80–0x85)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `NEW_CLASS` | u16 name_idx | Create new class |
| `INHERIT` | — | Set superclass |
| `NEW_INSTANCE` | — | Instantiate class |
| `NEW_ARRAY` | u16 count | Create array from stack elements |
| `NEW_MAP` | u16 count | Create map from stack key-value pairs |
| `NEW_SET` | u16 count | Create set from stack elements |

### Modules (0x90–0x92)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `IMPORT` | u16 name_idx | Import module |
| `IMPORT_FROM` | u16 mod_idx, u16 sym_idx | Import symbol from module |
| `EXPORT` | u16 name_idx | Export symbol |

### Concurrency (0xA0–0xA4)

| Opcode | Description |
|--------|-------------|
| `SPAWN` | Spawn fiber/task |
| `YIELD` | Yield current fiber |
| `AWAIT` | Await future |
| `SEND` | Send value to channel |
| `RECV` | Receive from channel |

### Error Handling (0xB0–0xB3)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `TRY_BEGIN` | u16 catch_off, u16 finally_off | Begin try block |
| `TRY_END` | — | End try block |
| `THROW` | — | Throw exception (top of stack) |
| `CATCH` | — | Push caught exception |

### Type Operations (0xC0–0xC2)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `TYPE_CHECK` | u16 type_idx | Check type, push bool |
| `TYPE_CAST` | u16 type_idx | Cast value to type |
| `INSTANCEOF` | — | Check instance-of relationship |

### Iterators (0xD0–0xD2)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `ITER_BEGIN` | — | Begin iteration over collection |
| `ITER_NEXT` | i16 end_offset | Get next element or jump |
| `ITER_CLOSE` | — | Close iterator |

### Debug (0xF0–0xF2)

| Opcode | Operands | Description |
|--------|----------|-------------|
| `DEBUG_BREAK` | — | Trigger debugger breakpoint |
| `DEBUG_LINE` | u16 line | Set current line number |
| `DEBUG_PRINT` | — | Print top of stack to stdout |

### System (0xFF)

| Opcode | Description |
|--------|-------------|
| `HALT` | Stop execution |

---

## Virtual Machine

### Architecture

The VM is a stack-based interpreter with:
- **Value stack**: Fixed-size stack (default 65,536 slots) for operands
- **Call frames**: Stack of call frames (max 4,096 depth) tracking return addresses
- **Globals table**: Hash map of global variable bindings
- **String interning**: Deduplication of string objects
- **GC integration**: Roots are traced from the stack, frames, and globals

### Execution Model

```cpp
VMConfig config;
config.stack_size = 65536;
config.max_call_depth = 4096;
config.gc_threshold = 1024 * 1024;  // 1MB

VM vm(config);

// Register native functions
vm.define_native("print", print_fn, 0);

// Execute a function
ObjFunction* main_fn = /* loaded from .greyc */;
Value result = vm.execute(main_fn);
```

### Call Convention

1. Caller pushes the callee value (function/closure) onto the stack
2. Caller pushes arguments left-to-right
3. `CALL argcount` instruction initiates the call
4. VM creates a new `CallFrame` pointing into the stack
5. Callee's locals occupy stack slots starting from the frame base
6. `RETURN` pops the frame and pushes the return value

### Native Functions

Native functions have the signature:
```cpp
using NativeFn = std::function<Value(int argc, Value* args)>;
```

Register them with:
```cpp
vm.define_native("function_name", callback, arity);
```

The `arity` parameter is informational; the VM passes the actual argument count.

---

## Object Model

All heap-allocated values extend `ObjHeader`:

```cpp
struct ObjHeader {
    ObjType type;          // discriminator enum
    bool    marked = false; // GC mark bit
    u32     hash = 0;      // cached hash
    ObjHeader* next = nullptr; // GC linked list
};
```

### Object Types

| Type | Class | Key Members |
|------|-------|-------------|
| String | `ObjString` | `std::string value`, `u32 hash` |
| Array | `ObjArray` | `std::vector<Value> elements` |
| Map | `ObjMap` | Open-addressing hash table with `MapEntry[]` |
| Set | `ObjSet` | `std::unordered_set<u64>` of value bits |
| Buffer | `ObjBuffer` | `std::vector<byte> data` |
| Function | `ObjFunction` | `Chunk chunk`, `u16 arity`, `u16 local_count` |
| Closure | `ObjClosure` | `ObjFunction*`, `vector<ObjUpvalue*>` |
| Upvalue | `ObjUpvalue` | `Value* location` (open) or `Value closed` |
| Class | `ObjClass` | `string name`, `ObjClass* superclass`, methods map |
| Instance | `ObjInstance` | `ObjClass* klass`, `ObjMap fields` |
| Module | `ObjModule` | `string name`, `ObjMap exports` |
| Fiber | `ObjFiber` | Cooperative coroutine with own stack |
| Future | `ObjFuture` | Awaitable async result |
| Channel | `ObjChannel` | Bounded or unbounded message queue |
| Iterator | `ObjIterator` | Iteration state for collections |
| NativeObj | `ObjNative` | Wrapped `NativeFn` with name and arity |
| BoundMethod | `ObjBoundMethod` | `Value receiver` + `ObjClosure* method` |

### String Interning

The VM interns strings to enable O(1) equality comparison by pointer. Use `make_string(gc, str)` to create GC-tracked strings.

### ObjMap

`ObjMap` uses open-addressing with linear probing. Keys are compared by `Value::operator==` (raw bit comparison). For string-keyed lookups across different `ObjString` pointers, iterate entries and compare string values directly.

---

## Memory Management

### Allocators

| Allocator | Use Case |
|-----------|----------|
| `BumpAllocator` | Fast, sequential allocation; bulk reset |
| `ArenaAllocator` | Multi-block arena; grows as needed |
| `PoolAllocator<T>` | Fixed-size object pool with free list |

### Garbage Collector

The GC uses a **mark-sweep** algorithm:

1. **Mark phase**: Starting from roots (stack, globals, frames), traverse all reachable objects
2. **Sweep phase**: Walk the allocation list, free unmarked objects, clear marks on survivors

```cpp
GarbageCollector gc;

// Track new objects
auto* str = new ObjString("hello");
gc.track(str);

// Manual collection
gc.collect();

// Query stats
size_t alive = gc.object_count();
size_t runs  = gc.collection_count();
```

The GC triggers automatically when `bytes_allocated` exceeds `gc_threshold`. After each collection, the threshold is multiplied by `gc_grow_factor` (default 2.0).

### ValueStack

```cpp
ValueStack stack;
stack.push(Value::integer(42));
Value top = stack.peek(0);   // look at top
Value val = stack.pop();      // remove top
size_t n  = stack.size();     // current depth
```

---

## Module System

### Module Files (.greyc)

Modules are serialized using the `ModuleWriter` and deserialized using `ModuleReader`:

```cpp
// Writing
ModuleWriter writer;
writer.begin();
writer.add_string("main");
writer.add_function(entry, chunk);
auto bytes = writer.finish();

// Reading
ModuleReader reader(bytes);
if (reader.validate()) {
    auto header = reader.header();
    auto functions = reader.functions();
    auto chunk = reader.get_function_chunk(0);
}
```

### Binary Format

```
┌──────────────────────────────────────┐
│ ModuleHeader (40 bytes)              │
│   magic: 0x47525959 ("GRYY")        │
│   version_major, version_minor       │
│   abi_info (ABIInfo struct)          │
│   function_count, string_count       │
│   type_count, debug_mapping_count    │
├──────────────────────────────────────┤
│ String Table                         │
│   [u32 length | utf8 chars]...       │
├──────────────────────────────────────┤
│ Function Table                       │
│   [FunctionEntry | code | consts]... │
├──────────────────────────────────────┤
│ Type Table                           │
│   [TypeDescriptor]...                │
├──────────────────────────────────────┤
│ Debug Mappings                       │
│   [DebugMapping]...                  │
└──────────────────────────────────────┘
```

### Module Loader

```cpp
ModuleLoader loader;
loader.add_search_path("./modules");
loader.add_search_path("/usr/lib/grey");

ObjModule* mod = loader.load("math", vm);
```

The loader searches paths in order, reads `.greyc` files, deserializes them, and caches loaded modules.

---

## Standard Library

### IO (`include/grey/stdlib/io.h`)

| Native Function | Arguments | Description |
|----------------|-----------|-------------|
| `file_read(path)` | string | Read file contents as string |
| `file_write(path, data)` | string, string | Write string to file |
| `file_append(path, data)` | string, string | Append to file |
| `file_exists(path)` | string | Check if file exists |
| `file_delete(path)` | string | Delete a file |
| `file_size(path)` | string | Get file size in bytes |
| `dir_create(path)` | string | Create directory recursively |
| `dir_list(path)` | string | List directory contents as array |
| `stdin_read_line()` | — | Read line from stdin |
| `stdout_write(...)` | any | Write to stdout |
| `stderr_write(...)` | any | Write to stderr |

### System (`include/grey/stdlib/system.h`)

| Native Function | Description |
|----------------|-------------|
| `time_now()` | Current time in milliseconds |
| `time_now_ns()` | Current time in nanoseconds |
| `sleep(ms)` | Sleep for milliseconds |
| `env_get(key)` | Get environment variable |
| `env_set(key, val)` | Set environment variable |
| `cwd()` | Get current working directory |
| `exec(cmd)` | Execute shell command, returns map |
| `exit(code)` | Exit process |
| `pid()` | Get process ID |
| `platform()` | Get OS name string |
| `arch()` | Get CPU architecture string |
| `cpu_count()` | Get number of CPU cores |
| `total_memory()` | Get total system memory |

### Collections (`include/grey/stdlib/collections.h`)

| Native Function | Description |
|----------------|-------------|
| `array_push(arr, val)` | Push value to array |
| `array_pop(arr)` | Pop from array |
| `array_length(arr)` | Get array length |
| `array_slice(arr, start, end)` | Slice array |
| `array_sort(arr)` | Sort array in place |
| `array_reverse(arr)` | Reverse array |
| `array_map(arr, fn)` | Map function over array |
| `array_filter(arr, fn)` | Filter array by predicate |
| `map_keys(map)` | Get map keys as array |
| `map_values(map)` | Get map values as array |
| `map_has(map, key)` | Check if key exists |
| `map_delete(map, key)` | Remove key from map |
| `set_add(set, val)` | Add to set |
| `set_has(set, val)` | Check membership |
| `set_union(a, b)` | Set union |
| `set_intersect(a, b)` | Set intersection |

### Networking (`include/grey/stdlib/net.h`)

| Native Function | Description |
|----------------|-------------|
| `dns_resolve(hostname)` | Resolve hostname to IP array |
| `http_get(url)` | HTTP GET, returns response map |
| `http_post(url, body, type)` | HTTP POST with body |
| `Channel(capacity)` | Create message channel |
| `channel_send(ch, val)` | Send value to channel |
| `channel_recv(ch)` | Receive from channel |
| `channel_close(ch)` | Close channel |

### Crypto (`include/grey/stdlib/crypto.h`)

| Function | Description |
|----------|-------------|
| `sha256(data, len)` | SHA-256 hash (returns 32 bytes) |
| `sha256_hex(str)` | SHA-256 hex digest |
| `hmac_sha256(key, klen, data, dlen)` | HMAC-SHA-256 |
| `random_bytes(count)` | Cryptographically random bytes |
| `bytes_to_hex(data, len)` | Convert bytes to hex string |
| `hex_to_bytes(hex)` | Convert hex string to bytes |

---

## Concurrency

### Fibers

Fibers are cooperative coroutines with their own stacks:

```
SPAWN    → Create a new fiber from a closure
YIELD    → Suspend current fiber, return to scheduler
```

### Futures

```
SPAWN    → With AsyncRuntime, spawns on thread pool
AWAIT    → Block until future resolves
```

### Channels

Channels provide typed communication between fibers/threads:

```cpp
auto* chan = new ObjChannel(capacity);
chan->send(Value::integer(42));

Value received;
chan->receive(received);  // blocks if empty

chan->close();
```

### AsyncRuntime

```cpp
AsyncRuntime async(4);  // 4 worker threads
async.start();

auto future = async.spawn([](){ return heavy_computation(); });
auto result = future->await();

async.stop();
```

---

## Sandbox & Security

The sandbox constrains resource usage for untrusted code:

```cpp
Sandbox::Policy policy;
policy.allow_file_read = false;
policy.allow_file_write = false;
policy.allow_network = false;
policy.allow_process = false;
policy.max_memory = 64 * 1024 * 1024;  // 64MB
policy.max_cpu_cycles = 1000000;
policy.max_call_depth = 256;

Sandbox sandbox(policy);
sandbox.enforce(vm);
```

### Deterministic Mode

Ensures reproducible execution (useful for smart contracts, testing):

```cpp
DeterministicEngine det;
det.enable();
det.apply_defaults(config);
// Disables random, networking, timers, etc.
```

---

## Debugging & Profiling

### Debugger

```cpp
Debugger debugger;
vm.set_debugger(&debugger);

debugger.set_breakpoint("main.grey", 42);
debugger.on_break([](VM& vm, u32 line) {
    auto trace = vm.format_stack_trace();
    std::cout << trace << std::endl;
});
```

### Profiler

```cpp
Profiler profiler;
profiler.start();

// ... run code ...

profiler.stop();
profiler.report(std::cout);           // formatted table
profiler.export_csv("profile.csv");   // raw CSV data
```

### Logger

```cpp
Logger logger;
logger.set_level(LogLevel::Debug);
logger.set_output(std::cerr);

logger.debug("VM", "Executing opcode %d", op);
logger.info("GC", "Collected %zu objects", freed);
logger.warn("Module", "Deprecated API used");
logger.error("Runtime", "Stack overflow");
```

Log levels: `Trace`, `Debug`, `Info`, `Warn`, `Error`, `Fatal`.

---

## ABI & Binary Format

### ABI Compatibility

```cpp
ABIInfo abi = current_abi();
// abi.version_major, abi.version_minor
// abi.pointer_size (4 or 8)
// abi.value_size (8)
// abi.endianness (0 = little, 1 = big)

bool ok = abi.is_compatible(other_abi);
```

### Calling Conventions

```cpp
enum class CallingConvention : u8 {
    GreyDefault = 0,  // Grey++ calling convention
    CDecl       = 1,  // C calling convention (for FFI)
    FastCall    = 2,  // Register-based
};
```

### Type Descriptors

```cpp
enum class TypeTag : u8 {
    Nil, Bool, Int, Float64, String,
    Array, Map, Set, Function, Class,
    Instance, Module, Buffer, Any, Void,
    Union, Optional, Generic, Enum, Tuple
};

struct TypeDescriptor {
    TypeTag tag;
    u16     name_index;
    u16     field_count;
    u16     method_count;
    u16     parent_type;
    // ...
};
```

---

## Embedding Guide

### Minimal Example

```cpp
#include "grey/vm.h"
#include "grey/bytecode.h"
#include "grey/objects.h"

using namespace grey;

int main() {
    VM vm;

    // Build a program: return 2 + 3
    Chunk chunk;
    chunk.emit_constant(Value::integer(2), 1);
    chunk.emit_constant(Value::integer(3), 1);
    chunk.emit(OpCode::ADD, 1);
    chunk.emit(OpCode::RETURN, 2);

    auto* fn = new ObjFunction();
    fn->chunk = chunk;
    vm.gc().track(fn);

    Value result = vm.execute(fn);
    // result.as_int() == 5
}
```

### Adding Native Functions

```cpp
vm.define_native("len", [](int argc, Value* args) -> Value {
    if (argc < 1 || !args[0].is_object()) return Value::integer(0);
    auto* hdr = args[0].as_object<ObjHeader>();
    if (hdr->type == ObjType::String)
        return Value::integer(static_cast<ObjString*>(hdr)->value.size());
    if (hdr->type == ObjType::Array)
        return Value::integer(static_cast<ObjArray*>(hdr)->elements.size());
    return Value::integer(0);
}, 1);
```

### Loading Modules

```cpp
#include "grey/abi.h"
#include "grey/module.h"

ModuleLoader loader;
loader.add_search_path("./modules");

ObjModule* math = loader.load("math", vm);
Value pi = math->exports.get(make_string(vm.gc(), "PI")).value_or(Value::nil());
```

### Error Handling

```cpp
try {
    vm.execute(fn);
} catch (const RuntimeError& e) {
    std::cerr << "Error [" << (int)e.code << "]: " << e.what() << std::endl;
}
```

Error codes: `TypeError`, `DivisionByZero`, `StackOverflow`, `StackUnderflow`, `InvalidOpcode`, `RuntimeError`, `IOError`, `ImportError`, `SecurityViolation`.

---

## API Reference

### Value

```cpp
struct Value {
    // Creation
    static Value nil();
    static Value boolean(bool b);
    static Value integer(i64 i);
    static Value number(f64 d);
    static Value object(void* p);

    // Type checking
    bool is_nil() const;
    bool is_bool() const;
    bool is_int() const;
    bool is_number() const;
    bool is_object() const;
    bool is_truthy() const;

    // Extraction
    bool as_bool() const;
    i64  as_int() const;
    f64  as_number() const;
    template<typename T> T* as_object() const;

    // Comparison
    bool operator==(const Value& other) const;
    bool operator!=(const Value& other) const;
};
```

### VM

```cpp
class VM {
    explicit VM(const VMConfig& config = VMConfig{});

    Value execute(ObjFunction* fn);
    Value execute(ObjClosure* closure, int argc, Value* args);
    Value call_function(Value callee, int argc, Value* args);

    void define_global(const std::string& name, Value value);
    Value get_global(const std::string& name);
    void define_native(const std::string& name, NativeFn fn, u16 arity);

    GarbageCollector& gc();
    const VMConfig& config() const;
    bool is_running() const;
    size_t cycle_count() const;
    std::string format_stack_trace() const;
};
```

### Chunk

```cpp
struct Chunk {
    std::vector<u8> code;
    std::vector<Value> constants;
    std::vector<LineInfo> lines;

    u32 emit(u8 byte, u32 line);
    u32 emit(OpCode op, u32 line);
    u32 emit_u16(u16 val, u32 line);
    u32 emit_constant(Value val, u32 line);
    u16 add_constant(Value val);
    u32 get_line(u32 offset) const;
};
```

### GarbageCollector

```cpp
class GarbageCollector {
    void track(ObjHeader* obj);
    size_t collect();
    size_t object_count() const;
    size_t collection_count() const;
    bool should_collect() const;
};
```

---

## License

MIT License. Copyright (c) 2026 Robert-Marshall01. See [LICENSE](LICENSE).
