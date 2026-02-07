# Grey++ Language Documentation

> **Grey++ v0.1.0** — The Universal Programming Language
>
> A multi-paradigm language with a universal AST — imperative, functional,
> declarative, and query constructs all share a single tree representation.

---

## Table of Contents

1.  [Getting Started](#1-getting-started)
2.  [CLI Reference](#2-cli-reference)
3.  [Language Syntax](#3-language-syntax)
    - [Comments](#comments)
    - [Literals](#literals)
    - [Variables & Scope](#variables--scope)
    - [Operators](#operators)
    - [Functions](#functions)
    - [Anonymous Functions (Lambdas)](#anonymous-functions-lambdas)
    - [Chained Calls](#chained-calls)
    - [Control Flow](#control-flow)
    - [Query Expressions](#query-expressions)
    - [Keywords as Identifiers](#keywords-as-identifiers)
4.  [Standard Library Reference](#4-standard-library-reference)
    - [Constants](#constants)
    - [I/O](#io)
    - [Control Flow Functions](#control-flow-functions)
    - [Logic](#logic)
    - [Math](#math)
    - [Collections](#collections)
    - [Object Operations](#object-operations)
    - [String Operations](#string-operations)
    - [Type Operations](#type-operations)
    - [JSON](#json)
    - [Time](#time)
    - [Hashing / Crypto](#hashing--crypto)
    - [Functional Utilities](#functional-utilities)
    - [Assertions / Testing](#assertions--testing)
    - [Data Tables](#data-tables)
    - [Pretty Print / Formatting](#pretty-print--formatting)
    - [Script Metadata](#script-metadata)
5.  [Query System](#5-query-system)
6.  [Examples](#6-examples)
7.  [Architecture](#7-architecture)
8.  [REPL Commands](#8-repl-commands)

---

## 1. Getting Started

### Requirements

- **Node.js** ≥ 18 (no external dependencies)

### Running a File

```bash
node src/index.js run examples/hello.greypp
```

### Compiling a File

Compile a `.greypp` source file to a standalone JavaScript file that runs
with Node.js — no Grey++ runtime dependency required.

```bash
node src/index.js compile examples/hello.greypp
# → Compiled → examples/hello.js

node examples/hello.js   # runs standalone
```

### Starting the REPL

```bash
node src/index.js
```

### Supported File Extensions

| Extension | Description            |
|-----------|------------------------|
| `.greypp` | Primary extension      |
| `.gpp`    | Short alias            |
| `.grey`   | Compact alias          |

---

## 2. CLI Reference

```
node src/index.js <command> [options]
```

| Command / Flag                           | Description                                    |
|------------------------------------------|------------------------------------------------|
| *(no command)*                           | Start the interactive REPL                     |
| `run <file>`                             | Execute a `.greypp` source file                |
| `run <file> --ast`                       | Execute and dump the AST tree                  |
| `run <file> --verbose` or `-v`           | Verbose output (file path, size, etc.)         |
| `compile <file>`                         | Compile to standalone `.js` (same directory)   |
| `compile <file> -o <out.js>`             | Compile to a specific output path              |
| `compile <file> --verbose` or `-v`       | Verbose compilation output                     |
| `compile <file> --ast`                   | Compile and dump the AST tree                  |
| `help`, `--help`, `-h`                   | Show help message                              |

### Compilation

The `compile` command transpiles Grey++ source into a **self-contained JavaScript
file** that embeds the entire standard library. The output has zero dependencies
on the Grey++ runtime — it can be distributed and run anywhere Node.js is
available.

```bash
# Default output: replaces the extension with .js
node src/index.js compile app.greypp
# → Compiled → app.js

# Custom output path
node src/index.js compile app.greypp -o dist/app.js
# → Compiled → dist/app.js

# Run the compiled file
node app.js
```

**What the compiler does:**

- Parses the `.greypp` source into a universal AST
- Transpiles each AST node to equivalent JavaScript
- Embeds the full Grey++ standard library (120+ functions) in the output
- Uses strict mode and strict equality (`===`/`!==`)
- Sanitises reserved-word identifiers (e.g. `public` → `_gpp_public`)
- Inlines the query table system for standalone execution
- Applies implicit-return semantics matching the interpreter

---

## 3. Language Syntax

### Comments

Single-line comments start with `//`. There are no block comments.

```
// This is a comment
print("hello")  // inline comment
```

### Literals

#### Numbers

Integer or floating-point. Supports dot-prefixed decimals.

```
42
3.14
.5          // → 0.5
-7          // unary minus
```

#### Strings

Double-quoted or single-quoted. Supports escape sequences.

```
"hello world"
'single quotes too'
```

**Escape sequences:**

| Sequence | Character       |
|----------|-----------------|
| `\n`     | Newline         |
| `\t`     | Tab             |
| `\r`     | Carriage return |
| `\\`     | Backslash       |
| `\"`     | Double quote    |
| `\'`     | Single quote    |
| `\0`     | Null character  |

#### Booleans

```
true
false
```

#### Nil

The built-in constant representing the absence of a value.

```
nil         // equivalent to null
```

#### Arrays

Ordered, zero-indexed collections. Trailing commas are allowed.

```
[1, 2, 3]
["a", "b", "c",]       // trailing comma OK
[]                      // empty array
[[1, 2], [3, 4]]       // nested
[1, "mixed", true, nil] // mixed types
```

#### Objects

Key-value maps. Keys can be identifiers, keywords, or strings. Trailing commas allowed.

```
{ name: "Alice", age: 30 }
{ "string-key": "value" }
{ from: "origin", select: "chosen" }    // keywords as keys
{}                                       // empty object
```

### Variables & Scope

Grey++ uses a functional scoping model. Variables are defined by **function declarations** — there is no `let`, `var`, or `const`. Each function body creates a new scope. Inner scopes shadow outer ones.

```
fn x() { 10 }
print(x())          // 10

fn outer() {
    fn x() { 20 }   // shadows the outer x
    x()
}
print(outer())      // 20
print(x())          // 10 — outer x unchanged
```

Parameters are scoped to their function:

```
fn greet(name) {
    str("Hello, ", name)
}
print(greet("Alice"))   // Hello, Alice
```

### Operators

#### Arithmetic

| Operator | Description    | Example       | Result |
|----------|----------------|---------------|--------|
| `+`      | Addition       | `2 + 3`       | `5`    |
| `-`      | Subtraction    | `10 - 4`      | `6`    |
| `*`      | Multiplication | `3 * 7`       | `21`   |
| `/`      | Division       | `15 / 4`      | `3.75` |
| `-`      | Unary minus    | `-5`          | `-5`   |

**Operator precedence** (highest to lowest):

1. Unary minus (`-x`)
2. Multiplication, Division (`*`, `/`)
3. Addition, Subtraction (`+`, `-`)
4. Comparisons (`==`, `!=`, `<`, `>`, `<=`, `>=`)

Parentheses override precedence:

```
2 + 3 * 4       // 14
(2 + 3) * 4     // 20
```

**Division by zero** throws a runtime error:

```
10 / 0           // Error: Division by zero
```

**String concatenation** uses `+`:

```
"hello" + " " + "world"    // "hello world"
"count: " + 42              // "count: 42"
```

#### Comparison

All comparisons use **strict equality** (no type coercion).

| Operator | Description      | Example     | Result  |
|----------|------------------|-------------|---------|
| `==`     | Equal (strict)   | `1 == 1`    | `true`  |
| `!=`     | Not equal        | `1 != 2`    | `true`  |
| `<`      | Less than        | `3 < 5`     | `true`  |
| `>`      | Greater than     | `5 > 3`     | `true`  |
| `<=`     | Less or equal    | `3 <= 3`    | `true`  |
| `>=`     | Greater or equal | `5 >= 6`    | `false` |

**Strict equality examples:**

```
0 == false      // false (different types)
"" == false     // false
nil == nil      // true
nil == false    // false
nil == 0        // false
1 == 1          // true
"a" == "a"      // true
```

### Functions

Named function declarations use `fn`:

```
fn name(param1, param2) {
    // body
    // last expression is the return value
}
```

**Implicit return** — the value of the last expression in the body is returned:

```
fn add(a, b) { a + b }
print(add(3, 4))        // 7
```

**Explicit return** — use `return` to exit early:

```
fn abs_val(x) {
    if_then(x < 0, fn() { return -x }, fn() { return x })
}
```

**No-parameter functions:**

```
fn greet() { "hello" }
print(greet())           // hello
```

**Closures** — functions capture their defining scope:

```
fn make_adder(x) {
    fn(y) { x + y }     // captures x
}
print(make_adder(10)(5))  // 15
```

**Recursive functions:**

```
fn factorial(n) {
    if_then(n <= 1, 1, fn() { n * factorial(n - 1) })
}
print(factorial(5))       // 120
```

**Function redefinition** — defining a function with the same name replaces it:

```
fn greet() { "hello" }
fn greet() { "hi" }      // replaces previous
print(greet())            // hi
```

### Anonymous Functions (Lambdas)

Anonymous functions use `fn` without a name:

```
fn(x) { x * 2 }
fn(a, b) { a + b }
fn() { 42 }
```

Commonly used as callbacks:

```
map([1, 2, 3], fn(x) { x * 2 })          // [2, 4, 6]
filter([1, 2, 3, 4], fn(x) { x > 2 })    // [3, 4]
sort([3, 1, 2], fn(a, b) { a - b })       // [1, 2, 3]
```

### Chained Calls

Expressions that return functions can be called immediately:

```
fn make_adder(x) {
    fn(y) { x + y }
}
make_adder(10)(5)                 // 15

// Triple chain
fn make_multi(x) {
    fn(y) {
        fn(z) { x + y + z }
    }
}
make_multi(1)(2)(3)               // 6

// Object method + chained call
fn obj() {
    { run: fn(x) { x * 2 } }
}
get(obj(), "run")(21)             // 42
```

### Control Flow

Grey++ uses **functions** for control flow rather than keyword-based `if/else` or `while` statements. This enables a highly functional style.

```
// Conditional
if_then(x > 0, "positive", "non-positive")

// Lazy evaluation with function wrappers
if_then(x > 10,
    fn() { expensive_computation() },
    fn() { fallback() }
)

// Multi-branch
cond(
    score >= 90, "A",
    score >= 80, "B",
    score >= 70, "C",
    "F"                 // default
)

// Value matching
match(color,
    "red",   "#FF0000",
    "green", "#00FF00",
    "blue",  "#0000FF",
    "#000000"           // default
)

// Boolean guards
when(is_admin, fn() { grant_access() })
unless(is_locked, fn() { open_door() })

// Recursion for iteration
fn count_down(n) {
    when(n > 0, fn() {
        print(n)
        count_down(n - 1)
    })
}
```

> **Lazy evaluation:** `if_then`, `cond`, and `match` all support lazy branches.
> If a branch value is a function with zero parameters, it will be called
> automatically. This prevents expensive or side-effecting code from running
> when the branch is not taken.

### Query Expressions

Built-in SQL-style queries against in-memory tables:

```
// Create a table
createTable("users", [
    { id: 1, name: "Alice", role: "admin" },
    { id: 2, name: "Bob",   role: "user" }
])

// Query it
query { select name, role from users where role == "admin" }
// → [{ name: "Alice", role: "admin" }]

// Select all columns
query { select * from users }

// Add rows dynamically
insertRow("users", { id: 3, name: "Carol", role: "user" })
```

See [Section 5: Query System](#5-query-system) for full details.

### Keywords as Identifiers

All Grey++ keywords can be used as identifiers in parameter names, object keys,
and general expressions. This prevents naming conflicts:

```
fn process(from, select, where) {
    str(from, " → ", select, " → ", where)
}
process("input", "transform", "output")

// As object keys
{ from: "origin", return: "result", fn: "function" }
```

**Reserved keywords that double as identifiers:**
`fn`, `query`, `select`, `from`, `where`, `return`, `true`, `false`

---

## 4. Standard Library Reference

Grey++ ships with **120+ built-in functions** available in every program.

### Constants

| Name  | Type   | Value  | Description                      |
|-------|--------|--------|----------------------------------|
| `nil` | null   | `null` | Represents absence of a value    |
| `PI`  | number | 3.14…  | Mathematical constant π          |
| `E`   | number | 2.71…  | Euler's number e                 |

---

### I/O

#### `print(...args)` → *void*

Print values to stdout.

```
print("Hello, world!")
print("Name:", name, "Age:", age)
print([1, 2, 3])
```

#### `log(...args)` → *void*

Print with `[LOG]` prefix.

```
log("Server started on port", 3000)
// [LOG] Server started on port 3000
```

#### `debug(...args)` → *void*

Print with `[DEBUG]` prefix.

```
debug("Current state:", state)
// [DEBUG] Current state: { ... }
```

#### `warn(...args)` → *void*

Print with `[WARN]` prefix.

```
warn("Deprecated function called")
// [WARN] Deprecated function called
```

#### `error(...args)` → *void*

Print to stderr with `[ERROR]` prefix.

```
error("Connection failed:", reason)
// [ERROR] Connection failed: timeout
```

---

### Control Flow Functions

#### `if_then(condition, then_value, else_value)` → *any*

Conditional evaluation. If `then_value` or `else_value` is a zero-arg function, it is called lazily.

```
if_then(age >= 18, "adult", "minor")

// Lazy branches (recommended for side effects / recursion)
if_then(n == 0,
    1,
    fn() { n * factorial(n - 1) }
)
```

#### `when(condition, fn)` → *any | undefined*

Execute `fn()` only if condition is truthy.

```
when(is_admin, fn() { show_admin_panel() })
```

#### `unless(condition, fn)` → *any | undefined*

Execute `fn()` only if condition is falsy.

```
unless(is_cached, fn() { fetch_data() })
```

#### `cond(...pairs)` → *any*

Multi-branch conditional. Takes alternating condition/value pairs with an optional default.

```
cond(
    score >= 90, "A",
    score >= 80, "B",
    score >= 70, "C",
    score >= 60, "D",
    "F"                 // default (odd last argument)
)
```

#### `match(value, ...pairs)` → *any*

Value-based dispatch (like `switch`). Tests strict equality.

```
match(command,
    "start",  fn() { start_server() },
    "stop",   fn() { stop_server() },
    "status", fn() { show_status() },
    fn() { print("Unknown command") }   // default
)
```

---

### Logic

#### `not(value)` → *boolean*

Logical negation.

```
not(true)         // false
not(false)        // true
not(0)            // true  (falsy)
not("")           // true  (falsy)
```

#### `and(a, b)` → *any*

Logical AND (short-circuit). Returns `b` if `a` is truthy, otherwise `a`.

```
and(true, true)     // true
and(true, false)    // false
and(false, true)    // false
```

#### `or(a, b)` → *any*

Logical OR (short-circuit). Returns `a` if truthy, otherwise `b`.

```
or(false, true)     // true
or(false, false)    // false
or("default", "")   // "default"
```

#### `all(...values)` → *boolean*

Returns `true` if **all** arguments are truthy.

```
all(true, 1, "yes")     // true
all(true, 0, "yes")     // false
```

#### `any(...values)` → *boolean*

Returns `true` if **any** argument is truthy.

```
any(false, 0, "yes")    // true
any(false, 0, "")       // false
```

---

### Math

#### Basic Operations

| Function              | Description                  | Example                  | Result     |
|-----------------------|------------------------------|--------------------------|------------|
| `abs(x)`              | Absolute value               | `abs(-7)`                | `7`        |
| `sqrt(x)`             | Square root                  | `sqrt(16)`               | `4`        |
| `pow(base, exp)`      | Exponentiation               | `pow(2, 10)`             | `1024`     |
| `floor(x)`            | Round down                   | `floor(3.7)`             | `3`        |
| `ceil(x)`             | Round up                     | `ceil(3.2)`              | `4`        |
| `round(x)`            | Round to nearest integer     | `round(3.5)`             | `4`        |
| `min(a, b, ...)`      | Minimum value                | `min(5, 3, 8)`           | `3`        |
| `max(a, b, ...)`      | Maximum value                | `max(5, 3, 8)`           | `8`        |
| `mod(a, b)`           | Modulo (remainder)           | `mod(10, 3)`             | `1`        |
| `clamp(val, lo, hi)`  | Clamp to range               | `clamp(15, 0, 10)`       | `10`       |
| `lerp(a, b, t)`       | Linear interpolation         | `lerp(0, 100, 0.5)`     | `50`       |

#### Trigonometry & Logarithms

| Function    | Description           | Example           | Result   |
|-------------|-----------------------|-------------------|----------|
| `sin(x)`    | Sine (radians)        | `sin(PI / 2)`     | `1`      |
| `cos(x)`    | Cosine (radians)      | `cos(0)`          | `1`      |
| `log_n(x)`  | Natural logarithm     | `log_n(E)`        | `1`      |
| `exp(x)`    | e^x                   | `exp(1)`          | `2.718…` |

#### Random

| Function              | Description                      | Example              |
|-----------------------|----------------------------------|----------------------|
| `random()`            | Random float in [0, 1)           | `random()`           |
| `random_int(lo, hi)`  | Random integer in [lo, hi]       | `random_int(1, 100)` |

#### Aggregation

| Function          | Description                                    | Example                     | Result |
|-------------------|------------------------------------------------|-----------------------------|--------|
| `sum(arr)` / `sum(a, b, ...)` | Sum of values                    | `sum([1, 2, 3])`            | `6`    |
| `avg(arr)` / `avg(a, b, ...)` | Average of values                | `avg([10, 20, 30])`         | `20`   |

Both `sum` and `avg` accept either a single array argument or multiple number arguments:

```
sum(1, 2, 3)        // 6
sum([1, 2, 3])      // 6
avg(10, 20)         // 15
avg([10, 20])       // 15
```

---

### Collections

#### Querying

| Function                | Description                                      | Example                                          | Result            |
|-------------------------|--------------------------------------------------|--------------------------------------------------|-------------------|
| `len(v)`                | Length of array, string, or object (key count)   | `len([1, 2, 3])`                                | `3`               |
| `head(arr)`             | First element                                    | `head([10, 20, 30])`                            | `10`              |
| `tail(arr)`             | All except first                                 | `tail([10, 20, 30])`                            | `[20, 30]`        |
| `last(arr)`             | Last element                                     | `last([10, 20, 30])`                            | `30`              |
| `find(arr, fn)`         | First matching element                           | `find([1, 2, 3], fn(x) { x > 1 })`             | `2`               |
| `index_of(arr, val)`    | Index of value (-1 if absent)                    | `index_of(["a", "b"], "b")`                     | `1`               |
| `includes(arr, val)`    | Check containment                                | `includes([1, 2, 3], 2)`                        | `true`            |
| `some(arr, fn)`         | Any element matches                              | `some([1, 2, 3], fn(x) { x > 2 })`             | `true`            |
| `every(arr, fn)`        | All elements match                               | `every([2, 4, 6], fn(x) { mod(x, 2) == 0 })`   | `true`            |
| `count(arr, fn?)`       | Count (all, or matching fn)                      | `count([1, 2, 3], fn(x) { x > 1 })`            | `2`               |

#### Transforming

| Function                        | Description                        | Example                                        | Result              |
|---------------------------------|------------------------------------|-------------------------------------------------|---------------------|
| `map(arr, fn)`                  | Transform each element             | `map([1, 2, 3], fn(x) { x * 2 })`             | `[2, 4, 6]`         |
| `filter(arr, fn)`              | Keep matching elements             | `filter([1, 2, 3, 4], fn(x) { x > 2 })`       | `[3, 4]`            |
| `reduce(arr, fn, init)`        | Fold into accumulator              | `reduce([1, 2, 3], fn(a, x) { a + x }, 0)`    | `6`                 |
| `sort(arr, fn)`                | Non-mutating sort                  | `sort([3, 1, 2], fn(a, b) { a - b })`          | `[1, 2, 3]`         |
| `reverse(arr)`                 | Reverse (non-mutating)             | `reverse([1, 2, 3])`                           | `[3, 2, 1]`         |
| `flat(arr)`                    | Flatten one level                  | `flat([[1, 2], [3]])`                           | `[1, 2, 3]`         |
| `flatten(arr)`                 | Flatten all levels (recursive)     | `flatten([[1, [2]], [[3]]])`                    | `[1, 2, 3]`         |
| `flat_map(arr, fn)`            | Map then flatten                   | `flat_map([1, 2], fn(x) { [x, x * 10] })`     | `[1, 10, 2, 20]`    |
| `uniq(arr)`                    | Remove duplicates                  | `uniq([1, 2, 2, 3, 3])`                        | `[1, 2, 3]`         |
| `group_by(arr, fn)`            | Group into object by key fn        | `group_by([1, 2, 3], fn(x) { mod(x, 2) })`    | `{1: [1,3], 0: [2]}`|
| `zip(a, b)`                    | Pair elements by index             | `zip([1, 2], ["a", "b"])`                      | `[[1,"a"],[2,"b"]]` |

#### Slicing & Building

| Function                   | Description                           | Example                                        | Result            |
|----------------------------|---------------------------------------|-------------------------------------------------|-------------------|
| `slice(arr, start, end?)`  | Sub-array                             | `slice([10, 20, 30, 40], 1, 3)`               | `[20, 30]`        |
| `take(arr, n)`             | First n elements                      | `take([1, 2, 3, 4, 5], 3)`                    | `[1, 2, 3]`       |
| `drop(arr, n)`             | Skip first n elements                 | `drop([1, 2, 3, 4, 5], 2)`                    | `[3, 4, 5]`       |
| `push(arr, ...vals)`       | Append values (non-mutating)          | `push([1, 2], 3, 4)`                          | `[1, 2, 3, 4]`    |
| `concat(a, b)`             | Concatenate arrays or strings         | `concat([1, 2], [3, 4])`                      | `[1, 2, 3, 4]`    |
| `range(start, end, step?)` | Generate sequential numbers           | `range(0, 5)`                                 | `[0, 1, 2, 3, 4]` |
| `repeat(n, fn)`            | Build array from function             | `repeat(3, fn(i) { i * 10 })`                 | `[0, 10, 20]`     |

#### Iteration

| Function             | Description                           | Example                                               |
|----------------------|---------------------------------------|-------------------------------------------------------|
| `forEach(arr, fn)`   | Execute fn for each element           | `forEach([1, 2, 3], fn(x) { print(x) })`            |

`forEach` returns the original array (for chaining), not the mapped results.

---

### Object Operations

| Function                      | Description                             | Example                                          | Result                    |
|-------------------------------|-----------------------------------------|--------------------------------------------------|---------------------------|
| `get(obj, key)`               | Read property (safe on nil)             | `get({ a: 1 }, "a")`                            | `1`                       |
| `set(obj, key, val)`          | Set property (non-mutating)             | `set({ a: 1 }, "b", 2)`                         | `{ a: 1, b: 2 }`         |
| `keys(obj)`                   | Array of keys                           | `keys({ a: 1, b: 2 })`                          | `["a", "b"]`              |
| `values(obj)`                 | Array of values                         | `values({ a: 1, b: 2 })`                        | `[1, 2]`                  |
| `entries(obj)`                | Array of [key, value] pairs             | `entries({ a: 1 })`                             | `[["a", 1]]`              |
| `merge(...objs)`              | Shallow merge (later wins)              | `merge({ a: 1 }, { b: 2 })`                     | `{ a: 1, b: 2 }`         |
| `pick(obj, ...keys)`         | Select specific keys                    | `pick({ a: 1, b: 2, c: 3 }, "a", "c")`         | `{ a: 1, c: 3 }`         |
| `omit(obj, ...keys)`         | Exclude specific keys                   | `omit({ a: 1, b: 2, c: 3 }, "b")`              | `{ a: 1, c: 3 }`         |
| `has_key(obj, key)`           | Check if key exists                     | `has_key({ a: 1 }, "a")`                        | `true`                    |
| `from_entries(arr)`           | Build object from [key, value] pairs    | `from_entries([["a", 1], ["b", 2]])`            | `{ a: 1, b: 2 }`         |
| `deep_clone(val)`             | Deep copy via JSON round-trip           | `deep_clone({ a: { b: 1 } })`                   | `{ a: { b: 1 } }` (copy) |

**Immutable objects:** `set`, `merge`, `pick`, and `omit` all return **new objects** — the original is never mutated.

```
fn original() { { x: 1, y: 2 } }
fn updated() { set(original(), "z", 3) }
// original() still has only x and y
// updated() has x, y, and z
```

---

### String Operations

| Function                         | Description                          | Example                                   | Result              |
|----------------------------------|--------------------------------------|--------------------------------------------|---------------------|
| `str(...args)`                   | Concatenate as string                | `str("Age: ", 25)`                        | `"Age: 25"`         |
| `upper(s)`                      | Uppercase                            | `upper("hello")`                          | `"HELLO"`           |
| `lower(s)`                      | Lowercase                            | `lower("HELLO")`                          | `"hello"`           |
| `trim(s)`                       | Strip whitespace                     | `trim("  hi  ")`                          | `"hi"`              |
| `split(s, sep)`                 | Split into array                     | `split("a,b,c", ",")`                    | `["a", "b", "c"]`   |
| `join(arr, sep?)`               | Join array into string               | `join(["a", "b"], "-")`                  | `"a-b"`             |
| `replace(s, from, to)`          | Replace first occurrence             | `replace("hello", "l", "r")`             | `"herlo"`           |
| `starts_with(s, prefix)`        | Check prefix                         | `starts_with("hello", "he")`             | `true`              |
| `ends_with(s, suffix)`          | Check suffix                         | `ends_with("hello", "lo")`               | `true`              |
| `char_at(s, i)`                 | Character at index                   | `char_at("hello", 1)`                    | `"e"`               |
| `substr(s, start, len?)`        | Substring                            | `substr("hello", 1, 3)`                  | `"ell"`             |
| `pad_start(s, n, ch?)`          | Pad from left                        | `pad_start("42", 5, "0")`               | `"00042"`           |
| `pad_end(s, n, ch?)`            | Pad from right                       | `pad_end("hi", 5, ".")`                 | `"hi..."`           |
| `repeat_str(s, n)`              | Repeat string n times                | `repeat_str("ab", 3)`                   | `"ababab"`          |
| `includes(s, sub)`              | Check substring (also works on arrays) | `includes("hello", "ell")`            | `true`              |
| `template(tmpl, ...vals)`       | Positional template replacement      | `template("Hello {0}, you are {1}", "Alice", 30)` | `"Hello Alice, you are 30"` |

---

### Type Operations

#### Type Checking

| Function         | Description                     | Example                | Result     |
|------------------|---------------------------------|------------------------|------------|
| `type_of(v)`     | Type as string                  | `type_of(42)`          | `"number"` |
| `is_array(v)`    | Is array?                       | `is_array([1, 2])`     | `true`     |
| `is_number(v)`   | Is number (not NaN)?            | `is_number(42)`        | `true`     |
| `is_string(v)`   | Is string?                      | `is_string("hi")`      | `true`     |
| `is_bool(v)`     | Is boolean?                     | `is_bool(true)`        | `true`     |
| `is_fn(v)`       | Is function?                    | `is_fn(fn(x) { x })`  | `true`     |
| `is_nil(v)`      | Is null/undefined?              | `is_nil(nil)`          | `true`     |

`type_of` returns one of: `"number"`, `"string"`, `"boolean"`, `"function"`, `"array"`, `"object"`, `"null"`, `"undefined"`.

#### Type Conversion

| Function          | Description            | Example                | Result     |
|-------------------|------------------------|------------------------|------------|
| `to_number(v)`    | Convert to number      | `to_number("42")`      | `42`       |
| `to_string(v)`    | Convert to string      | `to_string(42)`        | `"42"`     |
| `to_bool(v)`      | Convert to boolean     | `to_bool(0)`           | `false`    |
| `parse_int(s)`    | Parse integer (base 10)| `parse_int("42px")`    | `42`       |
| `parse_float(s)`  | Parse float            | `parse_float("3.14")`  | `3.14`     |

---

### JSON

| Function              | Description                                | Example                       |
|-----------------------|--------------------------------------------|-------------------------------|
| `json_stringify(v)`   | Convert to pretty-printed JSON string      | `json_stringify({ a: 1 })`   |
| `json_parse(s)`       | Parse JSON string to value                 | `json_parse("{\"a\":1}")`    |

```
fn data() { { name: "Alice", scores: [90, 85, 92] } }
print(json_stringify(data()))
// {
//   "name": "Alice",
//   "scores": [90, 85, 92]
// }
```

---

### Time

| Function           | Description                                  | Example                  |
|--------------------|----------------------------------------------|--------------------------|
| `timestamp()`      | Current time in milliseconds (epoch)         | `timestamp()`            |
| `now()`            | Current time as ISO 8601 string              | `now()`                  |
| `elapsed(start)`   | Milliseconds since `start`                   | `elapsed(start_time)`    |

```
fn start() { timestamp() }
// ... do work ...
print(str("Took ", elapsed(start()), "ms"))
```

---

### Hashing / Crypto

| Function     | Description                                      | Example               |
|--------------|--------------------------------------------------|-----------------------|
| `hash(s)`    | 32-bit hash of string (hex, 8 chars)             | `hash("hello")`       |
| `uuid()`     | Generate a random UUID v4                        | `uuid()`              |

```
print(hash("hello"))        // "4f9f2cab"
print(uuid())               // "3b241101-e2bb-4d37-97ba-a5e47820..."
```

`hash` is deterministic — the same input always produces the same output. `uuid` is random.

---

### Functional Utilities

#### `compose(f, g)` → *function*

Right-to-left function composition. Returns `fn(x) { f(g(x)) }`.

```
fn double(x) { x * 2 }
fn inc(x) { x + 1 }
fn double_after_inc() { compose(double, inc) }
print(double_after_inc()(4))    // 10 → inc(4)=5, double(5)=10
```

#### `pipe(...fns)` → *function*

Left-to-right function composition. Returns a function that passes its argument through each function in order.

```
fn pipeline() { pipe(fn(x) { x + 1 }, fn(x) { x * 2 }, fn(x) { x - 3 }) }
print(pipeline()(4))    // 7 → 4+1=5, 5*2=10, 10-3=7
```

#### `chain(init, ...fns)` → *any*

Immediately apply a pipeline of functions to a value.

```
chain(10,
    fn(x) { x * 2 },       // 20
    fn(x) { x + 5 },       // 25
    fn(x) { x * 3 }        // 75
)
```

#### `identity(x)` → *any*

Returns its argument unchanged. Useful as a default transform.

```
identity(42)    // 42
```

#### `constant(x)` → *function*

Returns a zero-argument function that always returns `x`.

```
fn always_42() { constant(42) }
print(always_42()())    // 42
```

#### `apply(fn, args)` → *any*

Call a function with an array of arguments.

```
fn add(a, b) { a + b }
apply(add, [3, 4])      // 7
```

#### `partial(fn, ...bound)` → *function*

Partially apply arguments to a function.

```
fn add(a, b) { a + b }
fn add5() { partial(add, 5) }
print(add5()(10))       // 15
print(add5()(20))       // 25
```

#### `memoize(fn)` → *function*

Cache function results by argument values (JSON-serialized key).

```
fn expensive(n) { n * n }
fn memo() { memoize(expensive) }
print(memo()(10))       // 100 (computed)
print(memo()(10))       // 100 (cached)
```

#### `tap(value, fn)` → *value*

Execute a side-effect function, then return the original value.

```
fn result() {
    tap(42, fn(v) { log(str("Processing: ", v)) })
}
// Logs: [LOG] Processing: 42
// Returns: 42
```

---

### Assertions / Testing

#### `assert(condition, message?)` → *true*

Throws if condition is falsy.

```
assert(1 + 1 == 2, "basic math")           // passes
assert(len([1, 2]) == 3, "length check")   // throws: Assertion failed: length check
```

#### `assert_eq(actual, expected, message?)` → *true*

Deep equality check via JSON comparison. Throws with a detailed diff on failure.

```
assert_eq([1, 2, 3], [1, 2, 3], "arrays match")          // passes
assert_eq({ a: 1 }, { a: 2 }, "objects match")            // throws with diff
```

#### `bench(label, fn)` → *result*

Time a function's execution and print the duration. Returns the function's result.

```
fn result() {
    bench("sort 10000 items", fn() {
        sort(range(0, 10000), fn(a, b) { b - a })
    })
}
// [BENCH] sort 10000 items: 12ms
```

---

### Data Tables

Tables are per-program in-memory stores used by the `query` system.

#### `createTable(name, rows?)` → *void*

Create (or replace) a named table with optional initial rows.

```
createTable("users", [
    { id: 1, name: "Alice" },
    { id: 2, name: "Bob" }
])
createTable("logs")    // empty table
```

#### `insertRow(tableName, row)` → *void*

Append a row to an existing table (auto-creates the table if needed).

```
insertRow("users", { id: 3, name: "Carol" })
```

Tables are isolated per program run — data does not leak between separate executions.

---

### Pretty Print / Formatting

#### `inspect(value)` → *value*

Pretty-print a value with full depth and colors. Returns the value for chaining.

```
inspect({ deeply: { nested: { data: [1, 2, 3] } } })
```

#### `table(arr)` → *arr*

Print array of objects as a formatted table. Returns the array.

```
table([
    { name: "Alice", score: 95 },
    { name: "Bob", score: 87 }
])
```

#### `banner(title)` → *void*

Print a boxed banner.

```
banner("Grey++ v0.1.0")
// ╔══════════════════╗
// ║  Grey++ v0.1.0  ║
// ╚══════════════════╝
```

#### `section(title)` → *void*

Print a section divider with title.

```
section("Setup")
// ── Setup ──────────────────────────────────────────────────────────
```

#### `divider()` → *void*

Print a horizontal line.

```
divider()
// ──────────────────────────────────────────────────────────────────
```

---

### Script Metadata

| Name       | Type   | Description                                 |
|------------|--------|---------------------------------------------|
| `__file__` | string | Absolute path of the currently running file |
| `__dir__`  | string | Directory of the currently running file     |

```
print(__file__)     // D:\projects\app\main.greypp
print(__dir__)      // D:\projects\app
```

These are only available in file execution mode, not in the REPL.

---

## 5. Query System

Grey++ includes a built-in SQL-inspired query system that operates on in-memory tables.

### Syntax

```
query {
    select <columns>
    from <table_name>
    where <condition>
}
```

- **`select`** — Required. Column names separated by commas, or `*` for all columns.
- **`from`** — Table name (must exist).
- **`where`** — Optional filter condition (any comparison expression).

### Creating and Populating Tables

```
// Create with initial data
createTable("employees", [
    { id: 1, name: "Alice", dept: "eng",   salary: 120000 },
    { id: 2, name: "Bob",   dept: "sales", salary: 85000 },
    { id: 3, name: "Carol", dept: "eng",   salary: 110000 }
])

// Add rows incrementally
insertRow("employees", { id: 4, name: "Dave", dept: "hr", salary: 90000 })
```

### Query Examples

```
// Select all columns
query { select * from employees }

// Select specific columns
query { select name, salary from employees }

// Filter with WHERE
query { select name, dept from employees where dept == "eng" }

// Numeric comparison
query { select name, salary from employees where salary > 100000 }

// Combine in functions
fn high_earners() {
    query { select name, salary from employees where salary >= 110000 }
}
forEach(high_earners(), fn(row) {
    print(str(get(row, "name"), ": $", get(row, "salary")))
})
```

### Supported WHERE Operators

`==`, `!=`, `<`, `>`, `<=`, `>=`

### Limitations

- Single-table queries only (no native JOINs — simulate with functions)
- No `ORDER BY`, `GROUP BY`, `LIMIT` — use `sort()`, `group_by()`, `take()` on results
- No aggregate functions in queries — use `reduce()`, `sum()`, `avg()` on results

---

## 6. Examples

The `examples/` directory contains 22 complete programs demonstrating Grey++ across many domains:

| File                    | Domain                   | Highlights                                                   |
|-------------------------|--------------------------|--------------------------------------------------------------|
| `hello.greypp`          | Basics                   | Hello World output                                           |
| `math.greypp`           | Basics                   | Arithmetic functions                                         |
| `fibonacci.greypp`      | Basics                   | Recursive Fibonacci                                          |
| `query.greypp`          | Data                     | Basic table queries                                          |
| `query_advanced.greypp` | Data                     | JOINs, GROUP BY simulation, live inserts                     |
| `oop.greypp`            | OOP                      | Encapsulation, inheritance, observer pattern                 |
| `functional.greypp`     | Functional               | Monads, currying, pattern matching, immutable lists          |
| `abstraction.greypp`    | Design Patterns          | Strategy, Adapter, Middleware, Repository                    |
| `prototyping.greypp`    | Rapid Prototyping        | Data models, validation, mock API, ETL pipeline              |
| `frontend.greypp`       | Frontend                 | Virtual DOM, Redux store, CSS-in-JS, components              |
| `backend.greypp`        | Backend                  | HTTP router, middleware, REST API, auth                      |
| `fullstack.greypp`      | Fullstack                | E-commerce: auth, products, cart, checkout, tests            |
| `data_science.greypp`   | Data Science             | Statistics, DataFrames, regression, charts                   |
| `ai_ml.greypp`          | AI / ML                  | Linear algebra, perceptron training, evaluation metrics      |
| `blockchain.greypp`     | Blockchain               | Transactions, mining, smart contracts, Merkle trees          |
| `gamedev.greypp`        | Game Dev                 | Vector math, ECS, physics, AI state machines, inventory      |
| `devops.greypp`         | DevOps                   | CI/CD pipelines, monitoring, canary deploys, log aggregation |
| `cloud.greypp`          | Cloud                    | Serverless, containers, auto-scaling, message queues         |
| `cybersecurity.greypp`  | Cybersecurity            | Crypto, firewall rules, IDS, JWT tokens                      |
| `iac.greypp`            | Infrastructure as Code   | VPCs, subnets, security groups, deployment plans             |
| `hardware.greypp`       | Hardware / IoT           | GPIO, sensors, actuators, communication protocols            |
| `speed.greypp`          | Performance              | Memoization, batching, LRU cache, object pools, benchmarks   |

Run any example:

```bash
node src/index.js run examples/frontend.greypp
```

---

## 7. Architecture

```
src/
├── index.js                 # CLI entry point (run / repl / help)
├── runner.js                # File loader, stdlib seeding, CLI
├── ast/
│   └── types.js             # Universal AST node kinds + factories
├── parser/
│   ├── lexer.js             # Tokenizer
│   └── parser.js            # Recursive-descent parser
├── runtime/
│   ├── kernel.js            # Plugin-based execution kernel
│   ├── scope.js             # Lexical scope chain
│   ├── registry.js          # Plugin loader
│   └── plugins/
│       ├── fn.js            # Function, expression, identifier executor
│       └── query.js         # SQL-style query executor
└── repl/
    └── repl.js              # Interactive REPL
```

### Kernel & Plugins

The **Kernel** is a thin, stateless dispatcher. Each AST node kind is handled by a registered executor plugin. Adding new language features (e.g., pattern matching, async, modules) requires only:

1. Define the AST node kind in `types.js`
2. Write a plugin in `runtime/plugins/`
3. Register it in `registry.js`

### Scope Model

Grey++ uses **lexical scoping** with a parent chain. Each function invocation creates an isolated child scope — parameters and locals cannot leak across calls.

```
RootScope (globals: print, map, filter, ...)
  └─ FunctionScope (params: x, y)
       └─ InnerFunctionScope (params: z)
```

---

## 8. REPL Commands

Start the REPL with `node src/index.js` (no arguments).

| Command | Description                           |
|---------|---------------------------------------|
| `.exit` | Quit the REPL                         |
| `.ast`  | Parse the current buffer and dump AST |

The REPL supports multiline input — braces are automatically balanced across lines:

```
grey++ > fn fib(n) {
  ... >     if_then(n <= 1, n, fn() { fib(n-1) + fib(n-2) })
  ... > }
grey++ > fib(10)
55
```

---

*Grey++ v0.1.0 — MIT License*
