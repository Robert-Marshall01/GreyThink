# Grey++ Self-Hosting Documentation

> Comprehensive API and internals reference for the Grey++ self-hosting compiler front-end.

---

## Table of Contents

- [1. Overview](#1-overview)
- [2. Running the Compiler](#2-running-the-compiler)
- [3. Lexer API](#3-lexer-api)
- [4. Parser API](#4-parser-api)
- [5. AST Node Reference](#5-ast-node-reference)
- [6. Semantic Analyzer](#6-semantic-analyzer)
- [7. Bridge](#7-bridge)
- [8. Pipeline API](#8-pipeline-api)
- [9. Built-in Registry](#9-built-in-registry)
- [10. Demos](#10-demos)

---

## 1. Overview

The Grey++ Self-Hosting front-end is a complete compiler pipeline written in Grey++ itself.
It processes Grey++ source code through four stages: **lexing → parsing → semantic analysis → serialization**.

All code is purely functional — no mutation, no loops, no `let`/`var`/`const`. State is threaded
through function parameters. Iteration uses recursion. Intermediate values are bound using
`fn name() { expr }`.

**Source file:** [src/selfhost.greypp](src/selfhost.greypp) (~1,510 lines)

---

## 2. Running the Compiler

### Execute the Self-Hosting Demo

```bash
# From the Grey++ project directory
node src/index.js run "../Grey Self-Hosting/src/selfhost.greypp"
```

### Use the Bootstrap Bridge

```bash
# Parse a .greypp file and output AST
node bootstrap/bridge.js <file.greypp> --ast

# Output JSON (backend-compatible)
node bootstrap/bridge.js <file.greypp> --json

# Output token stream
node bootstrap/bridge.js <file.greypp> --tokens
```

### Supported File Extensions

| Extension | Description        |
|-----------|--------------------|
| `.greypp` | Primary extension  |
| `.gpp`    | Short alias        |
| `.grey`   | Compact alias      |

---

## 3. Lexer API

The lexer converts Grey++ source text into a flat token stream.

### `tokenize(source)`

Main entry point. Returns an array of token objects.

```grey
fn tokens() { tokenize("fn add(a, b) { a + b }") }
// Returns: [
//   { type: "Fn", value: "fn", line: 1, col: 1 },
//   { type: "Ident", value: "add", line: 1, col: 4 },
//   { type: "LParen", value: "(", line: 1, col: 7 },
//   ...
// ]
```

### Token Types

| Category    | Tokens                                                        |
|-------------|---------------------------------------------------------------|
| Keywords    | `Fn`, `Query`, `Select`, `From`, `Where`, `Return`, `True`, `False` |
| Literals    | `Number`, `String`, `Ident`                                   |
| Operators   | `Plus`, `Minus`, `Star`, `Slash`, `Eq`, `EqEq`, `BangEq`, `Lt`, `Gt`, `LtEq`, `GtEq` |
| Delimiters  | `LParen`, `RParen`, `LBrace`, `RBrace`, `LBracket`, `RBracket` |
| Punctuation | `Comma`, `Semi`, `Colon`, `Dot`                              |
| Meta        | `EOF`, `Error`                                                |

### Character Classification

```grey
fn digit() { is_digit("5") }            // true
fn alpha() { is_alpha("x") }            // true
fn alnum() { is_alphanumeric("a3") }    // true — checks first char
fn ws() { is_whitespace(" ") }          // true
```

### Escape Sequences

The lexer handles standard escape sequences inside strings:

| Escape | Character       |
|--------|-----------------|
| `\n`   | Newline         |
| `\t`   | Tab             |
| `\\`   | Backslash       |
| `\"`   | Double quote    |

---

## 4. Parser API

The parser transforms a token stream into an AST using recursive descent with precedence climbing.

### `parse_program(tokens)`

Parses a complete Grey++ program.

```grey
fn ast() { parse_program(tokenize("fn greet() { print(\"hello\") }")) }
// Returns: { kind: "Program", body: [ { kind: "FnDecl", ... } ] }
```

### Expression Precedence (low → high)

1. Comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`)
2. Additive (`+`, `-`)
3. Multiplicative (`*`, `/`)
4. Postfix (function calls: `expr(args)`)
5. Unary (`-`)
6. Primary (literals, identifiers, grouped expressions)

### Key Parse Functions

| Function             | Parses                                  |
|----------------------|-----------------------------------------|
| `parse_program`      | Top-level program (list of statements)  |
| `parse_fn_decl`      | `fn name(params) { body }`              |
| `parse_anon_fn`      | `fn(params) { body }`                   |
| `parse_query`        | `query { select ... from ... where }`   |
| `parse_expression`   | Any expression (entry point)            |
| `parse_comparison`   | Comparison-level binary expressions     |
| `parse_additive`     | Addition / subtraction                  |
| `parse_multiplicative` | Multiplication / division             |
| `parse_postfix`      | Function call chaining                  |
| `parse_unary`        | Unary minus                             |
| `parse_primary`      | Literals, identifiers, arrays, objects  |
| `parse_block`        | `{ statement* }`                        |
| `parse_return`       | `return expr`                           |

### Threading Pattern

Every parse function takes `(tokens, pos)` and returns `{ node, pos }`:

```grey
fn result() { parse_expression(tokens, 0) }
fn ast_node() { get(result(), "node") }
fn next_pos() { get(result(), "pos") }
```

---

## 5. AST Node Reference

All nodes are plain objects with a `kind` field.

### Program

```grey
{ kind: "Program", body: [node, node, ...] }
```

### FnDecl

```grey
{ kind: "FnDecl", name: "add", params: ["a", "b"], body: [node, ...] }
```

Anonymous functions have `name: nil`.

### CallExpr

```grey
{ kind: "CallExpr", callee: "print", args: [node, ...] }
```

Callee can be a string (identifier) or a nested node (for chained calls).

### BinaryExpr

```grey
{ kind: "BinaryExpr", op: "+", left: node, right: node }
```

### Identifier

```grey
{ kind: "Identifier", name: "x" }
```

### NumberLit

```grey
{ kind: "NumberLit", value: 42 }
```

Supports integers and floating-point numbers.

### StringLit

```grey
{ kind: "StringLit", value: "hello" }
```

### BoolLit

```grey
{ kind: "BoolLit", value: true }
```

### ReturnStmt

```grey
{ kind: "ReturnStmt", value: node }
```

### ArrayLit

```grey
{ kind: "ArrayLit", elements: [node, node, ...] }
```

### ObjectLit

```grey
{ kind: "ObjectLit", entries: [{ key: "name", value: node }, ...] }
```

### Query

```grey
{ kind: "Query", select: selectNode, from: fromNode, where: whereNode }
```

### SelectClause

```grey
{ kind: "SelectClause", columns: ["col1", "col2"] }
```

`columns` can contain `"*"` for select-all.

### FromClause

```grey
{ kind: "FromClause", table: "users" }
```

### WhereClause

```grey
{ kind: "WhereClause", condition: node }
```

---

## 6. Semantic Analyzer

### `analyze(ast)`

Performs semantic analysis on a parsed AST. Returns a report object.

```grey
fn report() { analyze(ast) }
// { valid: true, errors: [], warnings: [] }
```

### Checks Performed

| Check                    | Severity | Description                                    |
|--------------------------|----------|------------------------------------------------|
| Undefined identifier     | Error    | Name used but never declared or built-in       |
| Undefined function call  | Error    | Function called but not declared                |
| Redefinition             | Warning  | Name declared twice in the same scope           |
| Arity mismatch           | Warning  | Function called with wrong number of arguments  |

### Scope Rules

- Each function body creates a new scope
- Function parameters are defined in the inner scope
- Top-level declarations are collected in a pre-pass (hoisting)
- Parent scope chain enables lexical scoping

### Built-in Registry

120+ built-in functions are pre-registered and never flagged as undefined:

```grey
// I/O
print, log, debug, warn, error

// Control flow
if_then, when, unless, cond, match

// Math
abs, sqrt, pow, floor, ceil, round, min, max, sin, cos, PI, E

// Collections
len, map, filter, reduce, forEach, find, sort, reverse, push, concat
range, repeat, take, drop, head, tail, last, zip, uniq, flat

// Objects
get, set, keys, values, entries, merge, has_key, pick, omit

// Strings
str, upper, lower, trim, split, join, replace, starts_with, ends_with

// Types
type_of, is_array, is_number, is_string, is_bool, is_fn, is_nil
to_number, to_string, to_bool

// Functional
compose, pipe, identity, constant, apply, partial, memoize, tap, chain

// And more...
```

---

## 7. Bridge

### `json_stringify(ast)`

Serializes the AST to JSON. The output format is identical to the existing Grey++ compiler's
internal AST representation, enabling zero-translation handoff to the backend.

```grey
fn json_output() { json_stringify(ast) }
// Produces formatted JSON consumable by the existing backend
```

### Bridge Architecture

```
Self-hosted front-end (Grey++)
        │
        │  json_stringify(ast)
        ▼
    JSON string
        │
        │  (bootstrap/bridge.js reads JSON)
        ▼
Existing backend (Node.js / TypeScript)
        │
        ▼
    Execution / Compilation
```

---

## 8. Pipeline API

### `compile_pipeline(source)`

Runs the full front-end pipeline: tokenize → parse → analyze → serialize.

```grey
fn result() { compile_pipeline("fn add(a, b) { a + b }") }
// Returns: {
//   tokens: [...],
//   ast: { kind: "Program", body: [...] },
//   analysis: { valid: true, errors: [], warnings: [] },
//   json: "{ ... }"
// }
```

### `pretty_print_ast(node, indent)`

Recursively formats an AST node as indented text for debugging.

```grey
fn formatted() { pretty_print_ast(ast, 0) }
print(formatted())
// Program
//   FnDecl: add(a, b)
//     BinaryExpr: +
//       Identifier: a
//       Identifier: b
```

---

## 9. Built-in Registry

The self-hosting compiler maintains a registry of all 120+ built-in functions.
This is used by the semantic analyzer to avoid false "undefined identifier" errors
for functions provided by the Grey++ runtime.

### Registry Categories

| Category        | Count | Examples                                           |
|-----------------|-------|----------------------------------------------------|
| I/O             | 5     | `print`, `log`, `debug`, `warn`, `error`           |
| Control flow    | 5     | `if_then`, `when`, `unless`, `cond`, `match`       |
| Logic           | 5     | `not`, `and`, `or`, `all`, `any`                   |
| Math            | 20    | `abs`, `sqrt`, `pow`, `floor`, `ceil`, `round`     |
| Collections     | 25+   | `len`, `map`, `filter`, `reduce`, `sort`, `range`  |
| Objects         | 11    | `get`, `set`, `keys`, `values`, `merge`, `has_key` |
| Strings         | 15    | `str`, `upper`, `lower`, `trim`, `split`, `join`   |
| Types           | 12    | `type_of`, `is_array`, `is_number`, `to_string`    |
| JSON            | 2     | `json_stringify`, `json_parse`                     |
| Time            | 3     | `timestamp`, `now`, `elapsed`                      |
| Crypto          | 2     | `hash`, `uuid`                                     |
| Functional      | 9     | `compose`, `pipe`, `identity`, `partial`, `memoize`|
| Testing         | 3     | `assert`, `assert_eq`, `bench`                     |
| Pretty print    | 5     | `inspect`, `table`, `banner`, `section`, `divider` |

---

## 10. Demos

The self-hosting source includes 7 built-in demonstrations that run automatically:

| Demo | Name                    | Tests                                            |
|------|-------------------------|--------------------------------------------------|
| 1    | Simple function         | `fn add(a, b) { a + b }` — declaration and call  |
| 2    | Higher-order functions  | Functions as arguments, closures                  |
| 3    | Operator precedence     | `2 + 3 * 4` → 14 (not 20)                        |
| 4    | Data structures         | Object and array literals                         |
| 5    | Query expressions       | SQL-style `query { select * from users }`         |
| 6    | Self-parse              | Grey++ parses its own source code                 |
| 7    | Semantic errors         | Detects undefined identifiers                     |

### Running Demos

```bash
node src/index.js run "../Grey Self-Hosting/src/selfhost.greypp"
```

Each demo prints its name, the input source, the lexed tokens, the parsed AST,
and the analysis result — proving the self-hosting pipeline works end-to-end.

---

## Conventions

All code in the self-hosting front-end follows these Grey++ conventions:

1. **`fn` only** — Every binding is a `fn` declaration. No `let`, `var`, `const`.
2. **Objects for data** — `{ key: value }` objects represent tokens, AST nodes, and results.
3. **Recursion for iteration** — `scan_loop`, `parse_body_loop`, etc. are recursive.
4. **`{ node, pos }` threading** — Parser state is passed and returned explicitly.
5. **`with_x(value)` pattern** — Computes a value once and binds it as a parameter.
6. **No mutation** — All functions are pure and return new values.
7. **Built-in functions** — Uses the 120+ Grey++ runtime builtins for logic, math, strings, etc.

---

*Grey++ Self-Hosting v1.0.0 — MIT License — Copyright (c) 2026 Robert-Marshall01*
