<p align="center">
  <strong>Grey++</strong><br>
  <em>The Universal Programming Language</em>
</p>

<p align="center">
  <img alt="Version" src="https://img.shields.io/badge/version-0.1.0-blue" />
  <img alt="License" src="https://img.shields.io/badge/license-MIT-green" />
  <img alt="Node" src="https://img.shields.io/badge/node-%E2%89%A5%2018-brightgreen" />
  <img alt="Zero Dependencies" src="https://img.shields.io/badge/dependencies-0-orange" />
</p>

---

A multi-paradigm language with a **universal AST** — imperative, functional, declarative, and query constructs all share a single tree representation.

Grey++ compiles to **standalone JavaScript** or runs via an interpreter, with zero external dependencies.

Warning: there may be instability, verify stability before deploying in a high-stakes environment.

## Highlights

- **Multi-paradigm** — functional, imperative, declarative, and query syntax in one language
- **Universal AST** — every construct compiles to the same tree, enabling cross-paradigm composition
- **120+ built-in functions** — collections, math, strings, types, functional utilities, assertions, and more
- **Built-in query system** — SQL-style `select / from / where` against in-memory tables
- **Compiles to JS** — emit standalone `.js` files with the full runtime embedded
- **Interactive REPL** — with multiline support, `.ast` dump, and auto-balanced braces
- **Zero dependencies** — pure Node.js (≥ 18), install nothing

## Quick Start

```bash
# Clone the repository
git clone https://github.com/Robert-Marshall01/GreyThink
cd grey-plus-plus

# Run a programme
node src/index.js run examples/hello.greypp

# Compile to standalone JS
node src/index.js compile examples/hello.greypp
node examples/hello.js

# Start the interactive REPL
node src/index.js
```

No `npm install` required — the project has zero runtime dependencies.

## Hello World

```
// hello.greypp
print("Hello from Grey++!")
print("The universal programming language.")
```

```bash
$ node src/index.js run examples/hello.greypp
Hello from Grey++!
The universal programming language.
```

## Language Overview

### Functions & Closures

```
fn factorial(n) {
    if_then(n <= 1, 1, fn() { n * factorial(n - 1) })
}
print(factorial(10))   // 3628800

fn make_adder(x) {
    fn(y) { x + y }
}
print(make_adder(10)(5))   // 15
```

### Collections & Functional Style

```
fn data() { [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] }

fn evens() { filter(data(), fn(x) { mod(x, 2) == 0 }) }
fn doubled() { map(evens(), fn(x) { x * 2 }) }
fn total() { reduce(doubled(), fn(a, b) { a + b }, 0) }

print(total())   // 60
```

### SQL-Style Queries

```
createTable("users", [
    { id: 1, name: "Alice", role: "admin" },
    { id: 2, name: "Bob",   role: "user"  },
    { id: 3, name: "Carol", role: "admin" }
])

query { select name from users where role == "admin" }
// → [{ name: "Alice" }, { name: "Carol" }]
```

### Pattern Matching & Control Flow

```
fn grade(score) {
    cond(
        score >= 90, "A",
        score >= 80, "B",
        score >= 70, "C",
        "F"
    )
}

fn http_status(code) {
    match(code,
        200, "OK",
        404, "Not Found",
        500, "Internal Server Error",
        "Unknown"
    )
}
```

### Object Operations

```
fn user() { { name: "Alice", age: 30, role: "admin" } }

fn updated() { set(user(), "email", "alice@example.com") }
fn subset() { pick(user(), "name", "role") }
fn public_info() { omit(user(), "age") }

print(keys(user()))   // ["name", "age", "role"]
```

### Compilation

```bash
# Compile .greypp to standalone .js
node src/index.js compile app.greypp

# Custom output path
node src/index.js compile app.greypp -o dist/app.js

# Run compiled file — no Grey++ runtime needed
node dist/app.js
```

Compiled output embeds the full standard library and runs on any machine with Node.js.

## CLI Reference

```
node src/index.js <command> [options]
```

| Command                             | Description                                |
|--------------------------------------|--------------------------------------------|
| *(none)*                             | Start the interactive REPL                 |
| `run <file>`                         | Execute a `.greypp` source file            |
| `run <file> --ast`                   | Run and dump the AST                       |
| `run <file> -v`                      | Verbose mode                               |
| `compile <file>`                     | Compile to standalone `.js`                |
| `compile <file> -o <out.js>`         | Compile to a custom output path            |
| `compile <file> -v`                  | Verbose compilation                        |
| `help`                               | Show help                                  |

**Supported file extensions:** `.greypp`, `.gpp`, `.grey`

## Standard Library (120+ functions)

| Category         | Functions                                                                                     |
|------------------|-----------------------------------------------------------------------------------------------|
| **I/O**          | `print`, `log`, `debug`, `warn`, `error`                                                      |
| **Control Flow** | `if_then`, `when`, `unless`, `cond`, `match`                                                  |
| **Logic**        | `not`, `and`, `or`, `all`, `any`                                                              |
| **Math**         | `abs`, `sqrt`, `pow`, `floor`, `ceil`, `round`, `min`, `max`, `mod`, `clamp`, `lerp`, `sum`, `avg`, `sin`, `cos`, `log_n`, `exp`, `random`, `random_int` |
| **Collections**  | `len`, `map`, `filter`, `reduce`, `forEach`, `find`, `some`, `every`, `sort`, `reverse`, `slice`, `push`, `concat`, `flat`, `flat_map`, `zip`, `uniq`, `group_by`, `count`, `range`, `repeat`, `take`, `drop`, `head`, `tail`, `last`, `index_of`, `includes`, `flatten` |
| **Objects**      | `get`, `set`, `keys`, `values`, `entries`, `merge`, `pick`, `omit`, `has_key`, `from_entries`, `deep_clone` |
| **Strings**      | `str`, `upper`, `lower`, `trim`, `split`, `join`, `replace`, `starts_with`, `ends_with`, `char_at`, `substr`, `pad_start`, `pad_end`, `repeat_str`, `template` |
| **Types**        | `type_of`, `is_array`, `is_number`, `is_string`, `is_bool`, `is_fn`, `is_nil`, `to_number`, `to_string`, `to_bool`, `parse_int`, `parse_float` |
| **JSON**         | `json_stringify`, `json_parse`                                                                |
| **Time**         | `timestamp`, `now`, `elapsed`                                                                 |
| **Crypto**       | `hash`, `uuid`                                                                                |
| **Functional**   | `compose`, `pipe`, `identity`, `constant`, `apply`, `partial`, `memoize`, `tap`, `chain`      |
| **Assertions**   | `assert`, `assert_eq`, `bench`                                                                |
| **Data Tables**  | `createTable`, `insertRow`                                                                    |
| **Formatting**   | `inspect`, `table`, `banner`, `section`, `divider`                                            |
| **Constants**    | `nil`, `PI`, `E`                                                                              |

See [DOCS.md](DOCS.md) for full function signatures, descriptions, and examples.

## Examples

The `examples/` directory contains 22 complete programmes:

| File                  | Domain                 |
|-----------------------|------------------------|
| `hello.greypp`        | Hello World            |
| `math.greypp`         | Arithmetic & recursion |
| `fibonacci.greypp`    | Recursive Fibonacci    |
| `query.greypp`        | Table queries          |
| `query_advanced.greypp` | Advanced queries     |
| `oop.greypp`          | Object-oriented design |
| `functional.greypp`   | Functional programming |
| `abstraction.greypp`  | Design patterns        |
| `prototyping.greypp`  | Rapid prototyping      |
| `frontend.greypp`     | Virtual DOM & Redux    |
| `backend.greypp`      | HTTP router & REST API |
| `fullstack.greypp`    | E-commerce app         |
| `data_science.greypp` | Statistics & DataFrames|
| `ai_ml.greypp`        | Perceptron & metrics   |
| `blockchain.greypp`   | Mining & smart contracts|
| `gamedev.greypp`      | ECS & physics          |
| `devops.greypp`       | CI/CD & monitoring     |
| `cloud.greypp`        | Serverless & containers|
| `cybersecurity.greypp`| Crypto & firewall      |
| `iac.greypp`          | Infrastructure as Code |
| `hardware.greypp`     | IoT & sensors          |
| `speed.greypp`        | Caching & benchmarks   |

```bash
node src/index.js run examples/frontend.greypp
```

## REPL

```bash
$ node src/index.js
grey++ > fn fib(n) {
  ... >     if_then(n <= 1, n, fn() { fib(n-1) + fib(n-2) })
  ... > }
grey++ > fib(10)
55
grey++ > .ast
grey++ > .exit
```

| Command | Description          |
|---------|----------------------|
| `.exit` | Quit the REPL        |
| `.ast`  | Dump the AST         |

## Architecture

```
src/
├── index.js                 # CLI entry point (run / compile / repl / help)
├── runner.js                # File runner + standard library
├── compiler.js              # AST → standalone JS transpiler
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
│       ├── fn.js            # Function / expression executor
│       └── query.js         # SQL-style query executor
└── repl/
    └── repl.js              # Interactive REPL
```

The **Kernel** is a thin, stateless dispatcher. Each AST node kind is handled by a registered executor plugin. Adding a new construct requires only defining the node kind, writing a plugin, and registering it — the core never changes.

## Documentation

Full language documentation lives in [DOCS.md](DOCS.md), including:

- Complete syntax reference
- All 120+ stdlib function signatures with examples
- Operator precedence rules
- Query system details
- Escape sequences
- Scope and closure semantics

## License

[MIT](LICENSE)
