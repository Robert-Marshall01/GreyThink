# Grey++ Self-Hosting

> **Grey++ is now partially self-hosting:** its compiler front-end is written in Grey++ itself, proving the language is expressive and robust enough to define and process its own programs.

---

## Overview

Grey Self-Hosting is a compiler front-end for the Grey++ programming language, written entirely in Grey++. It implements a complete pipeline: **lexer → parser → AST → semantic analysis → bridge to backend** — all in pure, functional Grey++ with no mutation and no loops.

This achieves the first self-hosting threshold:

> *"Grey++ can parse and understand itself using code written in Grey++."*

Warning: may be unstable, always verify stability before deploying.

## Architecture

```
Source Code (.greypp)
        │
        ▼
┌──────────────┐
│    Lexer     │  Grey++ source → Token stream
└──────┬───────┘  8 token categories, escape sequences, line/col tracking
       ▼
┌──────────────┐
│   Parser     │  Recursive descent with precedence climbing
└──────┬───────┘  Full Grey++ grammar: fn, query, expressions, data structures
       ▼
┌──────────────┐
│  Semantic    │  Scope tracking, name resolution, 120+ builtin registry
│  Analyzer    │  Undefined identifier detection, arity checking
└──────┬───────┘
       ▼
┌──────────────┐
│   Bridge     │  AST → JSON (zero-translation, same format as existing compiler)
└──────┬───────┘
       ▼
┌──────────────┐
│  Existing    │  Code generation, runtime, standard library
│  Backend     │  (Node.js / TypeScript — unchanged)
└──────────────┘
```

## Quick Start

### Run the Self-Hosting Demo

```bash
# From the Grey++ project directory
node src/index.js run "../Grey Self-Hosting/src/selfhost.greypp"
```

This runs the complete self-hosting pipeline with 7 demonstrations:
1. Simple function declaration & call
2. Higher-order functions & closures
3. Operator precedence
4. Data structures (objects & arrays)
5. Query expressions (SQL-style)
6. **Self-parse** — Grey++ parsing its own source code
7. Semantic error detection

### Run the Bootstrap Bridge

```bash
# Parse any .greypp file through the self-hosted front-end
node bootstrap/bridge.js examples/demo.greypp --ast
node bootstrap/bridge.js examples/demo.greypp --json
node bootstrap/bridge.js examples/demo.greypp --tokens
```

## Project Structure

```
Grey Self-Hosting/
├── README.md                      # This file
├── DESIGN.md                      # Architecture & design document
├── src/
│   └── selfhost.greypp            # Complete self-hosting front-end (all-in-one)
│                                  #   Section 1: Character utilities
│                                  #   Section 2: Token system
│                                  #   Section 3: Lexer (tokenizer)
│                                  #   Section 4: AST node constructors
│                                  #   Section 5: Recursive descent parser
│                                  #   Section 6: Semantic analyzer
│                                  #   Section 7: Bridge to backend
│                                  #   Section 8: Compiler pipeline
│                                  #   Section 9: AST pretty printer
│                                  #   Section 10: Self-hosting demos
├── examples/
│   ├── demo.greypp                # Sample Grey++ program for testing
│   └── self_parse_test.greypp     # Self-parse unit tests
└── bootstrap/
    └── bridge.js                  # Node.js bridge to existing backend
```

## Components

### Lexer (Tokenizer)
- Tokenizes Grey++ source into 30+ token types
- Handles: keywords, identifiers, numbers (int/float), strings (with escape sequences)
- Handles: operators (arithmetic, comparison, two-char), punctuation, comments
- Tracks line and column for error reporting
- Emits error tokens for unrecognized characters (no crash)

### Parser
- Recursive descent with explicit precedence climbing
- Supports full Grey++ grammar:
  - Named and anonymous function declarations
  - Function calls with chaining (`f(x)(y)`)
  - Binary expressions with correct precedence
  - Unary minus
  - Array and object literals
  - Query expressions (SQL-style: `select`, `from`, `where`)
  - Return statements
- Purely functional: position threaded via `{ node, pos }` result objects

### AST Model
14 node types matching the existing Grey++ compiler:

| Node          | Description                          |
|---------------|--------------------------------------|
| `Program`     | Root node with body array            |
| `FnDecl`      | Function declaration (named or anon) |
| `CallExpr`    | Function call with arguments         |
| `BinaryExpr`  | Binary operation (arithmetic, cmp)   |
| `Identifier`  | Variable/function reference          |
| `NumberLit`   | Numeric literal                      |
| `StringLit`   | String literal                       |
| `BoolLit`     | Boolean literal                      |
| `ReturnStmt`  | Return statement                     |
| `ArrayLit`    | Array literal                        |
| `ObjectLit`   | Object literal                       |
| `Query`       | Query expression                     |
| `SelectClause`| Query select clause                  |
| `FromClause`  | Query from clause                    |
| `WhereClause` | Query where clause                   |

### Semantic Analyzer
- Lexical scope tracking with parent chain
- Pre-pass declaration collection (functions visible before definition)
- 120+ built-in function registry
- Detects: undefined functions, undefined identifiers, redefinitions
- Reports errors and warnings without halting

### Bridge
- Zero-translation: AST objects use the exact same structure as the existing compiler
- `json_stringify(ast)` produces output the existing backend can consume directly

## Design Philosophy

### Purely Functional
The entire front-end is written without any mutation, variables, or loops. All state is threaded through function parameters. Iteration uses recursion. Intermediate values are bound using the `fn name() { expr }` and `with_x(value)` patterns.

### Self-Describing
The compiler can parse and output its own AST, enabling introspection and meta-programming. Demo 6 proves this by feeding a fragment of the self-hosting code to its own lexer and parser.

### Zero-Translation Bridge
The AST format is identical between self-hosted and existing compilers. No translation layer needed — just JSON serialization.

## Self-Hosting Checklist

- [x] Design Grey++ AST in Grey++
- [x] Implement lexer in Grey++
- [x] Implement parser in Grey++
- [x] Implement minimal semantic analyzer in Grey++
- [x] Implement AST/IR bridge to existing backend
- [x] Compile Grey++ front-end with existing toolchain
- [x] Run: Grey++ front-end → AST/IR → existing backend
- [x] Document: "Grey++ is now self-hosting at the front-end level"

## Resume Bullet

> **Invented and self-hosted a new programming language (Grey++):**
> Designed and implemented a Grey++ compiler front-end (lexer, parser, AST, and semantic analysis) written entirely in Grey++, enabling the language to parse and analyze its own source code and feed a shared backend. This self-hosting architecture validated Grey++ as a mature, self-defining compute stack rather than a toy language.

---

*Grey++ Self-Hosting v1.0.0 — MIT License — Copyright (c) 2026 Robert-Marshall01*
