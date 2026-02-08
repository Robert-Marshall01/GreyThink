# Grey++ Self-Hosting: Architecture & Design

> **Version:** 1.0.0  
> **Status:** Minimal Viable Self-Hosting (MVS)  
> **Language:** Grey++ (implemented in Grey++)

---

## 1. Overview

Grey Self-Hosting is a compiler front-end for Grey++ that is written entirely in Grey++.
It can lex, parse, semantically analyze, and serialize Grey++ source code — proving
that Grey++ is expressive and robust enough to define and process its own programs.

Warning: may be unstable, always verify before deploying in a high stakes environment.

### Self-Hosting Threshold

> "Grey++ can parse and understand itself using code written in Grey++."

This is achieved by implementing:
- A **lexer** (tokenizer) in Grey++
- A **recursive-descent parser** in Grey++
- An **AST model** matching the existing compiler's structure
- A **semantic analyzer** for basic checks
- A **bridge** to the existing backend (JSON serialization)

---

## 2. Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Grey++ Source (.greypp)                       │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│  LEXER (tokenize)                                               │
│  • Character classification (is_digit, is_alpha, is_whitespace) │
│  • Token emission (keywords, identifiers, literals, operators)  │
│  • Escape sequence handling in strings                          │
│  • Line/column tracking for error reporting                     │
│  • Comment skipping (single-line //)                            │
│  Output: Token stream [{ type, value, line, col }, ...]         │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│  PARSER (parse_program)                                         │
│  • Recursive descent with precedence climbing                   │
│  • Handles: fn decl, anon fn, query, expressions, statements   │
│  • Operator precedence: unary > mul/div > add/sub > comparison  │
│  • Postfix call chaining: expr(args)(args)                      │
│  • Literals: numbers, strings, booleans, arrays, objects        │
│  Output: AST { kind: "Program", body: [...] }                   │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│  SEMANTIC ANALYZER (analyze)                                    │
│  • Scope tracking (lexical, nested)                             │
│  • Function declaration collection (pre-pass)                   │
│  • Undefined identifier detection                               │
│  • Arity checking for known functions                           │
│  • Built-in function registry (120+ builtins)                   │
│  Output: { valid: bool, errors: [...], warnings: [...] }        │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│  BRIDGE (json_stringify)                                        │
│  • AST → JSON serialization                                     │
│  • Format matches existing JS backend's AST structure           │
│  • Zero-translation bridge (same node shapes)                   │
│  Output: JSON string consumable by existing backend             │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│  EXISTING BACKEND (Node.js / TypeScript)                        │
│  • Code generation (JS transpilation)                           │
│  • Runtime execution                                            │
│  • Standard library embedding                                   │
└─────────────────────────────────────────────────────────────────┘
```

---

## 3. Design Decisions

### 3.1 Purely Functional Implementation

Grey++ has no `let`/`var`/`const` and no `while`/`for` loops.
All state is threaded through function parameters:

- **Lexer**: Recursive `scan(source, pos, line, col, tokens)` — each step
  returns the next state.
- **Parser**: Each parse function takes `(tokens, pos)` and returns
  `{ node, pos }` — the parsed AST node and the new position.
- **Accumulation**: Uses the `with_result(computed_value)` pattern to
  compute a value once and bind it as a parameter, avoiding recomputation.

### 3.2 AST Compatibility

The self-hosted AST uses the **exact same structure** as the existing
JavaScript compiler:

| Node Kind     | Shape                                             |
|---------------|---------------------------------------------------|
| `Program`     | `{ kind, body: [nodes] }`                         |
| `FnDecl`      | `{ kind, name, params: [strings], body: [nodes] }`|
| `CallExpr`    | `{ kind, callee: string|node, args: [nodes] }`   |
| `BinaryExpr`  | `{ kind, op, left: node, right: node }`           |
| `NumberLit`   | `{ kind, value: number }`                         |
| `StringLit`   | `{ kind, value: string }`                         |
| `BoolLit`     | `{ kind, value: boolean }`                        |
| `Identifier`  | `{ kind, name: string }`                          |
| `ReturnStmt`  | `{ kind, value: node }`                           |
| `ArrayLit`    | `{ kind, elements: [nodes] }`                     |
| `ObjectLit`   | `{ kind, entries: [{key, value}] }`               |
| `Query`       | `{ kind, select, from, where }`                   |

This means the bridge to the existing backend is a **no-op** — just
`json_stringify(ast)`. The existing backend can consume the output directly.

### 3.3 Error Handling Strategy

- **Lexer**: Emits `Error` tokens for unrecognized characters and continues.
- **Parser**: Returns `Error` AST nodes for unexpected tokens.
- **Semantics**: Collects errors/warnings without halting.
- **MVP scope**: No error recovery (resync) in the parser.

### 3.4 Recursion Budget

Since Grey++ uses recursion for all looping, there's a practical limit on
input size (~10,000 characters due to Node.js stack depth). This is
acceptable for the MVP; future versions can use iterative backends.

---

## 4. Token Types

| Category   | Types                                                    |
|------------|----------------------------------------------------------|
| Keywords   | `Fn`, `Query`, `Select`, `From`, `Where`, `Return`      |
|            | `True`, `False`                                          |
| Literals   | `Number`, `String`, `Ident`                              |
| Operators  | `Plus`, `Minus`, `Star`, `Slash`                         |
|            | `Eq`, `EqEq`, `BangEq`, `Lt`, `Gt`, `LtEq`, `GtEq`     |
| Delimiters | `LParen`, `RParen`, `LBrace`, `RBrace`                   |
|            | `LBracket`, `RBracket`                                   |
| Punctuation| `Comma`, `Semi`, `Colon`, `Dot`                          |
| Meta       | `EOF`, `Error`                                           |

---

## 5. Grammar (Simplified EBNF)

```
program        = top_level* EOF
top_level      = fn_decl | query | expr_stmt
fn_decl        = "fn" IDENT "(" param_list ")" block
anon_fn        = "fn" "(" param_list ")" block
query          = "query" "{" clause* "}"
clause         = select_clause | from_clause | where_clause
select_clause  = "select" (column ("," column)* | "*")
from_clause    = "from" IDENT
where_clause   = "where" expression
block          = "{" statement* "}"
statement      = return_stmt | fn_decl | query | expr_stmt
return_stmt    = "return" expression ";"?
expr_stmt      = expression ";"?
expression     = comparison
comparison     = additive (("==" | "!=" | "<" | ">" | "<=" | ">=") additive)*
additive       = multiplicative (("+" | "-") multiplicative)*
multiplicative = postfix (("*" | "/") postfix)*
postfix        = unary ("(" arg_list ")")*
unary          = "-" unary | primary
primary        = NUMBER | STRING | "true" | "false" | IDENT
               | anon_fn | "(" expression ")" | array_lit | object_lit
array_lit      = "[" (expression ("," expression)*)? "]"
object_lit     = "{" (prop ("," prop)*)? "}"
prop           = (IDENT | STRING) ":" expression
param_list     = (IDENT ("," IDENT)*)?
arg_list       = (expression ("," expression)*)?
```

---

## 6. Boundaries

### In Scope (v1.0)
- Lexer with full Grey++ token set
- Parser for all Grey++ constructs
- AST model matching existing compiler
- Basic semantic analysis (scope, names)
- JSON bridge to existing backend
- Self-parse demo

### Out of Scope (v2+)
- Code generation in Grey++
- Optimization passes
- Full type system
- Error recovery in parser
- Module/import system
- Runtime reimplementation

---

## 7. Innovation Angles

1. **Purely Functional Compiler**: The entire front-end is written without
   mutation, loops, or mutable variables — purely recursive and functional.

2. **Self-Describing**: The compiler can parse and output its own AST,
   enabling introspection and meta-programming.

3. **Zero-Translation Bridge**: The AST format is identical between the
   self-hosted and existing compilers, making integration seamless.

4. **Language-Agnostic AST**: The AST structure can represent constructs
   from multiple paradigms (imperative, functional, declarative, query).

---

*Grey++ Self-Hosting v1.0.0 — MIT License*
