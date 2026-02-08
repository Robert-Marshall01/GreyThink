# Grey++ Programming Language

Welcome to **Grey++**, a programming language I’ve invented to unify and extend modern programming paradigms.

Grey++ source files use the following extensions:
- `.grey` — core Grey language files
- `.greypp` — Grey++ extended language files

---

## ✨ What is Grey++?
Grey++ is a meta‑language designed to:
- Normalize constructs across multiple languages (Python, JavaScript, C++, Rust, etc.)
- Provide a unified grammar and runtime for cross‑language interoperability
- Enable deterministic execution and reproducible systems
- Serve as the foundation for the **Grey Standard Library (GreyStd)**

---

## 🚀 Key Features
- **Unified Syntax** — one set of keywords (`bind`, `fn`, `loop`, `cond`, `struct`, `module`) across paradigms
- **Cross‑Language Translation** — Grey++ code can be mapped back into Python, JS, C++, and Rust
- **Runtime Integration** — Grey Runtime executes normalized ASTs with persistent memory and error reporting
- **Self‑Defined Ecosystem** — GreyStd (the standard library) is written entirely in Grey++
- **Deterministic Mode** — optional execution paths for reproducibility and Solidity‑style guarantees

---

## 📂 Project Structure
- `grey_grammar.ts` — defines universal Grey++ keywords and grammar
- `grey_ast.ts` — abstract syntax tree and normalization logic
- `grey_runtime.ts` — runtime execution engine
- `grey_translate.ts` — translators for Python, JS, C++, Rust
- `grey_repl.ts` — interactive REPL for multi‑language input/output
- `grey_test.ts` — automated tests for normalization and translation
- `.github/workflows/ci.yml` — CI/CD pipeline with Jest tests

---

## 🛠 Example
```grey
fn hello() {
    print("hi")
}

