# Grey++ Foundation

## Core Philosophy
- Universal AST representing imperative, functional, declarative, and query paradigms.
- Minimal primitives: fn, query, infer, sys, module.
- Plugin runtime kernel: modular execution engine (SQL, system calls, AI inference, framework adapters).
- Staged expansion: start lean (SQL + Python), then add JavaScript, Java, C#, Rust, Go.

## Stage 1 Goals
- Build REPL that runs SQL queries and Python-style functions together.
- Prove interoperability with universal AST.
- Keep runtime modular to avoid bottlenecks.
