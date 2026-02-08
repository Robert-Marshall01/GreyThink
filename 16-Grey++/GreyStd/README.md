# GreyStd — The Grey++ Standard Library

> **Core types, memory utilities, concurrency, IO, networking, serialization, time, crypto, system interfaces, modules, diagnostics, testing, reflection, and deterministic execution tools.**

---

## Overview

GreyStd is the comprehensive standard library for the Grey++ programming language. It provides everything a modern systems language needs — from foundational types like `String`, `Option`, and `Result` to advanced subsystems for async concurrency, cryptography, and deterministic replay execution.

Warning: contains instability, always verify stability before deploying.

## Module Map

| Module | Path | Description |
|---|---|---|
| **core** | `src/core/` | String, Bytes, Array, Map, Set, Tuple, Option, Result, Iterators, Math |
| **mem** | `src/mem/` | Smart pointers, arenas, cloning, borrowing utilities |
| **concurrent** | `src/concurrent/` | Tasks, futures, channels, mutexes, atomics, thread pools, event loop |
| **io** | `src/io/` | File I/O, directories, streams, buffered I/O, pipes, memory-mapped files |
| **net** | `src/net/` | TCP, UDP, HTTP client, WebSockets, DNS, TLS |
| **serial** | `src/serial/` | JSON, YAML, TOML, binary serialization, MessagePack |
| **time** | `src/time/` | Instant, Duration, clocks, timezones, scheduling |
| **crypto** | `src/crypto/` | Hashing, HMAC, RNG, symmetric/asymmetric encryption, KDF |
| **sys** | `src/sys/` | Environment variables, process spawning, signals, system info |
| **module** | `src/module/` | Module metadata, dynamic loading, versioning |
| **error** | `src/error/` | Error types, stack traces, logging, assertions, debug utilities |
| **test** | `src/test/` | Unit testing, assertions, test runner, benchmarks |
| **reflect** | `src/reflect/` | Type metadata, runtime inspection, dynamic invocation, attributes |
| **det** | `src/deterministic/` | Deterministic collections, scheduling, allocation, replay-safe IO |

## Quick Start

```grey
use greystd::core::{ String, Vec, HashMap, Option, Result };
use greystd::io::File;
use greystd::concurrent::{ spawn, channel };

fn main() -> Result<(), Error> {
    // Read a file
    let content = File::read_to_string("config.toml")?;

    // Parse into a map
    let config = greystd::serial::toml::parse(&content)?;

    // Spawn concurrent tasks with channels
    let (tx, rx) = channel::<String>();

    spawn(async {
        tx.send("Hello from Grey++!").await;
    });

    let msg = rx.recv().await?;
    println("{}", msg);

    Ok(())
}
```

## Design Principles

1. **Safety by default** — `Option` and `Result` for error-safe programming; no null pointers.
2. **Zero-cost abstractions** — Iterators, generics, and smart pointers compile down to optimal code.
3. **Async-first concurrency** — Built-in event loop, tasks, and channels with structured concurrency.
4. **Deterministic execution** — Replay-safe collections, scheduling, and IO for verifiable computation.
5. **Batteries included** — Crypto, networking, serialization, and testing out of the box.

## Architecture

```
greystd
├── core/         ← Foundation: types every program uses
├── mem/          ← Memory semantics made ergonomic
├── concurrent/   ← The engine room of modern software
├── io/           ← Real-world interaction
├── net/          ← Distributed systems support
├── serial/       ← Structured data interchange
├── time/         ← Temporal reasoning and scheduling
├── crypto/       ← Security primitives
├── sys/          ← OS interface layer
├── module/       ← Ecosystem glue
├── error/        ← Developer experience
├── test/         ← Quality assurance
├── reflect/      ← Runtime introspection
└── deterministic/ ← Grey++'s unique advantage
```

## License

licensed under MIT.
