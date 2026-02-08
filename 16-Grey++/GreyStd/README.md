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
// ── Read a config file and parse it ──
fn content() { file_read("config.toml") }
fn config() { toml_parse(content()) }

// ── Use Option and Result ──
fn port() { option_unwrap_or(map_get(config(), "port"), 3000) }
fn name() { result_unwrap(Ok("Grey++ App")) }

// ── Spawn concurrent tasks with channels ──
fn ch() { Channel_new(10) }
fn sender() { get(ch(), "sender") }
fn receiver() { get(ch(), "receiver") }

fn send_msg() { channel_send(sender(), "Hello from Grey++!") }
fn msg() { channel_recv(receiver()) }

// ── Print the result ──
fn main() {
  send_msg()
  print(msg())
}
```

> **File extension:** GreyStd source files use `.grey`, which is a supported Grey++ extension
> alongside `.greypp` and `.gpp`. All `.grey` files in this library are valid Grey++ code.

## Design Principles

1. **Safety by default** — `Option` and `Result` for error-safe programming; `nil`-aware helpers throughout.
2. **Pure functions everywhere** — All operations return new values; no mutation, no side effects in core logic.
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
