# GreyStd Documentation

> Comprehensive API reference and usage guide for the Grey++ Standard Library.

---

## Table of Contents

- [1. Core Module](#1-core-module)
  - [1.1 String](#11-string)
  - [1.2 Vec\<T\>](#12-vect)
  - [1.3 HashMap\<K, V\>](#13-hashmapk-v)
  - [1.4 HashSet\<T\>](#14-hashsett)
  - [1.5 Option\<T\>](#15-optiont)
  - [1.6 Result\<T, E\>](#16-resultt-e)
  - [1.7 Iterator](#17-iterator)
  - [1.8 Math](#18-math)
  - [1.9 Bytes & ByteBuffer](#19-bytes--bytebuffer)
  - [1.10 Array\<T, N\>](#110-arrayt-n)
  - [1.11 Tuple](#111-tuple)
- [2. Memory Module](#2-memory-module)
- [3. Concurrency Module](#3-concurrency-module)
- [4. IO Module](#4-io-module)
- [5. Networking Module](#5-networking-module)
- [6. Serialization Module](#6-serialization-module)
- [7. Time Module](#7-time-module)
- [8. Crypto Module](#8-crypto-module)
- [9. System Module](#9-system-module)
- [10. Module System](#10-module-system)
- [11. Error Handling](#11-error-handling)
- [12. Testing Framework](#12-testing-framework)
- [13. Reflection](#13-reflection)
- [14. Deterministic Execution](#14-deterministic-execution)
- [Prelude](#prelude)

---

## 1. Core Module

**Path:** `src/core/`

The core module provides the foundational types that every Grey++ program depends on.

### 1.1 String

**File:** `src/core/string.grey`

A heap-allocated, UTF-8 encoded, growable string type.

```grey
use greystd::core::string::{ String, StringSlice, StringBuilder };

let s = String::from("Hello, Grey++!");
let len = s.len();               // 14
let upper = s.to_uppercase();    // "HELLO, GREY++!"

// StringSlice — borrowed, zero-copy view
let slice: &str = s.as_str();
let sub = s.substring(0, 5);     // "Hello"

// StringBuilder — efficient concatenation
let mut sb = StringBuilder::new();
sb.push_str("Hello");
sb.push_str(", World!");
let result = sb.build();         // "Hello, World!"
```

**Key methods:**

| Method | Signature | Description |
|--------|-----------|-------------|
| `new` | `fn new() -> String` | Creates an empty string |
| `from` | `fn from(s: &str) -> String` | Creates from a string literal |
| `len` | `fn len(&self) -> usize` | Returns byte length |
| `is_empty` | `fn is_empty(&self) -> bool` | True if length is zero |
| `push_str` | `fn push_str(&mut self, s: &str)` | Appends a string slice |
| `push_char` | `fn push_char(&mut self, c: char)` | Appends a character |
| `contains` | `fn contains(&self, pat: &str) -> bool` | Substring test |
| `starts_with` | `fn starts_with(&self, pat: &str) -> bool` | Prefix test |
| `ends_with` | `fn ends_with(&self, pat: &str) -> bool` | Suffix test |
| `split` | `fn split(&self, sep: &str) -> Vec<String>` | Split by delimiter |
| `trim` | `fn trim(&self) -> &str` | Strip leading/trailing whitespace |
| `to_uppercase` | `fn to_uppercase(&self) -> String` | Uppercase conversion |
| `to_lowercase` | `fn to_lowercase(&self) -> String` | Lowercase conversion |
| `replace` | `fn replace(&self, from: &str, to: &str) -> String` | Replace occurrences |
| `chars` | `fn chars(&self) -> CharIterator` | Character iterator |
| `substring` | `fn substring(&self, start: usize, end: usize) -> &str` | Slice extraction |

**Traits implemented:** `Display`, `Debug`, `Clone`, `Eq`, `Hash`, `Add<StringSlice>`, `IntoIterator`

### 1.2 Vec\<T\>

**File:** `src/core/vec.grey`

A heap-allocated, contiguous, growable array. The workhorse collection of Grey++.

```grey
let mut v = Vec::new();
v.push(1);
v.push(2);
v.push(3);

let first = v[0];            // 1
let popped = v.pop();        // Some(3)
let doubled: Vec<i32> = v.iter().map(|x| x * 2).collect();
```

**Key methods:**

| Method | Signature | Description |
|--------|-----------|-------------|
| `new` | `fn new() -> Vec<T>` | Empty vector |
| `with_capacity` | `fn with_capacity(cap: usize) -> Vec<T>` | Pre-allocate |
| `from_slice` | `fn from_slice(s: &[T]) -> Vec<T>` | Copy from slice |
| `push` | `fn push(&mut self, val: T)` | Append element |
| `pop` | `fn pop(&mut self) -> Option<T>` | Remove last |
| `insert` | `fn insert(&mut self, idx: usize, val: T)` | Insert at index |
| `remove` | `fn remove(&mut self, idx: usize) -> T` | Remove at index |
| `len` | `fn len(&self) -> usize` | Element count |
| `is_empty` | `fn is_empty(&self) -> bool` | True if empty |
| `contains` | `fn contains(&self, val: &T) -> bool` | Membership test |
| `sort` | `fn sort(&mut self)` | In-place sort (timsort) |
| `dedup` | `fn dedup(&mut self)` | Remove consecutive duplicates |
| `iter` | `fn iter(&self) -> VecIter<T>` | Borrowing iterator |
| `drain` | `fn drain(&mut self, start, end) -> Vec<T>` | Remove range |
| `splice` | `fn splice(&mut self, start, end, iter)` | Replace range |
| `retain` | `fn retain(&mut self, f: fn(&T) -> bool)` | Keep matching |
| `reverse` | `fn reverse(&mut self)` | Reverse in-place |
| `windows` | `fn windows(&self, size: usize) -> WindowsIter<T>` | Sliding windows |
| `chunks` | `fn chunks(&self, size: usize) -> ChunksIter<T>` | Fixed-size chunks |

**Traits implemented:** `Drop`, `Clone`, `Eq`, `Debug`, `Index`, `IndexMut`, `IntoIterator`

### 1.3 HashMap\<K, V\>

**File:** `src/core/map.grey`

A hash map using Robin Hood open addressing.

```grey
let mut m = HashMap::new();
m.insert("name", "Grey++");
m.insert("version", "1.0");

let name = m.get(&"name");  // Some("Grey++")
m.remove(&"version");

for (k, v) in m.iter() {
    println("{}: {}", k, v);
}
```

**Key methods:** `new`, `with_capacity`, `insert`, `get`, `get_mut`, `remove`, `contains_key`, `len`, `is_empty`, `iter`, `keys`, `values`, `entry`, `clear`

### 1.4 HashSet\<T\>

**File:** `src/core/set.grey`

A hash set built on `HashMap<T, ()>`.

```grey
let mut s = HashSet::new();
s.insert(1);
s.insert(2);
s.insert(3);

let has_two = s.contains(&2);        // true
let union = s.union(&other_set);
let inter = s.intersection(&other_set);
let diff = s.difference(&other_set);
```

### 1.5 Option\<T\>

**File:** `src/core/option.grey`

Represents an optional value — either `Some(T)` or `None`. Eliminates null pointer errors.

```grey
let some: Option<i32> = Some(42);
let none: Option<i32> = None;

// Safe unwrapping
let val = some.unwrap_or(0);
let mapped = some.map(|x| x * 2);        // Some(84)
let chained = some.and_then(|x| if x > 0 { Some(x) } else { None });

// Flattening nested Options
let nested: Option<Option<i32>> = Some(Some(10));
let flat = nested.flatten();              // Some(10)

// Iteration
for val in Some(42) {
    println("{}", val);   // prints 42
}
```

**Key methods:** `is_some`, `is_none`, `unwrap`, `unwrap_or`, `unwrap_or_else`, `map`, `and_then`, `or`, `or_else`, `filter`, `flatten`, `ok_or`, `ok_or_else`, `iter`, `zip`, `unzip`, `take`, `replace`, `get_or_insert`

**Traits implemented:** `Clone`, `Eq`, `Debug`, `Display`, `IntoIterator`

### 1.6 Result\<T, E\>

**File:** `src/core/result.grey`

Represents success (`Ok(T)`) or failure (`Err(E)`). Used pervasively for error handling.

```grey
fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 { Err(String::from("division by zero")) }
    else { Ok(a / b) }
}

let r = divide(10.0, 3.0);
let value = r.unwrap_or(0.0);

// ? operator for early return
fn process() -> Result<(), Error> {
    let data = read_file("config.toml")?;
    let parsed = parse_config(&data)?;
    Ok(())
}

// Chaining
let result = Ok(5)
    .map(|x| x * 2)
    .and_then(|x| if x > 5 { Ok(x) } else { Err("too small") });
```

**Key methods:** `is_ok`, `is_err`, `unwrap`, `unwrap_err`, `unwrap_or`, `unwrap_or_else`, `map`, `map_err`, `and_then`, `or`, `or_else`, `ok`, `err`, `transpose`, `flatten`, `inspect`, `inspect_err`, `iter`

**Traits implemented:** `Clone`, `Eq`, `Debug`, `Display`, `IntoIterator`

### 1.7 Iterator

**File:** `src/core/iter.grey`

The iterator protocol is the backbone of Grey++'s collection processing. Iterators are lazy, composable, and zero-cost.

```grey
use greystd::core::iter::{ Iterator, IntoIterator, range, once, empty, from_fn };

// Chaining adapters
let result: Vec<i32> = range(1, 100)
    .filter(|x| x % 2 == 0)
    .map(|x| x * x)
    .take(10)
    .collect();

// Folding
let sum = vec![1, 2, 3, 4, 5].iter().fold(0, |acc, x| acc + x);

// Flattening nested iterators
let nested = vec![vec![1, 2], vec![3, 4]];
let flat: Vec<i32> = nested.into_iter().flatten().collect();
```

**Adapters (lazy):**

| Adapter | Description |
|---------|-------------|
| `map(f)` | Transform each element |
| `filter(p)` | Keep elements matching predicate |
| `flat_map(f)` | Map then flatten |
| `flatten()` | Flatten nested iterators |
| `take(n)` | First n elements |
| `skip(n)` | Skip first n elements |
| `take_while(p)` | Take while predicate holds |
| `skip_while(p)` | Skip while predicate holds |
| `chain(other)` | Concatenate two iterators |
| `zip(other)` | Pair elements from two iterators |
| `enumerate()` | Attach index to each element |
| `peekable()` | Allow peeking ahead |
| `inspect(f)` | Side-effect without consuming |
| `step_by(n)` | Take every nth element |
| `intersperse(sep)` | Insert separator between elements |
| `dedup()` | Remove consecutive duplicates |

**Consumers (eager):**

| Consumer | Description |
|----------|-------------|
| `collect()` | Gather into `Vec<T>` |
| `fold(init, f)` | Reduce to single value |
| `reduce(f)` | Reduce without initial value |
| `for_each(f)` | Side-effect on each element |
| `any(p)` / `all(p)` | Short-circuiting boolean tests |
| `find(p)` | First matching element |
| `position(p)` | Index of first match |
| `count()` | Number of elements |
| `last()` | Last element |
| `min()` / `max()` | Extrema |
| `sum()` / `product()` | Arithmetic aggregation |
| `partition(p)` | Split into two collections |
| `unzip()` | Split pairs into two Vecs |

**Factory functions:**

| Function | Description |
|----------|-------------|
| `empty()` | Yields nothing |
| `once(v)` | Yields a single element |
| `range(start, end)` | `[start, end)` integer range |
| `repeat_with(f)` | Infinite lazily-produced values |
| `from_fn(f)` | Values from a closure |
| `successors(first, f)` | Successive applications |
| `count_from(n)` | Infinite incrementing counter |

**Generators:**

```grey
let fib = generator {
    let (mut a, mut b) = (0, 1);
    loop {
        yield a;
        let next = a + b;
        a = b;
        b = next;
    }
};

let first_10: Vec<i64> = fib.take(10).collect();
```

### 1.8 Math

**File:** `src/core/math.grey`

Numeric utilities and constants.

| Function | Description |
|----------|-------------|
| `abs(x)` | Absolute value |
| `min(a, b)` | Minimum of two values |
| `max(a, b)` | Maximum of two values |
| `clamp(x, lo, hi)` | Clamp to range |
| `pow(base, exp)` | Exponentiation |
| `sqrt(x)` | Square root |
| `floor(x)` / `ceil(x)` | Rounding |
| `sin(x)` / `cos(x)` / `tan(x)` | Trigonometry |
| `log(x)` / `log2(x)` / `log10(x)` | Logarithms |

**Constants:** `PI`, `E`, `TAU`, `INFINITY`, `NEG_INFINITY`, `NAN`

### 1.9 Bytes & ByteBuffer

**File:** `src/core/bytes.grey`

Immutable byte sequences and a mutable byte buffer for binary protocols.

```grey
let bytes = Bytes::from_slice(&[0x48, 0x65, 0x6C, 0x6C, 0x6F]);
let slice = bytes.slice(0, 3);

let mut buf = ByteBuffer::new();
buf.write_u8(0xFF);
buf.write_u32_be(12345);
buf.write_str("hello");
let data = buf.freeze();   // -> Bytes
```

### 1.10 Array\<T, N\>

**File:** `src/core/array.grey`

Fixed-size, stack-allocated array with compile-time length.

```grey
let arr: Array<i32, 5> = Array::new([1, 2, 3, 4, 5]);
let first = arr[0];
let slice = arr.as_slice();
```

### 1.11 Tuple

**File:** `src/core/tuple.grey`

Heterogeneous fixed-size sequences. Supports up to 12 elements.

```grey
let pair = (42, "hello");
let triple = (1, 2.0, true);
let first = pair.0;
```

---

## 2. Memory Module

**Path:** `src/mem/`

Smart pointers and memory management utilities.

| Type | File | Description |
|------|------|-------------|
| `Box<T>` | `smart.grey` | Unique ownership, heap-allocated |
| `Rc<T>` | `smart.grey` | Reference-counted shared ownership (single-thread) |
| `Arc<T>` | `smart.grey` | Atomically reference-counted (thread-safe) |
| `Weak<T>` | `smart.grey` | Non-owning reference to Rc/Arc |
| `Pin<T>` | `pin.grey` | Pinned memory for self-referential types |
| `Arena` | `arena.grey` | Region-based bump allocator |

```grey
use greystd::mem::{ Box, Rc, Arc };

let b = Box::new(42);           // unique heap allocation
let rc = Rc::new("shared");     // shared within one thread
let arc = Arc::new(100);        // shared across threads

let weak = Rc::downgrade(&rc);  // non-owning reference
if let Some(val) = weak.upgrade() {
    println("still alive: {}", val);
}
```

---

## 3. Concurrency Module

**Path:** `src/concurrent/`

Async-first concurrency with structured task management.

| Component | File | Description |
|-----------|------|-------------|
| `spawn` / `Task` | `task.grey` | Spawn async tasks |
| `Future` / `Poll` | `future.grey` | Async computation trait |
| `Mutex<T>` | `mutex.grey` | Mutual exclusion lock |
| `RwLock<T>` | `mutex.grey` | Readers-writer lock |
| `Channel` | `channel.grey` | MPSC message passing |
| `Semaphore` | `mutex.grey` | Counting semaphore |
| `AtomicBool/I64/Ptr` | `atomic.grey` | Lock-free atomics |
| `Once` | `once.grey` | One-time initialization |
| `ThreadPool` | `pool.grey` | Worker thread pool |
| `EventLoop` | `event_loop.grey` | Async event loop |
| `sleep` / `sleep_sync` | `timer.grey` | Async and blocking sleep |
| `Delay` / `Interval` / `Timeout` | `timer.grey` | Timer utilities |

```grey
use greystd::concurrent::{ spawn, Mutex, Channel, sleep };
use greystd::time::Duration;

let counter = Arc::new(Mutex::new(0));

for _ in 0..10 {
    let c = counter.clone();
    spawn(async {
        let mut guard = c.lock();
        *guard += 1;
    });
}

sleep(Duration::from_millis(100)).await;
println("counter: {}", *counter.lock());

// Channels
let (tx, rx) = Channel::new();
spawn(async { tx.send(42); });
let val = rx.recv().await.unwrap();
```

---

## 4. IO Module

**Path:** `src/io/`

File system operations, streams, and console I/O.

| Component | File | Description |
|-----------|------|-------------|
| `print` / `println` | `mod.grey` | Write to stdout |
| `eprint` / `eprintln` | `mod.grey` | Write to stderr |
| `read_line` | `mod.grey` | Read line from stdin |
| `File` | `file.grey` | File operations |
| `Dir` | `dir.grey` | Directory operations |
| `InputStream` / `OutputStream` | `stream.grey` | Byte streams |
| `BufReader` / `BufWriter` | `stream.grey` | Buffered I/O |
| `Pipe` | `pipe.grey` | Inter-process pipes |
| `MemoryMappedFile` | `mmap.grey` | Memory-mapped files |

```grey
use greystd::io::{ println, File };

// Console output
println("Hello, World!");

// File I/O
let content = File::read_to_string("data.txt")?;
File::write_string("output.txt", &content)?;

// Streaming
let file = File::open("large.bin", OpenMode::Read)?;
let reader = BufReader::new(file);
for line in reader.lines() {
    process(line?);
}
```

---

## 5. Networking Module

**Path:** `src/net/`

TCP/UDP sockets, HTTP client, WebSockets, DNS, and TLS.

```grey
use greystd::net::http::{ HttpClient, HttpRequest, HttpMethod };
use greystd::net::url::Url;

// HTTP request
let client = HttpClient::new();
let resp = client.get("https://api.example.com/data").await?;
println("Status: {}", resp.status());
println("Body: {}", resp.body_string()?);

// TCP
let stream = TcpStream::connect("127.0.0.1:8080").await?;
stream.write_all(b"GET / HTTP/1.1\r\n\r\n").await?;

// URL parsing
let url = Url::parse("https://user:pass@example.com:443/path?q=1#frag")?;
```

---

## 6. Serialization Module

**Path:** `src/serial/`

Data format support with trait-based serialization.

```grey
use greystd::serial::{ Serialize, Deserialize };
use greystd::serial::json::{ to_json, from_json };
use greystd::serial::binary::{ base64_encode, base64_decode, hex_encode, hex_decode };

// JSON
let json_str = to_json(&my_struct);
let parsed: MyStruct = from_json(&json_str)?;

// Base64
let encoded = base64_encode(data.as_bytes());
let decoded = base64_decode(&encoded)?;

// Hex
let hex = hex_encode(&[0xDE, 0xAD, 0xBE, 0xEF]);  // "deadbeef"
```

**Primitive Serialize impls:** `bool`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `String`, `&str`, `Vec<T>`, `Option<T>`, `HashMap<K, V>`

**Primitive Deserialize impls:** `bool`, `i32`, `i64`, `u32`, `u64`, `f64`, `String`, `Vec<T>`, `Option<T>`, `HashMap<K, V>`

**Supported formats:** JSON, YAML, TOML, MessagePack, Binary (base64/hex)

---

## 7. Time Module

**Path:** `src/time/`

Temporal types, clocks, and scheduling.

```grey
use greystd::time::{ Duration, Instant };
use greystd::time::clock::{ SystemClock, FakeClock, Stopwatch };
use greystd::time::date::{ Date, DateTime, TimeZone };

// Duration
let d = Duration::from_secs(5) + Duration::from_millis(500);
println("{}ms", d.as_millis());  // 5500

// Clock
let now = SystemClock::unix_now();  // seconds since epoch

// FakeClock for testing
let mut clock = FakeClock::new(1000);
clock.advance(Duration::from_secs(60));
assert_eq(clock.unix_timestamp(), 1060);

// Stopwatch
let mut sw = Stopwatch::new();
sw.start();
expensive_operation();
sw.stop();
println("Elapsed: {}ms", sw.elapsed().as_millis());

// Scheduling
let cron = CronExpr::parse("0 */5 * * *")?;  // every 5 minutes
```

---

## 8. Crypto Module

**Path:** `src/crypto/`

Cryptographic primitives — hashing, encryption, and secure random.

```grey
use greystd::crypto::hash::{ Sha256, Sha512 };
use greystd::crypto::random::SecureRandom;
use greystd::crypto::aes::{ Aes256, AesKey };

// Hashing
let mut hasher = Sha256::new();
hasher.update(b"hello world");
let digest = hasher.finalize();  // 32 bytes

// Secure random
let mut rng = SecureRandom::new();
let key_bytes = rng.bytes(32);

// AES encryption
let key = AesKey::from_bytes(&key_bytes);
let cipher = Aes256::new(&key);
let encrypted = cipher.encrypt(plaintext, &iv)?;
let decrypted = cipher.decrypt(&encrypted, &iv)?;
```

---

## 9. System Module

**Path:** `src/sys/`

OS interfaces — processes, environment, FFI.

```grey
use greystd::sys::process::Command;
use greystd::sys::env;

// Spawn a process
let output = Command::new("ls")
    .arg("-la")
    .output()?;

println("stdout: {}", output.stdout);

// Environment
let home = env::var("HOME")?;
let cwd = env::current_dir();
```

---

## 10. Module System

**Path:** `src/module/`

Package metadata, versioning, and dynamic module loading.

```grey
use greystd::module::{ ModuleInfo, Version };

let v = Version::parse("1.2.3")?;
assert_eq(v.major(), 1);
assert_eq(v.minor(), 2);
assert_eq(v.patch(), 3);

// Compare versions
assert(Version::parse("2.0.0")? > Version::parse("1.9.9")?);
```

---

## 11. Error Handling

**Path:** `src/error/`

Structured error types with chaining, categorization, and stack traces.

```grey
use greystd::error::{ Error, ErrorKind, AnyError };

// Custom errors
let err = AnyError::new("something failed");
let wrapped = AnyError::with_source("outer operation", Box::new(inner_err));

// Error kinds
match err.kind() {
    ErrorKind::NotFound => println("file not found"),
    ErrorKind::PermissionDenied => println("access denied"),
    ErrorKind::Timeout => println("operation timed out"),
    _ => println("other error: {}", err),
}

// The ? operator propagates errors automatically
fn load_config() -> Result<Config, Error> {
    let text = File::read_to_string("config.toml")?;
    let config = parse_toml(&text)?;
    Ok(config)
}
```

`ErrorKind` variants: `NotFound`, `PermissionDenied`, `AlreadyExists`, `InvalidInput`, `InvalidData`, `Timeout`, `ConnectionRefused`, `ConnectionReset`, `BrokenPipe`, `OutOfMemory`, `Interrupted`, `Other`

All `ErrorKind` values implement `Clone`, `Copy`, `Eq`, and `PartialEq`.

---

## 12. Testing Framework

**Path:** `src/test/`

Built-in unit testing with assertions and test runners.

```grey
use greystd::test::{ TestCase, TestSuite, assert_eq, assert_ne, assert };

fn test_addition() {
    assert_eq(2 + 2, 4, "basic addition");
}

fn test_string_ops() {
    let s = String::from("hello");
    assert(s.contains("ell"), "contains substring");
    assert_ne(s.len(), 0, "string is not empty");
}

pub fn main() {
    let mut suite = TestSuite::new("My Tests");
    suite.add(TestCase::new("addition", test_addition));
    suite.add(TestCase::new("string_ops", test_string_ops));

    let report = suite.run();
    println(report.summary());
}
```

**Assertion functions:** `assert(cond, msg)`, `assert_eq(a, b, msg)`, `assert_ne(a, b, msg)`

**Benchmark support:** `src/test/bench.grey` — micro-benchmarking with warm-up, iterations, and statistical reporting.

---

## 13. Reflection

**Path:** `src/reflect/`

Runtime type introspection and metadata.

```grey
use greystd::reflect::{ TypeInfo, TypeId, FieldInfo };

let info = TypeInfo::of::<Vec<i32>>();
println("Type: {}", info.name());
println("Size: {} bytes", info.size());

// Type identity
let id1 = TypeId::of::<i32>();
let id2 = TypeId::of::<String>();
assert_ne(id1, id2);

// Struct field introspection
for field in info.fields() {
    println("  {}: {}", field.name(), field.type_name());
}
```

---

## 14. Deterministic Execution

**Path:** `src/deterministic/`

Grey++'s unique advantage — deterministic collections, scheduling, and replayable I/O for verifiable computing.

```grey
use greystd::deterministic::{ DeterministicRuntime, ReplayIo };

// Create a deterministic runtime with a fixed seed
let rt = DeterministicRuntime::new(42);
let io = rt.io();

// All randomness, time, and I/O are deterministic and replayable
let random_bytes = io.random_bytes(16);  // always the same for seed=42
let now = io.now();                       // controlled clock

// Deterministic collections maintain insertion order
use greystd::deterministic::det_map::DetHashMap;
let mut m = DetHashMap::new();
m.insert("b", 2);
m.insert("a", 1);
// Iteration order is guaranteed: [("b", 2), ("a", 1)]
```

**Components:** `DetHashMap`, `DetHashSet`, `DetVec`, `ReplayIo`, `DetScheduler`, `DetAllocator`

---

## Prelude

The prelude automatically imports the most essential types so you don't have to:

```grey
// These are always available without explicit `use`:
String, Bytes, ByteBuffer, Array, Vec, HashMap, HashSet, Tuple,
Option (Some, None), Result (Ok, Err),
Iterator, IntoIterator,
math,
Box, Rc, Arc,
Error, ErrorKind,
print, println, eprint, eprintln
```

To use the prelude explicitly:

```grey
use greystd::prelude::*;
```

---

## Project Structure

```
greystd/
├── greystd.toml          # Package manifest
├── LICENSE                # MIT License
├── README.md              # Project overview
├── DOCS.md                # This file
├── src/
│   ├── lib.grey           # Root module — re-exports all subsystems
│   ├── core/              # Foundational types
│   ├── mem/               # Smart pointers & memory
│   ├── concurrent/        # Async, locks, channels, atomics
│   ├── io/                # Files, streams, console
│   ├── net/               # TCP, UDP, HTTP, WebSocket
│   ├── serial/            # JSON, YAML, TOML, binary
│   ├── time/              # Clock, duration, scheduling
│   ├── crypto/            # Hash, AES, RNG
│   ├── sys/               # Process, env, FFI
│   ├── module/            # Versioning, module loading
│   ├── error/             # Error types & diagnostics
│   ├── test/              # Unit tests & benchmarks
│   ├── reflect/           # Runtime type info
│   └── deterministic/     # Deterministic execution
└── tests/
    └── validate.grey      # Validation test suite (87 tests)
```

---

*GreyStd v1.0.0 — MIT License — Copyright (c) 2026 Robert-Marshall01*
