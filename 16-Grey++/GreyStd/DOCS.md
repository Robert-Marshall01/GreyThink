# GreyStd Documentation

> Comprehensive API reference for the Grey++ Standard Library.

All code in GreyStd uses **Grey++ syntax**: `fn`-only declarations, objects `{key: value}` for data,
function-based control flow (`if_then`, `cond`, `match`, `when`, `unless`), and 120+ built-in functions.

---

## Table of Contents

- [1. Core Module](#1-core-module)
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

---

## 1. Core Module

**Path:** `src/core/`

### 1.1 String (`src/core/string.grey`)

String utilities beyond built-in `split`, `trim`, `upper`, `lower`, etc.

```grey
fn s() { string_new("hello world") }
fn rev() { string_reverse(s()) }           // { value: "dlrow olleh" }
fn cap() { string_capitalize(s()) }         // { value: "Hello world" }
fn title() { string_title(s()) }           // { value: "Hello World" }
fn lines() { string_lines("a\nb\nc") }    // ["a", "b", "c"]
fn words() { string_words("hello world") } // ["hello", "world"]

// StringBuilder
fn sb() { sb_push(sb_push(StringBuilder_new(), "foo"), "bar") }
fn result() { sb_build(sb()) }             // "foobar"
```

### 1.2 Vec (`src/core/vec.grey`)

Dynamic-array utilities with immutable push/pop returning new vectors.

```grey
fn v() { vec_new() }
fn v1() { vec_push(v(), 42) }
fn v2() { vec_push(v1(), 99) }
fn popped() { vec_pop(v2()) }   // { value: 99, vec: <vec with [42]> }
fn sorted() { vec_sort_by_key(v2(), fn(x) { x }) }
```

### 1.3 Map (`src/core/map.grey`)

Object/hashmap utilities.

```grey
fn m() { map_from_entries([["a", 1], ["b", 2], ["c", 3]]) }
fn val() { map_get_or(m(), "a", 0) }       // 1
fn updated() { map_update(m(), "a", fn(v) { add(v, 10) }) }
fn inverted() { map_invert(m()) }           // { "1": "a", "2": "b", ... }
fn freqs() { map_frequencies(["a", "b", "a", "c", "a"]) }
```

### 1.4 Set (`src/core/set.grey`)

Set operations using objects as backing store.

```grey
fn s1() { set_from([1, 2, 3]) }
fn s2() { set_from([2, 3, 4]) }
fn u() { set_union(s1(), s2()) }
fn i() { set_intersection(s1(), s2()) }
fn d() { set_difference(s1(), s2()) }
set_contains(s1(), 2)  // true
set_is_subset(s1(), u())  // true
```

### 1.5 Option (`src/core/option.grey`)

Optional value type: `Some(value)` or `None()`.

```grey
fn val() { Some(42) }
fn empty() { None() }
is_some(val())                    // true
is_none(empty())                  // true
option_unwrap(val())              // 42
option_unwrap_or(empty(), 0)      // 0
option_map(val(), fn(x) { mul(x, 2) })  // Some(84)
option_and_then(val(), fn(x) { if_then(gt(x, 0), fn() { Some(x) }, fn() { None() }) })
```

### 1.6 Result (`src/core/result.grey`)

Error-or-value type: `Ok(value)` or `Err(error)`.

```grey
fn success() { Ok(42) }
fn failure() { Err("oops") }
is_ok(success())                  // true
is_err(failure())                 // true
result_unwrap(success())          // 42
result_map(success(), fn(x) { add(x, 1) })  // Ok(43)
result_and_then(success(), fn(x) { Ok(mul(x, 2)) })
```

### 1.7 Iterator (`src/core/iter.grey`)

Lazy iterator protocol and combinators.

```grey
fn it() { iter_range(1, 10) }
fn sum() { iter_fold(it(), 0, fn(acc, x) { add(acc, x) }) }
fn evens() { iter_take_while(iter_range(0, 100), fn(x) { lt(x, 10) }) }
fn enumerated() { iter_enumerate(iter_from([10, 20, 30])) }
```

### 1.8 Math (`src/core/math.grey`)

Numerical utilities.

```grey
gcd(12, 8)        // 4
lcm(4, 6)         // 12
factorial(5)       // 120
fibonacci(10)      // 55
mean([1,2,3,4,5])  // 3
median([1,2,3,4,5])  // 3
std_dev([2,4,4,4,5,5,7,9])
is_even(4)         // true
is_odd(7)          // true
```

### 1.9 Bytes (`src/core/bytes.grey`)

Byte array and buffer utilities.

```grey
fn b() { bytes_from([72, 101, 108]) }
fn hex() { bytes_to_hex(b()) }
fn buf() { buf_write_u8(buf_write_u8(ByteBuffer_new(), 0xCA), 0xFE) }
fn frozen() { buf_freeze(buf()) }
```

### 1.10 Array (`src/core/array.grey`)

Array utilities beyond built-in `map`, `filter`, `reduce`.

```grey
fn a() { array_range_step(0, 10, 2) }       // [0, 2, 4, 6, 8]
fn chunks() { array_chunk([1,2,3,4,5], 2) }  // [[1,2],[3,4],[5]]
fn parts() { array_partition([1,2,3,4], fn(x) { is_even(x) }) }
fn windows() { array_windows([1,2,3,4], 3) } // [[1,2,3],[2,3,4]]
```

### 1.11 Tuple (`src/core/tuple.grey`)

Lightweight fixed-size groupings.

```grey
fn p() { pair(1, "hello") }
fst(p())   // 1
snd(p())   // "hello"
fn t() { triple("a", "b", "c") }
trd(t())   // "c"
tuple_swap(p())  // pair("hello", 1)
```

---

## 2. Memory Module

**Path:** `src/mem/`

### 2.1 Smart Pointers (`src/mem/smart_ptr.grey`)

```grey
fn b() { Box_new(42) }
box_unwrap(b())              // 42
box_map(b(), fn(x) { add(x, 1) })  // Box(43)

fn rc() { Rc_new("shared") }
fn rc2() { rc_clone(rc()) }
rc_count(rc2())              // 2
```

### 2.2 Arena (`src/mem/arena.grey`)

```grey
fn a() { Arena_new() }
fn r() { arena_alloc(a(), "value") }  // { arena: ..., handle: 0 }
arena_get(get(r(), "arena"), get(r(), "handle"))  // "value"
```

### 2.3 Clone (`src/mem/clone.grey`)

```grey
fn original() { { a: 1, b: { c: 2 } } }
fn cloned() { clone(original()) }  // deep copy
structurally_equal(original(), cloned())  // true
```

### 2.4 Borrow (`src/mem/borrow.grey`)

Copy-on-write semantics.

```grey
fn cow() { Cow_borrowed("hello") }
fn owned() { cow_to_owned(cow()) }
fn mapped() { cow_map(cow(), fn(s) { upper(s) }) }
```

### 2.5 RefCell (`src/mem/ref_wrapper.grey`)

```grey
fn cell() { Cell_new(0) }
fn updated() { cell_set(cell(), 42) }
cell_get(updated())  // 42
cell_update(cell(), fn(x) { add(x, 1) })
```

---

## 3. Concurrency Module

**Path:** `src/concurrent/`

### 3.1 Task (`src/concurrent/task.grey`)

```grey
fn t() { task_spawn(fn() { add(1, 2) }) }
fn named() { task_spawn_named("compute", fn() { factorial(10) }) }
fn all() { task_spawn_all([fn() { 1 }, fn() { 2 }, fn() { 3 }]) }
```

### 3.2 Future (`src/concurrent/future.grey`)

```grey
fn f() { future_ready(42) }
fn mapped() { future_map(f(), fn(x) { mul(x, 2) }) }
fn chained() { future_and_then(f(), fn(x) { future_ready(add(x, 1)) }) }
fn all() { future_join_all([future_ready(1), future_ready(2)]) }
```

### 3.3 Mutex (`src/concurrent/mutex.grey`)

```grey
fn m() { Mutex_new(0) }
fn result() { mutex_lock(m()) }   // { guard: 0, mutex: ... }
fn unlocked() { mutex_unlock(get(result(), "mutex")) }

fn sem() { Semaphore_new(3) }
fn acquired() { semaphore_acquire(sem()) }
```

### 3.4 Channel (`src/concurrent/channel.grey`)

```grey
fn ch() { Channel_new(10) }       // { sender: ..., receiver: ... }
fn s() { channel_send(get(ch(), "sender"), "hello") }
fn r() { channel_recv(get(ch(), "receiver")) }
```

### 3.5 Atomic (`src/concurrent/atomic.grey`)

```grey
fn flag() { AtomicBool_new(false) }
fn stored() { atomic_bool_store(flag(), true) }
atomic_bool_load(stored())  // true

fn counter() { AtomicInt_new(0) }
fn inc() { atomic_int_inc(counter()) }
```

### 3.6 ThreadPool (`src/concurrent/pool.grey`)

```grey
fn pool() { ThreadPool_new(4) }
fn submitted() { pool_submit(pool(), fn() { factorial(20) }) }
pool_stats(pool())
```

### 3.7 EventLoop (`src/concurrent/event_loop.grey`)

```grey
fn loop() { EventLoop_new() }
fn with_task() { loop_next_tick(loop(), fn() { println("tick!") }) }
fn with_timeout() { loop_set_timeout(loop(), 1000, fn() { println("delayed") }) }
```

### 3.8 Timer (`src/concurrent/timer.grey`)

```grey
fn sw() { Stopwatch_new() }
fn started() { stopwatch_start(sw()) }
// ... do work ...
fn stopped() { stopwatch_stop(started()) }
stopwatch_elapsed(stopped())  // Duration
```

---

## 4. IO Module

**Path:** `src/io/`

### 4.1 Core IO (`src/io/mod.grey`)

```grey
io_println("Hello, world!")
io_eprint("Warning!")
fn input() { io_read_line() }
```

### 4.2 File (`src/io/file.grey`)

```grey
fn content() { file_read("data.txt") }
file_write("output.txt", "Hello!")
file_append("log.txt", "Entry\n")
fn lines() { file_read_lines("data.txt") }
fn info() { FileInfo_new("path/to/file.txt") }
file_extension(info())  // "txt"
```

### 4.3 Directory (`src/io/dir.grey`)

```grey
fn d() { Dir_new("/home/user") }
fn entries() { dir_list(d()) }
fn parent() { dir_parent(d()) }
fn sub() { dir_join(d(), "documents") }
```

### 4.4 Streams (`src/io/stream.grey`)

```grey
fn input() { InputStream_from_string("line1\nline2\nline3") }
fn line() { stream_read_line(input()) }
fn out() { OutputStream_new() }
fn written() { stream_write(out(), "data") }
```

### 4.5 Buffered IO (`src/io/buffered.grey`)

```grey
fn reader() { BufReader_new("file.txt") }
fn lines() { bufreader_lines(reader()) }
fn writer() { BufWriter_new("output.txt") }
fn w() { bufwriter_write(writer(), "buffered data") }
bufwriter_flush(w())
```

### 4.6 Pipes (`src/io/pipe.grey`)

```grey
fn p() { Pipe_new() }    // { reader: ..., writer: ... }
fn written() { pipe_write(get(p(), "writer"), "hello") }
fn data() { pipe_read(get(p(), "reader")) }
```

### 4.7 Memory-Mapped Files (`src/io/mmap.grey`)

```grey
fn m() { MappedFile_new("large_file.bin") }
fn data() { mmap_read(m(), 0, 1024) }
mmap_size(m())
```

---

## 5. Networking Module

**Path:** `src/net/`

### 5.1 TCP (`src/net/tcp.grey`)

```grey
fn conn() { TcpStream_connect("example.com", 80) }
fn sent() { tcp_write(conn(), "GET / HTTP/1.1\r\n\r\n") }
fn response() { tcp_read(conn()) }
tcp_close(conn())

fn server() { TcpListener_bind("0.0.0.0", 8080) }
```

### 5.2 UDP (`src/net/udp.grey`)

```grey
fn sock() { UdpSocket_bind("0.0.0.0", 9000) }
fn sent() { udp_send_to(sock(), "hello", "192.168.1.1", 9001) }
fn received() { udp_recv_from(sock()) }
```

### 5.3 HTTP (`src/net/http.grey`)

```grey
fn client() { HttpClient_new() }
fn response() { http_get_url(client(), "https://api.example.com/data") }
response_status(response())
response_body(response())
fn json_data() { response_json(response()) }
```

### 5.4 WebSocket (`src/net/websocket.grey`)

```grey
fn ws() { WebSocket_connect("wss://echo.websocket.org") }
ws_send(ws(), "hello")
ws_on_message(ws(), fn(msg) { println(msg) })
ws_close(ws())
```

### 5.5 DNS (`src/net/dns.grey`)

```grey
fn ip() { dns_lookup("example.com") }
fn parsed() { url_parse("https://example.com/path?q=1") }
// { protocol: "https", host: "example.com", path: "/path", query: "q=1" }
```

### 5.6 TLS (`src/net/tls.grey`)

```grey
fn config() { TlsConfig_new() }
fn secure() { tls_set_min_version(config(), "1.2") }
fn wrapped() { tls_wrap(tcp_conn(), secure()) }
tls_handshake(wrapped())
```

---

## 6. Serialization Module

**Path:** `src/serial/`

### 6.1 JSON (`src/serial/json.grey`)

```grey
fn obj() { { name: "Grey", version: 1 } }
fn json_str() { json_serialize(obj()) }
fn pretty() { json_pretty(obj()) }
fn parsed() { json_parse(json_str()) }
json_is_valid(json_str())  // true

fn nested() { json_get_path(obj(), "name") }
fn flat() { json_flatten({ a: { b: 1 } }) }   // { "a.b": 1 }
fn diff() { json_diff({ a: 1 }, { a: 2, b: 3 }) }
```

### 6.2 YAML (`src/serial/yaml.grey`)

```grey
fn data() { yaml_parse("name: Grey\nversion: 1") }
fn yaml_str() { yaml_serialize(data()) }
fn doc() { YamlDoc_new({ key: "value" }) }
```

### 6.3 TOML (`src/serial/toml.grey`)

```grey
fn data() { toml_parse("name = \"Grey\"\nversion = 1") }
fn doc() { TomlDoc_new() }
fn with_data() { toml_doc_set(doc(), "name", "Grey") }
fn with_section() { toml_doc_add_section(doc(), "dependencies", { other_lib: "1.0" }) }
```

### 6.4 MsgPack (`src/serial/msgpack.grey`)

```grey
fn encoded() { msgpack_encode(42) }
msgpack_type(encoded())   // "fixint"
msgpack_value(encoded())  // 42
fn str_enc() { msgpack_encode("hello") }
```

### 6.5 Binary (`src/serial/binary.grey`)

```grey
fn enc() { encoder_write_u32(encoder_write_u8(BinaryEncoder_new(), 0xFF), 1000) }
fn bytes() { encoder_bytes(enc()) }
fn dec() { BinaryDecoder_new(bytes()) }
fn r1() { decoder_read_u8(dec()) }   // { value: 255, decoder: ... }
fn r2() { decoder_read_u32(get(r1(), "decoder")) }
```

---

## 7. Time Module

**Path:** `src/time/`

### 7.1 Duration (`src/time/duration.grey`)

```grey
fn d() { Duration_secs(5) }
duration_ms(d())        // 5000
duration_format(d())    // "5s"
fn d2() { Duration_mins(2) }
fn sum() { duration_add(d(), d2()) }
duration_format(sum())  // "2m 5s"
```

### 7.2 Instant (`src/time/instant.grey`)

```grey
fn start() { Instant_now() }
// ... do work ...
fn elapsed() { instant_elapsed(start()) }  // Duration
fn measured() { measure_time(fn() { factorial(20) }) }
// { result: 2432902008176640000, elapsed: Duration }
```

### 7.3 Clock (`src/time/clock.grey`)

```grey
fn now() { clock_now() }
clock_format(now())     // "14:30:45"
clock_format_ms(now())  // "14:30:45.123"
clock_epoch_ms()        // 1703001045123
```

### 7.4 Timezone (`src/time/timezone.grey`)

```grey
fn utc() { TZ_UTC() }
fn pst() { TZ_PST() }
fn dt() { DateTime_now(pst()) }
datetime_format(dt())   // "06:30:45 PST"
fn converted() { datetime_to_tz(dt(), utc()) }
```

### 7.5 Schedule (`src/time/schedule.grey`)

```grey
fn interval() { Schedule_interval(Duration_secs(5)) }
fn once() { Schedule_once(Duration_secs(10)) }
fn repeat() { Schedule_repeat(Duration_secs(1), 5) }

fn task() { ScheduledTask_new("cleanup", interval(), fn() { println("cleaning...") }) }
fn sched() { scheduler_add(Scheduler_new(), task()) }
fn next() { scheduler_tick(sched()) }
```

---

## 8. Crypto Module

**Path:** `src/crypto/`

### 8.1 Hash (`src/crypto/hash.grey`)

```grey
fn digest() { sha256("hello world") }
fn h() { HashDigest_new("sha256", "data") }
digest_hex(h())
hash_verify("data", digest(), "sha256")

fn hasher() { hasher_update(hasher_update(Hasher_new("sha256"), "part1"), "part2") }
fn final_hash() { hasher_finish(hasher()) }
```

### 8.2 Symmetric Encryption (`src/crypto/symmetric.grey`)

```grey
fn key() { generate_key_256() }
fn cipher() { aes256_cbc(key()) }
fn encrypted() { cipher_encrypt(cipher(), "secret message") }
fn decrypted() { cipher_decrypt(cipher(), encrypted()) }
```

### 8.3 Asymmetric Encryption (`src/crypto/asymmetric.grey`)

```grey
fn kp() { rsa_generate(2048) }
fn pub_key() { keypair_public(kp()) }
fn priv_key() { keypair_private(kp()) }
fn encrypted() { rsa_encrypt("secret", pub_key()) }
fn decrypted() { rsa_decrypt(encrypted(), priv_key()) }
fn sig() { rsa_sign("data", priv_key()) }
rsa_verify("data", sig(), pub_key())
```

### 8.4 HMAC (`src/crypto/hmac.grey`)

```grey
fn mac() { hmac_sha256("message", "secret_key") }
hmac_verify_sha256("message", "secret_key", mac())

fn tagged() { hmac_tag("important data", "key") }
hmac_tag_verify(tagged(), "key")  // true
```

### 8.5 KDF (`src/crypto/kdf.grey`)

```grey
fn salt() { generate_salt_16() }
fn derived() { pbkdf2_default("my_password", salt()) }
fn ph() { password_hash("my_password") }
password_verify("my_password", ph())  // true
```

### 8.6 Random (`src/crypto/random.grey`)

```grey
random_int(1, 100)           // 42
random_hex(16)               // "a3f2b1c4..."
random_uuid()                // "550e8400-e29b-..."
random_string(10)            // "aB3xK9mPq2"
random_token(32)             // URL-safe token
random_choice([1, 2, 3])     // 2
random_shuffle([1, 2, 3, 4]) // [3, 1, 4, 2]
dice_roll(6)                 // 1-6
coin_flip()                  // true/false
```

---

## 9. System Module

**Path:** `src/sys/`

### 9.1 Process (`src/sys/process.grey`)

```grey
fn proc() { process_current() }
fn result() { process_exec("echo hello") }
get(result(), "stdout")   // "hello\n"
get(result(), "success")  // true
process_sleep(1000)
```

### 9.2 Environment (`src/sys/env.grey`)

```grey
fn home() { env_get("HOME") }
fn port() { env_get_num("PORT", 3000) }
fn debug() { env_get_bool("DEBUG") }
env_require("DATABASE_URL")  // throws if not set
fn path() { env_path() }     // array of PATH entries
fn config() { env_parse_dotenv("KEY=value\nDB=postgres") }
```

### 9.3 Signal (`src/sys/signal.grey`)

```grey
fn reg() { signal_on(SignalRegistry_new(), SIGINT(), fn(s) { println("caught!") }) }
fn gs() { shutdown_register(GracefulShutdown_new(), fn() { println("cleaning up") }) }
```

### 9.4 System Info (`src/sys/info.grey`)

```grey
fn platform() { sys_platform() }
sys_os()          // "linux", "darwin", "windows"
sys_arch()        // "x64", "arm64"
sys_cpu_count()   // 8
fn mem() { sys_memory() }
format_bytes(get(mem(), "total"))  // "16.00 GB"
```

---

## 10. Module System

**Path:** `src/module/`

### 10.1 Loader (`src/module/loader.grey`)

```grey
fn reg() { ModuleRegistry_new() }
fn desc() { ModuleDescriptor_new("my_lib", "1.0.0", { greet: fn(name) { concat_str("Hi ", name) } }) }
fn r() { registry_register(reg(), "my_lib", desc()) }
fn exports() { module_exports(registry_get(r(), "my_lib")) }
```

### 10.2 Version (`src/module/version.grey`)

```grey
fn v() { version_parse("1.2.3") }
version_to_string(v())              // "1.2.3"
fn v2() { version_bump_minor(v()) } // 1.3.0
version_gt(v2(), v())               // true
version_compatible(v2(), v())        // true (same major)
version_is_stable(v())              // true
```

### 10.3 Metadata (`src/module/metadata.grey`)

```grey
fn meta() { PackageMeta_new("my_pkg", "1.0.0", "A great package") }
fn m() { meta_set_license(meta_set_authors(meta(), ["Alice"]), "MIT") }
fn with_dep() { meta_add_dep(m(), "greystd", "0.1.0") }
fn toml_str() { meta_to_toml(with_dep()) }
```

---

## 11. Error Handling

**Path:** `src/error/`

### 11.1 Error Types (`src/error/types.grey`)

```grey
fn e() { Error_new("something went wrong") }
fn te() { TypeError("expected number") }
fn wrapped() { error_wrap(te(), "in validate()") }
error_format(wrapped())  // "[TypeError] in validate(): expected number"
fn root() { error_root_cause(wrapped()) }  // original TypeError

fn result() { try_run(fn() { risky_operation() }) }
fn safe() { try_or(fn() { parse_int("abc") }, 0) }  // 0
```

### 11.2 Stack Traces (`src/error/trace.grey`)

```grey
fn frame() { StackFrame_new("my_func", "app.grey", 42) }
fn trace() { trace_push(StackTrace_empty(), frame()) }
trace_format(trace())  // "Stack trace:\n  at my_func (app.grey:42)"
```

### 11.3 Assertions (`src/error/assert.grey`)

```grey
assert_true(gt(x, 0), "x must be positive")
assert_eq(result(), 42, "computation result")
assert_contains(items, "apple", "should have apple")
assert_throws(fn() { panic("boom") }, "should throw")

fn checks() { check_all([
  fn() { check(gt(x, 0), "x positive") },
  fn() { check(lt(x, 100), "x under 100") }
]) }
```

### 11.4 Debug (`src/error/debug.grey`)

```grey
debug_print(some_complex_object)   // pretty-printed
debug_log("result", computation())
fn tapped() { tap(value, "step1") }  // logs and passes through
debug_time("factorial", fn() { factorial(1000) })
fn info() { debug_inspect(value) }
```

### 11.5 Logging (`src/error/log.grey`)

```grey
fn logger() { Logger_new("app") }
fn l() { log_info(logger(), "Server started") }
fn l2() { log_error(l(), "Connection failed") }
fn l3() { log_with(l2(), LOG_WARN(), "High latency", { ms: 500, endpoint: "/api" }) }
fn child() { logger_child(logger(), "db") }

// Simple global-style logging
info("Server starting")
warn("Disk space low")
error("Connection refused")
```

---

## 12. Testing Framework

**Path:** `src/test/`

### 12.1 Test Assertions (`src/test/assert.grey`)

Non-panicking assertions that return pass/fail results.

```grey
test_eq(actual, expected, "values match")
test_true(condition, "should be true")
test_contains(array, value, "should contain")
test_throws(fn() { panic("boom") }, "should throw")
test_approx(3.14159, 3.14, 0.01, "close to pi")
```

### 12.2 Test Runner (`src/test/runner.grey`)

```grey
fn suite() { TestSuite_new("My Tests") }
fn s() { suite_test(suite(), "addition", fn() {
  [
    test_eq(add(1, 2), 3, "1+2=3"),
    test_eq(add(0, 0), 0, "0+0=0")
  ]
}) }
fn result() { run_suite(s()) }
print_suite_results(result())
// Prints: [PASS] addition
//   Total: 1 | Passed: 1 | ...
```

### 12.3 Benchmarks (`src/test/bench.grey`)

```grey
fn b() { Benchmark_new("factorial", fn() { factorial(20) }) }
fn b_fast() { bench_iterations(b(), 1000) }
fn result() { run_benchmark(b_fast()) }
print_bench_result(result())
// Prints avg, min, max, median, p95, ops/sec
```

### 12.4 Unit Testing DSL (`src/test/unit.grey`)

```grey
describe("Math", fn() {
  [
    it("adds numbers", fn() { test_eq(add(1, 2), 3, "add") }),
    it("multiplies", fn() { test_eq(mul(3, 4), 12, "mul") }),
    it("factorial", fn() { test_eq(factorial(5), 120, "fact") })
  ]
})

// Or quick style:
run_tests("Quick", [
  it("works", fn() { pass("ok") })
])
```

---

## 13. Reflection

**Path:** `src/reflect/`

### 13.1 Type Metadata (`src/reflect/type_meta.grey`)

```grey
fn tm() { reflect_type(42) }         // TypeMeta for number
fn obj_tm() { reflect_type({ _type: "Widget", name: "btn" }) }
typemeta_name(obj_tm())              // "Widget"
typemeta_fields(obj_tm())            // { name: "string" }
```

### 13.2 Attributes (`src/reflect/attribute.grey`)

```grey
fn attrs() { attrset_add(attrset_add(AttributeSet_new(), attr_doc("A useful function")), attr_deprecated("use v2")) }
attrset_has(attrs(), "deprecated")   // true
fn annotated() { Annotated_new(my_func, attrs()) }
annotated_get_attr(annotated(), "doc")  // "A useful function"
```

### 13.3 Inspection (`src/reflect/inspect.grey`)

```grey
fn info() { inspect({ _type: "Config", port: 8080, debug: true }) }
detailed_type(42)                  // "integer"
detailed_type("hello")             // "string(5)"
fn shape() { inspect_shape(value) }
deep_eq([1, {a: 2}], [1, {a: 2}]) // true
```

### 13.4 Dynamic Invocation (`src/reflect/invoke.grey`)

```grey
fn dt() { dispatch_register(DispatchTable_new(), "greet", fn(name) { concat_str("Hi ", name) }) }
dispatch_call(dt(), "greet", ["Alice"])  // Ok("Hi Alice")

fn add3() { curry(fn(a, b, c) { add(add(a, b), c) }, 3) }
add3()(1)(2)(3)  // 6

fn result() { pipe(5, [fn(x) { mul(x, 2) }, fn(x) { add(x, 1) }]) }  // 11
```

---

## 14. Deterministic Execution

**Path:** `src/deterministic/`

### 14.1 Ordered Collections (`src/deterministic/collections.grey`)

```grey
fn om() { omap_insert(omap_insert(OrderedMap_new(), "b", 2), "a", 1) }
omap_keys(om())       // ["b", "a"] (insertion order)
omap_get(om(), "a")   // 1

fn ss() { sset_insert(sset_insert(SortedSet_default(), 3), 1) }
sset_items(ss())      // [1, 3]
```

### 14.2 Deterministic Scheduler (`src/deterministic/scheduler.grey`)

```grey
fn sched() { DetScheduler_new(42) }
fn s1() { det_schedule(sched(), "task_a", 1, fn() { "result_a" }) }
fn s2() { det_schedule(s1(), "task_b", 2, fn() { "result_b" }) }
fn done() { det_run_all(s2()) }
det_log(done())  // [{ name: "task_b", tick: 1 }, { name: "task_a", tick: 2 }]
```

### 14.3 IO Replay (`src/deterministic/replay_io.grey`)

```grey
// Record
fn rec() { io_record_read(io_record_read(IORecorder_new(), "stdin", "hello"), "file", "content") }
fn json() { io_recording_to_json(rec()) }

// Replay
fn player() { IOPlayer_from(rec()) }
fn r1() { io_replay_read(player()) }
get(r1(), "data")  // "hello"
fn r2() { io_replay_read(get(r1(), "player")) }
get(r2(), "data")  // "content"
```

### 14.4 Pool Allocator (`src/deterministic/allocator.grey`)

```grey
fn pool() { PoolAllocator_new(8) }
fn r() { pool_alloc(pool(), "value_a") }
fn p() { get(r(), "allocator") }
pool_get(p(), get(r(), "handle"))    // "value_a"
pool_allocated(p())                  // 1
fn freed() { pool_free(p(), get(r(), "handle")) }
pool_free_count(freed())             // 8

fn bump() { BumpAllocator_new(100) }
fn b() { bump_alloc(bump(), "data") }
bump_get(get(b(), "allocator"), get(b(), "handle"))
```

---

## Conventions

All GreyStd code follows these Grey++ conventions:

1. **Factory functions**: Types are created via `TypeName_new(args)` returning `{ _type: "TypeName", ... }`
2. **Method functions**: Operations are `type_method(self, args)` — standalone functions taking the object as first parameter
3. **Immutability**: Functions return new objects/arrays; they never mutate in place
4. **No keywords**: No `let`, `var`, `const`, `struct`, `class`, `trait`, `impl`, `enum`, `if`, `else`, `for`, `while`
5. **Control flow via functions**: `if_then(cond, then_fn, else_fn)`, `cond(...)`, `match(...)`, `when(...)`, `unless(...)`
6. **Iteration via HOFs**: `map()`, `filter()`, `reduce()`, `fold()`, `forEach()`, or recursion
7. **Everything is `fn`**: All bindings are `fn name(args) { body }`
