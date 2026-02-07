// Grey Runtime - Common Types and Definitions
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include <cstdint>
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>
#include <memory>
#include <functional>
#include <optional>
#include <variant>
#include <unordered_map>
#include <cassert>
#include <stdexcept>
#include <iostream>
#include <sstream>
#include <mutex>
#include <atomic>
#include <chrono>

namespace grey {

// ----- Fundamental Types -----
using u8  = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using i8  = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using f32 = float;
using f64 = double;
using byte = uint8_t;

// ----- Value Representation -----
// NaN-boxed value: uses IEEE 754 double NaN space to encode types
// Quiet NaN bits: 0x7FF8000000000000
// We use the remaining bits for type tags and payloads

static constexpr u64 QNAN        = 0x7FF8000000000000ULL;
static constexpr u64 TAG_NIL     = 0x0001000000000000ULL;
static constexpr u64 TAG_FALSE   = 0x0002000000000000ULL;
static constexpr u64 TAG_TRUE    = 0x0003000000000000ULL;
static constexpr u64 TAG_INT     = 0x0004000000000000ULL;
static constexpr u64 TAG_OBJ     = 0x8000000000000000ULL; // sign bit

union Value {
    u64 bits;
    f64 as_f64;

    Value() : bits(QNAN | TAG_NIL) {}
    explicit Value(f64 v) : as_f64(v) {}
    explicit Value(u64 raw_bits) : bits(raw_bits) {}

    static Value nil()           { Value v; v.bits = QNAN | TAG_NIL; return v; }
    static Value boolean(bool b) { Value v; v.bits = QNAN | (b ? TAG_TRUE : TAG_FALSE); return v; }
    static Value integer(i64 i)  { Value v; v.bits = QNAN | TAG_INT | (static_cast<u64>(i) & 0x0000FFFFFFFFFFFFULL); return v; }
    static Value number(f64 d)   { Value v; v.as_f64 = d; return v; }
    static Value object(void* p) {
        Value v;
        v.bits = QNAN | TAG_OBJ | reinterpret_cast<u64>(p);
        return v;
    }

    bool is_nil()     const { return bits == (QNAN | TAG_NIL); }
    bool is_bool()    const { return bits == (QNAN | TAG_TRUE) || bits == (QNAN | TAG_FALSE); }
    bool is_boolean()  const { return is_bool(); }
    bool is_true()    const { return bits == (QNAN | TAG_TRUE); }
    bool is_false()   const { return bits == (QNAN | TAG_FALSE); }
    bool is_number()  const { return (bits & QNAN) != QNAN; }
    bool is_int()     const { return (bits & (QNAN | TAG_INT)) == (QNAN | TAG_INT) && !is_object(); }
    bool is_object()  const { return (bits & (QNAN | TAG_OBJ)) == (QNAN | TAG_OBJ); }

    f64 as_number() const { return as_f64; }
    bool as_bool()  const { return is_true(); }
    bool as_boolean() const { return as_bool(); }
    i64  as_int()   const {
        u64 raw = bits & 0x0000FFFFFFFFFFFFULL;
        if (raw & 0x0000800000000000ULL)  // sign bit (bit 47)
            raw |= 0xFFFF000000000000ULL;  // sign-extend to 64 bits
        return static_cast<i64>(raw);
    }
    template<typename T = void>
    T* as_object()  const { return reinterpret_cast<T*>(bits & 0x0000FFFFFFFFFFFFULL); }

    bool is_truthy() const {
        if (is_nil() || is_false()) return false;
        if (is_true()) return true;
        if (is_number()) return as_f64 != 0.0;
        if (is_int()) return as_int() != 0;
        return true; // objects are truthy
    }

    bool operator==(const Value& other) const { return bits == other.bits; }
    bool operator!=(const Value& other) const { return bits != other.bits; }
};

// ----- Object Type Tags -----
enum class ObjType : u8 {
    String,
    Array,
    Map,
    Set,
    Function,
    Closure,
    NativeFunction,
    Class,
    Instance,
    Module,
    Fiber,
    Future,
    Channel,
    Buffer,
    Iterator,
    Upvalue,
    BoundMethod,
};

// ----- Base Object Header -----
struct ObjHeader {
    ObjType type;
    bool    is_marked = false; // GC mark bit
    u32     hash      = 0;
    ObjHeader* next   = nullptr; // intrusive linked list for GC

    ObjHeader(ObjType t) : type(t) {}
    virtual ~ObjHeader() = default;
};

// ----- Error/Exception Types -----
enum class ErrorCode : u32 {
    None = 0,
    RuntimeError,
    TypeError,
    DivisionByZero,
    StackOverflow,
    StackUnderflow,
    OutOfMemory,
    IndexOutOfBounds,
    KeyNotFound,
    NullReference,
    InvalidOpcode,
    ModuleNotFound,
    ImportError,
    IOError,
    NetworkError,
    TimeoutError,
    PermissionDenied,
    SandboxViolation,
    DeterminismViolation,
    AssertionFailed,
};

struct RuntimeError : std::runtime_error {
    ErrorCode code;
    std::string detail;
    std::vector<std::string> stack_trace;

    RuntimeError(ErrorCode c, const std::string& msg)
        : std::runtime_error(msg), code(c), detail(msg) {}
};

// ----- Result Type -----
template<typename T>
struct Result {
    std::optional<T> value;
    std::optional<RuntimeError> error;

    static Result ok(T val) { Result r; r.value = std::move(val); return r; }
    static Result err(ErrorCode code, const std::string& msg) {
        Result r;
        r.error = RuntimeError(code, msg);
        return r;
    }

    bool is_ok() const { return value.has_value(); }
    bool is_err() const { return error.has_value(); }
    T& unwrap() { return *value; }
    const T& unwrap() const { return *value; }
};

// ----- Logging -----
enum class LogLevel : u8 { Trace, Debug, Info, Warn, Error, Fatal };

inline void log(LogLevel level, const std::string& msg) {
    static const char* level_names[] = { "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL" };
    std::cerr << "[Grey:" << level_names[static_cast<int>(level)] << "] " << msg << "\n";
}

} // namespace grey
