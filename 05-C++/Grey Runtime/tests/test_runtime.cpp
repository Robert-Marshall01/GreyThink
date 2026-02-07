// Grey Runtime - Test Suite
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/common.h"
#include "grey/bytecode.h"
#include "grey/memory.h"
#include "grey/objects.h"
#include "grey/vm.h"
#include "grey/abi.h"
#include "grey/runtime.h"
#include "grey/stdlib/crypto.h"

#include <iostream>
#include <cassert>
#include <cstring>
#include <cmath>
#include <string>
#include <vector>

using namespace grey;

static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) static void test_##name()
#define RUN_TEST(name) do { \
    std::cout << "  " << #name << "... "; \
    try { test_##name(); tests_passed++; std::cout << "PASS" << std::endl; } \
    catch (const std::exception& e) { tests_failed++; std::cout << "FAIL: " << e.what() << std::endl; } \
    catch (...) { tests_failed++; std::cout << "FAIL: unknown exception" << std::endl; } \
} while(0)

#define ASSERT_TRUE(expr) do { if (!(expr)) throw std::runtime_error("Assertion failed: " #expr); } while(0)
#define ASSERT_FALSE(expr) ASSERT_TRUE(!(expr))
#define ASSERT_EQ(a, b) do { if ((a) != (b)) throw std::runtime_error("Assertion failed: " #a " == " #b); } while(0)

// ============================================================
// Value Tests
// ============================================================

TEST(value_nil) {
    Value v = Value::nil();
    ASSERT_TRUE(v.is_nil());
    ASSERT_FALSE(v.is_boolean());
    ASSERT_FALSE(v.is_int());
    ASSERT_FALSE(v.is_number());
    ASSERT_FALSE(v.is_object());
}

TEST(value_boolean) {
    Value t = Value::boolean(true);
    Value f = Value::boolean(false);
    ASSERT_TRUE(t.is_boolean());
    ASSERT_TRUE(f.is_boolean());
    ASSERT_TRUE(t.as_boolean());
    ASSERT_FALSE(f.as_boolean());
    ASSERT_FALSE(t.is_nil());
}

TEST(value_integer) {
    Value v = Value::integer(42);
    ASSERT_TRUE(v.is_int());
    ASSERT_EQ(v.as_int(), 42);

    Value neg = Value::integer(-100);
    ASSERT_EQ(neg.as_int(), -100);

    Value zero = Value::integer(0);
    ASSERT_EQ(zero.as_int(), 0);
}

TEST(value_number) {
    Value v = Value::number(3.14);
    ASSERT_TRUE(v.is_number());
    ASSERT_TRUE(std::abs(v.as_number() - 3.14) < 1e-10);
}

// ============================================================
// Chunk / Bytecode Tests
// ============================================================

TEST(chunk_emit) {
    Chunk chunk;
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    ASSERT_EQ(chunk.code.size(), 1u);
    ASSERT_EQ(chunk.code[0], static_cast<u8>(OpCode::RETURN));
}

TEST(chunk_constants) {
    Chunk chunk;
    u8 idx = chunk.add_constant(Value::integer(42));
    ASSERT_EQ(idx, 0);
    ASSERT_EQ(chunk.constants.size(), 1u);
    ASSERT_TRUE(chunk.constants[0].is_int());
    ASSERT_EQ(chunk.constants[0].as_int(), 42);
}

TEST(chunk_emit_constant) {
    Chunk chunk;
    chunk.emit_constant(Value::number(2.718), 1);
    // Should emit PUSH_CONST + u16 index (3 bytes total)
    ASSERT_EQ(chunk.code.size(), 3u);
    ASSERT_EQ(chunk.code[0], static_cast<u8>(OpCode::PUSH_CONST));
    ASSERT_EQ(chunk.code[1], 0); // high byte of index
    ASSERT_EQ(chunk.code[2], 0); // low byte of index
}

TEST(chunk_line_info) {
    Chunk chunk;
    chunk.emit(static_cast<u8>(OpCode::PUSH_NIL), 10);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 10);
    ASSERT_EQ(chunk.get_line(0), 10u);
    ASSERT_EQ(chunk.get_line(1), 10u);
}

TEST(opcode_names) {
    ASSERT_EQ(std::string(opcode_name(OpCode::RETURN)), std::string("RETURN"));
    ASSERT_EQ(std::string(opcode_name(OpCode::PUSH_CONST)), std::string("PUSH_CONST"));
    ASSERT_EQ(std::string(opcode_name(OpCode::ADD)), std::string("ADD"));
    ASSERT_EQ(std::string(opcode_name(OpCode::HALT)), std::string("HALT"));
}

// ============================================================
// Memory Tests
// ============================================================

TEST(bump_allocator) {
    BumpAllocator bump(1024);
    void* p1 = bump.allocate(64);
    ASSERT_TRUE(p1 != nullptr);
    void* p2 = bump.allocate(64);
    ASSERT_TRUE(p2 != nullptr);
    ASSERT_TRUE(p2 != p1);
    bump.reset();
    void* p3 = bump.allocate(64);
    ASSERT_TRUE(p3 != nullptr);
}

TEST(arena_allocator) {
    ArenaAllocator arena(256);
    for (int i = 0; i < 100; i++) {
        void* p = arena.allocate(32);
        ASSERT_TRUE(p != nullptr);
    }
    arena.reset();
}

TEST(value_stack) {
    ValueStack stack;
    stack.push(Value::integer(1));
    stack.push(Value::integer(2));
    stack.push(Value::integer(3));
    ASSERT_EQ(stack.size(), 3u);
    ASSERT_EQ(stack.pop().as_int(), 3);
    ASSERT_EQ(stack.peek(0).as_int(), 2);
    ASSERT_EQ(stack.size(), 2u);
}

TEST(garbage_collector_basic) {
    GarbageCollector gc;
    auto* str = new ObjString("hello");
    gc.track(str);
    ASSERT_EQ(gc.object_count(), 1u);
    // Without marking, collection should sweep
    gc.collect([](auto) {});
}

// ============================================================
// Object Tests
// ============================================================

TEST(string_object) {
    ObjString str("hello");
    ASSERT_EQ(str.type, ObjType::String);
    ASSERT_EQ(str.value, "hello");
    ASSERT_TRUE(str.hash != 0);

    ObjString str2("hello");
    ASSERT_EQ(str.hash, str2.hash);

    ObjString str3("world");
    ASSERT_TRUE(str.hash != str3.hash);
}

TEST(array_object) {
    ObjArray arr;
    ASSERT_EQ(arr.type, ObjType::Array);
    arr.elements.push_back(Value::integer(1));
    arr.elements.push_back(Value::integer(2));
    ASSERT_EQ(arr.elements.size(), 2u);
}

TEST(map_object) {
    ObjMap map;
    ASSERT_EQ(map.type, ObjType::Map);
    map.set(Value::integer(1), Value::integer(100));
    auto result = map.get(Value::integer(1));
    ASSERT_TRUE(result.has_value());
}

TEST(buffer_object) {
    ObjBuffer buf(64);
    ASSERT_EQ(buf.type, ObjType::Buffer);
    ASSERT_EQ(buf.data.size(), 64u);
    buf.data[0] = 0xAB;
    ASSERT_EQ(buf.data[0], 0xAB);
}

// ============================================================
// VM Execution Tests
// ============================================================

// Helper: create a simple VM and execute a chunk
static Value run_chunk(Chunk& chunk) {
    VMConfig config;
    VM vm(config);
    auto* fn = new ObjFunction();
    fn->chunk = chunk;
    fn->name = "<test>";
    vm.gc().track(fn);
    return vm.execute(fn);
}

TEST(vm_return_nil) {
    Chunk chunk;
    chunk.emit(static_cast<u8>(OpCode::PUSH_NIL), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_nil());
}

TEST(vm_return_constant) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(42), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_int());
    ASSERT_EQ(result.as_int(), 42);
}

TEST(vm_arithmetic_add) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(10), 1);
    chunk.emit_constant(Value::integer(20), 1);
    chunk.emit(static_cast<u8>(OpCode::ADD), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_int());
    ASSERT_EQ(result.as_int(), 30);
}

TEST(vm_arithmetic_sub) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(50), 1);
    chunk.emit_constant(Value::integer(20), 1);
    chunk.emit(static_cast<u8>(OpCode::SUB), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_int());
    ASSERT_EQ(result.as_int(), 30);
}

TEST(vm_arithmetic_mul) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(6), 1);
    chunk.emit_constant(Value::integer(7), 1);
    chunk.emit(static_cast<u8>(OpCode::MUL), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_int());
    ASSERT_EQ(result.as_int(), 42);
}

TEST(vm_arithmetic_div) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(100), 1);
    chunk.emit_constant(Value::integer(4), 1);
    chunk.emit(static_cast<u8>(OpCode::DIV), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_int());
    ASSERT_EQ(result.as_int(), 25);
}

TEST(vm_arithmetic_negate) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(42), 1);
    chunk.emit(static_cast<u8>(OpCode::NEG), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_int());
    ASSERT_EQ(result.as_int(), -42);
}

TEST(vm_boolean_ops) {
    // NOT true = false
    Chunk chunk;
    chunk.emit(static_cast<u8>(OpCode::PUSH_TRUE), 1);
    chunk.emit(static_cast<u8>(OpCode::NOT), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_boolean());
    ASSERT_FALSE(result.as_boolean());
}

TEST(vm_comparison_equal) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(42), 1);
    chunk.emit_constant(Value::integer(42), 1);
    chunk.emit(static_cast<u8>(OpCode::EQ), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_boolean());
    ASSERT_TRUE(result.as_boolean());
}

TEST(vm_comparison_less) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(10), 1);
    chunk.emit_constant(Value::integer(20), 1);
    chunk.emit(static_cast<u8>(OpCode::LT), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_boolean());
    ASSERT_TRUE(result.as_boolean());
}

TEST(vm_pop) {
    Chunk chunk;
    chunk.emit_constant(Value::integer(1), 1);
    chunk.emit_constant(Value::integer(2), 1);
    chunk.emit(static_cast<u8>(OpCode::POP), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_int());
    ASSERT_EQ(result.as_int(), 1);
}

TEST(vm_float_arithmetic) {
    Chunk chunk;
    chunk.emit_constant(Value::number(1.5), 1);
    chunk.emit_constant(Value::number(2.5), 1);
    chunk.emit(static_cast<u8>(OpCode::ADD), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);
    Value result = run_chunk(chunk);
    ASSERT_TRUE(result.is_number());
    ASSERT_TRUE(std::abs(result.as_number() - 4.0) < 1e-10);
}

// ============================================================
// ABI / Module Writer/Reader Tests
// ============================================================

TEST(abi_info) {
    auto abi = current_abi();
    ASSERT_EQ(abi.version_major, GREY_VERSION_MAJOR);
    ASSERT_EQ(abi.version_minor, GREY_VERSION_MINOR);
    ASSERT_TRUE(abi.pointer_size == 4 || abi.pointer_size == 8);
    ASSERT_TRUE(abi.is_compatible(abi));
}

TEST(module_writer_reader) {
    ModuleWriter writer;
    writer.begin();

    // Add strings
    writer.add_string("main");
    writer.add_string("hello");

    // Create a simple function
    Chunk chunk;
    chunk.emit_constant(Value::integer(99), 1);
    chunk.emit(static_cast<u8>(OpCode::RETURN), 1);

    FunctionEntry fn;
    fn.name_index = 0;
    fn.arity = 0;
    fn.local_count = 0;
    fn.upvalue_count = 0;
    fn.code_offset = 0;
    fn.code_size = 0;
    fn.constant_offset = 0;
    fn.constant_count = 0;
    fn.cc = CallingConvention::GreyDefault;
    writer.add_function(fn, chunk);

    auto data = writer.finish();
    ASSERT_TRUE(data.size() > sizeof(ModuleHeader));

    // Read it back
    ModuleReader reader(data);
    ASSERT_TRUE(reader.validate());

    auto hdr = reader.header();
    ASSERT_EQ(hdr.magic, GREY_MAGIC);
    ASSERT_EQ(hdr.version_major, GREY_VERSION_MAJOR);
    ASSERT_EQ(hdr.function_count, 1u);
    ASSERT_EQ(hdr.string_count, 2u);

    auto fns = reader.functions();
    ASSERT_EQ(fns.size(), 1u);
    ASSERT_EQ(fns[0].arity, 0);

    std::string name = reader.get_string(0);
    ASSERT_EQ(name, "main");
}

// ============================================================
// Crypto Tests
// ============================================================

TEST(sha256_empty) {
    auto hash = crypto::sha256_hex("");
    // SHA-256 of empty string
    ASSERT_EQ(hash, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
}

TEST(sha256_hello) {
    auto hash = crypto::sha256_hex("hello");
    ASSERT_EQ(hash, "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824");
}

TEST(hmac_sha256_basic) {
    std::string key = "key";
    std::string data = "data";
    auto mac = crypto::hmac_sha256(
        reinterpret_cast<const byte*>(key.data()), key.size(),
        reinterpret_cast<const byte*>(data.data()), data.size());
    auto hex = crypto::bytes_to_hex(mac.data(), 32);
    ASSERT_TRUE(hex.size() == 64); // 32 bytes = 64 hex chars
}

TEST(hex_roundtrip) {
    std::vector<byte> original = {0xDE, 0xAD, 0xBE, 0xEF};
    auto hex = crypto::bytes_to_hex(original.data(), original.size());
    ASSERT_EQ(hex, "deadbeef");
    auto decoded = crypto::hex_to_bytes(hex);
    ASSERT_EQ(decoded.size(), original.size());
    for (size_t i = 0; i < original.size(); i++) {
        ASSERT_EQ(decoded[i], original[i]);
    }
}

TEST(random_bytes_length) {
    auto bytes = crypto::random_bytes(32);
    ASSERT_EQ(bytes.size(), 32u);
    auto bytes2 = crypto::random_bytes(64);
    ASSERT_EQ(bytes2.size(), 64u);
}

// ============================================================
// Runtime Integration Tests
// ============================================================

TEST(runtime_init_shutdown) {
    Runtime runtime;
    Runtime::Config config;
    runtime.initialize(config);
    runtime.shutdown();
}

TEST(runtime_version) {
    std::string ver = Runtime::version();
    ASSERT_FALSE(ver.empty());
}

// ============================================================
// Test Runner
// ============================================================

int main() {
    std::cout << "========================================" << std::endl;
    std::cout << "  Grey Runtime Test Suite" << std::endl;
    std::cout << "========================================" << std::endl;
    std::cout << std::endl;

    // Value tests
    std::cout << "[Value Tests]" << std::endl;
    RUN_TEST(value_nil);
    RUN_TEST(value_boolean);
    RUN_TEST(value_integer);
    RUN_TEST(value_number);
    std::cout << std::endl;

    // Bytecode tests
    std::cout << "[Bytecode Tests]" << std::endl;
    RUN_TEST(chunk_emit);
    RUN_TEST(chunk_constants);
    RUN_TEST(chunk_emit_constant);
    RUN_TEST(chunk_line_info);
    RUN_TEST(opcode_names);
    std::cout << std::endl;

    // Memory tests
    std::cout << "[Memory Tests]" << std::endl;
    RUN_TEST(bump_allocator);
    RUN_TEST(arena_allocator);
    RUN_TEST(value_stack);
    RUN_TEST(garbage_collector_basic);
    std::cout << std::endl;

    // Object tests
    std::cout << "[Object Tests]" << std::endl;
    RUN_TEST(string_object);
    RUN_TEST(array_object);
    RUN_TEST(map_object);
    RUN_TEST(buffer_object);
    std::cout << std::endl;

    // VM tests
    std::cout << "[VM Execution Tests]" << std::endl;
    RUN_TEST(vm_return_nil);
    RUN_TEST(vm_return_constant);
    RUN_TEST(vm_arithmetic_add);
    RUN_TEST(vm_arithmetic_sub);
    RUN_TEST(vm_arithmetic_mul);
    RUN_TEST(vm_arithmetic_div);
    RUN_TEST(vm_arithmetic_negate);
    RUN_TEST(vm_boolean_ops);
    RUN_TEST(vm_comparison_equal);
    RUN_TEST(vm_comparison_less);
    RUN_TEST(vm_pop);
    RUN_TEST(vm_float_arithmetic);
    std::cout << std::endl;

    // ABI tests
    std::cout << "[ABI Tests]" << std::endl;
    RUN_TEST(abi_info);
    RUN_TEST(module_writer_reader);
    std::cout << std::endl;

    // Crypto tests
    std::cout << "[Crypto Tests]" << std::endl;
    RUN_TEST(sha256_empty);
    RUN_TEST(sha256_hello);
    RUN_TEST(hmac_sha256_basic);
    RUN_TEST(hex_roundtrip);
    RUN_TEST(random_bytes_length);
    std::cout << std::endl;

    // Runtime tests
    std::cout << "[Runtime Integration Tests]" << std::endl;
    RUN_TEST(runtime_init_shutdown);
    RUN_TEST(runtime_version);
    std::cout << std::endl;

    // Summary
    std::cout << "========================================" << std::endl;
    std::cout << "  Results: " << tests_passed << " passed, "
              << tests_failed << " failed, "
              << (tests_passed + tests_failed) << " total" << std::endl;
    std::cout << "========================================" << std::endl;

    return tests_failed > 0 ? 1 : 0;
}
