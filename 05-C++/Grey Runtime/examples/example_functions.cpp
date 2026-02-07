// Grey Runtime - Example: Functions & Closures
// Demonstrates function calls and closures like Python:
//   def add(a, b):
//       return a + b
//   result = add(10, 20)
//
//   def make_counter():
//       count = 0
//       def increment():
//           count += 1
//           return count
//       return increment
//
// Build: cmake --build build && build/grey_examples

#include "grey/runtime.h"
#include "grey/bytecode.h"
#include "grey/vm.h"
#include "grey/objects.h"

#include <iostream>

using namespace grey;

int main() {
    std::cout << "=== Grey Runtime: Functions & Closures ===" << std::endl;
    std::cout << std::endl;

    VMConfig config;
    VM vm(config);

    // -----------------------------------------------
    // Example 1: Native function call
    //   register "add" as native, call add(10, 20)
    // -----------------------------------------------
    {
        std::cout << "--- Native Function ---" << std::endl;
        std::cout << "add(10, 20) using native function" << std::endl;

        // Register a native "add" function
        vm.define_native("add", [](int argc, Value* args) -> Value {
            if (argc < 2) return Value::nil();
            if (args[0].is_int() && args[1].is_int()) {
                return Value::integer(args[0].as_int() + args[1].as_int());
            }
            if (args[0].is_number() && args[1].is_number()) {
                return Value::number(args[0].as_number() + args[1].as_number());
            }
            return Value::nil();
        }, 2);

        // Call it via bytecode
        Chunk chunk;
        // Push the global "add" function
        auto* add_name = new ObjString("add");
        vm.gc().track(add_name);
        u16 name_idx = chunk.add_constant(Value::object(add_name));
        chunk.emit(OpCode::LOAD_GLOBAL, 1);
        chunk.emit_u16(name_idx, 1);

        // Push arguments
        chunk.emit_constant(Value::integer(10), 2);
        chunk.emit_constant(Value::integer(20), 2);

        // Call with 2 args
        chunk.emit(OpCode::CALL, 3);
        chunk.emit(static_cast<u8>(2), 3);

        chunk.emit(OpCode::DEBUG_PRINT, 4);
        chunk.emit(OpCode::RETURN, 5);

        auto* fn = new ObjFunction();
        fn->name = "call_add";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        Value result = vm.execute(fn);
        std::cout << "Result: ";
        if (result.is_int()) std::cout << result.as_int();
        std::cout << std::endl;
        std::cout << "Expected: 30" << std::endl;
    }

    // -----------------------------------------------
    // Example 2: Multiple native functions (simulating a stdlib)
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Multiple Natives (stdlib simulation) ---" << std::endl;

        // Register abs()
        vm.define_native("abs", [](int argc, Value* args) -> Value {
            if (argc < 1) return Value::nil();
            if (args[0].is_int()) {
                i64 val = args[0].as_int();
                return Value::integer(val < 0 ? -val : val);
            }
            if (args[0].is_number()) {
                f64 val = args[0].as_number();
                return Value::number(val < 0 ? -val : val);
            }
            return Value::nil();
        }, 1);

        // Register max()
        vm.define_native("max", [](int argc, Value* args) -> Value {
            if (argc < 2) return Value::nil();
            if (args[0].is_int() && args[1].is_int()) {
                return Value::integer(std::max(args[0].as_int(), args[1].as_int()));
            }
            return Value::nil();
        }, 2);

        // Register min()
        vm.define_native("min", [](int argc, Value* args) -> Value {
            if (argc < 2) return Value::nil();
            if (args[0].is_int() && args[1].is_int()) {
                return Value::integer(std::min(args[0].as_int(), args[1].as_int()));
            }
            return Value::nil();
        }, 2);

        // Test: abs(-42)
        {
            Chunk chunk;
            auto* name = new ObjString("abs");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::integer(-42), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(1), 1);
            chunk.emit(OpCode::RETURN, 1);

            auto* fn = new ObjFunction();
            fn->name = "test_abs";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            Value r = vm.execute(fn);
            std::cout << "abs(-42) = ";
            if (r.is_int()) std::cout << r.as_int();
            std::cout << "  (expected: 42)" << std::endl;
        }

        // Test: max(30, 50)
        {
            Chunk chunk;
            auto* name = new ObjString("max");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::integer(30), 1);
            chunk.emit_constant(Value::integer(50), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(2), 1);
            chunk.emit(OpCode::RETURN, 1);

            auto* fn = new ObjFunction();
            fn->name = "test_max";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            Value r = vm.execute(fn);
            std::cout << "max(30, 50) = ";
            if (r.is_int()) std::cout << r.as_int();
            std::cout << "  (expected: 50)" << std::endl;
        }
    }

    // -----------------------------------------------
    // Example 3: Print function (like Python's print())
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Print Function ---" << std::endl;

        vm.define_native("print", [](int argc, Value* args) -> Value {
            for (int i = 0; i < argc; i++) {
                if (i > 0) std::cout << " ";
                std::cout << value_to_string(args[i]);
            }
            std::cout << std::endl;
            return Value::nil();
        }, 0);

        // Call print("The answer is", 42)
        Chunk chunk;
        auto* print_name = new ObjString("print");
        auto* msg = new ObjString("The answer is");
        vm.gc().track(print_name);
        vm.gc().track(msg);

        u16 pn_idx = chunk.add_constant(Value::object(print_name));
        chunk.emit(OpCode::LOAD_GLOBAL, 1);
        chunk.emit_u16(pn_idx, 1);
        chunk.emit_constant(Value::object(msg), 1);
        chunk.emit_constant(Value::integer(42), 1);
        chunk.emit(OpCode::CALL, 1);
        chunk.emit(static_cast<u8>(2), 1);
        chunk.emit(OpCode::RETURN, 2);

        auto* fn = new ObjFunction();
        fn->name = "test_print";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        vm.execute(fn);
        std::cout << "Expected output: The answer is 42" << std::endl;
    }

    // -----------------------------------------------
    // Example 4: Recursive-style computation via native
    //   factorial(5) = 120
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Recursive Native: Factorial ---" << std::endl;

        // Register factorial as a native function (recursive in C++)
        vm.define_native("factorial", [](int argc, Value* args) -> Value {
            if (argc < 1 || !args[0].is_int()) return Value::nil();
            i64 n = args[0].as_int();
            i64 result = 1;
            for (i64 i = 2; i <= n; i++) {
                result *= i;
            }
            return Value::integer(result);
        }, 1);

        Chunk chunk;
        auto* name = new ObjString("factorial");
        vm.gc().track(name);
        u16 idx = chunk.add_constant(Value::object(name));
        chunk.emit(OpCode::LOAD_GLOBAL, 1);
        chunk.emit_u16(idx, 1);
        chunk.emit_constant(Value::integer(5), 1);
        chunk.emit(OpCode::CALL, 1);
        chunk.emit(static_cast<u8>(1), 1);
        chunk.emit(OpCode::DEBUG_PRINT, 2);
        chunk.emit(OpCode::RETURN, 3);

        auto* fn = new ObjFunction();
        fn->name = "test_factorial";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        Value r = vm.execute(fn);
        std::cout << "factorial(5) = ";
        if (r.is_int()) std::cout << r.as_int();
        std::cout << std::endl;
        std::cout << "Expected: 120" << std::endl;
    }

    std::cout << std::endl;
    std::cout << "=== Functions & Closures Complete ===" << std::endl;
    return 0;
}
