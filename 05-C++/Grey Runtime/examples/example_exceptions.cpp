// Grey Runtime - Example: Exception Handling & Error Recovery
// Demonstrates error handling like Python:
//   try:
//       risky_operation()
//   except RuntimeError as e:
//       print("caught:", e)
//   finally:
//       cleanup()
//
// Build: cmake --build build && build/grey_examples

#include "grey/runtime.h"
#include "grey/bytecode.h"
#include "grey/vm.h"
#include "grey/objects.h"

#include <iostream>

using namespace grey;

int main() {
    std::cout << "=== Grey Runtime: Exception Handling ===" << std::endl;
    std::cout << std::endl;

    VMConfig config;
    VM vm(config);

    // -----------------------------------------------
    // Example 1: Native function that throws
    // -----------------------------------------------
    {
        std::cout << "--- Native Exception Throwing ---" << std::endl;

        vm.define_native("divide", [](int argc, Value* args) -> Value {
            if (argc < 2) return Value::nil();
            if (!args[0].is_int() || !args[1].is_int()) {
                throw RuntimeError(ErrorCode::TypeError, "divide expects integers");
            }
            if (args[1].as_int() == 0) {
                throw RuntimeError(ErrorCode::DivisionByZero, "division by zero");
            }
            return Value::integer(args[0].as_int() / args[1].as_int());
        }, 2);

        // Test: divide(10, 2) = 5 (no error)
        {
            Chunk chunk;
            auto* name = new ObjString("divide");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::integer(10), 1);
            chunk.emit_constant(Value::integer(2), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(2), 1);
            chunk.emit(OpCode::RETURN, 2);

            auto* fn = new ObjFunction();
            fn->name = "safe_divide";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            Value result = vm.execute(fn);
            std::cout << "divide(10, 2) = ";
            if (result.is_int()) std::cout << result.as_int();
            std::cout << "  (expected: 5)" << std::endl;
        }

        // Test: divide(10, 0) = DivisionByZero (caught in C++)
        {
            std::cout << std::endl;
            std::cout << "Testing divide(10, 0):" << std::endl;

            Chunk chunk;
            auto* name = new ObjString("divide");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::integer(10), 1);
            chunk.emit_constant(Value::integer(0), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(2), 1);
            chunk.emit(OpCode::RETURN, 2);

            auto* fn = new ObjFunction();
            fn->name = "unsafe_divide";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            try {
                vm.execute(fn);
                std::cout << "  ERROR: should have thrown!" << std::endl;
            } catch (const RuntimeError& e) {
                std::cout << "  Caught RuntimeError: " << e.what() << std::endl;
                std::cout << "  Error code: " << static_cast<int>(e.code) << std::endl;
                std::cout << "  (This is expected behavior!)" << std::endl;
            }
        }
    }

    // -----------------------------------------------
    // Example 2: Type checking / validation
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Type Validation ---" << std::endl;

        vm.define_native("require_string", [](int argc, Value* args) -> Value {
            if (argc < 1) {
                throw RuntimeError(ErrorCode::TypeError, "expected 1 argument");
            }
            if (!args[0].is_object() ||
                args[0].as_object<ObjHeader>()->type != ObjType::String) {
                throw RuntimeError(ErrorCode::TypeError,
                    "expected string, got " + value_to_string(args[0]));
            }
            auto* s = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
            return Value::integer(static_cast<i64>(s->value.length()));
        }, 1);

        // Test with a string
        {
            Chunk chunk;
            auto* name = new ObjString("require_string");
            auto* arg = new ObjString("hello world");
            vm.gc().track(name);
            vm.gc().track(arg);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::object(arg), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(1), 1);
            chunk.emit(OpCode::RETURN, 2);

            auto* fn = new ObjFunction();
            fn->name = "test_string_valid";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            Value r = vm.execute(fn);
            std::cout << "require_string(\"hello world\") = ";
            if (r.is_int()) std::cout << r.as_int();
            std::cout << " (length, expected: 11)" << std::endl;
        }

        // Test with an integer (should throw)
        {
            Chunk chunk;
            auto* name = new ObjString("require_string");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::integer(42), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(1), 1);
            chunk.emit(OpCode::RETURN, 2);

            auto* fn = new ObjFunction();
            fn->name = "test_string_invalid";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            try {
                vm.execute(fn);
                std::cout << "  ERROR: should have thrown!" << std::endl;
            } catch (const RuntimeError& e) {
                std::cout << "require_string(42) => TypeError: " << e.what() << std::endl;
                std::cout << "(Expected behavior)" << std::endl;
            }
        }
    }

    // -----------------------------------------------
    // Example 3: Error codes enumeration
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Error Code System ---" << std::endl;
        std::cout << "Available error codes:" << std::endl;
        std::cout << "  TypeError:        " << static_cast<int>(ErrorCode::TypeError) << std::endl;
        std::cout << "  DivisionByZero:   " << static_cast<int>(ErrorCode::DivisionByZero) << std::endl;
        std::cout << "  StackOverflow:    " << static_cast<int>(ErrorCode::StackOverflow) << std::endl;
        std::cout << "  StackUnderflow:   " << static_cast<int>(ErrorCode::StackUnderflow) << std::endl;
        std::cout << "  InvalidOpcode:    " << static_cast<int>(ErrorCode::InvalidOpcode) << std::endl;
        std::cout << "  RuntimeError:     " << static_cast<int>(ErrorCode::RuntimeError) << std::endl;
        std::cout << "  IOError:          " << static_cast<int>(ErrorCode::IOError) << std::endl;
    }

    std::cout << std::endl;
    std::cout << "=== Exception Handling Complete ===" << std::endl;
    return 0;
}
