// Grey Runtime - Example: Full Program
// Simulates a complete Python-like program using the Grey Runtime:
//
//   # FizzBuzz implementation
//   def fizzbuzz(n):
//       for i in range(1, n+1):
//           if i % 15 == 0: print("FizzBuzz")
//           elif i % 3 == 0: print("Fizz")
//           elif i % 5 == 0: print("Buzz")
//           else: print(i)
//
//   fizzbuzz(20)
//
// This uses native functions to simulate the full program flow.
//
// Build: cmake --build build && build/grey_examples

#include "grey/runtime.h"
#include "grey/bytecode.h"
#include "grey/vm.h"
#include "grey/objects.h"
#include "grey/memory.h"

#include <iostream>

using namespace grey;

int main() {
    std::cout << "=== Grey Runtime: Full Program (FizzBuzz) ===" << std::endl;
    std::cout << std::endl;

    VMConfig config;
    VM vm(config);

    // -----------------------------------------------
    // Register stdlib-like natives
    // -----------------------------------------------

    // print(args...) - prints all args separated by spaces
    vm.define_native("print", [](int argc, Value* args) -> Value {
        for (int i = 0; i < argc; i++) {
            if (i > 0) std::cout << " ";
            std::cout << value_to_string(args[i]);
        }
        std::cout << std::endl;
        return Value::nil();
    }, 0);

    // range(start, end) - returns an array [start, start+1, ..., end-1]
    vm.define_native("range", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_int() || !args[1].is_int()) return Value::nil();
        i64 start = args[0].as_int();
        i64 end = args[1].as_int();
        auto* arr = new ObjArray();
        vm.gc().track(arr);
        for (i64 i = start; i < end; i++) {
            arr->push(Value::integer(i));
        }
        return Value::object(arr);
    }, 2);

    // len(collection) - return length
    vm.define_native("len", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::integer(0);
        auto* hdr = args[0].as_object<ObjHeader>();
        switch (hdr->type) {
            case ObjType::String:
                return Value::integer(static_cast<i64>(
                    static_cast<ObjString*>(hdr)->value.size()));
            case ObjType::Array:
                return Value::integer(static_cast<i64>(
                    static_cast<ObjArray*>(hdr)->elements.size()));
            default:
                return Value::integer(0);
        }
    }, 1);

    // typeof(value) - return type name string
    vm.define_native("typeof", [&vm](int argc, Value* args) -> Value {
        if (argc < 1) return Value::nil();
        std::string type_name;
        if (args[0].is_nil()) type_name = "nil";
        else if (args[0].is_bool()) type_name = "bool";
        else if (args[0].is_int()) type_name = "int";
        else if (args[0].is_number()) type_name = "float";
        else if (args[0].is_object()) {
            auto* hdr = args[0].as_object<ObjHeader>();
            switch (hdr->type) {
                case ObjType::String: type_name = "string"; break;
                case ObjType::Array: type_name = "array"; break;
                case ObjType::Map: type_name = "map"; break;
                case ObjType::Function: type_name = "function"; break;
                case ObjType::Class: type_name = "class"; break;
                case ObjType::Instance: type_name = "instance"; break;
                default: type_name = "object"; break;
            }
        }
        return make_string(vm.gc(), type_name);
    }, 1);

    // -----------------------------------------------
    // FizzBuzz implementation via native
    // (Simulates what compiled bytecode would do)
    // -----------------------------------------------
    {
        std::cout << "--- FizzBuzz (1-20) ---" << std::endl;

        vm.define_native("fizzbuzz", [&vm](int argc, Value* args) -> Value {
            if (argc < 1 || !args[0].is_int()) return Value::nil();
            i64 n = args[0].as_int();
            for (i64 i = 1; i <= n; i++) {
                if (i % 15 == 0)      std::cout << "FizzBuzz" << std::endl;
                else if (i % 3 == 0)  std::cout << "Fizz" << std::endl;
                else if (i % 5 == 0)  std::cout << "Buzz" << std::endl;
                else                  std::cout << i << std::endl;
            }
            return Value::nil();
        }, 1);

        // Call fizzbuzz(20)
        Chunk chunk;
        auto* name = new ObjString("fizzbuzz");
        vm.gc().track(name);
        u16 idx = chunk.add_constant(Value::object(name));
        chunk.emit(OpCode::LOAD_GLOBAL, 1);
        chunk.emit_u16(idx, 1);
        chunk.emit_constant(Value::integer(20), 1);
        chunk.emit(OpCode::CALL, 1);
        chunk.emit(static_cast<u8>(1), 1);
        chunk.emit(OpCode::RETURN, 2);

        auto* fn = new ObjFunction();
        fn->name = "run_fizzbuzz";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        vm.execute(fn);
    }

    // -----------------------------------------------
    // Fibonacci via bytecode (iterative)
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Fibonacci Sequence ---" << std::endl;

        vm.define_native("fibonacci", [](int argc, Value* args) -> Value {
            if (argc < 1 || !args[0].is_int()) return Value::nil();
            i64 n = args[0].as_int();
            if (n <= 0) return Value::integer(0);
            if (n == 1) return Value::integer(1);
            i64 a = 0, b = 1;
            for (i64 i = 2; i <= n; i++) {
                i64 t = a + b;
                a = b;
                b = t;
            }
            return Value::integer(b);
        }, 1);

        // Print fibonacci(0) through fibonacci(10)
        for (int i = 0; i <= 10; i++) {
            Chunk chunk;
            auto* name = new ObjString("fibonacci");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::integer(i), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(1), 1);
            chunk.emit(OpCode::RETURN, 2);

            auto* fn = new ObjFunction();
            fn->name = "fib_call";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            Value r = vm.execute(fn);
            std::cout << "  fib(" << i << ") = ";
            if (r.is_int()) std::cout << r.as_int();
            std::cout << std::endl;
        }
    }

    // -----------------------------------------------
    // Demonstrate type checking
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Type Checking (like Python type()) ---" << std::endl;

        struct TestCase { std::string desc; Value val; };
        TestCase cases[] = {
            {"nil",             Value::nil()},
            {"true",            Value::boolean(true)},
            {"false",           Value::boolean(false)},
            {"42",              Value::integer(42)},
            {"3.14",            Value::number(3.14)},
        };

        // Also test string type
        auto* test_str = new ObjString("hello");
        vm.gc().track(test_str);

        auto* test_arr = new ObjArray();
        vm.gc().track(test_arr);
        test_arr->push(Value::integer(1));

        for (auto& tc : cases) {
            // Call typeof via bytecode
            Chunk chunk;
            auto* name = new ObjString("typeof");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(tc.val, 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(1), 1);
            chunk.emit(OpCode::RETURN, 2);

            auto* fn = new ObjFunction();
            fn->name = "typeof_test";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            Value r = vm.execute(fn);
            std::cout << "  typeof(" << tc.desc << ") = ";
            if (r.is_object() && r.as_object<ObjHeader>()->type == ObjType::String) {
                std::cout << static_cast<ObjString*>(r.as_object<ObjHeader>())->value;
            }
            std::cout << std::endl;
        }

        // Test string type
        {
            Chunk chunk;
            auto* name = new ObjString("typeof");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::object(test_str), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(1), 1);
            chunk.emit(OpCode::RETURN, 2);

            auto* fn = new ObjFunction();
            fn->name = "typeof_str";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            Value r = vm.execute(fn);
            std::cout << "  typeof(\"hello\") = ";
            if (r.is_object() && r.as_object<ObjHeader>()->type == ObjType::String) {
                std::cout << static_cast<ObjString*>(r.as_object<ObjHeader>())->value;
            }
            std::cout << std::endl;
        }

        // Test array type
        {
            Chunk chunk;
            auto* name = new ObjString("typeof");
            vm.gc().track(name);
            u16 idx = chunk.add_constant(Value::object(name));
            chunk.emit(OpCode::LOAD_GLOBAL, 1);
            chunk.emit_u16(idx, 1);
            chunk.emit_constant(Value::object(test_arr), 1);
            chunk.emit(OpCode::CALL, 1);
            chunk.emit(static_cast<u8>(1), 1);
            chunk.emit(OpCode::RETURN, 2);

            auto* fn = new ObjFunction();
            fn->name = "typeof_arr";
            fn->chunk = chunk;
            fn->arity = 0;
            vm.gc().track(fn);

            Value r = vm.execute(fn);
            std::cout << "  typeof([1]) = ";
            if (r.is_object() && r.as_object<ObjHeader>()->type == ObjType::String) {
                std::cout << static_cast<ObjString*>(r.as_object<ObjHeader>())->value;
            }
            std::cout << std::endl;
        }
    }

    // -----------------------------------------------
    // Final stats
    // -----------------------------------------------
    std::cout << std::endl;
    std::cout << "--- Runtime Stats ---" << std::endl;
    std::cout << "Objects alive: " << vm.gc().object_count() << std::endl;
    std::cout << "GC collections: " << vm.gc().collection_count() << std::endl;
    std::cout << "VM cycles executed: " << vm.cycle_count() << std::endl;

    std::cout << std::endl;
    std::cout << "=== Full Program Complete ===" << std::endl;
    return 0;
}
