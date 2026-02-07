// Grey Runtime - Example: String Concatenation & Collections
// Demonstrates strings, arrays, and maps like Python:
//   greeting = "Hello" + " " + "World!"
//   nums = [10, 20, 30]
//   data = {"name": "grey", "version": 1}
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
    std::cout << "=== Grey Runtime: Strings & Collections ===" << std::endl;
    std::cout << std::endl;

    VMConfig config;
    VM vm(config);

    // -----------------------------------------------
    // Example 1: String concatenation
    //   "Hello" + " " + "World!" => "Hello World!"
    // -----------------------------------------------
    {
        std::cout << "--- String Concatenation ---" << std::endl;
        std::cout << R"(Computing: "Hello" + " " + "World!")" << std::endl;

        Chunk chunk;

        // Create string constants via the GC
        auto* s1 = new ObjString("Hello");
        auto* s2 = new ObjString(" ");
        auto* s3 = new ObjString("World!");
        vm.gc().track(s1);
        vm.gc().track(s2);
        vm.gc().track(s3);

        chunk.emit_constant(Value::object(s1), 1);   // push "Hello"
        chunk.emit_constant(Value::object(s2), 1);   // push " "
        chunk.emit(OpCode::ADD, 1);                   // "Hello" + " " => "Hello "
        chunk.emit_constant(Value::object(s3), 2);   // push "World!"
        chunk.emit(OpCode::ADD, 2);                   // "Hello " + "World!" => "Hello World!"

        chunk.emit(OpCode::DEBUG_PRINT, 3);
        chunk.emit(OpCode::RETURN, 4);

        auto* fn = new ObjFunction();
        fn->name = "string_concat";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        Value result = vm.execute(fn);
        std::cout << "Result: ";
        if (result.is_object()) {
            auto* hdr = result.as_object<ObjHeader>();
            if (hdr && hdr->type == ObjType::String) {
                std::cout << "\"" << static_cast<ObjString*>(hdr)->value << "\"";
            }
        }
        std::cout << std::endl;
        std::cout << R"(Expected: "Hello World!")" << std::endl;
    }

    // -----------------------------------------------
    // Example 2: Array creation and access
    //   arr = [10, 20, 30]
    //   return arr[1]  => 20
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Array Creation ---" << std::endl;
        std::cout << "arr = [10, 20, 30]; return arr[1]" << std::endl;

        Chunk chunk;

        // Push elements onto stack, then create array
        chunk.emit_constant(Value::integer(10), 1);
        chunk.emit_constant(Value::integer(20), 1);
        chunk.emit_constant(Value::integer(30), 1);
        chunk.emit(OpCode::NEW_ARRAY, 2);  // create array from top 3 stack items
        chunk.emit_u16(3, 2);

        // Index into the array: arr[1]
        chunk.emit_constant(Value::integer(1), 3);
        chunk.emit(OpCode::LOAD_INDEX, 3);

        chunk.emit(OpCode::DEBUG_PRINT, 4);
        chunk.emit(OpCode::RETURN, 5);

        auto* fn = new ObjFunction();
        fn->name = "array_demo";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        Value result = vm.execute(fn);
        std::cout << "Result: ";
        if (result.is_int()) std::cout << result.as_int();
        std::cout << std::endl;
        std::cout << "Expected: 20" << std::endl;
    }

    // -----------------------------------------------
    // Example 3: Map creation
    //   m = {"x": 100, "y": 200}
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Map Creation ---" << std::endl;
        std::cout << R"(m = {"x": 100, "y": 200})" << std::endl;

        Chunk chunk;

        auto* key_x = new ObjString("x");
        auto* key_y = new ObjString("y");
        vm.gc().track(key_x);
        vm.gc().track(key_y);

        // Push key-value pairs
        chunk.emit_constant(Value::object(key_x), 1);
        chunk.emit_constant(Value::integer(100), 1);
        chunk.emit_constant(Value::object(key_y), 2);
        chunk.emit_constant(Value::integer(200), 2);
        chunk.emit(OpCode::NEW_MAP, 3);  // create map with 2 entries
        chunk.emit_u16(2, 3);

        // Access m["x"]
        chunk.emit(OpCode::DUP, 4);  // keep the map
        chunk.emit_constant(Value::object(key_x), 4);
        chunk.emit(OpCode::LOAD_INDEX, 4);

        chunk.emit(OpCode::DEBUG_PRINT, 5);

        // Pop the debug_print result, return the index load result
        chunk.emit(OpCode::RETURN, 6);

        auto* fn = new ObjFunction();
        fn->name = "map_demo";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        Value result = vm.execute(fn);
        std::cout << "Result: ";
        if (result.is_int()) std::cout << result.as_int();
        std::cout << std::endl;
        std::cout << R"(Expected: 100 (value of m["x"]))" << std::endl;
    }

    // -----------------------------------------------
    // Example 4: Demonstrate GC tracking
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Garbage Collector Status ---" << std::endl;
        std::cout << "Objects tracked: " << vm.gc().object_count() << std::endl;
        std::cout << "Collections run: " << vm.gc().collection_count() << std::endl;
        vm.gc().collect();
        std::cout << "After manual GC: " << vm.gc().object_count() << " objects" << std::endl;
    }

    // -----------------------------------------------
    // Example 5: Value type system
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Value Type System ---" << std::endl;

        Value nil_val = Value::nil();
        Value bool_val = Value::boolean(true);
        Value int_val = Value::integer(42);
        Value num_val = Value::number(3.14);

        std::cout << "nil    is_nil=" << nil_val.is_nil()
                  << " is_bool=" << nil_val.is_bool()
                  << " is_int=" << nil_val.is_int()
                  << " is_number=" << nil_val.is_number() << std::endl;

        std::cout << "true   is_nil=" << bool_val.is_nil()
                  << " is_bool=" << bool_val.is_bool()
                  << " is_int=" << bool_val.is_int()
                  << " is_number=" << bool_val.is_number() << std::endl;

        std::cout << "42     is_nil=" << int_val.is_nil()
                  << " is_bool=" << int_val.is_bool()
                  << " is_int=" << int_val.is_int()
                  << " is_number=" << int_val.is_number() << std::endl;

        std::cout << "3.14   is_nil=" << num_val.is_nil()
                  << " is_bool=" << num_val.is_bool()
                  << " is_int=" << num_val.is_int()
                  << " is_number=" << num_val.is_number() << std::endl;

        std::cout << std::endl;
        std::cout << "Truthiness:" << std::endl;
        std::cout << "  nil  => " << (nil_val.is_truthy() ? "truthy" : "falsy") << std::endl;
        std::cout << "  false => " << (Value::boolean(false).is_truthy() ? "truthy" : "falsy") << std::endl;
        std::cout << "  true  => " << (Value::boolean(true).is_truthy() ? "truthy" : "falsy") << std::endl;
        std::cout << "  0     => " << (Value::integer(0).is_truthy() ? "truthy" : "falsy") << std::endl;
        std::cout << "  42    => " << (Value::integer(42).is_truthy() ? "truthy" : "falsy") << std::endl;
    }

    std::cout << std::endl;
    std::cout << "=== Strings & Collections Complete ===" << std::endl;
    return 0;
}
