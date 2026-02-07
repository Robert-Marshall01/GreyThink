// Grey Runtime - Example: Arithmetic Operations
// Demonstrates basic arithmetic like a Python interpreter:
//   result = (10 + 20) * 3 - 15 / 5
//   print(result)  => 87
//
// Build: cmake --build build && build/grey_examples
// Or:    g++ -std=c++20 -Iinclude examples/example_arithmetic.cpp src/vm.cpp src/debug.cpp src/module.cpp src/abi.cpp -o example_arithmetic

#include "grey/runtime.h"
#include "grey/bytecode.h"
#include "grey/vm.h"
#include "grey/objects.h"

#include <iostream>

using namespace grey;

int main() {
    std::cout << "=== Grey Runtime: Arithmetic Example ===" << std::endl;
    std::cout << "Computing: (10 + 20) * 3 - 15 / 5" << std::endl;
    std::cout << std::endl;

    // Create a function with bytecode that computes (10 + 20) * 3 - 15 / 5
    VMConfig config;
    VM vm(config);

    Chunk chunk;

    // --- Step 1: 10 + 20 ---
    chunk.emit_constant(Value::integer(10), 1);  // push 10
    chunk.emit_constant(Value::integer(20), 1);  // push 20
    chunk.emit(OpCode::ADD, 1);                  // 10 + 20 = 30

    // --- Step 2: * 3 ---
    chunk.emit_constant(Value::integer(3), 2);   // push 3
    chunk.emit(OpCode::MUL, 2);                  // 30 * 3 = 90

    // --- Step 3: 15 / 5 ---
    chunk.emit_constant(Value::integer(15), 3);  // push 15
    chunk.emit_constant(Value::integer(5), 3);   // push 5
    chunk.emit(OpCode::DIV, 3);                  // 15 / 5 = 3

    // --- Step 4: subtract ---
    chunk.emit(OpCode::SUB, 4);                  // 90 - 3 = 87

    // --- Step 5: Print result ---
    chunk.emit(OpCode::DEBUG_PRINT, 5);          // print top of stack

    // --- Return ---
    chunk.emit(OpCode::RETURN, 6);

    auto* fn = new ObjFunction();
    fn->name = "arithmetic_demo";
    fn->chunk = chunk;
    fn->arity = 0;
    vm.gc().track(fn);

    Value result = vm.execute(fn);

    std::cout << std::endl;
    std::cout << "Result: ";
    if (result.is_int()) {
        std::cout << result.as_int() << std::endl;
    } else if (result.is_number()) {
        std::cout << result.as_number() << std::endl;
    }
    std::cout << "Expected: 87" << std::endl;

    // --- Also demonstrate floating-point ---
    std::cout << std::endl;
    std::cout << "--- Floating Point ---" << std::endl;
    std::cout << "Computing: 3.14 * 2.0 + 1.5" << std::endl;

    Chunk chunk2;
    chunk2.emit_constant(Value::number(3.14), 1);
    chunk2.emit_constant(Value::number(2.0), 1);
    chunk2.emit(OpCode::MUL, 1);
    chunk2.emit_constant(Value::number(1.5), 2);
    chunk2.emit(OpCode::ADD, 2);
    chunk2.emit(OpCode::DEBUG_PRINT, 3);
    chunk2.emit(OpCode::RETURN, 4);

    auto* fn2 = new ObjFunction();
    fn2->name = "float_demo";
    fn2->chunk = chunk2;
    fn2->arity = 0;
    vm.gc().track(fn2);

    Value result2 = vm.execute(fn2);
    std::cout << "Result: " << result2.as_number() << std::endl;
    std::cout << "Expected: 7.78" << std::endl;

    // --- Demonstrate power and modulo ---
    std::cout << std::endl;
    std::cout << "--- Power & Modulo ---" << std::endl;
    std::cout << "Computing: 2 ** 10 (power)" << std::endl;

    Chunk chunk3;
    chunk3.emit_constant(Value::integer(2), 1);
    chunk3.emit_constant(Value::integer(10), 1);
    chunk3.emit(OpCode::POW, 1);
    chunk3.emit(OpCode::DEBUG_PRINT, 2);
    chunk3.emit(OpCode::RETURN, 3);

    auto* fn3 = new ObjFunction();
    fn3->name = "power_demo";
    fn3->chunk = chunk3;
    fn3->arity = 0;
    vm.gc().track(fn3);

    Value result3 = vm.execute(fn3);
    std::cout << "Result: ";
    if (result3.is_int()) std::cout << result3.as_int();
    else if (result3.is_number()) std::cout << result3.as_number();
    std::cout << std::endl;
    std::cout << "Expected: 1024" << std::endl;

    std::cout << std::endl;
    std::cout << "Computing: 17 % 5 (modulo)" << std::endl;

    Chunk chunk4;
    chunk4.emit_constant(Value::integer(17), 1);
    chunk4.emit_constant(Value::integer(5), 1);
    chunk4.emit(OpCode::MOD, 1);
    chunk4.emit(OpCode::DEBUG_PRINT, 2);
    chunk4.emit(OpCode::RETURN, 3);

    auto* fn4 = new ObjFunction();
    fn4->name = "modulo_demo";
    fn4->chunk = chunk4;
    fn4->arity = 0;
    vm.gc().track(fn4);

    Value result4 = vm.execute(fn4);
    std::cout << "Result: ";
    if (result4.is_int()) std::cout << result4.as_int();
    std::cout << std::endl;
    std::cout << "Expected: 2" << std::endl;

    std::cout << std::endl << "=== Arithmetic Example Complete ===" << std::endl;
    return 0;
}
