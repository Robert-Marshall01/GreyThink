// Grey Runtime - Example: Variables & Control Flow
// Demonstrates variable storage and conditional branching like Python:
//   x = 10
//   y = 20
//   if x < y:
//       result = x + y    # 30
//   else:
//       result = x - y
//
// Build: cmake --build build && build/grey_examples

#include "grey/runtime.h"
#include "grey/bytecode.h"
#include "grey/vm.h"
#include "grey/objects.h"

#include <iostream>

using namespace grey;

int main() {
    std::cout << "=== Grey Runtime: Variables & Control Flow ===" << std::endl;
    std::cout << std::endl;

    VMConfig config;
    VM vm(config);

    // -----------------------------------------------
    // Example 1: Local variables
    // x = 42; y = x + 8; return y  =>  50
    // -----------------------------------------------
    {
        std::cout << "--- Local Variables ---" << std::endl;
        std::cout << "x = 42; y = x + 8; return y" << std::endl;

        Chunk chunk;
        // Local slot 0 = x, slot 1 = y
        // We need to push a nil placeholder for the function call frame
        // Store 42 into local slot 0
        chunk.emit_constant(Value::integer(42), 1);   // push 42
        chunk.emit(OpCode::STORE_LOCAL, 1);            // store to slot
        chunk.emit_u16(0, 1);                          // slot 0 (x)

        // Load x, push 8, add, store to y
        chunk.emit(OpCode::LOAD_LOCAL, 2);             // load slot 0
        chunk.emit_u16(0, 2);
        chunk.emit_constant(Value::integer(8), 2);     // push 8
        chunk.emit(OpCode::ADD, 2);                    // x + 8 = 50

        // Print and return
        chunk.emit(OpCode::DEBUG_PRINT, 3);
        chunk.emit(OpCode::RETURN, 4);

        auto* fn = new ObjFunction();
        fn->name = "variables_demo";
        fn->chunk = chunk;
        fn->arity = 0;
        fn->local_count = 2;
        vm.gc().track(fn);

        Value result = vm.execute(fn);
        std::cout << "Result: ";
        if (result.is_int()) std::cout << result.as_int();
        std::cout << std::endl;
        std::cout << "Expected: 50" << std::endl;
    }

    // -----------------------------------------------
    // Example 2: Conditional (if-else)
    // if 10 < 20: push 100 else: push 200
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- If-Else Branch ---" << std::endl;
        std::cout << "if 10 < 20: result = 100 else: result = 200" << std::endl;

        Chunk chunk;

        // Compare 10 < 20
        chunk.emit_constant(Value::integer(10), 1);
        chunk.emit_constant(Value::integer(20), 1);
        chunk.emit(OpCode::LT, 1);             // push true (10 < 20)

        // JUMP_IF_FALSE to else branch
        u32 jump_pos = chunk.emit(OpCode::JUMP_IF_FALSE, 2);
        u32 jump_arg = static_cast<u32>(chunk.code.size());
        chunk.emit_u16(0, 2);  // placeholder offset

        // Then branch: push 100
        chunk.emit(OpCode::POP, 3);            // pop the condition
        chunk.emit_constant(Value::integer(100), 3);

        // Jump over else
        u32 else_jump_pos = chunk.emit(OpCode::JUMP, 4);
        u32 else_jump_arg = static_cast<u32>(chunk.code.size());
        chunk.emit_u16(0, 4);  // placeholder

        // Else branch target: patch the if-false jump
        u32 else_target = static_cast<u32>(chunk.code.size());
        i16 if_offset = static_cast<i16>(else_target - (jump_arg + 2));
        chunk.code[jump_arg]     = static_cast<u8>((if_offset >> 8) & 0xFF);
        chunk.code[jump_arg + 1] = static_cast<u8>(if_offset & 0xFF);

        // Else: push 200
        chunk.emit(OpCode::POP, 5);
        chunk.emit_constant(Value::integer(200), 5);

        // End: patch else jump
        u32 end_target = static_cast<u32>(chunk.code.size());
        i16 else_offset = static_cast<i16>(end_target - (else_jump_arg + 2));
        chunk.code[else_jump_arg]     = static_cast<u8>((else_offset >> 8) & 0xFF);
        chunk.code[else_jump_arg + 1] = static_cast<u8>(else_offset & 0xFF);

        // Return result
        chunk.emit(OpCode::DEBUG_PRINT, 6);
        chunk.emit(OpCode::RETURN, 7);

        auto* fn = new ObjFunction();
        fn->name = "if_else_demo";
        fn->chunk = chunk;
        fn->arity = 0;
        vm.gc().track(fn);

        Value result = vm.execute(fn);
        std::cout << "Result: ";
        if (result.is_int()) std::cout << result.as_int();
        std::cout << std::endl;
        std::cout << "Expected: 100 (since 10 < 20 is true)" << std::endl;
    }

    // -----------------------------------------------
    // Example 3: Boolean operations
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Boolean Logic ---" << std::endl;

        // Test: not false => true
        std::cout << "not false => ";
        Chunk c1;
        c1.emit(OpCode::PUSH_FALSE, 1);
        c1.emit(OpCode::NOT, 1);
        c1.emit(OpCode::RETURN, 1);
        auto* f1 = new ObjFunction();
        f1->name = "not_false";
        f1->chunk = c1;
        f1->arity = 0;
        vm.gc().track(f1);
        Value r1 = vm.execute(f1);
        std::cout << (r1.is_bool() && r1.as_bool() ? "true" : "false") << std::endl;
        std::cout << "Expected: true" << std::endl;

        // Test: 5 == 5 => true
        std::cout << std::endl;
        std::cout << "5 == 5 => ";
        Chunk c2;
        c2.emit_constant(Value::integer(5), 1);
        c2.emit_constant(Value::integer(5), 1);
        c2.emit(OpCode::EQ, 1);
        c2.emit(OpCode::RETURN, 1);
        auto* f2 = new ObjFunction();
        f2->name = "eq_test";
        f2->chunk = c2;
        f2->arity = 0;
        vm.gc().track(f2);
        Value r2 = vm.execute(f2);
        std::cout << (r2.is_bool() && r2.as_bool() ? "true" : "false") << std::endl;
        std::cout << "Expected: true" << std::endl;

        // Test: 5 != 3 => true
        std::cout << std::endl;
        std::cout << "5 != 3 => ";
        Chunk c3;
        c3.emit_constant(Value::integer(5), 1);
        c3.emit_constant(Value::integer(3), 1);
        c3.emit(OpCode::NEQ, 1);
        c3.emit(OpCode::RETURN, 1);
        auto* f3 = new ObjFunction();
        f3->name = "neq_test";
        f3->chunk = c3;
        f3->arity = 0;
        vm.gc().track(f3);
        Value r3 = vm.execute(f3);
        std::cout << (r3.is_bool() && r3.as_bool() ? "true" : "false") << std::endl;
        std::cout << "Expected: true" << std::endl;
    }

    // -----------------------------------------------
    // Example 4: Stack manipulation (DUP, SWAP, POP)
    // -----------------------------------------------
    {
        std::cout << std::endl;
        std::cout << "--- Stack Operations ---" << std::endl;

        // DUP: push 7, dup, add => 14
        std::cout << "push 7; dup; add => ";
        Chunk c1;
        c1.emit_constant(Value::integer(7), 1);
        c1.emit(OpCode::DUP, 1);
        c1.emit(OpCode::ADD, 1);
        c1.emit(OpCode::RETURN, 1);
        auto* f1 = new ObjFunction();
        f1->name = "dup_test";
        f1->chunk = c1;
        f1->arity = 0;
        vm.gc().track(f1);
        Value r1 = vm.execute(f1);
        if (r1.is_int()) std::cout << r1.as_int();
        std::cout << std::endl;
        std::cout << "Expected: 14" << std::endl;

        // SWAP: push 3, push 5, swap, sub => 5 - 3 = 2
        std::cout << std::endl;
        std::cout << "push 3; push 5; swap; sub => ";
        Chunk c2;
        c2.emit_constant(Value::integer(3), 1);
        c2.emit_constant(Value::integer(5), 1);
        c2.emit(OpCode::SWAP, 1);
        c2.emit(OpCode::SUB, 1);
        c2.emit(OpCode::RETURN, 1);
        auto* f2 = new ObjFunction();
        f2->name = "swap_test";
        f2->chunk = c2;
        f2->arity = 0;
        vm.gc().track(f2);
        Value r2 = vm.execute(f2);
        if (r2.is_int()) std::cout << r2.as_int();
        std::cout << std::endl;
        std::cout << "Expected: 2" << std::endl;
    }

    std::cout << std::endl;
    std::cout << "=== Variables & Control Flow Complete ===" << std::endl;
    return 0;
}
