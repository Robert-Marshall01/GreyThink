// Grey Runtime - JIT Compiler Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/jit.h"

#ifdef GREY_PLATFORM_WINDOWS
    #include <windows.h>
#else
    #include <sys/mman.h>
#endif

namespace grey {

// ============================================================
// JIT Compiler
// ============================================================

JITCompiler::~JITCompiler() {
    // Free all compiled code blocks
    for (auto& [_, code] : compiled_) {
        if (code.code_ptr) {
#ifdef GREY_PLATFORM_WINDOWS
            VirtualFree(code.code_ptr, 0, MEM_RELEASE);
#else
            munmap(code.code_ptr, code.code_size);
#endif
        }
    }
}

// ============================================================
// Bytecode → JIT IR Lowering
// ============================================================

JITFunction JITCompiler::lower_to_ir(ObjFunction* fn) {
    JITFunction jit_fn;
    jit_fn.name = fn->name;
    jit_fn.num_params = fn->arity;

    Chunk& chunk = fn->chunk;
    JITBlock block;
    block.id = 0;

    u32 ip = 0;
    u32 reg = fn->arity; // registers start after params

    while (ip < chunk.code.size()) {
        OpCode op = static_cast<OpCode>(chunk.code[ip++]);

        switch (op) {
            case OpCode::PUSH_CONST: {
                u16 idx = chunk.read_u16(ip);
                JITInstruction inst;
                inst.op = JITOpKind::LoadConst;
                inst.dest = reg++;
                inst.imm = idx;
                block.instructions.push_back(inst);
                break;
            }
            case OpCode::ADD: {
                JITInstruction inst;
                inst.op = JITOpKind::Add;
                inst.dest = reg - 2;
                inst.src1 = reg - 2;
                inst.src2 = reg - 1;
                reg--;
                block.instructions.push_back(inst);
                break;
            }
            case OpCode::SUB: {
                JITInstruction inst;
                inst.op = JITOpKind::Sub;
                inst.dest = reg - 2;
                inst.src1 = reg - 2;
                inst.src2 = reg - 1;
                reg--;
                block.instructions.push_back(inst);
                break;
            }
            case OpCode::MUL: {
                JITInstruction inst;
                inst.op = JITOpKind::Mul;
                inst.dest = reg - 2;
                inst.src1 = reg - 2;
                inst.src2 = reg - 1;
                reg--;
                block.instructions.push_back(inst);
                break;
            }
            case OpCode::LOAD_LOCAL: {
                u16 slot = chunk.read_u16(ip);
                JITInstruction inst;
                inst.op = JITOpKind::LoadLocal;
                inst.dest = reg++;
                inst.imm = slot;
                block.instructions.push_back(inst);
                break;
            }
            case OpCode::STORE_LOCAL: {
                u16 slot = chunk.read_u16(ip);
                JITInstruction inst;
                inst.op = JITOpKind::StoreLocal;
                inst.dest = slot;
                inst.src1 = reg - 1;
                block.instructions.push_back(inst);
                break;
            }
            case OpCode::RETURN: {
                JITInstruction inst;
                inst.op = JITOpKind::Return;
                inst.src1 = reg - 1;
                block.instructions.push_back(inst);
                break;
            }
            case OpCode::JUMP_IF_FALSE: {
                i16 offset = chunk.read_i16(ip);
                JITInstruction inst;
                inst.op = JITOpKind::JumpIfFalse;
                inst.src1 = reg - 1;
                inst.label = ip + offset;
                block.instructions.push_back(inst);
                break;
            }
            default:
                // For unhandled opcodes, emit a deoptimization point
                {
                    JITInstruction inst;
                    inst.op = JITOpKind::Deopt;
                    inst.imm = ip - 1;
                    block.instructions.push_back(inst);
                }
                break;
        }
    }

    jit_fn.num_registers = reg;
    jit_fn.blocks.push_back(block);
    return jit_fn;
}

// ============================================================
// Optimization Passes
// ============================================================

void JITCompiler::optimize(JITFunction& fn) {
    if (opt_level_ >= 1) constant_folding(fn);
    if (opt_level_ >= 2) dead_code_elimination(fn);
}

void JITCompiler::constant_folding(JITFunction& fn) {
    for (auto& block : fn.blocks) {
        for (size_t i = 0; i + 2 < block.instructions.size(); i++) {
            auto& a = block.instructions[i];
            auto& b = block.instructions[i + 1];
            auto& c = block.instructions[i + 2];

            // Pattern: LoadConst r1, LoadConst r2, Add r3
            if (a.op == JITOpKind::LoadConst &&
                b.op == JITOpKind::LoadConst &&
                c.op == JITOpKind::Add &&
                c.src1 == a.dest && c.src2 == b.dest) {
                // Fold: replace with single LoadConst
                c.op = JITOpKind::LoadConst;
                c.imm = a.imm + b.imm; // simplified constant folding
                a.op = JITOpKind::Nop;
                b.op = JITOpKind::Nop;
            }
        }
    }
}

void JITCompiler::dead_code_elimination(JITFunction& fn) {
    for (auto& block : fn.blocks) {
        // Remove NOPs
        block.instructions.erase(
            std::remove_if(block.instructions.begin(), block.instructions.end(),
                [](const JITInstruction& inst) { return inst.op == JITOpKind::Nop; }),
            block.instructions.end()
        );
    }
}

void JITCompiler::inline_caching(JITFunction& fn, ObjFunction* src) {
    auto it = profiles_.find(src);
    if (it == profiles_.end()) return;

    // Insert type guards based on profile data
    for (auto& block : fn.blocks) {
        for (auto& inst : block.instructions) {
            if (inst.op == JITOpKind::Add || inst.op == JITOpKind::Sub ||
                inst.op == JITOpKind::Mul) {
                // Insert guard: check that operands are numbers
                JITInstruction guard;
                guard.op = JITOpKind::Guard;
                guard.src1 = inst.src1;
                // In a real implementation, we'd insert this before the operation
            }
        }
    }
}

void JITCompiler::speculative_optimization(JITFunction& fn, const ProfileData& profile) {
    // Use branch profiling to optimize hot paths
    for (auto& block : fn.blocks) {
        for (auto& inst : block.instructions) {
            if (inst.op == JITOpKind::JumpIfFalse || inst.op == JITOpKind::JumpIfTrue) {
                auto taken_it = profile.branch_taken.find(inst.label);
                auto not_taken_it = profile.branch_not_taken.find(inst.label);
                // If one direction is heavily biased, we can optimize for it
                // This would be used during native code generation
            }
        }
    }
}

// ============================================================
// Native Code Compilation (stub)
// ============================================================

CompiledCode JITCompiler::compile(const JITFunction& jit_fn) {
    CompiledCode result;

    // This is a placeholder for actual native code generation
    // A real JIT would:
    // 1. Allocate executable memory
    // 2. Emit machine code for each JIT instruction
    // 3. Handle register allocation
    // 4. Set up deoptimization points
    // 5. Mark the memory as executable

    // For now, we just note that compilation was attempted
    result.is_valid = false;
    result.code_ptr = nullptr;
    result.code_size = 0;

    return result;
}

// ============================================================
// AOT Compiler
// ============================================================

bool AOTCompiler::compile_module(ObjModule* /*module*/, const std::string& /*output_path*/) {
    // AOT compilation would:
    // 1. Iterate all functions in the module
    // 2. Lower each to native code
    // 3. Write to object file format (.o / .obj)
    // 4. Include metadata for linking
    return false; // not yet implemented
}

CompiledCode AOTCompiler::compile_function(ObjFunction* /*fn*/) {
    CompiledCode result;
    result.is_valid = false;
    return result;
}

bool AOTCompiler::link(const std::vector<std::string>& /*object_files*/,
                       const std::string& /*output_path*/) {
    // Would invoke the system linker or use LLD
    return false; // not yet implemented
}

} // namespace grey
