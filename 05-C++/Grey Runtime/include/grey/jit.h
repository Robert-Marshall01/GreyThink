// Grey Runtime - JIT / AOT Compilation Pipeline
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/bytecode.h"
#include "grey/objects.h"

#include <unordered_map>
#include <vector>

namespace grey {

// ============================================================
// 7.1 JIT Compiler
// ============================================================

// Intermediate representation for JIT
enum class JITOpKind : u8 {
    Nop,
    LoadConst,
    LoadLocal,
    StoreLocal,
    Add, Sub, Mul, Div, Mod,
    Neg,
    Eq, Neq, Lt, Lte, Gt, Gte,
    Not, And, Or,
    Jump, JumpIfTrue, JumpIfFalse,
    Call, Return,
    Phi,        // SSA phi node
    Guard,      // type guard for speculative optimization
    Deopt,      // deoptimization point
};

struct JITInstruction {
    JITOpKind op;
    u32 dest;       // destination register
    u32 src1;       // source 1
    u32 src2;       // source 2
    i64 imm;        // immediate value
    u32 label;      // branch target
};

struct JITBlock {
    u32 id;
    std::vector<JITInstruction> instructions;
    std::vector<u32> predecessors;
    std::vector<u32> successors;
    bool is_loop_header = false;
};

struct JITFunction {
    std::string name;
    std::vector<JITBlock> blocks;
    u32 num_registers = 0;
    u32 num_params = 0;
};

// Hot path profiling data
struct ProfileData {
    u64 call_count = 0;
    u64 loop_iterations = 0;
    std::unordered_map<u32, u64> branch_taken;     // offset -> count
    std::unordered_map<u32, u64> branch_not_taken;  // offset -> count

    // Type feedback for inline caching
    struct TypeFeedback {
        ObjType observed_type;
        u32 count;
    };
    std::unordered_map<u32, std::vector<TypeFeedback>> type_feedback;
};

// Compiled native code block
struct CompiledCode {
    void* code_ptr = nullptr;   // pointer to executable memory
    size_t code_size = 0;
    bool is_valid = false;

    // Deoptimization info
    struct DeoptPoint {
        u32 native_offset;
        u32 bytecode_offset;
        std::vector<u32> live_registers;
    };
    std::vector<DeoptPoint> deopt_points;
};

class JITCompiler {
public:
    JITCompiler() = default;
    ~JITCompiler();

    // Profile a function call (hot path detection)
    void record_call(ObjFunction* fn) {
        profiles_[fn].call_count++;
    }

    void record_branch(ObjFunction* fn, u32 offset, bool taken) {
        if (taken) profiles_[fn].branch_taken[offset]++;
        else profiles_[fn].branch_not_taken[offset]++;
    }

    void record_type(ObjFunction* fn, u32 offset, ObjType type) {
        auto& feedback = profiles_[fn].type_feedback[offset];
        for (auto& tf : feedback) {
            if (tf.observed_type == type) { tf.count++; return; }
        }
        feedback.push_back({type, 1});
    }

    // Check if function is hot enough to JIT
    bool is_hot(ObjFunction* fn) const {
        auto it = profiles_.find(fn);
        if (it == profiles_.end()) return false;
        return it->second.call_count >= hot_threshold_;
    }

    // Compile bytecode to JIT IR
    JITFunction lower_to_ir(ObjFunction* fn);

    // Optimize JIT IR
    void optimize(JITFunction& jit_fn);

    // Compile JIT IR to native code
    CompiledCode compile(const JITFunction& jit_fn);

    // Get compiled code for a function
    CompiledCode* get_compiled(ObjFunction* fn) {
        auto it = compiled_.find(fn);
        if (it != compiled_.end() && it->second.is_valid) return &it->second;
        return nullptr;
    }

    // Invalidate compiled code (for deoptimization)
    void invalidate(ObjFunction* fn) {
        auto it = compiled_.find(fn);
        if (it != compiled_.end()) {
            it->second.is_valid = false;
        }
    }

    // Configuration
    void set_hot_threshold(u64 t) { hot_threshold_ = t; }
    void set_optimization_level(int level) { opt_level_ = level; }

private:
    // Optimization passes
    void constant_folding(JITFunction& fn);
    void dead_code_elimination(JITFunction& fn);
    void inline_caching(JITFunction& fn, ObjFunction* src);
    void speculative_optimization(JITFunction& fn, const ProfileData& profile);

    std::unordered_map<ObjFunction*, ProfileData>   profiles_;
    std::unordered_map<ObjFunction*, CompiledCode>  compiled_;
    u64 hot_threshold_ = 1000;  // calls before JIT
    int opt_level_ = 2;         // optimization level (0-3)
};

// ============================================================
// 7.2 AOT Compiler
// ============================================================

class AOTCompiler {
public:
    struct AOTConfig {
        std::string target_arch = "x86_64"; // x86_64, aarch64, wasm32
        std::string target_os   = "native"; // native, linux, windows, macos
        int  opt_level = 2;
        bool static_link = true;
        bool strip_debug = false;
        std::vector<std::string> link_libs;
    };

    AOTCompiler() = default;
    explicit AOTCompiler(const AOTConfig& config) : config_(config) {}

    // Compile entire module to native object file
    bool compile_module(ObjModule* module, const std::string& output_path);

    // Compile function to native code
    CompiledCode compile_function(ObjFunction* fn);

    // Link object files into executable
    bool link(const std::vector<std::string>& object_files,
              const std::string& output_path);

    // Cross-compilation support
    void set_target(const std::string& arch, const std::string& os) {
        config_.target_arch = arch;
        config_.target_os = os;
    }

private:
    AOTConfig config_;
};

} // namespace grey
