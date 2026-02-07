// Grey Runtime - Virtual Machine (Core Execution Engine)
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/bytecode.h"
#include "grey/memory.h"
#include "grey/objects.h"

#include <functional>
#include <unordered_map>

namespace grey {

// Forward declarations
class ModuleLoader;
class Sandbox;
class Debugger;
class AsyncRuntime;

static constexpr size_t MAX_CALL_FRAMES = 4096;

// ============================================================
// VM Configuration
// ============================================================

struct VMConfig {
    size_t stack_size          = STACK_MAX;
    size_t max_call_depth      = MAX_CALL_FRAMES;
    size_t heap_initial_size   = 1024 * 1024;      // 1MB
    size_t gc_threshold        = 1024 * 1024;       // 1MB
    double gc_grow_factor      = 2.0;
    bool   deterministic_mode  = false;
    bool   sandbox_enabled     = false;
    size_t max_memory          = 0;                 // 0 = unlimited
    size_t max_cpu_cycles      = 0;                 // 0 = unlimited
    bool   debug_mode          = false;
    bool   jit_enabled         = false;
};

// ============================================================
// Try Frame for exception handling
// ============================================================

struct TryFrame {
    u32 catch_ip;
    u32 finally_ip;
    u32 frame_index;
    u32 stack_top;
};

// ============================================================
// Virtual Machine
// ============================================================

class VM {
public:
    explicit VM(const VMConfig& config = VMConfig{});
    ~VM();

    // ----- Execution -----
    Value execute(ObjFunction* main_function);
    Value execute(ObjClosure* closure, int arg_count = 0, Value* args = nullptr);
    Value call_function(Value callee, int arg_count, Value* args);

    // ----- Module System -----
    void set_module_loader(ModuleLoader* loader) { module_loader_ = loader; }
    ObjModule* load_module(const std::string& name);
    ObjModule* current_module();

    // ----- Global Variables -----
    void define_global(const std::string& name, Value value);
    Value get_global(const std::string& name);

    // ----- Native Functions -----
    void define_native(const std::string& name, NativeFn fn, u16 arity);

    // ----- GC Interface -----
    GarbageCollector& gc() { return gc_; }
    void collect_garbage();

    // ----- Sandbox -----
    void set_sandbox(Sandbox* sb) { sandbox_ = sb; }

    // ----- Debugger -----
    void set_debugger(Debugger* dbg) { debugger_ = dbg; }

    // ----- Async -----
    void set_async_runtime(AsyncRuntime* rt) { async_runtime_ = rt; }

    // ----- State -----
    const VMConfig& config() const { return config_; }
    bool is_running() const { return running_; }
    size_t cycle_count() const { return cycle_count_; }

    // ----- Stack access (for natives) -----
    ValueStack& stack() { return stack_; }
    Value peek(size_t distance = 0) { return stack_.peek(distance); }

    // ----- Error reporting -----
    std::string format_stack_trace() const;

private:
    // Core dispatch loop
    Value run();

    // Internal helpers
    void   push(Value v) { stack_.push(v); }
    Value  pop() { return stack_.pop(); }
    Value& peek_ref(size_t d = 0) { return stack_.peek(d); }

    // Frame management
    void push_frame(ObjClosure* closure, int arg_count);
    void pop_frame();
    CallFrame& current_frame() { return frames_[frame_count_ - 1]; }

    // Opcode handlers
    void op_arithmetic(OpCode op);
    void op_comparison(OpCode op);
    void op_call(u8 arg_count);
    void op_call_method(u16 name_idx, u8 arg_count);
    void op_closure(u16 func_idx);
    void op_class(u16 name_idx);
    void op_instance();
    void op_new_array(u16 count);
    void op_new_map(u16 count);
    void op_new_set(u16 count);
    void op_load_field(u16 name_idx);
    void op_store_field(u16 name_idx);
    void op_load_index();
    void op_store_index();
    void op_iter_begin();
    void op_iter_next(i16 offset);
    void op_iter_close();
    void op_import(u16 name_idx);
    void op_throw();
    void op_try_begin(u16 catch_off, u16 finally_off);

    // Upvalue management
    ObjUpvalue* capture_upvalue(Value* local);
    void close_upvalues(Value* last);

    // GC integration
    void mark_roots();
    void trace_object(ObjHeader* obj);

    // Error
    [[noreturn]] void runtime_error(ErrorCode code, const std::string& msg);
    bool handle_exception(Value exception);

    // ----- State -----
    VMConfig         config_;
    ValueStack       stack_;
    CallFrame        frames_[MAX_CALL_FRAMES];
    u32              frame_count_ = 0;
    GarbageCollector gc_;
    ArenaAllocator   arena_;

    // Globals
    std::unordered_map<std::string, Value> globals_;

    // String interning
    std::unordered_map<std::string, ObjString*> interned_strings_;

    // Open upvalues (linked list, sorted by stack position)
    ObjUpvalue* open_upvalues_ = nullptr;

    // Exception handling
    std::vector<TryFrame> try_stack_;
    Value current_exception_ = Value::nil();

    // External systems
    ModuleLoader* module_loader_  = nullptr;
    Sandbox*      sandbox_        = nullptr;
    Debugger*     debugger_       = nullptr;
    AsyncRuntime* async_runtime_  = nullptr;

    // Execution state
    bool   running_    = false;
    size_t cycle_count_ = 0;

    // Interned string helper
    ObjString* intern_string(const std::string& s);
    Value make_string_value(const std::string& s);
};

} // namespace grey
