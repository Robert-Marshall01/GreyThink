// Grey Runtime - Public API
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
//
// This is the main include header for the Grey Runtime.
// Include this single file to access the entire runtime.
#pragma once

#include "grey/common.h"
#include "grey/bytecode.h"
#include "grey/memory.h"
#include "grey/objects.h"
#include "grey/vm.h"
#include "grey/concurrency.h"
#include "grey/module.h"
#include "grey/sandbox.h"
#include "grey/jit.h"
#include "grey/debug.h"
#include "grey/abi.h"
#include "grey/stdlib/collections.h"
#include "grey/stdlib/io.h"
#include "grey/stdlib/net.h"
#include "grey/stdlib/system.h"
#include "grey/stdlib/crypto.h"

namespace grey {

// ============================================================
// Runtime — Top-level entry point
// ============================================================

class Runtime {
public:
    struct Config {
        VMConfig vm;
        bool enable_jit = false;
        bool enable_sandbox = false;
        bool enable_debug = false;
        bool enable_profiler = false;
        bool enable_determinism = false;
        bool deterministic = false;
        size_t async_workers = 0; // 0 = auto
        std::vector<std::string> module_paths;
        LogLevel log_level = LogLevel::Info;
    };

    explicit Runtime(const Config& config = Config{});
    ~Runtime();

    // ----- Initialize / Shutdown -----
    bool initialize();
    bool initialize(const Config& config);
    void shutdown();

    // ----- Execute -----
    Value execute_file(const std::string& path);
    Value execute_bytecode(const std::vector<byte>& bytecode);
    Value execute_function(ObjFunction* fn);
    Value eval(const std::string& source); // for REPL

    // ----- Module loading -----
    ObjModule* load_module(const std::string& name);
    void add_module_path(const std::string& path);

    // ----- Native function registration -----
    void define_native(const std::string& name, NativeFn fn, u16 arity);
    void define_global(const std::string& name, Value value);

    // ----- Access subsystems -----
    VM& vm() { return vm_; }
    GarbageCollector& gc() { return vm_.gc(); }
    ModuleLoader& modules() { return module_loader_; }
    Sandbox& sandbox() { return sandbox_; }
    DeterministicEngine& determinism() { return deterministic_; }
    AsyncRuntime& async() { return async_runtime_; }
    Debugger& debugger() { return debugger_; }
    Profiler& profiler() { return profiler_; }
    Logger& logger() { return logger_; }

#if GREY_ENABLE_JIT
    JITCompiler& jit() { return jit_compiler_; }
#endif

    // ----- State -----
    bool is_initialized() const { return initialized_; }
    const Config& config() const { return config_; }

    // ----- Version -----
    static const char* version() { return "1.0.0"; }
    static const char* name() { return "Grey Runtime"; }

private:
    void register_stdlib();
    void register_builtins();

    Config config_;
    VM vm_;
    ModuleLoader module_loader_;
    Sandbox sandbox_;
    DeterministicEngine deterministic_;
    AsyncRuntime async_runtime_;
    Debugger debugger_;
    Profiler profiler_;
    Logger logger_;

#if GREY_ENABLE_JIT
    JITCompiler jit_compiler_;
#endif

    bool initialized_ = false;
};

} // namespace grey
