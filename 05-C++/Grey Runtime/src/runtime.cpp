// Grey Runtime - Runtime Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/runtime.h"

namespace grey {

// ============================================================
// Constructor / Destructor
// ============================================================

Runtime::Runtime(const Config& config)
    : config_(config)
    , vm_(config.vm)
    , async_runtime_(config.async_workers)
{
    logger_.set_level(config.log_level);
}

Runtime::~Runtime() {
    shutdown();
}

// ============================================================
// Initialize
// ============================================================

bool Runtime::initialize() {
    if (initialized_) return true;

    logger_.info("Grey Runtime v" + std::string(version()) + " initializing...");

    // Set up module search paths
    for (auto& path : config_.module_paths) {
        module_loader_.add_search_path(path);
    }
    vm_.set_module_loader(&module_loader_);

    // Set up sandbox if enabled
    if (config_.enable_sandbox) {
        vm_.set_sandbox(&sandbox_);
    }

    // Set up deterministic mode
    if (config_.deterministic || config_.enable_determinism) {
        deterministic_.enable();
        deterministic_.apply_defaults();
    }

    // Set up debugger  
    if (config_.enable_debug) {
        vm_.set_debugger(&debugger_);
    }

    // Set up async runtime
    vm_.set_async_runtime(&async_runtime_);

    // Register standard library
    register_stdlib();
    register_builtins();

    // Start async workers
    async_runtime_.start();

    // Initialize networking
#if GREY_ENABLE_NETWORKING
    network_init();
#endif

    initialized_ = true;
    logger_.info("Grey Runtime initialized successfully");
    return true;
}

bool Runtime::initialize(const Config& config) {
    config_ = config;
    logger_.set_level(config.log_level);
    return initialize();
}

// ============================================================
// Shutdown
// ============================================================

void Runtime::shutdown() {
    if (!initialized_) return;

    logger_.info("Grey Runtime shutting down...");
    async_runtime_.stop();
#if GREY_ENABLE_NETWORKING
    network_shutdown();
#endif
    module_loader_.clear_cache();
    initialized_ = false;
}

// ============================================================
// Execute
// ============================================================

Value Runtime::execute_file(const std::string& path) {
    if (!initialized_) initialize();

    logger_.info("Executing file: " + path);

    // Read file
    std::ifstream file(path, std::ios::binary | std::ios::ate);
    if (!file.is_open()) {
        throw RuntimeError(ErrorCode::IOError, "Cannot open file: " + path);
    }

    auto size = file.tellg();
    file.seekg(0);
    std::vector<byte> data(size);
    file.read(reinterpret_cast<char*>(data.data()), size);

    return execute_bytecode(data);
}

Value Runtime::execute_bytecode(const std::vector<byte>& bytecode) {
    if (!initialized_) initialize();

    // Parse module binary format
    ModuleReader reader(bytecode);
    if (!reader.validate()) {
        throw RuntimeError(ErrorCode::ImportError, "Invalid bytecode module");
    }

    // Load main function
    auto functions = reader.functions();
    if (functions.empty()) {
        throw RuntimeError(ErrorCode::ImportError, "Module has no functions");
    }

    // Create function object from first entry (main)
    auto* main_fn = new ObjFunction();
    vm_.gc().track(main_fn);
    main_fn->name = reader.get_string(functions[0].name_index);
    main_fn->arity = functions[0].arity;
    main_fn->chunk = reader.get_function_chunk(0);

    return vm_.execute(main_fn);
}

Value Runtime::execute_function(ObjFunction* fn) {
    if (!initialized_) initialize();
    return vm_.execute(fn);
}

Value Runtime::eval(const std::string& /*source*/) {
    // Would integrate with Grey++ compiler here
    // For now, return nil
    logger_.warn("eval() requires Grey++ compiler integration");
    return Value::nil();
}

// ============================================================
// Module Loading
// ============================================================

ObjModule* Runtime::load_module(const std::string& name) {
    return module_loader_.load(name, vm_);
}

void Runtime::add_module_path(const std::string& path) {
    module_loader_.add_search_path(path);
}

// ============================================================
// Native Registration
// ============================================================

void Runtime::define_native(const std::string& name, NativeFn fn, u16 arity) {
    vm_.define_native(name, std::move(fn), arity);
}

void Runtime::define_global(const std::string& name, Value value) {
    vm_.define_global(name, value);
}

// ============================================================
// Register Standard Library
// ============================================================

void Runtime::register_stdlib() {
    register_collection_natives(vm_);
    register_io_natives(vm_);
    register_system_natives(vm_);

#if GREY_ENABLE_NETWORKING
    register_network_natives(vm_);
#endif

#if GREY_ENABLE_CRYPTO
    register_crypto_natives(vm_);
#endif
}

// ============================================================
// Register Built-in Functions
// ============================================================

void Runtime::register_builtins() {
    // print
    vm_.define_native("print", [this](int argc, Value* args) -> Value {
        for (int i = 0; i < argc; i++) {
            if (i > 0) std::cout << " ";
            std::cout << value_to_string(args[i]);
        }
        std::cout << "\n";
        return Value::nil();
    }, 0);

    // println
    vm_.define_native("println", [this](int argc, Value* args) -> Value {
        for (int i = 0; i < argc; i++) {
            if (i > 0) std::cout << " ";
            std::cout << value_to_string(args[i]);
        }
        std::cout << std::endl;
        return Value::nil();
    }, 0);

    // typeof
    vm_.define_native("typeof", [this](int argc, Value* args) -> Value {
        if (argc < 1) return make_string(vm_.gc(), "nil");
        Value v = args[0];
        if (v.is_nil()) return make_string(vm_.gc(), "nil");
        if (v.is_bool()) return make_string(vm_.gc(), "bool");
        if (v.is_int()) return make_string(vm_.gc(), "int");
        if (v.is_number()) return make_string(vm_.gc(), "number");
        if (v.is_object()) {
            auto* obj = v.as_object<ObjHeader>();
            switch (obj->type) {
                case ObjType::String:         return make_string(vm_.gc(), "string");
                case ObjType::Array:          return make_string(vm_.gc(), "array");
                case ObjType::Map:            return make_string(vm_.gc(), "map");
                case ObjType::Set:            return make_string(vm_.gc(), "set");
                case ObjType::Function:       return make_string(vm_.gc(), "function");
                case ObjType::Closure:        return make_string(vm_.gc(), "function");
                case ObjType::NativeFunction: return make_string(vm_.gc(), "function");
                case ObjType::Class:          return make_string(vm_.gc(), "class");
                case ObjType::Instance:       return make_string(vm_.gc(), "instance");
                case ObjType::Module:         return make_string(vm_.gc(), "module");
                case ObjType::Fiber:          return make_string(vm_.gc(), "fiber");
                case ObjType::Future:         return make_string(vm_.gc(), "future");
                case ObjType::Channel:        return make_string(vm_.gc(), "channel");
                case ObjType::Buffer:         return make_string(vm_.gc(), "buffer");
                case ObjType::Iterator:       return make_string(vm_.gc(), "iterator");
                case ObjType::BoundMethod:    return make_string(vm_.gc(), "function");
                default:                      return make_string(vm_.gc(), "object");
            }
        }
        return make_string(vm_.gc(), "unknown");
    }, 1);

    // assert
    vm_.define_native("assert", [](int argc, Value* args) -> Value {
        if (argc < 1) return Value::nil();
        if (!args[0].is_truthy()) {
            std::string msg = "Assertion failed";
            if (argc >= 2 && args[1].is_object()) {
                auto* obj = args[1].as_object<ObjHeader>();
                if (obj->type == ObjType::String) {
                    msg = static_cast<ObjString*>(obj)->value;
                }
            }
            throw RuntimeError(ErrorCode::AssertionFailed, msg);
        }
        return Value::nil();
    }, 1);

    // len
    vm_.define_native("len", [](int argc, Value* args) -> Value {
        if (argc < 1) return Value::integer(0);
        Value v = args[0];
        if (!v.is_object()) return Value::integer(0);
        auto* obj = v.as_object<ObjHeader>();
        switch (obj->type) {
            case ObjType::String: return Value::integer(static_cast<ObjString*>(obj)->value.size());
            case ObjType::Array:  return Value::integer(static_cast<ObjArray*>(obj)->length());
            case ObjType::Map:    return Value::integer(static_cast<ObjMap*>(obj)->count);
            case ObjType::Set:    return Value::integer(static_cast<ObjSet*>(obj)->size());
            case ObjType::Buffer: return Value::integer(static_cast<ObjBuffer*>(obj)->data.size());
            default: return Value::integer(0);
        }
    }, 1);

    // str - convert to string
    vm_.define_native("str", [this](int argc, Value* args) -> Value {
        if (argc < 1) return make_string(vm_.gc(), "");
        return make_string(vm_.gc(), value_to_string(args[0]));
    }, 1);

    // int - convert to integer
    vm_.define_native("int", [](int argc, Value* args) -> Value {
        if (argc < 1) return Value::integer(0);
        Value v = args[0];
        if (v.is_int()) return v;
        if (v.is_number()) return Value::integer(static_cast<i64>(v.as_number()));
        if (v.is_bool()) return Value::integer(v.as_bool() ? 1 : 0);
        if (v.is_object() && v.as_object<ObjHeader>()->type == ObjType::String) {
            try {
                return Value::integer(std::stoll(static_cast<ObjString*>(v.as_object<ObjHeader>())->value));
            } catch (...) {
                throw RuntimeError(ErrorCode::TypeError, "Cannot convert string to int");
            }
        }
        throw RuntimeError(ErrorCode::TypeError, "Cannot convert to int");
    }, 1);

    // float - convert to float
    vm_.define_native("float", [](int argc, Value* args) -> Value {
        if (argc < 1) return Value::number(0.0);
        Value v = args[0];
        if (v.is_number()) return v;
        if (v.is_int()) return Value::number(static_cast<f64>(v.as_int()));
        if (v.is_bool()) return Value::number(v.as_bool() ? 1.0 : 0.0);
        if (v.is_object() && v.as_object<ObjHeader>()->type == ObjType::String) {
            try {
                return Value::number(std::stod(static_cast<ObjString*>(v.as_object<ObjHeader>())->value));
            } catch (...) {
                throw RuntimeError(ErrorCode::TypeError, "Cannot convert string to float");
            }
        }
        throw RuntimeError(ErrorCode::TypeError, "Cannot convert to float");
    }, 1);

    // input
    vm_.define_native("input", [this](int argc, Value* args) -> Value {
        if (argc >= 1) {
            std::cout << value_to_string(args[0]);
        }
        std::string line;
        std::getline(std::cin, line);
        return make_string(vm_.gc(), line);
    }, 0);

    // gc_collect
    vm_.define_native("gc_collect", [this](int, Value*) -> Value {
        vm_.collect_garbage();
        return Value::nil();
    }, 0);

    // gc_stats
    vm_.define_native("gc_stats", [this](int, Value*) -> Value {
        auto* map = new ObjMap();
        vm_.gc().track(map);
        // In a full implementation, populate with GC stats
        return Value::object(map);
    }, 0);
}

} // namespace grey
