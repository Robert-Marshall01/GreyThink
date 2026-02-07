// Grey Runtime - CLI Entry Point
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
//
// Usage:
//   grey <file.greyc>           Run a compiled Grey++ module
//   grey --repl                 Start interactive REPL
//   grey --version              Print version information
//   grey --debug <file.greyc>   Run with debugger attached
//   grey --profile <file.greyc> Run with profiler enabled
//   grey --disasm <file.greyc>  Disassemble a module
//   grey --sandbox <file.greyc> Run in sandboxed mode
//   grey --deterministic <file> Run in deterministic mode
//   grey --jit <file.greyc>     Run with JIT compilation
//   grey --module-path <path>   Add module search path
//   grey --log-level <level>    Set log level (debug/info/warn/error)
//   grey --help                 Print help

#include "grey/runtime.h"
#include "grey/bytecode.h"
#include "grey/abi.h"
#include "grey/debug.h"
#include "grey/vm.h"

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstring>

using namespace grey;

// ============================================================
// CLI Argument Parser
// ============================================================

struct CLIOptions {
    std::string input_file;
    bool repl = false;
    bool show_version = false;
    bool show_help = false;
    bool debug_mode = false;
    bool profile_mode = false;
    bool disassemble = false;
    bool sandbox_mode = false;
    bool deterministic = false;
    bool jit_enabled = false;
    std::string log_level = "info";
    std::vector<std::string> module_paths;
    std::vector<std::string> program_args;
};

static void print_help() {
    std::cout <<
R"(Grey Runtime v)" << Runtime::version() << R"(
Usage: grey [options] [file.greyc] [-- args...]

Options:
  --help, -h            Show this help message
  --version, -v         Print version information
  --repl                Start interactive REPL
  --debug               Run with debugger attached
  --profile             Run with profiler enabled
  --disasm              Disassemble bytecode module
  --sandbox             Run in sandboxed mode
  --deterministic       Run in deterministic mode
  --jit                 Enable JIT compilation
  --module-path <path>  Add module search path (can be repeated)
  --log-level <level>   Set log level: debug, info, warn, error

Examples:
  grey program.greyc
  grey --debug program.greyc
  grey --sandbox --deterministic contract.greyc
  grey --repl
  grey --disasm module.greyc
)";
}

static void print_version() {
    std::cout << "Grey Runtime v" << Runtime::version() << std::endl;
    auto abi = current_abi();
    std::cout << "ABI: v" << abi.version_major << "." << abi.version_minor
              << ", " << (int)abi.pointer_size * 8 << "-bit"
              << ", " << (abi.endianness == 0 ? "little-endian" : "big-endian")
              << std::endl;
    std::cout << "Value size: " << (int)abi.value_size << " bytes" << std::endl;
    std::cout << "Build: C++" << __cplusplus << std::endl;
#if defined(GREY_PLATFORM_WINDOWS)
    std::cout << "Platform: Windows" << std::endl;
#elif defined(GREY_PLATFORM_LINUX)
    std::cout << "Platform: Linux" << std::endl;
#elif defined(GREY_PLATFORM_MACOS)
    std::cout << "Platform: macOS" << std::endl;
#else
    std::cout << "Platform: Unknown" << std::endl;
#endif
}

static CLIOptions parse_args(int argc, char* argv[]) {
    CLIOptions opts;
    bool after_separator = false;

    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];

        if (after_separator) {
            opts.program_args.push_back(arg);
            continue;
        }

        if (arg == "--") {
            after_separator = true;
            continue;
        }

        if (arg == "--help" || arg == "-h") {
            opts.show_help = true;
        } else if (arg == "--version" || arg == "-v") {
            opts.show_version = true;
        } else if (arg == "--repl") {
            opts.repl = true;
        } else if (arg == "--debug") {
            opts.debug_mode = true;
        } else if (arg == "--profile") {
            opts.profile_mode = true;
        } else if (arg == "--disasm") {
            opts.disassemble = true;
        } else if (arg == "--sandbox") {
            opts.sandbox_mode = true;
        } else if (arg == "--deterministic") {
            opts.deterministic = true;
        } else if (arg == "--jit") {
            opts.jit_enabled = true;
        } else if (arg == "--module-path" && i + 1 < argc) {
            opts.module_paths.push_back(argv[++i]);
        } else if (arg == "--log-level" && i + 1 < argc) {
            opts.log_level = argv[++i];
        } else if (arg[0] != '-') {
            opts.input_file = arg;
        } else {
            std::cerr << "Unknown option: " << arg << std::endl;
            opts.show_help = true;
        }
    }
    return opts;
}

// ============================================================
// Disassembler
// ============================================================

static void disassemble_chunk(const Chunk& chunk, const std::string& name) {
    std::cout << "=== " << name << " ===" << std::endl;
    size_t offset = 0;
    while (offset < chunk.code.size()) {
        std::printf("%04zu  ", offset);
        auto op = static_cast<OpCode>(chunk.code[offset]);
        std::cout << opcode_name(op);

        switch (op) {
            case OpCode::PUSH_CONST:
            case OpCode::LOAD_LOCAL:
            case OpCode::STORE_LOCAL:
            case OpCode::LOAD_GLOBAL:
            case OpCode::STORE_GLOBAL:
            case OpCode::LOAD_UPVALUE:
            case OpCode::STORE_UPVALUE:
            case OpCode::LOAD_FIELD:
            case OpCode::STORE_FIELD:
            case OpCode::IMPORT:
            case OpCode::IMPORT_FROM:
            case OpCode::NEW_CLASS:
                if (offset + 1 < chunk.code.size()) {
                    u8 idx = chunk.code[offset + 1];
                    std::printf(" %d", idx);
                    if (op == OpCode::PUSH_CONST && idx < chunk.constants.size()) {
                        std::cout << " (";
                        auto& val = chunk.constants[idx];
                        if (val.is_nil()) std::cout << "nil";
                        else if (val.is_boolean()) std::cout << (val.as_boolean() ? "true" : "false");
                        else if (val.is_int()) std::cout << val.as_int();
                        else if (val.is_number()) std::cout << val.as_number();
                        else if (val.is_object()) {
                            auto* obj = val.as_object<ObjHeader>();
                            if (obj && obj->type == ObjType::String) {
                                std::cout << "\"" << static_cast<ObjString*>(obj)->value << "\"";
                            } else {
                                std::cout << "<object>";
                            }
                        }
                        std::cout << ")";
                    }
                    offset += 2;
                } else {
                    offset += 1;
                }
                break;

            case OpCode::JUMP:
            case OpCode::JUMP_IF_FALSE:
            case OpCode::JUMP_IF_TRUE:
            case OpCode::LOOP:
            case OpCode::TRY_BEGIN:
                if (offset + 2 < chunk.code.size()) {
                    u16 target = static_cast<u16>(chunk.code[offset + 1]) |
                                 (static_cast<u16>(chunk.code[offset + 2]) << 8);
                    std::printf(" -> %d", target);
                    offset += 3;
                } else {
                    offset += 1;
                }
                break;

            case OpCode::CALL:
            case OpCode::NEW_ARRAY:
            case OpCode::NEW_MAP:
            case OpCode::NEW_SET:
                if (offset + 1 < chunk.code.size()) {
                    std::printf(" %d", chunk.code[offset + 1]);
                    offset += 2;
                } else {
                    offset += 1;
                }
                break;

            default:
                offset += 1;
                break;
        }
        std::cout << std::endl;
    }
    std::cout << "=== end " << name << " ===" << std::endl;
}

static void disassemble_module(const std::string& path) {
    std::ifstream file(path, std::ios::binary);
    if (!file.is_open()) {
        std::cerr << "Error: cannot open file: " << path << std::endl;
        return;
    }
    std::vector<byte> data((std::istreambuf_iterator<char>(file)),
                            std::istreambuf_iterator<char>());
    file.close();

    ModuleReader reader(data);
    if (!reader.validate()) {
        std::cerr << "Error: invalid Grey module: " << path << std::endl;
        return;
    }

    auto hdr = reader.header();
    std::cout << "Module: " << path << std::endl;
    std::cout << "Version: " << hdr.version_major << "." << hdr.version_minor << std::endl;
    std::cout << "Functions: " << hdr.function_count << std::endl;
    std::cout << "Strings: " << hdr.string_count << std::endl;
    std::cout << "Types: " << hdr.type_count << std::endl;
    std::cout << std::endl;

    auto fns = reader.functions();
    for (u32 i = 0; i < fns.size(); i++) {
        std::string name = reader.get_string(fns[i].name_index);
        if (name.empty()) name = "<function " + std::to_string(i) + ">";
        std::cout << "Function: " << name
                  << " (arity=" << fns[i].arity
                  << ", locals=" << fns[i].local_count
                  << ", upvalues=" << fns[i].upvalue_count << ")" << std::endl;

        auto chunk = reader.get_function_chunk(i);
        disassemble_chunk(chunk, name);
        std::cout << std::endl;
    }

    auto debug = reader.get_debug_info();
    if (!debug.empty()) {
        std::cout << "Debug Info (" << debug.size() << " mappings):" << std::endl;
        for (auto& m : debug) {
            std::printf("  IR#%u -> bytecode@%u  line %u:%u\n",
                        m.ir_node_id, m.bytecode_offset,
                        m.source_line, m.source_column);
        }
    }
}

// ============================================================
// REPL
// ============================================================

static void run_repl(Runtime& runtime) {
    std::cout << "Grey Runtime v" << Runtime::version() << " REPL" << std::endl;
    std::cout << "Type 'exit' or 'quit' to leave, 'help' for commands." << std::endl;
    std::cout << std::endl;

    while (true) {
        std::cout << "grey> ";
        std::cout.flush();

        std::string line;
        if (!std::getline(std::cin, line)) {
            std::cout << std::endl;
            break;
        }

        // Trim whitespace
        size_t start = line.find_first_not_of(" \t\r\n");
        if (start == std::string::npos) continue;
        size_t end = line.find_last_not_of(" \t\r\n");
        line = line.substr(start, end - start + 1);

        if (line.empty()) continue;
        if (line == "exit" || line == "quit") break;

        if (line == "help") {
            std::cout << "Commands:" << std::endl;
            std::cout << "  exit, quit    Exit the REPL" << std::endl;
            std::cout << "  help          Show this help" << std::endl;
            std::cout << "  .gc           Trigger garbage collection" << std::endl;
            std::cout << "  .gc-stats     Show GC statistics" << std::endl;
            std::cout << "  .modules      List loaded modules" << std::endl;
            std::cout << "  .version      Show version" << std::endl;
            std::cout << std::endl;
            continue;
        }

        if (line == ".gc") {
            runtime.gc().collect();
            std::cout << "GC cycle complete." << std::endl;
            continue;
        }

        if (line == ".gc-stats") {
            auto& gc = runtime.gc();
            std::cout << "Allocated objects: " << gc.object_count() << std::endl;
            std::cout << "Total collections: " << gc.collection_count() << std::endl;
            continue;
        }

        if (line == ".version") {
            print_version();
            continue;
        }

        if (line == ".modules") {
            std::cout << "Loaded modules:" << std::endl;
            // Module listing would come from the module loader
            std::cout << "  (module listing not yet implemented in REPL)" << std::endl;
            continue;
        }

        // Evaluate as Grey++ expression/statement
        try {
            auto result = runtime.eval(line);
            (void)result;
        } catch (const RuntimeError& err) {
            std::cerr << "Runtime Error: " << err.what() << std::endl;
        } catch (const std::exception& err) {
            std::cerr << "Error: " << err.what() << std::endl;
        }
    }

    std::cout << "Goodbye." << std::endl;
}

// ============================================================
// Main
// ============================================================

int main(int argc, char* argv[]) {
    CLIOptions opts = parse_args(argc, argv);

    if (opts.show_help) {
        print_help();
        return 0;
    }

    if (opts.show_version) {
        print_version();
        return 0;
    }

    // Build runtime configuration
    Runtime::Config config;
    config.enable_debug = opts.debug_mode;
    config.enable_jit = opts.jit_enabled;
    config.enable_sandbox = opts.sandbox_mode;
    config.enable_determinism = opts.deterministic;

    // Parse log level
    if (opts.log_level == "debug") config.log_level = LogLevel::Debug;
    else if (opts.log_level == "info") config.log_level = LogLevel::Info;
    else if (opts.log_level == "warn") config.log_level = LogLevel::Warn;
    else if (opts.log_level == "error") config.log_level = LogLevel::Error;

    // Initialize runtime
    Runtime runtime;
    runtime.initialize(config);

    // Add module search paths
    for (auto& path : opts.module_paths) {
        runtime.modules().add_search_path(path);
    }

    // Profiler setup
    if (opts.profile_mode) {
        runtime.profiler().start();
    }

    int exit_code = 0;

    if (opts.disassemble) {
        if (opts.input_file.empty()) {
            std::cerr << "Error: --disasm requires an input file" << std::endl;
            exit_code = 1;
        } else {
            disassemble_module(opts.input_file);
        }
    } else if (opts.repl || opts.input_file.empty()) {
        // REPL mode (default when no file given)
        run_repl(runtime);
    } else {
        // Execute file
        try {
            auto result = runtime.execute_file(opts.input_file);
            if (result.is_int()) {
                exit_code = static_cast<int>(result.as_int());
            }
        } catch (const RuntimeError& err) {
            std::cerr << "Grey Runtime Error [" << static_cast<int>(err.code) << "]: "
                      << err.what() << std::endl;
            exit_code = 1;
        } catch (const std::exception& err) {
            std::cerr << "Fatal Error: " << err.what() << std::endl;
            exit_code = 2;
        }
    }

    // Profiler report
    if (opts.profile_mode) {
        runtime.profiler().stop();
        std::cout << std::endl << "--- Profiler Report ---" << std::endl;
        runtime.profiler().report(std::cout);
        // Also export to CSV
        std::string csv_path = opts.input_file + ".profile.csv";
        runtime.profiler().export_csv(csv_path);
        std::cout << "Profile data exported to: " << csv_path << std::endl;
    }

    runtime.shutdown();
    return exit_code;
}
