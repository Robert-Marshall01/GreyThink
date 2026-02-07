// Grey Runtime - Example: Bytecode Serialization (ABI)
// Demonstrates compiling and serializing bytecode to a .greyc file,
// then loading and executing it — like Python's .pyc files.
//
// Build: cmake --build build && build/grey_examples

#include "grey/runtime.h"
#include "grey/bytecode.h"
#include "grey/vm.h"
#include "grey/objects.h"
#include "grey/abi.h"

#include <iostream>
#include <fstream>

using namespace grey;

int main() {
    std::cout << "=== Grey Runtime: Bytecode Serialization ===" << std::endl;
    std::cout << std::endl;

    // -----------------------------------------------
    // Step 1: Create bytecode for a program
    //   def compute(a, b):
    //       return a * b + 1
    //   result = compute(6, 7)  # => 43
    // -----------------------------------------------
    std::cout << "--- Step 1: Create Bytecode ---" << std::endl;

    Chunk compute_chunk;
    // a is local 0, b is local 1
    compute_chunk.emit(OpCode::LOAD_LOCAL, 1);
    compute_chunk.emit_u16(0, 1);                       // load a
    compute_chunk.emit(OpCode::LOAD_LOCAL, 1);
    compute_chunk.emit_u16(1, 1);                       // load b
    compute_chunk.emit(OpCode::MUL, 1);                 // a * b
    compute_chunk.emit_constant(Value::integer(1), 2);  // push 1
    compute_chunk.emit(OpCode::ADD, 2);                 // a*b + 1
    compute_chunk.emit(OpCode::RETURN, 3);

    std::cout << "  compute(a, b) = a * b + 1" << std::endl;
    std::cout << "  Bytecode size: " << compute_chunk.code.size() << " bytes" << std::endl;
    std::cout << "  Constants: " << compute_chunk.constants.size() << std::endl;

    // -----------------------------------------------
    // Step 2: Serialize to .greyc binary format
    // -----------------------------------------------
    std::cout << std::endl;
    std::cout << "--- Step 2: Serialize to Binary ---" << std::endl;

    ModuleWriter writer;
    writer.begin();

    // Add string table
    writer.add_string("compute");     // index 0
    writer.add_string("main");        // index 1

    // Add the compute function
    FunctionEntry fn_entry;
    fn_entry.name_index = 0;       // "compute"
    fn_entry.arity = 2;            // takes 2 args
    fn_entry.local_count = 2;
    fn_entry.upvalue_count = 0;
    fn_entry.code_offset = 0;
    fn_entry.code_size = 0;
    fn_entry.constant_offset = 0;
    fn_entry.constant_count = 0;
    fn_entry.cc = CallingConvention::GreyDefault;
    writer.add_function(fn_entry, compute_chunk);

    auto binary_data = writer.finish();
    std::cout << "  Serialized module size: " << binary_data.size() << " bytes" << std::endl;

    // Write to file
    std::string filename = "example_module.greyc";
    {
        std::ofstream file(filename, std::ios::binary);
        file.write(reinterpret_cast<const char*>(binary_data.data()), binary_data.size());
        std::cout << "  Written to: " << filename << std::endl;
    }

    // -----------------------------------------------
    // Step 3: Read back and validate
    // -----------------------------------------------
    std::cout << std::endl;
    std::cout << "--- Step 3: Read & Validate ---" << std::endl;

    std::vector<byte> loaded_data;
    {
        std::ifstream file(filename, std::ios::binary | std::ios::ate);
        auto size = file.tellg();
        file.seekg(0);
        loaded_data.resize(size);
        file.read(reinterpret_cast<char*>(loaded_data.data()), size);
    }

    ModuleReader reader(loaded_data);
    bool valid = reader.validate();
    std::cout << "  Module valid: " << (valid ? "YES" : "NO") << std::endl;

    if (valid) {
        auto hdr = reader.header();
        std::cout << "  Magic: 0x" << std::hex << hdr.magic << std::dec << std::endl;
        std::cout << "  Version: " << hdr.version_major << "." << hdr.version_minor << std::endl;
        std::cout << "  Functions: " << hdr.function_count << std::endl;
        std::cout << "  Strings: " << hdr.string_count << std::endl;

        auto functions = reader.functions();
        for (u32 i = 0; i < functions.size(); i++) {
            std::string name = reader.get_string(functions[i].name_index);
            std::cout << "  Function[" << i << "]: " << name
                      << " (arity=" << functions[i].arity
                      << ", locals=" << functions[i].local_count << ")" << std::endl;
        }

        // Get the chunk back
        auto restored_chunk = reader.get_function_chunk(0);
        std::cout << "  Restored bytecode size: " << restored_chunk.code.size() << " bytes" << std::endl;

        // -----------------------------------------------
        // Step 4: Execute the loaded bytecode
        // -----------------------------------------------
        std::cout << std::endl;
        std::cout << "--- Step 4: Execute Loaded Bytecode ---" << std::endl;

        VMConfig config;
        VM vm(config);

        auto* fn = new ObjFunction();
        fn->name = "compute";
        fn->chunk = restored_chunk;
        fn->arity = 2;
        fn->local_count = 2;
        vm.gc().track(fn);

        // Create a wrapper that calls compute(6, 7)
        auto* closure = new ObjClosure(fn);
        vm.gc().track(closure);

        Value args[2] = { Value::integer(6), Value::integer(7) };
        Value result = vm.execute(closure, 2, args);

        std::cout << "  compute(6, 7) = ";
        if (result.is_int()) std::cout << result.as_int();
        else if (result.is_number()) std::cout << result.as_number();
        std::cout << std::endl;
        std::cout << "  Expected: 43" << std::endl;
    }

    // -----------------------------------------------
    // Step 5: ABI compatibility check
    // -----------------------------------------------
    std::cout << std::endl;
    std::cout << "--- Step 5: ABI Info ---" << std::endl;
    auto abi = current_abi();
    std::cout << "  ABI version: " << abi.version_major << "." << abi.version_minor << std::endl;
    std::cout << "  Pointer size: " << (int)abi.pointer_size << " bytes" << std::endl;
    std::cout << "  Value size: " << (int)abi.value_size << " bytes" << std::endl;
    std::cout << "  Endianness: " << (abi.endianness == 0 ? "little" : "big") << std::endl;
    std::cout << "  Self-compatible: " << (abi.is_compatible(abi) ? "yes" : "no") << std::endl;

    // Cleanup
    std::remove(filename.c_str());
    std::cout << std::endl;
    std::cout << "  Cleaned up " << filename << std::endl;

    std::cout << std::endl;
    std::cout << "=== Bytecode Serialization Complete ===" << std::endl;
    return 0;
}
