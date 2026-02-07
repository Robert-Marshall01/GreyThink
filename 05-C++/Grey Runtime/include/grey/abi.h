// Grey Runtime - Compiler Integration & ABI
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/bytecode.h"
#include "grey/objects.h"

#include <vector>
#include <string>

namespace grey {

// ============================================================
// 9. Compiler Integration ABI
// ============================================================

// ----- Calling Convention -----
// Grey++ uses a register-based calling convention:
//   - Arguments passed on the value stack left-to-right
//   - Return value on top of stack
//   - Callee saves base pointer
//   - Caller cleans up arguments

enum class CallingConvention : u8 {
    GreyDefault = 0,   // Standard Grey++ calling convention
    GreyCDecl,         // C-compatible (for FFI)
    GreyFast,          // Optimized for small functions
};

// ----- Stack Frame Layout -----
// Each call frame on the stack has this layout:
//
//  [ ... caller's stack ... ]
//  [ arg N-1               ]  <- base_slot + N-1
//  [ arg N-2               ]
//  [ ...                   ]
//  [ arg 0                 ]  <- base_slot
//  [ --- frame boundary -- ]
//  [ local 0               ]  <- base_slot + arity
//  [ local 1               ]
//  [ ...                   ]
//  [ temporaries           ]  <- top of stack
//

struct FrameLayout {
    u16 arity;              // number of parameters
    u16 local_count;        // number of local variables
    u16 temp_count;         // estimated number of temporaries
    u16 upvalue_count;      // closed-over variables
    CallingConvention cc;
};

// ----- Type Representation -----
// Types are represented as tags for runtime type checking

enum class TypeTag : u16 {
    Void = 0,
    Nil,
    Bool,
    Int,
    Int8, Int16, Int32, Int64,
    UInt8, UInt16, UInt32, UInt64,
    Float32, Float64,
    String,
    Array,
    Map,
    Set,
    Function,
    Class,
    Instance,
    Module,
    Fiber,
    Future,
    Channel,
    Buffer,
    Any,
    // User-defined types start here
    UserDefined = 256,
};

struct TypeInfo {
    TypeTag tag;
    std::string name;
    u32 size;              // size in bytes (for value types)
    u32 alignment;
    TypeTag element_type;  // for generic containers (Array<T>, etc.)
    std::vector<std::pair<std::string, TypeTag>> fields; // for structs/classes
};

// ----- IR → Bytecode Mapping -----
// Records how compiler IR maps to bytecode for debugging

struct IRMapping {
    u32 ir_node_id;
    u32 bytecode_offset;
    u32 source_line;
    u32 source_column;
};

// ----- Module Binary Format -----
// Grey bytecode module format (.greyc)

static constexpr u32 GREY_MAGIC = 0x47524559; // "GREY"
static constexpr u16 GREY_VERSION_MAJOR = 1;
static constexpr u16 GREY_VERSION_MINOR = 0;

struct ModuleHeader {
    u32 magic;
    u16 version_major;
    u16 version_minor;
    u32 flags;
    u32 constant_pool_offset;
    u32 constant_pool_size;
    u32 code_offset;
    u32 code_size;
    u32 function_table_offset;
    u32 function_count;
    u32 string_table_offset;
    u32 string_count;
    u32 type_table_offset;
    u32 type_count;
    u32 debug_info_offset;
    u32 debug_info_size;
};

struct FunctionEntry {
    u32 name_index;        // index into string table
    u16 arity;
    u16 local_count;
    u16 upvalue_count;
    u32 code_offset;       // offset into code section
    u32 code_size;
    u32 constant_offset;   // offset into constant pool
    u32 constant_count;
    CallingConvention cc;
};

// ----- ABI Compatibility -----

struct ABIInfo {
    u16 version_major;
    u16 version_minor;
    CallingConvention default_cc;
    u8  pointer_size;      // 4 or 8
    u8  endianness;        // 0 = little, 1 = big
    u8  value_size;        // sizeof(Value)
    u8  alignment;

    bool is_compatible(const ABIInfo& other) const {
        return version_major == other.version_major &&
               pointer_size == other.pointer_size &&
               endianness == other.endianness &&
               value_size == other.value_size;
    }
};

inline ABIInfo current_abi() {
    ABIInfo abi;
    abi.version_major = GREY_VERSION_MAJOR;
    abi.version_minor = GREY_VERSION_MINOR;
    abi.default_cc = CallingConvention::GreyDefault;
    abi.pointer_size = sizeof(void*);
    abi.endianness = 0; // assume little-endian
    abi.value_size = sizeof(Value);
    abi.alignment = alignof(Value);
    return abi;
}

// ----- Module Serialization -----

class ModuleWriter {
public:
    ModuleWriter() = default;

    void begin();
    void add_function(const FunctionEntry& fn, const Chunk& chunk);
    void add_string(const std::string& s);
    void add_type(const TypeInfo& type);
    void add_debug_info(const std::vector<IRMapping>& mappings);
    std::vector<byte> finish();

private:
    std::vector<byte> buffer_;
    std::vector<FunctionEntry> functions_;
    std::vector<Chunk> chunks_;
    std::vector<std::string> strings_;
    std::vector<TypeInfo> types_;
    std::vector<IRMapping> debug_mappings_;
};

class ModuleReader {
public:
    explicit ModuleReader(const std::vector<byte>& data);

    bool validate() const;
    ModuleHeader header() const { return header_; }
    ABIInfo abi() const;

    std::vector<FunctionEntry> functions() const;
    std::string get_string(u32 index) const;
    TypeInfo get_type(u32 index) const;
    Chunk get_function_chunk(u32 index) const;
    std::vector<IRMapping> get_debug_info() const;

private:
    std::vector<byte> data_;
    ModuleHeader header_{};
};

} // namespace grey
