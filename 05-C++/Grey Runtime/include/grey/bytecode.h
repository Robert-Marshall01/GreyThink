// Grey Runtime - Bytecode Opcodes
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"

namespace grey {

// ----- Bytecode Opcodes -----
enum class OpCode : u8 {
    // Stack operations
    NOP = 0x00,
    PUSH_NIL,
    PUSH_TRUE,
    PUSH_FALSE,
    PUSH_CONST,       // push constant from constant pool (u16 index)
    PUSH_CONST_LONG,  // push constant (u32 index)
    PUSH_INT,         // push immediate i32
    PUSH_ZERO,
    PUSH_ONE,
    POP,
    DUP,
    DUP2,
    SWAP,
    ROT3,             // rotate top 3: [a b c] -> [c a b]

    // Arithmetic
    ADD = 0x20,
    SUB,
    MUL,
    DIV,
    MOD,
    NEG,
    POW,
    FLOOR_DIV,
    BIT_AND,
    BIT_OR,
    BIT_XOR,
    BIT_NOT,
    SHL,
    SHR,

    // Comparison
    EQ = 0x30,
    NEQ,
    LT,
    LTE,
    GT,
    GTE,

    // Logical
    NOT = 0x38,
    AND,
    OR,

    // Variables
    LOAD_LOCAL = 0x40,    // load local variable (u16 slot)
    STORE_LOCAL,          // store local variable (u16 slot)
    LOAD_GLOBAL,          // load global (u16 name index)
    STORE_GLOBAL,         // store global (u16 name index)
    LOAD_UPVALUE,         // load closure upvalue (u16 index)
    STORE_UPVALUE,        // store closure upvalue (u16 index)
    CLOSE_UPVALUE,        // close upvalue on stack top
    LOAD_FIELD,           // load field from object (u16 name index)
    STORE_FIELD,          // store field to object (u16 name index)
    LOAD_INDEX,           // obj[index]
    STORE_INDEX,          // obj[index] = val

    // Control flow
    JUMP = 0x60,          // unconditional jump (i16 offset)
    JUMP_LONG,            // unconditional jump (i32 offset)
    JUMP_IF_TRUE,         // conditional jump (i16 offset)
    JUMP_IF_FALSE,        // conditional jump (i16 offset)
    JUMP_IF_NIL,          // jump if nil
    LOOP,                 // loop back (u16 offset)

    // Functions
    CALL = 0x70,          // call function (u8 arg_count)
    CALL_METHOD,          // call method (u16 name, u8 arg_count)
    RETURN,               // return from function
    RETURN_NIL,           // return nil
    CLOSURE,              // create closure (u16 func index, upvalue descriptors)
    BIND_METHOD,          // bind method to class

    // Objects & Classes
    NEW_CLASS = 0x80,     // create class (u16 name index)
    INHERIT,              // inherit from superclass
    NEW_INSTANCE,         // instantiate class
    NEW_ARRAY,            // create array (u16 element count)
    NEW_MAP,              // create map (u16 entry count)
    NEW_SET,              // create set (u16 element count)

    // Modules
    IMPORT = 0x90,        // import module (u16 name index)
    IMPORT_FROM,          // import symbol from module (u16 mod, u16 sym)
    EXPORT,               // export symbol (u16 name index)

    // Concurrency
    SPAWN = 0xA0,         // spawn fiber/task
    YIELD,                // yield fiber
    AWAIT,                // await future
    SEND,                 // send to channel
    RECV,                 // receive from channel

    // Error handling
    TRY_BEGIN = 0xB0,     // begin try block (u16 catch offset, u16 finally offset)
    TRY_END,              // end try block
    THROW,                // throw exception
    CATCH,                // catch handler entry

    // Type operations
    TYPE_CHECK = 0xC0,    // check type (u16 type index)
    TYPE_CAST,            // cast type (u16 type index)
    INSTANCEOF,           // instanceof check

    // Iterator
    ITER_BEGIN = 0xD0,    // begin iteration
    ITER_NEXT,            // get next (jump if done, i16 offset)
    ITER_CLOSE,           // close iterator

    // Debugging
    DEBUG_BREAK = 0xF0,   // debugger breakpoint
    DEBUG_LINE,           // line number info (u16 line)
    DEBUG_PRINT,          // debug print top of stack

    // System
    HALT = 0xFF,          // stop execution
};

// ----- Backward-compatible opcode aliases -----
namespace OpAlias {
    static constexpr auto OP_NIL       = OpCode::PUSH_NIL;
    static constexpr auto OP_TRUE      = OpCode::PUSH_TRUE;
    static constexpr auto OP_FALSE     = OpCode::PUSH_FALSE;
    static constexpr auto OP_CONSTANT  = OpCode::PUSH_CONST;
    static constexpr auto OP_POP       = OpCode::POP;
    static constexpr auto OP_ADD       = OpCode::ADD;
    static constexpr auto OP_SUBTRACT  = OpCode::SUB;
    static constexpr auto OP_MULTIPLY  = OpCode::MUL;
    static constexpr auto OP_DIVIDE    = OpCode::DIV;
    static constexpr auto OP_NEGATE    = OpCode::NEG;
    static constexpr auto OP_NOT       = OpCode::NOT;
    static constexpr auto OP_EQUAL     = OpCode::EQ;
    static constexpr auto OP_LESS      = OpCode::LT;
    static constexpr auto OP_GREATER   = OpCode::GT;
    static constexpr auto OP_RETURN    = OpCode::RETURN;
    static constexpr auto OP_CALL      = OpCode::CALL;
}

inline const char* opcode_name(OpCode op) {
    switch (op) {
        case OpCode::NOP:           return "NOP";
        case OpCode::PUSH_NIL:      return "PUSH_NIL";
        case OpCode::PUSH_TRUE:     return "PUSH_TRUE";
        case OpCode::PUSH_FALSE:    return "PUSH_FALSE";
        case OpCode::PUSH_CONST:    return "PUSH_CONST";
        case OpCode::PUSH_CONST_LONG: return "PUSH_CONST_LONG";
        case OpCode::PUSH_INT:      return "PUSH_INT";
        case OpCode::PUSH_ZERO:     return "PUSH_ZERO";
        case OpCode::PUSH_ONE:      return "PUSH_ONE";
        case OpCode::POP:           return "POP";
        case OpCode::DUP:           return "DUP";
        case OpCode::DUP2:          return "DUP2";
        case OpCode::SWAP:          return "SWAP";
        case OpCode::ROT3:          return "ROT3";
        case OpCode::ADD:           return "ADD";
        case OpCode::SUB:           return "SUB";
        case OpCode::MUL:           return "MUL";
        case OpCode::DIV:           return "DIV";
        case OpCode::MOD:           return "MOD";
        case OpCode::NEG:           return "NEG";
        case OpCode::POW:           return "POW";
        case OpCode::FLOOR_DIV:     return "FLOOR_DIV";
        case OpCode::BIT_AND:       return "BIT_AND";
        case OpCode::BIT_OR:        return "BIT_OR";
        case OpCode::BIT_XOR:       return "BIT_XOR";
        case OpCode::BIT_NOT:       return "BIT_NOT";
        case OpCode::SHL:           return "SHL";
        case OpCode::SHR:           return "SHR";
        case OpCode::EQ:            return "EQ";
        case OpCode::NEQ:           return "NEQ";
        case OpCode::LT:            return "LT";
        case OpCode::LTE:           return "LTE";
        case OpCode::GT:            return "GT";
        case OpCode::GTE:           return "GTE";
        case OpCode::NOT:           return "NOT";
        case OpCode::AND:           return "AND";
        case OpCode::OR:            return "OR";
        case OpCode::LOAD_LOCAL:    return "LOAD_LOCAL";
        case OpCode::STORE_LOCAL:   return "STORE_LOCAL";
        case OpCode::LOAD_GLOBAL:   return "LOAD_GLOBAL";
        case OpCode::STORE_GLOBAL:  return "STORE_GLOBAL";
        case OpCode::LOAD_UPVALUE:  return "LOAD_UPVALUE";
        case OpCode::STORE_UPVALUE: return "STORE_UPVALUE";
        case OpCode::CLOSE_UPVALUE: return "CLOSE_UPVALUE";
        case OpCode::LOAD_FIELD:    return "LOAD_FIELD";
        case OpCode::STORE_FIELD:   return "STORE_FIELD";
        case OpCode::LOAD_INDEX:    return "LOAD_INDEX";
        case OpCode::STORE_INDEX:   return "STORE_INDEX";
        case OpCode::JUMP:          return "JUMP";
        case OpCode::JUMP_LONG:     return "JUMP_LONG";
        case OpCode::JUMP_IF_TRUE:  return "JUMP_IF_TRUE";
        case OpCode::JUMP_IF_FALSE: return "JUMP_IF_FALSE";
        case OpCode::JUMP_IF_NIL:   return "JUMP_IF_NIL";
        case OpCode::LOOP:          return "LOOP";
        case OpCode::CALL:          return "CALL";
        case OpCode::CALL_METHOD:   return "CALL_METHOD";
        case OpCode::RETURN:        return "RETURN";
        case OpCode::RETURN_NIL:    return "RETURN_NIL";
        case OpCode::CLOSURE:       return "CLOSURE";
        case OpCode::BIND_METHOD:   return "BIND_METHOD";
        case OpCode::NEW_CLASS:     return "NEW_CLASS";
        case OpCode::INHERIT:       return "INHERIT";
        case OpCode::NEW_INSTANCE:  return "NEW_INSTANCE";
        case OpCode::NEW_ARRAY:     return "NEW_ARRAY";
        case OpCode::NEW_MAP:       return "NEW_MAP";
        case OpCode::NEW_SET:       return "NEW_SET";
        case OpCode::IMPORT:        return "IMPORT";
        case OpCode::IMPORT_FROM:   return "IMPORT_FROM";
        case OpCode::EXPORT:        return "EXPORT";
        case OpCode::SPAWN:         return "SPAWN";
        case OpCode::YIELD:         return "YIELD";
        case OpCode::AWAIT:         return "AWAIT";
        case OpCode::SEND:          return "SEND";
        case OpCode::RECV:          return "RECV";
        case OpCode::TRY_BEGIN:     return "TRY_BEGIN";
        case OpCode::TRY_END:       return "TRY_END";
        case OpCode::THROW:         return "THROW";
        case OpCode::CATCH:         return "CATCH";
        case OpCode::TYPE_CHECK:    return "TYPE_CHECK";
        case OpCode::TYPE_CAST:     return "TYPE_CAST";
        case OpCode::INSTANCEOF:    return "INSTANCEOF";
        case OpCode::ITER_BEGIN:    return "ITER_BEGIN";
        case OpCode::ITER_NEXT:     return "ITER_NEXT";
        case OpCode::ITER_CLOSE:    return "ITER_CLOSE";
        case OpCode::DEBUG_BREAK:   return "DEBUG_BREAK";
        case OpCode::DEBUG_LINE:    return "DEBUG_LINE";
        case OpCode::DEBUG_PRINT:   return "DEBUG_PRINT";
        case OpCode::HALT:          return "HALT";
        default:                    return "UNKNOWN";
    }
}

// ----- Bytecode Chunk -----
struct LineInfo {
    u32 offset;
    u32 line;
};

struct Chunk {
    std::vector<u8>    code;        // bytecode
    std::vector<Value> constants;   // constant pool
    std::vector<LineInfo> lines;    // line info for debugging
    std::string source_name;       // source file name

    // Emit bytecode
    u32 emit(u8 byte, u32 line) {
        u32 offset = static_cast<u32>(code.size());
        code.push_back(byte);
        add_line(offset, line);
        return offset;
    }

    u32 emit(OpCode op, u32 line) { return emit(static_cast<u8>(op), line); }

    u32 emit_u16(u16 val, u32 line) {
        u32 offset = emit(static_cast<u8>((val >> 8) & 0xFF), line);
        emit(static_cast<u8>(val & 0xFF), line);
        return offset;
    }

    u32 emit_i16(i16 val, u32 line) {
        return emit_u16(static_cast<u16>(val), line);
    }

    u32 emit_u32(u32 val, u32 line) {
        u32 offset = emit(static_cast<u8>((val >> 24) & 0xFF), line);
        emit(static_cast<u8>((val >> 16) & 0xFF), line);
        emit(static_cast<u8>((val >> 8) & 0xFF), line);
        emit(static_cast<u8>(val & 0xFF), line);
        return offset;
    }

    u16 add_constant(Value val) {
        constants.push_back(val);
        return static_cast<u16>(constants.size() - 1);
    }

    // Emit OP_CONSTANT (PUSH_CONST) + constant index
    u32 emit_constant(Value val, u32 line) {
        u16 idx = add_constant(val);
        u32 offset = emit(OpCode::PUSH_CONST, line);
        emit_u16(idx, line);
        return offset;
    }

    void add_line(u32 offset, u32 line) {
        if (lines.empty() || lines.back().line != line) {
            lines.push_back({offset, line});
        }
    }

    u32 get_line(u32 offset) const {
        for (auto it = lines.rbegin(); it != lines.rend(); ++it) {
            if (it->offset <= offset) return it->line;
        }
        return 0;
    }

    // Read operands
    u16 read_u16(u32& ip) const {
        u16 val = (static_cast<u16>(code[ip]) << 8) | code[ip + 1];
        ip += 2;
        return val;
    }

    i16 read_i16(u32& ip) const {
        return static_cast<i16>(read_u16(ip));
    }

    u32 read_u32(u32& ip) const {
        u32 val = (static_cast<u32>(code[ip]) << 24) |
                  (static_cast<u32>(code[ip + 1]) << 16) |
                  (static_cast<u32>(code[ip + 2]) << 8) |
                  code[ip + 3];
        ip += 4;
        return val;
    }

    i32 read_i32(u32& ip) const {
        return static_cast<i32>(read_u32(ip));
    }
};

} // namespace grey
