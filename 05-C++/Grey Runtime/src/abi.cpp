// Grey Runtime - ABI / Module Serialization Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/abi.h"

#include <cstring>
#include <stdexcept>

namespace grey {

// ============================================================
// Helper: binary write/read utilities
// ============================================================

namespace {

void write_u8(std::vector<byte>& buf, u8 val) {
    buf.push_back(val);
}

void write_u16(std::vector<byte>& buf, u16 val) {
    buf.push_back(static_cast<byte>(val & 0xFF));
    buf.push_back(static_cast<byte>((val >> 8) & 0xFF));
}

void write_u32(std::vector<byte>& buf, u32 val) {
    buf.push_back(static_cast<byte>(val & 0xFF));
    buf.push_back(static_cast<byte>((val >> 8) & 0xFF));
    buf.push_back(static_cast<byte>((val >> 16) & 0xFF));
    buf.push_back(static_cast<byte>((val >> 24) & 0xFF));
}

void write_u64(std::vector<byte>& buf, u64 val) {
    for (int i = 0; i < 8; i++) {
        buf.push_back(static_cast<byte>((val >> (i * 8)) & 0xFF));
    }
}

void write_f64(std::vector<byte>& buf, f64 val) {
    u64 bits;
    std::memcpy(&bits, &val, sizeof(bits));
    write_u64(buf, bits);
}

void write_bytes(std::vector<byte>& buf, const byte* data, size_t len) {
    buf.insert(buf.end(), data, data + len);
}

void write_string(std::vector<byte>& buf, const std::string& s) {
    write_u32(buf, static_cast<u32>(s.size()));
    write_bytes(buf, reinterpret_cast<const byte*>(s.data()), s.size());
}

void write_at_u32(std::vector<byte>& buf, size_t offset, u32 val) {
    buf[offset + 0] = static_cast<byte>(val & 0xFF);
    buf[offset + 1] = static_cast<byte>((val >> 8) & 0xFF);
    buf[offset + 2] = static_cast<byte>((val >> 16) & 0xFF);
    buf[offset + 3] = static_cast<byte>((val >> 24) & 0xFF);
}

u8 read_u8(const byte* p) {
    return static_cast<u8>(p[0]);
}

u16 read_u16(const byte* p) {
    return static_cast<u16>(p[0]) |
           (static_cast<u16>(p[1]) << 8);
}

u32 read_u32(const byte* p) {
    return static_cast<u32>(p[0]) |
           (static_cast<u32>(p[1]) << 8) |
           (static_cast<u32>(p[2]) << 16) |
           (static_cast<u32>(p[3]) << 24);
}

u64 read_u64(const byte* p) {
    u64 val = 0;
    for (int i = 0; i < 8; i++) {
        val |= static_cast<u64>(p[i]) << (i * 8);
    }
    return val;
}

f64 read_f64(const byte* p) {
    u64 bits = read_u64(p);
    f64 val;
    std::memcpy(&val, &bits, sizeof(val));
    return val;
}

std::string read_string(const byte* p, size_t& offset) {
    u32 len = read_u32(p + offset);
    offset += 4;
    std::string s(reinterpret_cast<const char*>(p + offset), len);
    offset += len;
    return s;
}

} // anonymous namespace

// ============================================================
// ModuleWriter
// ============================================================

void ModuleWriter::begin() {
    buffer_.clear();
    functions_.clear();
    strings_.clear();
    types_.clear();
    debug_mappings_.clear();

    // Reserve space for the header (will be patched in finish())
    buffer_.resize(sizeof(ModuleHeader), 0);
}

void ModuleWriter::add_function(const FunctionEntry& fn, const Chunk& chunk) {
    FunctionEntry entry = fn;
    // Record name into string table if needed
    // The caller is responsible for setting name_index properly
    // We store the chunk data alongside
    (void)chunk; // chunk data will be serialized in finish()
    functions_.push_back(entry);
    // Store chunk data: we keep a copy alongside
    // We'll serialize chunks inline in finish()
    // For now, store bytecode and constants from the chunk
    // We need a parallel storage for chunk data
    chunks_.push_back(chunk);
}

void ModuleWriter::add_string(const std::string& s) {
    strings_.push_back(s);
}

void ModuleWriter::add_type(const TypeInfo& type) {
    types_.push_back(type);
}

void ModuleWriter::add_debug_info(const std::vector<IRMapping>& mappings) {
    debug_mappings_.insert(debug_mappings_.end(), mappings.begin(), mappings.end());
}

std::vector<byte> ModuleWriter::finish() {
    buffer_.clear();
    // Reserve header space
    buffer_.resize(sizeof(ModuleHeader), 0);

    ModuleHeader hdr{};
    hdr.magic = GREY_MAGIC;
    hdr.version_major = GREY_VERSION_MAJOR;
    hdr.version_minor = GREY_VERSION_MINOR;
    hdr.flags = 0;

    // ----- String Table -----
    hdr.string_table_offset = static_cast<u32>(buffer_.size());
    hdr.string_count = static_cast<u32>(strings_.size());
    for (auto& s : strings_) {
        write_string(buffer_, s);
    }

    // ----- Constant Pool -----
    // Collect all constants from all chunks into a merged pool
    hdr.constant_pool_offset = static_cast<u32>(buffer_.size());
    std::vector<u32> chunk_const_offsets; // per-function constant offset
    u32 total_constants = 0;
    for (size_t i = 0; i < chunks_.size(); i++) {
        chunk_const_offsets.push_back(total_constants);
        auto& chunk = chunks_[i];
        for (auto& val : chunk.constants) {
            // Serialize constant Value
            // Tag byte + data
            if (val.is_nil()) {
                write_u8(buffer_, 0); // tag: nil
            } else if (val.is_boolean()) {
                write_u8(buffer_, 1); // tag: bool
                write_u8(buffer_, val.as_boolean() ? 1 : 0);
            } else if (val.is_int()) {
                write_u8(buffer_, 2); // tag: int
                write_u64(buffer_, static_cast<u64>(val.as_int()));
            } else if (val.is_number()) {
                write_u8(buffer_, 3); // tag: number
                write_f64(buffer_, val.as_number());
            } else if (val.is_object()) {
                auto* obj = val.as_object<ObjHeader>();
                if (obj && obj->type == ObjType::String) {
                    write_u8(buffer_, 4); // tag: string
                    auto* str = static_cast<ObjString*>(obj);
                    write_string(buffer_, str->value);
                } else {
                    write_u8(buffer_, 0); // fallback: nil
                }
            } else {
                write_u8(buffer_, 0); // unknown → nil
            }
            total_constants++;
        }
    }
    hdr.constant_pool_size = static_cast<u32>(buffer_.size()) - hdr.constant_pool_offset;

    // ----- Code Section -----
    hdr.code_offset = static_cast<u32>(buffer_.size());
    std::vector<u32> chunk_code_offsets;
    std::vector<u32> chunk_code_sizes;
    for (size_t i = 0; i < chunks_.size(); i++) {
        chunk_code_offsets.push_back(static_cast<u32>(buffer_.size()) - hdr.code_offset);
        auto& chunk = chunks_[i];
        write_bytes(buffer_, chunk.code.data(), chunk.code.size());
        chunk_code_sizes.push_back(static_cast<u32>(chunk.code.size()));
    }
    hdr.code_size = static_cast<u32>(buffer_.size()) - hdr.code_offset;

    // ----- Function Table -----
    hdr.function_table_offset = static_cast<u32>(buffer_.size());
    hdr.function_count = static_cast<u32>(functions_.size());
    for (size_t i = 0; i < functions_.size(); i++) {
        auto& fn = functions_[i];
        write_u32(buffer_, fn.name_index);
        write_u16(buffer_, fn.arity);
        write_u16(buffer_, fn.local_count);
        write_u16(buffer_, fn.upvalue_count);
        write_u32(buffer_, i < chunk_code_offsets.size() ? chunk_code_offsets[i] : 0);
        write_u32(buffer_, i < chunk_code_sizes.size() ? chunk_code_sizes[i] : 0);
        write_u32(buffer_, i < chunk_const_offsets.size() ? chunk_const_offsets[i] : 0);
        write_u32(buffer_, i < chunks_.size() ? static_cast<u32>(chunks_[i].constants.size()) : 0);
        write_u8(buffer_, static_cast<u8>(fn.cc));
        // Padding to align
        write_u8(buffer_, 0);
        write_u16(buffer_, 0);
    }

    // ----- Type Table -----
    hdr.type_table_offset = static_cast<u32>(buffer_.size());
    hdr.type_count = static_cast<u32>(types_.size());
    for (auto& t : types_) {
        write_u16(buffer_, static_cast<u16>(t.tag));
        write_string(buffer_, t.name);
        write_u32(buffer_, t.size);
        write_u32(buffer_, t.alignment);
        write_u16(buffer_, static_cast<u16>(t.element_type));
        write_u32(buffer_, static_cast<u32>(t.fields.size()));
        for (auto& [name, tag] : t.fields) {
            write_string(buffer_, name);
            write_u16(buffer_, static_cast<u16>(tag));
        }
    }

    // ----- Debug Info -----
    hdr.debug_info_offset = static_cast<u32>(buffer_.size());
    for (auto& m : debug_mappings_) {
        write_u32(buffer_, m.ir_node_id);
        write_u32(buffer_, m.bytecode_offset);
        write_u32(buffer_, m.source_line);
        write_u32(buffer_, m.source_column);
    }
    hdr.debug_info_size = static_cast<u32>(buffer_.size()) - hdr.debug_info_offset;

    // ----- Patch Header -----
    std::memcpy(buffer_.data(), &hdr, sizeof(ModuleHeader));

    return buffer_;
}

// ============================================================
// ModuleReader
// ============================================================

ModuleReader::ModuleReader(const std::vector<byte>& data) : data_(data) {
    if (data.size() >= sizeof(ModuleHeader)) {
        std::memcpy(&header_, data.data(), sizeof(ModuleHeader));
    }
}

bool ModuleReader::validate() const {
    if (data_.size() < sizeof(ModuleHeader)) return false;
    if (header_.magic != GREY_MAGIC) return false;
    if (header_.version_major != GREY_VERSION_MAJOR) return false;
    return true;
}

ABIInfo ModuleReader::abi() const {
    return current_abi();
}

std::vector<FunctionEntry> ModuleReader::functions() const {
    std::vector<FunctionEntry> result;
    if (header_.function_count == 0) return result;

    size_t offset = header_.function_table_offset;
    for (u32 i = 0; i < header_.function_count; i++) {
        if (offset + 28 > data_.size()) break; // 28 bytes per entry

        FunctionEntry fn;
        fn.name_index = read_u32(data_.data() + offset); offset += 4;
        fn.arity = read_u16(data_.data() + offset); offset += 2;
        fn.local_count = read_u16(data_.data() + offset); offset += 2;
        fn.upvalue_count = read_u16(data_.data() + offset); offset += 2;
        fn.code_offset = read_u32(data_.data() + offset); offset += 4;
        fn.code_size = read_u32(data_.data() + offset); offset += 4;
        fn.constant_offset = read_u32(data_.data() + offset); offset += 4;
        fn.constant_count = read_u32(data_.data() + offset); offset += 4;
        fn.cc = static_cast<CallingConvention>(read_u8(data_.data() + offset)); offset += 1;
        offset += 3; // padding

        result.push_back(fn);
    }
    return result;
}

std::string ModuleReader::get_string(u32 index) const {
    size_t offset = header_.string_table_offset;
    for (u32 i = 0; i < header_.string_count; i++) {
        if (offset + 4 > data_.size()) return "";
        std::string s = read_string(data_.data(), offset);
        if (i == index) return s;
    }
    return "";
}

TypeInfo ModuleReader::get_type(u32 index) const {
    size_t offset = header_.type_table_offset;
    for (u32 i = 0; i < header_.type_count; i++) {
        if (offset + 2 > data_.size()) break;

        TypeInfo t;
        t.tag = static_cast<TypeTag>(read_u16(data_.data() + offset)); offset += 2;
        t.name = read_string(data_.data(), offset);
        if (offset + 12 > data_.size()) break;
        t.size = read_u32(data_.data() + offset); offset += 4;
        t.alignment = read_u32(data_.data() + offset); offset += 4;
        t.element_type = static_cast<TypeTag>(read_u16(data_.data() + offset)); offset += 2;
        u32 field_count = read_u32(data_.data() + offset); offset += 4;
        for (u32 f = 0; f < field_count; f++) {
            std::string fname = read_string(data_.data(), offset);
            TypeTag ftag = static_cast<TypeTag>(read_u16(data_.data() + offset)); offset += 2;
            t.fields.emplace_back(fname, ftag);
        }

        if (i == index) return t;
    }
    return {};
}

Chunk ModuleReader::get_function_chunk(u32 index) const {
    Chunk chunk;
    auto fns = functions();
    if (index >= fns.size()) return chunk;

    auto& fn = fns[index];

    // Read bytecode
    u32 abs_code_offset = header_.code_offset + fn.code_offset;
    if (abs_code_offset + fn.code_size <= data_.size()) {
        chunk.code.assign(
            data_.data() + abs_code_offset,
            data_.data() + abs_code_offset + fn.code_size);
    }

    // Read constants
    size_t offset = header_.constant_pool_offset;
    // Skip to the function's constant offset
    u32 const_idx = 0;
    while (const_idx < fn.constant_offset && offset < data_.size()) {
        u8 tag = read_u8(data_.data() + offset); offset += 1;
        switch (tag) {
            case 0: break; // nil
            case 1: offset += 1; break; // bool
            case 2: offset += 8; break; // int
            case 3: offset += 8; break; // number
            case 4: {
                u32 slen = read_u32(data_.data() + offset); offset += 4;
                offset += slen;
                break;
            }
            default: break;
        }
        const_idx++;
    }

    // Now read this function's constants
    for (u32 c = 0; c < fn.constant_count && offset < data_.size(); c++) {
        u8 tag = read_u8(data_.data() + offset); offset += 1;
        switch (tag) {
            case 0: // nil
                chunk.constants.push_back(Value::nil());
                break;
            case 1: { // bool
                bool b = read_u8(data_.data() + offset) != 0; offset += 1;
                chunk.constants.push_back(Value::boolean(b));
                break;
            }
            case 2: { // int
                i64 v = static_cast<i64>(read_u64(data_.data() + offset)); offset += 8;
                chunk.constants.push_back(Value::integer(v));
                break;
            }
            case 3: { // number
                f64 v = read_f64(data_.data() + offset); offset += 8;
                chunk.constants.push_back(Value::number(v));
                break;
            }
            case 4: { // string
                std::string s = read_string(data_.data(), offset);
                // NOTE: strings in constant pool during deserialization
                // need to be tracked by GC when loaded into VM
                // For now, store as nil placeholder - the module loader
                // will create proper ObjString objects
                chunk.constants.push_back(Value::nil());
                break;
            }
            default:
                chunk.constants.push_back(Value::nil());
                break;
        }
    }

    return chunk;
}

std::vector<IRMapping> ModuleReader::get_debug_info() const {
    std::vector<IRMapping> result;
    size_t offset = header_.debug_info_offset;
    size_t end = header_.debug_info_offset + header_.debug_info_size;

    while (offset + 16 <= end && offset + 16 <= data_.size()) {
        IRMapping m;
        m.ir_node_id = read_u32(data_.data() + offset); offset += 4;
        m.bytecode_offset = read_u32(data_.data() + offset); offset += 4;
        m.source_line = read_u32(data_.data() + offset); offset += 4;
        m.source_column = read_u32(data_.data() + offset); offset += 4;
        result.push_back(m);
    }
    return result;
}

} // namespace grey
