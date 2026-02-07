// Grey Runtime - Standard Library: IO
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/objects.h"

#include <fstream>
#include <sstream>
#include <cstring>
#include <algorithm>

namespace grey {

class VM;

// ============================================================
// Stream abstraction
// ============================================================

class Stream {
public:
    virtual ~Stream() = default;

    virtual size_t read(byte* buffer, size_t count) = 0;
    virtual size_t write(const byte* buffer, size_t count) = 0;
    virtual void   flush() = 0;
    virtual void   close() = 0;
    virtual bool   is_open() const = 0;
    virtual size_t position() const = 0;
    virtual void   seek(size_t pos) = 0;
    virtual size_t size() const = 0;
    virtual bool   eof() const = 0;
};

// ============================================================
// File Stream
// ============================================================

class FileStream : public Stream {
public:
    enum Mode { Read, Write, ReadWrite, Append };

    FileStream() = default;
    FileStream(const std::string& path, Mode mode) { open(path, mode); }
    ~FileStream() override { close(); }

    bool open(const std::string& path, Mode mode) {
        std::ios::openmode m = std::ios::binary;
        switch (mode) {
            case Read:      m |= std::ios::in; break;
            case Write:     m |= std::ios::out | std::ios::trunc; break;
            case ReadWrite: m |= std::ios::in | std::ios::out; break;
            case Append:    m |= std::ios::out | std::ios::app; break;
        }
        file_.open(path, m);
        path_ = path;
        return file_.is_open();
    }

    size_t read(byte* buffer, size_t count) override {
        file_.read(reinterpret_cast<char*>(buffer), count);
        return static_cast<size_t>(file_.gcount());
    }

    size_t write(const byte* buffer, size_t count) override {
        file_.write(reinterpret_cast<const char*>(buffer), count);
        return count;
    }

    void flush() override { file_.flush(); }
    void close() override { if (file_.is_open()) file_.close(); }
    bool is_open() const override { return file_.is_open(); }

    size_t position() const override {
        // const_cast for tellg
        return static_cast<size_t>(
            const_cast<std::fstream&>(file_).tellg());
    }

    void seek(size_t pos) override {
        file_.seekg(pos);
        file_.seekp(pos);
    }

    size_t size() const override {
        auto& f = const_cast<std::fstream&>(file_);
        auto cur = f.tellg();
        f.seekg(0, std::ios::end);
        auto s = f.tellg();
        f.seekg(cur);
        return static_cast<size_t>(s);
    }

    bool eof() const override { return file_.eof(); }

    const std::string& path() const { return path_; }

private:
    std::fstream file_;
    std::string path_;
};

// ============================================================
// Memory Stream (Buffer)
// ============================================================

class MemoryStream : public Stream {
public:
    MemoryStream() = default;
    explicit MemoryStream(const std::vector<byte>& data) : data_(data) {}

    size_t read(byte* buffer, size_t count) override {
        size_t available = data_.size() - pos_;
        size_t to_read = std::min(count, available);
        std::memcpy(buffer, data_.data() + pos_, to_read);
        pos_ += to_read;
        return to_read;
    }

    size_t write(const byte* buffer, size_t count) override {
        if (pos_ + count > data_.size()) data_.resize(pos_ + count);
        std::memcpy(data_.data() + pos_, buffer, count);
        pos_ += count;
        return count;
    }

    void flush() override {}
    void close() override {}
    bool is_open() const override { return true; }
    size_t position() const override { return pos_; }
    void seek(size_t pos) override { pos_ = std::min(pos, data_.size()); }
    size_t size() const override { return data_.size(); }
    bool eof() const override { return pos_ >= data_.size(); }

    const std::vector<byte>& data() const { return data_; }
    std::vector<byte>& data() { return data_; }

private:
    std::vector<byte> data_;
    size_t pos_ = 0;
};

// ============================================================
// Serialization (simple binary format)
// ============================================================

class Serializer {
public:
    void write_u8(u8 v) { data_.push_back(v); }
    void write_u16(u16 v) { write_u8(v >> 8); write_u8(v & 0xFF); }
    void write_u32(u32 v) { write_u16(v >> 16); write_u16(v & 0xFFFF); }
    void write_u64(u64 v) { write_u32(v >> 32); write_u32(v & 0xFFFFFFFF); }
    void write_f64(f64 v) { u64 bits; std::memcpy(&bits, &v, 8); write_u64(bits); }

    void write_string(const std::string& s) {
        write_u32(static_cast<u32>(s.size()));
        for (char c : s) write_u8(static_cast<u8>(c));
    }

    void write_bytes(const byte* data, size_t len) {
        write_u32(static_cast<u32>(len));
        for (size_t i = 0; i < len; i++) write_u8(data[i]);
    }

    void write_value(Value v) { write_u64(v.bits); }

    const std::vector<byte>& data() const { return data_; }

private:
    std::vector<byte> data_;
};

class Deserializer {
public:
    explicit Deserializer(const std::vector<byte>& data) : data_(data) {}
    explicit Deserializer(const byte* data, size_t len) : data_(data, data + len) {}

    u8  read_u8()  { return data_[pos_++]; }
    u16 read_u16() { return (static_cast<u16>(read_u8()) << 8) | read_u8(); }
    u32 read_u32() { return (static_cast<u32>(read_u16()) << 16) | read_u16(); }
    u64 read_u64() { return (static_cast<u64>(read_u32()) << 32) | read_u32(); }
    f64 read_f64() { u64 bits = read_u64(); f64 v; std::memcpy(&v, &bits, 8); return v; }

    std::string read_string() {
        u32 len = read_u32();
        std::string s(len, '\0');
        for (u32 i = 0; i < len; i++) s[i] = static_cast<char>(read_u8());
        return s;
    }

    std::vector<byte> read_bytes() {
        u32 len = read_u32();
        std::vector<byte> result(len);
        for (u32 i = 0; i < len; i++) result[i] = read_u8();
        return result;
    }

    Value read_value() { Value v; v.bits = read_u64(); return v; }

    bool at_end() const { return pos_ >= data_.size(); }
    size_t position() const { return pos_; }
    size_t remaining() const { return data_.size() - pos_; }

private:
    std::vector<byte> data_;
    size_t pos_ = 0;
};

// ============================================================
// Register IO natives
// ============================================================

void register_io_natives(VM& vm);

} // namespace grey
