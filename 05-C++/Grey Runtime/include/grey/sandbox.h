// Grey Runtime - Determinism & Sandboxing Layer
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include <unordered_set>

namespace grey {

// Forward
class VM;

// ============================================================
// 6.1 Deterministic Execution Mode
// ============================================================

class DeterministicEngine {
public:
    DeterministicEngine() = default;

    // Enable deterministic mode
    void enable() { enabled_ = true; }
    void disable() { enabled_ = false; }
    bool is_enabled() const { return enabled_; }

    // Deterministic random (seeded PRNG)
    void set_seed(u64 seed) { rng_state_ = seed; }
    u64 next_random() {
        // xorshift64
        rng_state_ ^= rng_state_ << 13;
        rng_state_ ^= rng_state_ >> 7;
        rng_state_ ^= rng_state_ << 17;
        return rng_state_;
    }

    // Deterministic time (virtual clock)
    u64 virtual_time() const { return virtual_time_; }
    void advance_time(u64 ticks) { virtual_time_ += ticks; }
    void set_time(u64 t) { virtual_time_ = t; }

    // Deterministic memory allocation order
    u64 next_alloc_id() { return alloc_counter_++; }

    // Execution step counting for replay
    u64 step_count() const { return step_count_; }
    void increment_step() { step_count_++; }

    // Validate — throw if non-deterministic operation attempted
    void check_syscall(const std::string& name) const {
        if (!enabled_) return;
        if (blocked_syscalls_.count(name)) {
            throw RuntimeError(ErrorCode::DeterminismViolation,
                "Non-deterministic syscall blocked in deterministic mode: " + name);
        }
    }

    void block_syscall(const std::string& name) {
        blocked_syscalls_.insert(name);
    }

    // Default blocked syscalls for deterministic mode
    void apply_defaults() {
        block_syscall("time.now");
        block_syscall("random");
        block_syscall("thread.spawn");
        block_syscall("file.read");
        block_syscall("file.write");
        block_syscall("net.connect");
        block_syscall("net.listen");
        block_syscall("env.get");
        block_syscall("process.exec");
    }

private:
    bool enabled_ = false;
    u64  rng_state_ = 0x12345678ABCDEF00ULL;
    u64  virtual_time_ = 0;
    u64  alloc_counter_ = 0;
    u64  step_count_ = 0;
    std::unordered_set<std::string> blocked_syscalls_;
};

// ============================================================
// 6.2 Sandbox
// ============================================================

// Capability-based security
enum class Capability : u32 {
    None          = 0,
    FileRead      = 1 << 0,
    FileWrite     = 1 << 1,
    NetworkAccess = 1 << 2,
    ProcessSpawn  = 1 << 3,
    EnvAccess     = 1 << 4,
    TimeAccess    = 1 << 5,
    CryptoAccess  = 1 << 6,
    SystemInfo    = 1 << 7,
    Threads       = 1 << 8,

    // Convenience groups
    AllIO = FileRead | FileWrite | NetworkAccess,
    All   = 0xFFFFFFFF,
};

inline Capability operator|(Capability a, Capability b) {
    return static_cast<Capability>(static_cast<u32>(a) | static_cast<u32>(b));
}
inline Capability operator&(Capability a, Capability b) {
    return static_cast<Capability>(static_cast<u32>(a) & static_cast<u32>(b));
}
inline bool has_cap(Capability set, Capability cap) {
    return (static_cast<u32>(set) & static_cast<u32>(cap)) != 0;
}

class Sandbox {
public:
    Sandbox() = default;

    // ----- Resource Limits -----
    void set_max_memory(size_t bytes) { max_memory_ = bytes; }
    void set_max_cpu_cycles(size_t cycles) { max_cycles_ = cycles; }
    void set_max_stack_depth(size_t depth) { max_stack_depth_ = depth; }
    void set_max_execution_time_ms(size_t ms) { max_time_ms_ = ms; }
    void set_max_io_bytes(size_t bytes) { max_io_bytes_ = bytes; }
    void set_max_open_files(size_t n) { max_open_files_ = n; }

    // ----- Capabilities -----
    void set_capabilities(Capability caps) { capabilities_ = caps; }
    void add_capability(Capability cap) { capabilities_ = capabilities_ | cap; }
    void remove_capability(Capability cap) {
        capabilities_ = static_cast<Capability>(
            static_cast<u32>(capabilities_) & ~static_cast<u32>(cap));
    }

    // ----- Allowed paths (file system isolation) -----
    void add_allowed_path(const std::string& path, bool writable = false) {
        allowed_paths_.push_back({path, writable});
    }

    // ----- Checks -----
    void check_capability(Capability cap) const {
        if (!has_cap(capabilities_, cap)) {
            throw RuntimeError(ErrorCode::SandboxViolation,
                "Sandbox: capability not granted");
        }
    }

    void check_memory(size_t current_bytes) const {
        if (max_memory_ > 0 && current_bytes > max_memory_) {
            throw RuntimeError(ErrorCode::OutOfMemory,
                "Sandbox: memory limit exceeded (" + std::to_string(current_bytes) +
                " > " + std::to_string(max_memory_) + ")");
        }
    }

    void check_cycles(size_t current_cycles) const {
        if (max_cycles_ > 0 && current_cycles > max_cycles_) {
            throw RuntimeError(ErrorCode::SandboxViolation,
                "Sandbox: CPU cycle limit exceeded");
        }
    }

    void check_stack_depth(size_t depth) const {
        if (max_stack_depth_ > 0 && depth > max_stack_depth_) {
            throw RuntimeError(ErrorCode::StackOverflow,
                "Sandbox: stack depth limit exceeded");
        }
    }

    void check_file_path(const std::string& path, bool writing) const {
        check_capability(writing ? Capability::FileWrite : Capability::FileRead);
        if (allowed_paths_.empty()) return; // no path restrictions
        for (auto& [allowed, writable] : allowed_paths_) {
            if (path.find(allowed) == 0) {
                if (writing && !writable) {
                    throw RuntimeError(ErrorCode::PermissionDenied,
                        "Sandbox: write not allowed to path: " + path);
                }
                return;
            }
        }
        throw RuntimeError(ErrorCode::PermissionDenied,
            "Sandbox: path not allowed: " + path);
    }

    void track_io(size_t bytes) {
        io_bytes_used_ += bytes;
        if (max_io_bytes_ > 0 && io_bytes_used_ > max_io_bytes_) {
            throw RuntimeError(ErrorCode::SandboxViolation,
                "Sandbox: IO byte limit exceeded");
        }
    }

    // ----- Isolation boundary -----
    struct IsolationContext {
        Capability capabilities;
        size_t max_memory;
        size_t max_cycles;
    };

    IsolationContext snapshot() const {
        return {capabilities_, max_memory_, max_cycles_};
    }

    void restore(const IsolationContext& ctx) {
        capabilities_ = ctx.capabilities;
        max_memory_ = ctx.max_memory;
        max_cycles_ = ctx.max_cycles;
    }

    // ----- Stats -----
    size_t io_bytes_used() const { return io_bytes_used_; }

private:
    Capability capabilities_ = Capability::All;
    size_t max_memory_ = 0;
    size_t max_cycles_ = 0;
    size_t max_stack_depth_ = 0;
    size_t max_time_ms_ = 0;
    size_t max_io_bytes_ = 0;
    size_t max_open_files_ = 256;
    size_t io_bytes_used_ = 0;

    struct PathEntry {
        std::string path;
        bool writable;
    };
    std::vector<PathEntry> allowed_paths_;
};

} // namespace grey
