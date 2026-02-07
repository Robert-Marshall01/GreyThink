// Grey Runtime - Standard Library: System Interfaces
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"

#include <chrono>
#include <cstdlib>
#include <string>
#include <vector>
#include <unordered_map>

namespace grey {

class VM;

// ============================================================
// Time
// ============================================================

namespace time {

inline u64 now_ms() {
    return static_cast<u64>(
        std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::system_clock::now().time_since_epoch()
        ).count()
    );
}

inline u64 now_ns() {
    return static_cast<u64>(
        std::chrono::duration_cast<std::chrono::nanoseconds>(
            std::chrono::high_resolution_clock::now().time_since_epoch()
        ).count()
    );
}

inline u64 monotonic_ms() {
    return static_cast<u64>(
        std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now().time_since_epoch()
        ).count()
    );
}

struct Timer {
    std::chrono::high_resolution_clock::time_point start;

    Timer() : start(std::chrono::high_resolution_clock::now()) {}

    u64 elapsed_ms() const {
        auto now = std::chrono::high_resolution_clock::now();
        return std::chrono::duration_cast<std::chrono::milliseconds>(now - start).count();
    }

    u64 elapsed_ns() const {
        auto now = std::chrono::high_resolution_clock::now();
        return std::chrono::duration_cast<std::chrono::nanoseconds>(now - start).count();
    }

    void reset() { start = std::chrono::high_resolution_clock::now(); }
};

void sleep_ms(u64 ms);

} // namespace time

// ============================================================
// Environment
// ============================================================

namespace env {

std::string get(const std::string& key);
void set(const std::string& key, const std::string& value);
bool has(const std::string& key);
std::unordered_map<std::string, std::string> all();
std::string cwd();
bool set_cwd(const std::string& path);

} // namespace env

// ============================================================
// Process
// ============================================================

namespace process {

struct ProcessResult {
    int exit_code;
    std::string stdout_output;
    std::string stderr_output;
};

ProcessResult exec(const std::string& command);
ProcessResult exec(const std::string& program, const std::vector<std::string>& args);
void exit(int code);
int pid();
std::vector<std::string> args();

} // namespace process

// ============================================================
// OS Abstraction
// ============================================================

namespace os {

enum class Platform {
    Windows,
    Linux,
    MacOS,
    Unknown,
};

Platform current_platform();
std::string platform_name();
std::string arch_name();
size_t page_size();
size_t total_memory();
size_t available_memory();
u32 cpu_count();

} // namespace os

// ============================================================
// Register system natives
// ============================================================

void register_system_natives(VM& vm);

} // namespace grey
