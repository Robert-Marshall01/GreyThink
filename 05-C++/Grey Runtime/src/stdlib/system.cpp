// Grey Runtime - System Standard Library Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/stdlib/system.h"
#include "grey/vm.h"

#include <thread>
#include <cstdlib>
#include <filesystem>

#ifdef GREY_PLATFORM_WINDOWS
    #include <windows.h>
    #include <direct.h>
#else
    #include <unistd.h>
    #include <sys/sysinfo.h>
#endif

namespace grey {

// ============================================================
// Time
// ============================================================

namespace time {

void sleep_ms(u64 ms) {
    std::this_thread::sleep_for(std::chrono::milliseconds(ms));
}

} // namespace time

// ============================================================
// Environment
// ============================================================

namespace env {

std::string get(const std::string& key) {
#ifdef GREY_PLATFORM_WINDOWS
    char* val = nullptr;
    size_t len = 0;
    if (_dupenv_s(&val, &len, key.c_str()) == 0 && val) {
        std::string result(val);
        free(val);
        return result;
    }
    return "";
#else
    const char* val = std::getenv(key.c_str());
    return val ? std::string(val) : "";
#endif
}

void set(const std::string& key, const std::string& value) {
#ifdef GREY_PLATFORM_WINDOWS
    _putenv_s(key.c_str(), value.c_str());
#else
    setenv(key.c_str(), value.c_str(), 1);
#endif
}

bool has(const std::string& key) {
#ifdef GREY_PLATFORM_WINDOWS
    char* val = nullptr;
    size_t len = 0;
    bool exists = (_dupenv_s(&val, &len, key.c_str()) == 0 && val);
    if (val) free(val);
    return exists;
#else
    return std::getenv(key.c_str()) != nullptr;
#endif
}

std::string cwd() {
    return std::filesystem::current_path().string();
}

bool set_cwd(const std::string& path) {
    std::error_code ec;
    std::filesystem::current_path(path, ec);
    return !ec;
}

std::unordered_map<std::string, std::string> all() {
    // Platform-specific environment variable enumeration would go here
    // For now return empty
    return {};
}

} // namespace env

// ============================================================
// Process
// ============================================================

namespace process {

ProcessResult exec(const std::string& command) {
    ProcessResult result;
    result.exit_code = -1;

#ifdef GREY_PLATFORM_WINDOWS
    FILE* pipe = _popen(command.c_str(), "r");
#else
    FILE* pipe = popen(command.c_str(), "r");
#endif
    if (!pipe) return result;

    char buffer[4096];
    while (fgets(buffer, sizeof(buffer), pipe)) {
        result.stdout_output += buffer;
    }

#ifdef GREY_PLATFORM_WINDOWS
    result.exit_code = _pclose(pipe);
#else
    result.exit_code = pclose(pipe);
#endif
    return result;
}

ProcessResult exec(const std::string& program, const std::vector<std::string>& args) {
    std::string cmd = program;
    for (auto& arg : args) {
        cmd += " " + arg;
    }
    return exec(cmd);
}

void exit(int code) {
    std::exit(code);
}

int pid() {
#ifdef GREY_PLATFORM_WINDOWS
    return static_cast<int>(GetCurrentProcessId());
#else
    return static_cast<int>(getpid());
#endif
}

std::vector<std::string> args() {
    // Would need to be set from main()
    return {};
}

} // namespace process

// ============================================================
// OS Abstraction
// ============================================================

namespace os {

Platform current_platform() {
#ifdef GREY_PLATFORM_WINDOWS
    return Platform::Windows;
#elif defined(GREY_PLATFORM_MACOS)
    return Platform::MacOS;
#elif defined(GREY_PLATFORM_LINUX)
    return Platform::Linux;
#else
    return Platform::Unknown;
#endif
}

std::string platform_name() {
    switch (current_platform()) {
        case Platform::Windows: return "windows";
        case Platform::Linux:   return "linux";
        case Platform::MacOS:   return "macos";
        default:                return "unknown";
    }
}

std::string arch_name() {
#if defined(__x86_64__) || defined(_M_X64)
    return "x86_64";
#elif defined(__aarch64__) || defined(_M_ARM64)
    return "aarch64";
#elif defined(__i386__) || defined(_M_IX86)
    return "x86";
#elif defined(__arm__) || defined(_M_ARM)
    return "arm";
#else
    return "unknown";
#endif
}

size_t page_size() {
#ifdef GREY_PLATFORM_WINDOWS
    SYSTEM_INFO si;
    GetSystemInfo(&si);
    return si.dwPageSize;
#else
    return static_cast<size_t>(sysconf(_SC_PAGESIZE));
#endif
}

u32 cpu_count() {
    return std::thread::hardware_concurrency();
}

size_t total_memory() {
#ifdef GREY_PLATFORM_WINDOWS
    MEMORYSTATUSEX ms;
    ms.dwLength = sizeof(ms);
    GlobalMemoryStatusEx(&ms);
    return static_cast<size_t>(ms.ullTotalPhys);
#elif defined(GREY_PLATFORM_LINUX)
    struct sysinfo si;
    sysinfo(&si);
    return si.totalram * si.mem_unit;
#else
    return 0;
#endif
}

size_t available_memory() {
#ifdef GREY_PLATFORM_WINDOWS
    MEMORYSTATUSEX ms;
    ms.dwLength = sizeof(ms);
    GlobalMemoryStatusEx(&ms);
    return static_cast<size_t>(ms.ullAvailPhys);
#elif defined(GREY_PLATFORM_LINUX)
    struct sysinfo si;
    sysinfo(&si);
    return si.freeram * si.mem_unit;
#else
    return 0;
#endif
}

} // namespace os

// ============================================================
// Register System Natives
// ============================================================

void register_system_natives(VM& vm) {

    // ---- Time ----
    vm.define_native("time_now", [](int, Value*) -> Value {
        return Value::number(static_cast<f64>(time::now_ms()));
    }, 0);

    vm.define_native("time_now_ns", [](int, Value*) -> Value {
        return Value::number(static_cast<f64>(time::now_ns()));
    }, 0);

    vm.define_native("time_monotonic", [](int, Value*) -> Value {
        return Value::number(static_cast<f64>(time::monotonic_ms()));
    }, 0);

    vm.define_native("sleep", [](int argc, Value* args) -> Value {
        if (argc < 1) return Value::nil();
        u64 ms = 0;
        if (args[0].is_int()) ms = static_cast<u64>(args[0].as_int());
        else if (args[0].is_number()) ms = static_cast<u64>(args[0].as_number());
        time::sleep_ms(ms);
        return Value::nil();
    }, 1);

    // ---- Environment ----
    vm.define_native("env_get", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* key = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        std::string val = env::get(key->value);
        if (val.empty()) return Value::nil();
        return make_string(vm.gc(), val);
    }, 1);

    vm.define_native("env_set", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object() || !args[1].is_object()) return Value::nil();
        auto* key = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        auto* val = static_cast<ObjString*>(args[1].as_object<ObjHeader>());
        env::set(key->value, val->value);
        return Value::nil();
    }, 2);

    vm.define_native("env_has", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::boolean(false);
        auto* key = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        return Value::boolean(env::has(key->value));
    }, 1);

    vm.define_native("cwd", [&vm](int, Value*) -> Value {
        return make_string(vm.gc(), env::cwd());
    }, 0);

    // ---- Process ----
    vm.define_native("exec", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* cmd = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        auto result = process::exec(cmd->value);

        auto* map = new ObjMap();
        vm.gc().track(map);
        map->set(make_string(vm.gc(), "exit_code"), Value::integer(result.exit_code));
        map->set(make_string(vm.gc(), "stdout"), make_string(vm.gc(), result.stdout_output));
        map->set(make_string(vm.gc(), "stderr"), make_string(vm.gc(), result.stderr_output));
        return Value::object(map);
    }, 1);

    vm.define_native("exit", [](int argc, Value* args) -> Value {
        int code = (argc >= 1 && args[0].is_int()) ? static_cast<int>(args[0].as_int()) : 0;
        process::exit(code);
        return Value::nil(); // unreachable
    }, 0);

    vm.define_native("pid", [](int, Value*) -> Value {
        return Value::integer(process::pid());
    }, 0);

    // ---- OS ----
    vm.define_native("platform", [&vm](int, Value*) -> Value {
        return make_string(vm.gc(), os::platform_name());
    }, 0);

    vm.define_native("arch", [&vm](int, Value*) -> Value {
        return make_string(vm.gc(), os::arch_name());
    }, 0);

    vm.define_native("cpu_count", [](int, Value*) -> Value {
        return Value::integer(os::cpu_count());
    }, 0);

    vm.define_native("total_memory", [](int, Value*) -> Value {
        return Value::integer(static_cast<i64>(os::total_memory()));
    }, 0);
}

} // namespace grey
