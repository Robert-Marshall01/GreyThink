// Grey Runtime - Debugging & Introspection
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/bytecode.h"
#include "grey/objects.h"

#include <set>
#include <chrono>
#include <fstream>
#include <algorithm>
#include <cstdio>

namespace grey {

// Forward
class VM;

// ============================================================
// Debugger
// ============================================================

enum class DebugAction : u8 {
    Continue,
    StepOver,
    StepInto,
    StepOut,
    Pause,
    Stop,
};

struct Breakpoint {
    u32 id;
    std::string file;
    u32 line;
    bool enabled = true;
    std::string condition; // conditional breakpoint expression
    u32 hit_count = 0;
};

struct StackFrame {
    std::string function_name;
    std::string file;
    u32 line;
    u32 ip;
    std::unordered_map<std::string, Value> locals;
};

class Debugger {
public:
    using BreakCallback = std::function<DebugAction(const std::vector<StackFrame>&)>;

    Debugger() = default;

    // ----- Breakpoints -----
    u32 add_breakpoint(const std::string& file, u32 line) {
        u32 id = next_bp_id_++;
        breakpoints_[id] = {id, file, line, true, "", 0};
        line_breakpoints_.insert({file, line});
        return id;
    }

    u32 add_conditional_breakpoint(const std::string& file, u32 line,
                                    const std::string& condition) {
        u32 id = next_bp_id_++;
        breakpoints_[id] = {id, file, line, true, condition, 0};
        line_breakpoints_.insert({file, line});
        return id;
    }

    void remove_breakpoint(u32 id) {
        auto it = breakpoints_.find(id);
        if (it != breakpoints_.end()) {
            line_breakpoints_.erase({it->second.file, it->second.line});
            breakpoints_.erase(it);
        }
    }

    void enable_breakpoint(u32 id, bool enabled) {
        auto it = breakpoints_.find(id);
        if (it != breakpoints_.end()) it->second.enabled = enabled;
    }

    bool has_breakpoint(const std::string& file, u32 line) const {
        return line_breakpoints_.count({file, line}) > 0;
    }

    // ----- Step control -----
    void set_action(DebugAction action) { action_ = action; }
    DebugAction current_action() const { return action_; }

    // ----- Callback -----
    void set_break_callback(BreakCallback cb) { on_break_ = std::move(cb); }

    // Called by VM on each instruction
    void on_instruction(VM& vm, u32 ip, const std::string& file, u32 line);

    // Called when a breakpoint or step is hit
    void trigger_break(VM& vm);

    // ----- Stack trace -----
    std::vector<StackFrame> capture_stack(VM& vm);

    // ----- Watch expressions -----
    void add_watch(const std::string& expr) { watches_.push_back(expr); }
    void remove_watch(size_t index) {
        if (index < watches_.size()) watches_.erase(watches_.begin() + index);
    }
    const std::vector<std::string>& watches() const { return watches_; }

    // ----- State -----
    bool is_paused() const { return paused_; }
    void set_paused(bool p) { paused_ = p; }

private:
    std::unordered_map<u32, Breakpoint> breakpoints_;
    struct LineKey {
        std::string file;
        u32 line;
        bool operator==(const LineKey& o) const { return file == o.file && line == o.line; }
    };
    struct LineKeyHash {
        size_t operator()(const LineKey& k) const {
            return std::hash<std::string>()(k.file) ^ std::hash<u32>()(k.line);
        }
    };
    std::unordered_set<LineKey, LineKeyHash> line_breakpoints_;
    u32 next_bp_id_ = 1;

    DebugAction action_ = DebugAction::Continue;
    bool paused_ = false;
    u32 step_depth_ = 0;
    u32 last_line_ = 0;
    std::string last_file_;

    std::vector<std::string> watches_;
    BreakCallback on_break_;
};

// ============================================================
// Profiler
// ============================================================

struct ProfileSample {
    std::string function_name;
    std::string file;
    u64 call_count = 0;
    u64 total_time_ns = 0;
    u64 self_time_ns = 0;
    u64 alloc_bytes = 0;
};

class Profiler {
public:
    Profiler() = default;

    void enable() { enabled_ = true; start_time_ = clock::now(); }
    void disable() { enabled_ = false; }
    void start() { enable(); }
    void stop() { disable(); }
    bool is_enabled() const { return enabled_; }

    // Called on function entry/exit
    void on_enter(const std::string& name, const std::string& file) {
        if (!enabled_) return;
        auto& sample = samples_[name];
        sample.function_name = name;
        sample.file = file;
        sample.call_count++;
        call_stack_.push_back({name, clock::now()});
    }

    void on_exit(const std::string& name) {
        if (!enabled_ || call_stack_.empty()) return;
        auto end = clock::now();
        auto& top = call_stack_.back();
        auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(
            end - top.enter_time).count();
        samples_[name].total_time_ns += elapsed;
        samples_[name].self_time_ns += elapsed;
        call_stack_.pop_back();

        // Subtract from parent's self time
        if (!call_stack_.empty()) {
            samples_[call_stack_.back().name].self_time_ns -= elapsed;
        }
    }

    void on_alloc(const std::string& name, size_t bytes) {
        if (!enabled_) return;
        samples_[name].alloc_bytes += bytes;
    }

    // Get results
    std::vector<ProfileSample> results() const {
        std::vector<ProfileSample> res;
        for (auto& [_, s] : samples_) res.push_back(s);
        return res;
    }

    void reset() { samples_.clear(); call_stack_.clear(); }

    // Export to file
    void export_report(const std::string& path) const {
        std::ofstream out(path);
        out << "Function,File,Calls,Total(ns),Self(ns),Alloc(bytes)\n";
        for (auto& [_, s] : samples_) {
            out << s.function_name << ","
                << s.file << ","
                << s.call_count << ","
                << s.total_time_ns << ","
                << s.self_time_ns << ","
                << s.alloc_bytes << "\n";
        }
    }

    void export_csv(const std::string& path) const { export_report(path); }

    // Print report to stream
    void report(std::ostream& out) const {
        auto sorted = results();
        std::sort(sorted.begin(), sorted.end(),
            [](const ProfileSample& a, const ProfileSample& b) {
                return a.total_time_ns > b.total_time_ns;
            });
        out << "Function                     Calls     Total(ms)  Self(ms)   Alloc\n";
        out << "--------------------------------------------------------------------\n";
        for (auto& s : sorted) {
            char buf[256];
            std::snprintf(buf, sizeof(buf), "%-28s %-9llu %-10.3f %-10.3f %llu\n",
                s.function_name.c_str(),
                (unsigned long long)s.call_count,
                s.total_time_ns / 1e6,
                s.self_time_ns / 1e6,
                (unsigned long long)s.alloc_bytes);
            out << buf;
        }
    }

private:
    using clock = std::chrono::high_resolution_clock;

    bool enabled_ = false;
    clock::time_point start_time_;

    struct CallEntry {
        std::string name;
        clock::time_point enter_time;
    };
    std::vector<CallEntry> call_stack_;
    std::unordered_map<std::string, ProfileSample> samples_;
};

// ============================================================
// Logger
// ============================================================

class Logger {
public:
    Logger() = default;

    void set_level(LogLevel level) { level_ = level; }
    void set_output(const std::string& path) {
        file_.open(path, std::ios::app);
        use_file_ = true;
    }

    void log(LogLevel level, const std::string& msg) {
        if (level < level_) return;
        static const char* names[] = {"TRACE","DEBUG","INFO","WARN","ERROR","FATAL"};
        auto now = std::chrono::system_clock::now();
        auto time = std::chrono::system_clock::to_time_t(now);

        std::ostringstream oss;
        oss << "[" << names[static_cast<int>(level)] << "] " << msg;
        std::string line = oss.str();

        if (use_file_ && file_.is_open()) {
            file_ << line << "\n";
            file_.flush();
        } else {
            std::cerr << line << "\n";
        }
    }

    void trace(const std::string& msg) { log(LogLevel::Trace, msg); }
    void debug(const std::string& msg) { log(LogLevel::Debug, msg); }
    void info(const std::string& msg)  { log(LogLevel::Info, msg); }
    void warn(const std::string& msg)  { log(LogLevel::Warn, msg); }
    void error(const std::string& msg) { log(LogLevel::Error, msg); }
    void fatal(const std::string& msg) { log(LogLevel::Fatal, msg); }

private:
    LogLevel level_ = LogLevel::Info;
    bool use_file_ = false;
    std::ofstream file_;
};

} // namespace grey
