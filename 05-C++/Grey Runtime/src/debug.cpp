// Grey Runtime - Debugger Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/debug.h"
#include "grey/vm.h"

namespace grey {

// ============================================================
// Debugger
// ============================================================

void Debugger::on_instruction(VM& vm, u32 ip, const std::string& file, u32 line) {
    if (paused_) return;

    bool should_break = false;

    switch (action_) {
        case DebugAction::Continue:
            // Check breakpoints
            should_break = has_breakpoint(file, line) && (file != last_file_ || line != last_line_);
            break;

        case DebugAction::StepOver:
            // Break at next line in same or shallower frame
            should_break = (line != last_line_ || file != last_file_);
            break;

        case DebugAction::StepInto:
            // Break at next instruction
            should_break = true;
            break;

        case DebugAction::StepOut:
            // Break when we return to the parent frame
            // This is handled by the VM's return logic
            break;

        case DebugAction::Pause:
            should_break = true;
            break;

        case DebugAction::Stop:
            return;
    }

    if (should_break) {
        last_line_ = line;
        last_file_ = file;

        // Increment hit counters
        for (auto& [_, bp] : breakpoints_) {
            if (bp.file == file && bp.line == line && bp.enabled) {
                bp.hit_count++;
            }
        }

        trigger_break(vm);
    }
}

void Debugger::trigger_break(VM& vm) {
    paused_ = true;

    auto frames = capture_stack(vm);

    if (on_break_) {
        action_ = on_break_(frames);
    }

    paused_ = false;
}

std::vector<StackFrame> Debugger::capture_stack(VM& /*vm*/) {
    std::vector<StackFrame> frames;
    // Provide at least the current location from debugger state
    if (!last_file_.empty()) {
        StackFrame frame;
        frame.function_name = "<current>";
        frame.file = last_file_;
        frame.line = last_line_;
        frame.ip = 0;
        frames.push_back(frame);
    }
    return frames;
}

} // namespace grey
