// Grey Runtime - VM Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/vm.h"
#include "grey/abi.h"
#include "grey/sandbox.h"
#include "grey/debug.h"
#include "grey/concurrency.h"
#include "grey/module.h"

#include <cmath>
#include <cstring>
#include <sstream>
#include <algorithm>

namespace grey {

// ============================================================
// Constructor / Destructor
// ============================================================

VM::VM(const VMConfig& config) : config_(config), arena_(config.heap_initial_size) {
    gc_.set_gc_threshold(config.gc_threshold);
    gc_.set_grow_factor(config.gc_grow_factor);

    // Set up GC tracing callback
    gc_.trace_callback = [this](ObjHeader* obj) { trace_object(obj); };
}

VM::~VM() {
    // GC will clean up tracked objects in its destructor
}

// ============================================================
// String Interning
// ============================================================

ObjString* VM::intern_string(const std::string& s) {
    auto it = interned_strings_.find(s);
    if (it != interned_strings_.end()) return it->second;

    auto* str = new ObjString(s);
    gc_.track(str);
    interned_strings_[s] = str;
    return str;
}

Value VM::make_string_value(const std::string& s) {
    return Value::object(intern_string(s));
}

// ============================================================
// Global Variables
// ============================================================

void VM::define_global(const std::string& name, Value value) {
    globals_[name] = value;
}

Value VM::get_global(const std::string& name) {
    auto it = globals_.find(name);
    if (it != globals_.end()) return it->second;
    return Value::nil();
}

// ============================================================
// Native Functions
// ============================================================

void VM::define_native(const std::string& name, NativeFn fn, u16 arity) {
    auto* native = new ObjNativeFunction(name, std::move(fn), arity);
    gc_.track(native);
    globals_[name] = Value::object(native);
}

// ============================================================
// Execute
// ============================================================

Value VM::execute(ObjFunction* main_function) {
    auto* closure = new ObjClosure(main_function);
    gc_.track(closure);
    return execute(closure);
}

Value VM::execute(ObjClosure* closure, int arg_count, Value* args) {
    // Push closure and arguments onto stack
    push(Value::object(closure));
    if (args) {
        for (int i = 0; i < arg_count; i++) {
            push(args[i]);
        }
    }
    push_frame(closure, arg_count);
    return run();
}

Value VM::call_function(Value callee, int arg_count, Value* args) {
    if (!callee.is_object()) {
        runtime_error(ErrorCode::TypeError, "Attempted to call a non-callable value");
    }

    auto* obj = callee.as_object<ObjHeader>();
    switch (obj->type) {
        case ObjType::Closure:
            return execute(static_cast<ObjClosure*>(obj), arg_count, args);
        case ObjType::NativeFunction: {
            auto* native = static_cast<ObjNativeFunction*>(obj);
            return native->function(arg_count, args);
        }
        default:
            runtime_error(ErrorCode::TypeError, "Value is not callable");
    }
}

// ============================================================
// Frame Management
// ============================================================

void VM::push_frame(ObjClosure* closure, int arg_count) {
    if (frame_count_ >= MAX_CALL_FRAMES) {
        runtime_error(ErrorCode::StackOverflow, "Call stack overflow");
    }

    if (sandbox_) {
        sandbox_->check_stack_depth(frame_count_);
    }

    CallFrame& frame = frames_[frame_count_++];
    frame.closure = closure;
    frame.function = closure->function;
    frame.ip = 0;
    frame.base_slot = static_cast<u32>(stack_.size()) - arg_count - 1;
    frame.is_native = false;
}

void VM::pop_frame() {
    if (frame_count_ == 0) return;
    frame_count_--;
}

// ============================================================
// Core Execution Loop
// ============================================================

Value VM::run() {
    running_ = true;

    CallFrame* frame = &current_frame();
    Chunk* chunk = &frame->function->chunk;

    #define READ_BYTE()    (chunk->code[frame->ip++])
    #define READ_OPCODE()  (static_cast<OpCode>(READ_BYTE()))
    #define READ_U16()     (chunk->read_u16(frame->ip))
    #define READ_I16()     (chunk->read_i16(frame->ip))
    #define READ_U32()     (chunk->read_u32(frame->ip))
    #define READ_I32()     (chunk->read_i32(frame->ip))
    #define READ_CONST()   (chunk->constants[READ_U16()])
    #define STACK_AT(i)    (stack_.at(frame->base_slot + (i)))

    while (running_) {
        // Cycle counting for sandbox
        cycle_count_++;
        if (sandbox_ && config_.max_cpu_cycles > 0) {
            sandbox_->check_cycles(cycle_count_);
        }

        // GC check
        if (gc_.should_collect()) {
            collect_garbage();
        }

        // Debugger hook
        if (debugger_ && config_.debug_mode) {
            u32 line = chunk->get_line(frame->ip);
            debugger_->on_instruction(*this, frame->ip, chunk->source_name, line);
        }

        OpCode instruction = READ_OPCODE();

        switch (instruction) {
            // ---- Stack Operations ----
            case OpCode::NOP:
                break;

            case OpCode::PUSH_NIL:
                push(Value::nil());
                break;

            case OpCode::PUSH_TRUE:
                push(Value::boolean(true));
                break;

            case OpCode::PUSH_FALSE:
                push(Value::boolean(false));
                break;

            case OpCode::PUSH_CONST: {
                u16 idx = READ_U16();
                push(chunk->constants[idx]);
                break;
            }

            case OpCode::PUSH_CONST_LONG: {
                u32 idx = READ_U32();
                push(chunk->constants[idx]);
                break;
            }

            case OpCode::PUSH_INT: {
                i32 val = READ_I32();
                push(Value::integer(val));
                break;
            }

            case OpCode::PUSH_ZERO:
                push(Value::integer(0));
                break;

            case OpCode::PUSH_ONE:
                push(Value::integer(1));
                break;

            case OpCode::POP:
                pop();
                break;

            case OpCode::DUP:
                push(peek());
                break;

            case OpCode::DUP2: {
                Value b = peek(0);
                Value a = peek(1);
                push(a);
                push(b);
                break;
            }

            case OpCode::SWAP: {
                Value a = pop();
                Value b = pop();
                push(a);
                push(b);
                break;
            }

            case OpCode::ROT3: {
                Value c = pop();
                Value b = pop();
                Value a = pop();
                push(c);
                push(a);
                push(b);
                break;
            }

            // ---- Arithmetic ----
            case OpCode::ADD:
            case OpCode::SUB:
            case OpCode::MUL:
            case OpCode::DIV:
            case OpCode::MOD:
            case OpCode::POW:
            case OpCode::FLOOR_DIV:
            case OpCode::BIT_AND:
            case OpCode::BIT_OR:
            case OpCode::BIT_XOR:
            case OpCode::SHL:
            case OpCode::SHR:
                op_arithmetic(instruction);
                break;

            case OpCode::NEG: {
                Value v = pop();
                if (v.is_number()) push(Value::number(-v.as_number()));
                else if (v.is_int()) push(Value::integer(-v.as_int()));
                else runtime_error(ErrorCode::TypeError, "Cannot negate non-numeric value");
                break;
            }

            case OpCode::BIT_NOT: {
                Value v = pop();
                if (v.is_int()) push(Value::integer(~v.as_int()));
                else runtime_error(ErrorCode::TypeError, "Bitwise NOT requires integer");
                break;
            }

            // ---- Comparison ----
            case OpCode::EQ:
            case OpCode::NEQ:
            case OpCode::LT:
            case OpCode::LTE:
            case OpCode::GT:
            case OpCode::GTE:
                op_comparison(instruction);
                break;

            // ---- Logical ----
            case OpCode::NOT: {
                Value v = pop();
                push(Value::boolean(!v.is_truthy()));
                break;
            }

            case OpCode::AND: {
                Value b = pop();
                Value a = pop();
                push(a.is_truthy() ? b : a);
                break;
            }

            case OpCode::OR: {
                Value b = pop();
                Value a = pop();
                push(a.is_truthy() ? a : b);
                break;
            }

            // ---- Variables ----
            case OpCode::LOAD_LOCAL: {
                u16 slot = READ_U16();
                push(STACK_AT(slot));
                break;
            }

            case OpCode::STORE_LOCAL: {
                u16 slot = READ_U16();
                STACK_AT(slot) = peek();
                break;
            }

            case OpCode::LOAD_GLOBAL: {
                u16 name_idx = READ_U16();
                auto* name_obj = chunk->constants[name_idx].as_object<ObjString>();
                auto it = globals_.find(name_obj->value);
                if (it == globals_.end()) {
                    runtime_error(ErrorCode::KeyNotFound,
                        "Undefined variable '" + name_obj->value + "'");
                }
                push(it->second);
                break;
            }

            case OpCode::STORE_GLOBAL: {
                u16 name_idx = READ_U16();
                auto* name_obj = chunk->constants[name_idx].as_object<ObjString>();
                globals_[name_obj->value] = peek();
                break;
            }

            case OpCode::LOAD_UPVALUE: {
                u16 slot = READ_U16();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }

            case OpCode::STORE_UPVALUE: {
                u16 slot = READ_U16();
                *frame->closure->upvalues[slot]->location = peek();
                break;
            }

            case OpCode::CLOSE_UPVALUE: {
                close_upvalues(stack_.data() + stack_.size() - 1);
                pop();
                break;
            }

            case OpCode::LOAD_FIELD: {
                u16 name_idx = READ_U16();
                op_load_field(name_idx);
                break;
            }

            case OpCode::STORE_FIELD: {
                u16 name_idx = READ_U16();
                op_store_field(name_idx);
                break;
            }

            case OpCode::LOAD_INDEX:
                op_load_index();
                break;

            case OpCode::STORE_INDEX:
                op_store_index();
                break;

            // ---- Control Flow ----
            case OpCode::JUMP: {
                i16 offset = READ_I16();
                frame->ip += offset;
                break;
            }

            case OpCode::JUMP_LONG: {
                i32 offset = READ_I32();
                frame->ip += offset;
                break;
            }

            case OpCode::JUMP_IF_TRUE: {
                i16 offset = READ_I16();
                if (peek().is_truthy()) frame->ip += offset;
                break;
            }

            case OpCode::JUMP_IF_FALSE: {
                i16 offset = READ_I16();
                if (!peek().is_truthy()) frame->ip += offset;
                break;
            }

            case OpCode::JUMP_IF_NIL: {
                i16 offset = READ_I16();
                if (peek().is_nil()) frame->ip += offset;
                break;
            }

            case OpCode::LOOP: {
                u16 offset = READ_U16();
                frame->ip -= offset;
                break;
            }

            // ---- Functions ----
            case OpCode::CALL: {
                u8 arg_count = READ_BYTE();
                op_call(arg_count);
                frame = &current_frame();
                chunk = &frame->function->chunk;
                break;
            }

            case OpCode::CALL_METHOD: {
                u16 name_idx = READ_U16();
                u8 arg_count = READ_BYTE();
                op_call_method(name_idx, arg_count);
                frame = &current_frame();
                chunk = &frame->function->chunk;
                break;
            }

            case OpCode::RETURN: {
                Value result = pop();
                close_upvalues(stack_.data() + frame->base_slot);
                u32 base = frame->base_slot;
                pop_frame();

                if (frame_count_ == 0) {
                    running_ = false;
                    return result;
                }

                stack_.set_top(base);
                push(result);
                frame = &current_frame();
                chunk = &frame->function->chunk;
                break;
            }

            case OpCode::RETURN_NIL: {
                close_upvalues(stack_.data() + frame->base_slot);
                u32 base = frame->base_slot;
                pop_frame();

                if (frame_count_ == 0) {
                    running_ = false;
                    return Value::nil();
                }

                stack_.set_top(base);
                push(Value::nil());
                frame = &current_frame();
                chunk = &frame->function->chunk;
                break;
            }

            case OpCode::CLOSURE: {
                u16 func_idx = READ_U16();
                op_closure(func_idx);
                break;
            }

            // ---- Objects & Classes ----
            case OpCode::NEW_CLASS: {
                u16 name_idx = READ_U16();
                op_class(name_idx);
                break;
            }

            case OpCode::INHERIT: {
                Value super = peek(1);
                Value sub = peek(0);
                if (!super.is_object() || super.as_object<ObjHeader>()->type != ObjType::Class) {
                    runtime_error(ErrorCode::TypeError, "Superclass must be a class");
                }
                auto* subclass = sub.as_object<ObjClass>();
                auto* superclass = super.as_object<ObjClass>();
                subclass->superclass = superclass;
                // Copy methods from superclass
                subclass->vtable = superclass->vtable;
                pop(); // pop subclass
                break;
            }

            case OpCode::NEW_INSTANCE:
                op_instance();
                break;

            case OpCode::NEW_ARRAY: {
                u16 count = READ_U16();
                op_new_array(count);
                break;
            }

            case OpCode::NEW_MAP: {
                u16 count = READ_U16();
                op_new_map(count);
                break;
            }

            case OpCode::NEW_SET: {
                u16 count = READ_U16();
                op_new_set(count);
                break;
            }

            case OpCode::BIND_METHOD: {
                u16 name_idx = READ_U16();
                auto* name_obj = chunk->constants[name_idx].as_object<ObjString>();
                Value method_val = pop();
                Value receiver = peek();
                if (!method_val.is_object()) {
                    runtime_error(ErrorCode::TypeError, "Cannot bind non-closure as method");
                }
                auto* closure = static_cast<ObjClosure*>(method_val.as_object<ObjHeader>());
                if (receiver.is_object() && receiver.as_object<ObjHeader>()->type == ObjType::Class) {
                    auto* klass = receiver.as_object<ObjClass>();
                    klass->add_method(name_obj->value, closure);
                }
                break;
            }

            // ---- Modules ----
            case OpCode::IMPORT: {
                u16 name_idx = READ_U16();
                op_import(name_idx);
                break;
            }

            case OpCode::EXPORT: {
                u16 name_idx = READ_U16();
                auto* name_obj = chunk->constants[name_idx].as_object<ObjString>();
                Value val = peek();
                ObjModule* mod = current_module();
                if (mod) {
                    mod->exports.set(Value::object(name_obj), val);
                }
                break;
            }

            // ---- Concurrency ----
            case OpCode::SPAWN: {
                Value callee = pop();
                if (!callee.is_object()) {
                    runtime_error(ErrorCode::TypeError, "spawn requires a callable");
                }
                auto* closure = static_cast<ObjClosure*>(callee.as_object<ObjHeader>());
                auto* fiber = new ObjFiber(closure);
                gc_.track(fiber);
                if (async_runtime_) {
                    async_runtime_->schedule_fiber(fiber);
                }
                push(Value::object(fiber));
                break;
            }

            case OpCode::YIELD: {
                // Cooperative yield - save state and return to scheduler
                Value yield_val = pop();
                push(yield_val);
                // In a real implementation, we'd switch to the scheduler here
                break;
            }

            case OpCode::AWAIT: {
                Value future_val = pop();
                if (!future_val.is_object() ||
                    future_val.as_object<ObjHeader>()->type != ObjType::Future) {
                    runtime_error(ErrorCode::TypeError, "await requires a future");
                }
                auto* future = future_val.as_object<ObjFuture>();
                if (future->is_pending()) {
                    // Block until resolved
                    Value result = future->await();
                    push(result);
                } else if (future->is_resolved()) {
                    push(future->result);
                } else {
                    runtime_error(ErrorCode::RuntimeError, "Future rejected");
                }
                break;
            }

            case OpCode::SEND: {
                Value val = pop();
                Value chan_val = pop();
                if (!chan_val.is_object() ||
                    chan_val.as_object<ObjHeader>()->type != ObjType::Channel) {
                    runtime_error(ErrorCode::TypeError, "send requires a channel");
                }
                auto* chan = chan_val.as_object<ObjChannel>();
                if (!chan->send(val)) {
                    runtime_error(ErrorCode::RuntimeError, "Channel closed");
                }
                push(Value::nil());
                break;
            }

            case OpCode::RECV: {
                Value chan_val = pop();
                if (!chan_val.is_object() ||
                    chan_val.as_object<ObjHeader>()->type != ObjType::Channel) {
                    runtime_error(ErrorCode::TypeError, "recv requires a channel");
                }
                auto* chan = chan_val.as_object<ObjChannel>();
                Value received;
                if (!chan->receive(received)) {
                    push(Value::nil());
                } else {
                    push(received);
                }
                break;
            }

            // ---- Error Handling ----
            case OpCode::TRY_BEGIN: {
                u16 catch_off = READ_U16();
                u16 finally_off = READ_U16();
                op_try_begin(catch_off, finally_off);
                break;
            }

            case OpCode::TRY_END: {
                if (!try_stack_.empty()) try_stack_.pop_back();
                break;
            }

            case OpCode::THROW:
                op_throw();
                frame = &current_frame();
                chunk = &frame->function->chunk;
                break;

            case OpCode::CATCH: {
                // Push exception value
                push(current_exception_);
                current_exception_ = Value::nil();
                break;
            }

            // ---- Type Operations ----
            case OpCode::TYPE_CHECK: {
                u16 type_idx = READ_U16();
                Value val = peek();
                // Simple type checking
                bool matches = false;
                TypeTag tag = static_cast<TypeTag>(type_idx);
                switch (tag) {
                    case TypeTag::Nil:     matches = val.is_nil(); break;
                    case TypeTag::Bool:    matches = val.is_bool(); break;
                    case TypeTag::Int:     matches = val.is_int(); break;
                    case TypeTag::Float64: matches = val.is_number(); break;
                    case TypeTag::String:
                        matches = val.is_object() &&
                                  val.as_object<ObjHeader>()->type == ObjType::String;
                        break;
                    case TypeTag::Array:
                        matches = val.is_object() &&
                                  val.as_object<ObjHeader>()->type == ObjType::Array;
                        break;
                    default: matches = true; break;
                }
                pop();
                push(Value::boolean(matches));
                break;
            }

            case OpCode::INSTANCEOF: {
                Value klass_val = pop();
                Value obj_val = pop();
                bool result = false;
                if (obj_val.is_object() && klass_val.is_object()) {
                    auto* obj_hdr = obj_val.as_object<ObjHeader>();
                    if (obj_hdr->type == ObjType::Instance) {
                        auto* inst = static_cast<ObjInstance*>(obj_hdr);
                        auto* target = klass_val.as_object<ObjClass>();
                        ObjClass* k = inst->klass;
                        while (k) {
                            if (k == target) { result = true; break; }
                            k = k->superclass;
                        }
                    }
                }
                push(Value::boolean(result));
                break;
            }

            // ---- Iterators ----
            case OpCode::ITER_BEGIN:
                op_iter_begin();
                break;

            case OpCode::ITER_NEXT: {
                i16 offset = READ_I16();
                op_iter_next(offset);
                break;
            }

            case OpCode::ITER_CLOSE:
                op_iter_close();
                break;

            // ---- Import From ----
            case OpCode::IMPORT_FROM: {
                u16 mod_idx = READ_U16();
                u16 sym_idx = READ_U16();
                op_import(mod_idx);  // ensure module loaded
                Value mod_val = pop();
                if (mod_val.is_object() &&
                    mod_val.as_object<ObjHeader>()->type == ObjType::Module) {
                    auto* mod = static_cast<ObjModule*>(mod_val.as_object<ObjHeader>());
                    auto* sym_name = chunk->constants[sym_idx].as_object<ObjString>();
                    auto exported = mod->exports.get(Value::object(sym_name));
                    push(exported.value_or(Value::nil()));
                } else {
                    push(Value::nil());
                }
                break;
            }

            // ---- Type Cast ----
            case OpCode::TYPE_CAST: {
                u16 type_idx = READ_U16();
                Value val = pop();
                TypeTag tag = static_cast<TypeTag>(type_idx);
                switch (tag) {
                    case TypeTag::Int:
                        if (val.is_number()) push(Value::integer(static_cast<i64>(val.as_number())));
                        else if (val.is_int()) push(val);
                        else if (val.is_bool()) push(Value::integer(val.as_bool() ? 1 : 0));
                        else push(val);
                        break;
                    case TypeTag::Float64:
                        if (val.is_int()) push(Value::number(static_cast<f64>(val.as_int())));
                        else if (val.is_number()) push(val);
                        else if (val.is_bool()) push(Value::number(val.as_bool() ? 1.0 : 0.0));
                        else push(val);
                        break;
                    case TypeTag::Bool:
                        push(Value::boolean(!val.is_nil() &&
                            !(val.is_bool() && !val.as_bool()) &&
                            !(val.is_int() && val.as_int() == 0)));
                        break;
                    case TypeTag::String: {
                        std::string s = value_to_string(val);
                        push(make_string(gc_, s));
                        break;
                    }
                    default:
                        push(val);
                        break;
                }
                break;
            }

            // ---- Debugging ----
            case OpCode::DEBUG_BREAK: {
                if (debugger_) {
                    debugger_->trigger_break(*this);
                }
                break;
            }

            case OpCode::DEBUG_LINE: {
                frame->line = READ_U16();
                break;
            }

            case OpCode::DEBUG_PRINT: {
                Value v = peek();
                std::cout << "[debug] " << value_to_string(v) << "\n";
                break;
            }

            // ---- System ----
            case OpCode::HALT:
                running_ = false;
                return stack_.size() > 0 ? pop() : Value::nil();

            default:
                runtime_error(ErrorCode::InvalidOpcode,
                    "Unknown opcode: " + std::to_string(static_cast<int>(instruction)));
        }
    }

    #undef READ_BYTE
    #undef READ_OPCODE
    #undef READ_U16
    #undef READ_I16
    #undef READ_U32
    #undef READ_I32
    #undef READ_CONST
    #undef STACK_AT

    return stack_.size() > 0 ? pop() : Value::nil();
}

// ============================================================
// Arithmetic Operations
// ============================================================

void VM::op_arithmetic(OpCode op) {
    Value b = pop();
    Value a = pop();

    // String concatenation for ADD
    if (op == OpCode::ADD && a.is_object() && b.is_object()) {
        auto* ah = a.as_object<ObjHeader>();
        auto* bh = b.as_object<ObjHeader>();
        if (ah->type == ObjType::String && bh->type == ObjType::String) {
            auto* sa = static_cast<ObjString*>(ah);
            auto* sb = static_cast<ObjString*>(bh);
            push(make_string_value(sa->value + sb->value));
            return;
        }
    }

    // Numeric operations
    if (a.is_int() && b.is_int()) {
        i64 av = a.as_int(), bv = b.as_int();
        switch (op) {
            case OpCode::ADD:       push(Value::integer(av + bv)); return;
            case OpCode::SUB:       push(Value::integer(av - bv)); return;
            case OpCode::MUL:       push(Value::integer(av * bv)); return;
            case OpCode::DIV:
                if (bv == 0) runtime_error(ErrorCode::DivisionByZero, "Division by zero");
                push(Value::integer(av / bv)); return;
            case OpCode::MOD:
                if (bv == 0) runtime_error(ErrorCode::DivisionByZero, "Modulo by zero");
                push(Value::integer(av % bv)); return;
            case OpCode::POW:
                push(Value::number(std::pow(static_cast<f64>(av), static_cast<f64>(bv)))); return;
            case OpCode::FLOOR_DIV:
                if (bv == 0) runtime_error(ErrorCode::DivisionByZero, "Division by zero");
                push(Value::integer(av / bv)); return;
            case OpCode::BIT_AND:   push(Value::integer(av & bv)); return;
            case OpCode::BIT_OR:    push(Value::integer(av | bv)); return;
            case OpCode::BIT_XOR:   push(Value::integer(av ^ bv)); return;
            case OpCode::SHL:       push(Value::integer(av << bv)); return;
            case OpCode::SHR:       push(Value::integer(av >> bv)); return;
            default: break;
        }
    }

    // Promote to float
    f64 av = a.is_number() ? a.as_number() : static_cast<f64>(a.as_int());
    f64 bv = b.is_number() ? b.as_number() : static_cast<f64>(b.as_int());

    if (!a.is_number() && !a.is_int()) {
        runtime_error(ErrorCode::TypeError, "Left operand is not numeric");
    }
    if (!b.is_number() && !b.is_int()) {
        runtime_error(ErrorCode::TypeError, "Right operand is not numeric");
    }

    switch (op) {
        case OpCode::ADD:       push(Value::number(av + bv)); break;
        case OpCode::SUB:       push(Value::number(av - bv)); break;
        case OpCode::MUL:       push(Value::number(av * bv)); break;
        case OpCode::DIV:
            if (bv == 0.0) runtime_error(ErrorCode::DivisionByZero, "Division by zero");
            push(Value::number(av / bv)); break;
        case OpCode::MOD:
            if (bv == 0.0) runtime_error(ErrorCode::DivisionByZero, "Modulo by zero");
            push(Value::number(std::fmod(av, bv))); break;
        case OpCode::POW:       push(Value::number(std::pow(av, bv))); break;
        case OpCode::FLOOR_DIV:
            if (bv == 0.0) runtime_error(ErrorCode::DivisionByZero, "Division by zero");
            push(Value::number(std::floor(av / bv))); break;
        default:
            runtime_error(ErrorCode::TypeError, "Bitwise ops require integers");
    }
}

// ============================================================
// Comparison Operations
// ============================================================

void VM::op_comparison(OpCode op) {
    Value b = pop();
    Value a = pop();

    if (op == OpCode::EQ) { push(Value::boolean(a == b)); return; }
    if (op == OpCode::NEQ) { push(Value::boolean(a != b)); return; }

    // Numeric comparison
    f64 av, bv;
    if (a.is_int()) av = static_cast<f64>(a.as_int());
    else if (a.is_number()) av = a.as_number();
    else { runtime_error(ErrorCode::TypeError, "Cannot compare non-numeric values"); }

    if (b.is_int()) bv = static_cast<f64>(b.as_int());
    else if (b.is_number()) bv = b.as_number();
    else { runtime_error(ErrorCode::TypeError, "Cannot compare non-numeric values"); }

    switch (op) {
        case OpCode::LT:  push(Value::boolean(av < bv)); break;
        case OpCode::LTE: push(Value::boolean(av <= bv)); break;
        case OpCode::GT:  push(Value::boolean(av > bv)); break;
        case OpCode::GTE: push(Value::boolean(av >= bv)); break;
        default: break;
    }
}

// ============================================================
// Call Operations
// ============================================================

void VM::op_call(u8 arg_count) {
    Value callee = peek(arg_count);

    if (!callee.is_object()) {
        runtime_error(ErrorCode::TypeError, "Cannot call non-object");
    }

    auto* obj = callee.as_object<ObjHeader>();
    switch (obj->type) {
        case ObjType::Closure: {
            auto* closure = static_cast<ObjClosure*>(obj);
            if (arg_count != closure->function->arity) {
                runtime_error(ErrorCode::TypeError,
                    "Expected " + std::to_string(closure->function->arity) +
                    " arguments but got " + std::to_string(arg_count));
            }
            push_frame(closure, arg_count);
            break;
        }
        case ObjType::NativeFunction: {
            auto* native = static_cast<ObjNativeFunction*>(obj);
            Value* args = stack_.data() + stack_.size() - arg_count;
            Value result = native->function(arg_count, args);
            stack_.set_top(stack_.size() - arg_count - 1);
            push(result);
            break;
        }
        case ObjType::Class: {
            auto* klass = static_cast<ObjClass*>(obj);
            auto* instance = new ObjInstance(klass);
            gc_.track(instance);
            // Replace the class on the stack with the instance
            stack_.at(stack_.size() - arg_count - 1) = Value::object(instance);
            // Call init if present
            ObjClosure* init = klass->find_method("init");
            if (init) {
                push_frame(init, arg_count);
            } else if (arg_count != 0) {
                runtime_error(ErrorCode::TypeError,
                    "Class has no init method but received arguments");
            }
            break;
        }
        case ObjType::BoundMethod: {
            auto* bound = static_cast<ObjBoundMethod*>(obj);
            stack_.at(stack_.size() - arg_count - 1) = bound->receiver;
            push_frame(bound->method, arg_count);
            break;
        }
        default:
            runtime_error(ErrorCode::TypeError,
                "Cannot call " + value_to_string(callee));
    }
}

void VM::op_call_method(u16 name_idx, u8 arg_count) {
    CallFrame& frame = current_frame();
    auto* name_obj = frame.function->chunk.constants[name_idx].as_object<ObjString>();
    Value receiver = peek(arg_count);

    if (!receiver.is_object()) {
        runtime_error(ErrorCode::TypeError, "Cannot call method on non-object");
    }

    auto* obj = receiver.as_object<ObjHeader>();
    if (obj->type == ObjType::Instance) {
        auto* inst = static_cast<ObjInstance*>(obj);
        ObjClosure* method = inst->klass->find_method(name_obj->value);
        if (!method) {
            runtime_error(ErrorCode::KeyNotFound,
                "Undefined method '" + name_obj->value + "'");
        }
        push_frame(method, arg_count);
    } else {
        runtime_error(ErrorCode::TypeError, "Cannot call method on this type");
    }
}

// ============================================================
// Closure
// ============================================================

void VM::op_closure(u16 func_idx) {
    CallFrame& frame = current_frame();
    auto* fn = frame.function->chunk.constants[func_idx].as_object<ObjFunction>();
    auto* closure = new ObjClosure(fn);
    gc_.track(closure);

    for (int i = 0; i < fn->upvalue_count; i++) {
        auto& desc = fn->upvalue_descriptors[i];
        if (desc.is_local) {
            closure->upvalues[i] = capture_upvalue(
                stack_.data() + frame.base_slot + desc.index);
        } else {
            closure->upvalues[i] = frame.closure->upvalues[desc.index];
        }
    }

    push(Value::object(closure));
}

// ============================================================
// Upvalue Management
// ============================================================

ObjUpvalue* VM::capture_upvalue(Value* local) {
    ObjUpvalue* prev = nullptr;
    ObjUpvalue* upvalue = open_upvalues_;

    while (upvalue && upvalue->location > local) {
        prev = upvalue;
        upvalue = upvalue->next_open;
    }

    if (upvalue && upvalue->location == local) {
        return upvalue;
    }

    auto* created = new ObjUpvalue(local);
    gc_.track(created);
    created->next_open = upvalue;

    if (prev) prev->next_open = created;
    else open_upvalues_ = created;

    return created;
}

void VM::close_upvalues(Value* last) {
    while (open_upvalues_ && open_upvalues_->location >= last) {
        ObjUpvalue* upvalue = open_upvalues_;
        upvalue->close();
        open_upvalues_ = upvalue->next_open;
    }
}

// ============================================================
// Class & Instance Operations
// ============================================================

void VM::op_class(u16 name_idx) {
    CallFrame& frame = current_frame();
    auto* name_obj = frame.function->chunk.constants[name_idx].as_object<ObjString>();
    auto* klass = new ObjClass(name_obj->value);
    gc_.track(klass);
    push(Value::object(klass));
}

void VM::op_instance() {
    Value klass_val = pop();
    if (!klass_val.is_object() ||
        klass_val.as_object<ObjHeader>()->type != ObjType::Class) {
        runtime_error(ErrorCode::TypeError, "Can only instantiate a class");
    }
    auto* klass = klass_val.as_object<ObjClass>();
    auto* instance = new ObjInstance(klass);
    gc_.track(instance);
    push(Value::object(instance));
}

// ============================================================
// Collection Constructors
// ============================================================

void VM::op_new_array(u16 count) {
    auto* arr = new ObjArray();
    gc_.track(arr);
    arr->elements.resize(count);
    for (int i = count - 1; i >= 0; i--) {
        arr->elements[i] = pop();
    }
    push(Value::object(arr));
}

void VM::op_new_map(u16 count) {
    auto* map = new ObjMap();
    gc_.track(map);
    // Stack has alternating key, value pairs
    std::vector<std::pair<Value, Value>> entries(count);
    for (int i = count - 1; i >= 0; i--) {
        Value val = pop();
        Value key = pop();
        entries[i] = {key, val};
    }
    for (auto& [key, val] : entries) {
        map->set(key, val);
    }
    push(Value::object(map));
}

void VM::op_new_set(u16 count) {
    auto* set = new ObjSet();
    gc_.track(set);
    std::vector<Value> elems(count);
    for (int i = count - 1; i >= 0; i--) {
        elems[i] = pop();
    }
    for (auto& elem : elems) {
        set->add(elem);
    }
    push(Value::object(set));
}

// ============================================================
// Field Access
// ============================================================

void VM::op_load_field(u16 name_idx) {
    CallFrame& frame = current_frame();
    auto* name_obj = frame.function->chunk.constants[name_idx].as_object<ObjString>();
    Value receiver = pop();

    if (!receiver.is_object()) {
        runtime_error(ErrorCode::TypeError, "Cannot access field on non-object");
    }

    auto* obj = receiver.as_object<ObjHeader>();
    if (obj->type == ObjType::Instance) {
        auto* inst = static_cast<ObjInstance*>(obj);
        Value val;
        if (inst->fields.get(Value::object(name_obj), val)) {
            push(val);
            return;
        }
        // Check for method
        ObjClosure* method = inst->klass->find_method(name_obj->value);
        if (method) {
            auto* bound = new ObjBoundMethod(receiver, method);
            gc_.track(bound);
            push(Value::object(bound));
            return;
        }
        runtime_error(ErrorCode::KeyNotFound,
            "Undefined property '" + name_obj->value + "'");
    } else if (obj->type == ObjType::Module) {
        auto* mod = static_cast<ObjModule*>(obj);
        Value val;
        if (mod->exports.get(Value::object(name_obj), val)) {
            push(val);
            return;
        }
        runtime_error(ErrorCode::KeyNotFound,
            "Module has no export '" + name_obj->value + "'");
    } else {
        runtime_error(ErrorCode::TypeError, "Cannot access field on this type");
    }
}

void VM::op_store_field(u16 name_idx) {
    CallFrame& frame = current_frame();
    auto* name_obj = frame.function->chunk.constants[name_idx].as_object<ObjString>();
    Value value = pop();
    Value receiver = pop();

    if (!receiver.is_object()) {
        runtime_error(ErrorCode::TypeError, "Cannot set field on non-object");
    }

    auto* obj = receiver.as_object<ObjHeader>();
    if (obj->type == ObjType::Instance) {
        auto* inst = static_cast<ObjInstance*>(obj);
        inst->fields.set(Value::object(name_obj), value);
        push(value);
    } else {
        runtime_error(ErrorCode::TypeError, "Cannot set field on this type");
    }
}

// ============================================================
// Index Access
// ============================================================

void VM::op_load_index() {
    Value index = pop();
    Value receiver = pop();

    if (!receiver.is_object()) {
        runtime_error(ErrorCode::TypeError, "Cannot index non-object");
    }

    auto* obj = receiver.as_object<ObjHeader>();
    switch (obj->type) {
        case ObjType::Array: {
            auto* arr = static_cast<ObjArray*>(obj);
            if (!index.is_int()) runtime_error(ErrorCode::TypeError, "Array index must be integer");
            i64 idx = index.as_int();
            if (idx < 0) idx += arr->length();
            if (idx < 0 || static_cast<size_t>(idx) >= arr->length()) {
                runtime_error(ErrorCode::IndexOutOfBounds, "Array index out of bounds");
            }
            push(arr->elements[idx]);
            break;
        }
        case ObjType::Map: {
            auto* map = static_cast<ObjMap*>(obj);
            Value val;
            if (!map->get(index, val)) {
                push(Value::nil());
            } else {
                push(val);
            }
            break;
        }
        case ObjType::String: {
            auto* str = static_cast<ObjString*>(obj);
            if (!index.is_int()) runtime_error(ErrorCode::TypeError, "String index must be integer");
            i64 idx = index.as_int();
            if (idx < 0) idx += str->value.size();
            if (idx < 0 || static_cast<size_t>(idx) >= str->value.size()) {
                runtime_error(ErrorCode::IndexOutOfBounds, "String index out of bounds");
            }
            push(make_string_value(std::string(1, str->value[idx])));
            break;
        }
        case ObjType::Buffer: {
            auto* buf = static_cast<ObjBuffer*>(obj);
            if (!index.is_int()) runtime_error(ErrorCode::TypeError, "Buffer index must be integer");
            i64 idx = index.as_int();
            if (idx < 0 || static_cast<size_t>(idx) >= buf->data.size()) {
                runtime_error(ErrorCode::IndexOutOfBounds, "Buffer index out of bounds");
            }
            push(Value::integer(buf->data[idx]));
            break;
        }
        default:
            runtime_error(ErrorCode::TypeError, "Cannot index this type");
    }
}

void VM::op_store_index() {
    Value value = pop();
    Value index = pop();
    Value receiver = pop();

    if (!receiver.is_object()) {
        runtime_error(ErrorCode::TypeError, "Cannot index non-object");
    }

    auto* obj = receiver.as_object<ObjHeader>();
    switch (obj->type) {
        case ObjType::Array: {
            auto* arr = static_cast<ObjArray*>(obj);
            if (!index.is_int()) runtime_error(ErrorCode::TypeError, "Array index must be integer");
            i64 idx = index.as_int();
            if (idx < 0) idx += arr->length();
            if (idx < 0 || static_cast<size_t>(idx) >= arr->length()) {
                runtime_error(ErrorCode::IndexOutOfBounds, "Array index out of bounds");
            }
            arr->elements[idx] = value;
            push(value);
            break;
        }
        case ObjType::Map: {
            auto* map = static_cast<ObjMap*>(obj);
            map->set(index, value);
            push(value);
            break;
        }
        case ObjType::Buffer: {
            auto* buf = static_cast<ObjBuffer*>(obj);
            if (!index.is_int()) runtime_error(ErrorCode::TypeError, "Buffer index must be integer");
            i64 idx = index.as_int();
            if (idx < 0 || static_cast<size_t>(idx) >= buf->data.size()) {
                runtime_error(ErrorCode::IndexOutOfBounds, "Buffer index out of bounds");
            }
            if (!value.is_int()) runtime_error(ErrorCode::TypeError, "Buffer value must be integer");
            buf->data[idx] = static_cast<byte>(value.as_int());
            push(value);
            break;
        }
        default:
            runtime_error(ErrorCode::TypeError, "Cannot index-assign this type");
    }
}

// ============================================================
// Iterator Operations
// ============================================================

void VM::op_iter_begin() {
    Value collection = pop();
    auto* iter = new ObjIterator(collection);
    gc_.track(iter);
    push(Value::object(iter));
}

void VM::op_iter_next(i16 offset) {
    CallFrame& frame = current_frame();
    Value iter_val = peek();

    if (!iter_val.is_object() ||
        iter_val.as_object<ObjHeader>()->type != ObjType::Iterator) {
        runtime_error(ErrorCode::TypeError, "Expected iterator");
    }

    auto* iter = iter_val.as_object<ObjIterator>();
    if (iter->done) {
        frame.ip += offset; // jump to end
        return;
    }

    if (!iter->source.is_object()) {
        iter->done = true;
        frame.ip += offset;
        return;
    }

    auto* src = iter->source.as_object<ObjHeader>();
    switch (src->type) {
        case ObjType::Array: {
            auto* arr = static_cast<ObjArray*>(src);
            if (iter->index >= arr->length()) {
                iter->done = true;
                frame.ip += offset;
            } else {
                push(arr->elements[iter->index++]);
            }
            break;
        }
        case ObjType::String: {
            auto* str = static_cast<ObjString*>(src);
            if (iter->index >= str->value.size()) {
                iter->done = true;
                frame.ip += offset;
            } else {
                push(make_string_value(std::string(1, str->value[iter->index++])));
            }
            break;
        }
        default:
            iter->done = true;
            frame.ip += offset;
            break;
    }
}

void VM::op_iter_close() {
    pop(); // pop iterator
}

// ============================================================
// Module Operations
// ============================================================

void VM::op_import(u16 name_idx) {
    CallFrame& frame = current_frame();
    auto* name_obj = frame.function->chunk.constants[name_idx].as_object<ObjString>();

    ObjModule* mod = load_module(name_obj->value);
    if (!mod) {
        runtime_error(ErrorCode::ModuleNotFound,
            "Module not found: " + name_obj->value);
    }
    push(Value::object(mod));
}

ObjModule* VM::load_module(const std::string& name) {
    if (module_loader_) {
        return module_loader_->load(name, *this);
    }
    return nullptr;
}

ObjModule* VM::current_module() {
    if (frame_count_ == 0) return nullptr;
    return current_frame().function->module;
}

// ============================================================
// Exception Handling
// ============================================================

void VM::op_throw() {
    current_exception_ = pop();
    if (!handle_exception(current_exception_)) {
        runtime_error(ErrorCode::RuntimeError,
            "Unhandled exception: " + value_to_string(current_exception_));
    }
}

void VM::op_try_begin(u16 catch_off, u16 finally_off) {
    TryFrame tf;
    tf.catch_ip = current_frame().ip + catch_off;
    tf.finally_ip = finally_off > 0 ? current_frame().ip + finally_off : 0;
    tf.frame_index = frame_count_ - 1;
    tf.stack_top = static_cast<u32>(stack_.size());
    try_stack_.push_back(tf);
}

bool VM::handle_exception(Value exception) {
    while (!try_stack_.empty()) {
        TryFrame& tf = try_stack_.back();

        // Unwind frames
        while (frame_count_ > tf.frame_index + 1) {
            pop_frame();
        }

        // Restore stack
        stack_.set_top(tf.stack_top);
        current_exception_ = exception;

        // Jump to catch handler
        current_frame().ip = tf.catch_ip;
        try_stack_.pop_back();
        return true;
    }
    return false;
}

[[noreturn]] void VM::runtime_error(ErrorCode code, const std::string& msg) {
    RuntimeError err(code, msg);
    err.stack_trace.push_back(format_stack_trace());
    throw err;
}

// ============================================================
// Stack Trace
// ============================================================

std::string VM::format_stack_trace() const {
    std::ostringstream oss;
    for (int i = frame_count_ - 1; i >= 0; i--) {
        const CallFrame& f = frames_[i];
        if (f.function) {
            u32 line = f.function->chunk.get_line(f.ip > 0 ? f.ip - 1 : 0);
            oss << "  at " << (f.function->name.empty() ? "<script>" : f.function->name)
                << " (" << f.function->chunk.source_name << ":" << line << ")\n";
        }
    }
    return oss.str();
}

// ============================================================
// Garbage Collection Integration
// ============================================================

void VM::collect_garbage() {
    mark_roots();
    gc_.collect();
}

void VM::mark_roots() {
    // Mark stack values
    for (size_t i = 0; i < stack_.size(); i++) {
        gc_.mark_value(stack_.at(i));
    }

    // Mark call frame closures
    for (u32 i = 0; i < frame_count_; i++) {
        if (frames_[i].closure) gc_.mark(frames_[i].closure);
    }

    // Mark open upvalues
    for (ObjUpvalue* uv = open_upvalues_; uv; uv = uv->next_open) {
        gc_.mark(uv);
    }

    // Mark globals
    for (auto& [name, val] : globals_) {
        gc_.mark_value(val);
    }

    // Mark interned strings  
    for (auto& [str, obj] : interned_strings_) {
        gc_.mark(obj);
    }

    // Mark exception
    gc_.mark_value(current_exception_);
}

void VM::trace_object(ObjHeader* obj) {
    switch (obj->type) {
        case ObjType::Closure: {
            auto* c = static_cast<ObjClosure*>(obj);
            gc_.mark(c->function);
            for (auto* uv : c->upvalues) gc_.mark(uv);
            break;
        }
        case ObjType::Function: {
            auto* fn = static_cast<ObjFunction*>(obj);
            for (auto& val : fn->chunk.constants) gc_.mark_value(val);
            if (fn->module) gc_.mark(fn->module);
            break;
        }
        case ObjType::Upvalue: {
            auto* uv = static_cast<ObjUpvalue*>(obj);
            gc_.mark_value(uv->closed);
            break;
        }
        case ObjType::Class: {
            auto* k = static_cast<ObjClass*>(obj);
            if (k->superclass) gc_.mark(k->superclass);
            for (auto& e : k->methods.entries) {
                if (e.occupied) { gc_.mark_value(e.key); gc_.mark_value(e.value); }
            }
            break;
        }
        case ObjType::Instance: {
            auto* inst = static_cast<ObjInstance*>(obj);
            gc_.mark(inst->klass);
            for (auto& e : inst->fields.entries) {
                if (e.occupied) { gc_.mark_value(e.key); gc_.mark_value(e.value); }
            }
            break;
        }
        case ObjType::Array: {
            auto* arr = static_cast<ObjArray*>(obj);
            for (auto& v : arr->elements) gc_.mark_value(v);
            break;
        }
        case ObjType::Map: {
            auto* map = static_cast<ObjMap*>(obj);
            for (auto& e : map->entries) {
                if (e.occupied) { gc_.mark_value(e.key); gc_.mark_value(e.value); }
            }
            break;
        }
        case ObjType::Module: {
            auto* mod = static_cast<ObjModule*>(obj);
            for (auto& e : mod->exports.entries) {
                if (e.occupied) { gc_.mark_value(e.key); gc_.mark_value(e.value); }
            }
            for (auto& e : mod->globals.entries) {
                if (e.occupied) { gc_.mark_value(e.key); gc_.mark_value(e.value); }
            }
            break;
        }
        case ObjType::BoundMethod: {
            auto* bm = static_cast<ObjBoundMethod*>(obj);
            gc_.mark_value(bm->receiver);
            gc_.mark(bm->method);
            break;
        }
        case ObjType::Fiber: {
            auto* fiber = static_cast<ObjFiber*>(obj);
            if (fiber->entry) gc_.mark(fiber->entry);
            gc_.mark_value(fiber->result);
            gc_.mark_value(fiber->error);
            break;
        }
        case ObjType::Future: {
            auto* future = static_cast<ObjFuture*>(obj);
            gc_.mark_value(future->result);
            gc_.mark_value(future->error);
            break;
        }
        case ObjType::Iterator: {
            auto* iter = static_cast<ObjIterator*>(obj);
            gc_.mark_value(iter->source);
            break;
        }
        default:
            break;
    }
}

} // namespace grey
