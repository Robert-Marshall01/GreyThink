// Grey Runtime - Object Model
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/bytecode.h"
#include <unordered_set>

namespace grey {

// Forward declarations
struct ObjString;
struct ObjArray;
struct ObjMap;
struct ObjFunction;
struct ObjClosure;
struct ObjClass;
struct ObjInstance;
struct ObjModule;
struct ObjUpvalue;
struct ObjBoundMethod;
struct ObjFiber;
struct ObjFuture;
struct ObjChannel;
struct ObjBuffer;
struct ObjIterator;

// ============================================================
// Object Types
// ============================================================

// ----- String -----
struct ObjString : ObjHeader {
    std::string value;

    explicit ObjString(const std::string& s)
        : ObjHeader(ObjType::String), value(s) {
        hash = compute_hash();
    }
    explicit ObjString(std::string&& s)
        : ObjHeader(ObjType::String), value(std::move(s)) {
        hash = compute_hash();
    }

    u32 compute_hash() const {
        u32 h = 2166136261u;
        for (char c : value) {
            h ^= static_cast<u8>(c);
            h *= 16777619u;
        }
        return h;
    }
};

// ----- Array -----
struct ObjArray : ObjHeader {
    std::vector<Value> elements;

    ObjArray() : ObjHeader(ObjType::Array) {}
    explicit ObjArray(size_t initial_size)
        : ObjHeader(ObjType::Array) {
        elements.resize(initial_size);
    }

    void push(Value v) { elements.push_back(v); }
    Value pop() {
        Value v = elements.back();
        elements.pop_back();
        return v;
    }
    size_t length() const { return elements.size(); }
};

// ----- Map -----
struct MapEntry {
    Value key;
    Value value;
    bool  occupied = false;
};

struct ObjMap : ObjHeader {
    std::vector<MapEntry> entries;
    size_t count = 0;
    size_t capacity = 0;

    ObjMap() : ObjHeader(ObjType::Map) { grow(16); }

    void grow(size_t new_cap) {
        std::vector<MapEntry> old = std::move(entries);
        entries.resize(new_cap);
        capacity = new_cap;
        count = 0;
        for (auto& e : old) {
            if (e.occupied) set(e.key, e.value);
        }
    }

    size_t find_index(Value key) const {
        if (capacity == 0) return SIZE_MAX;
        size_t idx = key.bits % capacity;
        for (size_t i = 0; i < capacity; i++) {
            size_t slot = (idx + i) % capacity;
            if (!entries[slot].occupied) return slot;
            if (entries[slot].key == key) return slot;
        }
        return SIZE_MAX;
    }

    bool set(Value key, Value value) {
        if (count + 1 > capacity * 3 / 4) grow(capacity * 2);
        size_t idx = find_index(key);
        if (idx == SIZE_MAX) return false;
        bool is_new = !entries[idx].occupied;
        entries[idx] = {key, value, true};
        if (is_new) count++;
        return is_new;
    }

    bool get(Value key, Value& out) const {
        if (capacity == 0) return false;
        size_t idx = find_index(key);
        if (idx == SIZE_MAX || !entries[idx].occupied) return false;
        if (entries[idx].key != key) return false;
        out = entries[idx].value;
        return true;
    }

    std::optional<Value> get(Value key) const {
        Value out;
        if (get(key, out)) return out;
        return std::nullopt;
    }

    bool remove(Value key) {
        if (capacity == 0) return false;
        size_t idx = find_index(key);
        if (idx == SIZE_MAX || !entries[idx].occupied) return false;
        entries[idx].occupied = false;
        // Tombstone: mark with nil key
        entries[idx].key = Value::nil();
        count--;
        return true;
    }
};

// ----- Set -----
struct ObjSet : ObjHeader {
    std::unordered_set<u64> elements; // set of Value bits

    ObjSet() : ObjHeader(ObjType::Set) {}

    bool add(Value v) { return elements.insert(v.bits).second; }
    bool contains(Value v) const { return elements.count(v.bits) > 0; }
    bool remove(Value v) { return elements.erase(v.bits) > 0; }
    size_t size() const { return elements.size(); }
};

// ----- Upvalue -----
struct ObjUpvalue : ObjHeader {
    Value* location;    // points into the stack (open) or to 'closed'
    Value  closed;      // holds value after variable leaves scope
    ObjUpvalue* next_open = nullptr;

    explicit ObjUpvalue(Value* slot)
        : ObjHeader(ObjType::Upvalue), location(slot) {}

    void close() {
        closed = *location;
        location = &closed;
    }
};

// ----- Function -----
struct UpvalueDescriptor {
    u16  index;
    bool is_local;
};

struct ObjFunction : ObjHeader {
    std::string name;
    u16 arity = 0;
    u16 upvalue_count = 0;
    u16 local_count = 0;
    Chunk chunk;
    std::vector<UpvalueDescriptor> upvalue_descriptors;
    ObjModule* module = nullptr;

    ObjFunction() : ObjHeader(ObjType::Function) {}
};

// ----- Closure -----
struct ObjClosure : ObjHeader {
    ObjFunction* function;
    std::vector<ObjUpvalue*> upvalues;

    explicit ObjClosure(ObjFunction* fn)
        : ObjHeader(ObjType::Closure), function(fn) {
        upvalues.resize(fn->upvalue_count, nullptr);
    }
};

// ----- Native Function -----
using NativeFn = std::function<Value(int arg_count, Value* args)>;

struct ObjNativeFunction : ObjHeader {
    std::string name;
    NativeFn    function;
    u16         arity;

    ObjNativeFunction(const std::string& n, NativeFn fn, u16 a)
        : ObjHeader(ObjType::NativeFunction), name(n), function(std::move(fn)), arity(a) {}
};

// ----- Bound Method -----
struct ObjBoundMethod : ObjHeader {
    Value      receiver;
    ObjClosure* method;

    ObjBoundMethod(Value recv, ObjClosure* m)
        : ObjHeader(ObjType::BoundMethod), receiver(recv), method(m) {}
};

// ----- Class -----
struct ObjClass : ObjHeader {
    std::string name;
    ObjClass*   superclass = nullptr;
    ObjMap      methods;
    ObjMap      static_methods;

    // VTable for dynamic dispatch
    struct VTableEntry {
        std::string name;
        ObjClosure* method;
    };
    std::vector<VTableEntry> vtable;

    explicit ObjClass(const std::string& n)
        : ObjHeader(ObjType::Class), name(n) {}

    void add_method(const std::string& name, ObjClosure* method) {
        // Add to vtable
        for (auto& entry : vtable) {
            if (entry.name == name) {
                entry.method = method;
                return;
            }
        }
        vtable.push_back({name, method});
    }

    ObjClosure* find_method(const std::string& name) const {
        for (auto& entry : vtable) {
            if (entry.name == name) return entry.method;
        }
        if (superclass) return superclass->find_method(name);
        return nullptr;
    }
};

// ----- Instance -----
struct ObjInstance : ObjHeader {
    ObjClass* klass;
    ObjMap    fields;

    explicit ObjInstance(ObjClass* c)
        : ObjHeader(ObjType::Instance), klass(c) {}
};

// ----- Module -----
struct ObjModule : ObjHeader {
    std::string name;
    std::string path;
    ObjMap      exports;
    ObjMap      globals;
    bool        loaded = false;

    explicit ObjModule(const std::string& n)
        : ObjHeader(ObjType::Module), name(n) {}
};

// ----- Buffer -----
struct ObjBuffer : ObjHeader {
    std::vector<byte> data;

    ObjBuffer() : ObjHeader(ObjType::Buffer) {}
    explicit ObjBuffer(size_t size) : ObjHeader(ObjType::Buffer) {
        data.resize(size, 0);
    }
    explicit ObjBuffer(const std::vector<byte>& d)
        : ObjHeader(ObjType::Buffer), data(d) {}
};

// ----- Iterator -----
struct ObjIterator : ObjHeader {
    Value source;       // the collection being iterated
    size_t index = 0;   // current position
    bool done = false;

    explicit ObjIterator(Value src)
        : ObjHeader(ObjType::Iterator), source(src) {}
};

// ============================================================
// Helper: Create string value
// ============================================================

inline Value make_string(GarbageCollector& gc, const std::string& s) {
    auto* obj = new ObjString(s);
    gc.track(obj);
    return Value::object(obj);
}

inline ObjString* as_string(Value v) {
    return v.as_object<ObjString>();
}

inline std::string value_to_string(Value v) {
    if (v.is_nil()) return "nil";
    if (v.is_true()) return "true";
    if (v.is_false()) return "false";
    if (v.is_number()) {
        std::ostringstream oss;
        oss << v.as_number();
        return oss.str();
    }
    if (v.is_int()) return std::to_string(v.as_int());
    if (v.is_object()) {
        auto* obj = v.as_object<ObjHeader>();
        switch (obj->type) {
            case ObjType::String:   return static_cast<ObjString*>(obj)->value;
            case ObjType::Array:    return "<array>";
            case ObjType::Map:      return "<map>";
            case ObjType::Set:      return "<set>";
            case ObjType::Function: return "<fn " + static_cast<ObjFunction*>(obj)->name + ">";
            case ObjType::Closure:  return "<closure " + static_cast<ObjClosure*>(obj)->function->name + ">";
            case ObjType::NativeFunction: return "<native " + static_cast<ObjNativeFunction*>(obj)->name + ">";
            case ObjType::Class:    return "<class " + static_cast<ObjClass*>(obj)->name + ">";
            case ObjType::Instance: return "<instance " + static_cast<ObjInstance*>(obj)->klass->name + ">";
            case ObjType::Module:   return "<module " + static_cast<ObjModule*>(obj)->name + ">";
            case ObjType::Fiber:    return "<fiber>";
            case ObjType::Future:   return "<future>";
            case ObjType::Channel:  return "<channel>";
            case ObjType::Buffer:   return "<buffer>";
            case ObjType::Iterator: return "<iterator>";
            default:                return "<object>";
        }
    }
    return "<unknown>";
}

} // namespace grey
