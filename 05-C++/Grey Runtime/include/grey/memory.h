// Grey Runtime - Memory System
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include <cstring>
#include <algorithm>
#include <list>

namespace grey {

// ============================================================
// 2.1 Heap Manager
// ============================================================

// ----- Bump Allocator -----
class BumpAllocator {
public:
    explicit BumpAllocator(size_t capacity)
        : capacity_(capacity), offset_(0) {
        memory_ = static_cast<byte*>(::malloc(capacity));
    }
    ~BumpAllocator() { ::free(memory_); }

    void* allocate(size_t size, size_t alignment = 8) {
        size_t aligned = (offset_ + alignment - 1) & ~(alignment - 1);
        if (aligned + size > capacity_) return nullptr;
        void* ptr = memory_ + aligned;
        offset_ = aligned + size;
        return ptr;
    }

    void reset() { offset_ = 0; }
    size_t used() const { return offset_; }
    size_t remaining() const { return capacity_ - offset_; }

private:
    byte*  memory_;
    size_t capacity_;
    size_t offset_;
};

// ----- Free-List Allocator -----
class FreeListAllocator {
    struct Block {
        size_t size;
        Block* next;
    };

public:
    explicit FreeListAllocator(size_t capacity)
        : capacity_(capacity), used_(0) {
        memory_ = static_cast<byte*>(::malloc(capacity));
        free_list_ = reinterpret_cast<Block*>(memory_);
        free_list_->size = capacity;
        free_list_->next = nullptr;
    }
    ~FreeListAllocator() { ::free(memory_); }

    void* allocate(size_t size) {
        size_t total = size + sizeof(size_t); // store size before payload
        size_t alignment = 8;
        total = (total + alignment - 1) & ~(alignment - 1);

        Block* prev = nullptr;
        Block* curr = free_list_;
        while (curr) {
            if (curr->size >= total) {
                // Found a fit
                if (curr->size >= total + sizeof(Block) + 16) {
                    // Split block
                    Block* remainder = reinterpret_cast<Block*>(
                        reinterpret_cast<byte*>(curr) + total);
                    remainder->size = curr->size - total;
                    remainder->next = curr->next;
                    if (prev) prev->next = remainder;
                    else free_list_ = remainder;
                    curr->size = total;
                } else {
                    // Use entire block
                    total = curr->size;
                    if (prev) prev->next = curr->next;
                    else free_list_ = curr->next;
                }
                used_ += total;
                // Store size and return payload pointer
                *reinterpret_cast<size_t*>(curr) = total;
                return reinterpret_cast<byte*>(curr) + sizeof(size_t);
            }
            prev = curr;
            curr = curr->next;
        }
        return nullptr; // OOM
    }

    void deallocate(void* ptr) {
        if (!ptr) return;
        byte* raw = static_cast<byte*>(ptr) - sizeof(size_t);
        size_t block_size = *reinterpret_cast<size_t*>(raw);
        used_ -= block_size;

        Block* freed = reinterpret_cast<Block*>(raw);
        freed->size = block_size;
        freed->next = free_list_;
        free_list_ = freed;
        // TODO: coalesce adjacent blocks
    }

    size_t used() const { return used_; }

private:
    byte*  memory_;
    size_t capacity_;
    size_t used_;
    Block* free_list_;
};

// ----- Arena Allocator -----
class ArenaAllocator {
    struct Region {
        byte*  memory;
        size_t capacity;
        size_t offset;
        Region* next;
    };

public:
    explicit ArenaAllocator(size_t region_size = 64 * 1024)
        : region_size_(region_size), head_(nullptr), total_allocated_(0) {
        grow();
    }

    ~ArenaAllocator() {
        Region* r = head_;
        while (r) {
            Region* next = r->next;
            ::free(r->memory);
            ::free(r);
            r = next;
        }
    }

    void* allocate(size_t size, size_t alignment = 8) {
        size_t aligned = (head_->offset + alignment - 1) & ~(alignment - 1);
        if (aligned + size > head_->capacity) {
            if (size > region_size_) {
                region_size_ = size * 2;
            }
            grow();
            aligned = (head_->offset + alignment - 1) & ~(alignment - 1);
        }
        void* ptr = head_->memory + aligned;
        head_->offset = aligned + size;
        total_allocated_ += size;
        return ptr;
    }

    void reset() {
        Region* r = head_;
        while (r) {
            r->offset = 0;
            r = r->next;
        }
        total_allocated_ = 0;
    }

    size_t total_allocated() const { return total_allocated_; }

private:
    void grow() {
        auto* region = static_cast<Region*>(::malloc(sizeof(Region)));
        region->memory = static_cast<byte*>(::malloc(region_size_));
        region->capacity = region_size_;
        region->offset = 0;
        region->next = head_;
        head_ = region;
    }

    size_t  region_size_;
    Region* head_;
    size_t  total_allocated_;
};

// ============================================================
// 2.2 Stack Model
// ============================================================

static constexpr size_t STACK_MAX = 65536;

struct CallFrame {
    u32    ip;                 // instruction pointer into chunk
    u32    base_slot;          // first slot in the value stack for this frame
    struct ObjFunction* function; // currently executing function
    struct ObjClosure*  closure;  // closure (if any)
    u32    line;               // current line (for errors)
    bool   is_native;          // native call frame
};

class ValueStack {
public:
    ValueStack() : top_(0) {
        data_.resize(STACK_MAX);
    }

    void push(Value v) {
        if (top_ >= STACK_MAX)
            throw RuntimeError(ErrorCode::StackOverflow, "Stack overflow");
        data_[top_++] = v;
    }

    Value pop() {
        if (top_ == 0)
            throw RuntimeError(ErrorCode::StackUnderflow, "Stack underflow");
        return data_[--top_];
    }

    Value& peek(size_t distance = 0) {
        return data_[top_ - 1 - distance];
    }

    const Value& peek(size_t distance = 0) const {
        return data_[top_ - 1 - distance];
    }

    Value& at(size_t index) { return data_[index]; }
    const Value& at(size_t index) const { return data_[index]; }

    size_t size() const { return top_; }
    void   set_top(size_t n) { top_ = n; }
    void   reset() { top_ = 0; }
    Value* data() { return data_.data(); }

private:
    std::vector<Value> data_;
    size_t top_;
};

// ============================================================
// 2.3 Garbage Collector (Mark-Sweep with generational hints)
// ============================================================

class GarbageCollector {
public:
    GarbageCollector() = default;
    ~GarbageCollector() { collect_all(); }

    // Register a new object
    void track(ObjHeader* obj) {
        obj->next = objects_;
        objects_ = obj;
        bytes_allocated_ += sizeof(*obj);
        num_objects_++;
    }

    // Mark an object as reachable
    void mark(ObjHeader* obj) {
        if (!obj || obj->is_marked) return;
        obj->is_marked = true;
        grey_stack_.push_back(obj);
    }

    // Mark a value if it's an object
    void mark_value(Value val) {
        if (val.is_object()) {
            mark(val.as_object<ObjHeader>());
        }
    }

    // Trace references from marked objects (subclasses override)
    using TraceCallback = std::function<void(ObjHeader*)>;
    TraceCallback trace_callback;

    // Run mark-sweep collection
    size_t collect() {
        size_t before = num_objects_;

        // Trace phase
        while (!grey_stack_.empty()) {
            ObjHeader* obj = grey_stack_.back();
            grey_stack_.pop_back();
            if (trace_callback) trace_callback(obj);
        }

        // Sweep phase
        ObjHeader** obj_ptr = &objects_;
        while (*obj_ptr) {
            if (!(*obj_ptr)->is_marked) {
                ObjHeader* unreached = *obj_ptr;
                *obj_ptr = unreached->next;
                free_object(unreached);
                num_objects_--;
            } else {
                (*obj_ptr)->is_marked = false;
                obj_ptr = &(*obj_ptr)->next;
            }
        }

        next_gc_ = bytes_allocated_ * gc_grow_factor_;
        collection_count_++;
        return before - num_objects_;
    }

    // Overload that accepts a trace callback for convenience
    size_t collect(std::function<void(ObjHeader*)> callback) {
        trace_callback = std::move(callback);
        return collect();
    }

    void collect_all() {
        while (objects_) {
            ObjHeader* obj = objects_;
            objects_ = objects_->next;
            free_object(obj);
        }
        num_objects_ = 0;
    }

    bool should_collect() const {
        return bytes_allocated_ > next_gc_;
    }

    size_t bytes_allocated() const { return bytes_allocated_; }
    size_t num_objects() const { return num_objects_; }
    size_t object_count() const { return num_objects_; }
    size_t collection_count() const { return collection_count_; }

    void set_gc_threshold(size_t threshold) { next_gc_ = threshold; }
    void set_grow_factor(double factor) { gc_grow_factor_ = factor; }

private:
    void free_object(ObjHeader* obj) {
        bytes_allocated_ -= sizeof(*obj);
        delete obj;
    }

    ObjHeader* objects_ = nullptr;
    std::vector<ObjHeader*> grey_stack_;
    size_t bytes_allocated_ = 0;
    size_t num_objects_ = 0;
    size_t collection_count_ = 0;
    size_t next_gc_ = 1024 * 1024; // 1MB initial threshold
    double gc_grow_factor_ = 2.0;
};

} // namespace grey
