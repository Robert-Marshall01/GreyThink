// Grey Runtime - Standard Library: Collections
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/objects.h"

#include <deque>
#include <algorithm>

namespace grey {

class VM;

// ============================================================
// Queue (double-ended)
// ============================================================

struct ObjQueue {
    std::deque<Value> elements;

    void enqueue(Value v) { elements.push_back(v); }
    Value dequeue() {
        if (elements.empty()) throw RuntimeError(ErrorCode::IndexOutOfBounds, "Queue is empty");
        Value v = elements.front();
        elements.pop_front();
        return v;
    }
    Value peek_front() const {
        if (elements.empty()) throw RuntimeError(ErrorCode::IndexOutOfBounds, "Queue is empty");
        return elements.front();
    }
    Value peek_back() const {
        if (elements.empty()) throw RuntimeError(ErrorCode::IndexOutOfBounds, "Queue is empty");
        return elements.back();
    }
    size_t size() const { return elements.size(); }
    bool is_empty() const { return elements.empty(); }
    void clear() { elements.clear(); }
};

// ============================================================
// Sorted Set (tree-based)
// ============================================================

struct SortedSet {
    // Simple sorted vector-based set
    std::vector<Value> elements;

    bool add(Value v) {
        auto it = std::lower_bound(elements.begin(), elements.end(), v,
            [](const Value& a, const Value& b) { return a.bits < b.bits; });
        if (it != elements.end() && it->bits == v.bits) return false;
        elements.insert(it, v);
        return true;
    }

    bool contains(Value v) const {
        return std::binary_search(elements.begin(), elements.end(), v,
            [](const Value& a, const Value& b) { return a.bits < b.bits; });
    }

    bool remove(Value v) {
        auto it = std::lower_bound(elements.begin(), elements.end(), v,
            [](const Value& a, const Value& b) { return a.bits < b.bits; });
        if (it == elements.end() || it->bits != v.bits) return false;
        elements.erase(it);
        return true;
    }

    size_t size() const { return elements.size(); }
};

// ============================================================
// Register collection natives
// ============================================================

void register_collection_natives(VM& vm);

} // namespace grey
