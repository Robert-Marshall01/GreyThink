// Grey Runtime - Collections Standard Library Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/stdlib/collections.h"
#include "grey/vm.h"

namespace grey {

void register_collection_natives(VM& vm) {

    // ---- Array ----

    vm.define_native("Array", [&vm](int argc, Value* args) -> Value {
        auto* arr = new ObjArray();
        vm.gc().track(arr);
        for (int i = 0; i < argc; i++) arr->push(args[i]);
        return Value::object(arr);
    }, 0);

    vm.define_native("array_push", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        arr->push(args[1]);
        return args[0];
    }, 2);

    vm.define_native("array_pop", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        if (arr->elements.empty()) return Value::nil();
        return arr->pop();
    }, 1);

    vm.define_native("array_length", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::integer(0);
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        return Value::integer(static_cast<i64>(arr->length()));
    }, 1);

    vm.define_native("array_slice", [&vm](int argc, Value* args) -> Value {
        if (argc < 3 || !args[0].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        i64 start = args[1].as_int();
        i64 end = args[2].as_int();
        if (start < 0) start += arr->length();
        if (end < 0) end += arr->length();
        start = std::max(i64(0), std::min(start, static_cast<i64>(arr->length())));
        end = std::max(i64(0), std::min(end, static_cast<i64>(arr->length())));

        auto* result = new ObjArray();
        vm.gc().track(result);
        for (i64 i = start; i < end; i++) {
            result->push(arr->elements[i]);
        }
        return Value::object(result);
    }, 3);

    vm.define_native("array_reverse", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        std::reverse(arr->elements.begin(), arr->elements.end());
        return args[0];
    }, 1);

    vm.define_native("array_contains", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::boolean(false);
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        for (auto& v : arr->elements) {
            if (v == args[1]) return Value::boolean(true);
        }
        return Value::boolean(false);
    }, 2);

    vm.define_native("array_sort", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        std::sort(arr->elements.begin(), arr->elements.end(),
            [](const Value& a, const Value& b) {
                // Numeric comparison
                f64 av = 0, bv = 0;
                if (a.is_int()) av = static_cast<f64>(a.as_int());
                else if (a.is_number()) av = a.as_number();
                if (b.is_int()) bv = static_cast<f64>(b.as_int());
                else if (b.is_number()) bv = b.as_number();
                if ((a.is_int() || a.is_number()) && (b.is_int() || b.is_number())) {
                    return av < bv;
                }
                // String comparison
                if (a.is_object() && b.is_object()) {
                    auto* ah = a.as_object<ObjHeader>();
                    auto* bh = b.as_object<ObjHeader>();
                    if (ah->type == ObjType::String && bh->type == ObjType::String) {
                        return static_cast<ObjString*>(ah)->value <
                               static_cast<ObjString*>(bh)->value;
                    }
                }
                return a.bits < b.bits;
            });
        return args[0];
    }, 1);

    vm.define_native("array_map", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object() || !args[1].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        auto* result = new ObjArray();
        vm.gc().track(result);
        for (auto& elem : arr->elements) {
            Value fn_args[] = { elem };
            Value mapped = vm.call_function(args[1], 1, fn_args);
            result->push(mapped);
        }
        return Value::object(result);
    }, 2);

    vm.define_native("array_filter", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object() || !args[1].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        auto* result = new ObjArray();
        vm.gc().track(result);
        for (auto& elem : arr->elements) {
            Value fn_args[] = { elem };
            Value keep = vm.call_function(args[1], 1, fn_args);
            if (keep.is_truthy()) result->push(elem);
        }
        return Value::object(result);
    }, 2);

    vm.define_native("array_reduce", [&vm](int argc, Value* args) -> Value {
        if (argc < 3 || !args[0].is_object() || !args[1].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        Value accumulator = args[2];
        for (auto& elem : arr->elements) {
            Value fn_args[] = { accumulator, elem };
            accumulator = vm.call_function(args[1], 2, fn_args);
        }
        return accumulator;
    }, 3);

    vm.define_native("array_find", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object() || !args[1].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        for (auto& elem : arr->elements) {
            Value fn_args[] = { elem };
            Value result = vm.call_function(args[1], 1, fn_args);
            if (result.is_truthy()) return elem;
        }
        return Value::nil();
    }, 2);

    vm.define_native("array_index_of", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::integer(-1);
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        for (size_t i = 0; i < arr->elements.size(); i++) {
            if (arr->elements[i] == args[1]) return Value::integer(static_cast<i64>(i));
        }
        return Value::integer(-1);
    }, 2);

    vm.define_native("array_join", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::nil();
        auto* arr = static_cast<ObjArray*>(args[0].as_object<ObjHeader>());
        std::string sep;
        if (args[1].is_object() && args[1].as_object<ObjHeader>()->type == ObjType::String) {
            sep = static_cast<ObjString*>(args[1].as_object<ObjHeader>())->value;
        }
        std::string result;
        for (size_t i = 0; i < arr->elements.size(); i++) {
            if (i > 0) result += sep;
            result += value_to_string(arr->elements[i]);
        }
        return make_string(vm.gc(), result);
    }, 2);

    // ---- Map ----

    vm.define_native("Map", [&vm](int, Value*) -> Value {
        auto* map = new ObjMap();
        vm.gc().track(map);
        return Value::object(map);
    }, 0);

    vm.define_native("map_set", [](int argc, Value* args) -> Value {
        if (argc < 3 || !args[0].is_object()) return Value::nil();
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        map->set(args[1], args[2]);
        return args[0];
    }, 3);

    vm.define_native("map_get", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::nil();
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        Value result;
        if (map->get(args[1], result)) return result;
        if (argc >= 3) return args[2]; // default value
        return Value::nil();
    }, 2);

    vm.define_native("map_has", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::boolean(false);
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        Value dummy;
        return Value::boolean(map->get(args[1], dummy));
    }, 2);

    vm.define_native("map_delete", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::boolean(false);
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        return Value::boolean(map->remove(args[1]));
    }, 2);

    vm.define_native("map_size", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::integer(0);
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        return Value::integer(static_cast<i64>(map->count));
    }, 1);

    vm.define_native("map_keys", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        auto* arr = new ObjArray();
        vm.gc().track(arr);
        for (auto& e : map->entries) {
            if (e.occupied) arr->push(e.key);
        }
        return Value::object(arr);
    }, 1);

    vm.define_native("map_values", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        auto* arr = new ObjArray();
        vm.gc().track(arr);
        for (auto& e : map->entries) {
            if (e.occupied) arr->push(e.value);
        }
        return Value::object(arr);
    }, 1);

    vm.define_native("map_entries", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        auto* arr = new ObjArray();
        vm.gc().track(arr);
        for (auto& e : map->entries) {
            if (e.occupied) {
                auto* pair = new ObjArray();
                vm.gc().track(pair);
                pair->push(e.key);
                pair->push(e.value);
                arr->push(Value::object(pair));
            }
        }
        return Value::object(arr);
    }, 1);

    vm.define_native("map_clear", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* map = static_cast<ObjMap*>(args[0].as_object<ObjHeader>());
        for (auto& e : map->entries) e.occupied = false;
        map->count = 0;
        return args[0];
    }, 1);

    // ---- Set ----

    vm.define_native("Set", [&vm](int, Value*) -> Value {
        auto* set = new ObjSet();
        vm.gc().track(set);
        return Value::object(set);
    }, 0);

    vm.define_native("set_add", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::boolean(false);
        auto* set = static_cast<ObjSet*>(args[0].as_object<ObjHeader>());
        return Value::boolean(set->add(args[1]));
    }, 2);

    vm.define_native("set_has", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::boolean(false);
        auto* set = static_cast<ObjSet*>(args[0].as_object<ObjHeader>());
        return Value::boolean(set->contains(args[1]));
    }, 2);

    vm.define_native("set_remove", [](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object()) return Value::boolean(false);
        auto* set = static_cast<ObjSet*>(args[0].as_object<ObjHeader>());
        return Value::boolean(set->remove(args[1]));
    }, 2);

    vm.define_native("set_size", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::integer(0);
        auto* set = static_cast<ObjSet*>(args[0].as_object<ObjHeader>());
        return Value::integer(static_cast<i64>(set->size()));
    }, 1);

    vm.define_native("set_union", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object() || !args[1].is_object()) return Value::nil();
        auto* a = static_cast<ObjSet*>(args[0].as_object<ObjHeader>());
        auto* b = static_cast<ObjSet*>(args[1].as_object<ObjHeader>());
        auto* result = new ObjSet();
        vm.gc().track(result);
        for (auto bits : a->elements) result->elements.insert(bits);
        for (auto bits : b->elements) result->elements.insert(bits);
        return Value::object(result);
    }, 2);

    vm.define_native("set_intersect", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object() || !args[1].is_object()) return Value::nil();
        auto* a = static_cast<ObjSet*>(args[0].as_object<ObjHeader>());
        auto* b = static_cast<ObjSet*>(args[1].as_object<ObjHeader>());
        auto* result = new ObjSet();
        vm.gc().track(result);
        for (auto bits : a->elements) {
            if (b->elements.count(bits)) result->elements.insert(bits);
        }
        return Value::object(result);
    }, 2);

    vm.define_native("set_difference", [&vm](int argc, Value* args) -> Value {
        if (argc < 2 || !args[0].is_object() || !args[1].is_object()) return Value::nil();
        auto* a = static_cast<ObjSet*>(args[0].as_object<ObjHeader>());
        auto* b = static_cast<ObjSet*>(args[1].as_object<ObjHeader>());
        auto* result = new ObjSet();
        vm.gc().track(result);
        for (auto bits : a->elements) {
            if (!b->elements.count(bits)) result->elements.insert(bits);
        }
        return Value::object(result);
    }, 2);

    vm.define_native("set_to_array", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* set = static_cast<ObjSet*>(args[0].as_object<ObjHeader>());
        auto* arr = new ObjArray();
        vm.gc().track(arr);
        for (auto bits : set->elements) {
            Value v;
            v.bits = bits;
            arr->push(v);
        }
        return Value::object(arr);
    }, 1);

    // ---- Buffer ----

    vm.define_native("Buffer", [&vm](int argc, Value* args) -> Value {
        size_t size = (argc >= 1 && args[0].is_int()) ? args[0].as_int() : 0;
        auto* buf = new ObjBuffer(size);
        vm.gc().track(buf);
        return Value::object(buf);
    }, 0);

    vm.define_native("buffer_size", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::integer(0);
        auto* buf = static_cast<ObjBuffer*>(args[0].as_object<ObjHeader>());
        return Value::integer(static_cast<i64>(buf->data.size()));
    }, 1);
}

} // namespace grey
