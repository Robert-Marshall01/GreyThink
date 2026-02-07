// Grey Runtime - IO Standard Library Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/stdlib/io.h"
#include "grey/vm.h"
#include "grey/sandbox.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <filesystem>

namespace grey {

void register_io_natives(VM& vm) {

    // ---- File Operations ----

    vm.define_native("file_read", [&vm](int argc, Value* args) -> Value {
        if (argc < 1) return Value::nil();
        if (!args[0].is_object() || args[0].as_object<ObjHeader>()->type != ObjType::String) {
            throw RuntimeError(ErrorCode::TypeError, "file_read expects a string path");
        }
        auto* path_obj = static_cast<ObjString*>(args[0].as_object<ObjHeader>());

        std::ifstream file(path_obj->value, std::ios::binary | std::ios::ate);
        if (!file.is_open()) {
            throw RuntimeError(ErrorCode::IOError, "Cannot open file: " + path_obj->value);
        }

        auto size = file.tellg();
        file.seekg(0);
        std::string content(size, '\0');
        file.read(content.data(), size);

        return make_string(vm.gc(), content);
    }, 1);

    vm.define_native("file_write", [](int argc, Value* args) -> Value {
        if (argc < 2) return Value::boolean(false);
        if (!args[0].is_object() || args[0].as_object<ObjHeader>()->type != ObjType::String) {
            throw RuntimeError(ErrorCode::TypeError, "file_write expects a string path");
        }
        auto* path = static_cast<ObjString*>(args[0].as_object<ObjHeader>());

        std::string content;
        if (args[1].is_object() && args[1].as_object<ObjHeader>()->type == ObjType::String) {
            content = static_cast<ObjString*>(args[1].as_object<ObjHeader>())->value;
        } else {
            content = value_to_string(args[1]);
        }

        std::ofstream file(path->value, std::ios::binary | std::ios::trunc);
        if (!file.is_open()) {
            throw RuntimeError(ErrorCode::IOError, "Cannot write file: " + path->value);
        }
        file.write(content.data(), content.size());

        return Value::boolean(true);
    }, 2);

    vm.define_native("file_append", [](int argc, Value* args) -> Value {
        if (argc < 2) return Value::boolean(false);
        if (!args[0].is_object() || args[0].as_object<ObjHeader>()->type != ObjType::String) {
            throw RuntimeError(ErrorCode::TypeError, "file_append expects a string path");
        }
        auto* path = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        std::string content = value_to_string(args[1]);

        std::ofstream file(path->value, std::ios::binary | std::ios::app);
        if (!file.is_open()) {
            throw RuntimeError(ErrorCode::IOError, "Cannot append file: " + path->value);
        }
        file.write(content.data(), content.size());
        return Value::boolean(true);
    }, 2);

    vm.define_native("file_exists", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::boolean(false);
        auto* path = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        return Value::boolean(std::filesystem::exists(path->value));
    }, 1);

    vm.define_native("file_delete", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::boolean(false);
        auto* path = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        return Value::boolean(std::filesystem::remove(path->value));
    }, 1);

    vm.define_native("file_size", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::integer(-1);
        auto* path = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        if (!std::filesystem::exists(path->value)) return Value::integer(-1);
        return Value::integer(static_cast<i64>(std::filesystem::file_size(path->value)));
    }, 1);

    // ---- Directory Operations ----

    vm.define_native("dir_create", [](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::boolean(false);
        auto* path = static_cast<ObjString*>(args[0].as_object<ObjHeader>());
        return Value::boolean(std::filesystem::create_directories(path->value));
    }, 1);

    vm.define_native("dir_list", [&vm](int argc, Value* args) -> Value {
        if (argc < 1 || !args[0].is_object()) return Value::nil();
        auto* path = static_cast<ObjString*>(args[0].as_object<ObjHeader>());

        auto* arr = new ObjArray();
        vm.gc().track(arr);

        if (std::filesystem::exists(path->value) && std::filesystem::is_directory(path->value)) {
            for (auto& entry : std::filesystem::directory_iterator(path->value)) {
                arr->push(make_string(vm.gc(), entry.path().filename().string()));
            }
        }
        return Value::object(arr);
    }, 1);

    // ---- Standard Streams ----

    vm.define_native("stdin_read_line", [&vm](int, Value*) -> Value {
        std::string line;
        if (std::getline(std::cin, line)) {
            return make_string(vm.gc(), line);
        }
        return Value::nil();
    }, 0);

    vm.define_native("stdout_write", [](int argc, Value* args) -> Value {
        for (int i = 0; i < argc; i++) {
            std::cout << value_to_string(args[i]);
        }
        std::cout.flush();
        return Value::nil();
    }, 0);

    vm.define_native("stderr_write", [](int argc, Value* args) -> Value {
        for (int i = 0; i < argc; i++) {
            std::cerr << value_to_string(args[i]);
        }
        std::cerr.flush();
        return Value::nil();
    }, 0);
}

} // namespace grey
