// Grey Runtime - Module Loader Implementation
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.

#include "grey/module.h"
#include "grey/vm.h"
#include "grey/abi.h"

#include <fstream>
#include <algorithm>
#include <queue>
#include <set>

namespace grey {

// ============================================================
// Module Path Resolution
// ============================================================

std::string ModuleLoader::resolve_path(const std::string& name) const {
    namespace fs = std::filesystem;

    // Check if it's an absolute path
    if (fs::path(name).is_absolute() && fs::exists(name)) {
        return name;
    }

    // Search in search paths
    for (auto& base : search_paths_) {
        // Try with .greyc extension
        fs::path p = fs::path(base) / (name + ".greyc");
        if (fs::exists(p)) return p.string();

        // Try with .grey extension
        p = fs::path(base) / (name + ".grey");
        if (fs::exists(p)) return p.string();

        // Try as directory with index
        p = fs::path(base) / name / "index.greyc";
        if (fs::exists(p)) return p.string();

        // Try as directory with package.grey manifest
        p = fs::path(base) / name / "package.grey";
        if (fs::exists(p)) return p.string();
    }

    return ""; // not found
}

// ============================================================
// Module Loading
// ============================================================

ObjModule* ModuleLoader::load(const std::string& name, VM& vm) {
    // Check cache first
    auto it = loaded_modules_.find(name);
    if (it != loaded_modules_.end()) return it->second;

    // Resolve path
    std::string path = resolve_path(name);
    if (path.empty()) return nullptr;

    // Read file
    std::ifstream file(path, std::ios::binary | std::ios::ate);
    if (!file.is_open()) return nullptr;

    auto size = file.tellg();
    file.seekg(0);
    std::vector<byte> data(size);
    file.read(reinterpret_cast<char*>(data.data()), size);

    // Create module
    auto* module = new ObjModule(name);
    vm.gc().track(module);
    module->path = path;

    // Parse bytecode
    ModuleReader reader(data);
    if (reader.validate()) {
        auto functions = reader.functions();
        for (size_t i = 0; i < functions.size(); i++) {
            auto* fn = new ObjFunction();
            vm.gc().track(fn);
            fn->name = reader.get_string(functions[i].name_index);
            fn->arity = functions[i].arity;
            fn->chunk = reader.get_function_chunk(static_cast<u32>(i));
            fn->module = module;

            // Export the function
            module->exports.set(make_string(vm.gc(), fn->name), Value::object(fn));
        }
    }

    module->loaded = true;
    loaded_modules_[name] = module;

    // Track file timestamp for hot reload
    file_timestamps_[path] = std::filesystem::last_write_time(path);

    return module;
}

// ============================================================
// Hot Reload
// ============================================================

bool ModuleLoader::reload(const std::string& name, VM& vm) {
    auto it = loaded_modules_.find(name);
    if (it == loaded_modules_.end()) return false;

    std::string path = it->second->path;
    auto ts_it = file_timestamps_.find(path);
    if (ts_it == file_timestamps_.end()) return false;

    auto current_ts = std::filesystem::last_write_time(path);
    if (current_ts == ts_it->second) return false; // not modified

    // Remove old module and reload
    loaded_modules_.erase(it);
    ObjModule* reloaded = load(name, vm);
    return reloaded != nullptr;
}

// ============================================================
// Dependency Resolution (Topological Sort)
// ============================================================

std::vector<std::string> ModuleLoader::resolve_load_order(
    const std::string& root_module) const
{
    std::vector<std::string> order;
    std::set<std::string> visited;
    std::set<std::string> in_stack;

    std::function<bool(const std::string&)> visit = [&](const std::string& name) -> bool {
        if (in_stack.count(name)) {
            // Circular dependency detected
            return false;
        }
        if (visited.count(name)) return true;

        in_stack.insert(name);

        auto it = dependencies_.find(name);
        if (it != dependencies_.end()) {
            for (auto& dep : it->second.depends_on) {
                if (!visit(dep)) return false;
            }
        }

        in_stack.erase(name);
        visited.insert(name);
        order.push_back(name);
        return true;
    };

    visit(root_module);
    return order;
}

// ============================================================
// Package Manifest
// ============================================================

PackageManifest PackageManifest::load_from_file(const std::string& path) {
    PackageManifest manifest;
    // Simple key-value parsing (in a real impl, use JSON/TOML)
    std::ifstream file(path);
    if (!file.is_open()) return manifest;

    std::string line;
    while (std::getline(file, line)) {
        auto eq = line.find('=');
        if (eq == std::string::npos) continue;
        std::string key = line.substr(0, eq);
        std::string val = line.substr(eq + 1);
        // Trim
        key.erase(0, key.find_first_not_of(" \t"));
        key.erase(key.find_last_not_of(" \t") + 1);
        val.erase(0, val.find_first_not_of(" \t\""));
        val.erase(val.find_last_not_of(" \t\"") + 1);

        if (key == "name") manifest.name = val;
        else if (key == "version") manifest.version = val;
        else if (key == "entry") manifest.entry_point = val;
    }

    return manifest;
}

} // namespace grey
