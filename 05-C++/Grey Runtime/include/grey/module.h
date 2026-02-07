// Grey Runtime - Module & Package System
// Copyright (c) 2026 Grey Runtime Project. All rights reserved.
#pragma once

#include "grey/common.h"
#include "grey/objects.h"

#include <filesystem>
#include <fstream>

namespace grey {

// Forward
class VM;

// ============================================================
// Module Loader
// ============================================================

class ModuleLoader {
public:
    ModuleLoader() = default;

    // Add a search path for modules
    void add_search_path(const std::string& path) {
        search_paths_.push_back(path);
    }

    // Load a module by name
    ObjModule* load(const std::string& name, VM& vm);

    // Register a built-in module
    void register_builtin(const std::string& name, ObjModule* module) {
        loaded_modules_[name] = module;
    }

    // Check if a module is loaded
    bool is_loaded(const std::string& name) const {
        return loaded_modules_.count(name) > 0;
    }

    // Get a loaded module
    ObjModule* get(const std::string& name) const {
        auto it = loaded_modules_.find(name);
        if (it != loaded_modules_.end()) return it->second;
        return nullptr;
    }

    // Resolve module path
    std::string resolve_path(const std::string& name) const;

    // Hot reload support
    bool reload(const std::string& name, VM& vm);

    // Dependency resolver
    struct Dependency {
        std::string name;
        std::string version;
        std::vector<std::string> depends_on;
    };

    void add_dependency(const Dependency& dep) {
        dependencies_[dep.name] = dep;
    }

    // Resolve load order (topological sort)
    std::vector<std::string> resolve_load_order(const std::string& root_module) const;

    // Cache management
    void clear_cache() { loaded_modules_.clear(); }

private:
    std::vector<std::string> search_paths_;
    std::unordered_map<std::string, ObjModule*> loaded_modules_;
    std::unordered_map<std::string, Dependency> dependencies_;

    // File timestamp cache for hot reload
    std::unordered_map<std::string, std::filesystem::file_time_type> file_timestamps_;
};

// ============================================================
// Package manifest
// ============================================================

struct PackageManifest {
    std::string name;
    std::string version;
    std::string entry_point;
    std::vector<ModuleLoader::Dependency> dependencies;

    static PackageManifest load_from_file(const std::string& path);
};

} // namespace grey
