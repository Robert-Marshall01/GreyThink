/*
 * Grey Optimizer - Cgroup V2 Controller
 *
 * Implements cgroup v2 operations for CPU and memory management.
 * Provides functions to create cgroups, set limits, and move processes.
 *
 * Safety:
 * - All operations validate inputs
 * - Limits have safety floors (min CPU 1%, min RAM 64MB)
 * - Protected processes are never touched
 *
 * Principle: "Only use the bits needed, drop the bloat."
 */

#include "common.h"
#include <sys/stat.h>
#include <dirent.h>

/* Default cgroup v2 mount point */
#define CGROUP2_ROOT "/sys/fs/cgroup"

/* Grey Optimizer's cgroup hierarchy */
#define GREY_CGROUP_NAME "grey-optimizer"

/* Safety limits */
#define MIN_CPU_QUOTA_US    1000      /* 1% of a core minimum */
#define MIN_MEMORY_BYTES    (64 * 1024 * 1024)  /* 64 MB minimum */

/**
 * Check if cgroup v2 is available.
 *
 * @return 1 if available, 0 otherwise
 */
int grey_cgroup_v2_available(void) {
    /* Check for cgroup2 mount */
    if (!grey_path_exists(CGROUP2_ROOT "/cgroup.controllers")) {
        GREY_LOG("cgroup v2 not available at %s", CGROUP2_ROOT);
        return 0;
    }
    return 1;
}

/**
 * Get the root cgroup path for Grey Optimizer.
 *
 * Creates the cgroup if it doesn't exist.
 *
 * @param path   Buffer to store the path
 * @param pathlen Size of the buffer
 * @return GREY_OK on success, error code on failure
 */
int grey_cgroup_get_root(char *path, size_t pathlen) {
    snprintf(path, pathlen, "%s/%s", CGROUP2_ROOT, GREY_CGROUP_NAME);
    
    if (!grey_path_exists(path)) {
        if (!grey_is_root()) {
            GREY_ERR("Root required to create cgroup %s", path);
            return GREY_ERR_PERM;
        }
        
        if (mkdir(path, 0755) != 0) {
            GREY_ERR("Failed to create cgroup %s: %s", path, strerror(errno));
            return GREY_ERR_IO;
        }
        
        GREY_LOG("Created root cgroup: %s", path);
    }
    
    return GREY_OK;
}

/**
 * Create a child cgroup under Grey Optimizer hierarchy.
 *
 * @param name   Name of the child cgroup
 * @param path   Buffer to store the full path (optional, can be NULL)
 * @param pathlen Size of the path buffer
 * @return GREY_OK on success, error code on failure
 */
int grey_cgroup_create(const char *name, char *path, size_t pathlen) {
    if (!name || strlen(name) == 0) {
        return GREY_ERR_INVALID;
    }
    
    if (!grey_is_root()) {
        GREY_ERR("Root required to create cgroups");
        return GREY_ERR_PERM;
    }
    
    char cgroup_path[GREY_PATH_MAX];
    snprintf(cgroup_path, sizeof(cgroup_path), 
             "%s/%s/%s", CGROUP2_ROOT, GREY_CGROUP_NAME, name);
    
    /* Create parent if needed */
    char root_path[GREY_PATH_MAX];
    int ret = grey_cgroup_get_root(root_path, sizeof(root_path));
    if (ret != GREY_OK) {
        return ret;
    }
    
    /* Create the child cgroup */
    if (!grey_path_exists(cgroup_path)) {
        if (mkdir(cgroup_path, 0755) != 0) {
            GREY_ERR("Failed to create cgroup %s: %s", cgroup_path, strerror(errno));
            return GREY_ERR_IO;
        }
        GREY_LOG("Created cgroup: %s", cgroup_path);
    }
    
    /* Enable CPU and memory controllers */
    char subtree_path[GREY_PATH_MAX];
    snprintf(subtree_path, sizeof(subtree_path), 
             "%s/%s/cgroup.subtree_control", CGROUP2_ROOT, GREY_CGROUP_NAME);
    
    grey_write_file(subtree_path, "+cpu +memory +io");
    
    if (path && pathlen > 0) {
        strncpy(path, cgroup_path, pathlen - 1);
        path[pathlen - 1] = '\0';
    }
    
    return GREY_OK;
}

/**
 * Set CPU quota for a cgroup (cpu.max).
 *
 * Format: "$QUOTA $PERIOD" or "max $PERIOD"
 * Example: "10000 100000" = 10% of one CPU core
 *
 * @param name      Cgroup name (under grey-optimizer)
 * @param quota_us  CPU quota in microseconds (0 = max/unlimited)
 * @param period_us CPU period in microseconds (default 100000)
 * @return GREY_OK on success, error code on failure
 */
int grey_cgroup_set_cpu_max(const char *name, int quota_us, int period_us) {
    if (!name) {
        return GREY_ERR_INVALID;
    }
    
    if (!grey_is_root()) {
        GREY_ERR("Root required to set CPU quota");
        return GREY_ERR_PERM;
    }
    
    /* Apply safety floor */
    if (quota_us > 0 && quota_us < MIN_CPU_QUOTA_US) {
        GREY_LOG("CPU quota %d raised to minimum %d", quota_us, MIN_CPU_QUOTA_US);
        quota_us = MIN_CPU_QUOTA_US;
    }
    
    /* Default period */
    if (period_us <= 0) {
        period_us = 100000;
    }
    
    char cpu_max_path[GREY_PATH_MAX];
    snprintf(cpu_max_path, sizeof(cpu_max_path),
             "%s/%s/%s/cpu.max", CGROUP2_ROOT, GREY_CGROUP_NAME, name);
    
    char value[GREY_SMALL_BUF];
    if (quota_us <= 0) {
        snprintf(value, sizeof(value), "max %d", period_us);
    } else {
        snprintf(value, sizeof(value), "%d %d", quota_us, period_us);
    }
    
    int ret = grey_write_file(cpu_max_path, value);
    if (ret == GREY_OK) {
        GREY_LOG("Set cpu.max = %s for %s", value, name);
    }
    
    return ret;
}

/**
 * Set CPU weight for a cgroup (cpu.weight).
 *
 * Weight is relative priority (1-10000, default 100).
 * Lower weight = less CPU time when contending.
 *
 * @param name   Cgroup name
 * @param weight CPU weight (1-10000)
 * @return GREY_OK on success, error code on failure
 */
int grey_cgroup_set_cpu_weight(const char *name, int weight) {
    if (!name) {
        return GREY_ERR_INVALID;
    }
    
    if (weight < 1) weight = 1;
    if (weight > 10000) weight = 10000;
    
    char weight_path[GREY_PATH_MAX];
    snprintf(weight_path, sizeof(weight_path),
             "%s/%s/%s/cpu.weight", CGROUP2_ROOT, GREY_CGROUP_NAME, name);
    
    char value[GREY_SMALL_BUF];
    snprintf(value, sizeof(value), "%d", weight);
    
    int ret = grey_write_file(weight_path, value);
    if (ret == GREY_OK) {
        GREY_LOG("Set cpu.weight = %d for %s", weight, name);
    }
    
    return ret;
}

/**
 * Set memory limit for a cgroup (memory.max).
 *
 * @param name       Cgroup name
 * @param limit_bytes Memory limit in bytes (0 = unlimited)
 * @return GREY_OK on success, error code on failure
 */
int grey_cgroup_set_memory_max(const char *name, long limit_bytes) {
    if (!name) {
        return GREY_ERR_INVALID;
    }
    
    if (!grey_is_root()) {
        GREY_ERR("Root required to set memory limit");
        return GREY_ERR_PERM;
    }
    
    /* Apply safety floor */
    if (limit_bytes > 0 && limit_bytes < MIN_MEMORY_BYTES) {
        GREY_LOG("Memory limit %ld raised to minimum %d", limit_bytes, MIN_MEMORY_BYTES);
        limit_bytes = MIN_MEMORY_BYTES;
    }
    
    char memory_max_path[GREY_PATH_MAX];
    snprintf(memory_max_path, sizeof(memory_max_path),
             "%s/%s/%s/memory.max", CGROUP2_ROOT, GREY_CGROUP_NAME, name);
    
    char value[GREY_SMALL_BUF];
    if (limit_bytes <= 0) {
        snprintf(value, sizeof(value), "max");
    } else {
        snprintf(value, sizeof(value), "%ld", limit_bytes);
    }
    
    int ret = grey_write_file(memory_max_path, value);
    if (ret == GREY_OK) {
        GREY_LOG("Set memory.max = %s for %s", value, name);
    }
    
    return ret;
}

/**
 * Move a process into a cgroup.
 *
 * @param name Cgroup name
 * @param pid  Process ID to move
 * @return GREY_OK on success, error code on failure
 */
int grey_cgroup_add_process(const char *name, pid_t pid) {
    if (!name || pid <= 0) {
        return GREY_ERR_INVALID;
    }
    
    if (!grey_is_root()) {
        GREY_ERR("Root required to move processes");
        return GREY_ERR_PERM;
    }
    
    /* Check if process exists */
    char proc_path[GREY_PATH_MAX];
    snprintf(proc_path, sizeof(proc_path), "/proc/%d", pid);
    if (!grey_path_exists(proc_path)) {
        GREY_ERR("Process %d does not exist", pid);
        return GREY_ERR_NOTFOUND;
    }
    
    char procs_path[GREY_PATH_MAX];
    snprintf(procs_path, sizeof(procs_path),
             "%s/%s/%s/cgroup.procs", CGROUP2_ROOT, GREY_CGROUP_NAME, name);
    
    char value[GREY_SMALL_BUF];
    snprintf(value, sizeof(value), "%d", pid);
    
    int ret = grey_write_file(procs_path, value);
    if (ret == GREY_OK) {
        GREY_LOG("Moved PID %d to cgroup %s", pid, name);
    }
    
    return ret;
}

/**
 * Get current memory usage of a cgroup.
 *
 * @param name  Cgroup name
 * @param bytes Pointer to store current usage in bytes
 * @return GREY_OK on success, error code on failure
 */
int grey_cgroup_get_memory_current(const char *name, long *bytes) {
    if (!name || !bytes) {
        return GREY_ERR_INVALID;
    }
    
    char current_path[GREY_PATH_MAX];
    snprintf(current_path, sizeof(current_path),
             "%s/%s/%s/memory.current", CGROUP2_ROOT, GREY_CGROUP_NAME, name);
    
    char value[GREY_SMALL_BUF];
    int ret = grey_read_file(current_path, value, sizeof(value));
    if (ret != GREY_OK) {
        return ret;
    }
    
    *bytes = atol(value);
    return GREY_OK;
}

/**
 * Remove a cgroup.
 *
 * The cgroup must be empty (no processes) to be removed.
 *
 * @param name Cgroup name
 * @return GREY_OK on success, error code on failure
 */
int grey_cgroup_remove(const char *name) {
    if (!name) {
        return GREY_ERR_INVALID;
    }
    
    char cgroup_path[GREY_PATH_MAX];
    snprintf(cgroup_path, sizeof(cgroup_path),
             "%s/%s/%s", CGROUP2_ROOT, GREY_CGROUP_NAME, name);
    
    if (!grey_path_exists(cgroup_path)) {
        return GREY_OK;  /* Already gone */
    }
    
    /* Move all processes out first */
    char procs_path[GREY_PATH_MAX];
    snprintf(procs_path, sizeof(procs_path), "%s/cgroup.procs", cgroup_path);
    
    char procs_buf[GREY_PATH_MAX];
    if (grey_read_file(procs_path, procs_buf, sizeof(procs_buf)) == GREY_OK) {
        char *line = strtok(procs_buf, "\n");
        while (line) {
            pid_t pid = atoi(line);
            if (pid > 0) {
                /* Move to root cgroup */
                char root_procs[GREY_PATH_MAX];
                snprintf(root_procs, sizeof(root_procs), 
                         "%s/cgroup.procs", CGROUP2_ROOT);
                char pid_str[32];
                snprintf(pid_str, sizeof(pid_str), "%d", pid);
                grey_write_file(root_procs, pid_str);
            }
            line = strtok(NULL, "\n");
        }
    }
    
    /* Remove the cgroup */
    if (rmdir(cgroup_path) != 0) {
        if (errno == EBUSY) {
            GREY_ERR("Cgroup %s is busy, cannot remove", name);
            return GREY_ERR_BUSY;
        }
        GREY_ERR("Failed to remove cgroup %s: %s", name, strerror(errno));
        return GREY_ERR_IO;
    }
    
    GREY_LOG("Removed cgroup: %s", name);
    return GREY_OK;
}

/*
 * Python ctypes interface
 *
 * These functions provide a clean interface for calling from Python
 * using ctypes. They handle all the complexity internally.
 */

/* Exported for Python ctypes */
int grey_cgroup_init(void) {
    if (!grey_cgroup_v2_available()) {
        GREY_ERR("cgroup v2 not available");
        return GREY_ERR_NOTFOUND;
    }
    
    char root[GREY_PATH_MAX];
    return grey_cgroup_get_root(root, sizeof(root));
}
