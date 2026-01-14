/**
 * ═══════════════════════════════════════════════════════════════════════════════
 * Grey Optimizer - Cgroup Wrapper
 * ═══════════════════════════════════════════════════════════════════════════════
 *
 * Provides cgroup (v2) operations for resource limiting.
 *
 * ═══════════════════════════════════════════════════════════════════════════════
 */

#include "enforce.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef __linux__
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define CGROUP_V2_ROOT "/sys/fs/cgroup"
#define CGROUP_GREY_PATH CGROUP_V2_ROOT "/" GREY_CGROUP_NAME

static int cgroup_initialized = 0;
static int cgroup_created = 0;

/* Saved defaults for rollback */
static struct {
    long cpu_period;
    long cpu_quota;
    long memory_max;
    int io_weight;
} saved_defaults = {0};

/* ═══════════════════════════════════════════════════════════════════════════════
 * Helpers
 * ═══════════════════════════════════════════════════════════════════════════════ */

static int write_file(const char *path, const char *value) {
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would write '%s' to %s\n", value, path);
        return GREY_SUCCESS;
    }

    int fd = open(path, O_WRONLY);
    if (fd < 0) {
        grey_set_error(strerror(errno));
        return GREY_ERROR_FILE_NOT_FOUND;
    }

    ssize_t len = strlen(value);
    ssize_t written = write(fd, value, len);
    close(fd);

    if (written != len) {
        grey_set_error("Failed to write cgroup value");
        return GREY_ERROR_APPLY_FAILED;
    }

    return GREY_SUCCESS;
}

static int read_file_value(const char *path, char *buffer, size_t size) {
    int fd = open(path, O_RDONLY);
    if (fd < 0) {
        return GREY_ERROR_FILE_NOT_FOUND;
    }

    ssize_t n = read(fd, buffer, size - 1);
    close(fd);

    if (n < 0) {
        return GREY_ERROR_GENERIC;
    }

    buffer[n] = '\0';
    return GREY_SUCCESS;
}

static int directory_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

static int create_cgroup_directory(void) {
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would create cgroup directory: %s\n", CGROUP_GREY_PATH);
        return GREY_SUCCESS;
    }

    if (directory_exists(CGROUP_GREY_PATH)) {
        return GREY_SUCCESS;
    }

    if (mkdir(CGROUP_GREY_PATH, 0755) != 0) {
        grey_set_error(strerror(errno));
        return GREY_ERROR_PERMISSION_DENIED;
    }

    cgroup_created = 1;
    return GREY_SUCCESS;
}

static int remove_cgroup_directory(void) {
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would remove cgroup directory: %s\n", CGROUP_GREY_PATH);
        return GREY_SUCCESS;
    }

    if (!directory_exists(CGROUP_GREY_PATH)) {
        return GREY_SUCCESS;
    }

    /* Move all processes out of cgroup first */
    char buffer[4096];
    char procs_path[GREY_MAX_PATH];
    snprintf(procs_path, sizeof(procs_path), "%s/cgroup.procs", CGROUP_GREY_PATH);
    
    if (read_file_value(procs_path, buffer, sizeof(buffer)) == GREY_SUCCESS) {
        /* Move each PID to root cgroup */
        char root_procs[GREY_MAX_PATH];
        snprintf(root_procs, sizeof(root_procs), "%s/cgroup.procs", CGROUP_V2_ROOT);
        
        char *pid_str = strtok(buffer, "\n");
        while (pid_str) {
            write_file(root_procs, pid_str);
            pid_str = strtok(NULL, "\n");
        }
    }

    if (rmdir(CGROUP_GREY_PATH) != 0) {
        grey_set_error(strerror(errno));
        return GREY_ERROR_GENERIC;
    }

    return GREY_SUCCESS;
}

#else

/* Stub implementations for non-Linux */
static int cgroup_initialized = 0;

#endif /* __linux__ */

/* ═══════════════════════════════════════════════════════════════════════════════
 * Public Functions
 * ═══════════════════════════════════════════════════════════════════════════════ */

int cgroup_wrapper_init(void) {
    if (cgroup_initialized) {
        return GREY_SUCCESS;
    }

#ifdef __linux__
    /* Check if cgroup v2 is available */
    if (!directory_exists(CGROUP_V2_ROOT)) {
        grey_set_error("Cgroup v2 not available");
        return GREY_ERROR_NOT_SUPPORTED;
    }

    /* Create our cgroup */
    int result = create_cgroup_directory();
    if (result != GREY_SUCCESS) {
        return result;
    }

    /* Save current defaults for rollback */
    saved_defaults.cpu_period = 100000;  /* 100ms default */
    saved_defaults.cpu_quota = -1;       /* unlimited */
    saved_defaults.memory_max = -1;      /* unlimited */
    saved_defaults.io_weight = 100;      /* default weight */

    cgroup_initialized = 1;
    return GREY_SUCCESS;
#else
    /* Non-Linux: simulation only */
    cgroup_initialized = 1;
    return GREY_SUCCESS;
#endif
}

void cgroup_wrapper_shutdown(void) {
    if (!cgroup_initialized) {
        return;
    }

#ifdef __linux__
    if (cgroup_created && !grey_is_simulation()) {
        remove_cgroup_directory();
    }
#endif

    cgroup_initialized = 0;
}

int cgroup_set_cpu_limit(int percent) {
    if (!cgroup_initialized) {
        grey_set_error("Cgroup not initialized");
        return GREY_ERROR_NOT_INITIALIZED;
    }

    if (percent < 1 || percent > 100) {
        grey_set_error("CPU limit must be 1-100");
        return GREY_ERROR_INVALID_ARGUMENT;
    }

#ifdef __linux__
    char value[64];
    char path[GREY_MAX_PATH];
    
    /* cpu.max format: "quota period" (in microseconds) */
    long period = 100000;  /* 100ms */
    long quota = (period * percent) / 100;
    
    snprintf(value, sizeof(value), "%ld %ld", quota, period);
    snprintf(path, sizeof(path), "%s/cpu.max", CGROUP_GREY_PATH);
    
    return write_file(path, value);
#else
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would set CPU limit to %d%%\n", percent);
    }
    return GREY_SUCCESS;
#endif
}

int cgroup_set_memory_limit(int percent) {
    if (!cgroup_initialized) {
        grey_set_error("Cgroup not initialized");
        return GREY_ERROR_NOT_INITIALIZED;
    }

    if (percent < 1 || percent > 100) {
        grey_set_error("Memory limit must be 1-100");
        return GREY_ERROR_INVALID_ARGUMENT;
    }

#ifdef __linux__
    char value[64];
    char path[GREY_MAX_PATH];
    
    /* Get total memory from meminfo */
    long total_mem = sysconf(_SC_PHYS_PAGES) * sysconf(_SC_PAGE_SIZE);
    long limit = (total_mem * percent) / 100;
    
    snprintf(value, sizeof(value), "%ld", limit);
    snprintf(path, sizeof(path), "%s/memory.max", CGROUP_GREY_PATH);
    
    return write_file(path, value);
#else
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would set memory limit to %d%%\n", percent);
    }
    return GREY_SUCCESS;
#endif
}

int cgroup_set_io_weight(int weight) {
    if (!cgroup_initialized) {
        grey_set_error("Cgroup not initialized");
        return GREY_ERROR_NOT_INITIALIZED;
    }

    if (weight < 1 || weight > 1000) {
        grey_set_error("IO weight must be 1-1000");
        return GREY_ERROR_INVALID_ARGUMENT;
    }

#ifdef __linux__
    char value[64];
    char path[GREY_MAX_PATH];
    
    /* io.weight format: "default <weight>" */
    snprintf(value, sizeof(value), "default %d", weight);
    snprintf(path, sizeof(path), "%s/io.weight", CGROUP_GREY_PATH);
    
    /* io.weight may not exist on all systems */
    int result = write_file(path, value);
    if (result == GREY_ERROR_FILE_NOT_FOUND) {
        /* Not critical - just log and continue */
        return GREY_SUCCESS;
    }
    return result;
#else
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would set IO weight to %d\n", weight);
    }
    return GREY_SUCCESS;
#endif
}

int cgroup_restore_defaults(void) {
    if (!cgroup_initialized) {
        return GREY_SUCCESS;
    }

#ifdef __linux__
    char path[GREY_MAX_PATH];
    
    /* Restore CPU to unlimited */
    snprintf(path, sizeof(path), "%s/cpu.max", CGROUP_GREY_PATH);
    write_file(path, "max 100000");
    
    /* Restore memory to unlimited */
    snprintf(path, sizeof(path), "%s/memory.max", CGROUP_GREY_PATH);
    write_file(path, "max");
    
    /* Restore IO weight */
    snprintf(path, sizeof(path), "%s/io.weight", CGROUP_GREY_PATH);
    write_file(path, "default 100");
    
    return GREY_SUCCESS;
#else
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would restore cgroup defaults\n");
    }
    return GREY_SUCCESS;
#endif
}
