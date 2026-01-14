/**
 * ═══════════════════════════════════════════════════════════════════════════════
 * Grey Optimizer - Sysctl Wrapper
 * ═══════════════════════════════════════════════════════════════════════════════
 *
 * Provides sysctl operations for kernel tunable optimization.
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

#define SYSCTL_PROC_PATH "/proc/sys"
#define MAX_TUNABLES 32
#define MAX_VALUE_SIZE 256

static int sysctl_initialized = 0;

/* Tunable definition */
typedef struct {
    const char *path;
    const char *optimized_value;
    char saved_value[MAX_VALUE_SIZE];
    int saved;
} sysctl_tunable_t;

/* Tunables to optimize */
static sysctl_tunable_t tunables[] = {
    /* VM tunables */
    {"/proc/sys/vm/swappiness", "10", {0}, 0},
    {"/proc/sys/vm/dirty_ratio", "10", {0}, 0},
    {"/proc/sys/vm/dirty_background_ratio", "5", {0}, 0},
    {"/proc/sys/vm/vfs_cache_pressure", "50", {0}, 0},
    
    /* Network tunables */
    {"/proc/sys/net/core/somaxconn", "4096", {0}, 0},
    {"/proc/sys/net/core/netdev_max_backlog", "4096", {0}, 0},
    
    /* Kernel tunables */
    {"/proc/sys/kernel/sched_autogroup_enabled", "0", {0}, 0},
    
    /* Sentinel */
    {NULL, NULL, {0}, 0}
};

/* ═══════════════════════════════════════════════════════════════════════════════
 * Helpers
 * ═══════════════════════════════════════════════════════════════════════════════ */

static int read_sysctl(const char *path, char *buffer, size_t size) {
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
    
    /* Remove trailing newline */
    char *nl = strchr(buffer, '\n');
    if (nl) *nl = '\0';
    
    return GREY_SUCCESS;
}

static int write_sysctl(const char *path, const char *value) {
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would write '%s' to %s\n", value, path);
        return GREY_SUCCESS;
    }

    int fd = open(path, O_WRONLY);
    if (fd < 0) {
        /* File not found is not critical - tunable may not exist */
        if (errno == ENOENT) {
            return GREY_SUCCESS;
        }
        grey_set_error(strerror(errno));
        return GREY_ERROR_PERMISSION_DENIED;
    }

    char buffer[MAX_VALUE_SIZE];
    snprintf(buffer, sizeof(buffer), "%s\n", value);
    
    ssize_t len = strlen(buffer);
    ssize_t written = write(fd, buffer, len);
    close(fd);

    if (written != len) {
        grey_set_error("Failed to write sysctl value");
        return GREY_ERROR_APPLY_FAILED;
    }

    return GREY_SUCCESS;
}

static int file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0;
}

#else

/* macOS implementation using sysctl */
#ifdef __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>

static int sysctl_initialized = 0;

/* macOS has different tunables accessible via sysctl(3) */
typedef struct {
    const char *name;
    int mib[4];
    size_t mib_len;
    int optimized_value;
    int saved_value;
    int saved;
} sysctl_tunable_macos_t;

static sysctl_tunable_macos_t macos_tunables[] = {
    /* Sentinel - add macOS-specific tunables as needed */
    {{0}, {0}, 0, 0, 0, 0}
};

#else

static int sysctl_initialized = 0;

#endif /* __APPLE__ */
#endif /* __linux__ */

/* ═══════════════════════════════════════════════════════════════════════════════
 * Public Functions
 * ═══════════════════════════════════════════════════════════════════════════════ */

int sysctl_wrapper_init(void) {
    if (sysctl_initialized) {
        return GREY_SUCCESS;
    }

#ifdef __linux__
    /* Save current values for rollback */
    for (int i = 0; tunables[i].path != NULL; i++) {
        if (file_exists(tunables[i].path)) {
            if (read_sysctl(tunables[i].path, tunables[i].saved_value, MAX_VALUE_SIZE) == GREY_SUCCESS) {
                tunables[i].saved = 1;
            }
        }
    }
#endif

    sysctl_initialized = 1;
    return GREY_SUCCESS;
}

void sysctl_wrapper_shutdown(void) {
    if (!sysctl_initialized) {
        return;
    }

    sysctl_initialized = 0;
}

int sysctl_apply_optimizations(void) {
    if (!sysctl_initialized) {
        grey_set_error("Sysctl not initialized");
        return GREY_ERROR_NOT_INITIALIZED;
    }

#ifdef __linux__
    int errors = 0;
    
    for (int i = 0; tunables[i].path != NULL; i++) {
        if (!file_exists(tunables[i].path)) {
            continue;
        }
        
        /* Save current value if not already saved */
        if (!tunables[i].saved) {
            if (read_sysctl(tunables[i].path, tunables[i].saved_value, MAX_VALUE_SIZE) == GREY_SUCCESS) {
                tunables[i].saved = 1;
            }
        }
        
        /* Apply optimized value */
        int result = write_sysctl(tunables[i].path, tunables[i].optimized_value);
        if (result != GREY_SUCCESS) {
            errors++;
        }
    }
    
    if (errors > 0) {
        grey_set_error("Some sysctls failed to apply");
        /* Don't return error - partial success is acceptable */
    }
    
    return GREY_SUCCESS;
#else
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would apply sysctl optimizations\n");
    }
    return GREY_SUCCESS;
#endif
}

int sysctl_restore_defaults(void) {
    if (!sysctl_initialized) {
        return GREY_SUCCESS;
    }

#ifdef __linux__
    for (int i = 0; tunables[i].path != NULL; i++) {
        if (!tunables[i].saved) {
            continue;
        }
        
        if (!file_exists(tunables[i].path)) {
            continue;
        }
        
        /* Restore saved value */
        write_sysctl(tunables[i].path, tunables[i].saved_value);
        tunables[i].saved = 0;
    }
    
    return GREY_SUCCESS;
#else
    if (grey_is_simulation()) {
        fprintf(stderr, "[SIMULATE] Would restore sysctl defaults\n");
    }
    return GREY_SUCCESS;
#endif
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Additional Utilities
 * ═══════════════════════════════════════════════════════════════════════════════ */

int sysctl_get_value(const char *name, char *buffer, size_t size) {
    if (!buffer || size == 0) {
        return GREY_ERROR_INVALID_ARGUMENT;
    }

#ifdef __linux__
    /* Convert dotted name to path */
    char path[GREY_MAX_PATH];
    snprintf(path, sizeof(path), SYSCTL_PROC_PATH "/%s", name);
    
    /* Replace dots with slashes */
    for (char *p = path + strlen(SYSCTL_PROC_PATH) + 1; *p; p++) {
        if (*p == '.') *p = '/';
    }
    
    return read_sysctl(path, buffer, size);
#else
    return GREY_ERROR_NOT_SUPPORTED;
#endif
}

int sysctl_set_value(const char *name, const char *value) {
    if (!name || !value) {
        return GREY_ERROR_INVALID_ARGUMENT;
    }

#ifdef __linux__
    /* Convert dotted name to path */
    char path[GREY_MAX_PATH];
    snprintf(path, sizeof(path), SYSCTL_PROC_PATH "/%s", name);
    
    /* Replace dots with slashes */
    for (char *p = path + strlen(SYSCTL_PROC_PATH) + 1; *p; p++) {
        if (*p == '.') *p = '/';
    }
    
    return write_sysctl(path, value);
#else
    return GREY_ERROR_NOT_SUPPORTED;
#endif
}
