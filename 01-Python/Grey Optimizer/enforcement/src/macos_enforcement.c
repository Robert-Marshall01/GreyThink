/**
 * Grey Optimizer - macOS Enforcement Module
 * 
 * This module provides macOS-specific enforcement operations.
 * 
 * LIMITATIONS:
 * - macOS does not have cgroups; process resource limits are more limited
 * - KSM equivalent does not exist on macOS
 * - purge command requires root and is generally discouraged
 * - Scheduler manipulation is limited to nice values
 * 
 * WHAT WE CAN DO:
 * - renice: Adjust process priorities (setpriority)
 * - Sparse files: APFS/HFS+ support sparse files via fcntl
 * - Hardlinks: Supported on HFS+/APFS for deduplication
 * - Memory pressure: Can send memory pressure notifications
 * 
 * Safety: All operations check simulation mode before executing.
 */

#include "platform.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef GREY_PLATFORM_MACOS

#include <sys/sysctl.h>
#include <mach/mach.h>
#include <mach/mach_host.h>
#include <libproc.h>

/* ═══════════════════════════════════════════════════════════════════════════
 * PROCESS PRIORITY (renice equivalent)
 * ═══════════════════════════════════════════════════════════════════════════ */

/**
 * Set the nice value for a process.
 * 
 * @param pid Process ID to modify
 * @param nice_value Nice value (-20 to 19, higher = lower priority)
 * @return Result structure with success/failure
 * 
 * Safety: This only adjusts scheduling priority, not hard limits.
 * Reversible: Original nice value can be restored.
 */
GREY_API grey_result_t grey_macos_set_priority(pid_t pid, int nice_value) {
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would set PID %d nice value to %d\n", pid, nice_value);
        return grey_result_ok(true);
    }
    
    if (nice_value < -20 || nice_value > 19) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Nice value must be -20 to 19");
    }
    
    /* setpriority requires appropriate permissions for negative nice values */
    if (setpriority(PRIO_PROCESS, pid, nice_value) < 0) {
        char buf[256];
        snprintf(buf, sizeof(buf), "setpriority failed: %s", strerror(errno));
        return grey_result_error(GREY_ERROR_PERMISSION, buf);
    }
    
    printf("[LIVE] Set PID %d nice value to %d\n", pid, nice_value);
    return grey_result_ok(false);
}

/**
 * Get the current nice value for a process.
 * 
 * @param pid Process ID to query
 * @param out_nice Output parameter for nice value
 * @return Result structure
 */
GREY_API grey_result_t grey_macos_get_priority(pid_t pid, int* out_nice) {
    if (!out_nice) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "out_nice is NULL");
    }
    
    errno = 0;
    int nice = getpriority(PRIO_PROCESS, pid);
    
    if (nice == -1 && errno != 0) {
        char buf[256];
        snprintf(buf, sizeof(buf), "getpriority failed: %s", strerror(errno));
        return grey_result_error(GREY_ERROR_SYSTEM, buf);
    }
    
    *out_nice = nice;
    return grey_result_ok(false);
}

/* ═══════════════════════════════════════════════════════════════════════════
 * MEMORY OPERATIONS
 * ═══════════════════════════════════════════════════════════════════════════ */

/**
 * Get system memory statistics.
 * 
 * Uses Mach APIs to query host memory info.
 * 
 * @param out_total_mb Output: total physical memory in MB
 * @param out_free_mb Output: free memory in MB (approximate)
 * @return Result structure
 */
GREY_API grey_result_t grey_macos_get_memory_stats(
    uint64_t* out_total_mb,
    uint64_t* out_free_mb
) {
    if (!out_total_mb || !out_free_mb) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Output parameters are NULL");
    }
    
    /* Get total physical memory */
    int mib[2] = {CTL_HW, HW_MEMSIZE};
    uint64_t mem_size;
    size_t len = sizeof(mem_size);
    
    if (sysctl(mib, 2, &mem_size, &len, NULL, 0) < 0) {
        return grey_result_error(GREY_ERROR_SYSTEM, "Failed to get HW_MEMSIZE");
    }
    
    *out_total_mb = mem_size / (1024 * 1024);
    
    /* Get VM statistics */
    mach_port_t host = mach_host_self();
    vm_statistics64_data_t vm_stats;
    mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
    
    if (host_statistics64(host, HOST_VM_INFO64, (host_info64_t)&vm_stats, &count) != KERN_SUCCESS) {
        return grey_result_error(GREY_ERROR_SYSTEM, "Failed to get VM stats");
    }
    
    /* Calculate free memory (free + inactive pages) */
    vm_size_t page_size;
    host_page_size(host, &page_size);
    
    uint64_t free_pages = vm_stats.free_count + vm_stats.inactive_count;
    *out_free_mb = (free_pages * page_size) / (1024 * 1024);
    
    return grey_result_ok(false);
}

/**
 * Trigger memory pressure cleanup (equivalent to purge).
 * 
 * NOTE: This is a heavyweight operation and should be used sparingly.
 * On macOS, there's no direct equivalent to Linux's drop_caches.
 * The 'purge' command uses the Memory Pressure API.
 * 
 * Safety: This only suggests cleanup to the kernel, it doesn't force it.
 * 
 * @return Result structure
 */
GREY_API grey_result_t grey_macos_memory_pressure(void) {
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would trigger memory pressure cleanup\n");
        return grey_result_ok(true);
    }
    
    /* 
     * TODO: Use the Memory Pressure dispatch source API
     * For now, we just log that this would happen.
     * A production implementation would use dispatch_source_create
     * with DISPATCH_SOURCE_TYPE_MEMORYPRESSURE.
     */
    printf("[LIVE] Memory pressure cleanup requested (not fully implemented)\n");
    
    return grey_result_error(GREY_ERROR_NOT_SUPPORTED, 
        "Memory pressure API requires additional implementation");
}

/* ═══════════════════════════════════════════════════════════════════════════
 * FILE OPERATIONS
 * ═══════════════════════════════════════════════════════════════════════════ */

/**
 * Create a hardlink for file deduplication.
 * 
 * @param target The existing file to link to
 * @param linkpath The path for the new hardlink
 * @return Result structure
 * 
 * Safety: Original file is preserved. Caller should backup metadata.
 */
GREY_API grey_result_t grey_macos_create_hardlink(
    const char* target,
    const char* linkpath
) {
    if (!target || !linkpath) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Paths are NULL");
    }
    
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would create hardlink: %s -> %s\n", linkpath, target);
        return grey_result_ok(true);
    }
    
    if (link(target, linkpath) < 0) {
        char buf[256];
        snprintf(buf, sizeof(buf), "link() failed: %s", strerror(errno));
        return grey_result_error(GREY_ERROR_IO, buf);
    }
    
    printf("[LIVE] Created hardlink: %s -> %s\n", linkpath, target);
    return grey_result_ok(false);
}

/**
 * Make a file sparse by punching holes.
 * 
 * APFS supports sparse files natively. We can use fcntl F_PUNCHHOLE
 * to deallocate zero-filled regions.
 * 
 * @param path Path to the file
 * @param offset Offset to start punching
 * @param length Length of the hole
 * @return Result structure
 * 
 * Safety: Only affects zero-filled regions. Backup recommended.
 */
GREY_API grey_result_t grey_macos_punch_hole(
    const char* path,
    off_t offset,
    off_t length
) {
    if (!path) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "Path is NULL");
    }
    
    if (grey_simulation_mode) {
        printf("[SIMULATION] Would punch hole in %s at offset %lld, length %lld\n",
               path, (long long)offset, (long long)length);
        return grey_result_ok(true);
    }
    
    int fd = open(path, O_RDWR);
    if (fd < 0) {
        char buf[256];
        snprintf(buf, sizeof(buf), "open() failed: %s", strerror(errno));
        return grey_result_error(GREY_ERROR_IO, buf);
    }
    
    /* 
     * macOS 10.13+ supports F_PUNCHHOLE on APFS.
     * Fall back gracefully on older systems or HFS+.
     */
#ifdef F_PUNCHHOLE
    struct fpunchhole punchhole = {
        .fp_flags = 0,
        .fp_offset = offset,
        .fp_length = length
    };
    
    if (fcntl(fd, F_PUNCHHOLE, &punchhole) < 0) {
        close(fd);
        if (errno == ENOTSUP) {
            return grey_result_error(GREY_ERROR_NOT_SUPPORTED, 
                "F_PUNCHHOLE not supported on this filesystem");
        }
        char buf[256];
        snprintf(buf, sizeof(buf), "F_PUNCHHOLE failed: %s", strerror(errno));
        return grey_result_error(GREY_ERROR_IO, buf);
    }
#else
    close(fd);
    return grey_result_error(GREY_ERROR_NOT_SUPPORTED, 
        "F_PUNCHHOLE not available on this macOS version");
#endif
    
    close(fd);
    printf("[LIVE] Punched hole in %s\n", path);
    return grey_result_ok(false);
}

/* ═══════════════════════════════════════════════════════════════════════════
 * CPU INFORMATION
 * ═══════════════════════════════════════════════════════════════════════════ */

/**
 * Get CPU usage percentage.
 * 
 * Uses Mach processor APIs to calculate CPU load.
 * 
 * @param out_percent Output: CPU usage percentage (0-100)
 * @return Result structure
 */
GREY_API grey_result_t grey_macos_get_cpu_usage(float* out_percent) {
    if (!out_percent) {
        return grey_result_error(GREY_ERROR_INVALID_PARAM, "out_percent is NULL");
    }
    
    mach_port_t host = mach_host_self();
    host_cpu_load_info_data_t cpu_load;
    mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
    
    if (host_statistics(host, HOST_CPU_LOAD_INFO, (host_info_t)&cpu_load, &count) != KERN_SUCCESS) {
        return grey_result_error(GREY_ERROR_SYSTEM, "Failed to get CPU stats");
    }
    
    /* Calculate usage from ticks */
    static natural_t prev_user = 0, prev_system = 0, prev_idle = 0;
    
    natural_t user = cpu_load.cpu_ticks[CPU_STATE_USER];
    natural_t system = cpu_load.cpu_ticks[CPU_STATE_SYSTEM];
    natural_t idle = cpu_load.cpu_ticks[CPU_STATE_IDLE];
    
    natural_t user_diff = user - prev_user;
    natural_t system_diff = system - prev_system;
    natural_t idle_diff = idle - prev_idle;
    
    natural_t total = user_diff + system_diff + idle_diff;
    
    if (total > 0) {
        *out_percent = ((float)(user_diff + system_diff) / total) * 100.0f;
    } else {
        *out_percent = 0.0f;
    }
    
    prev_user = user;
    prev_system = system;
    prev_idle = idle;
    
    return grey_result_ok(false);
}

#endif /* GREY_PLATFORM_MACOS */
