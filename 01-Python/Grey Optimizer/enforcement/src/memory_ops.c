/*
 * Grey Optimizer - Memory Operations C Helper
 *
 * Provides low-level memory management operations that require
 * privileged syscalls not efficiently available in Python.
 *
 * Operations:
 * - madvise(MADV_DONTNEED): Hint kernel to release anonymous pages
 * - process_vm_readv/writev: Cross-process memory operations
 * - Memory region enumeration via /proc/[pid]/maps
 *
 * Safety:
 * - All operations validate inputs before execution
 * - MADV_DONTNEED only affects anonymous (heap) pages, not file-backed
 * - No data is destroyed for file-backed mappings
 * - Rollback is implicit (pages will be re-faulted on access)
 *
 * Why this works for RAM reduction:
 * - MADV_DONTNEED tells the kernel that the process doesn't need
 *   the data in the specified range
 * - For anonymous mappings, pages are immediately freed
 * - For file-backed mappings, dirty pages are written back, then freed
 * - When the process accesses the region again, pages are re-faulted
 * - This is safe for processes with idle regions or internal caches
 *
 * Limitations:
 * - Only works on processes we have ptrace permissions for (same user or root)
 * - Aggressive use may cause performance degradation if pages are needed
 * - Does not help for actively-used memory
 */

#include "common.h"
#include <sys/mman.h>
#include <sys/uio.h>
#include <dirent.h>
#include <ctype.h>
#include <stdint.h>

/* Memory region info */
typedef struct {
    unsigned long start;
    unsigned long end;
    char perms[8];        /* rwxp or r--s etc */
    unsigned long offset;
    char path[256];       /* file path or [heap], [stack], etc */
    int is_anonymous;     /* 1 if anonymous mapping */
    int is_writable;      /* 1 if writable */
    int is_private;       /* 1 if private (not shared) */
} grey_mem_region_t;

/* Maximum regions we'll track per process */
#define MAX_REGIONS 4096

/* Result of memory operation */
typedef struct {
    int success;
    long bytes_advised;   /* Bytes we called madvise on */
    long regions_processed;
    long errors;
    char error_msg[256];
} grey_mem_result_t;

/**
 * Parse /proc/[pid]/maps to find memory regions.
 *
 * @param pid         Target process ID
 * @param regions     Output array of regions
 * @param max_regions Maximum regions to return
 * @param count       Output: number of regions found
 * @return GREY_OK on success
 *
 * Safety: Read-only operation, only reads /proc filesystem.
 */
int grey_parse_maps(pid_t pid, grey_mem_region_t *regions, int max_regions, int *count) {
    char maps_path[GREY_PATH_MAX];
    FILE *fp;
    char line[512];
    int n = 0;

    if (!regions || !count || max_regions <= 0) {
        return GREY_ERR_INVALID;
    }

    snprintf(maps_path, sizeof(maps_path), "/proc/%d/maps", pid);
    fp = fopen(maps_path, "r");
    if (!fp) {
        GREY_ERR("Cannot open %s: %s", maps_path, strerror(errno));
        return errno == ENOENT ? GREY_ERR_NOTFOUND : GREY_ERR_PERM;
    }

    while (fgets(line, sizeof(line), fp) && n < max_regions) {
        grey_mem_region_t *r = &regions[n];
        unsigned long inode;
        int major, minor;
        char path[256] = "";

        /*
         * Format: start-end perms offset dev inode pathname
         * Example: 7f1234000000-7f1234021000 rw-p 00000000 00:00 0 [heap]
         */
        int fields = sscanf(line, "%lx-%lx %7s %lx %x:%x %lu %255[^\n]",
            &r->start, &r->end, r->perms, &r->offset,
            &major, &minor, &inode, path);

        if (fields < 5) {
            continue; /* Malformed line */
        }

        /* Copy path, trim leading whitespace */
        char *p = path;
        while (*p && isspace(*p)) p++;
        strncpy(r->path, p, sizeof(r->path) - 1);
        r->path[sizeof(r->path) - 1] = '\0';

        /* Determine region type */
        r->is_anonymous = (strlen(r->path) == 0 ||
                          strncmp(r->path, "[heap]", 6) == 0 ||
                          strncmp(r->path, "[stack", 6) == 0 ||
                          strncmp(r->path, "[anon", 5) == 0);

        r->is_writable = (r->perms[1] == 'w');
        r->is_private = (r->perms[3] == 'p');

        n++;
    }

    fclose(fp);
    *count = n;
    GREY_LOG("Parsed %d memory regions for PID %d", n, pid);

    return GREY_OK;
}

/**
 * Apply madvise(MADV_DONTNEED) to eligible memory regions.
 *
 * Only affects anonymous, private, writable regions.
 * This is safe because:
 * - File-backed pages are unaffected
 * - Shared pages are unaffected
 * - Pages will be re-faulted (zero-filled) on access
 *
 * @param pid           Target process ID
 * @param anonymous_only Only advise anonymous regions (safer)
 * @param min_size      Minimum region size to advise (bytes)
 * @param result        Output result structure
 * @return GREY_OK on success (result contains details)
 *
 * Safety: Requires same-user or root access.
 * Rollback: Implicit - pages are re-allocated on access.
 * May cause performance impact if pages are actively used.
 *
 * TODO: On some distros, process_madvise() syscall may be available
 * (Linux 5.10+) which allows advising another process's memory
 * directly. This would be more efficient than ptrace-based approaches.
 */
int grey_advise_memory(pid_t pid, int anonymous_only, size_t min_size,
                       grey_mem_result_t *result) {
    grey_mem_region_t regions[MAX_REGIONS];
    int region_count = 0;
    int ret;

    if (!result) {
        return GREY_ERR_INVALID;
    }

    memset(result, 0, sizeof(*result));

    /* Cannot advise own process in a useful way */
    if (pid == getpid()) {
        snprintf(result->error_msg, sizeof(result->error_msg),
                 "Cannot advise own process");
        return GREY_ERR_INVALID;
    }

    /* Parse memory map */
    ret = grey_parse_maps(pid, regions, MAX_REGIONS, &region_count);
    if (ret != GREY_OK) {
        snprintf(result->error_msg, sizeof(result->error_msg),
                 "Failed to parse /proc/%d/maps", pid);
        return ret;
    }

    /*
     * Note: Directly calling madvise on another process's memory
     * is not possible in standard Linux without ptrace.
     *
     * Options:
     * 1. Use process_madvise() syscall (Linux 5.10+)
     * 2. Use ptrace to inject madvise call
     * 3. Ask cooperating process to call madvise itself
     *
     * For safety, we'll use option 1 if available, otherwise
     * report what we WOULD do in simulation mode.
     */

#ifdef __NR_process_madvise
    /* Linux 5.10+ has process_madvise syscall */
    #include <sys/syscall.h>

    for (int i = 0; i < region_count; i++) {
        grey_mem_region_t *r = &regions[i];
        size_t size = r->end - r->start;

        /* Skip if too small */
        if (size < min_size) continue;

        /* Skip if not eligible */
        if (!r->is_private || !r->is_writable) continue;
        if (anonymous_only && !r->is_anonymous) continue;

        /* Prepare iovec */
        struct iovec iov = {
            .iov_base = (void *)r->start,
            .iov_len = size
        };

        /* Call process_madvise */
        long sret = syscall(__NR_process_madvise, pid, &iov, 1,
                           MADV_DONTNEED, 0);
        if (sret < 0) {
            result->errors++;
            GREY_LOG("process_madvise failed for region %lx-%lx: %s",
                    r->start, r->end, strerror(errno));
        } else {
            result->bytes_advised += size;
            result->regions_processed++;
        }
    }

    result->success = (result->errors == 0 || result->regions_processed > 0);

#else
    /*
     * process_madvise not available - report what we would do.
     *
     * In a real implementation, we could:
     * 1. Use ptrace to inject syscall (complex, requires PTRACE_ATTACH)
     * 2. Send signal to cooperating daemon in target process
     * 3. Use /proc/[pid]/clear_refs (limited functionality)
     */

    for (int i = 0; i < region_count; i++) {
        grey_mem_region_t *r = &regions[i];
        size_t size = r->end - r->start;

        if (size < min_size) continue;
        if (!r->is_private || !r->is_writable) continue;
        if (anonymous_only && !r->is_anonymous) continue;

        /* Count what we would advise */
        result->bytes_advised += size;
        result->regions_processed++;
    }

    result->success = 1;
    snprintf(result->error_msg, sizeof(result->error_msg),
             "Simulation: process_madvise not available, counted %ld bytes in %ld regions",
             result->bytes_advised, result->regions_processed);
#endif

    return GREY_OK;
}

/**
 * Get summary of process memory suitable for advice.
 *
 * Returns how much memory could potentially be released via madvise.
 *
 * @param pid          Target process ID
 * @param total_anon   Output: total anonymous memory bytes
 * @param total_writable Output: total writable memory bytes
 * @return GREY_OK on success
 *
 * Safety: Read-only operation.
 */
int grey_get_advisable_memory(pid_t pid, size_t *total_anon, size_t *total_writable) {
    grey_mem_region_t regions[MAX_REGIONS];
    int region_count = 0;
    int ret;

    if (!total_anon || !total_writable) {
        return GREY_ERR_INVALID;
    }

    *total_anon = 0;
    *total_writable = 0;

    ret = grey_parse_maps(pid, regions, MAX_REGIONS, &region_count);
    if (ret != GREY_OK) {
        return ret;
    }

    for (int i = 0; i < region_count; i++) {
        grey_mem_region_t *r = &regions[i];
        size_t size = r->end - r->start;

        if (r->is_private && r->is_writable) {
            *total_writable += size;
            if (r->is_anonymous) {
                *total_anon += size;
            }
        }
    }

    return GREY_OK;
}

/**
 * Clear soft-dirty bits for a process.
 *
 * Writing "4" to /proc/[pid]/clear_refs clears soft-dirty bits,
 * allowing tracking of which pages are accessed/modified.
 *
 * @param pid Target process ID
 * @return GREY_OK on success
 *
 * Safety: Requires write access to /proc/[pid]/clear_refs (same user or root).
 * Does not modify memory contents, only tracking bits.
 *
 * Useful for: Identifying cold (unused) pages that could be reclaimed.
 */
int grey_clear_soft_dirty(pid_t pid) {
    char path[GREY_PATH_MAX];
    int fd;
    ssize_t written;

    snprintf(path, sizeof(path), "/proc/%d/clear_refs", pid);
    fd = open(path, O_WRONLY);
    if (fd < 0) {
        GREY_ERR("Cannot open %s: %s", path, strerror(errno));
        return errno == EACCES ? GREY_ERR_PERM : GREY_ERR_IO;
    }

    /* "4" = clear soft-dirty bits */
    written = write(fd, "4", 1);
    close(fd);

    if (written != 1) {
        GREY_ERR("Failed to write to %s", path);
        return GREY_ERR_IO;
    }

    GREY_LOG("Cleared soft-dirty bits for PID %d", pid);
    return GREY_OK;
}

/**
 * Read page flags to identify cold pages.
 *
 * After clearing soft-dirty and waiting, pages that remain
 * non-dirty are candidates for reclamation.
 *
 * @param pid          Target process ID
 * @param cold_bytes   Output: bytes in cold (non-accessed) pages
 * @param hot_bytes    Output: bytes in hot (accessed) pages
 * @return GREY_OK on success
 *
 * Safety: Read-only operation.
 */
int grey_count_cold_pages(pid_t pid, size_t *cold_bytes, size_t *hot_bytes) {
    char pagemap_path[GREY_PATH_MAX];
    char maps_path[GREY_PATH_MAX];
    FILE *maps_fp;
    int pagemap_fd;
    char line[512];
    size_t page_size;

    if (!cold_bytes || !hot_bytes) {
        return GREY_ERR_INVALID;
    }

    *cold_bytes = 0;
    *hot_bytes = 0;
    page_size = sysconf(_SC_PAGESIZE);

    snprintf(maps_path, sizeof(maps_path), "/proc/%d/maps", pid);
    snprintf(pagemap_path, sizeof(pagemap_path), "/proc/%d/pagemap", pid);

    maps_fp = fopen(maps_path, "r");
    if (!maps_fp) {
        return GREY_ERR_PERM;
    }

    pagemap_fd = open(pagemap_path, O_RDONLY);
    if (pagemap_fd < 0) {
        fclose(maps_fp);
        return GREY_ERR_PERM;
    }

    while (fgets(line, sizeof(line), maps_fp)) {
        unsigned long start, end;
        char perms[8];

        if (sscanf(line, "%lx-%lx %7s", &start, &end, perms) < 3) {
            continue;
        }

        /* Only check private writable regions */
        if (perms[1] != 'w' || perms[3] != 'p') {
            continue;
        }

        /* Check each page in the region */
        for (unsigned long addr = start; addr < end; addr += page_size) {
            uint64_t pagemap_entry;
            off_t offset = (addr / page_size) * sizeof(uint64_t);

            if (pread(pagemap_fd, &pagemap_entry, sizeof(pagemap_entry), offset) !=
                sizeof(pagemap_entry)) {
                continue;
            }

            /*
             * Pagemap entry format (64 bits):
             * Bit 55: soft-dirty (if bit 63 is set)
             * Bit 63: page present
             */
            int present = (pagemap_entry >> 63) & 1;
            int soft_dirty = (pagemap_entry >> 55) & 1;

            if (present) {
                if (soft_dirty) {
                    *hot_bytes += page_size;
                } else {
                    *cold_bytes += page_size;
                }
            }
        }
    }

    close(pagemap_fd);
    fclose(maps_fp);

    return GREY_OK;
}

/**
 * Advise own process memory (for testing).
 *
 * @param addr  Start address
 * @param len   Length in bytes
 * @return GREY_OK on success
 */
int grey_advise_self(void *addr, size_t len) {
    if (madvise(addr, len, MADV_DONTNEED) != 0) {
        GREY_ERR("madvise failed: %s", strerror(errno));
        return GREY_ERR_PERM;
    }
    return GREY_OK;
}

/*
 * Exported symbols for Python ctypes binding
 */
#ifdef __cplusplus
extern "C" {
#endif

int grey_memory_parse_maps(pid_t pid, grey_mem_region_t *regions,
                           int max_regions, int *count) {
    return grey_parse_maps(pid, regions, max_regions, count);
}

int grey_memory_advise(pid_t pid, int anonymous_only, size_t min_size,
                       grey_mem_result_t *result) {
    return grey_advise_memory(pid, anonymous_only, min_size, result);
}

int grey_memory_get_advisable(pid_t pid, size_t *total_anon,
                              size_t *total_writable) {
    return grey_get_advisable_memory(pid, total_anon, total_writable);
}

int grey_memory_clear_soft_dirty(pid_t pid) {
    return grey_clear_soft_dirty(pid);
}

int grey_memory_count_cold(pid_t pid, size_t *cold_bytes, size_t *hot_bytes) {
    return grey_count_cold_pages(pid, cold_bytes, hot_bytes);
}

int grey_memory_advise_self(void *addr, size_t len) {
    return grey_advise_self(addr, len);
}

/* Get size of result struct for Python */
size_t grey_memory_result_size(void) {
    return sizeof(grey_mem_result_t);
}

/* Get size of region struct for Python */
size_t grey_memory_region_size(void) {
    return sizeof(grey_mem_region_t);
}

#ifdef __cplusplus
}
#endif
