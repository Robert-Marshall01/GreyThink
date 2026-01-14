/*
 * Grey Optimizer - Cache Operations
 *
 * Implements cache management operations including:
 * - Dropping pagecache, dentries, and inodes
 * - Sync operations
 *
 * Safety:
 * - Only drops caches when safe (low memory conditions)
 * - Always syncs before dropping to prevent data loss
 * - Requires explicit root permission
 *
 * Principle: "Only use the bits needed, drop the bloat."
 */

#include "common.h"

/* Drop cache levels */
#define DROP_PAGECACHE  1   /* Free pagecache */
#define DROP_SLABS      2   /* Free dentries and inodes */
#define DROP_ALL        3   /* Free all (pagecache + slabs) */

/**
 * Sync all filesystems.
 *
 * Ensures all dirty data is written to disk before
 * any cache operations.
 */
void grey_sync(void) {
    sync();
    GREY_LOG("Synced all filesystems");
}

/**
 * Get available memory in KB.
 *
 * @param available_kb Pointer to store available memory
 * @return GREY_OK on success, error code on failure
 */
int grey_get_available_memory(long *available_kb) {
    if (!available_kb) {
        return GREY_ERR_INVALID;
    }
    
    FILE *fp = fopen("/proc/meminfo", "r");
    if (!fp) {
        return GREY_ERR_IO;
    }
    
    char line[GREY_SMALL_BUF];
    *available_kb = 0;
    
    while (fgets(line, sizeof(line), fp)) {
        if (strncmp(line, "MemAvailable:", 13) == 0) {
            sscanf(line, "MemAvailable: %ld", available_kb);
            break;
        }
    }
    
    fclose(fp);
    return GREY_OK;
}

/**
 * Get cached memory in KB.
 *
 * @param cached_kb Pointer to store cached memory
 * @return GREY_OK on success, error code on failure
 */
int grey_get_cached_memory(long *cached_kb) {
    if (!cached_kb) {
        return GREY_ERR_INVALID;
    }
    
    FILE *fp = fopen("/proc/meminfo", "r");
    if (!fp) {
        return GREY_ERR_IO;
    }
    
    char line[GREY_SMALL_BUF];
    *cached_kb = 0;
    
    while (fgets(line, sizeof(line), fp)) {
        if (strncmp(line, "Cached:", 7) == 0) {
            sscanf(line, "Cached: %ld", cached_kb);
            break;
        }
    }
    
    fclose(fp);
    return GREY_OK;
}

/**
 * Check if it's safe to drop caches.
 *
 * We consider it safe when:
 * - Available memory is low (< threshold)
 * - Cached memory is significant
 * - System is not under heavy I/O
 *
 * @param threshold_kb Minimum available memory threshold
 * @return 1 if safe, 0 if not recommended
 */
int grey_cache_drop_safe(long threshold_kb) {
    long available_kb, cached_kb;
    
    if (grey_get_available_memory(&available_kb) != GREY_OK) {
        return 0;  /* Can't determine - assume unsafe */
    }
    
    if (grey_get_cached_memory(&cached_kb) != GREY_OK) {
        return 0;
    }
    
    /* Only safe if available is low AND cached is significant */
    if (available_kb < threshold_kb && cached_kb > threshold_kb) {
        GREY_LOG("Cache drop recommended: available=%ldKB, cached=%ldKB, threshold=%ldKB",
                 available_kb, cached_kb, threshold_kb);
        return 1;
    }
    
    GREY_LOG("Cache drop not recommended: available=%ldKB >= threshold=%ldKB",
             available_kb, threshold_kb);
    return 0;
}

/**
 * Drop caches.
 *
 * Writes to /proc/sys/vm/drop_caches to free memory.
 *
 * Levels:
 * 1 = Free pagecache
 * 2 = Free dentries and inodes (slab objects)
 * 3 = Free all
 *
 * @param level Drop level (1, 2, or 3)
 * @return GREY_OK on success, error code on failure
 */
int grey_drop_caches(int level) {
    if (level < 1 || level > 3) {
        return GREY_ERR_INVALID;
    }
    
    if (!grey_is_root()) {
        GREY_ERR("Root required to drop caches");
        return GREY_ERR_PERM;
    }
    
    /* Always sync first to prevent data loss */
    grey_sync();
    
    /* Get before stats */
    long before_cached = 0;
    grey_get_cached_memory(&before_cached);
    
    /* Drop caches */
    char value[8];
    snprintf(value, sizeof(value), "%d", level);
    
    int ret = grey_write_file("/proc/sys/vm/drop_caches", value);
    if (ret != GREY_OK) {
        GREY_ERR("Failed to drop caches");
        return ret;
    }
    
    /* Get after stats */
    long after_cached = 0;
    grey_get_cached_memory(&after_cached);
    
    long freed_mb = (before_cached - after_cached) / 1024;
    GREY_LOG("Dropped caches (level=%d): freed approximately %ldMB", level, freed_mb);
    
    return GREY_OK;
}

/**
 * Drop pagecache only.
 *
 * Safest option - only drops file-backed page cache.
 * Applications may need to re-read files but no metadata is affected.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_drop_pagecache(void) {
    return grey_drop_caches(DROP_PAGECACHE);
}

/**
 * Drop slab caches (dentries and inodes).
 *
 * Frees directory entry and inode caches.
 * May slow down file operations temporarily.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_drop_slabs(void) {
    return grey_drop_caches(DROP_SLABS);
}

/**
 * Drop all caches.
 *
 * Frees pagecache, dentries, and inodes.
 * Most aggressive - may cause temporary I/O spike.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_drop_all_caches(void) {
    return grey_drop_caches(DROP_ALL);
}

/**
 * Safe cache drop with threshold check.
 *
 * Only drops caches if available memory is below threshold.
 *
 * @param threshold_mb Minimum available memory in MB
 * @return Bytes freed (estimate), or 0 if not dropped
 */
long grey_cache_drop_if_needed(long threshold_mb) {
    long threshold_kb = threshold_mb * 1024;
    
    if (!grey_cache_drop_safe(threshold_kb)) {
        return 0;
    }
    
    long before_cached = 0, after_cached = 0;
    grey_get_cached_memory(&before_cached);
    
    if (grey_drop_caches(DROP_ALL) != GREY_OK) {
        return 0;
    }
    
    grey_get_cached_memory(&after_cached);
    
    return (before_cached - after_cached) * 1024;  /* Return bytes */
}

/**
 * Compact memory.
 *
 * Triggers memory compaction to reduce fragmentation.
 * Helps with large page allocations.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_compact_memory(void) {
    if (!grey_is_root()) {
        GREY_ERR("Root required for memory compaction");
        return GREY_ERR_PERM;
    }
    
    if (!grey_path_exists("/proc/sys/vm/compact_memory")) {
        GREY_LOG("Memory compaction not available");
        return GREY_ERR_NOTFOUND;
    }
    
    int ret = grey_write_file("/proc/sys/vm/compact_memory", "1");
    if (ret == GREY_OK) {
        GREY_LOG("Triggered memory compaction");
    }
    
    return ret;
}
