/*
 * Grey Optimizer - KSM Controller
 *
 * Controls Kernel Same-page Merging (KSM) for memory deduplication.
 * KSM scans memory for identical pages and merges them, reducing
 * memory usage for processes with similar data patterns.
 *
 * Safety:
 * - Stores original KSM settings for rollback
 * - Validates all values before applying
 * - Works with or without KSM support
 *
 * Principle: "Only use the bits needed, drop the bloat."
 */

#include "common.h"

/* KSM sysfs path */
#define KSM_PATH "/sys/kernel/mm/ksm"

/* KSM configuration limits */
#define KSM_MIN_SLEEP_MS     20       /* Minimum 20ms between scans */
#define KSM_MAX_SLEEP_MS     10000    /* Maximum 10s between scans */
#define KSM_MIN_PAGES        10       /* Minimum pages to scan */
#define KSM_MAX_PAGES        10000    /* Maximum pages to scan */

/* Stored original state for rollback */
typedef struct {
    int run;
    int sleep_millisecs;
    int pages_to_scan;
    int valid;
} ksm_state_t;

static ksm_state_t g_original_state = { .valid = 0 };

/**
 * Check if KSM is available on this system.
 *
 * @return 1 if available, 0 otherwise
 */
int grey_ksm_available(void) {
    return grey_path_exists(KSM_PATH "/run");
}

/**
 * Read a KSM sysfs value.
 *
 * @param name  Name of the KSM file (e.g., "run", "sleep_millisecs")
 * @param value Pointer to store the value
 * @return GREY_OK on success, error code on failure
 */
static int ksm_read(const char *name, int *value) {
    char path[GREY_PATH_MAX];
    char buf[GREY_SMALL_BUF];
    
    snprintf(path, sizeof(path), "%s/%s", KSM_PATH, name);
    
    int ret = grey_read_file(path, buf, sizeof(buf));
    if (ret != GREY_OK) {
        return ret;
    }
    
    *value = atoi(buf);
    return GREY_OK;
}

/**
 * Write a KSM sysfs value.
 *
 * @param name  Name of the KSM file
 * @param value Value to write
 * @return GREY_OK on success, error code on failure
 */
static int ksm_write(const char *name, int value) {
    char path[GREY_PATH_MAX];
    char buf[GREY_SMALL_BUF];
    
    snprintf(path, sizeof(path), "%s/%s", KSM_PATH, name);
    snprintf(buf, sizeof(buf), "%d", value);
    
    return grey_write_file(path, buf);
}

/**
 * Get current KSM status.
 *
 * @param enabled Pointer to store enabled status (1=on, 0=off)
 * @return GREY_OK on success, error code on failure
 */
int grey_ksm_is_enabled(int *enabled) {
    if (!enabled) {
        return GREY_ERR_INVALID;
    }
    
    if (!grey_ksm_available()) {
        *enabled = 0;
        return GREY_OK;
    }
    
    int run;
    int ret = ksm_read("run", &run);
    if (ret != GREY_OK) {
        return ret;
    }
    
    *enabled = (run == 1);
    return GREY_OK;
}

/**
 * Get KSM statistics.
 *
 * @param pages_shared  Number of shared pages
 * @param pages_sharing Number of sites sharing pages
 * @param savings_kb    Estimated memory savings in KB
 * @return GREY_OK on success, error code on failure
 */
int grey_ksm_get_stats(long *pages_shared, long *pages_sharing, long *savings_kb) {
    if (!grey_ksm_available()) {
        if (pages_shared) *pages_shared = 0;
        if (pages_sharing) *pages_sharing = 0;
        if (savings_kb) *savings_kb = 0;
        return GREY_OK;
    }
    
    char buf[GREY_SMALL_BUF];
    
    if (pages_shared) {
        if (grey_read_file(KSM_PATH "/pages_shared", buf, sizeof(buf)) == GREY_OK) {
            *pages_shared = atol(buf);
        } else {
            *pages_shared = 0;
        }
    }
    
    if (pages_sharing) {
        if (grey_read_file(KSM_PATH "/pages_sharing", buf, sizeof(buf)) == GREY_OK) {
            *pages_sharing = atol(buf);
        } else {
            *pages_sharing = 0;
        }
    }
    
    if (savings_kb) {
        /* Savings = (pages_sharing - pages_shared) * page_size */
        long shared = 0, sharing = 0;
        grey_read_file(KSM_PATH "/pages_shared", buf, sizeof(buf));
        shared = atol(buf);
        grey_read_file(KSM_PATH "/pages_sharing", buf, sizeof(buf));
        sharing = atol(buf);
        
        /* Page size is typically 4KB */
        *savings_kb = (sharing - shared) * 4;
        if (*savings_kb < 0) *savings_kb = 0;
    }
    
    return GREY_OK;
}

/**
 * Enable and configure KSM.
 *
 * @param sleep_ms      Milliseconds between scans (20-10000)
 * @param pages_to_scan Pages to scan per pass (10-10000)
 * @return GREY_OK on success, error code on failure
 */
int grey_ksm_enable(int sleep_ms, int pages_to_scan) {
    if (!grey_ksm_available()) {
        GREY_ERR("KSM not available on this system");
        return GREY_ERR_NOTFOUND;
    }
    
    if (!grey_is_root()) {
        GREY_ERR("Root required to configure KSM");
        return GREY_ERR_PERM;
    }
    
    /* Clamp values to safe ranges */
    if (sleep_ms < KSM_MIN_SLEEP_MS) sleep_ms = KSM_MIN_SLEEP_MS;
    if (sleep_ms > KSM_MAX_SLEEP_MS) sleep_ms = KSM_MAX_SLEEP_MS;
    if (pages_to_scan < KSM_MIN_PAGES) pages_to_scan = KSM_MIN_PAGES;
    if (pages_to_scan > KSM_MAX_PAGES) pages_to_scan = KSM_MAX_PAGES;
    
    /* Save original state for rollback */
    if (!g_original_state.valid) {
        ksm_read("run", &g_original_state.run);
        ksm_read("sleep_millisecs", &g_original_state.sleep_millisecs);
        ksm_read("pages_to_scan", &g_original_state.pages_to_scan);
        g_original_state.valid = 1;
        GREY_LOG("Saved original KSM state");
    }
    
    /* Configure KSM */
    int ret;
    
    ret = ksm_write("sleep_millisecs", sleep_ms);
    if (ret != GREY_OK) {
        GREY_ERR("Failed to set KSM sleep_millisecs");
        return ret;
    }
    
    ret = ksm_write("pages_to_scan", pages_to_scan);
    if (ret != GREY_OK) {
        GREY_ERR("Failed to set KSM pages_to_scan");
        return ret;
    }
    
    /* Enable KSM */
    ret = ksm_write("run", 1);
    if (ret != GREY_OK) {
        GREY_ERR("Failed to enable KSM");
        return ret;
    }
    
    GREY_LOG("Enabled KSM (sleep=%dms, pages=%d)", sleep_ms, pages_to_scan);
    return GREY_OK;
}

/**
 * Disable KSM.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_ksm_disable(void) {
    if (!grey_ksm_available()) {
        return GREY_OK;
    }
    
    if (!grey_is_root()) {
        GREY_ERR("Root required to disable KSM");
        return GREY_ERR_PERM;
    }
    
    int ret = ksm_write("run", 0);
    if (ret != GREY_OK) {
        GREY_ERR("Failed to disable KSM");
        return ret;
    }
    
    GREY_LOG("Disabled KSM");
    return GREY_OK;
}

/**
 * Restore original KSM settings.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_ksm_restore(void) {
    if (!g_original_state.valid) {
        return GREY_OK;  /* Nothing to restore */
    }
    
    if (!grey_is_root()) {
        GREY_ERR("Root required to restore KSM");
        return GREY_ERR_PERM;
    }
    
    /* Restore in reverse order */
    ksm_write("run", g_original_state.run);
    ksm_write("sleep_millisecs", g_original_state.sleep_millisecs);
    ksm_write("pages_to_scan", g_original_state.pages_to_scan);
    
    g_original_state.valid = 0;
    GREY_LOG("Restored original KSM settings");
    
    return GREY_OK;
}

/**
 * Tune KSM for aggressive memory savings.
 *
 * Configures KSM for maximum deduplication at the cost of
 * some CPU overhead.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_ksm_tune_aggressive(void) {
    /* Aggressive: scan more pages, faster */
    return grey_ksm_enable(50, 1000);
}

/**
 * Tune KSM for balanced operation.
 *
 * Good balance between memory savings and CPU usage.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_ksm_tune_balanced(void) {
    return grey_ksm_enable(200, 100);
}

/**
 * Tune KSM for minimal CPU impact.
 *
 * Slow scanning to minimize CPU overhead while still
 * achieving some memory savings over time.
 *
 * @return GREY_OK on success, error code on failure
 */
int grey_ksm_tune_conservative(void) {
    return grey_ksm_enable(1000, 50);
}
