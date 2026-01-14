/**
 * ═══════════════════════════════════════════════════════════════════════════════
 * Grey Optimizer - Enforcement Module Header
 * ═══════════════════════════════════════════════════════════════════════════════
 *
 * Public API for the Grey Optimizer enforcement system.
 *
 * ═══════════════════════════════════════════════════════════════════════════════
 */

#ifndef GREY_ENFORCE_H
#define GREY_ENFORCE_H

#include <stdint.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ═══════════════════════════════════════════════════════════════════════════════
 * Version Information
 * ═══════════════════════════════════════════════════════════════════════════════ */

#define GREY_ENFORCE_VERSION     "1.0.0"
#define GREY_ENFORCE_API_VERSION 1

/* ═══════════════════════════════════════════════════════════════════════════════
 * Constants
 * ═══════════════════════════════════════════════════════════════════════════════ */

#define GREY_ERROR_MSG_SIZE     256
#define GREY_MAX_PATH           4096
#define GREY_CGROUP_NAME        "grey-optimizer"

/* ═══════════════════════════════════════════════════════════════════════════════
 * Error Codes
 * ═══════════════════════════════════════════════════════════════════════════════ */

typedef enum {
    GREY_SUCCESS                  = 0,
    GREY_ERROR_GENERIC           = -1,
    GREY_ERROR_NOT_INITIALIZED   = -2,
    GREY_ERROR_ALREADY_INITIALIZED = -3,
    GREY_ERROR_PERMISSION_DENIED = -4,
    GREY_ERROR_INVALID_ARGUMENT  = -5,
    GREY_ERROR_NOT_SUPPORTED     = -6,
    GREY_ERROR_INIT_FAILED       = -7,
    GREY_ERROR_APPLY_FAILED      = -8,
    GREY_ERROR_ROLLBACK_FAILED   = -9,
    GREY_ERROR_FILE_NOT_FOUND    = -10,
    GREY_ERROR_OUT_OF_MEMORY     = -11
} grey_error_t;

/* ═══════════════════════════════════════════════════════════════════════════════
 * Enumerations
 * ═══════════════════════════════════════════════════════════════════════════════ */

/**
 * Enforcement mode
 */
typedef enum {
    GREY_MODE_SIMULATION = 0,   /**< Simulation mode - no actual changes */
    GREY_MODE_LIVE       = 1    /**< Live mode - apply real enforcement */
} grey_enforce_mode_t;

/**
 * Predefined enforcement profiles
 */
typedef enum {
    GREY_PROFILE_CONSERVATIVE = 0,  /**< Light enforcement, minimal impact */
    GREY_PROFILE_BALANCED     = 1,  /**< Balanced enforcement */
    GREY_PROFILE_AGGRESSIVE   = 2   /**< Aggressive enforcement, maximum savings */
} grey_enforce_profile_t;

/* ═══════════════════════════════════════════════════════════════════════════════
 * Data Structures
 * ═══════════════════════════════════════════════════════════════════════════════ */

/**
 * Enforcement configuration
 */
typedef struct {
    int cpu_limit_percent;          /**< CPU usage limit (1-100) */
    int memory_limit_percent;       /**< Memory usage limit (1-100) */
    int io_weight;                  /**< I/O priority weight (1-1000) */
    int nice_value;                 /**< Process nice value (-20 to 19) */
    int oom_score_adj;             /**< OOM killer score adjustment (-1000 to 1000) */
} grey_enforce_config_t;

/**
 * Enforcement status
 */
typedef struct {
    int initialized;                /**< 1 if initialized, 0 otherwise */
    grey_enforce_mode_t mode;       /**< Current mode */
    grey_enforce_profile_t active_profile;  /**< Active profile */
    int enforcement_active;         /**< 1 if enforcement is active */
    time_t timestamp;              /**< Status timestamp */
} grey_enforce_status_t;

/**
 * Internal state (opaque to users)
 */
typedef struct {
    int initialized;
    grey_enforce_mode_t mode;
    grey_enforce_profile_t active_profile;
    int enforcement_active;
    char last_error[GREY_ERROR_MSG_SIZE];
} grey_enforce_state_t;

/* ═══════════════════════════════════════════════════════════════════════════════
 * Initialization
 * ═══════════════════════════════════════════════════════════════════════════════ */

/**
 * Initialize the enforcement system
 *
 * @param mode Enforcement mode (simulation or live)
 * @return GREY_SUCCESS on success, error code otherwise
 */
int grey_enforce_init(grey_enforce_mode_t mode);

/**
 * Shutdown the enforcement system
 * Automatically calls rollback if enforcement is active
 */
void grey_enforce_shutdown(void);

/* ═══════════════════════════════════════════════════════════════════════════════
 * Mode Control
 * ═══════════════════════════════════════════════════════════════════════════════ */

/**
 * Get current enforcement mode
 *
 * @return Current mode
 */
grey_enforce_mode_t grey_get_mode(void);

/**
 * Check if running in simulation mode
 *
 * @return 1 if simulation, 0 otherwise
 */
int grey_is_simulation(void);

/**
 * Check if running in live mode
 *
 * @return 1 if live, 0 otherwise
 */
int grey_is_live(void);

/* ═══════════════════════════════════════════════════════════════════════════════
 * Enforcement Operations
 * ═══════════════════════════════════════════════════════════════════════════════ */

/**
 * Apply a predefined enforcement profile
 *
 * @param profile Profile to apply
 * @return GREY_SUCCESS on success, error code otherwise
 */
int grey_enforce_apply_profile(grey_enforce_profile_t profile);

/**
 * Apply custom enforcement configuration
 *
 * @param config Configuration to apply
 * @return GREY_SUCCESS on success, error code otherwise
 */
int grey_enforce_apply_config(const grey_enforce_config_t *config);

/**
 * Rollback all enforcement to defaults
 *
 * @return GREY_SUCCESS on success, error code otherwise
 */
int grey_enforce_rollback(void);

/* ═══════════════════════════════════════════════════════════════════════════════
 * Status
 * ═══════════════════════════════════════════════════════════════════════════════ */

/**
 * Get current enforcement status
 *
 * @param status Pointer to status structure to fill
 * @return GREY_SUCCESS on success, error code otherwise
 */
int grey_enforce_get_status(grey_enforce_status_t *status);

/**
 * Check if enforcement is currently active
 *
 * @return 1 if active, 0 otherwise
 */
int grey_is_enforcement_active(void);

/* ═══════════════════════════════════════════════════════════════════════════════
 * Error Handling
 * ═══════════════════════════════════════════════════════════════════════════════ */

/**
 * Set error message
 *
 * @param msg Error message
 */
void grey_set_error(const char *msg);

/**
 * Get last error message
 *
 * @return Error message string
 */
const char* grey_get_last_error(void);

/**
 * Clear last error
 */
void grey_clear_error(void);

/* ═══════════════════════════════════════════════════════════════════════════════
 * Version
 * ═══════════════════════════════════════════════════════════════════════════════ */

/**
 * Get library version string
 *
 * @return Version string
 */
const char* grey_enforce_version(void);

/**
 * Get API version number
 *
 * @return API version number
 */
int grey_enforce_api_version(void);

/* ═══════════════════════════════════════════════════════════════════════════════
 * Subsystem Functions (internal, but declared here for linking)
 * ═══════════════════════════════════════════════════════════════════════════════ */

/* cgroup_wrapper.c */
int cgroup_wrapper_init(void);
void cgroup_wrapper_shutdown(void);
int cgroup_set_cpu_limit(int percent);
int cgroup_set_memory_limit(int percent);
int cgroup_set_io_weight(int weight);
int cgroup_restore_defaults(void);

/* sysctl_wrapper.c */
int sysctl_wrapper_init(void);
void sysctl_wrapper_shutdown(void);
int sysctl_apply_optimizations(void);
int sysctl_restore_defaults(void);

#ifdef __cplusplus
}
#endif

#endif /* GREY_ENFORCE_H */
