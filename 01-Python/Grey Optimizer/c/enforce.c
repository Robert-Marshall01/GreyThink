/**
 * ═══════════════════════════════════════════════════════════════════════════════
 * Grey Optimizer - Main Enforcement Module
 * ═══════════════════════════════════════════════════════════════════════════════
 *
 * Central enforcement interface providing a unified API for platform-specific
 * hardware enforcement operations.
 *
 * ═══════════════════════════════════════════════════════════════════════════════
 */

#include "enforce.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#ifdef __linux__
#include <unistd.h>
#include <sys/types.h>
#endif

#ifdef __APPLE__
#include <unistd.h>
#include <sys/types.h>
#endif

/* ═══════════════════════════════════════════════════════════════════════════════
 * Global State
 * ═══════════════════════════════════════════════════════════════════════════════ */

static grey_enforce_state_t global_state = {
    .initialized = 0,
    .mode = GREY_MODE_SIMULATION,
    .active_profile = GREY_PROFILE_BALANCED,
    .enforcement_active = 0,
    .last_error = {0}
};

/* ═══════════════════════════════════════════════════════════════════════════════
 * Initialization
 * ═══════════════════════════════════════════════════════════════════════════════ */

int grey_enforce_init(grey_enforce_mode_t mode) {
    if (global_state.initialized) {
        grey_set_error("Already initialized");
        return GREY_ERROR_ALREADY_INITIALIZED;
    }

    /* Check for root/admin privileges in live mode */
    if (mode == GREY_MODE_LIVE) {
#if defined(__linux__) || defined(__APPLE__)
        if (geteuid() != 0) {
            grey_set_error("Live mode requires root privileges");
            return GREY_ERROR_PERMISSION_DENIED;
        }
#endif
    }

    global_state.mode = mode;
    global_state.initialized = 1;
    global_state.enforcement_active = 0;
    
    /* Initialize subsystems */
    if (cgroup_wrapper_init() != GREY_SUCCESS) {
        global_state.initialized = 0;
        return GREY_ERROR_INIT_FAILED;
    }
    
    if (sysctl_wrapper_init() != GREY_SUCCESS) {
        global_state.initialized = 0;
        return GREY_ERROR_INIT_FAILED;
    }

    return GREY_SUCCESS;
}

void grey_enforce_shutdown(void) {
    if (!global_state.initialized) {
        return;
    }

    /* Cleanup subsystems */
    grey_enforce_rollback();
    cgroup_wrapper_shutdown();
    sysctl_wrapper_shutdown();

    global_state.initialized = 0;
    global_state.enforcement_active = 0;
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Mode Control
 * ═══════════════════════════════════════════════════════════════════════════════ */

grey_enforce_mode_t grey_get_mode(void) {
    return global_state.mode;
}

int grey_is_simulation(void) {
    return global_state.mode == GREY_MODE_SIMULATION;
}

int grey_is_live(void) {
    return global_state.mode == GREY_MODE_LIVE;
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Profile Application
 * ═══════════════════════════════════════════════════════════════════════════════ */

int grey_enforce_apply_profile(grey_enforce_profile_t profile) {
    if (!global_state.initialized) {
        grey_set_error("Not initialized");
        return GREY_ERROR_NOT_INITIALIZED;
    }

    grey_enforce_config_t config;
    
    /* Get profile configuration */
    switch (profile) {
        case GREY_PROFILE_CONSERVATIVE:
            config.cpu_limit_percent = 90;
            config.memory_limit_percent = 90;
            config.io_weight = 100;
            config.nice_value = 10;
            config.oom_score_adj = 0;
            break;
            
        case GREY_PROFILE_BALANCED:
            config.cpu_limit_percent = 75;
            config.memory_limit_percent = 80;
            config.io_weight = 80;
            config.nice_value = 15;
            config.oom_score_adj = 100;
            break;
            
        case GREY_PROFILE_AGGRESSIVE:
            config.cpu_limit_percent = 50;
            config.memory_limit_percent = 60;
            config.io_weight = 50;
            config.nice_value = 19;
            config.oom_score_adj = 500;
            break;
            
        default:
            grey_set_error("Unknown profile");
            return GREY_ERROR_INVALID_ARGUMENT;
    }

    return grey_enforce_apply_config(&config);
}

int grey_enforce_apply_config(const grey_enforce_config_t *config) {
    if (!global_state.initialized) {
        grey_set_error("Not initialized");
        return GREY_ERROR_NOT_INITIALIZED;
    }

    if (!config) {
        grey_set_error("NULL config");
        return GREY_ERROR_INVALID_ARGUMENT;
    }

    int result;

    /* In simulation mode, just log what would happen */
    if (global_state.mode == GREY_MODE_SIMULATION) {
        fprintf(stderr, "[SIMULATE] Would apply: CPU=%d%%, MEM=%d%%, IO=%d\n",
                config->cpu_limit_percent,
                config->memory_limit_percent,
                config->io_weight);
        global_state.enforcement_active = 1;
        return GREY_SUCCESS;
    }

    /* Live mode: actually apply enforcement */
    
    /* Apply cgroup limits */
    result = cgroup_set_cpu_limit(config->cpu_limit_percent);
    if (result != GREY_SUCCESS) {
        return result;
    }

    result = cgroup_set_memory_limit(config->memory_limit_percent);
    if (result != GREY_SUCCESS) {
        grey_enforce_rollback();
        return result;
    }

    result = cgroup_set_io_weight(config->io_weight);
    if (result != GREY_SUCCESS) {
        grey_enforce_rollback();
        return result;
    }

    /* Apply sysctl tunables */
    result = sysctl_apply_optimizations();
    if (result != GREY_SUCCESS) {
        grey_enforce_rollback();
        return result;
    }

    global_state.enforcement_active = 1;
    return GREY_SUCCESS;
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Rollback
 * ═══════════════════════════════════════════════════════════════════════════════ */

int grey_enforce_rollback(void) {
    if (!global_state.initialized) {
        grey_set_error("Not initialized");
        return GREY_ERROR_NOT_INITIALIZED;
    }

    if (!global_state.enforcement_active) {
        return GREY_SUCCESS;
    }

    if (global_state.mode == GREY_MODE_SIMULATION) {
        fprintf(stderr, "[SIMULATE] Would rollback enforcement\n");
        global_state.enforcement_active = 0;
        return GREY_SUCCESS;
    }

    int result = GREY_SUCCESS;

    /* Restore cgroups */
    if (cgroup_restore_defaults() != GREY_SUCCESS) {
        result = GREY_ERROR_ROLLBACK_FAILED;
    }

    /* Restore sysctl values */
    if (sysctl_restore_defaults() != GREY_SUCCESS) {
        result = GREY_ERROR_ROLLBACK_FAILED;
    }

    global_state.enforcement_active = 0;
    return result;
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Status
 * ═══════════════════════════════════════════════════════════════════════════════ */

int grey_enforce_get_status(grey_enforce_status_t *status) {
    if (!status) {
        grey_set_error("NULL status pointer");
        return GREY_ERROR_INVALID_ARGUMENT;
    }

    status->initialized = global_state.initialized;
    status->mode = global_state.mode;
    status->active_profile = global_state.active_profile;
    status->enforcement_active = global_state.enforcement_active;
    status->timestamp = time(NULL);

    return GREY_SUCCESS;
}

int grey_is_enforcement_active(void) {
    return global_state.enforcement_active;
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Error Handling
 * ═══════════════════════════════════════════════════════════════════════════════ */

void grey_set_error(const char *msg) {
    if (msg) {
        strncpy(global_state.last_error, msg, GREY_ERROR_MSG_SIZE - 1);
        global_state.last_error[GREY_ERROR_MSG_SIZE - 1] = '\0';
    }
}

const char* grey_get_last_error(void) {
    return global_state.last_error;
}

void grey_clear_error(void) {
    global_state.last_error[0] = '\0';
}

/* ═══════════════════════════════════════════════════════════════════════════════
 * Version Info
 * ═══════════════════════════════════════════════════════════════════════════════ */

const char* grey_enforce_version(void) {
    return GREY_ENFORCE_VERSION;
}

int grey_enforce_api_version(void) {
    return GREY_ENFORCE_API_VERSION;
}
