/*
 * Grey Optimizer - Scheduler Controller
 *
 * Implements Linux scheduler manipulation for process prioritization.
 * Uses sched_setscheduler and setpriority to control CPU allocation.
 *
 * Safety:
 * - Stores original scheduler state for rollback
 * - Never modifies kernel threads or init
 * - Validates PIDs before modification
 *
 * Principle: "Only use the bits needed, drop the bloat."
 */

#include "common.h"
#include <sched.h>
#include <sys/resource.h>

/* Scheduler policies */
#define GREY_SCHED_OTHER    0   /* Default time-sharing */
#define GREY_SCHED_FIFO     1   /* Real-time FIFO */
#define GREY_SCHED_RR       2   /* Real-time round-robin */
#define GREY_SCHED_BATCH    3   /* Batch processing */
#define GREY_SCHED_IDLE     5   /* Lowest priority */
#define GREY_SCHED_DEADLINE 6   /* Deadline scheduler */

/* Maximum processes we track for rollback */
#define MAX_TRACKED_PROCS 1024

/* Rollback state */
typedef struct {
    pid_t pid;
    int original_policy;
    int original_priority;
} proc_state_t;

static proc_state_t g_rollback_state[MAX_TRACKED_PROCS];
static int g_rollback_count = 0;

/**
 * Check if a process is protected from modification.
 *
 * Protected processes include:
 * - PID 1 (init/systemd)
 * - Kernel threads (PID < 300 or [kthreadd] children)
 * - Grey Optimizer itself
 *
 * @param pid Process ID to check
 * @return 1 if protected, 0 if can be modified
 */
int grey_is_protected(pid_t pid) {
    /* Never touch init or kernel threads */
    if (pid <= 1) {
        return 1;
    }
    
    /* Check process name for protection patterns */
    char comm_path[GREY_PATH_MAX];
    char comm[GREY_SMALL_BUF];
    
    snprintf(comm_path, sizeof(comm_path), "/proc/%d/comm", pid);
    if (grey_read_file(comm_path, comm, sizeof(comm)) != GREY_OK) {
        return 1;  /* Can't read = assume protected */
    }
    
    /* Protected process names */
    const char *protected[] = {
        "systemd",
        "init",
        "sshd",
        "grey-optim",  /* Our own processes */
        "kthreadd",
        "ksoftirqd",
        "kworker",
        "migration",
        "watchdog",
        NULL
    };
    
    for (const char **p = protected; *p != NULL; p++) {
        if (strstr(comm, *p) != NULL) {
            return 1;
        }
    }
    
    return 0;
}

/**
 * Get current scheduler policy for a process.
 *
 * @param pid    Process ID
 * @param policy Pointer to store current policy
 * @return GREY_OK on success, error code on failure
 */
int grey_sched_get_policy(pid_t pid, int *policy) {
    if (pid <= 0 || !policy) {
        return GREY_ERR_INVALID;
    }
    
    int p = sched_getscheduler(pid);
    if (p < 0) {
        GREY_ERR("sched_getscheduler(%d) failed: %s", pid, strerror(errno));
        return errno == ESRCH ? GREY_ERR_NOTFOUND : GREY_ERR_PERM;
    }
    
    *policy = p;
    return GREY_OK;
}

/**
 * Set scheduler policy for a process.
 *
 * @param pid    Process ID
 * @param policy Scheduler policy (SCHED_OTHER, SCHED_IDLE, etc.)
 * @return GREY_OK on success, error code on failure
 */
int grey_sched_set_policy(pid_t pid, int policy) {
    if (pid <= 0) {
        return GREY_ERR_INVALID;
    }
    
    if (grey_is_protected(pid)) {
        GREY_LOG("Refusing to modify protected process %d", pid);
        return GREY_ERR_PERM;
    }
    
    if (!grey_is_root() && policy != SCHED_OTHER) {
        GREY_ERR("Root required to set scheduler policy");
        return GREY_ERR_PERM;
    }
    
    /* Save current state for rollback */
    int current_policy;
    if (grey_sched_get_policy(pid, &current_policy) == GREY_OK) {
        if (g_rollback_count < MAX_TRACKED_PROCS) {
            g_rollback_state[g_rollback_count].pid = pid;
            g_rollback_state[g_rollback_count].original_policy = current_policy;
            g_rollback_state[g_rollback_count].original_priority = 0;
            g_rollback_count++;
        }
    }
    
    /* Set new policy */
    struct sched_param param = { .sched_priority = 0 };
    
    if (sched_setscheduler(pid, policy, &param) != 0) {
        GREY_ERR("sched_setscheduler(%d, %d) failed: %s", 
                 pid, policy, strerror(errno));
        return errno == EPERM ? GREY_ERR_PERM : GREY_ERR_IO;
    }
    
    GREY_LOG("Set scheduler policy %d for PID %d", policy, pid);
    return GREY_OK;
}

/**
 * Set SCHED_IDLE policy for a process.
 *
 * SCHED_IDLE is the lowest priority - the process only runs
 * when no other processes want CPU time. Perfect for non-critical
 * background tasks.
 *
 * @param pid Process ID
 * @return GREY_OK on success, error code on failure
 */
int grey_sched_set_idle(pid_t pid) {
    return grey_sched_set_policy(pid, SCHED_IDLE);
}

/**
 * Set SCHED_BATCH policy for a process.
 *
 * SCHED_BATCH is for CPU-intensive batch processing.
 * Similar to SCHED_OTHER but with a slight penalty to wake-up latency.
 *
 * @param pid Process ID
 * @return GREY_OK on success, error code on failure
 */
int grey_sched_set_batch(pid_t pid) {
    return grey_sched_set_policy(pid, SCHED_BATCH);
}

/**
 * Restore default scheduler (SCHED_OTHER) for a process.
 *
 * @param pid Process ID
 * @return GREY_OK on success, error code on failure
 */
int grey_sched_restore_default(pid_t pid) {
    return grey_sched_set_policy(pid, SCHED_OTHER);
}

/**
 * Get nice value for a process.
 *
 * @param pid  Process ID
 * @param nice Pointer to store nice value (-20 to 19)
 * @return GREY_OK on success, error code on failure
 */
int grey_get_nice(pid_t pid, int *nice) {
    if (pid <= 0 || !nice) {
        return GREY_ERR_INVALID;
    }
    
    errno = 0;
    int n = getpriority(PRIO_PROCESS, pid);
    if (n == -1 && errno != 0) {
        GREY_ERR("getpriority(%d) failed: %s", pid, strerror(errno));
        return errno == ESRCH ? GREY_ERR_NOTFOUND : GREY_ERR_PERM;
    }
    
    *nice = n;
    return GREY_OK;
}

/**
 * Set nice value for a process.
 *
 * Nice values range from -20 (highest priority) to 19 (lowest).
 * Only root can decrease nice (increase priority).
 *
 * @param pid  Process ID
 * @param nice Nice value (-20 to 19)
 * @return GREY_OK on success, error code on failure
 */
int grey_set_nice(pid_t pid, int nice) {
    if (pid <= 0) {
        return GREY_ERR_INVALID;
    }
    
    if (grey_is_protected(pid)) {
        GREY_LOG("Refusing to renice protected process %d", pid);
        return GREY_ERR_PERM;
    }
    
    /* Clamp to valid range */
    if (nice < -20) nice = -20;
    if (nice > 19) nice = 19;
    
    /* Save current for rollback */
    int current_nice;
    if (grey_get_nice(pid, &current_nice) == GREY_OK) {
        for (int i = 0; i < g_rollback_count; i++) {
            if (g_rollback_state[i].pid == pid) {
                g_rollback_state[i].original_priority = current_nice;
                break;
            }
        }
    }
    
    if (setpriority(PRIO_PROCESS, pid, nice) != 0) {
        GREY_ERR("setpriority(%d, %d) failed: %s", pid, nice, strerror(errno));
        return errno == EPERM ? GREY_ERR_PERM : GREY_ERR_IO;
    }
    
    GREY_LOG("Set nice %d for PID %d", nice, pid);
    return GREY_OK;
}

/**
 * Rollback all scheduler changes.
 *
 * Restores original scheduler policies for all modified processes.
 *
 * @return Number of processes restored
 */
int grey_sched_rollback_all(void) {
    int restored = 0;
    
    for (int i = 0; i < g_rollback_count; i++) {
        proc_state_t *state = &g_rollback_state[i];
        
        /* Check if process still exists */
        char proc_path[GREY_PATH_MAX];
        snprintf(proc_path, sizeof(proc_path), "/proc/%d", state->pid);
        if (!grey_path_exists(proc_path)) {
            continue;
        }
        
        /* Restore scheduler policy */
        struct sched_param param = { .sched_priority = 0 };
        if (sched_setscheduler(state->pid, state->original_policy, &param) == 0) {
            GREY_LOG("Restored scheduler for PID %d", state->pid);
            restored++;
        }
        
        /* Restore nice value if we changed it */
        if (state->original_priority != 0) {
            setpriority(PRIO_PROCESS, state->pid, state->original_priority);
        }
    }
    
    g_rollback_count = 0;
    return restored;
}

/**
 * Get count of modified processes.
 *
 * @return Number of processes with modified schedulers
 */
int grey_sched_get_modified_count(void) {
    return g_rollback_count;
}
