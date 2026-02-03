/**
 * @file fault_tolerant_fsm.h
 * @brief Fault-Tolerant State Machine for Aviation Systems
 *
 * INDUSTRY RELEVANCE:
 * Safety-critical avionics require robust state machines with:
 * - Formal verification support (state coverage, transition coverage)
 * - Watchdog-protected state transitions
 * - Recovery actions for all failure modes
 * - Deterministic timing guarantees
 * - Audit trail for certification evidence
 *
 * Used in: Flight control modes, engine FADEC, autopilot logic
 *
 * @note This is a stub demonstrating fault-tolerant FSM patterns.
 */

#ifndef GF_FAULT_TOLERANT_FSM_H
#define GF_FAULT_TOLERANT_FSM_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* FSM Configuration Limits                                                  */
/*===========================================================================*/

#define GF_FSM_MAX_STATES       16
#define GF_FSM_MAX_EVENTS       32
#define GF_FSM_MAX_TRANSITIONS  64
#define GF_FSM_MAX_HISTORY      32

/*===========================================================================*/
/* FSM Type Definitions                                                      */
/*===========================================================================*/

/**
 * @brief State identifier
 */
typedef uint8_t gf_state_id_t;

/**
 * @brief Event identifier
 */
typedef uint8_t gf_event_id_t;

/**
 * @brief Transition guard function
 * @return true if transition allowed
 */
typedef bool (*gf_guard_fn)(void* context);

/**
 * @brief Action function (entry/exit/transition)
 */
typedef void (*gf_action_fn)(void* context);

/**
 * @brief State health check function
 * @return 0 if healthy, negative on error
 */
typedef int (*gf_health_fn)(void* context);

/**
 * @brief State definition
 */
typedef struct {
    gf_state_id_t id;               /**< State identifier */
    const char* name;               /**< State name (for debug) */
    
    /* Actions */
    gf_action_fn on_entry;          /**< Entry action */
    gf_action_fn on_exit;           /**< Exit action */
    gf_action_fn do_activity;       /**< In-state activity */
    
    /* Health monitoring */
    gf_health_fn health_check;      /**< State health check */
    uint32_t max_dwell_ms;          /**< Maximum time in state (0=unlimited) */
    uint32_t min_dwell_ms;          /**< Minimum time before exit allowed */
    
    /* Fault handling */
    gf_state_id_t fault_state;      /**< State to enter on fault */
    bool is_safe_state;             /**< Is this a safe state? */
    bool is_terminal;               /**< Is this a terminal state? */
    
    /* Internal */
    uint8_t asil_level;             /**< Required ASIL level */
} gf_fsm_state_t;

/**
 * @brief Transition definition
 */
typedef struct {
    gf_state_id_t from_state;       /**< Source state */
    gf_state_id_t to_state;         /**< Target state */
    gf_event_id_t event;            /**< Triggering event */
    
    gf_guard_fn guard;              /**< Guard condition */
    gf_action_fn action;            /**< Transition action */
    
    uint8_t priority;               /**< Priority if multiple match */
    bool log_transition;            /**< Log this transition */
} gf_fsm_transition_t;

/**
 * @brief Transition history entry
 */
typedef struct {
    gf_state_id_t from_state;
    gf_state_id_t to_state;
    gf_event_id_t event;
    uint64_t timestamp_us;
    bool fault_transition;          /**< Was this a fault transition? */
} gf_fsm_history_t;

/**
 * @brief FSM configuration
 */
typedef struct {
    const char* name;               /**< FSM name */
    gf_state_id_t initial_state;    /**< Initial state */
    gf_state_id_t safe_state;       /**< Global safe state */
    
    const gf_fsm_state_t* states;   /**< State definitions */
    uint8_t state_count;
    
    const gf_fsm_transition_t* transitions;  /**< Transition table */
    uint8_t transition_count;
    
    void* context;                  /**< User context */
    
    /* Timing */
    uint32_t watchdog_ms;           /**< State watchdog timeout */
    uint32_t health_check_ms;       /**< Health check interval */
    
    /* Callbacks */
    void (*on_fault)(gf_state_id_t state, int fault_code, void* ctx);
    void (*on_transition)(gf_state_id_t from, gf_state_id_t to, void* ctx);
} gf_fsm_config_t;

/**
 * @brief FSM runtime status
 */
typedef struct {
    gf_state_id_t current_state;
    gf_state_id_t previous_state;
    uint64_t state_entry_time;
    uint32_t state_dwell_ms;
    
    uint32_t transition_count;
    uint32_t fault_count;
    uint32_t guard_failures;
    
    bool in_safe_state;
    bool watchdog_expired;
    int last_fault_code;
    
    gf_fsm_history_t history[GF_FSM_MAX_HISTORY];
    uint8_t history_index;
} gf_fsm_status_t;

/**
 * @brief FSM handle
 */
typedef struct gf_fault_tolerant_fsm* gf_fsm_t;

/*===========================================================================*/
/* Fault-Tolerant FSM API                                                    */
/*===========================================================================*/

/**
 * @brief Initialize fault-tolerant FSM
 * @param config FSM configuration
 * @param fsm Output handle
 * @return 0 on success
 */
int gf_fsm_init(const gf_fsm_config_t* config, gf_fsm_t* fsm);

/**
 * @brief Start FSM (enter initial state)
 * @param fsm FSM handle
 * @return 0 on success
 */
int gf_fsm_start(gf_fsm_t fsm);

/**
 * @brief Process event
 * @param fsm FSM handle
 * @param event Event identifier
 * @return 0 on transition taken, 1 if no transition, negative on error
 */
int gf_fsm_process_event(gf_fsm_t fsm, gf_event_id_t event);

/**
 * @brief Run periodic FSM tasks (activity, health checks)
 * @param fsm FSM handle
 * @param current_time_ms Current system time
 * @return 0 on success
 */
int gf_fsm_run(gf_fsm_t fsm, uint32_t current_time_ms);

/**
 * @brief Get current state
 * @param fsm FSM handle
 * @return Current state ID
 */
gf_state_id_t gf_fsm_get_state(gf_fsm_t fsm);

/**
 * @brief Check if in specific state
 * @param fsm FSM handle
 * @param state State to check
 * @return true if in state
 */
bool gf_fsm_is_in_state(gf_fsm_t fsm, gf_state_id_t state);

/**
 * @brief Force transition to safe state
 * @param fsm FSM handle
 * @param fault_code Fault code for logging
 * @return 0 on success
 */
int gf_fsm_enter_safe_state(gf_fsm_t fsm, int fault_code);

/**
 * @brief Reset FSM to initial state
 * @param fsm FSM handle
 * @return 0 on success
 */
int gf_fsm_reset(gf_fsm_t fsm);

/**
 * @brief Get FSM status
 * @param fsm FSM handle
 * @param status Output status
 * @return 0 on success
 */
int gf_fsm_get_status(gf_fsm_t fsm, gf_fsm_status_t* status);

/**
 * @brief Check if transition is valid
 * @param fsm FSM handle
 * @param event Event to check
 * @return true if transition exists and guard passes
 */
bool gf_fsm_can_transition(gf_fsm_t fsm, gf_event_id_t event);

/**
 * @brief Get transition history
 * @param fsm FSM handle
 * @param history Output buffer
 * @param max_entries Buffer capacity
 * @param out_count Number of entries returned
 * @return 0 on success
 */
int gf_fsm_get_history(gf_fsm_t fsm,
                        gf_fsm_history_t* history,
                        uint8_t max_entries,
                        uint8_t* out_count);

/**
 * @brief Deinitialize FSM
 * @param fsm FSM handle
 */
void gf_fsm_deinit(gf_fsm_t fsm);

/*===========================================================================*/
/* FSM Verification Helpers                                                  */
/*===========================================================================*/

/**
 * @brief Verify FSM configuration (for certification)
 * @param config FSM configuration to verify
 * @return 0 if valid, negative on issues found
 */
int gf_fsm_verify_config(const gf_fsm_config_t* config);

/**
 * @brief Check all states reachable from initial
 * @param config FSM configuration
 * @param unreachable Output: bitmask of unreachable states
 * @return Number of unreachable states
 */
int gf_fsm_check_reachability(const gf_fsm_config_t* config,
                               uint32_t* unreachable);

/**
 * @brief Check all states can reach safe state
 * @param config FSM configuration
 * @param unsafe Output: bitmask of states that cannot reach safe
 * @return Number of unsafe states
 */
int gf_fsm_check_safe_reachability(const gf_fsm_config_t* config,
                                    uint32_t* unsafe);

#ifdef __cplusplus
}
#endif

#endif /* GF_FAULT_TOLERANT_FSM_H */
