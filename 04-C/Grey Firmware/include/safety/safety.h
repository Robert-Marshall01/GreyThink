/**
 * @file safety.h
 * @brief Safety-Critical System Modules
 * 
 * INDUSTRY RELEVANCE:
 *   Safety-critical systems require redundancy and fail-safe operation:
 *   - Automotive (ISO 26262): ASIL A-D functional safety levels
 *   - Medical (IEC 62304): Class A-C software safety classification
 *   - Industrial (IEC 61508): SIL 1-4 safety integrity levels
 *   - Aerospace (DO-178C): DAL A-E design assurance levels
 *   - Railway (EN 50128): SIL 0-4 safety requirements
 * 
 * This module demonstrates:
 *   - Redundancy checking (dual/triple modular redundancy)
 *   - Fail-safe state machines
 *   - Watchdog escalation with safety states
 */

#ifndef GF_SAFETY_H
#define GF_SAFETY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Redundancy Checker                                                         */
/*===========================================================================*/

typedef enum {
    GF_REDUNDANCY_DUAL = 2,         /* Dual Modular Redundancy */
    GF_REDUNDANCY_TRIPLE = 3        /* Triple Modular Redundancy (voting) */
} gf_redundancy_mode_t;

typedef enum {
    GF_REDUNDANCY_OK = 0,
    GF_REDUNDANCY_MISMATCH,         /* Channels disagree */
    GF_REDUNDANCY_CHANNEL_FAULT,    /* Single channel failed */
    GF_REDUNDANCY_MAJORITY_FAULT    /* Majority failed (TMR) */
} gf_redundancy_result_t;

typedef struct {
    gf_redundancy_mode_t    mode;
    uint8_t                 channel_count;
    uint32_t                tolerance;      /* Acceptable deviation */
    bool                    auto_failover;
} gf_redundancy_config_t;

typedef struct {
    gf_redundancy_result_t  last_result;
    uint32_t                mismatch_count;
    uint32_t                failover_count;
    uint8_t                 active_channel;
    uint8_t                 failed_channels;
} gf_redundancy_status_t;

/**
 * @brief Initialize redundancy checker
 */
int gf_redundancy_init(const gf_redundancy_config_t *config);

/**
 * @brief Compare values from redundant channels (integer)
 */
gf_redundancy_result_t gf_redundancy_check_u32(const uint32_t *values, 
                                                uint8_t count,
                                                uint32_t *voted_value);

/**
 * @brief Compare values from redundant channels (float)
 */
gf_redundancy_result_t gf_redundancy_check_float(const float *values,
                                                  uint8_t count,
                                                  float tolerance,
                                                  float *voted_value);

/**
 * @brief Mark channel as failed
 */
void gf_redundancy_mark_failed(uint8_t channel);

/**
 * @brief Get redundancy status
 */
void gf_redundancy_get_status(gf_redundancy_status_t *status);

/*===========================================================================*/
/* Fail-Safe State Machine                                                    */
/*===========================================================================*/

typedef enum {
    GF_SAFETY_STATE_INIT = 0,       /* Initialization/self-test */
    GF_SAFETY_STATE_NORMAL,         /* Normal operation */
    GF_SAFETY_STATE_DEGRADED,       /* Reduced functionality */
    GF_SAFETY_STATE_SAFE,           /* Safe state (minimal operation) */
    GF_SAFETY_STATE_EMERGENCY,      /* Emergency shutdown in progress */
    GF_SAFETY_STATE_FAULT           /* Fault detected, awaiting reset */
} gf_safety_state_t;

typedef enum {
    GF_SAFETY_EVENT_INIT_OK = 0,    /* Self-test passed */
    GF_SAFETY_EVENT_INIT_FAIL,      /* Self-test failed */
    GF_SAFETY_EVENT_FAULT_MINOR,    /* Minor fault detected */
    GF_SAFETY_EVENT_FAULT_MAJOR,    /* Major fault detected */
    GF_SAFETY_EVENT_FAULT_CRITICAL, /* Critical/safety fault */
    GF_SAFETY_EVENT_RECOVERY,       /* Attempt recovery */
    GF_SAFETY_EVENT_RESET,          /* System reset requested */
    GF_SAFETY_EVENT_EXTERNAL_TRIP   /* External safety trip */
} gf_safety_event_t;

typedef void (*gf_safety_action_t)(gf_safety_state_t from, 
                                    gf_safety_state_t to, void *ctx);

typedef struct {
    gf_safety_state_t       initial_state;
    uint32_t                init_timeout_ms;
    uint32_t                safe_state_timeout_ms;
    gf_safety_action_t      on_enter_safe;
    gf_safety_action_t      on_leave_safe;
    void                   *action_ctx;
} gf_safety_fsm_config_t;

/**
 * @brief Initialize fail-safe state machine
 */
int gf_safety_fsm_init(const gf_safety_fsm_config_t *config);

/**
 * @brief Get current safety state
 */
gf_safety_state_t gf_safety_fsm_get_state(void);

/**
 * @brief Process safety event
 */
gf_safety_state_t gf_safety_fsm_event(gf_safety_event_t event);

/**
 * @brief Force transition to safe state
 */
void gf_safety_fsm_force_safe(const char *reason);

/**
 * @brief Check if system is in safe operating condition
 */
bool gf_safety_fsm_is_operational(void);

/**
 * @brief Run periodic safety checks
 */
void gf_safety_fsm_tick(void);

/*===========================================================================*/
/* Watchdog Safety Escalation                                                 */
/*===========================================================================*/

typedef enum {
    GF_SAFETY_WDT_LEVEL_NORMAL = 0,
    GF_SAFETY_WDT_LEVEL_WARNING,    /* First timeout: log warning */
    GF_SAFETY_WDT_LEVEL_DEGRADED,   /* Second: enter degraded mode */
    GF_SAFETY_WDT_LEVEL_SAFE,       /* Third: enter safe state */
    GF_SAFETY_WDT_LEVEL_RESET       /* Fourth: system reset */
} gf_safety_wdt_level_t;

typedef struct {
    uint32_t                timeout_ms;
    gf_safety_wdt_level_t   escalation_levels;  /* Max escalation */
    uint32_t                escalation_delay_ms;
    bool                    reset_on_final;
} gf_safety_wdt_config_t;

typedef struct {
    gf_safety_wdt_level_t   current_level;
    uint32_t                timeout_count;
    uint32_t                last_kick;
    bool                    escalation_active;
} gf_safety_wdt_status_t;

/**
 * @brief Initialize safety watchdog
 */
int gf_safety_wdt_init(const gf_safety_wdt_config_t *config);

/**
 * @brief Kick safety watchdog
 */
void gf_safety_wdt_kick(void);

/**
 * @brief Check watchdog and escalate if needed
 */
gf_safety_wdt_level_t gf_safety_wdt_check(void);

/**
 * @brief Reset escalation level
 */
void gf_safety_wdt_reset_escalation(void);

/**
 * @brief Get watchdog status
 */
void gf_safety_wdt_get_status(gf_safety_wdt_status_t *status);

#endif /* GF_SAFETY_H */
