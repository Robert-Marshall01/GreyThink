/**
 * @file safety_interlock.h
 * @brief Safety Interlock System
 * 
 * WHAT: Hardware and software interlock monitoring for machine safety.
 *       Implements safety relay outputs, E-stop circuits, guard monitoring,
 *       and safe-torque-off (STO) coordination.
 * 
 * WHY: Functional safety (IEC 61508/62443, ISO 13849) is critical in
 *      industrial automation. Understanding PLe/SIL requirements, dual-channel
 *      monitoring, and fail-safe design demonstrates awareness of safety
 *      standards valued in industrial firmware roles.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Industrial: CNC guards, robot cell fencing
 *   - Automotive assembly: Light curtains, safety mats
 *   - Medical devices: Door interlocks, beam shutters
 *   - Semiconductor: Interlock chains, toxic gas detection
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Dual-channel redundancy
 *   - Cross-checking for discrepancy detection
 *   - Safe state definition
 *   - Watchdog coordination
 *   - Safety relay pulse testing
 */

#ifndef GF_SAFETY_INTERLOCK_H
#define GF_SAFETY_INTERLOCK_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_SAFETY_MAX_CHANNELS      8       /* Max safety input channels */
#define GF_SAFETY_MAX_OUTPUTS       4       /* Max safety relay outputs */
#define GF_SAFETY_DISCREPANCY_MS    50      /* Max channel mismatch time */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_SAFETY_STATE_OK = 0,         /* All checks pass */
    GF_SAFETY_STATE_ESTOP,          /* E-stop activated */
    GF_SAFETY_STATE_GUARD_OPEN,     /* Guard door open */
    GF_SAFETY_STATE_DISCREPANCY,    /* Dual-channel mismatch */
    GF_SAFETY_STATE_COMM_LOSS,      /* Safety PLC comm lost */
    GF_SAFETY_STATE_FAULT           /* Internal fault */
} gf_safety_state_t;

typedef enum {
    GF_INTERLOCK_GUARD = 0,         /* Guard door switch */
    GF_INTERLOCK_ESTOP,             /* Emergency stop button */
    GF_INTERLOCK_LIGHT_CURTAIN,     /* Optical safety barrier */
    GF_INTERLOCK_SAFETY_MAT,        /* Pressure-sensitive mat */
    GF_INTERLOCK_ENABLE_SWITCH      /* Three-position enable */
} gf_interlock_type_t;

typedef struct {
    gf_interlock_type_t type;
    uint8_t             gpio_ch1;           /* Primary GPIO */
    uint8_t             gpio_ch2;           /* Redundant GPIO (or 0xFF) */
    bool                normally_closed;    /* NC = safe, NO = fault */
    bool                require_pulse_test; /* Periodic output test */
} gf_interlock_config_t;

typedef struct {
    gf_safety_state_t   state;
    uint8_t             tripped_channel;    /* Which input tripped */
    uint32_t            trip_timestamp;     /* When trip occurred */
    uint32_t            discrepancy_count;  /* Cross-check failures */
    bool                sto_active;         /* Safe Torque Off active */
} gf_safety_status_t;

/* Callback on state change */
typedef void (*gf_safety_callback)(gf_safety_state_t state, uint8_t channel, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize safety subsystem
 */
int gf_safety_init(void);

/**
 * @brief Configure an interlock channel (dual-channel)
 */
int gf_safety_add_interlock(uint8_t channel, const gf_interlock_config_t *config);

/**
 * @brief Get current safety state
 */
gf_safety_state_t gf_safety_get_state(void);

/**
 * @brief Get detailed status
 */
void gf_safety_get_status(gf_safety_status_t *status);

/**
 * @brief Reset safety system (requires all inputs clear)
 */
int gf_safety_reset(void);

/**
 * @brief Force safe state (emergency software trigger)
 */
void gf_safety_force_safe_state(void);

/**
 * @brief Register state change callback
 */
void gf_safety_set_callback(gf_safety_callback callback, void *ctx);

/**
 * @brief Poll inputs (call from main loop or timer ISR)
 */
void gf_safety_poll(void);

/**
 * @brief Check if machine can run (all interlocks satisfied)
 */
bool gf_safety_can_run(void);

/**
 * @brief Get driver descriptor
 */
const void* gf_safety_get_driver(void);

#endif /* GF_SAFETY_INTERLOCK_H */
