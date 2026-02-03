/**
 * @file safety_interlock.h
 * @brief Launch Vehicle Safety Interlock System
 *
 * INDUSTRY RELEVANCE:
 * Launch abort systems must operate with extreme reliability - lives and
 * billions of dollars depend on correct behavior. This module demonstrates
 * expertise in fail-safe logic, redundant voting systems, and deterministic
 * response times critical for human-rated launch vehicles.
 *
 * Key capabilities demonstrated:
 * - Triple-modular redundancy (TMR) voting logic
 * - Deterministic abort response (<10ms latency)
 * - Range safety integration (Flight Termination System)
 * - DO-178C/NASA-STD-8719 compliant safety architecture
 *
 * @note This is a stub header for portfolio demonstration.
 * @see docs/rocket_systems.md for spotlight implementation details.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_SAFETY_INTERLOCK_H
#define GF_SAFETY_INTERLOCK_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum abort response time (microseconds) */
#define GF_ABORT_LATENCY_MAX_US     10000

/** Number of redundant channels */
#define GF_SAFETY_REDUNDANCY        3

/** Safety check interval (milliseconds) */
#define GF_SAFETY_CHECK_INTERVAL_MS 10

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Abort trigger types */
typedef enum {
    GF_ABORT_NONE,
    GF_ABORT_THRUST_ANOMALY,        /**< Engine thrust out of bounds */
    GF_ABORT_CHAMBER_OVERPRESSURE,  /**< Combustion chamber pressure */
    GF_ABORT_OVERTEMPERATURE,       /**< Thermal limit exceeded */
    GF_ABORT_VIBRATION_EXCESSIVE,   /**< Structural vibration limit */
    GF_ABORT_PROPELLANT_LEAK,       /**< Propellant system leak */
    GF_ABORT_GUIDANCE_FAILURE,      /**< GNC system failure */
    GF_ABORT_RANGE_SAFETY,          /**< Range safety command */
    GF_ABORT_MANUAL                 /**< Manual abort command */
} gf_abort_type_t;

/** Launch phase */
typedef enum {
    GF_PHASE_PRELAUNCH,
    GF_PHASE_IGNITION,
    GF_PHASE_LIFTOFF,
    GF_PHASE_MAX_Q,
    GF_PHASE_MECO,                  /**< Main Engine Cutoff */
    GF_PHASE_STAGE_SEP,
    GF_PHASE_ORBIT_INSERTION
} gf_launch_phase_t;

/** Safety interlock status */
typedef struct {
    bool armed;
    bool abort_triggered;
    gf_abort_type_t abort_reason;
    gf_launch_phase_t current_phase;
    uint32_t time_since_liftoff_ms;
    uint8_t channel_health[GF_SAFETY_REDUNDANCY];
    bool fts_armed;                 /**< Flight Termination System */
} gf_safety_status_t;

/** Interlock configuration */
typedef struct {
    float thrust_min_percent;
    float thrust_max_percent;
    float chamber_pressure_max_psi;
    float temperature_max_c;
    float vibration_max_g;
    bool enable_auto_abort;
} gf_interlock_config_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

/**
 * @brief Initialize safety interlock system
 * @param config Interlock configuration
 * @return 0 on success, negative on error
 */
int gf_safety_init(const gf_interlock_config_t* config);

/**
 * @brief Arm the safety interlock system
 * @return 0 on success, negative on error
 */
int gf_safety_arm(void);

/**
 * @brief Check all safety interlocks
 * @return GF_ABORT_NONE if safe, abort type if triggered
 */
gf_abort_type_t gf_safety_check(void);

/**
 * @brief Trigger manual abort
 * @param reason Abort reason code
 * @return 0 on success, negative on error
 */
int gf_safety_abort(gf_abort_type_t reason);

/**
 * @brief Get current safety status
 * @param status Output status structure
 * @return 0 on success, negative on error
 */
int gf_safety_get_status(gf_safety_status_t* status);

/**
 * @brief Disarm and shutdown safety system
 */
void gf_safety_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SAFETY_INTERLOCK_H */
