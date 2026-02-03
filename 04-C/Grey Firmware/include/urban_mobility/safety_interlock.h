/**
 * @file safety_interlock.h
 * @brief Safety Interlock Module for Urban Mobility
 * 
 * INDUSTRY RELEVANCE:
 * Safety interlocks are mandatory for personal electric vehicles (PEVs)
 * and shared mobility fleets. Regulatory compliance, liability reduction,
 * and user safety require sophisticated interlock systems. Companies like
 * Segway-Ninebot, Okai, and major fleet operators rely on firmware for
 * speed limiting, geofencing, and accident prevention.
 * 
 * This module provides safety interlock logic for electric scooters,
 * e-bikes, and other urban mobility vehicles.
 * 
 * KEY CAPABILITIES:
 * - Speed limiting (zone-based)
 * - Tilt detection (rollover prevention)
 * - Obstacle detection integration
 * - Rider presence sensing
 * - Helmet detection (optional)
 * - Battery safety interlocks
 * - Brake failure detection
 * - Emergency stop functionality
 * 
 * STANDARDS COMPLIANCE:
 * - ISO 26262 (Functional safety)
 * - UL 2272 (E-mobility safety)
 * - EN 15194 (EPAC)
 * 
 * @author Grey Firmware Project
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

#define SI_MAX_INTERLOCKS      16    /**< Max interlock conditions */
#define SI_TILT_THRESHOLD_DEG  45    /**< Tilt angle for lockout */
#define SI_IMPACT_THRESHOLD_G  4.0f  /**< Impact detection threshold */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Interlock type */
typedef enum {
    SI_INTERLOCK_SPEED,        /**< Speed limit exceeded */
    SI_INTERLOCK_TILT,         /**< Excessive tilt angle */
    SI_INTERLOCK_BATTERY,      /**< Battery fault */
    SI_INTERLOCK_TEMPERATURE,  /**< Overtemperature */
    SI_INTERLOCK_BRAKE,        /**< Brake system fault */
    SI_INTERLOCK_MOTOR,        /**< Motor fault */
    SI_INTERLOCK_GEOFENCE,     /**< Geofence violation */
    SI_INTERLOCK_RIDER,        /**< No rider detected */
    SI_INTERLOCK_IMPACT,       /**< Impact detected */
    SI_INTERLOCK_MANUAL        /**< Manual lockout */
} si_interlock_t;

/** Interlock action */
typedef enum {
    SI_ACTION_NONE,
    SI_ACTION_WARN,            /**< Warning only */
    SI_ACTION_LIMIT,           /**< Reduce power/speed */
    SI_ACTION_COAST,           /**< Coast to stop */
    SI_ACTION_BRAKE,           /**< Active braking */
    SI_ACTION_LOCKOUT          /**< Complete lockout */
} si_action_t;

/** Safety state */
typedef enum {
    SI_STATE_SAFE,
    SI_STATE_WARNING,
    SI_STATE_LIMITING,
    SI_STATE_STOPPING,
    SI_STATE_LOCKED
} si_state_t;

/** Interlock status */
typedef struct {
    si_interlock_t type;
    si_action_t action;
    bool active;
    float trigger_value;
    float threshold;
    uint32_t timestamp;
} si_interlock_status_t;

/** Safety summary */
typedef struct {
    si_state_t state;
    uint8_t active_interlocks;
    float speed_limit_pct;     /**< Current speed limit */
    float power_limit_pct;     /**< Current power limit */
    bool ride_allowed;
    bool emergency_stop_active;
} si_summary_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize safety interlock system
 * @return 0 on success
 */
int si_init(void);

/**
 * @brief Enable/disable specific interlock
 * @param type Interlock type
 * @param enabled Enable flag
 * @param threshold Trigger threshold
 * @return 0 on success
 */
int si_configure(si_interlock_t type, bool enabled, float threshold);

/**
 * @brief Update sensor input
 * @param type Related interlock
 * @param value Current sensor value
 * @return 0 on success
 */
int si_update_sensor(si_interlock_t type, float value);

/**
 * @brief Get interlock status
 * @param type Interlock to query
 * @param status Output status
 * @return 0 on success
 */
int si_get_interlock(si_interlock_t type, si_interlock_status_t* status);

/**
 * @brief Get safety summary
 * @param summary Output summary
 * @return 0 on success
 */
int si_get_summary(si_summary_t* summary);

/**
 * @brief Trigger emergency stop
 * @return 0 on success
 */
int si_emergency_stop(void);

/**
 * @brief Clear interlocks (after fault resolution)
 * @return 0 on success
 */
int si_clear_interlocks(void);

/**
 * @brief Process interlock logic
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int si_process(uint32_t delta_ms);

/**
 * @brief Shutdown interlock system
 */
void si_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SAFETY_INTERLOCK_H */
