/**
 * @file safety_interlock.h
 * @brief Safety Interlock Module - Smart Construction Systems
 * 
 * @details Industry Relevance:
 * Construction equipment and site safety requires multiple layers of
 * protection to prevent injuries and equipment damage:
 * - Machine interlock (prevent operation during maintenance)
 * - Exclusion zone enforcement (keep workers away from hazards)
 * - Multi-key lockout/tagout (LOTO) systems
 * - Emergency stop networks (distributed E-stop)
 * - Permit-to-work electronic systems
 * 
 * Functional safety applies to construction equipment control systems.
 * Safety networks (PROFIsafe, CIP Safety, FSoE) enable distributed
 * safety functions with certified response times.
 * 
 * Regulations: OSHA 29 CFR 1926, EN ISO 13849, IEC 62061
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_CONSTRUCTION_SAFETY_INTERLOCK_H
#define GF_CONSTRUCTION_SAFETY_INTERLOCK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum interlock points */
#define GF_INTERLOCK_MAX_POINTS         64

/** Maximum e-stop stations */
#define GF_INTERLOCK_MAX_ESTOPS         16

/** Safety network scan time (ms) */
#define GF_INTERLOCK_SCAN_TIME_MS       4

/** Watchdog timeout (ms) */
#define GF_INTERLOCK_WATCHDOG_MS        100

/** Maximum lockout tags per point */
#define GF_INTERLOCK_MAX_TAGS           8

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Interlock types
 */
typedef enum {
    GF_INTERLOCK_GUARD,             /**< Physical guard switch */
    GF_INTERLOCK_LIGHT_CURTAIN,     /**< Light curtain/laser scanner */
    GF_INTERLOCK_PROXIMITY,         /**< Proximity detection */
    GF_INTERLOCK_KEY,               /**< Key-operated switch */
    GF_INTERLOCK_ZONE,              /**< Exclusion zone (GPS/UWB) */
    GF_INTERLOCK_PERMIT,            /**< Electronic permit */
    GF_INTERLOCK_SEQUENCE           /**< Sequence interlock */
} gf_interlock_type_t;

/**
 * @brief Safety integrity level
 */
typedef enum {
    GF_INTERLOCK_SIL_NONE,          /**< Non-safety */
    GF_INTERLOCK_SIL_1,             /**< SIL 1 */
    GF_INTERLOCK_SIL_2,             /**< SIL 2 */
    GF_INTERLOCK_SIL_3              /**< SIL 3 */
} gf_interlock_sil_t;

/**
 * @brief Interlock state
 */
typedef enum {
    GF_INTERLOCK_STATE_SAFE,        /**< Safe to operate */
    GF_INTERLOCK_STATE_TRIPPED,     /**< Interlock active */
    GF_INTERLOCK_STATE_BYPASSED,    /**< Authorized bypass */
    GF_INTERLOCK_STATE_FAULT,       /**< Sensor/wiring fault */
    GF_INTERLOCK_STATE_UNKNOWN      /**< State unknown */
} gf_interlock_state_t;

/**
 * @brief E-stop states
 */
typedef enum {
    GF_ESTOP_RELEASED,              /**< E-stop not active */
    GF_ESTOP_PRESSED,               /**< E-stop pressed */
    GF_ESTOP_RESET_REQUIRED,        /**< Needs manual reset */
    GF_ESTOP_FAULT                  /**< E-stop circuit fault */
} gf_estop_state_t;

/**
 * @brief Interlock point status
 */
typedef struct {
    uint8_t point_id;               /**< Point identifier */
    gf_interlock_type_t type;       /**< Interlock type */
    gf_interlock_state_t state;     /**< Current state */
    gf_interlock_sil_t sil;         /**< Required SIL */
    uint32_t trip_count;            /**< Total trip count */
    uint32_t last_trip_time;        /**< Last trip timestamp */
    uint8_t active_tags;            /**< Active lockout tags */
    bool bypass_active;             /**< Bypass in effect */
    uint32_t bypass_expires;        /**< Bypass expiration */
} gf_interlock_point_t;

/**
 * @brief E-stop station status
 */
typedef struct {
    uint8_t station_id;             /**< Station identifier */
    char location[32];              /**< Station location name */
    gf_estop_state_t state;         /**< Current state */
    uint32_t activations;           /**< Total activation count */
    uint32_t last_activation;       /**< Last activation timestamp */
    bool dual_channel_ok;           /**< Dual-channel agreement */
} gf_estop_station_t;

/**
 * @brief Lockout tag
 */
typedef struct {
    uint16_t tag_id;                /**< Tag identifier */
    uint8_t point_id;               /**< Locked point */
    char worker_id[16];             /**< Worker identification */
    uint32_t applied_time;          /**< When tag applied */
    char reason[64];                /**< Lockout reason */
    bool supervisor_key;            /**< Supervisor override present */
} gf_lockout_tag_t;

/**
 * @brief Safety system overall status
 */
typedef struct {
    gf_estop_state_t estop;         /**< Overall E-stop state */
    uint8_t interlocks_tripped;     /**< Count of tripped interlocks */
    uint8_t interlocks_bypassed;    /**< Count of bypassed */
    uint8_t active_lockouts;        /**< Active lockout tags */
    bool safety_relay_ok;           /**< Safety relay energized */
    bool watchdog_ok;               /**< Watchdog healthy */
    uint32_t last_scan_ms;          /**< Last scan time */
} gf_safety_status_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize safety interlock system
 * @return 0 on success, negative error code on failure
 */
int gf_interlock_init(void);

/**
 * @brief Process safety scan (call at scan rate)
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int gf_interlock_process(uint32_t delta_ms);

/**
 * @brief Register interlock point
 * @param point_id Point identifier
 * @param type Interlock type
 * @param sil Required SIL
 * @return 0 on success
 */
int gf_interlock_register(uint8_t point_id,
                          gf_interlock_type_t type,
                          gf_interlock_sil_t sil);

/**
 * @brief Update interlock input state
 * @param point_id Point to update
 * @param channel Channel number (0 or 1)
 * @param safe True if safe, false if tripped
 * @return 0 on success
 */
int gf_interlock_update(uint8_t point_id, uint8_t channel, bool safe);

/**
 * @brief Request bypass (requires authorization)
 * @param point_id Point to bypass
 * @param duration_s Bypass duration in seconds
 * @param auth_token Authorization token
 * @return 0 on success, -1 if not authorized
 */
int gf_interlock_bypass(uint8_t point_id,
                        uint32_t duration_s,
                        const char* auth_token);

/**
 * @brief Apply lockout tag
 * @param tag Tag information
 * @return 0 on success
 */
int gf_interlock_apply_lockout(const gf_lockout_tag_t* tag);

/**
 * @brief Remove lockout tag
 * @param tag_id Tag to remove
 * @param worker_id Worker removing (must match)
 * @return 0 on success
 */
int gf_interlock_remove_lockout(uint16_t tag_id, const char* worker_id);

/**
 * @brief Get interlock point status
 * @param point_id Point to query
 * @param point Output status
 * @return 0 on success
 */
int gf_interlock_get_point(uint8_t point_id, gf_interlock_point_t* point);

/**
 * @brief Get E-stop station status
 * @param station_id Station to query
 * @param station Output status
 * @return 0 on success
 */
int gf_interlock_get_estop(uint8_t station_id, gf_estop_station_t* station);

/**
 * @brief Reset E-stop after release
 * @param station_id Station to reset (0 for all)
 * @return 0 on success
 */
int gf_interlock_reset_estop(uint8_t station_id);

/**
 * @brief Get overall safety status
 * @param status Output status
 * @return 0 on success
 */
int gf_interlock_get_status(gf_safety_status_t* status);

/**
 * @brief Check if safe to operate (all interlocks clear)
 * @return true if safe
 */
bool gf_interlock_is_safe(void);

/**
 * @brief Shutdown safety interlock system
 * @return 0 on success
 */
int gf_interlock_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CONSTRUCTION_SAFETY_INTERLOCK_H */
