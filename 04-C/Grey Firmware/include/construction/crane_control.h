/**
 * @file crane_control.h
 * @brief Crane Control Loop Stub - Smart Construction Systems
 * 
 * @details Industry Relevance:
 * Tower cranes and mobile cranes are critical construction equipment
 * requiring safety-certified control systems:
 * - Load moment limiting (prevent tip-over)
 * - Anti-collision systems (multi-crane sites)
 * - Wind speed monitoring and lockout
 * - Operator assistance (anti-sway, path planning)
 * 
 * Safety-critical: Crane failures cause severe injuries and fatalities.
 * Modern systems use SIL-2/SIL-3 rated controllers with redundancy.
 * 
 * Market: Global crane market $45B+, increasing automation demand
 * for operator shortage mitigation and productivity gains.
 * 
 * Standards: EN 13849, IEC 62443, ISO 4309 (wire rope inspection),
 * OSHA 1926.1400-1442 (US crane standards)
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_CONSTRUCTION_CRANE_CONTROL_H
#define GF_CONSTRUCTION_CRANE_CONTROL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum rated load capacity (kg) */
#define GF_CRANE_MAX_LOAD_KG            20000

/** Maximum jib length (meters) */
#define GF_CRANE_MAX_JIB_M              80.0f

/** Maximum wind speed for operation (m/s) */
#define GF_CRANE_MAX_WIND_MS            20.0f

/** Load moment limit safety factor */
#define GF_CRANE_LMI_SAFETY_FACTOR      0.9f

/** Anti-sway control loop rate (Hz) */
#define GF_CRANE_CONTROL_RATE_HZ        50

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Crane operating modes
 */
typedef enum {
    GF_CRANE_MODE_MANUAL,           /**< Manual operator control */
    GF_CRANE_MODE_SEMI_AUTO,        /**< Operator with assistance */
    GF_CRANE_MODE_AUTO_PATH,        /**< Automated path following */
    GF_CRANE_MODE_MAINTENANCE,      /**< Maintenance mode */
    GF_CRANE_MODE_LOCKOUT,          /**< Safety lockout */
    GF_CRANE_MODE_EMERGENCY         /**< Emergency stop active */
} gf_crane_mode_t;

/**
 * @brief Motion axes
 */
typedef enum {
    GF_CRANE_AXIS_SLEW,             /**< Rotation (slew) */
    GF_CRANE_AXIS_TROLLEY,          /**< Trolley in/out */
    GF_CRANE_AXIS_HOIST,            /**< Hook up/down */
    GF_CRANE_AXIS_LUF               /**< Luffing (boom angle) */
} gf_crane_axis_t;

/**
 * @brief Load moment indicator status
 */
typedef struct {
    float load_kg;                  /**< Current load weight */
    float radius_m;                 /**< Load radius from center */
    float moment_pct;               /**< Load moment % of rated */
    float rated_capacity_kg;        /**< Capacity at current radius */
    bool overload_warning;          /**< 90% of rated */
    bool overload_cutout;           /**< Exceeded rated capacity */
} gf_crane_lmi_t;

/**
 * @brief Crane position state
 */
typedef struct {
    float slew_deg;                 /**< Rotation angle */
    float trolley_m;                /**< Trolley position from tower */
    float hoist_m;                  /**< Hook height from ground */
    float hook_load_kg;             /**< Hook load */
    float sway_x_deg;               /**< Load sway X axis */
    float sway_y_deg;               /**< Load sway Y axis */
} gf_crane_position_t;

/**
 * @brief Anti-collision zone
 */
typedef struct {
    uint8_t zone_id;                /**< Zone identifier */
    float min_slew_deg;             /**< Zone boundary min slew */
    float max_slew_deg;             /**< Zone boundary max slew */
    float min_radius_m;             /**< Zone boundary min radius */
    float max_radius_m;             /**< Zone boundary max radius */
    bool other_crane;               /**< Another crane in zone */
    bool obstacle;                  /**< Fixed obstacle */
} gf_crane_zone_t;

/**
 * @brief Motion command
 */
typedef struct {
    gf_crane_axis_t axis;           /**< Axis to move */
    float velocity;                 /**< Commanded velocity (% max) */
    float target;                   /**< Target position (optional) */
    bool use_target;                /**< Move to target mode */
    bool anti_sway;                 /**< Enable anti-sway */
} gf_crane_motion_t;

/**
 * @brief Environmental conditions
 */
typedef struct {
    float wind_speed_ms;            /**< Current wind speed */
    float wind_gust_ms;             /**< Peak gust */
    float wind_dir_deg;             /**< Wind direction */
    float temp_c;                   /**< Temperature */
    bool rain;                      /**< Rain detected */
    bool lightning;                 /**< Lightning nearby */
} gf_crane_environment_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize crane control system
 * @return 0 on success, negative error code on failure
 */
int gf_crane_init(void);

/**
 * @brief Process control loop iteration
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int gf_crane_process(uint32_t delta_ms);

/**
 * @brief Execute motion command
 * @param motion Motion command
 * @return 0 on success, -1 if blocked by safety
 */
int gf_crane_move(const gf_crane_motion_t* motion);

/**
 * @brief Emergency stop all motion
 * @return 0 on success
 */
int gf_crane_emergency_stop(void);

/**
 * @brief Get current position
 * @param position Output position structure
 * @return 0 on success
 */
int gf_crane_get_position(gf_crane_position_t* position);

/**
 * @brief Get load moment indicator status
 * @param lmi Output LMI structure
 * @return 0 on success
 */
int gf_crane_get_lmi(gf_crane_lmi_t* lmi);

/**
 * @brief Update environmental conditions
 * @param env Environmental data
 * @return 0 on success
 */
int gf_crane_update_environment(const gf_crane_environment_t* env);

/**
 * @brief Check anti-collision zones
 * @param zones Output array of zone statuses
 * @param max_zones Maximum zones to return
 * @return Number of zones
 */
int gf_crane_check_zones(gf_crane_zone_t* zones, uint8_t max_zones);

/**
 * @brief Set operating mode
 * @param mode Desired mode
 * @return 0 on success
 */
int gf_crane_set_mode(gf_crane_mode_t mode);

/**
 * @brief Shutdown crane control
 * @return 0 on success
 */
int gf_crane_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CONSTRUCTION_CRANE_CONTROL_H */
