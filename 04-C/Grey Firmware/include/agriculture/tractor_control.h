/**
 * @file tractor_control.h
 * @brief Autonomous Tractor Control Stub - Smart Agriculture Robotics
 * 
 * @details Industry Relevance:
 * Precision agriculture relies on autonomous tractors for planting, 
 * harvesting, and field maintenance. Control systems must handle:
 * - GPS-guided path following with RTK correction
 * - Implement control (seeders, sprayers, harvesters)
 * - Obstacle detection and avoidance (ISO 18497)
 * - Variable rate application based on field maps
 * 
 * Market drivers: Labor shortages, precision farming efficiency gains
 * of 15-20%, and sustainability requirements for reduced chemical usage.
 * 
 * Key standards: ISO 11783 (ISOBUS), SAE J1939, ISO 25119 (AgPL safety)
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_AGRICULTURE_TRACTOR_CONTROL_H
#define GF_AGRICULTURE_TRACTOR_CONTROL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum steering angle in degrees */
#define GF_TRACTOR_MAX_STEER_ANGLE      45.0f

/** Maximum ground speed in km/h */
#define GF_TRACTOR_MAX_SPEED_KMH        30.0f

/** GPS update rate in Hz */
#define GF_TRACTOR_GPS_RATE_HZ          10

/** Cross-track error threshold for path alarm (meters) */
#define GF_TRACTOR_XTE_ALARM_M          0.5f

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Tractor operating mode
 */
typedef enum {
    GF_TRACTOR_MODE_MANUAL,         /**< Manual operator control */
    GF_TRACTOR_MODE_AUTO_STEER,     /**< Auto-steering, manual throttle */
    GF_TRACTOR_MODE_FULL_AUTO,      /**< Fully autonomous operation */
    GF_TRACTOR_MODE_HEADLAND,       /**< Headland turn sequence */
    GF_TRACTOR_MODE_TRANSPORT,      /**< Road transport mode */
    GF_TRACTOR_MODE_EMERGENCY       /**< Emergency stop active */
} gf_tractor_mode_t;

/**
 * @brief GPS position with RTK correction status
 */
typedef struct {
    double latitude_deg;            /**< Latitude in degrees */
    double longitude_deg;           /**< Longitude in degrees */
    float altitude_m;               /**< Altitude above MSL */
    float heading_deg;              /**< True heading */
    float speed_kmh;                /**< Ground speed */
    uint8_t fix_quality;            /**< GPS fix quality (0-5) */
    bool rtk_fixed;                 /**< RTK fixed solution */
    float hdop;                     /**< Horizontal dilution of precision */
} gf_tractor_gps_t;

/**
 * @brief Tractor control state
 */
typedef struct {
    gf_tractor_mode_t mode;         /**< Current operating mode */
    gf_tractor_gps_t position;      /**< Current GPS position */
    float target_heading_deg;       /**< Desired heading */
    float steering_angle_deg;       /**< Current steering angle */
    float throttle_percent;         /**< Engine throttle 0-100% */
    float cross_track_error_m;      /**< Distance from guidance line */
    bool implement_lowered;         /**< Implement in working position */
    bool obstacle_detected;         /**< Obstacle in path */
    uint32_t area_worked_m2;        /**< Cumulative area covered */
} gf_tractor_state_t;

/**
 * @brief Implement control commands
 */
typedef struct {
    bool lower_implement;           /**< Lower/raise implement */
    float section_enables;          /**< Boom section enables (bitmask) */
    float application_rate;         /**< Variable rate 0-100% */
    bool headland_turn_start;       /**< Initiate headland sequence */
} gf_tractor_implement_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize tractor control system
 * @return 0 on success, negative error code on failure
 */
int gf_tractor_init(void);

/**
 * @brief Update GPS position from receiver
 * @param gps GPS data structure
 * @return 0 on success
 */
int gf_tractor_update_gps(const gf_tractor_gps_t* gps);

/**
 * @brief Set guidance line for auto-steering
 * @param line_ab AB line definition (start/end points)
 * @return 0 on success
 */
int gf_tractor_set_guidance_line(const void* line_ab);

/**
 * @brief Process control loop iteration
 * @param delta_ms Time since last call in milliseconds
 * @return Steering command or error code
 */
int gf_tractor_process(uint32_t delta_ms);

/**
 * @brief Get current tractor state
 * @param state Output state structure
 * @return 0 on success
 */
int gf_tractor_get_state(gf_tractor_state_t* state);

/**
 * @brief Emergency stop - immediate halt
 * @return 0 on success
 */
int gf_tractor_emergency_stop(void);

/**
 * @brief Send implement control commands
 * @param cmd Implement command structure
 * @return 0 on success
 */
int gf_tractor_control_implement(const gf_tractor_implement_t* cmd);

#ifdef __cplusplus
}
#endif

#endif /* GF_AGRICULTURE_TRACTOR_CONTROL_H */
