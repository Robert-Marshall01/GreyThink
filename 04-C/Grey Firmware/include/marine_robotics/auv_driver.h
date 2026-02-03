/**
 * @file auv_driver.h
 * @brief Autonomous Underwater Vehicle Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * AUVs are transforming ocean exploration, offshore energy inspection, and
 * marine research. These vehicles operate in GPS-denied environments and
 * require sophisticated navigation, pressure management, and acoustic
 * communication systems for missions lasting hours to days.
 * 
 * KEY CAPABILITIES:
 * - Depth control and pressure compensation
 * - Thruster control (multi-axis propulsion)
 * - DVL (Doppler Velocity Log) integration
 * - USBL/LBL acoustic positioning
 * - Obstacle avoidance sonar integration
 * - Emergency surface/recovery procedures
 * 
 * STANDARDS COMPLIANCE:
 * - ISO 13628 (Subsea Production Systems)
 * - DNV-ST-0510 (Framework for AUV Operations)
 * - IEC 61508 (Functional Safety)
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_AUV_DRIVER_H
#define GF_AUV_DRIVER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** AUV operating mode */
typedef enum {
    AUV_MODE_SURFACE,      /**< Surface operations */
    AUV_MODE_DIVE,         /**< Active diving */
    AUV_MODE_WAYPOINT,     /**< Following waypoint mission */
    AUV_MODE_STATION_KEEP, /**< Holding position */
    AUV_MODE_EMERGENCY     /**< Emergency ascent */
} auv_mode_t;

/** Navigation source */
typedef enum {
    NAV_GPS,               /**< GPS (surface only) */
    NAV_DVL,               /**< Doppler Velocity Log */
    NAV_USBL,              /**< Ultra-Short Baseline acoustic */
    NAV_INS,               /**< Inertial Navigation System */
    NAV_DEAD_RECKONING     /**< Dead reckoning fallback */
} auv_nav_source_t;

/** AUV state */
typedef struct {
    double latitude;       /**< Position (if known) */
    double longitude;
    float depth_m;         /**< Current depth in meters */
    float heading_deg;     /**< Heading in degrees */
    float pitch_deg;       /**< Pitch angle */
    float roll_deg;        /**< Roll angle */
    float velocity_mps;    /**< Forward velocity m/s */
    auv_mode_t mode;
    auv_nav_source_t nav_source;
    float battery_pct;     /**< Battery remaining */
    bool obstacle_detected;
} auv_state_t;

/** Waypoint definition */
typedef struct {
    double latitude;
    double longitude;
    float target_depth_m;
    float speed_mps;
    uint16_t dwell_time_s; /**< Time to hold at waypoint */
} auv_waypoint_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize AUV subsystem
 * @param vehicle_id Vehicle identifier
 * @return 0 on success, negative on error
 */
int auv_init(uint32_t vehicle_id);

/**
 * @brief Set target depth
 * @param depth_m Target depth in meters
 * @return 0 on success, negative on error
 */
int auv_set_depth(float depth_m);

/**
 * @brief Set heading
 * @param heading_deg Target heading in degrees
 * @return 0 on success, negative on error
 */
int auv_set_heading(float heading_deg);

/**
 * @brief Upload mission waypoints
 * @param waypoints Array of waypoints
 * @param count Number of waypoints
 * @return 0 on success, negative on error
 */
int auv_upload_mission(const auv_waypoint_t* waypoints, uint8_t count);

/**
 * @brief Start mission execution
 * @return 0 on success, negative on error
 */
int auv_start_mission(void);

/**
 * @brief Abort mission and surface
 * @return 0 on success, negative on error
 */
int auv_abort_mission(void);

/**
 * @brief Get current AUV state
 * @param state Output state structure
 * @return 0 on success, negative on error
 */
int auv_get_state(auv_state_t* state);

/**
 * @brief Emergency surface procedure
 * @return 0 on success, negative on error
 */
int auv_emergency_surface(void);

/**
 * @brief Shutdown AUV subsystem
 * @return 0 on success, negative on error
 */
int auv_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_AUV_DRIVER_H */
