/**
 * @file collision_avoidance.h
 * @brief Retail Robot Collision Avoidance System
 * 
 * INDUSTRY RELEVANCE:
 * Robots operating in public retail spaces must safely navigate around
 * customers, shopping carts, and other dynamic obstacles. This module
 * implements multi-layered safety with redundant sensing and fail-safe
 * behaviors meeting industrial robot safety standards.
 * 
 * KEY CAPABILITIES:
 * - Multi-sensor fusion (LiDAR, cameras, ultrasonics)
 * - Dynamic obstacle tracking and prediction
 * - Velocity-based safety zones
 * - Human detection and special handling
 * - Shopping cart and inventory recognition
 * - Emergency stop with situational recovery
 * 
 * SAFETY LAYERS:
 * 1. Protective field: Immediate stop zone
 * 2. Warning field: Slow down and alert
 * 3. Detection field: Plan avoidance
 * 
 * STANDARDS COMPLIANCE:
 * - ISO 13482 (Service Robots)
 * - IEC 61508 SIL 2 (Functional Safety)
 * - ANSI/RIA R15.08 (Mobile Robots)
 * - ISO 3691-4 (Driverless Trucks)
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_COLLISION_AVOIDANCE_H
#define GF_COLLISION_AVOIDANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Obstacle classification */
typedef enum {
    OBS_CLASS_UNKNOWN,
    OBS_CLASS_HUMAN,           /**< Person detected */
    OBS_CLASS_SHOPPING_CART,   /**< Shopping cart */
    OBS_CLASS_SHELF,           /**< Static shelf */
    OBS_CLASS_ROBOT,           /**< Other robot */
    OBS_CLASS_PALLET,          /**< Pallet/boxes */
    OBS_CLASS_OTHER            /**< Other dynamic */
} obstacle_class_t;

/** Safety zone status */
typedef enum {
    ZONE_CLEAR,                /**< No obstacles */
    ZONE_DETECTION,            /**< Planning avoidance */
    ZONE_WARNING,              /**< Slowing down */
    ZONE_PROTECTIVE,           /**< Immediate stop */
    ZONE_EMERGENCY             /**< Emergency triggered */
} zone_status_t;

/** Detected obstacle */
typedef struct {
    uint16_t obstacle_id;
    obstacle_class_t classification;
    float x;                   /**< Relative X (meters) */
    float y;                   /**< Relative Y (meters) */
    float velocity_x;          /**< Velocity X (m/s) */
    float velocity_y;          /**< Velocity Y (m/s) */
    float size_m;              /**< Approximate size */
    float confidence;          /**< Detection confidence */
    uint32_t last_seen_ms;
} obstacle_t;

/** Collision system status */
typedef struct {
    zone_status_t zone;
    uint8_t obstacles_tracked;
    float min_distance_m;      /**< Distance to nearest */
    float max_safe_velocity;   /**< Current safe speed */
    bool estop_active;
    uint32_t close_calls;      /**< Near-miss count */
} collision_status_t;

/** Sensor configuration */
typedef struct {
    bool lidar_enabled;
    bool camera_enabled;
    bool ultrasonic_enabled;
    float protective_distance_m;  /**< E-stop zone size */
    float warning_distance_m;     /**< Slowdown zone */
    float detection_distance_m;   /**< Planning zone */
    float max_robot_velocity;     /**< Maximum speed */
} collision_config_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize collision avoidance
 * @param config System configuration
 * @return 0 on success, negative on error
 */
int coll_avoid_init(const collision_config_t* config);

/**
 * @brief Update LiDAR scan data
 * @param ranges Range array (meters)
 * @param count Number of ranges
 * @param angle_min Start angle (radians)
 * @param angle_inc Angle increment
 * @return 0 on success, negative on error
 */
int coll_avoid_update_lidar(const float* ranges, uint16_t count,
                            float angle_min, float angle_inc);

/**
 * @brief Update camera detections
 * @param obstacles Detected obstacles from vision
 * @param count Number of obstacles
 * @return 0 on success, negative on error
 */
int coll_avoid_update_camera(const obstacle_t* obstacles, uint8_t count);

/**
 * @brief Process collision avoidance (call periodically)
 * @param delta_ms Time since last call
 * @return Recommended velocity multiplier (0.0-1.0)
 */
float coll_avoid_process(uint32_t delta_ms);

/**
 * @brief Get current system status
 * @param status Output status
 * @return 0 on success, negative on error
 */
int coll_avoid_get_status(collision_status_t* status);

/**
 * @brief Get tracked obstacles
 * @param obstacles Output array
 * @param max_count Maximum to return
 * @param count Actual count
 * @return 0 on success, negative on error
 */
int coll_avoid_get_obstacles(obstacle_t* obstacles, uint8_t max_count,
                             uint8_t* count);

/**
 * @brief Clear emergency stop condition
 * @return 0 on success, negative on error
 */
int coll_avoid_clear_estop(void);

/**
 * @brief Shutdown collision avoidance
 * @return 0 on success, negative on error
 */
int coll_avoid_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_COLLISION_AVOIDANCE_H */
