/**
 * @file collision_avoidance.h
 * @brief Collision Avoidance System for Edge Robotics Swarms
 * 
 * @details
 * Real-time collision avoidance for multi-robot systems. Implements
 * velocity obstacles, reciprocal collision avoidance, and cooperative
 * path planning for safe swarm operation.
 * 
 * INDUSTRY RELEVANCE:
 * - Autonomous vehicle coordination
 * - Warehouse robot fleets
 * - Drone swarm deconfliction
 * - Marine vessel traffic
 * - Aircraft collision avoidance (TCAS-like)
 * 
 * KEY FEATURES:
 * - Velocity obstacles (VO)
 * - Reciprocal velocity obstacles (RVO/ORCA)
 * - Time-to-collision estimation
 * - Emergency stop triggers
 * - Priority-based avoidance
 * - Predictive path planning
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_COLLISION_AVOIDANCE_H
#define GF_COLLISION_AVOIDANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum tracked agents */
#define GF_AVOID_MAX_AGENTS         64

/** Maximum obstacles */
#define GF_AVOID_MAX_OBSTACLES      128

/** Minimum safe distance (mm) */
#define GF_AVOID_MIN_DISTANCE_MM    500

/** Avoidance loop period (ms) */
#define GF_AVOID_LOOP_PERIOD_MS     50

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Avoidance status codes
 */
typedef enum {
    GF_AVOID_OK = 0,
    GF_AVOID_ERROR_NOT_INIT,
    GF_AVOID_ERROR_NULL_PTR,
    GF_AVOID_ERROR_NO_SOLUTION,
    GF_AVOID_WARN_CLOSE_CALL,
    GF_AVOID_WARN_EVASION_ACTIVE,
    GF_AVOID_CRITICAL_COLLISION,
    GF_AVOID_CRITICAL_DEADLOCK
} gf_avoid_status_t;

/**
 * @brief Avoidance algorithm
 */
typedef enum {
    GF_AVOID_ALG_SIMPLE,          /**< Simple distance-based */
    GF_AVOID_ALG_VO,              /**< Velocity obstacles */
    GF_AVOID_ALG_RVO,             /**< Reciprocal velocity obstacles */
    GF_AVOID_ALG_ORCA,            /**< Optimal reciprocal */
    GF_AVOID_ALG_POTENTIAL        /**< Potential field */
} gf_avoid_algorithm_t;

/**
 * @brief Agent priority
 */
typedef enum {
    GF_AVOID_PRIO_LOW,            /**< Yields to all */
    GF_AVOID_PRIO_NORMAL,         /**< Standard priority */
    GF_AVOID_PRIO_HIGH,           /**< Higher priority */
    GF_AVOID_PRIO_EMERGENCY       /**< Emergency vehicle */
} gf_avoid_priority_t;

/**
 * @brief Obstacle type
 */
typedef enum {
    GF_AVOID_OBS_STATIC,          /**< Static obstacle */
    GF_AVOID_OBS_DYNAMIC,         /**< Moving obstacle */
    GF_AVOID_OBS_AGENT,           /**< Other swarm agent */
    GF_AVOID_OBS_BOUNDARY         /**< Boundary/wall */
} gf_avoid_obs_type_t;

/**
 * @brief Agent state for avoidance
 */
typedef struct {
    uint8_t agent_id[8];          /**< Agent identifier */
    int32_t pos_x_mm;             /**< X position */
    int32_t pos_y_mm;             /**< Y position */
    int32_t vel_x_mm_s;           /**< X velocity */
    int32_t vel_y_mm_s;           /**< Y velocity */
    uint16_t radius_mm;           /**< Agent radius */
    int16_t max_speed_mm_s;       /**< Maximum speed */
    gf_avoid_priority_t priority; /**< Agent priority */
} gf_avoid_agent_t;

/**
 * @brief Obstacle definition
 */
typedef struct {
    uint16_t obstacle_id;         /**< Obstacle ID */
    gf_avoid_obs_type_t type;     /**< Obstacle type */
    int32_t pos_x_mm;             /**< X position */
    int32_t pos_y_mm;             /**< Y position */
    int32_t vel_x_mm_s;           /**< X velocity (dynamic) */
    int32_t vel_y_mm_s;           /**< Y velocity (dynamic) */
    uint16_t radius_mm;           /**< Obstacle radius */
    uint32_t last_update_ms;      /**< Last update time */
} gf_avoid_obstacle_t;

/**
 * @brief Avoidance command
 */
typedef struct {
    int32_t vel_x_mm_s;           /**< Commanded X velocity */
    int32_t vel_y_mm_s;           /**< Commanded Y velocity */
    bool stop_required;           /**< Emergency stop required */
    uint16_t time_to_collision_ms; /**< Time to nearest collision */
    uint8_t nearest_obstacle_id;  /**< Nearest obstacle */
    float avoidance_effort;       /**< Avoidance effort (0-1) */
} gf_avoid_command_t;

/**
 * @brief Configuration
 */
typedef struct {
    gf_avoid_algorithm_t algorithm; /**< Algorithm selection */
    uint16_t safety_distance_mm;  /**< Safety margin */
    uint16_t lookahead_ms;        /**< Prediction horizon */
    float max_avoidance_pct;      /**< Max speed reduction */
    bool cooperative_mode;        /**< Cooperative avoidance */
    bool emergency_stop_enable;   /**< Enable emergency stop */
} gf_avoid_config_t;

/**
 * @brief Alert callback
 */
typedef void (*gf_avoid_alert_cb_t)(gf_avoid_status_t status,
                                     uint16_t obstacle_id,
                                     uint16_t distance_mm,
                                     void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize collision avoidance
 * @param config Configuration
 * @return Status code
 */
gf_avoid_status_t gf_avoid_init(const gf_avoid_config_t* config);

/**
 * @brief Shutdown collision avoidance
 */
void gf_avoid_shutdown(void);

/**
 * @brief Update local agent state
 * @param agent Local agent state
 * @return Status code
 */
gf_avoid_status_t gf_avoid_update_self(const gf_avoid_agent_t* agent);

/**
 * @brief Update other agent state
 * @param agent Other agent state
 * @return Status code
 */
gf_avoid_status_t gf_avoid_update_agent(const gf_avoid_agent_t* agent);

/**
 * @brief Update obstacle
 * @param obstacle Obstacle info
 * @return Status code
 */
gf_avoid_status_t gf_avoid_update_obstacle(const gf_avoid_obstacle_t* obstacle);

/**
 * @brief Remove obstacle
 * @param obstacle_id Obstacle ID
 * @return Status code
 */
gf_avoid_status_t gf_avoid_remove_obstacle(uint16_t obstacle_id);

/**
 * @brief Compute avoidance command
 * @param desired_vel_x Desired X velocity
 * @param desired_vel_y Desired Y velocity
 * @param command Output command
 * @return Status code
 */
gf_avoid_status_t gf_avoid_compute(int32_t desired_vel_x,
                                    int32_t desired_vel_y,
                                    gf_avoid_command_t* command);

/**
 * @brief Get time to collision
 * @param obstacle_id Specific obstacle (0 for nearest)
 * @return Time to collision (ms), 0xFFFF if no collision
 */
uint16_t gf_avoid_get_ttc(uint16_t obstacle_id);

/**
 * @brief Check if path is clear
 * @param target_x Target X position
 * @param target_y Target Y position
 * @return True if path is clear
 */
bool gf_avoid_path_clear(int32_t target_x, int32_t target_y);

/**
 * @brief Trigger emergency stop
 * @return Status code
 */
gf_avoid_status_t gf_avoid_emergency_stop(void);

/**
 * @brief Register alert callback
 * @param callback Alert callback
 * @param user_data User context
 * @return Status code
 */
gf_avoid_status_t gf_avoid_register_callback(gf_avoid_alert_cb_t callback,
                                              void* user_data);

/**
 * @brief Process avoidance
 * @return Status code
 */
gf_avoid_status_t gf_avoid_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_COLLISION_AVOIDANCE_H */
