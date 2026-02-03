/**
 * @file warehouse_robot.h
 * @brief Autonomous Warehouse Robot Control Interface
 * 
 * @details
 * This module provides control interfaces for autonomous mobile robots (AMRs)
 * used in warehouse automation, including navigation, obstacle avoidance,
 * pick/place operations, and fleet coordination.
 * 
 * INDUSTRY RELEVANCE:
 * - Amazon Robotics / Kiva systems
 * - Warehouse automation (Locus, 6 River, Fetch)
 * - Distribution center operations
 * - E-commerce fulfillment
 * - Manufacturing material handling
 * - Cold storage/pharmaceutical logistics
 * 
 * KEY CAPABILITIES:
 * - SLAM-based navigation
 * - Dynamic obstacle avoidance  
 * - Pick/place manipulation
 * - Fleet coordination
 * - Battery management
 * - Safety systems (ISO 3691-4)
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_WAREHOUSE_ROBOT_H
#define GF_WAREHOUSE_ROBOT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_WR_OK = 0,
    GF_WR_ERROR_NOT_INITIALIZED,
    GF_WR_ERROR_NULL_PTR,
    GF_WR_ERROR_NAVIGATION,
    GF_WR_ERROR_LOCALIZATION,
    GF_WR_ERROR_OBSTACLE,
    GF_WR_ERROR_BATTERY_LOW,
    GF_WR_ERROR_MOTOR_FAULT,
    GF_WR_ERROR_ESTOP,
    GF_WR_ERROR_COMMS,
    GF_WR_WARN_PATH_BLOCKED
} gf_wr_status_t;

typedef enum {
    GF_WR_STATE_IDLE,             /**< Waiting for task */
    GF_WR_STATE_NAVIGATING,       /**< Moving to destination */
    GF_WR_STATE_PICKING,          /**< Picking item */
    GF_WR_STATE_PLACING,          /**< Placing item */
    GF_WR_STATE_CHARGING,         /**< At charging station */
    GF_WR_STATE_MANUAL,           /**< Manual control mode */
    GF_WR_STATE_EMERGENCY_STOP,   /**< E-stop activated */
    GF_WR_STATE_FAULT             /**< Fault condition */
} gf_wr_state_t;

typedef enum {
    GF_WR_TASK_PICK,              /**< Pick item from location */
    GF_WR_TASK_PLACE,             /**< Place item at location */
    GF_WR_TASK_TRANSPORT,         /**< Transport between locations */
    GF_WR_TASK_CHARGE,            /**< Go to charging station */
    GF_WR_TASK_WAIT,              /**< Wait at location */
    GF_WR_TASK_INVENTORY          /**< Inventory scan mission */
} gf_wr_task_type_t;

typedef struct {
    float x_m;                    /**< X position (meters) */
    float y_m;                    /**< Y position (meters) */
    float theta_rad;              /**< Heading (radians) */
    float confidence;             /**< Localization confidence (0-1) */
    uint8_t floor_id;             /**< Floor/level identifier */
    uint64_t timestamp_ms;        /**< Position timestamp */
} gf_wr_pose_t;

typedef struct {
    float x_m;                    /**< X coordinate */
    float y_m;                    /**< Y coordinate */
    uint8_t floor_id;             /**< Floor identifier */
    uint16_t zone_id;             /**< Zone identifier */
    uint16_t aisle_id;            /**< Aisle number */
    uint8_t rack_id;              /**< Rack number */
    uint8_t level_id;             /**< Shelf level */
} gf_wr_location_t;

typedef struct {
    uint32_t task_id;             /**< Task identifier */
    gf_wr_task_type_t type;       /**< Task type */
    gf_wr_location_t source;      /**< Source location */
    gf_wr_location_t destination; /**< Destination location */
    uint32_t item_id;             /**< Item/SKU if applicable */
    uint8_t priority;             /**< Task priority (0-255) */
    uint64_t deadline_ms;         /**< Task deadline */
} gf_wr_task_t;

typedef struct {
    uint16_t robot_id;            /**< Robot identifier */
    gf_wr_state_t state;          /**< Current state */
    gf_wr_pose_t pose;            /**< Current position */
    uint8_t battery_pct;          /**< Battery percentage */
    float velocity_mps;           /**< Current velocity */
    uint32_t current_task_id;     /**< Active task ID */
    uint32_t tasks_completed;     /**< Tasks completed today */
    float distance_today_m;       /**< Distance traveled today */
} gf_wr_status_report_t;

typedef void (*gf_wr_task_cb_t)(uint32_t task_id, gf_wr_status_t result, void* user_data);
typedef void (*gf_wr_obstacle_cb_t)(float distance_m, float angle_rad, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_wr_status_t gf_wr_init(uint16_t robot_id);
void gf_wr_shutdown(void);
gf_wr_status_t gf_wr_start_task(const gf_wr_task_t* task);
gf_wr_status_t gf_wr_cancel_task(uint32_t task_id);
gf_wr_status_t gf_wr_navigate_to(const gf_wr_location_t* destination);
gf_wr_status_t gf_wr_get_pose(gf_wr_pose_t* pose);
gf_wr_status_t gf_wr_get_status(gf_wr_status_report_t* status);
gf_wr_status_t gf_wr_emergency_stop(void);
gf_wr_status_t gf_wr_release_estop(void);
gf_wr_status_t gf_wr_set_manual_velocity(float linear_mps, float angular_radps);
gf_wr_status_t gf_wr_register_task_callback(gf_wr_task_cb_t callback, void* user_data);
gf_wr_status_t gf_wr_register_obstacle_callback(gf_wr_obstacle_cb_t callback, void* user_data);
gf_wr_status_t gf_wr_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_WAREHOUSE_ROBOT_H */
