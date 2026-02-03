/**
 * @file warehouse_robot.h
 * @brief Warehouse Robot Control Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Autonomous Mobile Robots (AMRs) revolutionize warehouse operations. The
 * $12B+ market includes:
 * - Goods-to-person picking (Amazon Kiva, Locus)
 * - Pallet transport (MiR, Fetch)
 * - Sorting systems (Geek+, GreyOrange)
 * - Collaborative picking (6 River Systems)
 * 
 * Embedded engineers need expertise in:
 * - LIDAR/camera-based navigation (SLAM)
 * - Multi-robot coordination
 * - Safety systems (obstacle detection, e-stop)
 * - Charging management and fleet scheduling
 * 
 * STANDARDS:
 * - ISO 3691-4 (Industrial Trucks - Driverless)
 * - ANSI/RIA R15.08 (Industrial Mobile Robots)
 * - IEC 61508 (Functional Safety)
 * - VDA 5050 (AGV Communication Interface)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_WAREHOUSE_ROBOT_H
#define GF_WAREHOUSE_ROBOT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define ROBOT_MAX_PAYLOAD_KG        1000  /**< Maximum payload capacity */
#define ROBOT_MAX_SPEED_MPS         2.0f  /**< Maximum speed m/s */
#define ROBOT_LIDAR_POINTS          360   /**< LIDAR scan points */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Robot operating mode */
typedef enum {
    ROBOT_MODE_IDLE,
    ROBOT_MODE_MANUAL,
    ROBOT_MODE_AUTONOMOUS,
    ROBOT_MODE_CHARGING,
    ROBOT_MODE_MAINTENANCE,
    ROBOT_MODE_EMERGENCY_STOP
} robot_mode_t;

/** Navigation state */
typedef enum {
    NAV_STATE_IDLE,
    NAV_STATE_NAVIGATING,
    NAV_STATE_WAITING,
    NAV_STATE_PICKING,
    NAV_STATE_DROPPING,
    NAV_STATE_BLOCKED,
    NAV_STATE_LOCALIZING
} nav_state_t;

/** Robot position */
typedef struct {
    float x_m;
    float y_m;
    float theta_rad;
    float confidence;            /**< Localization confidence */
    uint32_t timestamp;
} robot_pose_t;

/** Mission command */
typedef struct {
    uint32_t mission_id;
    char destination_id[32];
    float x_m;
    float y_m;
    float theta_rad;
    uint8_t priority;
    bool pick_action;
    bool drop_action;
} mission_t;

/** Robot status */
typedef struct {
    robot_mode_t mode;
    nav_state_t nav_state;
    robot_pose_t pose;
    float battery_soc_pct;
    float current_speed_mps;
    float payload_kg;
    uint32_t mission_id;
    uint8_t obstacles_detected;
    bool charging;
    bool error;
} robot_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int warehouse_robot_init(void);
int warehouse_robot_set_mode(robot_mode_t mode);
int warehouse_robot_send_mission(const mission_t *mission);
int warehouse_robot_cancel_mission(void);
int warehouse_robot_get_status(robot_status_t *status);
int warehouse_robot_emergency_stop(void);
int warehouse_robot_resume(void);
int warehouse_robot_dock_for_charging(void);
void warehouse_robot_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_WAREHOUSE_ROBOT_H */
