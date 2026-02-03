/**
 * @file shelf_robot.h
 * @brief Autonomous Shelf-Stocking Robot Driver
 * 
 * INDUSTRY RELEVANCE:
 * Retail automation is transforming store operations with autonomous
 * mobile robots (AMRs) handling inventory replenishment, stock checks,
 * and customer assistance. These robots operate in dynamic human
 * environments requiring sophisticated navigation and safety systems.
 * 
 * KEY CAPABILITIES:
 * - SLAM (Simultaneous Localization and Mapping)
 * - Dynamic path planning around customers/obstacles
 * - Shelf scanning and planogram verification
 * - Autonomous charging and shift scheduling
 * - Human-robot interaction (voice, display)
 * - Fleet coordination with central system
 * 
 * OPERATION MODES:
 * - Inventory scanning patrol
 * - Targeted restocking mission
 * - Customer assistance escort
 * - Return-to-base charging
 * - Emergency stop/yield
 * 
 * STANDARDS COMPLIANCE:
 * - ISO 3691-4 (Driverless Industrial Trucks)
 * - ANSI/RIA R15.08 (Industrial Mobile Robots)
 * - IEC 61508 (Functional Safety)
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_SHELF_ROBOT_H
#define GF_SHELF_ROBOT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Robot operating mode */
typedef enum {
    ROBOT_MODE_IDLE,           /**< Waiting for task */
    ROBOT_MODE_PATROL,         /**< Inventory scanning */
    ROBOT_MODE_RESTOCK,        /**< Restocking mission */
    ROBOT_MODE_ESCORT,         /**< Customer assistance */
    ROBOT_MODE_CHARGING,       /**< At charging station */
    ROBOT_MODE_EMERGENCY       /**< Emergency stop */
} robot_mode_t;

/** Navigation status */
typedef enum {
    NAV_STATUS_IDLE,
    NAV_STATUS_PLANNING,
    NAV_STATUS_MOVING,
    NAV_STATUS_AVOIDING,       /**< Obstacle avoidance */
    NAV_STATUS_ARRIVED,
    NAV_STATUS_BLOCKED
} robot_nav_status_t;

/** Robot state */
typedef struct {
    float x;                   /**< Position in store (meters) */
    float y;
    float heading;             /**< Heading in degrees */
    float velocity;            /**< Current velocity m/s */
    float battery_pct;         /**< Battery percentage */
    robot_mode_t mode;
    robot_nav_status_t nav_status;
    uint16_t items_scanned;    /**< Items scanned this shift */
    uint16_t items_restocked;  /**< Items restocked this shift */
    bool human_nearby;         /**< Human detected nearby */
} robot_state_t;

/** Restock task */
typedef struct {
    uint32_t task_id;
    uint64_t sku;              /**< Item SKU */
    uint8_t aisle;
    uint8_t shelf;
    uint8_t position;
    uint16_t quantity;
    uint8_t priority;
} restock_task_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize shelf robot
 * @param robot_id Robot identifier
 * @return 0 on success, negative on error
 */
int shelf_robot_init(uint32_t robot_id);

/**
 * @brief Start inventory patrol
 * @param zone_id Zone to patrol (0 = full store)
 * @return 0 on success, negative on error
 */
int shelf_robot_start_patrol(uint8_t zone_id);

/**
 * @brief Assign restock task
 * @param task Restock task details
 * @return 0 on success, negative on error
 */
int shelf_robot_assign_task(const restock_task_t* task);

/**
 * @brief Navigate to location
 * @param x Target X coordinate
 * @param y Target Y coordinate
 * @return 0 on success, negative on error
 */
int shelf_robot_goto(float x, float y);

/**
 * @brief Emergency stop
 * @return 0 on success, negative on error
 */
int shelf_robot_estop(void);

/**
 * @brief Resume from emergency stop
 * @return 0 on success, negative on error
 */
int shelf_robot_resume(void);

/**
 * @brief Return to charging station
 * @return 0 on success, negative on error
 */
int shelf_robot_return_to_base(void);

/**
 * @brief Get current robot state
 * @param state Output state
 * @return 0 on success, negative on error
 */
int shelf_robot_get_state(robot_state_t* state);

/**
 * @brief Shutdown robot
 * @return 0 on success, negative on error
 */
int shelf_robot_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SHELF_ROBOT_H */
