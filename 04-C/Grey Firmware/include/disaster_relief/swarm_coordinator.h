/**
 * @file swarm_coordinator.h
 * @brief Disaster Relief Drone Swarm Coordination
 * 
 * INDUSTRY RELEVANCE:
 * Multi-UAV coordination enables rapid disaster assessment, search and rescue,
 * and supply delivery. This module implements distributed swarm algorithms,
 * task allocation, and collision avoidance for coordinated drone operations
 * in degraded communication environments.
 * 
 * Key applications:
 * - Post-earthquake/hurricane damage assessment
 * - Search and rescue thermal imaging
 * - Medical supply delivery (Zipline model)
 * - Firefighting coordination
 * - Infrastructure inspection swarms
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_SWARM_COORDINATOR_H
#define GF_SWARM_COORDINATOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define SWARM_MAX_AGENTS            32      /**< Max swarm members */
#define SWARM_MAX_TASKS             64      /**< Max concurrent tasks */
#define SWARM_COMM_RANGE_M          5000    /**< Communication range (m) */
#define SWARM_SAFETY_DISTANCE_M     50      /**< Min separation (m) */
#define SWARM_UPDATE_RATE_HZ        10      /**< Coordination rate */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Agent role
 */
typedef enum {
    SWARM_ROLE_SCOUT = 0,           /**< Reconnaissance */
    SWARM_ROLE_TRANSPORT,           /**< Cargo delivery */
    SWARM_ROLE_RELAY,               /**< Communication relay */
    SWARM_ROLE_IMAGING,             /**< Mapping/imaging */
    SWARM_ROLE_RESCUE               /**< Search and rescue */
} swarm_role_t;

/**
 * @brief Task type
 */
typedef enum {
    SWARM_TASK_SURVEY_AREA = 0,
    SWARM_TASK_SEARCH_TARGET,
    SWARM_TASK_DELIVER_PAYLOAD,
    SWARM_TASK_ESTABLISH_RELAY,
    SWARM_TASK_ESCORT,
    SWARM_TASK_RTB                  /**< Return to base */
} swarm_task_type_t;

/**
 * @brief Agent status
 */
typedef struct {
    uint8_t agent_id;
    swarm_role_t role;
    float latitude;
    float longitude;
    float altitude_m;
    float heading_deg;
    float speed_mps;
    float battery_percent;
    uint8_t current_task_id;
    bool comm_active;
    uint32_t last_update;
} swarm_agent_t;

/**
 * @brief Task definition
 */
typedef struct {
    uint8_t task_id;
    swarm_task_type_t type;
    float target_lat;
    float target_lon;
    float target_alt;
    float priority;                 /**< 0-1, higher = more urgent */
    uint8_t assigned_agent;
    bool complete;
    uint32_t deadline;
} swarm_task_t;

/**
 * @brief Swarm state
 */
typedef struct {
    uint8_t agents_active;
    uint8_t tasks_pending;
    uint8_t tasks_complete;
    float coverage_percent;         /**< Area coverage */
    float avg_battery;
    bool leader_elected;
    uint8_t leader_id;
} swarm_state_t;

/**
 * @brief Coordinator configuration
 */
typedef struct {
    uint8_t agent_id;               /**< This agent's ID */
    swarm_role_t default_role;
    bool can_be_leader;
    float safety_margin_m;
    uint8_t task_allocation_algo;   /**< 0=Auction, 1=Hungarian, 2=Greedy */
} swarm_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize swarm coordinator
 * @param config Coordinator configuration
 * @return 0 on success, negative on error
 */
int swarm_init(const swarm_config_t* config);

/**
 * @brief Shutdown coordinator
 * @return 0 on success, negative on error
 */
int swarm_shutdown(void);

/**
 * @brief Update local agent state (broadcast to swarm)
 * @param state Local agent state
 * @return 0 on success, negative on error
 */
int swarm_update_self(const swarm_agent_t* state);

/**
 * @brief Get agent state
 * @param agent_id Agent ID
 * @param agent Output agent state
 * @return 0 on success, negative on error
 */
int swarm_get_agent(uint8_t agent_id, swarm_agent_t* agent);

/**
 * @brief Add task to task pool
 * @param task Task definition
 * @return Task ID on success, negative on error
 */
int swarm_add_task(const swarm_task_t* task);

/**
 * @brief Get next task for this agent
 * @param task Output task
 * @return 0 on success, negative on error
 */
int swarm_get_next_task(swarm_task_t* task);

/**
 * @brief Report task completion
 * @param task_id Task ID
 * @param success Task succeeded
 * @return 0 on success, negative on error
 */
int swarm_complete_task(uint8_t task_id, bool success);

/**
 * @brief Get swarm state
 * @param state Output state
 * @return 0 on success, negative on error
 */
int swarm_get_state(swarm_state_t* state);

/**
 * @brief Get collision avoidance command
 * @param avoid_heading Output heading to avoid collision (deg)
 * @return 1 if avoidance needed, 0 if clear, negative on error
 */
int swarm_collision_check(float* avoid_heading);

/**
 * @brief Process swarm coordination (call at SWARM_UPDATE_RATE_HZ)
 * @return 0 on success, negative on error
 */
int swarm_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SWARM_COORDINATOR_H */
