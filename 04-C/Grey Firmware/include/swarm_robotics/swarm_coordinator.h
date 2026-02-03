/**
 * @file swarm_coordinator.h
 * @brief Swarm Coordination for Edge Robotics Swarms
 * 
 * @details
 * Decentralized swarm coordination for multi-robot systems. Implements
 * task allocation, formation control, consensus algorithms, and
 * collective decision-making.
 * 
 * INDUSTRY RELEVANCE:
 * - Warehouse robotics (Amazon, Fetch)
 * - Agricultural swarm vehicles
 * - Search and rescue drones
 * - Underwater exploration (BlueROV)
 * - Space exploration rovers
 * 
 * KEY FEATURES:
 * - Decentralized task allocation
 * - Formation control
 * - Leader election
 * - Consensus algorithms
 * - Flocking behaviors
 * - Role assignment
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_SWARM_COORDINATOR_H
#define GF_SWARM_COORDINATOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum swarm size */
#define GF_SWARM_MAX_AGENTS         128

/** Maximum tasks */
#define GF_SWARM_MAX_TASKS          64

/** Agent ID length */
#define GF_SWARM_ID_LEN             8

/** Coordination loop period (ms) */
#define GF_SWARM_COORD_PERIOD_MS    100

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Swarm status codes
 */
typedef enum {
    GF_SWARM_OK = 0,
    GF_SWARM_ERROR_NOT_INIT,
    GF_SWARM_ERROR_NULL_PTR,
    GF_SWARM_ERROR_NO_AGENTS,
    GF_SWARM_ERROR_TASK_FAIL,
    GF_SWARM_ERROR_COMM_LOST,
    GF_SWARM_WARN_DEGRADED,
    GF_SWARM_WARN_AGENT_LOST
} gf_swarm_status_t;

/**
 * @brief Agent role
 */
typedef enum {
    GF_SWARM_ROLE_WORKER,         /**< General worker */
    GF_SWARM_ROLE_LEADER,         /**< Swarm leader */
    GF_SWARM_ROLE_SCOUT,          /**< Scout/explorer */
    GF_SWARM_ROLE_RELAY,          /**< Communication relay */
    GF_SWARM_ROLE_SUPERVISOR      /**< Human interface */
} gf_swarm_role_t;

/**
 * @brief Formation type
 */
typedef enum {
    GF_SWARM_FORM_NONE,           /**< No formation */
    GF_SWARM_FORM_LINE,           /**< Line formation */
    GF_SWARM_FORM_WEDGE,          /**< Wedge/V formation */
    GF_SWARM_FORM_CIRCLE,         /**< Circle formation */
    GF_SWARM_FORM_GRID,           /**< Grid pattern */
    GF_SWARM_FORM_CUSTOM          /**< Custom formation */
} gf_swarm_formation_t;

/**
 * @brief Task type
 */
typedef enum {
    GF_SWARM_TASK_EXPLORE,        /**< Area exploration */
    GF_SWARM_TASK_TRANSPORT,      /**< Object transport */
    GF_SWARM_TASK_MONITOR,        /**< Area monitoring */
    GF_SWARM_TASK_SEARCH,         /**< Search operation */
    GF_SWARM_TASK_ASSEMBLE,       /**< Group assembly */
    GF_SWARM_TASK_ESCORT          /**< Escort mission */
} gf_swarm_task_type_t;

/**
 * @brief Agent state
 */
typedef struct {
    uint8_t id[GF_SWARM_ID_LEN];  /**< Agent identifier */
    gf_swarm_role_t role;         /**< Current role */
    int32_t pos_x_mm;             /**< X position */
    int32_t pos_y_mm;             /**< Y position */
    int32_t pos_z_mm;             /**< Z position */
    int16_t heading_deg;          /**< Heading (degrees) */
    int16_t velocity_mm_s;        /**< Speed */
    uint8_t battery_pct;          /**< Battery level */
    uint8_t task_id;              /**< Current task ID */
    bool active;                  /**< Agent active */
    uint32_t last_update_ms;      /**< Last state update */
} gf_swarm_agent_t;

/**
 * @brief Task definition
 */
typedef struct {
    uint8_t task_id;              /**< Task identifier */
    gf_swarm_task_type_t type;    /**< Task type */
    int32_t target_x_mm;          /**< Target X */
    int32_t target_y_mm;          /**< Target Y */
    int32_t target_z_mm;          /**< Target Z */
    uint8_t priority;             /**< Priority (0-255) */
    uint8_t required_agents;      /**< Agents needed */
    uint8_t assigned_agents;      /**< Agents assigned */
    bool completed;               /**< Task completed */
} gf_swarm_task_t;

/**
 * @brief Formation parameters
 */
typedef struct {
    gf_swarm_formation_t type;    /**< Formation type */
    int32_t spacing_mm;           /**< Agent spacing */
    int32_t center_x_mm;          /**< Formation center X */
    int32_t center_y_mm;          /**< Formation center Y */
    int16_t heading_deg;          /**< Formation heading */
} gf_swarm_formation_params_t;

/**
 * @brief Swarm statistics
 */
typedef struct {
    uint8_t total_agents;         /**< Total agents */
    uint8_t active_agents;        /**< Active agents */
    uint8_t tasks_pending;        /**< Pending tasks */
    uint8_t tasks_complete;       /**< Completed tasks */
    uint8_t leader_id;            /**< Current leader */
    float cohesion;               /**< Swarm cohesion (0-1) */
} gf_swarm_stats_t;

/**
 * @brief Event callback
 */
typedef void (*gf_swarm_event_cb_t)(gf_swarm_status_t event,
                                     const uint8_t* agent_id,
                                     void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize swarm coordinator
 * @return Status code
 */
gf_swarm_status_t gf_swarm_init(void);

/**
 * @brief Shutdown swarm coordinator
 */
void gf_swarm_shutdown(void);

/**
 * @brief Register local agent
 * @param agent Agent state
 * @return Status code
 */
gf_swarm_status_t gf_swarm_register_agent(const gf_swarm_agent_t* agent);

/**
 * @brief Update agent state
 * @param agent Updated agent state
 * @return Status code
 */
gf_swarm_status_t gf_swarm_update_agent(const gf_swarm_agent_t* agent);

/**
 * @brief Get agent state
 * @param id Agent ID
 * @param agent Output agent state
 * @return Status code
 */
gf_swarm_status_t gf_swarm_get_agent(const uint8_t* id,
                                      gf_swarm_agent_t* agent);

/**
 * @brief Create task
 * @param task Task definition
 * @return Status code
 */
gf_swarm_status_t gf_swarm_create_task(const gf_swarm_task_t* task);

/**
 * @brief Set formation
 * @param params Formation parameters
 * @return Status code
 */
gf_swarm_status_t gf_swarm_set_formation(const gf_swarm_formation_params_t* params);

/**
 * @brief Elect leader
 * @return Status code
 */
gf_swarm_status_t gf_swarm_elect_leader(void);

/**
 * @brief Get statistics
 * @param stats Output statistics
 * @return Status code
 */
gf_swarm_status_t gf_swarm_get_stats(gf_swarm_stats_t* stats);

/**
 * @brief Register event callback
 * @param callback Event callback
 * @param user_data User context
 * @return Status code
 */
gf_swarm_status_t gf_swarm_register_callback(gf_swarm_event_cb_t callback,
                                              void* user_data);

/**
 * @brief Process coordination
 * @return Status code
 */
gf_swarm_status_t gf_swarm_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SWARM_COORDINATOR_H */
