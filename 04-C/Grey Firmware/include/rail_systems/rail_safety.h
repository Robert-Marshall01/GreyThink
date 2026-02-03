/**
 * @file rail_safety.h
 * @brief Safety Interlock Module for Smart Rail Systems
 * 
 * @details
 * Safety-critical interlock system implementing SIL-4 requirements
 * for rail applications. Provides interlocking logic, vital relay
 * control, and fail-safe operations.
 * 
 * INDUSTRY RELEVANCE:
 * - Computer-Based Interlocking (Siemens, Hitachi)
 * - EN 50129 SIL-4 compliance
 * - Vital relay interfaces
 * - Level crossing control
 * - Signaling systems
 * 
 * KEY FEATURES:
 * - Route locking and releasing
 * - Point interlocking
 * - Signal aspect control
 * - Conflicting route prevention
 * - Level crossing protection
 * - Fail-safe state management
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_RAIL_SAFETY_H
#define GF_RAIL_SAFETY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum routes */
#define GF_RAIL_MAX_ROUTES          64

/** Maximum signals */
#define GF_RAIL_MAX_SIGNALS         128

/** Maximum points */
#define GF_RAIL_MAX_POINTS          64

/** Safety integrity level */
#define GF_RAIL_SAFETY_LEVEL        4

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Safety status codes
 */
typedef enum {
    GF_RAIL_SAFE_OK = 0,
    GF_RAIL_SAFE_ERROR_NOT_INIT,
    GF_RAIL_SAFE_ERROR_NULL_PTR,
    GF_RAIL_SAFE_ERROR_CONFLICT,
    GF_RAIL_SAFE_ERROR_OCCUPIED,
    GF_RAIL_SAFE_ERROR_POINT_FAIL,
    GF_RAIL_SAFE_ERROR_TIMEOUT,
    GF_RAIL_SAFE_FAILSAFE_ACTIVE
} gf_rail_safe_status_t;

/**
 * @brief Signal aspect
 */
typedef enum {
    GF_SIGNAL_STOP,               /**< Red - Stop */
    GF_SIGNAL_CAUTION,            /**< Yellow - Caution */
    GF_SIGNAL_PRELIM_CAUTION,     /**< Double yellow */
    GF_SIGNAL_CLEAR,              /**< Green - Clear */
    GF_SIGNAL_CALL_ON,            /**< Proceed on sight */
    GF_SIGNAL_SHUNT,              /**< Shunting signal */
    GF_SIGNAL_FAULT               /**< Lamp failure */
} gf_signal_aspect_t;

/**
 * @brief Route state
 */
typedef enum {
    GF_ROUTE_FREE,                /**< Route not set */
    GF_ROUTE_SETTING,             /**< Route being set */
    GF_ROUTE_SET,                 /**< Route set, signal clear */
    GF_ROUTE_OCCUPIED,            /**< Train in route */
    GF_ROUTE_RELEASING,           /**< Route releasing */
    GF_ROUTE_LOCKED               /**< Route locked out */
} gf_route_state_t;

/**
 * @brief Level crossing state
 */
typedef enum {
    GF_CROSSING_OPEN,             /**< Barriers up, road open */
    GF_CROSSING_CLOSING,          /**< Barriers lowering */
    GF_CROSSING_CLOSED,           /**< Barriers down, rail clear */
    GF_CROSSING_FAILED            /**< Crossing failure */
} gf_crossing_state_t;

/**
 * @brief Route definition
 */
typedef struct {
    uint16_t route_id;            /**< Route identifier */
    uint16_t entry_signal;        /**< Entry signal ID */
    uint16_t exit_signal;         /**< Exit signal ID */
    uint16_t point_ids[8];        /**< Points in route */
    bool point_positions[8];      /**< Required positions */
    uint8_t point_count;          /**< Number of points */
    uint16_t section_ids[16];     /**< Track sections */
    uint8_t section_count;        /**< Number of sections */
    uint16_t conflicting[8];      /**< Conflicting route IDs */
    uint8_t conflict_count;       /**< Number of conflicts */
} gf_route_def_t;

/**
 * @brief Interlock state snapshot
 */
typedef struct {
    gf_route_state_t route_states[GF_RAIL_MAX_ROUTES];
    gf_signal_aspect_t signal_states[GF_RAIL_MAX_SIGNALS];
    bool point_positions[GF_RAIL_MAX_POINTS];
    bool point_locked[GF_RAIL_MAX_POINTS];
    bool failsafe_active;         /**< Failsafe state active */
    uint32_t last_update_ms;      /**< Last state update */
} gf_interlock_state_t;

/**
 * @brief Safety event callback
 */
typedef void (*gf_rail_safe_cb_t)(gf_rail_safe_status_t status,
                                   uint16_t element_id, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize safety interlock
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_init(void);

/**
 * @brief Shutdown safety interlock
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_shutdown(void);

/**
 * @brief Request route
 * @param route_id Route identifier
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_request_route(uint16_t route_id);

/**
 * @brief Cancel route
 * @param route_id Route identifier
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_cancel_route(uint16_t route_id);

/**
 * @brief Get signal aspect
 * @param signal_id Signal identifier
 * @return Current aspect
 */
gf_signal_aspect_t gf_rail_safe_get_signal(uint16_t signal_id);

/**
 * @brief Request point movement
 * @param point_id Point identifier
 * @param normal True for normal, false for reverse
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_request_point(uint16_t point_id, bool normal);

/**
 * @brief Lock point position
 * @param point_id Point identifier
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_lock_point(uint16_t point_id);

/**
 * @brief Close level crossing
 * @param crossing_id Crossing identifier
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_close_crossing(uint16_t crossing_id);

/**
 * @brief Get crossing state
 * @param crossing_id Crossing identifier
 * @return Crossing state
 */
gf_crossing_state_t gf_rail_safe_get_crossing(uint16_t crossing_id);

/**
 * @brief Get interlock state
 * @param state Output state
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_get_state(gf_interlock_state_t* state);

/**
 * @brief Trigger failsafe
 * @param reason Failsafe reason
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_failsafe(const char* reason);

/**
 * @brief Register safety callback
 * @param callback Safety event callback
 * @param user_data User context
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_register_callback(gf_rail_safe_cb_t callback,
                                                      void* user_data);

/**
 * @brief Process safety logic (call periodically)
 * @return Status code
 */
gf_rail_safe_status_t gf_rail_safe_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_RAIL_SAFETY_H */
