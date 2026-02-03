/**
 * @file mining_interlock.h
 * @brief Asteroid Mining Safety Interlock Module Stub
 * 
 * Industry Relevance:
 * Autonomous safety systems are critical for uncrewed space mining operations
 * where communication delays prevent real-time human intervention. This module
 * implements multi-layer fault protection for:
 * - Drill overheating and thermal runaway prevention
 * - Mechanical fault detection (motor stall, bit breakage)
 * - Structural integrity monitoring during extraction
 * - Autonomous abort and safe-state transitions
 * 
 * Standards: NASA-STD-8719.14 (Safety), ECSS-E-ST-40C (Software)
 * 
 * @author Grey Firmware Project
 */

#ifndef MINING_INTERLOCK_H
#define MINING_INTERLOCK_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Interlock fault codes */
typedef enum {
    INTERLOCK_FAULT_NONE = 0,
    INTERLOCK_FAULT_OVERHEAT,      /**< Drill temperature exceeded */
    INTERLOCK_FAULT_OVERTORQUE,    /**< Torque limit exceeded */
    INTERLOCK_FAULT_MOTOR_STALL,   /**< Motor rotation stopped */
    INTERLOCK_FAULT_BIT_BREAK,     /**< Drill bit breakage detected */
    INTERLOCK_FAULT_POWER_LOSS,    /**< Power supply fault */
    INTERLOCK_FAULT_VIBRATION,     /**< Excessive vibration */
    INTERLOCK_FAULT_ANCHOR_SLIP,   /**< Spacecraft anchor slippage */
    INTERLOCK_FAULT_COMM_LOSS      /**< Ground communication lost */
} interlock_fault_t;

/** Interlock system state */
typedef enum {
    INTERLOCK_STATE_NOMINAL,       /**< All systems within limits */
    INTERLOCK_STATE_WARNING,       /**< Approaching limits */
    INTERLOCK_STATE_ACTIVE,        /**< Interlock triggered */
    INTERLOCK_STATE_LOCKOUT        /**< Manual reset required */
} interlock_state_t;

/** Interlock thresholds */
typedef struct {
    float max_temp_c;              /**< Maximum drill temperature */
    float max_torque_nm;           /**< Maximum torque */
    float max_vibration_g;         /**< Maximum vibration */
    uint32_t stall_timeout_ms;     /**< Motor stall timeout */
    float min_anchor_force_n;      /**< Minimum anchor force */
} interlock_thresholds_t;

/** Interlock status report */
typedef struct {
    interlock_state_t state;       /**< Current state */
    interlock_fault_t fault;       /**< Active fault code */
    uint32_t fault_count;          /**< Total fault count */
    uint32_t last_fault_time;      /**< Last fault timestamp */
    bool requires_reset;           /**< Manual reset required */
} interlock_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize interlock system
 * @param thresholds Safety thresholds
 * @return 0 on success, negative on error
 */
int mining_interlock_init(const interlock_thresholds_t *thresholds);

/**
 * @brief Update interlock status (call periodically)
 * @param elapsed_ms Time since last update
 * @return Current fault code (FAULT_NONE if nominal)
 */
interlock_fault_t mining_interlock_update(uint32_t elapsed_ms);

/**
 * @brief Get current interlock status
 * @param status Output status structure
 * @return 0 on success, negative on error
 */
int mining_interlock_get_status(interlock_status_t *status);

/**
 * @brief Attempt to reset interlock after fault
 * @return 0 on success, -1 if reset not allowed
 */
int mining_interlock_reset(void);

/**
 * @brief Force immediate safe-state transition
 * @return 0 on success, negative on error
 */
int mining_interlock_emergency_stop(void);

/**
 * @brief Shutdown interlock system
 */
void mining_interlock_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* MINING_INTERLOCK_H */
