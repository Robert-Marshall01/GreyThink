/**
 * @file safety_interlock.h
 * @brief Robot Safety Interlock System
 * 
 * INDUSTRY RELEVANCE:
 * Industrial robots must meet stringent safety standards to protect workers.
 * This module demonstrates:
 * - Safety-rated monitored stop (STO, SS1, SS2)
 * - Speed and separation monitoring (SSM)
 * - Collaborative robot force/torque limiting
 * - Emergency stop chain and safety relay control
 * 
 * Applications: Industrial robots, collaborative robots, CNC machines,
 *               presses, material handling, surgical robots
 * Standards: ISO 10218, ISO/TS 15066, IEC 62443, ISO 13849 (PL e)
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_SAFETY_INTERLOCK_H
#define GF_SAFETY_INTERLOCK_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Safety Interlock Types
 ******************************************************************************/

/** Safety function types (IEC 61800-5-2) */
typedef enum {
    GF_SAFETY_STO,            /**< Safe Torque Off */
    GF_SAFETY_SS1,            /**< Safe Stop 1 (controlled stop then STO) */
    GF_SAFETY_SS2,            /**< Safe Stop 2 (controlled stop, monitor) */
    GF_SAFETY_SOS,            /**< Safe Operating Stop */
    GF_SAFETY_SLS,            /**< Safely Limited Speed */
    GF_SAFETY_SLP,            /**< Safely Limited Position */
    GF_SAFETY_SLT             /**< Safely Limited Torque */
} gf_safety_function_t;

/** Safety zone state */
typedef enum {
    GF_ZONE_CLEAR,            /**< No human detected */
    GF_ZONE_WARNING,          /**< Human approaching */
    GF_ZONE_SLOWDOWN,         /**< Human in slowdown zone */
    GF_ZONE_STOP              /**< Human in safety zone - STOP */
} gf_zone_state_t;

/** E-stop state */
typedef enum {
    GF_ESTOP_NORMAL,
    GF_ESTOP_TRIGGERED,
    GF_ESTOP_RESETTING,
    GF_ESTOP_FAULT            /**< E-stop circuit fault */
} gf_estop_state_t;

/** Safety integrity level */
typedef enum {
    GF_SIL_NONE,
    GF_SIL_1,                 /**< SIL 1 / Cat B */
    GF_SIL_2,                 /**< SIL 2 / Cat 2/PL c */
    GF_SIL_3                  /**< SIL 3 / Cat 4/PL e */
} gf_safety_sil_t;

/*******************************************************************************
 * Safety Interlock Configuration
 ******************************************************************************/

/** Safety zone configuration */
typedef struct {
    uint16_t warning_distance_mm;     /**< Warning zone distance */
    uint16_t slowdown_distance_mm;    /**< Slow zone distance */
    uint16_t stop_distance_mm;        /**< Stop zone distance */
    uint16_t max_reduced_speed_mms;   /**< Speed in slow zone */
    uint8_t scanner_count;            /**< Safety scanner count */
} gf_safety_zone_config_t;

/** Force/torque limits (collaborative) */
typedef struct {
    uint16_t max_force_n;             /**< Maximum contact force */
    uint16_t max_torque_nm;           /**< Maximum torque */
    uint16_t max_pressure_pa;         /**< Maximum pressure */
    uint32_t max_energy_j;            /**< Maximum kinetic energy */
    uint16_t force_time_ms;           /**< Max force duration */
} gf_cobot_limits_t;

/** Safety controller configuration */
typedef struct {
    gf_safety_sil_t sil_level;
    gf_safety_zone_config_t zones;
    gf_cobot_limits_t cobot;
    
    /* I/O configuration */
    uint8_t estop_input_gpio;
    uint8_t estop_output_gpio;
    uint8_t safety_relay_gpio;
    uint8_t enable_switch_gpio;
    uint8_t light_curtain_gpio;
    
    /* Timing */
    uint16_t sto_reaction_ms;         /**< STO activation time */
    uint16_t ss1_ramp_ms;             /**< SS1 deceleration time */
    uint16_t heartbeat_timeout_ms;    /**< Safety PLC comms timeout */
    
    /* Dual-channel monitoring */
    bool enable_dual_channel;
    bool enable_pulse_test;           /**< Test relay contacts */
    uint32_t test_interval_ms;
} gf_safety_config_t;

/** Safety status */
typedef struct {
    bool sto_active;
    bool ss1_active;
    bool ss2_active;
    gf_estop_state_t estop_state;
    gf_zone_state_t zone_state;
    
    /* Monitored values */
    uint16_t current_speed_mms;       /**< Current TCP speed */
    uint16_t current_force_n;         /**< Current force */
    uint16_t current_torque_nm;       /**< Current torque */
    
    /* Limit status */
    bool speed_limit_active;
    bool force_limit_active;
    bool position_limit_active;
    
    /* System health */
    bool safety_relay_ok;
    bool dual_channel_ok;
    bool plc_heartbeat_ok;
    uint16_t fault_code;
} gf_safety_status_t;

/*******************************************************************************
 * Safety Interlock Statistics
 ******************************************************************************/

typedef struct {
    uint32_t estop_activations;
    uint32_t sto_activations;
    uint32_t zone_violations;
    uint32_t force_limit_events;
    uint32_t speed_limit_events;
    uint32_t safety_tests_passed;
    uint32_t safety_tests_failed;
    uint32_t uptime_hours;
    float availability_pct;           /**< Safety system uptime */
} gf_safety_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize safety interlock
 * @param config Safety configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_init(const gf_safety_config_t *config);

/**
 * @brief Process safety monitoring (call at high rate)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_process(void);

/**
 * @brief Activate safety function
 * @param function Safety function to activate
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_activate(gf_safety_function_t function);

/**
 * @brief Request reset (requires manual confirmation)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_request_reset(void);

/**
 * @brief Confirm reset (with enable switch)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_confirm_reset(void);

/**
 * @brief Update zone sensor reading
 * @param zone_id Zone index
 * @param distance_mm Detected distance
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_update_zone(uint8_t zone_id, uint16_t distance_mm);

/**
 * @brief Update force/torque reading
 * @param force_n Current force
 * @param torque_nm Current torque
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_update_force(uint16_t force_n, uint16_t torque_nm);

/**
 * @brief Update speed reading
 * @param speed_mms Current TCP speed
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_update_speed(uint16_t speed_mms);

/**
 * @brief Check if motion is allowed
 * @return true if safe to move
 */
bool gf_safety_motion_allowed(void);

/**
 * @brief Get current safety status
 * @param status Output status
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_get_status(gf_safety_status_t *status);

/**
 * @brief Run safety self-test
 * @return GF_SUCCESS if all tests pass
 */
gf_error_t gf_safety_self_test(void);

/**
 * @brief Get safety statistics
 * @return Current statistics
 */
gf_safety_stats_t gf_safety_get_stats(void);

/**
 * @brief Shutdown safety interlock
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_safety_interlock_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SAFETY_INTERLOCK_H */
