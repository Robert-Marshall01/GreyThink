/**
 * @file drill_control.h
 * @brief Lunar Drill Control Loop Interface
 * 
 * INDUSTRY RELEVANCE:
 * Lunar drilling presents unique challenges: 1/6 Earth gravity affects chip
 * evacuation, extreme temperature swings cause thermal stress, and regolith
 * abrasiveness accelerates tool wear. Companies developing lunar drills include
 * Honeybee Robotics, Masten Space, and NASA. Critical skills include:
 * - Torque-controlled drilling with adaptive feed rate
 * - Thermal management in vacuum environment
 * - Fault-tolerant operation with limited human intervention
 * - Predictive maintenance for drill bit wear
 * 
 * This module implements control loops for rotary-percussive drills used
 * in lunar ISRU systems for water ice extraction and core sampling.
 * 
 * STANDARDS:
 * - NASA-STD-8739.8 (Software Assurance and Safety)
 * - DO-178C (Airborne Software - adapted for space)
 * - ISO 13849-1 (Machine Safety)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_DRILL_CONTROL_H
#define GF_DRILL_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define DRILL_MAX_RPM               3000  /**< Maximum drill speed */
#define DRILL_MAX_TORQUE_NM         500   /**< Maximum torque limit */
#define DRILL_MAX_DEPTH_M           3.0f  /**< Maximum drilling depth */
#define DRILL_CONTROL_RATE_HZ       1000  /**< Control loop frequency */
#define DRILL_TEMP_LIMIT_C          150   /**< Maximum motor temperature */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Drill operating mode */
typedef enum {
    DRILL_MODE_IDLE,             /**< Drill stopped */
    DRILL_MODE_ROTARY,           /**< Continuous rotation */
    DRILL_MODE_PERCUSSIVE,       /**< Impact drilling */
    DRILL_MODE_ROTARY_PERCUSSIVE,/**< Combined mode */
    DRILL_MODE_RETRACT,          /**< Bit retraction */
    DRILL_MODE_CLEAN,            /**< Chip clearing */
    DRILL_MODE_FAULT             /**< Fault condition */
} drill_mode_t;

/** Drill fault codes */
typedef enum {
    DRILL_FAULT_NONE = 0,
    DRILL_FAULT_OVERTEMP,        /**< Motor overheating */
    DRILL_FAULT_OVERTORQUE,      /**< Excessive torque */
    DRILL_FAULT_STALL,           /**< Motor stalled */
    DRILL_FAULT_SENSOR,          /**< Sensor failure */
    DRILL_FAULT_MECHANICAL,      /**< Mechanical jam */
    DRILL_FAULT_POWER,           /**< Power supply issue */
    DRILL_FAULT_COMM             /**< Communication lost */
} drill_fault_t;

/** Drill control parameters */
typedef struct {
    float target_rpm;            /**< Target rotation speed */
    float target_torque_nm;      /**< Torque limit */
    float feed_rate_mm_s;        /**< Drill advance rate */
    float target_depth_m;        /**< Target depth */
    bool percussive_enable;      /**< Enable percussion */
    uint16_t percussion_hz;      /**< Percussion frequency */
} drill_params_t;

/** Drill state telemetry */
typedef struct {
    drill_mode_t mode;
    drill_fault_t fault;
    float current_rpm;
    float current_torque_nm;
    float current_depth_m;
    float motor_temp_c;
    float bit_temp_c;
    float power_w;
    uint32_t runtime_seconds;
    uint32_t timestamp;
} drill_state_t;

/** PID controller gains */
typedef struct {
    float kp;                    /**< Proportional gain */
    float ki;                    /**< Integral gain */
    float kd;                    /**< Derivative gain */
    float integral_limit;        /**< Anti-windup limit */
} drill_pid_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize the drill control subsystem
 * @return 0 on success, negative error code on failure
 */
int drill_control_init(void);

/**
 * @brief Set drill operating parameters
 * @param params Pointer to drill parameters
 * @return 0 on success, negative error code on failure
 */
int drill_set_params(const drill_params_t *params);

/**
 * @brief Start drilling operation
 * @param mode Drilling mode to use
 * @return 0 on success, negative error code on failure
 */
int drill_start(drill_mode_t mode);

/**
 * @brief Stop drilling operation
 * @return 0 on success, negative error code on failure
 */
int drill_stop(void);

/**
 * @brief Get current drill state
 * @param state Pointer to store drill state
 * @return 0 on success, negative error code on failure
 */
int drill_get_state(drill_state_t *state);

/**
 * @brief Configure PID controller gains
 * @param rpm_pid PID gains for RPM control
 * @param torque_pid PID gains for torque control
 * @return 0 on success, negative error code on failure
 */
int drill_config_pid(const drill_pid_t *rpm_pid, const drill_pid_t *torque_pid);

/**
 * @brief Clear drill fault and attempt recovery
 * @return 0 on success, negative error code on failure
 */
int drill_clear_fault(void);

/**
 * @brief Process control loop (call at DRILL_CONTROL_RATE_HZ)
 * @param timestamp_us Current timestamp in microseconds
 * @return 0 on success, negative error code on failure
 */
int drill_process(uint64_t timestamp_us);

/**
 * @brief Shutdown drill control subsystem
 */
void drill_control_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_DRILL_CONTROL_H */
