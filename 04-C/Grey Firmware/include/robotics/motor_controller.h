/**
 * @file motor_controller.h
 * @brief Multi-Axis Motor Controller for Robotics
 * 
 * INDUSTRY RELEVANCE:
 * Industrial and collaborative robots require precise, coordinated motion
 * control across multiple actuators. This module demonstrates:
 * - Field-Oriented Control (FOC) for BLDC/PMSM motors
 * - Multi-axis trajectory planning and interpolation
 * - Torque, velocity, and position control modes
 * - Real-time EtherCAT/CANopen motor bus integration
 * 
 * Applications: Industrial robots, CNC machines, 3D printers, drones,
 *               exoskeletons, automated guided vehicles
 * Standards: IEC 61800-7 (CiA 402), EtherCAT, SERCOS III
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_MOTOR_CONTROLLER_H
#define GF_MOTOR_CONTROLLER_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Motor Controller Types
 ******************************************************************************/

/** Motor type */
typedef enum {
    GF_MOTOR_BLDC,            /**< Brushless DC */
    GF_MOTOR_PMSM,            /**< Permanent Magnet Synchronous */
    GF_MOTOR_STEPPER,         /**< Stepper motor */
    GF_MOTOR_DC_BRUSH,        /**< Brushed DC */
    GF_MOTOR_LINEAR           /**< Linear actuator */
} gf_motor_type_t;

/** Control mode */
typedef enum {
    GF_CTRL_TORQUE,           /**< Torque/current control */
    GF_CTRL_VELOCITY,         /**< Speed control */
    GF_CTRL_POSITION,         /**< Position control */
    GF_CTRL_PROFILE_POSITION, /**< Profiled positioning */
    GF_CTRL_HOMING            /**< Homing sequence */
} gf_control_mode_t;

/** Axis state (CiA 402 state machine) */
typedef enum {
    GF_AXIS_NOT_READY,
    GF_AXIS_SWITCH_ON_DISABLED,
    GF_AXIS_READY_TO_SWITCH_ON,
    GF_AXIS_SWITCHED_ON,
    GF_AXIS_OPERATION_ENABLED,
    GF_AXIS_QUICK_STOP_ACTIVE,
    GF_AXIS_FAULT_REACTION,
    GF_AXIS_FAULT
} gf_axis_state_t;

/** Homing method */
typedef enum {
    GF_HOME_LIMIT_SWITCH,
    GF_HOME_INDEX_PULSE,
    GF_HOME_CURRENT_THRESHOLD,
    GF_HOME_HARD_STOP,
    GF_HOME_ABSOLUTE_ENCODER
} gf_homing_method_t;

/*******************************************************************************
 * Motor Controller Configuration
 ******************************************************************************/

/** Motor parameters */
typedef struct {
    gf_motor_type_t type;
    uint8_t pole_pairs;           /**< Motor pole pairs */
    uint16_t rated_current_ma;    /**< Rated current */
    uint16_t peak_current_ma;     /**< Peak current limit */
    uint16_t rated_rpm;           /**< Rated speed */
    uint16_t encoder_resolution;  /**< Encoder counts/rev */
    uint16_t kt_mn_a;             /**< Torque constant (mNm/A) */
    uint16_t ke_mv_rad_s;         /**< Back-EMF constant */
    float inductance_uh;          /**< Phase inductance */
    float resistance_mohm;        /**< Phase resistance */
} gf_motor_params_t;

/** PID gains */
typedef struct {
    float kp;
    float ki;
    float kd;
    float integral_limit;
    float output_limit;
} gf_pid_gains_t;

/** Axis configuration */
typedef struct {
    uint8_t axis_id;
    char name[16];
    gf_motor_params_t motor;
    gf_pid_gains_t current_pid;   /**< Current/torque loop */
    gf_pid_gains_t velocity_pid;  /**< Velocity loop */
    gf_pid_gains_t position_pid;  /**< Position loop */
    int32_t min_position;         /**< Software limit (encoder counts) */
    int32_t max_position;
    uint32_t max_velocity;        /**< counts/s */
    uint32_t max_acceleration;    /**< counts/s² */
    gf_homing_method_t home_method;
    int32_t home_offset;
} gf_axis_config_t;

/** Motion profile */
typedef struct {
    int32_t target_position;      /**< Target position (counts) */
    uint32_t velocity;            /**< Profile velocity */
    uint32_t acceleration;        /**< Acceleration */
    uint32_t deceleration;        /**< Deceleration */
    bool relative;                /**< Relative move */
    bool immediate;               /**< Start immediately */
} gf_motion_profile_t;

/** Axis status */
typedef struct {
    gf_axis_state_t state;
    gf_control_mode_t mode;
    int32_t actual_position;      /**< Current position */
    int32_t target_position;
    int32_t actual_velocity;      /**< counts/s */
    int16_t actual_torque;        /**< Per-mille of rated */
    int16_t actual_current_ma;
    uint16_t temperature_c;
    bool in_motion;
    bool target_reached;
    bool home_found;
    uint16_t fault_code;
} gf_axis_status_t;

/*******************************************************************************
 * Motor Controller Statistics
 ******************************************************************************/

typedef struct {
    uint32_t moves_completed;
    uint32_t moves_aborted;
    uint32_t faults_count;
    uint64_t total_distance;      /**< Total encoder counts traveled */
    uint32_t following_errors;
    uint32_t overcurrent_events;
    uint32_t overtemp_events;
    float avg_cycle_time_us;
} gf_axis_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize motor controller
 * @param config Axis configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_init(const gf_axis_config_t *config);

/**
 * @brief Enable axis
 * @param axis_id Axis to enable
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_enable(uint8_t axis_id);

/**
 * @brief Disable axis
 * @param axis_id Axis to disable
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_disable(uint8_t axis_id);

/**
 * @brief Set control mode
 * @param axis_id Axis
 * @param mode Control mode
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_set_mode(uint8_t axis_id, gf_control_mode_t mode);

/**
 * @brief Execute motion profile
 * @param axis_id Axis
 * @param profile Motion profile
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_move(uint8_t axis_id, const gf_motion_profile_t *profile);

/**
 * @brief Set velocity setpoint (velocity mode)
 * @param axis_id Axis
 * @param velocity Target velocity (counts/s)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_set_velocity(uint8_t axis_id, int32_t velocity);

/**
 * @brief Set torque setpoint (torque mode)
 * @param axis_id Axis
 * @param torque Target torque (per-mille of rated)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_set_torque(uint8_t axis_id, int16_t torque);

/**
 * @brief Emergency stop
 * @param axis_id Axis (0xFF for all)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_quick_stop(uint8_t axis_id);

/**
 * @brief Start homing sequence
 * @param axis_id Axis
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_home(uint8_t axis_id);

/**
 * @brief Get axis status
 * @param axis_id Axis
 * @param status Output status
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_get_status(uint8_t axis_id, gf_axis_status_t *status);

/**
 * @brief Clear fault
 * @param axis_id Axis
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_clear_fault(uint8_t axis_id);

/**
 * @brief Run control loop iteration
 * @param axis_id Axis
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_control_loop(uint8_t axis_id);

/**
 * @brief Get axis statistics
 * @param axis_id Axis
 * @return Current statistics
 */
gf_axis_stats_t gf_motor_get_stats(uint8_t axis_id);

/**
 * @brief Shutdown motor controller
 * @param axis_id Axis
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_motor_shutdown(uint8_t axis_id);

#ifdef __cplusplus
}
#endif

#endif /* GF_MOTOR_CONTROLLER_H */
