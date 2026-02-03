/**
 * @file abs_control.h
 * @brief Anti-lock Braking System Control Loop
 *
 * INDUSTRY RELEVANCE:
 * ABS is a foundational automotive safety system requiring:
 * - Real-time wheel speed sensing and slip calculation
 * - PID or model-predictive control for brake modulation
 * - Sub-10ms control loop timing (typically 1-5ms)
 * - Fault detection and graceful degradation
 * - ASIL-C/D compliance per ISO 26262
 *
 * Used in: Vehicle braking systems, ESC, traction control
 *
 * @note This is a stub demonstrating automotive control system patterns.
 */

#ifndef GF_ABS_CONTROL_H
#define GF_ABS_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* ABS System Definitions                                                    */
/*===========================================================================*/

/**
 * @brief Number of wheels in system
 */
#define GF_ABS_WHEEL_COUNT  4

/**
 * @brief Wheel identifiers
 */
typedef enum {
    GF_WHEEL_FL = 0,            /**< Front left */
    GF_WHEEL_FR = 1,            /**< Front right */
    GF_WHEEL_RL = 2,            /**< Rear left */
    GF_WHEEL_RR = 3             /**< Rear right */
} gf_wheel_id_t;

/**
 * @brief ABS operating modes
 */
typedef enum {
    GF_ABS_MODE_OFF,            /**< ABS disabled */
    GF_ABS_MODE_STANDBY,        /**< Ready, not intervening */
    GF_ABS_MODE_ACTIVE,         /**< Actively controlling */
    GF_ABS_MODE_FAULT           /**< Fault detected, degraded */
} gf_abs_mode_t;

/**
 * @brief Brake valve states
 */
typedef enum {
    GF_VALVE_APPLY,             /**< Increase pressure */
    GF_VALVE_HOLD,              /**< Maintain pressure */
    GF_VALVE_RELEASE            /**< Decrease pressure */
} gf_valve_state_t;

/**
 * @brief Wheel speed sensor data
 */
typedef struct {
    uint16_t speed_rpm;         /**< Wheel speed in RPM * 10 */
    uint16_t pulse_count;       /**< Encoder pulses this cycle */
    int16_t acceleration;       /**< Wheel acceleration (rpm/s) */
    bool valid;                 /**< Sensor data valid */
    bool stall_detected;        /**< Wheel stall/lock detected */
} gf_wheel_speed_t;

/**
 * @brief Vehicle state information
 */
typedef struct {
    uint16_t vehicle_speed_kmh; /**< Vehicle speed km/h * 10 */
    int16_t yaw_rate;           /**< Yaw rate deg/s * 10 */
    int16_t lateral_accel;      /**< Lateral g * 100 */
    int16_t longitudinal_accel; /**< Longitudinal g * 100 */
    bool brake_applied;         /**< Brake pedal pressed */
    uint16_t brake_pressure;    /**< Master cylinder pressure (bar * 10) */
} gf_vehicle_state_t;

/**
 * @brief Per-wheel ABS control state
 */
typedef struct {
    gf_wheel_id_t wheel;
    gf_valve_state_t valve_cmd;     /**< Current valve command */
    uint8_t slip_percent;           /**< Wheel slip 0-100% */
    int16_t target_slip;            /**< Target slip point */
    int16_t pid_output;             /**< Controller output */
    uint8_t pressure_cycles;        /**< Pressure cycle count */
    bool intervention_active;       /**< ABS intervening */
} gf_wheel_control_t;

/**
 * @brief ABS controller configuration
 */
typedef struct {
    /* Control parameters */
    uint16_t loop_period_us;        /**< Control loop period */
    uint8_t target_slip_percent;    /**< Target slip (10-20%) */
    uint8_t slip_threshold;         /**< Intervention threshold */
    
    /* PID gains */
    int16_t kp;                     /**< Proportional gain * 100 */
    int16_t ki;                     /**< Integral gain * 100 */
    int16_t kd;                     /**< Derivative gain * 100 */
    
    /* Limits */
    uint8_t max_pressure_cycles;    /**< Max cycles per stop */
    uint16_t min_speed_kmh;         /**< Min activation speed */
    uint16_t pump_duty_max;         /**< Max pump duty cycle % */
    
    /* Tire parameters */
    uint16_t tire_circumference_mm; /**< Tire circumference */
    uint8_t wheel_teeth;            /**< Encoder teeth count */
} gf_abs_config_t;

/**
 * @brief ABS diagnostic status
 */
typedef struct {
    gf_abs_mode_t mode;
    bool sensor_fault[GF_ABS_WHEEL_COUNT];
    bool valve_fault[GF_ABS_WHEEL_COUNT];
    bool pump_fault;
    bool ecu_fault;
    uint8_t dtc_count;              /**< Diagnostic trouble codes */
    uint32_t interventions;         /**< Total interventions */
    uint32_t uptime_ms;
} gf_abs_status_t;

/**
 * @brief ABS controller handle
 */
typedef struct gf_abs_controller* gf_abs_handle_t;

/*===========================================================================*/
/* ABS Control API                                                           */
/*===========================================================================*/

/**
 * @brief Initialize ABS controller
 * @param config Configuration parameters
 * @param handle Output handle
 * @return 0 on success
 */
int gf_abs_init(const gf_abs_config_t* config, gf_abs_handle_t* handle);

/**
 * @brief Enable ABS system
 * @param handle Controller handle
 * @return 0 on success
 */
int gf_abs_enable(gf_abs_handle_t handle);

/**
 * @brief Disable ABS system
 * @param handle Controller handle
 * @return 0 on success
 */
int gf_abs_disable(gf_abs_handle_t handle);

/**
 * @brief Update wheel speed sensor input
 * @param handle Controller handle
 * @param wheel Wheel identifier
 * @param speed Wheel speed data
 * @return 0 on success
 */
int gf_abs_update_wheel_speed(gf_abs_handle_t handle,
                               gf_wheel_id_t wheel,
                               const gf_wheel_speed_t* speed);

/**
 * @brief Update vehicle state
 * @param handle Controller handle
 * @param state Vehicle state
 * @return 0 on success
 */
int gf_abs_update_vehicle_state(gf_abs_handle_t handle,
                                 const gf_vehicle_state_t* state);

/**
 * @brief Execute control loop iteration
 * @param handle Controller handle
 * @param controls Output: valve commands for each wheel
 * @return 0 on success, >0 if intervention active
 */
int gf_abs_control_step(gf_abs_handle_t handle,
                        gf_wheel_control_t controls[GF_ABS_WHEEL_COUNT]);

/**
 * @brief Get current ABS status
 * @param handle Controller handle
 * @param status Output status
 * @return 0 on success
 */
int gf_abs_get_status(gf_abs_handle_t handle, gf_abs_status_t* status);

/**
 * @brief Run ABS self-test
 * @param handle Controller handle
 * @return 0 on pass
 */
int gf_abs_self_test(gf_abs_handle_t handle);

/**
 * @brief Clear diagnostic trouble codes
 * @param handle Controller handle
 * @return 0 on success
 */
int gf_abs_clear_dtc(gf_abs_handle_t handle);

/**
 * @brief Deinitialize ABS controller
 * @param handle Controller handle
 */
void gf_abs_deinit(gf_abs_handle_t handle);

/*===========================================================================*/
/* Wheel Speed Estimation Helpers                                            */
/*===========================================================================*/

/**
 * @brief Calculate vehicle reference speed
 * @param speeds Array of wheel speeds
 * @return Estimated vehicle speed
 */
uint16_t gf_abs_calc_vehicle_speed(const gf_wheel_speed_t speeds[GF_ABS_WHEEL_COUNT]);

/**
 * @brief Calculate wheel slip percentage
 * @param wheel_speed Wheel speed
 * @param vehicle_speed Vehicle reference speed
 * @return Slip percentage 0-100
 */
uint8_t gf_abs_calc_slip(uint16_t wheel_speed, uint16_t vehicle_speed);

#ifdef __cplusplus
}
#endif

#endif /* GF_ABS_CONTROL_H */
