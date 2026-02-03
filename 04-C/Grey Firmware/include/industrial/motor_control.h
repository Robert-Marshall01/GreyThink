/**
 * @file motor_control.h
 * @brief PWM Motor Control Driver
 * 
 * WHAT: PWM-based motor control for DC, brushless, and stepper motors
 *       with configurable profiles and safety limits.
 * 
 * WHY: Motor control is fundamental to robotics, industrial automation,
 *      and consumer electronics. Understanding PWM generation, ramp profiles,
 *      overcurrent protection, and closed-loop control demonstrates
 *      mechatronics expertise essential for robotics roles.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Industrial: CNC machines, conveyor systems, robotic arms
 *   - Automotive: HVAC blowers, window motors, seat adjusters
 *   - Consumer: Drones, 3D printers, electric scooters
 *   - Medical: Infusion pumps, surgical robots, prosthetics
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - PWM frequency and duty cycle configuration
 *   - Ramp-up/ramp-down profiles for smooth acceleration
 *   - Current sensing and overcurrent protection
 *   - Dead-time insertion for H-bridge safety
 *   - Closed-loop speed control (PID integration points)
 */

#ifndef GF_MOTOR_CONTROL_H
#define GF_MOTOR_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_MOTOR_MAX_CHANNELS       4       /* Maximum motor channels */
#define GF_MOTOR_PWM_FREQ_HZ        20000   /* Default PWM frequency */
#define GF_MOTOR_DEAD_TIME_NS       500     /* H-bridge dead time */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_MOTOR_TYPE_DC = 0,           /* Brushed DC motor */
    GF_MOTOR_TYPE_BLDC,             /* Brushless DC (trapezoidal) */
    GF_MOTOR_TYPE_STEPPER,          /* Stepper motor */
    GF_MOTOR_TYPE_SERVO             /* RC servo */
} gf_motor_type_t;

typedef enum {
    GF_MOTOR_STATE_STOPPED = 0,
    GF_MOTOR_STATE_RUNNING,
    GF_MOTOR_STATE_BRAKING,
    GF_MOTOR_STATE_FAULT,
    GF_MOTOR_STATE_CALIBRATING
} gf_motor_state_t;

typedef enum {
    GF_MOTOR_DIR_CW = 0,            /* Clockwise */
    GF_MOTOR_DIR_CCW                /* Counter-clockwise */
} gf_motor_dir_t;

typedef struct {
    gf_motor_type_t     type;
    uint32_t            pwm_freq_hz;        /* PWM frequency */
    uint16_t            dead_time_ns;       /* Dead time for H-bridge */
    uint16_t            max_duty_pct;       /* Maximum duty cycle (0-1000 = 0-100.0%) */
    uint16_t            ramp_rate_ms;       /* Time to reach full speed */
    uint16_t            current_limit_ma;   /* Overcurrent threshold */
    bool                enable_brake;       /* Enable dynamic braking */
} gf_motor_config_t;

typedef struct {
    gf_motor_state_t    state;
    gf_motor_dir_t      direction;
    uint16_t            target_duty;        /* Target duty cycle (0-1000) */
    uint16_t            current_duty;       /* Current duty cycle */
    uint16_t            current_ma;         /* Measured current */
    int32_t             position;           /* Encoder position (if available) */
    uint32_t            fault_code;         /* Fault bits */
} gf_motor_status_t;

/* Callback for motor events */
typedef void (*gf_motor_event_cb)(uint8_t channel, gf_motor_state_t state, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize motor control channel
 * @param channel Motor channel (0 to GF_MOTOR_MAX_CHANNELS-1)
 * @param config Motor configuration
 * @return GF_OK on success
 */
int gf_motor_init(uint8_t channel, const gf_motor_config_t *config);

/**
 * @brief Set motor speed and direction
 * @param channel Motor channel
 * @param duty_pct Duty cycle (0-1000 = 0-100.0%)
 * @param direction Rotation direction
 */
int gf_motor_set_speed(uint8_t channel, uint16_t duty_pct, gf_motor_dir_t direction);

/**
 * @brief Stop motor (with optional braking)
 */
int gf_motor_stop(uint8_t channel, bool brake);

/**
 * @brief Emergency stop all motors
 */
void gf_motor_emergency_stop(void);

/**
 * @brief Get motor status
 */
void gf_motor_get_status(uint8_t channel, gf_motor_status_t *status);

/**
 * @brief Clear fault condition
 */
int gf_motor_clear_fault(uint8_t channel);

/**
 * @brief Register event callback
 */
void gf_motor_set_event_callback(gf_motor_event_cb callback, void *ctx);

/**
 * @brief Get driver descriptor for driver registry
 */
const void* gf_motor_get_driver(void);

#endif /* GF_MOTOR_CONTROL_H */
