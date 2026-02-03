/**
 * @file prosthetic_actuator.h
 * @brief Prosthetic Limb Actuator Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Modern prosthetic limbs use embedded systems for:
 * - Motor control (brushless DC, linear actuators)
 * - Force sensing and haptic feedback
 * - Pattern recognition from EMG/EEG signals
 * - Low-power operation for battery life
 * - Real-time response (<10ms latency)
 * 
 * Must comply with IEC 62304 (medical device software)
 * Companies: Ottobock, Ossur, Open Bionics, Mobius Bionics
 */

#ifndef GF_PROSTHETIC_ACTUATOR_H
#define GF_PROSTHETIC_ACTUATOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_PROSTHETIC_MAX_JOINTS       7     /* e.g., 5 fingers + wrist + elbow */
#define GF_PROSTHETIC_CONTROL_RATE_HZ  1000  /* 1kHz control loop */
#define GF_PROSTHETIC_MAX_GRIP_N       40    /* Max grip force */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_PROSTHETIC_OK = 0,
    GF_PROSTHETIC_ERROR_NULL_PTR,
    GF_PROSTHETIC_ERROR_NOT_INITIALIZED,
    GF_PROSTHETIC_ERROR_JOINT_LIMIT,
    GF_PROSTHETIC_ERROR_OVERCURRENT,
    GF_PROSTHETIC_ERROR_OVERTEMP,
    GF_PROSTHETIC_ERROR_FORCE_LIMIT,
    GF_PROSTHETIC_ERROR_MOTOR_FAULT,
    GF_PROSTHETIC_ERROR_SENSOR_FAULT,
    GF_PROSTHETIC_ERROR_LOW_BATTERY,
    GF_PROSTHETIC_WARN_HIGH_LOAD
} gf_prosthetic_status_t;

typedef enum {
    GF_ACTUATOR_BLDC,           /* Brushless DC motor */
    GF_ACTUATOR_LINEAR,         /* Linear actuator */
    GF_ACTUATOR_SERVO,          /* Hobby servo */
    GF_ACTUATOR_SMA             /* Shape memory alloy */
} gf_actuator_type_t;

typedef enum {
    GF_JOINT_FINGER_THUMB,
    GF_JOINT_FINGER_INDEX,
    GF_JOINT_FINGER_MIDDLE,
    GF_JOINT_FINGER_RING,
    GF_JOINT_FINGER_PINKY,
    GF_JOINT_WRIST_ROTATE,
    GF_JOINT_ELBOW_FLEX
} gf_prosthetic_joint_t;

typedef enum {
    GF_GRIP_NONE,
    GF_GRIP_POWER,              /* Full hand grip */
    GF_GRIP_PRECISION,          /* Thumb-index pinch */
    GF_GRIP_LATERAL,            /* Key grip */
    GF_GRIP_TRIPOD,             /* Three-finger grip */
    GF_GRIP_HOOK,               /* Hook grip */
    GF_GRIP_POINTER             /* Point with index */
} gf_grip_pattern_t;

/**
 * @brief Actuator configuration
 */
typedef struct {
    gf_prosthetic_joint_t joint;
    gf_actuator_type_t type;
    float gear_ratio;
    float max_speed_dps;        /* Degrees per second */
    float max_torque_nm;
    float min_position_deg;
    float max_position_deg;
    uint16_t encoder_ppr;       /* Pulses per revolution */
    bool has_force_sensor;
} gf_prosthetic_actuator_config_t;

/**
 * @brief Joint state
 */
typedef struct {
    gf_prosthetic_joint_t joint;
    float position_deg;
    float velocity_dps;
    float torque_nm;
    float force_n;              /* Contact force */
    float current_ma;
    float temperature_c;
    bool at_limit;
    bool in_contact;
} gf_prosthetic_joint_state_t;

/**
 * @brief Motion command
 */
typedef struct {
    gf_prosthetic_joint_t joint;
    float target_position_deg;
    float max_velocity_dps;
    float max_force_n;
    uint32_t duration_ms;
} gf_prosthetic_motion_cmd_t;

/**
 * @brief Grip execution parameters
 */
typedef struct {
    gf_grip_pattern_t pattern;
    float speed_pct;            /* 0-100% of max speed */
    float force_pct;            /* 0-100% of max force */
    float aperture_pct;         /* 0-100% open */
} gf_prosthetic_grip_cmd_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_prosthetic_contact_cb_t)(gf_prosthetic_joint_t joint,
                                            float force_n,
                                            void* user_data);

typedef void (*gf_prosthetic_fault_cb_t)(gf_prosthetic_joint_t joint,
                                          gf_prosthetic_status_t fault,
                                          void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_prosthetic_status_t gf_prosthetic_init(void);
void gf_prosthetic_shutdown(void);

/* Configuration */
gf_prosthetic_status_t gf_prosthetic_add_actuator(const gf_prosthetic_actuator_config_t* config);
gf_prosthetic_status_t gf_prosthetic_calibrate(gf_prosthetic_joint_t joint);

/* Control */
gf_prosthetic_status_t gf_prosthetic_enable(bool enable);
gf_prosthetic_status_t gf_prosthetic_move_joint(const gf_prosthetic_motion_cmd_t* cmd);
gf_prosthetic_status_t gf_prosthetic_set_grip(const gf_prosthetic_grip_cmd_t* cmd);
gf_prosthetic_status_t gf_prosthetic_stop(gf_prosthetic_joint_t joint);
gf_prosthetic_status_t gf_prosthetic_stop_all(void);

/* State */
gf_prosthetic_status_t gf_prosthetic_get_joint_state(gf_prosthetic_joint_t joint,
                                                      gf_prosthetic_joint_state_t* state);
bool gf_prosthetic_is_moving(gf_prosthetic_joint_t joint);
float gf_prosthetic_get_battery_pct(void);

/* Callbacks */
gf_prosthetic_status_t gf_prosthetic_register_contact_callback(gf_prosthetic_contact_cb_t cb,
                                                                 void* user_data);
gf_prosthetic_status_t gf_prosthetic_register_fault_callback(gf_prosthetic_fault_cb_t cb,
                                                               void* user_data);

/* Periodic processing */
gf_prosthetic_status_t gf_prosthetic_process(void);

#endif /* GF_PROSTHETIC_ACTUATOR_H */
