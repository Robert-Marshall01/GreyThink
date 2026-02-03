/**
 * @file flight_control.h
 * @brief Drone Flight Control System Interface
 *
 * INDUSTRY RELEVANCE:
 * UAV flight control systems are the heart of drone operations, encompassing
 * safety-critical real-time control. This module demonstrates:
 * - PID-based attitude and rate control loops
 * - Cascaded control architecture (position → velocity → attitude → rate)
 * - Safety interlocks (geofencing, altitude limits, battery failsafe)
 * - Real-time control at 400Hz+ loop rates
 *
 * These skills apply to leading drone companies (DJI, Skydio, Autel, Parrot),
 * defense contractors (Northrop Grumman, General Atomics), and open-source
 * flight stacks (PX4, ArduPilot, Betaflight). Also relevant for eVTOL
 * development (Joby, Lilium, Archer).
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires rigorous testing and certification.
 */

#ifndef GF_FLIGHT_CONTROL_H
#define GF_FLIGHT_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_FC_MAX_MOTORS            8       /**< Maximum motor outputs */
#define GF_FC_CONTROL_RATE_HZ       400     /**< Control loop rate */
#define GF_FC_MAX_WAYPOINTS         100     /**< Maximum mission waypoints */
#define GF_FC_DEFAULT_GEOFENCE_M    500.0f  /**< Default geofence radius */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Flight mode
 */
typedef enum {
    GF_FC_MODE_DISARMED,        /**< Motors disarmed (safe) */
    GF_FC_MODE_STABILIZE,       /**< Self-level only */
    GF_FC_MODE_ALT_HOLD,        /**< Altitude hold */
    GF_FC_MODE_POS_HOLD,        /**< Position hold (GPS) */
    GF_FC_MODE_LOITER,          /**< Loiter at position */
    GF_FC_MODE_RTL,             /**< Return to launch */
    GF_FC_MODE_AUTO,            /**< Autonomous mission */
    GF_FC_MODE_GUIDED,          /**< Guided by external system */
    GF_FC_MODE_ACRO,            /**< Acrobatic (rate mode) */
    GF_FC_MODE_LAND,            /**< Autonomous landing */
    GF_FC_MODE_EMERGENCY        /**< Emergency shutdown */
} gf_fc_mode_t;

/**
 * @brief Arm state
 */
typedef enum {
    GF_FC_DISARMED,             /**< Motors off, safe to handle */
    GF_FC_ARMING,               /**< Arming sequence in progress */
    GF_FC_ARMED,                /**< Armed, motors active */
    GF_FC_DISARMING             /**< Disarming sequence in progress */
} gf_fc_arm_state_t;

/**
 * @brief Safety flags (bitmask)
 */
typedef enum {
    GF_FC_SAFETY_GEOFENCE       = 0x0001,   /**< Geofence breach */
    GF_FC_SAFETY_ALTITUDE       = 0x0002,   /**< Altitude limit breach */
    GF_FC_SAFETY_BATTERY_LOW    = 0x0004,   /**< Battery low warning */
    GF_FC_SAFETY_BATTERY_CRIT   = 0x0008,   /**< Battery critical */
    GF_FC_SAFETY_GPS_LOST       = 0x0010,   /**< GPS signal lost */
    GF_FC_SAFETY_RC_LOST        = 0x0020,   /**< RC link lost */
    GF_FC_SAFETY_MOTOR_FAULT    = 0x0040,   /**< Motor fault detected */
    GF_FC_SAFETY_SENSOR_FAULT   = 0x0080,   /**< Sensor fault detected */
    GF_FC_SAFETY_CRASH          = 0x0100,   /**< Crash detected */
    GF_FC_SAFETY_NO_FLY_ZONE    = 0x0200    /**< In restricted airspace */
} gf_fc_safety_flag_t;

/**
 * @brief Euler angles (aerospace convention)
 */
typedef struct {
    float roll;                 /**< Roll angle (radians) */
    float pitch;                /**< Pitch angle (radians) */
    float yaw;                  /**< Yaw angle (radians) */
} gf_fc_euler_t;

/**
 * @brief Angular rates
 */
typedef struct {
    float roll_rate;            /**< Roll rate (rad/s) */
    float pitch_rate;           /**< Pitch rate (rad/s) */
    float yaw_rate;             /**< Yaw rate (rad/s) */
} gf_fc_rates_t;

/**
 * @brief 3D position (NED frame)
 */
typedef struct {
    float north;                /**< North position (meters) */
    float east;                 /**< East position (meters) */
    float down;                 /**< Down position (meters, negative = up) */
} gf_fc_position_t;

/**
 * @brief 3D velocity (NED frame)
 */
typedef struct {
    float vn;                   /**< North velocity (m/s) */
    float ve;                   /**< East velocity (m/s) */
    float vd;                   /**< Down velocity (m/s) */
} gf_fc_velocity_t;

/**
 * @brief Geographic position
 */
typedef struct {
    double latitude;            /**< Latitude (degrees) */
    double longitude;           /**< Longitude (degrees) */
    float altitude_msl;         /**< Altitude MSL (meters) */
    float altitude_agl;         /**< Altitude AGL (meters) */
} gf_fc_gps_pos_t;

/**
 * @brief PID controller gains
 */
typedef struct {
    float kp;                   /**< Proportional gain */
    float ki;                   /**< Integral gain */
    float kd;                   /**< Derivative gain */
    float ff;                   /**< Feedforward gain */
    float imax;                 /**< Integral windup limit */
    float output_min;           /**< Minimum output */
    float output_max;           /**< Maximum output */
} gf_fc_pid_gains_t;

/**
 * @brief Control setpoints
 */
typedef struct {
    gf_fc_euler_t attitude;     /**< Desired attitude */
    gf_fc_rates_t rates;        /**< Desired angular rates */
    float throttle;             /**< Desired throttle (0-1) */
    float altitude;             /**< Desired altitude (m) */
    gf_fc_position_t position;  /**< Desired position (NED) */
    gf_fc_velocity_t velocity;  /**< Desired velocity (NED) */
} gf_fc_setpoint_t;

/**
 * @brief Motor output configuration
 */
typedef struct {
    uint8_t num_motors;         /**< Number of motors */
    float motor_outputs[GF_FC_MAX_MOTORS]; /**< Motor commands (0-1) */
    uint16_t motor_pwm[GF_FC_MAX_MOTORS];  /**< PWM values */
} gf_fc_motors_t;

/**
 * @brief Geofence configuration
 */
typedef struct {
    bool enabled;               /**< Geofence enabled */
    gf_fc_gps_pos_t home;       /**< Home/center position */
    float radius_m;             /**< Horizontal radius */
    float max_altitude_m;       /**< Maximum altitude AGL */
    float min_altitude_m;       /**< Minimum altitude AGL */
    uint8_t breach_action;      /**< Action on breach (RTL, land, hover) */
} gf_fc_geofence_t;

/**
 * @brief Flight controller state
 */
typedef struct {
    gf_fc_mode_t mode;          /**< Current flight mode */
    gf_fc_arm_state_t arm_state;/**< Arm state */
    uint16_t safety_flags;      /**< Active safety flags */
    gf_fc_euler_t attitude;     /**< Current attitude */
    gf_fc_rates_t rates;        /**< Current angular rates */
    gf_fc_position_t position;  /**< Current position (NED) */
    gf_fc_velocity_t velocity;  /**< Current velocity (NED) */
    gf_fc_gps_pos_t gps;        /**< Current GPS position */
    float battery_voltage;      /**< Battery voltage */
    float battery_percent;      /**< Battery percentage */
    uint32_t flight_time_s;     /**< Flight time in seconds */
    gf_fc_motors_t motors;      /**< Motor outputs */
} gf_fc_state_t;

/**
 * @brief Mission waypoint
 */
typedef struct {
    uint16_t seq;               /**< Sequence number */
    uint8_t command;            /**< Waypoint command (navigate, land, etc.) */
    gf_fc_gps_pos_t location;   /**< Waypoint location */
    float param1;               /**< Command parameter 1 (hold time, etc.) */
    float param2;               /**< Command parameter 2 (radius, etc.) */
    float speed;                /**< Approach speed (m/s) */
} gf_fc_waypoint_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_fc_safety_cb_t)(uint16_t safety_flags, void* user_data);
typedef void (*gf_fc_mode_change_cb_t)(gf_fc_mode_t old_mode, gf_fc_mode_t new_mode, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize flight control system
 * @return 0 on success, negative error code on failure
 */
int gf_fc_init(void);

/**
 * @brief Shutdown flight control system
 */
void gf_fc_deinit(void);

/**
 * @brief Arm motors
 * @return 0 on success, negative if arming checks fail
 */
int gf_fc_arm(void);

/**
 * @brief Disarm motors
 * @param force Force disarm even in flight (emergency)
 * @return 0 on success
 */
int gf_fc_disarm(bool force);

/**
 * @brief Set flight mode
 * @param mode Desired flight mode
 * @return 0 on success, negative if mode unavailable
 */
int gf_fc_set_mode(gf_fc_mode_t mode);

/**
 * @brief Run control loop iteration (call at GF_FC_CONTROL_RATE_HZ)
 * @param dt Time step in seconds
 */
void gf_fc_update(float dt);

/**
 * @brief Set attitude setpoint
 * @param attitude Desired attitude (roll, pitch, yaw)
 * @param throttle Desired throttle (0-1)
 */
void gf_fc_set_attitude(const gf_fc_euler_t* attitude, float throttle);

/**
 * @brief Set position setpoint
 * @param position Desired position (NED frame)
 * @param yaw Desired yaw angle
 */
void gf_fc_set_position(const gf_fc_position_t* position, float yaw);

/**
 * @brief Set velocity setpoint
 * @param velocity Desired velocity (NED frame)
 * @param yaw_rate Desired yaw rate
 */
void gf_fc_set_velocity(const gf_fc_velocity_t* velocity, float yaw_rate);

/**
 * @brief Configure PID gains for rate loop
 * @param axis Axis (0=roll, 1=pitch, 2=yaw)
 * @param gains PID gains
 */
void gf_fc_set_rate_pid(uint8_t axis, const gf_fc_pid_gains_t* gains);

/**
 * @brief Configure PID gains for attitude loop
 * @param axis Axis (0=roll, 1=pitch, 2=yaw)
 * @param gains PID gains
 */
void gf_fc_set_attitude_pid(uint8_t axis, const gf_fc_pid_gains_t* gains);

/**
 * @brief Configure geofence
 * @param geofence Geofence configuration
 */
void gf_fc_set_geofence(const gf_fc_geofence_t* geofence);

/**
 * @brief Set home position
 * @param home Home GPS position
 */
void gf_fc_set_home(const gf_fc_gps_pos_t* home);

/**
 * @brief Get current flight state
 * @param[out] state Output state structure
 */
void gf_fc_get_state(gf_fc_state_t* state);

/**
 * @brief Upload mission waypoints
 * @param waypoints Waypoint array
 * @param count Number of waypoints
 * @return 0 on success
 */
int gf_fc_upload_mission(const gf_fc_waypoint_t* waypoints, uint16_t count);

/**
 * @brief Start mission execution
 * @return 0 on success
 */
int gf_fc_start_mission(void);

/**
 * @brief Pause mission
 */
void gf_fc_pause_mission(void);

/**
 * @brief Resume mission
 */
void gf_fc_resume_mission(void);

/**
 * @brief Set safety callback
 * @param callback Safety event callback
 * @param user_data User context
 */
void gf_fc_set_safety_callback(gf_fc_safety_cb_t callback, void* user_data);

/**
 * @brief Set mode change callback
 * @param callback Mode change callback
 * @param user_data User context
 */
void gf_fc_set_mode_callback(gf_fc_mode_change_cb_t callback, void* user_data);

/**
 * @brief Run pre-flight checks
 * @return 0 if all checks pass, bitmask of failed checks otherwise
 */
int gf_fc_preflight_check(void);

/**
 * @brief Emergency motor stop
 */
void gf_fc_emergency_stop(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_FLIGHT_CONTROL_H */
