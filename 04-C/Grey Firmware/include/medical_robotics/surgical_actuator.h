/**
 * @file surgical_actuator.h
 * @brief Surgical Robot Actuator Driver
 * 
 * INDUSTRY RELEVANCE:
 * Surgical robotics (da Vinci, Hugo RAS, Medtronic) requires sub-millimeter
 * precision with force-limited safety. This driver demonstrates haptic feedback,
 * tremor filtering, and FDA 21 CFR Part 11 compliance for Class III medical devices.
 * Critical for minimally invasive surgery and interventional procedures.
 * 
 * Key applications:
 * - da Vinci-style teleoperated surgery
 * - Orthopedic surgical robots (Mako, ROSA)
 * - Neurosurgical frame systems
 * - Catheter/endoscope navigation
 * - Radiation therapy positioning
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_SURGICAL_ACTUATOR_H
#define GF_SURGICAL_ACTUATOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define SURG_MAX_AXES               8       /**< Max controlled axes */
#define SURG_POSITION_RESOLUTION    0.001f  /**< Position resolution (mm) */
#define SURG_FORCE_LIMIT_N          10.0f   /**< Max force limit (N) */
#define SURG_TREMOR_FILTER_HZ       12.0f   /**< Tremor filter cutoff */
#define SURG_CONTROL_RATE_HZ        1000    /**< Control loop rate */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Actuator type
 */
typedef enum {
    SURG_ACTUATOR_LINEAR = 0,       /**< Linear actuator */
    SURG_ACTUATOR_ROTARY,           /**< Rotary joint */
    SURG_ACTUATOR_CABLE_DRIVEN,     /**< Cable-driven mechanism */
    SURG_ACTUATOR_PNEUMATIC         /**< Pneumatic actuator */
} surg_actuator_type_t;

/**
 * @brief Operating mode
 */
typedef enum {
    SURG_MODE_DISABLED = 0,
    SURG_MODE_HOMING,               /**< Finding home position */
    SURG_MODE_READY,                /**< Ready for commands */
    SURG_MODE_TELEOP,               /**< Teleoperated control */
    SURG_MODE_AUTONOMOUS,           /**< Autonomous motion */
    SURG_MODE_FORCE_LIMIT,          /**< Force limit active */
    SURG_MODE_FAULT                 /**< Fault condition */
} surg_mode_t;

/**
 * @brief Safety status flags
 */
typedef struct {
    bool e_stop_pressed;
    bool force_limit_active;
    bool workspace_boundary;
    bool watchdog_ok;
    bool encoder_fault;
    bool communication_ok;
    bool calibration_valid;
} surg_safety_status_t;

/**
 * @brief Axis state
 */
typedef struct {
    float position;                 /**< Current position (mm or deg) */
    float velocity;                 /**< Current velocity */
    float force;                    /**< Measured force (N or Nm) */
    float target_position;          /**< Commanded position */
    bool in_position;               /**< At target position */
    bool limit_active;              /**< Force/position limit active */
} surg_axis_state_t;

/**
 * @brief Actuator configuration
 */
typedef struct {
    surg_actuator_type_t type;
    float stroke_min;               /**< Minimum position */
    float stroke_max;               /**< Maximum position */
    float max_velocity;             /**< Maximum velocity */
    float max_acceleration;         /**< Maximum acceleration */
    float force_limit;              /**< Force limit (N) */
    bool enable_tremor_filter;
    bool enable_haptic_feedback;
} surg_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize surgical actuator
 * @param config Actuator configurations per axis
 * @param axis_count Number of axes
 * @return 0 on success, negative on error
 */
int surg_actuator_init(const surg_config_t* config, uint8_t axis_count);

/**
 * @brief Shutdown actuators safely
 * @return 0 on success, negative on error
 */
int surg_actuator_shutdown(void);

/**
 * @brief Home all axes
 * @return 0 on success, negative on error
 */
int surg_actuator_home(void);

/**
 * @brief Move to position
 * @param axis Axis index
 * @param position Target position
 * @param velocity Max velocity
 * @return 0 on success, negative on error
 */
int surg_actuator_move(uint8_t axis, float position, float velocity);

/**
 * @brief Set teleop input (from master device)
 * @param axis Axis index
 * @param master_position Master position input
 * @param master_force Master force input
 * @return Haptic force feedback (N)
 */
float surg_actuator_teleop(uint8_t axis, float master_position, 
                           float master_force);

/**
 * @brief Get axis state
 * @param axis Axis index
 * @param state Output state
 * @return 0 on success, negative on error
 */
int surg_actuator_get_state(uint8_t axis, surg_axis_state_t* state);

/**
 * @brief Get safety status
 * @param status Output safety status
 * @return 0 on success, negative on error
 */
int surg_actuator_get_safety(surg_safety_status_t* status);

/**
 * @brief Emergency stop
 * @return 0 on success, negative on error
 */
int surg_actuator_estop(void);

/**
 * @brief Process control loop (call at SURG_CONTROL_RATE_HZ)
 * @return 0 on success, negative on error
 */
int surg_actuator_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SURGICAL_ACTUATOR_H */
