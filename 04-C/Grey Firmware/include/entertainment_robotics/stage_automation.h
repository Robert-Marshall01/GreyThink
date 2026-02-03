/**
 * @file stage_automation.h
 * @brief Stage Automation Control Stub
 * 
 * Industry Relevance:
 * Entertainment venues require precise, safety-critical automation for
 * lighting, rigging, and scenic elements. This module demonstrates:
 * - DMX512 and sACN lighting protocol support
 * - Motion control for kinetic sculptures and scenery
 * - Cue-based show programming and playback
 * - E-stop and safety interlock integration
 * 
 * Applications: Theme parks, concerts, theater, broadcast studios, museums
 * 
 * @author Grey Firmware Project
 */

#ifndef STAGE_AUTOMATION_H
#define STAGE_AUTOMATION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Automation axis type */
typedef enum {
    AXIS_LINEAR,             /**< Linear motion (fly systems) */
    AXIS_ROTARY,             /**< Rotary motion (turntables) */
    AXIS_PAN_TILT,           /**< Pan/tilt (moving heads) */
    AXIS_LIFT,               /**< Vertical lift (elevators) */
    AXIS_CURTAIN             /**< Curtain track */
} axis_type_t;

/** Motion profile */
typedef enum {
    MOTION_LINEAR,           /**< Constant velocity */
    MOTION_S_CURVE,          /**< Smooth S-curve */
    MOTION_TRAPEZOID,        /**< Trapezoidal profile */
    MOTION_CUSTOM            /**< Custom profile */
} motion_profile_t;

/** Cue state */
typedef enum {
    CUE_IDLE,
    CUE_STANDBY,
    CUE_RUNNING,
    CUE_COMPLETE,
    CUE_FAULTED
} cue_state_t;

/** Axis configuration */
typedef struct {
    uint8_t axis_id;         /**< Unique axis identifier */
    axis_type_t type;        /**< Axis type */
    float max_velocity;      /**< Maximum velocity (units/s) */
    float max_accel;         /**< Maximum acceleration */
    float min_position;      /**< Soft limit minimum */
    float max_position;      /**< Soft limit maximum */
    bool require_home;       /**< Requires homing on startup */
} axis_config_t;

/** Cue definition */
typedef struct {
    uint16_t cue_number;     /**< Cue number (e.g., 101) */
    uint8_t axis_id;         /**< Target axis */
    float target_position;   /**< Target position */
    float velocity;          /**< Motion velocity */
    motion_profile_t profile;/**< Motion profile */
    float duration_s;        /**< Expected duration */
} cue_t;

/** Axis status */
typedef struct {
    uint8_t axis_id;         /**< Axis identifier */
    float position;          /**< Current position */
    float velocity;          /**< Current velocity */
    bool in_motion;          /**< Currently moving */
    bool homed;              /**< Homing complete */
    bool e_stop_active;      /**< E-stop engaged */
    cue_state_t cue_state;   /**< Current cue state */
} axis_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize stage automation system
 * @return 0 on success, negative on error
 */
int stage_automation_init(void);

/**
 * @brief Configure automation axis
 * @param config Axis configuration
 * @return 0 on success, negative on error
 */
int stage_automation_configure_axis(const axis_config_t *config);

/**
 * @brief Home an axis
 * @param axis_id Axis to home
 * @return 0 on success, negative on error
 */
int stage_automation_home(uint8_t axis_id);

/**
 * @brief Load cue for execution
 * @param cue Cue definition
 * @return 0 on success, negative on error
 */
int stage_automation_load_cue(const cue_t *cue);

/**
 * @brief Execute loaded cue (GO)
 * @param cue_number Cue to execute
 * @return 0 on success, negative on error
 */
int stage_automation_go(uint16_t cue_number);

/**
 * @brief Get axis status
 * @param axis_id Axis to query
 * @param status Output status
 * @return 0 on success, negative on error
 */
int stage_automation_get_status(uint8_t axis_id, axis_status_t *status);

/**
 * @brief Emergency stop all axes
 * @return 0 on success, negative on error
 */
int stage_automation_estop(void);

/**
 * @brief Shutdown stage automation
 */
void stage_automation_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* STAGE_AUTOMATION_H */
