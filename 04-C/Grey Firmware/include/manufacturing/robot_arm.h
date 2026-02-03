/**/**












































































































































































































































































































#endif /* GF_ROBOT_ARM_H */#endif}#ifdef __cplusplus                                 uint8_t config);                                 gf_robot_joints_t* joints,int gf_robot_inverse_kinematics(const gf_robot_pose_t* pose, */ * @return 0 on success, -1 if unreachable * @param config Desired configuration * @param[out] joints Output joint positions * @param pose Desired Cartesian pose * @brief Compute inverse kinematics/**int gf_robot_forward_kinematics(const gf_robot_joints_t* joints, gf_robot_pose_t* pose); */ * @return 0 on success * @param[out] pose Output Cartesian pose * @param joints Joint positions * @brief Compute forward kinematics/**void gf_robot_set_collision_callback(gf_robot_collision_cb_t callback, void* user_data); */ * @param user_data Callback context * @param callback Collision detected callback * @brief Set collision callback/**void gf_robot_set_digital_out(uint8_t channel, bool value); */ * @param value Output value * @param channel Output channel (0-31) * @brief Set digital output/**void gf_robot_get_io(gf_robot_io_t* io); */ * @param[out] io I/O state * @brief Read I/O state/**void gf_robot_set_safety(const gf_robot_safety_t* safety); */ * @param safety Safety configuration * @brief Configure safety parameters/**void gf_robot_set_tool(const gf_robot_tool_t* tool); */ * @param tool Tool configuration * @brief Configure tool/**void gf_robot_get_status(gf_robot_status_t* status); */ * @param[out] status Output status * @brief Get current status/**int gf_robot_reset(void); */ * @return 0 on success * @brief Clear faults and reset/**void gf_robot_estop(void); */ * @brief Emergency stop/**void gf_robot_stop(void); */ * @brief Stop all motion/**                          void* user_data);                          gf_robot_motion_done_cb_t callback,                          const gf_robot_motion_params_t* params,int gf_robot_move_linear(const gf_robot_pose_t* target, */ * @return 0 on success * @param user_data Callback context * @param callback Completion callback (optional) * @param params Motion parameters * @param target Target pose * @brief Move to Cartesian pose/**                          void* user_data);                          gf_robot_motion_done_cb_t callback,                          const gf_robot_motion_params_t* params,int gf_robot_move_joints(const gf_robot_joints_t* target, */ * @return 0 on success * @param user_data Callback context * @param callback Completion callback (optional) * @param params Motion parameters * @param target Target joint positions * @brief Move to joint positions/**int gf_robot_set_mode(gf_robot_mode_t mode); */ * @return 0 on success * @param mode Desired mode * @brief Set operation mode/**void gf_robot_servo_off(void); */ * @brief Disable servo power/**int gf_robot_servo_on(void); */ * @return 0 on success * @brief Enable servo power/**void gf_robot_deinit(void); */ * @brief Shutdown robot controller/**int gf_robot_init(uint8_t num_axes); */ * @return 0 on success, negative error code on failure * @param num_axes Number of robot axes * @brief Initialize robot controller/** * ───────────────────────────────────────────────────────────────────────────── */ * API Functions/* ─────────────────────────────────────────────────────────────────────────────typedef void (*gf_robot_collision_cb_t)(float force, const gf_robot_pose_t* pose, void* user_data);typedef void (*gf_robot_motion_done_cb_t)(bool success, void* user_data); * ───────────────────────────────────────────────────────────────────────────── */ * Callbacks/* ─────────────────────────────────────────────────────────────────────────────} gf_robot_io_t;    float analog_out[4];        /**< Analog outputs (0-10V) */    float analog_in[8];         /**< Analog inputs (0-10V) */    uint32_t digital_out;       /**< Digital output bitmask */    uint32_t digital_in;        /**< Digital input bitmask */typedef struct { */ * @brief Digital I/O state/**} gf_robot_status_t;    bool servo_on;              /**< Servos enabled */    bool in_motion;             /**< Currently moving */    uint16_t fault_code;        /**< Active fault code */    uint32_t program_line;      /**< Current program line */    float tcp_force_n;          /**< TCP force (N) */    float tcp_speed_mm_s;       /**< TCP speed (mm/s) */    float joint_torques[GF_ROBOT_MAX_AXES]; /**< Current torques (Nm) */    gf_robot_pose_t pose;       /**< Current Cartesian pose */    gf_robot_joints_t joints;   /**< Current joint positions */    gf_robot_mode_t mode;       /**< Operation mode */    gf_robot_state_t state;     /**< Current state */typedef struct { */ * @brief Robot status/**} gf_robot_safety_t;    float collision_threshold;  /**< Collision sensitivity */    bool collision_detect;      /**< Enable collision detection */    float workspace_max[3];     /**< Workspace maximum bounds */    float workspace_min[3];     /**< Workspace minimum bounds */    float max_power_w;          /**< Maximum power */    float max_force_n;          /**< Maximum force (for cobots) */    float max_velocity[GF_ROBOT_MAX_AXES]; /**< Per-axis velocity limits */typedef struct { */ * @brief Safety configuration/**} gf_robot_tool_t;    uint16_t tool_id;           /**< Tool identifier */    float cog[3];               /**< Center of gravity offset */    float mass_kg;              /**< Tool mass */    gf_robot_pose_t tcp;        /**< Tool center point offset */typedef struct { */ * @brief Tool (end effector) configuration/**} gf_robot_motion_params_t;    gf_robot_motion_t type;     /**< Motion type */    float blend_radius_mm;      /**< Blending radius for continuous path */    float jerk_percent;         /**< Jerk limit (% of max) */    float acceleration_percent; /**< Acceleration (% of max) */    float velocity_percent;     /**< Velocity (% of max) */typedef struct { */ * @brief Motion parameters/**} gf_robot_pose_t;    uint8_t config;             /**< Robot configuration flags */    float rz;                   /**< Rotation about Z (radians) */    float ry;                   /**< Rotation about Y (radians) */    float rx;                   /**< Rotation about X (radians) */    float z;                    /**< Z position (mm) */    float y;                    /**< Y position (mm) */    float x;                    /**< X position (mm) */typedef struct { */ * @brief Cartesian pose (position + orientation)/**} gf_robot_joints_t;    uint8_t num_axes;           /**< Number of valid axes */    float joints[GF_ROBOT_MAX_AXES]; /**< Joint angles (radians) */typedef struct { */ * @brief Joint positions (all axes)/**} gf_robot_motion_t;    GF_ROBOT_MOTION_CIRCULAR    /**< Circular arc interpolation */    GF_ROBOT_MOTION_LINEAR,     /**< Cartesian linear interpolation */    GF_ROBOT_MOTION_JOINT,      /**< Joint space interpolation */typedef enum { */ * @brief Motion type/**} gf_robot_state_t;    GF_ROBOT_STATE_ESTOP        /**< Emergency stopped */    GF_ROBOT_STATE_FAULT,       /**< Fault condition */    GF_ROBOT_STATE_HOLDING,     /**< Holding position */    GF_ROBOT_STATE_MOVING,      /**< In motion */    GF_ROBOT_STATE_IDLE,        /**< Idle, ready for commands */typedef enum { */ * @brief Robot state/**} gf_robot_mode_t;    GF_ROBOT_MODE_REMOTE        /**< External control (PLC/API) */    GF_ROBOT_MODE_AUTO,         /**< Automatic program execution */    GF_ROBOT_MODE_TEACH,        /**< Teaching mode */    GF_ROBOT_MODE_MANUAL,       /**< Manual jogging */typedef enum { */ * @brief Robot operation mode/** * ───────────────────────────────────────────────────────────────────────────── */ * Type Definitions/* ─────────────────────────────────────────────────────────────────────────────#define GF_ROBOT_CONTROL_RATE_HZ    1000    /**< Servo control rate */#define GF_ROBOT_MAX_WAYPOINTS      1000    /**< Maximum path waypoints */#define GF_ROBOT_MAX_AXES           7       /**< Maximum robot axes */ * ───────────────────────────────────────────────────────────────────────────── */ * Constants/* ─────────────────────────────────────────────────────────────────────────────#endifextern "C" {#ifdef __cplusplus#include <stdbool.h>#include <stdint.h>#define GF_ROBOT_ARM_H#ifndef GF_ROBOT_ARM_H */ *       Production requires certified safety systems. * @note This is a stub module demonstrating interface design. * * collaborative robots (cobots) for human-robot interaction. * Universal Robots), automation integrators, and companies developing * These skills apply to industrial robot manufacturers (FANUC, ABB, KUKA, * * - Integration with PLCs and industrial protocols * - Safety monitoring (collision detection, force limiting) * - Forward/inverse kinematics for 6-DOF manipulators * - Multi-axis servo control with trajectory planning * This module demonstrates: * applications in automotive, electronics, food processing, and logistics. * Industrial robotic arms are the backbone of modern manufacturing, with * INDUSTRY RELEVANCE: * * @brief Industrial Robot Arm Controller Interface * @file robot_arm.h * @file robot_arm.h
 * @brief Industrial Robot Arm Controller Interface
 *
 * INDUSTRY RELEVANCE:
 * Industrial robotics is a $50B+ market powering modern manufacturing.
 * This module demonstrates firmware expertise in:
 * - Multi-axis servo control with trajectory planning
 * - Inverse kinematics for end-effector positioning
 * - Force/torque sensing for collaborative robots (cobots)
 * - Safety standards (ISO 10218, ISO/TS 15066)
 *
 * These skills apply to robot manufacturers (Fanuc, KUKA, ABB, Universal Robots),
 * automation integrators, and companies developing next-generation collaborative
 * robots and autonomous mobile manipulators.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires certified safety systems.
 */

#ifndef GF_ROBOT_ARM_H
#define GF_ROBOT_ARM_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_ROBOT_MAX_AXES           7       /**< Maximum joint axes */
#define GF_ROBOT_MAX_WAYPOINTS      1000    /**< Maximum path waypoints */
#define GF_ROBOT_CONTROL_RATE_HZ    1000    /**< Servo control rate */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Robot operating mode
 */
typedef enum {
    GF_ROBOT_MODE_DISABLED,     /**< Motors disabled */
    GF_ROBOT_MODE_MANUAL,       /**< Manual jog mode */
    GF_ROBOT_MODE_AUTO,         /**< Automatic program execution */
    GF_ROBOT_MODE_TEACH,        /**< Teach pendant mode */
    GF_ROBOT_MODE_COBOT         /**< Collaborative mode (force-limited) */
} gf_robot_mode_t;

/**
 * @brief Motion type
 */
typedef enum {
    GF_ROBOT_MOTION_JOINT,      /**< Joint-space motion */
    GF_ROBOT_MOTION_LINEAR,     /**< Cartesian linear motion */
    GF_ROBOT_MOTION_CIRCULAR,   /**< Circular arc motion */
    GF_ROBOT_MOTION_SPLINE      /**< Spline interpolation */
} gf_robot_motion_t;

/**
 * @brief Robot status flags
 */
typedef enum {
    GF_ROBOT_STATUS_ENABLED     = 0x0001,   /**< Drives enabled */
    GF_ROBOT_STATUS_HOMED       = 0x0002,   /**< All axes homed */
    GF_ROBOT_STATUS_MOVING      = 0x0004,   /**< Motion in progress */
    GF_ROBOT_STATUS_FAULT       = 0x0008,   /**< Fault active */
    GF_ROBOT_STATUS_ESTOP       = 0x0010,   /**< E-stop pressed */
    GF_ROBOT_STATUS_AT_TARGET   = 0x0020,   /**< At target position */
    GF_ROBOT_STATUS_CONTACT     = 0x0040    /**< External contact detected */
} gf_robot_status_t;

/**
 * @brief Joint position (angles in radians)
 */
typedef struct {
    float joints[GF_ROBOT_MAX_AXES];    /**< Joint angles (radians) */
    uint8_t num_axes;                   /**< Number of active axes */
} gf_robot_joints_t;

/**
 * @brief Cartesian position (pose)
 */
typedef struct {
    float x;                    /**< X position (mm) */
    float y;                    /**< Y position (mm) */
    float z;                    /**< Z position (mm) */
    float rx;                   /**< Rotation about X (radians) */
    float ry;                   /**< Rotation about Y (radians) */
    float rz;                   /**< Rotation about Z (radians) */
} gf_robot_pose_t;

/**
 * @brief Force/torque sensor data
 */
typedef struct {
    float fx;                   /**< Force X (N) */
    float fy;                   /**< Force Y (N) */
    float fz;                   /**< Force Z (N) */
    float tx;                   /**< Torque X (Nm) */
    float ty;                   /**< Torque Y (Nm) */
    float tz;                   /**< Torque Z (Nm) */
} gf_robot_wrench_t;

/**
 * @brief Motion command
 */
typedef struct {
    gf_robot_motion_t type;     /**< Motion type */
    gf_robot_pose_t target;     /**< Target pose */
    float velocity;             /**< Velocity (% of max) 0-100 */
    float acceleration;         /**< Acceleration (% of max) 0-100 */
    float blend_radius_mm;      /**< Blending radius for continuous path */
    bool wait;                  /**< Wait for completion */
} gf_robot_motion_cmd_t;

/**
 * @brief Joint limits
 */
typedef struct {
    float min_pos[GF_ROBOT_MAX_AXES];       /**< Minimum angles (rad) */
    float max_pos[GF_ROBOT_MAX_AXES];       /**< Maximum angles (rad) */
    float max_vel[GF_ROBOT_MAX_AXES];       /**< Maximum velocities (rad/s) */
    float max_accel[GF_ROBOT_MAX_AXES];     /**< Maximum accelerations (rad/s²) */
    float max_torque[GF_ROBOT_MAX_AXES];    /**< Maximum torques (Nm) */
} gf_robot_limits_t;

/**
 * @brief Tool configuration
 */
typedef struct {
    gf_robot_pose_t tcp;        /**< Tool center point offset */
    float mass_kg;              /**< Tool mass */
    float cog[3];               /**< Center of gravity (mm) */
    float inertia[6];           /**< Inertia tensor */
} gf_robot_tool_t;

/**
 * @brief Safety zone
 */
typedef struct {
    char name[32];              /**< Zone name */
    float bounds[6];            /**< Bounding box (xmin,ymin,zmin,xmax,ymax,zmax) */
    bool entry_forbidden;       /**< Forbidden zone */
    float reduced_speed;        /**< Reduced speed percentage (0-100) */
} gf_robot_zone_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_robot_motion_complete_cb_t)(bool success, void* user_data);
typedef void (*gf_robot_contact_cb_t)(const gf_robot_wrench_t* wrench, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize robot controller
 * @param num_axes Number of robot axes
 * @return 0 on success, negative error code on failure
 */
int gf_robot_init(uint8_t num_axes);

/**
 * @brief Shutdown robot controller
 */
void gf_robot_deinit(void);

/**
 * @brief Enable servo drives
 * @return 0 on success
 */
int gf_robot_enable(void);

/**
 * @brief Disable servo drives
 */
void gf_robot_disable(void);

/**
 * @brief Set operating mode
 * @param mode Operating mode
 * @return 0 on success
 */
int gf_robot_set_mode(gf_robot_mode_t mode);

/**
 * @brief Run homing sequence
 * @return 0 on success
 */
int gf_robot_home(void);

/**
 * @brief Move to joint position
 * @param joints Target joint positions
 * @param velocity Velocity (0-100%)
 * @return 0 on success
 */
int gf_robot_move_joints(const gf_robot_joints_t* joints, float velocity);

/**
 * @brief Move to Cartesian pose
 * @param cmd Motion command
 * @return 0 on success
 */
int gf_robot_move(const gf_robot_motion_cmd_t* cmd);

/**
 * @brief Stop motion immediately
 * @param deceleration Deceleration rate (0 = immediate)
 */
void gf_robot_stop(float deceleration);

/**
 * @brief Emergency stop
 */
void gf_robot_estop(void);

/**
 * @brief Reset after E-stop or fault
 * @return 0 on success
 */
int gf_robot_reset(void);

/**
 * @brief Get current joint positions
 * @param[out] joints Output joints
 */
void gf_robot_get_joints(gf_robot_joints_t* joints);

/**
 * @brief Get current Cartesian pose
 * @param[out] pose Output pose
 */
void gf_robot_get_pose(gf_robot_pose_t* pose);

/**
 * @brief Get current force/torque
 * @param[out] wrench Output wrench
 */
void gf_robot_get_wrench(gf_robot_wrench_t* wrench);

/**
 * @brief Get robot status
 * @return Status flags bitmask
 */
uint16_t gf_robot_get_status(void);

/**
 * @brief Configure tool
 * @param tool Tool configuration
 */
void gf_robot_set_tool(const gf_robot_tool_t* tool);

/**
 * @brief Configure joint limits
 * @param limits Joint limits
 */
void gf_robot_set_limits(const gf_robot_limits_t* limits);

/**
 * @brief Add safety zone
 * @param zone Zone configuration
 * @return Zone ID, or negative error code
 */
int gf_robot_add_zone(const gf_robot_zone_t* zone);

/**
 * @brief Forward kinematics (joints → pose)
 * @param joints Joint positions
 * @param[out] pose Output pose
 */
void gf_robot_forward_kin(const gf_robot_joints_t* joints, gf_robot_pose_t* pose);

/**
 * @brief Inverse kinematics (pose → joints)
 * @param pose Target pose
 * @param[out] joints Output joints
 * @return 0 on success, -1 if unreachable
 */
int gf_robot_inverse_kin(const gf_robot_pose_t* pose, gf_robot_joints_t* joints);

/**
 * @brief Set motion complete callback
 * @param callback Callback function
 * @param user_data User context
 */
void gf_robot_set_motion_callback(gf_robot_motion_complete_cb_t callback, void* user_data);

/**
 * @brief Set contact detection callback
 * @param callback Callback function
 * @param user_data User context
 */
void gf_robot_set_contact_callback(gf_robot_contact_cb_t callback, void* user_data);

#ifdef __cplusplus
}
#endif

#endif /* GF_ROBOT_ARM_H */
