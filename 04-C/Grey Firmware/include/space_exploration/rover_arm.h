/**
 * @file rover_arm.h
 * @brief Planetary Rover Robotic Arm Actuator Driver
 * 
 * INDUSTRY RELEVANCE:
 * Mars/Moon rover missions (Perseverance, Curiosity, VIPER) rely on robotic arms
 * for sample collection, instrument placement, and terrain analysis. This driver
 * demonstrates embedded control for multi-DOF manipulators in extreme environments
 * with high latency command channels and autonomous fault recovery.
 * 
 * Key applications:
 * - NASA/ESA planetary exploration programs
 * - Lunar Gateway remote operations  
 * - Commercial space mining and ISRU systems
 * - Deep space probe instrument deployment
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_ROVER_ARM_H
#define GF_ROVER_ARM_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define ROVER_ARM_MAX_JOINTS        7       /**< 7-DOF arm configuration */
#define ROVER_ARM_MAX_TOOLS         4       /**< End effector tool count */
#define ROVER_ARM_POSITION_TOLERANCE 0.01f  /**< Position tolerance (radians) */
#define ROVER_ARM_FORCE_LIMIT       50.0f   /**< Max force limit (N) */
#define ROVER_ARM_TEMP_MIN          (-120)  /**< Min operating temp (C) */
#define ROVER_ARM_TEMP_MAX          50      /**< Max operating temp (C) */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Joint identifiers for 7-DOF arm
 */
typedef enum {
    ROVER_JOINT_SHOULDER_YAW = 0,
    ROVER_JOINT_SHOULDER_PITCH,
    ROVER_JOINT_ELBOW,
    ROVER_JOINT_WRIST_PITCH,
    ROVER_JOINT_WRIST_ROLL,
    ROVER_JOINT_GRIPPER_YAW,
    ROVER_JOINT_GRIPPER
} rover_joint_t;

/**
 * @brief End effector tool types
 */
typedef enum {
    ROVER_TOOL_GRIPPER = 0,     /**< Sample gripper */
    ROVER_TOOL_DRILL,           /**< Core drill */
    ROVER_TOOL_SCOOP,           /**< Soil scoop */
    ROVER_TOOL_SPECTROMETER     /**< Contact spectrometer */
} rover_tool_t;

/**
 * @brief Arm operating mode
 */
typedef enum {
    ROVER_ARM_MODE_STOWED = 0,  /**< Safe stowed position */
    ROVER_ARM_MODE_DEPLOYING,   /**< Deploying from stow */
    ROVER_ARM_MODE_READY,       /**< Ready for commands */
    ROVER_ARM_MODE_MOVING,      /**< Executing trajectory */
    ROVER_ARM_MODE_CONTACT,     /**< In contact with surface */
    ROVER_ARM_MODE_FAULT        /**< Fault condition */
} rover_arm_mode_t;

/**
 * @brief Joint state structure
 */
typedef struct {
    float position;             /**< Current position (radians) */
    float velocity;             /**< Current velocity (rad/s) */
    float torque;               /**< Current torque (Nm) */
    float temperature;          /**< Motor temperature (C) */
    bool limit_min;             /**< At minimum limit */
    bool limit_max;             /**< At maximum limit */
    bool fault;                 /**< Joint fault flag */
} rover_joint_state_t;

/**
 * @brief Arm configuration
 */
typedef struct {
    float joint_limits_min[ROVER_ARM_MAX_JOINTS];
    float joint_limits_max[ROVER_ARM_MAX_JOINTS];
    float max_velocity[ROVER_ARM_MAX_JOINTS];
    float max_acceleration[ROVER_ARM_MAX_JOINTS];
    bool enable_collision_avoidance;
    bool enable_force_feedback;
} rover_arm_config_t;

/**
 * @brief Arm telemetry
 */
typedef struct {
    rover_arm_mode_t mode;
    rover_joint_state_t joints[ROVER_ARM_MAX_JOINTS];
    rover_tool_t active_tool;
    float end_effector_force[3];    /**< Force at end effector (N) */
    float power_consumption;         /**< Current power draw (W) */
    uint32_t command_sequence;       /**< Last executed command ID */
    uint32_t uptime_seconds;
} rover_arm_telemetry_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize rover arm subsystem
 * @param config Arm configuration
 * @return 0 on success, negative on error
 */
int rover_arm_init(const rover_arm_config_t* config);

/**
 * @brief Shutdown rover arm (stow safely)
 * @return 0 on success, negative on error
 */
int rover_arm_shutdown(void);

/**
 * @brief Deploy arm from stowed position
 * @return 0 on success, negative on error
 */
int rover_arm_deploy(void);

/**
 * @brief Stow arm to safe position
 * @return 0 on success, negative on error
 */
int rover_arm_stow(void);

/**
 * @brief Move to joint positions
 * @param positions Target positions for each joint (radians)
 * @param duration Motion duration (seconds)
 * @return 0 on success, negative on error
 */
int rover_arm_move_joints(const float* positions, float duration);

/**
 * @brief Move end effector to Cartesian position
 * @param x X position (meters)
 * @param y Y position (meters)
 * @param z Z position (meters)
 * @param duration Motion duration (seconds)
 * @return 0 on success, negative on error
 */
int rover_arm_move_cartesian(float x, float y, float z, float duration);

/**
 * @brief Select active tool
 * @param tool Tool to activate
 * @return 0 on success, negative on error
 */
int rover_arm_select_tool(rover_tool_t tool);

/**
 * @brief Execute tool action (drill, grip, etc.)
 * @param action_id Tool-specific action identifier
 * @return 0 on success, negative on error
 */
int rover_arm_tool_action(uint8_t action_id);

/**
 * @brief Get current telemetry
 * @param telemetry Output telemetry structure
 * @return 0 on success, negative on error
 */
int rover_arm_get_telemetry(rover_arm_telemetry_t* telemetry);

/**
 * @brief Emergency stop all motion
 * @return 0 on success, negative on error
 */
int rover_arm_emergency_stop(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ROVER_ARM_H */
