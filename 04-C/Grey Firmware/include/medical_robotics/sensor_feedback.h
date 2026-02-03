/**
 * @file sensor_feedback.h
 * @brief Medical Robot Sensor Feedback Loop
 * 
 * INDUSTRY RELEVANCE:
 * Surgical robots require multi-modal sensing: force/torque, position, vision,
 * and tissue characterization. This module fuses sensor data for closed-loop
 * control with safety interlocks. Essential for autonomous and semi-autonomous
 * surgical procedures meeting IEC 62304 and ISO 13482 requirements.
 * 
 * Key applications:
 * - Force-limited tissue manipulation
 * - Visual servoing for instrument tracking
 * - Tissue stiffness characterization
 * - Collision avoidance in anatomy
 * - Real-time path adaptation
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_SENSOR_FEEDBACK_H
#define GF_SENSOR_FEEDBACK_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define SENSOR_MAX_FORCE_TORQUE     4       /**< Max F/T sensors */
#define SENSOR_MAX_ENCODERS         12      /**< Max position encoders */
#define SENSOR_MAX_CAMERAS          3       /**< Max vision cameras */
#define SENSOR_FUSION_RATE_HZ       500     /**< Sensor fusion rate */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Sensor type
 */
typedef enum {
    SENSOR_FORCE_TORQUE = 0,
    SENSOR_ENCODER_INCREMENTAL,
    SENSOR_ENCODER_ABSOLUTE,
    SENSOR_CAMERA_STEREO,
    SENSOR_CAMERA_ENDOSCOPE,
    SENSOR_ULTRASOUND,
    SENSOR_IMPEDANCE
} sensor_type_t;

/**
 * @brief Force/torque reading
 */
typedef struct {
    float fx, fy, fz;               /**< Force components (N) */
    float tx, ty, tz;               /**< Torque components (Nm) */
    uint32_t timestamp;
    bool valid;
} sensor_force_torque_t;

/**
 * @brief Position reading
 */
typedef struct {
    float position;                 /**< Position (mm or deg) */
    float velocity;                 /**< Velocity */
    uint32_t timestamp;
    bool valid;
    bool index_found;               /**< Index pulse detected */
} sensor_position_t;

/**
 * @brief Vision tracking result
 */
typedef struct {
    float x, y, z;                  /**< 3D position (mm) */
    float rx, ry, rz;               /**< Orientation (deg) */
    float confidence;               /**< Tracking confidence (0-1) */
    uint32_t timestamp;
    bool valid;
} sensor_vision_t;

/**
 * @brief Tissue characterization
 */
typedef struct {
    float stiffness;                /**< Tissue stiffness (N/mm) */
    float damping;                  /**< Damping coefficient */
    float impedance;                /**< Electrical impedance (ohm) */
    uint8_t tissue_type;            /**< Classified tissue type */
    float confidence;
} sensor_tissue_t;

/**
 * @brief Fused state estimate
 */
typedef struct {
    float position[6];              /**< 6-DOF position */
    float velocity[6];              /**< 6-DOF velocity */
    float force[6];                 /**< 6-DOF force/torque */
    sensor_tissue_t tissue;
    float covariance[36];           /**< State covariance */
    uint32_t timestamp;
    bool converged;
} sensor_fused_state_t;

/**
 * @brief Sensor configuration
 */
typedef struct {
    bool enable_force_torque;
    bool enable_vision;
    bool enable_tissue_sensing;
    float force_filter_cutoff;
    float position_filter_cutoff;
    uint8_t fusion_algorithm;       /**< 0=Kalman, 1=Complementary */
} sensor_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize sensor feedback system
 * @param config Sensor configuration
 * @return 0 on success, negative on error
 */
int sensor_feedback_init(const sensor_config_t* config);

/**
 * @brief Shutdown sensor system
 * @return 0 on success, negative on error
 */
int sensor_feedback_shutdown(void);

/**
 * @brief Register force/torque sensor
 * @param sensor_id Sensor identifier
 * @return 0 on success, negative on error
 */
int sensor_register_ft(uint8_t sensor_id);

/**
 * @brief Get force/torque reading
 * @param sensor_id Sensor ID
 * @param reading Output reading
 * @return 0 on success, negative on error
 */
int sensor_get_force_torque(uint8_t sensor_id, sensor_force_torque_t* reading);

/**
 * @brief Get position reading
 * @param encoder_id Encoder ID
 * @param reading Output reading
 * @return 0 on success, negative on error
 */
int sensor_get_position(uint8_t encoder_id, sensor_position_t* reading);

/**
 * @brief Get vision tracking result
 * @param camera_id Camera ID
 * @param result Output tracking result
 * @return 0 on success, negative on error
 */
int sensor_get_vision(uint8_t camera_id, sensor_vision_t* result);

/**
 * @brief Get fused state estimate
 * @param state Output fused state
 * @return 0 on success, negative on error
 */
int sensor_get_fused_state(sensor_fused_state_t* state);

/**
 * @brief Process sensor fusion (call at SENSOR_FUSION_RATE_HZ)
 * @return 0 on success, negative on error
 */
int sensor_feedback_process(void);

/**
 * @brief Tare force/torque sensors
 * @return 0 on success, negative on error
 */
int sensor_tare(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SENSOR_FEEDBACK_H */
