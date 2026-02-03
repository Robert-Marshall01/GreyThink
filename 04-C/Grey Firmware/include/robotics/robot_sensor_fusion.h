/**
 * @file robot_sensor_fusion.h
 * @brief Robot Sensor Fusion Module (IMU + Vision)
 * 
 * INDUSTRY RELEVANCE:
 * Autonomous robots require robust state estimation from multiple sensors.
 * This module demonstrates:
 * - Extended Kalman Filter (EKF) for 6-DOF pose estimation
 * - IMU + visual odometry fusion
 * - SLAM integration interface
 * - Dynamic obstacle detection and classification
 * 
 * Applications: Mobile robots, drones, autonomous vehicles, warehouse AGVs,
 *               service robots, inspection robots
 * Standards: ROS sensor_msgs, IEEE 1873
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_ROBOT_SENSOR_FUSION_H
#define GF_ROBOT_SENSOR_FUSION_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Sensor Fusion Types
 ******************************************************************************/

/** Sensor types */
typedef enum {
    GF_SENSOR_IMU,            /**< Inertial Measurement Unit */
    GF_SENSOR_CAMERA,         /**< Visual camera */
    GF_SENSOR_LIDAR,          /**< LiDAR */
    GF_SENSOR_WHEEL_ENCODER,  /**< Wheel odometry */
    GF_SENSOR_GPS,            /**< GPS/GNSS */
    GF_SENSOR_ULTRASONIC,     /**< Ultrasonic rangefinder */
    GF_SENSOR_DEPTH_CAMERA    /**< Depth camera */
} gf_robot_sensor_t;

/** Fusion algorithm */
typedef enum {
    GF_FUSION_COMPLEMENTARY,  /**< Simple complementary filter */
    GF_FUSION_EKF,            /**< Extended Kalman Filter */
    GF_FUSION_UKF,            /**< Unscented Kalman Filter */
    GF_FUSION_PARTICLE        /**< Particle filter */
} gf_fusion_algorithm_t;

/** Localization state */
typedef enum {
    GF_LOC_INITIALIZING,
    GF_LOC_TRACKING,          /**< Normal operation */
    GF_LOC_LOST,              /**< Localization lost */
    GF_LOC_RELOCATING         /**< Attempting recovery */
} gf_localization_state_t;

/*******************************************************************************
 * Sensor Fusion Configuration
 ******************************************************************************/

/** 3D vector (fixed-point Q16.16) */
typedef struct {
    int32_t x;
    int32_t y;
    int32_t z;
} gf_vector3_t;

/** Quaternion (fixed-point Q1.30) */
typedef struct {
    int32_t w;
    int32_t x;
    int32_t y;
    int32_t z;
} gf_quaternion_t;

/** IMU reading */
typedef struct {
    gf_vector3_t accel;           /**< Acceleration (mm/s²) */
    gf_vector3_t gyro;            /**< Angular velocity (mrad/s) */
    gf_vector3_t mag;             /**< Magnetometer (µT) */
    int16_t temperature_c10;      /**< Temperature (°C × 10) */
    uint32_t timestamp_us;
    bool is_calibrated;
} gf_imu_reading_t;

/** Visual odometry */
typedef struct {
    gf_vector3_t delta_pos;       /**< Position change (mm) */
    gf_quaternion_t delta_rot;    /**< Rotation change */
    uint16_t features_tracked;    /**< Feature count */
    uint8_t confidence_pct;       /**< Tracking confidence */
    uint32_t timestamp_us;
} gf_visual_odom_t;

/** Sensor fusion configuration */
typedef struct {
    gf_fusion_algorithm_t algorithm;
    uint32_t update_rate_hz;      /**< Fusion update rate */
    bool enable_imu;
    bool enable_vision;
    bool enable_wheel_odom;
    bool enable_gps;
    
    /* EKF tuning */
    float process_noise[6];       /**< Q matrix diagonal */
    float measurement_noise[6];   /**< R matrix diagonal */
    
    /* IMU calibration */
    gf_vector3_t accel_bias;
    gf_vector3_t gyro_bias;
    gf_vector3_t mag_hard_iron;
} gf_sensor_fusion_config_t;

/** Robot pose */
typedef struct {
    gf_vector3_t position;        /**< Position (mm) */
    gf_quaternion_t orientation;  /**< Orientation */
    gf_vector3_t velocity;        /**< Linear velocity (mm/s) */
    gf_vector3_t angular_vel;     /**< Angular velocity (mrad/s) */
    gf_vector3_t covariance;      /**< Position uncertainty (mm) */
    gf_localization_state_t state;
    uint32_t timestamp_us;
} gf_robot_pose_t;

/** Obstacle detection */
typedef struct {
    gf_vector3_t position;        /**< Obstacle center (mm) */
    gf_vector3_t size;            /**< Bounding box (mm) */
    int32_t velocity;             /**< Approach velocity (mm/s) */
    uint8_t classification;       /**< Object type */
    uint8_t confidence_pct;
    uint32_t track_id;
} gf_obstacle_t;

/*******************************************************************************
 * Sensor Fusion Statistics
 ******************************************************************************/

typedef struct {
    uint32_t fusion_updates;
    uint32_t imu_readings;
    uint32_t vision_updates;
    uint32_t localization_resets;
    uint32_t tracking_losses;
    float avg_update_time_us;
    float max_covariance;
    uint32_t obstacles_detected;
} gf_sensor_fusion_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize sensor fusion
 * @param config Fusion configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_fusion_init(const gf_sensor_fusion_config_t *config);

/**
 * @brief Update with IMU reading
 * @param imu IMU reading
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_fusion_update_imu(const gf_imu_reading_t *imu);

/**
 * @brief Update with visual odometry
 * @param vo Visual odometry
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_fusion_update_vision(const gf_visual_odom_t *vo);

/**
 * @brief Run fusion step
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_fusion_process(void);

/**
 * @brief Get current pose estimate
 * @param pose Output pose
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_fusion_get_pose(gf_robot_pose_t *pose);

/**
 * @brief Get detected obstacles
 * @param obstacles Output array
 * @param max_count Maximum obstacles
 * @return Number of obstacles
 */
uint8_t gf_robot_fusion_get_obstacles(gf_obstacle_t *obstacles, 
                                       uint8_t max_count);

/**
 * @brief Reset localization
 * @param initial_pose Known starting pose (or NULL)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_fusion_reset(const gf_robot_pose_t *initial_pose);

/**
 * @brief Trigger relocalization
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_fusion_relocalize(void);

/**
 * @brief Calibrate IMU
 * @param duration_ms Calibration duration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_imu_calibrate(uint32_t duration_ms);

/**
 * @brief Get fusion statistics
 * @return Current statistics
 */
gf_sensor_fusion_stats_t gf_robot_fusion_get_stats(void);

/**
 * @brief Shutdown sensor fusion
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_robot_fusion_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ROBOT_SENSOR_FUSION_H */
