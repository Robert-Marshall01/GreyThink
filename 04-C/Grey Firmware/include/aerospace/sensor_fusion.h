/**
 * @file sensor_fusion.h
 * @brief Flight Sensor Fusion Module (IMU + GPS)
 *
 * INDUSTRY RELEVANCE:
 * Avionics systems require high-integrity navigation solutions from
 * fused sensor data. This module demonstrates:
 * - Extended Kalman Filter (EKF) for state estimation
 * - IMU mechanization (accelerometer + gyroscope integration)
 * - GPS/INS coupling for bounded drift
 * - DO-178C compliance awareness (DAL-A capable)
 * - Fault detection and isolation for sensor failures
 *
 * Used in: Flight control computers, UAV autopilots, AHRS systems
 *
 * @note This is a stub demonstrating aerospace sensor fusion patterns.
 */

#ifndef GF_SENSOR_FUSION_H
#define GF_SENSOR_FUSION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Sensor Fusion Type Definitions                                            */
/*===========================================================================*/

/**
 * @brief 3-axis vector (for IMU data)
 */
typedef struct {
    float x;
    float y;
    float z;
} gf_vec3_t;

/**
 * @brief Quaternion for attitude representation
 */
typedef struct {
    float w;
    float x;
    float y;
    float z;
} gf_quat_t;

/**
 * @brief Euler angles (degrees)
 */
typedef struct {
    float roll;         /**< Roll angle (deg) */
    float pitch;        /**< Pitch angle (deg) */
    float yaw;          /**< Yaw/heading angle (deg) */
} gf_euler_t;

/**
 * @brief Geographic position
 */
typedef struct {
    double latitude;    /**< Latitude (degrees, WGS84) */
    double longitude;   /**< Longitude (degrees, WGS84) */
    float altitude;     /**< Altitude (meters, MSL) */
} gf_position_t;

/**
 * @brief Velocity (NED frame)
 */
typedef struct {
    float north;        /**< North velocity (m/s) */
    float east;         /**< East velocity (m/s) */
    float down;         /**< Down velocity (m/s) */
} gf_velocity_t;

/**
 * @brief IMU measurement
 */
typedef struct {
    gf_vec3_t accel;        /**< Accelerometer (m/s^2) */
    gf_vec3_t gyro;         /**< Gyroscope (rad/s) */
    gf_vec3_t mag;          /**< Magnetometer (uT) */
    uint64_t timestamp_us;  /**< Microsecond timestamp */
    bool valid;             /**< Data valid flag */
} gf_imu_data_t;

/**
 * @brief GPS measurement
 */
typedef struct {
    gf_position_t position;
    gf_velocity_t velocity;
    float pdop;             /**< Position DOP */
    float hdop;             /**< Horizontal DOP */
    float vdop;             /**< Vertical DOP */
    uint8_t satellites;     /**< Satellites in view */
    uint8_t fix_type;       /**< Fix type (0=none, 2=2D, 3=3D, 4=RTK) */
    uint64_t timestamp_us;  /**< Microsecond timestamp */
    bool valid;             /**< Fix valid flag */
} gf_gps_data_t;

/**
 * @brief Barometer measurement
 */
typedef struct {
    float pressure_pa;      /**< Pressure (Pa) */
    float temperature_c;    /**< Temperature (C) */
    float altitude_m;       /**< Pressure altitude (m) */
    uint64_t timestamp_us;
    bool valid;
} gf_baro_data_t;

/**
 * @brief Fused navigation solution
 */
typedef struct {
    /* Attitude */
    gf_quat_t quaternion;   /**< Attitude quaternion (NED to body) */
    gf_euler_t euler;       /**< Euler angles */
    gf_vec3_t angular_rate; /**< Body angular rates (rad/s) */
    
    /* Position/Velocity */
    gf_position_t position; /**< Geographic position */
    gf_velocity_t velocity; /**< NED velocity */
    float ground_speed;     /**< Ground speed (m/s) */
    float vertical_speed;   /**< Vertical speed (m/s, positive up) */
    float course;           /**< Course over ground (deg) */
    
    /* Accelerations */
    gf_vec3_t accel_body;   /**< Body acceleration (m/s^2) */
    gf_vec3_t accel_ned;    /**< NED acceleration (m/s^2) */
    float load_factor;      /**< Load factor (g) */
    
    /* Status */
    uint64_t timestamp_us;  /**< Solution timestamp */
    uint8_t solution_status;/**< Status (0=none, 1=coarse, 2=fine) */
    uint8_t gps_mode;       /**< GPS integration mode */
    bool attitude_valid;    /**< Attitude solution valid */
    bool position_valid;    /**< Position solution valid */
    
    /* Confidence */
    float attitude_sigma;   /**< Attitude uncertainty (deg) */
    float position_sigma;   /**< Position uncertainty (m) */
    float velocity_sigma;   /**< Velocity uncertainty (m/s) */
} gf_nav_solution_t;

/**
 * @brief Sensor fusion configuration
 */
typedef struct {
    /* IMU parameters */
    float imu_rate_hz;          /**< IMU update rate */
    float accel_noise;          /**< Accelerometer noise (m/s^2) */
    float gyro_noise;           /**< Gyroscope noise (rad/s) */
    float accel_bias_stability; /**< Accel bias stability */
    float gyro_bias_stability;  /**< Gyro bias stability */
    
    /* GPS parameters */
    float gps_rate_hz;          /**< GPS update rate */
    float gps_pos_noise;        /**< GPS position noise (m) */
    float gps_vel_noise;        /**< GPS velocity noise (m/s) */
    
    /* Filter tuning */
    float process_noise;        /**< Process noise scaling */
    float initial_attitude_sigma;
    float initial_position_sigma;
    
    /* Modes */
    bool use_magnetometer;      /**< Enable mag for heading */
    bool use_barometer;         /**< Enable baro for altitude */
    bool enable_zupt;           /**< Zero-velocity updates */
} gf_fusion_config_t;

/**
 * @brief Sensor fusion health status
 */
typedef struct {
    bool imu_healthy;
    bool gps_healthy;
    bool mag_healthy;
    bool baro_healthy;
    bool filter_healthy;
    uint8_t innovation_fault;   /**< Bitmask of sensors with large innovations */
    uint32_t imu_updates;
    uint32_t gps_updates;
    uint32_t filter_resets;
} gf_fusion_health_t;

/**
 * @brief Sensor fusion handle
 */
typedef struct gf_sensor_fusion* gf_sensor_fusion_t;

/*===========================================================================*/
/* Sensor Fusion API                                                         */
/*===========================================================================*/

/**
 * @brief Initialize sensor fusion
 * @param config Configuration parameters
 * @param fusion Output handle
 * @return 0 on success
 */
int gf_fusion_init(const gf_fusion_config_t* config,
                   gf_sensor_fusion_t* fusion);

/**
 * @brief Set initial position
 * @param fusion Fusion handle
 * @param position Initial position
 * @return 0 on success
 */
int gf_fusion_set_initial_position(gf_sensor_fusion_t fusion,
                                    const gf_position_t* position);

/**
 * @brief Process IMU measurement
 * @param fusion Fusion handle
 * @param imu IMU data
 * @return 0 on success
 */
int gf_fusion_update_imu(gf_sensor_fusion_t fusion,
                          const gf_imu_data_t* imu);

/**
 * @brief Process GPS measurement
 * @param fusion Fusion handle
 * @param gps GPS data
 * @return 0 on success
 */
int gf_fusion_update_gps(gf_sensor_fusion_t fusion,
                          const gf_gps_data_t* gps);

/**
 * @brief Process barometer measurement
 * @param fusion Fusion handle
 * @param baro Barometer data
 * @return 0 on success
 */
int gf_fusion_update_baro(gf_sensor_fusion_t fusion,
                           const gf_baro_data_t* baro);

/**
 * @brief Get current navigation solution
 * @param fusion Fusion handle
 * @param solution Output solution
 * @return 0 on success
 */
int gf_fusion_get_solution(gf_sensor_fusion_t fusion,
                            gf_nav_solution_t* solution);

/**
 * @brief Get fusion health status
 * @param fusion Fusion handle
 * @param health Output health status
 * @return 0 on success
 */
int gf_fusion_get_health(gf_sensor_fusion_t fusion,
                          gf_fusion_health_t* health);

/**
 * @brief Reset fusion filter
 * @param fusion Fusion handle
 * @return 0 on success
 */
int gf_fusion_reset(gf_sensor_fusion_t fusion);

/**
 * @brief Deinitialize fusion
 * @param fusion Fusion handle
 */
void gf_fusion_deinit(gf_sensor_fusion_t fusion);

/*===========================================================================*/
/* Coordinate Transform Utilities                                            */
/*===========================================================================*/

/**
 * @brief Convert quaternion to Euler angles
 * @param q Input quaternion
 * @param euler Output Euler angles
 */
void gf_quat_to_euler(const gf_quat_t* q, gf_euler_t* euler);

/**
 * @brief Convert Euler angles to quaternion
 * @param euler Input Euler angles
 * @param q Output quaternion
 */
void gf_euler_to_quat(const gf_euler_t* euler, gf_quat_t* q);

/**
 * @brief Rotate vector by quaternion
 * @param q Quaternion
 * @param v Input vector
 * @param result Output rotated vector
 */
void gf_quat_rotate(const gf_quat_t* q, const gf_vec3_t* v, gf_vec3_t* result);

/**
 * @brief Normalize quaternion
 * @param q Quaternion to normalize (in-place)
 */
void gf_quat_normalize(gf_quat_t* q);

#ifdef __cplusplus
}
#endif

#endif /* GF_SENSOR_FUSION_H */
