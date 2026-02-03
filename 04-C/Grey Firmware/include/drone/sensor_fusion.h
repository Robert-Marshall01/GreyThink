/**
 * @file sensor_fusion.h
 * @brief Drone GPS + IMU Sensor Fusion Interface
 *
 * INDUSTRY RELEVANCE:
 * Sensor fusion is fundamental to all autonomous vehicles, combining noisy
 * measurements into reliable state estimates. This module demonstrates:
 * - Extended Kalman Filter (EKF) for attitude and position estimation
 * - GPS/INS integration with outlier rejection
 * - Magnetometer calibration and interference handling
 * - Barometer fusion for altitude accuracy
 *
 * These skills apply to drone autopilot development, autonomous vehicles,
 * mobile robotics, and augmented reality systems. Relevant for companies
 * like DJI, Tesla, Waymo, and startups in the UAV space.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires validated filtering algorithms.
 */

#ifndef GF_SENSOR_FUSION_H
#define GF_SENSOR_FUSION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_FUSION_STATE_DIM         15      /**< EKF state vector dimension */
#define GF_FUSION_IMU_RATE_HZ       200     /**< IMU update rate */
#define GF_FUSION_GPS_RATE_HZ       10      /**< GPS update rate */
#define GF_FUSION_BARO_RATE_HZ      25      /**< Barometer update rate */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Sensor health status
 */
typedef enum {
    GF_SENSOR_OK,               /**< Sensor healthy */
    GF_SENSOR_DEGRADED,         /**< Reduced accuracy */
    GF_SENSOR_FAILED,           /**< Sensor failed */
    GF_SENSOR_NOT_PRESENT       /**< Sensor not detected */
} gf_sensor_health_t;

/**
 * @brief GPS fix type
 */
typedef enum {
    GF_GPS_NO_FIX,              /**< No fix */
    GF_GPS_FIX_2D,              /**< 2D fix (no altitude) */
    GF_GPS_FIX_3D,              /**< 3D autonomous fix */
    GF_GPS_FIX_DGPS,            /**< Differential GPS */
    GF_GPS_FIX_RTK_FLOAT,       /**< RTK float solution */
    GF_GPS_FIX_RTK_FIXED        /**< RTK fixed solution (cm accuracy) */
} gf_gps_fix_t;

/**
 * @brief Quaternion representation
 */
typedef struct {
    float w;                    /**< Scalar component */
    float x;                    /**< X component */
    float y;                    /**< Y component */
    float z;                    /**< Z component */
} gf_quaternion_t;

/**
 * @brief 3D vector
 */
typedef struct {
    float x;                    /**< X component */
    float y;                    /**< Y component */
    float z;                    /**< Z component */
} gf_vector3_t;

/**
 * @brief IMU raw measurements
 */
typedef struct {
    gf_vector3_t accel;         /**< Accelerometer (m/s²) */
    gf_vector3_t gyro;          /**< Gyroscope (rad/s) */
    gf_vector3_t mag;           /**< Magnetometer (gauss) */
    float temperature;          /**< IMU temperature (°C) */
    uint64_t timestamp_us;      /**< Measurement timestamp */
} gf_imu_data_t;

/**
 * @brief GPS measurement
 */
typedef struct {
    double latitude;            /**< Latitude (degrees) */
    double longitude;           /**< Longitude (degrees) */
    float altitude_msl;         /**< Altitude MSL (meters) */
    float vel_n;                /**< North velocity (m/s) */
    float vel_e;                /**< East velocity (m/s) */
    float vel_d;                /**< Down velocity (m/s) */
    float h_acc;                /**< Horizontal accuracy (m) */
    float v_acc;                /**< Vertical accuracy (m) */
    float s_acc;                /**< Speed accuracy (m/s) */
    gf_gps_fix_t fix_type;      /**< Fix type */
    uint8_t satellites;         /**< Satellites used */
    float hdop;                 /**< Horizontal DOP */
    float vdop;                 /**< Vertical DOP */
    uint64_t timestamp_us;      /**< Measurement timestamp */
} gf_gps_data_t;

/**
 * @brief Barometer measurement
 */
typedef struct {
    float pressure_pa;          /**< Pressure (Pascals) */
    float temperature_c;        /**< Temperature (°C) */
    float altitude_m;           /**< Calculated altitude (m) */
    uint64_t timestamp_us;      /**< Measurement timestamp */
} gf_baro_data_t;

/**
 * @brief Magnetometer calibration
 */
typedef struct {
    gf_vector3_t offset;        /**< Hard iron offset */
    float scale[3][3];          /**< Soft iron matrix */
    bool valid;                 /**< Calibration valid */
} gf_mag_cal_t;

/**
 * @brief Fusion state estimate
 */
typedef struct {
    gf_quaternion_t attitude;   /**< Attitude quaternion */
    gf_vector3_t position;      /**< Position NED (meters) */
    gf_vector3_t velocity;      /**< Velocity NED (m/s) */
    gf_vector3_t accel_bias;    /**< Accelerometer bias estimate */
    gf_vector3_t gyro_bias;     /**< Gyroscope bias estimate */
    float heading_mag;          /**< Magnetic heading (rad) */
    float heading_gps;          /**< GPS heading (rad) */
    double origin_lat;          /**< Origin latitude */
    double origin_lon;          /**< Origin longitude */
    float origin_alt;           /**< Origin altitude MSL */
} gf_fusion_state_t;

/**
 * @brief Estimation covariance (uncertainty)
 */
typedef struct {
    float attitude_var[3];      /**< Attitude variance (rad²) */
    float position_var[3];      /**< Position variance (m²) */
    float velocity_var[3];      /**< Velocity variance (m/s)² */
    float accel_bias_var[3];    /**< Accel bias variance */
    float gyro_bias_var[3];     /**< Gyro bias variance */
} gf_fusion_covariance_t;

/**
 * @brief Sensor fusion health status
 */
typedef struct {
    gf_sensor_health_t imu_health;      /**< IMU health */
    gf_sensor_health_t gps_health;      /**< GPS health */
    gf_sensor_health_t baro_health;     /**< Barometer health */
    gf_sensor_health_t mag_health;      /**< Magnetometer health */
    bool attitude_valid;                /**< Attitude estimate valid */
    bool position_valid;                /**< Position estimate valid */
    bool velocity_valid;                /**< Velocity estimate valid */
    float innovation_test_ratio;        /**< EKF innovation test ratio */
    uint32_t gps_denied_time_ms;        /**< Time without GPS */
} gf_fusion_health_t;

/**
 * @brief Fusion configuration
 */
typedef struct {
    float accel_noise;          /**< Accelerometer noise (m/s²) */
    float gyro_noise;           /**< Gyroscope noise (rad/s) */
    float gps_pos_noise;        /**< GPS position noise (m) */
    float gps_vel_noise;        /**< GPS velocity noise (m/s) */
    float baro_noise;           /**< Barometer noise (m) */
    float mag_noise;            /**< Magnetometer noise (gauss) */
    bool use_gps;               /**< Fuse GPS measurements */
    bool use_baro;              /**< Fuse barometer */
    bool use_mag;               /**< Fuse magnetometer */
    float gps_innovation_gate;  /**< GPS innovation gate threshold */
} gf_fusion_config_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize sensor fusion system
 * @param config Fusion configuration
 * @return 0 on success, negative error code on failure
 */
int gf_fusion_init(const gf_fusion_config_t* config);

/**
 * @brief Shutdown sensor fusion system
 */
void gf_fusion_deinit(void);

/**
 * @brief Process IMU measurement (call at IMU rate)
 * @param imu IMU data
 */
void gf_fusion_process_imu(const gf_imu_data_t* imu);

/**
 * @brief Process GPS measurement (call when GPS available)
 * @param gps GPS data
 */
void gf_fusion_process_gps(const gf_gps_data_t* gps);

/**
 * @brief Process barometer measurement
 * @param baro Barometer data
 */
void gf_fusion_process_baro(const gf_baro_data_t* baro);

/**
 * @brief Get current state estimate
 * @param[out] state Output state
 */
void gf_fusion_get_state(gf_fusion_state_t* state);

/**
 * @brief Get estimation covariance
 * @param[out] covariance Output covariance
 */
void gf_fusion_get_covariance(gf_fusion_covariance_t* covariance);

/**
 * @brief Get fusion health status
 * @param[out] health Output health status
 */
void gf_fusion_get_health(gf_fusion_health_t* health);

/**
 * @brief Set origin for local coordinate frame
 * @param lat Origin latitude (degrees)
 * @param lon Origin longitude (degrees)
 * @param alt Origin altitude MSL (meters)
 */
void gf_fusion_set_origin(double lat, double lon, float alt);

/**
 * @brief Reset filter with known attitude
 * @param attitude Initial attitude quaternion
 */
void gf_fusion_reset_attitude(const gf_quaternion_t* attitude);

/**
 * @brief Perform magnetometer calibration
 * @param samples Calibration samples (rotate in all directions)
 * @param num_samples Number of samples
 * @param[out] cal Output calibration
 * @return 0 on success, -1 if calibration fails
 */
int gf_fusion_calibrate_mag(const gf_vector3_t* samples, uint16_t num_samples, gf_mag_cal_t* cal);

/**
 * @brief Apply magnetometer calibration
 * @param cal Calibration to apply
 */
void gf_fusion_apply_mag_cal(const gf_mag_cal_t* cal);

/**
 * @brief Get Euler angles from current attitude
 * @param[out] roll Roll angle (radians)
 * @param[out] pitch Pitch angle (radians)
 * @param[out] yaw Yaw angle (radians)
 */
void gf_fusion_get_euler(float* roll, float* pitch, float* yaw);

/**
 * @brief Convert GPS to local NED position
 * @param gps GPS position
 * @param[out] ned Output NED position
 */
void gf_fusion_gps_to_ned(const gf_gps_data_t* gps, gf_vector3_t* ned);

/**
 * @brief Check if position estimate is reliable enough for autonomous flight
 * @return true if position reliable
 */
bool gf_fusion_position_ok_for_auto(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SENSOR_FUSION_H */
