/**
 * @file nav_fusion.h
 * @brief Navigation Sensor Fusion for Marine Robotics
 * 
 * INDUSTRY RELEVANCE:
 * Underwater navigation is inherently challenging due to GPS denial, making
 * sensor fusion critical for AUV operations. This module fuses DVL, IMU, 
 * depth sensor, and acoustic positioning data using extended Kalman filtering
 * to provide robust position estimates for mission-critical operations.
 * 
 * KEY CAPABILITIES:
 * - Extended Kalman Filter (EKF) for 6-DOF state estimation
 * - DVL bottom-track and water-track velocity integration
 * - IMU bias estimation and compensation
 * - USBL/LBL acoustic fix integration
 * - Pressure-based depth correction
 * - Dead reckoning with drift bounds
 * 
 * SENSOR INPUTS:
 * - IMU (accelerometer, gyroscope, magnetometer)
 * - DVL (Doppler Velocity Log)
 * - Pressure sensor (depth)
 * - USBL/LBL acoustic positioning
 * - GPS (when surfaced)
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_NAV_FUSION_H
#define GF_NAV_FUSION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Navigation solution quality */
typedef enum {
    NAV_QUAL_EXCELLENT,    /**< Multiple independent sources */
    NAV_QUAL_GOOD,         /**< DVL + IMU fusion */
    NAV_QUAL_DEGRADED,     /**< Dead reckoning only */
    NAV_QUAL_INVALID       /**< No valid solution */
} nav_quality_t;

/** IMU measurement */
typedef struct {
    float accel[3];        /**< Acceleration m/s^2 (x,y,z) */
    float gyro[3];         /**< Angular rate rad/s (x,y,z) */
    float mag[3];          /**< Magnetometer uT (x,y,z) */
    uint32_t timestamp_ms;
} imu_meas_t;

/** DVL measurement */
typedef struct {
    float velocity[3];     /**< Velocity m/s (x,y,z) */
    float altitude;        /**< Bottom altitude in meters */
    bool bottom_lock;      /**< Bottom track valid */
    uint32_t timestamp_ms;
} dvl_meas_t;

/** Acoustic position fix */
typedef struct {
    double latitude;
    double longitude;
    float depth_m;
    float error_m;         /**< Position error estimate */
    uint32_t timestamp_ms;
} acoustic_fix_t;

/** Navigation state estimate */
typedef struct {
    double latitude;
    double longitude;
    float depth_m;
    float position_error_m;  /**< Estimated position error */
    float velocity[3];       /**< Velocity in body frame */
    float attitude[3];       /**< Roll, pitch, yaw (radians) */
    nav_quality_t quality;
    uint32_t timestamp_ms;
} nav_state_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize navigation fusion system
 * @param initial_lat Initial latitude estimate
 * @param initial_lon Initial longitude estimate
 * @return 0 on success, negative on error
 */
int nav_fusion_init(double initial_lat, double initial_lon);

/**
 * @brief Process IMU measurement
 * @param meas IMU measurement
 * @return 0 on success, negative on error
 */
int nav_process_imu(const imu_meas_t* meas);

/**
 * @brief Process DVL measurement
 * @param meas DVL measurement
 * @return 0 on success, negative on error
 */
int nav_process_dvl(const dvl_meas_t* meas);

/**
 * @brief Process acoustic position fix
 * @param fix Acoustic fix
 * @return 0 on success, negative on error
 */
int nav_process_acoustic(const acoustic_fix_t* fix);

/**
 * @brief Process depth sensor measurement
 * @param depth_m Measured depth in meters
 * @param timestamp_ms Measurement timestamp
 * @return 0 on success, negative on error
 */
int nav_process_depth(float depth_m, uint32_t timestamp_ms);

/**
 * @brief Get current navigation state
 * @param state Output navigation state
 * @return 0 on success, negative on error
 */
int nav_get_state(nav_state_t* state);

/**
 * @brief Reset navigation filter
 * @param lat New initial latitude
 * @param lon New initial longitude
 * @return 0 on success, negative on error
 */
int nav_reset(double lat, double lon);

/**
 * @brief Shutdown navigation fusion
 * @return 0 on success, negative on error
 */
int nav_fusion_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_NAV_FUSION_H */
