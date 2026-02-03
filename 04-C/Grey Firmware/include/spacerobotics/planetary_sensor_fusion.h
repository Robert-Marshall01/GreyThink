/**
 * @file planetary_sensor_fusion.h
 * @brief Planetary Sensor Fusion Module Interface
 * 
 * INDUSTRY RELEVANCE:
 * Multi-sensor fusion for planetary exploration enables:
 * - Autonomous navigation on unknown terrain
 * - Hazard detection and avoidance (rocks, slopes, soft soil)
 * - Science target identification and prioritization
 * - Localization without GPS (visual odometry, SLAM)
 * - Atmospheric and geological data collection
 * - Resource detection (water ice, minerals)
 * 
 * This module demonstrates expertise in:
 * - Extended Kalman Filter for state estimation
 * - Visual-inertial odometry (VIO) algorithms
 * - LiDAR point cloud processing
 * - Terrain classification using multi-spectral imaging
 * - Delayed measurement handling (comm latency)
 * - Uncertainty quantification for autonomous decisions
 */

#ifndef GF_PLANETARY_SENSOR_FUSION_H
#define GF_PLANETARY_SENSOR_FUSION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_PSF_MAX_CAMERAS          4
#define GF_PSF_MAX_LIDAR_POINTS     1024
#define GF_PSF_MAX_SPECTRAL_BANDS   16

typedef enum {
    GF_PSF_OK = 0,
    GF_PSF_ERROR_NOT_INITIALIZED,
    GF_PSF_ERROR_SENSOR_TIMEOUT,
    GF_PSF_ERROR_CALIBRATION,
    GF_PSF_ERROR_NO_FEATURES,
    GF_PSF_ERROR_DIVERGENCE,
    GF_PSF_WARN_HIGH_UNCERTAINTY
} gf_psf_status_t;

typedef enum {
    GF_PSF_TERRAIN_UNKNOWN = 0,
    GF_PSF_TERRAIN_BEDROCK,
    GF_PSF_TERRAIN_SAND,
    GF_PSF_TERRAIN_GRAVEL,
    GF_PSF_TERRAIN_REGOLITH,
    GF_PSF_TERRAIN_ICE,
    GF_PSF_TERRAIN_HAZARD
} gf_psf_terrain_t;

typedef struct {
    float x, y, z;              /* Position in local frame (m) */
    float roll, pitch, yaw;     /* Orientation (rad) */
    float vx, vy, vz;           /* Velocity (m/s) */
    float covariance[36];       /* 6x6 covariance matrix */
    uint32_t timestamp_ms;
} gf_psf_pose_t;

typedef struct {
    float ax, ay, az;           /* Accelerometer (m/s²) */
    float gx, gy, gz;           /* Gyroscope (rad/s) */
    float temperature_c;
    uint32_t timestamp_ms;
} gf_psf_imu_t;

typedef struct {
    uint16_t point_count;
    float points[GF_PSF_MAX_LIDAR_POINTS][3];   /* x,y,z per point */
    float intensities[GF_PSF_MAX_LIDAR_POINTS];
    uint32_t timestamp_ms;
} gf_psf_lidar_t;

typedef struct {
    uint16_t width, height;
    uint8_t camera_id;
    float focal_length_px;
    void* image_data;
    uint32_t timestamp_ms;
} gf_psf_camera_t;

typedef struct {
    float distance_m;
    float slope_deg;
    gf_psf_terrain_t terrain;
    float traversability;       /* 0.0 = impassable, 1.0 = easy */
    float confidence;
} gf_psf_terrain_assessment_t;

typedef struct {
    bool use_imu;
    bool use_wheel_odometry;
    bool use_visual_odometry;
    bool use_lidar;
    float imu_noise_accel;
    float imu_noise_gyro;
    float wheel_slip_factor;
    uint16_t vo_features_min;
} gf_psf_config_t;

gf_psf_status_t gf_psf_init(const gf_psf_config_t* config);
void gf_psf_shutdown(void);
gf_psf_status_t gf_psf_update_imu(const gf_psf_imu_t* imu);
gf_psf_status_t gf_psf_update_wheel_odom(float left_m, float right_m, uint32_t dt_ms);
gf_psf_status_t gf_psf_update_camera(const gf_psf_camera_t* camera);
gf_psf_status_t gf_psf_update_lidar(const gf_psf_lidar_t* lidar);
gf_psf_status_t gf_psf_get_pose(gf_psf_pose_t* pose);
gf_psf_status_t gf_psf_assess_terrain(float range_m, float angle_deg, gf_psf_terrain_assessment_t* assessment);
gf_psf_status_t gf_psf_detect_hazards(float* hazard_map, uint16_t width, uint16_t height);
float gf_psf_get_uncertainty(void);
gf_psf_status_t gf_psf_reset(const gf_psf_pose_t* initial_pose);

#ifdef __cplusplus
}
#endif

#endif /* GF_PLANETARY_SENSOR_FUSION_H */
