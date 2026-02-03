/**
 * @file distributed_fusion.h
 * @brief Distributed Sensor Fusion for Edge Robotics Swarms
 * 
 * @details
 * Collaborative sensor fusion across multiple agents in a swarm.
 * Implements decentralized estimation, map merging, and collective
 * perception for enhanced situational awareness.
 * 
 * INDUSTRY RELEVANCE:
 * - Multi-robot SLAM
 * - Cooperative perception (autonomous vehicles)
 * - Distributed surveillance
 * - Environmental monitoring networks
 * - Collaborative inspection robots
 * 
 * KEY FEATURES:
 * - Decentralized Kalman filtering
 * - Map merging algorithms
 * - Cooperative localization
 * - Feature sharing
 * - Consensus-based estimation
 * - Bandwidth-efficient updates
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_DISTRIBUTED_FUSION_H
#define GF_DISTRIBUTED_FUSION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum sensor sources */
#define GF_DFUSION_MAX_SOURCES      32

/** Maximum features */
#define GF_DFUSION_MAX_FEATURES     256

/** Map grid resolution (mm) */
#define GF_DFUSION_GRID_RES_MM      100

/** Maximum map size (cells) */
#define GF_DFUSION_MAP_SIZE         1024

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Fusion status codes
 */
typedef enum {
    GF_DFUSION_OK = 0,
    GF_DFUSION_ERROR_NOT_INIT,
    GF_DFUSION_ERROR_NULL_PTR,
    GF_DFUSION_ERROR_NO_DATA,
    GF_DFUSION_ERROR_MAP_FULL,
    GF_DFUSION_ERROR_DIVERGED,
    GF_DFUSION_WARN_LOW_CONFIDENCE,
    GF_DFUSION_WARN_STALE_DATA
} gf_dfusion_status_t;

/**
 * @brief Sensor types
 */
typedef enum {
    GF_DFUSION_SENSOR_LIDAR,
    GF_DFUSION_SENSOR_CAMERA,
    GF_DFUSION_SENSOR_RADAR,
    GF_DFUSION_SENSOR_ULTRASONIC,
    GF_DFUSION_SENSOR_IMU,
    GF_DFUSION_SENSOR_GPS,
    GF_DFUSION_SENSOR_ODOMETRY
} gf_dfusion_sensor_t;

/**
 * @brief Feature type
 */
typedef enum {
    GF_DFUSION_FEAT_POINT,        /**< Point feature */
    GF_DFUSION_FEAT_LINE,         /**< Line segment */
    GF_DFUSION_FEAT_CORNER,       /**< Corner/vertex */
    GF_DFUSION_FEAT_PLANE,        /**< Planar region */
    GF_DFUSION_FEAT_OBJECT        /**< Detected object */
} gf_dfusion_feature_type_t;

/**
 * @brief Sensor observation
 */
typedef struct {
    uint8_t source_id[8];         /**< Source agent ID */
    gf_dfusion_sensor_t sensor;   /**< Sensor type */
    uint32_t timestamp_ms;        /**< Observation time */
    int32_t pos_x_mm;             /**< Observer X position */
    int32_t pos_y_mm;             /**< Observer Y position */
    int16_t heading_deg;          /**< Observer heading */
    uint8_t* data;                /**< Observation data */
    uint16_t data_len;            /**< Data length */
    float confidence;             /**< Confidence (0-1) */
} gf_dfusion_observation_t;

/**
 * @brief Feature descriptor
 */
typedef struct {
    uint32_t feature_id;          /**< Feature identifier */
    gf_dfusion_feature_type_t type; /**< Feature type */
    int32_t pos_x_mm;             /**< Feature X position */
    int32_t pos_y_mm;             /**< Feature Y position */
    int32_t pos_z_mm;             /**< Feature Z position */
    float covariance[9];          /**< 3x3 covariance */
    uint8_t observation_count;    /**< Times observed */
    float confidence;             /**< Confidence (0-1) */
} gf_dfusion_feature_t;

/**
 * @brief Map cell
 */
typedef struct {
    uint8_t occupancy;            /**< Occupancy probability (0-255) */
    uint8_t confidence;           /**< Confidence (0-255) */
    uint8_t height_cm;            /**< Height estimate (cm) */
    bool explored;                /**< Cell explored */
} gf_dfusion_cell_t;

/**
 * @brief Fusion estimate
 */
typedef struct {
    int32_t est_x_mm;             /**< Estimated X */
    int32_t est_y_mm;             /**< Estimated Y */
    int32_t est_z_mm;             /**< Estimated Z */
    int16_t heading_deg;          /**< Estimated heading */
    float confidence;             /**< Overall confidence */
    uint32_t feature_count;       /**< Tracked features */
    float map_coverage_pct;       /**< Map coverage */
} gf_dfusion_estimate_t;

/**
 * @brief Feature callback
 */
typedef void (*gf_dfusion_feature_cb_t)(const gf_dfusion_feature_t* feature,
                                         void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize distributed fusion
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_init(void);

/**
 * @brief Shutdown distributed fusion
 */
void gf_dfusion_shutdown(void);

/**
 * @brief Submit local observation
 * @param observation Sensor observation
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_submit(const gf_dfusion_observation_t* observation);

/**
 * @brief Receive remote observation
 * @param observation Remote observation
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_receive(const gf_dfusion_observation_t* observation);

/**
 * @brief Get fused estimate
 * @param estimate Output estimate
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_get_estimate(gf_dfusion_estimate_t* estimate);

/**
 * @brief Get feature
 * @param feature_id Feature ID
 * @param feature Output feature
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_get_feature(uint32_t feature_id,
                                            gf_dfusion_feature_t* feature);

/**
 * @brief Query map cell
 * @param x_mm X coordinate
 * @param y_mm Y coordinate
 * @param cell Output cell
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_query_map(int32_t x_mm, int32_t y_mm,
                                          gf_dfusion_cell_t* cell);

/**
 * @brief Export map region
 * @param x_min_mm Minimum X
 * @param y_min_mm Minimum Y
 * @param x_max_mm Maximum X
 * @param y_max_mm Maximum Y
 * @param cells Output cell array
 * @param max_cells Maximum cells
 * @param count Actual count
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_export_map(int32_t x_min_mm, int32_t y_min_mm,
                                           int32_t x_max_mm, int32_t y_max_mm,
                                           gf_dfusion_cell_t* cells,
                                           uint32_t max_cells, uint32_t* count);

/**
 * @brief Merge remote map
 * @param cells Remote cells
 * @param count Cell count
 * @param transform Coordinate transform
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_merge_map(const gf_dfusion_cell_t* cells,
                                          uint32_t count, const float* transform);

/**
 * @brief Register feature callback
 * @param callback Feature callback
 * @param user_data User context
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_register_callback(gf_dfusion_feature_cb_t callback,
                                                  void* user_data);

/**
 * @brief Process fusion
 * @return Status code
 */
gf_dfusion_status_t gf_dfusion_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_DISTRIBUTED_FUSION_H */
