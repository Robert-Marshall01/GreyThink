/**
 * @file victim_detection.h
 * @brief Victim Detection Sensor Fusion Stub
 * 
 * Industry Relevance:
 * Multi-sensor victim detection is a critical capability for SAR operations.
 * This module fuses thermal, visual, acoustic, and motion sensors for:
 * - Human body thermal signature detection
 * - Motion detection in debris fields
 * - Audio analysis for voice/distress signals
 * - AI-based victim classification and prioritization
 * 
 * Applications: Earthquake response, avalanche rescue, maritime rescue, fire rescue
 * 
 * @author Grey Firmware Project
 */

#ifndef VICTIM_DETECTION_H
#define VICTIM_DETECTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Detection confidence level */
typedef enum {
    DETECTION_NONE,
    DETECTION_LOW,           /**< Possible detection */
    DETECTION_MEDIUM,        /**< Probable detection */
    DETECTION_HIGH,          /**< Confirmed detection */
    DETECTION_VERIFIED       /**< Visually verified */
} detection_confidence_t;

/** Sensor modality */
typedef enum {
    SENSOR_THERMAL_IR,       /**< Thermal infrared */
    SENSOR_VISUAL_RGB,       /**< Visual RGB camera */
    SENSOR_MOTION_RADAR,     /**< Motion detection radar */
    SENSOR_ACOUSTIC,         /**< Microphone array */
    SENSOR_LIDAR             /**< LiDAR depth sensing */
} sensor_modality_t;

/** Detected victim record */
typedef struct {
    uint16_t detection_id;   /**< Unique detection ID */
    double latitude;         /**< Estimated latitude */
    double longitude;        /**< Estimated longitude */
    float altitude_m;        /**< Estimated altitude */
    detection_confidence_t confidence; /**< Confidence level */
    uint8_t sensor_mask;     /**< Sensors that detected (bitmask) */
    float thermal_signature; /**< Thermal delta from ambient (°C) */
    float motion_magnitude;  /**< Motion amplitude */
    float audio_level_db;    /**< Audio detection level (dB) */
    uint32_t timestamp;      /**< Detection timestamp */
} victim_detection_t;

/** Detection system statistics */
typedef struct {
    uint16_t total_detections; /**< Total detections this mission */
    uint16_t verified_count;   /**< Verified victims */
    uint16_t false_positive;   /**< False positives */
    float avg_confidence;      /**< Average confidence */
    uint32_t processing_ms;    /**< Last frame processing time */
} detection_stats_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize victim detection system
 * @return 0 on success, negative on error
 */
int victim_detection_init(void);

/**
 * @brief Enable/disable sensor modality
 * @param modality Sensor type
 * @param enable Enable or disable
 * @return 0 on success, negative on error
 */
int victim_detection_set_sensor(sensor_modality_t modality, bool enable);

/**
 * @brief Process current sensor frame
 * @param elapsed_ms Time since last call
 * @return Number of new detections, negative on error
 */
int victim_detection_update(uint32_t elapsed_ms);

/**
 * @brief Get detection by ID
 * @param detection_id Detection to retrieve
 * @param detection Output detection data
 * @return 0 on success, negative on error
 */
int victim_detection_get(uint16_t detection_id, victim_detection_t *detection);

/**
 * @brief Update detection status (verification)
 * @param detection_id Detection to update
 * @param confidence New confidence level
 * @return 0 on success, negative on error
 */
int victim_detection_verify(uint16_t detection_id, detection_confidence_t confidence);

/**
 * @brief Get detection statistics
 * @param stats Output statistics
 * @return 0 on success, negative on error
 */
int victim_detection_get_stats(detection_stats_t *stats);

/**
 * @brief Shutdown detection system
 */
void victim_detection_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* VICTIM_DETECTION_H */
