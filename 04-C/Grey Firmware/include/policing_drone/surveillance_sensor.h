/**
 * @file surveillance_sensor.h
 * @brief Police Drone Surveillance Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Law enforcement agencies increasingly deploy drones for situational awareness,
 * search and rescue, and crowd monitoring. This sensor interface enables
 * multi-modal sensing with privacy-preserving features and chain-of-custody
 * logging for evidentiary use.
 * 
 * TECHNICAL SCOPE:
 * - Multi-spectral imaging (visible, thermal, NIR)
 * - License plate recognition preprocessing
 * - Face detection with privacy masking options
 * - Audio capture with gunshot detection
 * - GPS/INS integration for evidence geolocation
 * - Real-time video streaming with encryption
 * 
 * PRIVACY FEATURES:
 * - Configurable face blur zones
 * - Geofenced recording restrictions
 * - Audit logging for all capture events
 * - Role-based access to stored footage
 * 
 * STANDARDS COMPLIANCE:
 * - NIJ Standard 0604.01 (Body-worn cameras)
 * - NIST SP 800-53 (Security controls)
 * - FAA Part 107 (UAS operations)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_SURVEILLANCE_SENSOR_H
#define GF_SURVEILLANCE_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define SURV_MAX_CAMERAS        4     /**< Maximum camera modules */
#define SURV_MAX_DETECTIONS     64    /**< Max detections per frame */
#define SURV_GEOFENCE_ZONES     16    /**< Geofence zone count */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Camera/sensor type */
typedef enum {
    SURV_CAMERA_VISIBLE,           /**< Standard RGB camera */
    SURV_CAMERA_THERMAL,           /**< LWIR thermal */
    SURV_CAMERA_NIR,               /**< Near-infrared */
    SURV_CAMERA_MULTISPECTRAL,     /**< Multi-band imaging */
    SURV_MIC_ARRAY                 /**< Acoustic sensor array */
} surv_sensor_type_t;

/** Detection type */
typedef enum {
    SURV_DETECT_PERSON,
    SURV_DETECT_VEHICLE,
    SURV_DETECT_LICENSE_PLATE,
    SURV_DETECT_FACE,
    SURV_DETECT_GUNSHOT,
    SURV_DETECT_FIRE,
    SURV_DETECT_CROWD
} surv_detection_t;

/** Recording mode */
typedef enum {
    SURV_REC_OFF,
    SURV_REC_CONTINUOUS,
    SURV_REC_EVENT_TRIGGERED,
    SURV_REC_BUFFERED            /**< Pre/post event buffer */
} surv_rec_mode_t;

/** Privacy mode */
typedef enum {
    SURV_PRIVACY_NONE,
    SURV_PRIVACY_FACE_BLUR,
    SURV_PRIVACY_BODY_BLUR,
    SURV_PRIVACY_ZONE_MASK
} surv_privacy_t;

/** Detection result */
typedef struct {
    surv_detection_t type;
    float confidence;
    uint16_t bbox_x, bbox_y;
    uint16_t bbox_w, bbox_h;
    float latitude;
    float longitude;
    uint64_t timestamp_us;
    bool privacy_applied;
} surv_detection_result_t;

/** Sensor status */
typedef struct {
    surv_sensor_type_t type;
    bool healthy;
    surv_rec_mode_t rec_mode;
    surv_privacy_t privacy_mode;
    uint32_t frames_captured;
    float storage_remaining_pct;
    float temp_c;
    bool streaming_active;
} surv_sensor_status_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize surveillance sensors */
int surv_init(void);

/** Configure camera */
int surv_config_camera(uint8_t cam_id, surv_sensor_type_t type, surv_rec_mode_t mode);

/** Set privacy mode */
int surv_set_privacy(uint8_t cam_id, surv_privacy_t privacy);

/** Start recording */
int surv_start_recording(uint8_t cam_id);

/** Stop recording */
int surv_stop_recording(uint8_t cam_id);

/** Get detections */
int surv_get_detections(surv_detection_result_t *results, uint8_t max_count);

/** Get sensor status */
int surv_get_status(uint8_t cam_id, surv_sensor_status_t *status);

/** Process frame (run detection) */
int surv_process_frame(uint8_t cam_id, uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_SURVEILLANCE_SENSOR_H */
