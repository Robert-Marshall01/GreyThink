/**
 * @file orbital_radar.h
 * @brief Orbital Radar Sensor Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Space debris tracking is critical for satellite operators, space stations,
 * and launch providers. With over 36,000 tracked objects and millions of
 * smaller fragments, collision avoidance is essential for space sustainability.
 * Companies like LeoLabs, ExoAnalytic, and government agencies (18th Space
 * Defense Squadron) operate ground and space-based radar networks.
 * 
 * This stub demonstrates embedded firmware for:
 * - High-frequency radar signal acquisition
 * - Real-time object detection and tracking
 * - Integration with Space Surveillance Network (SSN)
 * - Conjunction assessment data generation
 * 
 * STANDARDS:
 * - CCSDS 508.0-B (Orbit Data Messages)
 * - Space Data Association (SDA) protocols
 * - US Space Command catalog integration
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ORBITAL_RADAR_H
#define GF_ORBITAL_RADAR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration
 * ============================================================================ */

#define RADAR_MAX_TRACKS          1024   /**< Maximum simultaneous tracks */
#define RADAR_MAX_DETECTIONS      256    /**< Detections per scan */
#define RADAR_RANGE_MIN_KM        100    /**< Minimum detection range */
#define RADAR_RANGE_MAX_KM        40000  /**< Maximum detection range (GEO) */
#define RADAR_UPDATE_RATE_HZ      10     /**< Tracking update rate */

/* ============================================================================
 * Types
 * ============================================================================ */

/** Radar operating mode */
typedef enum {
    RADAR_MODE_STANDBY,
    RADAR_MODE_SEARCH,       /**< Wide-area search scan */
    RADAR_MODE_TRACK,        /**< Dedicated object tracking */
    RADAR_MODE_FENCE,        /**< Space fence mode */
    RADAR_MODE_CALIBRATION,
    RADAR_MODE_MAINTENANCE
} radar_mode_t;

/** Radar band selection */
typedef enum {
    RADAR_BAND_S,            /**< S-band (2-4 GHz) */
    RADAR_BAND_C,            /**< C-band (4-8 GHz) */
    RADAR_BAND_X,            /**< X-band (8-12 GHz) - high precision */
    RADAR_BAND_KU            /**< Ku-band (12-18 GHz) - small debris */
} radar_band_t;

/** Detection confidence level */
typedef enum {
    DETECT_CONF_LOW,         /**< Single return, possible noise */
    DETECT_CONF_MEDIUM,      /**< Multiple returns, correlating */
    DETECT_CONF_HIGH,        /**< Confirmed track, catalog match */
    DETECT_CONF_VERIFIED     /**< Cross-validated with other sensors */
} detection_confidence_t;

/** Raw radar detection */
typedef struct {
    uint32_t detection_id;
    uint64_t timestamp_us;
    float range_km;           /**< Slant range to object */
    float range_rate_km_s;    /**< Range rate (velocity component) */
    float azimuth_deg;        /**< Azimuth angle */
    float elevation_deg;      /**< Elevation angle */
    float rcs_dbsm;           /**< Radar cross section (dBsm) */
    float snr_db;             /**< Signal-to-noise ratio */
    detection_confidence_t confidence;
} radar_detection_t;

/** Radar sensor configuration */
typedef struct {
    radar_band_t band;
    float tx_power_kw;        /**< Transmit power */
    float antenna_gain_db;
    float pulse_width_us;
    float prf_hz;             /**< Pulse repetition frequency */
    float min_rcs_dbsm;       /**< Minimum detectable RCS */
    bool doppler_enabled;
    bool mtd_enabled;         /**< Moving target detection */
} radar_config_t;

/** Radar status */
typedef struct {
    radar_mode_t mode;
    uint32_t tracks_active;
    uint32_t detections_this_scan;
    float coverage_az_deg;
    float coverage_el_deg;
    bool transmitter_healthy;
    bool receiver_healthy;
    float temperature_c;
} radar_status_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize radar sensor
 * @return 0 on success, negative on error
 */
int radar_init(void);

/**
 * @brief Configure radar parameters
 * @param config Configuration structure
 * @return 0 on success
 */
int radar_configure(const radar_config_t *config);

/**
 * @brief Set operating mode
 * @param mode Target operating mode
 * @return 0 on success
 */
int radar_set_mode(radar_mode_t mode);

/**
 * @brief Get latest detections
 * @param detections Output buffer
 * @param max_count Maximum detections to return
 * @return Number of detections, negative on error
 */
int radar_get_detections(radar_detection_t *detections, uint16_t max_count);

/**
 * @brief Get radar status
 * @param status Output status structure
 * @return 0 on success
 */
int radar_get_status(radar_status_t *status);

/**
 * @brief Process radar data (call periodically)
 * @param time_ms Current system time
 * @return 0 on success
 */
int radar_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_ORBITAL_RADAR_H */
