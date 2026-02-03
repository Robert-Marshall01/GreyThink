/**
 * @file leak_detection.h
 * @brief Pipeline Leak Detection System Interface
 * 
 * INDUSTRY RELEVANCE:
 * Pipeline leaks cause billions in environmental damage and product loss
 * annually. Early detection using computational pipeline monitoring (CPM)
 * reduces response time from hours to minutes. This module implements
 * multiple detection methods for comprehensive coverage.
 * 
 * KEY CAPABILITIES:
 * - Mass balance leak detection (product loss accounting)
 * - Pressure point analysis (PPA) for rapid detection
 * - Negative pressure wave (NPW) detection for ruptures
 * - Statistical pattern recognition for small leaks
 * - Pig tracking integration for pig-based inspection
 * - Multi-zone segment monitoring
 * 
 * DETECTION METHODS:
 * - Rate of change monitoring
 * - Flow imbalance analysis
 * - Pressure transient detection
 * - Acoustic emission monitoring
 * - Fiber optic distributed sensing
 * 
 * STANDARDS COMPLIANCE:
 * - API 1130 (Computational Pipeline Monitoring)
 * - API 1160 (Managing System Integrity)
 * - PHMSA Regulations (49 CFR 195)
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_LEAK_DETECTION_H
#define GF_LEAK_DETECTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Leak detection method */
typedef enum {
    LEAK_METHOD_MASS_BALANCE,   /**< Input/output comparison */
    LEAK_METHOD_PPA,            /**< Pressure point analysis */
    LEAK_METHOD_NPW,            /**< Negative pressure wave */
    LEAK_METHOD_ACOUSTIC,       /**< Acoustic emission */
    LEAK_METHOD_STATISTICAL     /**< Pattern recognition */
} leak_method_t;

/** Leak severity classification */
typedef enum {
    LEAK_SEVERITY_NONE,
    LEAK_SEVERITY_MINOR,        /**< <1% of flow */
    LEAK_SEVERITY_MODERATE,     /**< 1-5% of flow */
    LEAK_SEVERITY_MAJOR,        /**< 5-20% of flow */
    LEAK_SEVERITY_RUPTURE       /**< >20% or pressure event */
} leak_severity_t;

/** Leak alert */
typedef struct {
    uint32_t alert_id;
    leak_method_t method;       /**< Detection method triggered */
    leak_severity_t severity;
    float estimated_location_km; /**< Distance from upstream meter */
    float estimated_rate;       /**< m³/h or kg/h */
    float confidence;           /**< 0.0-1.0 */
    uint32_t timestamp;
    uint8_t segment_id;
} leak_alert_t;

/** Segment configuration */
typedef struct {
    uint8_t segment_id;
    float length_km;
    float diameter_mm;
    uint8_t upstream_sensor;    /**< Upstream flow sensor ID */
    uint8_t downstream_sensor;  /**< Downstream flow sensor ID */
    float imbalance_threshold;  /**< % threshold for alarm */
    uint32_t settling_time_s;   /**< Time for transient settling */
} segment_config_t;

/** Detection system status */
typedef struct {
    bool system_armed;
    uint8_t segments_monitored;
    uint8_t active_alerts;
    uint32_t uptime_s;
    float avg_imbalance_pct;
    uint32_t alerts_total;
    uint32_t false_positive_count;
} leak_system_status_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize leak detection system
 * @param pipeline_id Pipeline identifier
 * @return 0 on success, negative on error
 */
int leak_init(uint32_t pipeline_id);

/**
 * @brief Configure pipeline segment
 * @param config Segment configuration
 * @return 0 on success, negative on error
 */
int leak_configure_segment(const segment_config_t* config);

/**
 * @brief Arm leak detection on segment
 * @param segment_id Segment to arm
 * @return 0 on success, negative on error
 */
int leak_arm_segment(uint8_t segment_id);

/**
 * @brief Disarm leak detection on segment
 * @param segment_id Segment to disarm
 * @return 0 on success, negative on error
 */
int leak_disarm_segment(uint8_t segment_id);

/**
 * @brief Process detection algorithms (call periodically)
 * @param delta_ms Time since last call
 * @return 0 on success, negative on error
 */
int leak_process(uint32_t delta_ms);

/**
 * @brief Get active alerts
 * @param alerts Output alert array
 * @param max_alerts Maximum alerts to return
 * @param count Output actual count
 * @return 0 on success, negative on error
 */
int leak_get_alerts(leak_alert_t* alerts, uint8_t max_alerts, uint8_t* count);

/**
 * @brief Acknowledge and clear alert
 * @param alert_id Alert to acknowledge
 * @return 0 on success, negative on error
 */
int leak_acknowledge_alert(uint32_t alert_id);

/**
 * @brief Get system status
 * @param status Output status
 * @return 0 on success, negative on error
 */
int leak_get_status(leak_system_status_t* status);

/**
 * @brief Shutdown leak detection
 * @return 0 on success, negative on error
 */
int leak_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_LEAK_DETECTION_H */
