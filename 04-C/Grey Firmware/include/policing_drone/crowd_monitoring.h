/**
 * @file crowd_monitoring.h
 * @brief Crowd Monitoring Telemetry Module
 * 
 * INDUSTRY RELEVANCE:
 * Public safety operations require real-time crowd density estimation
 * and flow analysis for event management, emergency response, and
 * tactical deployment. This module provides AI-driven crowd analytics
 * with predictive capabilities.
 * 
 * TECHNICAL SCOPE:
 * - Crowd density estimation (people/m²)
 * - Flow direction and velocity analysis
 * - Bottleneck and congestion detection
 * - Anomaly detection (sudden dispersal, stampede risk)
 * - Heat mapping for resource deployment
 * - Integration with incident command systems
 * 
 * ANALYTICS OUTPUTS:
 * - Real-time density maps
 * - Flow velocity vectors
 * - Predicted crowd movement (15-minute horizon)
 * - Risk score per zone
 * - Recommended patrol positions
 * 
 * STANDARDS COMPLIANCE:
 * - NFPA 1600 (Emergency management)
 * - ISO 22320 (Emergency management guidelines)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CROWD_MONITORING_H
#define GF_CROWD_MONITORING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define CROWD_MAX_ZONES         32    /**< Maximum monitoring zones */
#define CROWD_HEATMAP_RES       64    /**< Heatmap grid resolution */
#define CROWD_HISTORY_FRAMES    300   /**< 5 minutes at 1Hz */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Crowd density level */
typedef enum {
    CROWD_DENSITY_SPARSE,          /**< < 0.5 people/m² */
    CROWD_DENSITY_LIGHT,           /**< 0.5 - 1.5 people/m² */
    CROWD_DENSITY_MODERATE,        /**< 1.5 - 3.0 people/m² */
    CROWD_DENSITY_DENSE,           /**< 3.0 - 5.0 people/m² */
    CROWD_DENSITY_CRITICAL         /**< > 5.0 people/m² (dangerous) */
} crowd_density_t;

/** Crowd behavior classification */
typedef enum {
    CROWD_BEHAVIOR_NORMAL,
    CROWD_BEHAVIOR_GATHERING,
    CROWD_BEHAVIOR_DISPERSING,
    CROWD_BEHAVIOR_RUSHING,
    CROWD_BEHAVIOR_CONFRONTATION,
    CROWD_BEHAVIOR_PANIC
} crowd_behavior_t;

/** Risk level */
typedef enum {
    CROWD_RISK_LOW,
    CROWD_RISK_MODERATE,
    CROWD_RISK_ELEVATED,
    CROWD_RISK_HIGH,
    CROWD_RISK_CRITICAL
} crowd_risk_t;

/** Zone crowd metrics */
typedef struct {
    uint8_t zone_id;
    float density_per_m2;
    crowd_density_t density_level;
    float flow_velocity_m_s;
    float flow_direction_deg;      /**< 0 = North */
    crowd_behavior_t behavior;
    crowd_risk_t risk_level;
    uint32_t estimated_count;
    float congestion_index;        /**< 0-1, higher = more congested */
} crowd_zone_metrics_t;

/** Overall crowd status */
typedef struct {
    uint8_t zones_monitored;
    uint32_t total_count;
    crowd_risk_t highest_risk;
    crowd_behavior_t dominant_behavior;
    float heatmap[CROWD_HEATMAP_RES][CROWD_HEATMAP_RES];
    uint8_t critical_zones[8];
    uint8_t critical_zone_count;
    uint64_t timestamp_ms;
} crowd_status_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize crowd monitoring */
int crowd_init(void);

/** Define monitoring zone */
int crowd_define_zone(uint8_t zone_id, float lat, float lon, float radius_m);

/** Update zone with vision data */
int crowd_update_zone(uint8_t zone_id, uint32_t person_count, float avg_velocity);

/** Get zone metrics */
int crowd_get_zone_metrics(uint8_t zone_id, crowd_zone_metrics_t *metrics);

/** Get overall status */
int crowd_get_status(crowd_status_t *status);

/** Predict crowd movement */
int crowd_predict(uint8_t zone_id, uint32_t horizon_s, float *predicted_density);

/** Generate telemetry */
int crowd_generate_telemetry(uint8_t *buffer, size_t max_len);

/** Process analytics cycle */
int crowd_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_CROWD_MONITORING_H */
