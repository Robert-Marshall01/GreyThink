/**
 * @file customer_telemetry.h
 * @brief Retail Customer Telemetry Collector
 * 
 * INDUSTRY RELEVANCE:
 * Brick-and-mortar retailers need customer behavior analytics to compete
 * with e-commerce. This module enables privacy-compliant tracking of
 * foot traffic, dwell time, and engagement patterns for store optimization
 * and personalized experiences.
 * 
 * TECHNICAL SCOPE:
 * - Anonymous foot traffic counting
 * - Dwell time measurement by zone
 * - Customer journey mapping
 * - Engagement metrics (product interaction)
 * - Queue wait time estimation
 * - Heat mapping for store layout
 * 
 * PRIVACY FEATURES:
 * - No facial recognition storage
 * - Aggregated metrics only
 * - Opt-in for personal recommendations
 * - GDPR/CCPA compliant data handling
 * 
 * STANDARDS COMPLIANCE:
 * - GDPR (General Data Protection)
 * - CCPA (California Consumer Privacy)
 * - PCI DSS (for payment integration)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CUSTOMER_TELEMETRY_H
#define GF_CUSTOMER_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define RETAIL_MAX_ZONES        32    /**< Maximum store zones */
#define RETAIL_MAX_ENTRANCES    8     /**< Entrance/exit points */
#define RETAIL_HEATMAP_RES      32    /**< Heatmap resolution */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Zone type */
typedef enum {
    RETAIL_ZONE_ENTRANCE,
    RETAIL_ZONE_CHECKOUT,
    RETAIL_ZONE_DEPARTMENT,
    RETAIL_ZONE_DISPLAY,
    RETAIL_ZONE_FITTING,
    RETAIL_ZONE_SERVICE,
    RETAIL_ZONE_QUEUE
} retail_zone_t;

/** Customer segment (anonymous) */
typedef enum {
    CUST_SEGMENT_UNKNOWN,
    CUST_SEGMENT_BROWSER,          /**< Low engagement */
    CUST_SEGMENT_SHOPPER,          /**< High engagement */
    CUST_SEGMENT_RETURNER,         /**< Likely returning customer */
    CUST_SEGMENT_VIP               /**< Opted-in loyalty member */
} cust_segment_t;

/** Traffic direction */
typedef enum {
    TRAFFIC_IN,
    TRAFFIC_OUT,
    TRAFFIC_INTERNAL
} traffic_dir_t;

/** Zone metrics */
typedef struct {
    uint8_t zone_id;
    retail_zone_t type;
    uint32_t current_count;
    uint32_t total_visitors;
    float avg_dwell_time_s;
    float max_dwell_time_s;
    float conversion_rate;         /**< Engagement/purchase ratio */
    float utilization_pct;         /**< Crowding level */
} zone_metrics_t;

/** Store-wide metrics */
typedef struct {
    uint32_t current_occupancy;
    uint32_t total_visitors_today;
    float avg_visit_duration_min;
    float capture_rate;            /**< Passerby to entry ratio */
    float conversion_rate;         /**< Visitors to buyers */
    float avg_queue_wait_s;
    uint8_t zones_active;
    float heatmap[RETAIL_HEATMAP_RES][RETAIL_HEATMAP_RES];
    uint64_t timestamp_ms;
} store_metrics_t;

/** Hourly traffic summary */
typedef struct {
    uint8_t hour;                  /**< 0-23 */
    uint32_t entries;
    uint32_t exits;
    float avg_occupancy;
    float peak_occupancy;
} hourly_traffic_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize customer telemetry */
int retail_telemetry_init(void);

/** Define store zone */
int retail_define_zone(uint8_t zone_id, retail_zone_t type, float x, float y, float w, float h);

/** Record traffic event */
int retail_record_traffic(uint8_t entrance_id, traffic_dir_t direction);

/** Update zone occupancy from vision system */
int retail_update_zone(uint8_t zone_id, uint32_t count, float avg_dwell_s);

/** Get zone metrics */
int retail_get_zone_metrics(uint8_t zone_id, zone_metrics_t *metrics);

/** Get store metrics */
int retail_get_store_metrics(store_metrics_t *metrics);

/** Get hourly traffic */
int retail_get_hourly_traffic(hourly_traffic_t *traffic, uint8_t hour);

/** Generate telemetry for cloud */
int retail_generate_telemetry(uint8_t *buffer, size_t max_len);

/** Process telemetry cycle */
int retail_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_CUSTOMER_TELEMETRY_H */
