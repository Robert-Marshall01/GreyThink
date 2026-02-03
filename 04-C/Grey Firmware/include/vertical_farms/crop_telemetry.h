/**
 * @file crop_telemetry.h
 * @brief Crop Growth Telemetry Collector for Vertical Farms
 *
 * INDUSTRY RELEVANCE:
 * Data-driven agriculture requires comprehensive telemetry for yield
 * optimization, predictive maintenance, and regulatory compliance.
 * This module aggregates crop health metrics for analytics platforms.
 *
 * MARKET CONTEXT:
 * - Farm management software integration (Priva, Argus)
 * - Food safety traceability requirements
 * - Crop insurance documentation
 * - Research data collection for cultivar optimization
 * - Carbon footprint and sustainability reporting
 *
 * TECHNICAL APPROACH:
 * - Multi-sensor data aggregation and timestamping
 * - Growth rate calculation and trend analysis
 * - Harvest prediction algorithms
 * - Cloud platform integration (MQTT, REST)
 * - Local storage with sync on connectivity
 *
 * @author Grey Firmware Project
 */

#ifndef GF_CROP_TELEMETRY_H
#define GF_CROP_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Crop health indicators
 */
typedef struct {
    float leaf_area_index;       /**< LAI measurement */
    float chlorophyll_index;     /**< SPAD equivalent */
    float plant_height_cm;
    float stem_diameter_mm;
    float estimated_biomass_g;
    uint8_t health_score;        /**< 0-100 */
    bool stress_detected;
} gf_crop_health_t;

/**
 * @brief Growth telemetry record
 */
typedef struct {
    uint32_t timestamp;
    uint32_t zone_id;
    char cultivar[32];
    uint16_t days_from_seed;
    gf_crop_health_t health;
    float daily_growth_rate;
    float predicted_harvest_days;
    float predicted_yield_kg;
} gf_crop_record_t;

/**
 * @brief Harvest summary
 */
typedef struct {
    uint32_t batch_id;
    uint32_t harvest_date;
    char cultivar[32];
    float total_yield_kg;
    float quality_score;
    float resource_efficiency;
    uint32_t grow_days;
} gf_harvest_summary_t;

/* Function prototypes */
int gf_crop_telem_init(void);
int gf_crop_record_health(uint32_t zone, const gf_crop_health_t *health);
int gf_crop_get_record(uint32_t zone, gf_crop_record_t *record);
int gf_crop_record_harvest(const gf_harvest_summary_t *summary);
int gf_crop_predict_harvest(uint32_t zone, float *days, float *yield_kg);
int gf_crop_export_json(uint8_t *buffer, size_t max_len);
uint32_t gf_crop_get_active_zones(void);
void gf_crop_telem_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CROP_TELEMETRY_H */
