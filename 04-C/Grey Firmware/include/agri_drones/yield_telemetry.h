/**
 * @file yield_telemetry.h
 * @brief Agricultural Drone Yield Optimization Telemetry
 * 
 * INDUSTRY RELEVANCE:
 * Agricultural yield optimization through drone telemetry enables:
 * - Real-time yield prediction based on crop health data
 * - Historical yield mapping for trend analysis
 * - Integration with farm management systems (FMS)
 * - ROI calculation for precision agriculture investments
 * 
 * Connects field observations to agronomic decisions and economic outcomes.
 * Key for digital farming platforms: Climate Corp, Granular, FarmLogs
 * 
 * STANDARDS: AgGateway ADAPT, ISO 19156 (Observations)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_YIELD_TELEMETRY_H
#define GF_YIELD_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Crop types */
typedef enum {
    CROP_CORN,
    CROP_SOYBEAN,
    CROP_WHEAT,
    CROP_COTTON,
    CROP_RICE,
    CROP_SPECIALTY,
    CROP_COUNT
} crop_type_t;

/* Yield prediction confidence */
typedef enum {
    YIELD_CONF_LOW,
    YIELD_CONF_MEDIUM,
    YIELD_CONF_HIGH
} yield_confidence_t;

/* Zone yield data */
typedef struct {
    uint16_t zone_id;
    crop_type_t crop;
    float predicted_yield_kg_ha;
    float historical_yield_kg_ha;
    float yield_variance_pct;
    yield_confidence_t confidence;
    float stress_factor;
    uint32_t timestamp;
} zone_yield_t;

/* Field summary */
typedef struct {
    float total_area_ha;
    float avg_yield_kg_ha;
    float total_production_kg;
    float revenue_estimate;
    float compared_to_avg_pct;
    uint16_t zones_analyzed;
} field_summary_t;

/**
 * @brief Initialize yield telemetry
 * @param crop Field crop type
 * @param field_area_ha Total field area
 * @return 0 on success
 */
int yield_telem_init(crop_type_t crop, float field_area_ha);

/**
 * @brief Update zone yield prediction
 * @param zone_id Zone identifier
 * @param ndvi Current NDVI value
 * @param stress_factor Current stress factor (0-1)
 * @return 0 on success
 */
int yield_telem_update_zone(uint16_t zone_id, float ndvi, float stress_factor);

/**
 * @brief Get zone yield prediction
 * @param zone_id Zone to query
 * @param yield Output yield data
 * @return 0 on success
 */
int yield_telem_get_zone(uint16_t zone_id, zone_yield_t *yield);

/**
 * @brief Get field-level summary
 * @param summary Output summary structure
 * @return 0 on success
 */
int yield_telem_get_summary(field_summary_t *summary);

/**
 * @brief Generate telemetry packet for cloud upload
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written
 */
int yield_telem_generate(uint8_t *buffer, size_t max_len);

/**
 * @brief Set crop price for revenue estimation
 * @param price_per_kg Price per kilogram
 */
void yield_telem_set_price(float price_per_kg);

#ifdef __cplusplus
}
#endif

#endif /* GF_YIELD_TELEMETRY_H */
