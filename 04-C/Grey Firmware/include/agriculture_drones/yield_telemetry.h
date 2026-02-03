/**
 * @file yield_telemetry.h
 * @brief Agricultural Yield Optimization Telemetry
 * 
 * INDUSTRY RELEVANCE:
 * Data-driven agriculture increases yields by 10-20%:
 * - Combine harvester yield monitoring integration
 * - Historical yield map correlation
 * - Weather impact analysis
 * - Input cost optimization (seed, fertilizer, chemicals)
 * - ROI calculation per field zone
 * 
 * This stub demonstrates farm telemetry expertise for:
 * - Precision ag platforms (Climate Corp, Farmers Edge)
 * - Equipment manufacturers (John Deere, CNH, AGCO)
 * - Agricultural cooperatives and grain traders
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_AGRICULTURE_DRONES_YIELD_TELEMETRY_H
#define GF_AGRICULTURE_DRONES_YIELD_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/** Crop type */
typedef enum {
    CROP_CORN,
    CROP_SOYBEANS,
    CROP_WHEAT,
    CROP_COTTON,
    CROP_RICE,
    CROP_CUSTOM
} crop_type_t;

/** Yield prediction confidence */
typedef enum {
    CONFIDENCE_LOW,
    CONFIDENCE_MEDIUM,
    CONFIDENCE_HIGH,
    CONFIDENCE_VERY_HIGH
} yield_confidence_t;

/** Yield telemetry frame */
typedef struct {
    uint32_t timestamp;
    uint32_t field_id;
    crop_type_t crop;
    float area_ha;
    float predicted_yield_kg_ha;
    float actual_yield_kg_ha;
    float moisture_pct;
    float protein_pct;           /**< For grain quality */
    yield_confidence_t confidence;
    float cost_per_ha;
    float revenue_per_ha;
} yield_frame_t;

/** Season summary */
typedef struct {
    uint32_t season_year;
    float total_area_ha;
    float total_yield_kg;
    float avg_yield_kg_ha;
    float total_cost;
    float total_revenue;
    float roi_pct;
} season_summary_t;

/* API Functions */
int yield_telemetry_init(void);
int yield_telemetry_record(const yield_frame_t *frame);
int yield_telemetry_predict(uint32_t field_id, float *predicted_yield);
int yield_telemetry_get_season(uint32_t year, season_summary_t *summary);
int yield_telemetry_export(uint8_t *buffer, size_t max_len);

#endif /* GF_AGRICULTURE_DRONES_YIELD_TELEMETRY_H */
