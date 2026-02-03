/**
 * @file production_telemetry.h
 * @brief Orbital Manufacturing Production Yield Telemetry
 *
 * INDUSTRY RELEVANCE:
 * Space manufacturing requires meticulous quality tracking to justify
 * the high cost per kilogram to orbit. This module collects and reports
 * production metrics for ground control analysis and process optimization.
 *
 * MARKET CONTEXT:
 * - ISS National Lab commercialization requiring detailed ROI metrics
 * - Commercial space station operators needing production dashboards
 * - Pharmaceutical companies tracking crystallization quality
 * - Fiber optic manufacturers monitoring draw consistency
 * - Insurance and regulatory compliance documentation
 *
 * TECHNICAL APPROACH:
 * - Real-time yield calculation with quality grading
 * - Batch tracking and traceability
 * - Anomaly detection and alerting
 * - CCSDS-compliant telemetry formatting
 * - Ground station downlink optimization
 *
 * @author Grey Firmware Project
 */

#ifndef GF_PRODUCTION_TELEMETRY_H
#define GF_PRODUCTION_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Product quality grades
 */
typedef enum {
    QUALITY_GRADE_A,     /**< Premium - exceeds spec */
    QUALITY_GRADE_B,     /**< Standard - meets spec */
    QUALITY_GRADE_C,     /**< Acceptable - minor deviation */
    QUALITY_GRADE_D,     /**< Marginal - usable with caveats */
    QUALITY_GRADE_REJECT /**< Failed QC */
} gf_quality_grade_t;

/**
 * @brief Production batch record
 */
typedef struct {
    uint32_t batch_id;
    uint32_t start_time;
    uint32_t end_time;
    float input_mass_g;
    float output_mass_g;
    float yield_percent;
    gf_quality_grade_t grade;
    uint8_t defect_count;
    bool certified;
} gf_batch_record_t;

/**
 * @brief Production telemetry frame
 */
typedef struct {
    uint16_t frame_id;
    uint32_t timestamp;
    uint32_t current_batch;
    float instantaneous_rate;
    float cumulative_yield;
    float quality_index;
    uint8_t fault_flags;
    uint8_t sensor_health;
} gf_production_telem_t;

/* Function prototypes */
int gf_production_telem_init(void);
int gf_production_record_batch(const gf_batch_record_t *batch);
int gf_production_get_telemetry(gf_production_telem_t *telem);
int gf_production_format_ccsds(uint8_t *buffer, size_t max_len);
float gf_production_get_cumulative_yield(void);
uint32_t gf_production_get_batch_count(void);
void gf_production_reset_stats(void);
void gf_production_telem_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PRODUCTION_TELEMETRY_H */
