/**
 * @file preservation_telemetry.h
 * @brief Artifact Preservation Telemetry System
 * 
 * INDUSTRY RELEVANCE:
 * Proactive conservation requires continuous monitoring and predictive
 * analytics to identify artifacts at risk before visible damage occurs.
 * This module tracks cumulative exposure, predicts degradation rates,
 * and provides early warning for preventive conservation actions.
 * 
 * KEY CAPABILITIES:
 * - Cumulative light exposure tracking (lux-hours)
 * - Thermal cycling stress analysis
 * - Humidity fluctuation impact assessment
 * - Pollutant exposure logging
 * - Predictive degradation modeling
 * - Conservation priority scoring
 * 
 * DEGRADATION FACTORS:
 * - Photodegradation (light + UV)
 * - Hydrolysis (humidity)
 * - Oxidation (air quality)
 * - Thermal stress (temperature cycling)
 * - Biological (mold, pests)
 * 
 * ANALYTICS:
 * - Life expectancy estimation
 * - Risk trend analysis
 * - Conservation scheduling
 * - Loan condition reporting
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_PRESERVATION_TELEMETRY_H
#define GF_PRESERVATION_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Conservation priority */
typedef enum {
    CONSV_PRIORITY_LOW,
    CONSV_PRIORITY_MEDIUM,
    CONSV_PRIORITY_HIGH,
    CONSV_PRIORITY_URGENT
} consv_priority_t;

/** Degradation risk level */
typedef enum {
    RISK_MINIMAL,
    RISK_LOW,
    RISK_MODERATE,
    RISK_HIGH,
    RISK_SEVERE
} risk_level_t;

/** Artifact record */
typedef struct {
    uint64_t artifact_id;
    char accession_number[24];
    char title[64];
    uint8_t zone_id;
    uint16_t year_acquired;
    uint16_t age_years;
    float light_sensitivity;   /**< 1.0 = normal, 2.0 = high */
    float humidity_sensitivity;
    float temp_sensitivity;
} artifact_info_t;

/** Cumulative exposure data */
typedef struct {
    uint64_t artifact_id;
    float lux_hours_total;     /**< Cumulative light exposure */
    float uv_mj_total;         /**< Cumulative UV exposure */
    uint32_t temp_cycles;      /**< Temperature fluctuations */
    uint32_t humidity_cycles;  /**< Humidity fluctuations */
    float voc_exposure;        /**< Cumulative VOC exposure */
    uint32_t last_conservation; /**< Last conservation date */
    uint32_t measurement_start; /**< Tracking start date */
} exposure_data_t;

/** Risk assessment */
typedef struct {
    uint64_t artifact_id;
    risk_level_t overall_risk;
    risk_level_t light_risk;
    risk_level_t humidity_risk;
    risk_level_t temp_risk;
    risk_level_t air_risk;
    consv_priority_t priority;
    uint32_t estimated_intervention_date;
    char recommendation[128];
} risk_assessment_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize preservation telemetry
 * @param collection_id Collection identifier
 * @return 0 on success, negative on error
 */
int pres_telem_init(uint32_t collection_id);

/**
 * @brief Register artifact for monitoring
 * @param info Artifact information
 * @return 0 on success, negative on error
 */
int pres_telem_register(const artifact_info_t* info);

/**
 * @brief Update exposure data (called periodically)
 * @param artifact_id Artifact to update
 * @param delta_hours Hours since last update
 * @return 0 on success, negative on error
 */
int pres_telem_update_exposure(uint64_t artifact_id, float delta_hours);

/**
 * @brief Get exposure data
 * @param artifact_id Artifact to query
 * @param data Output exposure data
 * @return 0 on success, negative on error
 */
int pres_telem_get_exposure(uint64_t artifact_id, exposure_data_t* data);

/**
 * @brief Get risk assessment
 * @param artifact_id Artifact to assess
 * @param assessment Output assessment
 * @return 0 on success, negative on error
 */
int pres_telem_assess_risk(uint64_t artifact_id, risk_assessment_t* assessment);

/**
 * @brief Get conservation priority list
 * @param assessments Output array
 * @param max_count Maximum to return
 * @param count Actual count
 * @return 0 on success, negative on error
 */
int pres_telem_get_priorities(risk_assessment_t* assessments,
                              uint16_t max_count, uint16_t* count);

/**
 * @brief Record conservation event
 * @param artifact_id Artifact conserved
 * @param notes Conservation notes
 * @return 0 on success, negative on error
 */
int pres_telem_record_conservation(uint64_t artifact_id, const char* notes);

/**
 * @brief Shutdown preservation telemetry
 * @return 0 on success, negative on error
 */
int pres_telem_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PRESERVATION_TELEMETRY_H */
