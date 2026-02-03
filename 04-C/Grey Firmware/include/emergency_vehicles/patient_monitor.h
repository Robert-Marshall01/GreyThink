/**
 * @file patient_monitor.h
 * @brief In-Ambulance Patient Monitoring Module
 *
 * INDUSTRY RELEVANCE:
 * Continuous patient monitoring during transport enables early detection
 * of deterioration and hospital pre-notification. Integration with
 * electronic patient care records improves care coordination.
 *
 * MARKET CONTEXT:
 * - Philips, Zoll, Stryker patient monitoring integration
 * - Telehealth and physician consultation during transport
 * - Cardiac arrest and stroke protocol automation
 * - Research data collection for outcomes studies
 * - Quality improvement and protocol compliance
 *
 * TECHNICAL APPROACH:
 * - Multi-parameter vital signs aggregation
 * - HL7/FHIR data formatting for interoperability
 * - Alarm management and prioritization
 * - Waveform capture and transmission
 * - Integration with hospital EMR systems
 *
 * @author Grey Firmware Project
 */

#ifndef GF_PATIENT_MONITOR_H
#define GF_PATIENT_MONITOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Patient acuity level
 */
typedef enum {
    ACUITY_GREEN,        /**< Stable, minor */
    ACUITY_YELLOW,       /**< Urgent, not critical */
    ACUITY_RED,          /**< Critical, life-threatening */
    ACUITY_BLACK         /**< Expectant/deceased */
} gf_acuity_level_t;

/**
 * @brief Vital signs reading
 */
typedef struct {
    uint16_t heart_rate;         /**< BPM */
    uint16_t spo2_percent;       /**< Oxygen saturation */
    uint16_t resp_rate;          /**< Breaths per minute */
    uint16_t systolic_bp;        /**< mmHg */
    uint16_t diastolic_bp;       /**< mmHg */
    float temp_c;                /**< Core temperature */
    float etco2_mmhg;            /**< End-tidal CO2 */
    uint8_t gcs_score;           /**< Glasgow Coma Scale */
    uint32_t timestamp;
    bool valid;
} gf_vitals_t;

/**
 * @brief ECG waveform data
 */
typedef struct {
    int16_t samples[256];        /**< 1 second at 256 Hz */
    uint8_t lead_config;         /**< Lead placement */
    bool rhythm_regular;
    char interpretation[64];     /**< Auto-interpretation */
} gf_ecg_waveform_t;

/**
 * @brief Patient record
 */
typedef struct {
    uint32_t incident_id;
    uint32_t patient_id;
    gf_acuity_level_t acuity;
    gf_vitals_t vitals;
    char chief_complaint[128];
    char allergies[64];
    char medications[128];
} gf_patient_record_t;

/* Function prototypes */
int gf_patient_monitor_init(void);
int gf_patient_start_monitoring(uint32_t patient_id);
int gf_patient_get_vitals(gf_vitals_t *vitals);
int gf_patient_get_ecg(gf_ecg_waveform_t *ecg);
int gf_patient_set_acuity(gf_acuity_level_t acuity);
int gf_patient_transmit_to_hospital(const char *hospital_id);
int gf_patient_export_hl7(uint8_t *buffer, size_t max_len);
void gf_patient_monitor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PATIENT_MONITOR_H */
