/**
 * @file patient_telemetry.h
 * @brief Emergency Patient Telemetry Collector Interface
 * 
 * INDUSTRY RELEVANCE:
 * Emergency medical services (EMS) rely on real-time patient telemetry for:
 * - Prehospital vital sign monitoring
 * - Hospital destination decision support
 * - Continuous 12-lead ECG transmission
 * - Trauma team activation alerts
 * 
 * Systems like Philips Tempus, ZOLL X Series, and Stryker LifePak enable
 * ambulance-to-hospital data transmission. Embedded skills include:
 * - Multi-vital-sign acquisition synchronization
 * - Reliable transmission over cellular/satellite
 * - HIPAA-compliant data handling
 * - Emergency-grade reliability (lives at stake)
 * 
 * STANDARDS:
 * - IEC 60601-1 (Medical Electrical Equipment Safety)
 * - HIPAA (Patient Data Privacy)
 * - HL7 FHIR (Healthcare Data Interoperability)
 * - ISO 11073 (Point-of-Care Medical Devices)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_PATIENT_TELEMETRY_H
#define GF_PATIENT_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define PATIENT_MAX_VITALS          16   /**< Maximum vital parameters */
#define PATIENT_ECG_CHANNELS        12   /**< 12-lead ECG */
#define PATIENT_WAVEFORM_HZ         500  /**< ECG sample rate */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Vital sign type */
typedef enum {
    VITAL_HEART_RATE,
    VITAL_SPO2,
    VITAL_BLOOD_PRESSURE_SYSTOLIC,
    VITAL_BLOOD_PRESSURE_DIASTOLIC,
    VITAL_RESPIRATORY_RATE,
    VITAL_TEMPERATURE,
    VITAL_ETCO2,                 /**< End-tidal CO2 */
    VITAL_GLUCOSE,
    VITAL_GCS,                   /**< Glasgow Coma Scale */
    VITAL_PAIN_SCORE
} vital_type_t;

/** Patient acuity level */
typedef enum {
    ACUITY_CRITICAL,             /**< Immediate life threat */
    ACUITY_EMERGENT,             /**< Serious but stable */
    ACUITY_URGENT,               /**< Needs prompt care */
    ACUITY_LESS_URGENT,
    ACUITY_NON_URGENT
} acuity_level_t;

/** Vital sign reading */
typedef struct {
    vital_type_t type;
    float value;
    char unit[8];
    bool abnormal;
    uint32_t timestamp;
} vital_reading_t;

/** ECG waveform segment */
typedef struct {
    int16_t samples[PATIENT_ECG_CHANNELS][PATIENT_WAVEFORM_HZ];
    uint8_t lead_status;         /**< Bit mask of connected leads */
    bool pacing_detected;
    uint32_t timestamp;
} ecg_segment_t;

/** Patient encounter record */
typedef struct {
    char patient_id[20];
    char incident_id[20];
    acuity_level_t acuity;
    vital_reading_t vitals[PATIENT_MAX_VITALS];
    uint8_t vital_count;
    uint32_t encounter_start;
    char chief_complaint[64];
    bool trauma_alert;
} patient_encounter_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int patient_telemetry_init(void);
int patient_start_encounter(const char *incident_id);
int patient_record_vital(const vital_reading_t *vital);
int patient_record_ecg(const ecg_segment_t *segment);
int patient_set_acuity(acuity_level_t acuity);
int patient_get_encounter(patient_encounter_t *encounter);
int patient_transmit_to_hospital(const char *destination_id);
int patient_end_encounter(void);
void patient_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PATIENT_TELEMETRY_H */
