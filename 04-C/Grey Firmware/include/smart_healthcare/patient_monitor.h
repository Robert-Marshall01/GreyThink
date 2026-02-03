/**
 * @file patient_monitor.h
 * @brief Patient Monitoring System Interface for Smart Healthcare Facilities
 * 
 * @details
 * This module provides interfaces for real-time patient monitoring systems
 * including vital signs acquisition, waveform capture, and alarm management.
 * Designed for ICU, operating room, and bedside monitoring applications.
 * 
 * INDUSTRY RELEVANCE:
 * - ICU patient monitoring systems (GE, Philips, Mindray)
 * - Surgical suite monitoring
 * - Post-anesthesia care units (PACU)
 * - Neonatal intensive care
 * - Remote patient monitoring
 * - Telemedicine integration
 * 
 * REGULATORY COMPLIANCE:
 * - IEC 60601-1 (Medical electrical equipment safety)
 * - IEC 62304 (Medical device software lifecycle)
 * - FDA 21 CFR Part 11 (Electronic records)
 * - HL7 FHIR integration ready
 * - ISO 13485 quality management
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_PATIENT_MONITOR_H
#define GF_PATIENT_MONITOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Patient monitor status codes
 */
typedef enum {
    GF_PM_OK = 0,
    GF_PM_ERROR_NOT_INITIALIZED,
    GF_PM_ERROR_NULL_PTR,
    GF_PM_ERROR_INVALID_PATIENT,
    GF_PM_ERROR_SENSOR_FAULT,
    GF_PM_ERROR_ARRHYTHMIA,
    GF_PM_ERROR_CRITICAL_ALARM,
    GF_PM_WARN_LEAD_OFF,
    GF_PM_WARN_ARTIFACT
} gf_pm_status_t;

/**
 * @brief Vital sign types
 */
typedef enum {
    GF_PM_VITAL_HR,               /**< Heart rate (bpm) */
    GF_PM_VITAL_SPO2,             /**< Oxygen saturation (%) */
    GF_PM_VITAL_NIBP_SYS,         /**< Non-invasive BP systolic */
    GF_PM_VITAL_NIBP_DIA,         /**< Non-invasive BP diastolic */
    GF_PM_VITAL_NIBP_MAP,         /**< Mean arterial pressure */
    GF_PM_VITAL_TEMP,             /**< Body temperature */
    GF_PM_VITAL_RESP,             /**< Respiration rate */
    GF_PM_VITAL_ETCO2,            /**< End-tidal CO2 */
    GF_PM_VITAL_IBP1,             /**< Invasive BP channel 1 */
    GF_PM_VITAL_IBP2,             /**< Invasive BP channel 2 */
    GF_PM_VITAL_CVP,              /**< Central venous pressure */
    GF_PM_VITAL_CO                /**< Cardiac output */
} gf_pm_vital_t;

/**
 * @brief Alarm priority levels (IEC 60601-1-8)
 */
typedef enum {
    GF_PM_ALARM_LOW,              /**< Low priority (informational) */
    GF_PM_ALARM_MEDIUM,           /**< Medium priority (warning) */
    GF_PM_ALARM_HIGH              /**< High priority (critical) */
} gf_pm_alarm_priority_t;

/**
 * @brief ECG lead configuration
 */
typedef enum {
    GF_PM_ECG_3_LEAD,             /**< 3-lead monitoring */
    GF_PM_ECG_5_LEAD,             /**< 5-lead monitoring */
    GF_PM_ECG_12_LEAD             /**< 12-lead diagnostic */
} gf_pm_ecg_config_t;

/**
 * @brief Patient demographics (anonymized)
 */
typedef struct {
    uint32_t patient_id;          /**< Unique patient ID */
    uint8_t age_years;            /**< Age in years */
    char gender;                  /**< 'M', 'F', or 'U' */
    float height_cm;              /**< Height in cm */
    float weight_kg;              /**< Weight in kg */
    uint8_t bed_number;           /**< Bed/room number */
    uint8_t unit_id;              /**< Care unit ID */
} gf_pm_patient_t;

/**
 * @brief Vital signs snapshot
 */
typedef struct {
    int16_t heart_rate;           /**< Heart rate (bpm) */
    uint8_t spo2;                 /**< SpO2 (%) */
    int16_t nibp_systolic;        /**< Systolic BP (mmHg) */
    int16_t nibp_diastolic;       /**< Diastolic BP (mmHg) */
    int16_t nibp_map;             /**< MAP (mmHg) */
    float temperature;            /**< Temperature (°C) */
    uint8_t resp_rate;            /**< Respiration rate */
    uint8_t etco2;                /**< EtCO2 (mmHg) */
    uint64_t timestamp_ms;        /**< Measurement time */
    uint8_t alarm_flags;          /**< Active alarms bitmap */
} gf_pm_vitals_t;

/**
 * @brief Waveform data buffer
 */
typedef struct {
    int16_t* samples;             /**< Sample buffer */
    uint16_t sample_count;        /**< Number of samples */
    uint16_t sample_rate;         /**< Samples per second */
    uint8_t resolution_bits;      /**< ADC resolution */
    float scale_factor;           /**< mV per unit */
} gf_pm_waveform_t;

/**
 * @brief Alarm callback
 */
typedef void (*gf_pm_alarm_cb_t)(gf_pm_vital_t vital, gf_pm_alarm_priority_t priority,
                                  float value, float threshold, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_pm_status_t gf_pm_init(void);
void gf_pm_shutdown(void);
gf_pm_status_t gf_pm_admit_patient(const gf_pm_patient_t* patient);
gf_pm_status_t gf_pm_discharge_patient(uint32_t patient_id);
gf_pm_status_t gf_pm_get_vitals(uint32_t patient_id, gf_pm_vitals_t* vitals);
gf_pm_status_t gf_pm_get_waveform(uint32_t patient_id, gf_pm_vital_t type,
                                   gf_pm_waveform_t* waveform);
gf_pm_status_t gf_pm_set_alarm_limits(gf_pm_vital_t vital, float low, float high);
gf_pm_status_t gf_pm_register_alarm_callback(gf_pm_alarm_cb_t callback, void* user_data);
gf_pm_status_t gf_pm_silence_alarm(uint16_t duration_sec);
gf_pm_status_t gf_pm_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PATIENT_MONITOR_H */
