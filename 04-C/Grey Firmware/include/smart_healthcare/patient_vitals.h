/**
 * @file patient_vitals.h
 * @brief Patient Vitals Sensor Stub - Edge Healthcare AI
 * 
 * @details Industry Relevance:
 * Connected medical devices and wearables enable continuous patient
 * monitoring outside traditional clinical settings:
 * - Vital signs: HR, SpO2, BP, temperature, respiration rate
 * - ECG/EKG waveform capture and arrhythmia detection
 * - Fall detection and activity monitoring
 * - Medication adherence tracking
 * - Remote patient monitoring (RPM) for chronic disease management
 * 
 * Edge processing reduces latency for critical alerts and preserves
 * patient privacy by processing data locally. FDA 510(k) and CE marking
 * requirements apply to software as medical device (SaMD).
 * 
 * Market: Digital health market exceeds $200B, driven by aging
 * populations, chronic disease prevalence, and telehealth adoption.
 * 
 * Standards: IEC 62304 (medical software), ISO 13485 (QMS),
 * ISO 81001-5-1 (cybersecurity), HIPAA, GDPR
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_HEALTHCARE_PATIENT_VITALS_H
#define GF_HEALTHCARE_PATIENT_VITALS_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum sensors per patient device */
#define GF_VITALS_MAX_SENSORS           8

/** ECG sample rate (Hz) */
#define GF_VITALS_ECG_RATE_HZ           250

/** Vital signs check rate (Hz) */
#define GF_VITALS_CHECK_RATE_HZ         1

/** Critical alert threshold - low SpO2 (%) */
#define GF_VITALS_SPO2_CRITICAL         88

/** Critical alert threshold - low HR (bpm) */
#define GF_VITALS_HR_LOW_CRITICAL       40

/** Critical alert threshold - high HR (bpm) */
#define GF_VITALS_HR_HIGH_CRITICAL      180

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Sensor types for vital signs
 */
typedef enum {
    GF_VITALS_SENSOR_PPG,           /**< Photoplethysmography (HR, SpO2) */
    GF_VITALS_SENSOR_ECG,           /**< Electrocardiogram */
    GF_VITALS_SENSOR_TEMP,          /**< Temperature */
    GF_VITALS_SENSOR_BP,            /**< Blood pressure (cuff) */
    GF_VITALS_SENSOR_ACCEL,         /**< Accelerometer (activity, fall) */
    GF_VITALS_SENSOR_RESP,          /**< Respiration (impedance) */
    GF_VITALS_SENSOR_GLUCOSE,       /**< Continuous glucose */
    GF_VITALS_SENSOR_WEIGHT         /**< Scale measurement */
} gf_vitals_sensor_type_t;

/**
 * @brief Alert priority levels
 */
typedef enum {
    GF_VITALS_ALERT_INFO,           /**< Informational */
    GF_VITALS_ALERT_LOW,            /**< Low priority */
    GF_VITALS_ALERT_MEDIUM,         /**< Medium priority */
    GF_VITALS_ALERT_HIGH,           /**< High priority */
    GF_VITALS_ALERT_CRITICAL        /**< Critical - immediate action */
} gf_vitals_alert_level_t;

/**
 * @brief Current vital signs snapshot
 */
typedef struct {
    float heart_rate_bpm;           /**< Heart rate */
    float spo2_pct;                 /**< Oxygen saturation */
    float temp_c;                   /**< Body temperature */
    float systolic_mmhg;            /**< Systolic blood pressure */
    float diastolic_mmhg;           /**< Diastolic blood pressure */
    float resp_rate_bpm;            /**< Respiration rate */
    float glucose_mgdl;             /**< Blood glucose */
    uint16_t steps_today;           /**< Step count */
    uint32_t timestamp;             /**< Measurement time */
    uint8_t confidence_pct;         /**< Measurement confidence */
    bool motion_artifact;           /**< Motion affecting reading */
} gf_vitals_snapshot_t;

/**
 * @brief ECG waveform buffer
 */
typedef struct {
    int16_t samples[GF_VITALS_ECG_RATE_HZ * 10]; /**< 10 seconds */
    uint16_t sample_count;          /**< Samples in buffer */
    uint16_t sample_rate_hz;        /**< Sample rate */
    int16_t lead_off_status;        /**< Lead-off detection */
    bool valid;                     /**< Waveform validity */
} gf_vitals_ecg_t;

/**
 * @brief Vital signs alert
 */
typedef struct {
    uint32_t timestamp;             /**< Alert timestamp */
    gf_vitals_alert_level_t level;  /**< Alert level */
    uint8_t parameter;              /**< Which vital (enum) */
    float value;                    /**< Current value */
    float threshold;                /**< Threshold crossed */
    char message[64];               /**< Alert message */
    bool acknowledged;              /**< Alert acknowledged */
} gf_vitals_alert_t;

/**
 * @brief Patient device status
 */
typedef struct {
    char device_id[16];             /**< Device identifier */
    char patient_id[16];            /**< Patient identifier */
    float battery_pct;              /**< Battery percentage */
    bool wearing;                   /**< Device being worn */
    bool connectivity;              /**< Cloud connected */
    uint32_t last_sync;             /**< Last cloud sync time */
    uint8_t active_alerts;          /**< Active alert count */
    uint32_t measurements_24h;      /**< Measurements in 24h */
} gf_vitals_device_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize patient vitals monitoring
 * @return 0 on success, negative error code on failure
 */
int gf_vitals_init(void);

/**
 * @brief Process sensor readings
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int gf_vitals_process(uint32_t delta_ms);

/**
 * @brief Get current vital signs snapshot
 * @param snapshot Output snapshot structure
 * @return 0 on success
 */
int gf_vitals_get_snapshot(gf_vitals_snapshot_t* snapshot);

/**
 * @brief Get ECG waveform buffer
 * @param ecg Output ECG structure
 * @return 0 on success
 */
int gf_vitals_get_ecg(gf_vitals_ecg_t* ecg);

/**
 * @brief Get pending alert
 * @param index Alert index (0 = newest)
 * @param alert Output alert structure
 * @return 0 on success, -1 if no alert
 */
int gf_vitals_get_alert(uint8_t index, gf_vitals_alert_t* alert);

/**
 * @brief Acknowledge alert
 * @param timestamp Alert timestamp to acknowledge
 * @return 0 on success
 */
int gf_vitals_ack_alert(uint32_t timestamp);

/**
 * @brief Set alert thresholds
 * @param parameter Vital parameter
 * @param low_threshold Low threshold
 * @param high_threshold High threshold
 * @return 0 on success
 */
int gf_vitals_set_threshold(uint8_t parameter,
                            float low_threshold,
                            float high_threshold);

/**
 * @brief Get device status
 * @param device Output device status
 * @return 0 on success
 */
int gf_vitals_get_device(gf_vitals_device_t* device);

/**
 * @brief Trigger manual measurement (BP, weight, etc.)
 * @param sensor Sensor to trigger
 * @return 0 on success
 */
int gf_vitals_trigger_measurement(gf_vitals_sensor_type_t sensor);

/**
 * @brief Shutdown vitals monitoring
 * @return 0 on success
 */
int gf_vitals_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_HEALTHCARE_PATIENT_VITALS_H */
