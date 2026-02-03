/**
 * @file astronaut_safety_telemetry.h
 * @brief Astronaut Safety Telemetry Collector
 * 
 * INDUSTRY RELEVANCE:
 * Crew health monitoring is critical for long-duration missions where
 * medical evacuation is impossible. This module aggregates biometric,
 * environmental, and radiation data to provide real-time health status
 * and predictive alerts for mission control.
 * 
 * TECHNICAL SCOPE:
 * - Multi-crew biometric aggregation
 * - Radiation exposure tracking per astronaut
 * - Environmental hazard correlation
 * - Predictive health modeling
 * - CCSDS-compatible telemetry packets
 * 
 * DATA SOURCES:
 * - Wearable biosensors (HR, SpO2, body temp)
 * - Personal dosimeters
 * - Habitat environmental sensors
 * - Activity tracking (location, exercise, sleep)
 * 
 * STANDARDS COMPLIANCE:
 * - NASA-STD-3001 (Crew health requirements)
 * - CCSDS 133.0-B-2 (Telemetry)
 * - HL7 FHIR (Health data interoperability)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ASTRONAUT_SAFETY_TELEMETRY_H
#define GF_ASTRONAUT_SAFETY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define ASTRO_MAX_CREW          8     /**< Maximum crew members */
#define ASTRO_TELEMETRY_SIZE    512   /**< Max telemetry packet size */
#define ASTRO_HISTORY_DAYS      30    /**< Health history retention */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Crew status classification */
typedef enum {
    CREW_STATUS_NOMINAL,           /**< All parameters normal */
    CREW_STATUS_ELEVATED,          /**< Minor concerns */
    CREW_STATUS_MEDICAL_WATCH,     /**< Requires monitoring */
    CREW_STATUS_MEDICAL_EVENT,     /**< Active medical issue */
    CREW_STATUS_EMERGENCY          /**< Life-threatening */
} crew_status_t;

/** Activity state */
typedef enum {
    CREW_ACTIVITY_SLEEP,
    CREW_ACTIVITY_REST,
    CREW_ACTIVITY_WORK,
    CREW_ACTIVITY_EXERCISE,
    CREW_ACTIVITY_EVA,
    CREW_ACTIVITY_MEDICAL
} crew_activity_t;

/** Individual crew health data */
typedef struct {
    uint8_t crew_id;
    char name[32];
    crew_status_t status;
    crew_activity_t activity;
    float heart_rate_bpm;
    float blood_pressure_sys;
    float blood_pressure_dia;
    float spo2_pct;
    float body_temp_c;
    float radiation_dose_msv;      /**< Cumulative mission dose */
    float dose_rate_usv_hr;        /**< Current exposure rate */
    float stress_index;            /**< Composite stress metric */
    uint32_t sleep_hours_24h;      /**< Sleep in last 24 hours */
    bool dosimeter_active;
    bool biometrics_valid;
} crew_health_t;

/** Aggregated crew telemetry */
typedef struct {
    uint8_t crew_count;
    crew_health_t crew[ASTRO_MAX_CREW];
    float mission_elapsed_days;
    float habitat_o2_pct;
    float habitat_co2_ppm;
    float habitat_pressure_kpa;
    float habitat_temp_c;
    uint32_t alerts_active;
    uint64_t timestamp_ms;
} crew_telemetry_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize astronaut safety telemetry */
int astro_telemetry_init(void);

/** Register crew member */
int astro_register_crew(uint8_t crew_id, const char *name);

/** Update crew biometrics */
int astro_update_biometrics(uint8_t crew_id, float hr, float bp_sys, float bp_dia, float spo2, float temp);

/** Update crew radiation dose */
int astro_update_radiation(uint8_t crew_id, float dose_msv, float rate_usv_hr);

/** Get crew health status */
int astro_get_crew_health(uint8_t crew_id, crew_health_t *health);

/** Generate CCSDS telemetry packet */
int astro_generate_telemetry(uint8_t *buffer, size_t max_len);

/** Process telemetry cycle */
int astro_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_ASTRONAUT_SAFETY_TELEMETRY_H */
