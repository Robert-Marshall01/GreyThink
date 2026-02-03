/**
 * @file passenger_telemetry.h
 * @brief Space Tourism Passenger Health Telemetry
 * 
 * INDUSTRY RELEVANCE:
 * Space tourism operators must monitor passenger health during flight phases
 * that include high G-forces, microgravity, and radiation exposure. This
 * telemetry system provides:
 * - Real-time vital signs monitoring (HR, SpO2, BP)
 * - G-force exposure tracking
 * - Motion sickness detection
 * - Medical alert generation
 * 
 * Critical for insurance, regulatory compliance, and passenger safety during
 * commercial spaceflight operations.
 * 
 * STANDARDS: NASA-STD-3001, FAA 14 CFR Part 460
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_PASSENGER_TELEMETRY_H
#define GF_PASSENGER_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_PASSENGERS 8

/* Flight phases affecting passenger monitoring */
typedef enum {
    FLIGHT_PHASE_PREFLIGHT,
    FLIGHT_PHASE_ASCENT,
    FLIGHT_PHASE_COAST,
    FLIGHT_PHASE_REENTRY,
    FLIGHT_PHASE_LANDING,
    FLIGHT_PHASE_POSTFLIGHT
} flight_phase_t;

/* Passenger health status */
typedef enum {
    HEALTH_NOMINAL,
    HEALTH_ELEVATED,
    HEALTH_CONCERN,
    HEALTH_ALERT,
    HEALTH_EMERGENCY
} health_status_t;

/* Vital signs data */
typedef struct {
    uint8_t passenger_id;
    uint16_t heart_rate_bpm;
    uint8_t spo2_pct;
    uint16_t systolic_bp;
    uint16_t diastolic_bp;
    float body_temp_c;
    float g_force_current;
    float g_force_peak;
    float radiation_dose_usv;
    bool motion_sickness_detected;
    health_status_t status;
    uint32_t timestamp_ms;
} passenger_vitals_t;

/**
 * @brief Initialize passenger telemetry system
 * @param num_passengers Number of passengers to monitor
 * @return 0 on success
 */
int passenger_telem_init(uint8_t num_passengers);

/**
 * @brief Update passenger vitals
 * @param passenger_id Passenger seat number
 * @param vitals Updated vital signs
 * @return 0 on success
 */
int passenger_telem_update(uint8_t passenger_id, const passenger_vitals_t *vitals);

/**
 * @brief Get current passenger status
 * @param passenger_id Passenger to query
 * @param vitals Output vitals structure
 * @return 0 on success
 */
int passenger_telem_get(uint8_t passenger_id, passenger_vitals_t *vitals);

/**
 * @brief Set current flight phase (affects monitoring thresholds)
 * @param phase Current flight phase
 */
void passenger_telem_set_phase(flight_phase_t phase);

/**
 * @brief Check if any passenger requires medical attention
 * @return Passenger ID requiring attention, or -1 if all nominal
 */
int passenger_telem_check_alerts(void);

/**
 * @brief Generate telemetry packet for downlink
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written, or negative on error
 */
int passenger_telem_generate(uint8_t *buffer, size_t max_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_PASSENGER_TELEMETRY_H */
