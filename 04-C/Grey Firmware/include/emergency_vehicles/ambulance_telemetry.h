/**
 * @file ambulance_telemetry.h
 * @brief Ambulance Vehicle Telemetry System
 *
 * INDUSTRY RELEVANCE:
 * Modern ambulances are mobile ICUs requiring real-time data transmission
 * to hospitals for pre-arrival preparation. Vehicle telemetry enables
 * dispatch optimization, fleet management, and regulatory compliance.
 *
 * MARKET CONTEXT:
 * - Emergency Medical Services (EMS) modernization
 * - Hospital pre-notification systems
 * - Fleet management and maintenance scheduling
 * - Insurance and liability documentation
 * - First responder safety monitoring
 *
 * TECHNICAL APPROACH:
 * - GPS/GNSS positioning with dead reckoning backup
 * - Vehicle CAN bus integration for diagnostics
 * - Cellular/satellite uplink for real-time transmission
 * - Local buffering for coverage gaps
 * - Encrypted transmission for HIPAA compliance
 *
 * @author Grey Firmware Project
 */

#ifndef GF_AMBULANCE_TELEMETRY_H
#define GF_AMBULANCE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Ambulance operational status
 */
typedef enum {
    AMBULANCE_AVAILABLE,
    AMBULANCE_DISPATCHED,
    AMBULANCE_EN_ROUTE_SCENE,
    AMBULANCE_ON_SCENE,
    AMBULANCE_TRANSPORTING,
    AMBULANCE_AT_HOSPITAL,
    AMBULANCE_OUT_OF_SERVICE
} gf_ambulance_status_t;

/**
 * @brief Vehicle position data
 */
typedef struct {
    double latitude;
    double longitude;
    float altitude_m;
    float speed_kmh;
    float heading_deg;
    uint8_t satellites;
    float hdop;
    uint32_t timestamp;
} gf_vehicle_position_t;

/**
 * @brief Vehicle diagnostics
 */
typedef struct {
    float fuel_percent;
    float battery_volts;
    float engine_temp_c;
    float oil_pressure_psi;
    uint32_t odometer_km;
    uint16_t dtc_count;
    bool check_engine;
    bool maintenance_due;
} gf_vehicle_diag_t;

/**
 * @brief Call record
 */
typedef struct {
    uint32_t call_id;
    uint32_t dispatch_time;
    uint32_t scene_arrival;
    uint32_t transport_start;
    uint32_t hospital_arrival;
    float response_time_min;
    float transport_time_min;
    uint8_t patient_count;
} gf_call_record_t;

/* Function prototypes */
int gf_ambulance_telem_init(void);
int gf_ambulance_set_status(gf_ambulance_status_t status);
int gf_ambulance_get_position(gf_vehicle_position_t *pos);
int gf_ambulance_get_diagnostics(gf_vehicle_diag_t *diag);
int gf_ambulance_start_call(uint32_t call_id);
int gf_ambulance_end_call(gf_call_record_t *record);
int gf_ambulance_transmit_telemetry(void);
void gf_ambulance_telem_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_AMBULANCE_TELEMETRY_H */
