/**
 * @file passenger_safety_telemetry.h
 * @brief Telemetry Collector for Hyperloop Passenger Safety
 * 
 * INDUSTRY RELEVANCE:
 * Passenger transportation at near-sonic speeds requires comprehensive safety
 * telemetry for real-time monitoring, incident response, and regulatory
 * compliance. This module aggregates pod health, cabin environment, and
 * passenger status data for control center visualization and alerting.
 * 
 * Applications:
 * - Hyperloop operations centers
 * - High-speed rail monitoring
 * - Aviation cabin monitoring
 * - Cruise ship passenger systems
 * - Theme park ride safety
 * 
 * Standards: EN 50126 (Rail Safety), ISO 39001 (Road Traffic Safety), NFPA 130 (Transit)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_PASSENGER_SAFETY_TELEMETRY_H
#define GF_PASSENGER_SAFETY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define PSGR_TELEM_MAX_PODS         32      /* Maximum pods tracked */
#define PSGR_TELEM_FRAME_SIZE       128     /* Telemetry frame size */
#define PSGR_TELEM_RATE_HZ          10      /* Telemetry update rate */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Safety alert level */
typedef enum {
    SAFETY_GREEN,               /* All systems nominal */
    SAFETY_YELLOW,              /* Minor anomaly detected */
    SAFETY_ORANGE,              /* Significant concern */
    SAFETY_RED                  /* Emergency condition */
} safety_level_t;

/** Safety event type */
typedef enum {
    PSGR_EVENT_NONE,
    PSGR_EVENT_DECELERATION_HIGH,
    PSGR_EVENT_PRESSURE_CHANGE,
    PSGR_EVENT_VIBRATION_HIGH,
    PSGR_EVENT_TEMPERATURE_EXTREME,
    PSGR_EVENT_LEVITATION_FAULT,
    PSGR_EVENT_BRAKE_FAILURE,
    PSGR_EVENT_DOOR_BREACH,
    PSGR_EVENT_FIRE_DETECTED,
    PSGR_EVENT_MEDICAL_EMERGENCY,
    PSGR_EVENT_COLLISION_WARNING
} safety_event_t;

/** Passenger comfort metrics */
typedef struct {
    float g_force_longitudinal;
    float g_force_lateral;
    float g_force_vertical;
    float jerk_mps3;
    float noise_level_db;
    float vibration_rms_g;
    uint8_t comfort_score;      /* 0-100 */
} passenger_comfort_t;

/** Pod safety summary */
typedef struct {
    uint8_t pod_id;
    safety_level_t level;
    safety_event_t active_events[4];
    uint8_t event_count;
    uint8_t passenger_count;
    passenger_comfort_t comfort;
    bool emergency_active;
    uint32_t last_event_time;
} pod_safety_summary_t;

/** System-wide safety status */
typedef struct {
    uint8_t active_pods;
    uint8_t pods_in_emergency;
    uint16_t total_passengers;
    safety_level_t system_level;
    uint32_t trips_today;
    uint32_t incidents_today;
    float system_uptime_pct;
} system_safety_status_t;

/** Telemetry frame */
typedef struct {
    uint32_t timestamp_ms;
    uint16_t sequence;
    uint8_t pod_id;
    pod_safety_summary_t pod_summary;
    uint8_t raw_data[64];
    uint16_t crc;
} safety_telem_frame_t;

/** Module configuration */
typedef struct {
    uint8_t max_pods;
    uint8_t telemetry_rate_hz;
    float g_force_limit;
    float decel_warning_mps2;
    bool auto_emergency;
    bool passenger_alerts;
} safety_telem_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int safety_telem_init(const safety_telem_config_t *config);
void safety_telem_shutdown(void);

int safety_telem_update_pod(uint8_t pod_id, const pod_safety_summary_t *summary);
int safety_telem_get_pod_status(uint8_t pod_id, pod_safety_summary_t *summary);
int safety_telem_get_system_status(system_safety_status_t *status);

int safety_telem_report_event(uint8_t pod_id, safety_event_t event);
int safety_telem_clear_event(uint8_t pod_id, safety_event_t event);

int safety_telem_get_frame(uint8_t pod_id, safety_telem_frame_t *frame);
int safety_telem_broadcast_alert(safety_event_t event);

void safety_telem_update(uint32_t elapsed_ms);

#endif /* GF_PASSENGER_SAFETY_TELEMETRY_H */
