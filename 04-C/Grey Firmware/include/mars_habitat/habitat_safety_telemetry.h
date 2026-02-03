/**
 * @file habitat_safety_telemetry.h
 * @brief Telemetry Collector for Mars Habitat Safety Monitoring
 * 
 * INDUSTRY RELEVANCE:
 * Continuous telemetry collection is essential for habitat safety verification
 * and ground control situational awareness. This module aggregates sensor data
 * from all life support subsystems, formats CCSDS-compatible packets, and
 * provides real-time anomaly detection for crew safety.
 * 
 * Applications:
 * - Mars habitat mission control interface
 * - Autonomous habitat health monitoring
 * - Crew alert systems
 * - Long-duration mission data logging
 * - Post-incident analysis
 * 
 * Standards: CCSDS 133.0-B-2, NASA-STD-3001, ECSS-E-ST-50C (Communications)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_HABITAT_SAFETY_TELEMETRY_H
#define GF_HABITAT_SAFETY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define HAB_TELEM_MAX_CHANNELS      64      /* Maximum telemetry channels */
#define HAB_TELEM_FRAME_SIZE        256     /* Telemetry frame size bytes */
#define HAB_TELEM_RATE_HZ           1       /* Nominal telemetry rate */
#define HAB_TELEM_HISTORY_DEPTH     3600    /* 1 hour at 1 Hz */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Telemetry priority */
typedef enum {
    HAB_TELEM_ROUTINE,          /* Normal periodic telemetry */
    HAB_TELEM_ELEVATED,         /* Increased attention needed */
    HAB_TELEM_URGENT,           /* Requires prompt action */
    HAB_TELEM_EMERGENCY         /* Immediate action required */
} hab_telem_priority_t;

/** Safety event type */
typedef enum {
    HAB_EVENT_NONE,
    HAB_EVENT_PRESSURE_DROP,
    HAB_EVENT_O2_LOW,
    HAB_EVENT_CO2_HIGH,
    HAB_EVENT_TEMP_EXTREME,
    HAB_EVENT_FIRE_DETECTED,
    HAB_EVENT_RADIATION_SPIKE,
    HAB_EVENT_SEAL_BREACH,
    HAB_EVENT_POWER_LOSS,
    HAB_EVENT_COMM_LOSS
} hab_safety_event_t;

/** Telemetry point */
typedef struct {
    uint16_t channel_id;
    char name[24];
    float value;
    float min_limit;
    float max_limit;
    uint32_t timestamp_ms;
    bool in_limits;
    bool valid;
} hab_telem_point_t;

/** Safety status summary */
typedef struct {
    hab_safety_event_t active_events[8];
    uint8_t event_count;
    hab_telem_priority_t overall_priority;
    uint32_t last_event_time;
    bool crew_alerted;
    bool ground_notified;
} hab_safety_status_t;

/** Telemetry frame (CCSDS compatible) */
typedef struct {
    uint32_t sync_marker;
    uint16_t apid;
    uint16_t sequence;
    uint16_t length;
    uint32_t mission_time_s;
    hab_safety_status_t safety;
    uint8_t payload[200];
    uint16_t crc;
} hab_telem_frame_t;

/** Module configuration */
typedef struct {
    uint16_t apid;
    uint8_t telemetry_rate_hz;
    bool auto_alert;
    bool store_local;
    bool deep_space_mode;
} hab_telem_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int hab_telem_init(const hab_telem_config_t *config);
void hab_telem_shutdown(void);

int hab_telem_register_channel(uint16_t channel_id, const char *name,
                                float min_limit, float max_limit);
int hab_telem_update_value(uint16_t channel_id, float value);

int hab_telem_get_frame(hab_telem_frame_t *frame);
int hab_telem_get_safety_status(hab_safety_status_t *status);

int hab_telem_report_event(hab_safety_event_t event);
int hab_telem_clear_event(hab_safety_event_t event);

void hab_telem_update(uint32_t elapsed_ms);

#endif /* GF_HABITAT_SAFETY_TELEMETRY_H */
