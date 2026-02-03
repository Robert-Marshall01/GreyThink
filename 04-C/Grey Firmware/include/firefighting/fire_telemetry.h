/**
 * @file fire_telemetry.h
 * @brief Emergency Telemetry Collector for Smart Firefighting Systems
 * 
 * INDUSTRY RELEVANCE:
 * Fire incident data is critical for first responder coordination, insurance
 * claims, and post-incident investigation. This module enables:
 * - Real-time fire location and spread tracking
 * - Suppression system activation logging
 * - Firefighter location and status monitoring
 * - Integration with emergency dispatch (CAD) systems
 * 
 * Target applications: Building fire panels, wildfire monitoring stations,
 * incident command systems, fire investigation tools, insurance IoT.
 * 
 * Standards: NFPA 1221 (emergency communications), APCO P25 (radio),
 *            NIMS (incident command), CAP (Common Alerting Protocol)
 */

#ifndef GF_FIRE_TELEMETRY_H
#define GF_FIRE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Fire Telemetry Types                                                       */
/*===========================================================================*/

typedef enum {
    FIRE_STAGE_INCIPIENT,   /* Early detection, smoke only */
    FIRE_STAGE_GROWTH,      /* Flames visible, spreading */
    FIRE_STAGE_FULLY_DEV,   /* Fully developed fire */
    FIRE_STAGE_DECAY,       /* Fire diminishing */
    FIRE_STAGE_EXTINGUISHED /* Fire extinguished */
} fire_stage_t;

typedef enum {
    ALARM_SMOKE,            /* Smoke detector alarm */
    ALARM_HEAT,             /* Heat detector alarm */
    ALARM_FLAME,            /* Flame detector alarm */
    ALARM_MANUAL,           /* Manual pull station */
    ALARM_SPRINKLER_FLOW,   /* Water flow detected */
    ALARM_THERMAL_IMAGING,  /* Thermal camera detection */
    ALARM_GAS              /* Combustible gas detected */
} alarm_type_t;

typedef enum {
    EVENT_ALARM_INIT,       /* Initial alarm */
    EVENT_ALARM_VERIFY,     /* Alarm verified */
    EVENT_DISPATCH,         /* Fire department dispatched */
    EVENT_SUPPRESSION_ACT,  /* Suppression activated */
    EVENT_SUPPRESSION_COMP, /* Suppression complete */
    EVENT_ALL_CLEAR,        /* All clear given */
    EVENT_RESET             /* System reset */
} fire_event_t;

typedef struct {
    uint32_t incident_id;
    uint32_t start_time;
    fire_stage_t stage;
    uint8_t affected_zones;     /* Bitmap of affected zones */
    uint8_t suppression_zones;  /* Bitmap of activated suppression */
    float max_temperature;
    float spread_rate_m2s;      /* Fire spread rate (m²/s) */
    uint16_t detector_count;    /* Number of detectors in alarm */
    bool evacuation_active;
    bool fire_dept_notified;
    bool suppression_effective;
} incident_status_t;

typedef struct {
    fire_event_t event;
    uint32_t timestamp;
    uint8_t zone_id;
    alarm_type_t alarm_type;
    float temperature;
    char details[48];
} fire_event_record_t;

typedef struct {
    uint8_t firefighter_id;
    float latitude;
    float longitude;
    uint8_t floor;
    float air_remaining_min;    /* SCBA air remaining */
    float ambient_temp;
    bool mayday;                /* Mayday button pressed */
    bool motion_detected;       /* Motion sensor active */
    uint32_t last_update;
} firefighter_status_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int fire_telemetry_init(void);
int fire_telemetry_shutdown(void);

int fire_start_incident(uint32_t* incident_id);
int fire_get_incident_status(uint32_t incident_id, incident_status_t* status);
int fire_end_incident(uint32_t incident_id);

int fire_log_event(uint32_t incident_id, const fire_event_record_t* event);
int fire_get_event_log(uint32_t incident_id, fire_event_record_t* events,
                      uint16_t max_events, uint16_t* count);

int fire_update_firefighter(const firefighter_status_t* status);
int fire_get_firefighters(firefighter_status_t* statuses, uint8_t max_count,
                         uint8_t* count);

int fire_notify_dispatch(uint32_t incident_id, const char* address);
int fire_transmit_telemetry(uint32_t incident_id);

int fire_export_incident_report(uint32_t incident_id, uint8_t* data,
                               uint32_t max_len, uint32_t* len);

#endif /* GF_FIRE_TELEMETRY_H */
