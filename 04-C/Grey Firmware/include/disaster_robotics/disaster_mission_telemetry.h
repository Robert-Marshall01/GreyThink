/**
 * @file disaster_mission_telemetry.h
 * @brief Telemetry Collector for Disaster Response Mission Status
 * 
 * INDUSTRY RELEVANCE:
 * Disaster response operations require real-time mission telemetry for
 * command center coordination, operator situational awareness, and
 * post-incident analysis. This module aggregates robot status, mission
 * progress, hazard maps, and victim locations for comprehensive reporting.
 * 
 * Applications:
 * - USAR team coordination
 * - Multi-robot swarm coordination
 * - Incident command systems
 * - First responder dashboards
 * - After-action reporting
 * 
 * Standards: NIMS (National Incident Management System), DHS SAFECOM
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_DISASTER_MISSION_TELEMETRY_H
#define GF_DISASTER_MISSION_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define MISSION_MAX_ROBOTS          16      /* Maximum robots tracked */
#define MISSION_MAX_WAYPOINTS       64      /* Maximum waypoints */
#define MISSION_TELEM_RATE_HZ       5       /* Telemetry update rate */
#define MISSION_LOG_DEPTH           1000    /* Event log depth */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Mission phase */
typedef enum {
    MISSION_PHASE_STANDBY,      /* Awaiting deployment */
    MISSION_PHASE_DEPLOY,       /* Deploying to site */
    MISSION_PHASE_RECON,        /* Initial reconnaissance */
    MISSION_PHASE_SEARCH,       /* Active search */
    MISSION_PHASE_RESCUE,       /* Victim extraction */
    MISSION_PHASE_STABILIZE,    /* Structural stabilization */
    MISSION_PHASE_RECOVERY,     /* Evidence/remains recovery */
    MISSION_PHASE_WITHDRAW,     /* Withdrawal from site */
    MISSION_PHASE_COMPLETE      /* Mission complete */
} mission_phase_t;

/** Robot status */
typedef enum {
    ROBOT_IDLE,
    ROBOT_NAVIGATING,
    ROBOT_SEARCHING,
    ROBOT_EXTRACTING,
    ROBOT_RETURNING,
    ROBOT_CHARGING,
    ROBOT_FAULT,
    ROBOT_LOST_COMM
} robot_status_t;

/** Mission event type */
typedef enum {
    EVENT_MISSION_START,
    EVENT_PHASE_CHANGE,
    EVENT_VICTIM_FOUND,
    EVENT_VICTIM_EXTRACTED,
    EVENT_HAZARD_DETECTED,
    EVENT_ROBOT_DEPLOYED,
    EVENT_ROBOT_FAULT,
    EVENT_COMMS_LOST,
    EVENT_COMMS_RESTORED,
    EVENT_AREA_CLEARED,
    EVENT_MISSION_ABORT,
    EVENT_MISSION_COMPLETE
} mission_event_t;

/** Robot position */
typedef struct {
    float x_m;
    float y_m;
    float z_m;
    float heading_deg;
    float speed_mps;
    uint32_t timestamp_ms;
} robot_position_t;

/** Robot telemetry */
typedef struct {
    uint8_t robot_id;
    char robot_name[16];
    robot_status_t status;
    robot_position_t position;
    float battery_pct;
    float signal_strength_dbm;
    uint32_t uptime_s;
    uint16_t victims_found;
    uint16_t area_searched_m2;
} robot_telemetry_t;

/** Mission summary */
typedef struct {
    uint32_t mission_id;
    char incident_name[32];
    mission_phase_t phase;
    uint32_t start_time_unix;
    uint32_t elapsed_s;
    uint8_t robots_deployed;
    uint8_t robots_active;
    uint16_t victims_found;
    uint16_t victims_extracted;
    float area_searched_m2;
    float area_remaining_m2;
} mission_summary_t;

/** Event log entry */
typedef struct {
    uint32_t timestamp_ms;
    mission_event_t type;
    uint8_t robot_id;
    char description[48];
    float position_x;
    float position_y;
} event_log_entry_t;

/** Module configuration */
typedef struct {
    uint32_t mission_id;
    const char *incident_name;
    uint8_t telemetry_rate_hz;
    bool auto_logging;
    bool mesh_relay;
} mission_telem_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int mission_telem_init(const mission_telem_config_t *config);
void mission_telem_shutdown(void);

int mission_telem_set_phase(mission_phase_t phase);
int mission_telem_update_robot(const robot_telemetry_t *robot);
int mission_telem_log_event(mission_event_t event, uint8_t robot_id, const char *desc);

int mission_telem_get_summary(mission_summary_t *summary);
int mission_telem_get_robot(uint8_t robot_id, robot_telemetry_t *robot);
int mission_telem_get_events(event_log_entry_t *events, uint16_t max_count);

int mission_telem_report_victim(float x, float y, float confidence);
int mission_telem_mark_area_searched(float x, float y, float radius_m);

void mission_telem_update(uint32_t elapsed_ms);

#endif /* GF_DISASTER_MISSION_TELEMETRY_H */
