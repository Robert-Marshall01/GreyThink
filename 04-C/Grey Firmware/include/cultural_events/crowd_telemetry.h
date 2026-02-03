/**
 * @file crowd_telemetry.h
 * @brief Crowd Analytics and Flow Telemetry Interface
 * 
 * INDUSTRY RELEVANCE:
 * Crowd management critical for safety at large events:
 * - Hajj pilgrimage (millions of attendees)
 * - Music festivals (Coachella, Glastonbury)
 * - Sports events (World Cup, Olympics)
 * - Emergency evacuation planning
 * 
 * Prevents crowd crush incidents through real-time density
 * monitoring and flow optimization. Embedded engineers:
 * - Deploy edge vision for crowd counting
 * - Calculate density heatmaps
 * - Detect abnormal crowd behavior
 * - Trigger automated crowd control
 * 
 * STANDARDS:
 * - BS 9999 (Fire Safety in Buildings)
 * - ISO 45001 (Occupational Safety)
 * - Event Safety Guide (HSE UK)
 * - NFPA 102 (Assembly Seating)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CROWD_TELEMETRY_H
#define GF_CROWD_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define CROWD_MAX_ZONES           256  /**< Maximum monitored zones */
#define CROWD_MAX_CAMERAS         512  /**< Maximum vision cameras */
#define CROWD_DENSITY_CRITICAL    4.0f /**< Critical density (persons/m²) */
#define CROWD_DENSITY_WARNING     2.5f /**< Warning density threshold */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Crowd flow state */
typedef enum {
    FLOW_FREE,           /**< Free movement */
    FLOW_RESTRICTED,     /**< Slowed movement */
    FLOW_CONGESTED,      /**< Stop-and-go */
    FLOW_JAMMED,         /**< No movement */
    FLOW_TURBULENT       /**< Dangerous crowd waves */
} crowd_flow_t;

/** Alert severity */
typedef enum {
    ALERT_NONE,
    ALERT_ADVISORY,
    ALERT_WARNING,
    ALERT_CRITICAL,
    ALERT_EMERGENCY
} crowd_alert_t;

/** Crowd behavior */
typedef enum {
    BEHAVIOR_NORMAL,
    BEHAVIOR_RUSHING,
    BEHAVIOR_DISPERSING,
    BEHAVIOR_GATHERING,
    BEHAVIOR_PANICKING
} crowd_behavior_t;

/** Crowd zone metrics */
typedef struct {
    char zone_id[16];
    uint32_t count;
    float density_per_m2;
    float velocity_ms;       /**< Average movement speed */
    float direction_deg;     /**< Dominant flow direction */
    crowd_flow_t flow_state;
    crowd_behavior_t behavior;
    crowd_alert_t alert_level;
    uint32_t timestamp;
} crowd_zone_metrics_t;

/** Evacuation status */
typedef struct {
    char zone_id[16];
    uint32_t remaining_count;
    float evacuation_rate;   /**< persons/minute */
    uint32_t estimated_clear_sec;
    bool exits_clear;
} evacuation_status_t;

/** Event-wide metrics */
typedef struct {
    uint32_t total_attendance;
    uint32_t current_inside;
    float peak_density;
    uint32_t zones_at_warning;
    uint32_t zones_at_critical;
    crowd_alert_t max_alert;
} event_crowd_metrics_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int crowd_telemetry_init(void);
int crowd_register_zone(const char *zone_id, float area_m2, 
                        uint32_t max_capacity);
int crowd_add_camera(uint32_t camera_id, const char *zone_id);
int crowd_update_count(const char *zone_id, uint32_t count);
int crowd_get_zone_metrics(const char *zone_id, crowd_zone_metrics_t *metrics);
int crowd_get_event_metrics(event_crowd_metrics_t *metrics);
int crowd_start_evacuation(void);
int crowd_get_evacuation_status(const char *zone_id, 
                                evacuation_status_t *status);
int crowd_set_density_thresholds(float warning, float critical);
crowd_alert_t crowd_check_alerts(void);
void crowd_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CROWD_TELEMETRY_H */
