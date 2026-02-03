/**
 * @file audience_telemetry.h
 * @brief Audience Telemetry Collector for Performance Arts Systems
 * 
 * INDUSTRY RELEVANCE:
 * Live entertainment venues increasingly use audience sensing to optimize
 * show delivery, ensure safety, and gather engagement metrics. This module
 * aggregates crowd density, movement patterns, environmental comfort, and
 * engagement indicators for adaptive show control and venue management.
 * 
 * Applications:
 * - Concert and festival crowd management
 * - Theme park attraction optimization
 * - Sports venue experience systems
 * - Immersive theater experiences
 * - Museum/exhibition interactivity
 * 
 * Standards: EN 13200 (Spectator Facilities), NFPA 102 (Assembly Seating)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_AUDIENCE_TELEMETRY_H
#define GF_AUDIENCE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define AUDIENCE_MAX_ZONES          64      /* Maximum audience zones */
#define AUDIENCE_MAX_SENSORS        256     /* Maximum sensor inputs */
#define AUDIENCE_TELEMETRY_RATE_HZ  10      /* Update frequency */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Audience sensor type */
typedef enum {
    SENSOR_THERMAL_CAMERA,      /* Crowd thermal imaging */
    SENSOR_LIDAR_CROWD,         /* 3D crowd density */
    SENSOR_AUDIO_SPL,           /* Sound pressure level */
    SENSOR_CHEERING,            /* Audio engagement detection */
    SENSOR_MOTION,              /* Movement detection */
    SENSOR_TICKET_SCAN,         /* Entry/exit counting */
    SENSOR_WEARABLE             /* Wristband/RFID tracking */
} audience_sensor_t;

/** Engagement level */
typedef enum {
    ENGAGEMENT_LOW,
    ENGAGEMENT_MODERATE,
    ENGAGEMENT_HIGH,
    ENGAGEMENT_PEAK,
    ENGAGEMENT_DECLINING
} engagement_level_t;

/** Safety status */
typedef enum {
    CROWD_SAFE,
    CROWD_DENSE,
    CROWD_CONGESTED,
    CROWD_CRITICAL
} crowd_safety_t;

/** Zone metrics */
typedef struct {
    uint8_t zone_id;
    char zone_name[24];
    uint16_t capacity;
    uint16_t current_count;
    float density_ppm2;         /* People per square meter */
    float temperature_c;
    float noise_level_db;
    engagement_level_t engagement;
    crowd_safety_t safety;
    float flow_rate_ppm;        /* People per minute movement */
} zone_metrics_t;

/** Audience engagement */
typedef struct {
    float applause_level;       /* 0-100 */
    float cheering_level;       /* 0-100 */
    float movement_energy;      /* 0-100 */
    float participation_rate;   /* Percentage participating */
    uint32_t peak_moments;      /* Count of peak engagement */
    uint32_t duration_s;
} audience_engagement_t;

/** Venue summary */
typedef struct {
    uint32_t total_capacity;
    uint32_t current_attendance;
    float occupancy_pct;
    uint8_t zones_at_capacity;
    crowd_safety_t overall_safety;
    audience_engagement_t engagement;
    float avg_temperature_c;
    float avg_noise_db;
} venue_summary_t;

/** Emergency status */
typedef struct {
    bool evacuation_needed;
    bool crush_risk_detected;
    uint8_t congested_zones[8];
    uint8_t congestion_count;
    uint32_t estimated_evac_time_s;
} emergency_status_t;

/** Module configuration */
typedef struct {
    uint32_t venue_capacity;
    uint8_t num_zones;
    float max_density_ppm2;
    float crush_threshold_ppm2;
    bool auto_alerts;
    bool wearable_tracking;
} audience_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int audience_telem_init(const audience_config_t *config);
void audience_telem_shutdown(void);

int audience_telem_add_sensor(uint8_t zone_id, audience_sensor_t type);
int audience_telem_update_count(uint8_t zone_id, int16_t delta);

int audience_telem_get_zone(uint8_t zone_id, zone_metrics_t *metrics);
int audience_telem_get_venue(venue_summary_t *summary);
int audience_telem_get_engagement(audience_engagement_t *engagement);
int audience_telem_get_emergency(emergency_status_t *status);

bool audience_telem_is_zone_safe(uint8_t zone_id);
bool audience_telem_is_venue_safe(void);

void audience_telem_update(uint32_t elapsed_ms);

#endif /* GF_AUDIENCE_TELEMETRY_H */
