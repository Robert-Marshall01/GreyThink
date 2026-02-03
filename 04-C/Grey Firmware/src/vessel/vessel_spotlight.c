/**
 * @file vessel_spotlight.c
 * @brief Autonomous Vessel Control & Safety Spotlight Implementation
 * 
 * This module implements a production-grade autonomous vessel control system
 * demonstrating:
 * - Navigation control loop (heading, speed, collision avoidance)
 * - Cargo telemetry for weight distribution and stability
 * - Safety interlock logic for emergency stop and reroute
 * - Weather adaptation and sea state response
 * - IMO/COLREGS compliance for maritime safety
 * 
 * MARITIME AUTOMATION:
 * Autonomous vessels (MASS - Maritime Autonomous Surface Ships) represent
 * the future of shipping with reduced crew, improved efficiency, and
 * enhanced safety. This implementation covers shore-to-ship control,
 * situational awareness, and automated maneuvering.
 * 
 * VESSEL DYNAMICS:
 * Ship motion involves 6 degrees of freedom (surge, sway, heave, roll,
 * pitch, yaw). Stability depends on GM (metacentric height), cargo
 * distribution, and free surface effects. This implementation models
 * simplified dynamics for control purposes.
 * 
 * COLLISION AVOIDANCE:
 * COLREGs (International Regulations for Preventing Collisions at Sea)
 * define right-of-way rules. The ARPA (Automatic Radar Plotting Aid)
 * system tracks targets and computes CPA/TCPA for collision prediction.
 * 
 * CARGO MANAGEMENT:
 * Container ships require careful load planning for stability and
 * structural integrity. Bay plans, stack weights, and lashing forces
 * must be monitored continuously.
 * 
 * STANDARDS:
 * - IMO MSC.1/Circ.1604 (MASS Regulatory Scoping)
 * - SOLAS (Safety of Life at Sea)
 * - COLREGS (Collision Regulations)
 * - ISM Code (International Safety Management)
 * - ISO 19847 (Ship data management)
 * 
 * @author Grey Firmware Project
 */

#define _USE_MATH_DEFINES
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

/* M_PI definition for portability */
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define VESSEL_MAX_WAYPOINTS      64     /**< Maximum route waypoints */
#define VESSEL_MAX_TARGETS        128    /**< ARPA tracked targets */
#define VESSEL_MAX_CARGO_BAYS     20     /**< Cargo bay count */
#define VESSEL_MAX_CONTAINERS     1000   /**< Tracked containers */
#define VESSEL_MAX_EVENTS         256    /**< Event log size */
#define VESSEL_MAX_ALARMS         32     /**< Active alarm buffer */
#define VESSEL_HISTORY_SIZE       60     /**< Position history (minutes) */

/* Navigation constants */
#define EARTH_RADIUS_NM           3440.065f  /**< Earth radius in nautical miles */
#define KNOTS_TO_MPS              0.514444f  /**< Knots to m/s conversion */
#define NM_TO_METERS              1852.0f    /**< Nautical miles to meters */

/* Safety thresholds */
#define CPA_WARNING_NM            2.0f    /**< CPA warning threshold */
#define CPA_CRITICAL_NM           0.5f    /**< CPA critical threshold */
#define TCPA_WARNING_MIN          30.0f   /**< TCPA warning (minutes) */
#define TCPA_CRITICAL_MIN         10.0f   /**< TCPA critical (minutes) */
#define ROLL_WARNING_DEG          15.0f   /**< Roll angle warning */
#define ROLL_CRITICAL_DEG         25.0f   /**< Roll angle critical */
#define PITCH_WARNING_DEG         5.0f    /**< Pitch angle warning */
#define HEEL_LIMIT_DEG            2.0f    /**< Static heel limit */
#define GM_MIN_M                  0.15f   /**< Minimum metacentric height */
#define WIND_LIMIT_KTS            50.0f   /**< Wind speed limit */
#define WAVE_LIMIT_M              8.0f    /**< Significant wave height limit */

/* Control parameters */
#define HEADING_P_GAIN            0.5f    /**< Heading controller P gain */
#define HEADING_D_GAIN            0.1f    /**< Heading controller D gain */
#define SPEED_P_GAIN              0.3f    /**< Speed controller P gain */
#define RUDDER_LIMIT_DEG          35.0f   /**< Maximum rudder angle */
#define RUDDER_RATE_DEG_S         3.0f    /**< Rudder rate limit */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Navigation mode */
typedef enum {
    NAV_MODE_MANUAL,             /**< Manual helm control */
    NAV_MODE_HEADING_HOLD,       /**< Autopilot heading hold */
    NAV_MODE_TRACK_CONTROL,      /**< Track following mode */
    NAV_MODE_WAYPOINT,           /**< Waypoint navigation */
    NAV_MODE_COLLISION_AVOIDANCE,/**< Active collision avoidance */
    NAV_MODE_EMERGENCY,          /**< Emergency maneuvering */
    NAV_MODE_DRIFT               /**< Engines stopped, drifting */
} nav_mode_t;

/** Vessel state */
typedef enum {
    VESSEL_STATE_MOORED,
    VESSEL_STATE_ANCHORED,
    VESSEL_STATE_MANEUVERING,
    VESSEL_STATE_UNDERWAY,
    VESSEL_STATE_RESTRICTED,
    VESSEL_STATE_NOT_UNDER_COMMAND,
    VESSEL_STATE_EMERGENCY
} vessel_state_t;

/** Engine order */
typedef enum {
    ENGINE_FULL_ASTERN = -4,
    ENGINE_HALF_ASTERN = -2,
    ENGINE_SLOW_ASTERN = -1,
    ENGINE_STOP = 0,
    ENGINE_DEAD_SLOW = 1,
    ENGINE_SLOW = 2,
    ENGINE_HALF = 3,
    ENGINE_FULL = 4,
    ENGINE_FLANK = 5
} engine_order_t;

/** Target type (ARPA) */
typedef enum {
    TARGET_UNKNOWN,
    TARGET_VESSEL,
    TARGET_FISHING,
    TARGET_TUG,
    TARGET_PASSENGER,
    TARGET_CARGO,
    TARGET_TANKER,
    TARGET_HSC,              /**< High Speed Craft */
    TARGET_BUOY,
    TARGET_LAND
} target_type_t;

/** Collision risk level */
typedef enum {
    RISK_NONE,
    RISK_LOW,
    RISK_MEDIUM,
    RISK_HIGH,
    RISK_CRITICAL
} collision_risk_t;

/** Cargo type */
typedef enum {
    CARGO_CONTAINER,
    CARGO_BULK,
    CARGO_TANKER,
    CARGO_BREAKBULK,
    CARGO_RORO
} cargo_type_t;

/** Container status */
typedef enum {
    CONTAINER_EMPTY,
    CONTAINER_LOADED,
    CONTAINER_REEFER,        /**< Refrigerated */
    CONTAINER_HAZMAT,
    CONTAINER_OVERWEIGHT
} container_status_t;

/** Alarm severity */
typedef enum {
    SEVERITY_INFO,
    SEVERITY_WARNING,
    SEVERITY_ALARM,
    SEVERITY_CRITICAL
} alarm_severity_t;

/** Alarm type */
typedef enum {
    ALARM_CPA_WARNING,
    ALARM_CPA_CRITICAL,
    ALARM_ROLL_EXCESSIVE,
    ALARM_STABILITY_LOW,
    ALARM_CARGO_SHIFT,
    ALARM_WEATHER_LIMIT,
    ALARM_ENGINE_FAULT,
    ALARM_STEERING_FAULT,
    ALARM_GPS_LOST,
    ALARM_RADAR_FAULT,
    ALARM_GROUNDING_RISK,
    ALARM_COLLISION_IMMINENT,
    ALARM_OVERWEIGHT,
    ALARM_LASHING_FAILURE
} alarm_type_t;

/** Sea state (Douglas scale) */
typedef enum {
    SEA_STATE_0_CALM,        /**< 0-0.1m waves */
    SEA_STATE_1_RIPPLED,     /**< 0.1-0.5m */
    SEA_STATE_2_SMOOTH,      /**< 0.5-1.25m */
    SEA_STATE_3_SLIGHT,      /**< 1.25-2.5m */
    SEA_STATE_4_MODERATE,    /**< 2.5-4m */
    SEA_STATE_5_ROUGH,       /**< 4-6m */
    SEA_STATE_6_VERY_ROUGH,  /**< 6-9m */
    SEA_STATE_7_HIGH,        /**< 9-14m */
    SEA_STATE_8_VERY_HIGH,   /**< >14m */
    SEA_STATE_9_PHENOMENAL
} sea_state_t;

/** TMR sensor voting result */
typedef enum {
    TMR_AGREE_ALL,
    TMR_AGREE_2OF3,
    TMR_DISAGREE,
    TMR_FAULT
} tmr_result_t;

/** TMR sensor data */
typedef struct {
    float values[3];
    float voted_value;
    tmr_result_t status;
    uint32_t last_update;
    bool healthy;
} tmr_sensor_t;

/** Geographic position */
typedef struct {
    double latitude;
    double longitude;
} position_t;

/** Waypoint */
typedef struct {
    uint8_t waypoint_id;
    position_t position;
    float planned_speed_kts;
    float turn_radius_nm;
    float eta_seconds;       /**< Estimated time of arrival */
    bool reached;
} waypoint_t;

/** ARPA target */
typedef struct {
    uint16_t target_id;
    target_type_t type;
    position_t position;
    float course_deg;
    float speed_kts;
    float cpa_nm;            /**< Closest Point of Approach */
    float tcpa_min;          /**< Time to CPA (minutes) */
    float bcr;               /**< Bow Crossing Range */
    float bct;               /**< Bow Crossing Time */
    collision_risk_t risk;
    uint32_t track_start;
    uint32_t last_update;
    bool lost;
} arpa_target_t;

/** Container record */
typedef struct {
    uint32_t container_id;
    char iso_code[12];       /**< Container ISO code */
    container_status_t status;
    float weight_kg;
    float max_weight_kg;
    uint8_t bay;
    uint8_t row;
    uint8_t tier;
    int8_t temp_setpoint_c;  /**< For reefers */
    bool hazmat;
    bool overweight;
} container_t;

/** Cargo bay status */
typedef struct {
    uint8_t bay_id;
    float total_weight_kg;
    float max_weight_kg;
    float lcg_m;             /**< Longitudinal CG */
    float tcg_m;             /**< Transverse CG */
    float vcg_m;             /**< Vertical CG */
    uint16_t container_count;
    bool overloaded;
} cargo_bay_t;

/** Stability data */
typedef struct {
    float displacement_t;     /**< Tonnes */
    float draft_fwd_m;
    float draft_aft_m;
    float draft_mid_m;
    float trim_m;
    float heel_deg;
    float gm_m;              /**< Metacentric height */
    float gmt_m;             /**< Transverse GM */
    float gml_m;             /**< Longitudinal GM */
    float sf_max_pct;        /**< Shear force % of limit */
    float bm_max_pct;        /**< Bending moment % of limit */
    bool stable;
} stability_t;

/** Weather data */
typedef struct {
    float wind_speed_kts;
    float wind_dir_deg;
    float wave_height_m;
    float wave_period_s;
    float wave_dir_deg;
    float swell_height_m;
    float swell_period_s;
    float swell_dir_deg;
    float current_speed_kts;
    float current_dir_deg;
    float visibility_nm;
    sea_state_t sea_state;
    uint32_t timestamp;
} weather_t;

/** Navigation data */
typedef struct {
    position_t position;
    float course_deg;        /**< Course over ground */
    float heading_deg;       /**< Ship heading */
    float speed_kts;         /**< Speed over ground */
    float sog_kts;           /**< Speed over ground */
    float stw_kts;           /**< Speed through water */
    float rate_of_turn;      /**< deg/min */
    float depth_m;
    float rudder_angle_deg;
    float drift_angle_deg;
    engine_order_t engine_order;
    float rpm;
    float pitch_deg;
    uint32_t timestamp;
} nav_data_t;

/** Active alarm */
typedef struct {
    alarm_type_t type;
    alarm_severity_t severity;
    uint32_t timestamp;
    uint16_t source_id;
    float threshold;
    float value;
    bool acknowledged;
    bool active;
} active_alarm_t;

/** Event log entry */
typedef struct {
    uint8_t category;
    uint8_t source_id;
    uint16_t event_code;
    float values[2];
    uint32_t timestamp;
} event_entry_t;

/** Telemetry header (NMEA 2000 compatible) */
typedef struct {
    uint16_t sync_word;
    uint16_t pgn;            /**< Parameter Group Number */
    uint8_t priority;
    uint8_t source_addr;
    uint8_t dest_addr;
    uint16_t data_length;
    uint16_t sequence;
    uint32_t timestamp;
} telem_header_t;

/** Main vessel state structure */
typedef struct {
    bool initialized;
    
    /* Vessel identification */
    char name[32];
    uint32_t mmsi;           /**< Maritime Mobile Service Identity */
    uint32_t imo_number;
    float loa_m;             /**< Length overall */
    float beam_m;
    float draft_m;
    float dwt_t;             /**< Deadweight tonnage */
    
    /* Navigation state */
    nav_mode_t nav_mode;
    vessel_state_t state;
    nav_data_t current;
    nav_data_t previous;
    
    /* Route */
    waypoint_t route[VESSEL_MAX_WAYPOINTS];
    uint8_t waypoint_count;
    uint8_t current_waypoint;
    float route_distance_nm;
    float route_remaining_nm;
    
    /* ARPA targets */
    arpa_target_t targets[VESSEL_MAX_TARGETS];
    uint8_t target_count;
    uint8_t dangerous_targets;
    
    /* Cargo */
    cargo_type_t cargo_type;
    container_t containers[VESSEL_MAX_CONTAINERS];
    uint16_t container_count;
    cargo_bay_t bays[VESSEL_MAX_CARGO_BAYS];
    uint8_t bay_count;
    float total_cargo_weight_kg;
    
    /* Stability */
    stability_t stability;
    tmr_sensor_t roll_sensor;
    tmr_sensor_t pitch_sensor;
    
    /* Weather */
    weather_t weather;
    bool weather_limit_exceeded;
    
    /* Time tracking */
    uint32_t current_time_ms;
    uint32_t last_process_ms;
    uint32_t uptime_seconds;
    
    /* Control state */
    float heading_setpoint_deg;
    float speed_setpoint_kts;
    float rudder_command_deg;
    float heading_error_prev;
    bool collision_avoidance_active;
    bool emergency_stop;
    
    /* Alarms */
    active_alarm_t alarms[VESSEL_MAX_ALARMS];
    uint8_t alarm_count;
    uint8_t unacked_alarms;
    
    /* Events */
    event_entry_t events[VESSEL_MAX_EVENTS];
    uint16_t event_write_idx;
    uint16_t event_count;
    
    /* Telemetry */
    uint16_t telemetry_seq;
    
    /* Statistics */
    float total_distance_nm;
    uint32_t collision_avoidance_count;
    uint32_t emergency_count;
    float max_roll_deg;
    float min_gm_m;
    
    /* Response timing */
    uint32_t last_avoidance_response_us;
    uint32_t max_avoidance_response_us;
} vessel_system_t;

/* ============================================================================
 * Global State
 * ============================================================================ */

static vessel_system_t g_vessel = {0};

/* ============================================================================
 * TMR Voting Functions
 * ============================================================================ */

/**
 * @brief TMR voting for sensor readings
 */
static tmr_result_t tmr_vote(const float values[3], float *result)
{
    float diff01 = fabsf(values[0] - values[1]);
    float diff02 = fabsf(values[0] - values[2]);
    float diff12 = fabsf(values[1] - values[2]);
    
    const float tolerance = 1.0f; /* 1 degree/unit tolerance */
    
    /* Check all agree */
    if (diff01 < tolerance && diff02 < tolerance && diff12 < tolerance) {
        *result = (values[0] + values[1] + values[2]) / 3.0f;
        return TMR_AGREE_ALL;
    }
    
    /* Check 2-of-3 agreement */
    if (diff01 < tolerance) {
        *result = (values[0] + values[1]) / 2.0f;
        return TMR_AGREE_2OF3;
    }
    if (diff02 < tolerance) {
        *result = (values[0] + values[2]) / 2.0f;
        return TMR_AGREE_2OF3;
    }
    if (diff12 < tolerance) {
        *result = (values[1] + values[2]) / 2.0f;
        return TMR_AGREE_2OF3;
    }
    
    /* No agreement - use median */
    if (values[0] <= values[1] && values[1] <= values[2]) {
        *result = values[1];
    } else if (values[2] <= values[1] && values[1] <= values[0]) {
        *result = values[1];
    } else if (values[1] <= values[0] && values[0] <= values[2]) {
        *result = values[0];
    } else if (values[2] <= values[0] && values[0] <= values[1]) {
        *result = values[0];
    } else {
        *result = values[2];
    }
    
    return TMR_DISAGREE;
}

/**
 * @brief Update TMR sensor with new readings
 */
static void tmr_sensor_update(tmr_sensor_t *sensor, float v0, float v1, float v2,
                              uint32_t timestamp)
{
    sensor->values[0] = v0;
    sensor->values[1] = v1;
    sensor->values[2] = v2;
    sensor->status = tmr_vote(sensor->values, &sensor->voted_value);
    sensor->last_update = timestamp;
    sensor->healthy = (sensor->status != TMR_FAULT);
}

/* ============================================================================
 * Navigation Helpers
 * ============================================================================ */

/**
 * @brief Normalize angle to 0-360 range
 */
static float normalize_heading(float angle)
{
    while (angle < 0.0f) angle += 360.0f;
    while (angle >= 360.0f) angle -= 360.0f;
    return angle;
}

/**
 * @brief Calculate shortest angle difference
 */
static float angle_diff(float target, float current)
{
    float diff = target - current;
    while (diff > 180.0f) diff -= 360.0f;
    while (diff < -180.0f) diff += 360.0f;
    return diff;
}

/**
 * @brief Calculate distance between positions (Haversine formula)
 */
static float calc_distance_nm(const position_t *p1, const position_t *p2)
{
    double lat1 = p1->latitude * M_PI / 180.0;
    double lat2 = p2->latitude * M_PI / 180.0;
    double dlat = (p2->latitude - p1->latitude) * M_PI / 180.0;
    double dlon = (p2->longitude - p1->longitude) * M_PI / 180.0;
    
    double a = sin(dlat/2) * sin(dlat/2) +
               cos(lat1) * cos(lat2) * sin(dlon/2) * sin(dlon/2);
    double c = 2 * atan2(sqrt(a), sqrt(1-a));
    
    return (float)(EARTH_RADIUS_NM * c);
}

/**
 * @brief Calculate bearing between positions
 */
static float calc_bearing_deg(const position_t *from, const position_t *to)
{
    double lat1 = from->latitude * M_PI / 180.0;
    double lat2 = to->latitude * M_PI / 180.0;
    double dlon = (to->longitude - from->longitude) * M_PI / 180.0;
    
    double y = sin(dlon) * cos(lat2);
    double x = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon);
    
    float bearing = (float)(atan2(y, x) * 180.0 / M_PI);
    return normalize_heading(bearing);
}

/* ============================================================================
 * Alarm Management
 * ============================================================================ */

/**
 * @brief Trigger a new alarm
 */
static void trigger_alarm(alarm_type_t type, alarm_severity_t severity,
                          uint16_t source_id, float threshold, float value)
{
    /* Check for existing alarm */
    for (uint8_t i = 0; i < g_vessel.alarm_count; i++) {
        if (g_vessel.alarms[i].type == type && 
            g_vessel.alarms[i].source_id == source_id &&
            g_vessel.alarms[i].active) {
            /* Update existing */
            g_vessel.alarms[i].value = value;
            g_vessel.alarms[i].timestamp = g_vessel.current_time_ms;
            if (severity > g_vessel.alarms[i].severity) {
                g_vessel.alarms[i].severity = severity;
            }
            return;
        }
    }
    
    /* Add new alarm */
    if (g_vessel.alarm_count < VESSEL_MAX_ALARMS) {
        active_alarm_t *alarm = &g_vessel.alarms[g_vessel.alarm_count];
        alarm->type = type;
        alarm->severity = severity;
        alarm->source_id = source_id;
        alarm->threshold = threshold;
        alarm->value = value;
        alarm->timestamp = g_vessel.current_time_ms;
        alarm->acknowledged = false;
        alarm->active = true;
        g_vessel.alarm_count++;
        g_vessel.unacked_alarms++;
    }
}

/**
 * @brief Clear an alarm
 */
static void clear_alarm(alarm_type_t type, uint16_t source_id)
{
    for (uint8_t i = 0; i < g_vessel.alarm_count; i++) {
        if (g_vessel.alarms[i].type == type && 
            g_vessel.alarms[i].source_id == source_id &&
            g_vessel.alarms[i].active) {
            g_vessel.alarms[i].active = false;
            if (!g_vessel.alarms[i].acknowledged) {
                g_vessel.unacked_alarms--;
            }
        }
    }
}

/* ============================================================================
 * Event Logging
 * ============================================================================ */

/**
 * @brief Log an event to the ring buffer
 */
static void log_event(uint8_t category, uint8_t source_id, uint16_t event_code,
                      float value1, float value2)
{
    event_entry_t *event = &g_vessel.events[g_vessel.event_write_idx];
    event->category = category;
    event->source_id = source_id;
    event->event_code = event_code;
    event->values[0] = value1;
    event->values[1] = value2;
    event->timestamp = g_vessel.current_time_ms;
    
    g_vessel.event_write_idx = (g_vessel.event_write_idx + 1) % VESSEL_MAX_EVENTS;
    if (g_vessel.event_count < VESSEL_MAX_EVENTS) {
        g_vessel.event_count++;
    }
}

/* ============================================================================
 * ARPA & Collision Avoidance
 * ============================================================================ */

/**
 * @brief Calculate CPA/TCPA for a target
 */
static void calculate_cpa_tcpa(arpa_target_t *target)
{
    if (!g_vessel.initialized) return;
    
    /* Relative position and velocity */
    float rel_x = (float)((target->position.longitude - g_vessel.current.position.longitude) * 
                  60.0 * cos(g_vessel.current.position.latitude * M_PI / 180.0));
    float rel_y = (float)((target->position.latitude - g_vessel.current.position.latitude) * 60.0);
    
    /* Own ship velocity components */
    float own_vx = g_vessel.current.speed_kts * sinf(g_vessel.current.course_deg * M_PI / 180.0f);
    float own_vy = g_vessel.current.speed_kts * cosf(g_vessel.current.course_deg * M_PI / 180.0f);
    
    /* Target velocity components */
    float tgt_vx = target->speed_kts * sinf(target->course_deg * M_PI / 180.0f);
    float tgt_vy = target->speed_kts * cosf(target->course_deg * M_PI / 180.0f);
    
    /* Relative velocity */
    float rel_vx = tgt_vx - own_vx;
    float rel_vy = tgt_vy - own_vy;
    float rel_speed = sqrtf(rel_vx * rel_vx + rel_vy * rel_vy);
    
    if (rel_speed < 0.1f) {
        /* Targets moving together - no collision risk */
        target->cpa_nm = calc_distance_nm(&g_vessel.current.position, &target->position);
        target->tcpa_min = 999.0f;
        target->risk = RISK_NONE;
        return;
    }
    
    /* TCPA calculation */
    float dot = rel_x * rel_vx + rel_y * rel_vy;
    float tcpa_hrs = -dot / (rel_speed * rel_speed);
    target->tcpa_min = tcpa_hrs * 60.0f;
    
    /* CPA calculation */
    if (tcpa_hrs < 0) {
        /* Already past CPA */
        target->cpa_nm = calc_distance_nm(&g_vessel.current.position, &target->position);
        target->tcpa_min = 0.0f;
    } else {
        /* Future CPA position */
        float cpa_x = rel_x + rel_vx * tcpa_hrs;
        float cpa_y = rel_y + rel_vy * tcpa_hrs;
        target->cpa_nm = sqrtf(cpa_x * cpa_x + cpa_y * cpa_y);
    }
    
    /* Risk assessment */
    if (target->tcpa_min < 0 || target->tcpa_min > 60.0f) {
        target->risk = RISK_NONE;
    } else if (target->cpa_nm < CPA_CRITICAL_NM && target->tcpa_min < TCPA_CRITICAL_MIN) {
        target->risk = RISK_CRITICAL;
    } else if (target->cpa_nm < CPA_WARNING_NM && target->tcpa_min < TCPA_WARNING_MIN) {
        target->risk = RISK_HIGH;
    } else if (target->cpa_nm < CPA_WARNING_NM) {
        target->risk = RISK_MEDIUM;
    } else {
        target->risk = RISK_LOW;
    }
}

/**
 * @brief Process all ARPA targets
 */
static void process_arpa_targets(void)
{
    g_vessel.dangerous_targets = 0;
    
    for (uint8_t i = 0; i < g_vessel.target_count; i++) {
        arpa_target_t *target = &g_vessel.targets[i];
        if (target->lost) continue;
        
        calculate_cpa_tcpa(target);
        
        /* Count dangerous targets */
        if (target->risk >= RISK_HIGH) {
            g_vessel.dangerous_targets++;
        }
        
        /* Generate alarms */
        if (target->risk == RISK_CRITICAL) {
            trigger_alarm(ALARM_CPA_CRITICAL, SEVERITY_CRITICAL, target->target_id,
                         CPA_CRITICAL_NM, target->cpa_nm);
        } else if (target->risk == RISK_HIGH) {
            trigger_alarm(ALARM_CPA_WARNING, SEVERITY_ALARM, target->target_id,
                         CPA_WARNING_NM, target->cpa_nm);
        } else {
            clear_alarm(ALARM_CPA_CRITICAL, target->target_id);
            clear_alarm(ALARM_CPA_WARNING, target->target_id);
        }
    }
    
    /* Enable collision avoidance if needed */
    if (g_vessel.dangerous_targets > 0 && 
        g_vessel.nav_mode != NAV_MODE_EMERGENCY &&
        g_vessel.nav_mode != NAV_MODE_MANUAL) {
        if (!g_vessel.collision_avoidance_active) {
            g_vessel.collision_avoidance_active = true;
            g_vessel.collision_avoidance_count++;
            log_event(1, 0, 0x1000, (float)g_vessel.dangerous_targets, 0.0f);
        }
    } else if (g_vessel.dangerous_targets == 0 && g_vessel.collision_avoidance_active) {
        g_vessel.collision_avoidance_active = false;
        log_event(1, 0, 0x1001, 0.0f, 0.0f);
    }
}

/* ============================================================================
 * Navigation Control
 * ============================================================================ */

/**
 * @brief Heading control loop (PD controller)
 */
static void heading_control(float target_heading)
{
    float error = angle_diff(target_heading, g_vessel.current.heading_deg);
    float rate = g_vessel.current.rate_of_turn;
    
    /* PD control */
    float rudder = HEADING_P_GAIN * error - HEADING_D_GAIN * rate;
    
    /* Limit rudder angle */
    if (rudder > RUDDER_LIMIT_DEG) rudder = RUDDER_LIMIT_DEG;
    if (rudder < -RUDDER_LIMIT_DEG) rudder = -RUDDER_LIMIT_DEG;
    
    /* Rate limiting */
    float delta = rudder - g_vessel.rudder_command_deg;
    float max_delta = RUDDER_RATE_DEG_S * 0.1f; /* Assuming 100ms loop */
    if (delta > max_delta) delta = max_delta;
    if (delta < -max_delta) delta = -max_delta;
    
    g_vessel.rudder_command_deg += delta;
    g_vessel.heading_error_prev = error;
}

/**
 * @brief Collision avoidance maneuver
 */
static void collision_avoidance_maneuver(void)
{
    if (!g_vessel.collision_avoidance_active) return;
    
    uint32_t start_us = g_vessel.current_time_ms * 1000;
    
    /* Find most dangerous target */
    arpa_target_t *worst = NULL;
    float worst_risk = 0.0f;
    
    for (uint8_t i = 0; i < g_vessel.target_count; i++) {
        arpa_target_t *target = &g_vessel.targets[i];
        if (target->lost || target->risk < RISK_HIGH) continue;
        
        float risk_score = (CPA_WARNING_NM - target->cpa_nm) / CPA_WARNING_NM +
                          (TCPA_WARNING_MIN - target->tcpa_min) / TCPA_WARNING_MIN;
        if (risk_score > worst_risk) {
            worst_risk = risk_score;
            worst = target;
        }
    }
    
    if (worst == NULL) return;
    
    /* Determine avoidance action per COLREGs */
    float relative_bearing = normalize_heading(
        calc_bearing_deg(&g_vessel.current.position, &worst->position) - 
        g_vessel.current.heading_deg);
    
    float avoidance_heading = g_vessel.current.heading_deg;
    
    if (relative_bearing < 180.0f) {
        /* Target to starboard - turn starboard (Rule 14/15) */
        avoidance_heading = normalize_heading(g_vessel.current.heading_deg + 30.0f);
    } else {
        /* Target to port - generally maintain or turn starboard */
        avoidance_heading = normalize_heading(g_vessel.current.heading_deg + 15.0f);
    }
    
    /* Apply avoidance heading */
    g_vessel.heading_setpoint_deg = avoidance_heading;
    heading_control(avoidance_heading);
    
    /* Track response time */
    uint32_t response_us = g_vessel.current_time_ms * 1000 - start_us;
    g_vessel.last_avoidance_response_us = response_us;
    if (response_us > g_vessel.max_avoidance_response_us) {
        g_vessel.max_avoidance_response_us = response_us;
    }
    
    log_event(1, worst->target_id, 0x1010, avoidance_heading, worst->cpa_nm);
}

/**
 * @brief Waypoint navigation
 */
static void waypoint_navigation(void)
{
    if (g_vessel.waypoint_count == 0 || 
        g_vessel.current_waypoint >= g_vessel.waypoint_count) {
        return;
    }
    
    waypoint_t *wp = &g_vessel.route[g_vessel.current_waypoint];
    
    /* Distance and bearing to waypoint */
    float distance = calc_distance_nm(&g_vessel.current.position, &wp->position);
    float bearing = calc_bearing_deg(&g_vessel.current.position, &wp->position);
    
    /* Check if waypoint reached */
    if (distance < wp->turn_radius_nm || distance < 0.05f) {
        wp->reached = true;
        log_event(2, g_vessel.current_waypoint, 0x2000, distance, bearing);
        
        g_vessel.current_waypoint++;
        if (g_vessel.current_waypoint >= g_vessel.waypoint_count) {
            /* Route complete */
            g_vessel.nav_mode = NAV_MODE_HEADING_HOLD;
            log_event(2, 0, 0x2001, g_vessel.total_distance_nm, 0.0f);
        }
        return;
    }
    
    /* Update remaining distance */
    g_vessel.route_remaining_nm = distance;
    for (uint8_t i = g_vessel.current_waypoint + 1; i < g_vessel.waypoint_count; i++) {
        g_vessel.route_remaining_nm += calc_distance_nm(
            &g_vessel.route[i-1].position, &g_vessel.route[i].position);
    }
    
    /* Steer to waypoint */
    if (!g_vessel.collision_avoidance_active) {
        g_vessel.heading_setpoint_deg = bearing;
        g_vessel.speed_setpoint_kts = wp->planned_speed_kts;
        heading_control(bearing);
    }
}

/* ============================================================================
 * Stability Monitoring
 * ============================================================================ */

/**
 * @brief Calculate cargo center of gravity
 */
static void calculate_cargo_cg(void)
{
    float total_lcg = 0.0f;
    float total_tcg = 0.0f;
    float total_vcg = 0.0f;
    float total_weight = 0.0f;
    
    for (uint8_t i = 0; i < g_vessel.bay_count; i++) {
        cargo_bay_t *bay = &g_vessel.bays[i];
        if (bay->total_weight_kg > 0) {
            total_lcg += bay->lcg_m * bay->total_weight_kg;
            total_tcg += bay->tcg_m * bay->total_weight_kg;
            total_vcg += bay->vcg_m * bay->total_weight_kg;
            total_weight += bay->total_weight_kg;
        }
    }
    
    if (total_weight > 0) {
        g_vessel.total_cargo_weight_kg = total_weight;
    }
}

/**
 * @brief Check stability limits
 */
static void check_stability(void)
{
    stability_t *stab = &g_vessel.stability;
    
    /* Check roll */
    float roll = fabsf(g_vessel.roll_sensor.voted_value);
    if (roll > ROLL_CRITICAL_DEG) {
        trigger_alarm(ALARM_ROLL_EXCESSIVE, SEVERITY_CRITICAL, 0,
                     ROLL_CRITICAL_DEG, roll);
        stab->stable = false;
    } else if (roll > ROLL_WARNING_DEG) {
        trigger_alarm(ALARM_ROLL_EXCESSIVE, SEVERITY_WARNING, 0,
                     ROLL_WARNING_DEG, roll);
    } else {
        clear_alarm(ALARM_ROLL_EXCESSIVE, 0);
    }
    
    /* Track maximum roll */
    if (roll > g_vessel.max_roll_deg) {
        g_vessel.max_roll_deg = roll;
    }
    
    /* Check GM */
    if (stab->gm_m < GM_MIN_M) {
        trigger_alarm(ALARM_STABILITY_LOW, SEVERITY_CRITICAL, 0,
                     GM_MIN_M, stab->gm_m);
        stab->stable = false;
    } else {
        clear_alarm(ALARM_STABILITY_LOW, 0);
    }
    
    /* Track minimum GM */
    if (stab->gm_m < g_vessel.min_gm_m || g_vessel.min_gm_m == 0.0f) {
        g_vessel.min_gm_m = stab->gm_m;
    }
    
    /* Check heel (static list) */
    if (fabsf(stab->heel_deg) > HEEL_LIMIT_DEG) {
        trigger_alarm(ALARM_CARGO_SHIFT, SEVERITY_ALARM, 0,
                     HEEL_LIMIT_DEG, stab->heel_deg);
    } else {
        clear_alarm(ALARM_CARGO_SHIFT, 0);
    }
}

/**
 * @brief Check weather limits
 */
static void check_weather_limits(void)
{
    weather_t *wx = &g_vessel.weather;
    bool limit_exceeded = false;
    
    if (wx->wind_speed_kts > WIND_LIMIT_KTS) {
        trigger_alarm(ALARM_WEATHER_LIMIT, SEVERITY_ALARM, 0,
                     WIND_LIMIT_KTS, wx->wind_speed_kts);
        limit_exceeded = true;
    }
    
    if (wx->wave_height_m > WAVE_LIMIT_M) {
        trigger_alarm(ALARM_WEATHER_LIMIT, SEVERITY_ALARM, 1,
                     WAVE_LIMIT_M, wx->wave_height_m);
        limit_exceeded = true;
    }
    
    if (!limit_exceeded) {
        clear_alarm(ALARM_WEATHER_LIMIT, 0);
        clear_alarm(ALARM_WEATHER_LIMIT, 1);
    }
    
    g_vessel.weather_limit_exceeded = limit_exceeded;
}

/* ============================================================================
 * Public API Functions
 * ============================================================================ */

/**
 * @brief Initialize vessel control system
 */
int vessel_init(void)
{
    if (g_vessel.initialized) {
        return -1;
    }
    
    memset(&g_vessel, 0, sizeof(g_vessel));
    
    g_vessel.initialized = true;
    g_vessel.state = VESSEL_STATE_MOORED;
    g_vessel.nav_mode = NAV_MODE_MANUAL;
    g_vessel.stability.stable = true;
    g_vessel.stability.gm_m = 1.5f; /* Default safe GM */
    g_vessel.min_gm_m = g_vessel.stability.gm_m;
    
    log_event(0, 0, 0x0001, 0.0f, 0.0f);
    
    return 0;
}

/**
 * @brief Configure vessel parameters
 */
int vessel_config(const char *name, uint32_t mmsi, uint32_t imo,
                  float loa, float beam, float draft, float dwt)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    strncpy(g_vessel.name, name, sizeof(g_vessel.name) - 1);
    g_vessel.mmsi = mmsi;
    g_vessel.imo_number = imo;
    g_vessel.loa_m = loa;
    g_vessel.beam_m = beam;
    g_vessel.draft_m = draft;
    g_vessel.dwt_t = dwt;
    
    log_event(0, 0, 0x0010, loa, dwt);
    
    return 0;
}

/**
 * @brief Set navigation mode
 */
int vessel_set_nav_mode(nav_mode_t mode)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    nav_mode_t old_mode = g_vessel.nav_mode;
    g_vessel.nav_mode = mode;
    
    log_event(2, 0, 0x2010, (float)old_mode, (float)mode);
    
    return 0;
}

/**
 * @brief Add waypoint to route
 */
int vessel_add_waypoint(double lat, double lon, float speed, float turn_radius)
{
    if (!g_vessel.initialized || g_vessel.waypoint_count >= VESSEL_MAX_WAYPOINTS) {
        return -1;
    }
    
    waypoint_t *wp = &g_vessel.route[g_vessel.waypoint_count];
    wp->waypoint_id = g_vessel.waypoint_count;
    wp->position.latitude = lat;
    wp->position.longitude = lon;
    wp->planned_speed_kts = speed;
    wp->turn_radius_nm = turn_radius;
    wp->reached = false;
    
    /* Calculate route distance */
    if (g_vessel.waypoint_count > 0) {
        g_vessel.route_distance_nm += calc_distance_nm(
            &g_vessel.route[g_vessel.waypoint_count - 1].position, &wp->position);
    }
    
    g_vessel.waypoint_count++;
    log_event(2, wp->waypoint_id, 0x2020, (float)lat, (float)lon);
    
    return 0;
}

/**
 * @brief Update navigation data
 */
int vessel_update_nav(double lat, double lon, float heading, float course, 
                      float speed, float rot, float depth)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    /* Store previous */
    g_vessel.previous = g_vessel.current;
    
    /* Update current */
    g_vessel.current.position.latitude = lat;
    g_vessel.current.position.longitude = lon;
    g_vessel.current.heading_deg = heading;
    g_vessel.current.course_deg = course;
    g_vessel.current.speed_kts = speed;
    g_vessel.current.sog_kts = speed;
    g_vessel.current.rate_of_turn = rot;
    g_vessel.current.depth_m = depth;
    g_vessel.current.timestamp = g_vessel.current_time_ms;
    
    /* Accumulate distance */
    if (g_vessel.previous.timestamp > 0) {
        float dist = calc_distance_nm(&g_vessel.previous.position, 
                                      &g_vessel.current.position);
        g_vessel.total_distance_nm += dist;
    }
    
    return 0;
}

/**
 * @brief Update ARPA target
 */
int vessel_update_target(uint16_t id, target_type_t type, double lat, double lon,
                         float course, float speed)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    /* Find existing or create new */
    arpa_target_t *target = NULL;
    for (uint8_t i = 0; i < g_vessel.target_count; i++) {
        if (g_vessel.targets[i].target_id == id) {
            target = &g_vessel.targets[i];
            break;
        }
    }
    
    if (target == NULL) {
        if (g_vessel.target_count >= VESSEL_MAX_TARGETS) {
            return -2;
        }
        target = &g_vessel.targets[g_vessel.target_count];
        target->target_id = id;
        target->track_start = g_vessel.current_time_ms;
        g_vessel.target_count++;
    }
    
    target->type = type;
    target->position.latitude = lat;
    target->position.longitude = lon;
    target->course_deg = course;
    target->speed_kts = speed;
    target->last_update = g_vessel.current_time_ms;
    target->lost = false;
    
    /* Recalculate CPA/TCPA */
    calculate_cpa_tcpa(target);
    
    return 0;
}

/**
 * @brief Update stability sensors
 */
int vessel_update_stability(const float roll[3], const float pitch[3],
                            float gm, float displacement)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    tmr_sensor_update(&g_vessel.roll_sensor, roll[0], roll[1], roll[2],
                      g_vessel.current_time_ms);
    tmr_sensor_update(&g_vessel.pitch_sensor, pitch[0], pitch[1], pitch[2],
                      g_vessel.current_time_ms);
    
    g_vessel.stability.gm_m = gm;
    g_vessel.stability.gmt_m = gm;
    g_vessel.stability.displacement_t = displacement;
    g_vessel.stability.heel_deg = g_vessel.roll_sensor.voted_value;
    
    check_stability();
    
    return 0;
}

/**
 * @brief Update weather data
 */
int vessel_update_weather(float wind_speed, float wind_dir, float wave_height,
                          float wave_period, sea_state_t sea_state)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    g_vessel.weather.wind_speed_kts = wind_speed;
    g_vessel.weather.wind_dir_deg = wind_dir;
    g_vessel.weather.wave_height_m = wave_height;
    g_vessel.weather.wave_period_s = wave_period;
    g_vessel.weather.sea_state = sea_state;
    g_vessel.weather.timestamp = g_vessel.current_time_ms;
    
    check_weather_limits();
    
    return 0;
}

/**
 * @brief Configure cargo bay
 */
int vessel_config_bay(uint8_t bay_id, float max_weight, float lcg, float tcg, float vcg)
{
    if (!g_vessel.initialized || bay_id >= VESSEL_MAX_CARGO_BAYS) {
        return -1;
    }
    
    cargo_bay_t *bay = &g_vessel.bays[bay_id];
    bay->bay_id = bay_id;
    bay->max_weight_kg = max_weight;
    bay->lcg_m = lcg;
    bay->tcg_m = tcg;
    bay->vcg_m = vcg;
    
    if (bay_id >= g_vessel.bay_count) {
        g_vessel.bay_count = bay_id + 1;
    }
    
    return 0;
}

/**
 * @brief Update cargo bay weight
 */
int vessel_update_bay(uint8_t bay_id, float weight, uint16_t containers)
{
    if (!g_vessel.initialized || bay_id >= g_vessel.bay_count) {
        return -1;
    }
    
    cargo_bay_t *bay = &g_vessel.bays[bay_id];
    bay->total_weight_kg = weight;
    bay->container_count = containers;
    bay->overloaded = (weight > bay->max_weight_kg);
    
    if (bay->overloaded) {
        trigger_alarm(ALARM_OVERWEIGHT, SEVERITY_ALARM, bay_id,
                     bay->max_weight_kg, weight);
    } else {
        clear_alarm(ALARM_OVERWEIGHT, bay_id);
    }
    
    calculate_cargo_cg();
    
    return 0;
}

/**
 * @brief Emergency stop
 */
int vessel_emergency_stop(void)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    g_vessel.emergency_stop = true;
    g_vessel.nav_mode = NAV_MODE_EMERGENCY;
    g_vessel.state = VESSEL_STATE_EMERGENCY;
    g_vessel.current.engine_order = ENGINE_STOP;
    g_vessel.speed_setpoint_kts = 0.0f;
    g_vessel.emergency_count++;
    
    trigger_alarm(ALARM_COLLISION_IMMINENT, SEVERITY_CRITICAL, 0, 0.0f, 0.0f);
    log_event(3, 0, 0x3000, g_vessel.current.speed_kts, 0.0f);
    
    return 0;
}

/**
 * @brief Generate NMEA-style telemetry
 */
int vessel_get_telemetry(uint8_t *buffer, size_t max_len)
{
    if (!g_vessel.initialized || buffer == NULL || 
        max_len < sizeof(telem_header_t) + 64) {
        return -1;
    }
    
    telem_header_t *hdr = (telem_header_t *)buffer;
    hdr->sync_word = 0x1ACF;
    hdr->pgn = 0xF801;  /* Vessel telemetry PGN */
    hdr->priority = 3;
    hdr->source_addr = 0x01;
    hdr->dest_addr = 0xFF;
    hdr->sequence = g_vessel.telemetry_seq++;
    hdr->timestamp = g_vessel.current_time_ms;
    
    uint8_t *payload = buffer + sizeof(telem_header_t);
    size_t offset = 0;
    
    /* Navigation state */
    payload[offset++] = (uint8_t)g_vessel.nav_mode;
    payload[offset++] = (uint8_t)g_vessel.state;
    
    /* Position */
    memcpy(&payload[offset], &g_vessel.current.position.latitude, sizeof(double));
    offset += sizeof(double);
    memcpy(&payload[offset], &g_vessel.current.position.longitude, sizeof(double));
    offset += sizeof(double);
    
    /* Speed and heading */
    memcpy(&payload[offset], &g_vessel.current.speed_kts, sizeof(float));
    offset += sizeof(float);
    memcpy(&payload[offset], &g_vessel.current.heading_deg, sizeof(float));
    offset += sizeof(float);
    memcpy(&payload[offset], &g_vessel.current.course_deg, sizeof(float));
    offset += sizeof(float);
    
    /* Stability */
    memcpy(&payload[offset], &g_vessel.roll_sensor.voted_value, sizeof(float));
    offset += sizeof(float);
    memcpy(&payload[offset], &g_vessel.stability.gm_m, sizeof(float));
    offset += sizeof(float);
    
    /* Targets */
    payload[offset++] = g_vessel.target_count;
    payload[offset++] = g_vessel.dangerous_targets;
    
    /* Alarms */
    payload[offset++] = g_vessel.alarm_count;
    payload[offset++] = g_vessel.unacked_alarms;
    
    /* Cargo */
    memcpy(&payload[offset], &g_vessel.total_cargo_weight_kg, sizeof(float));
    offset += sizeof(float);
    
    hdr->data_length = (uint16_t)offset;
    
    return (int)(sizeof(telem_header_t) + offset);
}

/**
 * @brief Main processing loop
 */
int vessel_process(uint32_t time_ms)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    uint32_t delta_ms = time_ms - g_vessel.last_process_ms;
    g_vessel.current_time_ms = time_ms;
    g_vessel.last_process_ms = time_ms;
    g_vessel.uptime_seconds = time_ms / 1000;
    
    /* Age out lost targets */
    for (uint8_t i = 0; i < g_vessel.target_count; i++) {
        if (time_ms - g_vessel.targets[i].last_update > 60000) {
            g_vessel.targets[i].lost = true;
        }
    }
    
    /* Process ARPA targets */
    process_arpa_targets();
    
    /* Navigation control */
    switch (g_vessel.nav_mode) {
        case NAV_MODE_HEADING_HOLD:
            if (!g_vessel.collision_avoidance_active) {
                heading_control(g_vessel.heading_setpoint_deg);
            }
            break;
            
        case NAV_MODE_WAYPOINT:
        case NAV_MODE_TRACK_CONTROL:
            waypoint_navigation();
            break;
            
        case NAV_MODE_COLLISION_AVOIDANCE:
            collision_avoidance_maneuver();
            break;
            
        case NAV_MODE_EMERGENCY:
            /* Maintain zero speed */
            g_vessel.speed_setpoint_kts = 0.0f;
            break;
            
        default:
            break;
    }
    
    /* Collision avoidance override */
    if (g_vessel.collision_avoidance_active && 
        g_vessel.nav_mode != NAV_MODE_EMERGENCY) {
        collision_avoidance_maneuver();
    }
    
    (void)delta_ms;
    
    return 0;
}

/* ============================================================================
 * Query Functions
 * ============================================================================ */

nav_mode_t vessel_get_nav_mode(void)
{
    return g_vessel.initialized ? g_vessel.nav_mode : NAV_MODE_MANUAL;
}

vessel_state_t vessel_get_state(void)
{
    return g_vessel.initialized ? g_vessel.state : VESSEL_STATE_MOORED;
}

float vessel_get_speed(void)
{
    return g_vessel.initialized ? g_vessel.current.speed_kts : 0.0f;
}

float vessel_get_heading(void)
{
    return g_vessel.initialized ? g_vessel.current.heading_deg : 0.0f;
}

float vessel_get_roll(void)
{
    return g_vessel.initialized ? g_vessel.roll_sensor.voted_value : 0.0f;
}

float vessel_get_gm(void)
{
    return g_vessel.initialized ? g_vessel.stability.gm_m : 0.0f;
}

uint8_t vessel_get_dangerous_targets(void)
{
    return g_vessel.initialized ? g_vessel.dangerous_targets : 0;
}

bool vessel_is_collision_avoidance_active(void)
{
    return g_vessel.initialized && g_vessel.collision_avoidance_active;
}

bool vessel_is_stable(void)
{
    return g_vessel.initialized && g_vessel.stability.stable;
}

uint8_t vessel_get_alarm_count(void)
{
    return g_vessel.initialized ? g_vessel.alarm_count : 0;
}

uint16_t vessel_get_event_count(void)
{
    return g_vessel.initialized ? g_vessel.event_count : 0;
}

float vessel_get_total_distance(void)
{
    return g_vessel.initialized ? g_vessel.total_distance_nm : 0.0f;
}

/**
 * @brief Acknowledge an alarm
 */
int vessel_ack_alarm(uint8_t alarm_idx)
{
    if (!g_vessel.initialized || alarm_idx >= g_vessel.alarm_count) {
        return -1;
    }
    
    if (!g_vessel.alarms[alarm_idx].acknowledged) {
        g_vessel.alarms[alarm_idx].acknowledged = true;
        g_vessel.unacked_alarms--;
    }
    
    return 0;
}

/**
 * @brief Reset the vessel system
 */
int vessel_reset(void)
{
    if (!g_vessel.initialized) {
        return -1;
    }
    
    log_event(0, 0, 0x0002, 0.0f, 0.0f);
    memset(&g_vessel, 0, sizeof(g_vessel));
    return vessel_init();
}

/**
 * @brief Shutdown the vessel system
 */
void vessel_shutdown(void)
{
    if (g_vessel.initialized) {
        log_event(0, 0, 0x0003, 0.0f, 0.0f);
        g_vessel.initialized = false;
    }
}

/**
 * @brief Check if TMR sensors are healthy
 */
bool vessel_has_sensor_fault(void)
{
    return g_vessel.initialized && 
           (!g_vessel.roll_sensor.healthy || !g_vessel.pitch_sensor.healthy);
}

/**
 * @brief Get TMR status for roll sensor
 */
tmr_result_t vessel_get_roll_tmr_status(void)
{
    return g_vessel.initialized ? g_vessel.roll_sensor.status : TMR_FAULT;
}
