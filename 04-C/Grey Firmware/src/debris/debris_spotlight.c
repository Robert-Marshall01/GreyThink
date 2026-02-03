/**
 * @file debris_spotlight.c
 * @brief Orbital Debris Tracking & Collision Prediction Spotlight Implementation
 * 
 * This module implements a production-grade space debris tracking and collision
 * avoidance system demonstrating:
 * - Radar signal preprocessing for debris detection
 * - Orbital mechanics for trajectory prediction
 * - Collision probability calculation
 * - Conjunction data message generation
 * - TMR sensor voting for reliability
 * 
 * ORBITAL MECHANICS:
 * Objects in orbit follow Keplerian mechanics modified by perturbations
 * (J2 oblateness, atmospheric drag, solar radiation pressure). This
 * implementation uses simplified two-body propagation with J2 corrections.
 * 
 * COLLISION PREDICTION:
 * Conjunction screening identifies close approaches. Collision probability
 * is computed using covariance-based methods (NASA's CARA algorithms).
 * Miss distance and relative velocity determine severity.
 * 
 * DEBRIS ENVIRONMENT:
 * - LEO (200-2000 km): ~23,000 tracked objects, millions of sub-cm debris
 * - MEO (2000-35786 km): Navigation satellites, less congested
 * - GEO (35786 km): Communications satellites, valuable real estate
 * 
 * STANDARDS:
 * - CCSDS 502.0-B (Orbit Data Messages)
 * - CCSDS 508.0-B (Conjunction Data Messages)
 * - NASA-STD-8719.14 (Process for Limiting Orbital Debris)
 * - ISO 24113 (Space debris mitigation)
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

#define DEBRIS_MAX_TRACKS         512    /**< Maximum tracked objects */
#define DEBRIS_MAX_DETECTIONS     128    /**< Detections per scan cycle */
#define DEBRIS_MAX_CONJUNCTIONS   64     /**< Simultaneous conjunction events */
#define DEBRIS_MAX_ASSETS         16     /**< Protected assets (our satellites) */
#define DEBRIS_HISTORY_SIZE       60     /**< Orbital element history */
#define DEBRIS_MAX_EVENTS         256    /**< Event log size */
#define DEBRIS_MAX_ALARMS         32     /**< Active alarm buffer */

/* Physical constants */
#define MU_EARTH_KM3_S2          398600.4418  /**< Earth gravitational parameter */
#define RE_KM                    6378.137     /**< Earth equatorial radius */
#define J2_COEFFICIENT           1.08263e-3   /**< J2 oblateness coefficient */
#define OMEGA_EARTH_RAD_S        7.292115e-5  /**< Earth rotation rate */

/* Screening thresholds */
#define SCREEN_DISTANCE_LEO_KM   5.0f    /**< LEO screening distance */
#define SCREEN_DISTANCE_GEO_KM   15.0f   /**< GEO screening distance */
#define PC_RED_THRESHOLD         1e-4f   /**< High collision probability */
#define PC_YELLOW_THRESHOLD      1e-5f   /**< Medium collision probability */
#define HARD_BODY_RADIUS_M       15.0f   /**< Combined hard body radius */

/* Timing thresholds */
#define TCA_WARNING_HOURS        72      /**< Hours before TCA for warning */
#define TCA_ALERT_HOURS          24      /**< Hours before TCA for alert */
#define TCA_CRITICAL_HOURS       6       /**< Hours before TCA for critical */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Orbital regime */
typedef enum {
    ORBIT_LEO,               /**< Low Earth Orbit (< 2000 km) */
    ORBIT_MEO,               /**< Medium Earth Orbit */
    ORBIT_GEO,               /**< Geostationary */
    ORBIT_HEO,               /**< Highly Elliptical */
    ORBIT_UNKNOWN
} orbital_regime_t;

/** Object type classification */
typedef enum {
    OBJ_PAYLOAD,             /**< Active or inactive spacecraft */
    OBJ_ROCKET_BODY,         /**< Upper stages, boosters */
    OBJ_DEBRIS,              /**< Breakup fragments */
    OBJ_UNKNOWN_DEBRIS,      /**< Unidentified debris */
    OBJ_ANALYST              /**< Analyst-tracked object */
} object_type_t;

/** Track quality */
typedef enum {
    TRACK_TENTATIVE,         /**< Initial detection, unconfirmed */
    TRACK_ASSOCIATING,       /**< Correlating with catalog */
    TRACK_CONFIRMED,         /**< Catalog match found */
    TRACK_NEW,               /**< New uncataloged object */
    TRACK_LOST               /**< Track lost, coasting */
} track_quality_t;

/** Conjunction severity */
typedef enum {
    CONJ_GREEN,              /**< No concern, miss > 5km */
    CONJ_WATCH,              /**< Monitor, miss 1-5km */
    CONJ_YELLOW,             /**< Elevated concern, Pc > 1e-5 */
    CONJ_RED,                /**< High concern, Pc > 1e-4 */
    CONJ_EMERGENCY           /**< Maneuver required */
} conjunction_severity_t;

/** Maneuver recommendation */
typedef enum {
    MAN_NONE,
    MAN_MONITOR,
    MAN_PLAN,
    MAN_COMMIT,
    MAN_EXECUTE,
    MAN_COMPLETE
} maneuver_recommendation_t;

/** Alarm types */
typedef enum {
    ALARM_CONJUNCTION_RED,
    ALARM_CONJUNCTION_YELLOW,
    ALARM_NEW_OBJECT,
    ALARM_TRACK_LOST,
    ALARM_SENSOR_FAULT,
    ALARM_PREDICTION_DIVERGENCE,
    ALARM_MANEUVER_REQUIRED,
    ALARM_TCA_IMMINENT
} alarm_type_t;

/** Alarm severity */
typedef enum {
    SEVERITY_INFO,
    SEVERITY_WARNING,
    SEVERITY_ALARM,
    SEVERITY_CRITICAL
} alarm_severity_t;

/** TMR voting result */
typedef enum {
    TMR_AGREE_ALL,
    TMR_AGREE_2OF3,
    TMR_DISAGREE,
    TMR_FAULT
} tmr_result_t;

/** Sensor health */
typedef enum {
    SENSOR_HEALTH_OK,
    SENSOR_HEALTH_DEGRADED,
    SENSOR_HEALTH_FAULT,
    SENSOR_HEALTH_OFFLINE
} sensor_health_t;

/* ============================================================================
 * Data Structures
 * ============================================================================ */

/** State vector (ECI J2000) */
typedef struct {
    double x, y, z;          /**< Position [km] */
    double vx, vy, vz;       /**< Velocity [km/s] */
    double epoch_jd;         /**< Julian date epoch */
} state_vector_t;

/** Classical orbital elements */
typedef struct {
    double sma;              /**< Semi-major axis [km] */
    double ecc;              /**< Eccentricity */
    double inc;              /**< Inclination [rad] */
    double raan;             /**< Right ascension of ascending node [rad] */
    double aop;              /**< Argument of perigee [rad] */
    double ta;               /**< True anomaly [rad] */
    double epoch_jd;
} orbital_elements_t;

/** Covariance matrix (position only, 3x3) */
typedef struct {
    float cr_r, ct_r, ct_t, cn_r, cn_t, cn_n;  /**< Upper triangular RTN */
} covariance_3x3_t;

/** Full covariance (6x6, position + velocity) */
typedef struct {
    double elements[21];     /**< Upper triangular, row-major */
} covariance_6x6_t;

/** TMR sensor reading */
typedef struct {
    float readings[3];
    float voted_value;
    tmr_result_t vote_status;
    sensor_health_t health;
    uint64_t timestamp_us;
    uint32_t samples;
} tmr_sensor_t;

/** Radar detection */
typedef struct {
    uint32_t detection_id;
    uint64_t timestamp_us;
    tmr_sensor_t range_km;
    tmr_sensor_t range_rate_km_s;
    tmr_sensor_t azimuth_deg;
    tmr_sensor_t elevation_deg;
    float rcs_dbsm;
    float snr_db;
    bool valid;
} radar_detection_t;

/** Tracked object */
typedef struct {
    uint32_t track_id;
    uint32_t norad_id;           /**< NORAD catalog ID (0 if uncataloged) */
    char intl_designator[12];    /**< International designator */
    object_type_t type;
    track_quality_t quality;
    orbital_regime_t regime;
    
    state_vector_t state;        /**< Current state */
    orbital_elements_t elements; /**< Orbital elements */
    covariance_6x6_t covariance;
    
    float rcs_est_dbsm;          /**< Estimated radar cross section */
    float area_to_mass;          /**< Ballistic coefficient proxy */
    
    uint32_t obs_count;          /**< Observation count */
    uint64_t first_obs_time;
    uint64_t last_obs_time;
    float residual_rms;          /**< Fit residual RMS [km] */
    
    bool active;
} tracked_object_t;

/** Protected asset (our spacecraft) */
typedef struct {
    uint32_t asset_id;
    uint32_t norad_id;
    char name[32];
    state_vector_t state;
    covariance_6x6_t covariance;
    float hard_body_radius_m;
    float maneuver_delta_v_m_s;  /**< Available delta-V */
    bool maneuverable;
    bool configured;
} protected_asset_t;

/** Conjunction event */
typedef struct {
    uint32_t conjunction_id;
    uint32_t primary_id;         /**< Protected asset track ID */
    uint32_t secondary_id;       /**< Threat object track ID */
    
    double tca_jd;               /**< Time of closest approach */
    uint64_t tca_unix_ms;        /**< TCA in Unix milliseconds */
    
    float miss_distance_m;       /**< Total miss distance */
    float radial_miss_m;         /**< Radial component (R) */
    float in_track_miss_m;       /**< In-track component (T) */
    float cross_track_miss_m;    /**< Cross-track component (N) */
    float relative_velocity_m_s;
    
    float collision_probability;
    float mahalanobis_distance;
    
    conjunction_severity_t severity;
    maneuver_recommendation_t recommendation;
    
    state_vector_t primary_tca;  /**< Primary state at TCA */
    state_vector_t secondary_tca; /**< Secondary state at TCA */
    
    uint64_t creation_time_ms;
    uint64_t update_time_ms;
    uint16_t update_count;
    
    bool active;
    bool ack_by_operator;
} conjunction_event_t;

/** Active alarm */
typedef struct {
    alarm_type_t type;
    alarm_severity_t severity;
    uint32_t source_id;
    float threshold;
    float actual;
    uint64_t trigger_time_ms;
    bool acknowledged;
} active_alarm_t;

/** Event log entry */
typedef struct {
    uint8_t category;
    uint8_t source_id;
    uint16_t event_code;
    float value1;
    float value2;
    uint64_t timestamp_ms;
} event_entry_t;

/** CCSDS telemetry header */
typedef struct {
    uint16_t sync_word;
    uint16_t apid;
    uint16_t sequence_count;
    uint16_t packet_length;
    uint32_t timestamp_sec;
    uint16_t timestamp_subsec;
} telemetry_header_t;

/** System configuration */
typedef struct {
    float screen_distance_leo_km;
    float screen_distance_geo_km;
    float pc_red_threshold;
    float pc_yellow_threshold;
    uint8_t prediction_days;
    bool auto_maneuver_enabled;
    bool cdm_auto_generation;
} debris_config_t;

/** Main system state */
typedef struct {
    bool initialized;
    uint32_t current_time_ms;
    uint32_t last_process_ms;
    uint32_t uptime_seconds;
    
    /* Configuration */
    debris_config_t config;
    
    /* Tracking */
    tracked_object_t tracks[DEBRIS_MAX_TRACKS];
    uint16_t track_count;
    uint16_t new_objects_today;
    
    /* Protected assets */
    protected_asset_t assets[DEBRIS_MAX_ASSETS];
    uint8_t asset_count;
    
    /* Conjunctions */
    conjunction_event_t conjunctions[DEBRIS_MAX_CONJUNCTIONS];
    uint8_t conjunction_count;
    uint8_t red_conjunctions;
    uint8_t yellow_conjunctions;
    
    /* Detections */
    radar_detection_t detections[DEBRIS_MAX_DETECTIONS];
    uint16_t detection_count;
    uint32_t total_detections;
    
    /* Alarms */
    active_alarm_t alarms[DEBRIS_MAX_ALARMS];
    uint8_t alarm_count;
    uint8_t unacked_alarms;
    
    /* Events */
    event_entry_t events[DEBRIS_MAX_EVENTS];
    uint16_t event_write_idx;
    uint16_t event_count;
    
    /* Telemetry */
    uint16_t telemetry_seq;
    
    /* Statistics */
    uint32_t scans_completed;
    uint32_t conjunctions_detected;
    uint32_t maneuvers_executed;
    float closest_approach_m;
    
    /* Timing */
    uint32_t last_scan_time_ms;
    uint32_t last_prediction_time_ms;
    uint32_t scan_interval_ms;
    uint32_t prediction_interval_ms;
} debris_system_t;

/* ============================================================================
 * Global State
 * ============================================================================ */

static debris_system_t g_debris = {0};

/* ============================================================================
 * TMR Voting Functions
 * ============================================================================ */

/**
 * @brief Perform TMR voting on three sensor values
 */
static tmr_result_t tmr_vote(float values[3], float *result)
{
    float tolerance = fabsf(values[0]) * 0.05f + 0.1f; /* 5% + 0.1 absolute */
    
    float d01 = fabsf(values[0] - values[1]);
    float d02 = fabsf(values[0] - values[2]);
    float d12 = fabsf(values[1] - values[2]);
    
    /* All agree */
    if (d01 < tolerance && d02 < tolerance && d12 < tolerance) {
        *result = (values[0] + values[1] + values[2]) / 3.0f;
        return TMR_AGREE_ALL;
    }
    
    /* 2-of-3 agree */
    if (d01 < tolerance) {
        *result = (values[0] + values[1]) / 2.0f;
        return TMR_AGREE_2OF3;
    }
    if (d02 < tolerance) {
        *result = (values[0] + values[2]) / 2.0f;
        return TMR_AGREE_2OF3;
    }
    if (d12 < tolerance) {
        *result = (values[1] + values[2]) / 2.0f;
        return TMR_AGREE_2OF3;
    }
    
    /* All disagree - use median */
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
                              uint64_t timestamp_us)
{
    sensor->readings[0] = v0;
    sensor->readings[1] = v1;
    sensor->readings[2] = v2;
    sensor->timestamp_us = timestamp_us;
    sensor->samples++;
    
    sensor->vote_status = tmr_vote(sensor->readings, &sensor->voted_value);
    
    switch (sensor->vote_status) {
        case TMR_AGREE_ALL:
            sensor->health = SENSOR_HEALTH_OK;
            break;
        case TMR_AGREE_2OF3:
            sensor->health = SENSOR_HEALTH_DEGRADED;
            break;
        case TMR_DISAGREE:
        case TMR_FAULT:
            sensor->health = SENSOR_HEALTH_FAULT;
            break;
    }
}

/* ============================================================================
 * Orbital Mechanics Functions
 * ============================================================================ */

/**
 * @brief Calculate orbital period from semi-major axis
 */
static double orbital_period(double sma_km)
{
    return 2.0 * M_PI * sqrt((sma_km * sma_km * sma_km) / MU_EARTH_KM3_S2);
}

/**
 * @brief Classify orbital regime
 */
static orbital_regime_t classify_orbit(double sma_km, double ecc)
{
    double apogee = sma_km * (1.0 + ecc);
    double perigee = sma_km * (1.0 - ecc);
    
    if (ecc > 0.25 && apogee > 35000.0) {
        return ORBIT_HEO;
    }
    if (perigee < RE_KM + 2000.0) {
        return ORBIT_LEO;
    }
    if (sma_km > 42160.0 - 200.0 && sma_km < 42160.0 + 200.0 && ecc < 0.01) {
        return ORBIT_GEO;
    }
    return ORBIT_MEO;
}

/**
 * @brief Convert state vector to orbital elements
 */
static void state_to_elements(const state_vector_t *state, orbital_elements_t *elem)
{
    double r_mag = sqrt(state->x * state->x + state->y * state->y + 
                        state->z * state->z);
    double v_mag = sqrt(state->vx * state->vx + state->vy * state->vy + 
                        state->vz * state->vz);
    
    /* Specific angular momentum */
    double hx = state->y * state->vz - state->z * state->vy;
    double hy = state->z * state->vx - state->x * state->vz;
    double hz = state->x * state->vy - state->y * state->vx;
    double h_mag = sqrt(hx * hx + hy * hy + hz * hz);
    
    /* Node vector */
    double nx = -hy;
    double ny = hx;
    double n_mag = sqrt(nx * nx + ny * ny);
    
    /* Eccentricity vector */
    double rdotv = state->x * state->vx + state->y * state->vy + 
                   state->z * state->vz;
    double ex = (1.0 / MU_EARTH_KM3_S2) * 
                ((v_mag * v_mag - MU_EARTH_KM3_S2 / r_mag) * state->x - 
                 rdotv * state->vx);
    double ey = (1.0 / MU_EARTH_KM3_S2) * 
                ((v_mag * v_mag - MU_EARTH_KM3_S2 / r_mag) * state->y - 
                 rdotv * state->vy);
    double ez = (1.0 / MU_EARTH_KM3_S2) * 
                ((v_mag * v_mag - MU_EARTH_KM3_S2 / r_mag) * state->z - 
                 rdotv * state->vz);
    
    /* Orbital elements */
    elem->ecc = sqrt(ex * ex + ey * ey + ez * ez);
    elem->sma = 1.0 / (2.0 / r_mag - v_mag * v_mag / MU_EARTH_KM3_S2);
    elem->inc = acos(hz / h_mag);
    
    if (n_mag > 1e-6) {
        elem->raan = acos(nx / n_mag);
        if (ny < 0) elem->raan = 2.0 * M_PI - elem->raan;
    } else {
        elem->raan = 0.0;
    }
    
    if (n_mag > 1e-6 && elem->ecc > 1e-6) {
        elem->aop = acos((nx * ex + ny * ey) / (n_mag * elem->ecc));
        if (ez < 0) elem->aop = 2.0 * M_PI - elem->aop;
    } else {
        elem->aop = 0.0;
    }
    
    if (elem->ecc > 1e-6) {
        elem->ta = acos((ex * state->x + ey * state->y + ez * state->z) / 
                        (elem->ecc * r_mag));
        if (rdotv < 0) elem->ta = 2.0 * M_PI - elem->ta;
    } else {
        elem->ta = acos(state->x / r_mag);
        if (state->vy < 0) elem->ta = 2.0 * M_PI - elem->ta;
    }
    
    elem->epoch_jd = state->epoch_jd;
}

/**
 * @brief Propagate state vector using two-body + J2 (simplified)
 */
static void propagate_state(const state_vector_t *state0, double dt_sec,
                            state_vector_t *state1)
{
    /* Simplified two-body propagation with linear correction */
    /* Production would use RK4/RK78 integrator with full perturbations */
    
    double r_mag = sqrt(state0->x * state0->x + state0->y * state0->y + 
                        state0->z * state0->z);
    double r3 = r_mag * r_mag * r_mag;
    
    /* J2 perturbation factor */
    double z2_r2 = (state0->z * state0->z) / (r_mag * r_mag);
    double j2_factor = 1.5 * J2_COEFFICIENT * (RE_KM * RE_KM) / (r_mag * r_mag);
    
    /* Acceleration */
    double ax = -MU_EARTH_KM3_S2 * state0->x / r3 * 
                (1.0 + j2_factor * (1.0 - 5.0 * z2_r2));
    double ay = -MU_EARTH_KM3_S2 * state0->y / r3 * 
                (1.0 + j2_factor * (1.0 - 5.0 * z2_r2));
    double az = -MU_EARTH_KM3_S2 * state0->z / r3 * 
                (1.0 + j2_factor * (3.0 - 5.0 * z2_r2));
    
    /* Simple Euler integration (production would use higher-order method) */
    state1->x = state0->x + state0->vx * dt_sec + 0.5 * ax * dt_sec * dt_sec;
    state1->y = state0->y + state0->vy * dt_sec + 0.5 * ay * dt_sec * dt_sec;
    state1->z = state0->z + state0->vz * dt_sec + 0.5 * az * dt_sec * dt_sec;
    state1->vx = state0->vx + ax * dt_sec;
    state1->vy = state0->vy + ay * dt_sec;
    state1->vz = state0->vz + az * dt_sec;
    state1->epoch_jd = state0->epoch_jd + dt_sec / 86400.0;
}

/**
 * @brief Calculate relative state between two objects
 */
static void relative_state(const state_vector_t *primary,
                           const state_vector_t *secondary,
                           float *miss_m, float *rel_vel_m_s)
{
    double dx = (secondary->x - primary->x) * 1000.0; /* Convert to meters */
    double dy = (secondary->y - primary->y) * 1000.0;
    double dz = (secondary->z - primary->z) * 1000.0;
    
    double dvx = (secondary->vx - primary->vx) * 1000.0;
    double dvy = (secondary->vy - primary->vy) * 1000.0;
    double dvz = (secondary->vz - primary->vz) * 1000.0;
    
    *miss_m = (float)sqrt(dx * dx + dy * dy + dz * dz);
    *rel_vel_m_s = (float)sqrt(dvx * dvx + dvy * dvy + dvz * dvz);
}

/* ============================================================================
 * Collision Probability Functions
 * ============================================================================ */

/**
 * @brief Calculate collision probability using Foster method (simplified)
 */
static float calculate_collision_probability(float miss_m, float sigma_r_m,
                                            float sigma_t_m, float hbr_m)
{
    /* Simplified 2D collision probability */
    /* Production would use Alfano's method or Monte Carlo */
    
    if (miss_m <= 0.0f || sigma_r_m <= 0.0f || sigma_t_m <= 0.0f) {
        return 0.0f;
    }
    
    /* Combined covariance (RSS of position uncertainties) */
    float sigma_combined = sqrtf(sigma_r_m * sigma_r_m + sigma_t_m * sigma_t_m);
    
    /* Normalized miss distance squared */
    float u2 = (miss_m * miss_m) / (2.0f * sigma_combined * sigma_combined);
    
    /* Hard body radius squared normalized */
    float r2 = (hbr_m * hbr_m) / (sigma_combined * sigma_combined);
    
    /* Probability approximation using Gaussian disk integral */
    /* P = (r^2 / 2*sigma^2) * exp(-u^2) */
    float pc = (r2 / 2.0f) * expf(-u2);
    
    /* For very close approaches approaching the hard body radius */
    if (miss_m < hbr_m * 2.0f) {
        /* Boost probability for near-misses */
        pc *= (1.0f + (hbr_m * 2.0f - miss_m) / hbr_m);
    }
    
    /* Clamp to valid range */
    if (pc > 1.0f) pc = 1.0f;
    if (pc < 0.0f) pc = 0.0f;
    
    return pc;
}

/**
 * @brief Classify conjunction severity
 */
static conjunction_severity_t classify_conjunction(float miss_m, float pc,
                                                   float time_to_tca_hours)
{
    /* Emergency if imminent and high probability */
    if (time_to_tca_hours < TCA_CRITICAL_HOURS && pc >= PC_RED_THRESHOLD) {
        return CONJ_EMERGENCY;
    }
    
    /* Red for high probability */
    if (pc >= PC_RED_THRESHOLD) {
        return CONJ_RED;
    }
    
    /* Yellow for elevated probability */
    if (pc >= PC_YELLOW_THRESHOLD) {
        return CONJ_YELLOW;
    }
    
    /* Watch for close approach */
    if (miss_m < 1000.0f) {
        return CONJ_WATCH;
    }
    
    return CONJ_GREEN;
}

/* ============================================================================
 * Event and Alarm Functions
 * ============================================================================ */

/**
 * @brief Log an event
 */
static void log_event(uint8_t category, uint8_t source, uint16_t code,
                      float v1, float v2)
{
    event_entry_t *evt = &g_debris.events[g_debris.event_write_idx];
    evt->category = category;
    evt->source_id = source;
    evt->event_code = code;
    evt->value1 = v1;
    evt->value2 = v2;
    evt->timestamp_ms = g_debris.current_time_ms;
    
    g_debris.event_write_idx = (g_debris.event_write_idx + 1) % DEBRIS_MAX_EVENTS;
    if (g_debris.event_count < DEBRIS_MAX_EVENTS) {
        g_debris.event_count++;
    }
}

/**
 * @brief Trigger an alarm
 */
static void trigger_alarm(alarm_type_t type, alarm_severity_t severity,
                          uint32_t source, float threshold, float actual)
{
    /* Check for existing alarm of same type/source */
    for (uint8_t i = 0; i < g_debris.alarm_count; i++) {
        if (g_debris.alarms[i].type == type && 
            g_debris.alarms[i].source_id == source) {
            g_debris.alarms[i].actual = actual;
            g_debris.alarms[i].trigger_time_ms = g_debris.current_time_ms;
            return;
        }
    }
    
    /* Add new alarm */
    if (g_debris.alarm_count < DEBRIS_MAX_ALARMS) {
        active_alarm_t *alarm = &g_debris.alarms[g_debris.alarm_count++];
        alarm->type = type;
        alarm->severity = severity;
        alarm->source_id = source;
        alarm->threshold = threshold;
        alarm->actual = actual;
        alarm->trigger_time_ms = g_debris.current_time_ms;
        alarm->acknowledged = false;
        g_debris.unacked_alarms++;
        
        log_event(1, (uint8_t)type, 0x0100, threshold, actual);
    }
}

/**
 * @brief Clear an alarm
 */
static void clear_alarm(alarm_type_t type, uint32_t source)
{
    for (uint8_t i = 0; i < g_debris.alarm_count; i++) {
        if (g_debris.alarms[i].type == type && 
            g_debris.alarms[i].source_id == source) {
            if (!g_debris.alarms[i].acknowledged) {
                g_debris.unacked_alarms--;
            }
            /* Shift remaining alarms */
            for (uint8_t j = i; j < g_debris.alarm_count - 1; j++) {
                g_debris.alarms[j] = g_debris.alarms[j + 1];
            }
            g_debris.alarm_count--;
            
            log_event(1, (uint8_t)type, 0x0101, 0.0f, 0.0f);
            return;
        }
    }
}

/* ============================================================================
 * Detection Processing
 * ============================================================================ */

/**
 * @brief Process radar detection into track
 */
static tracked_object_t* process_detection(const radar_detection_t *det)
{
    /* Calculate position from range/azimuth/elevation */
    float range = det->range_km.voted_value;
    float az_rad = det->azimuth_deg.voted_value * (float)M_PI / 180.0f;
    float el_rad = det->elevation_deg.voted_value * (float)M_PI / 180.0f;
    
    /* Convert to ENU then to ECEF (simplified - assumes radar at origin) */
    float x = range * cosf(el_rad) * sinf(az_rad);
    float y = range * cosf(el_rad) * cosf(az_rad);
    float z = range * sinf(el_rad);
    
    /* Try to correlate with existing tracks */
    for (uint16_t i = 0; i < g_debris.track_count; i++) {
        if (!g_debris.tracks[i].active) continue;
        
        /* Simple distance threshold for correlation */
        float dx = (float)g_debris.tracks[i].state.x - x;
        float dy = (float)g_debris.tracks[i].state.y - y;
        float dz = (float)g_debris.tracks[i].state.z - z;
        float dist = sqrtf(dx * dx + dy * dy + dz * dz);
        
        if (dist < 50.0f) { /* 50 km correlation gate */
            /* Update track with new observation */
            g_debris.tracks[i].state.x = x;
            g_debris.tracks[i].state.y = y;
            g_debris.tracks[i].state.z = z;
            g_debris.tracks[i].obs_count++;
            g_debris.tracks[i].last_obs_time = det->timestamp_us;
            g_debris.tracks[i].quality = TRACK_CONFIRMED;
            
            return &g_debris.tracks[i];
        }
    }
    
    /* Create new track */
    if (g_debris.track_count < DEBRIS_MAX_TRACKS) {
        tracked_object_t *track = &g_debris.tracks[g_debris.track_count++];
        memset(track, 0, sizeof(tracked_object_t));
        
        track->track_id = g_debris.track_count;
        track->active = true;
        track->quality = TRACK_TENTATIVE;
        track->type = OBJ_UNKNOWN_DEBRIS;
        
        track->state.x = x;
        track->state.y = y;
        track->state.z = z;
        track->state.vx = -det->range_rate_km_s.voted_value * sinf(az_rad);
        track->state.vy = -det->range_rate_km_s.voted_value * cosf(az_rad);
        track->state.vz = 0;
        track->state.epoch_jd = 2451545.0 + (double)det->timestamp_us / 86400e6;
        
        track->rcs_est_dbsm = det->rcs_dbsm;
        track->obs_count = 1;
        track->first_obs_time = det->timestamp_us;
        track->last_obs_time = det->timestamp_us;
        
        /* Calculate orbital elements */
        state_to_elements(&track->state, &track->elements);
        track->regime = classify_orbit(track->elements.sma, track->elements.ecc);
        
        g_debris.new_objects_today++;
        log_event(2, track->track_id, 0x0200, track->rcs_est_dbsm, range);
        
        if (track->quality == TRACK_TENTATIVE) {
            trigger_alarm(ALARM_NEW_OBJECT, SEVERITY_INFO, track->track_id,
                          0.0f, track->rcs_est_dbsm);
        }
        
        return track;
    }
    
    return NULL;
}

/* ============================================================================
 * Conjunction Screening
 * ============================================================================ */

/**
 * @brief Screen all tracks against protected assets
 */
static void screen_conjunctions(void)
{
    for (uint8_t a = 0; a < g_debris.asset_count; a++) {
        if (!g_debris.assets[a].configured) continue;
        
        protected_asset_t *asset = &g_debris.assets[a];
        
        for (uint16_t t = 0; t < g_debris.track_count; t++) {
            if (!g_debris.tracks[t].active) continue;
            if (g_debris.tracks[t].track_id == asset->norad_id) continue;
            
            tracked_object_t *track = &g_debris.tracks[t];
            
            /* Propagate both objects to find TCA */
            /* Simplified: use current states to estimate miss distance */
            float miss_m, rel_vel;
            relative_state(&asset->state, &track->state, &miss_m, &rel_vel);
            
            /* Screening threshold based on regime */
            float screen_km = (track->regime == ORBIT_GEO) ? 
                              g_debris.config.screen_distance_geo_km :
                              g_debris.config.screen_distance_leo_km;
            
            if (miss_m / 1000.0f < screen_km) {
                /* Calculate collision probability */
                float sigma_r = 100.0f; /* m, assumed position uncertainty */
                float sigma_t = 200.0f;
                float hbr = asset->hard_body_radius_m + 5.0f;
                
                float pc = calculate_collision_probability(miss_m, sigma_r, 
                                                           sigma_t, hbr);
                
                /* Time to TCA (simplified - assume conjunction is now) */
                float time_to_tca_hours = 24.0f; /* Placeholder */
                
                conjunction_severity_t severity = classify_conjunction(
                    miss_m, pc, time_to_tca_hours);
                
                /* Check if conjunction already exists */
                bool found = false;
                for (uint8_t c = 0; c < g_debris.conjunction_count; c++) {
                    if (g_debris.conjunctions[c].primary_id == asset->asset_id &&
                        g_debris.conjunctions[c].secondary_id == track->track_id) {
                        /* Update existing conjunction */
                        g_debris.conjunctions[c].miss_distance_m = miss_m;
                        g_debris.conjunctions[c].relative_velocity_m_s = rel_vel;
                        g_debris.conjunctions[c].collision_probability = pc;
                        g_debris.conjunctions[c].severity = severity;
                        g_debris.conjunctions[c].update_time_ms = g_debris.current_time_ms;
                        g_debris.conjunctions[c].update_count++;
                        found = true;
                        break;
                    }
                }
                
                if (!found && g_debris.conjunction_count < DEBRIS_MAX_CONJUNCTIONS) {
                    /* Create new conjunction */
                    conjunction_event_t *conj = 
                        &g_debris.conjunctions[g_debris.conjunction_count++];
                    memset(conj, 0, sizeof(conjunction_event_t));
                    
                    conj->conjunction_id = g_debris.conjunctions_detected + 1;
                    conj->primary_id = asset->asset_id;
                    conj->secondary_id = track->track_id;
                    conj->miss_distance_m = miss_m;
                    conj->relative_velocity_m_s = rel_vel;
                    conj->collision_probability = pc;
                    conj->severity = severity;
                    conj->active = true;
                    conj->creation_time_ms = g_debris.current_time_ms;
                    conj->update_time_ms = g_debris.current_time_ms;
                    
                    conj->primary_tca = asset->state;
                    conj->secondary_tca = track->state;
                    
                    g_debris.conjunctions_detected++;
                    
                    log_event(3, asset->asset_id, 0x0300, miss_m, pc);
                    
                    /* Trigger appropriate alarm */
                    if (severity >= CONJ_RED) {
                        trigger_alarm(ALARM_CONJUNCTION_RED, SEVERITY_CRITICAL,
                                      conj->conjunction_id, 
                                      g_debris.config.pc_red_threshold, pc);
                        g_debris.red_conjunctions++;
                    } else if (severity >= CONJ_YELLOW) {
                        trigger_alarm(ALARM_CONJUNCTION_YELLOW, SEVERITY_WARNING,
                                      conj->conjunction_id,
                                      g_debris.config.pc_yellow_threshold, pc);
                        g_debris.yellow_conjunctions++;
                    }
                    
                    /* Track closest approach */
                    if (miss_m < g_debris.closest_approach_m || 
                        g_debris.closest_approach_m == 0) {
                        g_debris.closest_approach_m = miss_m;
                    }
                }
            }
        }
    }
}

/* ============================================================================
 * Public API Functions
 * ============================================================================ */

/**
 * @brief Initialize debris tracking system
 */
int debris_init(void)
{
    if (g_debris.initialized) {
        return -1;
    }
    
    memset(&g_debris, 0, sizeof(g_debris));
    
    /* Default configuration */
    g_debris.config.screen_distance_leo_km = SCREEN_DISTANCE_LEO_KM;
    g_debris.config.screen_distance_geo_km = SCREEN_DISTANCE_GEO_KM;
    g_debris.config.pc_red_threshold = PC_RED_THRESHOLD;
    g_debris.config.pc_yellow_threshold = PC_YELLOW_THRESHOLD;
    g_debris.config.prediction_days = 7;
    g_debris.config.auto_maneuver_enabled = false;
    g_debris.config.cdm_auto_generation = true;
    
    g_debris.scan_interval_ms = 60000;      /* 1 minute scans */
    g_debris.prediction_interval_ms = 300000; /* 5 minute predictions */
    
    g_debris.initialized = true;
    
    log_event(0, 0, 0x0001, 0.0f, 0.0f);
    
    return 0;
}

/**
 * @brief Configure a protected asset
 */
int debris_config_asset(uint8_t asset_id, uint32_t norad_id, const char *name,
                        const state_vector_t *state)
{
    if (!g_debris.initialized || asset_id >= DEBRIS_MAX_ASSETS) {
        return -1;
    }
    
    protected_asset_t *asset = &g_debris.assets[asset_id];
    
    if (!asset->configured) {
        g_debris.asset_count++;
    }
    
    asset->asset_id = asset_id;
    asset->norad_id = norad_id;
    strncpy(asset->name, name, sizeof(asset->name) - 1);
    asset->state = *state;
    asset->hard_body_radius_m = HARD_BODY_RADIUS_M;
    asset->maneuverable = true;
    asset->maneuver_delta_v_m_s = 50.0f;
    asset->configured = true;
    
    log_event(0, asset_id, 0x0010, (float)norad_id, 0.0f);
    
    return 0;
}

/**
 * @brief Update sensor readings from radar
 */
int debris_update_sensors(uint8_t detection_id, 
                          float range[3], float range_rate[3],
                          float azimuth[3], float elevation[3],
                          float rcs_dbsm, float snr_db,
                          uint64_t timestamp_us)
{
    if (!g_debris.initialized) {
        return -1;
    }
    
    if (g_debris.detection_count >= DEBRIS_MAX_DETECTIONS) {
        /* Buffer full, process and clear */
        g_debris.detection_count = 0;
    }
    
    radar_detection_t *det = &g_debris.detections[g_debris.detection_count++];
    det->detection_id = detection_id;
    det->timestamp_us = timestamp_us;
    det->rcs_dbsm = rcs_dbsm;
    det->snr_db = snr_db;
    det->valid = true;
    
    tmr_sensor_update(&det->range_km, range[0], range[1], range[2], timestamp_us);
    tmr_sensor_update(&det->range_rate_km_s, range_rate[0], range_rate[1], 
                      range_rate[2], timestamp_us);
    tmr_sensor_update(&det->azimuth_deg, azimuth[0], azimuth[1], azimuth[2], 
                      timestamp_us);
    tmr_sensor_update(&det->elevation_deg, elevation[0], elevation[1], 
                      elevation[2], timestamp_us);
    
    /* Check sensor health */
    if (det->range_km.health == SENSOR_HEALTH_FAULT) {
        trigger_alarm(ALARM_SENSOR_FAULT, SEVERITY_WARNING, detection_id,
                      0.0f, det->range_km.voted_value);
        det->valid = false;
    }
    
    g_debris.total_detections++;
    
    /* Process detection into track */
    if (det->valid) {
        process_detection(det);
    }
    
    return 0;
}

/**
 * @brief Get active conjunctions
 */
int debris_get_conjunctions(conjunction_event_t *events, uint8_t max_count)
{
    if (!g_debris.initialized || !events) {
        return -1;
    }
    
    uint8_t count = 0;
    for (uint8_t i = 0; i < g_debris.conjunction_count && count < max_count; i++) {
        if (g_debris.conjunctions[i].active) {
            events[count++] = g_debris.conjunctions[i];
        }
    }
    
    return count;
}

/**
 * @brief Get conjunction by ID
 */
int debris_get_conjunction_by_id(uint32_t conj_id, conjunction_event_t *event)
{
    if (!g_debris.initialized || !event) {
        return -1;
    }
    
    for (uint8_t i = 0; i < g_debris.conjunction_count; i++) {
        if (g_debris.conjunctions[i].conjunction_id == conj_id) {
            *event = g_debris.conjunctions[i];
            return 0;
        }
    }
    
    return -2;
}

/**
 * @brief Acknowledge conjunction
 */
int debris_acknowledge_conjunction(uint32_t conj_id)
{
    if (!g_debris.initialized) {
        return -1;
    }
    
    for (uint8_t i = 0; i < g_debris.conjunction_count; i++) {
        if (g_debris.conjunctions[i].conjunction_id == conj_id) {
            g_debris.conjunctions[i].ack_by_operator = true;
            log_event(3, i, 0x0301, (float)conj_id, 0.0f);
            return 0;
        }
    }
    
    return -2;
}

/**
 * @brief Generate CCSDS telemetry packet
 */
int debris_get_telemetry(uint8_t *buffer, size_t max_len)
{
    if (!g_debris.initialized || !buffer) {
        return -1;
    }
    
    if (max_len < sizeof(telemetry_header_t) + 64) {
        return -1;
    }
    
    telemetry_header_t *hdr = (telemetry_header_t *)buffer;
    hdr->sync_word = 0x1ACF;
    hdr->apid = 0x0200;  /* Debris subsystem APID */
    hdr->sequence_count = g_debris.telemetry_seq++;
    hdr->timestamp_sec = g_debris.current_time_ms / 1000;
    hdr->timestamp_subsec = (g_debris.current_time_ms % 1000) * 65;
    
    uint8_t *payload = buffer + sizeof(telemetry_header_t);
    size_t offset = 0;
    
    /* System status */
    payload[offset++] = g_debris.initialized ? 1 : 0;
    payload[offset++] = (uint8_t)(g_debris.uptime_seconds >> 24);
    payload[offset++] = (uint8_t)(g_debris.uptime_seconds >> 16);
    payload[offset++] = (uint8_t)(g_debris.uptime_seconds >> 8);
    payload[offset++] = (uint8_t)g_debris.uptime_seconds;
    
    /* Track counts */
    payload[offset++] = (uint8_t)(g_debris.track_count >> 8);
    payload[offset++] = (uint8_t)g_debris.track_count;
    payload[offset++] = g_debris.asset_count;
    
    /* Conjunction summary */
    payload[offset++] = g_debris.conjunction_count;
    payload[offset++] = g_debris.red_conjunctions;
    payload[offset++] = g_debris.yellow_conjunctions;
    
    /* Closest approach */
    memcpy(&payload[offset], &g_debris.closest_approach_m, sizeof(float));
    offset += sizeof(float);
    
    /* Detection statistics */
    memcpy(&payload[offset], &g_debris.total_detections, sizeof(uint32_t));
    offset += sizeof(uint32_t);
    memcpy(&payload[offset], &g_debris.scans_completed, sizeof(uint32_t));
    offset += sizeof(uint32_t);
    
    /* Alarm summary */
    payload[offset++] = g_debris.alarm_count;
    payload[offset++] = g_debris.unacked_alarms;
    
    alarm_severity_t max_severity = SEVERITY_INFO;
    for (uint8_t i = 0; i < g_debris.alarm_count; i++) {
        if (g_debris.alarms[i].severity > max_severity) {
            max_severity = g_debris.alarms[i].severity;
        }
    }
    payload[offset++] = (uint8_t)max_severity;
    
    hdr->packet_length = (uint16_t)offset;
    
    return (int)(sizeof(telemetry_header_t) + offset);
}

/**
 * @brief Main processing loop
 */
int debris_process(uint32_t time_ms)
{
    if (!g_debris.initialized) {
        return -1;
    }
    
    uint32_t delta_ms = time_ms - g_debris.last_process_ms;
    g_debris.current_time_ms = time_ms;
    g_debris.last_process_ms = time_ms;
    g_debris.uptime_seconds = time_ms / 1000;
    
    /* Periodic scan processing */
    if (time_ms - g_debris.last_scan_time_ms >= g_debris.scan_interval_ms) {
        g_debris.scans_completed++;
        g_debris.last_scan_time_ms = time_ms;
        
        /* Clear old detections */
        g_debris.detection_count = 0;
    }
    
    /* Periodic conjunction screening */
    if (time_ms - g_debris.last_prediction_time_ms >= g_debris.prediction_interval_ms) {
        screen_conjunctions();
        g_debris.last_prediction_time_ms = time_ms;
    }
    
    /* Check for stale tracks */
    uint64_t stale_threshold = (uint64_t)time_ms * 1000 - 3600000000ULL; /* 1 hour */
    for (uint16_t i = 0; i < g_debris.track_count; i++) {
        if (g_debris.tracks[i].active && 
            g_debris.tracks[i].last_obs_time < stale_threshold) {
            g_debris.tracks[i].quality = TRACK_LOST;
            trigger_alarm(ALARM_TRACK_LOST, SEVERITY_INFO, 
                          g_debris.tracks[i].track_id, 0.0f, 0.0f);
        }
    }
    
    /* Update conjunction status */
    g_debris.red_conjunctions = 0;
    g_debris.yellow_conjunctions = 0;
    for (uint8_t i = 0; i < g_debris.conjunction_count; i++) {
        if (g_debris.conjunctions[i].active) {
            if (g_debris.conjunctions[i].severity >= CONJ_RED) {
                g_debris.red_conjunctions++;
            } else if (g_debris.conjunctions[i].severity >= CONJ_YELLOW) {
                g_debris.yellow_conjunctions++;
            }
        }
    }
    
    /* Daily reset */
    if ((time_ms / 86400000) != ((time_ms - delta_ms) / 86400000)) {
        g_debris.new_objects_today = 0;
    }
    
    return 0;
}

/* ============================================================================
 * Query Functions
 * ============================================================================ */

uint16_t debris_get_track_count(void)
{
    return g_debris.initialized ? g_debris.track_count : 0;
}

uint8_t debris_get_asset_count(void)
{
    return g_debris.initialized ? g_debris.asset_count : 0;
}

uint8_t debris_get_conjunction_count(void)
{
    return g_debris.initialized ? g_debris.conjunction_count : 0;
}

uint8_t debris_get_red_count(void)
{
    return g_debris.initialized ? g_debris.red_conjunctions : 0;
}

uint8_t debris_get_yellow_count(void)
{
    return g_debris.initialized ? g_debris.yellow_conjunctions : 0;
}

float debris_get_closest_approach(void)
{
    return g_debris.initialized ? g_debris.closest_approach_m : 0.0f;
}

uint32_t debris_get_total_detections(void)
{
    return g_debris.initialized ? g_debris.total_detections : 0;
}

uint8_t debris_get_alarm_count(void)
{
    return g_debris.initialized ? g_debris.alarm_count : 0;
}

bool debris_has_red_conjunction(void)
{
    return g_debris.initialized && g_debris.red_conjunctions > 0;
}

bool debris_has_sensor_fault(void)
{
    for (uint8_t i = 0; i < g_debris.alarm_count; i++) {
        if (g_debris.alarms[i].type == ALARM_SENSOR_FAULT) {
            return true;
        }
    }
    return false;
}

/**
 * @brief Reset system
 */
int debris_reset(void)
{
    g_debris.initialized = false;
    memset(&g_debris, 0, sizeof(g_debris));
    return debris_init();
}

/**
 * @brief Shutdown system
 */
void debris_shutdown(void)
{
    log_event(0, 0, 0x00FF, 0.0f, 0.0f);
    g_debris.initialized = false;
}

/**
 * @brief Get track by ID
 */
int debris_get_track(uint32_t track_id, tracked_object_t *track)
{
    if (!g_debris.initialized || !track) {
        return -1;
    }
    
    for (uint16_t i = 0; i < g_debris.track_count; i++) {
        if (g_debris.tracks[i].track_id == track_id) {
            *track = g_debris.tracks[i];
            return 0;
        }
    }
    
    return -2;
}

/**
 * @brief Get event count
 */
uint16_t debris_get_event_count(void)
{
    return g_debris.initialized ? g_debris.event_count : 0;
}
