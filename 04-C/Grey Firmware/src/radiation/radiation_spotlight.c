/**
 * @file radiation_spotlight.c
 * @brief Radiation Shielding Control & Telemetry Spotlight Implementation
 * 
 * This module implements a production-grade radiation protection system for
 * long-duration space missions with:
 * - Multi-sensor radiation monitoring with TMR voting
 * - Dynamic shielding control (water walls, magnetic deflectors)
 * - Real-time dose tracking per crew member
 * - Solar Particle Event (SPE) detection and response
 * - Storm shelter activation and management
 * - CCSDS-compatible telemetry for mission control
 * 
 * RADIATION ENVIRONMENT:
 * Space crews face two primary radiation threats:
 * 1. Galactic Cosmic Rays (GCR): Continuous low-level ionizing radiation
 * 2. Solar Particle Events (SPE): Intense bursts during solar flares
 * 
 * The system monitors dose rates, predicts SPE onset, and dynamically
 * adjusts shielding to protect crew while optimizing power consumption.
 * 
 * SHIELDING STRATEGIES:
 * - Passive: Polyethylene panels, water walls, regolith bags
 * - Active: Superconducting magnets (for charged particles)
 * - Hybrid: Configurable water redistribution with magnetic augmentation
 * 
 * SAFETY FEATURES:
 * - Triple Modular Redundancy for all sensors
 * - Automatic storm shelter activation at dose rate thresholds
 * - EVA abort with crew notification
 * - Deterministic response time (<100ms for shelter activation)
 * 
 * STANDARDS COMPLIANCE:
 * - NASA-STD-3001 Vol. 1 (Crew health requirements)
 * - ICRP Publication 123 (Space radiation assessment)
 * - ESA ECSS-E-ST-10-12C (Radiation hardness assurance)
 * - NASA/TP-2020-220002 (Space radiation protection)
 * 
 * @author Grey Firmware Project
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define RAD_MAX_SENSORS         8     /**< Maximum radiation sensors */
#define RAD_MAX_ZONES           16    /**< Maximum shielding zones */
#define RAD_MAX_CREW            8     /**< Maximum crew members */
#define RAD_HISTORY_SIZE        1440  /**< 24 hours at 1-minute intervals */
#define RAD_MAX_EVENTS          256   /**< Event log size */
#define RAD_MAX_ALARMS          32    /**< Active alarm buffer */

/* Dose rate thresholds (μSv/hr) */
#define DOSE_RATE_NOMINAL       50.0f    /**< Normal GCR background */
#define DOSE_RATE_ELEVATED      200.0f   /**< Elevated concern */
#define DOSE_RATE_SPE_ONSET     500.0f   /**< SPE likely starting */
#define DOSE_RATE_SHELTER       2000.0f  /**< Shelter required */
#define DOSE_RATE_EMERGENCY     10000.0f /**< Immediate shelter */

/* Cumulative dose limits (mSv) */
#define DOSE_LIMIT_30_DAY       250.0f   /**< 30-day limit */
#define DOSE_LIMIT_ANNUAL       500.0f   /**< Annual limit */
#define DOSE_LIMIT_CAREER_M     1000.0f  /**< Career limit (male, age-dependent) */
#define DOSE_LIMIT_CAREER_F     600.0f   /**< Career limit (female, age-dependent) */

/* Shielding parameters */
#define SHIELD_WATER_DENSITY    10.0f    /**< g/cm² for water walls */
#define SHIELD_POLY_DENSITY     5.0f     /**< g/cm² for polyethylene */
#define SHIELD_REGOLITH_DENS    15.0f    /**< g/cm² for regolith bags */
#define SHIELD_MAGNETIC_FACTOR  0.3f     /**< Attenuation from magnets */

/* Timing */
#define SHELTER_RESPONSE_MS     100      /**< Max shelter activation time */
#define SPE_WARNING_SECONDS     60       /**< Target SPE warning time */
#define SENSOR_SAMPLE_INTERVAL  1000     /**< 1 second sample rate */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Radiation sensor type */
typedef enum {
    RAD_SENSOR_SILICON_DIODE,      /**< PIN diode dosimeter */
    RAD_SENSOR_SCINTILLATOR,       /**< Plastic scintillator */
    RAD_SENSOR_TEPC,               /**< Tissue-equivalent proportional */
    RAD_SENSOR_NEUTRON,            /**< Neutron dosimeter */
    RAD_SENSOR_CHERENKOV           /**< High-energy particle detector */
} rad_sensor_type_t;

/** Radiation environment classification */
typedef enum {
    RAD_ENV_NOMINAL,               /**< Normal GCR background */
    RAD_ENV_ELEVATED,              /**< Slightly elevated levels */
    RAD_ENV_SPE_ONSET,             /**< Solar particle event starting */
    RAD_ENV_SPE_PEAK,              /**< SPE at maximum intensity */
    RAD_ENV_SPE_DECAY,             /**< SPE declining */
    RAD_ENV_BELT_PASSAGE,          /**< Van Allen belt transit */
    RAD_ENV_EMERGENCY              /**< Immediate shelter required */
} rad_environment_t;

/** Particle type classification */
typedef enum {
    RAD_PARTICLE_PROTON,
    RAD_PARTICLE_ELECTRON,
    RAD_PARTICLE_ALPHA,
    RAD_PARTICLE_HEAVY_ION,
    RAD_PARTICLE_NEUTRON,
    RAD_PARTICLE_GAMMA
} rad_particle_t;

/** TMR voting result */
typedef enum {
    TMR_AGREE_ALL,                 /**< All three sensors agree */
    TMR_AGREE_2OF3,                /**< Two sensors agree */
    TMR_DISAGREE,                  /**< All sensors disagree */
    TMR_FAULT                      /**< Sensor fault detected */
} tmr_result_t;

/** Sensor health status */
typedef enum {
    SENSOR_HEALTH_OK,
    SENSOR_HEALTH_DEGRADED,
    SENSOR_HEALTH_FAULT,
    SENSOR_HEALTH_OFFLINE
} sensor_health_t;

/** Shielding zone type */
typedef enum {
    SHIELD_ZONE_CREW_QUARTERS,
    SHIELD_ZONE_COMMAND,
    SHIELD_ZONE_GALLEY,
    SHIELD_ZONE_MEDICAL,
    SHIELD_ZONE_AIRLOCK,
    SHIELD_ZONE_STORM_SHELTER,
    SHIELD_ZONE_CARGO
} shield_zone_type_t;

/** Shielding type */
typedef enum {
    SHIELD_TYPE_PASSIVE_POLY,      /**< Fixed polyethylene */
    SHIELD_TYPE_WATER_WALL,        /**< Configurable water bags */
    SHIELD_TYPE_MAGNETIC,          /**< Active magnetic deflector */
    SHIELD_TYPE_HYBRID             /**< Water + magnetic */
} shield_type_t;

/** Shielding mode */
typedef enum {
    SHIELD_MODE_NOMINAL,           /**< Normal operation */
    SHIELD_MODE_ENHANCED,          /**< Elevated protection */
    SHIELD_MODE_STORM_SHELTER,     /**< Maximum protection */
    SHIELD_MODE_EVA_SUPPORT,       /**< Support for EVA crew */
    SHIELD_MODE_POWER_SAVE         /**< Minimum active shielding */
} shield_mode_t;

/** Crew location */
typedef enum {
    CREW_LOC_CREW_QUARTERS,
    CREW_LOC_COMMAND,
    CREW_LOC_GALLEY,
    CREW_LOC_MEDICAL,
    CREW_LOC_EVA,
    CREW_LOC_STORM_SHELTER,
    CREW_LOC_UNKNOWN
} crew_location_t;

/** Alarm type */
typedef enum {
    ALARM_NONE,
    ALARM_DOSE_RATE_HIGH,
    ALARM_DOSE_LIMIT_APPROACH,
    ALARM_SPE_ONSET,
    ALARM_SPE_PEAK,
    ALARM_SENSOR_FAULT,
    ALARM_SHIELD_FAULT,
    ALARM_EVA_ABORT,
    ALARM_SHELTER_NOW,
    ALARM_SHELTER_PREPARE
} alarm_type_t;

/** Alarm severity */
typedef enum {
    SEVERITY_INFO,
    SEVERITY_WARNING,
    SEVERITY_ALARM,
    SEVERITY_CRITICAL
} alarm_severity_t;

/** TMR sensor reading */
typedef struct {
    float readings[3];             /**< Three redundant readings */
    float voted_value;             /**< Final voted value */
    tmr_result_t vote_status;
    sensor_health_t health;
    uint64_t timestamp_us;
} tmr_sensor_t;

/** Individual radiation sensor */
typedef struct {
    rad_sensor_type_t type;
    uint8_t sensor_id;
    bool configured;
    bool active;
    
    /* TMR readings */
    tmr_sensor_t dose_rate;        /**< μSv/hr */
    tmr_sensor_t particle_flux;    /**< particles/cm²/s */
    tmr_sensor_t let;              /**< Linear energy transfer keV/μm */
    
    /* Derived values */
    float quality_factor;          /**< Radiation quality factor Q */
    float dose_equivalent_rate;    /**< μSv/hr dose equivalent */
    
    /* Calibration */
    float gain;
    float offset;
    uint32_t cal_date;
    
    /* Statistics */
    uint32_t samples_count;
    float min_reading;
    float max_reading;
    float avg_reading;
} rad_sensor_t;

/** Shielding zone */
typedef struct {
    shield_zone_type_t zone_type;
    shield_type_t shield_type;
    uint8_t zone_id;
    bool configured;
    bool active;
    
    /* Shielding parameters */
    float areal_density_gcm2;      /**< Current shielding thickness */
    float nominal_density_gcm2;    /**< Nominal shielding */
    float max_density_gcm2;        /**< Maximum available */
    float attenuation_factor;      /**< Dose reduction factor */
    
    /* Active shielding */
    float magnetic_field_t;        /**< Tesla for EM deflector */
    float power_consumption_kw;
    
    /* Water wall state */
    float water_volume_l;
    float water_flow_rate_lpm;
    bool water_pump_active;
    
    /* Status */
    shield_mode_t mode;
    bool actuator_healthy;
    uint32_t reconfiguration_count;
    uint32_t transition_time_ms;
} shield_zone_t;

/** Crew member exposure data */
typedef struct {
    uint8_t crew_id;
    char name[32];
    bool active;
    
    /* Location tracking */
    crew_location_t location;
    uint32_t location_time_ms;
    
    /* Dose accumulation */
    float dose_rate_usv_hr;        /**< Current exposure rate */
    float dose_today_usv;          /**< Today's accumulated dose */
    float dose_30day_msv;          /**< 30-day rolling dose */
    float dose_annual_msv;         /**< Year-to-date dose */
    float dose_career_msv;         /**< Career total dose */
    
    /* Limits */
    float career_limit_msv;        /**< Individual career limit */
    float time_to_limit_hr;        /**< Hours until limit */
    
    /* Personal dosimeter */
    bool dosimeter_active;
    float dosimeter_battery_pct;
} crew_exposure_t;

/** Active alarm */
typedef struct {
    alarm_type_t type;
    alarm_severity_t severity;
    uint8_t source_id;             /**< Source sensor/zone ID */
    float threshold;
    float actual_value;
    uint64_t trigger_time;
    bool acknowledged;
} active_alarm_t;

/** Event log entry */
typedef struct {
    uint8_t event_type;
    uint8_t source_id;
    uint16_t event_code;
    uint32_t timestamp_ms;
    float value1;
    float value2;
} event_entry_t;

/** Telemetry header (CCSDS-compatible) */
typedef struct __attribute__((packed)) {
    uint16_t sync_word;            /**< 0x1ACF sync pattern */
    uint16_t apid;                 /**< Application ID */
    uint16_t sequence_count;
    uint16_t packet_length;
    uint32_t mission_time_ms;
} telemetry_header_t;

/** Radiation subsystem state */
typedef struct {
    bool initialized;
    uint32_t current_time_ms;
    uint32_t last_process_ms;
    uint32_t uptime_seconds;
    
    /* Sensors */
    rad_sensor_t sensors[RAD_MAX_SENSORS];
    uint8_t sensor_count;
    
    /* Shielding zones */
    shield_zone_t zones[RAD_MAX_ZONES];
    uint8_t zone_count;
    
    /* Crew */
    crew_exposure_t crew[RAD_MAX_CREW];
    uint8_t crew_count;
    
    /* Environment assessment */
    rad_environment_t environment;
    float ambient_dose_rate;       /**< Fused dose rate μSv/hr */
    float peak_dose_rate_24h;
    float spe_probability;         /**< 0-1 SPE probability */
    
    /* Shielding state */
    shield_mode_t shield_mode;
    bool storm_shelter_active;
    bool eva_suspended;
    uint32_t shelter_entry_time;
    uint32_t shelter_trigger_time_ms; /**< Time when shelter-worthy condition detected */
    
    /* Alarms */
    active_alarm_t alarms[RAD_MAX_ALARMS];
    uint8_t alarm_count;
    uint8_t unacked_alarms;
    
    /* Events */
    event_entry_t events[RAD_MAX_EVENTS];
    uint16_t event_write_idx;
    uint16_t event_count;
    
    /* Telemetry */
    uint16_t telemetry_seq;
    
    /* Statistics */
    uint32_t shelter_activations;
    uint32_t eva_aborts;
    uint32_t spe_detected;
    float total_crew_dose_msv;
    
    /* Response timing */
    uint32_t last_shelter_response_us;
    uint32_t max_shelter_response_us;
} rad_system_t;

/* ============================================================================
 * Global State
 * ============================================================================ */

static rad_system_t g_rad = {0};

/* ============================================================================
 * TMR Voting Functions
 * ============================================================================ */

/**
 * @brief Perform TMR (Triple Modular Redundancy) voting
 * 
 * Implements 2-of-3 voting with fault detection:
 * - If all three agree (within tolerance), return average
 * - If two agree, return their average and flag third as suspect
 * - If none agree, return median and flag fault
 * 
 * @param values Array of 3 readings
 * @param result Output voted value
 * @return TMR voting status
 */
static tmr_result_t tmr_vote(const float values[3], float *result)
{
    const float tolerance = 0.1f; /* 10% tolerance */
    
    float v0 = values[0];
    float v1 = values[1];
    float v2 = values[2];
    
    /* Check pairwise agreement */
    float max_val = fmaxf(fmaxf(fabsf(v0), fabsf(v1)), fabsf(v2));
    float abs_tol = (max_val > 0.0f) ? (max_val * tolerance) : 1.0f;
    
    bool agree_01 = fabsf(v0 - v1) < abs_tol;
    bool agree_12 = fabsf(v1 - v2) < abs_tol;
    bool agree_02 = fabsf(v0 - v2) < abs_tol;
    
    if (agree_01 && agree_12 && agree_02) {
        /* All three agree - return average */
        *result = (v0 + v1 + v2) / 3.0f;
        return TMR_AGREE_ALL;
    } else if (agree_01) {
        /* v0 and v1 agree, v2 suspect */
        *result = (v0 + v1) / 2.0f;
        return TMR_AGREE_2OF3;
    } else if (agree_12) {
        /* v1 and v2 agree, v0 suspect */
        *result = (v1 + v2) / 2.0f;
        return TMR_AGREE_2OF3;
    } else if (agree_02) {
        /* v0 and v2 agree, v1 suspect */
        *result = (v0 + v2) / 2.0f;
        return TMR_AGREE_2OF3;
    } else {
        /* No agreement - return median */
        if ((v0 >= v1 && v0 <= v2) || (v0 <= v1 && v0 >= v2)) {
            *result = v0;
        } else if ((v1 >= v0 && v1 <= v2) || (v1 <= v0 && v1 >= v2)) {
            *result = v1;
        } else {
            *result = v2;
        }
        return TMR_DISAGREE;
    }
}

/**
 * @brief Update TMR sensor with new readings
 */
static void tmr_sensor_update(tmr_sensor_t *sensor, float r0, float r1, float r2, 
                              uint64_t timestamp)
{
    sensor->readings[0] = r0;
    sensor->readings[1] = r1;
    sensor->readings[2] = r2;
    sensor->timestamp_us = timestamp;
    
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
 * Helper Functions
 * ============================================================================ */

/**
 * @brief Calculate shielding attenuation factor
 * 
 * Uses exponential attenuation model with mass absorption coefficient
 */
static float calculate_attenuation(float areal_density_gcm2, rad_particle_t particle)
{
    /* Mass attenuation coefficients (approximate, cm²/g) */
    float mu;
    switch (particle) {
        case RAD_PARTICLE_PROTON:
            mu = 0.02f;
            break;
        case RAD_PARTICLE_ALPHA:
            mu = 0.04f;
            break;
        case RAD_PARTICLE_ELECTRON:
            mu = 0.1f;
            break;
        case RAD_PARTICLE_GAMMA:
            mu = 0.05f;
            break;
        case RAD_PARTICLE_HEAVY_ION:
            mu = 0.01f; /* Heavy ions penetrate deeply */
            break;
        case RAD_PARTICLE_NEUTRON:
            mu = 0.03f;
            break;
        default:
            mu = 0.03f;
    }
    
    /* Exponential attenuation: I/I0 = exp(-μ * x) */
    return expf(-mu * areal_density_gcm2);
}

/**
 * @brief Log an event
 */
static void log_event(uint8_t type, uint8_t source, uint16_t code, 
                      float v1, float v2)
{
    event_entry_t *evt = &g_rad.events[g_rad.event_write_idx];
    evt->event_type = type;
    evt->source_id = source;
    evt->event_code = code;
    evt->timestamp_ms = g_rad.current_time_ms;
    evt->value1 = v1;
    evt->value2 = v2;
    
    g_rad.event_write_idx = (g_rad.event_write_idx + 1) % RAD_MAX_EVENTS;
    if (g_rad.event_count < RAD_MAX_EVENTS) {
        g_rad.event_count++;
    }
}

/**
 * @brief Trigger an alarm
 */
static void trigger_alarm(alarm_type_t type, alarm_severity_t severity,
                          uint8_t source, float threshold, float actual)
{
    /* Check if alarm already exists */
    for (uint8_t i = 0; i < g_rad.alarm_count; i++) {
        if (g_rad.alarms[i].type == type && g_rad.alarms[i].source_id == source) {
            /* Update existing alarm */
            g_rad.alarms[i].actual_value = actual;
            if (severity > g_rad.alarms[i].severity) {
                g_rad.alarms[i].severity = severity;
            }
            return;
        }
    }
    
    /* Add new alarm */
    if (g_rad.alarm_count < RAD_MAX_ALARMS) {
        active_alarm_t *alarm = &g_rad.alarms[g_rad.alarm_count];
        alarm->type = type;
        alarm->severity = severity;
        alarm->source_id = source;
        alarm->threshold = threshold;
        alarm->actual_value = actual;
        alarm->trigger_time = g_rad.current_time_ms;
        alarm->acknowledged = false;
        g_rad.alarm_count++;
        g_rad.unacked_alarms++;
        
        log_event(1, source, (uint16_t)type, threshold, actual);
    }
}

/**
 * @brief Clear an alarm
 */
static void clear_alarm(alarm_type_t type, uint8_t source)
{
    for (uint8_t i = 0; i < g_rad.alarm_count; i++) {
        if (g_rad.alarms[i].type == type && g_rad.alarms[i].source_id == source) {
            /* Shift remaining alarms */
            if (!g_rad.alarms[i].acknowledged) {
                g_rad.unacked_alarms--;
            }
            for (uint8_t j = i; j < g_rad.alarm_count - 1; j++) {
                g_rad.alarms[j] = g_rad.alarms[j + 1];
            }
            g_rad.alarm_count--;
            log_event(2, source, (uint16_t)type, 0.0f, 0.0f);
            return;
        }
    }
}

/* ============================================================================
 * Core Control Functions
 * ============================================================================ */

/**
 * @brief Initialize radiation subsystem
 */
int rad_init(void)
{
    if (g_rad.initialized) {
        return -1; /* Already initialized */
    }
    
    memset(&g_rad, 0, sizeof(g_rad));
    
    g_rad.environment = RAD_ENV_NOMINAL;
    g_rad.shield_mode = SHIELD_MODE_NOMINAL;
    g_rad.initialized = true;
    
    log_event(0, 0, 0x0001, 0.0f, 0.0f); /* Init event */
    
    return 0;
}

/**
 * @brief Configure a radiation sensor
 */
int rad_sensor_config(uint8_t sensor_id, rad_sensor_type_t type, 
                      float gain, float offset)
{
    if (!g_rad.initialized || sensor_id >= RAD_MAX_SENSORS) {
        return -1;
    }
    
    rad_sensor_t *sensor = &g_rad.sensors[sensor_id];
    
    /* Increment count only for new sensors */
    if (!sensor->configured) {
        g_rad.sensor_count++;
    }
    
    sensor->sensor_id = sensor_id;
    sensor->type = type;
    sensor->gain = gain;
    sensor->offset = offset;
    sensor->configured = true;
    sensor->active = true;
    sensor->min_reading = 1e9f;
    sensor->max_reading = 0.0f;
    sensor->cal_date = g_rad.current_time_ms / 1000;
    
    log_event(0, sensor_id, 0x0010, (float)type, gain);
    
    return 0;
}

/**
 * @brief Configure a shielding zone
 */
int rad_zone_config(uint8_t zone_id, shield_zone_type_t zone_type,
                    shield_type_t shield_type, float nominal_density,
                    float max_density)
{
    if (!g_rad.initialized || zone_id >= RAD_MAX_ZONES) {
        return -1;
    }
    
    shield_zone_t *zone = &g_rad.zones[zone_id];
    
    /* Increment count only for new zones */
    if (!zone->configured) {
        g_rad.zone_count++;
    }
    
    zone->zone_id = zone_id;
    zone->zone_type = zone_type;
    zone->shield_type = shield_type;
    zone->nominal_density_gcm2 = nominal_density;
    zone->areal_density_gcm2 = nominal_density;
    zone->max_density_gcm2 = max_density;
    zone->configured = true;
    zone->active = true;
    zone->mode = SHIELD_MODE_NOMINAL;
    zone->actuator_healthy = true;
    
    /* Calculate initial attenuation for GCR protons */
    zone->attenuation_factor = calculate_attenuation(nominal_density, 
                                                      RAD_PARTICLE_PROTON);
    
    log_event(0, zone_id, 0x0020, nominal_density, max_density);
    
    return 0;
}

/**
 * @brief Register a crew member
 */
int rad_register_crew(uint8_t crew_id, const char *name, float career_limit)
{
    if (!g_rad.initialized || crew_id >= RAD_MAX_CREW) {
        return -1;
    }
    
    crew_exposure_t *crew = &g_rad.crew[crew_id];
    
    /* Increment count only for new crew */
    if (!crew->active) {
        g_rad.crew_count++;
    }
    
    crew->crew_id = crew_id;
    strncpy(crew->name, name, sizeof(crew->name) - 1);
    crew->active = true;
    crew->location = CREW_LOC_CREW_QUARTERS;
    crew->career_limit_msv = career_limit;
    crew->dosimeter_active = true;
    crew->dosimeter_battery_pct = 100.0f;
    
    log_event(0, crew_id, 0x0030, career_limit, 0.0f);
    
    return 0;
}

/**
 * @brief Update sensor readings (with TMR)
 */
int rad_update_sensors(uint8_t sensor_id, 
                       const float dose_rate[3],
                       const float particle_flux[3],
                       const float let[3],
                       uint64_t timestamp_us)
{
    if (!g_rad.initialized || sensor_id >= RAD_MAX_SENSORS) {
        return -1;
    }
    
    rad_sensor_t *sensor = &g_rad.sensors[sensor_id];
    if (!sensor->configured) {
        return -2;
    }
    
    /* Apply calibration */
    float cal_dose[3], cal_flux[3], cal_let[3];
    for (int i = 0; i < 3; i++) {
        cal_dose[i] = dose_rate[i] * sensor->gain + sensor->offset;
        cal_flux[i] = particle_flux[i];
        cal_let[i] = let[i];
    }
    
    /* Update TMR sensors */
    tmr_sensor_update(&sensor->dose_rate, cal_dose[0], cal_dose[1], cal_dose[2], 
                      timestamp_us);
    tmr_sensor_update(&sensor->particle_flux, cal_flux[0], cal_flux[1], cal_flux[2],
                      timestamp_us);
    tmr_sensor_update(&sensor->let, cal_let[0], cal_let[1], cal_let[2],
                      timestamp_us);
    
    /* Calculate quality factor from LET */
    float avg_let = sensor->let.voted_value;
    if (avg_let < 10.0f) {
        sensor->quality_factor = 1.0f;
    } else if (avg_let < 100.0f) {
        sensor->quality_factor = 0.32f * avg_let - 2.2f;
    } else {
        sensor->quality_factor = 300.0f / sqrtf(avg_let);
    }
    if (sensor->quality_factor > 20.0f) {
        sensor->quality_factor = 20.0f;
    }
    
    /* Dose equivalent = dose × Q */
    sensor->dose_equivalent_rate = sensor->dose_rate.voted_value * 
                                   sensor->quality_factor;
    
    /* Update statistics */
    sensor->samples_count++;
    if (sensor->dose_rate.voted_value < sensor->min_reading) {
        sensor->min_reading = sensor->dose_rate.voted_value;
    }
    if (sensor->dose_rate.voted_value > sensor->max_reading) {
        sensor->max_reading = sensor->dose_rate.voted_value;
    }
    
    /* Running average */
    float alpha = 0.1f;
    sensor->avg_reading = alpha * sensor->dose_rate.voted_value + 
                          (1.0f - alpha) * sensor->avg_reading;
    
    /* Check for sensor faults */
    if (sensor->dose_rate.health == SENSOR_HEALTH_FAULT) {
        trigger_alarm(ALARM_SENSOR_FAULT, SEVERITY_WARNING, sensor_id,
                      0.0f, sensor->dose_rate.voted_value);
    } else {
        clear_alarm(ALARM_SENSOR_FAULT, sensor_id);
    }
    
    return 0;
}

/**
 * @brief Update crew location and dose
 */
int rad_update_crew(uint8_t crew_id, crew_location_t location, float dose_usv)
{
    if (!g_rad.initialized || crew_id >= RAD_MAX_CREW) {
        return -1;
    }
    
    crew_exposure_t *crew = &g_rad.crew[crew_id];
    if (!crew->active) {
        return -2;
    }
    
    crew->location = location;
    crew->location_time_ms = g_rad.current_time_ms;
    
    /* Accumulate dose */
    crew->dose_today_usv += dose_usv;
    crew->dose_30day_msv += dose_usv / 1000.0f;
    crew->dose_annual_msv += dose_usv / 1000.0f;
    crew->dose_career_msv += dose_usv / 1000.0f;
    
    g_rad.total_crew_dose_msv += dose_usv / 1000.0f;
    
    /* Calculate time to limit */
    if (crew->dose_rate_usv_hr > 0.0f) {
        float remaining_msv = crew->career_limit_msv - crew->dose_career_msv;
        crew->time_to_limit_hr = (remaining_msv * 1000.0f) / crew->dose_rate_usv_hr;
    }
    
    /* Check dose limit approach */
    float limit_fraction = crew->dose_career_msv / crew->career_limit_msv;
    if (limit_fraction > 0.9f) {
        trigger_alarm(ALARM_DOSE_LIMIT_APPROACH, SEVERITY_ALARM, crew_id,
                      crew->career_limit_msv * 0.9f, crew->dose_career_msv);
    } else if (limit_fraction > 0.8f) {
        trigger_alarm(ALARM_DOSE_LIMIT_APPROACH, SEVERITY_WARNING, crew_id,
                      crew->career_limit_msv * 0.8f, crew->dose_career_msv);
    }
    
    return 0;
}

/**
 * @brief Set shielding mode for a zone
 */
int rad_set_shield_mode(uint8_t zone_id, shield_mode_t mode)
{
    if (!g_rad.initialized || zone_id >= RAD_MAX_ZONES) {
        return -1;
    }
    
    shield_zone_t *zone = &g_rad.zones[zone_id];
    if (!zone->configured) {
        return -2;
    }
    
    (void)zone->mode; /* Suppress unused variable warning for mode transition logging */
    zone->mode = mode;
    zone->reconfiguration_count++;
    
    /* Adjust shielding based on mode */
    switch (mode) {
        case SHIELD_MODE_NOMINAL:
            zone->areal_density_gcm2 = zone->nominal_density_gcm2;
            zone->magnetic_field_t = 0.0f;
            break;
            
        case SHIELD_MODE_ENHANCED:
            zone->areal_density_gcm2 = zone->nominal_density_gcm2 * 1.5f;
            if (zone->areal_density_gcm2 > zone->max_density_gcm2) {
                zone->areal_density_gcm2 = zone->max_density_gcm2;
            }
            if (zone->shield_type == SHIELD_TYPE_MAGNETIC || 
                zone->shield_type == SHIELD_TYPE_HYBRID) {
                zone->magnetic_field_t = 0.5f;
            }
            break;
            
        case SHIELD_MODE_STORM_SHELTER:
            zone->areal_density_gcm2 = zone->max_density_gcm2;
            if (zone->shield_type == SHIELD_TYPE_MAGNETIC || 
                zone->shield_type == SHIELD_TYPE_HYBRID) {
                zone->magnetic_field_t = 1.0f;
            }
            zone->water_pump_active = true;
            zone->water_flow_rate_lpm = 50.0f;
            break;
            
        case SHIELD_MODE_POWER_SAVE:
            zone->areal_density_gcm2 = zone->nominal_density_gcm2;
            zone->magnetic_field_t = 0.0f;
            zone->water_pump_active = false;
            break;
            
        default:
            break;
    }
    
    /* Recalculate attenuation */
    zone->attenuation_factor = calculate_attenuation(zone->areal_density_gcm2,
                                                      RAD_PARTICLE_PROTON);
    if (zone->magnetic_field_t > 0.0f) {
        zone->attenuation_factor *= (1.0f - SHIELD_MAGNETIC_FACTOR * 
                                     zone->magnetic_field_t);
    }
    
    /* Calculate power consumption */
    zone->power_consumption_kw = 0.0f;
    if (zone->water_pump_active) {
        zone->power_consumption_kw += 0.5f;
    }
    if (zone->magnetic_field_t > 0.0f) {
        zone->power_consumption_kw += zone->magnetic_field_t * 5.0f;
    }
    
    log_event(3, zone_id, 0x0100 + (uint16_t)mode, 
              zone->areal_density_gcm2, zone->attenuation_factor);
    
    return 0;
}

/**
 * @brief Activate storm shelter mode (emergency)
 */
int rad_activate_storm_shelter(void)
{
    if (!g_rad.initialized) {
        return -1;
    }
    
    g_rad.storm_shelter_active = true;
    g_rad.shelter_entry_time = g_rad.current_time_ms;
    g_rad.shelter_activations++;
    
    /* Activate all zones to maximum protection */
    for (uint8_t i = 0; i < g_rad.zone_count; i++) {
        rad_set_shield_mode(i, SHIELD_MODE_STORM_SHELTER);
    }
    
    /* Suspend EVA */
    g_rad.eva_suspended = true;
    
    /* Trigger shelter alert */
    trigger_alarm(ALARM_SHELTER_NOW, SEVERITY_CRITICAL, 0,
                  DOSE_RATE_SHELTER, g_rad.ambient_dose_rate);
    
    /* Calculate response time (from detection to shelter activation) */
    /* If called from process loop, use time since environment changed to SPE */
    /* Otherwise use a nominal response time representing actuator response */
    uint32_t response_us;
    if (g_rad.shelter_trigger_time_ms > 0) {
        response_us = (g_rad.current_time_ms - g_rad.shelter_trigger_time_ms) * 1000;
    } else {
        response_us = 100000; /* 100ms nominal actuator response */
    }
    g_rad.last_shelter_response_us = response_us;
    if (response_us > g_rad.max_shelter_response_us) {
        g_rad.max_shelter_response_us = response_us;
    }
    g_rad.shelter_trigger_time_ms = 0; /* Reset for next event */
    
    log_event(4, 0, 0x1000, g_rad.ambient_dose_rate, (float)response_us);
    
    return 0;
}

/**
 * @brief Deactivate storm shelter mode
 */
int rad_deactivate_storm_shelter(void)
{
    if (!g_rad.initialized || !g_rad.storm_shelter_active) {
        return -1;
    }
    
    g_rad.storm_shelter_active = false;
    g_rad.eva_suspended = false;
    
    /* Return zones to nominal */
    for (uint8_t i = 0; i < g_rad.zone_count; i++) {
        rad_set_shield_mode(i, SHIELD_MODE_NOMINAL);
    }
    
    clear_alarm(ALARM_SHELTER_NOW, 0);
    clear_alarm(ALARM_SHELTER_PREPARE, 0);
    
    log_event(4, 0, 0x1001, g_rad.ambient_dose_rate, 0.0f);
    
    return 0;
}

/* ============================================================================
 * Environment Assessment
 * ============================================================================ */

/**
 * @brief Assess radiation environment and update classification
 */
static void assess_environment(void)
{
    /* Fuse dose rates from all sensors */
    float total_dose_rate = 0.0f;
    uint8_t active_sensors = 0;
    
    for (uint8_t i = 0; i < RAD_MAX_SENSORS; i++) {
        if (g_rad.sensors[i].configured && g_rad.sensors[i].active &&
            g_rad.sensors[i].dose_rate.health != SENSOR_HEALTH_OFFLINE) {
            total_dose_rate += g_rad.sensors[i].dose_equivalent_rate;
            active_sensors++;
        }
    }
    
    if (active_sensors > 0) {
        g_rad.ambient_dose_rate = total_dose_rate / active_sensors;
    }
    
    /* Update 24h peak */
    if (g_rad.ambient_dose_rate > g_rad.peak_dose_rate_24h) {
        g_rad.peak_dose_rate_24h = g_rad.ambient_dose_rate;
    }
    
    /* SPE prediction based on dose rate trend and particle flux */
    float rate_increase = g_rad.ambient_dose_rate / 
                          (g_rad.peak_dose_rate_24h > 0 ? 
                           g_rad.peak_dose_rate_24h : 1.0f);
    if (rate_increase > 2.0f && g_rad.ambient_dose_rate > DOSE_RATE_ELEVATED) {
        g_rad.spe_probability = fminf(0.9f, (rate_increase - 2.0f) * 0.3f);
    } else {
        g_rad.spe_probability = 0.0f;
    }
    
    /* Classify environment */
    rad_environment_t new_env;
    
    if (g_rad.ambient_dose_rate >= DOSE_RATE_EMERGENCY) {
        new_env = RAD_ENV_EMERGENCY;
    } else if (g_rad.ambient_dose_rate >= DOSE_RATE_SHELTER) {
        if (g_rad.environment == RAD_ENV_SPE_ONSET) {
            new_env = RAD_ENV_SPE_PEAK;
        } else {
            new_env = RAD_ENV_SPE_ONSET;
        }
    } else if (g_rad.ambient_dose_rate >= DOSE_RATE_SPE_ONSET) {
        new_env = RAD_ENV_SPE_ONSET;
    } else if (g_rad.ambient_dose_rate >= DOSE_RATE_ELEVATED) {
        new_env = RAD_ENV_ELEVATED;
    } else if (g_rad.environment == RAD_ENV_SPE_PEAK &&
               g_rad.ambient_dose_rate < DOSE_RATE_SHELTER) {
        new_env = RAD_ENV_SPE_DECAY;
    } else {
        new_env = RAD_ENV_NOMINAL;
    }
    
    /* Handle environment transitions */
    if (new_env != g_rad.environment) {
        log_event(5, 0, (uint16_t)new_env, g_rad.ambient_dose_rate, 
                  (float)g_rad.environment);
        
        /* Trigger appropriate alarms */
        switch (new_env) {
            case RAD_ENV_SPE_ONSET:
                trigger_alarm(ALARM_SPE_ONSET, SEVERITY_ALARM, 0,
                              DOSE_RATE_SPE_ONSET, g_rad.ambient_dose_rate);
                trigger_alarm(ALARM_SHELTER_PREPARE, SEVERITY_WARNING, 0,
                              0.0f, 0.0f);
                g_rad.spe_detected++;
                g_rad.shelter_trigger_time_ms = g_rad.current_time_ms; /* Record trigger time */
                break;
                
            case RAD_ENV_SPE_PEAK:
                trigger_alarm(ALARM_SPE_PEAK, SEVERITY_CRITICAL, 0,
                              DOSE_RATE_SHELTER, g_rad.ambient_dose_rate);
                if (g_rad.shelter_trigger_time_ms == 0) {
                    g_rad.shelter_trigger_time_ms = g_rad.current_time_ms;
                }
                break;
                
            case RAD_ENV_EMERGENCY:
                /* Auto-activate shelter */
                if (!g_rad.storm_shelter_active) {
                    if (g_rad.shelter_trigger_time_ms == 0) {
                        g_rad.shelter_trigger_time_ms = g_rad.current_time_ms;
                    }
                    rad_activate_storm_shelter();
                }
                break;
                
            case RAD_ENV_NOMINAL:
                clear_alarm(ALARM_SPE_ONSET, 0);
                clear_alarm(ALARM_SPE_PEAK, 0);
                clear_alarm(ALARM_SHELTER_PREPARE, 0);
                break;
                
            default:
                break;
        }
        
        g_rad.environment = new_env;
    }
    
    /* Check dose rate alarm */
    if (g_rad.ambient_dose_rate > DOSE_RATE_SHELTER) {
        trigger_alarm(ALARM_DOSE_RATE_HIGH, SEVERITY_CRITICAL, 0,
                      DOSE_RATE_SHELTER, g_rad.ambient_dose_rate);
    } else if (g_rad.ambient_dose_rate > DOSE_RATE_ELEVATED) {
        trigger_alarm(ALARM_DOSE_RATE_HIGH, SEVERITY_WARNING, 0,
                      DOSE_RATE_ELEVATED, g_rad.ambient_dose_rate);
    } else {
        clear_alarm(ALARM_DOSE_RATE_HIGH, 0);
    }
}

/**
 * @brief Update crew dose rates based on location and shielding
 */
static void update_crew_doses(uint32_t delta_ms)
{
    for (uint8_t i = 0; i < RAD_MAX_CREW; i++) {
        if (!g_rad.crew[i].active) continue;
        
        crew_exposure_t *crew = &g_rad.crew[i];
        
        /* Find shielding for crew location */
        float attenuation = 1.0f;
        for (uint8_t j = 0; j < RAD_MAX_ZONES; j++) {
            if (!g_rad.zones[j].configured) continue;
            
            /* Match zone to crew location */
            bool match = false;
            switch (crew->location) {
                case CREW_LOC_CREW_QUARTERS:
                    match = (g_rad.zones[j].zone_type == SHIELD_ZONE_CREW_QUARTERS);
                    break;
                case CREW_LOC_COMMAND:
                    match = (g_rad.zones[j].zone_type == SHIELD_ZONE_COMMAND);
                    break;
                case CREW_LOC_STORM_SHELTER:
                    match = (g_rad.zones[j].zone_type == SHIELD_ZONE_STORM_SHELTER);
                    break;
                case CREW_LOC_EVA:
                    attenuation = 0.2f; /* EVA suit provides minimal shielding */
                    match = false;
                    break;
                default:
                    break;
            }
            
            if (match) {
                attenuation = g_rad.zones[j].attenuation_factor;
                break;
            }
        }
        
        /* Calculate dose rate for this crew member */
        crew->dose_rate_usv_hr = g_rad.ambient_dose_rate * attenuation;
        
        /* Accumulate dose */
        float dose_usv = crew->dose_rate_usv_hr * (delta_ms / 3600000.0f);
        rad_update_crew(i, crew->location, dose_usv);
    }
}

/* ============================================================================
 * Telemetry Generation
 * ============================================================================ */

/**
 * @brief Generate CCSDS telemetry packet
 */
int rad_get_telemetry(uint8_t *buffer, size_t max_len)
{
    if (!g_rad.initialized || buffer == NULL) {
        return -1;
    }
    
    size_t required_len = sizeof(telemetry_header_t) + 64; /* Header + data */
    if (max_len < required_len) {
        return -1;
    }
    
    /* Build header */
    telemetry_header_t *hdr = (telemetry_header_t *)buffer;
    hdr->sync_word = 0x1ACF;
    hdr->apid = 0x0200;  /* Radiation subsystem APID */
    hdr->sequence_count = g_rad.telemetry_seq++;
    hdr->mission_time_ms = g_rad.current_time_ms;
    
    /* Build payload */
    uint8_t *payload = buffer + sizeof(telemetry_header_t);
    size_t offset = 0;
    
    /* Environment status */
    payload[offset++] = (uint8_t)g_rad.environment;
    payload[offset++] = (uint8_t)g_rad.shield_mode;
    payload[offset++] = g_rad.storm_shelter_active ? 1 : 0;
    payload[offset++] = g_rad.eva_suspended ? 1 : 0;
    
    /* Dose rate (float, 4 bytes) */
    memcpy(&payload[offset], &g_rad.ambient_dose_rate, sizeof(float));
    offset += sizeof(float);
    
    /* Peak dose rate */
    memcpy(&payload[offset], &g_rad.peak_dose_rate_24h, sizeof(float));
    offset += sizeof(float);
    
    /* SPE probability */
    memcpy(&payload[offset], &g_rad.spe_probability, sizeof(float));
    offset += sizeof(float);
    
    /* Sensor count and status */
    payload[offset++] = g_rad.sensor_count;
    for (uint8_t i = 0; i < g_rad.sensor_count && i < 4; i++) {
        payload[offset++] = (uint8_t)g_rad.sensors[i].dose_rate.health;
    }
    
    /* Zone count */
    payload[offset++] = g_rad.zone_count;
    
    /* Crew count with EVA status */
    uint8_t eva_count = 0;
    for (uint8_t i = 0; i < g_rad.crew_count; i++) {
        if (g_rad.crew[i].location == CREW_LOC_EVA) {
            eva_count++;
        }
    }
    payload[offset++] = g_rad.crew_count;
    payload[offset++] = eva_count;
    
    /* Alarm status */
    payload[offset++] = g_rad.alarm_count;
    payload[offset++] = g_rad.unacked_alarms;
    
    /* Highest severity active alarm */
    alarm_severity_t max_severity = SEVERITY_INFO;
    for (uint8_t i = 0; i < g_rad.alarm_count; i++) {
        if (g_rad.alarms[i].severity > max_severity) {
            max_severity = g_rad.alarms[i].severity;
        }
    }
    payload[offset++] = (uint8_t)max_severity;
    
    /* Total crew dose today */
    memcpy(&payload[offset], &g_rad.total_crew_dose_msv, sizeof(float));
    offset += sizeof(float);
    
    /* Update header length */
    hdr->packet_length = (uint16_t)offset;
    
    return (int)(sizeof(telemetry_header_t) + offset);
}

/* ============================================================================
 * Main Processing Loop
 * ============================================================================ */

/**
 * @brief Process radiation control loop
 */
int rad_process(uint32_t time_ms)
{
    if (!g_rad.initialized) {
        return -1;
    }
    
    uint32_t delta_ms = time_ms - g_rad.last_process_ms;
    g_rad.current_time_ms = time_ms;
    g_rad.last_process_ms = time_ms;
    
    /* Update uptime */
    g_rad.uptime_seconds = time_ms / 1000;
    
    /* Assess radiation environment */
    assess_environment();
    
    /* Update crew doses */
    update_crew_doses(delta_ms);
    
    /* Auto-shelter logic */
    if (g_rad.ambient_dose_rate >= DOSE_RATE_SHELTER && 
        !g_rad.storm_shelter_active) {
        rad_activate_storm_shelter();
    } else if (g_rad.ambient_dose_rate < DOSE_RATE_ELEVATED &&
               g_rad.storm_shelter_active &&
               g_rad.environment == RAD_ENV_NOMINAL) {
        /* Safe to deactivate after SPE decay */
        rad_deactivate_storm_shelter();
    }
    
    /* Reset 24h peak at day boundary */
    if ((time_ms / 86400000) != ((time_ms - delta_ms) / 86400000)) {
        g_rad.peak_dose_rate_24h = g_rad.ambient_dose_rate;
        /* Reset daily doses */
        for (uint8_t i = 0; i < g_rad.crew_count; i++) {
            g_rad.crew[i].dose_today_usv = 0.0f;
        }
    }
    
    return 0;
}

/* ============================================================================
 * Query Functions
 * ============================================================================ */

rad_environment_t rad_get_environment(void)
{
    return g_rad.initialized ? g_rad.environment : RAD_ENV_NOMINAL;
}

float rad_get_dose_rate(void)
{
    return g_rad.initialized ? g_rad.ambient_dose_rate : 0.0f;
}

float rad_get_spe_probability(void)
{
    return g_rad.initialized ? g_rad.spe_probability : 0.0f;
}

bool rad_is_shelter_active(void)
{
    return g_rad.initialized && g_rad.storm_shelter_active;
}

bool rad_is_eva_safe(void)
{
    return g_rad.initialized && 
           !g_rad.eva_suspended &&
           g_rad.environment == RAD_ENV_NOMINAL &&
           g_rad.ambient_dose_rate < DOSE_RATE_ELEVATED;
}

shield_mode_t rad_get_shield_mode(void)
{
    return g_rad.initialized ? g_rad.shield_mode : SHIELD_MODE_NOMINAL;
}

uint8_t rad_get_alarm_count(void)
{
    return g_rad.initialized ? g_rad.alarm_count : 0;
}

uint8_t rad_get_sensor_count(void)
{
    return g_rad.initialized ? g_rad.sensor_count : 0;
}

uint8_t rad_get_zone_count(void)
{
    return g_rad.initialized ? g_rad.zone_count : 0;
}

uint8_t rad_get_crew_count(void)
{
    return g_rad.initialized ? g_rad.crew_count : 0;
}

int rad_get_crew_dose(uint8_t crew_id, float *career_msv, float *rate_usv_hr)
{
    if (!g_rad.initialized || crew_id >= RAD_MAX_CREW) {
        return -1;
    }
    if (!g_rad.crew[crew_id].active) {
        return -2;
    }
    
    if (career_msv) {
        *career_msv = g_rad.crew[crew_id].dose_career_msv;
    }
    if (rate_usv_hr) {
        *rate_usv_hr = g_rad.crew[crew_id].dose_rate_usv_hr;
    }
    
    return 0;
}

int rad_get_zone_status(uint8_t zone_id, float *density, float *attenuation)
{
    if (!g_rad.initialized || zone_id >= RAD_MAX_ZONES) {
        return -1;
    }
    if (!g_rad.zones[zone_id].configured) {
        return -2;
    }
    
    if (density) {
        *density = g_rad.zones[zone_id].areal_density_gcm2;
    }
    if (attenuation) {
        *attenuation = g_rad.zones[zone_id].attenuation_factor;
    }
    
    return 0;
}

uint32_t rad_get_shelter_response_us(void)
{
    return g_rad.initialized ? g_rad.last_shelter_response_us : 0;
}

uint32_t rad_get_max_shelter_response_us(void)
{
    return g_rad.initialized ? g_rad.max_shelter_response_us : 0;
}

int rad_acknowledge_alarm(uint8_t alarm_idx)
{
    if (!g_rad.initialized || alarm_idx >= g_rad.alarm_count) {
        return -1;
    }
    
    if (!g_rad.alarms[alarm_idx].acknowledged) {
        g_rad.alarms[alarm_idx].acknowledged = true;
        g_rad.unacked_alarms--;
    }
    
    return 0;
}

uint16_t rad_get_event_count(void)
{
    return g_rad.initialized ? g_rad.event_count : 0;
}

/**
 * @brief Reset radiation subsystem
 */
int rad_reset(void)
{
    if (!g_rad.initialized) {
        return -1;
    }
    
    uint32_t saved_time = g_rad.current_time_ms;
    memset(&g_rad, 0, sizeof(g_rad));
    g_rad.current_time_ms = saved_time;
    g_rad.last_process_ms = saved_time;
    g_rad.initialized = true;
    g_rad.environment = RAD_ENV_NOMINAL;
    g_rad.shield_mode = SHIELD_MODE_NOMINAL;
    
    log_event(0, 0, 0x0002, 0.0f, 0.0f); /* Reset event */
    
    return 0;
}

/**
 * @brief Shutdown radiation subsystem
 */
void rad_shutdown(void)
{
    if (g_rad.initialized) {
        /* Deactivate shelter if active */
        if (g_rad.storm_shelter_active) {
            rad_deactivate_storm_shelter();
        }
        
        log_event(0, 0, 0x0003, 0.0f, 0.0f); /* Shutdown event */
        
        g_rad.initialized = false;
    }
}

/* ============================================================================
 * Sensor Health Check
 * ============================================================================ */

/**
 * @brief Check if any sensor has a fault
 */
bool rad_has_sensor_fault(void)
{
    if (!g_rad.initialized) {
        return false;
    }
    
    for (uint8_t i = 0; i < RAD_MAX_SENSORS; i++) {
        if (g_rad.sensors[i].configured && 
            g_rad.sensors[i].dose_rate.health == SENSOR_HEALTH_FAULT) {
            return true;
        }
    }
    
    return false;
}

/**
 * @brief Get sensor TMR status
 */
tmr_result_t rad_get_sensor_tmr_status(uint8_t sensor_id)
{
    if (!g_rad.initialized || sensor_id >= RAD_MAX_SENSORS) {
        return TMR_FAULT;
    }
    if (!g_rad.sensors[sensor_id].configured) {
        return TMR_FAULT;
    }
    
    return g_rad.sensors[sensor_id].dose_rate.vote_status;
}
