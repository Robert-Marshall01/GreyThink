/**
 * @file feeding_automation.h
 * @brief Automated Feeding Control for Aquaculture
 *
 * INDUSTRY RELEVANCE:
 * Feed represents 50-70% of aquaculture operating costs. Precision feeding
 * systems optimize feed conversion ratios (FCR), reduce waste, and improve
 * fish welfare. This module demonstrates embedded control for actuators and
 * scheduling systems used by modern fish farms worldwide.
 *
 * Key capabilities demonstrated:
 * - Demand-based feeding algorithms
 * - Multi-zone feeder coordination
 * - Feed rate calibration and monitoring
 * - Integration with water quality and fish behavior sensors
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_FEEDING_AUTOMATION_H
#define GF_FEEDING_AUTOMATION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Feeding mode */
typedef enum {
    GF_FEED_MODE_SCHEDULED,         /**< Time-based feeding */
    GF_FEED_MODE_DEMAND,            /**< Fish behavior triggered */
    GF_FEED_MODE_ADAPTIVE,          /**< AI-optimized feeding */
    GF_FEED_MODE_MANUAL             /**< Manual override */
} gf_feed_mode_t;

/** Feeder configuration */
typedef struct {
    uint8_t feeder_id;
    uint8_t zone_id;
    float feed_rate_kg_min;
    float daily_ration_kg;
    uint8_t meals_per_day;
} gf_feeder_config_t;

/** Feeding event record */
typedef struct {
    uint32_t timestamp;
    uint8_t feeder_id;
    float amount_kg;
    float duration_sec;
    float water_temp_c;
    float do_level_mg_l;
} gf_feed_event_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_feed_init(void);
int gf_feed_configure(const gf_feeder_config_t* config);
int gf_feed_dispense(uint8_t feeder_id, float amount_kg);
int gf_feed_set_mode(gf_feed_mode_t mode);
int gf_feed_get_daily_total(uint8_t zone_id, float* total_kg);
void gf_feed_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_FEEDING_AUTOMATION_H */


/** Engine operational states */
typedef enum {
    ENGINE_STATE_OFF,
    ENGINE_STATE_PRESTART,          /**< Pre-ignition checks */
    ENGINE_STATE_IGNITION,          /**< Igniter sequence */
    ENGINE_STATE_MAINSTAGE,         /**< Full thrust operation */
    ENGINE_STATE_THROTTLE,          /**< Throttled operation */
    ENGINE_STATE_SHUTDOWN,          /**< Controlled shutdown */
    ENGINE_STATE_ABORT,             /**< Emergency shutdown */
    ENGINE_STATE_FAULT              /**< Unrecoverable fault */
} engine_state_t;

/** Launch vehicle phase */
typedef enum {
    PHASE_PRELAUNCH,
    PHASE_TERMINAL_COUNT,
    PHASE_IGNITION_SEQUENCE,
    PHASE_LIFTOFF,
    PHASE_PITCH_PROGRAM,
    PHASE_MAX_Q,
    PHASE_THROTTLE_BUCKET,
    PHASE_MAX_THRUST,
    PHASE_MECO,                     /**< Main Engine Cutoff */
    PHASE_STAGE_SEPARATION,
    PHASE_MISSION_COMPLETE,
    PHASE_ABORT
} launch_phase_t;

/** Abort trigger types */
typedef enum {
    ABORT_NONE = 0,
    ABORT_THRUST_LOW,               /**< Thrust below threshold */
    ABORT_THRUST_HIGH,              /**< Thrust exceeding limit */
    ABORT_THRUST_ASYMMETRY,         /**< Inter-engine imbalance */
    ABORT_CHAMBER_OVERPRESSURE,     /**< Chamber pressure high */
    ABORT_CHAMBER_OVERTEMP,         /**< Chamber temperature high */
    ABORT_TURBOPUMP_OVERTEMP,       /**< Turbopump bearing hot */
    ABORT_VIBRATION_EXCESSIVE,      /**< Structural vibration */
    ABORT_PROPELLANT_LEAK,          /**< Propellant system leak */
    ABORT_IGNITION_FAILURE,         /**< Failed ignition */
    ABORT_GUIDANCE_FAILURE,         /**< GNC system failure */
    ABORT_SENSOR_FAILURE,           /**< Critical sensor loss */
    ABORT_RANGE_SAFETY,             /**< Range safety command */
    ABORT_MANUAL                    /**< Manual abort command */
} abort_type_t;

/** Alarm severity levels */
typedef enum {
    SEVERITY_INFO,
    SEVERITY_CAUTION,
    SEVERITY_WARNING,
    SEVERITY_ABORT
} alarm_severity_t;

/** Sensor types */
typedef enum {
    SENSOR_THRUST_MAIN,
    SENSOR_CHAMBER_PRESSURE,
    SENSOR_CHAMBER_TEMP,
    SENSOR_NOZZLE_TEMP,
    SENSOR_TURBOPUMP_TEMP,
    SENSOR_INJECTOR_PRESSURE,
    SENSOR_OXIDIZER_FLOW,
    SENSOR_FUEL_FLOW,
    SENSOR_VIBRATION_AXIAL,
    SENSOR_VIBRATION_LATERAL,
    SENSOR_PROPELLANT_TANK_PRESS,
    SENSOR_PROPELLANT_TANK_TEMP,
    SENSOR_TYPE_COUNT
} sensor_type_t;

/** Individual sensor reading */
typedef struct {
    uint8_t sensor_id;
    sensor_type_t type;
    uint8_t channel;                /**< Redundant channel (0-2) */
    float value;
    float uncertainty;
    uint64_t timestamp_ns;
    bool valid;
    bool in_range;
} sensor_reading_t;

/** TMR-voted sensor value */
typedef struct {
    sensor_type_t type;
    float voted_value;
    float channel_values[TMR_CHANNELS];
    uint8_t valid_channels;
    bool agreement;                 /**< Channels agree within tolerance */
    bool failed;                    /**< All channels failed */
} tmr_sensor_t;

/** Engine status */
typedef struct {
    uint8_t engine_id;
    engine_state_t state;
    float thrust_kn;                /**< Current thrust in kN */
    float thrust_pct;               /**< Thrust as % of nominal */
    float chamber_pressure_psi;
    float chamber_temp_c;
    float nozzle_temp_c;
    float turbopump_temp_c;
    float injector_dp_pct;          /**< Injector delta-P % */
    float oxidizer_flow_kg_s;
    float fuel_flow_kg_s;
    float mixture_ratio;
    float vibration_g;
    float isp_s;                    /**< Specific impulse seconds */
    uint32_t run_time_ms;
    bool healthy;
} engine_status_t;

/** Safety interlock status */
typedef struct {
    bool armed;
    bool abort_triggered;
    abort_type_t abort_reason;
    uint8_t abort_engine_id;        /**< Engine that triggered abort */
    uint64_t abort_timestamp_ns;
    uint32_t abort_latency_us;      /**< Measured abort response time */
    uint8_t tmr_health[TMR_CHANNELS];
    bool all_channels_healthy;
    bool fts_armed;                 /**< Flight Termination System */
    uint32_t safety_checks_per_sec;
} safety_status_t;

/** Telemetry frame header (CCSDS-like) */
typedef struct {
    uint16_t version;
    uint16_t spacecraft_id;
    uint16_t virtual_channel;
    uint32_t frame_count;
    uint64_t mission_time_ns;
    uint8_t frame_type;
    uint8_t apid;                   /**< Application ID */
} tlm_frame_header_t;

/** Telemetry statistics */
typedef struct {
    uint64_t frames_generated;
    uint64_t frames_transmitted;
    uint64_t frames_dropped;
    uint32_t current_rate_hz;
    float buffer_utilization;
} tlm_stats_t;

/** Alarm record */
typedef struct {
    uint32_t alarm_id;
    abort_type_t type;
    alarm_severity_t severity;
    uint8_t engine_id;
    uint64_t timestamp_ns;
    float value;
    float threshold;
    bool active;
    bool acknowledged;
} alarm_t;

/** Event log entry */
typedef struct {
    uint32_t event_id;
    uint64_t timestamp_ns;
    const char* message;
    uint8_t engine_id;
    float value;
} event_t;

/** History buffer for trend analysis */
typedef struct {
    float samples[HISTORY_SIZE];
    uint32_t write_index;
    uint32_t count;
} history_t;

/** Engine configuration */
typedef struct {
    uint8_t engine_id;
    const char* engine_name;
    float nominal_thrust_kn;
    float max_thrust_kn;
    float min_throttle_pct;
    float nominal_chamber_pressure_psi;
    float nominal_mixture_ratio;
    uint16_t sensor_count;
} engine_config_t;

/** Launch vehicle configuration */
typedef struct {
    uint16_t vehicle_id;
    const char* vehicle_name;
    uint8_t engine_count;
    uint8_t stage_number;
    bool human_rated;
    float max_q_dynamic_pressure_pa;
} vehicle_config_t;

/** Rocket system global state */
typedef struct {
    bool initialized;
    
    /* Vehicle configuration */
    vehicle_config_t vehicle;
    engine_config_t engines[MAX_ENGINES];
    
    /* Engine status */
    engine_status_t engine_status[MAX_ENGINES];
    
    /* Safety system */
    safety_status_t safety;
    bool safety_armed;
    
    /* Launch sequence */
    launch_phase_t phase;
    uint64_t liftoff_time_ns;
    uint64_t mission_time_ns;
    int32_t t_minus_ms;             /**< Countdown (negative before liftoff) */
    
    /* Sensor data */
    sensor_reading_t sensors[TOTAL_MAX_SENSORS];
    tmr_sensor_t tmr_sensors[MAX_ENGINES][SENSOR_TYPE_COUNT];
    uint16_t sensor_count;
    
    /* Telemetry */
    tlm_stats_t tlm_stats;
    uint8_t tlm_buffer[TLM_BUFFER_DEPTH][TLM_FRAME_SIZE];
    uint32_t tlm_write_index;
    uint32_t tlm_read_index;
    uint32_t tlm_frame_count;
    
    /* Alarms and events */
    alarm_t alarms[ALARM_MAX];
    uint8_t alarm_count;
    event_t events[EVENT_LOG_SIZE];
    uint16_t event_count;
    uint32_t next_alarm_id;
    uint32_t next_event_id;
    
    /* History buffers */
    history_t thrust_history[MAX_ENGINES];
    history_t chamber_temp_history[MAX_ENGINES];
    history_t vibration_history[MAX_ENGINES];
    
    /* Statistics */
    uint64_t total_run_time_ms;
    uint32_t ignition_count;
    uint32_t abort_count;
    uint32_t safety_checks;
    uint64_t last_safety_check_ns;
    
    /* Timing */
    uint64_t current_time_ns;
    uint32_t process_count;
} rocket_state_t;

/*===========================================================================*/
/* Global State                                                               */
/*===========================================================================*/

static rocket_state_t g_rocket = {0};

/*===========================================================================*/
/* Forward Declarations                                                       */
/*===========================================================================*/

static void rocket_log_event(uint8_t engine_id, const char* msg, float value);
static void rocket_set_alarm(abort_type_t type, alarm_severity_t severity,
                             uint8_t engine_id, float value, float threshold);
static void rocket_clear_alarm(abort_type_t type, uint8_t engine_id);
static void update_history(history_t* h, float value);
static float get_history_average(const history_t* h, uint32_t samples);
static float get_history_max(const history_t* h, uint32_t samples);

/*===========================================================================*/
/* Utility Functions                                                          */
/*===========================================================================*/

/**
 * @brief Get current time in nanoseconds (platform abstraction)
 */
static uint64_t rocket_get_time_ns(void) {
    return g_rocket.current_time_ns;
}

/**
 * @brief Update history buffer with new sample
 */
static void update_history(history_t* h, float value) {
    h->samples[h->write_index] = value;
    h->write_index = (h->write_index + 1) % HISTORY_SIZE;
    if (h->count < HISTORY_SIZE) h->count++;
}

/**
 * @brief Get average of last N samples from history
 */
static float get_history_average(const history_t* h, uint32_t samples) {
    if (h->count == 0) return 0;
    if (samples > h->count) samples = h->count;
    
    float sum = 0;
    uint32_t idx = (h->write_index + HISTORY_SIZE - 1) % HISTORY_SIZE;
    for (uint32_t i = 0; i < samples; i++) {
        sum += h->samples[idx];
        idx = (idx + HISTORY_SIZE - 1) % HISTORY_SIZE;
    }
    return sum / samples;
}

/**
 * @brief Get maximum value from last N samples
 */
static float get_history_max(const history_t* h, uint32_t samples) {
    if (h->count == 0) return 0;
    if (samples > h->count) samples = h->count;
    
    float max_val = h->samples[(h->write_index + HISTORY_SIZE - 1) % HISTORY_SIZE];
    uint32_t idx = (h->write_index + HISTORY_SIZE - 1) % HISTORY_SIZE;
    for (uint32_t i = 0; i < samples; i++) {
        if (h->samples[idx] > max_val) max_val = h->samples[idx];
        idx = (idx + HISTORY_SIZE - 1) % HISTORY_SIZE;
    }
    return max_val;
}

/*===========================================================================*/
/* Event Logging                                                              */
/*===========================================================================*/

/**
 * @brief Log an event to the event buffer
 */
static void rocket_log_event(uint8_t engine_id, const char* msg, float value) {
    if (g_rocket.event_count >= EVENT_LOG_SIZE) return;
    
    event_t* e = &g_rocket.events[g_rocket.event_count++];
    e->event_id = g_rocket.next_event_id++;
    e->timestamp_ns = rocket_get_time_ns();
    e->message = msg;
    e->engine_id = engine_id;
    e->value = value;
}

/*===========================================================================*/
/* Alarm Management                                                           */
/*===========================================================================*/

/**
 * @brief Set or update an alarm
 */
static void rocket_set_alarm(abort_type_t type, alarm_severity_t severity,
                             uint8_t engine_id, float value, float threshold) {
    /* Check if alarm already exists */
    for (uint8_t i = 0; i < g_rocket.alarm_count; i++) {
        if (g_rocket.alarms[i].type == type && 
            g_rocket.alarms[i].engine_id == engine_id) {
            g_rocket.alarms[i].value = value;
            g_rocket.alarms[i].severity = severity;
            return;
        }
    }
    
    /* Add new alarm */
    if (g_rocket.alarm_count >= ALARM_MAX) return;
    
    alarm_t* a = &g_rocket.alarms[g_rocket.alarm_count++];
    a->alarm_id = g_rocket.next_alarm_id++;
    a->type = type;
    a->severity = severity;
    a->engine_id = engine_id;
    a->timestamp_ns = rocket_get_time_ns();
    a->value = value;
    a->threshold = threshold;
    a->active = true;
    a->acknowledged = false;
}

/**
 * @brief Clear an alarm
 */
static void rocket_clear_alarm(abort_type_t type, uint8_t engine_id) {
    for (uint8_t i = 0; i < g_rocket.alarm_count; i++) {
        if (g_rocket.alarms[i].type == type && 
            g_rocket.alarms[i].engine_id == engine_id) {
            g_rocket.alarms[i].active = false;
        }
    }
}

/*===========================================================================*/
/* Triple Modular Redundancy (TMR) Voting                                    */
/*===========================================================================*/

/**
 * @brief Perform TMR voting on redundant sensor channels
 * @param values Array of channel values
 * @param valid Array of channel validity flags
 * @param count Number of channels
 * @param result Output voted result
 * @return true if voting succeeded
 */
static bool tmr_vote(const float values[], const bool valid[], uint8_t count,
                     tmr_sensor_t* result) {
    if (count != TMR_CHANNELS) return false;
    
    result->valid_channels = 0;
    for (uint8_t i = 0; i < TMR_CHANNELS; i++) {
        result->channel_values[i] = values[i];
        if (valid[i]) result->valid_channels++;
    }
    
    /* Need at least 2 valid channels */
    if (result->valid_channels < 2) {
        result->failed = true;
        result->agreement = false;
        return false;
    }
    
    result->failed = false;
    
    /* Check agreement between valid channels */
    float valid_values[TMR_CHANNELS];
    uint8_t valid_count = 0;
    for (uint8_t i = 0; i < TMR_CHANNELS; i++) {
        if (valid[i]) valid_values[valid_count++] = values[i];
    }
    
    /* Calculate mean of valid channels */
    float sum = 0;
    for (uint8_t i = 0; i < valid_count; i++) {
        sum += valid_values[i];
    }
    float mean = sum / valid_count;
    
    /* Check if all valid channels agree within tolerance */
    result->agreement = true;
    for (uint8_t i = 0; i < valid_count; i++) {
        float deviation = fabsf(valid_values[i] - mean);
        if (mean != 0 && (deviation / fabsf(mean)) > SENSOR_AGREEMENT_TOLERANCE) {
            result->agreement = false;
            break;
        }
    }
    
    /* Use median of three or mean of two */
    if (valid_count == 3 && result->agreement) {
        /* Sort and take median */
        float sorted[3];
        memcpy(sorted, valid_values, sizeof(sorted));
        for (int i = 0; i < 2; i++) {
            for (int j = i + 1; j < 3; j++) {
                if (sorted[i] > sorted[j]) {
                    float tmp = sorted[i];
                    sorted[i] = sorted[j];
                    sorted[j] = tmp;
                }
            }
        }
        result->voted_value = sorted[1];  /* Median */
    } else {
        result->voted_value = mean;
    }
    
    return true;
}

/*===========================================================================*/
/* Sensor Processing                                                          */
/*===========================================================================*/

/**
 * @brief Process TMR sensors for an engine
 */
static void process_engine_sensors(uint8_t engine_id) {
    engine_config_t* config = &g_rocket.engines[engine_id];
    engine_status_t* status = &g_rocket.engine_status[engine_id];
    
    /* For each sensor type, collect readings from all channels and vote */
    for (sensor_type_t type = 0; type < SENSOR_TYPE_COUNT; type++) {
        float values[TMR_CHANNELS] = {0};
        bool valid[TMR_CHANNELS] = {false};
        
        /* Find sensor readings for this engine and type */
        for (uint16_t i = 0; i < g_rocket.sensor_count; i++) {
            sensor_reading_t* s = &g_rocket.sensors[i];
            if (s->sensor_id / MAX_SENSORS_PER_ENGINE == engine_id &&
                s->type == type && s->channel < TMR_CHANNELS) {
                values[s->channel] = s->value;
                valid[s->channel] = s->valid;
            }
        }
        
        /* Perform TMR voting */
        tmr_sensor_t* tmr = &g_rocket.tmr_sensors[engine_id][type];
        tmr->type = type;
        tmr_vote(values, valid, TMR_CHANNELS, tmr);
    }
    
    /* Update engine status from voted values */
    status->thrust_kn = g_rocket.tmr_sensors[engine_id][SENSOR_THRUST_MAIN].voted_value;
    status->thrust_pct = (config->nominal_thrust_kn > 0) ? 
                         (status->thrust_kn / config->nominal_thrust_kn) * 100.0f : 0;
    status->chamber_pressure_psi = g_rocket.tmr_sensors[engine_id][SENSOR_CHAMBER_PRESSURE].voted_value;
    status->chamber_temp_c = g_rocket.tmr_sensors[engine_id][SENSOR_CHAMBER_TEMP].voted_value;
    status->nozzle_temp_c = g_rocket.tmr_sensors[engine_id][SENSOR_NOZZLE_TEMP].voted_value;
    status->turbopump_temp_c = g_rocket.tmr_sensors[engine_id][SENSOR_TURBOPUMP_TEMP].voted_value;
    status->oxidizer_flow_kg_s = g_rocket.tmr_sensors[engine_id][SENSOR_OXIDIZER_FLOW].voted_value;
    status->fuel_flow_kg_s = g_rocket.tmr_sensors[engine_id][SENSOR_FUEL_FLOW].voted_value;
    status->vibration_g = g_rocket.tmr_sensors[engine_id][SENSOR_VIBRATION_AXIAL].voted_value;
    
    /* Calculate derived values */
    if (status->fuel_flow_kg_s > 0) {
        status->mixture_ratio = status->oxidizer_flow_kg_s / status->fuel_flow_kg_s;
    }
    
    float injector_p = g_rocket.tmr_sensors[engine_id][SENSOR_INJECTOR_PRESSURE].voted_value;
    if (status->chamber_pressure_psi > 0) {
        status->injector_dp_pct = ((injector_p - status->chamber_pressure_psi) / 
                                   status->chamber_pressure_psi) * 100.0f;
    }
    
    /* Specific impulse calculation */
    float total_flow = status->oxidizer_flow_kg_s + status->fuel_flow_kg_s;
    if (total_flow > 0) {
        status->isp_s = (status->thrust_kn * 1000.0f) / (total_flow * 9.81f);
    }
    
    /* Update history buffers */
    update_history(&g_rocket.thrust_history[engine_id], status->thrust_pct);
    update_history(&g_rocket.chamber_temp_history[engine_id], status->chamber_temp_c);
    update_history(&g_rocket.vibration_history[engine_id], status->vibration_g);
}

/*===========================================================================*/
/* Safety Interlock Processing                                                */
/*===========================================================================*/

/**
 * @brief Check thrust anomaly conditions
 */
static abort_type_t check_thrust_safety(uint8_t engine_id) {
    engine_status_t* status = &g_rocket.engine_status[engine_id];
    
    if (status->state != ENGINE_STATE_MAINSTAGE &&
        status->state != ENGINE_STATE_THROTTLE) {
        return ABORT_NONE;
    }
    
    /* Low thrust check */
    if (status->thrust_pct < THRUST_NOMINAL_MIN_PCT) {
        rocket_set_alarm(ABORT_THRUST_LOW, SEVERITY_ABORT, engine_id,
                        status->thrust_pct, THRUST_NOMINAL_MIN_PCT);
        return ABORT_THRUST_LOW;
    }
    rocket_clear_alarm(ABORT_THRUST_LOW, engine_id);
    
    /* High thrust check */
    if (status->thrust_pct > THRUST_NOMINAL_MAX_PCT) {
        rocket_set_alarm(ABORT_THRUST_HIGH, SEVERITY_ABORT, engine_id,
                        status->thrust_pct, THRUST_NOMINAL_MAX_PCT);
        return ABORT_THRUST_HIGH;
    }
    rocket_clear_alarm(ABORT_THRUST_HIGH, engine_id);
    
    return ABORT_NONE;
}

/**
 * @brief Check inter-engine thrust asymmetry
 */
static abort_type_t check_thrust_asymmetry(void) {
    if (g_rocket.vehicle.engine_count < 2) return ABORT_NONE;
    
    /* Calculate average thrust of running engines */
    float sum = 0;
    uint8_t running = 0;
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        if (g_rocket.engine_status[i].state == ENGINE_STATE_MAINSTAGE) {
            sum += g_rocket.engine_status[i].thrust_pct;
            running++;
        }
    }
    
    if (running < 2) return ABORT_NONE;
    
    float avg = sum / running;
    
    /* Check each engine against average */
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        if (g_rocket.engine_status[i].state == ENGINE_STATE_MAINSTAGE) {
            float deviation = fabsf(g_rocket.engine_status[i].thrust_pct - avg);
            if (deviation > THRUST_DEVIATION_ABORT_PCT) {
                rocket_set_alarm(ABORT_THRUST_ASYMMETRY, SEVERITY_ABORT, i,
                                deviation, THRUST_DEVIATION_ABORT_PCT);
                return ABORT_THRUST_ASYMMETRY;
            }
        }
    }
    
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        rocket_clear_alarm(ABORT_THRUST_ASYMMETRY, i);
    }
    
    return ABORT_NONE;
}

/**
 * @brief Check thermal safety conditions
 */
static abort_type_t check_thermal_safety(uint8_t engine_id) {
    engine_status_t* status = &g_rocket.engine_status[engine_id];
    
    if (status->state == ENGINE_STATE_OFF) return ABORT_NONE;
    
    /* Chamber temperature check */
    if (status->chamber_temp_c > CHAMBER_TEMP_ABORT_C) {
        rocket_set_alarm(ABORT_CHAMBER_OVERTEMP, SEVERITY_ABORT, engine_id,
                        status->chamber_temp_c, CHAMBER_TEMP_ABORT_C);
        return ABORT_CHAMBER_OVERTEMP;
    } else if (status->chamber_temp_c > CHAMBER_TEMP_MAX_C) {
        rocket_set_alarm(ABORT_CHAMBER_OVERTEMP, SEVERITY_WARNING, engine_id,
                        status->chamber_temp_c, CHAMBER_TEMP_MAX_C);
    } else {
        rocket_clear_alarm(ABORT_CHAMBER_OVERTEMP, engine_id);
    }
    
    /* Turbopump temperature check */
    if (status->turbopump_temp_c > TURBOPUMP_TEMP_MAX_C) {
        rocket_set_alarm(ABORT_TURBOPUMP_OVERTEMP, SEVERITY_ABORT, engine_id,
                        status->turbopump_temp_c, TURBOPUMP_TEMP_MAX_C);
        return ABORT_TURBOPUMP_OVERTEMP;
    }
    rocket_clear_alarm(ABORT_TURBOPUMP_OVERTEMP, engine_id);
    
    return ABORT_NONE;
}

/**
 * @brief Check pressure safety conditions
 */
static abort_type_t check_pressure_safety(uint8_t engine_id) {
    engine_status_t* status = &g_rocket.engine_status[engine_id];
    
    if (status->state == ENGINE_STATE_OFF) return ABORT_NONE;
    
    /* Chamber overpressure check */
    if (status->chamber_pressure_psi > CHAMBER_PRESSURE_ABORT_PSI) {
        rocket_set_alarm(ABORT_CHAMBER_OVERPRESSURE, SEVERITY_ABORT, engine_id,
                        status->chamber_pressure_psi, CHAMBER_PRESSURE_ABORT_PSI);
        return ABORT_CHAMBER_OVERPRESSURE;
    } else if (status->chamber_pressure_psi > CHAMBER_PRESSURE_MAX_PSI) {
        rocket_set_alarm(ABORT_CHAMBER_OVERPRESSURE, SEVERITY_WARNING, engine_id,
                        status->chamber_pressure_psi, CHAMBER_PRESSURE_MAX_PSI);
    } else {
        rocket_clear_alarm(ABORT_CHAMBER_OVERPRESSURE, engine_id);
    }
    
    return ABORT_NONE;
}

/**
 * @brief Check vibration safety conditions
 */
static abort_type_t check_vibration_safety(uint8_t engine_id) {
    engine_status_t* status = &g_rocket.engine_status[engine_id];
    
    if (status->state == ENGINE_STATE_OFF) return ABORT_NONE;
    
    /* Check max vibration over recent history */
    float max_vib = get_history_max(&g_rocket.vibration_history[engine_id], 10);
    
    if (max_vib > VIBRATION_G_ABORT) {
        rocket_set_alarm(ABORT_VIBRATION_EXCESSIVE, SEVERITY_ABORT, engine_id,
                        max_vib, VIBRATION_G_ABORT);
        return ABORT_VIBRATION_EXCESSIVE;
    } else if (max_vib > VIBRATION_G_LIMIT) {
        rocket_set_alarm(ABORT_VIBRATION_EXCESSIVE, SEVERITY_WARNING, engine_id,
                        max_vib, VIBRATION_G_LIMIT);
    } else {
        rocket_clear_alarm(ABORT_VIBRATION_EXCESSIVE, engine_id);
    }
    
    return ABORT_NONE;
}

/**
 * @brief Check sensor health for critical failures
 */
static abort_type_t check_sensor_health(uint8_t engine_id) {
    /* Check for failed TMR sensors on critical parameters */
    sensor_type_t critical[] = {SENSOR_THRUST_MAIN, SENSOR_CHAMBER_PRESSURE,
                                SENSOR_CHAMBER_TEMP};
    
    for (size_t i = 0; i < sizeof(critical)/sizeof(critical[0]); i++) {
        tmr_sensor_t* tmr = &g_rocket.tmr_sensors[engine_id][critical[i]];
        if (tmr->failed) {
            rocket_set_alarm(ABORT_SENSOR_FAILURE, SEVERITY_ABORT, engine_id,
                            (float)critical[i], 0);
            return ABORT_SENSOR_FAILURE;
        }
    }
    
    rocket_clear_alarm(ABORT_SENSOR_FAILURE, engine_id);
    return ABORT_NONE;
}

/**
 * @brief Execute abort sequence
 */
static void execute_abort(abort_type_t reason, uint8_t engine_id) {
    uint64_t abort_start = rocket_get_time_ns();
    
    g_rocket.safety.abort_triggered = true;
    g_rocket.safety.abort_reason = reason;
    g_rocket.safety.abort_engine_id = engine_id;
    g_rocket.safety.abort_timestamp_ns = abort_start;
    g_rocket.phase = PHASE_ABORT;
    g_rocket.abort_count++;
    
    /* Command all engines to abort */
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        g_rocket.engine_status[i].state = ENGINE_STATE_ABORT;
        g_rocket.engine_status[i].thrust_kn = 0;
        g_rocket.engine_status[i].thrust_pct = 0;
    }
    
    /* Calculate abort latency */
    uint64_t abort_end = rocket_get_time_ns();
    g_rocket.safety.abort_latency_us = (uint32_t)((abort_end - abort_start) / 1000);
    
    rocket_log_event(engine_id, "ABORT TRIGGERED", (float)reason);
}

/**
 * @brief Main safety check processing
 */
static abort_type_t safety_check(void) {
    if (!g_rocket.safety.armed) return ABORT_NONE;
    if (g_rocket.safety.abort_triggered) return g_rocket.safety.abort_reason;
    
    g_rocket.safety_checks++;
    g_rocket.last_safety_check_ns = rocket_get_time_ns();
    
    abort_type_t result = ABORT_NONE;
    
    /* Check each engine */
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        /* Thrust checks */
        result = check_thrust_safety(i);
        if (result != ABORT_NONE) {
            execute_abort(result, i);
            return result;
        }
        
        /* Thermal checks */
        result = check_thermal_safety(i);
        if (result != ABORT_NONE) {
            execute_abort(result, i);
            return result;
        }
        
        /* Pressure checks */
        result = check_pressure_safety(i);
        if (result != ABORT_NONE) {
            execute_abort(result, i);
            return result;
        }
        
        /* Vibration checks */
        result = check_vibration_safety(i);
        if (result != ABORT_NONE) {
            execute_abort(result, i);
            return result;
        }
        
        /* Sensor health checks */
        result = check_sensor_health(i);
        if (result != ABORT_NONE) {
            execute_abort(result, i);
            return result;
        }
    }
    
    /* Vehicle-level checks */
    result = check_thrust_asymmetry();
    if (result != ABORT_NONE) {
        execute_abort(result, 0);
        return result;
    }
    
    return ABORT_NONE;
}

/*===========================================================================*/
/* Engine State Machine                                                       */
/*===========================================================================*/

/**
 * @brief Process engine state machine
 */
static void engine_process(uint8_t engine_id, uint32_t delta_ms) {
    engine_status_t* status = &g_rocket.engine_status[engine_id];
    engine_config_t* config = &g_rocket.engines[engine_id];
    
    switch (status->state) {
        case ENGINE_STATE_OFF:
            status->thrust_kn = 0;
            status->thrust_pct = 0;
            status->run_time_ms = 0;
            break;
            
        case ENGINE_STATE_PRESTART:
            /* Pre-ignition valve sequencing */
            /* Simulate transition to ignition after checks */
            status->state = ENGINE_STATE_IGNITION;
            rocket_log_event(engine_id, "Ignition sequence start", 0);
            break;
            
        case ENGINE_STATE_IGNITION:
            /* Igniter firing, engine ramping up */
            if (status->thrust_pct >= 90.0f) {
                status->state = ENGINE_STATE_MAINSTAGE;
                rocket_log_event(engine_id, "Mainstage achieved", status->thrust_pct);
            }
            break;
            
        case ENGINE_STATE_MAINSTAGE:
            status->run_time_ms += delta_ms;
            g_rocket.total_run_time_ms += delta_ms;
            
            /* Check health based on all parameters */
            status->healthy = (status->thrust_pct >= THRUST_NOMINAL_MIN_PCT &&
                              status->thrust_pct <= THRUST_NOMINAL_MAX_PCT &&
                              status->chamber_temp_c < CHAMBER_TEMP_MAX_C &&
                              status->vibration_g < VIBRATION_G_LIMIT);
            break;
            
        case ENGINE_STATE_THROTTLE:
            status->run_time_ms += delta_ms;
            g_rocket.total_run_time_ms += delta_ms;
            break;
            
        case ENGINE_STATE_SHUTDOWN:
            /* Controlled shutdown sequence */
            if (status->thrust_pct <= 5.0f) {
                status->state = ENGINE_STATE_OFF;
                rocket_log_event(engine_id, "Engine shutdown complete", 0);
            }
            break;
            
        case ENGINE_STATE_ABORT:
            /* Emergency shutdown - immediate */
            status->thrust_kn = 0;
            status->thrust_pct = 0;
            status->healthy = false;
            break;
            
        case ENGINE_STATE_FAULT:
            status->healthy = false;
            break;
    }
}

/*===========================================================================*/
/* Telemetry Generation                                                       */
/*===========================================================================*/

/**
 * @brief Generate a telemetry frame
 */
static int generate_telemetry_frame(uint8_t* buffer, size_t buffer_len) {
    if (buffer_len < TLM_FRAME_SIZE) return -1;
    
    memset(buffer, 0, TLM_FRAME_SIZE);
    
    /* Frame header */
    tlm_frame_header_t* header = (tlm_frame_header_t*)buffer;
    header->version = 1;
    header->spacecraft_id = g_rocket.vehicle.vehicle_id;
    header->virtual_channel = 0;
    header->frame_count = g_rocket.tlm_frame_count++;
    header->mission_time_ns = g_rocket.mission_time_ns;
    header->frame_type = 0x01;  /* Engine telemetry */
    header->apid = 100;
    
    /* Engine data section */
    size_t offset = sizeof(tlm_frame_header_t);
    
    /* Launch phase */
    buffer[offset++] = (uint8_t)g_rocket.phase;
    
    /* Safety status */
    buffer[offset++] = g_rocket.safety.armed ? 0x01 : 0x00;
    buffer[offset++] = g_rocket.safety.abort_triggered ? 0x01 : 0x00;
    buffer[offset++] = (uint8_t)g_rocket.safety.abort_reason;
    
    /* Engine count */
    buffer[offset++] = g_rocket.vehicle.engine_count;
    
    /* Per-engine data */
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count && 
                        offset + 32 < TLM_FRAME_SIZE; i++) {
        engine_status_t* eng = &g_rocket.engine_status[i];
        
        buffer[offset++] = eng->engine_id;
        buffer[offset++] = (uint8_t)eng->state;
        
        /* Thrust (2 bytes, 0.1 kN resolution) */
        uint16_t thrust_raw = (uint16_t)(eng->thrust_kn * 10);
        buffer[offset++] = thrust_raw >> 8;
        buffer[offset++] = thrust_raw & 0xFF;
        
        /* Chamber pressure (2 bytes, PSI) */
        uint16_t pressure_raw = (uint16_t)eng->chamber_pressure_psi;
        buffer[offset++] = pressure_raw >> 8;
        buffer[offset++] = pressure_raw & 0xFF;
        
        /* Chamber temperature (2 bytes, °C) */
        int16_t temp_raw = (int16_t)eng->chamber_temp_c;
        buffer[offset++] = (uint8_t)(temp_raw >> 8);
        buffer[offset++] = (uint8_t)(temp_raw & 0xFF);
        
        /* Vibration (1 byte, 0.1 g resolution) */
        buffer[offset++] = (uint8_t)(eng->vibration_g * 10);
        
        /* Health flags */
        buffer[offset++] = eng->healthy ? 0x01 : 0x00;
    }
    
    g_rocket.tlm_stats.frames_generated++;
    
    return (int)offset;
}

/*===========================================================================*/
/* Public API                                                                 */
/*===========================================================================*/

/**
 * @brief Initialize rocket telemetry system
 */
int rocket_init(void) {
    if (g_rocket.initialized) return -1;
    
    memset(&g_rocket, 0, sizeof(g_rocket));
    g_rocket.initialized = true;
    g_rocket.next_alarm_id = 1;
    g_rocket.next_event_id = 1;
    g_rocket.phase = PHASE_PRELAUNCH;
    
    /* Default TMR health */
    for (uint8_t i = 0; i < TMR_CHANNELS; i++) {
        g_rocket.safety.tmr_health[i] = 1;
    }
    g_rocket.safety.all_channels_healthy = true;
    
    rocket_log_event(0xFF, "Rocket system initialized", 0);
    
    return 0;
}

/**
 * @brief Configure vehicle
 */
int rocket_configure_vehicle(uint16_t vehicle_id, const char* name,
                             uint8_t engine_count, bool human_rated) {
    if (!g_rocket.initialized) return -1;
    if (engine_count > MAX_ENGINES) return -2;
    
    g_rocket.vehicle.vehicle_id = vehicle_id;
    g_rocket.vehicle.vehicle_name = name;
    g_rocket.vehicle.engine_count = engine_count;
    g_rocket.vehicle.human_rated = human_rated;
    
    /* Initialize engine status */
    for (uint8_t i = 0; i < engine_count; i++) {
        g_rocket.engine_status[i].engine_id = i;
        g_rocket.engine_status[i].state = ENGINE_STATE_OFF;
    }
    
    rocket_log_event(0xFF, "Vehicle configured", engine_count);
    
    return 0;
}

/**
 * @brief Configure an engine
 */
int rocket_configure_engine(uint8_t engine_id, const char* name,
                            float nominal_thrust_kn, float max_thrust_kn) {
    if (!g_rocket.initialized) return -1;
    if (engine_id >= g_rocket.vehicle.engine_count) return -2;
    
    engine_config_t* config = &g_rocket.engines[engine_id];
    config->engine_id = engine_id;
    config->engine_name = name;
    config->nominal_thrust_kn = nominal_thrust_kn;
    config->max_thrust_kn = max_thrust_kn;
    config->min_throttle_pct = 40.0f;  /* Default 40% throttle minimum */
    
    return 0;
}

/**
 * @brief Arm safety system
 */
int rocket_safety_arm(void) {
    if (!g_rocket.initialized) return -1;
    if (g_rocket.phase != PHASE_PRELAUNCH && g_rocket.phase != PHASE_TERMINAL_COUNT) {
        return -2;
    }
    
    g_rocket.safety.armed = true;
    g_rocket.safety.abort_triggered = false;
    g_rocket.safety.abort_reason = ABORT_NONE;
    g_rocket.safety_armed = true;
    
    rocket_log_event(0xFF, "Safety system ARMED", 0);
    
    return 0;
}

/**
 * @brief Get safety system status
 */
int rocket_get_safety_status(safety_status_t* status) {
    if (!g_rocket.initialized || !status) return -1;
    
    memcpy(status, &g_rocket.safety, sizeof(safety_status_t));
    g_rocket.safety.safety_checks_per_sec = g_rocket.safety_checks;
    g_rocket.safety_checks = 0;
    
    return 0;
}

/**
 * @brief Get engine status
 */
int rocket_get_engine_status(uint8_t engine_id, engine_status_t* status) {
    if (!g_rocket.initialized || !status) return -1;
    if (engine_id >= g_rocket.vehicle.engine_count) return -2;
    
    memcpy(status, &g_rocket.engine_status[engine_id], sizeof(engine_status_t));
    return 0;
}

/**
 * @brief Get launch phase
 */
launch_phase_t rocket_get_phase(void) {
    return g_rocket.phase;
}

/**
 * @brief Start ignition sequence
 */
int rocket_start_ignition(void) {
    if (!g_rocket.initialized) return -1;
    if (!g_rocket.safety.armed) return -2;
    if (g_rocket.phase != PHASE_TERMINAL_COUNT) return -3;
    
    g_rocket.phase = PHASE_IGNITION_SEQUENCE;
    
    /* Start all engines */
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        g_rocket.engine_status[i].state = ENGINE_STATE_PRESTART;
    }
    
    g_rocket.ignition_count++;
    rocket_log_event(0xFF, "Ignition sequence commanded", 0);
    
    return 0;
}

/**
 * @brief Trigger manual abort
 */
int rocket_abort(abort_type_t reason) {
    if (!g_rocket.initialized) return -1;
    
    execute_abort(reason != ABORT_NONE ? reason : ABORT_MANUAL, 0xFF);
    
    return 0;
}

/**
 * @brief Set launch phase
 */
int rocket_set_phase(launch_phase_t phase) {
    if (!g_rocket.initialized) return -1;
    if (g_rocket.safety.abort_triggered) return -2;
    
    launch_phase_t old_phase = g_rocket.phase;
    g_rocket.phase = phase;
    
    if (phase == PHASE_LIFTOFF && old_phase != PHASE_LIFTOFF) {
        g_rocket.liftoff_time_ns = rocket_get_time_ns();
        g_rocket.t_minus_ms = 0;
        rocket_log_event(0xFF, "LIFTOFF", 0);
    }
    
    return 0;
}

/**
 * @brief Inject sensor data for testing
 */
int rocket_inject_sensor(uint8_t engine_id, sensor_type_t type,
                         uint8_t channel, float value) {
    if (!g_rocket.initialized) return -1;
    if (engine_id >= g_rocket.vehicle.engine_count) return -2;
    if (channel >= TMR_CHANNELS) return -3;
    
    /* Find or create sensor entry */
    uint16_t sensor_id = engine_id * MAX_SENSORS_PER_ENGINE + type * TMR_CHANNELS + channel;
    
    for (uint16_t i = 0; i < g_rocket.sensor_count; i++) {
        if (g_rocket.sensors[i].sensor_id == sensor_id) {
            g_rocket.sensors[i].value = value;
            g_rocket.sensors[i].timestamp_ns = rocket_get_time_ns();
            g_rocket.sensors[i].valid = true;
            return 0;
        }
    }
    
    /* Add new sensor */
    if (g_rocket.sensor_count >= TOTAL_MAX_SENSORS) return -4;
    
    sensor_reading_t* s = &g_rocket.sensors[g_rocket.sensor_count++];
    s->sensor_id = sensor_id;
    s->type = type;
    s->channel = channel;
    s->value = value;
    s->timestamp_ns = rocket_get_time_ns();
    s->valid = true;
    s->in_range = true;
    
    return 0;
}

/**
 * @brief Inject complete engine state for testing
 */
int rocket_inject_engine_state(uint8_t engine_id, engine_state_t state,
                               float thrust_pct, float chamber_temp_c,
                               float vibration_g) {
    if (!g_rocket.initialized) return -1;
    if (engine_id >= g_rocket.vehicle.engine_count) return -2;
    
    engine_config_t* config = &g_rocket.engines[engine_id];
    float thrust_kn = (config->nominal_thrust_kn * thrust_pct) / 100.0f;
    
    /* Inject all TMR channels with same value for testing */
    for (uint8_t ch = 0; ch < TMR_CHANNELS; ch++) {
        rocket_inject_sensor(engine_id, SENSOR_THRUST_MAIN, ch, thrust_kn);
        rocket_inject_sensor(engine_id, SENSOR_CHAMBER_TEMP, ch, chamber_temp_c);
        rocket_inject_sensor(engine_id, SENSOR_VIBRATION_AXIAL, ch, vibration_g);
        rocket_inject_sensor(engine_id, SENSOR_CHAMBER_PRESSURE, ch, 3000.0f);
    }
    
    g_rocket.engine_status[engine_id].state = state;
    
    return 0;
}

/**
 * @brief Get telemetry statistics
 */
int rocket_get_tlm_stats(tlm_stats_t* stats) {
    if (!g_rocket.initialized || !stats) return -1;
    
    memcpy(stats, &g_rocket.tlm_stats, sizeof(tlm_stats_t));
    return 0;
}

/**
 * @brief Generate telemetry frame
 */
int rocket_generate_telemetry(void* buffer, size_t buffer_len, size_t* frame_len) {
    if (!g_rocket.initialized || !buffer || !frame_len) return -1;
    if (buffer_len < TLM_FRAME_SIZE) return -2;
    
    int len = generate_telemetry_frame(buffer, buffer_len);
    if (len < 0) return len;
    
    *frame_len = (size_t)len;
    return 0;
}

/**
 * @brief Get active alarms
 */
int rocket_get_alarms(alarm_t* alarms, uint8_t max_alarms, uint8_t* count) {
    if (!g_rocket.initialized || !alarms || !count) return -1;
    
    uint8_t n = 0;
    for (uint8_t i = 0; i < g_rocket.alarm_count && n < max_alarms; i++) {
        if (g_rocket.alarms[i].active) {
            memcpy(&alarms[n++], &g_rocket.alarms[i], sizeof(alarm_t));
        }
    }
    *count = n;
    
    return 0;
}

/**
 * @brief Get recent events
 */
int rocket_get_events(event_t* events, uint16_t max_events, uint16_t* count) {
    if (!g_rocket.initialized || !events || !count) return -1;
    
    uint16_t n = (g_rocket.event_count < max_events) ? 
                  g_rocket.event_count : max_events;
    
    for (uint16_t i = 0; i < n; i++) {
        memcpy(&events[i], &g_rocket.events[g_rocket.event_count - n + i], sizeof(event_t));
    }
    *count = n;
    
    return 0;
}

/**
 * @brief Check launch readiness
 */
int rocket_check_readiness(bool* ready, char* reason, size_t reason_len) {
    if (!g_rocket.initialized || !ready) return -1;
    
    *ready = true;
    
    /* Check safety armed */
    if (!g_rocket.safety.armed) {
        *ready = false;
        if (reason && reason_len > 0) {
            snprintf(reason, reason_len, "Safety not armed");
        }
        return 0;
    }
    
    /* Check all engines healthy */
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        engine_status_t* eng = &g_rocket.engine_status[i];
        
        /* Check TMR sensor health */
        for (sensor_type_t t = 0; t < SENSOR_TYPE_COUNT; t++) {
            if (g_rocket.tmr_sensors[i][t].failed) {
                *ready = false;
                if (reason && reason_len > 0) {
                    snprintf(reason, reason_len, "Engine %d sensor %d failed", i, t);
                }
                return 0;
            }
        }
    }
    
    /* Check for active abort-level alarms */
    for (uint8_t i = 0; i < g_rocket.alarm_count; i++) {
        if (g_rocket.alarms[i].active && 
            g_rocket.alarms[i].severity >= SEVERITY_ABORT) {
            *ready = false;
            if (reason && reason_len > 0) {
                snprintf(reason, reason_len, "Active abort alarm: type %d", 
                        g_rocket.alarms[i].type);
            }
            return 0;
        }
    }
    
    if (reason && reason_len > 0) {
        snprintf(reason, reason_len, "GO for launch");
    }
    
    return 0;
}

/**
 * @brief Main processing function (call periodically)
 */
int rocket_process(uint32_t delta_ms) {
    if (!g_rocket.initialized) return -1;
    
    g_rocket.current_time_ns += (uint64_t)delta_ms * 1000000ULL;
    g_rocket.process_count++;
    
    /* Update mission time */
    if (g_rocket.phase >= PHASE_LIFTOFF && g_rocket.phase < PHASE_ABORT) {
        g_rocket.mission_time_ns = g_rocket.current_time_ns - g_rocket.liftoff_time_ns;
    }
    
    /* Update countdown if pre-liftoff */
    if (g_rocket.phase < PHASE_LIFTOFF) {
        g_rocket.t_minus_ms -= delta_ms;
    }
    
    /* Process sensors for all engines */
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        process_engine_sensors(i);
    }
    
    /* Run safety checks */
    safety_check();
    
    /* Process engine state machines */
    for (uint8_t i = 0; i < g_rocket.vehicle.engine_count; i++) {
        engine_process(i, delta_ms);
    }
    
    return 0;
}

/**
 * @brief Shutdown rocket system
 */
int rocket_shutdown(void) {
    if (!g_rocket.initialized) return -1;
    
    rocket_log_event(0xFF, "Rocket system shutdown", 0);
    g_rocket.initialized = false;
    
    return 0;
}
