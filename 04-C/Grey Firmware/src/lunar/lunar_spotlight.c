/**
 * @file lunar_spotlight.c
 * @brief Regolith Extraction & Yield Telemetry Spotlight
 * 
 * WHAT: Comprehensive lunar mining control system demonstrating embedded 
 *       firmware for ISRU (In-Situ Resource Utilization) operations.
 * 
 * WHY: Lunar mining enables sustainable space exploration by extracting:
 *      - Water ice for propellant (H2/O2)
 *      - Oxygen for life support
 *      - Metals (iron, aluminum, titanium) for construction
 *      - Helium-3 for future fusion power
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - TMR (Triple Modular Redundancy) sensor voting
 *      - Drill control loops with torque/depth limits
 *      - Thermal management for vacuum/radiation environment
 *      - Safety interlocks for mechanical faults
 *      - CCSDS-compatible telemetry formatting
 *      - Variable regolith density adaptation
 * 
 * Industry applications: NASA Artemis, CLPS missions, lunar habitats
 * 
 * @author Grey Firmware Project
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define LUNAR_MAX_DRILLS            4
#define LUNAR_MAX_SENSORS           16
#define LUNAR_TMR_CHANNELS          3
#define LUNAR_TELEMETRY_BUFFER_SIZE 256

/* Drill limits */
#define DRILL_MAX_RPM               300
#define DRILL_MIN_RPM               10
#define DRILL_MAX_TORQUE_NM         150.0f
#define DRILL_TORQUE_WARNING_NM     120.0f
#define DRILL_MAX_DEPTH_M           3.0f
#define DRILL_FEED_RATE_MAX_MMS     5.0f

/* Thermal limits (lunar surface: -173°C to +127°C) */
#define DRILL_TEMP_MIN_C            (-100.0f)
#define DRILL_TEMP_MAX_C            150.0f
#define DRILL_TEMP_WARNING_C        120.0f
#define MOTOR_TEMP_CRITICAL_C       85.0f

/* Regolith characteristics */
#define REGOLITH_DENSITY_MIN        1.2f    /* g/cm³ - loose */
#define REGOLITH_DENSITY_MAX        2.0f    /* g/cm³ - compacted */
#define REGOLITH_ICE_THRESHOLD_PCT  5.0f    /* Water ice content */

/* TMR voting */
#define TMR_DISAGREEMENT_THRESHOLD  0.05f   /* 5% max deviation */
#define TMR_SENSOR_TIMEOUT_MS       1000

/* Safety timeouts */
#define DRILL_STALL_TIMEOUT_MS      3000
#define THERMAL_CHECK_INTERVAL_MS   500
#define TELEMETRY_INTERVAL_MS       1000

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Drill operating state */
typedef enum {
    DRILL_STATE_IDLE,
    DRILL_STATE_STARTING,
    DRILL_STATE_DRILLING,
    DRILL_STATE_RETRACTING,
    DRILL_STATE_FAULT,
    DRILL_STATE_EMERGENCY_STOP,
    DRILL_STATE_MAINTENANCE
} drill_state_t;

/** Drill fault codes */
typedef enum {
    DRILL_FAULT_NONE = 0,
    DRILL_FAULT_OVERCURRENT,
    DRILL_FAULT_OVERTEMP,
    DRILL_FAULT_STALL,
    DRILL_FAULT_OVERTORQUE,
    DRILL_FAULT_DEPTH_LIMIT,
    DRILL_FAULT_SENSOR_FAIL,
    DRILL_FAULT_AUGER_JAM,
    DRILL_FAULT_MOTOR_FAIL,
    DRILL_FAULT_TMR_DISAGREE,
    DRILL_FAULT_COMM_LOSS
} drill_fault_t;

/** Regolith processing phase */
typedef enum {
    ISRU_PHASE_IDLE,
    ISRU_PHASE_COLLECTING,
    ISRU_PHASE_HEATING,         /* Volatiles extraction */
    ISRU_PHASE_SEPARATING,      /* Material separation */
    ISRU_PHASE_STORING,
    ISRU_PHASE_FAULT
} isru_phase_t;

/** TMR sensor channel status */
typedef enum {
    TMR_CHANNEL_OK,
    TMR_CHANNEL_SUSPECT,
    TMR_CHANNEL_FAILED
} tmr_channel_status_t;

/** TMR voting result */
typedef enum {
    TMR_VOTE_UNANIMOUS,
    TMR_VOTE_MAJORITY,
    TMR_VOTE_DISAGREE,
    TMR_VOTE_ALL_FAILED
} tmr_vote_result_t;

/** Sensor type for TMR */
typedef enum {
    SENSOR_TYPE_TORQUE,
    SENSOR_TYPE_DEPTH,
    SENSOR_TYPE_TEMP,
    SENSOR_TYPE_RPM,
    SENSOR_TYPE_DENSITY,
    SENSOR_TYPE_ICE_CONTENT
} sensor_type_t;

/** TMR sensor reading */
typedef struct {
    float values[LUNAR_TMR_CHANNELS];
    tmr_channel_status_t status[LUNAR_TMR_CHANNELS];
    uint32_t timestamps[LUNAR_TMR_CHANNELS];
    float voted_value;
    tmr_vote_result_t vote_result;
    sensor_type_t type;
} tmr_sensor_t;

/** Drill configuration */
typedef struct {
    uint8_t drill_id;
    uint16_t max_rpm;
    float max_torque_nm;
    float max_depth_m;
    float feed_rate_mms;
    bool auger_enabled;
    bool auto_retract_on_fault;
} drill_config_t;

/** Drill telemetry */
typedef struct {
    uint8_t drill_id;
    drill_state_t state;
    drill_fault_t fault;
    uint16_t current_rpm;
    uint16_t target_rpm;
    float torque_nm;
    float depth_m;
    float feed_rate_mms;
    float motor_temp_c;
    float bit_temp_c;
    uint32_t motor_current_ma;
    uint32_t drill_time_s;
    uint32_t cycles;
    float wear_pct;
} drill_telemetry_t;

/** Regolith sample data */
typedef struct {
    float density_gcm3;
    float moisture_pct;
    float ice_content_pct;
    float iron_pct;
    float titanium_pct;
    float aluminum_pct;
    float particle_size_um;
    float hardness;
} regolith_sample_t;

/** Yield telemetry */
typedef struct {
    float total_mass_kg;
    float water_kg;
    float oxygen_kg;
    float metals_kg;
    float current_rate_kgh;
    float ore_grade_pct;
    uint32_t samples_collected;
    uint32_t runtime_s;
    float efficiency_pct;
} yield_telemetry_t;

/** Safety interlock state */
typedef struct {
    bool drill_enabled;
    bool thermal_ok;
    bool sensors_ok;
    bool comm_ok;
    bool power_ok;
    bool emergency_stop;
    uint32_t last_check_ms;
} safety_interlock_t;

/** ISRU processing status */
typedef struct {
    isru_phase_t phase;
    float hopper_fill_pct;
    float furnace_temp_c;
    float extraction_rate_kgh;
    float solar_power_kw;
    uint32_t batch_id;
} isru_status_t;

/** Complete lunar mining context */
typedef struct {
    /* Drills */
    drill_config_t drill_config[LUNAR_MAX_DRILLS];
    drill_telemetry_t drill_telem[LUNAR_MAX_DRILLS];
    uint8_t active_drills;
    
    /* TMR sensors */
    tmr_sensor_t torque_sensor[LUNAR_MAX_DRILLS];
    tmr_sensor_t depth_sensor[LUNAR_MAX_DRILLS];
    tmr_sensor_t temp_sensor[LUNAR_MAX_DRILLS];
    
    /* Regolith data */
    regolith_sample_t current_sample;
    yield_telemetry_t yield;
    
    /* Safety */
    safety_interlock_t safety;
    
    /* ISRU */
    isru_status_t isru;
    
    /* System state */
    bool initialized;
    uint32_t uptime_s;
    uint32_t fault_count;
    uint32_t telemetry_seq;
} lunar_ctx_t;

/*===========================================================================*/
/* Static Context                                                             */
/*===========================================================================*/

static lunar_ctx_t g_lunar;

/*===========================================================================*/
/* TMR Voting Implementation                                                  */
/*===========================================================================*/

/**
 * @brief Perform TMR voting on sensor values
 */
static tmr_vote_result_t lunar_tmr_vote(tmr_sensor_t *sensor)
{
    int valid_count = 0;
    float valid_sum = 0.0f;
    float valid_values[LUNAR_TMR_CHANNELS];
    
    /* Count valid channels */
    for (int i = 0; i < LUNAR_TMR_CHANNELS; i++) {
        if (sensor->status[i] == TMR_CHANNEL_OK) {
            valid_values[valid_count] = sensor->values[i];
            valid_sum += sensor->values[i];
            valid_count++;
        }
    }
    
    /* All failed */
    if (valid_count == 0) {
        sensor->vote_result = TMR_VOTE_ALL_FAILED;
        sensor->voted_value = 0.0f;
        return TMR_VOTE_ALL_FAILED;
    }
    
    /* Single valid - use it but flag */
    if (valid_count == 1) {
        sensor->voted_value = valid_values[0];
        sensor->vote_result = TMR_VOTE_DISAGREE;  /* Degraded mode */
        return TMR_VOTE_DISAGREE;
    }
    
    /* Two or three valid - check agreement */
    float mean = valid_sum / valid_count;
    float max_dev = 0.0f;
    
    for (int i = 0; i < valid_count; i++) {
        float dev = fabsf(valid_values[i] - mean);
        if (mean > 0.0f) {
            dev = dev / mean;  /* Relative deviation */
        }
        if (dev > max_dev) {
            max_dev = dev;
        }
    }
    
    /* Use median for three, mean for two */
    if (valid_count == 3) {
        /* Sort for median */
        float temp;
        for (int i = 0; i < 2; i++) {
            for (int j = i + 1; j < 3; j++) {
                if (valid_values[i] > valid_values[j]) {
                    temp = valid_values[i];
                    valid_values[i] = valid_values[j];
                    valid_values[j] = temp;
                }
            }
        }
        sensor->voted_value = valid_values[1];  /* Median */
    } else {
        sensor->voted_value = mean;
    }
    
    /* Determine vote quality */
    if (max_dev <= TMR_DISAGREEMENT_THRESHOLD) {
        sensor->vote_result = (valid_count == 3) ? 
                              TMR_VOTE_UNANIMOUS : TMR_VOTE_MAJORITY;
    } else {
        sensor->vote_result = TMR_VOTE_DISAGREE;
    }
    
    return sensor->vote_result;
}

/**
 * @brief Update TMR sensor with new reading
 */
static int lunar_tmr_update(tmr_sensor_t *sensor, uint8_t channel, 
                            float value, uint32_t timestamp)
{
    if (!sensor || channel >= LUNAR_TMR_CHANNELS) {
        return -1;
    }
    
    sensor->values[channel] = value;
    sensor->timestamps[channel] = timestamp;
    sensor->status[channel] = TMR_CHANNEL_OK;
    
    /* Perform voting after update */
    lunar_tmr_vote(sensor);
    
    return 0;
}

/**
 * @brief Check for timeout on TMR channels
 */
static void lunar_tmr_check_timeout(tmr_sensor_t *sensor, uint32_t current_ms)
{
    for (int i = 0; i < LUNAR_TMR_CHANNELS; i++) {
        if (sensor->status[i] == TMR_CHANNEL_OK) {
            uint32_t age = current_ms - sensor->timestamps[i];
            if (age > TMR_SENSOR_TIMEOUT_MS) {
                sensor->status[i] = TMR_CHANNEL_FAILED;
            }
        }
    }
    
    /* Re-vote after timeout check */
    lunar_tmr_vote(sensor);
}

/*===========================================================================*/
/* Drill Control Implementation                                               */
/*===========================================================================*/

/**
 * @brief Initialize drill subsystem
 */
static int lunar_drill_init(uint8_t drill_id, const drill_config_t *config)
{
    if (drill_id >= LUNAR_MAX_DRILLS || !config) {
        return -1;
    }
    
    memcpy(&g_lunar.drill_config[drill_id], config, sizeof(drill_config_t));
    
    /* Initialize telemetry */
    drill_telemetry_t *telem = &g_lunar.drill_telem[drill_id];
    memset(telem, 0, sizeof(drill_telemetry_t));
    telem->drill_id = drill_id;
    telem->state = DRILL_STATE_IDLE;
    telem->fault = DRILL_FAULT_NONE;
    
    /* Initialize TMR sensors */
    memset(&g_lunar.torque_sensor[drill_id], 0, sizeof(tmr_sensor_t));
    memset(&g_lunar.depth_sensor[drill_id], 0, sizeof(tmr_sensor_t));
    memset(&g_lunar.temp_sensor[drill_id], 0, sizeof(tmr_sensor_t));
    
    g_lunar.torque_sensor[drill_id].type = SENSOR_TYPE_TORQUE;
    g_lunar.depth_sensor[drill_id].type = SENSOR_TYPE_DEPTH;
    g_lunar.temp_sensor[drill_id].type = SENSOR_TYPE_TEMP;
    
    if (drill_id >= g_lunar.active_drills) {
        g_lunar.active_drills = drill_id + 1;
    }
    
    return 0;
}

/**
 * @brief Set drill RPM (with ramp limiting)
 */
static int lunar_drill_set_rpm(uint8_t drill_id, uint16_t target_rpm)
{
    if (drill_id >= LUNAR_MAX_DRILLS) {
        return -1;
    }
    
    drill_telemetry_t *telem = &g_lunar.drill_telem[drill_id];
    const drill_config_t *config = &g_lunar.drill_config[drill_id];
    
    /* Check safety interlock */
    if (!g_lunar.safety.drill_enabled) {
        return -2;
    }
    
    /* Limit to configured max */
    if (target_rpm > config->max_rpm) {
        target_rpm = config->max_rpm;
    }
    
    /* Allow setting if idle or already drilling */
    if (telem->state != DRILL_STATE_IDLE && 
        telem->state != DRILL_STATE_DRILLING) {
        return -3;
    }
    
    telem->target_rpm = target_rpm;
    
    /* Start drilling if not already */
    if (telem->state == DRILL_STATE_IDLE && target_rpm > 0) {
        telem->state = DRILL_STATE_STARTING;
    }
    
    return 0;
}

/**
 * @brief Drill control loop iteration
 */
static int lunar_drill_control_loop(uint8_t drill_id, uint32_t current_ms)
{
    (void)current_ms;  /* Used for future timing features */
    
    if (drill_id >= LUNAR_MAX_DRILLS) {
        return -1;
    }
    
    drill_telemetry_t *telem = &g_lunar.drill_telem[drill_id];
    const drill_config_t *config = &g_lunar.drill_config[drill_id];
    
    /* State machine */
    switch (telem->state) {
        case DRILL_STATE_IDLE:
            /* Nothing to do */
            break;
            
        case DRILL_STATE_STARTING:
            /* Ramp up RPM */
            if (telem->current_rpm < telem->target_rpm) {
                uint16_t ramp = 10;  /* RPM per iteration */
                telem->current_rpm += ramp;
                if (telem->current_rpm >= telem->target_rpm) {
                    telem->current_rpm = telem->target_rpm;
                    telem->state = DRILL_STATE_DRILLING;
                }
            }
            break;
            
        case DRILL_STATE_DRILLING:
            /* Monitor and control */
            {
                /* Check for faults */
                tmr_vote_result_t torque_vote = g_lunar.torque_sensor[drill_id].vote_result;
                float torque = g_lunar.torque_sensor[drill_id].voted_value;
                
                /* Overtorque check */
                if (torque > config->max_torque_nm) {
                    telem->fault = DRILL_FAULT_OVERTORQUE;
                    telem->state = DRILL_STATE_FAULT;
                    g_lunar.fault_count++;
                    break;
                }
                
                /* Torque warning - reduce RPM */
                if (torque > DRILL_TORQUE_WARNING_NM) {
                    if (telem->current_rpm > DRILL_MIN_RPM) {
                        telem->current_rpm -= 5;
                    }
                }
                
                /* Temperature check */
                float motor_temp = g_lunar.temp_sensor[drill_id].voted_value;
                if (motor_temp > MOTOR_TEMP_CRITICAL_C) {
                    telem->fault = DRILL_FAULT_OVERTEMP;
                    telem->state = DRILL_STATE_FAULT;
                    g_lunar.fault_count++;
                    break;
                }
                
                /* Depth limit check */
                float depth = g_lunar.depth_sensor[drill_id].voted_value;
                if (depth >= config->max_depth_m) {
                    telem->fault = DRILL_FAULT_DEPTH_LIMIT;
                    telem->state = DRILL_STATE_RETRACTING;
                    break;
                }
                
                /* TMR sensor health check */
                if (torque_vote == TMR_VOTE_ALL_FAILED) {
                    telem->fault = DRILL_FAULT_SENSOR_FAIL;
                    telem->state = DRILL_STATE_FAULT;
                    g_lunar.fault_count++;
                    break;
                }
                
                /* Update telemetry */
                telem->torque_nm = torque;
                telem->depth_m = depth;
                telem->motor_temp_c = motor_temp;
            }
            break;
            
        case DRILL_STATE_RETRACTING:
            /* Retract drill */
            {
                float depth = g_lunar.depth_sensor[drill_id].voted_value;
                if (depth <= 0.0f) {
                    telem->state = DRILL_STATE_IDLE;
                    telem->current_rpm = 0;
                    telem->target_rpm = 0;
                }
                /* Motor runs in reverse during retraction */
            }
            break;
            
        case DRILL_STATE_FAULT:
            /* Fault state - auto-retract if configured */
            if (config->auto_retract_on_fault) {
                telem->state = DRILL_STATE_RETRACTING;
            }
            telem->current_rpm = 0;
            break;
            
        case DRILL_STATE_EMERGENCY_STOP:
            /* Immediate stop - waiting for reset */
            telem->current_rpm = 0;
            telem->target_rpm = 0;
            break;
            
        case DRILL_STATE_MAINTENANCE:
            /* Manual mode */
            break;
    }
    
    return 0;
}

/**
 * @brief Emergency stop all drills
 */
static void lunar_drill_emergency_stop(void)
{
    for (uint8_t i = 0; i < LUNAR_MAX_DRILLS; i++) {
        g_lunar.drill_telem[i].state = DRILL_STATE_EMERGENCY_STOP;
        g_lunar.drill_telem[i].current_rpm = 0;
        g_lunar.drill_telem[i].target_rpm = 0;
    }
    g_lunar.safety.emergency_stop = true;
    g_lunar.safety.drill_enabled = false;
}

/**
 * @brief Reset drill after fault
 */
static int lunar_drill_reset(uint8_t drill_id)
{
    if (drill_id >= LUNAR_MAX_DRILLS) {
        return -1;
    }
    
    drill_telemetry_t *telem = &g_lunar.drill_telem[drill_id];
    
    /* Only reset from fault or e-stop states */
    if (telem->state != DRILL_STATE_FAULT && 
        telem->state != DRILL_STATE_EMERGENCY_STOP) {
        return -2;
    }
    
    telem->fault = DRILL_FAULT_NONE;
    telem->state = DRILL_STATE_IDLE;
    
    return 0;
}

/*===========================================================================*/
/* Regolith Analysis Implementation                                           */
/*===========================================================================*/

/**
 * @brief Analyze regolith sample
 */
static int lunar_analyze_sample(const regolith_sample_t *sample)
{
    if (!sample) {
        return -1;
    }
    
    /* Validate ranges */
    if (sample->density_gcm3 < REGOLITH_DENSITY_MIN || 
        sample->density_gcm3 > REGOLITH_DENSITY_MAX) {
        return -2;
    }
    
    /* Store sample */
    memcpy(&g_lunar.current_sample, sample, sizeof(regolith_sample_t));
    g_lunar.yield.samples_collected++;
    
    /* Adjust drilling parameters based on density */
    for (uint8_t i = 0; i < g_lunar.active_drills; i++) {
        drill_config_t *cfg = &g_lunar.drill_config[i];
        
        /* Higher density = slower feed rate */
        float density_factor = (REGOLITH_DENSITY_MAX - sample->density_gcm3) /
                               (REGOLITH_DENSITY_MAX - REGOLITH_DENSITY_MIN);
        cfg->feed_rate_mms = DRILL_FEED_RATE_MAX_MMS * (0.3f + 0.7f * density_factor);
    }
    
    /* Calculate ore grade */
    float ore_grade = sample->iron_pct + sample->titanium_pct + 
                      sample->aluminum_pct + (sample->ice_content_pct * 2.0f);
    g_lunar.yield.ore_grade_pct = ore_grade;
    
    return 0;
}

/**
 * @brief Calculate adaptive drilling parameters
 */
static int lunar_adapt_to_regolith(uint8_t drill_id)
{
    if (drill_id >= LUNAR_MAX_DRILLS) {
        return -1;
    }
    
    const regolith_sample_t *sample = &g_lunar.current_sample;
    drill_telemetry_t *telem = &g_lunar.drill_telem[drill_id];
    
    /* Increase wear model based on hardness */
    float wear_rate = 0.001f * sample->hardness;  /* % per cycle */
    telem->wear_pct += wear_rate;
    
    /* Clamp wear */
    if (telem->wear_pct > 100.0f) {
        telem->wear_pct = 100.0f;
    }
    
    /* Ice presence - risk of refreezing, affects auger duty */
    if (sample->ice_content_pct > REGOLITH_ICE_THRESHOLD_PCT) {
        /* Enable auger heating to prevent clogging */
    }
    
    return 0;
}

/*===========================================================================*/
/* Yield Telemetry Implementation                                             */
/*===========================================================================*/

/**
 * @brief Update yield telemetry
 */
static int lunar_update_yield(float mass_kg, float water_pct, 
                              float oxygen_pct, float metals_pct)
{
    yield_telemetry_t *yield = &g_lunar.yield;
    
    yield->total_mass_kg += mass_kg;
    yield->water_kg += mass_kg * (water_pct / 100.0f);
    yield->oxygen_kg += mass_kg * (oxygen_pct / 100.0f);
    yield->metals_kg += mass_kg * (metals_pct / 100.0f);
    
    /* Calculate efficiency based on ore grade */
    float theoretical_yield = mass_kg * (g_lunar.yield.ore_grade_pct / 100.0f);
    float actual_yield = yield->water_kg + yield->oxygen_kg + yield->metals_kg;
    
    if (theoretical_yield > 0.0f) {
        yield->efficiency_pct = (actual_yield / theoretical_yield) * 100.0f;
        if (yield->efficiency_pct > 100.0f) {
            yield->efficiency_pct = 100.0f;  /* Cap at 100% */
        }
    }
    
    return 0;
}

/**
 * @brief Get current yield telemetry
 */
static int lunar_get_yield(yield_telemetry_t *yield)
{
    if (!yield) {
        return -1;
    }
    
    memcpy(yield, &g_lunar.yield, sizeof(yield_telemetry_t));
    return 0;
}

/*===========================================================================*/
/* ISRU Processing Implementation                                             */
/*===========================================================================*/

/**
 * @brief Start ISRU processing cycle
 */
static int lunar_isru_start(uint32_t batch_id)
{
    if (g_lunar.isru.phase != ISRU_PHASE_IDLE) {
        return -1;  /* Already running */
    }
    
    g_lunar.isru.batch_id = batch_id;
    g_lunar.isru.phase = ISRU_PHASE_COLLECTING;
    g_lunar.isru.hopper_fill_pct = 0.0f;
    
    return 0;
}

/**
 * @brief ISRU state machine iteration
 */
static int lunar_isru_process(uint32_t delta_ms)
{
    isru_status_t *isru = &g_lunar.isru;
    
    switch (isru->phase) {
        case ISRU_PHASE_IDLE:
            break;
            
        case ISRU_PHASE_COLLECTING:
            /* Accumulate from drill operations */
            isru->hopper_fill_pct += 0.1f;  /* Simulated rate */
            if (isru->hopper_fill_pct >= 100.0f) {
                isru->hopper_fill_pct = 100.0f;
                isru->phase = ISRU_PHASE_HEATING;
                isru->furnace_temp_c = 20.0f;
            }
            break;
            
        case ISRU_PHASE_HEATING:
            /* Ramp furnace temperature for volatiles extraction */
            isru->furnace_temp_c += 10.0f;  /* °C per iteration */
            if (isru->furnace_temp_c >= 900.0f) {  /* Oxygen extraction temp */
                isru->phase = ISRU_PHASE_SEPARATING;
            }
            break;
            
        case ISRU_PHASE_SEPARATING:
            /* Material separation */
            isru->extraction_rate_kgh = 5.0f * (isru->solar_power_kw / 10.0f);
            
            /* Update yield */
            float mass = isru->extraction_rate_kgh * (delta_ms / 3600000.0f);
            lunar_update_yield(mass, 
                              g_lunar.current_sample.ice_content_pct,
                              30.0f,  /* Oxygen from regolith */
                              g_lunar.current_sample.iron_pct);
            
            /* Deplete hopper */
            isru->hopper_fill_pct -= 1.0f;
            if (isru->hopper_fill_pct <= 0.0f) {
                isru->hopper_fill_pct = 0.0f;
                isru->phase = ISRU_PHASE_STORING;
            }
            break;
            
        case ISRU_PHASE_STORING:
            /* Transfer to storage - quick transition */
            isru->phase = ISRU_PHASE_IDLE;
            break;
            
        case ISRU_PHASE_FAULT:
            /* Waiting for reset */
            break;
    }
    
    return 0;
}

/*===========================================================================*/
/* Safety Interlock Implementation                                            */
/*===========================================================================*/

/**
 * @brief Update safety interlock state
 */
static int lunar_safety_check(uint32_t current_ms)
{
    safety_interlock_t *safety = &g_lunar.safety;
    
    /* Check all TMR sensors for all drills */
    safety->sensors_ok = true;
    for (uint8_t i = 0; i < g_lunar.active_drills; i++) {
        lunar_tmr_check_timeout(&g_lunar.torque_sensor[i], current_ms);
        lunar_tmr_check_timeout(&g_lunar.depth_sensor[i], current_ms);
        lunar_tmr_check_timeout(&g_lunar.temp_sensor[i], current_ms);
        
        if (g_lunar.torque_sensor[i].vote_result == TMR_VOTE_ALL_FAILED ||
            g_lunar.depth_sensor[i].vote_result == TMR_VOTE_ALL_FAILED ||
            g_lunar.temp_sensor[i].vote_result == TMR_VOTE_ALL_FAILED) {
            safety->sensors_ok = false;
        }
    }
    
    /* Check thermal limits for all drills */
    safety->thermal_ok = true;
    for (uint8_t i = 0; i < g_lunar.active_drills; i++) {
        float temp = g_lunar.temp_sensor[i].voted_value;
        if (temp > MOTOR_TEMP_CRITICAL_C) {
            safety->thermal_ok = false;
        }
    }
    
    /* Determine drill enable status */
    safety->drill_enabled = safety->thermal_ok && 
                           safety->sensors_ok && 
                           safety->power_ok &&
                           safety->comm_ok &&
                           !safety->emergency_stop;
    
    safety->last_check_ms = current_ms;
    
    return 0;
}

/**
 * @brief Clear emergency stop
 */
static int lunar_safety_reset(void)
{
    /* Require all conditions to be OK */
    if (!g_lunar.safety.thermal_ok || !g_lunar.safety.sensors_ok) {
        return -1;
    }
    
    g_lunar.safety.emergency_stop = false;
    g_lunar.safety.drill_enabled = true;
    
    /* Reset all drill states */
    for (uint8_t i = 0; i < LUNAR_MAX_DRILLS; i++) {
        if (g_lunar.drill_telem[i].state == DRILL_STATE_EMERGENCY_STOP) {
            g_lunar.drill_telem[i].state = DRILL_STATE_IDLE;
            g_lunar.drill_telem[i].fault = DRILL_FAULT_NONE;
        }
    }
    
    return 0;
}

/*===========================================================================*/
/* Telemetry Formatting (CCSDS-style)                                        */
/*===========================================================================*/

/** CCSDS packet header (simplified) */
typedef struct {
    uint16_t packet_id;
    uint16_t sequence_ctrl;
    uint16_t length;
} ccsds_header_t;

/**
 * @brief Format drill telemetry into CCSDS packet
 */
static int lunar_format_drill_telemetry(uint8_t drill_id, uint8_t *buffer, 
                                        uint16_t buffer_size, uint16_t *length)
{
    if (!buffer || buffer_size < sizeof(ccsds_header_t) + sizeof(drill_telemetry_t)) {
        return -1;
    }
    
    if (drill_id >= LUNAR_MAX_DRILLS) {
        return -2;
    }
    
    /* CCSDS header */
    ccsds_header_t *hdr = (ccsds_header_t *)buffer;
    hdr->packet_id = 0x0800 | drill_id;  /* APID */
    hdr->sequence_ctrl = (uint16_t)(g_lunar.telemetry_seq++ & 0x3FFF);
    hdr->length = sizeof(drill_telemetry_t) - 1;  /* CCSDS length field */
    
    /* Payload */
    memcpy(buffer + sizeof(ccsds_header_t), 
           &g_lunar.drill_telem[drill_id], 
           sizeof(drill_telemetry_t));
    
    *length = sizeof(ccsds_header_t) + sizeof(drill_telemetry_t);
    
    return 0;
}

/**
 * @brief Format yield telemetry into CCSDS packet
 */
static int lunar_format_yield_telemetry(uint8_t *buffer, uint16_t buffer_size,
                                        uint16_t *length)
{
    if (!buffer || buffer_size < sizeof(ccsds_header_t) + sizeof(yield_telemetry_t)) {
        return -1;
    }
    
    ccsds_header_t *hdr = (ccsds_header_t *)buffer;
    hdr->packet_id = 0x0810;  /* APID for yield */
    hdr->sequence_ctrl = (uint16_t)(g_lunar.telemetry_seq++ & 0x3FFF);
    hdr->length = sizeof(yield_telemetry_t) - 1;
    
    memcpy(buffer + sizeof(ccsds_header_t), 
           &g_lunar.yield, 
           sizeof(yield_telemetry_t));
    
    *length = sizeof(ccsds_header_t) + sizeof(yield_telemetry_t);
    
    return 0;
}

/*===========================================================================*/
/* Main System API                                                            */
/*===========================================================================*/

/**
 * @brief Initialize lunar mining system
 */
int lunar_mining_init(void)
{
    memset(&g_lunar, 0, sizeof(lunar_ctx_t));
    
    /* Default safety state */
    g_lunar.safety.power_ok = true;
    g_lunar.safety.comm_ok = true;
    g_lunar.safety.drill_enabled = true;
    
    g_lunar.initialized = true;
    
    return 0;
}

/**
 * @brief Shutdown lunar mining system
 */
void lunar_mining_shutdown(void)
{
    /* Emergency stop all drills */
    lunar_drill_emergency_stop();
    
    /* Reset ISRU */
    g_lunar.isru.phase = ISRU_PHASE_IDLE;
    
    g_lunar.initialized = false;
}

/**
 * @brief Main processing loop
 */
int lunar_mining_process(uint32_t current_ms)
{
    if (!g_lunar.initialized) {
        return -1;
    }
    
    static uint32_t last_safety_check = 0;
    static uint32_t last_process = 0;
    
    /* Safety check */
    if (current_ms - last_safety_check >= THERMAL_CHECK_INTERVAL_MS) {
        lunar_safety_check(current_ms);
        last_safety_check = current_ms;
    }
    
    /* Process drills */
    for (uint8_t i = 0; i < g_lunar.active_drills; i++) {
        lunar_drill_control_loop(i, current_ms);
        lunar_adapt_to_regolith(i);
    }
    
    /* Process ISRU */
    uint32_t delta = current_ms - last_process;
    lunar_isru_process(delta);
    last_process = current_ms;
    
    /* Update runtime */
    g_lunar.yield.runtime_s = current_ms / 1000;
    
    return 0;
}

/**
 * @brief Get system status
 */
int lunar_get_status(bool *initialized, uint32_t *fault_count, 
                     uint32_t *active_drills)
{
    if (initialized) *initialized = g_lunar.initialized;
    if (fault_count) *fault_count = g_lunar.fault_count;
    if (active_drills) *active_drills = g_lunar.active_drills;
    return 0;
}

/*===========================================================================*/
/* Test-Accessible Functions                                                  */
/*===========================================================================*/

/* Expose internal functions for testing */
#ifdef UNIT_TEST

int test_lunar_drill_init(uint8_t id, const drill_config_t *cfg)
{
    return lunar_drill_init(id, cfg);
}

int test_lunar_drill_set_rpm(uint8_t id, uint16_t rpm)
{
    return lunar_drill_set_rpm(id, rpm);
}

int test_lunar_drill_control(uint8_t id, uint32_t ms)
{
    return lunar_drill_control_loop(id, ms);
}

void test_lunar_drill_estop(void)
{
    lunar_drill_emergency_stop();
}

int test_lunar_drill_reset(uint8_t id)
{
    return lunar_drill_reset(id);
}

int test_lunar_tmr_update(tmr_sensor_t *s, uint8_t ch, float val, uint32_t ts)
{
    return lunar_tmr_update(s, ch, val, ts);
}

tmr_vote_result_t test_lunar_tmr_vote(tmr_sensor_t *s)
{
    return lunar_tmr_vote(s);
}

int test_lunar_analyze_sample(const regolith_sample_t *s)
{
    return lunar_analyze_sample(s);
}

int test_lunar_update_yield(float m, float w, float o, float me)
{
    return lunar_update_yield(m, w, o, me);
}

int test_lunar_get_yield(yield_telemetry_t *y)
{
    return lunar_get_yield(y);
}

int test_lunar_isru_start(uint32_t batch)
{
    return lunar_isru_start(batch);
}

int test_lunar_isru_process(uint32_t delta)
{
    return lunar_isru_process(delta);
}

int test_lunar_safety_check(uint32_t ms)
{
    return lunar_safety_check(ms);
}

int test_lunar_safety_reset(void)
{
    return lunar_safety_reset();
}

int test_lunar_format_drill_telem(uint8_t id, uint8_t *buf, uint16_t sz, uint16_t *len)
{
    return lunar_format_drill_telemetry(id, buf, sz, len);
}

int test_lunar_format_yield_telem(uint8_t *buf, uint16_t sz, uint16_t *len)
{
    return lunar_format_yield_telemetry(buf, sz, len);
}

/* Direct context access for testing */
lunar_ctx_t *test_get_lunar_ctx(void)
{
    return &g_lunar;
}

#endif /* UNIT_TEST */
