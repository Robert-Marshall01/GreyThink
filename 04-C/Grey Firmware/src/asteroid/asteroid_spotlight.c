/**
 * @file asteroid_spotlight.c
 * @brief Asteroid Drill Control & Resource Telemetry Spotlight Implementation
 * 
 * Production-grade implementation demonstrating embedded space resource
 * extraction systems for asteroid mining operations. Features:
 * 
 * - PID-controlled drill motor with torque limiting
 * - TMR sensor voting for safety-critical measurements
 * - Multi-layer safety interlock system
 * - Material classification from drill feedback
 * - CCSDS-compatible telemetry packet generation
 * - Yield tracking and mission economics
 * - Autonomous fault detection and recovery
 * 
 * Standards:
 * - NASA-STD-8719.14 (Safety Standard for Robotic Missions)
 * - ECSS-E-ST-40C (Space Engineering - Software)
 * - CCSDS 133.0-B (Space Packet Protocol)
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

#define DRILL_MAX_UNITS             4           /* Maximum drill units */
#define DRILL_TMR_CHANNELS          3           /* Triple redundancy */
#define DRILL_TELEMETRY_SIZE        64          /* Telemetry packet size */

/* Drill operating limits */
#define DRILL_MAX_TORQUE_NM         500.0f      /* Maximum torque */
#define DRILL_MAX_RPM               1200.0f     /* Maximum rotation speed */
#define DRILL_MAX_DEPTH_M           25.0f       /* Maximum drilling depth */
#define DRILL_MAX_TEMP_C            150.0f      /* Overheat threshold */
#define DRILL_THERMAL_WARNING_C     120.0f      /* Thermal warning */
#define DRILL_STALL_TIMEOUT_MS      5000        /* Motor stall timeout */
#define DRILL_ANCHOR_MIN_FORCE_N    500.0f      /* Minimum anchor force */

/* PID controller gains */
#define PID_KP_TORQUE               0.8f
#define PID_KI_TORQUE               0.15f
#define PID_KD_TORQUE               0.05f
#define PID_KP_RPM                  0.5f
#define PID_KI_RPM                  0.1f
#define PID_KD_RPM                  0.02f

/* Material detection thresholds */
#define MATERIAL_REGOLITH_TORQUE    50.0f       /* Soft regolith */
#define MATERIAL_SILICATE_TORQUE    150.0f      /* Rocky material */
#define MATERIAL_CARBONACEOUS_TORQUE 200.0f     /* C-type material */
#define MATERIAL_METALLIC_TORQUE    350.0f      /* Iron-nickel core */
#define MATERIAL_ICE_VIBRATION      5.0f        /* Ice vibration signature */

/* Drilling rates (m/hour) - accelerated for simulation */
#define RATE_REGOLITH               200.0f
#define RATE_SILICATE               50.0f
#define RATE_CARBONACEOUS           30.0f
#define RATE_METALLIC               10.0f
#define RATE_ICE                    150.0f

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Drill operational state */
typedef enum {
    DRILL_STATE_IDLE,
    DRILL_STATE_PRE_CHECK,
    DRILL_STATE_ANCHORING,
    DRILL_STATE_SURFACE_SURVEY,
    DRILL_STATE_DRILLING,
    DRILL_STATE_SAMPLE_COLLECT,
    DRILL_STATE_RETRACTING,
    DRILL_STATE_FAULT,
    DRILL_STATE_MAINTENANCE
} drill_state_t;

/** Drill fault codes */
typedef enum {
    DRILL_FAULT_NONE = 0,
    DRILL_FAULT_OVERHEAT,
    DRILL_FAULT_OVERTORQUE,
    DRILL_FAULT_STALL,
    DRILL_FAULT_BIT_BREAK,
    DRILL_FAULT_POWER_LOSS,
    DRILL_FAULT_VIBRATION,
    DRILL_FAULT_ANCHOR_SLIP,
    DRILL_FAULT_SENSOR_FAIL,
    DRILL_FAULT_COMM_LOSS,
    DRILL_FAULT_DEPTH_LIMIT
} drill_fault_t;

/** Material type detection */
typedef enum {
    MATERIAL_UNKNOWN,
    MATERIAL_REGOLITH,
    MATERIAL_SILICATE,
    MATERIAL_CARBONACEOUS,
    MATERIAL_METALLIC,
    MATERIAL_ICE
} material_t;

/** TMR channel status */
typedef enum {
    TMR_CHAN_OK,
    TMR_CHAN_SUSPECT,
    TMR_CHAN_FAILED
} tmr_status_t;

/** TMR voting result */
typedef enum {
    TMR_ALL_AGREE,
    TMR_MAJORITY,
    TMR_DISAGREE,
    TMR_ALL_FAILED
} tmr_vote_t;

/** Sensor type for TMR */
typedef enum {
    SENSOR_TORQUE,
    SENSOR_TEMPERATURE,
    SENSOR_VIBRATION,
    SENSOR_DEPTH
} sensor_type_t;

/** TMR sensor set */
typedef struct {
    float values[DRILL_TMR_CHANNELS];
    uint32_t timestamps[DRILL_TMR_CHANNELS];
    tmr_status_t status[DRILL_TMR_CHANNELS];
    tmr_vote_t vote_result;
    sensor_type_t type;
} tmr_sensor_t;

/** PID controller state */
typedef struct {
    float kp;
    float ki;
    float kd;
    float setpoint;
    float prev_error;
    float integral;
    float output;
    float output_min;
    float output_max;
    uint32_t last_update_ms;
} pid_controller_t;

/** Sample record */
typedef struct {
    uint32_t sample_id;
    material_t material;
    float depth_m;
    float mass_kg;
    float purity_pct;
    uint32_t start_time;
    uint32_t end_time;
    bool valid;
} sample_record_t;

/** Yield statistics */
typedef struct {
    uint32_t total_samples;
    uint32_t successful_samples;
    float total_mass_kg;
    float ice_mass_kg;
    float metal_mass_kg;
    float silicate_mass_kg;
    float avg_purity;
    float extraction_rate;   /* kg/hour */
    uint32_t drilling_time_s;
} yield_stats_t;

/** Drill environment */
typedef struct {
    float temperature_c;
    float vibration_g;
    float anchor_force_n;
    float power_available_w;
    bool comm_link_ok;
} drill_environment_t;

/** Drill unit state */
typedef struct {
    uint8_t unit_id;
    drill_state_t state;
    drill_fault_t fault;
    uint32_t fault_count;
    
    /* Operational data */
    float current_torque_nm;
    float current_rpm;
    float current_depth_m;
    float bit_temperature_c;
    float vibration_g;
    float power_w;
    material_t detected_material;
    
    /* TMR sensors */
    tmr_sensor_t torque_sensors;
    tmr_sensor_t temp_sensors;
    tmr_sensor_t vibe_sensors;
    tmr_sensor_t depth_sensors;
    
    /* PID controllers */
    pid_controller_t torque_pid;
    pid_controller_t rpm_pid;
    
    /* Sample tracking */
    sample_record_t current_sample;
    yield_stats_t yield;
    
    /* Environment */
    drill_environment_t env;
    
    /* Timing */
    uint32_t state_start_ms;
    uint32_t drilling_start_ms;
    uint32_t last_motion_ms;
    
    /* Wear tracking */
    float bit_wear_pct;
} drill_unit_t;

/** Configuration structure */
typedef struct {
    uint8_t unit_id;
    float max_torque_nm;
    float target_rpm;
    float max_depth_m;
    float overheat_temp_c;
    bool percussion_enabled;
} drill_config_t;

/** Telemetry packet */
typedef struct {
    /* CCSDS header */
    uint8_t version;
    uint16_t apid;
    uint16_t sequence;
    uint16_t length;
    
    /* Payload */
    uint8_t state;
    uint8_t fault;
    uint8_t material;
    float torque_nm;
    float rpm;
    float depth_m;
    float temperature_c;
    float yield_kg;
    float bit_wear_pct;
    uint32_t timestamp;
    
    uint8_t checksum;
} drill_telemetry_t;

/*===========================================================================*/
/* Static Variables                                                           */
/*===========================================================================*/

static drill_unit_t g_drills[DRILL_MAX_UNITS];
static bool g_module_initialized = false;
static uint32_t g_uptime_ms = 0;
static uint16_t g_telemetry_sequence = 0;

/* Motor control outputs (simulation) */
static float g_motor_power[DRILL_MAX_UNITS];
static float g_anchor_actuator[DRILL_MAX_UNITS];

/*===========================================================================*/
/* PID Controller Functions                                                   */
/*===========================================================================*/

/**
 * @brief Initialize PID controller
 */
static void pid_init(pid_controller_t *pid, float kp, float ki, float kd,
                     float min, float max) {
    if (!pid) return;
    
    pid->kp = kp;
    pid->ki = ki;
    pid->kd = kd;
    pid->setpoint = 0.0f;
    pid->prev_error = 0.0f;
    pid->integral = 0.0f;
    pid->output = 0.0f;
    pid->output_min = min;
    pid->output_max = max;
    pid->last_update_ms = 0;
}

/**
 * @brief Update PID controller
 */
static float pid_update(pid_controller_t *pid, float measured, uint32_t current_time) {
    if (!pid) return 0.0f;
    
    float dt = (current_time - pid->last_update_ms) / 1000.0f;
    if (dt <= 0.0f || dt > 10.0f) {
        dt = 0.1f;
    }
    
    float error = pid->setpoint - measured;
    
    /* Proportional */
    float p_term = pid->kp * error;
    
    /* Integral with anti-windup */
    pid->integral += error * dt;
    float i_max = (pid->output_max - pid->output_min) / (2.0f * pid->ki + 0.001f);
    if (pid->integral > i_max) pid->integral = i_max;
    if (pid->integral < -i_max) pid->integral = -i_max;
    float i_term = pid->ki * pid->integral;
    
    /* Derivative */
    float d_term = pid->kd * (error - pid->prev_error) / dt;
    
    /* Sum and clamp */
    pid->output = p_term + i_term + d_term;
    if (pid->output > pid->output_max) pid->output = pid->output_max;
    if (pid->output < pid->output_min) pid->output = pid->output_min;
    
    pid->prev_error = error;
    pid->last_update_ms = current_time;
    
    return pid->output;
}

/**
 * @brief Reset PID controller
 */
static void pid_reset(pid_controller_t *pid) {
    if (!pid) return;
    pid->prev_error = 0.0f;
    pid->integral = 0.0f;
    pid->output = 0.0f;
}

/*===========================================================================*/
/* TMR Voting Functions                                                       */
/*===========================================================================*/

/**
 * @brief Perform TMR voting on sensor values
 */
static float tmr_vote(tmr_sensor_t *sensor) {
    if (!sensor) return NAN;
    
    int valid_count = 0;
    float values[DRILL_TMR_CHANNELS];
    
    for (int i = 0; i < DRILL_TMR_CHANNELS; i++) {
        if (sensor->status[i] != TMR_CHAN_FAILED) {
            values[valid_count++] = sensor->values[i];
        }
    }
    
    if (valid_count == 0) {
        sensor->vote_result = TMR_ALL_FAILED;
        return NAN;
    }
    
    if (valid_count == 1) {
        sensor->vote_result = TMR_MAJORITY;
        return values[0];
    }
    
    /* Check agreement within tolerance */
    float tolerance = 0.05f;  /* 5% tolerance */
    float avg = 0.0f;
    for (int i = 0; i < valid_count; i++) {
        avg += values[i];
    }
    avg /= valid_count;
    
    int agree_count = 0;
    for (int i = 0; i < valid_count; i++) {
        if (fabsf(values[i] - avg) / (fabsf(avg) + 0.001f) < tolerance) {
            agree_count++;
        }
    }
    
    if (agree_count == valid_count) {
        sensor->vote_result = TMR_ALL_AGREE;
    } else if (agree_count >= 2) {
        sensor->vote_result = TMR_MAJORITY;
    } else {
        sensor->vote_result = TMR_DISAGREE;
    }
    
    return avg;
}

/**
 * @brief Check TMR sensor health
 */
static void tmr_check_health(tmr_sensor_t *sensor, uint32_t current_time) {
    if (!sensor) return;
    
    for (int i = 0; i < DRILL_TMR_CHANNELS; i++) {
        /* Check for stale readings */
        if (current_time - sensor->timestamps[i] > 5000) {
            sensor->status[i] = TMR_CHAN_FAILED;
        }
    }
}

/*===========================================================================*/
/* Sensor Reading Functions                                                   */
/*===========================================================================*/

/**
 * @brief Read torque sensors with TMR
 */
static float read_torque(drill_unit_t *drill) {
    if (!drill) return NAN;
    
    float base_torque = drill->current_torque_nm;
    
    for (int i = 0; i < DRILL_TMR_CHANNELS; i++) {
        if (drill->torque_sensors.status[i] != TMR_CHAN_FAILED) {
            /* Simulate slight variation */
            drill->torque_sensors.values[i] = base_torque * (1.0f + (i - 1) * 0.01f);
            drill->torque_sensors.timestamps[i] = g_uptime_ms;
            drill->torque_sensors.status[i] = TMR_CHAN_OK;
        }
    }
    
    drill->torque_sensors.type = SENSOR_TORQUE;
    tmr_check_health(&drill->torque_sensors, g_uptime_ms);
    return tmr_vote(&drill->torque_sensors);
}

/**
 * @brief Read temperature sensors with TMR
 */
static float read_temperature(drill_unit_t *drill) {
    if (!drill) return NAN;
    
    float base_temp = drill->bit_temperature_c;
    
    for (int i = 0; i < DRILL_TMR_CHANNELS; i++) {
        if (drill->temp_sensors.status[i] != TMR_CHAN_FAILED) {
            drill->temp_sensors.values[i] = base_temp + (i - 1) * 0.5f;
            drill->temp_sensors.timestamps[i] = g_uptime_ms;
            drill->temp_sensors.status[i] = TMR_CHAN_OK;
        }
    }
    
    drill->temp_sensors.type = SENSOR_TEMPERATURE;
    tmr_check_health(&drill->temp_sensors, g_uptime_ms);
    return tmr_vote(&drill->temp_sensors);
}

/**
 * @brief Read vibration sensors with TMR
 */
static float read_vibration(drill_unit_t *drill) {
    if (!drill) return NAN;
    
    float base_vibe = drill->vibration_g;
    
    for (int i = 0; i < DRILL_TMR_CHANNELS; i++) {
        if (drill->vibe_sensors.status[i] != TMR_CHAN_FAILED) {
            drill->vibe_sensors.values[i] = base_vibe * (1.0f + (i - 1) * 0.02f);
            drill->vibe_sensors.timestamps[i] = g_uptime_ms;
            drill->vibe_sensors.status[i] = TMR_CHAN_OK;
        }
    }
    
    drill->vibe_sensors.type = SENSOR_VIBRATION;
    tmr_check_health(&drill->vibe_sensors, g_uptime_ms);
    return tmr_vote(&drill->vibe_sensors);
}

/**
 * @brief Read depth sensors with TMR
 */
static float read_depth(drill_unit_t *drill) {
    if (!drill) return NAN;
    
    float base_depth = drill->current_depth_m;
    
    for (int i = 0; i < DRILL_TMR_CHANNELS; i++) {
        if (drill->depth_sensors.status[i] != TMR_CHAN_FAILED) {
            drill->depth_sensors.values[i] = base_depth + (i - 1) * 0.001f;
            drill->depth_sensors.timestamps[i] = g_uptime_ms;
            drill->depth_sensors.status[i] = TMR_CHAN_OK;
        }
    }
    
    drill->depth_sensors.type = SENSOR_DEPTH;
    tmr_check_health(&drill->depth_sensors, g_uptime_ms);
    return tmr_vote(&drill->depth_sensors);
}

/*===========================================================================*/
/* Material Classification                                                    */
/*===========================================================================*/

/**
 * @brief Classify material from drill feedback
 */
static material_t classify_material(drill_unit_t *drill) {
    if (!drill) return MATERIAL_UNKNOWN;
    
    float torque = drill->current_torque_nm;
    float vibration = drill->vibration_g;
    
    /* Ice has distinct vibration signature */
    if (vibration > MATERIAL_ICE_VIBRATION && torque < MATERIAL_SILICATE_TORQUE) {
        return MATERIAL_ICE;
    }
    
    /* Classify by torque */
    if (torque < MATERIAL_REGOLITH_TORQUE) {
        return MATERIAL_REGOLITH;
    } else if (torque < MATERIAL_SILICATE_TORQUE) {
        return MATERIAL_SILICATE;
    } else if (torque < MATERIAL_CARBONACEOUS_TORQUE) {
        return MATERIAL_CARBONACEOUS;
    } else if (torque < MATERIAL_METALLIC_TORQUE) {
        return MATERIAL_METALLIC;
    } else {
        return MATERIAL_METALLIC;  /* Very hard material */
    }
}

/**
 * @brief Get drilling rate for material
 */
static float get_drilling_rate(material_t material) {
    switch (material) {
        case MATERIAL_REGOLITH:     return RATE_REGOLITH;
        case MATERIAL_SILICATE:     return RATE_SILICATE;
        case MATERIAL_CARBONACEOUS: return RATE_CARBONACEOUS;
        case MATERIAL_METALLIC:     return RATE_METALLIC;
        case MATERIAL_ICE:          return RATE_ICE;
        default:                    return RATE_SILICATE;
    }
}

/*===========================================================================*/
/* Safety Interlock Functions                                                 */
/*===========================================================================*/

/**
 * @brief Check all safety interlocks
 */
static drill_fault_t check_safety_interlocks(drill_unit_t *drill) {
    if (!drill) return DRILL_FAULT_NONE;
    
    /* Already in fault state */
    if (drill->state == DRILL_STATE_FAULT) {
        return drill->fault;
    }
    
    /* Read sensors */
    float temp = read_temperature(drill);
    float torque = read_torque(drill);
    float vibe = read_vibration(drill);
    
    /* TMR sensor failure */
    if (drill->torque_sensors.vote_result == TMR_ALL_FAILED ||
        drill->temp_sensors.vote_result == TMR_ALL_FAILED) {
        return DRILL_FAULT_SENSOR_FAIL;
    }
    
    /* Overheat check */
    if (temp > DRILL_MAX_TEMP_C) {
        return DRILL_FAULT_OVERHEAT;
    }
    
    /* Overtorque check */
    if (torque > DRILL_MAX_TORQUE_NM) {
        return DRILL_FAULT_OVERTORQUE;
    }
    
    /* Excessive vibration */
    if (vibe > 20.0f) {  /* 20g limit */
        return DRILL_FAULT_VIBRATION;
    }
    
    /* Anchor slip during drilling */
    if (drill->state == DRILL_STATE_DRILLING &&
        drill->env.anchor_force_n < DRILL_ANCHOR_MIN_FORCE_N) {
        return DRILL_FAULT_ANCHOR_SLIP;
    }
    
    /* Motor stall detection */
    if (drill->state == DRILL_STATE_DRILLING &&
        drill->current_rpm < 10.0f &&
        g_motor_power[drill->unit_id] > 50.0f) {
        if ((g_uptime_ms - drill->last_motion_ms) > DRILL_STALL_TIMEOUT_MS) {
            return DRILL_FAULT_STALL;
        }
    } else {
        drill->last_motion_ms = g_uptime_ms;
    }
    
    /* Depth limit */
    if (drill->current_depth_m > DRILL_MAX_DEPTH_M) {
        return DRILL_FAULT_DEPTH_LIMIT;
    }
    
    /* Communication loss during drilling */
    if (drill->state == DRILL_STATE_DRILLING && !drill->env.comm_link_ok) {
        return DRILL_FAULT_COMM_LOSS;
    }
    
    return DRILL_FAULT_NONE;
}

/**
 * @brief Execute emergency stop
 */
static void emergency_stop(drill_unit_t *drill) {
    if (!drill) return;
    
    /* Stop motor immediately */
    g_motor_power[drill->unit_id] = 0.0f;
    
    /* Transition to fault state */
    drill->state = DRILL_STATE_FAULT;
    drill->state_start_ms = g_uptime_ms;
    
    /* Reset controllers */
    pid_reset(&drill->torque_pid);
    pid_reset(&drill->rpm_pid);
}

/*===========================================================================*/
/* State Machine Functions                                                    */
/*===========================================================================*/

/**
 * @brief Handle IDLE state
 */
static void state_idle(drill_unit_t *drill) {
    (void)drill;  /* Nothing to do */
}

/**
 * @brief Handle PRE_CHECK state
 */
static void state_pre_check(drill_unit_t *drill) {
    /* Verify all sensors operational */
    read_torque(drill);
    read_temperature(drill);
    read_vibration(drill);
    read_depth(drill);
    
    bool sensors_ok = (drill->torque_sensors.vote_result != TMR_ALL_FAILED &&
                       drill->temp_sensors.vote_result != TMR_ALL_FAILED);
    
    /* Check environment */
    bool env_ok = (drill->env.power_available_w > 100.0f);
    
    if (sensors_ok && env_ok) {
        drill->state = DRILL_STATE_ANCHORING;
        drill->state_start_ms = g_uptime_ms;
    }
    
    /* Timeout */
    if ((g_uptime_ms - drill->state_start_ms) > 10000) {
        drill->fault = DRILL_FAULT_SENSOR_FAIL;
        emergency_stop(drill);
    }
}

/**
 * @brief Handle ANCHORING state
 */
static void state_anchoring(drill_unit_t *drill) {
    /* Simulate anchor deployment */
    g_anchor_actuator[drill->unit_id] += 10.0f * 0.1f;  /* Increase force */
    
    if (g_anchor_actuator[drill->unit_id] > 100.0f) {
        g_anchor_actuator[drill->unit_id] = 100.0f;
    }
    
    /* Update simulated anchor force */
    drill->env.anchor_force_n = g_anchor_actuator[drill->unit_id] * 10.0f;
    
    if (drill->env.anchor_force_n >= DRILL_ANCHOR_MIN_FORCE_N) {
        drill->state = DRILL_STATE_SURFACE_SURVEY;
        drill->state_start_ms = g_uptime_ms;
    }
    
    /* Timeout */
    if ((g_uptime_ms - drill->state_start_ms) > 30000) {
        drill->fault = DRILL_FAULT_ANCHOR_SLIP;
        emergency_stop(drill);
    }
}

/**
 * @brief Handle SURFACE_SURVEY state
 */
static void state_surface_survey(drill_unit_t *drill) {
    /* Low-power surface sampling */
    drill->rpm_pid.setpoint = 100.0f;  /* Low RPM for survey */
    
    float power = pid_update(&drill->rpm_pid, drill->current_rpm, g_uptime_ms);
    g_motor_power[drill->unit_id] = power;
    
    /* Simulate motor response */
    drill->current_rpm += power * 0.1f;
    if (drill->current_rpm > 100.0f) drill->current_rpm = 100.0f;
    
    /* Simulate light drilling */
    drill->current_torque_nm = 20.0f + drill->current_rpm * 0.1f;
    drill->vibration_g = 1.0f + drill->current_rpm * 0.01f;
    
    /* Classify surface material */
    drill->detected_material = classify_material(drill);
    
    /* Survey complete after short time */
    if ((g_uptime_ms - drill->state_start_ms) > 5000) {
        drill->state = DRILL_STATE_DRILLING;
        drill->state_start_ms = g_uptime_ms;
        drill->drilling_start_ms = g_uptime_ms;
        
        /* Start sample record */
        drill->current_sample.sample_id = g_uptime_ms;
        drill->current_sample.material = drill->detected_material;
        drill->current_sample.start_time = g_uptime_ms;
        drill->current_sample.depth_m = drill->current_depth_m;
        drill->current_sample.mass_kg = 0.0f;
        drill->current_sample.valid = false;
    }
}

/**
 * @brief Handle DRILLING state
 */
static void state_drilling(drill_unit_t *drill) {
    /* Set target RPM based on material */
    float target_rpm;
    switch (drill->detected_material) {
        case MATERIAL_REGOLITH:     target_rpm = DRILL_MAX_RPM; break;
        case MATERIAL_SILICATE:     target_rpm = DRILL_MAX_RPM * 0.6f; break;
        case MATERIAL_CARBONACEOUS: target_rpm = DRILL_MAX_RPM * 0.4f; break;
        case MATERIAL_METALLIC:     target_rpm = DRILL_MAX_RPM * 0.2f; break;
        case MATERIAL_ICE:          target_rpm = DRILL_MAX_RPM * 0.8f; break;
        default:                    target_rpm = DRILL_MAX_RPM * 0.5f; break;
    }
    
    drill->rpm_pid.setpoint = target_rpm;
    drill->torque_pid.setpoint = DRILL_MAX_TORQUE_NM * 0.8f;  /* 80% torque limit */
    
    /* Update motor control */
    float rpm_power = pid_update(&drill->rpm_pid, drill->current_rpm, g_uptime_ms);
    float torque_power = pid_update(&drill->torque_pid, DRILL_MAX_TORQUE_NM - drill->current_torque_nm, g_uptime_ms);
    
    /* Use minimum of RPM and torque-limited power */
    float power = fminf(rpm_power, torque_power + 50.0f);
    g_motor_power[drill->unit_id] = power;
    
    /* Simulate motor response */
    drill->current_rpm += (target_rpm - drill->current_rpm) * 0.1f;
    
    /* Simulate torque based on material */
    float base_torque = 0.0f;
    switch (drill->detected_material) {
        case MATERIAL_REGOLITH:     base_torque = 30.0f; break;
        case MATERIAL_SILICATE:     base_torque = 120.0f; break;
        case MATERIAL_CARBONACEOUS: base_torque = 180.0f; break;
        case MATERIAL_METALLIC:     base_torque = 350.0f; break;
        case MATERIAL_ICE:          base_torque = 60.0f; break;
        default:                    base_torque = 100.0f; break;
    }
    drill->current_torque_nm = base_torque + drill->current_rpm * 0.05f;
    
    /* Simulate drilling progress */
    float rate = get_drilling_rate(drill->detected_material);
    drill->current_depth_m += rate / 3600.0f * 0.1f;  /* Per 100ms cycle */
    
    /* Simulate vibration */
    drill->vibration_g = 2.0f + drill->current_rpm * 0.005f;
    if (drill->detected_material == MATERIAL_ICE) {
        drill->vibration_g += 3.0f;  /* Ice causes more vibration */
    }
    
    /* Simulate heating */
    drill->bit_temperature_c += (drill->current_torque_nm * drill->current_rpm / 100000.0f);
    /* Natural cooling */
    drill->bit_temperature_c -= (drill->bit_temperature_c - 20.0f) * 0.01f;
    
    /* Accumulate sample mass (accelerated for simulation) */
    float density = 2.5f;  /* kg/m³ estimate */
    drill->current_sample.mass_kg += rate / 3600.0f * 0.1f * density * 0.1f;  /* Core sample */
    
    /* Bit wear */
    drill->bit_wear_pct += drill->current_torque_nm / DRILL_MAX_TORQUE_NM * 0.001f;
    if (drill->bit_wear_pct > 100.0f) drill->bit_wear_pct = 100.0f;
    
    /* Re-classify material periodically */
    drill->detected_material = classify_material(drill);
    drill->current_sample.material = drill->detected_material;
    
    /* Check for target depth or collection threshold */
    if (drill->current_sample.mass_kg >= 1.0f ||
        drill->current_depth_m >= 10.0f) {
        drill->state = DRILL_STATE_SAMPLE_COLLECT;
        drill->state_start_ms = g_uptime_ms;
    }
}

/**
 * @brief Handle SAMPLE_COLLECT state
 */
static void state_sample_collect(drill_unit_t *drill) {
    /* Slow motor for collection */
    drill->rpm_pid.setpoint = 50.0f;
    float power = pid_update(&drill->rpm_pid, drill->current_rpm, g_uptime_ms);
    g_motor_power[drill->unit_id] = power;
    
    drill->current_rpm *= 0.9f;  /* Slow down */
    
    /* Collection complete - finalize sample */
    if ((g_uptime_ms - drill->state_start_ms) > 3000) {
        drill->current_sample.end_time = g_uptime_ms;
        drill->current_sample.depth_m = drill->current_depth_m;
        drill->current_sample.purity_pct = 85.0f + ((float)(g_uptime_ms % 10)) * 1.5f;  /* Simulated */
        drill->current_sample.valid = true;
        
        /* Update yield statistics */
        drill->yield.total_samples++;
        drill->yield.successful_samples++;
        drill->yield.total_mass_kg += drill->current_sample.mass_kg;
        
        switch (drill->current_sample.material) {
            case MATERIAL_ICE:
                drill->yield.ice_mass_kg += drill->current_sample.mass_kg;
                break;
            case MATERIAL_METALLIC:
                drill->yield.metal_mass_kg += drill->current_sample.mass_kg;
                break;
            default:
                drill->yield.silicate_mass_kg += drill->current_sample.mass_kg;
                break;
        }
        
        drill->yield.avg_purity = (drill->yield.avg_purity * (drill->yield.total_samples - 1) +
                                   drill->current_sample.purity_pct) / drill->yield.total_samples;
        
        uint32_t elapsed_s = (g_uptime_ms - drill->drilling_start_ms) / 1000;
        if (elapsed_s > 0) {
            drill->yield.extraction_rate = drill->yield.total_mass_kg * 3600.0f / elapsed_s;
        }
        
        drill->yield.drilling_time_s += elapsed_s;
        
        drill->state = DRILL_STATE_RETRACTING;
        drill->state_start_ms = g_uptime_ms;
    }
}

/**
 * @brief Handle RETRACTING state
 */
static void state_retracting(drill_unit_t *drill) {
    /* Reverse motor slowly */
    g_motor_power[drill->unit_id] = -20.0f;  /* Negative for reverse */
    
    drill->current_rpm = -50.0f;  /* Reverse rotation */
    drill->current_depth_m -= 0.5f * 0.1f;  /* Retract at 0.5 m/s */
    
    if (drill->current_depth_m <= 0.0f) {
        drill->current_depth_m = 0.0f;
        drill->current_rpm = 0.0f;
        g_motor_power[drill->unit_id] = 0.0f;
        
        /* Release anchor */
        g_anchor_actuator[drill->unit_id] = 0.0f;
        drill->env.anchor_force_n = 0.0f;
        
        drill->state = DRILL_STATE_IDLE;
        drill->state_start_ms = g_uptime_ms;
    }
}

/**
 * @brief Handle FAULT state
 */
static void state_fault(drill_unit_t *drill) {
    /* Motor stopped */
    g_motor_power[drill->unit_id] = 0.0f;
    drill->current_rpm = 0.0f;
    
    /* Cool down */
    drill->bit_temperature_c -= (drill->bit_temperature_c - 20.0f) * 0.05f;
}

/**
 * @brief Main state machine update
 */
static void update_state_machine(drill_unit_t *drill) {
    if (!drill) return;
    
    /* Check safety interlocks first */
    drill_fault_t fault = check_safety_interlocks(drill);
    if (fault != DRILL_FAULT_NONE && drill->state != DRILL_STATE_FAULT) {
        drill->fault = fault;
        drill->fault_count++;
        emergency_stop(drill);
        return;
    }
    
    /* Execute state handler */
    switch (drill->state) {
        case DRILL_STATE_IDLE:
            state_idle(drill);
            break;
        case DRILL_STATE_PRE_CHECK:
            state_pre_check(drill);
            break;
        case DRILL_STATE_ANCHORING:
            state_anchoring(drill);
            break;
        case DRILL_STATE_SURFACE_SURVEY:
            state_surface_survey(drill);
            break;
        case DRILL_STATE_DRILLING:
            state_drilling(drill);
            break;
        case DRILL_STATE_SAMPLE_COLLECT:
            state_sample_collect(drill);
            break;
        case DRILL_STATE_RETRACTING:
            state_retracting(drill);
            break;
        case DRILL_STATE_FAULT:
            state_fault(drill);
            break;
        case DRILL_STATE_MAINTENANCE:
        default:
            /* Wait states */
            break;
    }
}

/*===========================================================================*/
/* Telemetry Functions                                                        */
/*===========================================================================*/

/**
 * @brief Calculate checksum
 */
static uint8_t calculate_checksum(const uint8_t *data, size_t len) {
    uint8_t sum = 0;
    for (size_t i = 0; i < len - 1; i++) {
        sum ^= data[i];
    }
    return sum;
}

/**
 * @brief Generate telemetry packet
 */
static int generate_telemetry(drill_unit_t *drill, drill_telemetry_t *packet) {
    if (!drill || !packet) return -1;
    
    memset(packet, 0, sizeof(drill_telemetry_t));
    
    /* CCSDS header */
    packet->version = 0;
    packet->apid = 0x200 + drill->unit_id;
    packet->sequence = g_telemetry_sequence++;
    packet->length = sizeof(drill_telemetry_t) - 7;  /* Exclude header */
    
    /* Payload */
    packet->state = (uint8_t)drill->state;
    packet->fault = (uint8_t)drill->fault;
    packet->material = (uint8_t)drill->detected_material;
    packet->torque_nm = drill->current_torque_nm;
    packet->rpm = drill->current_rpm;
    packet->depth_m = drill->current_depth_m;
    packet->temperature_c = drill->bit_temperature_c;
    packet->yield_kg = drill->yield.total_mass_kg;
    packet->bit_wear_pct = drill->bit_wear_pct;
    packet->timestamp = g_uptime_ms;
    
    /* Checksum */
    packet->checksum = calculate_checksum((uint8_t*)packet, sizeof(drill_telemetry_t));
    
    return 0;
}

/*===========================================================================*/
/* Public API                                                                 */
/*===========================================================================*/

/**
 * @brief Initialize asteroid drill module
 */
int asteroid_drill_init(void) {
    if (g_module_initialized) {
        return -1;
    }
    
    memset(g_drills, 0, sizeof(g_drills));
    memset(g_motor_power, 0, sizeof(g_motor_power));
    memset(g_anchor_actuator, 0, sizeof(g_anchor_actuator));
    
    g_uptime_ms = 0;
    
    for (uint8_t i = 0; i < DRILL_MAX_UNITS; i++) {
        g_drills[i].unit_id = i;
        g_drills[i].state = DRILL_STATE_IDLE;
        g_drills[i].bit_temperature_c = 20.0f;  /* Ambient */
        g_drills[i].env.power_available_w = 1000.0f;
        g_drills[i].env.comm_link_ok = true;
        g_drills[i].env.anchor_force_n = 0.0f;
        
        /* Initialize TMR sensors */
        for (uint8_t j = 0; j < DRILL_TMR_CHANNELS; j++) {
            g_drills[i].torque_sensors.status[j] = TMR_CHAN_OK;
            g_drills[i].temp_sensors.status[j] = TMR_CHAN_OK;
            g_drills[i].vibe_sensors.status[j] = TMR_CHAN_OK;
            g_drills[i].depth_sensors.status[j] = TMR_CHAN_OK;
        }
        
        /* Initialize PID controllers */
        pid_init(&g_drills[i].torque_pid, PID_KP_TORQUE, PID_KI_TORQUE, PID_KD_TORQUE, 0.0f, 100.0f);
        pid_init(&g_drills[i].rpm_pid, PID_KP_RPM, PID_KI_RPM, PID_KD_RPM, 0.0f, 100.0f);
    }
    
    g_telemetry_sequence = 0;
    g_module_initialized = true;
    
    return 0;
}

/**
 * @brief Configure drill unit
 */
int asteroid_drill_configure(const drill_config_t *config) {
    if (!g_module_initialized || !config) {
        return -1;
    }
    
    if (config->unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    drill_unit_t *drill = &g_drills[config->unit_id];
    
    if (drill->state != DRILL_STATE_IDLE &&
        drill->state != DRILL_STATE_MAINTENANCE) {
        return -1;
    }
    
    drill->rpm_pid.setpoint = config->target_rpm;
    
    return 0;
}

/**
 * @brief Start drilling operation
 */
int asteroid_drill_start(uint8_t unit_id) {
    if (!g_module_initialized || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    drill_unit_t *drill = &g_drills[unit_id];
    
    if (drill->state != DRILL_STATE_IDLE) {
        return -1;
    }
    
    /* Clear previous fault */
    drill->fault = DRILL_FAULT_NONE;
    
    /* Start pre-check sequence */
    drill->state = DRILL_STATE_PRE_CHECK;
    drill->state_start_ms = g_uptime_ms;
    drill->last_motion_ms = g_uptime_ms;
    
    return 0;
}

/**
 * @brief Abort drilling operation
 */
int asteroid_drill_abort(uint8_t unit_id) {
    if (!g_module_initialized || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    drill_unit_t *drill = &g_drills[unit_id];
    
    /* Stop motor */
    g_motor_power[unit_id] = 0.0f;
    drill->current_rpm = 0.0f;
    
    /* Begin retraction if drilling */
    if (drill->state == DRILL_STATE_DRILLING ||
        drill->state == DRILL_STATE_SAMPLE_COLLECT) {
        drill->state = DRILL_STATE_RETRACTING;
        drill->state_start_ms = g_uptime_ms;
    } else {
        drill->state = DRILL_STATE_IDLE;
    }
    
    return 0;
}

/**
 * @brief Reset drill after fault
 */
int asteroid_drill_reset(uint8_t unit_id) {
    if (!g_module_initialized || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    drill_unit_t *drill = &g_drills[unit_id];
    
    if (drill->state != DRILL_STATE_FAULT) {
        return 0;  /* Not in fault, nothing to reset */
    }
    
    /* Only allow reset if cooled down and retracted */
    if (drill->bit_temperature_c > 50.0f) {
        return -1;  /* Still too hot */
    }
    
    drill->fault = DRILL_FAULT_NONE;
    drill->state = DRILL_STATE_IDLE;
    drill->state_start_ms = g_uptime_ms;
    
    pid_reset(&drill->torque_pid);
    pid_reset(&drill->rpm_pid);
    
    return 0;
}

/**
 * @brief Update drill subsystem
 */
void asteroid_drill_update(uint32_t elapsed_ms) {
    if (!g_module_initialized) return;
    
    g_uptime_ms += elapsed_ms;
    
    for (uint8_t i = 0; i < DRILL_MAX_UNITS; i++) {
        update_state_machine(&g_drills[i]);
    }
}

/**
 * @brief Get drill state
 */
int asteroid_drill_get_state(uint8_t unit_id, drill_state_t *state) {
    if (!g_module_initialized || !state || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    *state = g_drills[unit_id].state;
    return 0;
}

/**
 * @brief Get drill fault
 */
int asteroid_drill_get_fault(uint8_t unit_id, drill_fault_t *fault) {
    if (!g_module_initialized || !fault || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    *fault = g_drills[unit_id].fault;
    return 0;
}

/**
 * @brief Get yield statistics
 */
int asteroid_drill_get_yield(uint8_t unit_id, yield_stats_t *stats) {
    if (!g_module_initialized || !stats || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    memcpy(stats, &g_drills[unit_id].yield, sizeof(yield_stats_t));
    return 0;
}

/**
 * @brief Get current sample
 */
int asteroid_drill_get_sample(uint8_t unit_id, sample_record_t *sample) {
    if (!g_module_initialized || !sample || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    memcpy(sample, &g_drills[unit_id].current_sample, sizeof(sample_record_t));
    return 0;
}

/**
 * @brief Get telemetry packet
 */
int asteroid_drill_get_telemetry(uint8_t unit_id, uint8_t *buffer, size_t max_len) {
    if (!g_module_initialized || !buffer || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    if (max_len < sizeof(drill_telemetry_t)) {
        return -1;
    }
    
    drill_telemetry_t packet;
    if (generate_telemetry(&g_drills[unit_id], &packet) != 0) {
        return -1;
    }
    
    memcpy(buffer, &packet, sizeof(drill_telemetry_t));
    return sizeof(drill_telemetry_t);
}

/**
 * @brief Set environment parameter
 */
int asteroid_drill_set_environment(uint8_t unit_id, const char *param, float value) {
    if (!g_module_initialized || !param || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    drill_unit_t *drill = &g_drills[unit_id];
    
    if (strcmp(param, "temperature") == 0) {
        drill->bit_temperature_c = value;
    } else if (strcmp(param, "torque") == 0) {
        drill->current_torque_nm = value;
    } else if (strcmp(param, "anchor_force") == 0) {
        drill->env.anchor_force_n = value;
    } else if (strcmp(param, "power") == 0) {
        drill->env.power_available_w = value;
    } else if (strcmp(param, "vibration") == 0) {
        drill->vibration_g = value;
    } else if (strcmp(param, "depth") == 0) {
        drill->current_depth_m = value;
    } else if (strcmp(param, "comm_link") == 0) {
        drill->env.comm_link_ok = (value > 0.5f);
    } else {
        return -1;
    }
    
    return 0;
}

/**
 * @brief Inject sensor fault for testing
 */
int asteroid_drill_inject_fault(uint8_t unit_id, sensor_type_t sensor, uint8_t channel) {
    if (!g_module_initialized || unit_id >= DRILL_MAX_UNITS) {
        return -1;
    }
    
    if (channel >= DRILL_TMR_CHANNELS) {
        return -1;
    }
    
    drill_unit_t *drill = &g_drills[unit_id];
    
    switch (sensor) {
        case SENSOR_TORQUE:
            drill->torque_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        case SENSOR_TEMPERATURE:
            drill->temp_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        case SENSOR_VIBRATION:
            drill->vibe_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        case SENSOR_DEPTH:
            drill->depth_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        default:
            return -1;
    }
    
    return 0;
}

/**
 * @brief Shutdown drill module
 */
void asteroid_drill_shutdown(void) {
    if (!g_module_initialized) return;
    
    /* Emergency stop all drills */
    for (uint8_t i = 0; i < DRILL_MAX_UNITS; i++) {
        if (g_drills[i].state != DRILL_STATE_IDLE) {
            emergency_stop(&g_drills[i]);
        }
    }
    
    g_module_initialized = false;
}
