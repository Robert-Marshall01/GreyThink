/**
 * @file orbital_spotlight.c
 * @brief Zero-Gravity Fabrication Control Spotlight
 * 
 * WHAT: Comprehensive orbital manufacturing control system demonstrating 
 *       embedded firmware for space-based material processing.
 * 
 * WHY: Microgravity enables unique manufacturing processes:
 *      - Perfect crystal growth (semiconductors, pharmaceuticals)
 *      - Ultra-pure material synthesis (ZBLAN fiber, alloys)
 *      - Containerless processing (no contamination)
 *      - Novel composite fabrication
 *      - Large-scale structure assembly
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - TMR (Triple Modular Redundancy) sensor voting
 *      - Vacuum/thermal process control loops
 *      - Microgravity drift compensation
 *      - Material yield tracking and quality metrics
 *      - Safety interlocks for thermal/vacuum anomalies
 *      - CCSDS-compatible telemetry formatting
 *      - Real-time process stability monitoring
 * 
 * Industry applications: ISS National Lab, Axiom Space, Varda Space
 * Standards: NASA-STD-8729, ESA ECSS-E-HB-32-20
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

#define FAB_MAX_CHAMBERS            4
#define FAB_MAX_SENSORS             24
#define FAB_TMR_CHANNELS            3
#define FAB_TELEMETRY_BUFFER_SIZE   512
#define FAB_SAMPLE_HISTORY_SIZE     64

/* Vacuum chamber limits */
#define VACUUM_TARGET_TORR          1e-6f
#define VACUUM_WARNING_TORR         1e-4f
#define VACUUM_CRITICAL_TORR        1e-2f
#define VACUUM_PUMP_TIMEOUT_MS      60000

/* Temperature control - process specific */
#define TEMP_AMBIENT_C              20.0f
#define TEMP_HEATING_MAX_C          2500.0f     /* For metal processing */
#define TEMP_RATE_MAX_CS            50.0f       /* °C/second max ramp */
#define TEMP_OVERSHOOT_MAX_C        5.0f
#define TEMP_STABILITY_THRESHOLD_C  0.5f

/* Crystal growth parameters */
#define CRYSTAL_GROWTH_RATE_UM_HR   10.0f       /* Maximum growth rate */
#define CRYSTAL_DEFECT_THRESHOLD    0.001f      /* Defects per mm² */
#define CRYSTAL_ORIENTATION_TOL_DEG 0.01f

/* Vapor deposition parameters */
#define DEPOSITION_RATE_NM_S        1.0f
#define DEPOSITION_UNIFORMITY_PCT   99.0f
#define PRECURSOR_FLOW_SCCM_MAX     100.0f

/* Microgravity environment */
#define MICROGRAVITY_THRESHOLD_G    1e-4f       /* Acceptable g-level */
#define VIBRATION_THRESHOLD_MG      10.0f       /* milli-g RMS */
#define ATTITUDE_STABILITY_DEG      0.01f

/* TMR voting thresholds */
#define TMR_TEMP_THRESHOLD_C        2.0f
#define TMR_PRESSURE_THRESHOLD_PCT  5.0f
#define TMR_SENSOR_TIMEOUT_MS       500

/* Safety timeouts */
#define THERMAL_RUNAWAY_MS          5000
#define VACUUM_LOSS_TIMEOUT_MS      10000
#define PROCESS_ABORT_TIMEOUT_MS    30000

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Fabrication process state */
typedef enum {
    FAB_STATE_IDLE,
    FAB_STATE_INITIALIZING,
    FAB_STATE_VACUUM_PUMP,
    FAB_STATE_HEATING,
    FAB_STATE_DEPOSITION,
    FAB_STATE_GROWTH,
    FAB_STATE_COOLING,
    FAB_STATE_QUALITY_CHECK,
    FAB_STATE_UNLOADING,
    FAB_STATE_ABORT,
    FAB_STATE_MAINTENANCE
} fab_state_t;

/** Process type for the chamber */
typedef enum {
    PROCESS_CRYSTAL_GROWTH,
    PROCESS_VAPOR_DEPOSITION,
    PROCESS_ALLOY_MELTING,
    PROCESS_FIBER_DRAWING,
    PROCESS_3D_PRINTING,
    PROCESS_CONTAINERLESS
} fab_process_type_t;

/** Fault codes */
typedef enum {
    FAB_FAULT_NONE = 0,
    FAB_FAULT_VACUUM_LOSS,
    FAB_FAULT_OVERTEMP,
    FAB_FAULT_UNDERTEMP,
    FAB_FAULT_THERMAL_RUNAWAY,
    FAB_FAULT_PRESSURE_SPIKE,
    FAB_FAULT_SENSOR_FAIL,
    FAB_FAULT_VIBRATION,
    FAB_FAULT_ATTITUDE_LOSS,
    FAB_FAULT_POWER_FAIL,
    FAB_FAULT_PRECURSOR_EMPTY,
    FAB_FAULT_TMR_DISAGREE,
    FAB_FAULT_GROWTH_DEFECT,
    FAB_FAULT_CONTAMINATION,
    FAB_FAULT_COMM_LOSS
} fab_fault_t;

/** TMR channel status */
typedef enum {
    TMR_CHAN_OK,
    TMR_CHAN_SUSPECT,
    TMR_CHAN_FAILED
} tmr_chan_status_t;

/** TMR voting result */
typedef enum {
    TMR_UNANIMOUS,
    TMR_MAJORITY,
    TMR_DISAGREE,
    TMR_ALL_FAILED
} tmr_result_t;

/** Sensor type for TMR */
typedef enum {
    SENSOR_TEMPERATURE,
    SENSOR_PRESSURE,
    SENSOR_FLOW_RATE,
    SENSOR_THICKNESS,
    SENSOR_COMPOSITION,
    SENSOR_VIBRATION,
    SENSOR_ATTITUDE
} fab_sensor_type_t;

/** TMR sensor reading structure */
typedef struct {
    float values[FAB_TMR_CHANNELS];
    tmr_chan_status_t status[FAB_TMR_CHANNELS];
    uint32_t timestamps[FAB_TMR_CHANNELS];
    float voted_value;
    tmr_result_t vote_result;
    fab_sensor_type_t type;
} tmr_sensor_t;

/** Chamber configuration */
typedef struct {
    uint8_t chamber_id;
    fab_process_type_t process;
    float target_temp_c;
    float target_pressure_torr;
    float hold_time_sec;
    bool atmosphere_control;
    char atmosphere_gas[16];
} fab_chamber_config_t;

/** Environment state */
typedef struct {
    float temperature_c;
    float pressure_torr;
    float g_level;
    float vibration_mg;
    float attitude_error_deg;
    bool vacuum_stable;
    bool thermal_stable;
    bool gravity_acceptable;
} fab_environment_t;

/** Process control parameters */
typedef struct {
    float setpoint;
    float kp;
    float ki;
    float kd;
    float integral;
    float prev_error;
    float output;
    float output_min;
    float output_max;
    uint32_t last_update_ms;
} pid_controller_t;

/** Material sample tracking */
typedef struct {
    uint32_t sample_id;
    fab_process_type_t process;
    float mass_mg;
    float thickness_um;
    float uniformity_pct;
    float defect_density;
    bool passed_qc;
    uint32_t start_time;
    uint32_t end_time;
} fab_sample_t;

/** Yield tracking statistics */
typedef struct {
    uint32_t total_samples;
    uint32_t passed_samples;
    uint32_t failed_samples;
    float avg_yield_pct;
    float avg_thickness_um;
    float avg_uniformity_pct;
    float material_consumed_g;
    float material_produced_g;
} yield_stats_t;

/** Chamber state */
typedef struct {
    uint8_t chamber_id;
    fab_state_t state;
    fab_process_type_t process;
    fab_fault_t active_fault;
    fab_environment_t env;
    pid_controller_t temp_pid;
    pid_controller_t pressure_pid;
    tmr_sensor_t temp_sensors;
    tmr_sensor_t pressure_sensors;
    tmr_sensor_t vibration_sensors;
    fab_sample_t current_sample;
    yield_stats_t yield;
    uint32_t process_start_ms;
    uint32_t state_start_ms;
    uint32_t fault_count;
    bool emergency_stop;
} fab_chamber_t;

/** Telemetry packet structure */
typedef struct {
    uint16_t apid;               /* CCSDS Application ID */
    uint16_t sequence;
    uint32_t timestamp;
    uint8_t chamber_id;
    uint8_t state;
    uint8_t fault_code;
    float temperature_c;
    float pressure_torr;
    float g_level;
    float yield_pct;
    float sample_thickness_um;
    uint8_t checksum;
} fab_telemetry_t;

/*===========================================================================*/
/* Module State                                                               */
/*===========================================================================*/

static fab_chamber_t g_chambers[FAB_MAX_CHAMBERS];
static uint8_t g_active_chamber_count = 0;
static bool g_module_initialized = false;
static uint16_t g_telemetry_sequence = 0;
static uint32_t g_uptime_ms = 0;

/* Simulated hardware registers */
static float g_heater_power[FAB_MAX_CHAMBERS];
static float g_pump_speed[FAB_MAX_CHAMBERS];
static bool g_chamber_door_closed[FAB_MAX_CHAMBERS];

/*===========================================================================*/
/* TMR Voting Functions                                                       */
/*===========================================================================*/

/**
 * @brief Perform TMR voting on sensor readings
 * @param sensor TMR sensor structure to process
 * @return Voted value or NaN if all failed
 */
static float tmr_vote(tmr_sensor_t *sensor) {
    if (!sensor) {
        return NAN;
    }

    uint8_t valid_count = 0;
    float valid_values[FAB_TMR_CHANNELS];
    
    /* Collect valid readings */
    for (uint8_t i = 0; i < FAB_TMR_CHANNELS; i++) {
        if (sensor->status[i] == TMR_CHAN_OK) {
            valid_values[valid_count++] = sensor->values[i];
        }
    }
    
    if (valid_count == 0) {
        sensor->vote_result = TMR_ALL_FAILED;
        sensor->voted_value = NAN;
        return NAN;
    }
    
    if (valid_count == 1) {
        /* Only one valid - use it but flag as suspect */
        sensor->vote_result = TMR_MAJORITY;
        sensor->voted_value = valid_values[0];
        return valid_values[0];
    }
    
    if (valid_count == 2) {
        /* Two valid - average if close, flag otherwise */
        float diff = fabsf(valid_values[0] - valid_values[1]);
        float threshold = (sensor->type == SENSOR_TEMPERATURE) ? 
                          TMR_TEMP_THRESHOLD_C : 
                          fabsf(valid_values[0]) * TMR_PRESSURE_THRESHOLD_PCT / 100.0f;
        
        if (diff <= threshold) {
            sensor->vote_result = TMR_MAJORITY;
            sensor->voted_value = (valid_values[0] + valid_values[1]) / 2.0f;
        } else {
            sensor->vote_result = TMR_DISAGREE;
            sensor->voted_value = (valid_values[0] + valid_values[1]) / 2.0f;
        }
        return sensor->voted_value;
    }
    
    /* Three valid - median voting */
    /* Sort for median */
    for (uint8_t i = 0; i < 2; i++) {
        for (uint8_t j = i + 1; j < 3; j++) {
            if (valid_values[i] > valid_values[j]) {
                float tmp = valid_values[i];
                valid_values[i] = valid_values[j];
                valid_values[j] = tmp;
            }
        }
    }
    
    float median = valid_values[1];
    float spread = valid_values[2] - valid_values[0];
    float threshold = (sensor->type == SENSOR_TEMPERATURE) ? 
                      TMR_TEMP_THRESHOLD_C : 
                      fabsf(median) * TMR_PRESSURE_THRESHOLD_PCT / 100.0f;
    
    if (spread <= threshold) {
        sensor->vote_result = TMR_UNANIMOUS;
    } else {
        sensor->vote_result = TMR_MAJORITY;
    }
    
    sensor->voted_value = median;
    return median;
}

/**
 * @brief Check TMR sensor health
 * @param sensor TMR sensor structure
 * @param current_time Current timestamp in ms
 */
static void tmr_check_health(tmr_sensor_t *sensor, uint32_t current_time) {
    if (!sensor) return;
    
    for (uint8_t i = 0; i < FAB_TMR_CHANNELS; i++) {
        /* Check for timeout */
        if ((current_time - sensor->timestamps[i]) > TMR_SENSOR_TIMEOUT_MS) {
            sensor->status[i] = TMR_CHAN_FAILED;
        }
        
        /* Check for out-of-range */
        if (sensor->status[i] == TMR_CHAN_OK) {
            if (sensor->type == SENSOR_TEMPERATURE) {
                if (sensor->values[i] < -273.0f || sensor->values[i] > 3000.0f) {
                    sensor->status[i] = TMR_CHAN_FAILED;
                }
            } else if (sensor->type == SENSOR_PRESSURE) {
                if (sensor->values[i] < 0.0f || sensor->values[i] > 1e6f) {
                    sensor->status[i] = TMR_CHAN_FAILED;
                }
            }
        }
    }
}

/*===========================================================================*/
/* PID Controller Functions                                                   */
/*===========================================================================*/

/**
 * @brief Initialize PID controller
 */
static void pid_init(pid_controller_t *pid, float kp, float ki, float kd,
                     float out_min, float out_max) {
    if (!pid) return;
    
    pid->setpoint = 0.0f;
    pid->kp = kp;
    pid->ki = ki;
    pid->kd = kd;
    pid->integral = 0.0f;
    pid->prev_error = 0.0f;
    pid->output = 0.0f;
    pid->output_min = out_min;
    pid->output_max = out_max;
    pid->last_update_ms = 0;
}

/**
 * @brief Update PID controller
 * @param pid Controller instance
 * @param measured Current process value
 * @param current_time Current time in ms
 * @return Controller output
 */
static float pid_update(pid_controller_t *pid, float measured, uint32_t current_time) {
    if (!pid) return 0.0f;
    
    float dt = (current_time - pid->last_update_ms) / 1000.0f;
    if (dt <= 0.0f || dt > 10.0f) {
        dt = 0.1f;  /* Default 100ms */
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
    
    /* Sum and clamp output */
    pid->output = p_term + i_term + d_term;
    if (pid->output > pid->output_max) pid->output = pid->output_max;
    if (pid->output < pid->output_min) pid->output = pid->output_min;
    
    pid->prev_error = error;
    pid->last_update_ms = current_time;
    
    return pid->output;
}

/**
 * @brief Reset PID controller state
 */
static void pid_reset(pid_controller_t *pid) {
    if (!pid) return;
    pid->integral = 0.0f;
    pid->prev_error = 0.0f;
    pid->output = 0.0f;
}

/*===========================================================================*/
/* Environment Monitoring                                                     */
/*===========================================================================*/

/**
 * @brief Read and vote temperature sensors
 * @param chamber Chamber instance
 * @return Voted temperature or NAN on failure
 */
static float read_temperature(fab_chamber_t *chamber) {
    if (!chamber) return NAN;
    
    /* Simulate reading from three sensors */
    float base_temp = chamber->env.temperature_c;
    
    for (uint8_t i = 0; i < FAB_TMR_CHANNELS; i++) {
        if (chamber->temp_sensors.status[i] != TMR_CHAN_FAILED) {
            /* Simulate slight variation between sensors */
            chamber->temp_sensors.values[i] = base_temp + (float)(i - 1) * 0.1f;
            chamber->temp_sensors.timestamps[i] = g_uptime_ms;
            chamber->temp_sensors.status[i] = TMR_CHAN_OK;
        }
    }
    
    chamber->temp_sensors.type = SENSOR_TEMPERATURE;
    tmr_check_health(&chamber->temp_sensors, g_uptime_ms);
    return tmr_vote(&chamber->temp_sensors);
}

/**
 * @brief Read and vote pressure sensors
 * @param chamber Chamber instance
 * @return Voted pressure or NAN on failure
 */
static float read_pressure(fab_chamber_t *chamber) {
    if (!chamber) return NAN;
    
    float base_pressure = chamber->env.pressure_torr;
    
    for (uint8_t i = 0; i < FAB_TMR_CHANNELS; i++) {
        if (chamber->pressure_sensors.status[i] != TMR_CHAN_FAILED) {
            chamber->pressure_sensors.values[i] = base_pressure * (1.0f + (float)(i - 1) * 0.01f);
            chamber->pressure_sensors.timestamps[i] = g_uptime_ms;
            chamber->pressure_sensors.status[i] = TMR_CHAN_OK;
        }
    }
    
    chamber->pressure_sensors.type = SENSOR_PRESSURE;
    tmr_check_health(&chamber->pressure_sensors, g_uptime_ms);
    return tmr_vote(&chamber->pressure_sensors);
}

/**
 * @brief Check environmental stability
 * @param chamber Chamber instance
 * @return true if environment is stable
 */
static bool check_environment_stable(fab_chamber_t *chamber) {
    if (!chamber) return false;
    
    /* Check vacuum */
    chamber->env.vacuum_stable = (chamber->env.pressure_torr <= VACUUM_WARNING_TORR);
    
    /* Check thermal stability */
    float temp_error = fabsf(chamber->env.temperature_c - chamber->temp_pid.setpoint);
    chamber->env.thermal_stable = (temp_error <= TEMP_STABILITY_THRESHOLD_C);
    
    /* Check microgravity */
    chamber->env.gravity_acceptable = (chamber->env.g_level <= MICROGRAVITY_THRESHOLD_G);
    
    return chamber->env.vacuum_stable && 
           chamber->env.thermal_stable && 
           chamber->env.gravity_acceptable;
}

/**
 * @brief Check for vibration disturbances
 * @param chamber Chamber instance
 * @return true if vibration is acceptable
 */
static bool check_vibration(fab_chamber_t *chamber) {
    if (!chamber) return false;
    
    /* Read vibration sensors with TMR */
    for (uint8_t i = 0; i < FAB_TMR_CHANNELS; i++) {
        if (chamber->vibration_sensors.status[i] != TMR_CHAN_FAILED) {
            chamber->vibration_sensors.values[i] = chamber->env.vibration_mg;
            chamber->vibration_sensors.timestamps[i] = g_uptime_ms;
            chamber->vibration_sensors.status[i] = TMR_CHAN_OK;
        }
    }
    
    chamber->vibration_sensors.type = SENSOR_VIBRATION;
    tmr_check_health(&chamber->vibration_sensors, g_uptime_ms);
    float voted_vib = tmr_vote(&chamber->vibration_sensors);
    
    return (voted_vib <= VIBRATION_THRESHOLD_MG);
}

/*===========================================================================*/
/* Safety Interlock Functions                                                 */
/*===========================================================================*/

/**
 * @brief Check for thermal runaway condition
 * @param chamber Chamber instance
 * @return true if thermal runaway detected
 */
static bool detect_thermal_runaway(fab_chamber_t *chamber) {
    if (!chamber) return false;
    
    static float prev_temps[FAB_MAX_CHAMBERS] = {0};
    static uint32_t prev_times[FAB_MAX_CHAMBERS] = {0};
    
    uint8_t id = chamber->chamber_id;
    if (id >= FAB_MAX_CHAMBERS) return false;
    
    /* Detect time reset (module re-init) and reset tracking state */
    if (g_uptime_ms < prev_times[id]) {
        prev_temps[id] = chamber->env.temperature_c;
        prev_times[id] = g_uptime_ms;
        return false;
    }
    
    if (prev_times[id] == 0) {
        prev_temps[id] = chamber->env.temperature_c;
        prev_times[id] = g_uptime_ms;
        return false;
    }
    
    float dt = (g_uptime_ms - prev_times[id]) / 1000.0f;
    if (dt < 0.1f) return false;
    
    float rate = (chamber->env.temperature_c - prev_temps[id]) / dt;
    
    prev_temps[id] = chamber->env.temperature_c;
    prev_times[id] = g_uptime_ms;
    
    /* Check for excessive heating rate */
    if (rate > TEMP_RATE_MAX_CS * 2.0f) {
        return true;
    }
    
    /* Check for temperature way above setpoint */
    if (chamber->env.temperature_c > chamber->temp_pid.setpoint + 100.0f) {
        return true;
    }
    
    return false;
}

/**
 * @brief Check all safety interlocks
 * @param chamber Chamber instance
 * @return Fault code if interlock triggered, FAB_FAULT_NONE otherwise
 */
static fab_fault_t check_safety_interlocks(fab_chamber_t *chamber) {
    if (!chamber) return FAB_FAULT_NONE;
    
    /* Already in fault state - don't re-check */
    if (chamber->state == FAB_STATE_ABORT) {
        return chamber->active_fault;
    }
    
    /* Read sensors to get fresh TMR voting results */
    read_temperature(chamber);
    read_pressure(chamber);
    
    /* TMR sensor disagreement */
    if (chamber->temp_sensors.vote_result == TMR_ALL_FAILED) {
        return FAB_FAULT_SENSOR_FAIL;
    }
    if (chamber->temp_sensors.vote_result == TMR_DISAGREE) {
        return FAB_FAULT_TMR_DISAGREE;
    }
    
    /* Temperature limits - check against max or excessive overshoot */
    /* Check absolute limit FIRST before thermal runaway (which checks rate and relative overshoot) */
    if (chamber->env.temperature_c > TEMP_HEATING_MAX_C) {
        return FAB_FAULT_OVERTEMP;
    }
    
    /* Thermal runaway - check rate and relative overshoot */
    if (detect_thermal_runaway(chamber)) {
        return FAB_FAULT_THERMAL_RUNAWAY;
    }
    
    /* Vacuum loss - check if we've progressed past vacuum pump phase */
    if (chamber->state >= FAB_STATE_HEATING &&
        chamber->env.pressure_torr > VACUUM_WARNING_TORR) {
        return FAB_FAULT_VACUUM_LOSS;
    }
    
    /* Vibration check */
    if (!check_vibration(chamber) && 
        (chamber->state == FAB_STATE_DEPOSITION || 
         chamber->state == FAB_STATE_GROWTH)) {
        return FAB_FAULT_VIBRATION;
    }
    
    /* Attitude check for precision processes */
    if (chamber->env.attitude_error_deg > ATTITUDE_STABILITY_DEG &&
        chamber->process == PROCESS_CRYSTAL_GROWTH) {
        return FAB_FAULT_ATTITUDE_LOSS;
    }
    
    return FAB_FAULT_NONE;
}

/**
 * @brief Execute emergency stop
 * @param chamber Chamber instance
 */
static void emergency_stop(fab_chamber_t *chamber) {
    if (!chamber) return;
    
    chamber->emergency_stop = true;
    
    /* Shut down heaters immediately */
    g_heater_power[chamber->chamber_id] = 0.0f;
    
    /* Stop any material feed */
    /* (simulation - would control valves here) */
    
    /* Transition to abort state */
    chamber->state = FAB_STATE_ABORT;
    chamber->state_start_ms = g_uptime_ms;
    
    /* Reset PID controllers */
    pid_reset(&chamber->temp_pid);
    pid_reset(&chamber->pressure_pid);
}

/*===========================================================================*/
/* State Machine Functions                                                    */
/*===========================================================================*/

/**
 * @brief Handle IDLE state
 */
static void state_idle(fab_chamber_t *chamber) {
    /* Maintain minimal monitoring */
    read_temperature(chamber);
    read_pressure(chamber);
    
    /* Heaters off */
    g_heater_power[chamber->chamber_id] = 0.0f;
}

/**
 * @brief Handle VACUUM_PUMP state
 */
static void state_vacuum_pump(fab_chamber_t *chamber) {
    /* Run vacuum pump */
    g_pump_speed[chamber->chamber_id] = 100.0f;  /* Full speed */
    
    /* Simulate pump-down - faster reduction for practical testing */
    if (chamber->env.pressure_torr > VACUUM_TARGET_TORR) {
        chamber->env.pressure_torr *= 0.5f;  /* 50% reduction per cycle */
    }
    
    /* Check for completion */
    if (chamber->env.pressure_torr <= VACUUM_TARGET_TORR) {
        chamber->state = FAB_STATE_HEATING;
        chamber->state_start_ms = g_uptime_ms;
    }
    
    /* Check for timeout */
    if ((g_uptime_ms - chamber->state_start_ms) > VACUUM_PUMP_TIMEOUT_MS) {
        chamber->active_fault = FAB_FAULT_VACUUM_LOSS;
        emergency_stop(chamber);
    }
}

/**
 * @brief Handle HEATING state
 */
static void state_heating(fab_chamber_t *chamber) {
    float current_temp = read_temperature(chamber);
    
    /* Update PID controller */
    float power = pid_update(&chamber->temp_pid, current_temp, g_uptime_ms);
    g_heater_power[chamber->chamber_id] = power;
    
    /* Simulate heating - realistic thermal model */
    float delta_t = power * 0.05f;  /* Max ~5°C per 100ms cycle = 50°C/s */
    chamber->env.temperature_c += delta_t;
    
    /* Check for temperature reached */
    float error = fabsf(chamber->temp_pid.setpoint - current_temp);
    if (error <= TEMP_STABILITY_THRESHOLD_C) {
        /* Transition based on process type */
        if (chamber->process == PROCESS_CRYSTAL_GROWTH) {
            chamber->state = FAB_STATE_GROWTH;
        } else {
            chamber->state = FAB_STATE_DEPOSITION;
        }
        chamber->state_start_ms = g_uptime_ms;
    }
}

/**
 * @brief Handle DEPOSITION state (vapor deposition, etc.)
 */
static void state_deposition(fab_chamber_t *chamber) {
    float current_temp = read_temperature(chamber);
    
    /* Maintain temperature */
    float power = pid_update(&chamber->temp_pid, current_temp, g_uptime_ms);
    g_heater_power[chamber->chamber_id] = power;
    
    /* Simulate deposition process */
    chamber->current_sample.thickness_um += DEPOSITION_RATE_NM_S / 1000.0f;
    
    /* Check for uniformity based on environment stability */
    if (check_environment_stable(chamber)) {
        chamber->current_sample.uniformity_pct = 99.5f;
    } else {
        chamber->current_sample.uniformity_pct *= 0.99f;
    }
    
    /* Check for target thickness (simplified - would come from config) */
    float target_thickness = 10.0f;  /* 10 µm target */
    if (chamber->current_sample.thickness_um >= target_thickness) {
        chamber->state = FAB_STATE_COOLING;
        chamber->state_start_ms = g_uptime_ms;
    }
}

/**
 * @brief Handle GROWTH state (crystal growth)
 */
static void state_growth(fab_chamber_t *chamber) {
    float current_temp = read_temperature(chamber);
    
    /* Very precise temperature control for crystal growth */
    float power = pid_update(&chamber->temp_pid, current_temp, g_uptime_ms);
    g_heater_power[chamber->chamber_id] = power;
    
    /* Check environment conditions */
    bool stable = check_environment_stable(chamber);
    
    /* Simulate crystal growth */
    float growth_rate = CRYSTAL_GROWTH_RATE_UM_HR / 3600.0f;  /* Per second */
    if (stable) {
        chamber->current_sample.thickness_um += growth_rate;
        /* Low defect density when stable */
        chamber->current_sample.defect_density = 0.0001f;
    } else {
        /* Reduced growth rate and more defects when unstable */
        chamber->current_sample.thickness_um += growth_rate * 0.5f;
        chamber->current_sample.defect_density += 0.001f;
    }
    
    /* Check for target crystal size */
    float target_size = 50.0f;  /* 50 µm crystal */
    if (chamber->current_sample.thickness_um >= target_size) {
        chamber->state = FAB_STATE_COOLING;
        chamber->state_start_ms = g_uptime_ms;
    }
}

/**
 * @brief Handle COOLING state
 */
static void state_cooling(fab_chamber_t *chamber) {
    /* Turn off heaters */
    g_heater_power[chamber->chamber_id] = 0.0f;
    
    /* Simulate cooling */
    float ambient = TEMP_AMBIENT_C;
    float cooling_rate = 0.1f;  /* °C per cycle */
    
    if (chamber->env.temperature_c > ambient) {
        chamber->env.temperature_c -= cooling_rate;
    }
    
    /* Check for safe handling temperature */
    if (chamber->env.temperature_c <= 50.0f) {
        chamber->state = FAB_STATE_QUALITY_CHECK;
        chamber->state_start_ms = g_uptime_ms;
    }
}

/**
 * @brief Handle QUALITY_CHECK state
 */
static void state_quality_check(fab_chamber_t *chamber) {
    fab_sample_t *sample = &chamber->current_sample;
    
    /* Perform quality checks */
    bool qc_passed = true;
    
    /* Check thickness within tolerance */
    /* (would compare to target from config) */
    
    /* Check uniformity */
    if (sample->uniformity_pct < DEPOSITION_UNIFORMITY_PCT) {
        qc_passed = false;
    }
    
    /* Check defect density for crystals */
    if (chamber->process == PROCESS_CRYSTAL_GROWTH) {
        if (sample->defect_density > CRYSTAL_DEFECT_THRESHOLD) {
            qc_passed = false;
            chamber->active_fault = FAB_FAULT_GROWTH_DEFECT;
        }
    }
    
    sample->passed_qc = qc_passed;
    sample->end_time = g_uptime_ms;
    
    /* Update yield statistics */
    chamber->yield.total_samples++;
    if (qc_passed) {
        chamber->yield.passed_samples++;
    } else {
        chamber->yield.failed_samples++;
    }
    chamber->yield.avg_yield_pct = 
        (float)chamber->yield.passed_samples / (float)chamber->yield.total_samples * 100.0f;
    
    /* Transition to unloading */
    chamber->state = FAB_STATE_UNLOADING;
    chamber->state_start_ms = g_uptime_ms;
}

/**
 * @brief Handle UNLOADING state
 */
static void state_unloading(fab_chamber_t *chamber) {
    /* Vent chamber (if needed) */
    /* Open door (simulation) */
    g_chamber_door_closed[chamber->chamber_id] = false;
    
    /* Sample removed - return to idle */
    chamber->state = FAB_STATE_IDLE;
    chamber->state_start_ms = g_uptime_ms;
    
    /* Reset sample tracking */
    memset(&chamber->current_sample, 0, sizeof(fab_sample_t));
}

/**
 * @brief Handle ABORT state
 */
static void state_abort(fab_chamber_t *chamber) {
    /* Ensure heaters are off */
    g_heater_power[chamber->chamber_id] = 0.0f;
    
    /* Continue cooling */
    if (chamber->env.temperature_c > TEMP_AMBIENT_C) {
        chamber->env.temperature_c -= 1.0f;  /* Fast cooling */
    }
    
    /* Wait for safe conditions before allowing reset */
    if (chamber->env.temperature_c <= 50.0f &&
        (g_uptime_ms - chamber->state_start_ms) > PROCESS_ABORT_TIMEOUT_MS) {
        /* Can be reset by operator */
        chamber->state = FAB_STATE_MAINTENANCE;
    }
}

/**
 * @brief Main state machine update
 * @param chamber Chamber instance
 */
static void update_state_machine(fab_chamber_t *chamber) {
    if (!chamber) return;
    
    /* Check safety interlocks first */
    fab_fault_t fault = check_safety_interlocks(chamber);
    if (fault != FAB_FAULT_NONE && chamber->state != FAB_STATE_ABORT) {
        chamber->active_fault = fault;
        chamber->fault_count++;
        emergency_stop(chamber);
        return;
    }
    
    /* Execute state handler */
    switch (chamber->state) {
        case FAB_STATE_IDLE:
            state_idle(chamber);
            break;
        case FAB_STATE_VACUUM_PUMP:
            state_vacuum_pump(chamber);
            break;
        case FAB_STATE_HEATING:
            state_heating(chamber);
            break;
        case FAB_STATE_DEPOSITION:
            state_deposition(chamber);
            break;
        case FAB_STATE_GROWTH:
            state_growth(chamber);
            break;
        case FAB_STATE_COOLING:
            state_cooling(chamber);
            break;
        case FAB_STATE_QUALITY_CHECK:
            state_quality_check(chamber);
            break;
        case FAB_STATE_UNLOADING:
            state_unloading(chamber);
            break;
        case FAB_STATE_ABORT:
            state_abort(chamber);
            break;
        case FAB_STATE_INITIALIZING:
        case FAB_STATE_MAINTENANCE:
        default:
            /* Wait states - no action */
            break;
    }
}

/*===========================================================================*/
/* Telemetry Functions                                                        */
/*===========================================================================*/

/**
 * @brief Calculate simple checksum for telemetry
 */
static uint8_t calculate_checksum(const uint8_t *data, size_t len) {
    uint8_t sum = 0;
    for (size_t i = 0; i < len - 1; i++) {
        sum ^= data[i];
    }
    return sum;
}

/**
 * @brief Generate CCSDS-compatible telemetry packet
 * @param chamber Chamber instance
 * @param packet Output packet buffer
 * @return 0 on success, -1 on error
 */
static int generate_telemetry(fab_chamber_t *chamber, fab_telemetry_t *packet) {
    if (!chamber || !packet) return -1;
    
    memset(packet, 0, sizeof(fab_telemetry_t));
    
    /* CCSDS header */
    packet->apid = 0x0100 + chamber->chamber_id;  /* Application ID */
    packet->sequence = g_telemetry_sequence++;
    packet->timestamp = g_uptime_ms;
    
    /* Chamber data */
    packet->chamber_id = chamber->chamber_id;
    packet->state = (uint8_t)chamber->state;
    packet->fault_code = (uint8_t)chamber->active_fault;
    
    /* Environment data */
    packet->temperature_c = chamber->env.temperature_c;
    packet->pressure_torr = chamber->env.pressure_torr;
    packet->g_level = chamber->env.g_level;
    
    /* Yield data */
    packet->yield_pct = chamber->yield.avg_yield_pct;
    packet->sample_thickness_um = chamber->current_sample.thickness_um;
    
    /* Checksum */
    packet->checksum = calculate_checksum((uint8_t *)packet, sizeof(fab_telemetry_t));
    
    return 0;
}

/*===========================================================================*/
/* Public API Functions                                                       */
/*===========================================================================*/

/**
 * @brief Initialize the orbital fabrication module
 * @return 0 on success, -1 on error
 */
int orbital_fab_init(void) {
    if (g_module_initialized) {
        return -1;  /* Already initialized */
    }
    
    memset(g_chambers, 0, sizeof(g_chambers));
    memset(g_heater_power, 0, sizeof(g_heater_power));
    memset(g_pump_speed, 0, sizeof(g_pump_speed));
    
    /* Reset uptime for clean test state */
    g_uptime_ms = 0;
    
    for (uint8_t i = 0; i < FAB_MAX_CHAMBERS; i++) {
        g_chambers[i].chamber_id = i;
        g_chambers[i].state = FAB_STATE_IDLE;
        g_chambers[i].env.temperature_c = TEMP_AMBIENT_C;
        g_chambers[i].env.pressure_torr = 760.0f;  /* Atmospheric */
        g_chambers[i].env.g_level = 1e-5f;  /* Microgravity */
        g_chambers[i].env.vibration_mg = 1.0f;
        g_chambers[i].env.attitude_error_deg = 0.001f;
        g_chamber_door_closed[i] = false;
        
        /* Initialize TMR sensors */
        for (uint8_t j = 0; j < FAB_TMR_CHANNELS; j++) {
            g_chambers[i].temp_sensors.status[j] = TMR_CHAN_OK;
            g_chambers[i].pressure_sensors.status[j] = TMR_CHAN_OK;
            g_chambers[i].vibration_sensors.status[j] = TMR_CHAN_OK;
        }
        
        /* Initialize PID controllers */
        pid_init(&g_chambers[i].temp_pid, 0.5f, 0.1f, 0.05f, 0.0f, 100.0f);
        pid_init(&g_chambers[i].pressure_pid, 1.0f, 0.5f, 0.0f, 0.0f, 100.0f);
    }
    
    g_active_chamber_count = 0;
    g_telemetry_sequence = 0;
    g_module_initialized = true;
    
    return 0;
}

/**
 * @brief Configure a fabrication chamber
 * @param config Chamber configuration
 * @return 0 on success, -1 on error
 */
int orbital_fab_configure(const fab_chamber_config_t *config) {
    if (!g_module_initialized || !config) {
        return -1;
    }
    
    if (config->chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    fab_chamber_t *chamber = &g_chambers[config->chamber_id];
    
    /* Only configure in IDLE or MAINTENANCE state */
    if (chamber->state != FAB_STATE_IDLE && 
        chamber->state != FAB_STATE_MAINTENANCE) {
        return -1;
    }
    
    chamber->process = config->process;
    chamber->temp_pid.setpoint = config->target_temp_c;
    chamber->pressure_pid.setpoint = config->target_pressure_torr;
    
    g_chamber_door_closed[config->chamber_id] = true;
    
    return 0;
}

/**
 * @brief Start fabrication process
 * @param chamber_id Chamber to start
 * @return 0 on success, -1 on error
 */
int orbital_fab_start(uint8_t chamber_id) {
    if (!g_module_initialized || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    fab_chamber_t *chamber = &g_chambers[chamber_id];
    
    /* Can only start from IDLE */
    if (chamber->state != FAB_STATE_IDLE) {
        return -1;
    }
    
    /* Check door closed */
    if (!g_chamber_door_closed[chamber_id]) {
        return -1;
    }
    
    /* Initialize sample tracking */
    chamber->current_sample.sample_id = g_uptime_ms;  /* Use time as ID */
    chamber->current_sample.process = chamber->process;
    chamber->current_sample.start_time = g_uptime_ms;
    chamber->current_sample.thickness_um = 0.0f;
    chamber->current_sample.uniformity_pct = 100.0f;
    chamber->current_sample.defect_density = 0.0f;
    chamber->current_sample.passed_qc = false;
    
    /* Clear any previous fault */
    chamber->active_fault = FAB_FAULT_NONE;
    chamber->emergency_stop = false;
    
    /* Start vacuum pump-down */
    chamber->state = FAB_STATE_VACUUM_PUMP;
    chamber->state_start_ms = g_uptime_ms;
    chamber->process_start_ms = g_uptime_ms;
    
    return 0;
}

/**
 * @brief Abort fabrication process
 * @param chamber_id Chamber to abort
 * @return 0 on success, -1 on error
 */
int orbital_fab_abort(uint8_t chamber_id) {
    if (!g_module_initialized || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    fab_chamber_t *chamber = &g_chambers[chamber_id];
    
    if (chamber->state == FAB_STATE_IDLE) {
        return 0;  /* Already idle */
    }
    
    chamber->active_fault = FAB_FAULT_NONE;  /* Operator-initiated abort */
    emergency_stop(chamber);
    
    return 0;
}

/**
 * @brief Reset chamber after fault
 * @param chamber_id Chamber to reset
 * @return 0 on success, -1 on error
 */
int orbital_fab_reset(uint8_t chamber_id) {
    if (!g_module_initialized || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    fab_chamber_t *chamber = &g_chambers[chamber_id];
    
    /* Can only reset from MAINTENANCE state */
    if (chamber->state != FAB_STATE_MAINTENANCE) {
        return -1;
    }
    
    /* Check temperature is safe */
    if (chamber->env.temperature_c > 50.0f) {
        return -1;
    }
    
    chamber->state = FAB_STATE_IDLE;
    chamber->active_fault = FAB_FAULT_NONE;
    chamber->emergency_stop = false;
    
    /* Reset PID controllers */
    pid_reset(&chamber->temp_pid);
    pid_reset(&chamber->pressure_pid);
    
    return 0;
}

/**
 * @brief Update module (call periodically)
 * @param elapsed_ms Milliseconds since last update
 */
void orbital_fab_update(uint32_t elapsed_ms) {
    if (!g_module_initialized) return;
    
    g_uptime_ms += elapsed_ms;
    
    for (uint8_t i = 0; i < FAB_MAX_CHAMBERS; i++) {
        update_state_machine(&g_chambers[i]);
    }
}

/**
 * @brief Get chamber state
 * @param chamber_id Chamber to query
 * @param state Output state
 * @return 0 on success, -1 on error
 */
int orbital_fab_get_state(uint8_t chamber_id, fab_state_t *state) {
    if (!g_module_initialized || !state || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    *state = g_chambers[chamber_id].state;
    return 0;
}

/**
 * @brief Get chamber fault code
 * @param chamber_id Chamber to query
 * @param fault Output fault code
 * @return 0 on success, -1 on error
 */
int orbital_fab_get_fault(uint8_t chamber_id, fab_fault_t *fault) {
    if (!g_module_initialized || !fault || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    *fault = g_chambers[chamber_id].active_fault;
    return 0;
}

/**
 * @brief Get environment data
 * @param chamber_id Chamber to query
 * @param env Output environment structure
 * @return 0 on success, -1 on error
 */
int orbital_fab_get_environment(uint8_t chamber_id, fab_environment_t *env) {
    if (!g_module_initialized || !env || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    *env = g_chambers[chamber_id].env;
    return 0;
}

/**
 * @brief Get yield statistics
 * @param chamber_id Chamber to query
 * @param yield Output yield stats
 * @return 0 on success, -1 on error
 */
int orbital_fab_get_yield(uint8_t chamber_id, yield_stats_t *yield) {
    if (!g_module_initialized || !yield || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    *yield = g_chambers[chamber_id].yield;
    return 0;
}

/**
 * @brief Get current sample info
 * @param chamber_id Chamber to query
 * @param sample Output sample structure
 * @return 0 on success, -1 on error
 */
int orbital_fab_get_sample(uint8_t chamber_id, fab_sample_t *sample) {
    if (!g_module_initialized || !sample || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    *sample = g_chambers[chamber_id].current_sample;
    return 0;
}

/**
 * @brief Generate telemetry packet
 * @param chamber_id Chamber to report
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written or -1 on error
 */
int orbital_fab_get_telemetry(uint8_t chamber_id, uint8_t *buffer, size_t max_len) {
    if (!g_module_initialized || !buffer || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    if (max_len < sizeof(fab_telemetry_t)) {
        return -1;
    }
    
    fab_telemetry_t packet;
    if (generate_telemetry(&g_chambers[chamber_id], &packet) != 0) {
        return -1;
    }
    
    memcpy(buffer, &packet, sizeof(fab_telemetry_t));
    return (int)sizeof(fab_telemetry_t);
}

/**
 * @brief Inject sensor fault for testing
 * @param chamber_id Chamber to inject
 * @param sensor_type Which sensor type
 * @param channel TMR channel (0-2)
 * @return 0 on success, -1 on error
 */
int orbital_fab_inject_sensor_fault(uint8_t chamber_id, 
                                     fab_sensor_type_t sensor_type,
                                     uint8_t channel) {
    if (!g_module_initialized || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    if (channel >= FAB_TMR_CHANNELS) {
        return -1;
    }
    
    fab_chamber_t *chamber = &g_chambers[chamber_id];
    
    switch (sensor_type) {
        case SENSOR_TEMPERATURE:
            chamber->temp_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        case SENSOR_PRESSURE:
            chamber->pressure_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        case SENSOR_VIBRATION:
            chamber->vibration_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        default:
            return -1;
    }
    
    return 0;
}

/**
 * @brief Set environment parameter for testing
 * @param chamber_id Chamber to modify
 * @param param Parameter name
 * @param value New value
 * @return 0 on success, -1 on error
 */
int orbital_fab_set_environment(uint8_t chamber_id, const char *param, float value) {
    if (!g_module_initialized || !param || chamber_id >= FAB_MAX_CHAMBERS) {
        return -1;
    }
    
    fab_chamber_t *chamber = &g_chambers[chamber_id];
    
    if (strcmp(param, "temperature") == 0) {
        chamber->env.temperature_c = value;
    } else if (strcmp(param, "pressure") == 0) {
        chamber->env.pressure_torr = value;
    } else if (strcmp(param, "g_level") == 0) {
        chamber->env.g_level = value;
    } else if (strcmp(param, "vibration") == 0) {
        chamber->env.vibration_mg = value;
    } else if (strcmp(param, "attitude") == 0) {
        chamber->env.attitude_error_deg = value;
    } else {
        return -1;
    }
    
    return 0;
}

/**
 * @brief Shutdown the orbital fabrication module
 */
void orbital_fab_shutdown(void) {
    if (!g_module_initialized) return;
    
    /* Emergency stop all chambers */
    for (uint8_t i = 0; i < FAB_MAX_CHAMBERS; i++) {
        if (g_chambers[i].state != FAB_STATE_IDLE) {
            emergency_stop(&g_chambers[i]);
        }
    }
    
    g_module_initialized = false;
}
