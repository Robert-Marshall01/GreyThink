/**
 * @file hyperloop_spotlight.c
 * @brief Hyperloop Pod Control & Safety Spotlight Implementation
 * 
 * Production-grade implementation demonstrating embedded high-speed
 * transportation systems for hyperloop pod control. Features:
 * 
 * - PID-controlled acceleration and braking profiles
 * - Magnetic levitation gap control with TMR sensing
 * - Cabin pressure stabilization for passenger comfort
 * - Multi-layer safety interlock system
 * - Emergency stop and depressurization handling
 * - Real-time telemetry packet generation
 * - Passenger comfort metrics (g-force, jerk limiting)
 * 
 * Standards:
 * - EN 50126 (Railway RAMS)
 * - IEC 62278 (Safety-related software)
 * - ASIL-D adapted for transit (critical safety)
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

#define POD_MAX_UNITS               4           /* Maximum pods in system */
#define POD_TMR_CHANNELS            3           /* Triple redundancy */
#define POD_TELEMETRY_SIZE          64          /* Telemetry packet size */

/* Operating limits */
#define POD_MAX_SPEED_MPS           350.0f      /* ~1260 km/h */
#define POD_CRUISE_SPEED_MPS        300.0f      /* Normal cruise velocity */
#define POD_MAX_ACCEL_MPS2          5.0f        /* 0.5g acceleration limit */
#define POD_MAX_DECEL_MPS2          10.0f       /* 1g braking limit */
#define POD_EMERGENCY_DECEL_MPS2    15.0f       /* Emergency braking (1.5g) */
#define POD_MAX_JERK_MPS3           2.0f        /* Jerk limit for comfort */

/* Levitation parameters */
#define LEVITATION_GAP_MM           15.0f       /* Nominal levitation gap */
#define LEVITATION_MIN_GAP_MM       5.0f        /* Minimum safe gap */
#define LEVITATION_MAX_GAP_MM       25.0f       /* Maximum before touchdown */
#define LEVITATION_CURRENT_MAX_A    500.0f      /* Maximum magnet current */

/* Pressure parameters */
#define CABIN_PRESSURE_KPA          101.3f      /* Sea-level equivalent */
#define CABIN_PRESSURE_MIN_KPA      75.0f       /* Minimum safe pressure */
#define CABIN_PRESSURE_MAX_KPA      110.0f      /* Maximum pressure */
#define TUBE_PRESSURE_NOMINAL_PA    100.0f      /* Near-vacuum tube */
#define TUBE_PRESSURE_WARNING_PA    500.0f      /* Pressure rising */
#define TUBE_PRESSURE_BREACH_PA     5000.0f     /* Major breach */

/* Environmental limits */
#define POD_MOTOR_TEMP_MAX_C        120.0f      /* Motor overtemp */
#define POD_BRAKE_TEMP_MAX_C        400.0f      /* Brake overtemp */
#define POD_CABIN_TEMP_MIN_C        18.0f       /* Minimum cabin temp */
#define POD_CABIN_TEMP_MAX_C        28.0f       /* Maximum cabin temp */

/* Timing */
#define POD_DOOR_SEAL_TIME_MS       3000        /* Door seal verification */
#define POD_LEVITATE_TIME_MS        2000        /* Levitation startup */
#define POD_DOCK_TIME_MS            5000        /* Docking sequence */

/* PID controller gains */
#define PID_KP_SPEED                0.5f
#define PID_KI_SPEED                0.1f
#define PID_KD_SPEED                0.05f
#define PID_KP_LEVITATION           2.0f
#define PID_KI_LEVITATION           0.5f
#define PID_KD_LEVITATION           0.2f
#define PID_KP_PRESSURE             0.3f
#define PID_KI_PRESSURE             0.1f
#define PID_KD_PRESSURE             0.02f

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Pod operational state */
typedef enum {
    POD_STATE_IDLE,
    POD_STATE_BOARDING,
    POD_STATE_SEALED,
    POD_STATE_LEVITATING,
    POD_STATE_ACCELERATING,
    POD_STATE_CRUISING,
    POD_STATE_DECELERATING,
    POD_STATE_LANDING,
    POD_STATE_DOCKING,
    POD_STATE_EMERGENCY,
    POD_STATE_MAINTENANCE
} pod_state_t;

/** Pod fault codes */
typedef enum {
    POD_FAULT_NONE = 0,
    POD_FAULT_LEVITATION,
    POD_FAULT_PROPULSION,
    POD_FAULT_BRAKING,
    POD_FAULT_CABIN_PRESSURE,
    POD_FAULT_TUBE_PRESSURE,
    POD_FAULT_GUIDANCE,
    POD_FAULT_POWER,
    POD_FAULT_COMMS,
    POD_FAULT_DOOR_SEAL,
    POD_FAULT_THERMAL,
    POD_FAULT_SENSOR_FAIL
} pod_fault_t;

/** Safety event type */
typedef enum {
    SAFETY_EVENT_NONE,
    SAFETY_EVENT_DECEL_HIGH,
    SAFETY_EVENT_PRESSURE_CHANGE,
    SAFETY_EVENT_VIBRATION,
    SAFETY_EVENT_LEVITATION_FAULT,
    SAFETY_EVENT_BRAKE_FAILURE,
    SAFETY_EVENT_TUBE_BREACH,
    SAFETY_EVENT_EMERGENCY_STOP
} safety_event_t;

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

/** TMR sensor set */
typedef struct {
    float values[POD_TMR_CHANNELS];
    uint32_t timestamps[POD_TMR_CHANNELS];
    tmr_status_t status[POD_TMR_CHANNELS];
    tmr_vote_t vote_result;
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

/** Pod kinematics */
typedef struct {
    float position_m;
    float velocity_mps;
    float acceleration_mps2;
    float jerk_mps3;
    float prev_velocity_mps;
    float prev_acceleration_mps2;
} pod_kinematics_t;

/** Levitation state */
typedef struct {
    float gap_mm;
    float current_a;
    float temperature_c;
    bool active;
    bool stable;
} levitation_t;

/** Cabin environment */
typedef struct {
    float pressure_kpa;
    float temperature_c;
    float humidity_pct;
    float o2_pct;
    float co2_ppm;
} cabin_env_t;

/** Pod health metrics */
typedef struct {
    float battery_soc_pct;
    float motor_temp_c;
    float brake_temp_c;
    float brake_pad_pct;
    uint32_t distance_m;
    uint32_t trip_count;
} pod_health_t;

/** Tube environment */
typedef struct {
    float pressure_pa;
    float temperature_c;
    bool breach_detected;
    uint32_t section_id;
} tube_env_t;

/** Safety status */
typedef struct {
    safety_event_t active_events[4];
    uint8_t event_count;
    float g_force_longitudinal;
    float g_force_lateral;
    float g_force_vertical;
    bool emergency_active;
    bool evacuation_needed;
} safety_status_t;

/** Pod unit state */
typedef struct {
    uint8_t pod_id;
    pod_state_t state;
    pod_fault_t fault;
    uint32_t fault_count;
    
    /* Kinematics */
    pod_kinematics_t kinematics;
    float target_velocity_mps;
    float target_position_m;
    float track_length_m;
    
    /* Levitation */
    levitation_t levitation;
    tmr_sensor_t gap_sensors;
    
    /* Cabin */
    cabin_env_t cabin;
    tmr_sensor_t cabin_pressure_sensors;
    bool door_sealed;
    uint8_t passenger_count;
    
    /* Tube */
    tube_env_t tube;
    tmr_sensor_t tube_pressure_sensors;
    
    /* Health */
    pod_health_t health;
    
    /* Safety */
    safety_status_t safety;
    
    /* Controllers */
    pid_controller_t speed_pid;
    pid_controller_t levitation_pid;
    pid_controller_t pressure_pid;
    
    /* Timing */
    uint32_t state_start_ms;
    uint32_t trip_start_ms;
    
} pod_unit_t;

/*===========================================================================*/
/* Static Variables                                                           */
/*===========================================================================*/

static pod_unit_t g_pods[POD_MAX_UNITS];
static bool g_module_initialized = false;
static uint32_t g_uptime_ms = 0;
static uint16_t g_telemetry_sequence = 0;

/* Simulated actuator outputs */
static float g_motor_power[POD_MAX_UNITS];
static float g_brake_force[POD_MAX_UNITS];
static float g_levitation_current[POD_MAX_UNITS];

/*===========================================================================*/
/* PID Controller Functions                                                   */
/*===========================================================================*/

static void pid_init(pid_controller_t *pid, float kp, float ki, float kd,
                     float out_min, float out_max) {
    pid->kp = kp;
    pid->ki = ki;
    pid->kd = kd;
    pid->setpoint = 0.0f;
    pid->prev_error = 0.0f;
    pid->integral = 0.0f;
    pid->output = 0.0f;
    pid->output_min = out_min;
    pid->output_max = out_max;
    pid->last_update_ms = 0;
}

static float pid_update(pid_controller_t *pid, float measured, uint32_t now_ms) {
    float dt = (now_ms - pid->last_update_ms) / 1000.0f;
    if (dt <= 0.0f || dt > 1.0f) dt = 0.01f;
    
    float error = pid->setpoint - measured;
    
    /* Anti-windup: only integrate if output not saturated */
    if (pid->output > pid->output_min && pid->output < pid->output_max) {
        pid->integral += error * dt;
    }
    
    /* Clamp integral */
    float max_integral = (pid->output_max - pid->output_min) / pid->ki;
    if (pid->integral > max_integral) pid->integral = max_integral;
    if (pid->integral < -max_integral) pid->integral = -max_integral;
    
    float derivative = (error - pid->prev_error) / dt;
    
    pid->output = pid->kp * error + pid->ki * pid->integral + pid->kd * derivative;
    
    if (pid->output > pid->output_max) pid->output = pid->output_max;
    if (pid->output < pid->output_min) pid->output = pid->output_min;
    
    pid->prev_error = error;
    pid->last_update_ms = now_ms;
    
    return pid->output;
}

/*===========================================================================*/
/* TMR Sensor Functions                                                       */
/*===========================================================================*/

static float tmr_vote(tmr_sensor_t *sensor) {
    float values[POD_TMR_CHANNELS];
    uint8_t valid_count = 0;
    
    for (uint8_t i = 0; i < POD_TMR_CHANNELS; i++) {
        if (sensor->status[i] == TMR_CHAN_OK) {
            values[valid_count++] = sensor->values[i];
        }
    }
    
    if (valid_count == 0) {
        sensor->vote_result = TMR_ALL_FAILED;
        return 0.0f;
    }
    
    if (valid_count == 1) {
        sensor->vote_result = TMR_DISAGREE;
        return values[0];
    }
    
    if (valid_count == 2) {
        sensor->vote_result = TMR_MAJORITY;
        return (values[0] + values[1]) / 2.0f;
    }
    
    /* Sort for median */
    for (uint8_t i = 0; i < 2; i++) {
        for (uint8_t j = i + 1; j < 3; j++) {
            if (values[i] > values[j]) {
                float tmp = values[i];
                values[i] = values[j];
                values[j] = tmp;
            }
        }
    }
    
    /* Check agreement */
    float spread = values[2] - values[0];
    float avg = (values[0] + values[1] + values[2]) / 3.0f;
    
    if (spread < avg * 0.1f) {
        sensor->vote_result = TMR_ALL_AGREE;
    } else {
        sensor->vote_result = TMR_MAJORITY;
    }
    
    return values[1];  /* Median */
}

static void read_gap_sensors(pod_unit_t *pod) {
    /* Simulate TMR gap sensor readings */
    for (uint8_t i = 0; i < POD_TMR_CHANNELS; i++) {
        if (pod->gap_sensors.status[i] == TMR_CHAN_OK) {
            pod->gap_sensors.values[i] = pod->levitation.gap_mm + 
                                          ((float)(g_uptime_ms % 10) - 5.0f) * 0.1f;
            pod->gap_sensors.timestamps[i] = g_uptime_ms;
        }
    }
    pod->levitation.gap_mm = tmr_vote(&pod->gap_sensors);
}

static void read_cabin_pressure(pod_unit_t *pod) {
    for (uint8_t i = 0; i < POD_TMR_CHANNELS; i++) {
        if (pod->cabin_pressure_sensors.status[i] == TMR_CHAN_OK) {
            pod->cabin_pressure_sensors.values[i] = pod->cabin.pressure_kpa +
                                                     ((float)(g_uptime_ms % 10) - 5.0f) * 0.01f;
            pod->cabin_pressure_sensors.timestamps[i] = g_uptime_ms;
        }
    }
    pod->cabin.pressure_kpa = tmr_vote(&pod->cabin_pressure_sensors);
}

static void read_tube_pressure(pod_unit_t *pod) {
    for (uint8_t i = 0; i < POD_TMR_CHANNELS; i++) {
        if (pod->tube_pressure_sensors.status[i] == TMR_CHAN_OK) {
            pod->tube_pressure_sensors.values[i] = pod->tube.pressure_pa +
                                                    ((float)(g_uptime_ms % 10) - 5.0f) * 1.0f;
            pod->tube_pressure_sensors.timestamps[i] = g_uptime_ms;
        }
    }
    pod->tube.pressure_pa = tmr_vote(&pod->tube_pressure_sensors);
}

/*===========================================================================*/
/* Safety Interlock Functions                                                 */
/*===========================================================================*/

static void emergency_stop(pod_unit_t *pod) {
    pod->state = POD_STATE_EMERGENCY;
    pod->state_start_ms = g_uptime_ms;
    
    /* Maximum braking */
    g_brake_force[pod->pod_id] = 100.0f;
    g_motor_power[pod->pod_id] = 0.0f;
    
    /* Maintain levitation for safety */
    pod->levitation.active = true;
    
    /* Record event */
    if (pod->safety.event_count < 4) {
        pod->safety.active_events[pod->safety.event_count++] = SAFETY_EVENT_EMERGENCY_STOP;
    }
    pod->safety.emergency_active = true;
}

static pod_fault_t check_safety_interlocks(pod_unit_t *pod) {
    if (!pod) return POD_FAULT_NONE;
    
    /* Only check during active operation */
    if (pod->state == POD_STATE_IDLE || 
        pod->state == POD_STATE_MAINTENANCE ||
        pod->state == POD_STATE_EMERGENCY) {
        return POD_FAULT_NONE;
    }
    
    /* Levitation gap check */
    if (pod->levitation.active) {
        if (pod->levitation.gap_mm < LEVITATION_MIN_GAP_MM ||
            pod->levitation.gap_mm > LEVITATION_MAX_GAP_MM) {
            return POD_FAULT_LEVITATION;
        }
    }
    
    /* Cabin pressure check */
    if (pod->cabin.pressure_kpa < CABIN_PRESSURE_MIN_KPA) {
        return POD_FAULT_CABIN_PRESSURE;
    }
    
    /* Tube pressure check */
    if (pod->tube.pressure_pa > TUBE_PRESSURE_BREACH_PA) {
        pod->tube.breach_detected = true;
        return POD_FAULT_TUBE_PRESSURE;
    }
    
    /* Motor temperature check */
    if (pod->health.motor_temp_c > POD_MOTOR_TEMP_MAX_C) {
        return POD_FAULT_THERMAL;
    }
    
    /* Brake temperature check */
    if (pod->health.brake_temp_c > POD_BRAKE_TEMP_MAX_C) {
        return POD_FAULT_BRAKING;
    }
    
    /* TMR sensor failure check */
    if (pod->gap_sensors.vote_result == TMR_ALL_FAILED ||
        pod->cabin_pressure_sensors.vote_result == TMR_ALL_FAILED ||
        pod->tube_pressure_sensors.vote_result == TMR_ALL_FAILED) {
        return POD_FAULT_SENSOR_FAIL;
    }
    
    /* Door seal check during travel */
    if (pod->state >= POD_STATE_LEVITATING && 
        pod->state <= POD_STATE_DECELERATING) {
        if (!pod->door_sealed) {
            return POD_FAULT_DOOR_SEAL;
        }
    }
    
    return POD_FAULT_NONE;
}

static void update_g_forces(pod_unit_t *pod) {
    /* Calculate g-forces from acceleration */
    pod->safety.g_force_longitudinal = pod->kinematics.acceleration_mps2 / 9.81f;
    pod->safety.g_force_lateral = 0.0f;  /* Simulated */
    pod->safety.g_force_vertical = 1.0f + (pod->levitation.gap_mm - LEVITATION_GAP_MM) * 0.01f;
    
    /* Check for excessive g-forces */
    if (fabsf(pod->safety.g_force_longitudinal) > 1.5f) {
        if (pod->safety.event_count < 4) {
            pod->safety.active_events[pod->safety.event_count++] = SAFETY_EVENT_DECEL_HIGH;
        }
    }
}

/*===========================================================================*/
/* State Machine Functions                                                    */
/*===========================================================================*/

static void state_idle(pod_unit_t *pod) {
    g_motor_power[pod->pod_id] = 0.0f;
    g_brake_force[pod->pod_id] = 100.0f;  /* Holding brake */
    g_levitation_current[pod->pod_id] = 0.0f;
    
    pod->levitation.active = false;
    pod->kinematics.velocity_mps = 0.0f;
    pod->kinematics.acceleration_mps2 = 0.0f;
}

static void state_boarding(pod_unit_t *pod) {
    /* Door open, awaiting passengers */
    pod->door_sealed = false;
    
    /* Systems on standby */
    g_motor_power[pod->pod_id] = 0.0f;
    g_brake_force[pod->pod_id] = 100.0f;
}

static void state_sealed(pod_unit_t *pod) {
    /* Door closed and sealed */
    if (!pod->door_sealed) {
        /* Simulate door sealing */
        if ((g_uptime_ms - pod->state_start_ms) > POD_DOOR_SEAL_TIME_MS) {
            pod->door_sealed = true;
        }
    }
    
    /* Pressurize cabin */
    pod->pressure_pid.setpoint = CABIN_PRESSURE_KPA;
    float pressure_adjust = pid_update(&pod->pressure_pid, pod->cabin.pressure_kpa, g_uptime_ms);
    pod->cabin.pressure_kpa += pressure_adjust * 0.1f;
    
    /* Ready to levitate once sealed and pressurized */
    if (pod->door_sealed && 
        fabsf(pod->cabin.pressure_kpa - CABIN_PRESSURE_KPA) < 1.0f) {
        pod->state = POD_STATE_LEVITATING;
        pod->state_start_ms = g_uptime_ms;
    }
}

static void state_levitating(pod_unit_t *pod) {
    /* Engage magnetic levitation */
    pod->levitation.active = true;
    
    /* Ramp up levitation current */
    float target_current = 200.0f;  /* Initial hover current */
    if (g_levitation_current[pod->pod_id] < target_current) {
        g_levitation_current[pod->pod_id] += 50.0f * 0.1f;  /* Ramp rate */
    }
    pod->levitation.current_a = g_levitation_current[pod->pod_id];
    
    /* Simulate gap response */
    pod->levitation.gap_mm = LEVITATION_GAP_MM - 5.0f + 
                              (g_levitation_current[pod->pod_id] / target_current) * 5.0f;
    
    /* Clamp gap */
    if (pod->levitation.gap_mm > LEVITATION_GAP_MM) {
        pod->levitation.gap_mm = LEVITATION_GAP_MM;
    }
    
    /* Check if stable levitation achieved */
    if (fabsf(pod->levitation.gap_mm - LEVITATION_GAP_MM) < 1.0f &&
        (g_uptime_ms - pod->state_start_ms) > POD_LEVITATE_TIME_MS) {
        pod->levitation.stable = true;
        pod->state = POD_STATE_ACCELERATING;
        pod->state_start_ms = g_uptime_ms;
        pod->trip_start_ms = g_uptime_ms;
    }
}

static void state_accelerating(pod_unit_t *pod) {
    /* Set target velocity */
    pod->speed_pid.setpoint = pod->target_velocity_mps > 0 ? 
                               pod->target_velocity_mps : POD_CRUISE_SPEED_MPS;
    
    /* Release brake */
    g_brake_force[pod->pod_id] = 0.0f;
    
    /* Calculate required motor power */
    float power = pid_update(&pod->speed_pid, pod->kinematics.velocity_mps, g_uptime_ms);
    g_motor_power[pod->pod_id] = power;
    
    /* Simulate velocity response with acceleration limiting */
    float desired_accel = power * 0.1f;  /* Scale power to acceleration */
    
    /* Limit acceleration for passenger comfort */
    if (desired_accel > POD_MAX_ACCEL_MPS2) {
        desired_accel = POD_MAX_ACCEL_MPS2;
    }
    
    /* Limit jerk */
    float jerk = (desired_accel - pod->kinematics.acceleration_mps2) / 0.1f;
    if (jerk > POD_MAX_JERK_MPS3) {
        desired_accel = pod->kinematics.acceleration_mps2 + POD_MAX_JERK_MPS3 * 0.1f;
    }
    
    pod->kinematics.prev_acceleration_mps2 = pod->kinematics.acceleration_mps2;
    pod->kinematics.acceleration_mps2 = desired_accel;
    pod->kinematics.jerk_mps3 = (pod->kinematics.acceleration_mps2 - 
                                  pod->kinematics.prev_acceleration_mps2) / 0.1f;
    
    pod->kinematics.prev_velocity_mps = pod->kinematics.velocity_mps;
    pod->kinematics.velocity_mps += pod->kinematics.acceleration_mps2 * 0.1f;
    
    /* Update position */
    pod->kinematics.position_m += pod->kinematics.velocity_mps * 0.1f;
    pod->health.distance_m = (uint32_t)pod->kinematics.position_m;
    
    /* Maintain levitation */
    pod->levitation_pid.setpoint = LEVITATION_GAP_MM;
    float lev_adjust = pid_update(&pod->levitation_pid, pod->levitation.gap_mm, g_uptime_ms);
    g_levitation_current[pod->pod_id] += lev_adjust;
    if (g_levitation_current[pod->pod_id] > LEVITATION_CURRENT_MAX_A) {
        g_levitation_current[pod->pod_id] = LEVITATION_CURRENT_MAX_A;
    }
    if (g_levitation_current[pod->pod_id] < 0) {
        g_levitation_current[pod->pod_id] = 0;
    }
    
    /* Simulate motor heating */
    pod->health.motor_temp_c += g_motor_power[pod->pod_id] * 0.001f;
    pod->health.motor_temp_c -= (pod->health.motor_temp_c - 40.0f) * 0.01f;
    
    /* Transition to cruising when at target speed */
    if (fabsf(pod->kinematics.velocity_mps - pod->speed_pid.setpoint) < 5.0f &&
        pod->kinematics.velocity_mps > 50.0f) {
        pod->state = POD_STATE_CRUISING;
        pod->state_start_ms = g_uptime_ms;
    }
}

static void state_cruising(pod_unit_t *pod) {
    /* Maintain cruise velocity */
    float power = pid_update(&pod->speed_pid, pod->kinematics.velocity_mps, g_uptime_ms);
    g_motor_power[pod->pod_id] = power;
    
    /* Minimal acceleration at cruise */
    pod->kinematics.prev_acceleration_mps2 = pod->kinematics.acceleration_mps2;
    pod->kinematics.acceleration_mps2 = (pod->speed_pid.setpoint - pod->kinematics.velocity_mps) * 0.1f;
    
    pod->kinematics.prev_velocity_mps = pod->kinematics.velocity_mps;
    pod->kinematics.velocity_mps += pod->kinematics.acceleration_mps2 * 0.1f;
    pod->kinematics.position_m += pod->kinematics.velocity_mps * 0.1f;
    pod->health.distance_m = (uint32_t)pod->kinematics.position_m;
    
    /* Maintain levitation */
    float lev_adjust = pid_update(&pod->levitation_pid, pod->levitation.gap_mm, g_uptime_ms);
    g_levitation_current[pod->pod_id] += lev_adjust * 0.1f;
    
    /* Motor cooling at cruise */
    pod->health.motor_temp_c -= (pod->health.motor_temp_c - 60.0f) * 0.005f;
    
    /* Check for deceleration point (simulated based on position) */
    float remaining_distance = pod->track_length_m - pod->kinematics.position_m;
    float stopping_distance = (pod->kinematics.velocity_mps * pod->kinematics.velocity_mps) / 
                               (2.0f * POD_MAX_DECEL_MPS2);
    
    if (remaining_distance < stopping_distance * 1.2f) {
        pod->state = POD_STATE_DECELERATING;
        pod->state_start_ms = g_uptime_ms;
    }
}

static void state_decelerating(pod_unit_t *pod) {
    /* Reduce motor power and apply brakes */
    g_motor_power[pod->pod_id] = 0.0f;
    
    /* Calculate required deceleration to stop at target */
    float remaining = pod->track_length_m - pod->kinematics.position_m;
    float v_sq = pod->kinematics.velocity_mps * pod->kinematics.velocity_mps;
    float required_decel = v_sq / (2.0f * remaining);
    
    /* Clamp deceleration */
    if (required_decel > POD_MAX_DECEL_MPS2) {
        required_decel = POD_MAX_DECEL_MPS2;
    }
    
    /* Apply brakes proportionally */
    g_brake_force[pod->pod_id] = (required_decel / POD_MAX_DECEL_MPS2) * 100.0f;
    
    /* Limit jerk */
    float jerk = (-required_decel - pod->kinematics.acceleration_mps2) / 0.1f;
    if (fabsf(jerk) > POD_MAX_JERK_MPS3) {
        required_decel = pod->kinematics.acceleration_mps2 - 
                         (jerk > 0 ? POD_MAX_JERK_MPS3 : -POD_MAX_JERK_MPS3) * 0.1f;
        if (required_decel > 0) required_decel = -0.5f;
    }
    
    pod->kinematics.prev_acceleration_mps2 = pod->kinematics.acceleration_mps2;
    pod->kinematics.acceleration_mps2 = -required_decel;
    pod->kinematics.jerk_mps3 = (pod->kinematics.acceleration_mps2 - 
                                  pod->kinematics.prev_acceleration_mps2) / 0.1f;
    
    pod->kinematics.prev_velocity_mps = pod->kinematics.velocity_mps;
    pod->kinematics.velocity_mps += pod->kinematics.acceleration_mps2 * 0.1f;
    if (pod->kinematics.velocity_mps < 0) pod->kinematics.velocity_mps = 0;
    
    pod->kinematics.position_m += pod->kinematics.velocity_mps * 0.1f;
    
    /* Brake heating */
    pod->health.brake_temp_c += g_brake_force[pod->pod_id] * 0.05f;
    pod->health.brake_temp_c -= (pod->health.brake_temp_c - 40.0f) * 0.02f;
    
    /* Transition to landing when nearly stopped */
    if (pod->kinematics.velocity_mps < 5.0f) {
        pod->state = POD_STATE_LANDING;
        pod->state_start_ms = g_uptime_ms;
    }
}

static void state_landing(pod_unit_t *pod) {
    /* Reduce levitation */
    g_levitation_current[pod->pod_id] -= 20.0f * 0.1f;
    if (g_levitation_current[pod->pod_id] < 0) {
        g_levitation_current[pod->pod_id] = 0;
    }
    
    pod->levitation.current_a = g_levitation_current[pod->pod_id];
    pod->levitation.gap_mm = g_levitation_current[pod->pod_id] * 0.05f;
    
    /* Full stop */
    g_brake_force[pod->pod_id] = 100.0f;
    pod->kinematics.velocity_mps = 0;
    pod->kinematics.acceleration_mps2 = 0;
    
    /* Touched down */
    if (pod->levitation.gap_mm < 1.0f) {
        pod->levitation.active = false;
        pod->levitation.stable = false;
        pod->state = POD_STATE_DOCKING;
        pod->state_start_ms = g_uptime_ms;
    }
}

static void state_docking(pod_unit_t *pod) {
    /* Docking sequence */
    if ((g_uptime_ms - pod->state_start_ms) > POD_DOCK_TIME_MS) {
        pod->health.trip_count++;
        pod->state = POD_STATE_IDLE;
        pod->state_start_ms = g_uptime_ms;
    }
}

static void state_emergency(pod_unit_t *pod) {
    /* Emergency braking */
    g_motor_power[pod->pod_id] = 0.0f;
    
    /* Apply maximum braking (but not beyond emergency limit) */
    float decel = POD_EMERGENCY_DECEL_MPS2;
    g_brake_force[pod->pod_id] = 100.0f;
    
    pod->kinematics.acceleration_mps2 = -decel;
    pod->kinematics.velocity_mps += pod->kinematics.acceleration_mps2 * 0.1f;
    if (pod->kinematics.velocity_mps < 0) {
        pod->kinematics.velocity_mps = 0;
    }
    
    pod->kinematics.position_m += pod->kinematics.velocity_mps * 0.1f;
    
    /* Maintain levitation if possible */
    if (pod->gap_sensors.vote_result != TMR_ALL_FAILED) {
        pod->levitation_pid.setpoint = LEVITATION_GAP_MM;
        float lev_adjust = pid_update(&pod->levitation_pid, pod->levitation.gap_mm, g_uptime_ms);
        g_levitation_current[pod->pod_id] += lev_adjust;
    }
    
    /* Brake heating (emergency braking generates significant heat) */
    pod->health.brake_temp_c += 2.0f;
    
    /* Return to idle once stopped */
    if (pod->kinematics.velocity_mps == 0) {
        /* Keep in emergency until manually reset */
    }
}

static void update_state_machine(pod_unit_t *pod) {
    if (!pod) return;
    
    /* Read sensors */
    read_gap_sensors(pod);
    read_cabin_pressure(pod);
    read_tube_pressure(pod);
    
    /* Update g-force calculations */
    update_g_forces(pod);
    
    /* Check safety interlocks */
    pod_fault_t fault = check_safety_interlocks(pod);
    if (fault != POD_FAULT_NONE && pod->state != POD_STATE_EMERGENCY) {
        pod->fault = fault;
        pod->fault_count++;
        emergency_stop(pod);
        return;
    }
    
    /* Execute state handler */
    switch (pod->state) {
        case POD_STATE_IDLE:
            state_idle(pod);
            break;
        case POD_STATE_BOARDING:
            state_boarding(pod);
            break;
        case POD_STATE_SEALED:
            state_sealed(pod);
            break;
        case POD_STATE_LEVITATING:
            state_levitating(pod);
            break;
        case POD_STATE_ACCELERATING:
            state_accelerating(pod);
            break;
        case POD_STATE_CRUISING:
            state_cruising(pod);
            break;
        case POD_STATE_DECELERATING:
            state_decelerating(pod);
            break;
        case POD_STATE_LANDING:
            state_landing(pod);
            break;
        case POD_STATE_DOCKING:
            state_docking(pod);
            break;
        case POD_STATE_EMERGENCY:
            state_emergency(pod);
            break;
        case POD_STATE_MAINTENANCE:
            /* No automatic actions */
            break;
    }
}

/*===========================================================================*/
/* Telemetry Generation                                                       */
/*===========================================================================*/

static int generate_telemetry(pod_unit_t *pod, uint8_t *buffer, size_t max_len) {
    if (!pod || !buffer || max_len < 48) return -1;
    
    memset(buffer, 0, max_len);
    
    /* Header */
    buffer[0] = 0x1A;  /* Sync */
    buffer[1] = 0xCF;
    buffer[2] = 0xFC;
    buffer[3] = 0x1D;
    
    /* APID */
    uint16_t apid = 0x200 + pod->pod_id;
    buffer[4] = (apid >> 8) & 0xFF;
    buffer[5] = apid & 0xFF;
    
    /* Sequence */
    buffer[6] = (g_telemetry_sequence >> 8) & 0xFF;
    buffer[7] = g_telemetry_sequence & 0xFF;
    g_telemetry_sequence++;
    
    /* Length */
    buffer[8] = 0x00;
    buffer[9] = 40;  /* Payload length */
    
    /* Pod ID and state */
    buffer[10] = pod->pod_id;
    buffer[11] = (uint8_t)pod->state;
    buffer[12] = (uint8_t)pod->fault;
    
    /* Velocity (m/s * 10) */
    uint16_t vel = (uint16_t)(pod->kinematics.velocity_mps * 10.0f);
    buffer[13] = (vel >> 8) & 0xFF;
    buffer[14] = vel & 0xFF;
    
    /* Position (km * 10) */
    uint16_t pos = (uint16_t)(pod->kinematics.position_m / 100.0f);
    buffer[15] = (pos >> 8) & 0xFF;
    buffer[16] = pos & 0xFF;
    
    /* Acceleration (mps² * 100) */
    int16_t accel = (int16_t)(pod->kinematics.acceleration_mps2 * 100.0f);
    buffer[17] = (accel >> 8) & 0xFF;
    buffer[18] = accel & 0xFF;
    
    /* Levitation gap (mm * 10) */
    uint16_t gap = (uint16_t)(pod->levitation.gap_mm * 10.0f);
    buffer[19] = (gap >> 8) & 0xFF;
    buffer[20] = gap & 0xFF;
    
    /* Cabin pressure (kPa * 10) */
    uint16_t cabin_p = (uint16_t)(pod->cabin.pressure_kpa * 10.0f);
    buffer[21] = (cabin_p >> 8) & 0xFF;
    buffer[22] = cabin_p & 0xFF;
    
    /* Tube pressure (Pa) */
    uint16_t tube_p = (uint16_t)pod->tube.pressure_pa;
    buffer[23] = (tube_p >> 8) & 0xFF;
    buffer[24] = tube_p & 0xFF;
    
    /* G-force longitudinal (g * 100) */
    int16_t gf = (int16_t)(pod->safety.g_force_longitudinal * 100.0f);
    buffer[25] = (gf >> 8) & 0xFF;
    buffer[26] = gf & 0xFF;
    
    /* Motor temp (°C) */
    buffer[27] = (uint8_t)pod->health.motor_temp_c;
    
    /* Brake temp (°C) */
    buffer[28] = (uint8_t)(pod->health.brake_temp_c / 2);  /* Scaled */
    
    /* Battery SOC */
    buffer[29] = (uint8_t)pod->health.battery_soc_pct;
    
    /* Passenger count */
    buffer[30] = pod->passenger_count;
    
    /* Flags */
    uint8_t flags = 0;
    if (pod->door_sealed) flags |= 0x01;
    if (pod->levitation.active) flags |= 0x02;
    if (pod->levitation.stable) flags |= 0x04;
    if (pod->safety.emergency_active) flags |= 0x80;
    buffer[31] = flags;
    
    /* CRC-16 (simplified) */
    uint16_t crc = 0;
    for (size_t i = 0; i < 32; i++) {
        crc += buffer[i];
    }
    buffer[46] = (crc >> 8) & 0xFF;
    buffer[47] = crc & 0xFF;
    
    return 48;
}

/*===========================================================================*/
/* Public API                                                                 */
/*===========================================================================*/

/**
 * @brief Initialize hyperloop pod control module
 */
int hyperloop_pod_init(void) {
    if (g_module_initialized) {
        return -1;
    }
    
    memset(g_pods, 0, sizeof(g_pods));
    memset(g_motor_power, 0, sizeof(g_motor_power));
    memset(g_brake_force, 0, sizeof(g_brake_force));
    memset(g_levitation_current, 0, sizeof(g_levitation_current));
    
    g_uptime_ms = 0;
    
    for (uint8_t i = 0; i < POD_MAX_UNITS; i++) {
        g_pods[i].pod_id = i;
        g_pods[i].state = POD_STATE_IDLE;
        g_pods[i].track_length_m = 50000.0f;  /* 50 km default */
        
        /* Initialize cabin */
        g_pods[i].cabin.pressure_kpa = CABIN_PRESSURE_KPA;
        g_pods[i].cabin.temperature_c = 22.0f;
        g_pods[i].cabin.humidity_pct = 45.0f;
        g_pods[i].cabin.o2_pct = 21.0f;
        g_pods[i].cabin.co2_ppm = 400.0f;
        
        /* Initialize tube */
        g_pods[i].tube.pressure_pa = TUBE_PRESSURE_NOMINAL_PA;
        g_pods[i].tube.temperature_c = 20.0f;
        
        /* Initialize health */
        g_pods[i].health.battery_soc_pct = 100.0f;
        g_pods[i].health.motor_temp_c = 40.0f;
        g_pods[i].health.brake_temp_c = 40.0f;
        g_pods[i].health.brake_pad_pct = 100.0f;
        
        /* Initialize TMR sensors */
        for (uint8_t j = 0; j < POD_TMR_CHANNELS; j++) {
            g_pods[i].gap_sensors.status[j] = TMR_CHAN_OK;
            g_pods[i].cabin_pressure_sensors.status[j] = TMR_CHAN_OK;
            g_pods[i].tube_pressure_sensors.status[j] = TMR_CHAN_OK;
        }
        
        /* Initialize PID controllers */
        pid_init(&g_pods[i].speed_pid, PID_KP_SPEED, PID_KI_SPEED, PID_KD_SPEED, -100.0f, 100.0f);
        pid_init(&g_pods[i].levitation_pid, PID_KP_LEVITATION, PID_KI_LEVITATION, 
                 PID_KD_LEVITATION, -50.0f, 50.0f);
        pid_init(&g_pods[i].pressure_pid, PID_KP_PRESSURE, PID_KI_PRESSURE, 
                 PID_KD_PRESSURE, -10.0f, 10.0f);
    }
    
    g_telemetry_sequence = 0;
    g_module_initialized = true;
    
    return 0;
}

/**
 * @brief Configure pod parameters
 */
typedef struct {
    uint8_t pod_id;
    float track_length_m;
    float cruise_speed_mps;
    uint8_t passenger_capacity;
} hyperloop_config_t;

int hyperloop_pod_configure(const hyperloop_config_t *config) {
    if (!g_module_initialized || !config) {
        return -1;
    }
    
    if (config->pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[config->pod_id];
    
    if (pod->state != POD_STATE_IDLE && pod->state != POD_STATE_MAINTENANCE) {
        return -1;
    }
    
    pod->track_length_m = config->track_length_m;
    pod->target_velocity_mps = config->cruise_speed_mps;
    
    return 0;
}

/**
 * @brief Start pod trip
 */
int hyperloop_pod_start(uint8_t pod_id) {
    if (!g_module_initialized || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[pod_id];
    
    if (pod->state != POD_STATE_IDLE) {
        return -1;
    }
    
    pod->state = POD_STATE_BOARDING;
    pod->state_start_ms = g_uptime_ms;
    pod->kinematics.position_m = 0.0f;
    pod->fault = POD_FAULT_NONE;
    
    /* Clear safety events */
    pod->safety.event_count = 0;
    pod->safety.emergency_active = false;
    
    return 0;
}

/**
 * @brief Seal door and begin journey
 */
int hyperloop_pod_seal(uint8_t pod_id, uint8_t passenger_count) {
    if (!g_module_initialized || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[pod_id];
    
    if (pod->state != POD_STATE_BOARDING) {
        return -1;
    }
    
    pod->passenger_count = passenger_count;
    pod->state = POD_STATE_SEALED;
    pod->state_start_ms = g_uptime_ms;
    
    return 0;
}

/**
 * @brief Emergency stop
 */
int hyperloop_pod_emergency_stop(uint8_t pod_id) {
    if (!g_module_initialized || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    emergency_stop(&g_pods[pod_id]);
    return 0;
}

/**
 * @brief Reset pod from emergency/fault state
 */
int hyperloop_pod_reset(uint8_t pod_id) {
    if (!g_module_initialized || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[pod_id];
    
    if (pod->state != POD_STATE_EMERGENCY && 
        pod->state != POD_STATE_IDLE) {
        return -1;
    }
    
    /* Only reset if velocity is zero */
    if (pod->kinematics.velocity_mps > 0.1f) {
        return -1;
    }
    
    pod->state = POD_STATE_IDLE;
    pod->state_start_ms = g_uptime_ms;
    pod->fault = POD_FAULT_NONE;
    pod->safety.emergency_active = false;
    pod->safety.event_count = 0;
    
    return 0;
}

/**
 * @brief Update pod subsystem
 */
void hyperloop_pod_update(uint32_t elapsed_ms) {
    if (!g_module_initialized) return;
    
    g_uptime_ms += elapsed_ms;
    
    for (uint8_t i = 0; i < POD_MAX_UNITS; i++) {
        update_state_machine(&g_pods[i]);
    }
}

/**
 * @brief Get pod state
 */
int hyperloop_pod_get_state(uint8_t pod_id, pod_state_t *state) {
    if (!g_module_initialized || !state || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    *state = g_pods[pod_id].state;
    return 0;
}

/**
 * @brief Get pod fault
 */
int hyperloop_pod_get_fault(uint8_t pod_id, pod_fault_t *fault) {
    if (!g_module_initialized || !fault || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    *fault = g_pods[pod_id].fault;
    return 0;
}

/** Kinematics structure for external use */
typedef struct {
    float position_m;
    float velocity_mps;
    float acceleration_mps2;
    float levitation_gap_mm;
} hyperloop_kinematics_t;

/**
 * @brief Get pod kinematics
 */
int hyperloop_pod_get_kinematics(uint8_t pod_id, hyperloop_kinematics_t *kin) {
    if (!g_module_initialized || !kin || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[pod_id];
    
    kin->position_m = pod->kinematics.position_m;
    kin->velocity_mps = pod->kinematics.velocity_mps;
    kin->acceleration_mps2 = pod->kinematics.acceleration_mps2;
    kin->levitation_gap_mm = pod->levitation.gap_mm;
    
    return 0;
}

/** Cabin structure for external use */
typedef struct {
    float pressure_kpa;
    float temperature_c;
    float humidity_pct;
    uint8_t passenger_count;
    bool door_sealed;
} hyperloop_cabin_t;

/**
 * @brief Get cabin status
 */
int hyperloop_pod_get_cabin(uint8_t pod_id, hyperloop_cabin_t *cabin) {
    if (!g_module_initialized || !cabin || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[pod_id];
    
    cabin->pressure_kpa = pod->cabin.pressure_kpa;
    cabin->temperature_c = pod->cabin.temperature_c;
    cabin->humidity_pct = pod->cabin.humidity_pct;
    cabin->passenger_count = pod->passenger_count;
    cabin->door_sealed = pod->door_sealed;
    
    return 0;
}

/** Safety structure for external use */
typedef struct {
    float g_force_longitudinal;
    float g_force_lateral;
    float g_force_vertical;
    bool emergency_active;
    uint8_t event_count;
} hyperloop_safety_t;

/**
 * @brief Get safety status
 */
int hyperloop_pod_get_safety(uint8_t pod_id, hyperloop_safety_t *safety) {
    if (!g_module_initialized || !safety || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[pod_id];
    
    safety->g_force_longitudinal = pod->safety.g_force_longitudinal;
    safety->g_force_lateral = pod->safety.g_force_lateral;
    safety->g_force_vertical = pod->safety.g_force_vertical;
    safety->emergency_active = pod->safety.emergency_active;
    safety->event_count = pod->safety.event_count;
    
    return 0;
}

/**
 * @brief Get telemetry frame
 */
int hyperloop_pod_get_telemetry(uint8_t pod_id, uint8_t *buffer, size_t max_len) {
    if (!g_module_initialized || !buffer || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    return generate_telemetry(&g_pods[pod_id], buffer, max_len);
}

/**
 * @brief Inject environment condition for testing
 */
int hyperloop_pod_set_environment(uint8_t pod_id, const char *param, float value) {
    if (!g_module_initialized || !param || pod_id >= POD_MAX_UNITS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[pod_id];
    
    if (strcmp(param, "tube_pressure") == 0) {
        pod->tube.pressure_pa = value;
    } else if (strcmp(param, "cabin_pressure") == 0) {
        pod->cabin.pressure_kpa = value;
    } else if (strcmp(param, "motor_temp") == 0) {
        pod->health.motor_temp_c = value;
    } else if (strcmp(param, "brake_temp") == 0) {
        pod->health.brake_temp_c = value;
    } else if (strcmp(param, "gap") == 0) {
        pod->levitation.gap_mm = value;
    } else {
        return -1;
    }
    
    return 0;
}

/** Sensor type for fault injection */
typedef enum {
    SENSOR_GAP,
    SENSOR_CABIN_PRESSURE,
    SENSOR_TUBE_PRESSURE
} hyperloop_sensor_t;

/**
 * @brief Inject sensor fault for testing
 */
int hyperloop_pod_inject_fault(uint8_t pod_id, hyperloop_sensor_t sensor, uint8_t channel) {
    if (!g_module_initialized || pod_id >= POD_MAX_UNITS || channel >= POD_TMR_CHANNELS) {
        return -1;
    }
    
    pod_unit_t *pod = &g_pods[pod_id];
    
    switch (sensor) {
        case SENSOR_GAP:
            pod->gap_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        case SENSOR_CABIN_PRESSURE:
            pod->cabin_pressure_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        case SENSOR_TUBE_PRESSURE:
            pod->tube_pressure_sensors.status[channel] = TMR_CHAN_FAILED;
            break;
        default:
            return -1;
    }
    
    return 0;
}

/**
 * @brief Shutdown hyperloop module
 */
void hyperloop_pod_shutdown(void) {
    memset(g_pods, 0, sizeof(g_pods));
    g_module_initialized = false;
}
