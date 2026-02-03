/**
 * @file renewable_spotlight.c
 * @brief Wind Turbine Control & Grid Synchronization Spotlight
 * 
 * Complete implementation of wind turbine control including:
 * - Pitch control loop with PID for power regulation
 * - Yaw control for wind direction tracking
 * - Grid synchronization with phase-locked loop
 * - Maximum Power Point Tracking (MPPT)
 * - Safety interlocks and emergency shutdown
 * 
 * Target: IEC 61400 compliant wind turbine control
 */

#include "renewable/wind_turbine.h"
#include "renewable/grid_sync.h"
#include "core/scheduler.h"
#include "core/error_handler.h"
#include <string.h>
#include <math.h>
#include <stdbool.h>

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define TURBINE_CONTROL_PERIOD_MS    100     /* 10 Hz control loop */
#define PITCH_CONTROL_PERIOD_MS      50      /* 20 Hz pitch loop */
#define YAW_CONTROL_PERIOD_MS        1000    /* 1 Hz yaw loop */
#define GRID_SYNC_PERIOD_MS          10      /* 100 Hz grid sync */

#define PITCH_MIN_DEG               0.0f
#define PITCH_MAX_DEG               90.0f
#define PITCH_RATE_MAX_DPS          10.0f

#define YAW_RATE_MAX_DPS            0.5f
#define YAW_DEADBAND_DEG            5.0f

#define ROTOR_OVERSPEED_RPM         22.0f
#define ROTOR_OVERSPEED_TRIP_RPM    25.0f
#define GENERATOR_OVERTEMP_C        120

#define GRID_FREQ_NOMINAL_HZ        50.0f
#define GRID_FREQ_TOLERANCE_HZ      0.5f
#define GRID_PHASE_TOLERANCE_DEG    5.0f
#define GRID_VOLTAGE_NOMINAL_V      690.0f

#define PI 3.14159265359f

/*===========================================================================*/
/* PID Controller                                                             */
/*===========================================================================*/

typedef struct {
    float kp;
    float ki;
    float kd;
    float integral;
    float prev_error;
    float output_min;
    float output_max;
    bool anti_windup;
} gf_pid_controller_t;

static float gf_pid_update(gf_pid_controller_t* pid, float setpoint, float measured, float dt)
{
    if (!pid || dt <= 0.0f) {
        return 0.0f;
    }
    
    float error = setpoint - measured;
    
    /* Proportional */
    float p_term = pid->kp * error;
    
    /* Integral with anti-windup */
    pid->integral += error * dt;
    if (pid->anti_windup) {
        float max_integral = (pid->output_max - pid->output_min) / (2.0f * pid->ki + 0.001f);
        if (pid->integral > max_integral) pid->integral = max_integral;
        if (pid->integral < -max_integral) pid->integral = -max_integral;
    }
    float i_term = pid->ki * pid->integral;
    
    /* Derivative */
    float derivative = (error - pid->prev_error) / dt;
    float d_term = pid->kd * derivative;
    pid->prev_error = error;
    
    /* Sum and clamp */
    float output = p_term + i_term + d_term;
    if (output > pid->output_max) output = pid->output_max;
    if (output < pid->output_min) output = pid->output_min;
    
    return output;
}

static void gf_pid_reset(gf_pid_controller_t* pid)
{
    if (pid) {
        pid->integral = 0.0f;
        pid->prev_error = 0.0f;
    }
}

/*===========================================================================*/
/* Turbine State                                                              */
/*===========================================================================*/

typedef struct {
    /* Configuration */
    gf_turbine_config_t config;
    bool initialized;
    
    /* State */
    gf_turbine_state_t state;
    gf_turbine_state_t prev_state;
    
    /* Sensors */
    gf_turbine_wind_t wind;
    gf_turbine_rotor_t rotor;
    gf_turbine_generator_t generator;
    gf_turbine_pitch_t pitch;
    gf_turbine_yaw_t yaw;
    
    /* Control */
    gf_turbine_control_mode_t control_mode;
    float power_limit_kw;
    float pitch_setpoint_deg;
    float yaw_setpoint_deg;
    bool yaw_tracking_enabled;
    
    /* PID controllers */
    gf_pid_controller_t pitch_pid;
    gf_pid_controller_t power_pid;
    gf_pid_controller_t yaw_pid;
    
    /* Statistics */
    uint32_t runtime_hours;
    uint32_t energy_produced_kwh;
    uint64_t last_energy_update_ms;
    
    /* Timing */
    uint64_t control_timestamp_ms;
    uint64_t pitch_timestamp_ms;
    uint64_t yaw_timestamp_ms;
    
    /* Callbacks */
    gf_turbine_state_cb_t state_callback;
    void* state_cb_user_data;
    gf_turbine_alarm_cb_t alarm_callback;
    void* alarm_cb_user_data;
} gf_turbine_ctx_t;

static gf_turbine_ctx_t g_turbine = {0};

/*===========================================================================*/
/* Grid Sync State                                                            */
/*===========================================================================*/

typedef struct {
    /* Configuration */
    gf_grid_config_t config;
    bool initialized;
    
    /* State */
    gf_grid_state_t state;
    gf_grid_fault_t fault;
    
    /* Measurements */
    gf_grid_measurement_t measurement;
    
    /* Sync status */
    float freq_error_hz;
    float voltage_error_v;
    float phase_error_deg;
    bool freq_locked;
    bool voltage_matched;
    bool phase_locked;
    uint32_t lock_time_ms;
    
    /* Phase-locked loop */
    float pll_phase;
    float pll_frequency;
    gf_pid_controller_t pll_pid;
    
    /* Power control */
    gf_grid_power_setpoint_t power_setpoint;
    bool export_enabled;
    
    /* Timing */
    uint64_t sync_start_ms;
    uint64_t last_process_ms;
    
    /* Callbacks */
    gf_grid_state_cb_t state_callback;
    void* state_cb_user_data;
    gf_grid_fault_cb_t fault_callback;
    void* fault_cb_user_data;
} gf_grid_ctx_t;

static gf_grid_ctx_t g_grid = {0};

/*===========================================================================*/
/* Helper Functions                                                           */
/*===========================================================================*/

static uint64_t get_time_ms(void)
{
    /* Stub - would use HAL or RTOS tick */
    static uint64_t fake_time = 0;
    return fake_time++;
}

static float clamp_f(float value, float min_val, float max_val)
{
    if (value < min_val) return min_val;
    if (value > max_val) return max_val;
    return value;
}

static float normalize_angle(float angle_deg)
{
    while (angle_deg > 180.0f) angle_deg -= 360.0f;
    while (angle_deg < -180.0f) angle_deg += 360.0f;
    return angle_deg;
}

/*===========================================================================*/
/* Aerodynamics Model                                                         */
/*===========================================================================*/

/**
 * @brief Power coefficient (Cp) as function of tip speed ratio and pitch
 * 
 * Simplified Heier model: Cp = c1 * (c2/λi - c3*β - c4) * e^(-c5/λi) + c6*λ
 * where λi = 1 / (1/(λ+0.08β) - 0.035/(β³+1))
 */
static float calculate_power_coefficient(float tip_speed_ratio, float pitch_deg)
{
    float lambda = tip_speed_ratio;
    float beta = pitch_deg;
    
    /* Avoid division by zero */
    if (lambda < 0.1f) lambda = 0.1f;
    if (beta < 0.0f) beta = 0.0f;
    
    /* Simplified Cp calculation */
    float lambda_i = 1.0f / (1.0f / (lambda + 0.08f * beta) - 0.035f / (beta * beta * beta + 1.0f));
    
    float c1 = 0.5176f;
    float c2 = 116.0f;
    float c3 = 0.4f;
    float c4 = 5.0f;
    float c5 = 21.0f;
    float c6 = 0.0068f;
    
    float cp = c1 * (c2 / lambda_i - c3 * beta - c4) * expf(-c5 / lambda_i) + c6 * lambda;
    
    /* Clamp to physical limits (Betz limit = 0.593) */
    return clamp_f(cp, 0.0f, 0.5f);
}

/**
 * @brief Calculate mechanical power from wind
 * P = 0.5 * ρ * A * v³ * Cp
 */
static float calculate_mechanical_power(float wind_speed_mps, float rotor_diameter_m,
                                          float air_density, float cp)
{
    float area = PI * (rotor_diameter_m / 2.0f) * (rotor_diameter_m / 2.0f);
    float power_w = 0.5f * air_density * area * wind_speed_mps * wind_speed_mps * wind_speed_mps * cp;
    return power_w / 1000.0f;  /* Return kW */
}

/**
 * @brief Calculate optimal rotor speed for MPPT
 */
static float calculate_optimal_rotor_speed(float wind_speed_mps, float rotor_diameter_m,
                                            float optimal_tsr)
{
    /* ω = λ * v / R */
    float radius = rotor_diameter_m / 2.0f;
    float omega_rad_s = optimal_tsr * wind_speed_mps / radius;
    float rpm = omega_rad_s * 60.0f / (2.0f * PI);
    return rpm;
}

/*===========================================================================*/
/* Turbine Control Implementation                                             */
/*===========================================================================*/

/**
 * @brief Update pitch control
 */
static void turbine_update_pitch(float dt)
{
    if (!g_turbine.initialized || dt <= 0.0f) return;
    
    float pitch_cmd = g_turbine.pitch_setpoint_deg;
    
    /* In power production mode, use power PID to adjust pitch */
    if (g_turbine.state == GF_TURBINE_STATE_POWER_PRODUCTION &&
        g_turbine.control_mode == GF_TURBINE_CONTROL_MPPT) {
        
        /* Below rated wind: pitch = 0 for max Cp */
        if (g_turbine.wind.speed_mps < g_turbine.config.rated_wind_mps) {
            pitch_cmd = 0.0f;
        } else {
            /* Above rated: pitch to limit power */
            float power_error = g_turbine.rotor.power_kw - g_turbine.config.rated_power_kw;
            pitch_cmd = gf_pid_update(&g_turbine.pitch_pid, 0.0f, -power_error, dt);
        }
    }
    
    /* Power limit mode */
    if (g_turbine.control_mode == GF_TURBINE_CONTROL_POWER_LIMIT) {
        float power_error = g_turbine.rotor.power_kw - g_turbine.power_limit_kw;
        pitch_cmd = gf_pid_update(&g_turbine.power_pid, 0.0f, -power_error, dt);
    }
    
    /* Emergency stop: feather blades */
    if (g_turbine.state == GF_TURBINE_STATE_EMERGENCY_STOP) {
        pitch_cmd = PITCH_MAX_DEG;
    }
    
    /* Rate limit pitch command */
    float max_delta = PITCH_RATE_MAX_DPS * dt;
    float delta = pitch_cmd - g_turbine.pitch.setpoint_deg;
    if (delta > max_delta) delta = max_delta;
    if (delta < -max_delta) delta = -max_delta;
    
    g_turbine.pitch.setpoint_deg += delta;
    g_turbine.pitch.setpoint_deg = clamp_f(g_turbine.pitch.setpoint_deg, PITCH_MIN_DEG, PITCH_MAX_DEG);
    
    /* Simulate blade actuation (first order lag) */
    float tau = 0.5f;  /* Time constant */
    for (int i = 0; i < g_turbine.config.num_blades && i < GF_TURBINE_MAX_BLADES; i++) {
        g_turbine.pitch.angle_deg[i] += (g_turbine.pitch.setpoint_deg - g_turbine.pitch.angle_deg[i]) * (dt / tau);
    }
    
    g_turbine.pitch.rate_dps = delta / dt;
    g_turbine.pitch.synchronized = true;
}

/**
 * @brief Update yaw control
 */
static void turbine_update_yaw(float dt)
{
    if (!g_turbine.initialized || !g_turbine.yaw_tracking_enabled || dt <= 0.0f) return;
    
    /* Calculate yaw error */
    g_turbine.yaw.wind_direction_deg = g_turbine.wind.direction_deg;
    g_turbine.yaw.error_deg = normalize_angle(g_turbine.yaw.wind_direction_deg - g_turbine.yaw.angle_deg);
    
    /* Deadband to avoid hunting */
    if (fabsf(g_turbine.yaw.error_deg) < YAW_DEADBAND_DEG) {
        g_turbine.yaw.tracking = false;
        return;
    }
    
    g_turbine.yaw.tracking = true;
    
    /* PID control */
    float yaw_rate = gf_pid_update(&g_turbine.yaw_pid, 0.0f, -g_turbine.yaw.error_deg, dt);
    
    /* Rate limit */
    yaw_rate = clamp_f(yaw_rate, -YAW_RATE_MAX_DPS, YAW_RATE_MAX_DPS);
    
    /* Update yaw angle */
    g_turbine.yaw.angle_deg += yaw_rate * dt;
    
    /* Normalize to 0-360 */
    while (g_turbine.yaw.angle_deg < 0.0f) g_turbine.yaw.angle_deg += 360.0f;
    while (g_turbine.yaw.angle_deg >= 360.0f) g_turbine.yaw.angle_deg -= 360.0f;
}

/**
 * @brief Update rotor and generator model
 */
static void turbine_update_rotor(float dt)
{
    if (!g_turbine.initialized || dt <= 0.0f) return;
    
    /* Calculate tip speed ratio */
    float radius = g_turbine.config.rotor_diameter_m / 2.0f;
    float omega_rad_s = g_turbine.rotor.speed_rpm * 2.0f * PI / 60.0f;
    
    if (g_turbine.wind.speed_mps > 0.5f) {
        g_turbine.rotor.tip_speed_ratio = omega_rad_s * radius / g_turbine.wind.speed_mps;
    } else {
        g_turbine.rotor.tip_speed_ratio = 0.0f;
    }
    
    /* Calculate Cp and mechanical power */
    float avg_pitch = 0.0f;
    for (int i = 0; i < g_turbine.config.num_blades && i < GF_TURBINE_MAX_BLADES; i++) {
        avg_pitch += g_turbine.pitch.angle_deg[i];
    }
    avg_pitch /= g_turbine.config.num_blades;
    
    float cp = calculate_power_coefficient(g_turbine.rotor.tip_speed_ratio, avg_pitch);
    float mech_power_kw = calculate_mechanical_power(g_turbine.wind.speed_mps,
                                                      g_turbine.config.rotor_diameter_m,
                                                      g_turbine.wind.density_kgm3,
                                                      cp);
    
    /* Limit to rated power */
    mech_power_kw = clamp_f(mech_power_kw, 0.0f, g_turbine.config.rated_power_kw * 1.2f);
    
    /* First-order lag for power */
    float tau = 2.0f;
    g_turbine.rotor.power_kw += (mech_power_kw - g_turbine.rotor.power_kw) * (dt / tau);
    
    /* Calculate torque: P = T * ω */
    if (omega_rad_s > 0.1f) {
        g_turbine.rotor.torque_nm = g_turbine.rotor.power_kw * 1000.0f / omega_rad_s;
    } else {
        g_turbine.rotor.torque_nm = 0.0f;
    }
    
    /* Rotor speed dynamics based on MPPT setpoint */
    if (g_turbine.state == GF_TURBINE_STATE_POWER_PRODUCTION) {
        float optimal_rpm = calculate_optimal_rotor_speed(g_turbine.wind.speed_mps,
                                                          g_turbine.config.rotor_diameter_m, 7.0f);
        g_turbine.rotor.speed_setpoint_rpm = clamp_f(optimal_rpm, 0.0f, GF_TURBINE_MAX_ROTOR_RPM);
        
        /* Simplified rotor dynamics */
        g_turbine.rotor.speed_rpm += (g_turbine.rotor.speed_setpoint_rpm - g_turbine.rotor.speed_rpm) * (dt / 10.0f);
    } else if (g_turbine.state == GF_TURBINE_STATE_STOPPING ||
               g_turbine.state == GF_TURBINE_STATE_EMERGENCY_STOP) {
        g_turbine.rotor.speed_rpm *= (1.0f - dt / 30.0f);  /* Slow deceleration */
    }
    
    /* Generator output (simplified) */
    float gen_efficiency = 0.95f;
    g_turbine.generator.power_kw = g_turbine.rotor.power_kw * gen_efficiency;
    g_turbine.generator.efficiency = gen_efficiency;
    
    /* Generator frequency from rotor speed through gearbox */
    float gen_rpm = g_turbine.rotor.speed_rpm * g_turbine.config.gear_ratio;
    g_turbine.generator.frequency_hz = gen_rpm * g_turbine.config.generator_poles / 120.0f;
    
    /* Voltage and current */
    g_turbine.generator.voltage_v = GRID_VOLTAGE_NOMINAL_V;
    if (g_turbine.generator.voltage_v > 0.0f) {
        float apparent_power = g_turbine.generator.power_kw * 1000.0f / 0.9f;  /* Assume PF=0.9 */
        g_turbine.generator.current_a = apparent_power / (g_turbine.generator.voltage_v * 1.732f);
    }
    g_turbine.generator.power_factor = 0.9f;
}

/**
 * @brief Check safety interlocks
 */
static gf_turbine_status_t turbine_check_safety(void)
{
    /* Overspeed protection */
    if (g_turbine.rotor.speed_rpm > ROTOR_OVERSPEED_TRIP_RPM) {
        if (g_turbine.alarm_callback) {
            g_turbine.alarm_callback(GF_TURBINE_ERROR_OVERSPEED, "Rotor overspeed trip",
                                      g_turbine.alarm_cb_user_data);
        }
        return GF_TURBINE_ERROR_OVERSPEED;
    }
    
    /* Overspeed warning */
    if (g_turbine.rotor.speed_rpm > ROTOR_OVERSPEED_RPM) {
        return GF_TURBINE_WARN_HIGH_WIND;
    }
    
    /* Generator overtemperature */
    if (g_turbine.generator.temperature_c > GENERATOR_OVERTEMP_C) {
        if (g_turbine.alarm_callback) {
            g_turbine.alarm_callback(GF_TURBINE_ERROR_SENSOR_FAULT, "Generator overtemp",
                                      g_turbine.alarm_cb_user_data);
        }
        return GF_TURBINE_ERROR_SENSOR_FAULT;
    }
    
    /* High wind */
    if (g_turbine.wind.speed_mps > g_turbine.config.cut_out_wind_mps) {
        return GF_TURBINE_WARN_HIGH_WIND;
    }
    
    /* Low wind */
    if (g_turbine.wind.speed_mps < g_turbine.config.cut_in_wind_mps) {
        return GF_TURBINE_WARN_LOW_WIND;
    }
    
    return GF_TURBINE_OK;
}

/**
 * @brief State machine update
 */
static void turbine_update_state(gf_turbine_status_t safety_status)
{
    gf_turbine_state_t new_state = g_turbine.state;
    
    switch (g_turbine.state) {
    case GF_TURBINE_STATE_STOPPED:
        /* Wait for start command and conditions */
        break;
        
    case GF_TURBINE_STATE_STARTING:
        /* Checking conditions, initializing */
        if (g_turbine.wind.speed_mps >= g_turbine.config.cut_in_wind_mps &&
            g_turbine.wind.speed_mps <= g_turbine.config.cut_out_wind_mps) {
            new_state = GF_TURBINE_STATE_RUNNING;
        }
        break;
        
    case GF_TURBINE_STATE_RUNNING:
        /* Accelerating to operating speed */
        if (g_turbine.rotor.speed_rpm > 5.0f && g_turbine.generator.power_kw > 10.0f) {
            new_state = GF_TURBINE_STATE_POWER_PRODUCTION;
        }
        break;
        
    case GF_TURBINE_STATE_POWER_PRODUCTION:
        /* Normal operation */
        if (safety_status == GF_TURBINE_ERROR_OVERSPEED) {
            new_state = GF_TURBINE_STATE_EMERGENCY_STOP;
        } else if (safety_status == GF_TURBINE_WARN_HIGH_WIND ||
                   safety_status == GF_TURBINE_WARN_LOW_WIND) {
            new_state = GF_TURBINE_STATE_STOPPING;
        }
        break;
        
    case GF_TURBINE_STATE_STOPPING:
        /* Controlled shutdown */
        if (g_turbine.rotor.speed_rpm < 1.0f) {
            new_state = GF_TURBINE_STATE_STOPPED;
        }
        break;
        
    case GF_TURBINE_STATE_EMERGENCY_STOP:
        /* Emergency - feather blades */
        if (g_turbine.rotor.speed_rpm < 1.0f) {
            new_state = GF_TURBINE_STATE_FAULT;
        }
        break;
        
    case GF_TURBINE_STATE_FAULT:
        /* Requires manual reset */
        break;
        
    case GF_TURBINE_STATE_MAINTENANCE:
        /* Locked out */
        break;
    }
    
    /* State transition */
    if (new_state != g_turbine.state) {
        g_turbine.prev_state = g_turbine.state;
        g_turbine.state = new_state;
        
        if (g_turbine.state_callback) {
            g_turbine.state_callback(g_turbine.prev_state, g_turbine.state,
                                      g_turbine.state_cb_user_data);
        }
    }
}

/*===========================================================================*/
/* Turbine API Implementation                                                 */
/*===========================================================================*/

gf_turbine_status_t gf_turbine_init(const gf_turbine_config_t* config)
{
    if (!config) {
        return GF_TURBINE_ERROR_NULL_PTR;
    }
    
    memset(&g_turbine, 0, sizeof(g_turbine));
    memcpy(&g_turbine.config, config, sizeof(gf_turbine_config_t));
    
    /* Initialize PID controllers */
    g_turbine.pitch_pid = (gf_pid_controller_t){
        .kp = 0.5f, .ki = 0.1f, .kd = 0.05f,
        .output_min = 0.0f, .output_max = PITCH_MAX_DEG,
        .anti_windup = true
    };
    
    g_turbine.power_pid = (gf_pid_controller_t){
        .kp = 0.1f, .ki = 0.02f, .kd = 0.01f,
        .output_min = 0.0f, .output_max = PITCH_MAX_DEG,
        .anti_windup = true
    };
    
    g_turbine.yaw_pid = (gf_pid_controller_t){
        .kp = 0.1f, .ki = 0.01f, .kd = 0.0f,
        .output_min = -YAW_RATE_MAX_DPS, .output_max = YAW_RATE_MAX_DPS,
        .anti_windup = true
    };
    
    /* Default values */
    g_turbine.wind.density_kgm3 = 1.225f;  /* Sea level */
    g_turbine.power_limit_kw = config->rated_power_kw;
    g_turbine.yaw_tracking_enabled = true;
    g_turbine.control_mode = config->mode;
    g_turbine.state = GF_TURBINE_STATE_STOPPED;
    
    g_turbine.initialized = true;
    return GF_TURBINE_OK;
}

void gf_turbine_shutdown(void)
{
    if (g_turbine.state != GF_TURBINE_STATE_STOPPED) {
        gf_turbine_stop();
    }
    g_turbine.initialized = false;
}

gf_turbine_status_t gf_turbine_start(void)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    if (g_turbine.state == GF_TURBINE_STATE_FAULT) {
        return GF_TURBINE_ERROR_PITCH_FAULT;
    }
    
    gf_pid_reset(&g_turbine.pitch_pid);
    gf_pid_reset(&g_turbine.power_pid);
    gf_pid_reset(&g_turbine.yaw_pid);
    
    g_turbine.prev_state = g_turbine.state;
    g_turbine.state = GF_TURBINE_STATE_STARTING;
    
    if (g_turbine.state_callback) {
        g_turbine.state_callback(g_turbine.prev_state, g_turbine.state,
                                  g_turbine.state_cb_user_data);
    }
    
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_stop(void)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    g_turbine.prev_state = g_turbine.state;
    g_turbine.state = GF_TURBINE_STATE_STOPPING;
    
    if (g_turbine.state_callback) {
        g_turbine.state_callback(g_turbine.prev_state, g_turbine.state,
                                  g_turbine.state_cb_user_data);
    }
    
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_emergency_stop(void)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    g_turbine.prev_state = g_turbine.state;
    g_turbine.state = GF_TURBINE_STATE_EMERGENCY_STOP;
    g_turbine.pitch_setpoint_deg = PITCH_MAX_DEG;  /* Feather blades */
    
    if (g_turbine.state_callback) {
        g_turbine.state_callback(g_turbine.prev_state, g_turbine.state,
                                  g_turbine.state_cb_user_data);
    }
    
    return GF_TURBINE_OK;
}

gf_turbine_state_t gf_turbine_get_state(void)
{
    return g_turbine.state;
}

gf_turbine_status_t gf_turbine_set_pitch(float angle_deg)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    if (angle_deg < PITCH_MIN_DEG || angle_deg > PITCH_MAX_DEG) {
        return GF_TURBINE_ERROR_INVALID_PARAM;
    }
    
    g_turbine.pitch_setpoint_deg = angle_deg;
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_get_pitch(gf_turbine_pitch_t* pitch)
{
    if (!pitch) {
        return GF_TURBINE_ERROR_NULL_PTR;
    }
    
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    memcpy(pitch, &g_turbine.pitch, sizeof(gf_turbine_pitch_t));
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_set_yaw(float angle_deg)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    g_turbine.yaw_setpoint_deg = normalize_angle(angle_deg);
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_enable_yaw_tracking(bool enable)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    g_turbine.yaw_tracking_enabled = enable;
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_set_power_limit(float power_kw)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    if (power_kw < 0.0f || power_kw > g_turbine.config.rated_power_kw * 1.1f) {
        return GF_TURBINE_ERROR_INVALID_PARAM;
    }
    
    g_turbine.power_limit_kw = power_kw;
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_set_control_mode(gf_turbine_control_mode_t mode)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    g_turbine.control_mode = mode;
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_update_wind(const gf_turbine_wind_t* wind)
{
    if (!wind) {
        return GF_TURBINE_ERROR_NULL_PTR;
    }
    
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    memcpy(&g_turbine.wind, wind, sizeof(gf_turbine_wind_t));
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_get_telemetry(gf_turbine_telemetry_t* telemetry)
{
    if (!telemetry) {
        return GF_TURBINE_ERROR_NULL_PTR;
    }
    
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    telemetry->timestamp_ms = (uint32_t)get_time_ms();
    telemetry->state = g_turbine.state;
    memcpy(&telemetry->wind, &g_turbine.wind, sizeof(gf_turbine_wind_t));
    memcpy(&telemetry->rotor, &g_turbine.rotor, sizeof(gf_turbine_rotor_t));
    memcpy(&telemetry->generator, &g_turbine.generator, sizeof(gf_turbine_generator_t));
    memcpy(&telemetry->pitch, &g_turbine.pitch, sizeof(gf_turbine_pitch_t));
    memcpy(&telemetry->yaw, &g_turbine.yaw, sizeof(gf_turbine_yaw_t));
    
    if (g_turbine.config.rated_power_kw > 0.0f) {
        telemetry->capacity_factor = g_turbine.generator.power_kw / g_turbine.config.rated_power_kw;
    }
    
    telemetry->runtime_hours = g_turbine.runtime_hours;
    telemetry->energy_produced_kwh = g_turbine.energy_produced_kwh;
    
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_register_state_callback(gf_turbine_state_cb_t cb,
                                                        void* user_data)
{
    g_turbine.state_callback = cb;
    g_turbine.state_cb_user_data = user_data;
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_register_alarm_callback(gf_turbine_alarm_cb_t cb,
                                                        void* user_data)
{
    g_turbine.alarm_callback = cb;
    g_turbine.alarm_cb_user_data = user_data;
    return GF_TURBINE_OK;
}

gf_turbine_status_t gf_turbine_process(void)
{
    if (!g_turbine.initialized) {
        return GF_TURBINE_ERROR_NOT_INITIALIZED;
    }
    
    uint64_t now = get_time_ms();
    float dt = (float)(now - g_turbine.control_timestamp_ms) / 1000.0f;
    
    if (dt < 0.001f) dt = 0.001f;
    if (dt > 1.0f) dt = 1.0f;
    
    g_turbine.control_timestamp_ms = now;
    
    /* Update control loops */
    turbine_update_pitch(dt);
    turbine_update_yaw(dt);
    turbine_update_rotor(dt);
    
    /* Check safety */
    gf_turbine_status_t safety_status = turbine_check_safety();
    
    /* Update state machine */
    turbine_update_state(safety_status);
    
    /* Update energy counter */
    if (g_turbine.state == GF_TURBINE_STATE_POWER_PRODUCTION) {
        float hours = (float)(now - g_turbine.last_energy_update_ms) / 3600000.0f;
        g_turbine.energy_produced_kwh += (uint32_t)(g_turbine.generator.power_kw * hours);
        g_turbine.last_energy_update_ms = now;
    }
    
    return safety_status;
}

/*===========================================================================*/
/* Grid Sync API Implementation                                               */
/*===========================================================================*/

gf_grid_status_t gf_grid_init(const gf_grid_config_t* config)
{
    if (!config) {
        return GF_GRID_ERROR_NULL_PTR;
    }
    
    memset(&g_grid, 0, sizeof(g_grid));
    memcpy(&g_grid.config, config, sizeof(gf_grid_config_t));
    
    /* Initialize PLL PID */
    g_grid.pll_pid = (gf_pid_controller_t){
        .kp = 10.0f, .ki = 100.0f, .kd = 0.1f,
        .output_min = -5.0f, .output_max = 5.0f,
        .anti_windup = true
    };
    
    g_grid.state = GF_GRID_STATE_DISCONNECTED;
    g_grid.pll_frequency = config->nominal_frequency_hz;
    
    g_grid.initialized = true;
    return GF_GRID_OK;
}

void gf_grid_shutdown(void)
{
    if (g_grid.state != GF_GRID_STATE_DISCONNECTED) {
        gf_grid_disconnect();
    }
    g_grid.initialized = false;
}

gf_grid_status_t gf_grid_connect(void)
{
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    g_grid.state = GF_GRID_STATE_MONITORING;
    g_grid.sync_start_ms = get_time_ms();
    gf_pid_reset(&g_grid.pll_pid);
    
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_disconnect(void)
{
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    gf_grid_state_t old_state = g_grid.state;
    g_grid.state = GF_GRID_STATE_DISCONNECTED;
    g_grid.export_enabled = false;
    
    if (g_grid.state_callback && old_state != g_grid.state) {
        g_grid.state_callback(old_state, g_grid.state, g_grid.state_cb_user_data);
    }
    
    return GF_GRID_OK;
}

gf_grid_state_t gf_grid_get_state(void)
{
    return g_grid.state;
}

gf_grid_status_t gf_grid_update_measurements(const gf_grid_measurement_t* meas)
{
    if (!meas) {
        return GF_GRID_ERROR_NULL_PTR;
    }
    
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    memcpy(&g_grid.measurement, meas, sizeof(gf_grid_measurement_t));
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_get_measurements(gf_grid_measurement_t* meas)
{
    if (!meas) {
        return GF_GRID_ERROR_NULL_PTR;
    }
    
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    memcpy(meas, &g_grid.measurement, sizeof(gf_grid_measurement_t));
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_start_sync(void)
{
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    if (g_grid.state != GF_GRID_STATE_MONITORING) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    g_grid.state = GF_GRID_STATE_PRE_SYNC;
    g_grid.lock_time_ms = 0;
    
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_get_sync_status(gf_grid_sync_status_t* status)
{
    if (!status) {
        return GF_GRID_ERROR_NULL_PTR;
    }
    
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    status->freq_error_hz = g_grid.freq_error_hz;
    status->voltage_error_v = g_grid.voltage_error_v;
    status->phase_error_deg = g_grid.phase_error_deg;
    status->freq_locked = g_grid.freq_locked;
    status->voltage_matched = g_grid.voltage_matched;
    status->phase_locked = g_grid.phase_locked;
    status->ready_to_close = g_grid.freq_locked && g_grid.voltage_matched && g_grid.phase_locked;
    status->lock_time_ms = g_grid.lock_time_ms;
    
    return GF_GRID_OK;
}

bool gf_grid_is_synchronized(void)
{
    return g_grid.state == GF_GRID_STATE_CONNECTED || g_grid.state == GF_GRID_STATE_EXPORTING;
}

gf_grid_status_t gf_grid_set_power(const gf_grid_power_setpoint_t* setpoint)
{
    if (!setpoint) {
        return GF_GRID_ERROR_NULL_PTR;
    }
    
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    memcpy(&g_grid.power_setpoint, setpoint, sizeof(gf_grid_power_setpoint_t));
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_set_power_factor(float pf)
{
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    if (pf < -1.0f || pf > 1.0f) {
        return GF_GRID_ERROR_FREQ_OUT_OF_RANGE;
    }
    
    g_grid.power_setpoint.power_factor = pf;
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_enable_export(bool enable)
{
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    if (enable && g_grid.state != GF_GRID_STATE_CONNECTED) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    g_grid.export_enabled = enable;
    
    if (enable) {
        g_grid.state = GF_GRID_STATE_EXPORTING;
    } else if (g_grid.state == GF_GRID_STATE_EXPORTING) {
        g_grid.state = GF_GRID_STATE_CONNECTED;
    }
    
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_get_fault(gf_grid_fault_t* fault)
{
    if (!fault) {
        return GF_GRID_ERROR_NULL_PTR;
    }
    
    *fault = g_grid.fault;
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_clear_fault(void)
{
    g_grid.fault = GF_GRID_FAULT_NONE;
    
    if (g_grid.state == GF_GRID_STATE_FAULT) {
        g_grid.state = GF_GRID_STATE_DISCONNECTED;
    }
    
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_register_state_callback(gf_grid_state_cb_t cb, void* user_data)
{
    g_grid.state_callback = cb;
    g_grid.state_cb_user_data = user_data;
    return GF_GRID_OK;
}

gf_grid_status_t gf_grid_register_fault_callback(gf_grid_fault_cb_t cb, void* user_data)
{
    g_grid.fault_callback = cb;
    g_grid.fault_cb_user_data = user_data;
    return GF_GRID_OK;
}

/**
 * @brief Phase-locked loop for grid synchronization
 */
static void grid_update_pll(float dt)
{
    if (dt <= 0.0f) return;
    
    /* Phase detector: compare inverter phase to grid phase */
    float phase_error = normalize_angle(g_grid.measurement.phase_angle_deg - g_grid.pll_phase);
    g_grid.phase_error_deg = phase_error;
    
    /* Loop filter (PI controller) */
    float freq_correction = gf_pid_update(&g_grid.pll_pid, 0.0f, phase_error, dt);
    
    /* VCO: update phase based on frequency */
    g_grid.pll_frequency = g_grid.config.nominal_frequency_hz + freq_correction;
    g_grid.pll_phase += g_grid.pll_frequency * 360.0f * dt;
    
    /* Normalize phase */
    while (g_grid.pll_phase > 180.0f) g_grid.pll_phase -= 360.0f;
    while (g_grid.pll_phase < -180.0f) g_grid.pll_phase += 360.0f;
    
    /* Frequency error */
    g_grid.freq_error_hz = g_grid.measurement.frequency_hz - g_grid.config.nominal_frequency_hz;
    
    /* Check locks */
    g_grid.freq_locked = fabsf(g_grid.freq_error_hz) < g_grid.config.freq_tolerance_hz;
    g_grid.phase_locked = fabsf(phase_error) < GRID_PHASE_TOLERANCE_DEG;
    
    /* Voltage check */
    float avg_voltage = (g_grid.measurement.voltage_v[0] + g_grid.measurement.voltage_v[1] +
                         g_grid.measurement.voltage_v[2]) / 3.0f;
    g_grid.voltage_error_v = avg_voltage - g_grid.config.nominal_voltage_v;
    float volt_error_pct = fabsf(g_grid.voltage_error_v) / g_grid.config.nominal_voltage_v * 100.0f;
    g_grid.voltage_matched = volt_error_pct < g_grid.config.voltage_tolerance_pct;
}

/**
 * @brief Grid state machine
 */
static void grid_update_state(void)
{
    gf_grid_state_t new_state = g_grid.state;
    
    switch (g_grid.state) {
    case GF_GRID_STATE_DISCONNECTED:
        break;
        
    case GF_GRID_STATE_MONITORING:
        /* Just observing grid */
        break;
        
    case GF_GRID_STATE_PRE_SYNC:
        /* Waiting for conditions */
        if (g_grid.freq_locked && g_grid.voltage_matched) {
            new_state = GF_GRID_STATE_SYNCHRONIZING;
        }
        break;
        
    case GF_GRID_STATE_SYNCHRONIZING:
        /* Tracking phase */
        if (g_grid.freq_locked && g_grid.voltage_matched && g_grid.phase_locked) {
            g_grid.lock_time_ms += GRID_SYNC_PERIOD_MS;
            if (g_grid.lock_time_ms > 500) {  /* Lock for 500ms */
                new_state = GF_GRID_STATE_CONNECTED;
            }
        } else {
            g_grid.lock_time_ms = 0;
        }
        break;
        
    case GF_GRID_STATE_CONNECTED:
    case GF_GRID_STATE_EXPORTING:
        /* Check for faults */
        if (!g_grid.freq_locked) {
            if (g_grid.freq_error_hz > 2.0f) {
                g_grid.fault = GF_GRID_FAULT_OVERFREQ;
            } else if (g_grid.freq_error_hz < -2.0f) {
                g_grid.fault = GF_GRID_FAULT_UNDERFREQ;
            }
            new_state = GF_GRID_STATE_FAULT;
        } else if (!g_grid.voltage_matched) {
            if (g_grid.voltage_error_v > 0) {
                g_grid.fault = GF_GRID_FAULT_OVERVOLTAGE;
            } else {
                g_grid.fault = GF_GRID_FAULT_UNDERVOLTAGE;
            }
            new_state = GF_GRID_STATE_FAULT;
        }
        break;
        
    case GF_GRID_STATE_FAULT_RIDE_THROUGH:
        /* Attempting to ride through */
        break;
        
    case GF_GRID_STATE_ISLANDED:
        /* Detected island condition */
        break;
        
    case GF_GRID_STATE_FAULT:
        /* Faulted - disconnect */
        g_grid.export_enabled = false;
        break;
    }
    
    if (new_state != g_grid.state) {
        gf_grid_state_t old_state = g_grid.state;
        g_grid.state = new_state;
        
        if (g_grid.state_callback) {
            g_grid.state_callback(old_state, new_state, g_grid.state_cb_user_data);
        }
        
        if (new_state == GF_GRID_STATE_FAULT && g_grid.fault_callback) {
            g_grid.fault_callback(g_grid.fault, &g_grid.measurement, g_grid.fault_cb_user_data);
        }
    }
}

gf_grid_status_t gf_grid_process(void)
{
    if (!g_grid.initialized) {
        return GF_GRID_ERROR_NOT_INITIALIZED;
    }
    
    uint64_t now = get_time_ms();
    float dt = (float)(now - g_grid.last_process_ms) / 1000.0f;
    
    if (dt < 0.001f) dt = 0.001f;
    if (dt > 0.1f) dt = 0.1f;
    
    g_grid.last_process_ms = now;
    
    /* Update PLL */
    if (g_grid.state >= GF_GRID_STATE_PRE_SYNC && g_grid.state <= GF_GRID_STATE_EXPORTING) {
        grid_update_pll(dt);
    }
    
    /* Update state machine */
    grid_update_state();
    
    /* Timeout check */
    if (g_grid.state == GF_GRID_STATE_SYNCHRONIZING) {
        if ((now - g_grid.sync_start_ms) > g_grid.config.sync_timeout_ms) {
            return GF_GRID_ERROR_SYNC_TIMEOUT;
        }
    }
    
    return GF_GRID_OK;
}

/*===========================================================================*/
/* Integrated Control Functions                                               */
/*===========================================================================*/

/**
 * @brief Combined turbine + grid control cycle
 */
gf_turbine_status_t gf_renewable_process(void)
{
    gf_turbine_status_t turbine_status = gf_turbine_process();
    gf_grid_status_t grid_status = gf_grid_process();
    
    /* Link turbine generator to grid */
    if (g_turbine.state == GF_TURBINE_STATE_POWER_PRODUCTION &&
        g_grid.state == GF_GRID_STATE_CONNECTED) {
        
        /* Update grid measurements from generator */
        g_grid.measurement.active_power_kw = g_turbine.generator.power_kw;
        g_grid.measurement.frequency_hz = GRID_FREQ_NOMINAL_HZ;  /* Grid-tied: locked to grid */
        g_grid.measurement.power_factor = g_turbine.generator.power_factor;
    }
    
    /* Handle grid faults */
    if (grid_status != GF_GRID_OK && grid_status != GF_GRID_ERROR_NOT_INITIALIZED) {
        if (g_turbine.state == GF_TURBINE_STATE_POWER_PRODUCTION) {
            /* Reduce power on grid issues */
            gf_turbine_set_power_limit(g_turbine.config.rated_power_kw * 0.5f);
        }
    }
    
    return turbine_status;
}
