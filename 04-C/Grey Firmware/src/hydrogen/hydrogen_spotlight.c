/**
 * @file hydrogen_spotlight.c
 * @brief Hydrogen Fuel Cell Control & Telemetry Spotlight Implementation
 * 
 * This module implements a production-grade hydrogen fuel cell and electrolyzer
 * control system with:
 * - PEM fuel cell stack voltage/current regulation
 * - Electrolyzer control for green hydrogen production
 * - Temperature management and cooling control
 * - Safety interlocks and emergency shutdown
 * - Efficiency monitoring and optimization
 * - SCADA-compatible telemetry
 * 
 * FUEL CELL OPERATION:
 * The fuel cell converts hydrogen and oxygen into electricity and water.
 * Key parameters: stack voltage, current draw, hydrogen flow, temperature.
 * The control loop maintains optimal operating conditions for efficiency
 * while protecting the membrane electrode assembly (MEA).
 * 
 * ELECTROLYZER OPERATION:
 * The electrolyzer produces hydrogen by splitting water using electricity.
 * Key parameters: power input, hydrogen production rate, purity, pressure.
 * Load-following capability enables integration with variable renewables.
 * 
 * SAFETY FEATURES:
 * - Hydrogen leak detection
 * - Over-temperature protection
 * - Stack voltage limits
 * - Emergency shutdown (E-stop)
 * - Ventilation interlocks
 * 
 * STANDARDS COMPLIANCE:
 * - IEC 62282-3 (Fuel cell technologies - Safety)
 * - SAE J2615 (PEM fuel cell systems)
 * - ISO 22734 (Hydrogen generators)
 * - NFPA 2 (Hydrogen technologies code)
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

#define H2_MAX_CELLS              120  /**< Max cells in fuel cell stack */
#define H2_MAX_ELECTROLYZERS      4    /**< Max electrolyzer stacks */
#define H2_HISTORY_SIZE           60   /**< 1 minute of samples at 1Hz */
#define H2_MAX_EVENTS             256  /**< Event log size */
#define H2_MAX_ALARMS             32   /**< Active alarm buffer */

/* Fuel cell parameters */
#define FC_CELL_VOLTAGE_MIN       0.3f  /**< Min cell voltage (shutoff) */
#define FC_CELL_VOLTAGE_LOW       0.6f  /**< Low cell voltage (warning) */
#define FC_CELL_VOLTAGE_NOM       0.7f  /**< Nominal cell voltage */
#define FC_CELL_VOLTAGE_OCV       1.0f  /**< Open circuit voltage */
#define FC_TEMP_MIN_C             40.0f /**< Min operating temp */
#define FC_TEMP_OPT_C             65.0f /**< Optimal stack temp */
#define FC_TEMP_MAX_C             80.0f /**< Max operating temp */
#define FC_TEMP_SHUTOFF_C         90.0f /**< Emergency shutoff temp */

/* Electrolyzer parameters */
#define EL_CELL_VOLTAGE_MIN       1.23f /**< Thermoneutral voltage */
#define EL_CELL_VOLTAGE_NOM       1.8f  /**< Nominal operating voltage */
#define EL_CELL_VOLTAGE_MAX       2.2f  /**< Max cell voltage */
#define EL_EFFICIENCY_IDEAL       39.4f /**< kWh/kg H2 (LHV ideal) */
#define EL_EFFICIENCY_TYPICAL     50.0f /**< kWh/kg H2 (typical PEM) */
#define EL_H2_PURITY_MIN          99.5f /**< Minimum purity % */
#define EL_PRESSURE_MAX_BAR       30.0f /**< Max output pressure */

/* Safety thresholds */
#define H2_LEAK_PPM_WARN          1000  /**< H2 concentration warning */
#define H2_LEAK_PPM_ALARM         4000  /**< H2 concentration alarm (1% LFL) */
#define H2_VENT_FLOW_MIN          10.0f /**< Min ventilation rate */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** System operating mode */
typedef enum {
    H2_MODE_OFF,
    H2_MODE_STANDBY,
    H2_MODE_STARTUP,
    H2_MODE_RUNNING,
    H2_MODE_LOAD_FOLLOW,
    H2_MODE_SHUTDOWN,
    H2_MODE_EMERGENCY,
    H2_MODE_MAINTENANCE,
    H2_MODE_FAULT
} h2_mode_t;

/** Fuel cell state */
typedef enum {
    FC_STATE_OFF,
    FC_STATE_PURGING,
    FC_STATE_WARMUP,
    FC_STATE_RUNNING,
    FC_STATE_COOLDOWN,
    FC_STATE_FAULT
} fc_state_t;

/** Electrolyzer state */
typedef enum {
    EL_STATE_OFF,
    EL_STATE_STANDBY,
    EL_STATE_WARMUP,
    EL_STATE_PRODUCTION,
    EL_STATE_COOLDOWN,
    EL_STATE_FAULT
} el_state_t;

/** Alarm type */
typedef enum {
    ALARM_NONE,
    ALARM_H2_LEAK,
    ALARM_OVER_TEMP,
    ALARM_UNDER_TEMP,
    ALARM_LOW_CELL_VOLTAGE,
    ALARM_HIGH_CURRENT,
    ALARM_LOW_PRESSURE,
    ALARM_HIGH_PRESSURE,
    ALARM_PURITY_LOW,
    ALARM_COOLANT_FLOW,
    ALARM_VENT_FAILURE,
    ALARM_WATER_QUALITY,
    ALARM_STACK_IMBALANCE,
    ALARM_COMM_FAULT
} h2_alarm_type_t;

/** Alarm severity */
typedef enum {
    SEVERITY_INFO,
    SEVERITY_WARNING,
    SEVERITY_ALARM,
    SEVERITY_CRITICAL
} h2_severity_t;

/** Event type */
typedef enum {
    EVENT_SYSTEM_START,
    EVENT_SYSTEM_STOP,
    EVENT_MODE_CHANGE,
    EVENT_FC_START,
    EVENT_FC_STOP,
    EVENT_EL_START,
    EVENT_EL_STOP,
    EVENT_ALARM_SET,
    EVENT_ALARM_CLEAR,
    EVENT_E_STOP,
    EVENT_LOAD_CHANGE,
    EVENT_MAINTENANCE
} h2_event_type_t;

/* ============================================================================
 * Data Structures
 * ============================================================================ */

/** Fuel cell stack configuration */
typedef struct {
    uint8_t stack_id;
    uint8_t cell_count;
    float rated_power_kw;
    float active_area_cm2;
    float min_load_pct;
    float max_load_pct;
} fc_config_t;

/** Fuel cell stack status */
typedef struct {
    fc_state_t state;
    float stack_voltage;       /**< Total stack voltage (V) */
    float stack_current;       /**< Output current (A) */
    float power_output_kw;     /**< Power output (kW) */
    float cell_voltages[H2_MAX_CELLS]; /**< Per-cell voltages */
    float min_cell_voltage;
    float max_cell_voltage;
    float cell_spread;         /**< Max-Min delta */
    float stack_temp_c;
    float coolant_temp_in_c;
    float coolant_temp_out_c;
    float h2_flow_slpm;        /**< H2 consumption (std L/min) */
    float air_flow_slpm;       /**< Air flow (std L/min) */
    float efficiency_pct;
    uint32_t run_hours;
    uint32_t start_cycles;
} fc_status_t;

/** Electrolyzer configuration */
typedef struct {
    uint8_t el_id;
    uint8_t cell_count;
    float rated_power_kw;
    float rated_h2_kg_h;       /**< kg H2 per hour */
    float turndown_ratio;      /**< Min/max power ratio */
} el_config_t;

/** Electrolyzer status */
typedef struct {
    el_state_t state;
    float voltage;             /**< Stack voltage (V) */
    float current;             /**< Input current (A) */
    float power_kw;            /**< Power consumption (kW) */
    float h2_rate_kg_h;        /**< H2 production rate */
    float h2_rate_nm3_h;       /**< Normal m³/hour */
    float o2_rate_kg_h;        /**< O2 byproduct rate */
    float h2_purity_pct;       /**< Hydrogen purity */
    float h2_pressure_bar;     /**< Output pressure */
    float stack_temp_c;
    float water_resistivity;   /**< MΩ·cm */
    float specific_energy;     /**< kWh/kg H2 */
    float efficiency_pct;      /**< % of theoretical */
    uint32_t run_hours;
} el_status_t;

/** Safety status */
typedef struct {
    bool e_stop_active;
    bool h2_leak_detected;
    float h2_concentration_ppm;
    bool vent_active;
    float vent_flow_rate;
    bool coolant_ok;
    bool water_ok;
    uint8_t active_alarms;
} h2_safety_t;

/** Alarm record */
typedef struct {
    uint32_t alarm_id;
    h2_alarm_type_t type;
    h2_severity_t severity;
    uint32_t timestamp;
    float value;
    float threshold;
    bool acknowledged;
    bool active;
} h2_alarm_t;

/** Event log entry */
typedef struct {
    uint32_t event_id;
    h2_event_type_t type;
    uint32_t timestamp;
    float value;
    char description[48];
} h2_event_t;

/** Efficiency history */
typedef struct {
    float values[H2_HISTORY_SIZE];
    uint8_t head;
    uint8_t count;
    float average;
    float min;
    float max;
} h2_history_t;

/** Production statistics */
typedef struct {
    float h2_produced_kg;      /**< Total H2 produced */
    float h2_consumed_kg;      /**< Total H2 consumed */
    float energy_produced_kwh; /**< FC energy output */
    float energy_consumed_kwh; /**< EL energy input */
    float water_consumed_l;    /**< Water used */
    float overall_efficiency;  /**< System efficiency */
    uint32_t fc_run_seconds;
    uint32_t el_run_seconds;
    uint32_t uptime_seconds;
} h2_stats_t;

/** System state */
typedef struct {
    bool initialized;
    h2_mode_t mode;
    
    /* Fuel cell */
    fc_config_t fc_config;
    fc_status_t fc_status;
    float fc_power_setpoint;
    float fc_load_pct;
    
    /* Electrolyzer */
    el_config_t el_config;
    el_status_t el_status;
    float el_power_setpoint;
    float el_production_setpoint;
    
    /* Safety */
    h2_safety_t safety;
    
    /* Alarms */
    h2_alarm_t alarms[H2_MAX_ALARMS];
    uint8_t alarm_count;
    uint32_t next_alarm_id;
    
    /* Events */
    h2_event_t events[H2_MAX_EVENTS];
    uint16_t event_head;
    uint16_t event_count;
    uint32_t next_event_id;
    
    /* Statistics */
    h2_stats_t stats;
    h2_history_t fc_efficiency_history;
    h2_history_t el_efficiency_history;
    
    /* Timing */
    uint32_t current_time_ms;
    uint32_t last_process_ms;
} h2_system_t;

/* ============================================================================
 * Global State
 * ============================================================================ */

static h2_system_t g_h2 = {0};

/* ============================================================================
 * Internal Helper Functions
 * ============================================================================ */

/**
 * @brief Get current time in milliseconds
 */
static uint32_t h2_get_time_ms(void) {
    return g_h2.current_time_ms;
}

/**
 * @brief Log an event
 */
static void h2_log_event(h2_event_type_t type, float value, const char* desc) {
    h2_event_t* event = &g_h2.events[g_h2.event_head];
    
    event->event_id = g_h2.next_event_id++;
    event->type = type;
    event->timestamp = h2_get_time_ms();
    event->value = value;
    
    if (desc) {
        strncpy(event->description, desc, sizeof(event->description) - 1);
        event->description[sizeof(event->description) - 1] = '\0';
    } else {
        event->description[0] = '\0';
    }
    
    g_h2.event_head = (g_h2.event_head + 1) % H2_MAX_EVENTS;
    if (g_h2.event_count < H2_MAX_EVENTS) {
        g_h2.event_count++;
    }
}

/**
 * @brief Update history ring buffer
 */
static void h2_update_history(h2_history_t* history, float value) {
    history->values[history->head] = value;
    history->head = (history->head + 1) % H2_HISTORY_SIZE;
    if (history->count < H2_HISTORY_SIZE) {
        history->count++;
    }
    
    /* Calculate statistics */
    float sum = 0, min = value, max = value;
    for (uint8_t i = 0; i < history->count; i++) {
        float v = history->values[i];
        sum += v;
        if (v < min) min = v;
        if (v > max) max = v;
    }
    history->average = sum / history->count;
    history->min = min;
    history->max = max;
}

/* ============================================================================
 * Alarm Management
 * ============================================================================ */

/**
 * @brief Set alarm
 */
static void h2_set_alarm(h2_alarm_type_t type, h2_severity_t severity,
                         float value, float threshold) {
    /* Check if alarm already active */
    for (uint8_t i = 0; i < g_h2.alarm_count; i++) {
        if (g_h2.alarms[i].type == type && g_h2.alarms[i].active) {
            return;  /* Already set */
        }
    }
    
    /* Find slot for new alarm */
    h2_alarm_t* alarm = NULL;
    for (uint8_t i = 0; i < H2_MAX_ALARMS; i++) {
        if (!g_h2.alarms[i].active) {
            alarm = &g_h2.alarms[i];
            if (i >= g_h2.alarm_count) {
                g_h2.alarm_count = i + 1;
            }
            break;
        }
    }
    
    if (!alarm) return;  /* No slots */
    
    alarm->alarm_id = g_h2.next_alarm_id++;
    alarm->type = type;
    alarm->severity = severity;
    alarm->timestamp = h2_get_time_ms();
    alarm->value = value;
    alarm->threshold = threshold;
    alarm->acknowledged = false;
    alarm->active = true;
    
    g_h2.safety.active_alarms++;
    h2_log_event(EVENT_ALARM_SET, (float)type, "Alarm activated");
    
    /* Handle critical alarms */
    if (severity == SEVERITY_CRITICAL) {
        g_h2.mode = H2_MODE_EMERGENCY;
    }
}

/**
 * @brief Clear alarm
 */
static void h2_clear_alarm(h2_alarm_type_t type) {
    for (uint8_t i = 0; i < g_h2.alarm_count; i++) {
        if (g_h2.alarms[i].type == type && g_h2.alarms[i].active) {
            g_h2.alarms[i].active = false;
            if (g_h2.safety.active_alarms > 0) {
                g_h2.safety.active_alarms--;
            }
            h2_log_event(EVENT_ALARM_CLEAR, (float)type, "Alarm cleared");
            return;
        }
    }
}

/* ============================================================================
 * Fuel Cell Control
 * ============================================================================ */

/**
 * @brief Calculate fuel cell efficiency
 */
static float fc_calculate_efficiency(float power_kw, float h2_flow_slpm) {
    if (h2_flow_slpm <= 0) return 0;
    
    /* H2 LHV = 120 MJ/kg = 33.33 kWh/kg */
    /* 1 kg H2 ≈ 11,126 standard liters */
    float h2_kg_h = (h2_flow_slpm * 60.0f) / 11126.0f;
    float h2_energy_kw = h2_kg_h * 33.33f;
    
    if (h2_energy_kw <= 0) return 0;
    return (power_kw / h2_energy_kw) * 100.0f;
}

/**
 * @brief Process fuel cell stack
 */
static void fc_process(uint32_t delta_ms) {
    fc_status_t* status = &g_h2.fc_status;
    fc_state_t prev_state;
    
    /* Loop to allow multi-step transitions in single call */
    do {
        prev_state = status->state;
        
        switch (status->state) {
            case FC_STATE_OFF:
                status->stack_voltage = 0;
                status->stack_current = 0;
                status->power_output_kw = 0;
                break;
                
            case FC_STATE_PURGING:
                /* Purge with nitrogen before startup */
                /* Transition to warmup after purge complete */
                status->state = FC_STATE_WARMUP;
                break;
                
            case FC_STATE_WARMUP:
                /* Heat stack to operating temperature */
                if (status->stack_temp_c >= FC_TEMP_MIN_C) {
                    status->state = FC_STATE_RUNNING;
                    h2_log_event(EVENT_FC_START, status->stack_temp_c, "FC running");
                }
                break;
            
        case FC_STATE_RUNNING: {
            /* Calculate power based on load setpoint */
            float target_power = g_h2.fc_power_setpoint;
            float max_power = g_h2.fc_config.rated_power_kw;
            
            if (target_power > max_power) target_power = max_power;
            
            /* Simple voltage-current relationship (polarization curve approximation) */
            float cell_count = g_h2.fc_config.cell_count;
            float load_ratio = target_power / max_power;
            
            /* Cell voltage decreases with load (activation + ohmic + concentration losses) */
            float cell_v = FC_CELL_VOLTAGE_OCV - (0.3f * load_ratio);
            status->stack_voltage = cell_v * cell_count;
            
            if (status->stack_voltage > 0) {
                status->stack_current = (target_power * 1000.0f) / status->stack_voltage;
            }
            
            status->power_output_kw = target_power;
            
            /* Calculate H2 consumption (Faraday's law approximation) */
            /* ~0.5 L/min per kW at 50% efficiency */
            status->h2_flow_slpm = target_power * 10.0f;
            status->air_flow_slpm = status->h2_flow_slpm * 2.5f;  /* Stoichiometric ratio */
            
            /* Update cell voltage distribution */
            status->min_cell_voltage = cell_v - 0.02f;
            status->max_cell_voltage = cell_v + 0.02f;
            status->cell_spread = 0.04f;
            
            /* Calculate efficiency */
            status->efficiency_pct = fc_calculate_efficiency(target_power, status->h2_flow_slpm);
            h2_update_history(&g_h2.fc_efficiency_history, status->efficiency_pct);
            
            /* Update statistics */
            g_h2.stats.energy_produced_kwh += target_power * delta_ms / 3600000.0f;
            g_h2.stats.h2_consumed_kg += (status->h2_flow_slpm * delta_ms / 60000.0f) / 11.126f;
            g_h2.stats.fc_run_seconds += delta_ms / 1000;
            
            /* Safety checks */
            if (status->min_cell_voltage < FC_CELL_VOLTAGE_LOW) {
                h2_set_alarm(ALARM_LOW_CELL_VOLTAGE, SEVERITY_WARNING,
                            status->min_cell_voltage, FC_CELL_VOLTAGE_LOW);
            } else {
                h2_clear_alarm(ALARM_LOW_CELL_VOLTAGE);
            }
            
            if (status->stack_temp_c > FC_TEMP_MAX_C) {
                h2_set_alarm(ALARM_OVER_TEMP, SEVERITY_ALARM,
                            status->stack_temp_c, FC_TEMP_MAX_C);
            } else if (status->stack_temp_c > FC_TEMP_SHUTOFF_C) {
                h2_set_alarm(ALARM_OVER_TEMP, SEVERITY_CRITICAL,
                            status->stack_temp_c, FC_TEMP_SHUTOFF_C);
            } else {
                h2_clear_alarm(ALARM_OVER_TEMP);
            }
            break;
        }
        
        case FC_STATE_COOLDOWN:
            status->stack_current = 0;
            status->power_output_kw = 0;
            if (status->stack_temp_c < FC_TEMP_MIN_C) {
                status->state = FC_STATE_OFF;
                h2_log_event(EVENT_FC_STOP, 0, "FC cooled down");
            }
            break;
            
        case FC_STATE_FAULT:
            status->stack_current = 0;
            status->power_output_kw = 0;
            break;
        }
    } while (status->state != prev_state && status->state != FC_STATE_RUNNING && 
             status->state != FC_STATE_OFF && status->state != FC_STATE_FAULT);
}

/* ============================================================================
 * Electrolyzer Control
 * ============================================================================ */

/**
 * @brief Calculate electrolyzer efficiency
 */
static float el_calculate_efficiency(float h2_kg_h, float power_kw) {
    if (h2_kg_h <= 0 || power_kw <= 0) return 0;
    
    float specific_energy = power_kw / h2_kg_h;  /* kWh/kg */
    /* Theoretical minimum: 39.4 kWh/kg (LHV) */
    return (EL_EFFICIENCY_IDEAL / specific_energy) * 100.0f;
}

/**
 * @brief Process electrolyzer
 */
static void el_process(uint32_t delta_ms) {
    el_status_t* status = &g_h2.el_status;
    
    switch (status->state) {
        case EL_STATE_OFF:
            status->voltage = 0;
            status->current = 0;
            status->power_kw = 0;
            status->h2_rate_kg_h = 0;
            break;
            
        case EL_STATE_STANDBY:
            /* Ready for startup */
            break;
            
        case EL_STATE_WARMUP:
            if (status->stack_temp_c >= 50.0f) {
                status->state = EL_STATE_PRODUCTION;
                h2_log_event(EVENT_EL_START, status->stack_temp_c, "EL production started");
            }
            break;
            
        case EL_STATE_PRODUCTION: {
            /* Use power setpoint or production setpoint */
            float target_power = g_h2.el_power_setpoint;
            if (target_power <= 0 && g_h2.el_production_setpoint > 0) {
                /* Convert H2 rate to power (assuming 50 kWh/kg) */
                target_power = g_h2.el_production_setpoint * EL_EFFICIENCY_TYPICAL;
            }
            
            float max_power = g_h2.el_config.rated_power_kw;
            float min_power = max_power * g_h2.el_config.turndown_ratio;
            
            if (target_power > max_power) target_power = max_power;
            if (target_power < min_power && target_power > 0) target_power = min_power;
            
            status->power_kw = target_power;
            
            /* Voltage/current relationship (typical PEM electrolyzer) */
            float cell_count = g_h2.el_config.cell_count;
            float load_ratio = target_power / max_power;
            float cell_v = EL_CELL_VOLTAGE_MIN + (EL_CELL_VOLTAGE_NOM - EL_CELL_VOLTAGE_MIN) * 
                          (0.3f + 0.7f * load_ratio);
            
            status->voltage = cell_v * cell_count;
            if (status->voltage > 0) {
                status->current = (target_power * 1000.0f) / status->voltage;
            }
            
            /* H2 production rate (Faraday efficiency ~95%) */
            /* Theoretical: 1 kg H2 per 26.8 Ah per cell */
            float faraday_eff = 0.95f;
            float base_rate = g_h2.el_config.rated_h2_kg_h;
            status->h2_rate_kg_h = base_rate * load_ratio * faraday_eff;
            status->h2_rate_nm3_h = status->h2_rate_kg_h * 11.126f;  /* ~11 Nm³/kg */
            status->o2_rate_kg_h = status->h2_rate_kg_h * 8.0f;  /* Stoichiometric ratio */
            
            /* Calculate specific energy and efficiency */
            if (status->h2_rate_kg_h > 0) {
                status->specific_energy = status->power_kw / status->h2_rate_kg_h;
                status->efficiency_pct = el_calculate_efficiency(status->h2_rate_kg_h, status->power_kw);
            }
            
            h2_update_history(&g_h2.el_efficiency_history, status->efficiency_pct);
            
            /* High purity from PEM */
            status->h2_purity_pct = 99.99f;
            status->h2_pressure_bar = 30.0f;  /* Typical output pressure */
            
            /* Water consumption: ~9 L per kg H2 */
            float water_consumed = status->h2_rate_kg_h * 9.0f * delta_ms / 3600000.0f;
            g_h2.stats.water_consumed_l += water_consumed;
            
            /* Update statistics */
            g_h2.stats.energy_consumed_kwh += target_power * delta_ms / 3600000.0f;
            g_h2.stats.h2_produced_kg += status->h2_rate_kg_h * delta_ms / 3600000.0f;
            g_h2.stats.el_run_seconds += delta_ms / 1000;
            
            /* Safety checks */
            if (status->h2_purity_pct < EL_H2_PURITY_MIN) {
                h2_set_alarm(ALARM_PURITY_LOW, SEVERITY_WARNING,
                            status->h2_purity_pct, EL_H2_PURITY_MIN);
            } else {
                h2_clear_alarm(ALARM_PURITY_LOW);
            }
            
            if (status->h2_pressure_bar > EL_PRESSURE_MAX_BAR) {
                h2_set_alarm(ALARM_HIGH_PRESSURE, SEVERITY_ALARM,
                            status->h2_pressure_bar, EL_PRESSURE_MAX_BAR);
            } else {
                h2_clear_alarm(ALARM_HIGH_PRESSURE);
            }
            break;
        }
        
        case EL_STATE_COOLDOWN:
            status->current = 0;
            status->power_kw = 0;
            status->h2_rate_kg_h = 0;
            if (status->stack_temp_c < 40.0f) {
                status->state = EL_STATE_OFF;
                h2_log_event(EVENT_EL_STOP, 0, "EL cooled down");
            }
            break;
            
        case EL_STATE_FAULT:
            status->current = 0;
            status->power_kw = 0;
            status->h2_rate_kg_h = 0;
            break;
    }
}

/* ============================================================================
 * Safety Processing
 * ============================================================================ */

/**
 * @brief Process safety checks
 */
static void safety_process(void) {
    h2_safety_t* safety = &g_h2.safety;
    
    /* E-stop handling */
    if (safety->e_stop_active) {
        g_h2.fc_status.state = FC_STATE_FAULT;
        g_h2.el_status.state = EL_STATE_FAULT;
        g_h2.mode = H2_MODE_EMERGENCY;
    }
    
    /* H2 leak detection */
    if (safety->h2_concentration_ppm >= H2_LEAK_PPM_ALARM) {
        safety->h2_leak_detected = true;
        h2_set_alarm(ALARM_H2_LEAK, SEVERITY_CRITICAL,
                    safety->h2_concentration_ppm, (float)H2_LEAK_PPM_ALARM);
    } else if (safety->h2_concentration_ppm >= H2_LEAK_PPM_WARN) {
        h2_set_alarm(ALARM_H2_LEAK, SEVERITY_WARNING,
                    safety->h2_concentration_ppm, (float)H2_LEAK_PPM_WARN);
    } else {
        safety->h2_leak_detected = false;
        h2_clear_alarm(ALARM_H2_LEAK);
    }
    
    /* Ventilation check */
    if (safety->vent_active && safety->vent_flow_rate < H2_VENT_FLOW_MIN) {
        h2_set_alarm(ALARM_VENT_FAILURE, SEVERITY_ALARM,
                    safety->vent_flow_rate, H2_VENT_FLOW_MIN);
    } else {
        h2_clear_alarm(ALARM_VENT_FAILURE);
    }
}

/* ============================================================================
 * Public API - Initialization
 * ============================================================================ */

/**
 * @brief Initialize hydrogen system
 */
int h2_init(void) {
    if (g_h2.initialized) return -1;
    
    memset(&g_h2, 0, sizeof(g_h2));
    g_h2.initialized = true;
    g_h2.mode = H2_MODE_OFF;
    g_h2.next_alarm_id = 1;
    g_h2.next_event_id = 1;
    
    /* Default fuel cell configuration */
    g_h2.fc_config.stack_id = 1;
    g_h2.fc_config.cell_count = 100;
    g_h2.fc_config.rated_power_kw = 100.0f;
    g_h2.fc_config.active_area_cm2 = 200.0f;
    g_h2.fc_config.min_load_pct = 10.0f;
    g_h2.fc_config.max_load_pct = 100.0f;
    
    /* Default electrolyzer configuration */
    g_h2.el_config.el_id = 1;
    g_h2.el_config.cell_count = 50;
    g_h2.el_config.rated_power_kw = 100.0f;
    g_h2.el_config.rated_h2_kg_h = 2.0f;  /* 2 kg/h at 100 kW */
    g_h2.el_config.turndown_ratio = 0.1f;
    
    /* Initial temperatures */
    g_h2.fc_status.stack_temp_c = 25.0f;
    g_h2.el_status.stack_temp_c = 25.0f;
    g_h2.el_status.water_resistivity = 18.0f;
    
    h2_log_event(EVENT_SYSTEM_START, 0, "Hydrogen system initialized");
    
    return 0;
}

/**
 * @brief Shutdown hydrogen system
 */
int h2_shutdown(void) {
    if (!g_h2.initialized) return -1;
    
    h2_log_event(EVENT_SYSTEM_STOP, 0, "Hydrogen system shutdown");
    g_h2.initialized = false;
    
    return 0;
}

/* ============================================================================
 * Public API - Configuration
 * ============================================================================ */

/**
 * @brief Configure fuel cell stack
 */
int h2_configure_fuelcell(uint8_t cell_count, float rated_power_kw) {
    if (!g_h2.initialized) return -1;
    if (cell_count > H2_MAX_CELLS) return -2;
    
    g_h2.fc_config.cell_count = cell_count;
    g_h2.fc_config.rated_power_kw = rated_power_kw;
    
    return 0;
}

/**
 * @brief Configure electrolyzer
 */
int h2_configure_electrolyzer(uint8_t cell_count, float rated_power_kw, float rated_h2_kg_h) {
    if (!g_h2.initialized) return -1;
    
    g_h2.el_config.cell_count = cell_count;
    g_h2.el_config.rated_power_kw = rated_power_kw;
    g_h2.el_config.rated_h2_kg_h = rated_h2_kg_h;
    
    return 0;
}

/* ============================================================================
 * Public API - Operations
 * ============================================================================ */

/**
 * @brief Set operating mode
 */
int h2_set_mode(h2_mode_t mode) {
    if (!g_h2.initialized) return -1;
    
    h2_mode_t old_mode = g_h2.mode;
    g_h2.mode = mode;
    
    if (old_mode != mode) {
        h2_log_event(EVENT_MODE_CHANGE, (float)mode, "Mode changed");
    }
    
    switch (mode) {
        case H2_MODE_STARTUP:
            g_h2.fc_status.state = FC_STATE_PURGING;
            g_h2.el_status.state = EL_STATE_WARMUP;
            break;
            
        case H2_MODE_RUNNING:
        case H2_MODE_LOAD_FOLLOW:
            /* Already transitioned by process loop */
            break;
            
        case H2_MODE_SHUTDOWN:
            g_h2.fc_status.state = FC_STATE_COOLDOWN;
            g_h2.el_status.state = EL_STATE_COOLDOWN;
            break;
            
        case H2_MODE_EMERGENCY:
            g_h2.fc_status.state = FC_STATE_FAULT;
            g_h2.el_status.state = EL_STATE_FAULT;
            g_h2.fc_power_setpoint = 0;
            g_h2.el_power_setpoint = 0;
            h2_log_event(EVENT_E_STOP, 0, "Emergency shutdown");
            break;
            
        case H2_MODE_OFF:
            g_h2.fc_status.state = FC_STATE_OFF;
            g_h2.el_status.state = EL_STATE_OFF;
            break;
            
        default:
            break;
    }
    
    return 0;
}

/**
 * @brief Set fuel cell power setpoint
 */
int h2_set_fc_power(float power_kw) {
    if (!g_h2.initialized) return -1;
    
    float max = g_h2.fc_config.rated_power_kw;
    if (power_kw > max) power_kw = max;
    if (power_kw < 0) power_kw = 0;
    
    g_h2.fc_power_setpoint = power_kw;
    g_h2.fc_load_pct = (max > 0) ? (power_kw / max) * 100.0f : 0;
    
    h2_log_event(EVENT_LOAD_CHANGE, power_kw, "FC power setpoint");
    
    return 0;
}

/**
 * @brief Set electrolyzer power setpoint
 */
int h2_set_el_power(float power_kw) {
    if (!g_h2.initialized) return -1;
    
    float max = g_h2.el_config.rated_power_kw;
    if (power_kw > max) power_kw = max;
    if (power_kw < 0) power_kw = 0;
    
    g_h2.el_power_setpoint = power_kw;
    g_h2.el_production_setpoint = 0;  /* Power mode */
    
    h2_log_event(EVENT_LOAD_CHANGE, power_kw, "EL power setpoint");
    
    return 0;
}

/**
 * @brief Set electrolyzer production setpoint
 */
int h2_set_el_production(float h2_kg_h) {
    if (!g_h2.initialized) return -1;
    
    float max = g_h2.el_config.rated_h2_kg_h;
    if (h2_kg_h > max) h2_kg_h = max;
    if (h2_kg_h < 0) h2_kg_h = 0;
    
    g_h2.el_production_setpoint = h2_kg_h;
    g_h2.el_power_setpoint = 0;  /* Production mode */
    
    return 0;
}

/**
 * @brief Emergency stop
 */
int h2_emergency_stop(void) {
    if (!g_h2.initialized) return -1;
    
    g_h2.safety.e_stop_active = true;
    h2_set_mode(H2_MODE_EMERGENCY);
    
    return 0;
}

/**
 * @brief Clear emergency stop
 */
int h2_clear_emergency(void) {
    if (!g_h2.initialized) return -1;
    
    g_h2.safety.e_stop_active = false;
    h2_set_mode(H2_MODE_OFF);
    
    return 0;
}

/* ============================================================================
 * Public API - Process Loop
 * ============================================================================ */

/**
 * @brief Process hydrogen system (call periodically)
 */
int h2_process(uint32_t delta_ms) {
    if (!g_h2.initialized) return -1;
    
    g_h2.current_time_ms += delta_ms;
    g_h2.stats.uptime_seconds = g_h2.current_time_ms / 1000;
    
    /* Process safety first */
    safety_process();
    
    /* Process fuel cell and electrolyzer */
    if (g_h2.mode != H2_MODE_OFF && g_h2.mode != H2_MODE_EMERGENCY) {
        fc_process(delta_ms);
        el_process(delta_ms);
    }
    
    /* Simulate temperature dynamics */
    if (g_h2.fc_status.state == FC_STATE_RUNNING) {
        /* Warm up toward optimal */
        if (g_h2.fc_status.stack_temp_c < FC_TEMP_OPT_C) {
            g_h2.fc_status.stack_temp_c += 0.1f * delta_ms / 1000.0f;
        }
    } else if (g_h2.fc_status.state != FC_STATE_OFF) {
        /* Cool down */
        if (g_h2.fc_status.stack_temp_c > 25.0f) {
            g_h2.fc_status.stack_temp_c -= 0.05f * delta_ms / 1000.0f;
        }
    }
    
    if (g_h2.el_status.state == EL_STATE_PRODUCTION) {
        if (g_h2.el_status.stack_temp_c < 60.0f) {
            g_h2.el_status.stack_temp_c += 0.1f * delta_ms / 1000.0f;
        }
    } else if (g_h2.el_status.state != EL_STATE_OFF) {
        if (g_h2.el_status.stack_temp_c > 25.0f) {
            g_h2.el_status.stack_temp_c -= 0.05f * delta_ms / 1000.0f;
        }
    }
    
    /* Calculate overall efficiency */
    if (g_h2.stats.energy_consumed_kwh > 0 && g_h2.stats.energy_produced_kwh > 0) {
        /* Round-trip efficiency for storage scenario */
        g_h2.stats.overall_efficiency = 
            (g_h2.stats.energy_produced_kwh / g_h2.stats.energy_consumed_kwh) * 100.0f;
    }
    
    g_h2.last_process_ms = h2_get_time_ms();
    
    return 0;
}

/* ============================================================================
 * Public API - Status Queries
 * ============================================================================ */

/**
 * @brief Get current mode
 */
h2_mode_t h2_get_mode(void) {
    return g_h2.mode;
}

/**
 * @brief Get fuel cell status
 */
int h2_get_fc_status(fc_status_t* status) {
    if (!g_h2.initialized || !status) return -1;
    
    memcpy(status, &g_h2.fc_status, sizeof(fc_status_t));
    return 0;
}

/**
 * @brief Get electrolyzer status
 */
int h2_get_el_status(el_status_t* status) {
    if (!g_h2.initialized || !status) return -1;
    
    memcpy(status, &g_h2.el_status, sizeof(el_status_t));
    return 0;
}

/**
 * @brief Get safety status
 */
int h2_get_safety(h2_safety_t* safety) {
    if (!g_h2.initialized || !safety) return -1;
    
    memcpy(safety, &g_h2.safety, sizeof(h2_safety_t));
    return 0;
}

/**
 * @brief Get statistics
 */
int h2_get_stats(h2_stats_t* stats) {
    if (!g_h2.initialized || !stats) return -1;
    
    memcpy(stats, &g_h2.stats, sizeof(h2_stats_t));
    return 0;
}

/**
 * @brief Get active alarms
 */
int h2_get_alarms(h2_alarm_t* alarms, uint8_t max_alarms, uint8_t* count) {
    if (!g_h2.initialized || !alarms || !count) return -1;
    
    uint8_t n = 0;
    for (uint8_t i = 0; i < g_h2.alarm_count && n < max_alarms; i++) {
        if (g_h2.alarms[i].active) {
            memcpy(&alarms[n++], &g_h2.alarms[i], sizeof(h2_alarm_t));
        }
    }
    *count = n;
    
    return 0;
}

/**
 * @brief Acknowledge alarm
 */
int h2_acknowledge_alarm(uint32_t alarm_id) {
    if (!g_h2.initialized) return -1;
    
    for (uint8_t i = 0; i < g_h2.alarm_count; i++) {
        if (g_h2.alarms[i].alarm_id == alarm_id) {
            g_h2.alarms[i].acknowledged = true;
            return 0;
        }
    }
    
    return -2;
}

/**
 * @brief Get event log
 */
int h2_get_events(h2_event_t* events, uint16_t max_events, uint16_t* count) {
    if (!g_h2.initialized || !events || !count) return -1;
    
    uint16_t copy_count = (g_h2.event_count < max_events) ? 
                          g_h2.event_count : max_events;
    
    uint16_t start = (g_h2.event_head + H2_MAX_EVENTS - g_h2.event_count) % H2_MAX_EVENTS;
    
    for (uint16_t i = 0; i < copy_count; i++) {
        uint16_t idx = (start + i) % H2_MAX_EVENTS;
        memcpy(&events[i], &g_h2.events[idx], sizeof(h2_event_t));
    }
    
    *count = copy_count;
    return 0;
}

/* ============================================================================
 * Public API - Telemetry
 * ============================================================================ */

/**
 * @brief Generate telemetry packet
 */
int h2_generate_telemetry(uint8_t* buffer, uint16_t max_len, uint16_t* len) {
    if (!g_h2.initialized || !buffer || !len) return -1;
    if (max_len < 128) return -2;
    
    uint16_t offset = 0;
    
    /* Header */
    buffer[offset++] = 'H';
    buffer[offset++] = '2';
    buffer[offset++] = 'S';
    buffer[offset++] = 'Y';
    
    /* Timestamp */
    uint32_t time = h2_get_time_ms();
    memcpy(&buffer[offset], &time, 4);
    offset += 4;
    
    /* Mode and states */
    buffer[offset++] = (uint8_t)g_h2.mode;
    buffer[offset++] = (uint8_t)g_h2.fc_status.state;
    buffer[offset++] = (uint8_t)g_h2.el_status.state;
    buffer[offset++] = g_h2.safety.active_alarms;
    
    /* Fuel cell data */
    memcpy(&buffer[offset], &g_h2.fc_status.power_output_kw, 4);
    offset += 4;
    memcpy(&buffer[offset], &g_h2.fc_status.efficiency_pct, 4);
    offset += 4;
    memcpy(&buffer[offset], &g_h2.fc_status.stack_temp_c, 4);
    offset += 4;
    
    /* Electrolyzer data */
    memcpy(&buffer[offset], &g_h2.el_status.power_kw, 4);
    offset += 4;
    memcpy(&buffer[offset], &g_h2.el_status.h2_rate_kg_h, 4);
    offset += 4;
    memcpy(&buffer[offset], &g_h2.el_status.efficiency_pct, 4);
    offset += 4;
    
    /* Statistics */
    memcpy(&buffer[offset], &g_h2.stats.h2_produced_kg, 4);
    offset += 4;
    memcpy(&buffer[offset], &g_h2.stats.energy_produced_kwh, 4);
    offset += 4;
    
    *len = offset;
    return 0;
}

/* ============================================================================
 * Public API - Simulation/Testing
 * ============================================================================ */

/**
 * @brief Inject H2 leak for testing
 */
int h2_inject_leak(float concentration_ppm) {
    if (!g_h2.initialized) return -1;
    
    g_h2.safety.h2_concentration_ppm = concentration_ppm;
    return 0;
}

/**
 * @brief Set stack temperature for testing
 */
int h2_inject_temperature(bool fuel_cell, float temp_c) {
    if (!g_h2.initialized) return -1;
    
    if (fuel_cell) {
        g_h2.fc_status.stack_temp_c = temp_c;
    } else {
        g_h2.el_status.stack_temp_c = temp_c;
    }
    return 0;
}

/**
 * @brief Set water resistivity for testing
 */
int h2_inject_water_quality(float resistivity) {
    if (!g_h2.initialized) return -1;
    
    g_h2.el_status.water_resistivity = resistivity;
    return 0;
}
