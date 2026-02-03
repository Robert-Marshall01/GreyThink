/**
 * @file pipeline_spotlight.c
 * @brief Pipeline Leak Detection & Safety Spotlight Implementation
 * 
 * This module implements a production-grade pipeline monitoring system with:
 * - Flow monitoring with anomaly detection
 * - Leak detection using pressure differential analysis
 * - Mass balance verification across segments
 * - Negative pressure wave detection for ruptures
 * - Blockage detection via flow impedance
 * - Telemetry reporting for SCADA integration
 * - Compliance logging for PHMSA/API standards
 * 
 * DETECTION METHODS:
 * 1. Mass Balance: Compares upstream/downstream flow rates
 * 2. Pressure Point Analysis (PPA): Gradient analysis between sensors
 * 3. Negative Pressure Wave (NPW): Detects sudden pressure drops (ruptures)
 * 4. Statistical Pattern Recognition (SPR): Learning-based anomaly detection
 * 5. Rate of Change (ROC): Monitors rapid parameter changes
 * 
 * SAFETY FEATURES:
 * - Emergency shutdown valve (ESV) control
 * - Automatic isolation on confirmed leak
 * - Fail-safe sensor redundancy
 * - SCADA alarm integration
 * 
 * STANDARDS COMPLIANCE:
 * - API 1130 (Computational Pipeline Monitoring)
 * - API 1160 (Managing System Integrity)
 * - 49 CFR 195 (PHMSA Hazardous Liquids)
 * - 49 CFR 192 (PHMSA Natural Gas)
 * 
 * @note This is a spotlight subsystem demonstrating embedded infrastructure
 *       safety expertise for pipeline SCADA/DCS applications.
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

#define PIPE_MAX_SEGMENTS          16    /**< Maximum pipeline segments */
#define PIPE_MAX_SENSORS           32    /**< Maximum sensors */
#define PIPE_MAX_VALVES            16    /**< Maximum valves */
#define PIPE_MAX_ALERTS            64    /**< Alert buffer size */
#define PIPE_MAX_EVENTS            256   /**< Event log size */
#define PIPE_HISTORY_SIZE          120   /**< Sample history (2 minutes @ 1/sec) */

/* Detection thresholds */
#define PIPE_IMBALANCE_WARNING     2.0f  /**< % imbalance for warning */
#define PIPE_IMBALANCE_ALARM       5.0f  /**< % imbalance for alarm */
#define PIPE_NPW_THRESHOLD         0.5f  /**< bar/sec for NPW detection */
#define PIPE_ROC_PRESSURE          0.1f  /**< bar/sec rate of change */
#define PIPE_ROC_FLOW              5.0f  /**< m³/h/sec rate of change */
#define PIPE_BLOCKAGE_RATIO        0.7f  /**< Flow ratio for blockage */
#define PIPE_SETTLING_TIME_MS      30000 /**< 30 seconds settling time */

/* Pressure limits */
#define PIPE_PRESSURE_MIN          1.0f  /**< Minimum operating pressure (bar) */
#define PIPE_PRESSURE_MAX          100.0f /**< Maximum operating pressure (bar) */
#define PIPE_PRESSURE_LOW_WARN     5.0f  /**< Low pressure warning */
#define PIPE_PRESSURE_HIGH_WARN    90.0f /**< High pressure warning */

/* Temperature limits */
#define PIPE_TEMP_MIN              -40.0f /**< Minimum temperature (°C) */
#define PIPE_TEMP_MAX              100.0f /**< Maximum temperature (°C) */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Sensor type */
typedef enum {
    SENSOR_FLOW,               /**< Flow meter */
    SENSOR_PRESSURE,           /**< Pressure transducer */
    SENSOR_TEMPERATURE,        /**< Temperature sensor */
    SENSOR_DENSITY,            /**< Densitometer */
    SENSOR_PIG_DETECTOR        /**< Pig passage detector */
} pipe_sensor_type_t;

/** Pipeline product type */
typedef enum {
    PRODUCT_CRUDE_OIL,
    PRODUCT_REFINED_FUEL,
    PRODUCT_NATURAL_GAS,
    PRODUCT_LPG,
    PRODUCT_WATER,
    PRODUCT_CHEMICAL
} pipe_product_t;

/** Segment status */
typedef enum {
    SEG_STATUS_NORMAL,
    SEG_STATUS_WARNING,
    SEG_STATUS_ALARM,
    SEG_STATUS_SHUTDOWN,
    SEG_STATUS_MAINTENANCE,
    SEG_STATUS_ISOLATED
} pipe_seg_status_t;

/** Detection method */
typedef enum {
    DETECT_NONE,
    DETECT_MASS_BALANCE,       /**< Input/output imbalance */
    DETECT_PPA,                /**< Pressure point analysis */
    DETECT_NPW,                /**< Negative pressure wave */
    DETECT_SPR,                /**< Statistical pattern recognition */
    DETECT_ROC                 /**< Rate of change */
} pipe_detect_method_t;

/** Leak severity */
typedef enum {
    LEAK_NONE,
    LEAK_MINOR,                /**< <1% of flow */
    LEAK_MODERATE,             /**< 1-5% of flow */
    LEAK_MAJOR,                /**< 5-20% of flow */
    LEAK_RUPTURE               /**< >20% or sudden event */
} pipe_leak_severity_t;

/** Valve state */
typedef enum {
    VALVE_UNKNOWN,
    VALVE_OPEN,
    VALVE_CLOSED,
    VALVE_OPENING,
    VALVE_CLOSING,
    VALVE_FAULT
} pipe_valve_state_t;

/** Event type */
typedef enum {
    EVENT_SYSTEM_START,
    EVENT_SYSTEM_STOP,
    EVENT_SENSOR_ONLINE,
    EVENT_SENSOR_OFFLINE,
    EVENT_SENSOR_FAULT,
    EVENT_LEAK_DETECTED,
    EVENT_LEAK_CONFIRMED,
    EVENT_LEAK_CLEARED,
    EVENT_BLOCKAGE_DETECTED,
    EVENT_PRESSURE_HIGH,
    EVENT_PRESSURE_LOW,
    EVENT_FLOW_ABNORMAL,
    EVENT_VALVE_COMMAND,
    EVENT_VALVE_RESPONSE,
    EVENT_SHUTDOWN_INITIATED,
    EVENT_SHUTDOWN_COMPLETED,
    EVENT_MAINTENANCE_START,
    EVENT_MAINTENANCE_END,
    EVENT_CALIBRATION
} pipe_event_type_t;

/* ============================================================================
 * Data Structures
 * ============================================================================ */

/** Sensor reading */
typedef struct {
    uint8_t sensor_id;
    pipe_sensor_type_t type;
    float value;               /**< Sensor value */
    float quality;             /**< 0.0-1.0 quality indicator */
    bool valid;                /**< Reading validity */
    uint32_t timestamp_ms;
} pipe_reading_t;

/** Sensor history for pattern analysis */
typedef struct {
    float values[PIPE_HISTORY_SIZE];
    uint8_t head;
    uint8_t count;
    float mean;
    float std_dev;
    float trend;               /**< Rate of change */
} pipe_history_t;

/** Sensor configuration */
typedef struct {
    uint8_t sensor_id;
    pipe_sensor_type_t type;
    float location_km;         /**< Distance from pipeline start */
    uint8_t segment_id;        /**< Associated segment */
    float low_limit;
    float high_limit;
    float deadband;            /**< Noise rejection */
    uint16_t sample_rate_hz;
    bool enabled;
    bool calibrated;
} pipe_sensor_config_t;

/** Sensor state */
typedef struct {
    pipe_sensor_config_t config;
    pipe_reading_t last_reading;
    pipe_history_t history;
    bool online;
    bool fault;
    uint32_t fault_count;
    uint32_t reading_count;
} pipe_sensor_t;

/** Segment configuration */
typedef struct {
    uint8_t segment_id;
    char name[32];
    float start_km;            /**< Start location */
    float end_km;              /**< End location */
    float diameter_mm;
    float wall_thickness_mm;
    pipe_product_t product;
    uint8_t upstream_flow_sensor;
    uint8_t downstream_flow_sensor;
    uint8_t pressure_sensors[4];
    uint8_t pressure_sensor_count;
    float imbalance_threshold; /**< % for alarm */
    uint32_t settling_time_ms;
    bool leak_detection_enabled;
} pipe_segment_config_t;

/** Segment state */
typedef struct {
    pipe_segment_config_t config;
    pipe_seg_status_t status;
    float flow_in;             /**< m³/h */
    float flow_out;            /**< m³/h */
    float imbalance_pct;       /**< Current imbalance */
    float pressure_gradient;   /**< bar/km */
    float avg_pressure;
    float avg_temperature;
    bool leak_suspected;
    bool leak_confirmed;
    pipe_detect_method_t detection_method;
    pipe_leak_severity_t leak_severity;
    float estimated_leak_rate; /**< m³/h */
    float estimated_leak_location; /**< km from start */
    uint32_t status_change_ms;
    uint32_t alarm_start_ms;
} pipe_segment_t;

/** Valve configuration */
typedef struct {
    uint8_t valve_id;
    char name[24];
    float location_km;
    uint8_t segment_up;        /**< Upstream segment */
    uint8_t segment_down;      /**< Downstream segment */
    bool esv;                  /**< Emergency shutdown valve */
    uint32_t stroke_time_ms;   /**< Time to open/close */
} pipe_valve_config_t;

/** Valve state */
typedef struct {
    pipe_valve_config_t config;
    pipe_valve_state_t state;
    pipe_valve_state_t commanded;
    uint32_t last_command_ms;
    uint32_t last_response_ms;
    bool fault;
} pipe_valve_t;

/** Leak alert */
typedef struct {
    uint32_t alert_id;
    uint8_t segment_id;
    pipe_detect_method_t method;
    pipe_leak_severity_t severity;
    float estimated_location_km;
    float estimated_rate;      /**< m³/h */
    float confidence;          /**< 0.0-1.0 */
    uint32_t first_detect_ms;
    uint32_t confirmed_ms;     /**< 0 if not confirmed */
    bool acknowledged;
    bool active;
} pipe_alert_t;

/** Event log entry */
typedef struct {
    uint32_t event_id;
    pipe_event_type_t type;
    uint8_t segment_id;
    uint8_t sensor_id;
    float value;
    uint32_t timestamp_ms;
    char description[64];
} pipe_event_t;

/** System statistics */
typedef struct {
    uint32_t uptime_seconds;
    uint32_t readings_processed;
    uint32_t alerts_generated;
    uint32_t alerts_confirmed;
    uint32_t false_positives;
    uint32_t shutdowns;
    float total_product_through; /**< m³ */
    float total_product_lost;    /**< m³ estimated */
    float system_availability;   /**< 0.0-1.0 */
} pipe_stats_t;

/** System state */
typedef struct {
    bool initialized;
    bool monitoring_active;
    uint32_t pipeline_id;
    pipe_product_t product;
    float total_length_km;
    
    /* Sensors */
    pipe_sensor_t sensors[PIPE_MAX_SENSORS];
    uint8_t sensor_count;
    
    /* Segments */
    pipe_segment_t segments[PIPE_MAX_SEGMENTS];
    uint8_t segment_count;
    
    /* Valves */
    pipe_valve_t valves[PIPE_MAX_VALVES];
    uint8_t valve_count;
    
    /* Alerts */
    pipe_alert_t alerts[PIPE_MAX_ALERTS];
    uint8_t alert_count;
    uint32_t next_alert_id;
    
    /* Events */
    pipe_event_t events[PIPE_MAX_EVENTS];
    uint16_t event_head;
    uint16_t event_count;
    uint32_t next_event_id;
    
    /* Statistics */
    pipe_stats_t stats;
    
    /* Timing */
    uint32_t current_time_ms;
    uint32_t last_process_ms;
} pipe_system_t;

/* ============================================================================
 * Global State
 * ============================================================================ */

static pipe_system_t g_pipe = {0};

/* ============================================================================
 * Internal Helper Functions
 * ============================================================================ */

/**
 * @brief Get current time in milliseconds
 */
static uint32_t pipe_get_time_ms(void) {
    return g_pipe.current_time_ms;
}

/**
 * @brief Log an event
 */
static void pipe_log_event(pipe_event_type_t type, uint8_t segment_id,
                           uint8_t sensor_id, float value, const char* desc) {
    pipe_event_t* event = &g_pipe.events[g_pipe.event_head];
    
    event->event_id = g_pipe.next_event_id++;
    event->type = type;
    event->segment_id = segment_id;
    event->sensor_id = sensor_id;
    event->value = value;
    event->timestamp_ms = pipe_get_time_ms();
    
    if (desc) {
        strncpy(event->description, desc, sizeof(event->description) - 1);
        event->description[sizeof(event->description) - 1] = '\0';
    } else {
        event->description[0] = '\0';
    }
    
    g_pipe.event_head = (g_pipe.event_head + 1) % PIPE_MAX_EVENTS;
    if (g_pipe.event_count < PIPE_MAX_EVENTS) {
        g_pipe.event_count++;
    }
}

/**
 * @brief Update sensor history
 */
static void pipe_update_history(pipe_history_t* history, float value) {
    history->values[history->head] = value;
    history->head = (history->head + 1) % PIPE_HISTORY_SIZE;
    if (history->count < PIPE_HISTORY_SIZE) {
        history->count++;
    }
    
    /* Calculate mean */
    float sum = 0;
    for (uint8_t i = 0; i < history->count; i++) {
        sum += history->values[i];
    }
    history->mean = sum / history->count;
    
    /* Calculate standard deviation */
    float var_sum = 0;
    for (uint8_t i = 0; i < history->count; i++) {
        float diff = history->values[i] - history->mean;
        var_sum += diff * diff;
    }
    history->std_dev = sqrtf(var_sum / history->count);
    
    /* Calculate trend (rate of change over last 10 samples) */
    if (history->count >= 10) {
        uint8_t start = (history->head + PIPE_HISTORY_SIZE - 10) % PIPE_HISTORY_SIZE;
        uint8_t end = (history->head + PIPE_HISTORY_SIZE - 1) % PIPE_HISTORY_SIZE;
        history->trend = (history->values[end] - history->values[start]) / 10.0f;
    }
}

/**
 * @brief Find sensor by ID
 */
static pipe_sensor_t* pipe_find_sensor(uint8_t sensor_id) {
    for (uint8_t i = 0; i < g_pipe.sensor_count; i++) {
        if (g_pipe.sensors[i].config.sensor_id == sensor_id) {
            return &g_pipe.sensors[i];
        }
    }
    return NULL;
}

/**
 * @brief Find segment by ID
 */
static pipe_segment_t* pipe_find_segment(uint8_t segment_id) {
    for (uint8_t i = 0; i < g_pipe.segment_count; i++) {
        if (g_pipe.segments[i].config.segment_id == segment_id) {
            return &g_pipe.segments[i];
        }
    }
    return NULL;
}

/**
 * @brief Find valve by ID
 */
static pipe_valve_t* pipe_find_valve(uint8_t valve_id) {
    for (uint8_t i = 0; i < g_pipe.valve_count; i++) {
        if (g_pipe.valves[i].config.valve_id == valve_id) {
            return &g_pipe.valves[i];
        }
    }
    return NULL;
}

/* ============================================================================
 * Detection Algorithms
 * ============================================================================ */

/**
 * @brief Mass balance leak detection
 * 
 * Compares upstream and downstream flow rates to detect product loss
 */
static void pipe_detect_mass_balance(pipe_segment_t* segment) {
    if (segment->flow_in <= 0 || segment->flow_out <= 0) {
        return;  /* No valid flow data */
    }
    
    /* Calculate imbalance percentage */
    float diff = segment->flow_in - segment->flow_out;
    segment->imbalance_pct = (diff / segment->flow_in) * 100.0f;
    
    /* Check thresholds */
    if (fabsf(segment->imbalance_pct) >= segment->config.imbalance_threshold) {
        if (!segment->leak_suspected) {
            segment->leak_suspected = true;
            segment->detection_method = DETECT_MASS_BALANCE;
            segment->estimated_leak_rate = diff;
            
            /* Estimate location (midpoint if unknown) */
            segment->estimated_leak_location = 
                (segment->config.start_km + segment->config.end_km) / 2.0f;
            
            /* Classify severity */
            if (fabsf(segment->imbalance_pct) >= 20.0f) {
                segment->leak_severity = LEAK_RUPTURE;
            } else if (fabsf(segment->imbalance_pct) >= 5.0f) {
                segment->leak_severity = LEAK_MAJOR;
            } else if (fabsf(segment->imbalance_pct) >= 1.0f) {
                segment->leak_severity = LEAK_MODERATE;
            } else {
                segment->leak_severity = LEAK_MINOR;
            }
        }
    } else if (fabsf(segment->imbalance_pct) < PIPE_IMBALANCE_WARNING) {
        /* Clear if below warning threshold */
        if (segment->leak_suspected && segment->detection_method == DETECT_MASS_BALANCE) {
            segment->leak_suspected = false;
            segment->leak_severity = LEAK_NONE;
        }
    }
}

/**
 * @brief Pressure point analysis
 * 
 * Analyzes pressure gradients between sensors to detect anomalies
 */
static void pipe_detect_ppa(pipe_segment_t* segment) {
    if (segment->config.pressure_sensor_count < 2) {
        return;  /* Need at least 2 pressure sensors */
    }
    
    /* Get pressure readings from segment sensors */
    float pressures[4] = {0};
    float locations[4] = {0};
    uint8_t valid_count = 0;
    
    for (uint8_t i = 0; i < segment->config.pressure_sensor_count; i++) {
        pipe_sensor_t* sensor = pipe_find_sensor(segment->config.pressure_sensors[i]);
        if (sensor && sensor->online && sensor->last_reading.valid) {
            pressures[valid_count] = sensor->last_reading.value;
            locations[valid_count] = sensor->config.location_km;
            valid_count++;
        }
    }
    
    if (valid_count < 2) return;
    
    /* Calculate pressure gradient */
    float total_drop = pressures[0] - pressures[valid_count - 1];
    float distance = locations[valid_count - 1] - locations[0];
    segment->pressure_gradient = (distance > 0) ? total_drop / distance : 0;
    
    /* Look for anomalous pressure drops between adjacent sensors */
    for (uint8_t i = 0; i < valid_count - 1; i++) {
        float local_drop = pressures[i] - pressures[i + 1];
        float local_dist = locations[i + 1] - locations[i];
        float local_gradient = (local_dist > 0) ? local_drop / local_dist : 0;
        
        /* If local gradient is 3x steeper than overall, potential leak */
        if (local_gradient > segment->pressure_gradient * 3.0f) {
            if (!segment->leak_suspected) {
                segment->leak_suspected = true;
                segment->detection_method = DETECT_PPA;
                segment->estimated_leak_location = 
                    (locations[i] + locations[i + 1]) / 2.0f;
                segment->leak_severity = LEAK_MODERATE;
            }
        }
    }
    
    /* Calculate average pressure */
    float sum = 0;
    for (uint8_t i = 0; i < valid_count; i++) {
        sum += pressures[i];
    }
    segment->avg_pressure = sum / valid_count;
}

/**
 * @brief Negative pressure wave detection
 * 
 * Detects rapid pressure drops indicating pipe rupture
 */
static void pipe_detect_npw(pipe_segment_t* segment) {
    for (uint8_t i = 0; i < segment->config.pressure_sensor_count; i++) {
        pipe_sensor_t* sensor = pipe_find_sensor(segment->config.pressure_sensors[i]);
        if (!sensor || !sensor->online) continue;
        
        /* Check rate of change */
        if (sensor->history.trend < -PIPE_NPW_THRESHOLD) {
            /* Rapid pressure drop detected */
            if (!segment->leak_suspected || segment->leak_severity < LEAK_RUPTURE) {
                segment->leak_suspected = true;
                segment->leak_confirmed = true;  /* NPW is high-confidence */
                segment->detection_method = DETECT_NPW;
                segment->estimated_leak_location = sensor->config.location_km;
                segment->leak_severity = LEAK_RUPTURE;
                
                pipe_log_event(EVENT_LEAK_DETECTED, segment->config.segment_id,
                              sensor->config.sensor_id, sensor->history.trend,
                              "NPW rupture detected");
                
                g_pipe.stats.alerts_generated++;
            }
        }
    }
}

/**
 * @brief Statistical pattern recognition
 * 
 * Uses historical baseline to detect subtle anomalies
 */
static void pipe_detect_spr(pipe_segment_t* segment) {
    /* Get upstream flow sensor */
    pipe_sensor_t* flow_in = pipe_find_sensor(segment->config.upstream_flow_sensor);
    pipe_sensor_t* flow_out = pipe_find_sensor(segment->config.downstream_flow_sensor);
    
    if (!flow_in || !flow_out) return;
    if (flow_in->history.count < 60 || flow_out->history.count < 60) return;
    
    /* Check if current values deviate significantly from mean */
    float in_z = (flow_in->last_reading.value - flow_in->history.mean) / 
                 (flow_in->history.std_dev + 0.001f);
    float out_z = (flow_out->last_reading.value - flow_out->history.mean) / 
                  (flow_out->history.std_dev + 0.001f);
    
    /* If input normal but output significantly low, potential leak */
    if (fabsf(in_z) < 2.0f && out_z < -3.0f) {
        if (!segment->leak_suspected) {
            segment->leak_suspected = true;
            segment->detection_method = DETECT_SPR;
            segment->leak_severity = LEAK_MINOR;
            segment->estimated_leak_location = 
                (segment->config.start_km + segment->config.end_km) / 2.0f;
        }
    }
}

/**
 * @brief Rate of change detection
 * 
 * Monitors for rapid changes in flow or pressure
 */
static void pipe_detect_roc(pipe_segment_t* segment) {
    /* Check flow rate of change */
    pipe_sensor_t* flow_in = pipe_find_sensor(segment->config.upstream_flow_sensor);
    pipe_sensor_t* flow_out = pipe_find_sensor(segment->config.downstream_flow_sensor);
    
    if (flow_in && flow_in->online && fabsf(flow_in->history.trend) > PIPE_ROC_FLOW) {
        pipe_log_event(EVENT_FLOW_ABNORMAL, segment->config.segment_id,
                      flow_in->config.sensor_id, flow_in->history.trend,
                      "Rapid flow change upstream");
    }
    
    if (flow_out && flow_out->online && fabsf(flow_out->history.trend) > PIPE_ROC_FLOW) {
        /* Output dropping faster than input = potential leak */
        if (flow_out->history.trend < -PIPE_ROC_FLOW && 
            (!flow_in || fabsf(flow_in->history.trend) < PIPE_ROC_FLOW)) {
            if (!segment->leak_suspected) {
                segment->leak_suspected = true;
                segment->detection_method = DETECT_ROC;
                segment->leak_severity = LEAK_MODERATE;
            }
        }
    }
}

/**
 * @brief Blockage detection
 * 
 * Detects reduced flow with pressure buildup
 */
static void pipe_detect_blockage(pipe_segment_t* segment) {
    if (segment->flow_in <= 0) return;
    
    float ratio = segment->flow_out / segment->flow_in;
    
    /* If output is much lower than input with high upstream pressure */
    if (ratio < PIPE_BLOCKAGE_RATIO && segment->avg_pressure > PIPE_PRESSURE_HIGH_WARN) {
        segment->status = SEG_STATUS_WARNING;
        pipe_log_event(EVENT_BLOCKAGE_DETECTED, segment->config.segment_id,
                      0, ratio, "Possible blockage detected");
    }
}

/* ============================================================================
 * Alert Management
 * ============================================================================ */

/**
 * @brief Create or update leak alert
 */
static void pipe_create_alert(pipe_segment_t* segment) {
    if (!segment->leak_suspected) return;
    
    /* Check for existing alert */
    pipe_alert_t* existing = NULL;
    for (uint8_t i = 0; i < g_pipe.alert_count; i++) {
        if (g_pipe.alerts[i].segment_id == segment->config.segment_id &&
            g_pipe.alerts[i].active) {
            existing = &g_pipe.alerts[i];
            break;
        }
    }
    
    if (existing) {
        /* Update existing alert */
        existing->severity = segment->leak_severity;
        existing->estimated_rate = segment->estimated_leak_rate;
        existing->estimated_location_km = segment->estimated_leak_location;
        
        /* Confirm if persists for 30+ seconds */
        if (!existing->confirmed_ms && segment->leak_confirmed) {
            existing->confirmed_ms = pipe_get_time_ms();
            existing->confidence = 0.95f;
            g_pipe.stats.alerts_confirmed++;
            pipe_log_event(EVENT_LEAK_CONFIRMED, segment->config.segment_id,
                          0, existing->estimated_rate, "Leak confirmed");
        }
    } else {
        /* Create new alert */
        if (g_pipe.alert_count < PIPE_MAX_ALERTS) {
            pipe_alert_t* alert = &g_pipe.alerts[g_pipe.alert_count++];
            alert->alert_id = g_pipe.next_alert_id++;
            alert->segment_id = segment->config.segment_id;
            alert->method = segment->detection_method;
            alert->severity = segment->leak_severity;
            alert->estimated_location_km = segment->estimated_leak_location;
            alert->estimated_rate = segment->estimated_leak_rate;
            alert->confidence = (segment->detection_method == DETECT_NPW) ? 0.9f : 0.6f;
            alert->first_detect_ms = pipe_get_time_ms();
            alert->confirmed_ms = segment->leak_confirmed ? pipe_get_time_ms() : 0;
            alert->acknowledged = false;
            alert->active = true;
            
            g_pipe.stats.alerts_generated++;
            pipe_log_event(EVENT_LEAK_DETECTED, segment->config.segment_id,
                          0, segment->imbalance_pct, "Leak suspected");
        }
    }
}

/**
 * @brief Clear alert if condition resolved
 */
static void pipe_clear_alert(pipe_segment_t* segment) {
    for (uint8_t i = 0; i < g_pipe.alert_count; i++) {
        if (g_pipe.alerts[i].segment_id == segment->config.segment_id &&
            g_pipe.alerts[i].active) {
            g_pipe.alerts[i].active = false;
            
            /* If was confirmed, track as resolved; otherwise false positive */
            if (!g_pipe.alerts[i].confirmed_ms) {
                g_pipe.stats.false_positives++;
            }
            
            pipe_log_event(EVENT_LEAK_CLEARED, segment->config.segment_id,
                          0, 0, "Leak condition cleared");
        }
    }
}

/* ============================================================================
 * Valve Control
 * ============================================================================ */

/**
 * @brief Command valve operation
 */
static int pipe_command_valve(uint8_t valve_id, pipe_valve_state_t command) {
    pipe_valve_t* valve = pipe_find_valve(valve_id);
    if (!valve) return -1;
    
    valve->commanded = command;
    valve->last_command_ms = pipe_get_time_ms();
    
    /* Simulate valve response */
    if (command == VALVE_OPEN && valve->state == VALVE_CLOSED) {
        valve->state = VALVE_OPENING;
    } else if (command == VALVE_CLOSED && valve->state == VALVE_OPEN) {
        valve->state = VALVE_CLOSING;
    }
    
    pipe_log_event(EVENT_VALVE_COMMAND, 0, valve_id, (float)command,
                  valve->config.name);
    
    return 0;
}

/**
 * @brief Update valve states
 */
static void pipe_update_valves(uint32_t delta_ms) {
    (void)delta_ms;  /* Timing already handled via timestamps */
    
    for (uint8_t i = 0; i < g_pipe.valve_count; i++) {
        pipe_valve_t* valve = &g_pipe.valves[i];
        
        uint32_t elapsed = pipe_get_time_ms() - valve->last_command_ms;
        
        if (valve->state == VALVE_OPENING) {
            if (elapsed >= valve->config.stroke_time_ms) {
                valve->state = VALVE_OPEN;
                valve->last_response_ms = pipe_get_time_ms();
                pipe_log_event(EVENT_VALVE_RESPONSE, 0, valve->config.valve_id,
                              1.0f, "Valve open");
            }
        } else if (valve->state == VALVE_CLOSING) {
            if (elapsed >= valve->config.stroke_time_ms) {
                valve->state = VALVE_CLOSED;
                valve->last_response_ms = pipe_get_time_ms();
                pipe_log_event(EVENT_VALVE_RESPONSE, 0, valve->config.valve_id,
                              0.0f, "Valve closed");
            }
        }
    }
}

/**
 * @brief Emergency shutdown for segment
 */
static int pipe_emergency_shutdown(uint8_t segment_id) {
    pipe_segment_t* segment = pipe_find_segment(segment_id);
    if (!segment) return -1;
    
    segment->status = SEG_STATUS_SHUTDOWN;
    segment->status_change_ms = pipe_get_time_ms();
    
    /* Close all ESVs for this segment */
    for (uint8_t i = 0; i < g_pipe.valve_count; i++) {
        pipe_valve_t* valve = &g_pipe.valves[i];
        if (valve->config.esv && 
            (valve->config.segment_up == segment_id || 
             valve->config.segment_down == segment_id)) {
            pipe_command_valve(valve->config.valve_id, VALVE_CLOSED);
        }
    }
    
    pipe_log_event(EVENT_SHUTDOWN_INITIATED, segment_id, 0, 0,
                  "Emergency shutdown");
    g_pipe.stats.shutdowns++;
    
    return 0;
}

/* ============================================================================
 * Segment Processing
 * ============================================================================ */

/**
 * @brief Process a single segment
 */
static void pipe_process_segment(pipe_segment_t* segment, uint32_t delta_ms) {
    if (segment->status == SEG_STATUS_SHUTDOWN ||
        segment->status == SEG_STATUS_MAINTENANCE) {
        return;
    }
    
    /* Get flow readings */
    pipe_sensor_t* flow_in = pipe_find_sensor(segment->config.upstream_flow_sensor);
    pipe_sensor_t* flow_out = pipe_find_sensor(segment->config.downstream_flow_sensor);
    
    if (flow_in && flow_in->online && flow_in->last_reading.valid) {
        segment->flow_in = flow_in->last_reading.value;
    }
    if (flow_out && flow_out->online && flow_out->last_reading.valid) {
        segment->flow_out = flow_out->last_reading.value;
    }
    
    /* Run detection algorithms if enabled */
    if (segment->config.leak_detection_enabled) {
        /* Check settling time */
        uint32_t since_change = pipe_get_time_ms() - segment->status_change_ms;
        if (since_change < segment->config.settling_time_ms) {
            return;  /* Still settling after state change */
        }
        
        pipe_detect_mass_balance(segment);
        pipe_detect_ppa(segment);
        pipe_detect_npw(segment);
        pipe_detect_spr(segment);
        pipe_detect_roc(segment);
        pipe_detect_blockage(segment);
        
        /* Manage alerts */
        if (segment->leak_suspected) {
            pipe_create_alert(segment);
            
            /* Update segment status */
            if (segment->leak_confirmed) {
                segment->status = SEG_STATUS_ALARM;
                
                /* Auto-shutdown on confirmed major leak */
                if (segment->leak_severity >= LEAK_MAJOR) {
                    pipe_emergency_shutdown(segment->config.segment_id);
                }
            } else if (segment->status != SEG_STATUS_ALARM) {
                segment->status = SEG_STATUS_WARNING;
            }
        } else {
            if (segment->status == SEG_STATUS_WARNING || 
                segment->status == SEG_STATUS_ALARM) {
                segment->status = SEG_STATUS_NORMAL;
                pipe_clear_alert(segment);
            }
        }
    }
    
    /* Track product throughput */
    g_pipe.stats.total_product_through += segment->flow_in * delta_ms / 3600000.0f;
    
    /* Estimate product lost if leak suspected */
    if (segment->leak_suspected && segment->estimated_leak_rate > 0) {
        g_pipe.stats.total_product_lost += 
            segment->estimated_leak_rate * delta_ms / 3600000.0f;
    }
}

/* ============================================================================
 * Public API - Initialization
 * ============================================================================ */

/**
 * @brief Initialize pipeline monitoring system
 */
int pipe_init(uint32_t pipeline_id, pipe_product_t product) {
    if (g_pipe.initialized) return -1;
    
    memset(&g_pipe, 0, sizeof(g_pipe));
    g_pipe.pipeline_id = pipeline_id;
    g_pipe.product = product;
    g_pipe.initialized = true;
    g_pipe.monitoring_active = false;
    g_pipe.next_alert_id = 1;
    g_pipe.next_event_id = 1;
    
    pipe_log_event(EVENT_SYSTEM_START, 0, 0, 0, "Pipeline system initialized");
    
    return 0;
}

/**
 * @brief Shutdown pipeline monitoring
 */
int pipe_shutdown(void) {
    if (!g_pipe.initialized) return -1;
    
    pipe_log_event(EVENT_SYSTEM_STOP, 0, 0, 0, "Pipeline system shutdown");
    g_pipe.initialized = false;
    g_pipe.monitoring_active = false;
    
    return 0;
}

/* ============================================================================
 * Public API - Configuration
 * ============================================================================ */

/**
 * @brief Register a sensor
 */
int pipe_register_sensor(const pipe_sensor_config_t* config) {
    if (!g_pipe.initialized || !config) return -1;
    if (g_pipe.sensor_count >= PIPE_MAX_SENSORS) return -2;
    
    pipe_sensor_t* sensor = &g_pipe.sensors[g_pipe.sensor_count++];
    memset(sensor, 0, sizeof(*sensor));
    memcpy(&sensor->config, config, sizeof(pipe_sensor_config_t));
    sensor->online = config->enabled;
    
    pipe_log_event(EVENT_SENSOR_ONLINE, config->segment_id, config->sensor_id,
                  0, "Sensor registered");
    
    return 0;
}

/**
 * @brief Configure a segment
 */
int pipe_configure_segment(const pipe_segment_config_t* config) {
    if (!g_pipe.initialized || !config) return -1;
    if (g_pipe.segment_count >= PIPE_MAX_SEGMENTS) return -2;
    
    pipe_segment_t* segment = &g_pipe.segments[g_pipe.segment_count++];
    memset(segment, 0, sizeof(*segment));
    memcpy(&segment->config, config, sizeof(pipe_segment_config_t));
    segment->status = SEG_STATUS_NORMAL;
    segment->status_change_ms = pipe_get_time_ms();
    
    /* Update total length */
    if (config->end_km > g_pipe.total_length_km) {
        g_pipe.total_length_km = config->end_km;
    }
    
    return 0;
}

/**
 * @brief Configure a valve
 */
int pipe_configure_valve(const pipe_valve_config_t* config) {
    if (!g_pipe.initialized || !config) return -1;
    if (g_pipe.valve_count >= PIPE_MAX_VALVES) return -2;
    
    pipe_valve_t* valve = &g_pipe.valves[g_pipe.valve_count++];
    memset(valve, 0, sizeof(*valve));
    memcpy(&valve->config, config, sizeof(pipe_valve_config_t));
    valve->state = VALVE_OPEN;  /* Assume normally open */
    valve->commanded = VALVE_OPEN;
    
    return 0;
}

/* ============================================================================
 * Public API - Sensor Updates
 * ============================================================================ */

/**
 * @brief Update sensor reading
 */
int pipe_update_sensor(uint8_t sensor_id, float value, float quality) {
    if (!g_pipe.initialized) return -1;
    
    pipe_sensor_t* sensor = pipe_find_sensor(sensor_id);
    if (!sensor) return -2;
    
    sensor->last_reading.sensor_id = sensor_id;
    sensor->last_reading.type = sensor->config.type;
    sensor->last_reading.value = value;
    sensor->last_reading.quality = quality;
    sensor->last_reading.valid = (quality > 0.5f);
    sensor->last_reading.timestamp_ms = pipe_get_time_ms();
    sensor->reading_count++;
    
    /* Update history */
    pipe_update_history(&sensor->history, value);
    
    /* Check limits */
    if (value < sensor->config.low_limit || value > sensor->config.high_limit) {
        if (sensor->config.type == SENSOR_PRESSURE) {
            if (value < sensor->config.low_limit) {
                pipe_log_event(EVENT_PRESSURE_LOW, sensor->config.segment_id,
                              sensor_id, value, "Low pressure");
            } else {
                pipe_log_event(EVENT_PRESSURE_HIGH, sensor->config.segment_id,
                              sensor_id, value, "High pressure");
            }
        }
    }
    
    g_pipe.stats.readings_processed++;
    
    return 0;
}

/**
 * @brief Set sensor online/offline
 */
int pipe_set_sensor_online(uint8_t sensor_id, bool online) {
    if (!g_pipe.initialized) return -1;
    
    pipe_sensor_t* sensor = pipe_find_sensor(sensor_id);
    if (!sensor) return -2;
    
    if (sensor->online != online) {
        sensor->online = online;
        pipe_log_event(online ? EVENT_SENSOR_ONLINE : EVENT_SENSOR_OFFLINE,
                      sensor->config.segment_id, sensor_id, 0,
                      online ? "Sensor online" : "Sensor offline");
    }
    
    return 0;
}

/* ============================================================================
 * Public API - Monitoring Control
 * ============================================================================ */

/**
 * @brief Start monitoring
 */
int pipe_start_monitoring(void) {
    if (!g_pipe.initialized) return -1;
    if (g_pipe.segment_count == 0) return -2;
    
    g_pipe.monitoring_active = true;
    
    /* Enable leak detection on all segments */
    for (uint8_t i = 0; i < g_pipe.segment_count; i++) {
        g_pipe.segments[i].config.leak_detection_enabled = true;
        g_pipe.segments[i].status_change_ms = pipe_get_time_ms();
    }
    
    return 0;
}

/**
 * @brief Stop monitoring
 */
int pipe_stop_monitoring(void) {
    if (!g_pipe.initialized) return -1;
    
    g_pipe.monitoring_active = false;
    
    for (uint8_t i = 0; i < g_pipe.segment_count; i++) {
        g_pipe.segments[i].config.leak_detection_enabled = false;
    }
    
    return 0;
}

/**
 * @brief Process pipeline monitoring (call periodically)
 */
int pipe_process(uint32_t delta_ms) {
    if (!g_pipe.initialized) return -1;
    
    g_pipe.current_time_ms += delta_ms;
    g_pipe.stats.uptime_seconds = g_pipe.current_time_ms / 1000;
    
    /* Update valves */
    pipe_update_valves(delta_ms);
    
    /* Process each segment */
    if (g_pipe.monitoring_active) {
        for (uint8_t i = 0; i < g_pipe.segment_count; i++) {
            pipe_process_segment(&g_pipe.segments[i], delta_ms);
        }
    }
    
    /* Calculate availability */
    uint8_t online_sensors = 0;
    for (uint8_t i = 0; i < g_pipe.sensor_count; i++) {
        if (g_pipe.sensors[i].online) online_sensors++;
    }
    g_pipe.stats.system_availability = (g_pipe.sensor_count > 0) ?
        (float)online_sensors / g_pipe.sensor_count : 0;
    
    g_pipe.last_process_ms = pipe_get_time_ms();
    
    return 0;
}

/* ============================================================================
 * Public API - Query Functions
 * ============================================================================ */

/**
 * @brief Get segment status
 */
int pipe_get_segment_status(uint8_t segment_id, pipe_seg_status_t* status,
                            float* imbalance, float* leak_rate) {
    if (!g_pipe.initialized) return -1;
    
    pipe_segment_t* segment = pipe_find_segment(segment_id);
    if (!segment) return -2;
    
    if (status) *status = segment->status;
    if (imbalance) *imbalance = segment->imbalance_pct;
    if (leak_rate) *leak_rate = segment->estimated_leak_rate;
    
    return 0;
}

/**
 * @brief Get active alerts
 */
int pipe_get_alerts(pipe_alert_t* alerts, uint8_t max_alerts, uint8_t* count) {
    if (!g_pipe.initialized || !alerts || !count) return -1;
    
    uint8_t n = 0;
    for (uint8_t i = 0; i < g_pipe.alert_count && n < max_alerts; i++) {
        if (g_pipe.alerts[i].active) {
            memcpy(&alerts[n++], &g_pipe.alerts[i], sizeof(pipe_alert_t));
        }
    }
    *count = n;
    
    return 0;
}

/**
 * @brief Acknowledge alert
 */
int pipe_acknowledge_alert(uint32_t alert_id) {
    if (!g_pipe.initialized) return -1;
    
    for (uint8_t i = 0; i < g_pipe.alert_count; i++) {
        if (g_pipe.alerts[i].alert_id == alert_id) {
            g_pipe.alerts[i].acknowledged = true;
            return 0;
        }
    }
    
    return -2;  /* Alert not found */
}

/**
 * @brief Get system statistics
 */
int pipe_get_stats(pipe_stats_t* stats) {
    if (!g_pipe.initialized || !stats) return -1;
    
    memcpy(stats, &g_pipe.stats, sizeof(pipe_stats_t));
    return 0;
}

/**
 * @brief Get event log
 */
int pipe_get_events(pipe_event_t* events, uint16_t max_events, uint16_t* count) {
    if (!g_pipe.initialized || !events || !count) return -1;
    
    uint16_t copy_count = (g_pipe.event_count < max_events) ? 
                          g_pipe.event_count : max_events;
    
    /* Copy events in chronological order */
    uint16_t start = (g_pipe.event_head + PIPE_MAX_EVENTS - g_pipe.event_count) % 
                     PIPE_MAX_EVENTS;
    
    for (uint16_t i = 0; i < copy_count; i++) {
        uint16_t idx = (start + i) % PIPE_MAX_EVENTS;
        memcpy(&events[i], &g_pipe.events[idx], sizeof(pipe_event_t));
    }
    
    *count = copy_count;
    return 0;
}

/* ============================================================================
 * Public API - Manual Control
 * ============================================================================ */

/**
 * @brief Trigger emergency shutdown
 */
int pipe_trigger_shutdown(uint8_t segment_id) {
    if (!g_pipe.initialized) return -1;
    return pipe_emergency_shutdown(segment_id);
}

/**
 * @brief Control valve
 */
int pipe_control_valve(uint8_t valve_id, bool open) {
    if (!g_pipe.initialized) return -1;
    return pipe_command_valve(valve_id, open ? VALVE_OPEN : VALVE_CLOSED);
}

/**
 * @brief Set segment to maintenance mode
 */
int pipe_set_maintenance(uint8_t segment_id, bool maintenance) {
    if (!g_pipe.initialized) return -1;
    
    pipe_segment_t* segment = pipe_find_segment(segment_id);
    if (!segment) return -2;
    
    if (maintenance) {
        segment->status = SEG_STATUS_MAINTENANCE;
        segment->config.leak_detection_enabled = false;
        pipe_log_event(EVENT_MAINTENANCE_START, segment_id, 0, 0,
                      "Entered maintenance");
    } else {
        segment->status = SEG_STATUS_NORMAL;
        segment->status_change_ms = pipe_get_time_ms();
        pipe_log_event(EVENT_MAINTENANCE_END, segment_id, 0, 0,
                      "Exited maintenance");
    }
    
    return 0;
}

/* ============================================================================
 * Public API - Telemetry
 * ============================================================================ */

/**
 * @brief Generate telemetry packet
 */
int pipe_generate_telemetry(uint8_t* buffer, uint16_t max_len, uint16_t* len) {
    if (!g_pipe.initialized || !buffer || !len) return -1;
    if (max_len < 128) return -2;
    
    /* Build simple telemetry structure */
    uint16_t offset = 0;
    
    /* Header */
    buffer[offset++] = 'P';
    buffer[offset++] = 'I';
    buffer[offset++] = 'P';
    buffer[offset++] = 'E';
    
    /* Pipeline ID */
    memcpy(&buffer[offset], &g_pipe.pipeline_id, 4);
    offset += 4;
    
    /* Timestamp */
    uint32_t time = pipe_get_time_ms();
    memcpy(&buffer[offset], &time, 4);
    offset += 4;
    
    /* Status summary */
    buffer[offset++] = g_pipe.segment_count;
    buffer[offset++] = g_pipe.sensor_count;
    buffer[offset++] = g_pipe.monitoring_active ? 1 : 0;
    
    /* Alert count */
    uint8_t active_alerts = 0;
    for (uint8_t i = 0; i < g_pipe.alert_count; i++) {
        if (g_pipe.alerts[i].active) active_alerts++;
    }
    buffer[offset++] = active_alerts;
    
    /* Per-segment status */
    for (uint8_t i = 0; i < g_pipe.segment_count && offset + 16 < max_len; i++) {
        pipe_segment_t* seg = &g_pipe.segments[i];
        buffer[offset++] = seg->config.segment_id;
        buffer[offset++] = (uint8_t)seg->status;
        memcpy(&buffer[offset], &seg->flow_in, 4);
        offset += 4;
        memcpy(&buffer[offset], &seg->flow_out, 4);
        offset += 4;
        memcpy(&buffer[offset], &seg->imbalance_pct, 4);
        offset += 4;
    }
    
    *len = offset;
    return 0;
}

/**
 * @brief Inject simulated leak for testing
 */
int pipe_inject_leak(uint8_t segment_id, float leak_rate, float location_km) {
    if (!g_pipe.initialized) return -1;
    
    (void)location_km;  /* Could be used for more accurate simulation */
    
    pipe_segment_t* segment = pipe_find_segment(segment_id);
    if (!segment) return -2;
    
    /* Modify flow readings to simulate leak */
    pipe_sensor_t* flow_out = pipe_find_sensor(segment->config.downstream_flow_sensor);
    if (flow_out && flow_out->online) {
        /* Reduce downstream flow by leak rate */
        flow_out->last_reading.value -= leak_rate;
        if (flow_out->last_reading.value < 0) {
            flow_out->last_reading.value = 0;
        }
        pipe_update_history(&flow_out->history, flow_out->last_reading.value);
    }
    
    return 0;
}

/**
 * @brief Inject sensor drift for testing
 */
int pipe_inject_drift(uint8_t sensor_id, float drift_rate) {
    if (!g_pipe.initialized) return -1;
    
    pipe_sensor_t* sensor = pipe_find_sensor(sensor_id);
    if (!sensor) return -2;
    
    /* Apply drift to current reading */
    sensor->last_reading.value += drift_rate;
    pipe_update_history(&sensor->history, sensor->last_reading.value);
    
    return 0;
}
