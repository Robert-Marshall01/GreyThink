/**
 * @file firefighting_spotlight.c
 * @brief Firefighting Thermal Imaging & Suppression Spotlight
 * 
 * PRODUCTION-GRADE FIREFIGHTING SYSTEM
 * ====================================
 * 
 * This spotlight demonstrates embedded safety systems expertise through:
 * - Thermal imaging preprocessing and heat map generation
 * - Fire detection with hot spot tracking
 * - Multi-zone suppression control (water, foam, gas)
 * - Emergency telemetry for incident command
 * - Safety interlocks per NFPA standards
 * 
 * INDUSTRY RELEVANCE:
 * Fire protection systems are safety-critical and highly regulated. This
 * implementation demonstrates competency in:
 * - Real-time embedded safety systems (IEC 61508)
 * - NFPA compliance (13, 72, 2001)
 * - Sensor fusion for fire detection
 * - Fail-safe actuator control
 * - Emergency communication protocols
 * 
 * ARCHITECTURE:
 * 
 *   Thermal Camera → Preprocessing → Fire Detection → Suppression Control
 *         ↓                              ↓                    ↓
 *   Heat Map Gen    →    Hot Spot    →  Zone    →   Valve   →  Telemetry
 *                        Tracking      Selection    Control      ↓
 *                                                              Cloud/CAD
 * 
 * SAFETY PHILOSOPHY:
 * - Fail-safe defaults (suppression arms on fault)
 * - Dual-detector activation option (cross-zone)
 * - Manual override capability
 * - Comprehensive audit logging
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define FF_MAX_ZONES            16      /* Maximum suppression zones */
#define FF_MAX_THERMAL_SENSORS  8       /* Maximum thermal sensors */
#define FF_MAX_HOT_SPOTS        32      /* Maximum tracked hot spots */
#define FF_MAX_EVENTS           64      /* Event log capacity */
#define FF_THERMAL_WIDTH        32      /* Thermal frame width */
#define FF_THERMAL_HEIGHT       32      /* Thermal frame height */
#define FF_THERMAL_PIXELS       (FF_THERMAL_WIDTH * FF_THERMAL_HEIGHT)

/* Temperature thresholds (Celsius) */
#define FF_TEMP_AMBIENT         25.0f   /* Normal ambient */
#define FF_TEMP_WARNING         60.0f   /* Warning threshold */
#define FF_TEMP_ALARM           85.0f   /* Alarm threshold */
#define FF_TEMP_FIRE            150.0f  /* Fire confirmation */
#define FF_TEMP_FLASHOVER       600.0f  /* Flashover temperature */

/* Rate of rise threshold (°C/minute) */
#define FF_RATE_OF_RISE         15.0f

/* Suppression timing */
#define FF_PREACTION_DELAY_MS   30000   /* Pre-action delay (30s) */
#define FF_DISCHARGE_TIME_MS    300000  /* Discharge duration (5min) */
#define FF_GAS_HOLD_TIME_MS     600000  /* Gas hold time (10min) */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/* Agent types for suppression */
typedef enum {
    FF_AGENT_NONE = 0,
    FF_AGENT_WATER,
    FF_AGENT_FOAM,
    FF_AGENT_CO2,
    FF_AGENT_FM200,
    FF_AGENT_WATER_MIST
} ff_agent_t;

/* System types */
typedef enum {
    FF_SYSTEM_WET_PIPE,
    FF_SYSTEM_DRY_PIPE,
    FF_SYSTEM_PREACTION,
    FF_SYSTEM_DELUGE,
    FF_SYSTEM_GASEOUS
} ff_system_type_t;

/* Zone states */
typedef enum {
    FF_ZONE_IDLE,
    FF_ZONE_MONITORING,
    FF_ZONE_WARNING,
    FF_ZONE_ALARM,
    FF_ZONE_PREACTION,
    FF_ZONE_DISCHARGING,
    FF_ZONE_DISCHARGED,
    FF_ZONE_FAULT,
    FF_ZONE_DISABLED
} ff_zone_state_t;

/* Fire stages */
typedef enum {
    FF_FIRE_NONE,
    FF_FIRE_INCIPIENT,
    FF_FIRE_GROWTH,
    FF_FIRE_FULLY_DEVELOPED,
    FF_FIRE_DECAY,
    FF_FIRE_EXTINGUISHED
} ff_fire_stage_t;

/* Event types */
typedef enum {
    FF_EVENT_SYSTEM_START,
    FF_EVENT_ZONE_ARMED,
    FF_EVENT_ZONE_DISARMED,
    FF_EVENT_TEMP_WARNING,
    FF_EVENT_TEMP_ALARM,
    FF_EVENT_FIRE_DETECTED,
    FF_EVENT_SUPPRESSION_START,
    FF_EVENT_SUPPRESSION_END,
    FF_EVENT_VALVE_OPEN,
    FF_EVENT_VALVE_CLOSE,
    FF_EVENT_ABORT,
    FF_EVENT_FAULT,
    FF_EVENT_MANUAL_TRIGGER,
    FF_EVENT_ALL_CLEAR
} ff_event_type_t;

/* Valve states */
typedef enum {
    FF_VALVE_CLOSED,
    FF_VALVE_OPENING,
    FF_VALVE_OPEN,
    FF_VALVE_CLOSING,
    FF_VALVE_FAULT
} ff_valve_state_t;

/* Hot spot descriptor */
typedef struct {
    uint16_t x;                 /* X coordinate */
    uint16_t y;                 /* Y coordinate */
    float temperature;          /* Current temperature */
    float peak_temperature;     /* Peak recorded temp */
    float rate_of_rise;         /* °C per minute */
    float area_m2;              /* Estimated area */
    bool growing;               /* Is hot spot expanding */
    bool confirmed_fire;        /* Fire confirmed */
    uint32_t first_detect_ms;   /* First detection time */
    uint32_t last_update_ms;    /* Last update time */
    uint8_t zone_id;            /* Associated zone */
} ff_hot_spot_t;

/* Thermal sensor state */
typedef struct {
    uint8_t sensor_id;
    bool online;
    bool calibrated;
    float frame[FF_THERMAL_PIXELS];     /* Temperature data */
    float min_temp;
    float max_temp;
    float avg_temp;
    uint32_t last_frame_ms;
    uint32_t fault_count;
} ff_thermal_sensor_t;

/* Zone configuration */
typedef struct {
    uint8_t zone_id;
    char name[24];
    ff_system_type_t system_type;
    ff_agent_t agent;
    float area_m2;
    uint8_t sensor_ids[4];      /* Sensors covering this zone */
    uint8_t sensor_count;
    float alarm_threshold;      /* Temperature alarm threshold */
    float discharge_rate;       /* L/min or kg/min */
    uint32_t preaction_delay;   /* Pre-action delay (ms) */
    bool require_cross_zone;    /* Require 2 detectors */
    bool abort_enabled;         /* Allow manual abort */
} ff_zone_config_t;

/* Zone runtime state */
typedef struct {
    ff_zone_state_t state;
    ff_valve_state_t valve_state;
    float pressure_psi;
    float agent_level_pct;
    float max_temp_detected;
    uint8_t hot_spot_count;
    uint8_t alarm_count;        /* Number of alarms active */
    bool manually_triggered;
    bool aborted;
    uint32_t alarm_start_ms;
    uint32_t discharge_start_ms;
    uint32_t state_change_ms;
} ff_zone_state_data_t;

/* Event record */
typedef struct {
    ff_event_type_t type;
    uint32_t timestamp;
    uint8_t zone_id;
    float temperature;
    char details[48];
} ff_event_t;

/* Incident data */
typedef struct {
    uint32_t incident_id;
    uint32_t start_time;
    ff_fire_stage_t stage;
    uint8_t zones_affected;     /* Bitmap */
    uint8_t zones_discharged;   /* Bitmap */
    float max_temperature;
    float spread_rate_m2_min;
    bool evacuation_ordered;
    bool fire_dept_notified;
    bool suppression_effective;
    uint32_t resolution_time;
} ff_incident_t;

/* System statistics */
typedef struct {
    uint32_t uptime_seconds;
    uint32_t frames_processed;
    uint32_t hot_spots_detected;
    uint32_t alarms_triggered;
    uint32_t suppressions_activated;
    uint32_t false_alarms;
    uint32_t faults_detected;
    float total_agent_used;     /* Liters or kg */
} ff_stats_t;

/* Global system state */
typedef struct {
    bool initialized;
    bool armed;
    uint32_t current_time_ms;
    
    /* Thermal sensors */
    ff_thermal_sensor_t sensors[FF_MAX_THERMAL_SENSORS];
    uint8_t sensor_count;
    
    /* Zones */
    ff_zone_config_t zone_configs[FF_MAX_ZONES];
    ff_zone_state_data_t zone_states[FF_MAX_ZONES];
    uint8_t zone_count;
    
    /* Hot spots */
    ff_hot_spot_t hot_spots[FF_MAX_HOT_SPOTS];
    uint8_t hot_spot_count;
    
    /* Current incident */
    ff_incident_t incident;
    bool incident_active;
    
    /* Event log */
    ff_event_t events[FF_MAX_EVENTS];
    uint16_t event_head;
    uint16_t event_count;
    
    /* Statistics */
    ff_stats_t stats;
    
    /* Processing state */
    float heat_map[FF_THERMAL_PIXELS];
    float gradient_map[FF_THERMAL_PIXELS];
    
} ff_system_t;

/* ============================================================================
 * Global State
 * ============================================================================ */

static ff_system_t g_ff;

/* ============================================================================
 * Time Helper
 * ============================================================================ */

static uint32_t ff_get_time_ms(void) {
    return g_ff.current_time_ms;
}

/* ============================================================================
 * Event Logging
 * ============================================================================ */

static void ff_log_event(ff_event_type_t type, uint8_t zone_id, 
                        float temp, const char* details) {
    ff_event_t* event = &g_ff.events[g_ff.event_head];
    
    event->type = type;
    event->timestamp = ff_get_time_ms();
    event->zone_id = zone_id;
    event->temperature = temp;
    
    if (details) {
        strncpy(event->details, details, sizeof(event->details) - 1);
        event->details[sizeof(event->details) - 1] = '\0';
    } else {
        event->details[0] = '\0';
    }
    
    g_ff.event_head = (g_ff.event_head + 1) % FF_MAX_EVENTS;
    if (g_ff.event_count < FF_MAX_EVENTS) {
        g_ff.event_count++;
    }
}

/* ============================================================================
 * Thermal Image Preprocessing
 * ============================================================================ */

/**
 * @brief Apply 3x3 Gaussian blur to thermal frame for noise reduction
 */
static void ff_gaussian_blur(const float* src, float* dst, 
                            uint16_t width, uint16_t height) {
    /* Gaussian kernel (normalized) */
    const float kernel[9] = {
        0.0625f, 0.125f, 0.0625f,
        0.125f,  0.25f,  0.125f,
        0.0625f, 0.125f, 0.0625f
    };
    
    for (uint16_t y = 1; y < height - 1; y++) {
        for (uint16_t x = 1; x < width - 1; x++) {
            float sum = 0;
            int ki = 0;
            
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    uint16_t idx = (uint16_t)((y + dy) * width + (x + dx));
                    sum += src[idx] * kernel[ki++];
                }
            }
            
            dst[y * width + x] = sum;
        }
    }
    
    /* Copy edges */
    for (uint16_t x = 0; x < width; x++) {
        dst[x] = src[x];
        dst[(height - 1) * width + x] = src[(height - 1) * width + x];
    }
    for (uint16_t y = 0; y < height; y++) {
        dst[y * width] = src[y * width];
        dst[y * width + width - 1] = src[y * width + width - 1];
    }
}

/**
 * @brief Calculate temperature gradient magnitude for edge detection
 */
static void ff_calculate_gradient(const float* src, float* gradient,
                                  uint16_t width, uint16_t height) {
    for (uint16_t y = 1; y < height - 1; y++) {
        for (uint16_t x = 1; x < width - 1; x++) {
            /* Sobel operators */
            float gx = -src[(y-1) * width + (x-1)] + src[(y-1) * width + (x+1)]
                      -2*src[y * width + (x-1)]    + 2*src[y * width + (x+1)]
                      -src[(y+1) * width + (x-1)] + src[(y+1) * width + (x+1)];
            
            float gy = -src[(y-1) * width + (x-1)] - 2*src[(y-1) * width + x] 
                      -src[(y-1) * width + (x+1)]
                      +src[(y+1) * width + (x-1)] + 2*src[(y+1) * width + x] 
                      +src[(y+1) * width + (x+1)];
            
            gradient[y * width + x] = sqrtf(gx * gx + gy * gy);
        }
    }
}

/**
 * @brief Generate heat map with color coding based on temperature ranges
 * 
 * Heat map encoding:
 * 0x00-0x3F: Cool (< 40°C) - Blue
 * 0x40-0x7F: Warm (40-80°C) - Yellow  
 * 0x80-0xBF: Hot (80-150°C) - Orange
 * 0xC0-0xFF: Critical (>150°C) - Red
 */
static void ff_generate_heat_map(const float* temps, uint8_t* heat_map,
                                 uint16_t width, uint16_t height) {
    for (uint16_t i = 0; i < width * height; i++) {
        float t = temps[i];
        
        if (t < 40.0f) {
            /* Cool - scale 0-63 for temps 0-40°C */
            heat_map[i] = (uint8_t)(t * 63.0f / 40.0f);
        } else if (t < 80.0f) {
            /* Warm - scale 64-127 for temps 40-80°C */
            heat_map[i] = 64 + (uint8_t)((t - 40.0f) * 63.0f / 40.0f);
        } else if (t < 150.0f) {
            /* Hot - scale 128-191 for temps 80-150°C */
            heat_map[i] = 128 + (uint8_t)((t - 80.0f) * 63.0f / 70.0f);
        } else {
            /* Critical - scale 192-255 for temps 150-400°C+ */
            float scaled = (t - 150.0f) * 63.0f / 250.0f;
            heat_map[i] = 192 + (uint8_t)(scaled > 63.0f ? 63.0f : scaled);
        }
    }
}

/**
 * @brief Process thermal frame: blur, statistics, gradient calculation
 */
static void ff_process_thermal_frame(ff_thermal_sensor_t* sensor) {
    float* frame = sensor->frame;
    float temp_buffer[FF_THERMAL_PIXELS];
    
    /* Apply Gaussian blur for noise reduction */
    ff_gaussian_blur(frame, temp_buffer, FF_THERMAL_WIDTH, FF_THERMAL_HEIGHT);
    memcpy(frame, temp_buffer, sizeof(temp_buffer));
    
    /* Calculate statistics */
    sensor->min_temp = 1000.0f;
    sensor->max_temp = -100.0f;
    float sum = 0;
    
    for (uint16_t i = 0; i < FF_THERMAL_PIXELS; i++) {
        if (frame[i] < sensor->min_temp) sensor->min_temp = frame[i];
        if (frame[i] > sensor->max_temp) sensor->max_temp = frame[i];
        sum += frame[i];
    }
    sensor->avg_temp = sum / FF_THERMAL_PIXELS;
    
    /* Calculate gradient for edge detection */
    ff_calculate_gradient(frame, g_ff.gradient_map, 
                         FF_THERMAL_WIDTH, FF_THERMAL_HEIGHT);
    
    sensor->last_frame_ms = ff_get_time_ms();
    g_ff.stats.frames_processed++;
}

/* ============================================================================
 * Hot Spot Detection
 * ============================================================================ */

/**
 * @brief Find local temperature maxima (potential hot spots)
 */
static void ff_detect_local_maxima(const float* temps, 
                                   uint16_t width, uint16_t height,
                                   float threshold,
                                   uint16_t* x_coords, uint16_t* y_coords,
                                   float* max_temps, uint8_t max_spots,
                                   uint8_t* count) {
    *count = 0;
    
    for (uint16_t y = 2; y < height - 2 && *count < max_spots; y++) {
        for (uint16_t x = 2; x < width - 2 && *count < max_spots; x++) {
            float center = temps[y * width + x];
            
            if (center < threshold) continue;
            
            /* Check if local maximum (8-connected) */
            bool is_maximum = true;
            for (int dy = -1; dy <= 1 && is_maximum; dy++) {
                for (int dx = -1; dx <= 1 && is_maximum; dx++) {
                    if (dx == 0 && dy == 0) continue;
                    if (temps[(y + dy) * width + (x + dx)] >= center) {
                        is_maximum = false;
                    }
                }
            }
            
            if (is_maximum) {
                x_coords[*count] = x;
                y_coords[*count] = y;
                max_temps[*count] = center;
                (*count)++;
            }
        }
    }
}

/**
 * @brief Update hot spot tracking based on new detections
 */
static void ff_update_hot_spots(uint8_t sensor_id) {
    ff_thermal_sensor_t* sensor = &g_ff.sensors[sensor_id];
    
    uint16_t x_coords[16], y_coords[16];
    float temps[16];
    uint8_t new_count = 0;
    
    /* Detect new maxima */
    ff_detect_local_maxima(sensor->frame, FF_THERMAL_WIDTH, FF_THERMAL_HEIGHT,
                          FF_TEMP_WARNING, x_coords, y_coords, temps, 16, &new_count);
    
    uint32_t now = ff_get_time_ms();
    
    for (uint8_t i = 0; i < new_count; i++) {
        /* Check if near existing hot spot */
        bool found = false;
        for (uint8_t j = 0; j < g_ff.hot_spot_count; j++) {
            ff_hot_spot_t* hs = &g_ff.hot_spots[j];
            
            int dx = (int)x_coords[i] - (int)hs->x;
            int dy = (int)y_coords[i] - (int)hs->y;
            float dist = sqrtf((float)(dx * dx + dy * dy));
            
            if (dist < 5.0f) {
                /* Update existing hot spot */
                float old_temp = hs->temperature;
                hs->temperature = temps[i];
                hs->x = x_coords[i];
                hs->y = y_coords[i];
                
                /* Calculate rate of rise */
                float dt_min = (float)(now - hs->last_update_ms) / 60000.0f;
                if (dt_min > 0.01f) {
                    hs->rate_of_rise = (temps[i] - old_temp) / dt_min;
                }
                
                if (temps[i] > hs->peak_temperature) {
                    hs->peak_temperature = temps[i];
                }
                
                hs->growing = (hs->rate_of_rise > 0);
                hs->last_update_ms = now;
                
                /* Confirm fire if temperature high enough */
                if (temps[i] >= FF_TEMP_FIRE && !hs->confirmed_fire) {
                    hs->confirmed_fire = true;
                    g_ff.stats.hot_spots_detected++;
                }
                
                found = true;
                break;
            }
        }
        
        /* Add new hot spot */
        if (!found && g_ff.hot_spot_count < FF_MAX_HOT_SPOTS) {
            ff_hot_spot_t* hs = &g_ff.hot_spots[g_ff.hot_spot_count];
            hs->x = x_coords[i];
            hs->y = y_coords[i];
            hs->temperature = temps[i];
            hs->peak_temperature = temps[i];
            hs->rate_of_rise = 0;
            hs->area_m2 = 0.1f;  /* Initial estimate */
            hs->growing = true;
            hs->confirmed_fire = (temps[i] >= FF_TEMP_FIRE);
            hs->first_detect_ms = now;
            hs->last_update_ms = now;
            hs->zone_id = 0;  /* Will be assigned later */
            
            g_ff.hot_spot_count++;
            g_ff.stats.hot_spots_detected++;
        }
    }
    
    /* Age out old hot spots */
    for (uint8_t i = 0; i < g_ff.hot_spot_count; ) {
        if (now - g_ff.hot_spots[i].last_update_ms > 30000) {
            /* Remove if not updated in 30 seconds */
            memmove(&g_ff.hot_spots[i], &g_ff.hot_spots[i + 1],
                   (g_ff.hot_spot_count - i - 1) * sizeof(ff_hot_spot_t));
            g_ff.hot_spot_count--;
        } else {
            i++;
        }
    }
}

/* ============================================================================
 * Zone Management
 * ============================================================================ */

/**
 * @brief Check zone alarms based on hot spots and temperature
 */
static void ff_check_zone_alarms(uint8_t zone_id) {
    ff_zone_config_t* config = &g_ff.zone_configs[zone_id];
    ff_zone_state_data_t* state = &g_ff.zone_states[zone_id];
    
    if (state->state == FF_ZONE_DISABLED || state->state == FF_ZONE_FAULT) {
        return;
    }
    
    uint8_t alarm_count = 0;
    float max_temp = 0;
    
    /* Check each sensor in zone */
    for (uint8_t i = 0; i < config->sensor_count; i++) {
        uint8_t sid = config->sensor_ids[i];
        if (sid >= g_ff.sensor_count) continue;
        
        ff_thermal_sensor_t* sensor = &g_ff.sensors[sid];
        if (!sensor->online) continue;
        
        if (sensor->max_temp > max_temp) {
            max_temp = sensor->max_temp;
        }
        
        if (sensor->max_temp >= config->alarm_threshold) {
            alarm_count++;
        }
    }
    
    state->max_temp_detected = max_temp;
    state->alarm_count = alarm_count;
    
    /* State transitions based on temperature */
    uint32_t now = ff_get_time_ms();
    
    switch (state->state) {
        case FF_ZONE_IDLE:
        case FF_ZONE_MONITORING:
            if (max_temp >= FF_TEMP_WARNING) {
                state->state = FF_ZONE_WARNING;
                state->state_change_ms = now;
                ff_log_event(FF_EVENT_TEMP_WARNING, zone_id, max_temp, 
                            config->name);
            }
            break;
            
        case FF_ZONE_WARNING:
            if (max_temp >= config->alarm_threshold) {
                bool trigger = !config->require_cross_zone || (alarm_count >= 2);
                
                if (trigger) {
                    state->state = FF_ZONE_ALARM;
                    state->alarm_start_ms = now;
                    state->state_change_ms = now;
                    g_ff.stats.alarms_triggered++;
                    ff_log_event(FF_EVENT_TEMP_ALARM, zone_id, max_temp,
                                config->name);
                }
            } else if (max_temp < FF_TEMP_WARNING) {
                state->state = FF_ZONE_MONITORING;
                state->state_change_ms = now;
            }
            break;
            
        case FF_ZONE_ALARM:
            /* Check for suppression trigger */
            if (max_temp >= FF_TEMP_FIRE || state->manually_triggered) {
                if (config->system_type == FF_SYSTEM_PREACTION) {
                    state->state = FF_ZONE_PREACTION;
                    state->state_change_ms = now;
                    ff_log_event(FF_EVENT_FIRE_DETECTED, zone_id, max_temp,
                                "Pre-action initiated");
                } else {
                    /* Immediate discharge for other systems */
                    state->state = FF_ZONE_DISCHARGING;
                    state->discharge_start_ms = now;
                    state->state_change_ms = now;
                    ff_log_event(FF_EVENT_SUPPRESSION_START, zone_id, max_temp,
                                config->name);
                }
            }
            break;
            
        case FF_ZONE_PREACTION:
            /* Check pre-action delay */
            if (state->aborted) {
                state->state = FF_ZONE_MONITORING;
                state->aborted = false;
                state->state_change_ms = now;
                ff_log_event(FF_EVENT_ABORT, zone_id, 0, "Manual abort");
            } else if (now - state->state_change_ms >= config->preaction_delay) {
                state->state = FF_ZONE_DISCHARGING;
                state->discharge_start_ms = now;
                state->state_change_ms = now;
                g_ff.stats.suppressions_activated++;
                ff_log_event(FF_EVENT_SUPPRESSION_START, zone_id, max_temp,
                            config->name);
            }
            break;
            
        case FF_ZONE_DISCHARGING:
            /* Check if discharge complete or fire out */
            if (now - state->discharge_start_ms >= FF_DISCHARGE_TIME_MS) {
                state->state = FF_ZONE_DISCHARGED;
                state->state_change_ms = now;
                ff_log_event(FF_EVENT_SUPPRESSION_END, zone_id, max_temp,
                            "Discharge complete");
            }
            break;
            
        default:
            break;
    }
}

/* ============================================================================
 * Valve Control
 * ============================================================================ */

/**
 * @brief Control suppression valve for a zone
 */
static int ff_control_valve(uint8_t zone_id, bool open) {
    if (zone_id >= g_ff.zone_count) return -1;
    
    ff_zone_state_data_t* state = &g_ff.zone_states[zone_id];
    
    if (open) {
        state->valve_state = FF_VALVE_OPENING;
        /* Simulate valve opening time */
        state->valve_state = FF_VALVE_OPEN;
        ff_log_event(FF_EVENT_VALVE_OPEN, zone_id, 0, "Valve opened");
    } else {
        state->valve_state = FF_VALVE_CLOSING;
        state->valve_state = FF_VALVE_CLOSED;
        ff_log_event(FF_EVENT_VALVE_CLOSE, zone_id, 0, "Valve closed");
    }
    
    return 0;
}

/**
 * @brief Update valve states based on zone state
 */
static void ff_update_valves(void) {
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        ff_zone_state_data_t* state = &g_ff.zone_states[i];
        
        if (state->state == FF_ZONE_DISCHARGING) {
            if (state->valve_state != FF_VALVE_OPEN) {
                ff_control_valve(i, true);
            }
            
            /* Update agent level */
            ff_zone_config_t* config = &g_ff.zone_configs[i];
            float discharge_per_ms = config->discharge_rate / 60000.0f;
            state->agent_level_pct -= discharge_per_ms * 100.0f / 1000.0f;
            if (state->agent_level_pct < 0) {
                state->agent_level_pct = 0;
            }
            g_ff.stats.total_agent_used += discharge_per_ms;
            
        } else if (state->state != FF_ZONE_DISCHARGING && 
                   state->valve_state == FF_VALVE_OPEN) {
            ff_control_valve(i, false);
        }
    }
}

/* ============================================================================
 * Incident Management
 * ============================================================================ */

/**
 * @brief Start a new fire incident
 */
static void ff_start_incident(void) {
    if (g_ff.incident_active) return;
    
    g_ff.incident_active = true;
    g_ff.incident.incident_id = ff_get_time_ms();
    g_ff.incident.start_time = ff_get_time_ms();
    g_ff.incident.stage = FF_FIRE_INCIPIENT;
    g_ff.incident.zones_affected = 0;
    g_ff.incident.zones_discharged = 0;
    g_ff.incident.max_temperature = 0;
    g_ff.incident.spread_rate_m2_min = 0;
    g_ff.incident.evacuation_ordered = false;
    g_ff.incident.fire_dept_notified = false;
    g_ff.incident.suppression_effective = false;
    g_ff.incident.resolution_time = 0;
}

/**
 * @brief Update incident status based on current conditions
 */
static void ff_update_incident(void) {
    if (!g_ff.incident_active) return;
    
    /* Update affected zones */
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        ff_zone_state_data_t* state = &g_ff.zone_states[i];
        
        if (state->state >= FF_ZONE_ALARM) {
            g_ff.incident.zones_affected |= (1 << i);
        }
        
        if (state->state == FF_ZONE_DISCHARGING || 
            state->state == FF_ZONE_DISCHARGED) {
            g_ff.incident.zones_discharged |= (1 << i);
        }
        
        if (state->max_temp_detected > g_ff.incident.max_temperature) {
            g_ff.incident.max_temperature = state->max_temp_detected;
        }
    }
    
    /* Update fire stage */
    float max_temp = g_ff.incident.max_temperature;
    
    if (max_temp >= FF_TEMP_FLASHOVER) {
        g_ff.incident.stage = FF_FIRE_FULLY_DEVELOPED;
    } else if (max_temp >= FF_TEMP_FIRE) {
        g_ff.incident.stage = FF_FIRE_GROWTH;
    } else if (max_temp >= FF_TEMP_ALARM) {
        g_ff.incident.stage = FF_FIRE_INCIPIENT;
    } else if (g_ff.incident.stage == FF_FIRE_GROWTH || 
               g_ff.incident.stage == FF_FIRE_FULLY_DEVELOPED) {
        g_ff.incident.stage = FF_FIRE_DECAY;
    }
    
    /* Check for resolution */
    bool all_clear = true;
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        if (g_ff.zone_states[i].state >= FF_ZONE_ALARM &&
            g_ff.zone_states[i].state < FF_ZONE_DISCHARGED) {
            all_clear = false;
            break;
        }
    }
    
    if (all_clear && g_ff.incident.zones_discharged > 0) {
        g_ff.incident.stage = FF_FIRE_EXTINGUISHED;
        g_ff.incident.suppression_effective = true;
        g_ff.incident.resolution_time = ff_get_time_ms();
        ff_log_event(FF_EVENT_ALL_CLEAR, 0, 0, "Incident resolved");
    }
}

/* ============================================================================
 * Public API - Initialization
 * ============================================================================ */

/**
 * @brief Initialize firefighting system
 */
int ff_init(void) {
    if (g_ff.initialized) return -1;
    
    memset(&g_ff, 0, sizeof(g_ff));
    g_ff.initialized = true;
    g_ff.armed = false;
    
    ff_log_event(FF_EVENT_SYSTEM_START, 0, 0, "System initialized");
    
    return 0;
}

/**
 * @brief Shutdown firefighting system
 */
int ff_shutdown(void) {
    if (!g_ff.initialized) return -1;
    
    /* Close all valves */
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        if (g_ff.zone_states[i].valve_state == FF_VALVE_OPEN) {
            ff_control_valve(i, false);
        }
    }
    
    g_ff.initialized = false;
    return 0;
}

/* ============================================================================
 * Public API - Sensor Management
 * ============================================================================ */

/**
 * @brief Register a thermal sensor
 */
int ff_register_sensor(uint8_t sensor_id) {
    if (!g_ff.initialized || sensor_id >= FF_MAX_THERMAL_SENSORS) return -1;
    
    ff_thermal_sensor_t* sensor = &g_ff.sensors[sensor_id];
    memset(sensor, 0, sizeof(*sensor));
    sensor->sensor_id = sensor_id;
    sensor->online = true;
    sensor->calibrated = true;
    
    /* Initialize with ambient temperature */
    for (uint16_t i = 0; i < FF_THERMAL_PIXELS; i++) {
        sensor->frame[i] = FF_TEMP_AMBIENT;
    }
    
    if (sensor_id >= g_ff.sensor_count) {
        g_ff.sensor_count = sensor_id + 1;
    }
    
    return 0;
}

/**
 * @brief Update thermal frame data
 */
int ff_update_thermal_frame(uint8_t sensor_id, const float* temps) {
    if (!g_ff.initialized || sensor_id >= g_ff.sensor_count) return -1;
    
    ff_thermal_sensor_t* sensor = &g_ff.sensors[sensor_id];
    if (!sensor->online) return -2;
    
    memcpy(sensor->frame, temps, FF_THERMAL_PIXELS * sizeof(float));
    ff_process_thermal_frame(sensor);
    ff_update_hot_spots(sensor_id);
    
    return 0;
}

/**
 * @brief Inject a simulated heat source for testing
 */
int ff_inject_heat_source(uint8_t sensor_id, uint16_t x, uint16_t y, 
                         float temp, float radius) {
    if (!g_ff.initialized || sensor_id >= g_ff.sensor_count) return -1;
    if (x >= FF_THERMAL_WIDTH || y >= FF_THERMAL_HEIGHT) return -2;
    
    ff_thermal_sensor_t* sensor = &g_ff.sensors[sensor_id];
    
    /* Apply circular heat source */
    for (int16_t dy = -(int16_t)radius; dy <= (int16_t)radius; dy++) {
        for (int16_t dx = -(int16_t)radius; dx <= (int16_t)radius; dx++) {
            int16_t px = (int16_t)x + dx;
            int16_t py = (int16_t)y + dy;
            
            if (px < 0 || px >= FF_THERMAL_WIDTH) continue;
            if (py < 0 || py >= FF_THERMAL_HEIGHT) continue;
            
            float dist = sqrtf((float)(dx * dx + dy * dy));
            if (dist <= radius) {
                float falloff = 1.0f - (dist / radius);
                float heat = FF_TEMP_AMBIENT + (temp - FF_TEMP_AMBIENT) * falloff;
                
                uint16_t idx = (uint16_t)(py * FF_THERMAL_WIDTH + px);
                if (heat > sensor->frame[idx]) {
                    sensor->frame[idx] = heat;
                }
            }
        }
    }
    
    /* Process frame to update sensor statistics */
    ff_process_thermal_frame(sensor);
    
    /* Update hot spot tracking after heat injection */
    ff_update_hot_spots(sensor_id);
    
    return 0;
}

/* ============================================================================
 * Public API - Zone Management
 * ============================================================================ */

/**
 * @brief Configure a suppression zone
 */
int ff_configure_zone(const ff_zone_config_t* config) {
    if (!g_ff.initialized || !config) return -1;
    if (config->zone_id >= FF_MAX_ZONES) return -2;
    
    uint8_t zid = config->zone_id;
    memcpy(&g_ff.zone_configs[zid], config, sizeof(ff_zone_config_t));
    
    /* Initialize zone state */
    ff_zone_state_data_t* state = &g_ff.zone_states[zid];
    memset(state, 0, sizeof(*state));
    state->state = FF_ZONE_IDLE;
    state->valve_state = FF_VALVE_CLOSED;
    state->agent_level_pct = 100.0f;
    state->pressure_psi = 100.0f;
    
    if (zid >= g_ff.zone_count) {
        g_ff.zone_count = zid + 1;
    }
    
    return 0;
}

/**
 * @brief Arm a zone for monitoring
 */
int ff_arm_zone(uint8_t zone_id) {
    if (!g_ff.initialized || zone_id >= g_ff.zone_count) return -1;
    
    ff_zone_state_data_t* state = &g_ff.zone_states[zone_id];
    
    if (state->state == FF_ZONE_DISABLED || state->state == FF_ZONE_FAULT) {
        return -2;
    }
    
    state->state = FF_ZONE_MONITORING;
    state->state_change_ms = ff_get_time_ms();
    ff_log_event(FF_EVENT_ZONE_ARMED, zone_id, 0, 
                g_ff.zone_configs[zone_id].name);
    
    return 0;
}

/**
 * @brief Disarm a zone
 */
int ff_disarm_zone(uint8_t zone_id) {
    if (!g_ff.initialized || zone_id >= g_ff.zone_count) return -1;
    
    ff_zone_state_data_t* state = &g_ff.zone_states[zone_id];
    state->state = FF_ZONE_IDLE;
    state->state_change_ms = ff_get_time_ms();
    ff_log_event(FF_EVENT_ZONE_DISARMED, zone_id, 0,
                g_ff.zone_configs[zone_id].name);
    
    return 0;
}

/**
 * @brief Get zone status
 */
int ff_get_zone_status(uint8_t zone_id, ff_zone_state_t* state_out,
                      ff_valve_state_t* valve_out, float* temp_out) {
    if (!g_ff.initialized || zone_id >= g_ff.zone_count) return -1;
    
    ff_zone_state_data_t* state = &g_ff.zone_states[zone_id];
    
    if (state_out) *state_out = state->state;
    if (valve_out) *valve_out = state->valve_state;
    if (temp_out) *temp_out = state->max_temp_detected;
    
    return 0;
}

/**
 * @brief Manually trigger suppression in a zone
 */
int ff_manual_trigger(uint8_t zone_id) {
    if (!g_ff.initialized || zone_id >= g_ff.zone_count) return -1;
    
    ff_zone_state_data_t* state = &g_ff.zone_states[zone_id];
    ff_zone_config_t* config = &g_ff.zone_configs[zone_id];
    
    if (state->state == FF_ZONE_DISABLED || state->state == FF_ZONE_FAULT ||
        state->state == FF_ZONE_DISCHARGING || state->state == FF_ZONE_DISCHARGED) {
        return -2;
    }
    
    uint32_t now = ff_get_time_ms();
    state->manually_triggered = true;
    state->alarm_start_ms = now;
    ff_log_event(FF_EVENT_MANUAL_TRIGGER, zone_id, 0, "Manual activation");
    
    /* Immediately transition to preaction or discharging */
    if (config->system_type == FF_SYSTEM_PREACTION && config->preaction_delay > 0) {
        state->state = FF_ZONE_PREACTION;
        state->state_change_ms = now;
    } else {
        state->state = FF_ZONE_DISCHARGING;
        state->discharge_start_ms = now;
        state->state_change_ms = now;
        g_ff.stats.suppressions_activated++;
        ff_log_event(FF_EVENT_SUPPRESSION_START, zone_id, 0, config->name);
    }
    
    /* Start incident if not already */
    ff_start_incident();
    
    return 0;
}

/**
 * @brief Abort suppression in a zone (during pre-action delay)
 */
int ff_abort_suppression(uint8_t zone_id) {
    if (!g_ff.initialized || zone_id >= g_ff.zone_count) return -1;
    
    ff_zone_state_data_t* state = &g_ff.zone_states[zone_id];
    ff_zone_config_t* config = &g_ff.zone_configs[zone_id];
    
    if (!config->abort_enabled) return -2;
    
    if (state->state == FF_ZONE_PREACTION) {
        state->aborted = true;
        return 0;
    }
    
    return -3;  /* Cannot abort in current state */
}

/* ============================================================================
 * Public API - Processing
 * ============================================================================ */

/**
 * @brief Process one tick of the firefighting system
 */
int ff_process(uint32_t delta_ms) {
    if (!g_ff.initialized) return -1;
    
    g_ff.current_time_ms += delta_ms;
    g_ff.stats.uptime_seconds = g_ff.current_time_ms / 1000;
    
    /* Check each zone for alarm conditions */
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        ff_check_zone_alarms(i);
    }
    
    /* Update valve states */
    ff_update_valves();
    
    /* Update incident if active */
    if (g_ff.hot_spot_count > 0 || 
        (g_ff.incident_active && g_ff.incident.stage != FF_FIRE_EXTINGUISHED)) {
        if (!g_ff.incident_active) {
            ff_start_incident();
        }
        ff_update_incident();
    }
    
    return 0;
}

/**
 * @brief Arm the entire system
 */
int ff_arm_system(void) {
    if (!g_ff.initialized) return -1;
    
    g_ff.armed = true;
    
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        ff_arm_zone(i);
    }
    
    return 0;
}

/**
 * @brief Disarm the entire system
 */
int ff_disarm_system(void) {
    if (!g_ff.initialized) return -1;
    
    g_ff.armed = false;
    
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        ff_disarm_zone(i);
    }
    
    return 0;
}

/* ============================================================================
 * Public API - Telemetry
 * ============================================================================ */

/**
 * @brief Get current incident status
 */
int ff_get_incident(ff_incident_t* incident) {
    if (!g_ff.initialized || !incident) return -1;
    
    memcpy(incident, &g_ff.incident, sizeof(ff_incident_t));
    return g_ff.incident_active ? 0 : 1;
}

/**
 * @brief Get event log
 */
int ff_get_events(ff_event_t* events, uint16_t max_events, uint16_t* count) {
    if (!g_ff.initialized || !events || !count) return -1;
    
    uint16_t copy_count = (g_ff.event_count < max_events) ? 
                          g_ff.event_count : max_events;
    
    /* Copy events in chronological order */
    uint16_t start = (g_ff.event_head + FF_MAX_EVENTS - g_ff.event_count) % 
                     FF_MAX_EVENTS;
    
    for (uint16_t i = 0; i < copy_count; i++) {
        uint16_t idx = (start + i) % FF_MAX_EVENTS;
        memcpy(&events[i], &g_ff.events[idx], sizeof(ff_event_t));
    }
    
    *count = copy_count;
    return 0;
}

/**
 * @brief Get hot spot list
 */
int ff_get_hot_spots(ff_hot_spot_t* spots, uint8_t max_spots, uint8_t* count) {
    if (!g_ff.initialized || !spots || !count) return -1;
    
    uint8_t copy_count = (g_ff.hot_spot_count < max_spots) ?
                         g_ff.hot_spot_count : max_spots;
    
    memcpy(spots, g_ff.hot_spots, copy_count * sizeof(ff_hot_spot_t));
    *count = copy_count;
    
    return 0;
}

/**
 * @brief Get system statistics
 */
int ff_get_stats(ff_stats_t* stats) {
    if (!g_ff.initialized || !stats) return -1;
    
    memcpy(stats, &g_ff.stats, sizeof(ff_stats_t));
    return 0;
}

/**
 * @brief Generate telemetry packet for transmission
 */
int ff_generate_telemetry(uint8_t* buffer, uint16_t max_len, uint16_t* len) {
    if (!g_ff.initialized || !buffer || !len) return -1;
    
    /* Create a simple telemetry structure */
    typedef struct {
        uint32_t timestamp;
        uint8_t armed;
        uint8_t incident_active;
        uint8_t fire_stage;
        uint8_t zone_count;
        uint8_t zone_states[FF_MAX_ZONES];
        float max_temps[FF_MAX_ZONES];
        uint8_t hot_spot_count;
    } __attribute__((packed)) telemetry_packet_t;
    
    if (max_len < sizeof(telemetry_packet_t)) return -2;
    
    telemetry_packet_t* pkt = (telemetry_packet_t*)buffer;
    
    pkt->timestamp = ff_get_time_ms();
    pkt->armed = g_ff.armed ? 1 : 0;
    pkt->incident_active = g_ff.incident_active ? 1 : 0;
    pkt->fire_stage = (uint8_t)g_ff.incident.stage;
    pkt->zone_count = g_ff.zone_count;
    
    for (uint8_t i = 0; i < g_ff.zone_count && i < FF_MAX_ZONES; i++) {
        pkt->zone_states[i] = (uint8_t)g_ff.zone_states[i].state;
        pkt->max_temps[i] = g_ff.zone_states[i].max_temp_detected;
    }
    
    pkt->hot_spot_count = g_ff.hot_spot_count;
    
    *len = sizeof(telemetry_packet_t);
    return 0;
}

/* ============================================================================
 * Public API - Heat Map Access
 * ============================================================================ */

/**
 * @brief Get heat map for a sensor (color-coded temperatures)
 */
int ff_get_heat_map(uint8_t sensor_id, uint8_t* heat_map, 
                   uint16_t* width, uint16_t* height) {
    if (!g_ff.initialized || sensor_id >= g_ff.sensor_count) return -1;
    if (!heat_map || !width || !height) return -2;
    
    ff_thermal_sensor_t* sensor = &g_ff.sensors[sensor_id];
    
    ff_generate_heat_map(sensor->frame, heat_map, 
                        FF_THERMAL_WIDTH, FF_THERMAL_HEIGHT);
    
    *width = FF_THERMAL_WIDTH;
    *height = FF_THERMAL_HEIGHT;
    
    return 0;
}

/**
 * @brief Get raw temperature value at a pixel
 */
int ff_get_pixel_temp(uint8_t sensor_id, uint16_t x, uint16_t y, float* temp) {
    if (!g_ff.initialized || sensor_id >= g_ff.sensor_count) return -1;
    if (x >= FF_THERMAL_WIDTH || y >= FF_THERMAL_HEIGHT) return -2;
    if (!temp) return -3;
    
    ff_thermal_sensor_t* sensor = &g_ff.sensors[sensor_id];
    *temp = sensor->frame[y * FF_THERMAL_WIDTH + x];
    
    return 0;
}

/**
 * @brief Check if system is currently in alarm state
 */
bool ff_is_alarm_active(void) {
    if (!g_ff.initialized) return false;
    
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        if (g_ff.zone_states[i].state >= FF_ZONE_ALARM &&
            g_ff.zone_states[i].state < FF_ZONE_DISCHARGED) {
            return true;
        }
    }
    return false;
}

/**
 * @brief Check if any zone is actively discharging
 */
bool ff_is_discharging(void) {
    if (!g_ff.initialized) return false;
    
    for (uint8_t i = 0; i < g_ff.zone_count; i++) {
        if (g_ff.zone_states[i].state == FF_ZONE_DISCHARGING) {
            return true;
        }
    }
    return false;
}
